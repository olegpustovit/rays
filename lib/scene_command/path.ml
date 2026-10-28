type point = { x : float; y : float }

type command =
  | Move_to of point
  | Line_to of point
  | Quadratic_to of point * point
  | Cubic_to of point * point * point
  | Close

type t = command array
type fill_rule = Even_odd | Non_zero
type mesh = { vertices : point array; indices : int array }
type cap = Butt | Square | Round
type join = Miter | Bevel | Round

type error =
  | Empty_path
  | Invalid_tolerance of float
  | Invalid_width of float
  | Invalid_miter_limit of float
  | Non_finite_point of point
  | Missing_current_point
  | Open_contour
  | Complexity_limit

let of_commands commands = Array.copy commands
let finite p = Float.is_finite p.x && Float.is_finite p.y

module Points = struct
  type t = { mutable values : point array; mutable length : int }

  let create () = { values = Array.make 16 { x = 0.; y = 0. }; length = 0 }

  let add t value =
    if t.length = Array.length t.values then begin
      if t.length >= 1_048_576 then raise Exit;
      let values = Array.make (2 * t.length) value in
      Array.blit t.values 0 values 0 t.length;
      t.values <- values
    end;
    t.values.(t.length) <- value;
    t.length <- t.length + 1

  let array t = Array.sub t.values 0 t.length
end

let midpoint a b = { x = (a.x +. b.x) *. 0.5; y = (a.y +. b.y) *. 0.5 }

let distance_to_line_squared p a b =
  let dx = b.x -. a.x and dy = b.y -. a.y in
  let denominator = (dx *. dx) +. (dy *. dy) in
  if denominator = 0. then
    let x = p.x -. a.x and y = p.y -. a.y in
    (x *. x) +. (y *. y)
  else
    let cross = ((p.x -. a.x) *. dy) -. ((p.y -. a.y) *. dx) in
    cross *. cross /. denominator

let flatten ~tolerance path =
  if (not (Float.is_finite tolerance)) || tolerance <= 0. then Error (Invalid_tolerance tolerance)
  else if Array.length path = 0 then Error Empty_path
  else
    let tolerance_squared = tolerance *. tolerance in
    let contours = ref [] and current = ref None and start = ref None in
    let points = ref (Points.create ()) in
    let finish closed =
      let values = Points.array !points in
      if Array.length values > 0 then contours := (values, closed) :: !contours;
      points := Points.create ();
      current := None;
      start := None
    in
    let rec quadratic output depth a control b =
      if depth = 24 || distance_to_line_squared control a b <= tolerance_squared then
        Points.add output b
      else
        let ab = midpoint a control and bc = midpoint control b in
        let center = midpoint ab bc in
        quadratic output (depth + 1) a ab center;
        quadratic output (depth + 1) center bc b
    and cubic output depth a c1 c2 b =
      if
        depth = 24
        || max (distance_to_line_squared c1 a b) (distance_to_line_squared c2 a b)
           <= tolerance_squared
      then Points.add output b
      else
        let a1 = midpoint a c1 and c12 = midpoint c1 c2 and c2b = midpoint c2 b in
        let left = midpoint a1 c12 and right = midpoint c12 c2b in
        let center = midpoint left right in
        cubic output (depth + 1) a a1 left center;
        cubic output (depth + 1) center right c2b b
    in
    let bad = ref None in
    try
      Array.iter
        (fun command ->
          let check p =
            if not (finite p) then (
              bad := Some p;
              raise Exit)
          in
          match command with
          | Move_to p ->
              check p;
              if !current <> None then finish false;
              Points.add !points p;
              current := Some p;
              start := Some p
          | Line_to p ->
              check p;
              (match !current with None -> raise Not_found | Some _ -> Points.add !points p);
              current := Some p
          | Quadratic_to (control, p) ->
              check control;
              check p;
              (match !current with
              | None -> raise Not_found
              | Some a -> quadratic !points 0 a control p);
              current := Some p
          | Cubic_to (c1, c2, p) ->
              check c1;
              check c2;
              check p;
              (match !current with None -> raise Not_found | Some a -> cubic !points 0 a c1 c2 p);
              current := Some p
          | Close ->
              (match (!current, !start) with
              | Some last, Some first when last <> first -> Points.add !points first
              | Some _, Some _ -> ()
              | _ -> raise Not_found);
              finish true)
        path;
      if !current <> None then finish false;
      Ok (Array.of_list (List.rev_map fst !contours))
    with
    | Not_found -> Error Missing_current_point
    | Exit -> (
        match !bad with Some p -> Error (Non_finite_point p) | None -> Error Complexity_limit)

type crossing = { x0 : float; x1 : float; winding : int }

let tessellate ~tolerance ~fill_rule path =
  let contours_closed =
    let open_contour = ref false and active = ref false in
    Array.iter
      (function
        | Move_to _ ->
            if !active then open_contour := true;
            active := true
        | Close -> active := false
        | _ -> ())
      path;
    not (!active || !open_contour)
  in
  if not contours_closed then Error Open_contour
  else
    match flatten ~tolerance path with
    | Error _ as error -> error
    | Ok contours -> (
        let ys = Array.concat (Array.to_list (Array.map (Array.map (fun p -> p.y)) contours)) in
        Array.sort Float.compare ys;
        let unique = Points.create () in
        Array.iter
          (fun y ->
            if unique.length = 0 || unique.values.(unique.length - 1).y <> y then
              Points.add unique { x = 0.; y })
          ys;
        let vertices = Points.create () and triangles = ref [] in
        let add_quad left right y0 y1 =
          let base = vertices.length in
          Points.add vertices { x = left.x0; y = y0 };
          Points.add vertices { x = right.x0; y = y0 };
          Points.add vertices { x = right.x1; y = y1 };
          Points.add vertices { x = left.x1; y = y1 };
          triangles :=
            base :: (base + 1) :: (base + 2) :: base :: (base + 2) :: (base + 3) :: !triangles
        in
        try
          for band = 0 to unique.length - 2 do
            let y0 = unique.values.(band).y and y1 = unique.values.(band + 1).y in
            if y1 > y0 then begin
              let middle = (y0 +. y1) *. 0.5 and edges = ref [] in
              Array.iter
                (fun contour ->
                  for i = 0 to Array.length contour - 2 do
                    let a = contour.(i) and b = contour.(i + 1) in
                    let low = min a.y b.y and high = max a.y b.y in
                    if a.y <> b.y && middle >= low && middle < high then begin
                      let at y = a.x +. ((y -. a.y) *. (b.x -. a.x) /. (b.y -. a.y)) in
                      edges :=
                        { x0 = at y0; x1 = at y1; winding = (if b.y > a.y then 1 else -1) }
                        :: !edges
                    end
                  done)
                contours;
              let edges = Array.of_list !edges in
              Array.sort
                (fun a b -> Float.compare ((a.x0 +. a.x1) *. 0.5) ((b.x0 +. b.x1) *. 0.5))
                edges;
              let inside = ref false and winding = ref 0 and left = ref None in
              Array.iter
                (fun edge ->
                  let was_inside = !inside in
                  (match fill_rule with
                  | Even_odd -> inside := not !inside
                  | Non_zero ->
                      winding := !winding + edge.winding;
                      inside := !winding <> 0);
                  if (not was_inside) && !inside then left := Some edge
                  else if was_inside && not !inside then
                    match !left with
                    | Some start -> add_quad start edge y0 y1
                    | None -> assert false)
                edges
            end
          done;
          Ok { vertices = Points.array vertices; indices = Array.of_list (List.rev !triangles) }
        with Exit -> Error Complexity_limit)

let stroke ~tolerance ~width ~cap ~join ~miter_limit path =
  if (not (Float.is_finite width)) || width <= 0. then Error (Invalid_width width)
  else if (not (Float.is_finite miter_limit)) || miter_limit < 1. then
    Error (Invalid_miter_limit miter_limit)
  else
    match flatten ~tolerance path with
    | Error _ as error -> error
    | Ok contours -> (
        let vertices = Points.create () and indices = ref [] in
        let triangle a b c = indices := a :: b :: c :: !indices in
        let vertex p =
          let index = vertices.length in
          Points.add vertices p;
          index
        in
        let add_triangle a b c = triangle (vertex a) (vertex b) (vertex c) in
        let half = width *. 0.5 in
        let unit a b =
          let dx = b.x -. a.x and dy = b.y -. a.y in
          let length = sqrt ((dx *. dx) +. (dy *. dy)) in
          if length = 0. then None else Some (dx /. length, dy /. length)
        in
        let offset p nx ny scale = { x = p.x +. (nx *. scale); y = p.y +. (ny *. scale) } in
        let fan center radius start_angle delta =
          let denominator =
            if tolerance >= radius then Float.pi
            else 2. *. acos (max (-1.) (1. -. (tolerance /. radius)))
          in
          let steps = min 64 (max 1 (int_of_float (ceil (abs_float delta /. denominator)))) in
          let previous = ref (offset center (cos start_angle) (sin start_angle) radius) in
          for i = 1 to steps do
            let angle = start_angle +. (delta *. float i /. float steps) in
            let next = offset center (cos angle) (sin angle) radius in
            add_triangle center !previous next;
            previous := next
          done
        in
        let segment a b =
          match unit a b with
          | None -> ()
          | Some (dx, dy) ->
              let nx = -.dy and ny = dx in
              let a0 = offset a nx ny half
              and a1 = offset a nx ny (-.half)
              and b0 = offset b nx ny half
              and b1 = offset b nx ny (-.half) in
              let base = vertices.length in
              Points.add vertices a0;
              Points.add vertices a1;
              Points.add vertices b1;
              Points.add vertices b0;
              triangle base (base + 1) (base + 2);
              triangle base (base + 2) (base + 3)
        in
        let add_join a b c =
          match (unit a b, unit b c) with
          | Some (dx0, dy0), Some (dx1, dy1) ->
              let cross = (dx0 *. dy1) -. (dy0 *. dx1) in
              if abs_float cross > 1e-15 then begin
                let side = if cross > 0. then 1. else -1. in
                let n0x = -.dy0 *. side
                and n0y = dx0 *. side
                and n1x = -.dy1 *. side
                and n1y = dx1 *. side in
                let p0 = offset b n0x n0y half and p1 = offset b n1x n1y half in
                match join with
                | Bevel -> add_triangle b p0 p1
                | Round ->
                    let a0 = atan2 n0y n0x and a1 = atan2 n1y n1x in
                    let raw = a1 -. a0 in
                    let delta =
                      if side > 0. && raw < 0. then raw +. (2. *. Float.pi)
                      else if side < 0. && raw > 0. then raw -. (2. *. Float.pi)
                      else raw
                    in
                    fan b half a0 delta
                | Miter ->
                    let sx = n0x +. n1x and sy = n0y +. n1y in
                    let length = sqrt ((sx *. sx) +. (sy *. sy)) in
                    let mx = sx /. length and my = sy /. length in
                    let scale = half /. ((mx *. n1x) +. (my *. n1y)) in
                    if abs_float scale > half *. miter_limit then add_triangle b p0 p1
                    else
                      let tip = offset b mx my scale in
                      add_triangle p0 tip p1
              end
          | _ -> ()
        in
        let add_cap point direction at_start =
          let dx, dy = direction in
          let nx = -.dy and ny = dx in
          match cap with
          | Butt -> ()
          | Square ->
              let sign = if at_start then -1. else 1. in
              let inner0 = offset point nx ny half and inner1 = offset point nx ny (-.half) in
              let end_point = offset point dx dy (sign *. half) in
              let outer0 = offset end_point nx ny half
              and outer1 = offset end_point nx ny (-.half) in
              add_triangle inner0 inner1 outer1;
              add_triangle inner0 outer1 outer0
          | Round ->
              let angle = atan2 ny nx in
              fan point half angle (if at_start then Float.pi else -.Float.pi)
        in
        try
          Array.iter
            (fun contour ->
              let count = Array.length contour in
              let closed = count > 2 && contour.(0) = contour.(count - 1) in
              for i = 0 to count - 2 do
                segment contour.(i) contour.(i + 1)
              done;
              if closed then begin
                for i = 1 to count - 2 do
                  add_join contour.(i - 1) contour.(i) contour.(i + 1)
                done;
                add_join contour.(count - 2) contour.(0) contour.(1)
              end
              else if count >= 2 then begin
                for i = 1 to count - 2 do
                  add_join contour.(i - 1) contour.(i) contour.(i + 1)
                done;
                (match unit contour.(0) contour.(1) with
                | Some direction -> add_cap contour.(0) direction true
                | None -> ());
                match unit contour.(count - 2) contour.(count - 1) with
                | Some direction -> add_cap contour.(count - 1) direction false
                | None -> ()
              end)
            contours;
          Ok { vertices = Points.array vertices; indices = Array.of_list (List.rev !indices) }
        with Exit -> Error Complexity_limit)
