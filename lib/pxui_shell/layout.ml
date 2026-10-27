open Rays

  type panel = Editor_core.Panels.panel =
    | View of string | Canvas of string | Graph | List | Lisp | Inspector | Spreadsheet | Outline | Timeline
  type axis = Editor_core.Panels.axis
  type t = Editor_core.Panels.t =
    | Leaf of panel
    | Split of { axis : axis; size : Editor_core.Panels.size; a : t; b : t }
    | Tile of t list
    | Float of t
  type path = int list
  type bounds = int * int * int * int

  let default = Editor_core.Panels.default

  let splitter_width = 1
  let collapsed_width = 24
  let header_height = 24
  let header_margin = 4
  let status_height = 25  (* the line-2 hairline and the 24-point bar *)
  let timeline_height = 24
  let timeline_strip = 25  (* the strip under a tree without a timeline: hairline and bar *)

  (* a collapsed panel is its header: as tall in a stack, a 28-point column in a row *)
  let strip axis = if axis = `V then header_height else collapsed_width

  type leaf = { path : path; panel : panel; frame : bounds; header : bounds; body : bounds; floating : bool }
  type splitter = { node : path option; axis : axis; size : Editor_core.Panels.size; bounds : bounds;
                    start : int; span : int }
  let sides (s : splitter) =
    let x, y, _, _ = s.bounds in
    let first = (if s.axis = `H then x else y) - s.start in
    first, s.span - first - splitter_width
  let resized s how : Editor_core.Panels.size =
    let first, second = sides s in
    Editor_core.Panels.clamp_size (match how with
      (* the middle of the point the first side ends on, so the ratio places it there again *)
      | `Ratio -> `Ratio ((float first +. 0.5) /. float (max 1 (s.span - splitter_width)))
      | `First -> `First first | `Second -> `Second second)

  (* the rows of a size menu: the three ways, the one in use greyed *)
  let size_rows (s : splitter) =
    let a, b = if s.axis = `H then "left", "right" else "top", "bottom" in
    let now = match s.size with `Ratio _ -> 0 | `First _ -> 1 | `Second _ -> 2 in
    List.mapi (fun i label -> label, i <> now) [ "By ratio"; "Fix " ^ a ^ " side"; "Fix " ^ b ^ " side" ]
  let size_ways = [ `Ratio; `First; `Second ]

  type geometry = { leaves : leaf list; splitters : splitter list; status_at : bounds;
                    timeline_at : bounds }
  type panes = { view : bounds; graph : bounds; inspector : bounds; status : bounds;
                 timeline : bounds }

  let expand panel hidden = List.filter (( <> ) panel) hidden

  (* A hidden panel vanishes; a hidden viewport keeps a strip with its expand button. *)
  type presence = Live | Strip | Gone
  let join x y = match x, y with
    | Live, _ | _, Live -> Live | Strip, _ | _, Strip -> Strip | Gone, Gone -> Gone
  let rec presence ~state hidden path = function
    | Leaf _ when (state path).Editor_core.Panels.window <> None -> Gone
    | Leaf Timeline when (state path).Editor_core.Panels.collapsed -> Gone  (* it has no header to keep *)
    | Leaf _ when (state path).Editor_core.Panels.collapsed -> Strip
    | Leaf p -> if not (List.mem p hidden) then Live
        else (match p with View _ -> Strip | _ -> Gone)
    | Float _ -> Gone
    | Split { a; b; _ } -> join (presence ~state hidden (path @ [0]) a) (presence ~state hidden (path @ [1]) b)
    | Tile cells -> List.mapi (fun i c -> presence ~state hidden (path @ [i]) c) cells
        |> List.fold_left join Gone

  let is_float = function Float _ -> true | _ -> false

  (* A run of splits by ratio along one axis is one row of columns whose weights are the
     products of the ratios, so [0.45 | 0.55 * 0.6364 | ...] divides the width once.  A split
     with a fixed side is one column of the run and is placed on its own. *)
  let rec columns axis weight path = function
    | Split ({ size = `Ratio ratio; _ } as s) when s.axis = axis && not (is_float s.a || is_float s.b) ->
        columns axis (weight *. ratio) (path @ [ 0 ]) s.a
        @ columns axis (weight *. (1. -. ratio)) (path @ [ 1 ]) s.b
    | tree -> [ weight, path, tree ]

  let rec split_size path tree : Editor_core.Panels.size = match path, tree with
    | [], Split s -> s.size
    | 0 :: rest, Split s -> split_size rest s.a
    | 1 :: rest, Split s -> split_size rest s.b
    | _ -> `Ratio 0.5

  let rec minimum axis = function
    | Leaf p when axis = `H ->
        (match p with View _ -> 220 | Graph | List | Lisp -> 180 | _ -> 120)
    | Split { a; b; _ } -> max (minimum axis a) (minimum axis b)
    | Tile cells -> List.fold_left (fun m c -> max m (minimum axis c)) 0 cells
    | Leaf _ | Float _ -> 0

  (* Sizes of the columns of one row.  A column is (presence, weight, minimum, fixed): a
     fixed extent (a strip, the timeline's own height) is kept, the rest share what is
     left by weight, each at least its minimum when the minimums fit, the last
     weighted one taking the rounding remainder. *)
  let distribute total cols =
    let n = Array.length cols in
    let present = Array.fold_left (fun k (p, _, _, _) -> if p = Gone then k else k + 1) 0 cols in
    let available = max 3 (total - (max 0 (present - 1) * splitter_width)) in
    let sizes = Array.make n 0 in
    let fixed = ref 0 and weight = ref 0. and required = ref 0 and live = ref (-1)
    and last = ref (-1) in
    let weighted (p, _, _, f) = p <> Gone && f = 0 in
    Array.iteri (fun i ((p, w, m, f) as col) ->
      if p <> Gone then last := i;
      if weighted col then (weight := !weight +. w; required := !required + m; live := i)
      else if p <> Gone then (sizes.(i) <- f; fixed := !fixed + f)) cols;
    let flexible = max 3 (available - !fixed) in
    Array.iteri (fun i ((_, w, _, _) as col) -> if weighted col then
      sizes.(i) <- max 1 (int_of_float (float_of_int flexible *. w /. !weight))) cols;
    let rest = if !live >= 0 then !live else !last in
    if rest >= 0 then begin
      let used = Array.fold_left ( + ) 0 sizes in
      sizes.(rest) <- max 1 (sizes.(rest) + available - used)
    end;
    let short = ref false in
    Array.iteri (fun i ((_, _, m, _) as col) -> if weighted col && sizes.(i) < m then short := true) cols;
    if !live >= 0 && !required <= flexible && !short then begin
      let extra = flexible - !required and assigned = ref 0 in
      Array.iteri (fun i ((_, w, m, _) as col) -> if weighted col then begin
        sizes.(i) <- m + int_of_float (float_of_int extra *. w /. !weight);
        assigned := !assigned + sizes.(i) end) cols;
      sizes.(!live) <- sizes.(!live) + flexible - !assigned
    end;
    sizes

  let rec common a b = match a, b with
    | x :: a, y :: b when x = y -> x :: common a b
    | _ -> []

  let rec prefix under path = match under, path with
    | [], _ -> true
    | x :: under, y :: path -> x = y && prefix under path
    | _ -> false

  let geometry ?(state = fun _ -> Editor_core.Panels.default_state) ?(hidden = [ Timeline ]) tree (frame : Frame.t) =
    let all = Editor_core.Panels.leaves tree in
    let header = min header_height (max 0 ((snd frame.input.size) - 1)) in
    let timeline = if List.mem Timeline hidden || List.exists (fun (_, p) -> p = Timeline) all
      then 0 else min timeline_strip (max 0 ((snd frame.input.size) - header - 1)) in
    (* the status strip spans the window below the tree and the timeline strip *)
    let status = min status_height (max 0 ((snd frame.input.size) - header - timeline - 1)) in
    let bottom = (snd frame.input.size) - status - timeline in
    let leaves = ref [] and splitters = ref [] and floats = ref [] in
    let leaf ?(floating = false) path panel outer =
      let collapsed = (state path).Editor_core.Panels.collapsed in
      (* a collapsed window is a short tab, its title and the expand button, wherever its place
         came from: a saved window or the default one of a ui/floating *)
      let (fx, fy, fw, fh) as outer = match outer with
        | x, y, w, _ when floating && collapsed ->
            x, y, min w (max 90 (60 + (7 * String.length (Editor_core.Panels.name panel)))),
            header_margin + header_height + 2
        | outer -> outer in
      (* a window keeps its 1-point line-3 edge on all four sides: header and body lie inside it *)
      let x, y, w, h = if floating then fx + 1, fy + 1, max 1 (fw - 2), max 1 (fh - 2) else outer in
      (* a header is a 24-point row under a 4-point margin; a docked collapsed pane is its row alone *)
      (* a docked timeline under 90 points is its strip alone; a taller one (timeline.html) has
         a header too *)
      let strip = panel = Timeline && not floating && h < 90 in
      let margin = if (strip || collapsed) && not floating then 0 else min header_margin (max 0 (h - 2)) in
      let hh = if strip then 0
        else min header_height (max 0 (if collapsed then h - margin else h - margin - 1)) in
      let body = if collapsed then 0 else max 1 (h - margin - hh) in
      (* a viewport window's picture stands 12 points in from the sheet, under a frame (windows.html) *)
      let inset = floating && (match panel with View _ -> true | _ -> false) && not collapsed && w > 40 && body > 40 in
      leaves := { path; panel; floating; frame = outer; header = (x, y + margin, w, hh);
                  body = (if inset then (x + 12, y + margin + hh, w - 24, body - 12) else (x, y + margin + hh, w, body)) } :: !leaves in
    let cut total n = (* n cells and n-1 gutters over [total] *)
      let cell = max 1 ((total - ((n - 1) * splitter_width)) / n) in
      Array.init n (fun i -> if i = n - 1 then max 1 (total - (i * (cell + splitter_width))) else cell) in
    let rec place ?(floating = false) ((x, y, w, h) as rect) path = function
      | Leaf p -> if presence ~state hidden path (Leaf p) <> Gone then leaf ~floating path p rect
      | Float t -> floats := ((x + (w / 8), y + (h / 8), w - (w / 4), h - (h / 4)), path @ [ 0 ], t)
                             :: !floats
      | Split { a; b; _ } when is_float a || is_float b ->
          place ~floating rect (path @ [ 0 ]) a; place ~floating rect (path @ [ 1 ]) b
      | Split { axis; size = (`First points | `Second points) as size; a; b } ->
          (* one side keeps its points, the other takes the rest: the other keeps its minimum
             first, then the fixed side shrinks, never below one point *)
          let pa = presence ~state hidden (path @ [ 0 ]) a and pb = presence ~state hidden (path @ [ 1 ]) b in
          if pb = Gone then (if pa <> Gone then place ~floating rect (path @ [ 0 ]) a)
          else if pa = Gone then place ~floating rect (path @ [ 1 ]) b
          else begin
            let horizontal = axis = `H in
            let total = if horizontal then w else h in
            let available = max 2 (total - splitter_width) in
            let first = match size with `First _ -> true | _ -> false in
            let fixed_presence, other, other_presence = if first then pa, b, pb else pb, a, pa in
            let fixed =
              if other_presence = Strip then available - min (strip axis) (available - 1)
              else max 1 (min (if fixed_presence = Strip then strip axis else points)
                     (available - min (max 1 (minimum axis other)) (available - 1))) in
            let size_a = if first then fixed else available - fixed in
            let origin = if horizontal then x else y in
            let rect_at from extent = if horizontal then (from, y, extent, h) else (x, from, w, extent) in
            place ~floating (rect_at origin size_a) (path @ [ 0 ]) a;
            splitters := { node = Some path; axis; size; bounds = rect_at (origin + size_a) splitter_width;
                           start = origin; span = total } :: !splitters;
            place ~floating (rect_at (origin + size_a + splitter_width) (available - size_a)) (path @ [ 1 ]) b
          end
      | Split { axis; _ } as tree ->
          let cols = Array.of_list (columns axis 1. [] tree) in
          let info = Array.map (fun (weight, col_path, sub) ->
            let p = presence ~state hidden (path @ col_path) sub in
            p, weight, minimum axis sub,
            (match p, sub with
             | Strip, _ -> strip axis
             | Live, Leaf Timeline when axis = `V -> timeline_height
             | _ -> 0)) cols in
          let horizontal = axis = `H in
          let sizes = distribute (if horizontal then w else h) info in
          let cursor = ref (if horizontal then x else y) in
          let prior = ref None and placed = ref [] and gutters = ref [] in
          Array.iteri (fun i (_, col_path, sub) ->
            let p, _, _, _ = info.(i) in
            if p <> Gone then begin
              Option.iter (fun before ->
                gutters := (common before col_path, if horizontal then (!cursor, y, splitter_width, h)
                            else (x, !cursor, w, splitter_width)) :: !gutters;
                cursor := !cursor + splitter_width) !prior;
              let rect = if horizontal then (!cursor, y, sizes.(i), h) else (x, !cursor, w, sizes.(i)) in
              place ~floating rect (path @ col_path) sub;
              placed := (col_path, if horizontal then !cursor, sizes.(i) else !cursor, sizes.(i)) :: !placed;
              cursor := !cursor + sizes.(i);
              prior := Some col_path
            end) cols;
          let extent node =
            match List.filter (fun (cp, _) -> prefix node cp) (List.rev !placed) with
            | [] -> 0, 1
            | (_, (start, _)) :: _ as under ->
                let _, (s, len) = List.nth under (List.length under - 1) in start, s + len - start in
          List.iter (fun (node, bounds) ->
            let start, span = extent node in
            splitters := { node = Some (path @ node); axis; size = split_size node tree; bounds; start; span }
                         :: !splitters)
            (List.rev !gutters)
      | Tile cells ->
          let cells = List.mapi (fun i cell -> i, cell) cells
            |> List.filter (fun (i, cell) -> is_float cell || presence ~state hidden (path @ [i]) cell <> Gone) in
          let n = List.length cells in
          if n > 0 then begin
          let columns = int_of_float (Float.ceil (sqrt (float_of_int n))) in
          let rows = (n + columns - 1) / columns in
          let heights = cut h rows in
          let offset sizes k = let s = ref 0 in
            for j = 0 to k - 1 do s := !s + sizes.(j) + splitter_width done; !s in
          (* a short last row stretches its cells over the whole width *)
          let widths = Array.init rows (fun r -> cut w (min columns (n - (r * columns)))) in
          List.iteri (fun i (original, cell) ->
            let c = i mod columns and r = i / columns in
            let cx = x + offset widths.(r) c and cy = y + offset heights r in
            if c > 0 then splitters := { node = None; axis = `H; size = `Ratio 0.5;
              bounds = (cx - splitter_width, cy, splitter_width, heights.(r)); start = 0; span = 1 }
              :: !splitters;
            if r > 0 && c = 0 then splitters := { node = None; axis = `V; size = `Ratio 0.5;
              bounds = (x, cy - splitter_width, w, splitter_width); start = 0; span = 1 } :: !splitters;
            place ~floating (cx, cy, widths.(r).(c), heights.(r)) (path @ [ original ]) cell) cells
          end in
    place (0, 0, (fst frame.input.size), max 1 bottom) [] tree;
    let rec drain () = match List.rev !floats with
      | [] -> ()
      | queue -> floats := [];
          List.iter (fun (rect, path, t) -> place ~floating:true rect path t) queue; drain () in
    drain ();
    List.iter (fun (path, panel) -> match (state path).Editor_core.Panels.window with
      | None -> ()
      | Some (x, y, w, h) ->
          let w = min (fst frame.input.size) w and h = min (snd frame.input.size) h in
          let x = max 0 (min x ((fst frame.input.size) - w)) and y = max 0 (min y ((snd frame.input.size) - header_height)) in
          leaf ~floating:true path panel (x, y, w, min h ((snd frame.input.size) - y))) all;
    { leaves = List.rev !leaves; splitters = List.rev !splitters;
      status_at = (0, (snd frame.input.size) - status, (fst frame.input.size), status);
      timeline_at = (0, bottom, (fst frame.input.size), timeline) }

  let find geometry panel = List.find_opt (fun l -> l.panel = panel && (let _, _, _, h = l.body in h > 0)) geometry.leaves
  let first_view geometry = List.find_opt (fun l -> (let _, _, _, h = l.body in h > 0)
      && match l.panel with View _ -> true | _ -> false)
      geometry.leaves

  let zero = (0, 0, 0, 0)
  let panes g =
    let body panel = match find g panel with Some l -> l.body | None -> zero in
    { view = (match first_view g with Some l -> l.body | None -> zero);
      graph = body Graph; inspector = body Inspector; status = g.status_at; timeline = g.timeline_at }
