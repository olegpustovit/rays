module S = Rdk.Surface_index
module H = Flow.Ray_host
let sphere_prim k = -2 - k

(* The nearest triangle of one prototype, in its own space. *)
let static_prototype geometry =
  match S.create geometry with
  | Error e -> Error (Rdk.Error.to_string e)
  | Ok index ->
      let positions = Rdk.Geometry.positions geometry in
      Ok (fun q ->
        let direction = S.View.Constant_direction { x = q.(3); y = q.(4); z = q.(5) } in
        let origin = Rdk.Packed.Float3.View.of_owned_exn ~x:[| q.(0) |] ~y:[| q.(1) |] ~z:[| q.(2) |] in
        let primitives = [| -1 |] and triangles = [| -1 |] and a = [| 0. |] and b = [| 0. |] and c = [| 0. |]
        and distances = [| infinity |] in
        S.View.raycast_many_into ~grain:1 index ~queries:origin ~directions:direction
          ~min_distance:q.(6) ~max_distance:q.(7) ~tolerance:0. ~direction_mode:S.Ray_forward
          ~surface_hit:S.Ray_first_surface ~primitives ~triangles ~barycentric_a:a ~barycentric_b:b
          ~barycentric_c:c ~distances;
        if primitives.(0) < 0 then H.miss
        else
          let point corner = Rdk.Packed.Float3.get positions (S.View.triangle_point index triangles.(0) corner) in
          let x0, y0, z0 = point 0 and x1, y1, z1 = point 1 and x2, y2, z2 = point 2 in
          let ux = x1 -. x0 and uy = y1 -. y0 and uz = z1 -. z0 and vx = x2 -. x0 and vy = y2 -. y0 and vz = z2 -. z0 in
          let nx = uy *. vz -. uz *. vy and ny = uz *. vx -. ux *. vz and nz = ux *. vy -. uy *. vx in
          let n = Float.sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
          { H.t = distances.(0); prim = primitives.(0); u = b.(0); v = c.(0); nx = nx /. n; ny = ny /. n;
            nz = nz /. n; hit = true; instance = 0 })

(* Vertex motion: the triangles of [geometry] with each point moved toward [ending]'s at the query's time,
   intersected directly (Moller-Trumbore) since the interpolated surface changes per ray. *)
let moving_prototype geometry ending =
  let positions = Rdk.Geometry.positions geometry and ends = Rdk.Geometry.positions ending in
  if Rdk.Packed.Float3.length positions <> Rdk.Packed.Float3.length ends then
    Error "scene/trace: a moving geometry's end keyframe must have its start's points"
  else match S.create geometry with
  | Error e -> Error (Rdk.Error.to_string e)
  | Ok index -> Ok (fun q ->
      let time = H.shutter q.(8) in
      let norm = Float.sqrt ((q.(3) *. q.(3)) +. (q.(4) *. q.(4)) +. (q.(5) *. q.(5))) in
      let dx = q.(3) /. norm and dy = q.(4) /. norm and dz = q.(5) /. norm in
      let nearest = ref H.miss in
      for triangle = 0 to S.triangle_count index - 1 do
        let point corner =
          let k = S.Private.triangle_point index triangle corner in
          let x0, y0, z0 = Rdk.Packed.Float3.get positions k and x1, y1, z1 = Rdk.Packed.Float3.get ends k in
          x0 +. ((x1 -. x0) *. time), y0 +. ((y1 -. y0) *. time), z0 +. ((z1 -. z0) *. time) in
        let x0, y0, z0 = point 0 and x1, y1, z1 = point 1 and x2, y2, z2 = point 2 in
        let ux = x1 -. x0 and uy = y1 -. y0 and uz = z1 -. z0 and vx = x2 -. x0 and vy = y2 -. y0 and vz = z2 -. z0 in
        let px = (dy *. vz) -. (dz *. vy) and py = (dz *. vx) -. (dx *. vz) and pz = (dx *. vy) -. (dy *. vx) in
        let det = (ux *. px) +. (uy *. py) +. (uz *. pz) in
        if det <> 0. then begin
          let sx = q.(0) -. x0 and sy = q.(1) -. y0 and sz = q.(2) -. z0 in
          let b = ((sx *. px) +. (sy *. py) +. (sz *. pz)) /. det in
          let qx = (sy *. uz) -. (sz *. uy) and qy = (sz *. ux) -. (sx *. uz) and qz = (sx *. uy) -. (sy *. ux) in
          let c = ((dx *. qx) +. (dy *. qy) +. (dz *. qz)) /. det in
          let t = ((vx *. qx) +. (vy *. qy) +. (vz *. qz)) /. det in
          if b >= 0. && c >= 0. && b +. c <= 1. && t >= q.(6) && t <= q.(7) && ((not !nearest.H.hit) || t < !nearest.H.t) then begin
            let nx = (uy *. vz) -. (uz *. vy) and ny = (uz *. vx) -. (ux *. vz) and nz = (ux *. vy) -. (uy *. vx) in
            let n = Float.sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
            nearest := { H.t; prim = S.Private.triangle_primitive index triangle; u = b; v = c; nx = nx /. n;
                         ny = ny /. n; nz = nz /. n; hit = true; instance = 0 }
          end
        end
      done;
      !nearest)
let prototype ?ending geometry = match ending with
  | None -> static_prototype geometry
  | Some ending -> moving_prototype geometry ending

(* The ray in an instance's space; the hit back in world space. [w] is world_to_object. *)
let instance_hit trace w k q =
  let dx = q.(3) and dy = q.(4) and dz = q.(5) in
  let norm = Float.sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz)) in
  let transform x y z translate = [| (w.(0) *. x) +. (w.(1) *. y) +. (w.(2) *. z) +. (if translate then w.(3) else 0.);
    (w.(4) *. x) +. (w.(5) *. y) +. (w.(6) *. z) +. (if translate then w.(7) else 0.);
    (w.(8) *. x) +. (w.(9) *. y) +. (w.(10) *. z) +. (if translate then w.(11) else 0.) |] in
  let o = transform q.(0) q.(1) q.(2) true and d = transform (dx /. norm) (dy /. norm) (dz /. norm) false in
  let scale = Float.sqrt ((d.(0) *. d.(0)) +. (d.(1) *. d.(1)) +. (d.(2) *. d.(2))) in
  let h = trace [| o.(0); o.(1); o.(2); d.(0); d.(1); d.(2); q.(6) *. scale; q.(7) *. scale |] in
  if not h.H.hit then h
  else
    let n = [| w.(0) *. h.nx +. w.(4) *. h.ny +. w.(8) *. h.nz; w.(1) *. h.nx +. w.(5) *. h.ny +. w.(9) *. h.nz;
               w.(2) *. h.nx +. w.(6) *. h.ny +. w.(10) *. h.nz |] in
    let length = Float.sqrt ((n.(0) *. n.(0)) +. (n.(1) *. n.(1)) +. (n.(2) *. n.(2))) in
    { h with t = h.t /. scale; nx = n.(0) /. length; ny = n.(1) /. length; nz = n.(2) /. length; instance = k }

let with_spheres spheres q nearest =
  let nearest = ref nearest in
  let norm = Float.sqrt ((q.(3) *. q.(3)) +. (q.(4) *. q.(4)) +. (q.(5) *. q.(5))) in
  let dx = q.(3) /. norm and dy = q.(4) /. norm and dz = q.(5) /. norm in
  Array.iteri (fun k (cx, cy, cz, r) ->
    let ox = q.(0) -. cx and oy = q.(1) -. cy and oz = q.(2) -. cz in
    let b = (ox *. dx) +. (oy *. dy) +. (oz *. dz) in
    let disc = (b *. b) -. ((ox *. ox) +. (oy *. oy) +. (oz *. oz) -. (r *. r)) in
    if disc >= 0. then begin
      let root = Float.sqrt disc in
      let t = if -.b -. root >= q.(6) then -.b -. root else -.b +. root in
      if t >= q.(6) && t <= q.(7) && ((not !nearest.H.hit) || t < !nearest.H.t) then
        nearest := { H.t; prim = sphere_prim k; u = 0.; v = 0.; nx = (ox +. (t *. dx)) /. r;
                     ny = (oy +. (t *. dy)) /. r; nz = (oz +. (t *. dz)) /. r; hit = true; instance = -1 }
    end) spheres;
  !nearest

(* Strand segments (a, b, radius): linear round curves, each a capsule (the cylinder within the segment, the
   end spheres beyond it); a hit reports prim = segment, u = the curve parameter, instance = -2, and
   pathtrace.metal's normal (the offset from the axis point at u, less its tangential part). *)
type strand = float * float * float * float * float * float * float
let with_strands (strands : strand array) q nearest =
  if strands = [||] then nearest else
  let nearest = ref nearest in
  let norm = Float.sqrt ((q.(3) *. q.(3)) +. (q.(4) *. q.(4)) +. (q.(5) *. q.(5))) in
  let d = [| q.(3) /. norm; q.(4) /. norm; q.(5) /. norm |] and o = [| q.(0); q.(1); q.(2) |] in
  let dot a b = (a.(0) *. b.(0)) +. (a.(1) *. b.(1)) +. (a.(2) *. b.(2)) in
  Array.iteri (fun k (ax, ay, az, bx, by, bz, r) ->
    let a = [| ax; ay; az |] and b = [| bx; by; bz |] in
    let ba = Array.map2 ( -. ) b a and oa = Array.map2 ( -. ) o a in
    let baba = dot ba ba and bard = dot ba d and baoa = dot ba oa in
    let limit = if !nearest.H.hit then !nearest.H.t else q.(7) and best = ref infinity in
    let consider valid t = let y = baoa +. (t *. bard) in
      if valid y && t >= q.(6) && t <= limit && t < !best then best := t in
    let roots disc minus_b scale valid = if disc >= 0. then
      List.iter (fun s -> consider valid ((minus_b +. (s *. Float.sqrt disc)) /. scale)) [ -1.; 1. ] in
    let big_a = baba -. (bard *. bard) and big_b = (baba *. dot d oa) -. (baoa *. bard) in
    let big_c = (baba *. dot oa oa) -. (baoa *. baoa) -. (r *. r *. baba) in
    if big_a > 0. then roots ((big_b *. big_b) -. (big_a *. big_c)) (-.big_b) big_a (fun y -> y > 0. && y < baba);
    List.iter (fun (centre, valid) -> let oc = Array.map2 ( -. ) o centre in let half = dot oc d in
      roots ((half *. half) -. (dot oc oc -. (r *. r))) (-.half) 1. valid) [ (a, fun y -> y <= 0.); (b, fun y -> y >= baba) ];
    let t = !best in
    if t <= limit then begin
      let u = Float.min 1. (Float.max 0. ((baoa +. (t *. bard)) /. baba)) in
      let off = Array.init 3 (fun i -> o.(i) +. (t *. d.(i)) -. (a.(i) +. (u *. ba.(i)))) in
      let along = dot off ba /. Float.max baba 1e-8 in
      let off = Array.mapi (fun i x -> x -. (ba.(i) *. along)) off in
      let n = Float.sqrt (dot off off) in
      nearest := { H.t; prim = k; u; v = 0.; nx = off.(0) /. n; ny = off.(1) /. n; nz = off.(2) /. n;
                   hit = true; instance = -2 }
    end) strands;
  !nearest

let ( let* ) = Result.bind
let register ?(spheres = [||]) ?(strands = [||]) ?ending id geometry =
  let* trace = prototype ?ending geometry in
  H.register id (fun q -> with_spheres spheres q (with_strands strands q (trace q)));
  Ok ()

(* Instances of prototypes, each with its row-major 3x4 object-to-world transform; [instance] reports the
   index in the array, [prim] the primitive within its prototype. *)
let register_instances ?(spheres = [||]) ?(strands = [||]) ?motion id instances =
  let* traces = Array.fold_left (fun acc (geometry, transform) ->
    let* acc = acc in
    let* trace = prototype geometry in
    Ok ((trace, transform, H.world_to_object transform) :: acc)) (Ok []) instances in
  let traces = Array.of_list (List.rev traces) in
  if Option.fold ~none:false ~some:(fun ends -> Array.length ends <> Array.length traces) motion then
    Error "scene/trace: motion needs one end transform per instance"
  else begin
  H.register id (fun q ->
    let nearest = ref H.miss in
    Array.iteri (fun k (trace, transform, w) ->
      (* instance motion: the object-to-world transform interpolated at the query's time *)
      let w = match motion with None -> w | Some ends -> H.world_to_object (H.lerp transform ends.(k) (H.shutter q.(8))) in
      let h = instance_hit trace w k q in
      if h.H.hit && ((not !nearest.H.hit) || h.t < !nearest.H.t) then nearest := h) traces;
    with_spheres spheres q (with_strands strands q !nearest));
  Ok ()
  end
let unregister = H.unregister
