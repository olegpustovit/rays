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
        let direction = S.Private.Constant_direction { x = q.(3); y = q.(4); z = q.(5) } in
        let origin = Rdk.Packed.Float3.Private.of_owned_exn ~x:[| q.(0) |] ~y:[| q.(1) |] ~z:[| q.(2) |] in
        let primitives = [| -1 |] and triangles = [| -1 |] and a = [| 0. |] and b = [| 0. |] and c = [| 0. |]
        and distances = [| infinity |] in
        S.Private.raycast_many_into ~grain:1 index ~queries:origin ~directions:direction
          ~min_distance:q.(6) ~max_distance:q.(7) ~tolerance:0. ~direction_mode:S.Ray_forward
          ~surface_hit:S.Ray_first_surface ~primitives ~triangles ~barycentric_a:a ~barycentric_b:b
          ~barycentric_c:c ~distances;
        if primitives.(0) < 0 then H.miss
        else
          let point corner = Rdk.Packed.Float3.get positions (S.Private.triangle_point index triangles.(0) corner) in
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

let ( let* ) = Result.bind
let register ?(spheres = [||]) ?ending id geometry =
  let* trace = prototype ?ending geometry in
  H.register id (fun q -> with_spheres spheres q (trace q));
  Ok ()

(* Instances of prototypes, each with its row-major 3x4 object-to-world transform; [instance] reports the
   index in the array, [prim] the primitive within its prototype. *)
let register_instances ?(spheres = [||]) ?motion id instances =
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
    with_spheres spheres q !nearest);
  Ok ()
  end
let unregister = H.unregister
