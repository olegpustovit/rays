module S = Rdk.Surface_index
let sphere_prim k = -2 - k
let register ?(spheres = [||]) id geometry =
  match S.create geometry with
  | Error e -> Error (Rdk.Error.to_string e)
  | Ok index ->
      let positions = Rdk.Geometry.positions geometry in
      let triangles q =
        let direction = Rdk.Surface_index.Private.Constant_direction { x = q.(3); y = q.(4); z = q.(5) } in
        let origin = Rdk.Packed.Float3.Private.of_owned_exn ~x:[| q.(0) |] ~y:[| q.(1) |] ~z:[| q.(2) |] in
        let primitives = [| -1 |] and triangles = [| -1 |] and a = [| 0. |] and b = [| 0. |] and c = [| 0. |]
        and distances = [| infinity |] in
        S.Private.raycast_many_into ~grain:1 index ~queries:origin ~directions:direction
          ~min_distance:q.(6) ~max_distance:q.(7) ~tolerance:0. ~direction_mode:S.Ray_forward
          ~surface_hit:S.Ray_first_surface ~primitives ~triangles ~barycentric_a:a ~barycentric_b:b
          ~barycentric_c:c ~distances;
        if primitives.(0) < 0 then Flow.Ray_host.miss
        else
          let point corner = Rdk.Packed.Float3.get positions (S.Private.triangle_point index triangles.(0) corner) in
          let x0, y0, z0 = point 0 and x1, y1, z1 = point 1 and x2, y2, z2 = point 2 in
          let ux = x1 -. x0 and uy = y1 -. y0 and uz = z1 -. z0 and vx = x2 -. x0 and vy = y2 -. y0 and vz = z2 -. z0 in
          let nx = uy *. vz -. uz *. vy and ny = uz *. vx -. ux *. vz and nz = ux *. vy -. uy *. vx in
          let n = Float.sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
          { Flow.Ray_host.t = distances.(0); prim = primitives.(0); u = b.(0); v = c.(0);
            nx = nx /. n; ny = ny /. n; nz = nz /. n; hit = true } in
      let query q =
        let nearest = ref (triangles q) in
        let norm = Float.sqrt ((q.(3) *. q.(3)) +. (q.(4) *. q.(4)) +. (q.(5) *. q.(5))) in
        let dx = q.(3) /. norm and dy = q.(4) /. norm and dz = q.(5) /. norm in
        Array.iteri (fun k (cx, cy, cz, r) ->
          let ox = q.(0) -. cx and oy = q.(1) -. cy and oz = q.(2) -. cz in
          let b = (ox *. dx) +. (oy *. dy) +. (oz *. dz) in
          let disc = (b *. b) -. ((ox *. ox) +. (oy *. oy) +. (oz *. oz) -. (r *. r)) in
          if disc >= 0. then begin
            let root = Float.sqrt disc in
            let t = if -.b -. root >= q.(6) then -.b -. root else -.b +. root in
            if t >= q.(6) && t <= q.(7) && ((not !nearest.Flow.Ray_host.hit) || t < !nearest.Flow.Ray_host.t) then begin
              let px = ox +. (t *. dx) and py = oy +. (t *. dy) and pz = oz +. (t *. dz) in
              nearest := { Flow.Ray_host.t; prim = sphere_prim k; u = 0.; v = 0.; nx = px /. r; ny = py /. r; nz = pz /. r; hit = true }
            end
          end) spheres;
        !nearest in
      Flow.Ray_host.register id query;
      Ok ()
let unregister = Flow.Ray_host.unregister
