open Rays_math
open Plane_generators

type revolve_type = Revolve_closed | Revolve_open_arc

let run ?cancel ?(grain = 16_384) ?primitives
    ?(revolve_type = Revolve_closed) ?(connectivity = Grid_quads)
    ?(start_angle = 0.) ?(end_angle = 2. *. Float.pi)
    ?(reverse_cross_sections = false) ?(caps = false) ?cap_group
    ?(uv_attribute = Some "uv") ~divisions ~(origin : Rays_math.Vec3.t)
    ~(axis : Rays_math.Vec3.t) geometry =
  let operation = "Rdk_mesh.Revolve.revolve" in
  Cancel.check_opt cancel;
  let topology = Geometry.topology geometry in
  let source = Topology.Private.view topology
  and source_positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let source_vertices = Geometry.vertex_count geometry
  and source_primitives = Geometry.primitive_count geometry in
  let selected primitive = match primitives with
    | None -> true
    | Some group -> Group.mem primitive group in
  let polygon_surface = match connectivity with
    | Grid_quads | Grid_triangles | Grid_alternating_triangles
    | Grid_reverse_triangles -> true
    | Grid_points | Grid_rows | Grid_columns | Grid_rows_and_columns -> false in
  let selection_error = match primitives with
    | Some group when Group.owner group <> Group.Primitive ->
        Some "primitive selection has the wrong owner"
    | Some group when Group.length group <> source_primitives ->
        Some "primitive selection length does not match topology"
    | None | Some _ -> None in
  let uv_error = match uv_attribute with
    | Some name when String.trim name = "" || String.equal name "P"
        || String.equal name "N" ->
        Some "UV attribute name must be non-empty and cannot be P or N"
    | None | Some _ -> None in
  let cap_error = match cap_group with
    | Some name when String.trim name = "" -> Some "cap group name must not be empty"
    | Some _ when not caps -> Some "cap_group requires caps=true"
    | None | Some _ -> None in
  if grain <= 0 then Error (operation ^ ": grain must be positive")
  else if divisions < (match revolve_type with Revolve_closed -> 3
      | Revolve_open_arc -> 1) then
    Error (operation ^ ": divisions are too small for the revolve type")
  else if divisions > Sys.max_array_length -
      (match revolve_type with Revolve_closed -> 0 | Revolve_open_arc -> 1) then
    Error (operation ^ ": angular cardinality exceeds OCaml array limits")
  else if not (Float.is_finite origin.x && Float.is_finite origin.y && Float.is_finite origin.z
      && Float.is_finite axis.x && Float.is_finite axis.y && Float.is_finite axis.z
      && Float.is_finite start_angle && Float.is_finite end_angle) then
    Error (operation ^ ": origin, axis, and angles must be finite")
  else if revolve_type = Revolve_open_arc
      && (not (Float.is_finite (end_angle -. start_angle))
          || end_angle = start_angle) then
    Error (operation ^ ": open arc angles must have a finite non-zero span")
  else if caps && (revolve_type <> Revolve_closed || not polygon_surface) then
    Error (operation ^
      ": caps require a closed polygon-surface revolution")
  else match selection_error, uv_error, cap_error with
  | Some message, _, _ | _, Some message, _ | _, _, Some message ->
      Error (operation ^ ": " ^ message)
  | None, None, None ->
      let axis_scale = max (abs_float axis.x)
          (max (abs_float axis.y) (abs_float axis.z)) in
      if axis_scale = 0. || not (Float.is_finite axis_scale) then
        Error (operation ^ ": axis must be non-zero")
      else
        let sx = axis.x /. axis_scale and sy = axis.y /. axis_scale
        and sz = axis.z /. axis_scale in
        let axis_length = sqrt ((sx *. sx) +. (sy *. sy) +. (sz *. sz)) in
        if axis_length = 0. || not (Float.is_finite axis_length) then
          Error (operation ^ ": axis normalization failed")
        else
          let ax = sx /. axis_length and ay = sy /. axis_length
          and az = sz /. axis_length in
          let angular_samples = match revolve_type with
            | Revolve_closed -> divisions
            | Revolve_open_arc -> divisions + 1 in
          let angle_span = match revolve_type with
            | Revolve_closed -> 2. *. Float.pi
            | Revolve_open_arc -> end_angle -. start_angle in
          let angle_cos = Array.make angular_samples 0.
          and angle_sin = Array.make angular_samples 0. in
          for sample = 0 to angular_samples - 1 do
            let angle = start_angle +.
                (angle_span *. float_of_int sample /. float_of_int divisions) in
            angle_cos.(sample) <- cos angle;
            angle_sin.(sample) <- sin angle
          done;
          let selected_vertices = Bytes.make source_vertices '\000'
          and on_axis = Bytes.make source_vertices '\000'
          and profile_u = Array.make source_vertices 0.
          and ring_offsets = Array.make (source_vertices + 1) 0
          and input_errors = Array.make source_primitives 0 in
          let robust_distance a b =
            let dx = source_positions.x.(b) -. source_positions.x.(a)
            and dy = source_positions.y.(b) -. source_positions.y.(a)
            and dz = source_positions.z.(b) -. source_positions.z.(a) in
            let scale = max (abs_float dx) (max (abs_float dy) (abs_float dz)) in
            if scale = 0. then 0.
            else if not (Float.is_finite scale) then infinity
            else
              let x = dx /. scale and y = dy /. scale and z = dz /. scale in
              scale *. sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
          if source_primitives > 0 then Parallel.for_
              ~chunk_size:(max 1 (grain / 16)) ~start:0
              ~finish:(source_primitives - 1) (fun primitive ->
            if selected primitive then begin
              Cancel.check_opt cancel;
              if Topology.primitive_kind topology primitive = Topology.Polygon then
                input_errors.(primitive) <- 1
              else
                let first = source.primitive_offsets.(primitive)
                and last = source.primitive_offsets.(primitive + 1) in
                if last - first < 2 then input_errors.(primitive) <- 2
                else begin
                  let cumulative = ref 0. in
                  for local = 0 to last - first - 1 do
                    let vertex = first + local
                    and point = source.vertex_points.(first + local) in
                    Bytes.set selected_vertices vertex '\001';
                    let x = source_positions.x.(point)
                    and y = source_positions.y.(point)
                    and z = source_positions.z.(point) in
                    if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
                      input_errors.(primitive) <- 3
                    else begin
                      let rx = x -. origin.x and ry = y -. origin.y
                      and rz = z -. origin.z in
                      if not (Float.is_finite rx && Float.is_finite ry && Float.is_finite rz) then
                        input_errors.(primitive) <- 3
                      else begin
                        let along = (rx *. ax) +. (ry *. ay) +. (rz *. az) in
                        let qx = rx -. (along *. ax)
                        and qy = ry -. (along *. ay)
                        and qz = rz -. (along *. az) in
                        let radial_scale = max (abs_float qx)
                            (max (abs_float qy) (abs_float qz)) in
                        if radial_scale = 0. then Bytes.set on_axis vertex '\001'
                        else if not (Float.is_finite radial_scale) then
                          input_errors.(primitive) <- 3
                      end
                    end;
                    if local > 0 then begin
                      let previous = source.vertex_points.(vertex - 1) in
                      let length = robust_distance previous point in
                      if length = 0. || not (Float.is_finite length) then
                        input_errors.(primitive) <- 4
                      else cumulative := !cumulative +. length
                    end;
                    profile_u.(vertex) <- !cumulative
                  done;
                  let closed = Topology.primitive_kind topology primitive
                      = Topology.Closed_polyline in
                  let total = if closed then begin
                      let a = source.vertex_points.(last - 1)
                      and b = source.vertex_points.(first) in
                      let length = robust_distance a b in
                      if length = 0. || not (Float.is_finite length) then begin
                        input_errors.(primitive) <- 4; !cumulative
                      end else !cumulative +. length
                    end else !cumulative in
                  if total = 0. || not (Float.is_finite total) then
                    input_errors.(primitive) <- 5
                  else for vertex = first to last - 1 do
                    profile_u.(vertex) <- profile_u.(vertex) /. total
                  done
                end
            end);
          let failed = ref 0 in
          while !failed < source_primitives && input_errors.(!failed) = 0 do
            incr failed
          done;
          if !failed < source_primitives then
            Error (Printf.sprintf "%s: primitive %d %s" operation !failed
              (match input_errors.(!failed) with
               | 1 -> "is a polygon, not a polygon curve"
               | 2 -> "has fewer than two vertices"
               | 3 -> "contains a non-finite or unrepresentable position"
               | 4 -> "contains a zero-length or non-finite edge"
               | _ -> "has zero or non-finite length"))
          else begin
            let point_limit = Sys.max_array_length
            and primitive_limit = min (Sys.max_array_length - 1)
                Sys.max_string_length in
            let cardinality_error = ref None in
            for vertex = 0 to source_vertices - 1 do
              let count = if Bytes.get selected_vertices vertex = '\000' then 0
                else if Bytes.get on_axis vertex <> '\000' then 1
                else angular_samples in
              if ring_offsets.(vertex) > point_limit - count then
                cardinality_error := Some "point cardinality exceeds OCaml array limits"
              else ring_offsets.(vertex + 1) <- ring_offsets.(vertex) + count
            done;
            let output_points = ring_offsets.(source_vertices) in
            let source_output_counts = Array.make source_primitives 0
            and primitive_bases = Array.make (source_primitives + 1) 0 in
            let checked_add primitive value =
              if source_output_counts.(primitive) > primitive_limit - value
              then cardinality_error := Some
                  "primitive cardinality exceeds OCaml array limits"
              else source_output_counts.(primitive) <-
                  source_output_counts.(primitive) + value in
            let checked_cells primitive cells multiplier =
              if cells <> 0 && multiplier > primitive_limit / cells then
                cardinality_error := Some
                  "primitive cardinality exceeds OCaml array limits"
              else checked_add primitive (cells * multiplier) in
            for primitive = 0 to source_primitives - 1 do
              if selected primitive then begin
                let first = source.primitive_offsets.(primitive)
                and last = source.primitive_offsets.(primitive + 1) in
                let size = last - first in
                (match connectivity with
                 | Grid_rows | Grid_rows_and_columns ->
                     let rows = ref 0 in
                     for vertex = first to last - 1 do
                       if Bytes.get on_axis vertex = '\000' then incr rows
                     done;
                     checked_add primitive !rows
                 | Grid_points | Grid_columns | Grid_quads | Grid_triangles
                 | Grid_alternating_triangles | Grid_reverse_triangles -> ());
                (match connectivity with
                 | Grid_columns | Grid_rows_and_columns ->
                     checked_add primitive angular_samples
                 | Grid_points | Grid_rows | Grid_quads | Grid_triangles
                 | Grid_alternating_triangles | Grid_reverse_triangles -> ());
                if polygon_surface then begin
                  let edges = size - 1 +
                      if Topology.primitive_kind topology primitive
                         = Topology.Closed_polyline then 1 else 0 in
                  for local = 0 to edges - 1 do
                    let current = first + local
                    and next = first + ((local + 1) mod size) in
                    let current_axis = Bytes.get on_axis current <> '\000'
                    and next_axis = Bytes.get on_axis next <> '\000' in
                    if not (current_axis && next_axis) then
                      checked_cells primitive divisions
                        (if current_axis || next_axis || connectivity = Grid_quads
                         then 1 else 2)
                  done;
                  if caps && Topology.primitive_kind topology primitive
                      = Topology.Open_polyline then begin
                    if Bytes.get on_axis first = '\000' then checked_add primitive 1;
                    if Bytes.get on_axis (last - 1) = '\000' then
                      checked_add primitive 1
                  end
                end
              end
            done;
            for primitive = 0 to source_primitives - 1 do
              if primitive_bases.(primitive) > primitive_limit
                  - source_output_counts.(primitive) then
                cardinality_error := Some
                    "primitive cardinality exceeds OCaml array limits"
              else primitive_bases.(primitive + 1) <- primitive_bases.(primitive)
                  + source_output_counts.(primitive)
            done;
            match !cardinality_error with
            | Some message -> Error (operation ^ ": " ^ message)
            | None ->
                let output_primitives = primitive_bases.(source_primitives) in
                let primitive_sizes = Array.make output_primitives 0
                and primitive_map = Array.make output_primitives 0
                and primitive_kinds = Bytes.make output_primitives '\000'
                and cap_flags = Bytes.make output_primitives '\000'
                and surface_edge_bases = Array.make source_vertices (-1)
                and surface_edge_next = Array.make source_vertices (-1)
                and surface_edge_local = Array.make source_vertices 0 in
                let source_vertex primitive logical =
                  let first = source.primitive_offsets.(primitive)
                  and last = source.primitive_offsets.(primitive + 1) in
                  if reverse_cross_sections then last - 1 - logical
                  else first + logical in
                if source_primitives > 0 then Parallel.for_
                    ~chunk_size:(max 1 (grain / 32)) ~start:0
                    ~finish:(source_primitives - 1) (fun primitive ->
                  if selected primitive then begin
                    Cancel.check_opt cancel;
                    let first = source.primitive_offsets.(primitive)
                    and last = source.primitive_offsets.(primitive + 1) in
                    let size = last - first
                    and output = ref primitive_bases.(primitive) in
                    let emit size kind cap =
                      let at = !output in
                      primitive_sizes.(at) <- size;
                      primitive_map.(at) <- primitive;
                      Bytes.set primitive_kinds at kind;
                      if cap then Bytes.set cap_flags at '\001';
                      incr output in
                    (match connectivity with
                     | Grid_rows | Grid_rows_and_columns ->
                         for logical = 0 to size - 1 do
                           let vertex = source_vertex primitive logical in
                           if Bytes.get on_axis vertex = '\000' then emit
                               angular_samples
                               (if revolve_type = Revolve_closed
                                then '\002' else '\001') false
                         done
                     | Grid_points | Grid_columns | Grid_quads | Grid_triangles
                     | Grid_alternating_triangles | Grid_reverse_triangles -> ());
                    (match connectivity with
                     | Grid_columns | Grid_rows_and_columns ->
                         for _sample = 0 to angular_samples - 1 do
                           emit size
                             (if Topology.primitive_kind topology primitive
                                 = Topology.Closed_polyline then '\002' else '\001')
                             false
                         done
                     | Grid_points | Grid_rows | Grid_quads | Grid_triangles
                     | Grid_alternating_triangles | Grid_reverse_triangles -> ());
                    if polygon_surface then begin
                      let edges = size - 1 +
                          if Topology.primitive_kind topology primitive
                              = Topology.Closed_polyline then 1 else 0 in
                      for local = 0 to edges - 1 do
                        let current = source_vertex primitive local
                        and next = source_vertex primitive ((local + 1) mod size) in
                        let current_axis = Bytes.get on_axis current <> '\000'
                        and next_axis = Bytes.get on_axis next <> '\000' in
                        if not (current_axis && next_axis) then begin
                          surface_edge_bases.(current) <- !output;
                          surface_edge_next.(current) <- next;
                          surface_edge_local.(current) <- local;
                          for _side = 0 to divisions - 1 do
                            if current_axis || next_axis
                                || connectivity = Grid_quads then emit
                                (if current_axis || next_axis then 3 else 4)
                                '\000' false
                            else begin emit 3 '\000' false; emit 3 '\000' false end
                          done
                        end
                      done;
                      if caps && Topology.primitive_kind topology primitive
                          = Topology.Open_polyline then begin
                        let start = source_vertex primitive 0
                        and finish = source_vertex primitive (size - 1) in
                        if Bytes.get on_axis start = '\000' then
                          emit divisions '\000' true;
                        if Bytes.get on_axis finish = '\000' then
                          emit divisions '\000' true
                      end
                    end;
                    assert (!output = primitive_bases.(primitive + 1))
                  end);
                let primitive_offsets = Array.make (output_primitives + 1) 0 in
                for primitive = 0 to output_primitives - 1 do
                  let size = primitive_sizes.(primitive) in
                  if primitive_offsets.(primitive) > point_limit - size then
                    cardinality_error := Some
                      "vertex cardinality exceeds OCaml array limits"
                  else primitive_offsets.(primitive + 1) <-
                      primitive_offsets.(primitive) + size
                done;
                (match !cardinality_error with
                 | Some message -> Error (operation ^ ": " ^ message)
                 | None ->
                  let output_vertices = primitive_offsets.(output_primitives) in
                  let px = Array.make output_points 0.
                  and py = Array.make output_points 0.
                  and pz = Array.make output_points 0.
                  and point_map = Array.make output_points 0
                  and vertex_points = Array.make output_vertices 0
                  and vertex_map = Array.make output_vertices 0
                  and invalid_positions = Bytes.make source_vertices '\000' in
                  let point_uv_x, point_uv_y = match uv_attribute, connectivity with
                    | Some _, Grid_points -> Some (Array.make output_points 0.),
                        Some (Array.make output_points 0.)
                    | _ -> None, None in
                  let vertex_uv_x, vertex_uv_y = match uv_attribute, connectivity with
                    | Some _, (Grid_rows | Grid_columns | Grid_rows_and_columns
                        | Grid_quads | Grid_triangles
                        | Grid_alternating_triangles | Grid_reverse_triangles) ->
                        Some (Array.make output_vertices 0.),
                        Some (Array.make output_vertices 0.)
                    | _ -> None, None in
                  if source_vertices > 0 then Parallel.for_ ~chunk_size:grain
                      ~start:0 ~finish:(source_vertices - 1) (fun vertex ->
                    let first = ring_offsets.(vertex)
                    and last = ring_offsets.(vertex + 1) in
                    if first < last then begin
                      if vertex land 4095 = 0 then Cancel.check_opt cancel;
                      let source_point = source.vertex_points.(vertex) in
                      let x = source_positions.x.(source_point)
                      and y = source_positions.y.(source_point)
                      and z = source_positions.z.(source_point) in
                      let rx = x -. origin.x and ry = y -. origin.y
                      and rz = z -. origin.z in
                      let along = (rx *. ax) +. (ry *. ay) +. (rz *. az) in
                      let qx = rx -. (along *. ax)
                      and qy = ry -. (along *. ay)
                      and qz = rz -. (along *. az) in
                      let base_x = origin.x +. (along *. ax)
                      and base_y = origin.y +. (along *. ay)
                      and base_z = origin.z +. (along *. az) in
                      let cross_x = (ay *. qz) -. (az *. qy)
                      and cross_y = (az *. qx) -. (ax *. qz)
                      and cross_z = (ax *. qy) -. (ay *. qx) in
                      for local = 0 to last - first - 1 do
                        let output = first + local
                        and cosine = if last - first = 1 then 1.
                          else angle_cos.(local)
                        and sine = if last - first = 1 then 0.
                          else angle_sin.(local) in
                        let ox = base_x +. (qx *. cosine) +. (cross_x *. sine)
                        and oy = base_y +. (qy *. cosine) +. (cross_y *. sine)
                        and oz = base_z +. (qz *. cosine) +. (cross_z *. sine) in
                        if Float.is_finite ox && Float.is_finite oy && Float.is_finite oz then begin
                          px.(output) <- ox; py.(output) <- oy; pz.(output) <- oz
                        end else Bytes.set invalid_positions vertex '\001';
                        point_map.(output) <- source_point;
                        (match point_uv_x, point_uv_y with
                         | Some ux, Some uy ->
                             ux.(output) <- if reverse_cross_sections
                               then 1. -. profile_u.(vertex)
                               else profile_u.(vertex);
                             uy.(output) <- if last - first = 1 then 0.
                               else float_of_int local /. float_of_int divisions
                         | None, None -> () | _ -> assert false)
                      done
                    end);
                  let invalid = ref 0 in
                  while !invalid < source_vertices
                      && Bytes.get invalid_positions !invalid = '\000' do
                    incr invalid
                  done;
                  if !invalid < source_vertices then Error (Printf.sprintf
                      "%s: generated ring for source vertex %d is not finite"
                      operation !invalid)
                  else begin
                    let ring_point vertex sample =
                      let count = ring_offsets.(vertex + 1) - ring_offsets.(vertex) in
                      if count = 1 then ring_offsets.(vertex)
                      else ring_offsets.(vertex) + sample in
                    let uv_u vertex = if reverse_cross_sections
                      then 1. -. profile_u.(vertex) else profile_u.(vertex) in
                    let set_corner corner point vertex u v =
                      vertex_points.(corner) <- point;
                      vertex_map.(corner) <- vertex;
                      match vertex_uv_x, vertex_uv_y with
                      | Some ux, Some uy -> ux.(corner) <- u; uy.(corner) <- v
                      | None, None -> () | _ -> assert false in
                    if polygon_surface && source_vertices > 0 then
                      Parallel.for_ ~chunk_size:(max 1 (grain / divisions))
                        ~start:0 ~finish:(source_vertices - 1) (fun current ->
                      let base = surface_edge_bases.(current) in
                      if base >= 0 then begin
                        if current land 4095 = 0 then Cancel.check_opt cancel;
                        let next = surface_edge_next.(current)
                        and local = surface_edge_local.(current)
                        and current_axis = Bytes.get on_axis current <> '\000'
                        and next_axis = Bytes.get on_axis
                            surface_edge_next.(current) <> '\000'
                        and output = ref base in
                        let finish () = incr output in
                        for side = 0 to divisions - 1 do
                          let next_side = if revolve_type = Revolve_closed
                            then (side + 1) mod angular_samples else side + 1 in
                          let p0 = ring_point current side
                          and p1 = ring_point current next_side
                          and p2 = ring_point next next_side
                          and p3 = ring_point next side
                          and u0 = uv_u current and u1 = uv_u next
                          and v0 = float_of_int side /. float_of_int divisions
                          and v1 = float_of_int (side + 1)
                              /. float_of_int divisions in
                          if current_axis then begin
                            let at = primitive_offsets.(!output) in
                            set_corner at p0 current u0 v0;
                            set_corner (at + 1) p2 next u1 v1;
                            set_corner (at + 2) p3 next u1 v0;
                            finish ()
                          end else if next_axis then begin
                            let at = primitive_offsets.(!output) in
                            set_corner at p0 current u0 v0;
                            set_corner (at + 1) p1 current u0 v1;
                            set_corner (at + 2) p2 next u1 v1;
                            finish ()
                          end else if connectivity = Grid_quads then begin
                            let at = primitive_offsets.(!output) in
                            set_corner at p0 current u0 v0;
                            set_corner (at + 1) p1 current u0 v1;
                            set_corner (at + 2) p2 next u1 v1;
                            set_corner (at + 3) p3 next u1 v0;
                            finish ()
                          end else begin
                            let reverse = connectivity = Grid_reverse_triangles
                              || connectivity = Grid_alternating_triangles
                                && ((local + side) land 1 = 1) in
                            let at = primitive_offsets.(!output) in
                            if reverse then begin
                              set_corner at p0 current u0 v0;
                              set_corner (at + 1) p1 current u0 v1;
                              set_corner (at + 2) p3 next u1 v0;
                              finish ();
                              let at = primitive_offsets.(!output) in
                              set_corner at p1 current u0 v1;
                              set_corner (at + 1) p2 next u1 v1;
                              set_corner (at + 2) p3 next u1 v0;
                              finish ()
                            end else begin
                              set_corner at p0 current u0 v0;
                              set_corner (at + 1) p1 current u0 v1;
                              set_corner (at + 2) p2 next u1 v1;
                              finish ();
                              let at = primitive_offsets.(!output) in
                              set_corner at p0 current u0 v0;
                              set_corner (at + 1) p2 next u1 v1;
                              set_corner (at + 2) p3 next u1 v0;
                              finish ()
                            end
                          end
                        done
                      end);
                    if source_primitives > 0 && output_primitives > 0 then
                      Parallel.for_ ~chunk_size:(max 1 (grain / 64)) ~start:0
                        ~finish:(source_primitives - 1) (fun primitive ->
                      if selected primitive then begin
                        Cancel.check_opt cancel;
                        let first = source.primitive_offsets.(primitive)
                        and last = source.primitive_offsets.(primitive + 1) in
                        let size = last - first
                        and output = ref primitive_bases.(primitive) in
                        let finish () = incr output in
                        (match connectivity with
                         | Grid_rows | Grid_rows_and_columns ->
                             for logical = 0 to size - 1 do
                               let vertex = source_vertex primitive logical in
                               if Bytes.get on_axis vertex = '\000' then begin
                                 let at = primitive_offsets.(!output)
                                 and u = uv_u vertex in
                                 for sample = 0 to angular_samples - 1 do
                                   set_corner (at + sample) (ring_point vertex sample)
                                     vertex u
                                     (float_of_int sample /. float_of_int divisions)
                                 done;
                                 finish ()
                               end
                             done
                         | Grid_points | Grid_columns | Grid_quads | Grid_triangles
                         | Grid_alternating_triangles | Grid_reverse_triangles -> ());
                        (match connectivity with
                         | Grid_columns | Grid_rows_and_columns ->
                             for sample = 0 to angular_samples - 1 do
                               let at = primitive_offsets.(!output)
                               and v = float_of_int sample /. float_of_int divisions in
                               for logical = 0 to size - 1 do
                                 let vertex = source_vertex primitive logical in
                                 set_corner (at + logical) (ring_point vertex sample)
                                   vertex (uv_u vertex) v
                               done;
                               finish ()
                             done
                         | Grid_points | Grid_rows | Grid_quads | Grid_triangles
                         | Grid_alternating_triangles | Grid_reverse_triangles -> ());
                        if polygon_surface then begin
                          let edges = size - 1 +
                              if Topology.primitive_kind topology primitive
                                  = Topology.Closed_polyline then 1 else 0 in
                          for local = 0 to edges - 1 do
                            let current = source_vertex primitive local in
                            let base = surface_edge_bases.(current) in
                            if base >= 0 then begin
                              let next = surface_edge_next.(current) in
                              let multiplier = if Bytes.get on_axis current <> '\000'
                                  || Bytes.get on_axis next <> '\000'
                                  || connectivity = Grid_quads then 1 else 2 in
                              output := !output + (divisions * multiplier)
                            end
                          done;
                          if caps && Topology.primitive_kind topology primitive
                              = Topology.Open_polyline then begin
                            let start = source_vertex primitive 0
                            and finish_vertex = source_vertex primitive (size - 1) in
                            if Bytes.get on_axis start = '\000' then begin
                              let at = primitive_offsets.(!output) in
                              for local = 0 to divisions - 1 do
                                let sample = local in
                                set_corner (at + local) (ring_point start sample)
                                  start (0.5 +. (0.5 *. angle_cos.(sample)))
                                  (0.5 +. (0.5 *. angle_sin.(sample)))
                              done;
                              finish ()
                            end;
                            if Bytes.get on_axis finish_vertex = '\000' then begin
                              let at = primitive_offsets.(!output) in
                              for local = 0 to divisions - 1 do
                                let sample = divisions - 1 - local in
                                set_corner (at + local)
                                  (ring_point finish_vertex sample) finish_vertex
                                  (0.5 +. (0.5 *. angle_cos.(sample)))
                                  (0.5 +. (0.5 *. angle_sin.(sample)))
                              done;
                              finish ()
                            end
                          end
                        end;
                        assert (!output = primitive_bases.(primitive + 1))
                      end);
                    let output_positions = Packed.Float3.Private.of_owned_exn
                        ~x:px ~y:py ~z:pz
                    and output_topology = Topology.Private.create_validated_owned
                        ~point_count:output_points ~vertex_points
                        ~primitive_offsets ~primitive_kinds in
                    let uv_owner = if connectivity = Grid_points
                      then Attribute.Point else Attribute.Vertex in
                    let rec remap_attributes result = function
                      | [] -> Ok (List.rev result)
                      | attribute :: rest ->
                          let generated_conflict = match uv_attribute with
                            | Some name -> Attribute.owner attribute = uv_owner
                                && String.equal (Attribute.name attribute) name
                            | None -> false in
                          if generated_conflict then remap_attributes result rest
                          else Result.bind
                              (Topology_remap.mapped_attribute ~grain:16_384
                                ~point_map ~vertex_map ~primitive_map
                                ~drop:Topology_remap.drop_point_vertex_normals
                                attribute)
                              (function
                                | None -> remap_attributes result rest
                                | Some value -> remap_attributes
                                    (value :: result) rest) in
                    Result.bind
                      (remap_attributes [] (Geometry.attributes geometry))
                      (fun attributes ->
                      let attributes = match uv_attribute, point_uv_x, point_uv_y,
                          vertex_uv_x, vertex_uv_y with
                        | None, _, _, _, _ -> attributes
                        | Some name, Some x, Some y, None, None ->
                            (Attribute.create_owned ~name ~owner:Attribute.Point
                              (Attribute.Float2
                                (Packed.Float2.of_owned ~x ~y |> Support.get_ok))
                              |> Support.get_ok) :: attributes
                        | Some name, None, None, Some x, Some y ->
                            (Attribute.create_owned ~name ~owner:Attribute.Vertex
                              (Attribute.Float2
                                (Packed.Float2.of_owned ~x ~y |> Support.get_ok))
                              |> Support.get_ok) :: attributes
                        | _ -> assert false in
                      let groups = List.map
                          (Topology_remap.mapped_group ~grain:16_384 ~point_map ~vertex_map
                            ~primitive_map)
                          (Geometry.groups geometry) in
                      let edge_groups = match Geometry.edge_groups geometry with
                        | [] -> []
                        | source_groups ->
                            let source_index = Topology_index.create ?cancel topology
                            and target_index = Topology_index.create ?cancel
                                output_topology in
                            let target_view = Topology_index.Private.view
                                target_index in
                            List.map (fun source_group ->
                              Edge_group.init ~grain ~topology:output_topology
                                ~index:target_index
                                ~name:(Edge_group.name source_group)
                                (fun target_edge ->
                                  let target_a = target_view.edge_a.(target_edge)
                                  and target_b = target_view.edge_b.(target_edge) in
                                  let source_a = point_map.(target_a)
                                  and source_b = point_map.(target_b) in
                                  source_a <> source_b
                                  && match Topology_index.find_edge source_index
                                      ~a:source_a ~b:source_b with
                                     | Some source_edge ->
                                         Edge_group.mem source_edge source_group
                                     | None -> false)) source_groups in
                      Result.bind (Geometry.create ~positions:output_positions
                          ~topology:output_topology ~attributes ~groups
                          ~edge_groups ()) (fun output_geometry ->
                        match cap_group with
                        | None -> Ok output_geometry
                        | Some name -> Geometry.with_group
                            (Group.init ~grain ~owner:Group.Primitive ~name
                              output_primitives (fun primitive ->
                                Bytes.get cap_flags primitive <> '\000'))
                            output_geometry))
                  end)
          end
