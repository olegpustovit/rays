

let run ?cancel ?(grain = 16_384) ?primitives geometry =
  Error.guard ~operation:"triangulate" ~code:"invalid_topology" @@ fun () ->
  try
  if grain <= 0 then invalid_arg "Rdk_mesh.Triangulate.triangulate: grain must be positive";
  Cancel.check_opt cancel;
  let topology = Geometry.topology geometry in
  let primitive_count = Topology.primitive_count topology in
  (match primitives with
   | Some group when Group.owner group <> Group.Primitive ->
       invalid_arg "Rdk_mesh.Triangulate.triangulate: selection must own primitives"
   | Some group when Group.length group <> primitive_count ->
       invalid_arg
         "Rdk_mesh.Triangulate.triangulate: selection length does not match primitive count"
   | None | Some _ -> ());
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let topology_view = Topology.View.view topology in
  let selected primitive = match primitives with
    | None -> true
    | Some group -> Group.mem primitive group in
  let full_selection = match primitives with
    | None -> true
    | Some group -> Group.cardinality group = primitive_count in
  let collapsed_quad_removed primitive =
    let first = topology_view.primitive_offsets.(primitive)
    and last = topology_view.primitive_offsets.(primitive + 1) in
    if last - first <> 4 then -1
    else
      let removed = ref (-1) and collapsed = ref 0 in
      for local = 0 to 3 do
        let next = (local + 1) land 3 in
        let a = topology_view.vertex_points.(first + local)
        and b = topology_view.vertex_points.(first + next) in
        if positions.x.(a) = positions.x.(b)
           && positions.y.(a) = positions.y.(b)
           && positions.z.(a) = positions.z.(b) then begin
          incr collapsed;
          removed := next
        end
      done;
      if !collapsed <> 1 then -1
      else
        let remaining ordinal = if ordinal < !removed then ordinal
          else ordinal + 1 in
        let a = topology_view.vertex_points.(first + remaining 0)
        and b = topology_view.vertex_points.(first + remaining 1)
        and c = topology_view.vertex_points.(first + remaining 2) in
        let ux = positions.x.(b) -. positions.x.(a)
        and uy = positions.y.(b) -. positions.y.(a)
        and uz = positions.z.(b) -. positions.z.(a)
        and vx = positions.x.(c) -. positions.x.(a)
        and vy = positions.y.(c) -. positions.y.(a)
        and vz = positions.z.(c) -. positions.z.(a) in
        let scale = Float.max (abs_float ux) (abs_float uy) in
        let scale = Float.max scale (abs_float uz) in
        let scale = Float.max scale (abs_float vx) in
        let scale = Float.max scale (abs_float vy) in
        let scale = Float.max scale (abs_float vz) in
        if scale = 0. || not (Float.is_finite scale) then -1
        else
          let ux = ux /. scale and uy = uy /. scale and uz = uz /. scale
          and vx = vx /. scale and vy = vy /. scale and vz = vz /. scale in
          let nx = (uy *. vz) -. (uz *. vy)
          and ny = (uz *. vx) -. (ux *. vz)
          and nz = (ux *. vy) -. (uy *. vx) in
          if nx = 0. && ny = 0. && nz = 0. then -1 else !removed in
  let record_min target value =
    let rec loop current =
      if value >= current then ()
      else if not (Atomic.compare_and_set target current value) then
        loop (Atomic.get target) in
    loop (Atomic.get target) in
  let collapsed_removals = Bytes.make primitive_count '\000'
  and primitive_bases = Array.make (primitive_count + 1) 1
  and vertex_bases = if full_selection then [||]
      else Array.make (primitive_count + 1) 0
  and first_selected_curve = Atomic.make max_int
  and changes = Atomic.make false in
  primitive_bases.(0) <- 0;
  let average_size = if primitive_count = 0 then 1 else
      max 1 (Topology.vertex_count topology / primitive_count) in
  let primitive_grain = max 1 (grain / average_size) in
  Parallel.for_ ~chunk_size:primitive_grain ~start:0
    ~finish:(primitive_count - 1) (fun primitive ->
      if primitive land 1023 = 0 then Cancel.check_opt cancel;
      let size = topology_view.primitive_offsets.(primitive + 1)
          - topology_view.primitive_offsets.(primitive) in
      if not (selected primitive) then vertex_bases.(primitive + 1) <- size
      else if Bytes.get topology_view.primitive_kinds primitive <> '\000' then begin
        if not full_selection then vertex_bases.(primitive + 1) <- size;
        record_min first_selected_curve primitive
      end else if size = 3 then begin
        if not full_selection then vertex_bases.(primitive + 1) <- 3
      end
      else begin
        Atomic.set changes true;
        let removed = collapsed_quad_removed primitive in
        Bytes.set collapsed_removals primitive (Char.chr (removed + 1));
        let triangles = if removed >= 0 then 1 else size - 2 in
        primitive_bases.(primitive + 1) <- triangles;
        if not full_selection then vertex_bases.(primitive + 1) <- triangles * 3
      end);
  let curve = Atomic.get first_selected_curve in
  if curve <> max_int then Error (Printf.sprintf
      "Rdk_mesh.Triangulate.triangulate: primitive %d is a curve, not a polygon" curve)
  else if not (Atomic.get changes) then Ok geometry
  else begin
    let cardinality_error = ref None in
    for primitive = 0 to primitive_count - 1 do
      if primitive land 4095 = 0 then Cancel.check_opt cancel;
      let next_primitives = primitive_bases.(primitive + 1) in
      let next_vertices = if full_selection then next_primitives * 3
        else vertex_bases.(primitive + 1) in
      if primitive_bases.(primitive) > Sys.max_array_length - next_primitives
         || (not full_selection
             && vertex_bases.(primitive) > Sys.max_array_length - next_vertices)
         || (full_selection && primitive_bases.(primitive)
             > (Sys.max_array_length / 3) - next_primitives) then
        cardinality_error := Some
          "Rdk_mesh.Triangulate.triangulate: output cardinality exceeds array limits"
      else begin
        primitive_bases.(primitive + 1) <-
          primitive_bases.(primitive) + next_primitives;
        if not full_selection then vertex_bases.(primitive + 1) <-
          vertex_bases.(primitive) + next_vertices
      end
    done;
    let output_primitives = primitive_bases.(primitive_count)
    and output_vertex_count = if full_selection then
        primitive_bases.(primitive_count) * 3
      else vertex_bases.(primitive_count) in
    if output_primitives >= Sys.max_array_length then cardinality_error := Some
        "Rdk_mesh.Triangulate.triangulate: output cardinality exceeds array limits";
    match !cardinality_error with
    | Some message -> Error message
    | None ->
      let output_vertices = Array.make output_vertex_count 0
      and vertex_map = Array.make output_vertex_count 0
      and output_offsets = Array.make (output_primitives + 1) 0
      and output_kinds = Bytes.make output_primitives '\000'
      and primitive_map = Array.make output_primitives 0
      and first_failure = Atomic.make max_int in
      let block_count = if primitive_count = 0 then 0
        else (primitive_count + primitive_grain - 1) / primitive_grain in
      Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(block_count - 1) (fun block ->
        let scratch = Polygon_triangulation.create_scratch () in
        let current_primitive = ref 0
        and current_output_primitive = ref 0
        and current_output_vertex = ref 0 in
        let emit triangle old_a old_b old_c =
          let vertex = !current_output_vertex + (triangle * 3)
          and target_primitive = !current_output_primitive + triangle in
          output_vertices.(vertex) <- topology_view.vertex_points.(old_a);
          output_vertices.(vertex + 1) <- topology_view.vertex_points.(old_b);
          output_vertices.(vertex + 2) <- topology_view.vertex_points.(old_c);
          vertex_map.(vertex) <- old_a;
          vertex_map.(vertex + 1) <- old_b;
          vertex_map.(vertex + 2) <- old_c;
          output_offsets.(target_primitive) <- vertex;
          primitive_map.(target_primitive) <- !current_primitive
        in
        let first_primitive = block * primitive_grain
        and last_primitive = min (primitive_count - 1)
            (((block + 1) * primitive_grain) - 1) in
        for primitive = first_primitive to last_primitive do
          if primitive land 1023 = 0 then Cancel.check_opt cancel;
          let source_first = topology_view.primitive_offsets.(primitive)
          and source_last = topology_view.primitive_offsets.(primitive + 1)
          and output_primitive = primitive_bases.(primitive)
          and output_vertex = if full_selection then primitive_bases.(primitive) * 3
            else vertex_bases.(primitive) in
          let source_size = source_last - source_first in
          current_primitive := primitive;
          current_output_primitive := output_primitive;
          current_output_vertex := output_vertex;
          if not (selected primitive) || source_size = 3 then begin
            output_offsets.(output_primitive) <- output_vertex;
            Bytes.set output_kinds output_primitive
              (Bytes.get topology_view.primitive_kinds primitive);
            primitive_map.(output_primitive) <- primitive;
            Array.blit topology_view.vertex_points source_first output_vertices
              output_vertex source_size;
            for local = 0 to source_size - 1 do
              vertex_map.(output_vertex + local) <- source_first + local
            done
          end else if Bytes.get collapsed_removals primitive <> '\000' then begin
            let removed = Char.code (Bytes.get collapsed_removals primitive) - 1 in
            let remaining ordinal = if ordinal < removed then ordinal
              else ordinal + 1 in
            emit 0 (source_first + remaining 0) (source_first + remaining 1)
              (source_first + remaining 2)
          end else if primitive < Atomic.get first_failure then
            match Polygon_triangulation.primitive ?cancel ~positions
                ~topology:topology_view ~scratch primitive ~emit with
            | Ok () -> ()
            | Error _ -> record_min first_failure primitive
        done);
      output_offsets.(output_primitives) <- output_vertex_count;
      let failure = Atomic.get first_failure in
      if failure <> max_int then
        let scratch = Polygon_triangulation.create_scratch () in
        (match Polygon_triangulation.primitive ?cancel ~positions
            ~topology:topology_view ~scratch failure
            ~emit:(fun _ _ _ _ -> ()) with
         | Error message -> Error ("Rdk_mesh.Triangulate.triangulate: " ^ message)
         | Ok () -> Error (Printf.sprintf
             "Rdk_mesh.Triangulate.triangulate: primitive %d failed triangulation" failure))
      else begin
        let output_topology = Topology.View.create_validated_owned
            ~point_count:(Geometry.point_count geometry)
            ~vertex_points:output_vertices ~primitive_offsets:output_offsets
            ~primitive_kinds:output_kinds in
        Topology_remap.preserving_points ?cancel ~grain
          ~topology:output_topology ~vertex_map ~primitive_map geometry
      end
  end
  with Invalid_argument message -> Error message
