open Rdk

module Solid = Rdk_boolean.Boolean_solid
module Extract = Rdk_boolean.Boolean_extract
module Seam = Rdk_boolean.Boolean_seam
module Materialization = Rdk_boolean.Boolean_materialization
module Complex = Rdk_boolean.Boolean_complex

let fail format = Printf.ksprintf failwith format
let check condition message = if not condition then fail "%s" message
let get = function Ok value -> value | Error error -> fail "%s" (Error.to_string error)
let get_string = function Ok value -> value | Error message -> fail "%s" message

let geometry points triangles =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.polygons_owned ~point_count:count
      ~vertex_points:triangles
      ~primitive_offsets:(Array.init ((Array.length triangles / 3) + 1)
        (fun primitive -> primitive * 3)) |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let polygon_geometry points vertex_points primitive_offsets =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.polygons_owned ~point_count:count ~vertex_points
      ~primitive_offsets |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let tetra ~origin:(ox, oy, oz) size = geometry
    [|ox,oy,oz; ox+.size,oy,oz; ox,oy+.size,oz; ox,oy,oz+.size|]
    [|0;2;1; 0;1;3; 1;2;3; 2;0;3|]

let cube ~origin:(ox, oy, oz) size = geometry
    [|ox,oy,oz; ox+.size,oy,oz; ox+.size,oy+.size,oz; ox,oy+.size,oz;
      ox,oy,oz+.size; ox+.size,oy,oz+.size;
      ox+.size,oy+.size,oz+.size; ox,oy+.size,oz+.size|]
    [|0;3;2; 0;2;1; 4;5;6; 4;6;7; 0;1;5; 0;5;4;
      3;7;6; 3;6;2; 0;4;7; 0;7;3; 1;2;6; 1;6;5|]

let cube_quads ~origin:(ox, oy, oz) size = polygon_geometry
    [|ox,oy,oz; ox+.size,oy,oz; ox+.size,oy+.size,oz; ox,oy+.size,oz;
      ox,oy,oz+.size; ox+.size,oy,oz+.size;
      ox+.size,oy+.size,oz+.size; ox,oy+.size,oz+.size|]
    [|0;3;2;1; 4;5;6;7; 0;1;5;4; 3;7;6;2; 0;4;7;3; 1;2;6;5|]
    [|0;4;8;12;16;20;24|]

let torus ~major_segments ~minor_segments ~center_x ~major ~minor =
  let point_count = major_segments * minor_segments in
  let points = Array.make point_count (0., 0., 0.) in
  let point u v = (u mod major_segments) * minor_segments
      + (v mod minor_segments) in
  for u = 0 to major_segments - 1 do
    let alpha = 2. *. Float.pi *. float_of_int u /. float_of_int major_segments in
    for v = 0 to minor_segments - 1 do
      let beta = 2. *. Float.pi *. float_of_int v /. float_of_int minor_segments in
      let radial = major +. (minor *. cos beta) in
      (* The stress cutter rotates the ordinary XZ torus by pi/2 around X. *)
      let source_y = minor *. sin beta and source_z = radial *. sin alpha in
      let cosine = cos (Float.pi *. 0.5) and sine = sin (Float.pi *. 0.5) in
      points.(point u v) <-
        center_x +. (radial *. cos alpha),
        (cosine *. source_y) -. (sine *. source_z),
        (sine *. source_y) +. (cosine *. source_z)
    done
  done;
  let triangles = Array.make (major_segments * minor_segments * 6) 0
  and cursor = ref 0 in
  let triangle a b c =
    triangles.(!cursor) <- a; triangles.(!cursor + 1) <- b;
    triangles.(!cursor + 2) <- c; cursor := !cursor + 3 in
  for u = 0 to major_segments - 1 do
    for v = 0 to minor_segments - 1 do
      let a = point u v and b = point u (v + 1)
      and c = point (u + 1) (v + 1) and d = point (u + 1) v in
      triangle a b c; triangle a c d
    done
  done;
  geometry points triangles

let merge_geometry geometries =
  let point_count = Array.fold_left
      (fun count geometry -> count + Geometry.point_count geometry) 0 geometries
  and vertex_count = Array.fold_left
      (fun count geometry -> count + Geometry.vertex_count geometry) 0 geometries
  and primitive_count = Array.fold_left
      (fun count geometry -> count + Geometry.primitive_count geometry) 0 geometries in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. and vertices = Array.make vertex_count 0
  and offsets = Array.make (primitive_count + 1) 0 in
  let point_base = ref 0 and vertex_base = ref 0 and primitive_base = ref 0 in
  Array.iter (fun source ->
    let positions = Packed.Float3.Private.view (Geometry.positions source)
    and topology = Topology.Private.view (Geometry.topology source) in
    Array.blit positions.x 0 x !point_base (Array.length positions.x);
    Array.blit positions.y 0 y !point_base (Array.length positions.y);
    Array.blit positions.z 0 z !point_base (Array.length positions.z);
    Array.iteri (fun vertex point ->
      vertices.(!vertex_base + vertex) <- !point_base + point)
      topology.vertex_points;
    for primitive = 0 to Geometry.primitive_count source - 1 do
      offsets.(!primitive_base + primitive) <-
        !vertex_base + topology.primitive_offsets.(primitive)
    done;
    point_base := !point_base + Geometry.point_count source;
    vertex_base := !vertex_base + Geometry.vertex_count source;
    primitive_base := !primitive_base + Geometry.primitive_count source) geometries;
  offsets.(primitive_count) <- vertex_count;
  let topology = Topology.polygons_owned ~point_count ~vertex_points:vertices
      ~primitive_offsets:offsets |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let signature geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and topology = Topology.Private.view (Geometry.topology geometry) in
  Array.copy positions.x, Array.copy positions.y, Array.copy positions.z,
  Array.copy topology.vertex_points, Array.copy topology.primitive_offsets

let signed_volume geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and topology = Topology.Private.view (Geometry.topology geometry)
  and volume = ref 0. in
  for triangle = 0 to Geometry.primitive_count geometry - 1 do
    let offset = topology.primitive_offsets.(triangle) in
    let a = topology.vertex_points.(offset)
    and b = topology.vertex_points.(offset + 1)
    and c = topology.vertex_points.(offset + 2) in
    volume := !volume +.
      (positions.x.(a) *. ((positions.y.(b) *. positions.z.(c))
                           -. (positions.z.(b) *. positions.y.(c)))
       +. positions.y.(a) *. ((positions.z.(b) *. positions.x.(c))
                              -. (positions.x.(b) *. positions.z.(c)))
       +. positions.z.(a) *. ((positions.x.(b) *. positions.y.(c))
                              -. (positions.y.(b) *. positions.x.(c)))) /. 6.
  done;
  !volume

let surface_area geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and topology = Topology.Private.view (Geometry.topology geometry)
  and area = ref 0. in
  for triangle = 0 to Geometry.primitive_count geometry - 1 do
    let offset = topology.primitive_offsets.(triangle) in
    let a = topology.vertex_points.(offset)
    and b = topology.vertex_points.(offset + 1)
    and c = topology.vertex_points.(offset + 2) in
    let abx = positions.x.(b) -. positions.x.(a)
    and aby = positions.y.(b) -. positions.y.(a)
    and abz = positions.z.(b) -. positions.z.(a)
    and acx = positions.x.(c) -. positions.x.(a)
    and acy = positions.y.(c) -. positions.y.(a)
    and acz = positions.z.(c) -. positions.z.(a) in
    let nx = (aby *. acz) -. (abz *. acy)
    and ny = (abz *. acx) -. (abx *. acz)
    and nz = (abx *. acy) -. (aby *. acx) in
    area := !area +. (sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) *. 0.5)
  done;
  !area

let test_build_once_query_many () =
  let left = tetra ~origin:(0.,0.,0.) 2.
  and right = tetra ~origin:(0.5,0.2,0.2) 2. in
  let solid = Solid.prepare ~grain:1 ~left ~right () |> get in
  check (Solid.vertex_count solid > 8 && Solid.facet_count solid > 8
      && Solid.shell_count solid > 0)
    "prepared Boolean arrangement is unexpectedly empty";
  let union = Solid.extract ~expression:Extract.union solid |> get
  and intersection = Solid.extract ~expression:Extract.intersection solid |> get
  and difference = Solid.extract ~expression:Extract.difference solid |> get
  and custom = Solid.extract
      ~expression:(Extract.And (Extract.Left, Extract.Not Extract.Right)) solid
      |> get in
  check (Geometry.primitive_count union > 0
      && Geometry.primitive_count intersection > 0
      && Geometry.primitive_count difference > 0)
    "build-once Boolean query lost a solid region";
  check (signature difference = signature custom)
    "typed expression query differs from the difference convenience expression"

let test_domain_exactness () =
  let left = tetra ~origin:(0.,0.,0.) 2.
  and right = tetra ~origin:(0.5,0.2,0.2) 2. in
  let run domains = Parallel.run ~domains (fun () ->
      Solid.prepare ~grain:1 ~left ~right () |> get
      |> Solid.extract ~expression:Extract.xor |> get |> signature) in
  check (run 1 = run 4) "build-once Boolean differs between domain counts"

let test_shatter_products () =
  let left = tetra ~origin:(0.,0.,0.) 2.
  and right = tetra ~origin:(0.5,0.2,0.2) 2. in
  let run domains = Parallel.run ~domains (fun () ->
      let solid = Solid.prepare ~grain:1 ~left ~right () |> get in
      let products = Solid.shatter_with_ancestry solid |> get in
      Array.map (fun ancestry -> signature (Extract.geometry ancestry)) products) in
  check (run 1 = run 4) "Boolean shatter products differ between domain counts";
  let solid = Solid.prepare ~grain:1 ~left ~right () |> get in
  let products = Solid.shatter_with_ancestry solid |> get in
  check (Array.length products = 3
      && Array.for_all (fun ancestry ->
        Geometry.primitive_count (Extract.geometry ancestry) > 0) products)
    "transverse shatter lost an A-only, overlap, or B-only region";
  let union = Solid.extract ~expression:Extract.union solid |> get in
  let primitive_sum = Array.fold_left (fun count ancestry ->
      count + Geometry.primitive_count (Extract.geometry ancestry)) 0 products in
  check (primitive_sum > Geometry.primitive_count union)
    "shatter did not retain duplicate internal walls between region products";
  let shatter_volume = Array.fold_left (fun volume ancestry ->
      volume +. signed_volume (Extract.geometry ancestry)) 0. products in
  check (abs_float (shatter_volume -. signed_volume union) <= 1e-10)
    "shatter region volumes do not partition the Boolean union";
  let nested = Solid.prepare ~grain:1
      ~left:(tetra ~origin:(0.,0.,0.) 4.)
      ~right:(tetra ~origin:(1.,1.,1.) 0.5) () |> get
      |> Solid.shatter_with_ancestry |> get in
  check (Geometry.primitive_count (Extract.geometry nested.(0)) > 0
      && Geometry.primitive_count (Extract.geometry nested.(1)) > 0
      && Geometry.primitive_count (Extract.geometry nested.(2)) = 0)
    "nested shatter produced a non-empty B-only region";
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Solid.shatter_with_ancestry ~cancel solid with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail "unexpected shatter cancellation error: %s"
       (Error.to_string error)
   | Ok _ -> fail "cancelled Boolean shatter completed")

let test_degenerate_rejection () =
  let degenerate = geometry
      [|0.,0.,0.; 0.5,0.,0.; 1.,0.,0.|] [|0;1;2|]
  and solid = tetra ~origin:(0.,0.,0.) 1. in
  match Solid.prepare ~grain:1 ~left:degenerate ~right:solid () with
  | Error error when Error.code error = "degenerate_triangle" -> ()
  | Error error when Error.code error = "invalid_surface" -> ()
  | Error error -> fail "unexpected degenerate error: %s" (Error.to_string error)
  | Ok _ -> fail "degenerate source triangle entered the solid Boolean pipeline"

let test_cancellation () =
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Solid.prepare ~cancel ~grain:1
      ~left:(tetra ~origin:(0.,0.,0.) 1.)
      ~right:(tetra ~origin:(0.2,0.2,0.2) 1.) () with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled Boolean preparation completed"

let test_transaction_ancestry () =
  let solid = Solid.prepare ~grain:1
      ~left:(tetra ~origin:(0.,0.,0.) 2.)
      ~right:(tetra ~origin:(0.5,0.2,0.2) 2.) () |> get in
  let ancestry = Solid.extract_with_ancestry ~expression:Extract.union solid |> get in
  check (Geometry.primitive_count (Extract.geometry ancestry) > 0)
    "transaction ancestry query returned no boundary";
  for primitive = 0 to Geometry.primitive_count (Extract.geometry ancestry) - 1 do
    check (Extract.primitive_face ancestry primitive >= 0)
      "transaction ancestry query lost its source face"
  done

let tiny_edge_signature group =
  Array.init (Edge_group.length group) (fun edge -> Edge_group.mem edge group)

let test_tiny_seam_candidate_policy () =
  let left = tetra ~origin:(0.,0.,0.) 2.
  and right = tetra ~origin:(0.5,0.2,0.2) 2. in
  let build domains = Parallel.run ~domains (fun () ->
      let solid = Solid.prepare ~grain:1 ~left ~right () |> get in
      let ancestry = Solid.extract_with_ancestry ~expression:Extract.union solid
          |> get
      and seam = Solid.seams ~grain:1 ~parallel_cutoff:1 solid |> get in
      let none = Materialization.tiny_seam_edges ~grain:1 ~threshold:0.
          ancestry seam |> get
      and candidates = Materialization.tiny_seam_edges ~grain:1 ~threshold:100.
          ancestry seam |> get in
      let safe = Materialization.safe_independent_edges ~grain:1 candidates
          (Extract.geometry ancestry) |> get in
      check (Edge_group.cardinality none = 0)
        "zero threshold selected a positive-length Boolean seam edge";
      check (Edge_group.cardinality candidates > 0)
        "large threshold did not select any extracted Boolean seam edge";
      check (Edge_group.cardinality safe <= Edge_group.cardinality candidates)
        "strict Boolean seam contraction planning exceeded its candidate set";
      let geometry = Extract.geometry ancestry in
      let index = Topology_index.create (Geometry.topology geometry)
          |> Topology_index.Private.view
      and complex = Extract.Private.complex ancestry in
      let seam_facets = Bytes.make (Complex.facet_count complex) '\000' in
      for complex_edge = 0 to Complex.edge_count complex - 1 do
        if Seam.Private.is_seam_edge seam complex_edge then begin
          let first, last = Complex.edge_incident_range complex complex_edge in
          for incident = first to last - 1 do
            Bytes.unsafe_set seam_facets
              (Complex.edge_incident_facet complex incident) '\001'
          done
        end
      done;
      Edge_group.iter (fun edge ->
        let adjacent = ref false in
        for incident = index.edge_offsets.(edge)
            to index.edge_offsets.(edge + 1) - 1 do
          let primitive = index.primitive_of_vertex.(index.edge_vertices.(incident)) in
          let facet = Extract.Private.primitive_complex_facet ancestry primitive in
          if Bytes.unsafe_get seam_facets facet <> '\000' then adjacent := true
        done;
        check !adjacent
          "tiny-edge policy selected a surface edge outside exact seam facets")
        candidates;
      tiny_edge_signature candidates, tiny_edge_signature safe) in
  check (build 1 = build 4)
    "tiny Boolean seam candidate selection differs between domain counts";
  let first = Solid.prepare ~grain:1 ~left ~right () |> get
  and second = Solid.prepare ~grain:1 ~left ~right () |> get in
  let ancestry = Solid.extract_with_ancestry ~expression:Extract.union first |> get
  and foreign_seam = Solid.seams second |> get in
  (match Materialization.tiny_seam_edges ~grain:1 ~threshold:1.
      ancestry foreign_seam with
   | Error error when Error.code error = "invalid_input" -> ()
   | Error error -> fail "unexpected ownership error: %s" (Error.to_string error)
   | Ok _ -> fail "foreign Boolean seam product entered materialization");
  let local_seam = Solid.seams first |> get in
  let no_cleanup = Materialization.collapse_tiny_seam_batch ~grain:1
      ~threshold:0. ~require_closed:true ancestry local_seam
      (Extract.geometry ancestry) |> get in
  check (Materialization.cleanup_candidate_count no_cleanup = 0
      && Materialization.cleanup_collapsed_count no_cleanup = 0
      && Materialization.cleanup_geometry no_cleanup = Extract.geometry ancestry)
    "zero-threshold Boolean cleanup was not an exact no-op";
  for point = 0 to Geometry.point_count (Materialization.cleanup_geometry no_cleanup) - 1 do
    check (Materialization.cleanup_point_source no_cleanup point = point)
      "zero-threshold cleanup changed point ancestry"
  done;
  let split = Materialization.split_seam_points ~grain:1 no_cleanup |> get in
  let split_geometry = Materialization.cleanup_geometry split
  and original_geometry = Materialization.cleanup_geometry no_cleanup in
  check (Geometry.point_count split_geometry > Geometry.point_count original_geometry
      && Geometry.vertex_count split_geometry = Geometry.vertex_count original_geometry
      && Geometry.primitive_count split_geometry
         = Geometry.primitive_count original_geometry)
    "unique seam points did not duplicate only the point domain";
  check (Edge_group.cardinality (Materialization.cleanup_seam_edges split)
      = 2 * Edge_group.cardinality
          (Materialization.cleanup_seam_edges no_cleanup))
    "unique seam points did not duplicate both manifold sides of each seam edge";
  let split_positions = Packed.Float3.Private.view
      (Geometry.positions split_geometry)
  and original_positions = Packed.Float3.Private.view
      (Geometry.positions original_geometry) in
  for point = 0 to Geometry.point_count split_geometry - 1 do
    let source = Materialization.cleanup_point_source split point in
    check (split_positions.x.(point) = original_positions.x.(source)
        && split_positions.y.(point) = original_positions.y.(source)
        && split_positions.z.(point) = original_positions.z.(source))
      "unique seam point position does not follow packed source ancestry"
  done;
  let add_ids owner name count geometry =
    let attribute = Attribute.create_owned ~owner ~name
        (Attribute.Int (Array.init count Fun.id)) |> get_string in
    Geometry.with_attribute attribute geometry |> get_string in
  let enriched = original_geometry
      |> add_ids Attribute.Point "point_id" (Geometry.point_count original_geometry)
      |> add_ids Attribute.Vertex "vertex_id" (Geometry.vertex_count original_geometry)
      |> add_ids Attribute.Primitive "primitive_id"
           (Geometry.primitive_count original_geometry) in
  let point_group = Group.init ~owner:Group.Point ~name:"even_points"
      (Geometry.point_count original_geometry) (fun point -> point land 1 = 0) in
  let enriched = Geometry.with_group point_group enriched |> get_string in
  let user_seam = Edge_group.with_name "user_seam"
      (Materialization.cleanup_seam_edges no_cleanup) in
  let enriched = Geometry.with_edge_group user_seam enriched |> get_string in
  let enriched_cleanup = Materialization.collapse_tiny_seam_batch ~grain:1
      ~threshold:0. ~require_closed:true ancestry local_seam enriched |> get
      |> Materialization.split_seam_points ~grain:1 |> get in
  let enriched_output = Materialization.cleanup_geometry enriched_cleanup in
  let int_values owner name = match Geometry.find_attribute ~owner name enriched_output with
    | Some attribute ->
        (match Attribute.Private.storage attribute with
         | Attribute.Int values -> values
         | _ -> fail "seam split changed integer ID storage")
    | None -> fail "seam split lost an ID attribute" in
  let point_ids = int_values Attribute.Point "point_id"
  and vertex_ids = int_values Attribute.Vertex "vertex_id"
  and primitive_ids = int_values Attribute.Primitive "primitive_id" in
  for point = 0 to Array.length point_ids - 1 do
    check (point_ids.(point)
        = Materialization.cleanup_point_source enriched_cleanup point)
      "seam split point attribute did not follow source ancestry"
  done;
  check (vertex_ids = Array.init (Array.length vertex_ids) Fun.id
      && primitive_ids = Array.init (Array.length primitive_ids) Fun.id)
    "seam split changed unchanged vertex/primitive payload order";
  let copied_group = Geometry.find_group ~owner:Group.Point "even_points"
      enriched_output |> Option.get in
  for point = 0 to Geometry.point_count enriched_output - 1 do
    check (Group.mem point copied_group
        = (Materialization.cleanup_point_source enriched_cleanup point land 1 = 0))
      "seam split point group did not follow source ancestry"
  done;
  let copied_seam = Geometry.find_edge_group "user_seam" enriched_output
      |> Option.get in
  check (Edge_group.cardinality copied_seam
      = Edge_group.cardinality (Materialization.cleanup_seam_edges enriched_cleanup))
    "seam split native edge-group membership diverged from exact seam ancestry";
  let split_signature domains = Parallel.run ~domains (fun () ->
      let solid = Solid.prepare ~grain:1 ~left ~right () |> get in
      let ancestry = Solid.extract_with_ancestry ~expression:Extract.union solid
          |> get
      and seam = Solid.seams ~grain:1 ~parallel_cutoff:1 solid |> get in
      let cleanup = Materialization.collapse_tiny_seam_batch ~grain:1
          ~threshold:0. ~require_closed:true ancestry seam
          (Extract.geometry ancestry) |> get
          |> Materialization.split_seam_points ~grain:1 |> get in
      let output = Materialization.cleanup_geometry cleanup in
      Array.init (Geometry.point_count output)
        (Materialization.cleanup_point_source cleanup),
      tiny_edge_signature (Materialization.cleanup_seam_edges cleanup),
      signature output) in
  check (split_signature 1 = split_signature 4)
    "unique Boolean seam points differ between domain counts";
  (match Materialization.split_seam_points ~grain:0 no_cleanup with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected seam-split grain error: %s"
       (Error.to_string error)
   | Ok _ -> fail "zero seam-split grain was accepted");
  let split_cancel = Cancel.create () in
  Cancel.cancel split_cancel;
  (match Materialization.split_seam_points ~cancel:split_cancel ~grain:1
      no_cleanup with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail "unexpected seam-split cancellation error: %s"
       (Error.to_string error)
   | Ok _ -> fail "cancelled seam-point split completed");
  let cleanup_left = cube ~origin:(0.,0.,0.) 2. in
  let cleanup_solid = Solid.prepare ~grain:1 ~left:cleanup_left
      ~right:(cube ~origin:(0.60000000000000009,
        0.60000000000000009,0.60000000000000009) 2.) () |> get in
  let cleanup_ancestry = Solid.extract_with_ancestry
      ~expression:Extract.union cleanup_solid |> get
  and cleanup_seam = Solid.seams cleanup_solid |> get in
  let candidate_geometry = Extract.geometry cleanup_ancestry in
  let candidates = Materialization.tiny_seam_edges ~grain:1 ~threshold:100.
      cleanup_ancestry cleanup_seam |> get in
  let index = Topology_index.create (Geometry.topology candidate_geometry)
      |> Topology_index.Private.view
  and positions = Packed.Float3.Private.view
      (Geometry.positions candidate_geometry) in
  let threshold = ref infinity in
  Edge_group.iter (fun edge ->
    let a = index.edge_a.(edge) and b = index.edge_b.(edge) in
    threshold := Float.min !threshold
        (Float.hypot (positions.x.(a) -. positions.x.(b))
          (Float.hypot (positions.y.(a) -. positions.y.(b))
             (positions.z.(a) -. positions.z.(b))))) candidates;
  let cleanup = Materialization.collapse_tiny_seam_batch ~grain:1
      ~threshold:!threshold ~require_closed:true cleanup_ancestry cleanup_seam
      candidate_geometry |> get in
  check (Materialization.cleanup_candidate_count cleanup > 0
      && Materialization.cleanup_collapsed_count cleanup > 0
      && Geometry.primitive_count (Materialization.cleanup_geometry cleanup)
         < Geometry.primitive_count (Extract.geometry cleanup_ancestry))
    "Boolean cleanup batch did not contract a verified seam-adjacent edge";
  let continued = Materialization.collapse_tiny_seams ~grain:1
      ~threshold:!threshold ~require_closed:true ~max_batches:8 ~strict:false
      cleanup_ancestry cleanup_seam candidate_geometry |> get in
  check (Materialization.cleanup_batch_count continued >= 1
      && Materialization.cleanup_collapsed_count continued
         >= Materialization.cleanup_collapsed_count cleanup)
    "bounded Boolean cleanup did not retain cumulative batch diagnostics";
  let strict_result = Materialization.collapse_tiny_seams ~grain:1
      ~threshold:!threshold ~require_closed:true ~max_batches:8 ~strict:true
      cleanup_ancestry cleanup_seam candidate_geometry in
  (if Materialization.cleanup_remaining_candidate_count continued = 0 then
     ignore (strict_result |> get)
   else match strict_result with
     | Error error when Error.code error = "unresolved_cleanup" -> ()
     | Error error -> fail "unexpected strict cleanup error: %s"
         (Error.to_string error)
     | Ok _ -> fail "strict Boolean cleanup published unresolved candidates");
  (match Materialization.collapse_tiny_seams ~grain:1 ~threshold:0.
      ~require_closed:true ~max_batches:0 cleanup_ancestry cleanup_seam
      candidate_geometry with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected cleanup batch-bound error: %s"
       (Error.to_string error)
   | Ok _ -> fail "Boolean cleanup accepted a zero batch bound");
  let cleaned_geometry = Materialization.cleanup_geometry cleanup
  and source_geometry = Extract.geometry cleanup_ancestry in
  let cleaned_positions = Packed.Float3.Private.view
      (Geometry.positions cleaned_geometry)
  and source_positions = Packed.Float3.Private.view
      (Geometry.positions source_geometry) in
  for point = 0 to Geometry.point_count cleaned_geometry - 1 do
    let source = Materialization.cleanup_point_source cleanup point in
    check (source >= 0 && source < Geometry.point_count source_geometry
        && cleaned_positions.x.(point) = source_positions.x.(source)
        && cleaned_positions.y.(point) = source_positions.y.(source)
        && cleaned_positions.z.(point) = source_positions.z.(source))
      "cleanup point ancestry does not reconstruct least-endpoint position"
  done;
  for vertex = 0 to Geometry.vertex_count cleaned_geometry - 1 do
    check (Materialization.cleanup_vertex_source cleanup vertex >= 0
        && Materialization.cleanup_vertex_source cleanup vertex
           < Geometry.vertex_count source_geometry)
      "cleanup vertex ancestry is out of range"
  done;
  for primitive = 0 to Geometry.primitive_count cleaned_geometry - 1 do
    check (Materialization.cleanup_primitive_source cleanup primitive >= 0
        && Materialization.cleanup_primitive_source cleanup primitive
           < Geometry.primitive_count source_geometry)
      "cleanup primitive ancestry is out of range"
  done;
  let cleaned_seams = Materialization.cleanup_seam_edges cleanup in
  check (Edge_group.topology_data_id cleaned_seams
      = Topology.data_id (Geometry.topology cleaned_geometry)
      && Edge_group.cardinality cleaned_seams > 0)
    "cleanup lost its topology-affine exact seam edge product";
  let cleanup_signature domains = Parallel.run ~domains (fun () ->
      let solid = Solid.prepare ~grain:1 ~left:cleanup_left
          ~right:(cube ~origin:(0.60000000000000009,
            0.60000000000000009,0.60000000000000009) 2.) () |> get in
      let ancestry = Solid.extract_with_ancestry ~expression:Extract.union solid
          |> get
      and seam = Solid.seams ~grain:1 ~parallel_cutoff:1 solid |> get in
      let geometry = Extract.geometry ancestry in
      let candidates = Materialization.tiny_seam_edges ~grain:1 ~threshold:100.
          ancestry seam |> get in
      let index = Topology_index.create (Geometry.topology geometry)
          |> Topology_index.Private.view
      and positions = Packed.Float3.Private.view (Geometry.positions geometry) in
      let threshold = ref infinity in
      Edge_group.iter (fun edge ->
        let a = index.edge_a.(edge) and b = index.edge_b.(edge) in
        threshold := Float.min !threshold
            (Float.hypot (positions.x.(a) -. positions.x.(b))
              (Float.hypot (positions.y.(a) -. positions.y.(b))
                 (positions.z.(a) -. positions.z.(b))))) candidates;
      let value = Materialization.collapse_tiny_seam_batch ~grain:1
          ~threshold:!threshold ~require_closed:true ancestry seam geometry |> get in
      let output = Materialization.cleanup_geometry value in
      Materialization.cleanup_candidate_count value,
      Materialization.cleanup_collapsed_count value,
      Array.init (Geometry.point_count output)
        (Materialization.cleanup_point_source value),
      Array.init (Geometry.vertex_count output)
        (Materialization.cleanup_vertex_source value),
      Array.init (Geometry.primitive_count output)
        (Materialization.cleanup_primitive_source value),
      tiny_edge_signature (Materialization.cleanup_seam_edges value),
      signature output) in
  check (cleanup_signature 1 = cleanup_signature 4)
    "verified Boolean cleanup differs between domain counts";
  (match Materialization.collapse_tiny_seam_batch ~grain:1 ~threshold:0.
      ~require_closed:true ancestry local_seam
      (tetra ~origin:(10.,0.,0.) 1.) with
   | Error error when Error.code error = "geometry_mismatch" -> ()
   | Error error -> fail "unexpected cleanup ownership error: %s"
       (Error.to_string error)
   | Ok _ -> fail "foreign geometry entered Boolean cleanup");
  let invalid grain threshold expected =
    match Materialization.tiny_seam_edges ~grain ~threshold ancestry local_seam with
    | Error error when Error.code error = expected -> ()
    | Error error -> fail "unexpected tiny-edge parameter error: %s"
        (Error.to_string error)
    | Ok _ -> fail "invalid tiny-edge parameter was accepted" in
  invalid 0 1. "invalid_parameter";
  invalid 1 infinity "invalid_parameter";
  invalid 1 (-1.) "invalid_parameter";
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Materialization.tiny_seam_edges ~cancel ~grain:1 ~threshold:1.
      ancestry local_seam with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail "unexpected tiny-edge cancellation error: %s"
       (Error.to_string error)
   | Ok _ -> fail "cancelled tiny-edge selection completed")

let test_rounded_surface_verifier () =
  let closed = tetra ~origin:(0.,0.,0.) 2. in
  ignore (Materialization.verify_surface ~grain:1 ~require_closed:true closed
    |> get);
  let open_triangle = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|] [|0;1;2|] in
  ignore (Materialization.verify_surface ~grain:1 ~require_closed:false
    open_triangle |> get);
  (match Materialization.verify_surface ~grain:1 ~require_closed:true open_triangle with
   | Error error when Error.code error = "open_output" -> ()
   | Error error -> fail "unexpected open-surface verification error: %s"
       (Error.to_string error)
   | Ok () -> fail "open triangle passed closed Boolean verification");
  let crossing = geometry
      [|-1.,0.,0.; 1.,0.,0.; 0.,1.,0.;
        0.,0.,-1.; 0.,0.,1.; 0.,-1.,0.|]
      [|0;1;2; 3;4;5|] in
  let run domains = Parallel.run ~domains (fun () ->
      Materialization.verify_surface ~grain:1 ~require_closed:false crossing
      |> Result.map_error Error.to_string) in
  check (run 1 = run 4)
    "rounded-surface verification differs between domain counts";
  (match Materialization.verify_surface ~grain:1 ~require_closed:false crossing with
   | Error error when Error.code error = "surface_self_intersection" -> ()
   | Error error -> fail "unexpected surface-intersection error: %s"
       (Error.to_string error)
   | Ok () -> fail "crossing rounded triangles passed Boolean verification");
  let coplanar_overlap = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.;
        0.25,0.25,0.; 1.25,0.25,0.; 0.25,1.25,0.|]
      [|0;1;2; 3;4;5|] in
  (match Materialization.verify_surface ~grain:1 ~require_closed:false
      coplanar_overlap with
   | Error error when Error.code error = "surface_self_intersection" -> ()
   | Error error -> fail "unexpected coplanar verification error: %s"
       (Error.to_string error)
   | Ok () -> fail "coplanar overlapping triangles passed Boolean verification");
  let opposite_duplicate = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|] [|0;1;2; 0;2;1|] in
  ignore (Materialization.verify_surface ~grain:1 ~require_closed:false
    ~allow_opposite_duplicates:true opposite_duplicate |> get);
  (match Materialization.verify_surface ~grain:1 ~require_closed:false
      opposite_duplicate with
   | Error error when Error.code error = "surface_self_intersection" -> ()
   | Error error -> fail "unexpected opposite-wall verification error: %s"
       (Error.to_string error)
   | Ok () -> fail "opposite duplicate passed without the explicit cut-wall policy");
  let same_duplicate = geometry
      [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|] [|0;1;2; 0;1;2|] in
  (match Materialization.verify_surface ~grain:1 ~require_closed:false
      ~allow_opposite_duplicates:true same_duplicate with
   | Error error when Error.code error = "surface_self_intersection" -> ()
   | Error error -> fail "unexpected same-wall verification error: %s"
       (Error.to_string error)
   | Ok () -> fail "same-facing duplicate passed the opposite-wall policy");
  let tiny = 1e-100 in
  let tiny_triangle = geometry
      [|0.,0.,0.; tiny,0.,0.; 0.,tiny,0.|] [|0;1;2|] in
  ignore (Materialization.verify_surface ~grain:1 ~require_closed:false
    tiny_triangle |> get);
  (match Materialization.verify_surface ~grain:0 ~require_closed:false closed with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected verifier grain error: %s" (Error.to_string error)
   | Ok () -> fail "zero verifier grain was accepted");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Materialization.verify_surface ~cancel ~grain:1
      ~require_closed:false closed with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail "unexpected verifier cancellation error: %s"
       (Error.to_string error)
   | Ok () -> fail "cancelled rounded-surface verification completed")

let test_detriangulation_policy () =
  let left = cube_quads ~origin:(0.,0.,0.) 2.
  and right = tetra ~origin:(10.,0.,0.) 1. in
  let build domains = Parallel.run ~domains (fun () ->
      let solid = Solid.prepare ~grain:1 ~left ~right () |> get in
      let ancestry = Solid.extract_with_ancestry ~expression:Extract.union solid
          |> get
      and seam = Solid.seams ~grain:1 ~parallel_cutoff:1 solid |> get in
      let cleanup = Materialization.collapse_tiny_seam_batch ~grain:1
          ~threshold:0. ~require_closed:true ancestry seam
          (Extract.geometry ancestry) |> get in
      let output = Materialization.detriangulate ~grain:1 ~assume_flat:false
          ~mode:Materialization.Unchanged_polygons ancestry cleanup |> get in
      let geometry = Materialization.cleanup_geometry output in
      Array.init (Geometry.point_count geometry)
        (Materialization.cleanup_point_source output),
      Array.init (Geometry.vertex_count geometry)
        (Materialization.cleanup_vertex_source output),
      Array.init (Geometry.primitive_count geometry)
        (Materialization.cleanup_primitive_source output),
      signature geometry) in
  check (build 1 = build 4)
    "Boolean detriangulation differs between domain counts";
  let solid = Solid.prepare ~grain:1 ~left ~right () |> get in
  let ancestry = Solid.extract_with_ancestry ~expression:Extract.union solid |> get
  and seam = Solid.seams solid |> get in
  let cleanup = Materialization.collapse_tiny_seam_batch ~grain:1
      ~threshold:0. ~require_closed:true ancestry seam
      (Extract.geometry ancestry) |> get in
  let unchanged = Materialization.detriangulate ~grain:1 ~assume_flat:false
      ~mode:Materialization.Unchanged_polygons ancestry cleanup |> get in
  let output = Materialization.cleanup_geometry unchanged in
  check (Geometry.primitive_count output = 10)
    "unchanged-polygon detriangulation did not reconstruct six cube quads";
  let quads = ref 0 and triangles = ref 0 in
  for primitive = 0 to Geometry.primitive_count output - 1 do
    match Topology.primitive_size (Geometry.topology output) primitive with
    | 4 -> incr quads | 3 -> incr triangles
    | size -> fail "detriangulation emitted unexpected %d-corner polygon" size
  done;
  check (!quads = 6 && !triangles = 4)
    "detriangulation output polygon cardinalities are wrong";
  let all = Materialization.detriangulate ~grain:1 ~assume_flat:true
      ~mode:Materialization.All_polygons ancestry cleanup |> get in
  check (signature (Materialization.cleanup_geometry all) = signature output)
    "all/unchanged detriangulation disagree on uncut planar source polygons";
  let crossing_solid = Solid.prepare ~grain:1
      ~left:(cube ~origin:(0.,0.,0.) 2.)
      ~right:(cube ~origin:(0.60000000000000009,
        0.60000000000000009,0.60000000000000009) 2.) () |> get in
  let crossing_ancestry = Solid.extract_with_ancestry
      ~expression:Extract.union crossing_solid |> get
  and crossing_seam = Solid.seams crossing_solid |> get in
  let crossing_cleanup = Materialization.collapse_tiny_seam_batch ~grain:1
      ~threshold:0. ~require_closed:true crossing_ancestry crossing_seam
      (Extract.geometry crossing_ancestry) |> get in
  let crossing_output = Materialization.detriangulate ~grain:1
      ~assume_flat:false ~mode:Materialization.All_polygons crossing_ancestry
      crossing_cleanup |> get in
  check (Edge_group.cardinality
      (Materialization.cleanup_seam_edges crossing_output) > 0)
    "detriangulation dissolved exact Boolean seam edges";
  (match Materialization.detriangulate ~grain:0 ~assume_flat:false
      ~mode:Materialization.All_polygons ancestry cleanup with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected detriangulation grain error: %s"
       (Error.to_string error)
   | Ok _ -> fail "zero detriangulation grain was accepted");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Materialization.detriangulate ~cancel ~grain:1 ~assume_flat:false
      ~mode:Materialization.All_polygons ancestry cleanup with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail "unexpected detriangulation cancellation error: %s"
       (Error.to_string error)
   | Ok _ -> fail "cancelled detriangulation completed")

let test_open_sheet_preparation () =
  let solid = cube ~origin:(0.,0.,0.) 2.
  and sheet = geometry
      [|-1.,1.,1.; 3.,1.,1.; -1.,1.,2.5; 3.,1.,2.5|]
      [|0;1;2; 1;3;2|] in
  let prepared = Solid.prepare ~grain:1 ~left:solid ~right:sheet
      ~right_treatment:Solid.Surface () |> get in
  let seams = Solid.seams prepared |> get in
  check (Geometry.primitive_count (Seam.curves seams) > 0)
    "open sheet preparation lost its solid intersection seam";
  let sheet_primitive_count geometry =
    let positions = Packed.Float3.Private.view (Geometry.positions geometry)
    and topology = Topology.Private.view (Geometry.topology geometry)
    and count = ref 0 in
    for primitive = 0 to Geometry.primitive_count geometry - 1 do
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1)
      and on_sheet = ref true in
      for vertex = first to last - 1 do
        let point = topology.vertex_points.(vertex) in
        if positions.y.(point) <> 1. then on_sheet := false
      done;
      if !on_sheet then incr count
    done;
    !count in
  let product operation = Solid.extract_product ~operation prepared |> get in
  let union = product Solid.Union
  and intersection = product Solid.Intersection
  and difference = product Solid.Difference
  and reverse = product Solid.Reverse_difference in
  let outside = sheet_primitive_count reverse
  and inside = sheet_primitive_count intersection in
  check (outside > 0 && inside > 0)
    "solid/surface classification lost its inside or outside sheet patches";
  check (sheet_primitive_count union = outside)
    "solid/surface union did not retain exactly the outside sheet patches";
  check (sheet_primitive_count difference = inside * 2)
    "solid-minus-surface did not create paired walls for every inside patch";
  (match Solid.extract_product ~require_closed:true
      ~operation:Solid.Intersection prepared with
   | Error error when Error.code error = "open_output" -> ()
   | Error error -> fail "unexpected closed-sheet diagnostic: %s"
       (Error.to_string error)
   | Ok _ -> fail "open treatment-aware Boolean product passed closed validation");
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Solid.extract_product ~cancel:cancelled
      ~operation:Solid.Intersection prepared with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail "unexpected product cancellation diagnostic: %s"
       (Error.to_string error)
   | Ok _ -> fail "cancelled treatment-aware Boolean extraction completed");
  let ancestry = Solid.extract_product_with_ancestry
      ~operation:Solid.Intersection prepared |> get in
  for primitive = 0 to Geometry.primitive_count (Extract.geometry ancestry) - 1 do
    check (Extract.primitive_side ancestry primitive = Complex.Right)
      "solid/surface product selected payload ancestry from the solid"
  done;
  let run domains = Parallel.run ~domains (fun () ->
      let prepared = Solid.prepare ~grain:1 ~left:solid ~right:sheet
          ~right_treatment:Solid.Surface () |> get in
      [|Solid.Union; Solid.Intersection; Solid.Difference;
        Solid.Reverse_difference; Solid.Xor|]
      |> Array.map (fun operation ->
        Solid.extract_product ~operation prepared |> get |> signature)) in
  check (run 1 = run 4)
    "solid/surface products differ between one and four domains"

let test_surface_surface_products () =
  let sheet x0 x1 = geometry
      [|x0,0.,0.; x1,0.,0.; x1,2.,0.; x0,2.,0.|]
      [|0;1;2; 0;2;3|] in
  let left = sheet 0. 2. and right = sheet 1. 3. in
  let run domains = Parallel.run ~domains (fun () ->
      let prepared = Solid.prepare ~grain:1 ~left ~right
          ~left_treatment:Solid.Surface ~right_treatment:Solid.Surface () |> get in
      [|Solid.Union; Solid.Intersection; Solid.Difference;
        Solid.Reverse_difference; Solid.Xor|]
      |> Array.map (fun operation ->
        Solid.extract_product ~operation prepared |> get)) in
  let products = run 1 in
  let expected = [|6.; 2.; 2.; 2.; 4.|] in
  Array.iteri (fun index geometry ->
    check (abs_float (surface_area geometry -. expected.(index)) < 1e-12)
      (Printf.sprintf "surface/surface product %d has the wrong exact patch area" index))
    products;
  check (Array.map signature products = Array.map signature (run 4))
    "surface/surface products differ between one and four domains";
  let transverse = Solid.prepare ~grain:1
      ~left:(geometry [|0.,0.,0.; 2.,0.,0.; 0.,2.,0.|] [|0;1;2|])
      ~right:(geometry [|1.,-1.,-1.; 1.,3.,-1.; 1.,-1.,1.|] [|0;1;2|])
      ~left_treatment:Solid.Surface ~right_treatment:Solid.Surface () |> get in
  let intersection = Solid.extract_product ~operation:Solid.Intersection transverse
      |> get
  and seams = Solid.seams transverse |> get in
  check (Geometry.primitive_count intersection = 0)
    "transverse surface/surface intersection invented an area patch";
  check (Geometry.primitive_count (Seam.curves seams) > 0)
    "transverse surface/surface intersection lost its curve product"

let test_certified_rounding_repair () =
  let host = Box_generator.box ~grain:128 ~center:(Rays_math.Vec3.create 0. 0. 0.)
      ~rotation:(Rays_math.Vec3.create 0.08 (-0.13) 0.04)
      ~size:(Rays_math.Vec3.create 5.2 3.3 2.7)
      ~x_divisions:6 ~y_divisions:6 ~z_divisions:6
      ~connectivity:Box_generator.Box_quads ~consolidate_points:true () |> get in
  let cutters = merge_geometry (Array.init 7 (fun index ->
      let x = -2.2 +. (4.4 *. float_of_int index /. 6.) in
      torus ~major_segments:24 ~minor_segments:10 ~center_x:x
        ~major:0.42 ~minor:0.16)) in
  let run domains = Parallel.run ~domains (fun () ->
      let prepared = Solid.prepare ~grain:128 ~left:host ~right:cutters
          ~resolve_right_self_intersections:true () |> get in
      let ancestry = Solid.extract_product_with_ancestry
          ~defer_rounded_slivers:true ~operation:Solid.Intersection prepared |> get in
      let seam = Solid.seams ~grain:128 prepared |> get in
      let cleanup = Materialization.collapse_tiny_seams ~grain:128 ~threshold:0.
          ~require_closed:true ancestry seam (Extract.geometry ancestry) |> get in
      Materialization.cleanup_rounding_repaired_count cleanup,
      Materialization.cleanup_geometry cleanup |> signature) in
  let repaired, result = run 1 in
  check (repaired > 0) "rounding-repair fixture did not exercise a certified repair";
  let x, _, _, _, offsets = result in
  check (Array.length x = 2_258 && Array.length offsets = 4_541)
    "rounding repair changed its documented output cardinality";
  check ((repaired, result) = run 4)
    "certified rounding repair differs between one and four domains"

let run () =
  test_build_once_query_many ();
  test_domain_exactness ();
  test_shatter_products ();
  test_degenerate_rejection ();
  test_cancellation ();
  test_transaction_ancestry ();
  test_tiny_seam_candidate_policy ();
  test_rounded_surface_verifier ();
  test_detriangulation_policy ();
  test_open_sheet_preparation ();
  test_surface_surface_products ();
  test_certified_rounding_repair ()
