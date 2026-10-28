open Rdk_test_support
open Rays
open Rdk


let get_rdk = function
  | Ok value -> value
  | Error error -> fail (Error.to_string error)

let grid ?(columns = 24) ?(rows = 18) () =
  Plane_generators.grid ~counts:Plane_generators.Grid_point_counts
    ~connectivity:Plane_generators.Grid_alternating_triangles ~columns ~rows ~size:8. ()
  |> get_rdk

let topology_arrays geometry =
  let view = Topology.View.view (Geometry.topology geometry) in
  view.vertex_points, view.primitive_offsets, view.primitive_kinds

let same_geometry left right =
  let lp = Packed.Float3.View.view (Geometry.positions left)
  and rp = Packed.Float3.View.view (Geometry.positions right) in
  let lv, lo, lk = topology_arrays left and rv, ro, rk = topology_arrays right in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lv = rv && lo = ro && lk = rk
  && List.map (fun value -> Attribute.owner value, Attribute.name value,
       Attribute.payload_bytes value) (Geometry.attributes left)
     = List.map (fun value -> Attribute.owner value, Attribute.name value,
       Attribute.payload_bytes value) (Geometry.attributes right)
  && List.map (fun value -> Group.owner value, Group.name value,
       Group.cardinality value) (Geometry.groups left)
     = List.map (fun value -> Group.owner value, Group.name value,
       Group.cardinality value) (Geometry.groups right)

let test_reduces_and_preserves_boundary () =
  let source = grid () in
  let source_count = Geometry.primitive_count source in
  let output = Poly_reduce.run ~grain:17 ~target:(Poly_reduce.Reduce_ratio 0.5) source
      |> get_rdk in
  check (Geometry.primitive_count output < source_count)
    "PolyReduce did not reduce a dense triangle grid";
  check (Geometry.primitive_count output >= (source_count / 2) - 1)
    "PolyReduce overshot its target";
  let source_positions = Packed.Float3.View.view (Geometry.positions source)
  and output_positions = Packed.Float3.View.view (Geometry.positions output) in
  let boundary_coordinate x z = abs_float x = 4. || abs_float z = 4. in
  let source_boundary = ref [] and output_boundary = ref [] in
  for point = 0 to Geometry.point_count source - 1 do
    if boundary_coordinate source_positions.x.(point) source_positions.z.(point)
    then source_boundary := (source_positions.x.(point), source_positions.y.(point),
      source_positions.z.(point)) :: !source_boundary
  done;
  for point = 0 to Geometry.point_count output - 1 do
    if boundary_coordinate output_positions.x.(point) output_positions.z.(point)
    then output_boundary := (output_positions.x.(point), output_positions.y.(point),
      output_positions.z.(point)) :: !output_boundary
  done;
  check (List.sort compare !source_boundary = List.sort compare !output_boundary)
    "PolyReduce changed a preserved open boundary"

let test_original_positions_and_constraints () =
  let source = grid ~columns:18 ~rows:14 () in
  let positions = Packed.Float3.View.view (Geometry.positions source) in
  let hard = Group.init ~owner:Group.Point ~name:"pin"
      (Geometry.point_count source) (fun point -> point = 100) in
  let output = Poly_reduce.run ~grain:11 ~target:(Poly_reduce.Reduce_ratio 0.35)
      ~preserve_boundary:false ~hard_points:hard ~only_original_positions:true
      ~output_group:"reduced" source |> get_rdk in
  let reduced = Geometry.find_group ~owner:Group.Primitive "reduced" output
      |> Option.get in
  check (Group.cardinality reduced = Geometry.primitive_count output)
    "PolyReduce output group did not cover the operated surface";
  let output_positions = Packed.Float3.View.view (Geometry.positions output) in
  for point = 0 to Geometry.point_count output - 1 do
    let found = ref false in
    for source_point = 0 to Geometry.point_count source - 1 do
      if output_positions.x.(point) = positions.x.(source_point)
          && output_positions.y.(point) = positions.y.(source_point)
          && output_positions.z.(point) = positions.z.(source_point)
      then found := true
    done;
    check !found "PolyReduce original-position mode invented a position"
  done;
  let pinned = ref false in
  for point = 0 to Geometry.point_count output - 1 do
    if output_positions.x.(point) = positions.x.(100)
        && output_positions.y.(point) = positions.y.(100)
        && output_positions.z.(point) = positions.z.(100) then pinned := true
  done;
  check !pinned "PolyReduce removed a hard point"

let test_payload_group_and_edge_ancestry () =
  let base = grid ~columns:20 ~rows:16 () in
  let point_count = Geometry.point_count base
  and vertex_count = Geometry.vertex_count base
  and primitive_count = Geometry.primitive_count base in
  let point_id = Attribute.create_owned ~owner:Attribute.Point ~name:"point_id"
      (Attribute.Int (Array.init point_count Fun.id)) |> Result.get_ok
  and corner_id = Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner_id"
      (Attribute.Int (Array.init vertex_count Fun.id)) |> Result.get_ok
  and source_face = Attribute.create_owned ~owner:Attribute.Primitive
      ~name:"source_face" (Attribute.Int (Array.init primitive_count Fun.id))
      |> Result.get_ok
  and detail = Attribute.create_owned ~owner:Attribute.Detail ~name:"tag"
      (Attribute.Text [|"reduce-fixture"|]) |> Result.get_ok in
  let ordered = Group.ordered ~owner:Group.Point ~name:"ordered_points"
      ~length:point_count [|3;7;11;15;19|] |> Result.get_ok in
  let topology = Geometry.topology base in
  let index = Topology_index.create topology in
  let boundary = Edge_group.init ~topology ~index ~name:"boundary"
      (fun edge -> Topology_index.edge_incidence_count index edge = 1) in
  let source = Geometry.create ~positions:(Geometry.positions base) ~topology
      ~attributes:[point_id;corner_id;source_face;detail] ~groups:[ordered]
      ~edge_groups:[boundary] () |> Result.get_ok in
  let output = Poly_reduce.run ~grain:23 ~target:(Poly_reduce.Reduce_ratio 0.45)
      ~preserve_boundary:true source |> get_rdk in
  let attribute owner name = Geometry.find_attribute ~owner name output
      |> Option.get in
  check (Attribute.length (attribute Attribute.Point "point_id")
      = Geometry.point_count output
      && Attribute.length (attribute Attribute.Vertex "corner_id")
         = Geometry.vertex_count output
      && Attribute.length (attribute Attribute.Primitive "source_face")
         = Geometry.primitive_count output)
    "PolyReduce payload cardinality";
  check (Attribute.storage_id (attribute Attribute.Detail "tag")
      = Attribute.storage_id detail)
    "PolyReduce did not structurally share detail payload";
  (match Geometry.find_group ~owner:Group.Point "ordered_points" output with
   | Some group -> check (Group.is_ordered group)
       "PolyReduce lost ordered-group traversal"
   | None -> fail "PolyReduce dropped an ordered point group");
  (match Geometry.find_edge_group "boundary" output with
   | Some group -> check (Edge_group.cardinality group
         = Edge_group.cardinality boundary)
       "PolyReduce changed preserved boundary edge ancestry"
   | None -> fail "PolyReduce dropped native boundary ancestry")

let test_noop_invalid_and_cancel () =
  let source = grid ~columns:8 ~rows:7 () in
  check (Poly_reduce.run ~target:(Poly_reduce.Reduce_ratio 1.) source |> get_rdk == source)
    "PolyReduce ratio-one no-op lost geometry identity";
  let all_hard = Group.init ~owner:Group.Point ~name:"all_hard"
      (Geometry.point_count source) (fun _ -> true) in
  check (Poly_reduce.run ~target:(Poly_reduce.Reduce_ratio 0.) ~hard_points:all_hard
      source |> get_rdk == source)
    "fully constrained PolyReduce lost geometry identity";
  let wrong = Group.init ~owner:Group.Point ~name:"wrong"
      (Geometry.point_count source) (fun _ -> true) in
  check (match Poly_reduce.run ~primitives:wrong source with Error _ -> true
    | Ok _ -> false) "PolyReduce accepted a point-owned primitive selection";
  check (match Poly_reduce.run ~equalize_lengths:(-1.) source with
    | Error _ -> true | Ok _ -> false)
    "PolyReduce accepted a negative equal-length weight";
  check (match Poly_reduce.run ~max_normal_deviation:(Float.pi +. 0.01) source with
    | Error _ -> true | Ok _ -> false)
    "PolyReduce accepted an invalid normal-deviation limit";
  let curve = Line_geometry.polyline [|(0.,0.,0.); (1.,0.,0.); (2.,0.,0.)|] |> get_rdk in
  check (match Poly_reduce.run ~target:(Poly_reduce.Reduce_primitive_count 0) curve with
    | Error _ -> true | Ok _ -> false) "PolyReduce accepted curve topology";
  let topology = Geometry.topology source in
  let positions = Packed.Float3.View.view (Geometry.positions source) in
  let non_finite = Geometry.create
      ~positions:(Packed.Float3.View.of_owned_exn
        ~x:(Array.mapi (fun point value -> if point = 3 then Float.nan else value)
          positions.x)
        ~y:(Array.copy positions.y) ~z:(Array.copy positions.z))
      ~topology () |> Result.get_ok in
  check (match Poly_reduce.run ~target:(Poly_reduce.Reduce_ratio 0.5) non_finite with
    | Error _ -> true | Ok _ -> false)
    "PolyReduce accepted a non-finite point position";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  check (match Poly_reduce.run ~cancel:cancelled
      ~target:(Poly_reduce.Reduce_ratio 0.5) source with
    | Error error -> Error.code error = "cancelled"
    | Ok _ -> false) "PolyReduce cancellation did not reach the public boundary"

let test_parallel_exact () =
  let source = grid ~columns:80 ~rows:60 () in
  let one = Parallel.run ~domains:1 (fun () ->
    Poly_reduce.run ~grain:127 ~target:(Poly_reduce.Reduce_ratio 0.42)
      ~preserve_boundary:false source |> get_rdk) in
  let many = Parallel.run ~domains:4 (fun () ->
    Poly_reduce.run ~grain:127 ~target:(Poly_reduce.Reduce_ratio 0.42)
      ~preserve_boundary:false source |> get_rdk) in
  check (same_geometry one many)
    "PolyReduce one-domain/multi-domain geometry drift"

let test_extreme_coordinate_normalization () =
  let limit = Float.max_float in
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|-.limit;limit;limit;-.limit|] ~y:[|0.;0.;0.;0.|]
      ~z:[|-.limit;-.limit;limit;limit|] in
  let topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;1;2;0;2;3|] ~primitive_offsets:[|0;3;6|]
      |> Result.get_ok in
  let source = Geometry.create ~positions ~topology () |> Result.get_ok in
  let output = Poly_reduce.run ~target:(Poly_reduce.Reduce_primitive_count 1)
      ~preserve_boundary:false source |> get_rdk in
  let positions = Packed.Float3.View.view (Geometry.positions output) in
  for point = 0 to Geometry.point_count output - 1 do
    check (Float.is_finite positions.x.(point)
        && Float.is_finite positions.y.(point)
        && Float.is_finite positions.z.(point))
      "PolyReduce extreme-coordinate normalization produced non-finite output"
  done

let run () =
  test_reduces_and_preserves_boundary ();
  test_original_positions_and_constraints ();
  test_payload_group_and_edge_ancestry ();
  test_noop_invalid_and_cancel ();
  test_parallel_exact ();
  test_extreme_coordinate_normalization ();
  print_endline "poly reduce tests passed"
