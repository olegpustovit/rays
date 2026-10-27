open Rdk
open Rdk_test_support

let group owner name length members =
  let builder = Group.Builder.create ~owner ~name length in
  List.iter (fun member -> Group.Builder.set builder member true) members;
  Group.Builder.freeze builder

let add ~owner ~name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> Result.get_ok in
  Geometry.with_attribute attribute geometry |> Result.get_ok

let source ?(free_point = false) () =
  let point_count = if free_point then 8 else 7 in
  let topology = Rdk_test_support.Tb.create ~point_count () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  Rdk_test_support.Tb.add_triangle topology 2 1 3;
  Rdk_test_support.Tb.add_open_polyline topology [|4;5;6|];
  let topology = Rdk_test_support.Tb.freeze topology in
  let hard_index = Topology_index.create topology in
  let hard = Edge_group.init ~topology ~index:hard_index ~name:"hard"
      (fun edge ->
        let a, b = Topology_index.edge_points hard_index edge in
        a = 0 && b = 1) in
  let geometry = Geometry.create
      ~positions:(Packed.Float3.Private.of_owned_exn
        ~x:(Array.init point_count float_of_int)
        ~y:(Array.make point_count 0.) ~z:(Array.make point_count 0.))
      ~topology ~edge_groups:[hard] () |> Result.get_ok in
  geometry
  |> add ~owner:Attribute.Point ~name:"targetpt"
       (Attribute.Int (if free_point then [|1;-1;-1;-1;-1;-1;-1;-1|]
         else [|3;-1;4;-1;5;6;-1|]))
  |> add ~owner:Attribute.Vertex ~name:"targetv"
       (Attribute.Int [|3;-1;-1;-1;-1;-1;6;-1;-1|])
  |> add ~owner:Attribute.Primitive ~name:"targetprim"
       (Attribute.Int [|4;-1;0|])
  |> add ~owner:Attribute.Point ~name:"weight"
       (Attribute.Float (Array.init point_count (fun point ->
          0.25 +. float_of_int point)))
  |> add ~owner:Attribute.Point ~name:"rows"
       (Attribute.Int_array (Packed.Int_array.create_owned
         ~offsets:(Array.init (point_count + 1) Fun.id)
         ~values:(Array.init point_count (fun point -> 100 + point))
         |> Result.get_ok))
  |> add ~owner:Attribute.Point ~name:"N"
       (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
         ~x:(Array.make point_count 0.) ~y:(Array.make point_count 1.)
         ~z:(Array.make point_count 0.)))
  |> add ~owner:Attribute.Vertex ~name:"N"
       (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
         ~x:(Array.make 9 0.) ~y:(Array.make 9 1.) ~z:(Array.make 9 0.)))
  |> Geometry.with_group (group Group.Point "marked" point_count
       (if free_point then [0;7] else [0]))
       |> Result.get_ok

let vertex_points geometry =
  (Topology.Private.view (Geometry.topology geometry)).vertex_points

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing attribute " ^ name)

let test_point_rewire_and_edge_ancestry () =
  let geometry = source () in
  let selected = group Group.Point "selected" 7 [0] in
  let output = Rewire_vertices.run
      ~selection:(Transform_ops.Selected_points selected) ~keep_unused_points:true
      ~original_point_attribute:"origpt" ~owner:Attribute.Point
      ~target_attribute:"targetpt" geometry |> get_ok in
  check (vertex_points output = [|3;1;2;2;1;3;4;5;6|])
    "Rewire Vertices point target output";
  check (int_attribute Attribute.Vertex "origpt" output
      = [|0;1;2;2;1;3;4;5;6|])
    "Rewire Vertices original-point provenance";
  check (Geometry.find_attribute ~owner:Attribute.Point "N" output = None
      && Geometry.find_attribute ~owner:Attribute.Vertex "N" output = None)
    "Rewire Vertices retained stale normals";
  let hard = Geometry.find_edge_group "hard" output |> Option.get
  and index = Topology_index.create (Geometry.topology output) in
  let edge = Topology_index.find_edge_index index ~a:1 ~b:3 in
  check (edge >= 0 && Edge_group.mem edge hard
      && Edge_group.topology_data_id hard = Topology.data_id (Geometry.topology output))
    "Rewire Vertices native edge ancestry"

let test_selection_promotion () =
  let geometry = source () in
  let selected_vertex = group Group.Vertex "one_corner" 9 [2] in
  let output = Rewire_vertices.run
      ~selection:(Transform_ops.Selected_vertices selected_vertex)
      ~keep_unused_points:true ~owner:Attribute.Point
      ~target_attribute:"targetpt" geometry |> get_ok in
  check (vertex_points output = [|0;1;4;4;1;3;4;5;6|])
    "Rewire Vertices did not promote vertex selection to point owner";
  let selected_vertex = group Group.Vertex "one_vertex" 9 [0] in
  let output = Rewire_vertices.run
      ~selection:(Transform_ops.Selected_vertices selected_vertex)
      ~keep_unused_points:true ~owner:Attribute.Vertex
      ~target_attribute:"targetv" geometry |> get_ok in
  check (vertex_points output = [|3;1;2;2;1;3;4;5;6|])
    "Rewire Vertices vertex target selection";
  let primitive = group Group.Primitive "first" 3 [0] in
  let output = Rewire_vertices.run
      ~selection:(Transform_ops.Selected_primitives primitive)
      ~keep_unused_points:true ~owner:Attribute.Primitive
      ~target_attribute:"targetprim" geometry |> get_ok in
  check (vertex_points output = [|4;4;4;2;1;3;4;5;6|])
    "Rewire Vertices primitive target selection";
  let hard = Geometry.find_edge_group "hard" geometry |> Option.get in
  let output = Rewire_vertices.run ~selection:(Transform_ops.Selected_edges hard)
      ~keep_unused_points:true ~owner:Attribute.Point
      ~target_attribute:"targetpt" geometry |> get_ok in
  check (vertex_points output = [|3;1;2;2;1;3;4;5;6|])
    "Rewire Vertices edge-to-point selection promotion"

let test_recursive_chains_and_cycles () =
  let geometry = source ()
      |> add ~owner:Attribute.Point ~name:"chain"
           (Attribute.Int [|1;2;3;-1;5;4;4|]) in
  let output = Rewire_vertices.run ~recursive:true ~keep_unused_points:true
      ~owner:Attribute.Point ~target_attribute:"chain" geometry |> get_ok in
  check (vertex_points output = [|3;3;3;3;3;3;4;5;4|])
    "Rewire Vertices recursive chain/cycle policy"

let test_cleanup_and_payload () =
  let geometry = source ~free_point:true () in
  let selected = group Group.Point "selected" 8 [0] in
  let output = Rewire_vertices.run ~selection:(Transform_ops.Selected_points selected)
      ~owner:Attribute.Point ~target_attribute:"targetpt" geometry |> get_ok in
  check (Geometry.point_count output = 7
      && vertex_points output = [|0;0;1;1;0;2;3;4;5|])
    "Rewire Vertices newly-unused cleanup";
  let positions = Packed.Float3.Private.view (Geometry.positions output) in
  check (positions.x = [|1.;2.;3.;4.;5.;6.;7.|])
    "Rewire Vertices did not preserve an originally free point";
  let weight = match Geometry.find_attribute ~owner:Attribute.Point "weight" output with
    | Some attribute ->
        (match Attribute.Private.storage attribute with
         | Attribute.Float values -> values | _ -> assert false)
    | None -> assert false in
  check (weight = [|1.25;2.25;3.25;4.25;5.25;6.25;7.25|])
    "Rewire Vertices fixed-width point payload compaction";
  let rows = match Geometry.find_attribute ~owner:Attribute.Point "rows" output with
    | Some attribute ->
        (match Attribute.Private.storage attribute with
         | Attribute.Int_array values -> Packed.Int_array.Private.view values
         | _ -> assert false)
    | None -> assert false in
  check (rows.offsets = [|0;1;2;3;4;5;6;7|]
      && rows.values = [|101;102;103;104;105;106;107|])
    "Rewire Vertices ragged point payload compaction";
  let marked = Geometry.find_group ~owner:Group.Point "marked" output
      |> Option.get in
  check (Group.cardinality marked = 1 && Group.mem 6 marked)
    "Rewire Vertices point group compaction"

let test_delete_noop_and_errors () =
  let geometry = source () in
  let no_targets = geometry |> add ~owner:Attribute.Point ~name:"none"
      (Attribute.Int (Array.make 7 (-1))) in
  let unchanged = Rewire_vertices.run ~keep_unused_points:true
      ~owner:Attribute.Point ~target_attribute:"none" no_targets |> get_ok in
  check (unchanged == no_targets) "Rewire Vertices missed identity fast path";
  let metadata = Rewire_vertices.run ~keep_unused_points:true
      ~delete_target_attribute:true ~original_point_attribute:"orig"
      ~owner:Attribute.Point ~target_attribute:"none" no_targets |> get_ok in
  check (Geometry.topology metadata == Geometry.topology no_targets
      && Geometry.find_attribute ~owner:Attribute.Point "none" metadata = None
      && int_attribute Attribute.Vertex "orig" metadata
         = [|0;1;2;2;1;3;4;5;6|])
    "Rewire Vertices metadata-only output";
  let expect code work = match work () with
    | Error error -> check (Error.code error = code)
        "Rewire Vertices wrong structured error"
    | Ok _ -> fail "Rewire Vertices accepted malformed input" in
  expect "invalid_rewire_vertices" (fun () -> Rewire_vertices.run ~grain:0
    ~owner:Attribute.Point ~target_attribute:"targetpt" geometry);
  expect "invalid_rewire_vertices" (fun () -> Rewire_vertices.run
    ~owner:Attribute.Point ~target_attribute:"missing" geometry);
  expect "invalid_rewire_vertices" (fun () -> Rewire_vertices.run
    ~owner:Attribute.Point ~target_attribute:"weight" geometry);
  expect "invalid_rewire_vertices" (fun () -> Rewire_vertices.run ~recursive:true
    ~owner:Attribute.Vertex ~target_attribute:"targetv" geometry);
  expect "invalid_rewire_vertices" (fun () -> Rewire_vertices.run
    ~owner:Attribute.Detail ~target_attribute:"targetpt" geometry);
  let wrong = group Group.Point "wrong" 2 [] in
  expect "invalid_rewire_vertices" (fun () -> Rewire_vertices.run
    ~selection:(Transform_ops.Selected_points wrong) ~owner:Attribute.Point
    ~target_attribute:"targetpt" geometry);
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  expect "cancelled" (fun () -> Rewire_vertices.run ~cancel
    ~owner:Attribute.Point ~target_attribute:"targetpt" geometry)

let scale_source count =
  let count = count - (count mod 3) in
  let primitive_count = count / 3 in
  let topology = Topology.polygons_owned ~point_count:count
      ~vertex_points:(Array.init count Fun.id)
      ~primitive_offsets:(Array.init (primitive_count + 1) (fun primitive ->
        primitive * 3)) |> Result.get_ok in
  Geometry.create
    ~positions:(Packed.Float3.Private.of_owned_exn
      ~x:(Array.init count float_of_int) ~y:(Array.make count 0.)
      ~z:(Array.make count 0.)) ~topology () |> Result.get_ok
  |> add ~owner:Attribute.Point ~name:"target"
       (Attribute.Int (Array.init count (fun point ->
          if point mod 3 = 0 then point + 1 else -1)))

let test_parallel_exactness () =
  let geometry = scale_source 300_000 in
  let run domains = Parallel.run ~domains (fun () ->
    Rewire_vertices.run ~grain:257 ~keep_unused_points:true
      ~original_point_attribute:"orig" ~owner:Attribute.Point
      ~target_attribute:"target" geometry |> get_ok) in
  let one = run 1 and four = run 4 in
  check (vertex_points one = vertex_points four
      && int_attribute Attribute.Vertex "orig" one
         = int_attribute Attribute.Vertex "orig" four)
    "Rewire Vertices differs across domain counts";
  let count = Geometry.point_count geometry in
  check (vertex_points one
         = Array.init count (fun p -> if p mod 3 = 0 then p + 1 else p)
      && int_attribute Attribute.Vertex "orig" one = Array.init count Fun.id)
    "Rewire Vertices target/original-point values";
  check (Geometry.point_count one = Geometry.point_count geometry
      && Geometry.primitive_count one = Geometry.primitive_count geometry)
    "Rewire Vertices scale cardinality"

let run () =
  test_point_rewire_and_edge_ancestry ();
  test_selection_promotion ();
  test_recursive_chains_and_cycles ();
  test_cleanup_and_payload ();
  test_delete_noop_and_errors ();
  test_parallel_exactness ();
  print_endline "Rewire Vertices tests passed"
