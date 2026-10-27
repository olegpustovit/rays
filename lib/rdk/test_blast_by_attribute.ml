open Rdk
open Rdk_test_support

let with_attribute owner name storage geometry =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok
  |> fun attribute -> Geometry.with_attribute attribute geometry |> Result.get_ok

let group_members owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | None -> fail ("missing group " ^ name)
  | Some group ->
      let result = ref [] in
      Group.iter (fun element -> result := element :: !result) group;
      List.rev !result

let equal_storage left right =
  match Attribute.Private.storage left, Attribute.Private.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.Private.view left
      and right = Packed.Int_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.Private.view left
      and right = Packed.Float_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.Private.view left
      and right = Packed.Float2.Private.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.Private.view left
      and right = Packed.Float3.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.Private.view left
      and right = Packed.Float4.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | _ -> false

let equal_geometry left right =
  let left_positions = Packed.Float3.Private.view (Geometry.positions left)
  and right_positions = Packed.Float3.Private.view (Geometry.positions right)
  and left_topology = Topology.Private.view (Geometry.topology left)
  and right_topology = Topology.Private.view (Geometry.topology right) in
  left_positions.x = right_positions.x
  && left_positions.y = right_positions.y
  && left_positions.z = right_positions.z
  && left_topology.point_count = right_topology.point_count
  && left_topology.vertex_points = right_topology.vertex_points
  && left_topology.primitive_offsets = right_topology.primitive_offsets
  && Bytes.equal left_topology.primitive_kinds right_topology.primitive_kinds
  && List.equal (fun left right ->
      Attribute.owner left = Attribute.owner right
      && String.equal (Attribute.name left) (Attribute.name right)
      && equal_storage left right)
      (Geometry.attributes left) (Geometry.attributes right)
  && List.equal (fun left right ->
      Group.owner left = Group.owner right
      && String.equal (Group.name left) (Group.name right)
      && Group.ordered_elements left = Group.ordered_elements right
      && Group.length left = Group.length right
      && let equal = ref true in
         for element = 0 to Group.length left - 1 do
           if Group.mem element left <> Group.mem element right then equal := false
         done;
         !equal) (Geometry.groups left) (Geometry.groups right)
  && List.equal (fun left right ->
      String.equal (Edge_group.name left) (Edge_group.name right)
      && Edge_group.length left = Edge_group.length right
      && let equal = ref true in
         for edge = 0 to Edge_group.length left - 1 do
           if Edge_group.mem edge left <> Edge_group.mem edge right then equal := false
         done;
         !equal) (Geometry.edge_groups left) (Geometry.edge_groups right)

let point_source () =
  let geometry = Line_geometry.points (Array.init 6 (fun point -> float_of_int point, 0., 0.))
      |> with_attribute Attribute.Point "value"
           (Attribute.Float [|0.; 1.; 2.; 3.; 4.; 5.|])
      |> with_attribute Attribute.Point "class"
           (Attribute.Int [|0; 1; 2; 3; 4; 5|]) in
  let base = Group.init ~grain:1 ~owner:Group.Point ~name:"base" 6
      (fun point -> point = 1 || point = 2 || point = 4)
  and old = Group.init ~grain:1 ~owner:Group.Point ~name:"picked" 6
      (fun _ -> true) in
  geometry |> Geometry.with_group base |> Result.get_ok
  |> Geometry.with_group old |> Result.get_ok

let two_triangles () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 0.; 3.; 4.; 3.|]
      ~y:[|0.; 0.; 1.; 0.; 0.; 1.|] ~z:(Array.make 6 0.) in
  let builder = Rdk_test_support.Tb.create ~point_count:6 () in
  Rdk_test_support.Tb.add_triangle builder 0 1 2;
  Rdk_test_support.Tb.add_triangle builder 3 4 5;
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze builder) ()
  |> Result.get_ok
  |> with_attribute Attribute.Point "point_id"
       (Attribute.Int (Array.init 6 Fun.id))
  |> with_attribute Attribute.Vertex "corner_id"
       (Attribute.Int (Array.init 6 (fun index -> index + 10)))
  |> with_attribute Attribute.Primitive "class" (Attribute.Int [|1; 2|])
  |> with_attribute Attribute.Primitive "primitive_id" (Attribute.Int [|20; 21|])
  |> with_attribute Attribute.Detail "tag" (Attribute.Text [|"source"|])
  |> Group_mesh.group_edges ~grain:1 ~name:"source_edges" |> get_ok

let test_modes_groups_and_base () =
  let source = point_source () in
  let group mode = Blast_by_attribute.blast ~grain:1 ~owner:Blast_by_attribute.Blast_points
      ~attribute:"value" ~mode ~output:(Blast_by_attribute.Blast_group "picked") source
      |> get_ok in
  check (group_members Group.Point "picked" (group (Blast_by_attribute.Blast_below 3.))
      = [0; 1; 2]) "Blast by Attribute strict threshold";
  check (group_members Group.Point "picked"
      (group (Blast_by_attribute.Blast_range { minimum = 1.; maximum = 3. }))
      = [1; 2; 3]) "Blast by Attribute inclusive range";
  check (group_members Group.Point "picked"
      (group (Blast_by_attribute.Blast_width { center = 2.; width = 2. }))
      = [1; 2; 3]) "Blast by Attribute inclusive width";
  let base = Geometry.find_group ~owner:Group.Point "base" source |> Option.get in
  let inverted = Blast_by_attribute.blast ~grain:1 ~base ~invert:true
      ~owner:Blast_by_attribute.Blast_points ~attribute:"class" ~mode:(Blast_by_attribute.Blast_below 3.)
      ~output:(Blast_by_attribute.Blast_group "picked") source |> get_ok in
  check (group_members Group.Point "picked" inverted = [4])
    "Blast by Attribute invert escaped or ignored base group";
  let integer = Blast_by_attribute.blast ~grain:1 ~owner:Blast_by_attribute.Blast_points
      ~attribute:"class" ~mode:(Blast_by_attribute.Blast_range { minimum = 2.; maximum = 4. })
      ~output:(Blast_by_attribute.Blast_group "integer_pick") source |> get_ok in
  check (group_members Group.Point "integer_pick" integer = [2; 3; 4])
    "Blast by Attribute integer classification";
  let no_delete = Blast_by_attribute.blast ~grain:1 ~owner:Blast_by_attribute.Blast_points
      ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below (-1.))
      ~output:Blast_by_attribute.Blast_delete source |> get_ok in
  check (no_delete == source)
    "Blast by Attribute empty deletion lost structural identity";
  let empty = Line_geometry.points [||]
      |> with_attribute Attribute.Point "value" (Attribute.Float [||]) in
  let empty_group = Blast_by_attribute.blast ~owner:Blast_by_attribute.Blast_points
      ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below 0.)
      ~output:(Blast_by_attribute.Blast_group "empty") empty |> get_ok in
  check (group_members Group.Point "empty" empty_group = [])
    "Blast by Attribute empty cardinality"

let test_delete_and_shared_planner () =
  let source = two_triangles () in
  let point_values = Attribute.create_owned ~owner:Attribute.Point ~name:"kill"
      (Attribute.Int [|1; 0; 0; 0; 0; 0|]) |> Result.get_ok in
  let source = Geometry.with_attribute point_values source |> Result.get_ok in
  let point_output = Blast_by_attribute.blast ~grain:1 ~owner:Blast_by_attribute.Blast_points
      ~attribute:"kill" ~mode:(Blast_by_attribute.Blast_range { minimum = 1.; maximum = 1. })
      ~output:Blast_by_attribute.Blast_delete source |> get_ok in
  check (Geometry.point_count point_output = 5
      && Geometry.primitive_count point_output = 1)
    "Blast by Attribute point deletion topology";
  let selection = Group.init ~grain:1 ~owner:Group.Primitive
      ~name:"expected" 2 (fun primitive -> primitive = 0) in
  let expected = Deletion.delete ~grain:1 ~compact_points:true selection source |> get_ok
  and actual = Blast_by_attribute.blast ~grain:1 ~remove_unused_points:true
      ~owner:Blast_by_attribute.Blast_primitives ~attribute:"class"
      ~mode:(Blast_by_attribute.Blast_below 2.) ~output:Blast_by_attribute.Blast_delete source |> get_ok in
  check (equal_geometry expected actual)
    "Blast by Attribute diverged from authoritative Delete remapping";
  check (Geometry.point_count actual = 3 && Geometry.primitive_count actual = 1
      && Geometry.find_edge_group "source_edges" actual <> None)
    "Blast by Attribute primitive compaction/native-edge ancestry"

let expect_invalid work message = match work () with
  | Error error -> check (Error.code error = "invalid_blast") message
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let test_errors_and_cancellation () =
  let source = point_source () in
  expect_invalid (fun () -> Blast_by_attribute.blast ~owner:Blast_by_attribute.Blast_points
      ~attribute:"" ~mode:(Blast_by_attribute.Blast_below 0.) ~output:Blast_by_attribute.Blast_delete source)
    "empty Blast attribute name";
  expect_invalid (fun () -> Blast_by_attribute.blast ~owner:Blast_by_attribute.Blast_points
      ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below 0.)
      ~output:(Blast_by_attribute.Blast_group "") source) "empty Blast output group";
  expect_invalid (fun () -> Blast_by_attribute.blast ~grain:0
      ~owner:Blast_by_attribute.Blast_points ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below 0.)
      ~output:Blast_by_attribute.Blast_delete source) "zero Blast grain";
  expect_invalid (fun () -> Blast_by_attribute.blast ~owner:Blast_by_attribute.Blast_points
      ~attribute:"missing" ~mode:(Blast_by_attribute.Blast_below 0.)
      ~output:Blast_by_attribute.Blast_delete source) "missing Blast attribute";
  let wrong = with_attribute Attribute.Point "vector"
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:[|0.; 1.; 2.; 3.; 4.; 5.|] ~y:(Array.make 6 0.)
        ~z:(Array.make 6 0.))) source in
  expect_invalid (fun () -> Blast_by_attribute.blast ~owner:Blast_by_attribute.Blast_points
      ~attribute:"vector" ~mode:(Blast_by_attribute.Blast_below 0.)
      ~output:Blast_by_attribute.Blast_delete wrong) "vector Blast attribute";
  expect_invalid (fun () -> Blast_by_attribute.blast ~owner:Blast_by_attribute.Blast_primitives
      ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below 0.)
      ~output:Blast_by_attribute.Blast_delete source) "wrong-owner Blast attribute";
  List.iter (fun mode -> expect_invalid (fun () ->
      Blast_by_attribute.blast ~owner:Blast_by_attribute.Blast_points ~attribute:"value" ~mode
        ~output:Blast_by_attribute.Blast_delete source) "invalid Blast mode")
    [Blast_by_attribute.Blast_below Float.nan;
     Blast_by_attribute.Blast_range { minimum = 2.; maximum = 1. };
     Blast_by_attribute.Blast_width { center = 0.; width = -1. };
     Blast_by_attribute.Blast_width { center = max_float; width = max_float }];
  let wrong_owner = Group.init ~owner:Group.Primitive ~name:"wrong" 0
      (fun _ -> false) in
  expect_invalid (fun () -> Blast_by_attribute.blast ~base:wrong_owner
      ~owner:Blast_by_attribute.Blast_points ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below 0.)
      ~output:Blast_by_attribute.Blast_delete source) "wrong-owner Blast base";
  let wrong_length = Group.init ~owner:Group.Point ~name:"wrong" 7
      (fun _ -> false) in
  expect_invalid (fun () -> Blast_by_attribute.blast ~base:wrong_length
      ~owner:Blast_by_attribute.Blast_points ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below 0.)
      ~output:Blast_by_attribute.Blast_delete source) "wrong-length Blast base";
  expect_invalid (fun () -> Blast_by_attribute.blast ~remove_unused_points:true
      ~owner:Blast_by_attribute.Blast_points ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below 0.)
      ~output:Blast_by_attribute.Blast_delete source) "point unused-point removal";
  expect_invalid (fun () -> Blast_by_attribute.blast ~remove_unused_points:true
      ~owner:Blast_by_attribute.Blast_primitives ~attribute:"missing"
      ~mode:(Blast_by_attribute.Blast_below 0.) ~output:(Blast_by_attribute.Blast_group "picked") source)
    "group-output unused-point removal";
  let non_finite = Line_geometry.points [|0., 0., 0.; 1., 0., 0.; 2., 0., 0.|]
      |> with_attribute Attribute.Point "value"
           (Attribute.Float [|Float.nan; 1.; Float.infinity|]) in
  let all = Group.init ~owner:Group.Point ~name:"all" 3 (fun _ -> true) in
  let error domains = Parallel.run ~domains (fun () ->
      Blast_by_attribute.blast ~grain:1 ~base:all ~owner:Blast_by_attribute.Blast_points
        ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below 2.)
        ~output:(Blast_by_attribute.Blast_group "picked") non_finite) in
  List.iter (fun domains -> match error domains with
    | Error error -> check (Error.code error = "invalid_blast"
          && String.contains (Error.message error) '0')
        "non-finite Blast diagnostic"
    | Ok _ -> fail "Blast accepted non-finite operated attribute") [1; 4];
  let safe_base = Group.init ~owner:Group.Point ~name:"safe" 3
      (fun point -> point = 1) in
  (* The base keeps the non-finite values out: only point 1 is read, and 1 < 2. *)
  check (group_members Group.Point "picked"
      (Blast_by_attribute.blast ~base:safe_base ~owner:Blast_by_attribute.Blast_points
        ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below 2.)
        ~output:(Blast_by_attribute.Blast_group "picked") non_finite |> get_ok) = [1])
    "Blast read a non-finite value outside its base group";
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Blast_by_attribute.blast ~cancel ~owner:Blast_by_attribute.Blast_points
      ~attribute:"value" ~mode:(Blast_by_attribute.Blast_below 2.)
      ~output:Blast_by_attribute.Blast_delete source with
   | Error error -> check (Error.code error = "cancelled")
       "Blast cancellation code"
   | Ok _ -> fail "cancelled Blast published geometry")

let test_dense_parallel_exactness () =
  let source = Plane_generators.grid ~columns:480 ~rows:300 ~size:20. () |> get_ok in
  let point_count = Geometry.point_count source
  and primitive_count = Geometry.primitive_count source in
  let source = source
      |> with_attribute Attribute.Point "density"
           (Attribute.Float (Array.init point_count (fun point ->
             float_of_int (point mod 1_009) /. 1_008.)))
      |> with_attribute Attribute.Primitive "class"
           (Attribute.Int (Array.init primitive_count (fun primitive ->
             (primitive * 17) mod 1_009))) in
  let run_point domains = Parallel.run ~domains (fun () ->
      Blast_by_attribute.blast ~grain:257 ~owner:Blast_by_attribute.Blast_points
        ~attribute:"density"
        ~mode:(Blast_by_attribute.Blast_range { minimum = 0.35; maximum = 0.65 })
        ~output:(Blast_by_attribute.Blast_group "picked") source |> get_ok) in
  let run_primitive domains = Parallel.run ~domains (fun () ->
      Blast_by_attribute.blast ~grain:257 ~remove_unused_points:true
        ~owner:Blast_by_attribute.Blast_primitives ~attribute:"class"
        ~mode:(Blast_by_attribute.Blast_below 400.) ~output:Blast_by_attribute.Blast_delete source |> get_ok) in
  check (equal_geometry (run_point 1) (run_point 4))
    "dense Blast group one/four-domain exactness";
  check (equal_geometry (run_primitive 1) (run_primitive 4))
    "dense Blast delete one/four-domain exactness"

let run () =
  test_modes_groups_and_base ();
  test_delete_and_shared_planner ();
  test_errors_and_cancellation ();
  test_dense_parallel_exactness ();
  print_endline "blast by attribute tests passed"
