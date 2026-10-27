open Rays
open Rdk

let fail message = prerr_endline ("test_duplicate: " ^ message); exit 1
let check condition message = if not condition then fail message
let get = function Ok value -> value | Error error -> fail (Error.to_string error)
let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> Result.get_ok

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value -> (match Attribute.Private.storage value with
      | Attribute.Int values -> values
      | _ -> fail (name ^ " storage"))
  | None -> fail ("missing " ^ name)

let text_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some value -> (match Attribute.Private.storage value with
      | Attribute.Text values -> values
      | _ -> fail (name ^ " storage"))
  | None -> fail ("missing " ^ name)

let fixture () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;1.;2.;0.;1.;2.;9.|] ~y:[|0.;0.;0.;1.;1.;1.;9.|]
      ~z:(Array.make 7 0.) in
  let topology = Topology.polygons_owned ~point_count:7
      ~vertex_points:[|0;1;4;3; 1;2;5;4|]
      ~primitive_offsets:[|0;4;8|] |> Result.get_ok in
  let geometry = Geometry.create ~positions ~topology
      ~attributes:[
        attribute Attribute.Point "id" (Attribute.Int [|10;11;12;13;14;15;16|]);
        attribute Attribute.Point "weights" (Attribute.Float_array
          (Packed.Float_array.create_owned
            ~offsets:[|0;1;3;3;4;6;7;9|]
            ~values:[|0.;1.;1.5;3.;4.;4.5;5.;6.;6.5|] |> Result.get_ok));
        attribute Attribute.Point "N" (Attribute.Float3
          (Packed.Float3.Private.of_owned_exn ~x:(Array.make 7 0.)
            ~y:(Array.make 7 0.) ~z:(Array.make 7 1.)));
        attribute Attribute.Vertex "corner"
          (Attribute.Int [|100;101;102;103;200;201;202;203|]);
        attribute Attribute.Vertex "links" (Attribute.Int_array
          (Packed.Int_array.create_owned
            ~offsets:[|0;1;1;3;4;5;7;8;10|]
            ~values:[|0;2;20;3;4;5;50;6;7;70|] |> Result.get_ok));
        attribute Attribute.Primitive "piece" (Attribute.Text [|"left";"right"|]);
        attribute Attribute.Detail "author" (Attribute.Text [|"duplicate"|])]
      ~groups:[
        Group.ordered ~owner:Group.Point ~name:"path" ~length:7 [|6;1;2;5|]
          |> Result.get_ok;
        Group.ordered ~owner:Group.Vertex ~name:"corners" ~length:8 [|7;4;5|]
          |> Result.get_ok;
        Group.init ~owner:Group.Primitive ~name:"right" 2
          (fun primitive -> primitive = 1);
        Group.init ~owner:Group.Primitive ~name:"copy_1" 2
          (fun primitive -> primitive = 0)] () |> Result.get_ok in
  let geometry = Group_mesh.group_edges ~grain:1 ~name:"all_edges" geometry |> get in
  let index = Topology_index.create (Geometry.topology geometry) in
  let shared = Edge_group.init ~topology:(Geometry.topology geometry) ~index
      ~name:"shared_edge" (fun edge ->
        let a, b = Topology_index.edge_points index edge in
        a = 1 && b = 4) in
  Geometry.with_edge_group shared geometry |> Result.get_ok

let group owner name geometry =
  Geometry.find_group ~owner name geometry |> Option.get

let test_restricted_payload_and_groups () =
  let source = fixture () and move = Rays_math.Mat4.translation (Rays_math.Vec3.create 10. 0. 0.) in
  let right = group Group.Primitive "right" source in
  let output = Instance_copy.duplicate ~grain:1 ~copies:2 ~transform:move
      ~primitives:right ~copy_group_prefix:"copy_" source |> get in
  let topology = Topology.Private.view (Geometry.topology output)
  and positions = Packed.Float3.Private.view (Geometry.positions output) in
  check (Geometry.point_count output = 15 && Geometry.vertex_count output = 16
      && Geometry.primitive_count output = 4)
    "restricted cardinality";
  check (topology.vertex_points
      = [|0;1;4;3; 1;2;5;4; 7;8;10;9; 11;12;14;13|])
    "restricted copy-major topology";
  check (positions.x = [|0.;1.;2.;0.;1.;2.;9.; 11.;12.;11.;12.;
                         21.;22.;21.;22.|])
    "cumulative selected-point transforms";
  check (int_attribute Attribute.Point "id" output
      = [|10;11;12;13;14;15;16; 11;12;14;15; 11;12;14;15|]
      && int_attribute Attribute.Vertex "corner" output
      = [|100;101;102;103;200;201;202;203;
          200;201;202;203;200;201;202;203|])
    "fixed payload ancestry";
  let weights = match Geometry.find_attribute ~owner:Attribute.Point "weights"
      output with
    | Some value -> (match Attribute.Private.storage value with
        | Attribute.Float_array values -> values | _ -> fail "weights storage")
    | None -> fail "missing weights"
  and links = match Geometry.find_attribute ~owner:Attribute.Vertex "links"
      output with
    | Some value -> (match Attribute.Private.storage value with
        | Attribute.Int_array values -> values | _ -> fail "links storage")
    | None -> fail "missing links" in
  check (Packed.Float_array.get weights 7 = [|1.;1.5|]
      && Packed.Float_array.get weights 14 = [|5.|]
      && Packed.Int_array.get links 8 = [|4|]
      && Packed.Int_array.get links 15 = [|7;70|])
    "ragged payload ancestry";
  check (text_attribute Attribute.Primitive "piece" output
      = [|"left";"right";"right";"right"|]
      && text_attribute Attribute.Detail "author" output = [|"duplicate"|])
    "primitive/detail payload ancestry";
  let path = group Group.Point "path" output
  and corners = group Group.Vertex "corners" output in
  check (Group.ordered_elements path = Some [|6;1;2;5;7;8;10;11;12;14|]
      && Group.ordered_elements corners = Some [|7;4;5;11;8;9;15;12;13|])
    "copy-major ordered-group ancestry";
  let edges = Geometry.find_edge_group "all_edges" output |> Option.get in
  check (Edge_group.length edges = 15 && Edge_group.cardinality edges = 15)
    "native-edge ancestry";
  let shared = Geometry.find_edge_group "shared_edge" output |> Option.get
  and output_index = Topology_index.create (Geometry.topology output) in
  let shared_edges = [|
    Topology_index.find_edge_index output_index ~a:1 ~b:4;
    Topology_index.find_edge_index output_index ~a:7 ~b:9;
    Topology_index.find_edge_index output_index ~a:11 ~b:13|] in
  check (Edge_group.cardinality shared = 3
      && Array.for_all (fun edge -> edge >= 0 && Edge_group.mem edge shared)
           shared_edges)
    "partial native-edge ancestry";
  let first = group Group.Primitive "copy_1" output
  and second = group Group.Primitive "copy_2" output in
  check (Group.cardinality first = 1 && Group.mem 2 first
      && Group.cardinality second = 1 && Group.mem 3 second)
    "per-copy output groups replace collisions";
  let preserved = Instance_copy.duplicate ~grain:1 ~copies:2 ~transform:move
      ~primitives:right ~copy_group_prefix:"copy_" ~preserve_groups:true source
      |> get in
  let first = group Group.Primitive "copy_1" preserved in
  check (Group.cardinality first = 2 && Group.mem 0 first && Group.mem 2 first)
    "per-copy output groups preserve collisions";
  let singular = Instance_copy.duplicate ~primitives:right
      ~transform:(Rays_math.Mat4.scaling (Rays_math.Vec3.create 1. 0. 1.)) source |> get in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" singular = None)
    "singular selected transform normal invalidation";
  let untyped_n = Geometry.with_attribute
      (attribute Attribute.Point "N" (Attribute.Int (Array.make 7 9))) source
      |> Result.get_ok in
  let untyped_n = Instance_copy.duplicate ~primitives:right
      ~transform:(Rays_math.Mat4.scaling (Rays_math.Vec3.create 1. 0. 1.)) untyped_n |> get in
  check (int_attribute Attribute.Point "N" untyped_n
      = [|9;9;9;9;9;9;9;9;9;9;9|])
    "singular transform retained non-normal N storage";
  let non_cumulative = Instance_copy.duplicate ~copies:2 ~cumulative:false ~transform:move
      ~primitives:right source |> get in
  let positions = Packed.Float3.Private.view
      (Geometry.positions non_cumulative) in
  check (positions.x.(7) = 11. && positions.x.(11) = 11.)
    "non-cumulative restricted transform"

let test_identity_and_errors () =
  let source = fixture () in
  let right = group Group.Primitive "right" source in
  check (Instance_copy.duplicate ~copies:0 ~primitives:right source |> get == source)
    "zero-copy identity";
  let empty = Group.init ~owner:Group.Primitive ~name:"empty" 2
      (fun _ -> false) in
  check (Instance_copy.duplicate ~copies:3 ~primitives:empty source |> get == source)
    "empty-selection identity";
  let expect label = function
    | Error _ -> () | Ok _ -> fail ("accepted " ^ label) in
  expect "point source selection"
    (Instance_copy.duplicate ~primitives:(group Group.Point "path" source) source);
  expect "wrong-length source selection"
    (Instance_copy.duplicate ~primitives:(Group.init ~owner:Group.Primitive ~name:"short" 1
      (fun _ -> true)) source);
  expect "empty copy-group prefix"
    (Instance_copy.duplicate ~copy_group_prefix:" " source);
  expect "copy-group count bound"
    (Instance_copy.duplicate ~copies:4_097 ~primitives:empty ~copy_group_prefix:"copy_"
      source);
  let many_primitives = 524_289 in
  let topology = Topology.polygons_owned ~point_count:3
      ~vertex_points:(Array.init (many_primitives * 3) (fun vertex -> vertex mod 3))
      ~primitive_offsets:(Array.init (many_primitives + 1)
        (fun primitive -> primitive * 3)) |> Result.get_ok in
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;1.;0.|] ~y:[|0.;0.;1.|] ~z:[|0.;0.;0.|] in
  let dense = Geometry.create ~positions ~topology () |> Result.get_ok in
  let dense_empty = Group.init ~owner:Group.Primitive ~name:"empty"
      many_primitives (fun _ -> false) in
  expect "copy-group payload bound"
    (Instance_copy.duplicate ~copies:4_096 ~primitives:dense_empty
      ~copy_group_prefix:"copy_" dense);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Instance_copy.duplicate ~cancel:cancelled ~copies:3 ~primitives:right source with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail ("unexpected cancellation: " ^ Error.to_string error)
   | Ok _ -> fail "ignored cancellation")

let equal_geometry left right =
  let lp = Packed.Float3.Private.view (Geometry.positions left)
  and rp = Packed.Float3.Private.view (Geometry.positions right)
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && int_attribute Attribute.Point "id" left
      = int_attribute Attribute.Point "id" right
  && int_attribute Attribute.Vertex "corner" left
      = int_attribute Attribute.Vertex "corner" right
  && let lg = group Group.Point "marked" left
     and rg = group Group.Point "marked" right in
     Group.ordered_elements lg = Group.ordered_elements rg
     && let equal_groups =
          let left = Geometry.groups left and right = Geometry.groups right in
          List.length left = List.length right
          && List.for_all2 (fun left right ->
            Group.owner left = Group.owner right
            && String.equal (Group.name left) (Group.name right)
            && Group.length left = Group.length right
            && Group.ordered_elements left = Group.ordered_elements right
            && let same = ref true in
               for element = 0 to Group.length left - 1 do
                 if Group.mem element left <> Group.mem element right then
                   same := false
               done;
               !same) left right in
        equal_groups
     && let le = Geometry.find_edge_group "all_edges" left |> Option.get
        and re = Geometry.find_edge_group "all_edges" right |> Option.get in
        Edge_group.length le = Edge_group.length re
        && let same = ref true in
           for edge = 0 to Edge_group.length le - 1 do
             if Edge_group.mem edge le <> Edge_group.mem edge re then same := false
           done;
           !same

let test_parallel_exact () =
  let source = Plane_generators.grid ~columns:400 ~rows:250 ~size:20. () |> get in
  let point_count = Geometry.point_count source
  and vertex_count = Geometry.vertex_count source
  and primitive_count = Geometry.primitive_count source in
  let source = Geometry.create ~positions:(Geometry.positions source)
      ~topology:(Geometry.topology source)
      ~attributes:[
        attribute Attribute.Point "id" (Attribute.Int (Array.init point_count Fun.id));
        attribute Attribute.Vertex "corner"
          (Attribute.Int (Array.init vertex_count (fun vertex -> vertex land 3)))]
      ~groups:[Group.ordered ~owner:Group.Point ~name:"marked"
        ~length:point_count
        (Array.init ((point_count + 16) / 17) (fun index -> index * 17)
          |> Array.to_list |> List.filter (fun point -> point < point_count)
          |> Array.of_list) |> Result.get_ok] () |> Result.get_ok in
  let selected = Group.init ~owner:Group.Primitive ~name:"selected"
      primitive_count (fun primitive -> primitive land 1 = 0) in
  let source = Geometry.with_group selected source |> Result.get_ok
      |> Group_mesh.group_edges ~name:"all_edges" |> get in
  let cook domains = Parallel.run ~domains (fun () ->
      Instance_copy.duplicate ~grain:4_096 ~copies:4 ~primitives:selected
        ~copy_group_prefix:"copy_"
        ~transform:(Rays_math.Mat4.translation (Rays_math.Vec3.create 0. 0.5 0.)) source |> get) in
  let one = cook 1 and four = cook 4 in
  check (equal_geometry one four) "one/four-domain restricted output differs"

let run () =
  test_restricted_payload_and_groups ();
  test_identity_and_errors ();
  test_parallel_exact ();
  print_endline "Duplicate tests passed"
