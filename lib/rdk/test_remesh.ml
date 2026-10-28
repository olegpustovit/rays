open Rays
open Rdk
open Rdk_test_support

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let add_attribute ~owner ~name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let grid ?(columns = 7) ?(rows = 6) ?(size = 6.) () =
  Plane_generators.grid ~counts:Plane_generators.Grid_point_counts
    ~connectivity:Plane_generators.Grid_alternating_triangles ~columns ~rows ~size () |> get

let attribute_equal left right =
  Attribute.owner left = Attribute.owner right
  && Attribute.name left = Attribute.name right
  && match Attribute.Private.storage left, Attribute.Private.storage right with
  | Attribute.Float a, Attribute.Float b -> a = b
  | Attribute.Int a, Attribute.Int b -> a = b
  | Attribute.Text a, Attribute.Text b -> a = b
  | Attribute.Float2 a, Attribute.Float2 b ->
      let a = Packed.Float2.Private.view a and b = Packed.Float2.Private.view b in
      a.x = b.x && a.y = b.y
  | Attribute.Float3 a, Attribute.Float3 b ->
      let a = Packed.Float3.Private.view a and b = Packed.Float3.Private.view b in
      a.x = b.x && a.y = b.y && a.z = b.z
  | Attribute.Float4 a, Attribute.Float4 b ->
      let a = Packed.Float4.Private.view a and b = Packed.Float4.Private.view b in
      a.x = b.x && a.y = b.y && a.z = b.z && a.w = b.w
  | Attribute.Int_array a, Attribute.Int_array b ->
      let a = Packed.Int_array.Private.view a
      and b = Packed.Int_array.Private.view b in
      a.offsets = b.offsets && a.values = b.values
  | Attribute.Float_array a, Attribute.Float_array b ->
      let a = Packed.Float_array.Private.view a
      and b = Packed.Float_array.Private.view b in
      a.offsets = b.offsets && a.values = b.values
  | _ -> false

let group_equal left right =
  Group.owner left = Group.owner right && Group.name left = Group.name right
  && Group.length left = Group.length right
  && Group.ordered_elements left = Group.ordered_elements right
  && let same = ref true in
     for element = 0 to Group.length left - 1 do
       if Group.mem element left <> Group.mem element right then same := false
     done;
     !same

let edge_pairs geometry group =
  let index = Topology_index.create (Geometry.topology geometry) in
  let pairs = ref [] in
  Edge_group.iter (fun edge ->
    let a, b = Topology_index.edge_points index edge in
    pairs := (a, b) :: !pairs) group;
  List.sort compare !pairs

let geometry_equal left right =
  let lp = Packed.Float3.Private.view (Geometry.positions left)
  and rp = Packed.Float3.Private.view (Geometry.positions right)
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal attribute_equal (Geometry.attributes left)
      (Geometry.attributes right)
  && List.equal group_equal (Geometry.groups left) (Geometry.groups right)
  && List.map Edge_group.name (Geometry.edge_groups left)
      = List.map Edge_group.name (Geometry.edge_groups right)
  && List.for_all2 (fun left_group right_group ->
       edge_pairs left left_group = edge_pairs right right_group)
       (Geometry.edge_groups left) (Geometry.edge_groups right)

let scalar owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing attribute " ^ name)

let check_triangles geometry =
  let topology = Topology.Private.view (Geometry.topology geometry) in
  for primitive = 0 to Geometry.primitive_count geometry - 1 do
    check (Bytes.get topology.primitive_kinds primitive = '\000'
      && topology.primitive_offsets.(primitive + 1)
         - topology.primitive_offsets.(primitive) = 3)
      "Remesh emitted a non-triangle primitive"
  done

let seam_fixture () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;1.;1.;0.|] ~y:[|0.;0.;1.;1.|] ~z:[|0.;0.;0.;0.|] in
  let topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;1;2; 0;2;3|] ~primitive_offsets:[|0;3;6|]
      |> get_string in
  let uv = Packed.Float2.of_owned ~x:[|0.;1.;1.; 2.;3.;2.|]
      ~y:[|0.;0.;1.; 0.;1.;1.|] |> get_string in
  let geometry = Geometry.create ~positions ~topology () |> get_string
      |> add_attribute ~owner:Attribute.Vertex ~name:"uv" (Attribute.Float2 uv)
      |> add_attribute ~owner:Attribute.Point ~name:"target"
           (Attribute.Float [|0.4;0.4;0.4;0.4|])
      |> add_attribute ~owner:Attribute.Detail ~name:"tag"
           (Attribute.Text [|"fixture"|]) in
  let hard_points = Group.init ~owner:Group.Point ~name:"pin" 4
      (fun point -> point = 0) in
  geometry, hard_points

let test_adaptive_hard_and_uv_seams () =
  let source, hard_points = seam_fixture () in
  let detail = Geometry.find_attribute ~owner:Attribute.Detail "tag" source
      |> Option.get in
  let output = Remesh.run ~grain:3 ~iterations:2 ~target_length:1.
      ~target_size_attribute:"target" ~hard_points
      ~output_hard_edges:"features" ~output_mesh_size:"effective_size"
      ~output_quality:"quality" source |> get in
  check_triangles output;
  let sizes = scalar Attribute.Point "effective_size" output in
  Array.iter (fun value -> check (value = 0.4)
    "Remesh adaptive target-size ancestry drift") sizes;
  let output_detail = Geometry.find_attribute ~owner:Attribute.Detail "tag" output
      |> Option.get in
  check (Attribute.storage_id detail = Attribute.storage_id output_detail)
    "Remesh copied immutable detail payload";
  let features = Geometry.find_edge_group "features" output |> Option.get
  and index = Topology_index.create (Geometry.topology output) in
  check (Edge_group.cardinality features > Topology_index.boundary_edge_count index)
    "Remesh did not preserve the UV discontinuity as a hard interior feature";
  let positions = Packed.Float3.Private.view (Geometry.positions output) in
  let pinned = ref false in
  for point = 0 to Geometry.point_count output - 1 do
    if positions.x.(point) = 0. && positions.y.(point) = 0.
        && positions.z.(point) = 0. then pinned := true
  done;
  check !pinned "Remesh removed or moved an explicit hard point"

let test_input_points_only () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;2.;2.;0.|] ~y:[|0.;0.;1.;1.|] ~z:[|0.;0.;0.;0.|] in
  let topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;1;2;3|] ~primitive_offsets:[|0;4|] |> get_string in
  let source = Geometry.create ~positions ~topology () |> get_string in
  let output = Remesh.run ~iterations:2 ~smoothing:0.
      ~use_input_points_only:true ~target_length:0.1
      ~recompute_point_normals:false source |> get in
  check (Geometry.point_count output = 4 && Geometry.primitive_count output = 2)
    "Remesh input-points-only mode changed point cardinality";
  check_triangles output

(* A payload-rich fixture: split, collapse and flip all fire, attributes and
   groups of every owner ride along. The hashes were produced by the chained
   split/collapse/flip kernels before the local-edit structure replaced them;
   the structure must reproduce their output byte for byte. *)
let component_hashes geometry =
  let mix hash value = Rays_math.Hash.mix_with 1_000_003 hash value land max_int in
  let mix_float hash value = mix hash (Hashtbl.hash (Int64.bits_of_float value)) in
  let floats values = Array.fold_left mix_float 17 values
  and ints values = Array.fold_left mix 17 values in
  let positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and topology = Topology.Private.view (Geometry.topology geometry) in
  [ "positions", mix (mix (floats positions.x) (floats positions.y)) (floats positions.z);
    "vertex_points", ints topology.vertex_points;
    "primitive_offsets", ints topology.primitive_offsets ]
  @ List.map (fun attribute ->
      let name = Printf.sprintf "attribute %s/%s"
          (match Attribute.owner attribute with Attribute.Point -> "point"
            | Vertex -> "vertex" | Primitive -> "primitive" | Detail -> "detail")
          (Attribute.name attribute) in
      name, (match Attribute.Private.storage attribute with
        | Attribute.Float values -> floats values
        | Attribute.Int values -> ints values
        | Attribute.Text values -> Array.fold_left (fun h v -> mix h (Hashtbl.hash v)) 17 values
        | Attribute.Float2 values ->
            let values = Packed.Float2.Private.view values in
            mix (floats values.x) (floats values.y)
        | Attribute.Float3 values ->
            let values = Packed.Float3.Private.view values in
            mix (mix (floats values.x) (floats values.y)) (floats values.z)
        | _ -> 1)) (Geometry.attributes geometry)
  @ List.map (fun group ->
      let members = ref 17 in
      for element = 0 to Group.length group - 1 do
        if Group.mem element group then members := mix !members element
      done;
      let order = match Group.ordered_elements group with
        | None -> 0 | Some order -> ints order in
      Printf.sprintf "group %s (len %d)" (Group.name group) (Group.length group),
      mix !members order) (Geometry.groups geometry)
  @ List.map (fun group ->
      Printf.sprintf "edge group %s" (Edge_group.name group),
      List.fold_left (fun h (a, b) -> mix (mix h a) b) 17 (edge_pairs geometry group))
      (Geometry.edge_groups geometry)

let fixture_hash geometry =
  let mix hash value = Rays_math.Hash.mix_with 1_000_003 hash value land max_int in
  List.fold_left (fun hash (_, value) -> mix hash value) 17 (component_hashes geometry)

let dump label geometry =
  if Sys.getenv_opt "RAYS_REMESH_DUMP" <> None then
    List.iter (fun (name, value) -> Printf.eprintf "%s %s = %d\n%!" label name value)
      (component_hashes geometry)

let payload_fixture () =
  let source = Plane_generators.grid ~counts:Plane_generators.Grid_point_counts
      ~connectivity:Plane_generators.Grid_alternating_triangles ~uv_attribute:"uv"
      ~columns:9 ~rows:7 ~size:6. () |> get in
  let point_count = Geometry.point_count source
  and vertex_count = Geometry.vertex_count source
  and primitive_count = Geometry.primitive_count source in
  let source = source
    |> add_attribute ~owner:Attribute.Point ~name:"point_id"
         (Attribute.Int (Array.init point_count (fun point -> point * 3)))
    |> add_attribute ~owner:Attribute.Primitive ~name:"material"
         (Attribute.Text (Array.init primitive_count (fun primitive ->
            if primitive mod 3 = 0 then "red" else "blue")))
    |> add_attribute ~owner:Attribute.Detail ~name:"tag"
         (Attribute.Text [|"payload"|]) in
  let ordered = Group.ordered ~owner:Group.Point ~name:"ordered_points"
      ~length:point_count (Array.init (point_count / 2) (fun index ->
        point_count - 1 - (index * 2))) |> get_string
  and corners = Group.init ~owner:Group.Vertex ~name:"corners" vertex_count
      (fun vertex -> vertex mod 5 <> 2)
  and faces = Group.init ~owner:Group.Primitive ~name:"faces" primitive_count
      (fun primitive -> primitive mod 4 < 2)
  and pinned = Group.init ~owner:Group.Point ~name:"pinned" point_count
      (fun point -> point = 12 || point = 40) in
  let index = Topology_index.create (Geometry.topology source) in
  let authored = Edge_group.init ~topology:(Geometry.topology source) ~index
      ~name:"authored_edges" (fun edge -> edge mod 3 <> 1) in
  let source = List.fold_left (fun geometry group ->
      Geometry.with_group group geometry |> get_string) source
      [ordered; corners; faces; pinned] in
  Geometry.with_edge_group authored source |> get_string, pinned

let test_local_edits_match_chained_kernels () =
  let source, pinned = payload_fixture () in
  let run domains = Parallel.run ~domains (fun () ->
    Remesh.run ~grain:13 ~iterations:2 ~smoothing:0.3 ~project:true
      ~target_length:0.8 ~hard_points:pinned ~output_hard_edges:"hard"
      ~output_mesh_size:"size" ~output_quality:"quality" source |> get) in
  let output = run 1 in
  dump "payload" output;
  check_triangles output;
  check (Geometry.find_group ~owner:Group.Point "ordered_points" output
      |> Option.get |> Group.is_ordered)
    "Remesh dropped the explicit order of a point group";
  check (Geometry.find_edge_group "authored_edges" output <> None)
    "Remesh dropped an authored edge group";
  let flips_only = Remesh.run ~grain:7 ~iterations:2 ~smoothing:0.
      ~project:false ~use_input_points_only:true ~target_length:0.8
      ~recompute_point_normals:false source |> get in
  dump "flips" flips_only;
  check (fixture_hash output = 212434250856407980)
    (Printf.sprintf "Remesh payload fixture drifted from the chained kernels: %d"
      (fixture_hash output));
  check (geometry_equal output (run 4))
    "Remesh payload fixture differs between one and four domains";
  check (Geometry.point_count flips_only = Geometry.point_count source)
    "Remesh input-points-only fixture changed point cardinality";
  check (fixture_hash flips_only = 1893477268345860603)
    (Printf.sprintf "Remesh flip-only fixture drifted from the chained kernels: %d"
      (fixture_hash flips_only))

let test_errors_and_cancellation () =
  let source = grid ~columns:4 ~rows:4 () in
  let expect code result = match result with
    | Error error -> check (Error.code error = code)
        ("Remesh returned unexpected error code " ^ Error.code error)
    | Ok _ -> fail ("Remesh accepted invalid input for " ^ code) in
  expect "invalid_remesh" (Remesh.run ~target_length:0. source);
  expect "invalid_remesh" (Remesh.run ~target_length:1. ~iterations:(-1) source);
  expect "invalid_remesh" (Remesh.run ~target_length:1. ~smoothing:1.1 source);
  let wrong = Group.init ~owner:Group.Vertex ~name:"wrong"
      (Geometry.vertex_count source) (fun _ -> true) in
  expect "invalid_remesh" (Remesh.run ~target_length:1. ~hard_points:wrong source);
  let bad_size = source |> add_attribute ~owner:Attribute.Point ~name:"size"
      (Attribute.Float (Array.init (Geometry.point_count source)
        (fun point -> if point = 2 then Float.nan else 1.))) in
  expect "invalid_remesh"
    (Remesh.run ~target_length:1. ~target_size_attribute:"size" bad_size);
  let curve = Line_geometry.polyline [|0.,0.,0.;1.,0.,0.;2.,0.,0.|] |> get in
  expect "invalid_remesh" (Remesh.run ~target_length:1. curve);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect "cancelled" (Remesh.run ~cancel:cancelled ~target_length:1. source)

let test_parallel_exact () =
  let source = grid ~columns:14 ~rows:12 ~size:6. () in
  let run domains = Parallel.run ~domains (fun () ->
    Remesh.run ~grain:31 ~iterations:1 ~target_length:0.45
      ~output_hard_edges:"hard" ~output_mesh_size:"size"
      ~output_quality:"quality" source |> get) in
  let one = run 1 and four = run 4 in
  check (geometry_equal one four)
    "Remesh one-domain/multi-domain geometry drift"

let run () =
  test_adaptive_hard_and_uv_seams ();
  test_input_points_only ();
  test_errors_and_cancellation ();
  test_parallel_exact ();
  test_local_edits_match_chained_kernels ();
  print_endline "remesh tests passed"
