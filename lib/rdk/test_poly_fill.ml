open Rays
open Rdk
open Rdk_test_support

let near ?(epsilon = 1e-12) left right = abs_float (left -. right) <= epsilon

let equal_storage left right =
  match Attribute.View.storage left, Attribute.View.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.View.view left
      and right = Packed.Float2.View.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.View.view left
      and right = Packed.Float3.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.View.view left
      and right = Packed.Float4.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | _ -> false

let add_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get in
  Geometry.with_attribute attribute geometry |> get

let open_box ?(offset = 0.) () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|-1. +. offset; 1. +. offset; 1. +. offset; -1. +. offset;
           -1. +. offset; 1. +. offset; 1. +. offset; -1. +. offset|]
      ~y:[|-1.; -1.; 1.; 1.; -1.; -1.; 1.; 1.|]
      ~z:[|-1.; -1.; -1.; -1.; 1.; 1.; 1.; 1.|] in
  let topology = Topology.polygons_owned ~point_count:8
      ~vertex_points:[|
        0;3;2;1;
        0;1;5;4;
        1;2;6;5;
        2;3;7;6;
        3;0;4;7
      |] ~primitive_offsets:[|0;4;8;12;16;20|] |> get in
  let geometry = Geometry.create ~positions ~topology () |> get in
  let geometry = geometry
      |> add_attribute Attribute.Point "weight"
           (Attribute.Float (Array.init 8 float_of_int))
      |> add_attribute Attribute.Point "point_id"
           (Attribute.Int (Array.init 8 Fun.id))
      |> add_attribute Attribute.Point "point_tag"
           (Attribute.Text (Array.init 8 (Printf.sprintf "p%d")))
      |> add_attribute Attribute.Point "point_uv"
           (Attribute.Float2 (Packed.Float2.of_owned
             ~x:(Array.init 8 float_of_int)
             ~y:(Array.init 8 (fun point -> float_of_int (-point))) |> get))
      |> add_attribute Attribute.Point "point_vector"
           (Attribute.Float3 (Packed.Float3.View.of_owned_exn
             ~x:(Array.init 8 float_of_int)
             ~y:(Array.init 8 (fun point -> float_of_int (point + 10)))
             ~z:(Array.init 8 (fun point -> float_of_int (point + 20)))))
      |> add_attribute Attribute.Point "point_quaternion"
           (Attribute.Float4 (Packed.Float4.of_owned
             ~x:(Array.init 8 float_of_int) ~y:(Array.make 8 0.)
             ~z:(Array.make 8 0.) ~w:(Array.make 8 1.) |> get))
      |> add_attribute Attribute.Point "point_int_array"
           (Attribute.Int_array (Packed.Int_array.create_owned
             ~offsets:(Array.init 9 (fun index -> index * 2))
             ~values:(Array.init 16 Fun.id) |> get))
      |> add_attribute Attribute.Point "point_float_array"
           (Attribute.Float_array (Packed.Float_array.create_owned
             ~offsets:(Array.init 9 (fun index -> index * 2))
             ~values:(Array.init 16 float_of_int) |> get))
      |> add_attribute Attribute.Vertex "vertex_weight"
           (Attribute.Float (Array.init 20 float_of_int))
      |> add_attribute Attribute.Primitive "primitive_id"
           (Attribute.Int [|10; 20; 30; 40; 50|]) in
  let top = Group.init ~owner:Group.Point ~name:"top" 8
      (fun point -> point >= 4)
  and first_vertex = Group.init ~owner:Group.Vertex ~name:"first_vertex" 20
      (fun vertex -> vertex = 0)
  and bottom = Group.init ~owner:Group.Primitive ~name:"bottom" 5
      (fun primitive -> primitive = 0) in
  let geometry = geometry |> Geometry.with_group top |> get
      |> Geometry.with_group first_vertex |> get
      |> Geometry.with_group bottom |> get in
  let index = Topology_index.create topology in
  let rim = Edge_group.init ~topology ~index ~name:"rim" (fun edge ->
      Topology_index.edge_incidence_count index edge = 1
      && let a, b = Topology_index.edge_points index edge in
         let positions = Packed.Float3.View.view positions in
         positions.z.(a) = 1. && positions.z.(b) = 1.) in
  Geometry.with_edge_group rim geometry |> get

let group_cardinality owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some group -> Group.cardinality group
  | None -> fail ("missing group " ^ name)

let assert_closed geometry message =
  let index = Topology_index.create (Geometry.topology geometry) in
  check (Topology_index.boundary_edge_count index = 0) message

let assert_source_prefix source output =
  let source = Topology.View.view (Geometry.topology source)
  and output = Topology.View.view (Geometry.topology output) in
  check (Array.sub output.vertex_points 0 (Array.length source.vertex_points)
      = source.vertex_points) "Poly Fill changed source corner prefix";
  check (Array.sub output.primitive_offsets 0 (Array.length source.primitive_offsets)
      = source.primitive_offsets) "Poly Fill changed source primitive prefix";
  check (Bytes.sub output.primitive_kinds 0 (Bytes.length source.primitive_kinds)
      = source.primitive_kinds) "Poly Fill changed source primitive kinds"

let test_modes_and_payload () =
  let source = open_box () in
  let single = Poly_fill.run ~mode:Poly_fill.Fill_single_polygon
      ~patch_group:"patch" source |> get_rdk in
  check (Geometry.point_count single = 8 && Geometry.vertex_count single = 24
      && Geometry.primitive_count single = 6) "single-polygon cardinality";
  assert_source_prefix source single;
  assert_closed single "single-polygon fill left a boundary";
  check (group_cardinality Group.Primitive "patch" single = 1)
    "single-polygon patch group";
  check (group_cardinality Group.Primitive "bottom" single = 1)
    "generated patch inherited a primitive group";
  let single_topology = Topology.View.view (Geometry.topology single)
  and single_positions = Packed.Float3.View.view (Geometry.positions single) in
  let single_first = single_topology.primitive_offsets.(5) in
  let sa = single_topology.vertex_points.(single_first)
  and sb = single_topology.vertex_points.(single_first + 1)
  and sc = single_topology.vertex_points.(single_first + 2) in
  let sux = single_positions.x.(sb) -. single_positions.x.(sa)
  and suy = single_positions.y.(sb) -. single_positions.y.(sa)
  and svx = single_positions.x.(sc) -. single_positions.x.(sa)
  and svy = single_positions.y.(sc) -. single_positions.y.(sa) in
  check (sux *. svy -. suy *. svx > 0.)
    "default patch winding did not oppose the source boundary";
  let triangles = Poly_fill.run ~mode:Poly_fill.Fill_triangles
      ~patch_group:"patch" source |> get_rdk in
  let triangle_vertices = Topology.View.view (Geometry.topology triangles)
      |> fun topology -> Array.sub topology.vertex_points 20 6 in
  check (triangle_vertices = [|6;4;5;4;6;7|])
    "triangle fill changed ordered ear clipping";
  check (Geometry.point_count triangles = 8
      && Geometry.vertex_count triangles = 26
      && Geometry.primitive_count triangles = 7)
    "triangle fill cardinality";
  assert_closed triangles "triangle fill left a boundary";
  check (group_cardinality Group.Primitive "patch" triangles = 2)
    "triangle patch group";
  let fan = Poly_fill.run ~mode:Poly_fill.Fill_triangle_fan
      ~patch_group:"patch" source |> get_rdk in
  check (Geometry.point_count fan = 9 && Geometry.vertex_count fan = 32
      && Geometry.primitive_count fan = 9) "triangle-fan cardinality";
  assert_closed fan "triangle fan left a boundary";
  let positions = Packed.Float3.View.view (Geometry.positions fan) in
  check (near positions.x.(8) 0. && near positions.y.(8) 0.
      && near positions.z.(8) 1.) "triangle-fan center position";
  let weight = Geometry.find_attribute ~owner:Attribute.Point "weight" fan
      |> Option.get in
  (match Attribute.View.storage weight with
   | Attribute.Float values -> check (near values.(8) 5.5)
       "triangle-fan point center did not average numeric data"
   | _ -> fail "point weight storage changed");
  let point_id = Geometry.find_attribute ~owner:Attribute.Point "point_id" fan
      |> Option.get in
  (match Attribute.View.storage point_id with
   | Attribute.Int values -> check (values.(8) >= 4 && values.(8) <= 7)
       "triangle-fan discrete center ancestry"
   | _ -> fail "point id storage changed");
  check (group_cardinality Group.Point "top" fan = 4)
    "triangle-fan center entered source point group";
  check (group_cardinality Group.Vertex "first_vertex" fan = 1)
    "new patch vertices entered source vertex group";
  check (group_cardinality Group.Primitive "bottom" fan = 1)
    "new patch primitives entered source primitive group";
  check (group_cardinality Group.Primitive "patch" fan = 4)
    "triangle-fan patch group";
  let source_index = Topology_index.create (Geometry.topology source) in
  let boundary_vertices = ref [] in
  for edge = 0 to Topology_index.edge_count source_index - 1 do
    if Topology_index.edge_incidence_count source_index edge = 1 then begin
      let a, b = Topology_index.edge_points source_index edge in
      let source_positions = Packed.Float3.View.view (Geometry.positions source) in
      if source_positions.z.(a) = 1. && source_positions.z.(b) = 1. then
        boundary_vertices := Topology_index.edge_vertex source_index ~edge ~local:0
          :: !boundary_vertices
    end
  done;
  let expected_vertex_mean = List.fold_left (fun sum vertex ->
      sum +. float_of_int vertex) 0. !boundary_vertices
      /. float_of_int (List.length !boundary_vertices) in
  let vertex_weight = Geometry.find_attribute ~owner:Attribute.Vertex
      "vertex_weight" fan |> Option.get in
  (match Attribute.View.storage vertex_weight with
   | Attribute.Float values ->
       let topology = Topology.View.view (Geometry.topology fan) in
       for vertex = 20 to Array.length topology.vertex_points - 1 do
         if topology.vertex_points.(vertex) = 8 then
           check (near values.(vertex) expected_vertex_mean)
             "triangle-fan vertex center did not average boundary corners"
       done
   | _ -> fail "vertex weight storage changed");
  List.iter (fun (owner, name) ->
    let attribute = Geometry.find_attribute ~owner name fan |> Option.get in
    check (Attribute.length attribute = match owner with
      | Attribute.Point -> 9 | Attribute.Vertex -> 32
      | Attribute.Primitive -> 9 | Attribute.Detail -> 1)
      ("attribute length mismatch for " ^ name))
    [Attribute.Point, "point_tag"; Attribute.Point, "point_uv";
     Attribute.Point, "point_vector"; Attribute.Point, "point_quaternion";
     Attribute.Point, "point_int_array"; Attribute.Point, "point_float_array";
     Attribute.Primitive, "primitive_id"];
  let rim = Geometry.find_edge_group "rim" fan |> Option.get in
  check (Edge_group.cardinality rim = 4)
    "source native edge group did not survive fill";
  List.iter (fun (label, geometry) ->
    let rim = Geometry.find_edge_group "rim" geometry |> Option.get in
    let edges = Topology_index.edge_count
        (Topology_index.create (Geometry.topology geometry)) in
    check (Edge_group.length rim = edges)
      (label ^ " native edge-group target cardinality"))
    ["single", single; "triangles", triangles; "fan", fan]

let test_unique_reverse_and_normals () =
  let source = open_box () in
  let unique = Poly_fill.run ~mode:Poly_fill.Fill_single_polygon ~unique_points:true
      source |> get_rdk in
  check (Geometry.point_count unique = 12
      && Topology_index.boundary_edge_count
           (Topology_index.create (Geometry.topology unique)) = 8)
    "unique fill did not detach the patch boundary";
  check (group_cardinality Group.Point "top" unique = 8)
    "unique boundary points did not inherit point-group ancestry";
  let unique_rim = Geometry.find_edge_group "rim" unique |> Option.get in
  check (Edge_group.length unique_rim = Topology_index.edge_count
      (Topology_index.create (Geometry.topology unique)))
    "unique fill native edge-group target cardinality";
  let reversed = Poly_fill.run ~mode:Poly_fill.Fill_single_polygon
      ~reverse_patches:true source |> get_rdk in
  let topology = Topology.View.view (Geometry.topology reversed) in
  let first = topology.primitive_offsets.(5) in
  let positions = Packed.Float3.View.view (Geometry.positions reversed) in
  let a = topology.vertex_points.(first)
  and b = topology.vertex_points.(first + 1)
  and c = topology.vertex_points.(first + 2) in
  let ux = positions.x.(b) -. positions.x.(a)
  and uy = positions.y.(b) -. positions.y.(a)
  and vx = positions.x.(c) -. positions.x.(a)
  and vy = positions.y.(c) -. positions.y.(a) in
  check (ux *. vy -. uy *. vx < 0.) "reverse_patches did not reverse winding";
  let point_normal = Attribute.create_owned ~name:"N" ~owner:Attribute.Point
      (Attribute.Float3 (Packed.Float3.View.of_owned_exn
        ~x:(Array.make 8 1.) ~y:(Array.make 8 0.) ~z:(Array.make 8 0.)))
      |> get in
  let with_normal = Geometry.with_attribute point_normal source |> get in
  let updated = Poly_fill.run ~mode:Poly_fill.Fill_triangle_fan
      ~update_point_normals:true with_normal |> get_rdk in
  let normal = Geometry.find_attribute ~owner:Attribute.Point "N" updated
      |> Option.get in
  check (Attribute.length normal = 9) "updated point normal length";
  (match Attribute.View.storage normal with
   | Attribute.Float3 values ->
       let values = Packed.Float3.View.view values in
       check (near values.x.(8) 0. && near values.y.(8) 0.
          && values.z.(8) > 0.999999) "patch center normal was not recomputed"
   | _ -> fail "point normal storage changed");
  let no_normal = Poly_fill.run ~mode:Poly_fill.Fill_triangle_fan
      ~update_point_normals:true source |> get_rdk in
  check (Geometry.find_attribute ~owner:Attribute.Point "N" no_normal = None)
    "point-normal update created an unrequested normal field"

let test_selection_and_failures () =
  let left = open_box () and right = open_box ~offset:4. () in
  let merged = Mesh_merge.run [left; right] |> get_rdk in
  let topology = Geometry.topology merged in
  let index = Topology_index.create topology in
  let positions = Packed.Float3.View.view (Geometry.positions merged) in
  let one_edge = ref (-1) and interior = ref (-1) in
  for edge = 0 to Topology_index.edge_count index - 1 do
    let a, b = Topology_index.edge_points index edge in
    if !one_edge < 0 && Topology_index.edge_incidence_count index edge = 1
        && positions.x.(a) < 2. && positions.x.(b) < 2.
        && positions.z.(a) = 1. && positions.z.(b) = 1. then one_edge := edge;
    if !interior < 0 && Topology_index.edge_incidence_count index edge = 2 then
      interior := edge
  done;
  let selected = Edge_group.init ~topology ~index ~name:"selected"
      (fun edge -> edge = !one_edge) in
  let output = Poly_fill.run ~boundary:selected ~mode:Poly_fill.Fill_single_polygon
      merged |> get_rdk in
  check (Geometry.primitive_count output = Geometry.primitive_count merged + 1)
    "partial edge selection did not auto-complete exactly one loop";
  let bad = Edge_group.init ~topology ~index ~name:"bad"
      (fun edge -> edge = !interior) in
  (match Poly_fill.run ~boundary:bad merged with
   | Error error -> check (Error.code error = "invalid_geometry")
       "non-boundary selection error code"
   | Ok _ -> fail "Poly Fill accepted a non-boundary edge selection");
  let empty = Edge_group.init ~topology ~index ~name:"empty" (fun _ -> false) in
  let unchanged = Poly_fill.run ~boundary:empty merged |> get_rdk in
  check (unchanged == merged) "empty explicit boundary selection was not identity";
  let other = open_box ~offset:20. () in
  let other_topology = Geometry.topology other in
  let other_index = Topology_index.create other_topology in
  let foreign = Edge_group.init ~topology:other_topology ~index:other_index
      ~name:"foreign" (fun edge -> edge = 0) in
  (match Poly_fill.run ~boundary:foreign merged with
   | Error error -> check (Error.code error = "invalid_geometry")
       "foreign edge-group error code"
   | Ok _ -> fail "Poly Fill accepted a foreign edge group");
  let branched_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 0.; -1.; 0.|] ~y:[|0.; 0.; 1.; 0.; -1.|]
      ~z:(Array.make 5 0.) in
  let branched_topology = Topology.polygons_owned ~point_count:5
      ~vertex_points:[|0;1;2; 0;3;4|]
      ~primitive_offsets:[|0;3;6|] |> get in
  let branched = Geometry.create ~positions:branched_positions
      ~topology:branched_topology () |> get in
  (match Poly_fill.run branched with
   | Error error -> check (Error.code error = "invalid_geometry")
       "branched-boundary error code"
   | Ok _ -> fail "Poly Fill accepted a branched boundary");
  let nonfinite = open_box () in
  let source_positions = Packed.Float3.View.view (Geometry.positions nonfinite) in
  let positions = Packed.Float3.View.of_owned_exn
      ~x:(Array.mapi (fun point value -> if point = 4 then Float.nan else value)
        source_positions.x)
      ~y:(Array.copy source_positions.y) ~z:(Array.copy source_positions.z) in
  let nonfinite = Geometry.with_positions positions nonfinite |> get in
  (match Poly_fill.run nonfinite with
   | Error error -> check (Error.code error = "invalid_geometry")
       "non-finite error code"
   | Ok _ -> fail "Poly Fill accepted a non-finite loop position");
  (match Poly_fill.run ~patch_group:"" merged with
   | Error error -> check (Error.code error = "invalid_geometry")
       "empty patch-group error code"
   | Ok _ -> fail "Poly Fill accepted an empty patch group");
  let polygon points =
    let count = Array.length points in
    let positions = Packed.Float3.View.of_owned_exn
        ~x:(Array.map (fun (x, _, _) -> x) points)
        ~y:(Array.map (fun (_, y, _) -> y) points)
        ~z:(Array.map (fun (_, _, z) -> z) points) in
    let topology = Topology.polygons_owned ~point_count:count
        ~vertex_points:(Array.init count Fun.id)
        ~primitive_offsets:[|0; count|] |> get in
    Geometry.create ~positions ~topology () |> get in
  let concave = polygon [|0.,0.,0.; 2.,0.,0.; 2.,2.,0.; 1.,0.8,0.; 0.,2.,0.|] in
  let concave_fill = Poly_fill.run ~mode:Poly_fill.Fill_triangles concave |> get_rdk in
  check (Geometry.primitive_count concave_fill = 4
      && Geometry.vertex_count concave_fill = 14)
    "concave loop triangulation cardinality";
  let crossed = polygon [|0.,0.,0.; 2.,2.,0.; 0.,2.,0.; 2.,0.,0.|] in
  (match Poly_fill.run ~mode:Poly_fill.Fill_triangles crossed with
   | Error error -> check (Error.code error = "invalid_geometry")
       "self-intersection error code"
   | Ok _ -> fail "Poly Fill triangulated a self-intersecting loop");
  let diagonal_source = open_box () in
  let diagonal_topology = Topology.View.view
      (Geometry.topology diagonal_source) in
  let topology = Topology.create_owned ~point_count:8
      ~vertex_points:(Array.append diagonal_topology.vertex_points [|4;6|])
      ~primitive_offsets:[|0;4;8;12;16;20;22|]
      ~primitive_kinds:[|Topology.Polygon; Topology.Polygon; Topology.Polygon;
        Topology.Polygon; Topology.Polygon; Topology.Open_polyline|] |> get in
  let diagonal_source = Geometry.create
      ~positions:(Geometry.positions diagonal_source) ~topology () |> get in
  let diagonal_index = Topology_index.create topology in
  let all_edges = Edge_group.init ~topology ~index:diagonal_index ~name:"all"
      (fun _ -> true) in
  let diagonal_source = Geometry.with_edge_group all_edges diagonal_source
      |> get in
  let diagonal_fill = Poly_fill.run ~mode:Poly_fill.Fill_triangles diagonal_source
      |> get_rdk in
  let mapped = Geometry.find_edge_group "all" diagonal_fill |> Option.get in
  check (Edge_group.length mapped = Topology_index.edge_count
      (Topology_index.create (Geometry.topology diagonal_fill)))
    "existing source diagonal was double-counted in target edge cardinality";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Poly_fill.run ~cancel:cancelled merged with
   | Error error -> check (Error.code error = "cancelled")
       "Poly Fill cancellation error code"
   | Ok _ -> fail "cancelled Poly Fill published geometry")

let many_open_boxes count =
  let point_count = count * 8 and primitive_count = count * 5
  and vertex_count = count * 20 in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. and vertex_points = Array.make vertex_count 0
  and primitive_offsets = Array.init (primitive_count + 1) (fun p -> p * 4) in
  let local_x = [|-1.;1.;1.;-1.;-1.;1.;1.;-1.|]
  and local_y = [|-1.;-1.;1.;1.;-1.;-1.;1.;1.|]
  and local_z = [|-1.;-1.;-1.;-1.;1.;1.;1.;1.|]
  and local_vertices = [|0;3;2;1; 0;1;5;4; 1;2;6;5; 2;3;7;6; 3;0;4;7|] in
  for box = 0 to count - 1 do
    let point_base = box * 8 and vertex_base = box * 20 in
    let gx = box mod 256 and gy = box / 256 in
    for local = 0 to 7 do
      x.(point_base + local) <- float_of_int gx *. 3. +. local_x.(local);
      y.(point_base + local) <- float_of_int gy *. 3. +. local_y.(local);
      z.(point_base + local) <- local_z.(local)
    done;
    for local = 0 to 19 do
      vertex_points.(vertex_base + local) <- point_base + local_vertices.(local)
    done
  done;
  let positions = Packed.Float3.View.of_owned_exn ~x ~y ~z in
  let topology = Topology.polygons_owned ~point_count ~vertex_points
      ~primitive_offsets |> get in
  let geometry = Geometry.create ~positions ~topology () |> get in
  geometry
  |> add_attribute Attribute.Point "id"
       (Attribute.Int (Array.init point_count Fun.id))
  |> add_attribute Attribute.Vertex "u"
       (Attribute.Float (Array.init vertex_count float_of_int))
  |> add_attribute Attribute.Primitive "piece"
       (Attribute.Int (Array.init primitive_count (fun primitive -> primitive / 5)))

let test_parallel () =
  let source = many_open_boxes 20_000 in
  let triangles domains = Parallel.run ~domains (fun () ->
      Poly_fill.run ~grain:257 ~mode:Poly_fill.Fill_triangles
        ~patch_group:"patch" source |> get_rdk) in
  let triangles_one = triangles 1 and triangles_four = triangles 4 in
  check (equal_geometry triangles_one triangles_four)
    "triangle fill differs across domain counts";
  check (Geometry.point_count triangles_one = 20_000 * 8
      && Geometry.vertex_count triangles_one = 20_000 * 26
      && Geometry.primitive_count triangles_one = 20_000 * 7)
    "triangle fill scale cardinality";
  let run domains = Parallel.run ~domains (fun () ->
      Poly_fill.run ~grain:257 ~mode:Poly_fill.Fill_triangle_fan
        ~unique_points:true ~patch_group:"patch" source |> get_rdk) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four) "Poly Fill differs across domain counts";
  check (Geometry.point_count one = 20_000 * 13
      && Geometry.vertex_count one = 20_000 * 32
      && Geometry.primitive_count one = 20_000 * 9
      && group_cardinality Group.Primitive "patch" one = 20_000 * 4)
    "Poly Fill scale cardinality"

let run () =
  test_modes_and_payload ();
  test_unique_reverse_and_normals ();
  test_selection_and_failures ();
  test_parallel ();
  print_endline "poly fill tests passed"
