open Rdk

module Solid = Rdk_boolean.Boolean_solid
module Extract = Rdk_boolean.Boolean_extract
module Payload = Rdk_boolean.Boolean_payload

let fail format = Printf.ksprintf failwith format
let check condition message = if not condition then fail "%s" message
let get = function Ok value -> value | Error error -> fail "%s" (Error.to_string error)
let get_string = function Ok value -> value | Error message -> fail "%s" message

let attribute name storage =
  Attribute.create_owned ~name ~owner:Attribute.Primitive storage |> get_string

let float2 x y = Packed.Float2.of_owned ~x ~y |> get_string
let float4 x y z w = Packed.Float4.of_owned ~x ~y ~z ~w |> get_string

let primitive_attributes base ~with_label =
  let scalar offset = Array.init 4 (fun face -> offset +. float_of_int face)
  and integer offset = Array.init 4 (fun face -> offset + face) in
  let x = scalar (base +. 30.) and y = scalar (base +. 40.)
  and z = scalar (base +. 50.) and w = scalar (base +. 60.) in
  let offsets = [|0;2;4;6;8|]
  and int_values = Array.init 8 (fun slot -> int_of_float base + slot)
  and float_values = Array.init 8 (fun slot -> base +. (float_of_int slot /. 10.)) in
  let attributes = [
    attribute "weight" (Attribute.Float (scalar base));
    attribute "id" (Attribute.Int (integer (int_of_float base + 100)));
    attribute "uv_pair" (Attribute.Float2 (float2 x y));
    attribute "direction" (Attribute.Float3
      (Packed.Float3.Private.of_owned_exn ~x:(Array.copy x) ~y:(Array.copy y)
        ~z:(Array.copy z)));
    attribute "rgba" (Attribute.Float4
      (float4 (Array.copy x) (Array.copy y) (Array.copy z) w));
    attribute "int_rows" (Attribute.Int_array
      (Packed.Int_array.Private.create_validated_owned
        ~offsets:(Array.copy offsets) ~values:int_values));
    attribute "float_rows" (Attribute.Float_array
      (Packed.Float_array.Private.create_validated_owned
        ~offsets:(Array.copy offsets) ~values:float_values));
  ] in
  if with_label then
    attribute "label" (Attribute.Text
      (Array.init 4 (fun face -> Printf.sprintf "left-%d" face))) :: attributes
  else attributes

let tetra ~origin:(ox,oy,oz) ~base ~with_label =
  let points = [|ox,oy,oz; ox+.1.,oy,oz; ox,oy+.1.,oz; ox,oy,oz+.1.|] in
  let x = Array.map (fun (x,_,_) -> x) points
  and y = Array.map (fun (_,y,_) -> y) points
  and z = Array.map (fun (_,_,z) -> z) points in
  let topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;2;1; 0;1;3; 1;2;3; 2;0;3|]
      ~primitive_offsets:[|0;3;6;9;12|] |> get_string in
  let groups = [
    Group.init ~owner:Group.Primitive ~name:"selected" 4
      (fun face -> if base < 20. then face land 1 = 0 else face land 1 = 1);
    Group.ordered ~owner:Group.Primitive ~name:"ordered" ~length:4
      (if base < 20. then [|3;1|] else [|2;0|]) |> get_string;
  ] @ if with_label then [
    Group.init ~owner:Group.Primitive ~name:"left_only" 4 ((=) 0)
  ] else [] in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology ~attributes:(primitive_attributes base ~with_label) ~groups ()
    |> get_string

let cube_quads ~origin:(ox,oy,oz) =
  let points = [|
    ox,oy,oz; ox+.1.,oy,oz; ox+.1.,oy+.1.,oz; ox,oy+.1.,oz;
    ox,oy,oz+.1.; ox+.1.,oy,oz+.1.; ox+.1.,oy+.1.,oz+.1.; ox,oy+.1.,oz+.1.
  |] in
  let x = Array.map (fun (x,_,_) -> x) points
  and y = Array.map (fun (_,y,_) -> y) points
  and z = Array.map (fun (_,_,z) -> z) points in
  let topology = Topology.polygons_owned ~point_count:8
      ~vertex_points:[|0;3;2;1; 4;5;6;7; 0;1;5;4;
                       3;7;6;2; 0;4;7;3; 1;2;6;5|]
      ~primitive_offsets:[|0;4;8;12;16;20;24|] |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let duplicated_tetra ~origin:(ox,oy,oz) =
  let points = [|
    ox,oy,oz; ox+.1.,oy,oz; ox,oy+.1.,oz; ox,oy,oz+.1.;
    ox,oy,oz; ox+.1.,oy,oz; ox,oy+.1.,oz; ox,oy,oz+.1.
  |] in
  let x = Array.map (fun (x,_,_) -> x) points
  and y = Array.map (fun (_,y,_) -> y) points
  and z = Array.map (fun (_,_,z) -> z) points in
  let topology = Topology.polygons_owned ~point_count:8
      ~vertex_points:[|0;2;1; 0;1;3; 1;2;3; 2;0;3;
                       4;6;5; 4;5;7; 5;6;7; 6;4;7|]
      ~primitive_offsets:[|0;3;6;9;12;15;18;21;24|] |> get_string in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let make_ancestry left right =
  Solid.prepare ~grain:1 ~left ~right () |> get
  |> Solid.extract_with_ancestry ~expression:Extract.union |> get

let find_storage owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | None -> fail "missing attribute %s" name
  | Some attribute -> Attribute.Private.storage attribute

let validate left right ancestry output =
  check (Geometry.positions output == Geometry.positions (Extract.geometry ancestry)
      && Geometry.topology output == Geometry.topology (Extract.geometry ancestry))
    "primitive payload transfer rebuilt Boolean position/topology storage";
  let weight = match find_storage Attribute.Primitive "weight" output with
    | Attribute.Float values -> values | _ -> assert false
  and ids = match find_storage Attribute.Primitive "id" output with
    | Attribute.Int values -> values | _ -> assert false
  and labels = match find_storage Attribute.Primitive "label" output with
    | Attribute.Text values -> values | _ -> assert false
  and pairs = match find_storage Attribute.Primitive "uv_pair" output with
    | Attribute.Float2 value -> Packed.Float2.Private.view value | _ -> assert false
  and directions = match find_storage Attribute.Primitive "direction" output with
    | Attribute.Float3 value -> Packed.Float3.Private.view value | _ -> assert false
  and colors = match find_storage Attribute.Primitive "rgba" output with
    | Attribute.Float4 value -> Packed.Float4.Private.view value | _ -> assert false
  and int_rows = match find_storage Attribute.Primitive "int_rows" output with
    | Attribute.Int_array value -> Packed.Int_array.Private.view value | _ -> assert false
  and float_rows = match find_storage Attribute.Primitive "float_rows" output with
    | Attribute.Float_array value -> Packed.Float_array.Private.view value | _ -> assert false in
  let selected = Option.get (Geometry.find_group ~owner:Group.Primitive "selected" output)
  and left_only = Option.get (Geometry.find_group ~owner:Group.Primitive "left_only" output) in
  let ordered = Option.get
      (Geometry.find_group ~owner:Group.Primitive "ordered" output) in
  for primitive = 0 to Geometry.primitive_count output - 1 do
    let face = Extract.primitive_face ancestry primitive in
    let is_left = Extract.primitive_side ancestry primitive = Rdk_boolean.Boolean_complex.Left in
    let base = if is_left then 10. else 20. in
    check (weight.(primitive) = base +. float_of_int face)
      "primitive float ancestry is wrong";
    check (ids.(primitive) = int_of_float base + 100 + face)
      "primitive integer ancestry is wrong";
    check (labels.(primitive) = if is_left then Printf.sprintf "left-%d" face else "")
      "primitive text/default ancestry is wrong";
    check (pairs.x.(primitive) = base +. 30. +. float_of_int face
        && pairs.y.(primitive) = base +. 40. +. float_of_int face
        && directions.z.(primitive) = base +. 50. +. float_of_int face
        && colors.w.(primitive) = base +. 60. +. float_of_int face)
      "primitive tuple ancestry is wrong";
    let int_first = int_rows.offsets.(primitive)
    and float_first = float_rows.offsets.(primitive) in
    check (int_rows.offsets.(primitive + 1) - int_first = 2
        && int_rows.values.(int_first) = int_of_float base + (face * 2)
        && float_rows.offsets.(primitive + 1) - float_first = 2
        && float_rows.values.(float_first) = base +. (float_of_int (face * 2) /. 10.))
      "primitive CSR ancestry is wrong";
    check (Group.mem primitive selected =
        (if is_left then face land 1 = 0 else face land 1 = 1))
      "primitive group ancestry is wrong";
    check (Group.mem primitive left_only = (is_left && face = 0))
      "missing-side primitive group default is wrong"
  done;
  let order = Option.get (Group.ordered_elements ordered) in
  let source_order = Array.map (fun primitive ->
      (match Extract.primitive_side ancestry primitive with
       | Rdk_boolean.Boolean_complex.Left -> 0 | Rdk_boolean.Boolean_complex.Right -> 1),
      Extract.primitive_face ancestry primitive) order in
  check (source_order = [|0,3; 0,1; 1,2; 1,0|])
    "ordered primitive group ancestry did not preserve left/right source order";
  ignore left; ignore right

let signature geometry =
  let weight = match find_storage Attribute.Primitive "weight" geometry with
    | Attribute.Float values -> Array.copy values | _ -> assert false
  and ids = match find_storage Attribute.Primitive "id" geometry with
    | Attribute.Int values -> Array.copy values | _ -> assert false
  and selected = Option.get
      (Geometry.find_group ~owner:Group.Primitive "selected" geometry) in
  weight, ids, Array.init (Geometry.primitive_count geometry)
    (fun primitive -> Group.mem primitive selected)

let test_complete_primitive_payload () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
  and right = tetra ~origin:(10.,0.,0.) ~base:20. ~with_label:false in
  let ancestry = make_ancestry left right in
  let output = Payload.copy_primitives ~grain:1 ancestry |> get in
  validate left right ancestry output;
  let run domains = Parallel.run ~domains (fun () ->
      let ancestry = make_ancestry left right in
      Payload.copy_primitives ~grain:1 ancestry |> get |> signature) in
  check (run 1 = run 4) "primitive Boolean payload differs between domain counts"

let test_schema_and_parameter_errors () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
  and right = tetra ~origin:(10.,0.,0.) ~base:20. ~with_label:false in
  let left = Geometry.with_attribute
      (attribute "clash" (Attribute.Float (Array.make 4 0.))) left |> get_string
  and right = Geometry.with_attribute
      (attribute "clash" (Attribute.Int (Array.make 4 0))) right |> get_string in
  let ancestry = make_ancestry left right in
  (match Payload.copy_primitives ~grain:1 ancestry with
   | Error error when Error.code error = "invalid_payload" -> ()
   | Error error -> fail "unexpected schema error: %s" (Error.to_string error)
   | Ok _ -> fail "mismatched primitive attribute storage was accepted");
  (match Payload.copy_primitives ~grain:0 ancestry with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected grain error: %s" (Error.to_string error)
   | Ok _ -> fail "non-positive payload grain was accepted")

let test_cancellation () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
  and right = tetra ~origin:(10.,0.,0.) ~base:20. ~with_label:false in
  let ancestry = make_ancestry left right and cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Payload.copy_primitives ~cancel ~grain:1 ancestry with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled primitive payload transfer completed"

let add_edge_group name selected geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  let group = Edge_group.init ~grain:1 ~topology ~index ~name selected in
  Geometry.with_edge_group group geometry |> get_string

let add_edge_group_by_points name selected geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  let group = Edge_group.init ~grain:1 ~topology ~index ~name (fun edge ->
      let first, second = Topology_index.edge_points index edge in
      selected first second) in
  Geometry.with_edge_group group geometry |> get_string

let add_corner_payload base geometry =
  let point_count = Geometry.point_count geometry
  and vertex_count = Geometry.vertex_count geometry in
  let make owner name storage = Attribute.create_owned ~name ~owner storage |> get_string in
  let attrs owner prefix count =
    let scalar = Array.init count (fun index -> base +. float_of_int index)
    and integers = Array.init count (fun index -> int_of_float base + index)
    and text = Array.init count (fun index -> Printf.sprintf "%s-%d" prefix index)
    and x = Array.init count (fun index -> base +. 1. +. float_of_int index)
    and y = Array.init count (fun index -> base +. 2. +. float_of_int index)
    and z = Array.init count (fun index -> base +. 3. +. float_of_int index)
    and w = Array.init count (fun index -> base +. 4. +. float_of_int index)
    and offsets = Array.init (count + 1) (fun index -> index * 2)
    and int_values = Array.init (count * 2) (fun index -> int_of_float base + index)
    and float_values = Array.init (count * 2) (fun index -> base +. float_of_int index /. 10.) in
    [
      make owner (prefix ^ "_float") (Attribute.Float scalar);
      make owner (prefix ^ "_int") (Attribute.Int integers);
      make owner (prefix ^ "_text") (Attribute.Text text);
      make owner (prefix ^ "_f2")
        (Attribute.Float2 (float2 (Array.copy x) (Array.copy y)));
      make owner (if owner = Attribute.Vertex then "N" else prefix ^ "_f3")
        (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
          ~x:(Array.copy x) ~y:(Array.copy y) ~z:(Array.copy z)));
      make owner (prefix ^ "_f4")
        (Attribute.Float4 (float4 x y z w));
      make owner (prefix ^ "_ia") (Attribute.Int_array
        (Packed.Int_array.Private.create_validated_owned
          ~offsets:(Array.copy offsets) ~values:int_values));
      make owner (prefix ^ "_fa") (Attribute.Float_array
        (Packed.Float_array.Private.create_validated_owned
          ~offsets:(Array.copy offsets) ~values:float_values));
    ] in
  let attributes = Geometry.attributes geometry
      @ attrs Attribute.Point "p" point_count
      @ attrs Attribute.Vertex "v" vertex_count in
  let point_group = Group.ordered ~owner:Group.Point ~name:"point_ordered"
      ~length:point_count [|3;1|] |> get_string
  and vertex_group = Group.ordered ~owner:Group.Vertex ~name:"vertex_ordered"
      ~length:vertex_count [|11;2|] |> get_string in
  let topology = Geometry.topology geometry in
  let edge_index = Topology_index.create topology in
  let edge_group = Edge_group.init ~grain:1 ~topology ~index:edge_index
      ~name:"source_edges" (fun edge -> edge land 1 = 0) in
  Geometry.create ~positions:(Geometry.positions geometry)
    ~topology:(Geometry.topology geometry) ~attributes
    ~groups:(Geometry.groups geometry @ [point_group; vertex_group])
    ~edge_groups:(Geometry.edge_groups geometry @ [edge_group]) () |> get_string

let fixed_source_value base source = base +. float_of_int source

let validate_corner_payload ancestry output =
  let topology = Topology.Private.view (Geometry.topology output) in
  let pfloat = match find_storage Attribute.Point "p_float" output with
    | Attribute.Float value -> value | _ -> assert false
  and vfloat = match find_storage Attribute.Vertex "v_float" output with
    | Attribute.Float value -> value | _ -> assert false
  and normal = match find_storage Attribute.Vertex "N" output with
    | Attribute.Float3 value -> Packed.Float3.Private.view value | _ -> assert false
  and pia = match find_storage Attribute.Point "p_ia" output with
    | Attribute.Int_array value -> Packed.Int_array.Private.view value | _ -> assert false
  and vfa = match find_storage Attribute.Vertex "v_fa" output with
    | Attribute.Float_array value -> Packed.Float_array.Private.view value | _ -> assert false in
  for corner = 0 to Geometry.vertex_count output - 1 do
    let primitive = corner / 3 and local = corner mod 3 in
    let wa,wb,wc = Extract.corner_barycentric ancestry primitive local in
    let source_local = if wa >= wb && wa >= wc then 0 else if wb >= wc then 1 else 2 in
    let is_left = Extract.primitive_side ancestry primitive = Rdk_boolean.Boolean_complex.Left in
    let base = if is_left then 10. else 20.
    and point_source = Extract.primitive_source_point ancestry primitive source_local
    and vertex_source = Extract.primitive_source_vertex ancestry primitive source_local in
    let point = topology.vertex_points.(corner) in
    check (pfloat.(point) = fixed_source_value base point_source)
      "point Float payload did not consolidate through exact source identity";
    check (vfloat.(corner) = fixed_source_value base vertex_source)
      "vertex Float payload did not follow source corner ancestry";
    let length = sqrt (normal.x.(corner) *. normal.x.(corner)
        +. normal.y.(corner) *. normal.y.(corner)
        +. normal.z.(corner) *. normal.z.(corner)) in
    check (abs_float (length -. 1.) < 1e-12)
      "interpolated vertex N was not normalized";
    let pfirst = pia.offsets.(point) and vfirst = vfa.offsets.(corner) in
    check (pia.offsets.(point + 1) - pfirst = 2
        && pia.values.(pfirst) = int_of_float base + (point_source * 2)
        && vfa.offsets.(corner + 1) - vfirst = 2
        && vfa.values.(vfirst) = base +. float_of_int (vertex_source * 2) /. 10.)
      "point/vertex CSR payload ancestry is wrong"
  done;
  let point_group = Option.get
      (Geometry.find_group ~owner:Group.Point "point_ordered" output)
  and vertex_group = Option.get
      (Geometry.find_group ~owner:Group.Vertex "vertex_ordered" output) in
  check (Group.is_ordered point_group && Group.is_ordered vertex_group)
    "ordered point/vertex groups lost ordered ownership";
  let point_order = Option.get (Group.ordered_elements point_group)
  and vertex_order = Option.get (Group.ordered_elements vertex_group) in
  let point_source point =
    let corner = ref (-1) in
    let index = ref 0 in
    while !corner < 0 && !index < Array.length topology.vertex_points do
      if topology.vertex_points.(!index) = point then corner := !index;
      incr index
    done;
    check (!corner >= 0) "ordered output point has no incident corner";
    let primitive = !corner / 3 and local = !corner mod 3 in
    (match Extract.primitive_side ancestry primitive with
     | Rdk_boolean.Boolean_complex.Left -> 0 | Rdk_boolean.Boolean_complex.Right -> 1),
    Extract.primitive_source_point ancestry primitive local in
  let vertex_source vertex =
    let primitive = vertex / 3 and local = vertex mod 3 in
    (match Extract.primitive_side ancestry primitive with
     | Rdk_boolean.Boolean_complex.Left -> 0 | Rdk_boolean.Boolean_complex.Right -> 1),
    Extract.primitive_source_vertex ancestry primitive local in
  check (Array.map point_source point_order = [|0,3; 0,1; 1,3; 1,1|])
    "ordered point group did not preserve operand/source traversal";
  check (Array.map vertex_source vertex_order = [|0,11; 0,2; 1,11; 1,2|])
    "ordered vertex group did not preserve operand/source traversal";
  let source_edges = Option.get (Geometry.find_edge_group "source_edges" output) in
  check (Edge_group.cardinality source_edges > 0)
    "native edge-group ancestry produced an empty disjoint result"

let corner_signature geometry =
  let pfloat = match find_storage Attribute.Point "p_float" geometry with
    | Attribute.Float value -> Array.copy value | _ -> assert false
  and vfloat = match find_storage Attribute.Vertex "v_float" geometry with
    | Attribute.Float value -> Array.copy value | _ -> assert false in
  let edges = Option.get (Geometry.find_edge_group "source_edges" geometry) in
  pfloat, vfloat, Array.init (Edge_group.length edges)
    (fun edge -> Edge_group.mem edge edges)

let test_corner_payload () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
      |> add_corner_payload 10.
  and right = tetra ~origin:(10.,0.,0.) ~base:20. ~with_label:false
      |> add_corner_payload 20. in
  let ancestry = make_ancestry left right in
  let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
  let output = Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:1e-12 ancestry primitive |> get in
  validate_corner_payload ancestry output;
  let run domains = Parallel.run ~domains (fun () ->
      let ancestry = make_ancestry left right in
      let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
      Payload.copy_points_and_vertices ~grain:1 ~point_conflict:Payload.Reject
        ~point_tolerance:1e-12
        ancestry primitive |> get |> corner_signature) in
  check (run 1 = run 4) "point/vertex Boolean payload differs between domain counts"

let add_seam_payload base geometry =
  let point = Attribute.create_owned ~name:"seam" ~owner:Attribute.Point
      (Attribute.Float (Array.make (Geometry.point_count geometry) base)) |> get_string in
  Geometry.with_attribute point geometry |> get_string

let test_point_conflict_policy () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
      |> add_seam_payload 1.
  and right = tetra ~origin:(0.5,0.2,0.2) ~base:20. ~with_label:false
      |> add_seam_payload 2. in
  let ancestry = make_ancestry left right in
  let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
  (match Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:1e-12 ancestry primitive with
   | Error error when Error.code error = "point_payload_conflict" -> ()
   | Error error -> fail "unexpected point conflict: %s" (Error.to_string error)
   | Ok _ -> fail "discontinuous point payload crossed a Boolean seam silently");
  let promoted = Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Promote_to_vertex ~point_tolerance:1e-12
      ancestry primitive |> get in
  check (Geometry.find_attribute ~owner:Attribute.Point "seam" promoted = None)
    "promoted point payload retained a conflicting point plane";
  let seam = match find_storage Attribute.Vertex "seam" promoted with
    | Attribute.Float value -> value | _ -> assert false in
  check (Array.exists ((=) 1.) seam && Array.exists ((=) 2.) seam)
    "promoted point payload lost one operand's seam values";
  let vertex = Attribute.create_owned ~name:"seam" ~owner:Attribute.Vertex
      (Attribute.Float (Array.make (Geometry.vertex_count left) 0.)) |> get_string in
  let left = Geometry.with_attribute vertex left |> get_string in
  let vertex = Attribute.create_owned ~name:"seam" ~owner:Attribute.Vertex
      (Attribute.Float (Array.make (Geometry.vertex_count right) 0.)) |> get_string in
  let right = Geometry.with_attribute vertex right |> get_string in
  let ancestry = make_ancestry left right in
  let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
  (match Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Promote_to_vertex ~point_tolerance:1e-12
      ancestry primitive with
   | Error error when Error.code error = "promotion_collision" -> ()
   | Error error -> fail "unexpected promotion collision: %s" (Error.to_string error)
   | Ok _ -> fail "point-to-vertex name collision was accepted")

let add_affine_payload geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and topology = Topology.Private.view (Geometry.topology geometry) in
  let values = Array.init (Geometry.point_count geometry) (fun point ->
      positions.x.(point) +. positions.y.(point) +. positions.z.(point)) in
  let point_attribute = Attribute.create_owned ~name:"affine" ~owner:Attribute.Point
      (Attribute.Float values) |> get_string in
  let vertex_values = Array.init (Geometry.vertex_count geometry) (fun vertex ->
      values.(topology.vertex_points.(vertex))) in
  let vertex_attribute = Attribute.create_owned ~name:"v_affine"
      ~owner:Attribute.Vertex (Attribute.Float vertex_values) |> get_string in
  geometry |> Geometry.with_attribute point_attribute |> get_string
  |> Geometry.with_attribute vertex_attribute |> get_string

let test_continuous_point_agreement () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
      |> add_affine_payload
  and right = tetra ~origin:(0.5,0.2,0.2) ~base:20. ~with_label:false
      |> add_affine_payload in
  let ancestry = make_ancestry left right in
  let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
  let output = Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:1e-12 ancestry primitive |> get in
  let affine = match find_storage Attribute.Point "affine" output with
    | Attribute.Float value -> value | _ -> assert false
  and vertex_affine = match find_storage Attribute.Vertex "v_affine" output with
    | Attribute.Float value -> value | _ -> assert false
  and positions = Packed.Float3.Private.view (Geometry.positions output) in
  for point = 0 to Geometry.point_count output - 1 do
    let expected = positions.x.(point) +. positions.y.(point) +. positions.z.(point) in
    check (abs_float (affine.(point) -. expected) < 1e-12)
      "continuous affine point payload changed across Boolean ancestry"
  done;
  let topology = Topology.Private.view (Geometry.topology output) in
  for vertex = 0 to Geometry.vertex_count output - 1 do
    let point = topology.vertex_points.(vertex) in
    let expected = positions.x.(point) +. positions.y.(point) +. positions.z.(point) in
    check (abs_float (vertex_affine.(vertex) -. expected) < 1e-12)
      "vertex barycentric interpolation changed an affine field"
  done;
  (match Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:(-1.) ancestry primitive with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected point tolerance error: %s" (Error.to_string error)
   | Ok _ -> fail "negative point payload tolerance was accepted")

let add_point_seam_group selected geometry =
  let group = Group.init ~owner:Group.Point ~name:"point_seam"
      (Geometry.point_count geometry) (fun _ -> selected) in
  Geometry.with_group group geometry |> get_string

let test_point_group_conflict_policy () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
      |> add_point_seam_group true
  and right = tetra ~origin:(0.5,0.2,0.2) ~base:20. ~with_label:false
      |> add_point_seam_group false in
  let ancestry = make_ancestry left right in
  let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
  (match Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:1e-12 ancestry primitive with
   | Error error when Error.code error = "point_group_conflict" -> ()
   | Error error -> fail "unexpected point group conflict: %s" (Error.to_string error)
   | Ok _ -> fail "discontinuous point group crossed a Boolean seam silently");
  let output = Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Promote_to_vertex ~point_tolerance:1e-12
      ancestry primitive |> get in
  check (Geometry.find_group ~owner:Group.Point "point_seam" output = None)
    "promoted point group retained point ownership";
  let group = Option.get
      (Geometry.find_group ~owner:Group.Vertex "point_seam" output) in
  check (Group.cardinality group > 0
      && Group.cardinality group < Geometry.vertex_count output)
    "promoted point group lost one side of its seam membership"

let add_named_attribute owner name storage geometry =
  let value = Attribute.create_owned ~name ~owner storage |> get_string in
  Geometry.with_attribute value geometry |> get_string

let test_corner_errors_and_cancellation () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
      |> add_named_attribute Attribute.Point "point_clash"
           (Attribute.Float (Array.make 4 0.))
  and right = tetra ~origin:(10.,0.,0.) ~base:20. ~with_label:false
      |> add_named_attribute Attribute.Point "point_clash"
           (Attribute.Int (Array.make 4 0)) in
  let ancestry = make_ancestry left right in
  let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
  (match Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:1e-12 ancestry primitive with
   | Error error when Error.code error = "attribute_storage_mismatch" -> ()
   | Error error -> fail "unexpected point schema error: %s" (Error.to_string error)
   | Ok _ -> fail "mismatched point attribute storage was accepted");
  (match Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:infinity ancestry primitive with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected infinite tolerance error: %s" (Error.to_string error)
   | Ok _ -> fail "infinite point payload tolerance was accepted");
  (match Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:nan ancestry primitive with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | Error error -> fail "unexpected NaN tolerance error: %s" (Error.to_string error)
   | Ok _ -> fail "NaN point payload tolerance was accepted");
  let unrelated = tetra ~origin:(20.,0.,0.) ~base:30. ~with_label:false in
  (match Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:1e-12 ancestry unrelated with
   | Error error when Error.code error = "geometry_mismatch" -> ()
   | Error error -> fail "unexpected target identity error: %s" (Error.to_string error)
   | Ok _ -> fail "unrelated corner payload target was accepted");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Payload.copy_points_and_vertices ~cancel ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:1e-12 ancestry primitive with
   | Error error when Error.code error = "cancelled" -> ()
   | Error error -> fail "unexpected corner cancellation error: %s"
       (Error.to_string error)
   | Ok _ -> fail "cancelled corner payload transfer completed")

let add_ragged_point_payload side geometry =
  let offsets = [|0;1;3;6;7|]
  and values = Array.init 7 (fun index -> side +. float_of_int index /. 10.) in
  geometry |> add_named_attribute Attribute.Point "ragged"
    (Attribute.Float_array
       (Packed.Float_array.Private.create_validated_owned ~offsets ~values))

let test_variable_float_array_policy () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
      |> add_ragged_point_payload 10.
  and right = tetra ~origin:(10.,0.,0.) ~base:20. ~with_label:false
      |> add_ragged_point_payload 20. in
  let ancestry = make_ancestry left right in
  let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
  let output = Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:1e-12 ancestry primitive |> get in
  let rows = match find_storage Attribute.Point "ragged" output with
    | Attribute.Float_array value -> Packed.Float_array.Private.view value
    | _ -> assert false in
  let output_positions = Packed.Float3.Private.view (Geometry.positions output) in
  let validate_source source side point =
    let positions = Packed.Float3.Private.view (Geometry.positions source) in
    let source_point = ref (-1) in
    for candidate = 0 to Geometry.point_count source - 1 do
      if positions.x.(candidate) = output_positions.x.(point)
          && positions.y.(candidate) = output_positions.y.(point)
          && positions.z.(candidate) = output_positions.z.(point) then
        source_point := candidate
    done;
    check (!source_point >= 0) "ragged output point lost its source coordinate";
    let expected_offsets = [|0;1;3;6;7|] in
    let expected_first = expected_offsets.(!source_point)
    and expected_last = expected_offsets.(!source_point + 1)
    and first = rows.offsets.(point) and last = rows.offsets.(point + 1) in
    check (last - first = expected_last - expected_first)
      "unequal Float-array fallback changed row length";
    for component = 0 to last - first - 1 do
      check (rows.values.(first + component)
          = side +. float_of_int (expected_first + component) /. 10.)
        "unequal Float-array fallback copied the wrong dominant row"
    done in
  for point = 0 to Geometry.point_count output - 1 do
    if output_positions.x.(point) < 5. then validate_source left 10. point
    else validate_source right 20. point
  done

let test_coincident_edge_group_union_ancestry () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
      |> add_edge_group "coincident_edges" (fun _ -> false)
  and right = tetra ~origin:(0.,0.,0.) ~base:20. ~with_label:false
      |> add_edge_group "coincident_edges" (fun _ -> true) in
  let run domains = Parallel.run ~domains (fun () ->
      let ancestry = make_ancestry left right in
      let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
      let output = Payload.copy_points_and_vertices ~grain:1
          ~point_conflict:Payload.Reject ~point_tolerance:1e-12
          ancestry primitive |> get in
      let group = Option.get
          (Geometry.find_edge_group "coincident_edges" output) in
      check (Edge_group.cardinality group = 6)
        "coincident Boolean discarded the non-preferred member's edge group";
      Array.init (Edge_group.length group) (fun edge -> Edge_group.mem edge group)) in
  check (run 1 = run 4)
    "coincident native edge-group ancestry differs between domain counts"

let test_same_operand_coincident_edge_group_union_ancestry () =
  let left = duplicated_tetra ~origin:(0.,0.,0.)
      |> add_edge_group_by_points "self_coincident_edges"
           (fun first second -> first >= 4 && second >= 4)
  and right = tetra ~origin:(10.,0.,0.) ~base:20. ~with_label:false
      |> add_edge_group "self_coincident_edges" (fun _ -> false) in
  let run domains = Parallel.run ~domains (fun () ->
      let ancestry = Solid.prepare ~resolve_left_self_intersections:true
          ~grain:1 ~left ~right () |> get
        |> Solid.extract_with_ancestry ~expression:Extract.difference |> get in
      let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
      let output = Payload.copy_points_and_vertices ~grain:1
          ~point_conflict:Payload.Reject ~point_tolerance:1e-12
          ancestry primitive |> get in
      let group = Option.get
          (Geometry.find_edge_group "self_coincident_edges" output) in
      check (Geometry.primitive_count output = 4
          && Edge_group.cardinality group = 6)
        "same-operand coincident member lost native edge-group ancestry";
      Array.init (Edge_group.length group) (fun edge -> Edge_group.mem edge group)) in
  check (run 1 = run 4)
    "same-operand coincident edge ancestry differs between domain counts"

let test_split_native_edge_ancestry () =
  let left = tetra ~origin:(0.,0.,0.) ~base:10. ~with_label:true
      |> add_edge_group "cut_edges" (fun _ -> false)
  and right = tetra ~origin:(0.5,0.2,0.2) ~base:20. ~with_label:false
      |> add_edge_group "cut_edges" (fun _ -> true) in
  let run domains = Parallel.run ~domains (fun () ->
      let ancestry = make_ancestry left right in
      let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
      let output = Payload.copy_points_and_vertices ~grain:1
          ~point_conflict:Payload.Reject ~point_tolerance:1e-12
          ancestry primitive |> get in
      let group = Option.get (Geometry.find_edge_group "cut_edges" output)
      and positions = Packed.Float3.Private.view (Geometry.positions output)
      and source = Packed.Float3.Private.view (Geometry.positions right) in
      let index = Topology_index.create (Geometry.topology output) in
      let is_original point =
        let found = ref false in
        for source_point = 0 to Geometry.point_count right - 1 do
          if positions.x.(point) = source.x.(source_point)
              && positions.y.(point) = source.y.(source_point)
              && positions.z.(point) = source.z.(source_point) then found := true
        done;
        !found in
      let split = ref false in
      Edge_group.iter (fun edge ->
        let a,b = Topology_index.edge_points index edge in
        if not (is_original a && is_original b) then split := true) group;
      check !split
        "transverse Boolean did not retain a split native source edge";
      Array.init (Edge_group.length group) (fun edge -> Edge_group.mem edge group)) in
  check (run 1 = run 4)
    "split native edge-group ancestry differs between domain counts"

let test_ngon_diagonals_are_not_native_edges () =
  let left = cube_quads ~origin:(0.,0.,0.)
      |> add_edge_group "cube_edges" (fun _ -> true)
  and right = tetra ~origin:(10.,0.,0.) ~base:20. ~with_label:false
      |> add_edge_group "cube_edges" (fun _ -> false) in
  let ancestry = make_ancestry left right in
  let primitive = Payload.copy_primitives ~grain:1 ancestry |> get in
  let output = Payload.copy_points_and_vertices ~grain:1
      ~point_conflict:Payload.Reject ~point_tolerance:1e-12
      ancestry primitive |> get in
  let group = Option.get (Geometry.find_edge_group "cube_edges" output) in
  check (Edge_group.cardinality group = 12)
    "triangulation diagonals inside source quads inherited native edge membership"

let test_surface_cut_payload () =
  let left = cube_quads ~origin:(0.,0.,0.) in
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|-1.; 2.; -1.; 2.|] ~y:[|0.5; 0.5; 0.5; 0.5|]
      ~z:[|0.25; 0.25; 0.75; 0.75|] in
  let topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;1;2; 1;3;2|] ~primitive_offsets:[|0;3;6|]
      |> get_string in
  let cut_id = Attribute.create_owned ~name:"cut_id" ~owner:Attribute.Primitive
      (Attribute.Int [|42;43|]) |> get_string
  and cut_group = Group.init ~owner:Group.Primitive ~name:"cut_surface" 2
      (fun _ -> true) in
  let right = Geometry.create ~positions ~topology ~attributes:[cut_id]
      ~groups:[cut_group] () |> get_string in
  let run domains = Parallel.run ~domains (fun () ->
      let ancestry = Solid.prepare ~grain:1 ~left ~right
          ~right_treatment:Solid.Surface () |> get
        |> Solid.extract_product_with_ancestry ~operation:Solid.Difference |> get in
      let output = Payload.copy_primitives ~grain:1 ancestry |> get in
      let ids = match find_storage Attribute.Primitive "cut_id" output with
        | Attribute.Int values -> values | _ -> assert false in
      let group = Option.get
          (Geometry.find_group ~owner:Group.Primitive "cut_surface" output) in
      let sheet_count = ref 0 in
      for primitive = 0 to Geometry.primitive_count output - 1 do
        match Extract.primitive_side ancestry primitive with
        | Rdk_boolean.Boolean_complex.Left ->
            check (ids.(primitive) = 0 && not (Group.mem primitive group))
              "solid side inherited surface-only cut payload"
        | Rdk_boolean.Boolean_complex.Right ->
            incr sheet_count;
            check ((ids.(primitive) = 42 || ids.(primitive) = 43)
                && Group.mem primitive group)
              "paired surface cut wall lost its primitive payload"
      done;
      check (!sheet_count > 0 && !sheet_count land 1 = 0)
        "surface cut payload did not cover paired walls";
      Array.copy ids,
      Array.init (Group.length group) (fun element -> Group.mem element group)) in
  check (run 1 = run 4)
    "surface cut payload differs between one and four domains"

let run () =
  test_complete_primitive_payload ();
  test_schema_and_parameter_errors ();
  test_cancellation ();
  test_corner_payload ();
  test_point_conflict_policy ();
  test_continuous_point_agreement ();
  test_point_group_conflict_policy ();
  test_corner_errors_and_cancellation ();
  test_variable_float_array_policy ();
  test_coincident_edge_group_union_ancestry ();
  test_same_operand_coincident_edge_group_union_ancestry ();
  test_split_native_edge_ancestry ();
  test_ngon_diagonals_are_not_native_edges ();
  test_surface_cut_payload ()
