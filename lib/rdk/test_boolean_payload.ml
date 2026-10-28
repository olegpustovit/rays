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
      (Packed.Float3.View.of_owned_exn ~x:(Array.copy x) ~y:(Array.copy y)
        ~z:(Array.copy z)));
    attribute "rgba" (Attribute.Float4
      (float4 (Array.copy x) (Array.copy y) (Array.copy z) w));
    attribute "int_rows" (Attribute.Int_array
      (Packed.Int_array.View.create_validated_owned
        ~offsets:(Array.copy offsets) ~values:int_values));
    attribute "float_rows" (Attribute.Float_array
      (Packed.Float_array.View.create_validated_owned
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
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
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
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
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
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let find_storage owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | None -> fail "missing attribute %s" name
  | Some attribute -> Attribute.View.storage attribute

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
    | Attribute.Float2 value -> Packed.Float2.View.view value | _ -> assert false
  and directions = match find_storage Attribute.Primitive "direction" output with
    | Attribute.Float3 value -> Packed.Float3.View.view value | _ -> assert false
  and colors = match find_storage Attribute.Primitive "rgba" output with
    | Attribute.Float4 value -> Packed.Float4.View.view value | _ -> assert false
  and int_rows = match find_storage Attribute.Primitive "int_rows" output with
    | Attribute.Int_array value -> Packed.Int_array.View.view value | _ -> assert false
  and float_rows = match find_storage Attribute.Primitive "float_rows" output with
    | Attribute.Float_array value -> Packed.Float_array.View.view value | _ -> assert false in
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
        (Attribute.Float3 (Packed.Float3.View.of_owned_exn
          ~x:(Array.copy x) ~y:(Array.copy y) ~z:(Array.copy z)));
      make owner (prefix ^ "_f4")
        (Attribute.Float4 (float4 x y z w));
      make owner (prefix ^ "_ia") (Attribute.Int_array
        (Packed.Int_array.View.create_validated_owned
          ~offsets:(Array.copy offsets) ~values:int_values));
      make owner (prefix ^ "_fa") (Attribute.Float_array
        (Packed.Float_array.View.create_validated_owned
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

let corner_signature geometry =
  let pfloat = match find_storage Attribute.Point "p_float" geometry with
    | Attribute.Float value -> Array.copy value | _ -> assert false
  and vfloat = match find_storage Attribute.Vertex "v_float" geometry with
    | Attribute.Float value -> Array.copy value | _ -> assert false in
  let edges = Option.get (Geometry.find_edge_group "source_edges" geometry) in
  pfloat, vfloat, Array.init (Edge_group.length edges)
    (fun edge -> Edge_group.mem edge edges)

let add_seam_payload base geometry =
  let point = Attribute.create_owned ~name:"seam" ~owner:Attribute.Point
      (Attribute.Float (Array.make (Geometry.point_count geometry) base)) |> get_string in
  Geometry.with_attribute point geometry |> get_string

let add_affine_payload geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry) in
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

let add_point_seam_group selected geometry =
  let group = Group.init ~owner:Group.Point ~name:"point_seam"
      (Geometry.point_count geometry) (fun _ -> selected) in
  Geometry.with_group group geometry |> get_string

let add_named_attribute owner name storage geometry =
  let value = Attribute.create_owned ~name ~owner storage |> get_string in
  Geometry.with_attribute value geometry |> get_string

let add_ragged_point_payload side geometry =
  let offsets = [|0;1;3;6;7|]
  and values = Array.init 7 (fun index -> side +. float_of_int index /. 10.) in
  geometry |> add_named_attribute Attribute.Point "ragged"
    (Attribute.Float_array
       (Packed.Float_array.View.create_validated_owned ~offsets ~values))

let test_surface_cut_payload () =
  let left = cube_quads ~origin:(0.,0.,0.) in
  let positions = Packed.Float3.View.of_owned_exn
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
  test_surface_cut_payload ()
