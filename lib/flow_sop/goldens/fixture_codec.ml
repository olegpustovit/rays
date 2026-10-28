open Rdk

let form node = Flow.Syntax.make node
let sym value = form (Flow.Syntax.Sym value)
let str value = form (Flow.Syntax.Str value)
let num value = form (Flow.Syntax.Num (string_of_int value))
let list values = form (Flow.Syntax.List values)
let tagged tag values = list (sym tag :: values)

let floats values =
  list
    (Array.to_list
       (Array.map (fun value -> str (Printf.sprintf "%016Lx" (Int64.bits_of_float value))) values))

let ints values = list (Array.to_list (Array.map num values))

let attribute_owner = function
  | Attribute.Point -> "point"
  | Vertex -> "vertex"
  | Primitive -> "primitive"
  | Detail -> "detail"

let group_owner = function Group.Point -> "point" | Vertex -> "vertex" | Primitive -> "primitive"

let encode geometry =
  let position = Packed.Float3.View.view (Geometry.positions geometry) in
  let topology = Geometry.topology geometry in
  let topology_view = Topology.View.view topology in
  let topology_form =
    tagged "topology"
      [
        num topology_view.point_count;
        ints topology_view.vertex_points;
        ints topology_view.primitive_offsets;
        list
          (List.init (Topology.primitive_count topology) (fun i ->
               sym
                 (match Topology.primitive_kind topology i with
                 | Polygon -> "polygon"
                 | Open_polyline -> "open_polyline"
                 | Closed_polyline -> "closed_polyline")));
      ]
  in
  let attribute value =
    let kind, planes =
      match Attribute.storage value with
      | Attribute.Float xs -> ("float", [ floats xs ])
      | Int xs -> ("int", [ ints xs ])
      | Text xs -> ("text", [ list (Array.to_list (Array.map str xs)) ])
      | Float2 xs ->
          let v = Packed.Float2.View.view xs in
          ("float2", [ floats v.x; floats v.y ])
      | Float3 xs ->
          let v = Packed.Float3.View.view xs in
          ("float3", [ floats v.x; floats v.y; floats v.z ])
      | Float4 xs ->
          let v = Packed.Float4.View.view xs in
          ("float4", [ floats v.x; floats v.y; floats v.z; floats v.w ])
      | Int_array xs ->
          let v = Packed.Int_array.View.view xs in
          ("int_array", [ ints v.offsets; ints v.values ])
      | Float_array xs ->
          let v = Packed.Float_array.View.view xs in
          ("float_array", [ ints v.offsets; floats v.values ])
    in
    tagged "attribute"
      [
        sym (attribute_owner (Attribute.owner value));
        str (Attribute.name value);
        sym kind;
        list planes;
      ]
  in
  let group value =
    let members =
      Array.init (Group.length value) Fun.id
      |> Array.to_list
      |> List.filter (fun i -> Group.mem i value)
      |> List.map num
    in
    let order =
      match Group.ordered_elements value with
      | None -> [ sym "unordered"; list members ]
      | Some values -> [ sym "ordered"; ints values ]
    in
    tagged "group" [ sym (group_owner (Group.owner value)); str (Group.name value); list order ]
  in
  let edge_group value =
    let members =
      Array.init (Edge_group.length value) Fun.id
      |> Array.to_list
      |> List.filter (fun i -> Edge_group.mem i value)
      |> List.map num
    in
    tagged "edge_group" [ str (Edge_group.name value); list members ]
  in
  let root =
    tagged "geometry"
      [
        tagged "positions" [ floats position.x; floats position.y; floats position.z ];
        topology_form;
        tagged "attributes" (List.map attribute (Geometry.attributes geometry));
        tagged "groups" (List.map group (Geometry.groups geometry));
        tagged "edge_groups" (List.map edge_group (Geometry.edge_groups geometry));
      ]
  in
  Flow.Lisp.flat root

let malformed what = failwith ("malformed frozen geometry: " ^ what)

let children tag form =
  match form.Flow.Syntax.node with
  | List (head :: values) when head.node = Sym tag -> values
  | _ -> malformed ("expected " ^ tag)

let symbol = function
  | { Flow.Syntax.node = Sym value; _ } -> value
  | _ -> malformed "expected symbol"

let text = function
  | { Flow.Syntax.node = Str value; _ } -> value
  | _ -> malformed "expected string"

let integer = function
  | { Flow.Syntax.node = Num value; _ } -> (
      try int_of_string value with Failure _ -> malformed "invalid integer")
  | _ -> malformed "expected integer"

let array decode form =
  match form.Flow.Syntax.node with
  | List values -> Array.of_list (List.map decode values)
  | _ -> malformed "expected list"

let float_value value =
  let bits = text value in
  let hex = function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false in
  if String.length bits <> 16 || not (String.for_all hex bits) then malformed "invalid float bits";
  try Int64.float_of_bits (Int64.of_string ("0x" ^ bits))
  with Failure _ -> malformed "invalid float bits"

let float_array form = array float_value form
let int_array form = array integer form
let text_array form = array text form

let parsed_owner = function
  | "point" -> (Attribute.Point, Group.Point)
  | "vertex" -> (Vertex, Group.Vertex)
  | "primitive" -> (Primitive, Group.Primitive)
  | "detail" -> (Detail, Group.Point)
  | _ -> malformed "unknown owner"

let checked = function Ok value -> value | Error message -> malformed message

let decode source =
  let root =
    match Flow.Syntax.parse source with
    | Ok [ root ] -> root
    | Ok _ -> malformed "expected one top-level geometry"
    | Error diagnostic -> malformed (Diagnostic.to_string diagnostic)
  in
  let fields = children "geometry" root in
  let unique tag =
    match
      List.filter
        (fun value ->
          match value.Flow.Syntax.node with
          | List ({ node = Sym name; _ } :: _) -> name = tag
          | _ -> false)
        fields
    with
    | [ value ] -> value
    | _ -> malformed ("missing or duplicate " ^ tag)
  in
  let positions =
    match children "positions" (unique "positions") with
    | [ x; y; z ] ->
        Packed.Float3.of_owned ~x:(float_array x) ~y:(float_array y) ~z:(float_array z) |> checked
    | _ -> malformed "positions require x/y/z planes"
  in
  let topology =
    match children "topology" (unique "topology") with
    | [ points; vertices; offsets; kinds ] ->
        let kinds =
          array
            (fun value ->
              match symbol value with
              | "polygon" -> Topology.Polygon
              | "open_polyline" -> Open_polyline
              | "closed_polyline" -> Closed_polyline
              | _ -> malformed "unknown primitive kind")
            kinds
        in
        Topology.create_owned ~point_count:(integer points) ~vertex_points:(int_array vertices)
          ~primitive_offsets:(int_array offsets) ~primitive_kinds:kinds
        |> checked
    | _ -> malformed "invalid topology"
  in
  let attributes =
    List.map
      (fun value ->
        match children "attribute" value with
        | [ owner_value; name; kind; payload ] ->
            let owner, _ = parsed_owner (symbol owner_value) in
            let planes =
              match payload.node with List values -> values | _ -> malformed "attribute planes"
            in
            let storage =
              match (symbol kind, planes) with
              | "float", [ values ] -> Attribute.Float (float_array values)
              | "int", [ values ] -> Int (int_array values)
              | "text", [ values ] -> Text (text_array values)
              | "float2", [ x; y ] ->
                  Float2 (Packed.Float2.of_owned ~x:(float_array x) ~y:(float_array y) |> checked)
              | "float3", [ x; y; z ] ->
                  Float3
                    (Packed.Float3.of_owned ~x:(float_array x) ~y:(float_array y) ~z:(float_array z)
                    |> checked)
              | "float4", [ x; y; z; w ] ->
                  Float4
                    (Packed.Float4.of_owned ~x:(float_array x) ~y:(float_array y) ~z:(float_array z)
                       ~w:(float_array w)
                    |> checked)
              | "int_array", [ offsets; values ] ->
                  Int_array
                    (Packed.Int_array.create_owned ~offsets:(int_array offsets)
                       ~values:(int_array values)
                    |> checked)
              | "float_array", [ offsets; values ] ->
                  Float_array
                    (Packed.Float_array.create_owned ~offsets:(int_array offsets)
                       ~values:(float_array values)
                    |> checked)
              | _ -> malformed "invalid attribute storage"
            in
            Attribute.create_owned ~owner ~name:(text name) storage |> checked
        | _ -> malformed "invalid attribute")
      (children "attributes" (unique "attributes"))
  in
  let groups =
    List.map
      (fun value ->
        match children "group" value with
        | [ owner_value; name; payload ] ->
            let _, owner = parsed_owner (symbol owner_value) in
            if symbol owner_value = "detail" then malformed "groups cannot use detail owner";
            let order =
              match payload.node with
              | List [ kind; members ] ->
                  let values = int_array members in
                  ( (match symbol kind with
                    | "ordered" -> Some values
                    | "unordered" -> None
                    | _ -> malformed "unknown group order"),
                    values )
              | _ -> malformed "invalid group members"
            in
            let order_kind, values = order in
            let length =
              match owner with
              | Group.Point -> Topology.point_count topology
              | Vertex -> Topology.vertex_count topology
              | Primitive -> Topology.primitive_count topology
            in
            let group =
              match order_kind with
              | Some order -> Group.ordered ~owner ~name:(text name) ~length order |> checked
              | None ->
                  let members = Array.make length false in
                  Array.iter
                    (fun index ->
                      if index < 0 || index >= length || members.(index) then
                        malformed "invalid group member";
                      members.(index) <- true)
                    values;
                  Group.init ~grain:4096 ~owner ~name:(text name) length (Array.get members)
            in
            group
        | _ -> malformed "invalid group")
      (children "groups" (unique "groups"))
  in
  let edge_groups =
    List.map
      (fun value ->
        match children "edge_group" value with
        | [ name; members ] ->
            let index = Topology_index.create topology in
            let member_values = int_array members in
            let members = Array.make (Topology_index.edge_count index) false in
            Array.iter
              (fun edge ->
                if edge < 0 || edge >= Array.length members || members.(edge) then
                  malformed "invalid edge group member";
                members.(edge) <- true)
              member_values;
            Edge_group.init ~grain:4096 ~topology ~index ~name:(text name) (Array.get members)
        | _ -> malformed "invalid edge group")
      (children "edge_groups" (unique "edge_groups"))
  in
  Geometry.create ~positions ~topology ~attributes ~groups ~edge_groups () |> checked

let encode_image image =
  let payload =
    match Sop.Image.rgba8 image with
    | Some bytes ->
        tagged "rgba8"
          [
            str
              (String.init
                 (Bytes.length bytes * 2)
                 (fun i ->
                   let byte = Char.code (Bytes.get bytes (i / 2)) in
                   "0123456789abcdef".[if i mod 2 = 0 then byte lsr 4 else byte land 15]));
          ]
    | None -> tagged "rgba" [ floats (Sop.Image.storage image) ]
  in
  Flow.Lisp.flat
    (tagged "image" [ num (Sop.Image.width image); num (Sop.Image.height image); payload ])

let decode_image source =
  let root =
    match Flow.Syntax.parse source with
    | Ok [ root ] -> root
    | Ok _ -> malformed "expected one top-level image"
    | Error diagnostic -> malformed (Diagnostic.to_string diagnostic)
  in
  match children "image" root with
  | [ width; height; storage ] -> (
      let width = integer width and height = integer height in
      let image =
        match storage.node with
        | List [ { node = Sym "rgba8"; _ }; payload ] ->
            let hex = text payload in
            if String.length hex mod 2 <> 0 then malformed "odd rgba8 hex length";
            let bytes = Bytes.create (String.length hex / 2) in
            for i = 0 to Bytes.length bytes - 1 do
              try Bytes.set bytes i (Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))
              with Failure _ -> malformed "invalid rgba8 hex"
            done;
            Sop.Image.of_owned_rgba8 ~width ~height bytes
        | List [ { node = Sym "rgba"; _ }; values ] ->
            Sop.Image.of_owned_rgba ~width ~height (float_array values)
        | _ -> malformed "invalid image storage"
      in
      match image with
      | Ok value -> value
      | Error error -> malformed (Diagnostic.error_to_string error))
  | _ -> malformed "invalid image"
