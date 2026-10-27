open Rdk
module P = Param

let text value = P.Text_value value
let choice value = P.Choice_value value
let integer value = P.Int_value value
let float value = P.Float_value value
let boolean value = P.Bool_value value
let value_text = function
  | P.Text_value value | P.Choice_value value -> Printf.sprintf "%S" value
  | Int_value value -> string_of_int value
  | Float_value value -> Flow.Lisp.float value
  | Bool_value value -> string_of_bool value
let change_case key label values =
  let signature = String.concat "\n" (List.map (fun (name,value) -> name ^ "=" ^ value_text value) values) in
  let identity = Digest.to_hex (Digest.string signature) in
  key, Printf.sprintf "%s [%s]" label identity, values

let node_keys = ["line"; "noise_displace"; "set_color"; "set_float"; "set_int";
  "set_vector"; "set_orient"; "set_transform"; "delete_attributes";
  "delete_edge_group"; "group_delete"; "group_rename"; "rename_attributes";
  "rename_edge_group"; "swap_attributes"]

let input_fixture () =
  let geometry = List.assoc "mixed" Port_gate.standard_inputs |> List.hd in
  let n = Geometry.point_count geometry in
  let normal = Packed.Float3.of_owned ~x:(Array.make n 0.) ~y:(Array.make n 1.) ~z:(Array.make n 0.)
      |> Rdk_test_support.get_string_ok in
  let geometry = Geometry.with_attribute (Attribute.create_owned ~name:"N" ~owner:Attribute.Point (Attribute.Float3 normal)
      |> Rdk_test_support.get_string_ok) geometry |> Rdk_test_support.get_string_ok in
  let owners = [Attribute.Point; Vertex; Primitive; Detail] in
  let count = function Attribute.Point -> Geometry.point_count geometry | Vertex -> Geometry.vertex_count geometry
    | Primitive -> Geometry.primitive_count geometry | Detail -> 1 in
  let geometry = List.fold_left (fun geometry owner ->
    let length = count owner in
    let vec2 = Packed.Float2.of_owned ~x:(Array.make length 1.) ~y:(Array.make length 2.)
        |> Rdk_test_support.get_string_ok in
    List.fold_left (fun geometry (name, storage) -> Geometry.with_attribute
      (Attribute.create_owned ~name ~owner storage |> Rdk_test_support.get_string_ok) geometry
      |> Rdk_test_support.get_string_ok) geometry
      ["old_float", Attribute.Float (Array.init length float_of_int);
       "old_int", Attribute.Int (Array.init length Fun.id);
       "old_vec2", Attribute.Float2 vec2;
       "old_text", Attribute.Text (Array.make length "fixture")]) geometry owners in
  let geometry = Geometry.with_group
      (Group.init ~owner:Group.Point ~name:"é猫\"" (Geometry.point_count geometry) (fun i -> i = 0)) geometry
      |> Rdk_test_support.get_string_ok in
  let topology = Geometry.topology geometry in
  let edge = Edge_group.init ~topology ~index:(Topology_index.create topology) ~name:"old_edges" (fun i -> i mod 2 = 0) in
  Geometry.with_edge_group edge geometry |> Rdk_test_support.get_string_ok

let with_normal geometry =
  List.fold_left (fun geometry owner ->
    let count = match owner with Attribute.Point -> Geometry.point_count geometry
      | Vertex -> Geometry.vertex_count geometry | _ -> assert false in
    let normal = Packed.Float3.of_owned ~x:(Array.make count 0.) ~y:(Array.make count 1.)
        ~z:(Array.make count 0.) |> Rdk_test_support.get_string_ok in
    Geometry.with_attribute (Attribute.create_owned ~name:"N" ~owner (Attribute.Float3 normal)
        |> Rdk_test_support.get_string_ok) geometry |> Rdk_test_support.get_string_ok)
    geometry [Attribute.Point; Vertex]

let noise_points count =
  Kernel.generate_point_ranges count (fun ~first ~last ~x ~y ~z ->
    for i = first to last - 1 do
      x.(i) <- float_of_int (i mod 65) *. 0.13 -. 0.4;
      y.(i) <- sin (float_of_int i *. 0.27);
      z.(i) <- float_of_int (i / 65) *. 0.19 -. 0.3
    done)

let with_noise_payload geometry =
  let add name owner storage geometry = Geometry.with_attribute
      (Attribute.create_owned ~name ~owner storage |> Rdk_test_support.get_string_ok) geometry
      |> Rdk_test_support.get_string_ok in
  let vector count f = Packed.Float3.Private.of_owned_exn
      ~x:(Array.init count f) ~y:(Array.make count 0.37) ~z:(Array.make count (-1.2)) in
  let geometry = add "N" Attribute.Point
      (Attribute.Float3 (vector (Geometry.point_count geometry) (fun i -> cos (float_of_int i *. 0.23)))) geometry in
  let geometry = add "N" Attribute.Vertex
      (Attribute.Float3 (vector (Geometry.vertex_count geometry) (fun _ -> 1.))) geometry in
  let geometry = add "id" Attribute.Point
      (Attribute.Int (Array.init (Geometry.point_count geometry) Fun.id)) geometry in
  let geometry = add "weight" Attribute.Primitive
      (Attribute.Float (Array.init (Geometry.primitive_count geometry) float_of_int)) geometry in
  let geometry = add "tag" Attribute.Detail (Attribute.Text [|"preserved"|]) geometry in
  Geometry.with_group (Group.init ~owner:Group.Point ~name:"alternating" (Geometry.point_count geometry)
      (fun i -> i mod 2 = 0)) geometry |> Rdk_test_support.get_string_ok

let noise_mixed () =
  let topology = Topology.create_owned ~point_count:6 ~vertex_points:[|0;1;2;3;1;4;5|]
      ~primitive_offsets:[|0;4;7|] ~primitive_kinds:[|Topology.Polygon;Topology.Open_polyline|]
      |> Rdk_test_support.get_string_ok in
  let geometry = Geometry.create ~positions:(Geometry.positions (noise_points 6)) ~topology ()
      |> Rdk_test_support.get_string_ok |> with_noise_payload in
  let geometry = Geometry.with_group (Group.init ~owner:Group.Vertex ~name:"corners" 7 (fun i -> i mod 3 = 0)) geometry
      |> Rdk_test_support.get_string_ok in
  let geometry = Geometry.with_group (Group.init ~owner:Group.Primitive ~name:"polygon" 2 (fun i -> i = 0)) geometry
      |> Rdk_test_support.get_string_ok in
  let edge = Edge_group.init ~topology ~index:(Topology_index.create topology) ~name:"selected_edges"
      (fun i -> i mod 2 = 0) in
  Geometry.with_edge_group edge geometry |> Rdk_test_support.get_string_ok

let noise_grid () =
  Plane_generators.grid ~connectivity:Grid_quads ~columns:64 ~rows:64 ~size:8. ()
  |> Rdk_test_support.get_ok |> with_noise_payload

let with_color geometry =
  List.fold_left (fun geometry owner ->
    let count = match owner with Attribute.Point -> Geometry.point_count geometry | Vertex -> Geometry.vertex_count geometry
      | Primitive -> Geometry.primitive_count geometry | Detail -> 1 in
    let color = Attribute.Float4 (Packed.Float4.of_owned ~x:(Array.make count 0.25)
        ~y:(Array.make count 0.5) ~z:(Array.make count 0.75) ~w:(Array.make count 1.)
        |> Rdk_test_support.get_string_ok) in
    Geometry.with_attribute (Attribute.create_owned ~name:"Cd" ~owner color
        |> Rdk_test_support.get_string_ok) geometry |> Rdk_test_support.get_string_ok)
    geometry [Attribute.Point; Vertex; Primitive; Detail]

let with_color_groups geometry =
  List.fold_left (fun geometry (owner, group_owner) ->
    let count = match owner with Attribute.Point -> Geometry.point_count geometry
      | Vertex -> Geometry.vertex_count geometry | Primitive -> Geometry.primitive_count geometry | Detail -> assert false in
    let geometry = Geometry.with_group
        (Group.ordered ~owner:group_owner ~name:"ordered" ~length:count
          (Array.init ((count+1)/2) (fun i -> ((count-1)/2-i)*2))
         |> Rdk_test_support.get_string_ok) geometry |> Rdk_test_support.get_string_ok in
    List.fold_left (fun geometry (name,selected) -> Geometry.with_group
      (Group.init ~owner:group_owner ~name count selected) geometry
      |> Rdk_test_support.get_string_ok) geometry
      ["selected",(fun i -> i mod 2 = 0);"full",(fun _ -> true);"empty",(fun _ -> false)])
    geometry [Attribute.Point,Group.Point;Vertex,Group.Vertex;Primitive,Group.Primitive]

let with_cd_storage geometry storage =
  List.fold_left (fun geometry owner ->
    let count = match owner with Attribute.Point -> Geometry.point_count geometry | Vertex -> Geometry.vertex_count geometry
      | Primitive -> Geometry.primitive_count geometry | Detail -> 1 in
    let floats () = Array.init count (fun i -> float_of_int i *. 0.125) in
    let storage = match storage with
      | "float" -> Attribute.Float (floats ())
      | "int" -> Attribute.Int (Array.init count Fun.id)
      | "text" -> Attribute.Text (Array.make count "wrong")
      | "float2" -> Attribute.Float2 (Packed.Float2.of_owned ~x:(Array.make count 1.)
          ~y:(Array.make count 2.) |> Rdk_test_support.get_string_ok)
      | "float3" -> Attribute.Float3 (Packed.Float3.of_owned ~x:(Array.make count 1.)
          ~y:(Array.make count 2.) ~z:(Array.make count 3.) |> Rdk_test_support.get_string_ok)
      | "float4" -> Attribute.Float4 (Packed.Float4.of_owned ~x:(Array.make count 1.)
          ~y:(Array.make count 2.) ~z:(Array.make count 3.) ~w:(Array.make count 4.)
          |> Rdk_test_support.get_string_ok)
      | "float_array" -> Attribute.Float_array (Packed.Float_array.create_owned
          ~offsets:(Array.init (count + 1) Fun.id) ~values:(floats ()) |> Rdk_test_support.get_string_ok)
      | "int_array" -> Attribute.Int_array (Packed.Int_array.create_owned
          ~offsets:(Array.init (count + 1) Fun.id) ~values:(Array.init count Fun.id) |> Rdk_test_support.get_string_ok)
      | _ -> assert false in
    Geometry.with_attribute (Attribute.create_owned ~name:"Cd" ~owner storage
        |> Rdk_test_support.get_string_ok) geometry |> Rdk_test_support.get_string_ok)
    geometry [Attribute.Point; Vertex; Primitive; Detail]

let color_raw geometry channel value =
  List.fold_left (fun geometry owner ->
    let count = match owner with Attribute.Point -> Geometry.point_count geometry
      | Vertex -> Geometry.vertex_count geometry | Primitive -> Geometry.primitive_count geometry | Detail -> 1 in
    let component index = Array.init count (fun i -> if index=channel && i<2 then value else 0.5) in
    let color = Attribute.Float4 (Packed.Float4.of_owned ~x:(component 0) ~y:(component 1)
        ~z:(component 2) ~w:(component 3) |> Rdk_test_support.get_string_ok) in
    Geometry.with_attribute (Attribute.create_owned ~owner ~name:"Cd" color
        |> Rdk_test_support.get_string_ok) geometry |> Rdk_test_support.get_string_ok)
    geometry [Attribute.Point; Vertex; Primitive; Detail]

let raw_color_fixtures geometry =
  List.concat_map (fun channel -> List.map (fun (label,value) ->
      "raw_" ^ string_of_int channel ^ "_" ^ label, [color_raw geometry channel value])
    ["nan",Int64.float_of_bits 0x7ff8000000000123L; "negative_infinity",neg_infinity]) [0;1;2;3]

let test_fixtures () = List.map (fun (label, geometries) -> label, List.map with_normal geometries)
    Port_gate.standard_inputs

let metadata_fixture () =
  let points count = Port_gate.points count in
  let topology = Topology.create_owned ~point_count:6 ~vertex_points:[|0;1;2;3;1;4;5|]
      ~primitive_offsets:[|0;4;7|] ~primitive_kinds:[|Topology.Polygon;Topology.Open_polyline|]
      |> Rdk_test_support.get_string_ok in
  let geometry = Geometry.create ~positions:(Geometry.positions (points 6)) ~topology ()
      |> Rdk_test_support.get_string_ok in
  let add owner count geometry =
    let array value = Array.make count value in
    let csr = Packed.Float_array.create_owned ~offsets:(Array.init (count+1) (fun i -> i*2))
        ~values:(Array.make (count*2) (-0.)) |> Rdk_test_support.get_string_ok in
    let int_csr = Packed.Int_array.create_owned ~offsets:(Array.init (count+1) (fun i -> i*2))
        ~values:(Array.make (count*2) max_int) |> Rdk_test_support.get_string_ok in
    let float3 = Packed.Float3.of_owned ~x:(array 2.) ~y:(array (-0.)) ~z:(array 0.5)
        |> Rdk_test_support.get_string_ok in
    List.fold_left (fun geometry (name, storage) -> Geometry.with_attribute
      (Attribute.create_owned ~owner ~name storage |> Rdk_test_support.get_string_ok) geometry
      |> Rdk_test_support.get_string_ok) geometry
      ["old_float",Attribute.Float (array (-0.)); "old_int",Attribute.Int (array max_int);
       "old_vec2",Attribute.Float2 (Packed.Float2.of_owned ~x:(array (-0.)) ~y:(array 2.) |> Rdk_test_support.get_string_ok);
       "old_vec3",Attribute.Float3 float3;
       "old_vec4",Attribute.Float4 (Packed.Float4.of_owned ~x:(array (-0.)) ~y:(array 2.) ~z:(array 3.) ~w:(array 4.) |> Rdk_test_support.get_string_ok);
       "old_text",Attribute.Text (array "猫\t😀"); "old_float_array",Attribute.Float_array csr;
       "old_int_array",Attribute.Int_array int_csr] in
  let geometry = List.fold_left (fun geometry (owner,count) -> add owner count geometry) geometry
      [Attribute.Point,6;Vertex,7;Primitive,2;Detail,1] in
  let geometry = Geometry.with_attribute
      (Attribute.create_owned ~owner:Attribute.Vertex ~name:"P" (Attribute.Float (Array.make 7 (-0.)))
       |> Rdk_test_support.get_string_ok) geometry |> Rdk_test_support.get_string_ok in
  let geometry = List.fold_left (fun geometry name -> Geometry.with_attribute
      (Attribute.create_owned ~owner:Attribute.Point ~name (Attribute.Text (Array.make 6 "猫\t😀"))
       |> Rdk_test_support.get_string_ok) geometry |> Rdk_test_support.get_string_ok)
      geometry ["é猫";"a-b";"back\\slash";"a b";"[x]";"*"] in
  let group owner count name indices = Group.ordered ~owner ~length:count ~name indices |> Rdk_test_support.get_string_ok in
  let geometry = List.fold_left (fun geometry group -> Geometry.with_group group geometry |> Rdk_test_support.get_string_ok)
      geometry [group Group.Point 6 "old_points" [|4;1;3|];group Point 6 "target" [|2;4|];
        group Point 6 "old" [|4;1|];
        group Point 6 "unused" [||];group Vertex 7 "old_vertices" [|6;0|];
        group Vertex 7 "old" [|6;0|];group Vertex 7 "target" [|0;4|];
        group Primitive 2 "old_primitives" [|1|];
        group Primitive 2 "old" [|1|];group Primitive 2 "target" [|0|];
        group Point 6 "alternating" [|0;2;4|];group Vertex 7 "corners" [|0;3;6|];
        group Primitive 2 "polygon" [|0|];group Point 6 "é猫" [|1|];
        group Point 6 "a-b" [|2|];group Point 6 "back\\slash" [|3|];
        group Point 6 "a b" [|4|];group Point 6 "[x]" [|5|];group Point 6 "*" [|0|]] in
  let topology = Geometry.topology geometry and index = Topology_index.create (Geometry.topology geometry) in
  let edge name selected = Edge_group.init ~topology ~index ~name selected in
  let edges = edge "edges" (fun edge -> edge < 2) in
  let geometry = Geometry.with_edge_group edges geometry |> Rdk_test_support.get_string_ok in
  let geometry = Geometry.with_edge_group (edge "old_edges" (fun edge -> edge < 2)) geometry
      |> Rdk_test_support.get_string_ok in
  let geometry = Geometry.with_edge_group (edge "selected_edges" (fun edge -> edge mod 2 = 0)) geometry
      |> Rdk_test_support.get_string_ok in
  let geometry = Geometry.with_edge_group (edge "old" (fun edge -> edge < 2)) geometry
      |> Rdk_test_support.get_string_ok in
  Geometry.with_edge_group (edge "target" (fun edge -> edge = 1 || edge = 2)) geometry
      |> Rdk_test_support.get_string_ok

let metadata_grid () =
  let size = 65 in
  let vertex_points = Array.init (64*64*4) (fun index ->
    let cell = index / 4 and corner = index mod 4 in
    let x = cell mod 64 and y = cell / 64 in
    (y + (if corner >= 2 then 1 else 0))*size + x + (if corner=1 || corner=2 then 1 else 0)) in
  let topology = Topology.polygons_owned ~point_count:(size*size) ~vertex_points
      ~primitive_offsets:(Array.init (64*64+1) (fun i -> i*4)) |> Rdk_test_support.get_string_ok in
  Geometry.create ~positions:(Geometry.positions (Port_gate.points (size*size))) ~topology ()
  |> Rdk_test_support.get_string_ok

let metadata_rules rows = Param.String_table.encode rows
let rule label key rows =
  change_case key label ["rules", text (metadata_rules rows)]

let authored_metadata_cases () =
  let table rows = Param.String_table.encode rows in
  let rules key rows = change_case key ("authored rules " ^ key) ["rules", text (table rows)] in
  let delete_attributes =
    let fields = ["point_pattern";"vertex_pattern";"primitive_pattern";"detail_pattern"] in
    let patterns = ["*";"N id";"* ^id";"P";"absent";" \t ";"é猫";"id\\ ";"\\ id"] in
    [change_case "delete_attributes" "authored delete defaults" [];
     change_case "delete_attributes" "authored delete inverse defaults" ["delete_non_selected", boolean true]]
    @ List.concat_map (fun field -> List.concat_map (fun pattern ->
        List.map (fun inverse -> change_case "delete_attributes" "authored delete pattern"
          [field, text pattern; "delete_non_selected", boolean inverse]) [false; true]) patterns) fields
  in
  let rename_attributes =
    let patterns = ["old_*","new_*";"old_*","old_*";"missing","new";
      "old_float","old_int";"old_float","\\ ";"old_float","P";
      "old_?nt","new_?nt";"old_[fi]*","new_[fi]*";"N","normal";
      "id","N";"weight","renamed_weight";"tag","é猫"] in
    [rules "rename_attributes" []]
    @ List.concat_map (fun owner -> List.concat_map (fun policy ->
        List.map (fun (pattern,replacement) -> rules "rename_attributes"
          [[owner;pattern;replacement;policy]]) patterns) ["skip";"error";"overwrite"])
        ["point";"vertex";"primitive";"detail";" ANY ";"*"]
    @ [rules "rename_attributes" [["any";"old_*";"mid_*";"skip"];["any";"mid_*";"new_*";"overwrite"]];
       rules "rename_attributes" [["point";"old_float";"old_int";"overwrite"];["point";"old_int";"old_vec2";"overwrite"]]]
  in
  let group_delete =
    let patterns = ["*";"absent";"alternating corners polygon selected_edges";"* ^alternating";" \t ";"[";"\\"] in
    let tables = [[]] @ List.concat_map (fun owner -> List.map (fun pattern -> [[owner;pattern]]) patterns)
        [" POINTS ";"vertices";"primitives";"edges";"any";"*"] @
      [[["points";"alternating"];["vertices";"corners"];["edges";"selected_edges"]];
       [["any";"*"];["any";"*"]];[["point";"["];["edge";"\\"]];[["edge";"\\"];["point";"["]]] in
    List.concat_map (fun rows -> List.map (fun unused ->
      change_case "group_delete" "authored group delete rules"
        ["rules", text (table rows); "delete_unused", boolean unused]) [false;true]) tables
  in
  let group_rename =
    let patterns = ["old","target";"old","old";"missing","new";"old","\\ ";
      "old*","new*";"[","new";"\\","new";"*","new";"alternating","renamed";
      "corners","renamed";"polygon","renamed";"selected_edges","renamed"] in
    [rules "group_rename" []]
    @ List.concat_map (fun owner -> List.concat_map (fun policy ->
        List.map (fun (pattern,replacement) -> rules "group_rename"
          [[owner;pattern;replacement;policy]]) patterns)
        ["skip";"error";"overwrite";"union"])
        ["point";"vertex";"primitive";"edge";" ANY ";"*"]
    @ [rules "group_rename" [["any";"old";"target";"union"];["any";"target";"renamed";"overwrite"]];
       rules "group_rename" [["point";"old";"target";"error"];["edge";"[";"new";"skip"]];
       rules "group_rename" [["edge";"[";"new";"skip"];["point";"old";"target";"error"]]]
    @ List.map (fun pattern -> rules "group_rename" [["point";pattern;"renamed";"skip"]])
        ["é猫";"a-b";"back\\\\slash";"a\\ b";"\\[x\\]";"\\*"]
  in
  let swap_attributes =
    let kinds = ["float";"int";"vec2";"vec3";"vec4";"text";"float_array";"int_array"] in
    let pairs = ["old_*","new_*";"old_*","old_*";"missing","old_float";
      "old_float","missing";"missing","missing2";"old_float","old_int";
      "old_float","old_float";"absent_*","old_*";"old_?nt","new_?nt";
      "old_[fi]*","new_[fi]*";"old_float","\\ "] @
      List.map (fun kind -> "old_"^kind,"new_"^kind) kinds in
    [rules "swap_attributes" []]
    @ List.concat_map (fun owner -> List.concat_map (fun method_ ->
        List.map (fun (source,destination) -> rules "swap_attributes"
          [[owner;source;destination;method_]]) pairs) ["swap";"move";"copy"])
        [" POINTS ";"vertices";"primitives";"detail"]
    @ List.concat_map (fun method_ -> List.map (fun (source,destination) ->
        rules "swap_attributes" [["point";source;destination;method_]])
        (["P","P";"P","new_P";"missing","P";"P","old_vec3";"old_vec3","P"] @
         List.concat_map (fun kind -> ["old_"^kind,"P";"P","old_"^kind]) kinds))
        ["swap";"move";"copy"]
    @ [rules "swap_attributes" [["point";"old_float";"old_int";"swap"];["point";"old_int";"old_vec2";"swap"]];
       rules "swap_attributes" [["point";"old_*";"mid_*";"copy"];["point";"mid_*";"new_*";"move"]];
       rules "swap_attributes" [["point";"old_vec3";"P";"move"];["point";"P";"new_P";"copy"]];
       rules "swap_attributes" [["vertex";"P";"new_P";"swap"]]]
    @ List.concat_map (fun method_ -> List.map (fun pattern ->
        rules "swap_attributes" [["point";pattern;"renamed";method_]])
        ["é猫";"a-b";"back\\\\slash";"a\\ b";"\\[x\\]";"\\*"])
        ["swap";"move";"copy"]
  in
  let delete_edges = List.map (fun name -> change_case "delete_edge_group" "authored edge delete"
      ["name", text name]) ["edges";"selected_edges";"absent";"";" selected_edges ";"*"] in
  let rename_edges = List.concat_map (fun source -> List.map (fun destination ->
      change_case "rename_edge_group" "authored edge rename"
        ["from", text source;"into",text destination])
      ["renamed";"selected_edges";"";" ";"é猫\""])
      ["edges";"selected_edges";"absent";" selected_edges ";"*"] in
  delete_attributes @ rename_attributes @ group_delete @ group_rename @ swap_attributes @ delete_edges @ rename_edges

  let metadata_cases =
  [rule "rename wildcard" "rename_attributes" [["any";"old_*";"new_*";"skip"]];
   rule "rename error conflict" "rename_attributes" [["any";"old_*";"new_*";"error"]];
   rule "rename overwrite conflict" "rename_attributes" [["any";"old_*";"new_*";"overwrite"]];
   rule "rename chained" "rename_attributes"
     [["any";"old_*";"mid_*";"skip"];["any";"mid_*";"new_*";"overwrite"]];
   rule "swap all methods" "swap_attributes"
     [["point";"old_float";"old_int";"swap"];["point";"old_int";"old_vec2";"copy"]];
   rule "swap move" "swap_attributes" [["vertices";"old_*";"new_*";"move"]];
   rule "swap detail" "swap_attributes" [["detail";"old_*";"new_*";"copy"]];
   rule "swap malformed pattern" "swap_attributes" [["point";"[";"x";"swap"]];
   rule "group rename union" "group_rename" [["any";"old_*";"new_*";"union"]];
   rule "group rename overwrite" "group_rename" [["any";"old_*";"new_*";"overwrite"]];
   rule "group rename skip" "group_rename" [["any";"old_*";"new_*";"skip"]];
   rule "group rename error conflict" "group_rename" [["any";"old_*";"new_*";"error"]];
   rule "group delete unused" "group_delete" [["point";"old_*"];["edge";"edges"]];
   change_case "group_delete" "group delete unused flag" ["delete_unused", boolean true];
   change_case "delete_attributes" "delete owner patterns"
     ["point_pattern", text "old_*";"detail_pattern", text "old_text"];
   change_case "delete_attributes" "delete inverse"
     ["delete_non_selected", boolean true;"vertex_pattern", text "old_int"];
   change_case "delete_edge_group" "delete edge name" ["name", text "old_edges"];
   change_case "rename_edge_group" "rename edge name" ["from", text "old_edges";"into", text "renamed_edges"];
   rule "malformed group pattern" "group_delete" [["point";"["]];
   change_case "group_delete" "delete empty group" ["delete_unused", boolean true];
   change_case "delete_attributes" "delete malformed pattern" ["point_pattern", text "["]]

let setters =
  let owners = ["Point";"Vertex";"Primitive";"Detail"] in
  List.concat_map (fun owner ->
    let common = ["owner", choice owner] in
    [change_case "set_float" ("owner " ^ owner)
       (common @ ["name", text "new float";"value", float (-2.5)]);
     change_case "set_int" ("owner " ^ owner)
       (common @ ["name", text "id";"value", integer max_int]);
     change_case "set_vector" ("owner " ^ owner)
       (common @ ["x", float 2.;"y", float (-3.);"z", float 0.5])]) owners
  @ [change_case "set_orient" "zero quaternion"
       ["x",float 0.;"y",float 0.;"z",float 0.;"w",float 0.];
     change_case "set_orient" "large" ["x",float 1e308];
     change_case "set_orient" "nonunit quaternion" ["x",float 1.;"y",float 2.;"z",float 3.;"w",float (-4.)];
     change_case "set_transform" "affine"
       ["m01",float (-2.);"m03",float 4.];
     change_case "set_float" "blank name refusal" ["name",text " \t\n"];
     change_case "set_int" "blank name refusal" ["name",text " \t\n"];
     change_case "set_vector" "blank name refusal" ["name",text " \t\n"]]

let cases () =
  let geometry = input_fixture () in
  let no_input = ["none", []] in
  let fixtures = test_fixtures () in
  let authored_metadata = metadata_fixture () in
  let raw_metadata =
    let positions = Packed.Float3.Private.view (Geometry.positions authored_metadata) in
    let x = Array.copy positions.x in
    x.(0) <- nan; x.(1) <- infinity; x.(2) <- neg_infinity;
    let geometry = Geometry.with_positions
        (Packed.Float3.of_owned ~x ~y:(Array.copy positions.y) ~z:(Array.copy positions.z)
         |> Rdk_test_support.get_string_ok) authored_metadata |> Rdk_test_support.get_string_ok in
    Geometry.with_attribute (Attribute.create_owned ~owner:Point ~name:"raw_float"
      (Attribute.Float [|nan;infinity;neg_infinity;-0.;0.;1.|]) |> Rdk_test_support.get_string_ok)
      geometry |> Rdk_test_support.get_string_ok in
  let color_base = with_color_groups authored_metadata in
  let metadata_inputs = ["points0", [Port_gate.points 0]; "points1", [Port_gate.points 1];
    "fixture", [authored_metadata]; "grid", [metadata_grid ()];
    "points32769", [Port_gate.points 32769]; "metadata", [geometry]; "raw", [raw_metadata]] in
  let setter_inputs = ["empty", [Port_gate.points 0]; "single", [Port_gate.points 1];
    "mixed", [List.assoc "mixed" fixtures |> List.hd];
    "large", [Port_gate.points 32769]] in
  let noise_inputs = ["points0", [with_noise_payload (noise_points 0)];
    "points1", [with_noise_payload (noise_points 1)];
    "points4225", [with_noise_payload (noise_points 4225)];
    "grid", [noise_grid ()];
    "mixed", [noise_mixed ()]] in
  let color_inputs = List.map (fun (label, geometries) -> label, List.map with_color geometries) fixtures
      @ ["large", [with_color (Port_gate.points 32769)]]
      @ ["metadata", [with_color geometry]]
      @ ["metadata_authored", [with_color authored_metadata];
         "metadata_raw", [with_color raw_metadata];
         "owner_groups", [with_color color_base]]
      @ raw_color_fixtures color_base
      @ List.map (fun storage -> "owner_groups_Cd_" ^ storage, [with_cd_storage color_base storage])
          ["float";"int";"text";"float2";"float3";"float4";"float_array";"int_array"]
      @ List.map (fun storage -> "Cd_" ^ storage, [with_cd_storage geometry storage])
          ["float";"int";"text";"float2";"float3";"float4";"float_array";"int_array"] in
  let factory = function
    | "line" -> Sop.Nodes.Line.factory
    | "noise_displace" -> Sop.Nodes.Noise_displace.factory
    | "set_color" -> Sop.Nodes.Set_color.factory
    | "set_float" -> Sop.Nodes.Set_float.factory
    | "set_int" -> Sop.Nodes.Set_int.factory
    | "set_vector" -> Sop.Nodes.Set_vector.factory
    | "set_orient" -> Sop.Nodes.Set_orient.factory
    | "set_transform" -> Sop.Nodes.Set_transform.factory
    | "delete_attributes" -> Sop.Nodes.Delete_attributes.factory
    | "delete_edge_group" -> Sop.Nodes.Delete_edge_group.factory
    | "group_delete" -> Sop.Nodes.Group_delete.factory
    | "group_rename" -> Sop.Nodes.Group_rename.factory
    | "rename_attributes" -> Sop.Nodes.Rename_attributes.factory
    | "rename_edge_group" -> Sop.Nodes.Rename_edge_group.factory
    | "swap_attributes" -> Sop.Nodes.Swap_attributes.factory
    | key -> List.find (fun factory -> Sop.Edit_graph.factory_key factory = key)
        Sop_catalog.Editor.factories in
  let node key inputs cases =
    { Port_gate.key; native = factory key; inputs; cases; ulps = 4; bench = None } in
  let cases_for key rows =
    List.iter (fun (case_key, _, _) ->
      if not (List.mem case_key node_keys) then
        failwith ("supplemental golden has unexpected case key: " ^ case_key)) rows;
    let selected = List.filter_map (fun (case_key, label, values) ->
      if key = case_key then Some (label, values) else None) rows in
    let seen = Hashtbl.create (List.length selected) in
    List.filter (fun case ->
      if Hashtbl.mem seen case then false else (Hashtbl.add seen case (); true)) selected in
  let line_sizes = List.concat_map (fun mode -> List.map (fun count ->
    change_case "line" (mode ^ " count " ^ string_of_int count)
      ["kind", choice mode; "points", integer count])
    (if mode = "Points" then [1;2;6;4225;32769] else [2;6;4225;32769])) ["Polygon curve";"Points"] in
  let over_capacity = max Sys.max_floatarray_length Sys.max_array_length + 1 in
  [node "line" no_input (cases_for "line" [change_case "line" "nondefault" ["points",integer 11;"length",float 3.75;
       "origin_x",float (-2.5);"origin_y",float 1.25;"origin_z",float (-0.75);
       "direction_x",float 0.25;"direction_y",float (-0.5);"direction_z",float 0.75];
       change_case "line" "points mode" ["kind",choice "Points";"points",integer 6];
       change_case "line" "zero direction refusal" ["direction_x",float 0.;"direction_y",float 0.;"direction_z",float 0.];
       change_case "line" "negative length clamps" ["length",float (-2.)];
       change_case "line" "negative points clamps" ["kind",choice "Points";"points",integer (-10)];
       change_case "line" "subnormal direction" ["points",integer 17;"direction_x",float (Float.next_after 0. 1.);"direction_z",float (-0.)];
       change_case "line" "huge direction" ["points",integer 100001;"length",float (sqrt 2.);
         "origin_x",float 1.;"origin_y",float 2.;"origin_z",float 3.;
         "direction_x",float Float.max_float;"direction_y",float Float.max_float];
       change_case "line" "mixed exponents" ["points",integer 32769;"direction_x",float Float.min_float;
         "direction_y",float 1.;"direction_z",float Float.max_float];
       change_case "line" "signed zero origin" ["points",integer 17;"origin_x",float (-0.);
         "origin_y",float (-0.);"origin_z",float (-0.);"direction_x",float (-1.);
         "direction_y",float (-0.);"direction_z",float 0.];
       change_case "line" "zero points endpoint error" ["kind",choice "Points";"points",integer 1;
         "direction_x",float 0.;"direction_y",float 0.;"direction_z",float 0.];
       change_case "line" "finite endpoint cancellation" ["points",integer 17;"length",float Float.max_float;
         "origin_x",float Float.max_float;"direction_x",float (-1.)];
       change_case "line" "max endpoint" ["points",integer 17;"origin_x",float Float.max_float;
         "direction_y",float 1.];
       change_case "line" "tiny length" ["points",integer 17;"length",float (Float.next_after 0. 1.);
         "direction_x",float 1.;"direction_y",float 1.;"direction_z",float 1.];
       change_case "line" "zero length" ["length",float 0.];
       change_case "line" "negative zero length" ["length",float (-0.)];
       change_case "line" "over capacity" ["points",integer over_capacity];
       change_case "line" "max int points" ["points",integer max_int]]
       @ List.map (fun (_, label, values) -> label, values) line_sizes);
   node "noise_displace"
     (noise_inputs @ ["points32769", [with_noise_payload (noise_points 32769)]])
     (cases_for "noise_displace"
       ([change_case "noise_displace" "defaults" [];
         change_case "noise_displace" "normal_3d" ["mode",choice "normal_3d"];
         change_case "noise_displace" "seed" ["seed",integer 17];
         change_case "noise_displace" "amplitude" ["amplitude",float (-0.7)];
         change_case "noise_displace" "frequency" ["frequency",float 0.16];
         change_case "noise_displace" "context seed" ["context_seed",boolean true];
         change_case "noise_displace" "normal deterministic"
           ["mode",choice "normal_3d";"seed",integer 311;"amplitude",float 0.8;"frequency",float 0.16];
         change_case "noise_displace" "context deterministic"
           ["mode",choice "normal_3d";"context_seed",boolean true;"amplitude",float 0.5;"frequency",float 0.25];
         change_case "noise_displace" "zero" ["amplitude",float 0.;"frequency",float 0.]]
        @ List.map (fun mode -> change_case "noise_displace" ("large " ^ mode)
            ["mode",choice mode;"seed",integer 311;"amplitude",float 0.8;"frequency",float 0.16])
            ["height_2d";"normal_3d"]));
   node "set_color" color_inputs (cases_for "set_color"
      ([change_case "set_color" "point selected group"
        ["owner",choice "Point";"group",text "selected";"color_r",float 0.2;"color_g",float 0.4;
         "color_b",float 0.6;"alpha",float 0.8];
      change_case "set_color" "vertex all"
        ["owner",choice "Vertex";"color_r",float 0.1;"color_g",float 0.3;"color_b",float 0.9;"alpha",float 0.5];
      change_case "set_color" "trimmed ordered group"
        ["owner",choice "Primitive";"group",text " dest_a "];
      change_case "set_color" "unicode group"
        ["owner",choice "Point";"group",text "é猫\""];
      change_case "set_color" "missing group error"
        ["owner",choice "Point";"group",text "missing"];
      change_case "set_color" "detail group refusal"
        ["owner",choice "Detail";"group",text "selected"]]
       @ List.concat_map (fun owner -> List.map (fun group ->
         change_case "set_color" "authored color owner/group"
           ["owner",choice owner;"group",text group;"color_r",float 0.2;"color_g",float 0.4;
            "color_b",float 0.6;"alpha",float 0.8])
         (if owner="Detail" then ["";" \t\r\n "]
          else ["";" \t\r\n ";"selected";"ordered";"full";"empty";"missingé";" selected "]))
         ["Point";"Vertex";"Primitive";"Detail"]));
   node "set_float" setter_inputs [];
   node "set_int" setter_inputs [];
   node "set_vector" setter_inputs [];
   node "set_orient" setter_inputs [];
   node "set_transform" setter_inputs [];
   node "delete_attributes" (metadata_inputs @ ["reference", [geometry; authored_metadata];
       "reference_reverse", [authored_metadata; geometry]])
     (cases_for "delete_attributes" (metadata_cases @ authored_metadata_cases ()));
   node "delete_edge_group" metadata_inputs (cases_for "delete_edge_group" metadata_cases);
   node "group_delete" metadata_inputs (cases_for "group_delete" (metadata_cases @ authored_metadata_cases ()));
   node "group_rename" metadata_inputs (cases_for "group_rename" (metadata_cases @ authored_metadata_cases ()));
   node "rename_attributes" metadata_inputs (cases_for "rename_attributes" (metadata_cases @ authored_metadata_cases ()));
   node "rename_edge_group" metadata_inputs (cases_for "rename_edge_group" (metadata_cases @ authored_metadata_cases ()));
   node "swap_attributes" metadata_inputs (cases_for "swap_attributes" (metadata_cases @ authored_metadata_cases ()))]
  |> fun nodes ->
    let nodes = List.map (fun node -> if List.mem node.Port_gate.key ["set_float";"set_int";"set_vector";"set_orient";"set_transform"]
      then {node with Port_gate.cases = cases_for node.key setters} else node) nodes in
    List.iter (fun key ->
      if not (List.exists (fun node -> node.Port_gate.key = key && node.Port_gate.cases <> []) nodes) then
        failwith ("supplemental golden has no cases for " ^ key)) node_keys;
    nodes

let capture ~dir ~out_dir ~on_coverage ~on_rows =
  let cases = cases () in
  let cache = Port_gate.create_fixture_cache () in
  let coverage = Port_gate.coverage_rows cases in
  let seen = Hashtbl.create (List.length coverage) in
  let duplicates = List.filter (fun row ->
    if Hashtbl.mem seen row then true else (Hashtbl.add seen row (); false)) coverage in
  if duplicates <> [] then
    failwith ("supplemental coverage has duplicate identities: " ^ string_of_int (List.length coverage)
      ^ " rows but " ^ string_of_int (Hashtbl.length seen) ^ " unique; examples: "
      ^ String.concat " | " (List.take 8 duplicates));
  on_coverage coverage;
  let total = ref 0 in
  List.iter (fun node ->
    let started = Unix.gettimeofday () in
    let rows = Port_gate.capture_goldens ~dir ~out_dir ~cache node in
    let coverage_count = List.length (Port_gate.coverage_rows [node]) in
    if List.length (List.sort_uniq String.compare rows) <> coverage_count then
      failwith (Printf.sprintf "supplemental %s identity mismatch: %d rows vs %d coverage rows"
        node.Port_gate.key (List.length rows) coverage_count);
    on_rows node rows (Unix.gettimeofday () -. started);
    total := !total + List.length rows) cases;
  !total
