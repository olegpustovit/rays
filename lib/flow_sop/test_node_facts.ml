open Sop
open Rdk
open Rdk_test_support

let context domains = Context.create ~domains ~grain:37 () |> get
let session () = Session.create ~max_entries:64 ~max_payload_bytes:16_000_000 |> get
let cook session domains node = match Session.cook session ~context:(context domains) node with
  | Ok output -> output
  | Error error -> fail (Diagnostic.error_to_string error)

let fixture () =
  let geometry = Plane_generators.grid ~columns:8 ~rows:6 ~size:2. () |> get_rdk in
  let geometry = Normal_ops.run ~owner:Attribute.Point geometry |> get_rdk in
  let point_normal = Geometry.find_attribute ~owner:Attribute.Point "N" geometry |> Option.get in
  let geometry = Normal_ops.run ~owner:Attribute.Vertex geometry |> get_rdk in
  Geometry.with_attribute point_normal geometry |> get

let test_declarations () =
  let native = Sop_catalog.Editor.factories in
  let lowered = Result.bind
    (Flow.Syntax.parse "(workspace facts (graph unused :context value 0.0))")
    (Flow_sop.Lower.workspace ~factories:native)
    |> Result.map_error Diagnostic.to_string |> get in
  let factories = native @ lowered.factories in
  let geometry = fixture () in
  let topology = Geometry.topology geometry in
  let edges = Edge_group.init ~topology ~index:(Topology_index.create topology)
    ~name:"edges" (fun _ -> true) in
  let geometry = Geometry.with_edge_group edges geometry |> get in
  let count = Geometry.point_count geometry in
  let uv = Packed.Float2.of_owned ~x:(Array.make count 0.) ~y:(Array.make count 0.) |> get in
  let uv = Attribute.create_owned ~name:"uv" ~owner:Attribute.Point (Float2 uv) |> get in
  let source = Lisp_sop.snapshot (Geometry.with_attribute uv geometry |> get) in
  let image = Edit_graph.instantiate Image_nodes.noise_factory [] |> Result.get_ok in
  let image, _ = Node.apply_parameters image
      ["width", Param.Int_value 1; "height", Param.Int_value 1] |> Result.get_ok in
  let evaluator = session () in
  let preserved = ref [] in
  let verify key node =
    let declaration = Node.facts node in
    if declaration.topology = Node.Preserved && declaration.cook_mode = Node.Duplicate_input 0 then begin
      let input = Result.get_ok (Sop.Payload.geometry (cook evaluator 1 source).payload) in
      let one = Result.get_ok (Sop.Payload.geometry (cook evaluator 1 node).payload) in
      let cold = session () in
      let eight = Result.get_ok (Sop.Payload.geometry (cook cold 8 node).payload) in
      Session.close cold;
      check (Geometry.topology one == Geometry.topology input)
        (Node.operation node ^ " replaced Preserved topology");
      check (geometry_bytes one = geometry_bytes eight)
        (Node.operation node ^ " differs at eight domains");
      preserved := key :: !preserved
    end in
  List.iter (fun factory ->
    verify (Edit_graph.factory_key factory) (Edit_graph.instantiate_optional factory
      (List.map (function "image" -> Some image | _ -> Some source)
        (Edit_graph.factory_input_types factory)) |> get)) factories;
  List.iter (fun key ->
    verify key (Lisp_sop.node ~with_:["source", source]
      (Printf.sprintf "(sop/%s (sop/ext_source))" key)))
    ["transform"; "noise_displace"; "peak"; "bend"; "mountain"; "normals";
     "attribute_noise"; "attribute_noise_quaternion"; "color_by_height"];
  List.iter (fun name -> check (List.mem name !preserved) (name ^ " lacks a Preserved declaration"))
    ["transform"; "soft_transform"; "noise_displace"; "peak"; "bend"; "mountain";
     "normals"; "attribute_noise"; "attribute_noise_quaternion"; "color_by_height"];
  let peak = Lisp_sop.node ~with_:["source", source] {|(sop/peak (sop/ext_source))|} in
  let noise = Lisp_sop.node ~with_:["source", source] {|(sop/noise_displace (sop/ext_source))|} in
  check ((Node.facts noise).reads = ["P"]) "Height 2D noise reads unexpected planes";
  let noise, _ = Node.apply_parameters noise ["mode", Param.Choice_value "normal_3d"] |> get in
  check ((Node.facts noise).reads = ["P"; "N"]) "Normal 3D noise facts omit N";
  let one = Result.get_ok (Sop.Payload.geometry (cook evaluator 1 noise).payload) in
  let cold = session () in
  let eight = Result.get_ok (Sop.Payload.geometry (cook cold 8 noise).payload) in
  check (geometry_bytes one = geometry_bytes eight) "Normal 3D SOP differs across domains";
  Session.close cold;
  let updated, _ = Node.apply_parameters peak ["direction_attribute", Param.Text_value "direction"] |> get in
  check (List.mem "direction" (Node.facts updated).reads
      && not (List.mem "N" (Node.facts updated).reads)) "PPX rebuild retained stale facts";
  Session.close evaluator;
  Printf.printf "facts: %d registered Preserved nodes checked at one/eight domains\n"
    (List.length !preserved)

let () = test_declarations ()
