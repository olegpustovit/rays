let fail message = raise (Error.Fail message)
let get_ok = function Ok value -> value | Error message -> fail message

let remap_attribute ?cancel ?(grain = 16_384) point_map vertex_map
    primitive_map attribute =
  let mapping = match Attribute.owner attribute with
    | Attribute.Point -> Some point_map
    | Attribute.Vertex -> Some vertex_map
    | Attribute.Primitive -> Some primitive_map
    | Attribute.Detail -> None in
  match mapping with
  | None -> attribute
  | Some mapping -> Topology_remap.attribute ?cancel ~grain mapping attribute

let remap_group ?cancel ?(grain = 16_384) point_map vertex_map
    primitive_map group =
  let mapping = match Group.owner group with
    | Group.Point -> point_map
    | Group.Vertex -> vertex_map
    | Group.Primitive -> primitive_map in
  Topology_remap.group ?cancel ~grain mapping group

let apply_primitives ?cancel ?(grain = 16_384) permutation geometry =
  let source_topology = Geometry.topology geometry in
  let source = Topology.View.view source_topology in
  let primitive_count = Array.length permutation in
  let offsets = Array.make (primitive_count + 1) 0 in
  for primitive = 0 to primitive_count - 1 do
    let source_primitive = permutation.(primitive) in
    offsets.(primitive + 1) <- offsets.(primitive)
      + source.primitive_offsets.(source_primitive + 1)
      - source.primitive_offsets.(source_primitive)
  done;
  let vertex_count = offsets.(primitive_count) in
  let vertex_points = Array.make vertex_count 0
  and vertex_map = Array.make vertex_count 0
  and kinds = Bytes.make primitive_count '\000' in
  if primitive_count > 0 then Parallel.for_ ~chunk_size:(max 1 (grain / 8))
      ~start:0 ~finish:(primitive_count - 1) (fun primitive ->
        if primitive land 4095 = 0 then Cancel.check_opt cancel;
        let source_primitive = permutation.(primitive)
        and output_first = offsets.(primitive) in
        let source_first = source.primitive_offsets.(source_primitive)
        and source_last = source.primitive_offsets.(source_primitive + 1) in
        Bytes.set kinds primitive (Bytes.get source.primitive_kinds source_primitive);
        for source_vertex = source_first to source_last - 1 do
          let output_vertex = output_first + source_vertex - source_first in
          vertex_points.(output_vertex) <- source.vertex_points.(source_vertex);
          vertex_map.(output_vertex) <- source_vertex
        done);
  let topology = Topology.View.create_validated_owned
      ~point_count:(Geometry.point_count geometry) ~vertex_points
      ~primitive_offsets:offsets ~primitive_kinds:kinds in
  let attributes = List.map (fun attribute ->
      match Attribute.owner attribute with
      | Attribute.Vertex | Attribute.Primitive ->
          remap_attribute ?cancel ~grain [||] vertex_map permutation attribute
      | Attribute.Point | Attribute.Detail -> attribute)
      (Geometry.attributes geometry)
  and groups = List.map (fun group -> match Group.owner group with
      | Group.Vertex | Group.Primitive ->
          remap_group ?cancel ~grain [||] vertex_map permutation group
      | Group.Point -> group) (Geometry.groups geometry) in
  let edge_groups = Topology_remap.edge_groups ?cancel ~source_topology
      ~target_topology:topology
      ~point_map:(Array.init (Geometry.point_count geometry) Fun.id)
      (Geometry.edge_groups geometry) |> get_ok in
  Geometry.create ~positions:(Geometry.positions geometry) ~topology
    ~attributes ~groups ~edge_groups () |> get_ok

(* Stable primitive reorder by an integer primitive attribute. *)
let sort_primitives ?cancel ?(grain = 16_384) ~name geometry =
  Rdk_core.Error.guard ~operation:"sort" ~code:"invalid_sort" @@ fun () ->
  try
    Cancel.check_opt cancel;
    let keys = match Geometry.find_attribute ~owner:Attribute.Primitive name geometry with
      | Some attribute ->
          (match Attribute.View.storage attribute with
           | Attribute.Int values -> values
           | _ -> fail ("sort attribute " ^ name ^ " must be integer"))
      | None -> fail ("missing sort attribute " ^ name) in
    let order = Array.init (Array.length keys) Fun.id in
    Array.stable_sort (fun a b -> Int.compare keys.(a) keys.(b)) order;
    Ok (apply_primitives ?cancel ~grain order geometry)
  with Error.Fail message | Invalid_argument message -> Error message
