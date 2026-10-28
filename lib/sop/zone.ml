type kind = Points | Pieces

type element = {
  index : int; key : int; position : float * float * float;
  piece : Rdk.Geometry.t option; digest : string;
}

let max_elements = 4096
let error code message = Error (Diagnostic.error ~code message)
let too_many what count = error "E_ITER_BOUND"
    (Printf.sprintf "A loop over %s runs %d iterations; the limit is %d." what count max_elements)

let keys owner count name geometry =
  let by_index = Array.init count Fun.id in
  match name with
  | None -> by_index
  | Some name ->
      match Option.map Rdk.Attribute.storage
          (Rdk.Geometry.find_attribute ~owner name geometry) with
      | Some (Rdk.Attribute.Int values) when Array.length values = count -> values
      | Some (Rdk.Attribute.Float values) when Array.length values = count ->
          Array.map (fun v -> int_of_float (Float.floor v)) values
      | _ -> by_index

let order keys =
  let order = Array.init (Array.length keys) Fun.id in
  Array.stable_sort (fun a b -> compare keys.(a) keys.(b)) order;
  order

let digest geometry =
  let view = Rdk.Topology.View.view (Rdk.Geometry.topology geometry) in
  let positions = Rdk.Packed.Float3.View.view (Rdk.Geometry.positions geometry) in
  let attributes = List.map (fun attribute ->
    Rdk.Attribute.name attribute, Rdk.Attribute.owner attribute,
    Rdk.Attribute.kind_name attribute, Rdk.Attribute.length attribute)
    (Rdk.Geometry.attributes geometry) in
  Digest.string (Marshal.to_string
    (view.vertex_points, view.primitive_offsets, view.primitive_kinds,
     positions.x, positions.y, positions.z, attributes) [])

let elements kind ?key geometry =
  match kind with
  | Points ->
      let count = Rdk.Geometry.point_count geometry in
      if count > max_elements then too_many "points" count else
      let positions = Rdk.Geometry.positions geometry in
      let keys = keys Rdk.Attribute.Point count key geometry in
      Ok (Array.mapi (fun index source ->
        { index; key = keys.(source); position = Rdk.Packed.Float3.get positions source;
          piece = None; digest = "" }) (order keys))
  | Pieces ->
      let count = Rdk.Geometry.primitive_count geometry in
      let keys = keys Rdk.Attribute.Primitive count key geometry in
      let distinct = List.sort_uniq compare (Array.to_list keys) in
      let pieces = List.length distinct in
      if pieces > max_elements then too_many "pieces" pieces else
      let rank = Hashtbl.create (max 1 pieces) in
      List.iteri (fun i k -> Hashtbl.replace rank k i) distinct;
      let assignment = Array.map (Hashtbl.find rank) keys in
      let parts = Rdk.Deletion.primitive_partitions ~piece_count:pieces
          ~primitive_pieces:assignment geometry in
      let keys_by_piece = Array.of_list distinct in
      Ok (Array.mapi (fun index part ->
        let position = if Rdk.Geometry.point_count part = 0 then (0., 0., 0.)
          else Rdk.Packed.Float3.get (Rdk.Geometry.positions part) 0 in
        { index; key = keys_by_piece.(index); position; piece = Some part;
          digest = digest part }) parts)

let element_node element =
  let geometry = Option.get element.piece in
  Node.Build.make_geometry ~operation:"zone_element" ~version:1
    ~parameters:("content=" ^ Digest.to_hex element.digest)
    ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static
    ~inputs:[||] (fun ~node_id:_ _ _ ->
      Ok Node.Build.{ geometry; instances = None })

let node ?(report : element array -> unit = ignore) ?(live = false) ?(stamp = "") ~kind ?key ?select ~source_attribute ~source_base ~body
    ~inputs () =
  let parameters = Printf.sprintf "kind=%s;key=%S;source=%S;base=%d;inputs=%d;live=%b"
      (match kind with Points -> "points" | Pieces -> "pieces")
      (Option.value key ~default:"") source_attribute source_base (Array.length inputs) live in
  let parameters = parameters ^ Option.fold ~none:"" ~some:(fun i -> ";select=" ^ string_of_int i) select in
  let parameters = if stamp = "" then parameters else parameters ^ ";frame-state=" ^ stamp in
  Node.Build.make_geometry ~operation:"zone" ~version:1 ~parameters
    ~cook_mode:Node.Generic
    ~dependencies:(if live then Context.Dependencies.one Context.Dependencies.Input
                   else Context.Dependencies.static) ~inputs
    ~expand:(fun context nodes geometries ->
      match elements kind ?key geometries.(0) with
      | Error _ as error -> error
      | Ok elements ->
          report elements;
          let elements = match select with
            | None -> elements
            | Some index when index >= 0 && index < Array.length elements -> [|elements.(index)|]
            | Some _ -> [||] in
          (* the body is built once per cook: what does not read the element is shared *)
          let element = body ~inputs:nodes ~context in
          Ok (Array.map element elements))
    (fun ~node_id:_ context parts ->
      match Rdk.Mesh_merge.run ~cancel:(Context.cancel_token context)
          ~grain:(Context.grain context) ~source_attribute
          ~source_base:(source_base + Option.value ~default:0 select)
          (Array.to_list parts) with
      | Ok geometry -> Ok Node.Build.{ geometry; instances = None }
      | Error e -> Error (Diagnostic.error ~code:(Rdk.Error.code e)
          ~cause:(Rdk.Error.to_string e) "zone could not merge its elements"))
