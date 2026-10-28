

type planes = {
  x : float array;
  y : float array;
  z : float array;
}

let validate_selection topology = function
  | None -> Ok ()
  | Some (Element_selection.Selected_points group) ->
      if Group.owner group <> Group.Point then Error
          "Rdk.Deform: point selection must own points"
      else if Group.length group <> Topology.point_count topology then Error
          "Rdk.Deform: point selection length does not match point count"
      else Ok ()
  | Some (Element_selection.Selected_vertices group) ->
      if Group.owner group <> Group.Vertex then Error
          "Rdk.Deform: vertex selection must own vertices"
      else if Group.length group <> Topology.vertex_count topology then Error
          "Rdk.Deform: vertex selection length does not match vertex count"
      else Ok ()
  | Some (Element_selection.Selected_primitives group) ->
      if Group.owner group <> Group.Primitive then Error
          "Rdk.Deform: primitive selection must own primitives"
      else if Group.length group <> Topology.primitive_count topology then Error
          "Rdk.Deform: primitive selection length does not match primitive count"
      else Ok ()
  | Some (Element_selection.Selected_edges group) ->
      if Edge_group.topology_data_id group <> Topology.data_id topology then Error
          "Rdk.Deform: edge selection belongs to different topology"
      else Ok ()

let[@inline] point_selected selection index point = match selection with
  | None -> true
  | Some (Element_selection.Selected_points group) -> Group.mem point group
  | Some (Element_selection.Selected_vertices group) ->
      let index = Option.get index in
      let count = Topology_index.point_incidence_count index point in
      let local = ref 0 and found = ref false in
      while !local < count && not !found do
        found := Group.mem (Topology_index.point_vertex index ~point ~local:!local)
            group;
        incr local
      done;
      !found
  | Some (Element_selection.Selected_primitives group) ->
      let index = Option.get index in
      let count = Topology_index.point_incidence_count index point in
      let local = ref 0 and found = ref false in
      while !local < count && not !found do
        let vertex = Topology_index.point_vertex index ~point ~local:!local in
        found := Group.mem (Topology_index.primitive_of_vertex index vertex) group;
        incr local
      done;
      !found
  | Some (Element_selection.Selected_edges group) ->
      let index = Option.get index in
      let count = Topology_index.point_edge_count index point in
      let local = ref 0 and found = ref false in
      while !local < count && not !found do
        found := Edge_group.mem (Topology_index.point_edge index ~point
            ~local:!local) group;
        incr local
      done;
      !found

let point_vector_attribute operation name geometry =
  if String.equal name "P" then
    let values = Packed.Float3.View.view (Geometry.positions geometry) in
    Ok { x = values.x; y = values.y; z = values.z }
  else match Geometry.find_attribute ~owner:Attribute.Point name geometry with
    | None -> Error (Printf.sprintf "%s: missing point float3 attribute %s"
        operation name)
    | Some attribute ->
        (match Attribute.View.storage attribute with
         | Attribute.Float3 values ->
             let values = Packed.Float3.View.view values in
             Ok { x = values.x; y = values.y; z = values.z }
         | _ -> Error (Printf.sprintf
             "%s: point attribute %s must have float3 storage" operation name))

let vertex_normals geometry =
  match Geometry.find_attribute ~owner:Attribute.Vertex "N" geometry with
  | None -> Ok None
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Float3 values ->
           let values = Packed.Float3.View.view values in
           Ok (Some { x = values.x; y = values.y; z = values.z })
       | _ -> Error "Rdk.Deform: vertex N must have float3 storage")

let face_vectors ?cancel ~grain geometry =
  Result.map (fun ((x, y, z), _) -> { x; y; z })
    (Face_normals.polygon_area_vector ?cancel ~grain ~need_inverse:false
      ~first_failure:true ~operation:"Rdk.Deform" geometry)

let point_vectors_from_vertices ?cancel ~grain topology source =
  let topology_view = Topology.View.view topology in
  let point_count = topology_view.point_count in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. and incidence = Array.make point_count 0 in
  for vertex = 0 to Array.length topology_view.vertex_points - 1 do
    Cancel.poll cancel vertex 16_383;
    let point = topology_view.vertex_points.(vertex) in
    x.(point) <- x.(point) +. source.x.(vertex);
    y.(point) <- y.(point) +. source.y.(vertex);
    z.(point) <- z.(point) +. source.z.(vertex);
    incidence.(point) <- incidence.(point) + 1
  done;
  Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(point_count - 1)
    (fun point ->
      Cancel.poll cancel point 4095;
      let count = incidence.(point) in
      if count > 1 then begin
        let inverse = 1. /. float_of_int count in
        x.(point) <- x.(point) *. inverse;
        y.(point) <- y.(point) *. inverse;
        z.(point) <- z.(point) *. inverse
      end);
  { x; y; z }

let geometric_point_vectors ?cancel ~grain geometry =
  Result.map (fun faces ->
    let topology = Geometry.topology geometry in
    let topology_view = Topology.View.view topology in
    let point_count = topology_view.point_count in
    let x = Array.make point_count 0. and y = Array.make point_count 0.
    and z = Array.make point_count 0. in
    let primitive = ref 0 in
    for vertex = 0 to Array.length topology_view.vertex_points - 1 do
      Cancel.poll cancel vertex 16_383;
      while vertex >= topology_view.primitive_offsets.(!primitive + 1) do
        incr primitive
      done;
      let point = topology_view.vertex_points.(vertex) in
      x.(point) <- x.(point) +. faces.x.(!primitive);
      y.(point) <- y.(point) +. faces.y.(!primitive);
      z.(point) <- z.(point) +. faces.z.(!primitive)
    done;
    { x; y; z }) (face_vectors ?cancel ~grain geometry)

let resolve_directions ?cancel ~grain geometry =
  (match Geometry.find_attribute ~owner:Attribute.Point "N" geometry with
       | Some attribute ->
           (match Attribute.View.storage attribute with
            | Attribute.Float3 values ->
                let values = Packed.Float3.View.view values in
                Ok { x = values.x; y = values.y; z = values.z }
            | _ -> Error "Rdk.Deform: point N must have float3 storage")
       | None ->
           Result.bind (vertex_normals geometry) (function
             | Some values -> Ok (point_vectors_from_vertices ?cancel
                 ~grain (Geometry.topology geometry) values)
             | None -> geometric_point_vectors ?cancel ~grain geometry))

let with_point_normals ?cancel ~grain ?primitives geometry =
  let selection = Option.map (fun group -> Element_selection.Selected_primitives group) primitives in
  Ok (Error.get (Normal_ops.run ?cancel ~grain ?selection ?primitives
    ~owner:Attribute.Point ~weighting:Normal_ops.Face_area geometry))

let normals ?cancel ~grain ?primitives geometry =
  if grain <= 0 then Error "Rdk_mesh.Deform.normals: grain must be positive"
  else with_point_normals ?cancel ~grain ?primitives geometry
