type boundary =
  | Smooth_free
  | Smooth_unshared

let fail code message =
  Error (Error.make ~operation:"smooth" ~code message)

let validate_point_group label geometry = function
  | None -> Ok ()
  | Some group when Group.owner group <> Group.Point ->
      fail "invalid_selection" (label ^ " must own points")
  | Some group when Group.length group <> Geometry.point_count geometry ->
      fail "invalid_selection" (label ^ " length does not match point count")
  | Some _ -> Ok ()

let point_on_unshared_edge (index : Topology_index.View.view) point =
  let first = index.point_edge_offsets.(point)
  and last = index.point_edge_offsets.(point + 1) in
  let boundary = ref false and incidence = ref first in
  while !incidence < last && not !boundary do
    let edge = index.point_edges.(!incidence) in
    boundary := index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) = 1;
    incr incidence
  done;
  !boundary

let make_update_selection ?cancel ~grain ~boundary ~index
    ~constrained_points geometry =
  let point_count = Geometry.point_count geometry in
  let unrestricted = constrained_points = None && boundary = Smooth_free in
  if unrestricted then None
  else
    let index = Topology_index.View.view index in
    Some (Group.init ~grain ~owner:Group.Point ~name:"__smooth_update"
    point_count (fun point ->
      Cancel.poll cancel point 4095;
        let constrained = match constrained_points with
          | None -> false
          | Some group -> Group.mem point group in
        if constrained then false
        else match boundary with
          | Smooth_free -> true
          | Smooth_unshared -> not (point_on_unshared_edge index point)))

let remap_error error =
  Error.make ~hints:(Error.hints error) ~operation:"smooth"
    ~code:(Error.code error) (Error.message error)

let run ?cancel ?(grain = 16_384) ?constrained_points
    ?(boundary = Smooth_free) ?(iterations = 1)
    ?(mode = Attribute_blur.Laplacian 0.5)
    ?(recompute_normals = true)
    ~attributes geometry =
  try
    if grain <= 0 then fail "invalid_parameter" "grain must be positive"
    else
      Result.bind (validate_point_group "constrained point group" geometry
          constrained_points) (fun () ->
      match Attribute_pattern.compile attributes with
      | Error message -> fail "invalid_blur" message
      | Ok attribute_pattern ->
      let topology = Geometry.topology geometry in
      let index = Topology_index.create ?cancel topology in
      let selection = make_update_selection ?cancel ~grain ~boundary ~index
          ~constrained_points geometry in
      let source_position_id = Packed.Float3.data_id (Geometry.positions geometry) in
      let had_point_normals = Geometry.find_attribute ~owner:Attribute.Point "N"
          geometry <> None
      and had_vertex_normals = Geometry.find_attribute ~owner:Attribute.Vertex "N"
          geometry <> None in
      let point_normals_smoothed = had_point_normals
          && Attribute_pattern.matches attribute_pattern "N" in
      match Attribute_ops.blur_points { cancel; grain = Some grain; selection;
          iterations; mode; pattern = attributes } geometry with
      | Error error -> Error (remap_error error)
      | Ok output ->
          let positions_changed = Packed.Float3.data_id (Geometry.positions output)
              <> source_position_id in
          if recompute_normals && (had_point_normals || had_vertex_normals)
              && positions_changed && not point_normals_smoothed then
            (match Deform.normals ?cancel ~grain output with
             | Ok output -> Ok output
             | Error message -> fail "invalid_normals" message)
          else Ok output)
  with
  | Cancel.Cancelled -> fail "cancelled" "smoothing was cancelled"
  | Invalid_argument message -> fail "invalid_parameter" message
