type t = {
  primitive_of_vertex : int array;
  point_offsets : int array;
  point_vertices : int array;
}

let create_uncached ?cancel topology =
  let source = Topology.View.view topology in
  let point_count = source.point_count
  and vertex_count = Array.length source.vertex_points
  and primitive_count = Bytes.length source.primitive_kinds in
  let primitive_of_vertex = Array.make vertex_count (-1)
  and point_offsets = Array.make (point_count + 1) 0 in
  for primitive = 0 to primitive_count - 1 do
    if primitive land 4095 = 0 then Cancel.check_opt cancel;
    for vertex = source.primitive_offsets.(primitive)
        to source.primitive_offsets.(primitive + 1) - 1 do
      if vertex land 16_383 = 0 then Cancel.check_opt cancel;
      primitive_of_vertex.(vertex) <- primitive;
      let point = source.vertex_points.(vertex) in
      point_offsets.(point + 1) <- point_offsets.(point + 1) + 1
    done
  done;
  for point = 0 to point_count - 1 do
    point_offsets.(point + 1) <- point_offsets.(point + 1) + point_offsets.(point)
  done;
  let point_vertices = Array.make vertex_count 0
  and point_cursor = Array.copy point_offsets in
  for vertex = 0 to vertex_count - 1 do
    if vertex land 16_383 = 0 then Cancel.check_opt cancel;
    let point = source.vertex_points.(vertex) in
    let output = point_cursor.(point) in
    point_vertices.(output) <- vertex;
    point_cursor.(point) <- output + 1
  done;
  { primitive_of_vertex; point_offsets; point_vertices }

(* Bounded: at most [cache_capacity] live indices, released with their topology. *)
let cache_capacity = 64
let cache = Support.Identity_cache.create ~id:Topology.data_id cache_capacity

let create ?cancel topology =
  Cancel.check_opt cancel;
  Support.Identity_cache.find_or_add cache topology (fun () ->
    create_uncached ?cancel topology)

type index = t

module View = struct
  type view = {
    primitive_of_vertex : int array;
    point_offsets : int array;
    point_vertices : int array;
  }

  let view (value : index) = {
    primitive_of_vertex = value.primitive_of_vertex;
    point_offsets = value.point_offsets;
    point_vertices = value.point_vertices;
  }
end
