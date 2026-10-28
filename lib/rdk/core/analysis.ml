type connectivity_owner = Connectivity_points | Connectivity_primitives

(* Signed volume of a closed polygon shell: the divergence-theorem fan sum,
   exact for non-convex faces. *)
let signed_volume geometry =
  let t = Topology.View.view (Geometry.topology geometry)
  and p = Packed.Float3.View.view (Geometry.positions geometry) in
  let volume = ref 0. in
  for primitive = 0 to Bytes.length t.primitive_kinds - 1 do
    let first = t.primitive_offsets.(primitive) and last = t.primitive_offsets.(primitive + 1) in
    let a = t.vertex_points.(first) in
    for k = first + 1 to last - 2 do
      let b = t.vertex_points.(k) and c = t.vertex_points.(k + 1) in
      let bx = p.x.(b) and by = p.y.(b) and bz = p.z.(b)
      and cx = p.x.(c) and cy = p.y.(c) and cz = p.z.(c) in
      volume := !volume +. (p.x.(a) *. ((by *. cz) -. (bz *. cy))
        +. p.y.(a) *. ((bz *. cx) -. (bx *. cz))
        +. p.z.(a) *. ((bx *. cy) -. (by *. cx)))
    done
  done;
  !volume /. 6.

type disjoint_set = { parent : int array; rank : bytes }

let disjoint_set count allowed =
  { parent = Array.init count (fun element -> if allowed element then element else -1);
    rank = Bytes.make count '\000' }

let root set element =
  let cursor = ref element in
  while set.parent.(!cursor) <> !cursor do cursor := set.parent.(!cursor) done;
  let result = !cursor in
  let cursor = ref element in
  while set.parent.(!cursor) <> !cursor do
    let next = set.parent.(!cursor) in
    set.parent.(!cursor) <- result;
    cursor := next
  done;
  result

let union set left right =
  if left >= 0 && right >= 0 && set.parent.(left) >= 0 && set.parent.(right) >= 0
  then begin
    let left = root set left and right = root set right in
    if left <> right then begin
      let left_rank = Char.code (Bytes.unsafe_get set.rank left)
      and right_rank = Char.code (Bytes.unsafe_get set.rank right) in
      if left_rank < right_rank then set.parent.(left) <- right
      else if left_rank > right_rank then set.parent.(right) <- left
      else begin
        set.parent.(right) <- left;
        Bytes.unsafe_set set.rank left (Char.chr (left_rank + 1))
      end
    end
  end

let component_classes set =
  let count = Array.length set.parent in
  (* [parent] and [rank] are locally owned after the final union. Reuse rank
     byte 255 as an assigned-class marker and parent as the output plane.
     Union-by-rank cannot reach 255 for an OCaml-sized array. Marking every
     visited path node makes later traversals stop at an already classified
     ancestor, so stable classes need one pass and no owner-sized class map. *)
  let next_class = ref 0 in
  for element = 0 to count - 1 do
    if set.parent.(element) >= 0 then begin
      let representative = ref element in
      while Char.code (Bytes.unsafe_get set.rank !representative) <> 255
          && set.parent.(!representative) <> !representative do
        representative := set.parent.(!representative)
      done;
      let class_id =
        if Char.code (Bytes.unsafe_get set.rank !representative) = 255 then
          set.parent.(!representative)
        else begin
          let class_id = !next_class in
          incr next_class;
          set.parent.(!representative) <- class_id;
          Bytes.unsafe_set set.rank !representative '\255';
          class_id
        end in
      let cursor = ref element in
      while Char.code (Bytes.unsafe_get set.rank !cursor) <> 255 do
        let next = set.parent.(!cursor) in
        set.parent.(!cursor) <- class_id;
        Bytes.unsafe_set set.rank !cursor '\255';
        cursor := next
      done
    end
  done;
  set.parent, !next_class

let require_connectivity_group label owner count = function
  | None -> Ok ()
  | Some group when Group.owner group <> owner -> Error (Printf.sprintf
      "Connectivity: %s must be %s-owned" label
      (match owner with Group.Point -> "point" | Group.Vertex -> "vertex"
        | Group.Primitive -> "primitive"))
  | Some group when Group.length group <> count -> Error (Printf.sprintf
      "Connectivity: %s length does not match geometry" label)
  | Some _ -> Ok ()

let classify_connectivity ?primitives ?points ?seams owner geometry =
  Error.guard ~operation:"connectivity" ~code:"invalid_connectivity"
    ~cancelled:"connectivity classification was cancelled" @@ fun () ->
  try
  let point_count = Geometry.point_count geometry
  and primitive_count = Geometry.primitive_count geometry in
  let topology_value = Geometry.topology geometry in
  let validation =
    if owner = Connectivity_primitives && points <> None then
      Error "Connectivity: point include group is available only in point mode"
    else Result.bind
        (require_connectivity_group "primitive include group" Group.Primitive
           primitive_count primitives)
        (fun () -> require_connectivity_group "point include group" Group.Point
          point_count points)
  in
  Result.bind validation (fun () ->
    match seams with
    | Some group when Edge_group.topology_data_id group
        <> Topology.data_id topology_value ->
        Error "Connectivity: seam group belongs to a different topology"
    | None | Some _ ->
        let topology = Topology.View.view topology_value in
        let primitive_allowed primitive = match primitives with
          | None -> true | Some group -> Group.mem primitive group in
        let point_allowed point = match points with
          | None -> true | Some group -> Group.mem point group in
        match owner with
        | Connectivity_points ->
            let set = disjoint_set point_count point_allowed in
            let index_value = Topology_index.create topology_value in
            let reverse = Topology_index.View.view index_value in
            let edge_validation = match seams with
              | Some group when Edge_group.length group
                  <> Topology_index.edge_count index_value ->
                  Error "Connectivity: seam group length does not match topology"
              | None | Some _ -> Ok () in
            Result.map (fun () ->
              for primitive = 0 to primitive_count - 1 do
                if primitive_allowed primitive then begin
                  let first = topology.primitive_offsets.(primitive)
                  and last = topology.primitive_offsets.(primitive + 1) in
                  for vertex = first to last - 1 do
                    let edge = reverse.edge_of_vertex.(vertex) in
                    let is_seam = edge >= 0 && match seams with
                      | None -> false | Some group -> Edge_group.mem edge group in
                    if edge >= 0 && not is_seam then
                      union set reverse.edge_a.(edge) reverse.edge_b.(edge)
                  done
                end
              done;
              component_classes set) edge_validation
        | Connectivity_primitives ->
            let set = disjoint_set primitive_count primitive_allowed in
            match seams with
            | None ->
                let first_primitive = Array.make point_count (-1) in
                for primitive = 0 to primitive_count - 1 do
                  if primitive_allowed primitive then begin
                    let first = topology.primitive_offsets.(primitive)
                    and last = topology.primitive_offsets.(primitive + 1) in
                    for vertex = first to last - 1 do
                      let point = topology.vertex_points.(vertex) in
                      let previous = first_primitive.(point) in
                      if previous < 0 then first_primitive.(point) <- primitive
                      else union set primitive previous
                    done
                  end
                done;
                Ok (component_classes set)
            | Some group ->
                let index = Topology_index.create topology_value in
                let reverse = Topology_index.View.view index in
                if Edge_group.length group <> Topology_index.edge_count index then
                  Error "Connectivity: seam group length does not match topology"
                else begin
                  for edge = 0 to Topology_index.edge_count index - 1 do
                    if not (Edge_group.mem edge group) then begin
                      let first = reverse.edge_offsets.(edge)
                      and last = reverse.edge_offsets.(edge + 1) in
                      for left_slot = first to last - 2 do
                        let left = reverse.primitive_of_vertex.(reverse.edge_vertices.(left_slot)) in
                        if primitive_allowed left then
                          for right_slot = left_slot + 1 to last - 1 do
                            let right = reverse.primitive_of_vertex.(reverse.edge_vertices.(right_slot)) in
                            if primitive_allowed right then union set left right
                          done
                      done
                    end
                  done;
                  Ok (component_classes set)
                end)
  with Invalid_argument message ->
    Error.fail_with ~operation:"connectivity" ~code:"invalid_parameter" message

