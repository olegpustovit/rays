
type projection_scratch = {
  count : int;
  primitives : int array;
  triangles : int array;
  barycentric_a : float array;
  barycentric_b : float array;
  barycentric_c : float array;
  distances_squared : float array;
}

let operation = "Rdk_mesh.Remesh.remesh"
let fail message = Error (operation ^ ": " ^ message)

let validate_name label = function
  | None -> Ok ()
  | Some name when String.trim name = "" ->
      fail (label ^ " name must not be empty")
  | Some _ -> Ok ()

let validate_point_group geometry = function
  | None -> Ok ()
  | Some group when Group.owner group <> Group.Point ->
      fail "hard point selection must own points"
  | Some group when Group.length group <> Geometry.point_count geometry ->
      fail "hard point selection length does not match point count"
  | Some _ -> Ok ()

let validate_edge_group geometry = function
  | None -> Ok ()
  | Some group ->
      let topology = Geometry.topology geometry in
      let index = Topology_index.create topology in
      if Edge_group.topology_data_id group <> Topology.data_id topology then
        fail "hard edge selection belongs to a different topology"
      else if Edge_group.length group <> Topology_index.edge_count index then
        fail "hard edge selection length does not match topology edge count"
      else Ok ()

let validate_positions ?cancel geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let invalid = ref (-1) and point = ref 0 in
  while !point < Array.length positions.x && !invalid < 0 do
    if !point land 4095 = 0 then Cancel.check_opt cancel;
    if not (Float.is_finite positions.x.(!point) && Float.is_finite positions.y.(!point)
        && Float.is_finite positions.z.(!point)) then invalid := !point;
    incr point
  done;
  if !invalid < 0 then Ok ()
  else fail (Printf.sprintf "point %d has a non-finite position" !invalid)

let target_values ?cancel name geometry = match name with
  | None -> Ok None
  | Some name ->
      if String.trim name = "" then fail "target-size attribute name must not be empty"
      else match Geometry.find_attribute ~owner:Attribute.Point name geometry with
        | None -> fail (Printf.sprintf
            "point target-size attribute %S does not exist" name)
        | Some attribute ->
            (match Attribute.View.storage attribute with
             | Attribute.Float values ->
                 let invalid = ref (-1) and point = ref 0 in
                 while !point < Array.length values && !invalid < 0 do
                   if !point land 4095 = 0 then Cancel.check_opt cancel;
                   if not (Float.is_finite values.(!point)) || values.(!point) <= 0. then
                     invalid := !point;
                   incr point
                 done;
                 if !invalid < 0 then Ok (Some values)
                 else fail (Printf.sprintf
                     "target-size attribute %S must be finite and positive at point %d"
                     name !invalid)
             | Attribute.Int _ | Attribute.Int_array _ | Attribute.Float_array _
             | Attribute.Float2 _ | Attribute.Float3 _ | Attribute.Float4 _
             | Attribute.Text _ -> fail (Printf.sprintf
                 "point target-size attribute %S must be scalar float" name))

let fresh_group_name geometry owner base reserved =
  let rec loop suffix =
    let name = if suffix = 0 then base else base ^ string_of_int suffix in
    if Some name = reserved || Geometry.find_group ~owner name geometry <> None
    then loop (suffix + 1) else name
  in
  loop 0

let fresh_edge_group_name geometry base reserved =
  let rec loop suffix =
    let name = if suffix = 0 then base else base ^ string_of_int suffix in
    if Some name = reserved || Geometry.find_edge_group name geometry <> None
    then loop (suffix + 1) else name
  in
  loop 0

let uv_reader name geometry =
  match Geometry.find_attribute ~owner:Attribute.Vertex name geometry with
  | None -> Ok None
  | Some attribute ->
      let equal = match Attribute.View.storage attribute with
        | Attribute.Float values -> Some (fun a b -> values.(a) = values.(b))
        | Attribute.Float2 values ->
            let values = Packed.Float2.View.view values in
            Some (fun a b -> values.x.(a) = values.x.(b)
              && values.y.(a) = values.y.(b))
        | Attribute.Float3 values ->
            let values = Packed.Float3.View.view values in
            Some (fun a b -> values.x.(a) = values.x.(b)
              && values.y.(a) = values.y.(b) && values.z.(a) = values.z.(b))
        | Attribute.Float4 values ->
            let values = Packed.Float4.View.view values in
            Some (fun a b -> values.x.(a) = values.x.(b)
              && values.y.(a) = values.y.(b) && values.z.(a) = values.z.(b)
              && values.w.(a) = values.w.(b))
        | Attribute.Int _ | Attribute.Int_array _ | Attribute.Float_array _
        | Attribute.Text _ -> None in
      (match equal with
       | Some equal -> Ok (Some equal)
       | None -> fail (Printf.sprintf
           "vertex UV attribute %S must be float, float2, float3, or float4"
           name))

let endpoint_vertices topology index edge incidence =
  let vertex = index.Topology_index.View.edge_vertices.(incidence) in
  let next = index.next_vertex.(vertex) in
  if next < 0 then None
  else
    let a = index.edge_a.(edge) and b = index.edge_b.(edge)
    and point = topology.Topology.View.vertex_points.(vertex)
    and next_point = topology.vertex_points.(next) in
    if point = a && next_point = b then Some (vertex, next)
    else if point = b && next_point = a then Some (next, vertex)
    else None

let install_feature_groups ?cancel ~grain ~hard_points ~hard_edges
    ~preserve_uv_seams ~uv_attribute ~point_name ~edge_name geometry =
  let topology_value = Geometry.topology geometry in
  let topology = Topology.View.view topology_value in
  let index_value = Topology_index.create ?cancel topology_value in
  let index = Topology_index.View.view index_value in
  let uv = if preserve_uv_seams then uv_reader uv_attribute geometry else Ok None in
  Result.bind uv (fun uv_equal ->
    let feature_edges = Edge_group.init ~grain ~topology:topology_value
        ~index:index_value ~name:edge_name (fun edge ->
      if edge land 4095 = 0 then Cancel.check_opt cancel;
      let explicit = match hard_edges with
        | None -> false | Some group -> Edge_group.mem edge group in
      if explicit then true
      else
        let first = index.edge_offsets.(edge)
        and last = index.edge_offsets.(edge + 1) in
        if last - first > 2 then true
        else match uv_equal with
          | None -> false
          | Some equal when last - first = 2 ->
              (match endpoint_vertices topology index edge first,
                  endpoint_vertices topology index edge (first + 1) with
               | Some (a0, b0), Some (a1, b1) ->
                   not (equal a0 a1 && equal b0 b1)
               | _ -> true)
          | Some _ -> false) in
    let points = Group.init ~grain ~owner:Group.Point ~name:point_name
        (Geometry.point_count geometry) (fun point -> match hard_points with
          | None -> false | Some group -> Group.mem point group) in
    Result.bind (Geometry.with_edge_group feature_edges geometry) (fun geometry ->
      Geometry.with_group points geometry))

let feature_group name geometry = match Geometry.find_edge_group name geometry with
  | Some group -> Ok group
  | None -> fail "internal feature-edge ancestry was lost"

let primitive_is_triangle topology primitive =
  Bytes.unsafe_get topology.Topology.View.primitive_kinds primitive = '\000'
  && topology.primitive_offsets.(primitive + 1)
     - topology.primitive_offsets.(primitive) = 3

let checked_add label left right =
  if left < 0 || right < 0 || left > Sys.max_array_length - right then
    invalid_arg (operation ^ ": " ^ label ^ " exceeds array limits");
  left + right

(* ------------------------------------------------------------------------ *)
(* Local edit structure.

   One remesh iteration edits a mutable triangle mesh in place: split, collapse
   and flip update point, corner and edge incidence directly, and the result is
   materialized into one [Geometry.t] per iteration instead of one per stage.
   Points, corners and triangles carry ancestry into the iteration-start
   geometry, so attributes and groups are interpolated once, with the rules the
   separate split, collapse and flip kernels apply: a midpoint or split corner
   weighs its two ancestors equally, a collapsed pair keeps the lower point's
   payload and averages positions, and a flipped corner rotates its payload
   with the corner. Wherever a selection tie-breaks on edge index, edges are
   renumbered in [Topology_index] order (first sight while scanning corners in
   output order), so the output stays byte-identical to the chained kernels.

   Storage is O(points + corners + edges) in packed arrays that grow
   geometrically; each edit costs O(local valence) and no edit looks an edge
   up: split derives child edges from the parent's, collapse merges the two
   edges it knows meet, and flip creates the one diagonal it checked absent.
   The structure is sequential and rebuilt from the materialized geometry
   (and its shared reverse index) every iteration. *)

type local = {
  cancel : Cancel.t option;
  grain : int;
  mutable start : Geometry.t;   (* geometry the ancestry refers to *)
  (* points *)
  mutable points : int;
  mutable x : float array; mutable y : float array; mutable z : float array;
  mutable p_left : int array; mutable p_right : int array;
  mutable p_weight : float array;
  mutable p_alive : Bytes.t;
  mutable p_used : Bytes.t;            (* incident to a triangle when created *)
  mutable p_first : int array;         (* first corner at the point, or -1 *)
  mutable p_target : float array;      (* per-point target size, when adaptive *)
  point_groups : Group.t array;        (* iteration-start point groups *)
  mutable p_groups : Bytes.t array;    (* one byte per point, per point group *)
  p_orders : int array option array;   (* explicit order per point group *)
  vertex_groups : Group.t array;       (* iteration-start vertex groups *)
  v_orders : int array option array;   (* explicit order, over corner slots *)
  (* corners: three per triangle slot *)
  mutable tris : int;
  mutable c_point : int array;
  mutable c_left : int array; mutable c_right : int array;
  mutable c_weight : float array;
  mutable c_next_at_point : int array;
  mutable c_edge : int array;
  mutable c_next_in_edge : int array;
  mutable t_alive : Bytes.t;
  mutable t_source : int array;        (* iteration-start primitive *)
  mutable t_children : int array;      (* first child slot of a split source *)
  mutable t_child_count : int array;
  source_tris : int;
  (* edges *)
  mutable edges : int;
  mutable live_edges : int;
  mutable e_a : int array; mutable e_b : int array;
  mutable e_first : int array;         (* first corner, or -1 *)
  mutable e_incidence : int array;
  mutable e_alive : Bytes.t;           (* present in the table *)
  edge_group_names : string array;
  mutable e_groups : Bytes.t array;    (* one byte per edge, per edge group *)
  mutable scratch : int array;         (* per-edit work list, reused *)
  mutable scratch_count : int;
}

let capacity_for label current needed =
  if needed > Sys.max_array_length then
    invalid_arg (operation ^ ": " ^ label ^ " exceeds array limits");
  min Sys.max_array_length (max needed (max 16 (current * 2)))

let grow_int source capacity fill =
  let target = Array.make capacity fill in
  Array.blit source 0 target 0 (Array.length source); target

let grow_float source capacity =
  let target = Array.make capacity 0. in
  Array.blit source 0 target 0 (Array.length source); target

let grow_bytes source capacity =
  let target = Bytes.make capacity '\000' in
  Bytes.blit source 0 target 0 (Bytes.length source); target

let scratch_push l value =
  if l.scratch_count = Array.length l.scratch then
    l.scratch <- grow_int l.scratch (max 64 (2 * l.scratch_count)) 0;
  l.scratch.(l.scratch_count) <- value;
  l.scratch_count <- l.scratch_count + 1

let ensure_points l needed =
  if needed > Array.length l.x then begin
    let capacity = capacity_for "point cardinality" (Array.length l.x) needed in
    l.x <- grow_float l.x capacity; l.y <- grow_float l.y capacity;
    l.z <- grow_float l.z capacity;
    l.p_left <- grow_int l.p_left capacity 0;
    l.p_right <- grow_int l.p_right capacity 0;
    l.p_weight <- grow_float l.p_weight capacity;
    l.p_alive <- grow_bytes l.p_alive capacity;
    l.p_used <- grow_bytes l.p_used capacity;
    l.p_first <- grow_int l.p_first capacity (-1);
    if Array.length l.p_target > 0 then
      l.p_target <- grow_float l.p_target capacity;
    l.p_groups <- Array.map (fun bytes -> grow_bytes bytes capacity) l.p_groups
  end

let ensure_tris l needed =
  if needed > Array.length l.t_source then begin
    let capacity = capacity_for "primitive cardinality"
        (Array.length l.t_source) needed in
    if capacity > Sys.max_array_length / 3 then
      invalid_arg (operation ^ ": vertex cardinality exceeds array limits");
    let corners = capacity * 3 in
    l.c_point <- grow_int l.c_point corners 0;
    l.c_left <- grow_int l.c_left corners 0;
    l.c_right <- grow_int l.c_right corners 0;
    l.c_weight <- grow_float l.c_weight corners;
    l.c_next_at_point <- grow_int l.c_next_at_point corners (-1);
    l.c_edge <- grow_int l.c_edge corners (-1);
    l.c_next_in_edge <- grow_int l.c_next_in_edge corners (-1);
    l.t_alive <- grow_bytes l.t_alive capacity;
    l.t_source <- grow_int l.t_source capacity 0;
    l.t_children <- grow_int l.t_children capacity (-1);
    l.t_child_count <- grow_int l.t_child_count capacity 0
  end

let ensure_edges l needed =
  if needed > Array.length l.e_a then begin
    let capacity = capacity_for "edge cardinality" (Array.length l.e_a) needed in
    l.e_a <- grow_int l.e_a capacity 0; l.e_b <- grow_int l.e_b capacity 0;
    l.e_first <- grow_int l.e_first capacity (-1);
    l.e_incidence <- grow_int l.e_incidence capacity 0;
    l.e_alive <- grow_bytes l.e_alive capacity;
    l.e_groups <- Array.map (fun bytes -> grow_bytes bytes capacity) l.e_groups
  end

let edge_add l a b =
  ensure_edges l (l.edges + 1);
  let edge = l.edges in
  l.edges <- edge + 1;
  l.live_edges <- l.live_edges + 1;
  l.e_a.(edge) <- a; l.e_b.(edge) <- b;
  l.e_first.(edge) <- -1; l.e_incidence.(edge) <- 0;
  Bytes.unsafe_set l.e_alive edge '\001';
  Array.iter (fun bytes -> Bytes.unsafe_set bytes edge '\000') l.e_groups;
  edge

let edge_kill l edge =
  if Bytes.unsafe_get l.e_alive edge <> '\000' then begin
    Bytes.unsafe_set l.e_alive edge '\000';
    l.e_first.(edge) <- -1;
    l.live_edges <- l.live_edges - 1
  end

let edge_revive l edge =
  if Bytes.unsafe_get l.e_alive edge = '\000' then begin
    Bytes.unsafe_set l.e_alive edge '\001';
    l.live_edges <- l.live_edges + 1
  end

let[@inline] edge_live l edge = Bytes.unsafe_get l.e_alive edge <> '\000'

let[@inline] corner_next corner = corner - (corner mod 3) + ((corner + 1) mod 3)
let[@inline] corner_previous corner = corner - (corner mod 3) + ((corner + 2) mod 3)
let[@inline] opposite_point l corner =
  l.c_point.(corner_next (corner_next corner))

let attach_edge l corner edge =
  l.c_edge.(corner) <- edge;
  l.c_next_in_edge.(corner) <- l.e_first.(edge);
  l.e_first.(edge) <- corner;
  l.e_incidence.(edge) <- l.e_incidence.(edge) + 1

(* The edge joining [a] and [b], found along [a]'s corners, or -1. *)
let adjacent_edge l a b =
  let found = ref (-1) and corner = ref l.p_first.(a) in
  while !found < 0 && !corner >= 0 do
    let previous = corner_previous !corner in
    if l.c_point.(corner_next !corner) = b then found := l.c_edge.(!corner)
    else if l.c_point.(previous) = b then found := l.c_edge.(previous);
    corner := l.c_next_at_point.(!corner)
  done;
  !found

(* Unlink a corner from its edge; the edge slot survives even at zero
   incidence so a rewired triangle can rejoin it with its groups intact. *)
let detach_edge_soft l corner =
  let edge = l.c_edge.(corner) in
  if l.e_first.(edge) = corner then l.e_first.(edge) <- l.c_next_in_edge.(corner)
  else begin
    let cursor = ref l.e_first.(edge) in
    while l.c_next_in_edge.(!cursor) <> corner do
      cursor := l.c_next_in_edge.(!cursor)
    done;
    l.c_next_in_edge.(!cursor) <- l.c_next_in_edge.(corner)
  end;
  l.e_incidence.(edge) <- l.e_incidence.(edge) - 1;
  l.c_edge.(corner) <- -1;
  edge

let detach_edge l corner =
  let edge = detach_edge_soft l corner in
  if l.e_incidence.(edge) = 0 then edge_kill l edge

let attach_point l corner point =
  l.c_point.(corner) <- point;
  l.c_next_at_point.(corner) <- l.p_first.(point);
  l.p_first.(point) <- corner

let detach_point l corner =
  let point = l.c_point.(corner) in
  if l.p_first.(point) = corner then
    l.p_first.(point) <- l.c_next_at_point.(corner)
  else begin
    let cursor = ref l.p_first.(point) in
    while l.c_next_at_point.(!cursor) <> corner do
      cursor := l.c_next_at_point.(!cursor)
    done;
    l.c_next_at_point.(!cursor) <- l.c_next_at_point.(corner)
  end

let detach_triangle l tri =
  let first = tri * 3 in
  detach_edge l first; detach_edge l (first + 1); detach_edge l (first + 2);
  detach_point l first; detach_point l (first + 1); detach_point l (first + 2);
  Bytes.unsafe_set l.t_alive tri '\000'

let point_group_index l name = let found = ref (-1) in
  Array.iteri (fun index group ->
    if Group.name group = name then found := index) l.point_groups;
  !found

let edge_group_index l name = let found = ref (-1) in
  Array.iteri (fun index candidate ->
    if String.equal candidate name then found := index) l.edge_group_names;
  !found

let local_of_geometry ?cancel ~grain ~target_size_attribute geometry =
  let topology_value = Geometry.topology geometry in
  let topology = Topology.View.view topology_value in
  let tris = Bytes.length topology.primitive_kinds in
  for primitive = 0 to tris - 1 do
    if primitive land 4095 = 0 then Cancel.check_opt cancel;
    if not (primitive_is_triangle topology primitive) then
      invalid_arg (Printf.sprintf "%s: primitive %d is not a triangle"
        operation primitive)
  done;
  let points = topology.point_count and corners = tris * 3 in
  (* The shared reverse index is usually cached for this geometry (feature
     installation, smoothing and projection build it); its edge numbering is
     the one the selections tie-break on. *)
  let index = Topology_index.View.view (Topology_index.create ?cancel topology_value) in
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let point_groups = Geometry.groups geometry
      |> List.filter (fun group -> Group.owner group = Group.Point)
      |> Array.of_list in
  let vertex_groups = Geometry.groups geometry
      |> List.filter (fun group -> Group.owner group = Group.Vertex)
      |> Array.of_list in
  let edge_groups = Array.of_list (Geometry.edge_groups geometry) in
  let p_target = match target_size_attribute with
    | None -> [||]
    | Some name -> (match Geometry.find_attribute ~owner:Attribute.Point name
        geometry with
      | Some attribute -> (match Attribute.View.storage attribute with
          | Attribute.Float values -> Array.copy values
          | _ -> invalid_arg (Printf.sprintf
              "%s: point target-size attribute %S must be scalar float"
              operation name))
      | None -> invalid_arg (Printf.sprintf
          "%s: point target-size attribute %S does not exist" operation name)) in
  let l = {
    cancel; grain; start = geometry;
    points; x = Array.copy positions.x; y = Array.copy positions.y;
    z = Array.copy positions.z;
    p_left = Array.init points Fun.id; p_right = Array.init points Fun.id;
    p_weight = Array.make points 0.;
    p_alive = Bytes.make points '\001'; p_used = Bytes.make points '\000';
    p_first = Array.make points (-1); p_target;
    point_groups;
    p_groups = Array.map (fun group -> Bytes.init points (fun point ->
      if Group.mem point group then '\001' else '\000')) point_groups;
    p_orders = Array.map (fun group -> Group.ordered_elements group) point_groups;
    vertex_groups;
    v_orders = Array.map (fun group -> Group.ordered_elements group) vertex_groups;
    tris; c_point = Array.sub topology.vertex_points 0 corners;
    c_left = Array.init corners Fun.id; c_right = Array.init corners Fun.id;
    c_weight = Array.make corners 0.;
    c_next_at_point = Array.make corners (-1);
    c_edge = Array.copy index.edge_of_vertex;
    c_next_in_edge = Array.make corners (-1);
    t_alive = Bytes.make tris '\001'; t_source = Array.init tris Fun.id;
    t_children = Array.make tris (-1); t_child_count = Array.make tris 0;
    source_tris = tris;
    edges = Array.length index.edge_a; live_edges = Array.length index.edge_a;
    e_a = Array.copy index.edge_a; e_b = Array.copy index.edge_b;
    e_first = Array.make (Array.length index.edge_a) (-1);
    e_incidence = Array.make (Array.length index.edge_a) 0;
    e_alive = Bytes.make (Array.length index.edge_a) '\001';
    edge_group_names = Array.map Edge_group.name edge_groups;
    e_groups = Array.map (fun _ -> Bytes.make (Array.length index.edge_a) '\000')
        edge_groups;
    scratch = Array.make 64 0; scratch_count = 0;
  } in
  for corner = 0 to corners - 1 do
    if corner land 16383 = 0 then Cancel.check_opt cancel;
    let point = l.c_point.(corner) in
    attach_point l corner point;
    Bytes.unsafe_set l.p_used point '\001';
    attach_edge l corner index.edge_of_vertex.(corner)
  done;
  Array.iteri (fun index group ->
    if Edge_group.length group <> l.edges then
      invalid_arg (operation ^ ": internal edge group does not match its topology");
    let bytes = l.e_groups.(index) in
    Edge_group.iter (fun edge -> Bytes.unsafe_set bytes edge '\001') group)
    edge_groups;
  l

(* Output order: surviving source slots and the children of split slots, in
   source order; edges by first sight along that corner sequence. *)
type ranking = {
  tri_count : int;
  order : int array;        (* rank -> triangle slot *)
  t_rank : int array;       (* slot -> rank, or -1 *)
  edge_count : int;
  e_order : int array;      (* rank -> edge slot *)
  point_offsets : int array;  (* CSR of live corners per point, output order *)
  point_corners : int array;
}

let rank ?(incidence = true) l =
  let order = Array.make l.tris 0 and count = ref 0 in
  let emit tri = order.(!count) <- tri; incr count in
  for tri = 0 to l.source_tris - 1 do
    if tri land 4095 = 0 then Cancel.check_opt l.cancel;
    if Bytes.unsafe_get l.t_alive tri <> '\000' then emit tri
    else begin
      let first = l.t_children.(tri) in
      if first >= 0 then
        for child = first to first + l.t_child_count.(tri) - 1 do
          if Bytes.unsafe_get l.t_alive child <> '\000' then emit child
        done
    end
  done;
  let t_rank = Array.make l.tris (-1) in
  for rank = 0 to !count - 1 do t_rank.(order.(rank)) <- rank done;
  let e_rank = Array.make l.edges (-1) and e_order = Array.make l.live_edges 0
  and edge_count = ref 0 in
  for rank = 0 to !count - 1 do
    let first = order.(rank) * 3 in
    for corner = first to first + 2 do
      let edge = l.c_edge.(corner) in
      if e_rank.(edge) < 0 then begin
        e_rank.(edge) <- !edge_count; e_order.(!edge_count) <- edge;
        incr edge_count
      end
    done
  done;
  let point_offsets, point_corners = if not incidence then [||], [||] else begin
    let point_offsets = Array.make (l.points + 1) 0 in
    for rank = 0 to !count - 1 do
      let first = order.(rank) * 3 in
      for corner = first to first + 2 do
        let point = l.c_point.(corner) in
        point_offsets.(point + 1) <- point_offsets.(point + 1) + 1
      done
    done;
    for point = 0 to l.points - 1 do
      point_offsets.(point + 1) <- point_offsets.(point + 1) + point_offsets.(point)
    done;
    let point_corners = Array.make (!count * 3) 0
    and cursor = Array.sub point_offsets 0 l.points in
    for rank = 0 to !count - 1 do
      let first = order.(rank) * 3 in
      for corner = first to first + 2 do
        let point = l.c_point.(corner) in
        point_corners.(cursor.(point)) <- corner;
        cursor.(point) <- cursor.(point) + 1
      done
    done;
    point_offsets, point_corners
  end in
  { tri_count = !count; order; t_rank; edge_count = !edge_count; e_order;
    point_offsets; point_corners }

(* The two incident corners of a two-sided edge, ordered as [Topology_index]
   would list them: by output corner position. *)
let sided_corners l ranking edge =
  let first = l.e_first.(edge) in
  let second = l.c_next_in_edge.(first) in
  let position corner = ranking.t_rank.(corner / 3) * 3 + (corner mod 3) in
  if position first <= position second then first, second else second, first

let[@inline] local_length l a b =
  Float.hypot (l.x.(a) -. l.x.(b))
    (Float.hypot (l.y.(a) -. l.y.(b)) (l.z.(a) -. l.z.(b)))

let[@inline] local_target l target_length a b =
  if Array.length l.p_target = 0 then target_length
  else 0.5 *. (l.p_target.(a) +. l.p_target.(b))

(* Selection over the freshly built structure: edge slots are in index order. *)
let select_long l ~target_length =
  let selected = Bytes.make l.edges '\000' and count = ref 0 in
  for edge = 0 to l.edges - 1 do
    if edge land 4095 = 0 then Cancel.check_opt l.cancel;
    let a = l.e_a.(edge) and b = l.e_b.(edge) in
    if local_length l a b > (4. /. 3.) *. local_target l target_length a b
    then begin Bytes.unsafe_set selected edge '\001'; incr count end
  done;
  selected, !count

let interpolate_target l a b =
  let t = 0.5 in
  let value = ((1. -. t) *. l.p_target.(a)) +. (t *. l.p_target.(b)) in
  if not (Float.is_finite value) || value <= 0. then
    invalid_arg (Printf.sprintf
      "%s: interpolated target size is not finite and positive between points %d and %d"
      operation a b);
  value

(* Rebuild every point and edge incidence list from the live triangles in slot
   order, then retire edges that no triangle references. *)
let relink l =
  Array.fill l.p_first 0 l.points (-1);
  Array.fill l.e_first 0 l.edges (-1);
  Array.fill l.e_incidence 0 l.edges 0;
  for tri = 0 to l.tris - 1 do
    if tri land 4095 = 0 then Cancel.check_opt l.cancel;
    if Bytes.unsafe_get l.t_alive tri <> '\000' then
      for corner = tri * 3 to tri * 3 + 2 do
        let point = l.c_point.(corner) and edge = l.c_edge.(corner) in
        l.c_next_at_point.(corner) <- l.p_first.(point);
        l.p_first.(point) <- corner;
        l.c_next_in_edge.(corner) <- l.e_first.(edge);
        l.e_first.(edge) <- corner;
        l.e_incidence.(edge) <- l.e_incidence.(edge) + 1
      done
  done;
  for edge = 0 to l.edges - 1 do
    if l.e_incidence.(edge) = 0 then edge_kill l edge
  done

(* Split every selected edge at its midpoint and every incident triangle into
   two to four, in the source split's emission pattern. Child edges are
   derived from the parent's: an unselected parent edge keeps its slot, a
   selected one is replaced by its two halves (which inherit its groups), and
   edges interior to the parent are fresh. No edge is looked up. *)
let split l selected count =
  let start_edges = l.edges and start_points = l.points in
  ensure_points l (checked_add "split point cardinality" start_points count);
  ensure_edges l (checked_add "split edge cardinality" start_edges (2 * count));
  let midpoint_of_edge = Array.make start_edges (-1)
  and half_low = Array.make start_edges (-1)
  and half_high = Array.make start_edges (-1) in
  for edge = 0 to start_edges - 1 do
    if edge land 4095 = 0 then Cancel.check_opt l.cancel;
    if Bytes.unsafe_get selected edge <> '\000' then begin
      let point = l.points in
      l.points <- point + 1;
      midpoint_of_edge.(edge) <- point;
      let a = l.e_a.(edge) and b = l.e_b.(edge) in
      l.x.(point) <- 0.5 *. l.x.(a) +. 0.5 *. l.x.(b);
      l.y.(point) <- 0.5 *. l.y.(a) +. 0.5 *. l.y.(b);
      l.z.(point) <- 0.5 *. l.z.(a) +. 0.5 *. l.z.(b);
      if not (Float.is_finite l.x.(point) && Float.is_finite l.y.(point) && Float.is_finite l.z.(point))
      then invalid_arg (Printf.sprintf "%s: split point %d is non-finite"
          operation point);
      l.p_left.(point) <- a; l.p_right.(point) <- b; l.p_weight.(point) <- 0.5;
      Bytes.unsafe_set l.p_alive point '\001';
      Bytes.unsafe_set l.p_used point '\001';
      l.p_first.(point) <- -1;
      if Array.length l.p_target > 0 then
        l.p_target.(point) <- interpolate_target l a b;
      Array.iter (fun bytes -> Bytes.unsafe_set bytes point
        (if Bytes.unsafe_get bytes a <> '\000' && Bytes.unsafe_get bytes b <> '\000'
         then '\001' else '\000')) l.p_groups;
      let low = edge_add l a point and high = edge_add l b point in
      half_low.(edge) <- low; half_high.(edge) <- high;
      Array.iter (fun bytes ->
        let bit = Bytes.unsafe_get bytes edge in
        Bytes.unsafe_set bytes low bit; Bytes.unsafe_set bytes high bit) l.e_groups
    end
  done;
  (* Ordered point groups: unchanged members keep their order, new midpoint
     members follow in point order. *)
  Array.iteri (fun index order -> match order with
    | None -> ()
    | Some order ->
        let bytes = l.p_groups.(index) in
        let added = ref 0 in
        for point = start_points to l.points - 1 do
          if Bytes.unsafe_get bytes point <> '\000' then incr added
        done;
        if !added > 0 then begin
          let extended = Array.make (Array.length order + !added) 0 in
          Array.blit order 0 extended 0 (Array.length order);
          let cursor = ref (Array.length order) in
          for point = start_points to l.points - 1 do
            if Bytes.unsafe_get bytes point <> '\000' then begin
              extended.(!cursor) <- point; incr cursor end
          done;
          l.p_orders.(index) <- Some extended
        end) l.p_orders;
  let first_child = l.tris in
  let additions = ref 0 in
  for tri = 0 to l.source_tris - 1 do
    if tri land 4095 = 0 then Cancel.check_opt l.cancel;
    let first = tri * 3 in
    let mask = ref 0 in
    for local = 0 to 2 do
      if midpoint_of_edge.(l.c_edge.(first + local)) >= 0 then
        mask := !mask lor (1 lsl local)
    done;
    if !mask <> 0 then
      additions := checked_add "split primitive cardinality" !additions
          (1 + (!mask land 1) + ((!mask lsr 1) land 1) + ((!mask lsr 2) land 1))
  done;
  ensure_tris l (checked_add "split primitive cardinality" l.tris !additions);
  let saved_point = Array.make 3 0 and saved_edge = Array.make 3 0
  and interior = Array.make 6 (-1) in
  (* Descriptors 0-2 are the parent's corners, 3-5 the midpoints of its
     edges 0-2. *)
  let point_of descriptor =
    if descriptor < 3 then saved_point.(descriptor)
    else midpoint_of_edge.(saved_edge.(descriptor - 3)) in
  let interior_edge low high =
    let slot = match low, high with
      | 2, 3 -> 0 | 0, 4 -> 1 | 1, 5 -> 2 | 3, 4 -> 3 | 4, 5 -> 4 | 3, 5 -> 5
      | _ -> invalid_arg (operation ^ ": split pattern references an unknown edge") in
    if interior.(slot) < 0 then begin
      let a = point_of low and b = point_of high in
      interior.(slot) <- edge_add l (min a b) (max a b)
    end;
    interior.(slot) in
  let edge_of_pair d0 d1 =
    let low = min d0 d1 and high = max d0 d1 in
    if high < 3 then saved_edge.(if (low + 1) mod 3 = high then low else high)
    else if low >= 3 then interior_edge low high
    else begin
      let local = high - 3 in
      if low = local || low = (local + 1) mod 3 then begin
        let edge = saved_edge.(local) in
        if saved_point.(low) = l.e_a.(edge) then half_low.(edge) else half_high.(edge)
      end else interior_edge low high
    end in
  let emit tri descriptors =
    let child = l.tris in
    l.tris <- child + 1;
    l.t_source.(child) <- tri;
    l.t_children.(child) <- -1;
    l.t_child_count.(child) <- 0;
    Bytes.unsafe_set l.t_alive child '\001';
    let first = tri * 3 and target = child * 3 in
    for slot = 0 to 2 do
      let descriptor = descriptors.(slot) in
      let corner = target + slot in
      l.c_point.(corner) <- point_of descriptor;
      if descriptor < 3 then begin
        l.c_left.(corner) <- first + descriptor;
        l.c_right.(corner) <- first + descriptor;
        l.c_weight.(corner) <- 0.
      end else begin
        let local = descriptor - 3 in
        l.c_left.(corner) <- first + local;
        l.c_right.(corner) <- first + ((local + 1) mod 3);
        l.c_weight.(corner) <- 0.5
      end;
      l.c_edge.(corner) <- edge_of_pair descriptor descriptors.((slot + 1) mod 3)
    done in
  let pattern = [|
    [||];
    [| [|0;3;2|]; [|3;1;2|] |];
    [| [|1;4;0|]; [|4;2;0|] |];
    [| [|0;3;2|]; [|3;4;2|]; [|3;1;4|] |];
    [| [|2;5;1|]; [|5;0;1|] |];
    [| [|2;5;1|]; [|5;3;1|]; [|5;0;3|] |];
    [| [|1;4;0|]; [|4;5;0|]; [|4;2;5|] |];
    [| [|0;3;5|]; [|3;1;4|]; [|5;4;2|]; [|3;4;5|] |] |] in
  for tri = 0 to l.source_tris - 1 do
    if tri land 4095 = 0 then Cancel.check_opt l.cancel;
    let first = tri * 3 in
    let mask = ref 0 in
    for local = 0 to 2 do
      let edge = l.c_edge.(first + local) in
      saved_point.(local) <- l.c_point.(first + local);
      saved_edge.(local) <- edge;
      if midpoint_of_edge.(edge) >= 0 then mask := !mask lor (1 lsl local)
    done;
    if !mask <> 0 then begin
      Bytes.unsafe_set l.t_alive tri '\000';
      Array.fill interior 0 6 (-1);
      let children = pattern.(!mask) in
      l.t_children.(tri) <- l.tris;
      l.t_child_count.(tri) <- Array.length children;
      Array.iter (emit tri) children
    end
  done;
  relink l;
  (* Ordered vertex groups: a split corner is replaced by its unweighted
     descendants in slot order; new midpoint corners that are members follow. *)
  if Array.exists Option.is_some l.v_orders then begin
    let corners = l.tris * 3 and start_corners = first_child * 3 in
    let bucket_first = Array.make start_corners (-1)
    and bucket_next = Array.make corners (-1) in
    for corner = corners - 1 downto start_corners do
      if l.c_weight.(corner) = 0. then begin
        let source = l.c_left.(corner) in
        bucket_next.(corner) <- bucket_first.(source);
        bucket_first.(source) <- corner
      end
    done;
    Array.iteri (fun index order -> match order with
      | None -> ()
      | Some order ->
          let group = l.vertex_groups.(index) in
          let output = ref [] in
          for position = Array.length order - 1 downto 0 do
            let corner = order.(position) in
            if Bytes.unsafe_get l.t_alive (corner / 3) <> '\000' then
              output := corner :: !output
            else begin
              let children = ref [] and cursor = ref bucket_first.(corner) in
              while !cursor >= 0 do
                children := !cursor :: !children;
                cursor := bucket_next.(!cursor)
              done;
              output := List.rev_append !children !output
            end
          done;
          let appended = ref [] in
          for corner = corners - 1 downto start_corners do
            if l.c_weight.(corner) <> 0.
                && Group.mem l.c_left.(corner) group
                && Group.mem l.c_right.(corner) group then
              appended := corner :: !appended
          done;
          l.v_orders.(index) <- Some (Array.of_list (!output @ !appended)))
      l.v_orders
  end

(* Points held by hard selection, a feature edge, or a non-two-sided edge. *)
let constrained_points l ~hard ~feature =
  let out = Bytes.make l.points '\000' in
  if hard >= 0 then Bytes.blit l.p_groups.(hard) 0 out 0 l.points;
  let features = if feature >= 0 then Some l.e_groups.(feature) else None in
  for edge = 0 to l.edges - 1 do
    if edge_live l edge
        && (l.e_incidence.(edge) <> 2
            || match features with
              | Some bytes -> Bytes.unsafe_get bytes edge <> '\000'
              | None -> false) then begin
      Bytes.unsafe_set out l.e_a.(edge) '\001';
      Bytes.unsafe_set out l.e_b.(edge) '\001'
    end
  done;
  out

(* Stable LSD radix sort of candidate ranks by the bit pattern of their
   non-negative lengths (monotone in the value), four 16-bit passes; ties keep
   rank order, which is the (length, edge index) order the chained kernel
   sorted by. *)
let sort_candidates bits keys count =
  let scratch_bits = Array.make count 0 and scratch_keys = Array.make count 0
  and counts = Array.make 65_537 0 in
  let pass_from source_bits source_keys target_bits target_keys shift =
    Array.fill counts 0 65_537 0;
    for index = 0 to count - 1 do
      let digit = (source_bits.(index) lsr shift) land 0xffff in
      counts.(digit + 1) <- counts.(digit + 1) + 1
    done;
    for digit = 0 to 65_535 do
      counts.(digit + 1) <- counts.(digit + 1) + counts.(digit)
    done;
    for index = 0 to count - 1 do
      let value = source_bits.(index) in
      let digit = (value lsr shift) land 0xffff in
      let slot = counts.(digit) in
      counts.(digit) <- slot + 1;
      target_bits.(slot) <- value;
      target_keys.(slot) <- source_keys.(index)
    done in
  (* Four passes leave the sorted data back in [bits] and [keys]. *)
  pass_from bits keys scratch_bits scratch_keys 0;
  pass_from scratch_bits scratch_keys bits keys 16;
  pass_from bits keys scratch_bits scratch_keys 32;
  pass_from scratch_bits scratch_keys bits keys 48

(* Same test as the chained kernel's, without tuple allocation: the triangle
   keeps a non-degenerate normal on the old side after [a] and [b] move to
   the midpoint. *)
let collapse_preserves_triangle l tri a b mx my mz =
  let first = tri * 3 in
  let p0 = l.c_point.(first) and p1 = l.c_point.(first + 1)
  and p2 = l.c_point.(first + 2) in
  let abx = l.x.(p1) -. l.x.(p0) and aby = l.y.(p1) -. l.y.(p0)
  and abz = l.z.(p1) -. l.z.(p0) and acx = l.x.(p2) -. l.x.(p0)
  and acy = l.y.(p2) -. l.y.(p0) and acz = l.z.(p2) -. l.z.(p0) in
  let scale = Float.max (Float.max (Float.abs abx) (Float.abs aby))
      (Float.max (Float.abs abz) (Float.max (Float.abs acx)
        (Float.max (Float.abs acy) (Float.abs acz)))) in
  let flat = scale = 0. in
  let abx = if flat then 0. else abx /. scale
  and aby = if flat then 0. else aby /. scale
  and abz = if flat then 0. else abz /. scale
  and acx = if flat then 0. else acx /. scale
  and acy = if flat then 0. else acy /. scale
  and acz = if flat then 0. else acz /. scale in
  let ox = aby *. acz -. abz *. acy and oy = abz *. acx -. abx *. acz
  and oz = abx *. acy -. aby *. acx in
  let x0 = if p0 = a || p0 = b then mx else l.x.(p0)
  and y0 = if p0 = a || p0 = b then my else l.y.(p0)
  and z0 = if p0 = a || p0 = b then mz else l.z.(p0)
  and x1 = if p1 = a || p1 = b then mx else l.x.(p1)
  and y1 = if p1 = a || p1 = b then my else l.y.(p1)
  and z1 = if p1 = a || p1 = b then mz else l.z.(p1)
  and x2 = if p2 = a || p2 = b then mx else l.x.(p2)
  and y2 = if p2 = a || p2 = b then my else l.y.(p2)
  and z2 = if p2 = a || p2 = b then mz else l.z.(p2) in
  let abx = x1 -. x0 and aby = y1 -. y0 and abz = z1 -. z0
  and acx = x2 -. x0 and acy = y2 -. y0 and acz = z2 -. z0 in
  let scale = Float.max (Float.max (Float.abs abx) (Float.abs aby))
      (Float.max (Float.abs abz) (Float.max (Float.abs acx)
        (Float.max (Float.abs acy) (Float.abs acz)))) in
  if scale = 0. then false
  else begin
    let abx = abx /. scale and aby = aby /. scale and abz = abz /. scale
    and acx = acx /. scale and acy = acy /. scale and acz = acz /. scale in
    let nx = aby *. acz -. abz *. acy
    and ny = abz *. acx -. abx *. acz
    and nz = abx *. acy -. aby *. acx in
    let new_norm2 = nx *. nx +. ny *. ny +. nz *. nz
    and old_norm2 = ox *. ox +. oy *. oy +. oz *. oz in
    new_norm2 > 1e-28 && old_norm2 > 1e-28
    && ox *. nx +. oy *. ny +. oz *. nz > 1e-14 *. sqrt (old_norm2 *. new_norm2)
  end

(* Does collapsing [(a, b)] keep every other triangle at [point] valid and
   unreserved? *)
let inspect_point l ranking used_tris tri_a tri_b a b mx my mz point =
  let first = ranking.point_offsets.(point)
  and last = ranking.point_offsets.(point + 1) in
  let valid = ref true and slot = ref first in
  while !slot < last && !valid do
    let tri = ranking.point_corners.(!slot) / 3 in
    if tri <> tri_a && tri <> tri_b then
      if Bytes.unsafe_get used_tris tri <> '\000'
          || not (collapse_preserves_triangle l tri a b mx my mz) then
        valid := false;
    incr slot
  done;
  !valid

let reserve_point ranking used_tris point =
  for slot = ranking.point_offsets.(point) to ranking.point_offsets.(point + 1) - 1 do
    Bytes.unsafe_set used_tris (ranking.point_corners.(slot) / 3) '\001'
  done

(* The link condition: [a] and [b] share exactly the opposite points of the
   edge's two triangles as neighbours. *)
let link_condition l ranking stamps edge_stamps stamp v0 v1 a b =
  let c = opposite_point l v0 and d = opposite_point l v1 in
  for slot = ranking.point_offsets.(a) to ranking.point_offsets.(a + 1) - 1 do
    let corner = ranking.point_corners.(slot) in
    let out = l.c_point.(corner_next corner)
    and incoming = l.c_point.(corner_previous corner) in
    if out <> a then stamps.(out) <- stamp;
    if incoming <> a then stamps.(incoming) <- stamp
  done;
  let common = ref 0 and valid = ref true in
  for slot = ranking.point_offsets.(b) to ranking.point_offsets.(b + 1) - 1 do
    let corner = ranking.point_corners.(slot) in
    let previous = corner_previous corner in
    let out_edge = l.c_edge.(corner) and in_edge = l.c_edge.(previous) in
    if edge_stamps.(out_edge) <> stamp then begin
      edge_stamps.(out_edge) <- stamp;
      let neighbor = l.c_point.(corner_next corner) in
      if neighbor <> b && stamps.(neighbor) = stamp then begin
        incr common;
        if neighbor <> c && neighbor <> d then valid := false
      end
    end;
    if edge_stamps.(in_edge) <> stamp then begin
      edge_stamps.(in_edge) <- stamp;
      let neighbor = l.c_point.(previous) in
      if neighbor <> b && stamps.(neighbor) = stamp then begin
        incr common;
        if neighbor <> c && neighbor <> d then valid := false
      end
    end
  done;
  !valid && !common = (if c = d then 1 else 2)

(* Independent short edges whose collapse keeps every neighbouring triangle
   oriented; greedy from the shortest, ties by output edge index. *)
let select_short l ranking ~target_length ~hard ~feature =
  let constrained = constrained_points l ~hard ~feature in
  let features = if feature >= 0 then Some l.e_groups.(feature) else None in
  let bits = Array.make ranking.edge_count 0
  and keys = Array.make ranking.edge_count 0 in
  let count = ref 0 in
  for rank = 0 to ranking.edge_count - 1 do
    if rank land 4095 = 0 then Cancel.check_opt l.cancel;
    let edge = ranking.e_order.(rank) in
    let a = l.e_a.(edge) and b = l.e_b.(edge) in
    if l.e_incidence.(edge) = 2
        && (match features with
          | Some bytes -> Bytes.unsafe_get bytes edge = '\000' | None -> true)
        && Bytes.unsafe_get constrained a = '\000'
        && Bytes.unsafe_get constrained b = '\000' then begin
      let length = local_length l a b in
      if length < (4. /. 5.) *. local_target l target_length a b then begin
        bits.(!count) <- Int64.to_int (Int64.bits_of_float length);
        keys.(!count) <- rank;
        incr count
      end
    end
  done;
  sort_candidates bits keys !count;
  let chosen = Bytes.make l.edges '\000'
  and used_points = Bytes.make l.points '\000'
  and used_tris = Bytes.make l.tris '\000'
  and stamps = Array.make l.points (-1)
  and edge_stamps = Array.make l.edges (-1)
  and chosen_count = ref 0 in
  for stamp = 0 to !count - 1 do
    if stamp land 4095 = 0 then Cancel.check_opt l.cancel;
    let edge = ranking.e_order.(keys.(stamp)) in
    let a = l.e_a.(edge) and b = l.e_b.(edge) in
    let v0, v1 = sided_corners l ranking edge in
    let tri_a = v0 / 3 and tri_b = v1 / 3 in
    if Bytes.unsafe_get used_points a = '\000'
        && Bytes.unsafe_get used_points b = '\000'
        && Bytes.unsafe_get used_tris tri_a = '\000'
        && Bytes.unsafe_get used_tris tri_b = '\000'
        && link_condition l ranking stamps edge_stamps stamp v0 v1 a b then begin
      let mx = 0.5 *. (l.x.(a) +. l.x.(b))
      and my = 0.5 *. (l.y.(a) +. l.y.(b))
      and mz = 0.5 *. (l.z.(a) +. l.z.(b)) in
      if inspect_point l ranking used_tris tri_a tri_b a b mx my mz a
          && inspect_point l ranking used_tris tri_a tri_b a b mx my mz b then begin
        Bytes.unsafe_set chosen edge '\001'; incr chosen_count;
        Bytes.unsafe_set used_points a '\001';
        Bytes.unsafe_set used_points b '\001';
        reserve_point ranking used_tris a;
        reserve_point ranking used_tris b
      end
    end
  done;
  chosen, !chosen_count

(* Average of two coordinates as the fuse kernel computes it. *)
let[@inline] average_pair a b =
  let sum = a +. b in
  if Float.is_finite sum then sum /. 2.
  else begin
    let scale = Float.max (Float.abs a) (Float.abs b) in
    let normalized = if scale <> 0. then (a /. scale) +. (b /. scale) else 0. in
    (normalized /. 2.) *. scale
  end

(* Collapse each chosen edge into its lower point: the two side triangles die,
   the higher point's corners move over, its edges to the two opposite points
   merge into the lower point's, and its other edges are renamed. The link
   condition guarantees those are the only shared neighbours, so no edge is
   looked up. Merged targets union the groups of their sources, as the fuse
   kernel's edge-group remap does. *)
let collapse l chosen =
  for edge = 0 to l.edges - 1 do
    if edge land 4095 = 0 then Cancel.check_opt l.cancel;
    if Bytes.unsafe_get chosen edge <> '\000' then begin
      let a = l.e_a.(edge) and b = l.e_b.(edge) in
      l.x.(a) <- average_pair l.x.(a) l.x.(b);
      l.y.(a) <- average_pair l.y.(a) l.y.(b);
      l.z.(a) <- average_pair l.z.(a) l.z.(b);
      if not (Float.is_finite l.x.(a) && Float.is_finite l.y.(a) && Float.is_finite l.z.(a)) then
        invalid_arg (Printf.sprintf
          "%s: collapse position is non-finite at point %d" operation a);
      Array.iter (fun bytes ->
        if Bytes.unsafe_get bytes b <> '\000' then Bytes.unsafe_set bytes a '\001')
        l.p_groups;
      (* Side triangles and their edges to the opposite points. *)
      let v0 = l.e_first.(edge) in
      let v1 = l.c_next_in_edge.(v0) in
      let side corner =
        (* [corner] leaves the edge's first endpoint, [next] leaves the
           second towards the opposite point, [previous] returns from it. *)
        let next = corner_next corner and previous = corner_previous corner in
        let to_a, to_b = if l.c_point.(corner) = a
          then l.c_edge.(previous), l.c_edge.(next)
          else l.c_edge.(next), l.c_edge.(previous) in
        l.c_point.(corner_next next), to_a, to_b in
      let c, ac, bc = side v0 and d, ad, bd = side v1 in
      detach_triangle l (v0 / 3);
      detach_triangle l (v1 / 3);
      (* Remaining triangles at [b]. *)
      l.scratch_count <- 0;
      let corner = ref l.p_first.(b) in
      while !corner >= 0 do
        scratch_push l !corner;
        corner := l.c_next_at_point.(!corner)
      done;
      let merge source target =
        edge_revive l target;
        Array.iter (fun bytes ->
          if Bytes.unsafe_get bytes source <> '\000' then
            Bytes.unsafe_set bytes target '\001') l.e_groups;
        target in
      let retarget source other =
        if other = c then merge source ac
        else if other = d then merge source ad
        else begin
          l.e_a.(source) <- min a other; l.e_b.(source) <- max a other;
          source
        end in
      for index = 0 to l.scratch_count - 1 do
        let corner = l.scratch.(index) in
        let previous = corner_previous corner in
        let out = detach_edge_soft l corner
        and incoming = detach_edge_soft l previous in
        detach_point l corner;
        attach_point l corner a;
        attach_edge l corner (retarget out l.c_point.(corner_next corner));
        attach_edge l previous (retarget incoming l.c_point.(previous))
      done;
      if l.e_incidence.(bc) = 0 then edge_kill l bc;
      if l.e_incidence.(bd) = 0 then edge_kill l bd;
      Bytes.unsafe_set l.p_alive b '\000';
      l.p_first.(b) <- -1;
      l.p_left.(b) <- a
    end
  done;
(* Ordered point groups follow the fuse rule: visit the source order mapped
     to survivors, then the remaining members in point order. *)
  Array.iteri (fun index order -> match order with
    | None -> ()
    | Some order ->
        let bytes = l.p_groups.(index) in
        let seen = Bytes.make l.points '\000' in
        let output = Array.make (Array.length order) 0 and cursor = ref 0 in
        let visit point =
          if Bytes.unsafe_get seen point = '\000' then begin
            Bytes.unsafe_set seen point '\001';
            if Bytes.unsafe_get bytes point <> '\000' then begin
              output.(!cursor) <- point; incr cursor end
          end in
        Array.iter (fun point ->
          let point = if Bytes.unsafe_get l.p_alive point <> '\000' then point
            else l.p_left.(point) in
          visit point) order;
        let rest = ref [] in
        for point = l.points - 1 downto 0 do
          if Bytes.unsafe_get l.p_alive point <> '\000'
              && Bytes.unsafe_get seen point = '\000'
              && Bytes.unsafe_get bytes point <> '\000' then rest := point :: !rest
        done;
        let result = Array.append (Array.sub output 0 !cursor)
            (Array.of_list !rest) in
        l.p_orders.(index) <- Some result) l.p_orders

let same_orientation_local l a0 b0 c0 a1 b1 c1 =
  let abx = l.x.(b0) -. l.x.(a0) and aby = l.y.(b0) -. l.y.(a0)
  and abz = l.z.(b0) -. l.z.(a0) and acx = l.x.(c0) -. l.x.(a0)
  and acy = l.y.(c0) -. l.y.(a0) and acz = l.z.(c0) -. l.z.(a0) in
  let ox = aby *. acz -. abz *. acy and oy = abz *. acx -. abx *. acz
  and oz = abx *. acy -. aby *. acx in
  let abx = l.x.(b1) -. l.x.(a1) and aby = l.y.(b1) -. l.y.(a1)
  and abz = l.z.(b1) -. l.z.(a1) and acx = l.x.(c1) -. l.x.(a1)
  and acy = l.y.(c1) -. l.y.(a1) and acz = l.z.(c1) -. l.z.(a1) in
  let nx = aby *. acz -. abz *. acy and ny = abz *. acx -. abx *. acz
  and nz = abx *. acy -. aby *. acx in
  let old2 = ox *. ox +. oy *. oy +. oz *. oz
  and new2 = nx *. nx +. ny *. ny +. nz *. nz in
  old2 > 0. && new2 > 0.
  && ox *. nx +. oy *. ny +. oz *. nz > 1e-14 *. sqrt (old2 *. new2)

(* Are [a] and [b] joined by an edge? Checked along [a]'s live corners. *)
let adjacent ranking l a b =
  let found = ref false in
  for slot = ranking.point_offsets.(a) to ranking.point_offsets.(a + 1) - 1 do
    let corner = ranking.point_corners.(slot) in
    if l.c_point.(corner_next corner) = b || l.c_point.(corner_previous corner) = b
    then found := true
  done;
  !found

(* Independent two-sided edges whose flip lowers the valence penalty and keeps
   both triangles oriented; greedy in output edge order. *)
let select_flip l ranking ~hard ~feature =
  let constrained = constrained_points l ~hard ~feature in
  let features = if feature >= 0 then Some l.e_groups.(feature) else None in
  let valence = Array.make l.points 0 and boundary = Bytes.make l.points '\000' in
  for edge = 0 to l.edges - 1 do
    if edge_live l edge then begin
      let a = l.e_a.(edge) and b = l.e_b.(edge) in
      valence.(a) <- valence.(a) + 1;
      if b <> a then valence.(b) <- valence.(b) + 1;
      if l.e_incidence.(edge) <> 2 then begin
        Bytes.unsafe_set boundary a '\001'; Bytes.unsafe_set boundary b '\001'
      end
    end
  done;
  let ideal point = if Bytes.unsafe_get boundary point <> '\000' then 4 else 6 in
  let penalty point degree = abs (degree - ideal point) in
  let eligible = Bytes.make l.edges '\000' in
  for rank = 0 to ranking.edge_count - 1 do
    if rank land 4095 = 0 then Cancel.check_opt l.cancel;
    let edge = ranking.e_order.(rank) in
    if l.e_incidence.(edge) = 2
        && not (match features with
          | Some bytes -> Bytes.unsafe_get bytes edge <> '\000' | None -> false)
    then begin
      let va, vb = sided_corners l ranking edge in
      if va / 3 <> vb / 3 then begin
        let a = l.e_a.(edge) and b = l.e_b.(edge)
        and c = opposite_point l va and d = opposite_point l vb in
        if a <> b && a <> c && a <> d && b <> c && b <> d && c <> d
            && Bytes.unsafe_get constrained a = '\000'
            && Bytes.unsafe_get constrained b = '\000'
            && not (adjacent ranking l c d) then begin
          let before = penalty a valence.(a) + penalty b valence.(b)
              + penalty c valence.(c) + penalty d valence.(d)
          and after = penalty a (valence.(a) - 1) + penalty b (valence.(b) - 1)
              + penalty c (valence.(c) + 1) + penalty d (valence.(d) + 1) in
          if after < before
              && same_orientation_local l a b c d c b
              && same_orientation_local l b a d c d a
          then Bytes.unsafe_set eligible edge '\001'
        end
      end
    end
  done;
  let chosen = Bytes.make l.edges '\000' and used_tris = Bytes.make l.tris '\000'
  and count = ref 0 in
  for rank = 0 to ranking.edge_count - 1 do
    let edge = ranking.e_order.(rank) in
    if Bytes.unsafe_get eligible edge <> '\000' then begin
      let va, vb = sided_corners l ranking edge in
      let tri_a = va / 3 and tri_b = vb / 3 in
      if Bytes.unsafe_get used_tris tri_a = '\000'
          && Bytes.unsafe_get used_tris tri_b = '\000' then begin
        Bytes.unsafe_set chosen edge '\001'; incr count;
        Bytes.unsafe_set used_tris tri_a '\001';
        Bytes.unsafe_set used_tris tri_b '\001'
      end
    end
  done;
  chosen, !count

(* Rotate each chosen edge: triangle A (a0, b0, c) becomes (d, c, a0) and
   triangle B (b0, a0, d) becomes (c, d, b0) in their own corner slots, corner
   payload rotating one slot with them; the new diagonal inherits the edge's
   groups. *)
let flip l ranking chosen =
  let saved = Array.make (Array.length l.e_groups) '\000' in
  (* Corner payload moves one slot back; ordered vertex groups follow it. *)
  let slot_map = if Array.exists Option.is_some l.v_orders
    then Array.init (l.tris * 3) Fun.id else [||] in
  let rotate_slots tri slot0 point0 point1 point2 =
    let first = tri * 3 in
    let slot local = first + ((slot0 - first + local) mod 3) in
    let left = Array.init 3 (fun local -> l.c_left.(slot local))
    and right = Array.init 3 (fun local -> l.c_right.(slot local))
    and weight = Array.init 3 (fun local -> l.c_weight.(slot local)) in
    for local = 0 to 2 do
      let corner = slot local and source = (local + 1) mod 3 in
      l.c_left.(corner) <- left.(source);
      l.c_right.(corner) <- right.(source);
      l.c_weight.(corner) <- weight.(source)
    done;
    attach_point l (slot 0) point0;
    attach_point l (slot 1) point1;
    attach_point l (slot 2) point2 in
  for rank = 0 to ranking.edge_count - 1 do
    let edge = ranking.e_order.(rank) in
    if Bytes.unsafe_get chosen edge <> '\000' then begin
      let va, vb = sided_corners l ranking edge in
      let tri_a = va / 3 and tri_b = vb / 3 in
      let a0 = l.c_point.(va) and b0 = l.c_point.(corner_next va)
      and c = opposite_point l va and d = opposite_point l vb in
      Array.iteri (fun index bytes -> saved.(index) <- Bytes.unsafe_get bytes edge)
        l.e_groups;
      let ac = l.c_edge.(corner_next (corner_next va))
      and bc = l.c_edge.(corner_next va)
      and bd = l.c_edge.(corner_next (corner_next vb))
      and ad = l.c_edge.(corner_next vb) in
      let first_a = tri_a * 3 and first_b = tri_b * 3 in
      for corner = first_a to first_a + 2 do ignore (detach_edge_soft l corner) done;
      for corner = first_b to first_b + 2 do ignore (detach_edge_soft l corner) done;
      for corner = first_a to first_a + 2 do detach_point l corner done;
      for corner = first_b to first_b + 2 do detach_point l corner done;
      edge_kill l edge;
      (* An earlier flip in this pass may already have made (c, d): the
         shared edge then carries this flip's groups, as the last remap wins. *)
      let diagonal = match adjacent_edge l c d with
        | -1 -> edge_add l (min c d) (max c d)
        | existing -> existing in
      rotate_slots tri_a va d c a0;
      rotate_slots tri_b vb c d b0;
      (* A is now (d, c, a0): edges (d,c) (c,a0) (a0,d); B is (c, d, b0):
         edges (c,d) (d,b0) (b0,c). *)
      let slot_a local = first_a + ((va - first_a + local) mod 3)
      and slot_b local = first_b + ((vb - first_b + local) mod 3) in
      attach_edge l (slot_a 0) diagonal; attach_edge l (slot_a 1) ac;
      attach_edge l (slot_a 2) ad;
      attach_edge l (slot_b 0) diagonal; attach_edge l (slot_b 1) bd;
      attach_edge l (slot_b 2) bc;
      Array.iteri (fun index bytes -> Bytes.unsafe_set bytes diagonal saved.(index))
        l.e_groups;
      if slot_map <> [||] then
        for local = 0 to 2 do
          slot_map.(first_a + ((va - first_a + local + 1) mod 3)) <-
            first_a + ((va - first_a + local) mod 3);
          slot_map.(first_b + ((vb - first_b + local + 1) mod 3)) <-
            first_b + ((vb - first_b + local) mod 3)
        done
    end
  done;
  Array.iteri (fun index order -> match order with
    | None -> ()
    | Some order -> l.v_orders.(index) <- Some (Array.map (fun corner ->
        slot_map.(corner)) order)) l.v_orders

(* One Geometry per iteration: compact live points (dropping points that lost
   every triangle), emit triangles in output order, interpolate payload once
   through ancestry, and rebuild groups and edge groups. *)
let materialize l =
  let ranking = rank ~incidence:false l in
  let used_after = Bytes.make l.points '\000' in
  for rank = 0 to ranking.tri_count - 1 do
    let first = ranking.order.(rank) * 3 in
    Bytes.unsafe_set used_after l.c_point.(first) '\001';
    Bytes.unsafe_set used_after l.c_point.(first + 1) '\001';
    Bytes.unsafe_set used_after l.c_point.(first + 2) '\001'
  done;
  let p_map = Array.make l.points (-1) and point_count = ref 0 in
  for point = 0 to l.points - 1 do
    if point land 16383 = 0 then Cancel.check_opt l.cancel;
    if Bytes.unsafe_get l.p_alive point <> '\000'
        && (Bytes.unsafe_get used_after point <> '\000'
            || Bytes.unsafe_get l.p_used point = '\000') then begin
      p_map.(point) <- !point_count; incr point_count
    end
  done;
  let point_count = !point_count in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0.
  and point_left = Array.make point_count 0 and point_right = Array.make point_count 0
  and point_weight = Array.make point_count 0. in
  for point = 0 to l.points - 1 do
    let target = p_map.(point) in
    if target >= 0 then begin
      x.(target) <- l.x.(point); y.(target) <- l.y.(point); z.(target) <- l.z.(point);
      point_left.(target) <- l.p_left.(point);
      point_right.(target) <- l.p_right.(point);
      point_weight.(target) <- l.p_weight.(point)
    end
  done;
  let corner_count = ranking.tri_count * 3 in
  let vertex_points = Array.make corner_count 0
  and vertex_left = Array.make corner_count 0
  and vertex_right = Array.make corner_count 0
  and vertex_weight = Array.make corner_count 0.
  and primitive_source = Array.make ranking.tri_count 0 in
  for rank = 0 to ranking.tri_count - 1 do
    if rank land 4095 = 0 then Cancel.check_opt l.cancel;
    let tri = ranking.order.(rank) in
    primitive_source.(rank) <- l.t_source.(tri);
    for local = 0 to 2 do
      let corner = tri * 3 + local and target = rank * 3 + local in
      let point = p_map.(l.c_point.(corner)) in
      if point < 0 then invalid_arg (operation ^ ": output references a dropped point");
      vertex_points.(target) <- point;
      vertex_left.(target) <- l.c_left.(corner);
      vertex_right.(target) <- l.c_right.(corner);
      vertex_weight.(target) <- l.c_weight.(corner)
    done
  done;
  let topology = Topology.View.create_validated_owned ~point_count
      ~vertex_points
      ~primitive_offsets:(Array.init (ranking.tri_count + 1) (fun rank -> rank * 3))
      ~primitive_kinds:(Bytes.make ranking.tri_count '\000') in
  let cancel = l.cancel and grain = l.grain in
  let attributes = Geometry.attributes l.start |> List.filter_map (fun attribute ->
    if String.equal (Attribute.name attribute) "N"
        && (Attribute.owner attribute = Attribute.Point
            || Attribute.owner attribute = Attribute.Vertex) then None
    else Some (Subdivide.interpolate_owned_attribute ?cancel ~grain
      ~point_left ~point_right ~point_weight ~vertex_left ~vertex_right
      ~vertex_weight ~primitive_source attribute)) in
  let corner_rank = lazy (
    let corner_rank = Array.make (l.tris * 3) (-1) in
    for rank = 0 to ranking.tri_count - 1 do
      let first = ranking.order.(rank) * 3 in
      for local = 0 to 2 do corner_rank.(first + local) <- rank * 3 + local done
    done;
    corner_rank) in
  (* Groups keep the source list order; point and vertex groups take their
     tracked membership and order, primitive groups follow ancestry. *)
  let next_point = ref 0 and next_vertex = ref 0 in
  let groups = Geometry.groups l.start |> List.map (fun group ->
    match Group.owner group with
    | Group.Point ->
        let index = !next_point in
        incr next_point;
        let bytes = l.p_groups.(index) in
        let builder = Group.Builder.create ~owner:Group.Point
            ~name:(Group.name group) point_count in
        for point = 0 to l.points - 1 do
          let target = p_map.(point) in
          if target >= 0 && Bytes.unsafe_get bytes point <> '\000' then
            Group.Builder.set builder target true
        done;
        let target = Group.Builder.freeze builder in
        (match l.p_orders.(index) with
         | None -> target
         | Some order ->
             let mapped = Array.to_list order |> List.filter_map (fun point ->
               let target = p_map.(point) in
               if target >= 0 then Some target else None) |> Array.of_list in
             Group.View.with_owned_order mapped target)
    | Group.Vertex ->
        let index = !next_vertex in
        incr next_vertex;
        let target = Group.init ~grain ~owner:Group.Vertex ~name:(Group.name group)
            corner_count (fun corner ->
              Group.mem vertex_left.(corner) group
              && Group.mem vertex_right.(corner) group) in
        (match l.v_orders.(index) with
         | None -> target
         | Some order ->
             let corner_rank = Lazy.force corner_rank in
             let mapped = Array.to_list order |> List.filter_map (fun corner ->
               let target = corner_rank.(corner) in
               if target >= 0 then Some target else None) |> Array.of_list in
             Group.View.with_owned_order mapped target)
    | Group.Primitive ->
        let target = Group.init ~grain ~owner:Group.Primitive
            ~name:(Group.name group) ranking.tri_count (fun rank ->
              Group.mem primitive_source.(rank) group) in
        if not (Group.is_ordered group) then target
        else Group.View.remap_order ~source:group
          ~source_of_target:primitive_source target) in
  let edge_groups = Array.to_list (Array.mapi (fun index name ->
    let source = l.e_groups.(index) in
    let bits = Bytes.make ((ranking.edge_count + 7) / 8) '\000' in
    for rank = 0 to ranking.edge_count - 1 do
      if Bytes.unsafe_get source ranking.e_order.(rank) <> '\000' then begin
        let slot = rank lsr 3 and mask = 1 lsl (rank land 7) in
        Bytes.unsafe_set bits slot
          (Char.chr (Char.code (Bytes.unsafe_get bits slot) lor mask))
      end
    done;
    Edge_group.View.of_owned_bits ~topology ~edge_count:ranking.edge_count
      ~name bits) l.edge_group_names) in
  Result.map (fun geometry -> geometry, p_map)
    (Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
      ~topology ~attributes ~groups ~edge_groups ())

(* Smoothing constraints over the materialized points. *)
let constraint_group l point_map ~hard ~feature ~name count =
  let bytes = constrained_points l ~hard ~feature in
  let out = Bytes.make count '\000' in
  for point = 0 to l.points - 1 do
    let target = point_map.(point) in
    if target >= 0 && Bytes.unsafe_get bytes point <> '\000' then
      Bytes.unsafe_set out target '\001'
  done;
  Group.init ~grain:l.grain ~owner:Group.Point ~name count (fun point ->
    Bytes.unsafe_get out point <> '\000')

let projection_scratch scratch count = match !scratch with
  | Some value when value.count = count -> value
  | None | Some _ ->
      let value = {
        count;
        primitives = Array.make count (-1);
        triangles = Array.make count (-1);
        barycentric_a = Array.make count 0.;
        barycentric_b = Array.make count 0.;
        barycentric_c = Array.make count 0.;
        distances_squared = Array.make count infinity;
      } in
      scratch := Some value;
      value

let project_to_reference ?cancel ~grain ~surface ~scratch reference geometry =
    let count = Geometry.point_count geometry in
    let topology = Geometry.topology geometry in
    let index = Topology_index.create ?cancel topology in
    let used = Group.init ~grain ~owner:Group.Point ~name:"__rdk_remesh_used"
        count (fun point -> Topology_index.point_incidence_count index point > 0) in
    let scratch = projection_scratch scratch count in
    let primitives = scratch.primitives and triangles = scratch.triangles
    and a = scratch.barycentric_a and b = scratch.barycentric_b
    and c = scratch.barycentric_c and distances = scratch.distances_squared in
    Surface_index.View.closest_many_into ?cancel ~selection:used ~grain surface
      ~queries:(Geometry.positions geometry) ~max_distance_squared:infinity
      ~primitives ~triangles ~barycentric_a:a ~barycentric_b:b
      ~barycentric_c:c ~distances_squared:distances;
    let missed = ref (-1) in
    Group.iter (fun point -> if triangles.(point) < 0 && !missed < 0 then
      missed := point) used;
    if !missed >= 0 then fail (Printf.sprintf
        "could not project point %d to the input surface" !missed)
    else
      let source_positions = Packed.Float3.View.view (Geometry.positions reference)
      and source_topology = Topology.View.view (Geometry.topology reference)
      and current = Packed.Float3.View.view (Geometry.positions geometry) in
      let x = Array.copy current.x and y = Array.copy current.y
      and z = Array.copy current.z in
      if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
          (fun point ->
        if point land 4095 = 0 then Cancel.check_opt cancel;
        if Group.mem point used then begin
          let triangle = triangles.(point) in
          let vertex0 = Surface_index.View.triangle_vertex surface triangle 0
          and vertex1 = Surface_index.View.triangle_vertex surface triangle 1
          and vertex2 = Surface_index.View.triangle_vertex surface triangle 2 in
          let p0 = source_topology.vertex_points.(vertex0)
          and p1 = source_topology.vertex_points.(vertex1)
          and p2 = source_topology.vertex_points.(vertex2) in
          x.(point) <- a.(point) *. source_positions.x.(p0)
            +. b.(point) *. source_positions.x.(p1)
            +. c.(point) *. source_positions.x.(p2);
          y.(point) <- a.(point) *. source_positions.y.(p0)
            +. b.(point) *. source_positions.y.(p1)
            +. c.(point) *. source_positions.y.(p2);
          z.(point) <- a.(point) *. source_positions.z.(p0)
            +. b.(point) *. source_positions.z.(p1)
            +. c.(point) *. source_positions.z.(p2)
        end);
      Geometry.with_positions
        (Packed.Float3.View.of_owned_exn ~x ~y ~z) geometry

let install_outputs ?cancel ~grain ~target_length ~target_size_attribute
    ~edge_name ~output_hard_edges ~output_mesh_size ~output_quality geometry =
  let topology_value = Geometry.topology geometry in
  let topology = Topology.View.view topology_value in
  Result.bind (feature_group edge_name geometry) (fun features ->
  let with_hard_edges = match output_hard_edges with
    | None -> Ok geometry
    | Some name ->
        let index_value = Topology_index.create ?cancel topology_value in
        let index = Topology_index.View.view index_value in
        let group = Edge_group.init ~grain ~topology:topology_value
            ~index:index_value ~name (fun edge ->
          Edge_group.mem edge features
          || index.edge_offsets.(edge + 1) - index.edge_offsets.(edge) <> 2) in
        Geometry.with_edge_group group geometry in
  Result.bind with_hard_edges (fun geometry ->
  let with_size = match output_mesh_size with
    | None -> Ok geometry
    | Some name ->
        (match target_size_attribute with
         | Some source ->
             (match Geometry.find_attribute ~owner:Attribute.Point source geometry with
              | Some attribute -> Result.bind (Attribute.with_name name attribute)
                  (fun attribute -> Geometry.with_attribute attribute geometry)
              | None -> fail "internal target-size attribute ancestry was lost")
         | None ->
             let values = Array.make (Geometry.point_count geometry) target_length in
             Result.bind (Attribute.create_owned ~owner:Attribute.Point ~name
                 (Attribute.Float values)) (fun attribute ->
               Geometry.with_attribute attribute geometry)) in
  Result.bind with_size (fun geometry -> match output_quality with
    | None -> Ok geometry
    | Some name ->
        let positions = Packed.Float3.View.view (Geometry.positions geometry) in
        let primitive_count = Geometry.primitive_count geometry in
        let quality = Parallel.init_array ~grain primitive_count (fun primitive ->
          if primitive land 4095 = 0 then Cancel.check_opt cancel;
          if not (primitive_is_triangle topology primitive) then 0.
          else
            let first = topology.primitive_offsets.(primitive) in
            let p0 = topology.vertex_points.(first)
            and p1 = topology.vertex_points.(first + 1)
            and p2 = topology.vertex_points.(first + 2) in
            let dx01 = positions.x.(p1) -. positions.x.(p0)
            and dy01 = positions.y.(p1) -. positions.y.(p0)
            and dz01 = positions.z.(p1) -. positions.z.(p0)
            and dx12 = positions.x.(p2) -. positions.x.(p1)
            and dy12 = positions.y.(p2) -. positions.y.(p1)
            and dz12 = positions.z.(p2) -. positions.z.(p1)
            and dx20 = positions.x.(p0) -. positions.x.(p2)
            and dy20 = positions.y.(p0) -. positions.y.(p2)
            and dz20 = positions.z.(p0) -. positions.z.(p2) in
            let scale = Float.max (Float.max (Float.abs dx01) (Float.abs dy01))
                (Float.max (Float.abs dz01) (Float.max (Float.abs dx12)
                  (Float.max (Float.abs dy12) (Float.max (Float.abs dz12)
                    (Float.max (Float.abs dx20)
                      (Float.max (Float.abs dy20) (Float.abs dz20))))))) in
            if scale = 0. then 0.
            else
              let ax = dx01 /. scale and ay = dy01 /. scale
              and az = dz01 /. scale and bx = -.dx20 /. scale
              and by = -.dy20 /. scale and bz = -.dz20 /. scale in
              let cx = ay *. bz -. az *. by
              and cy = az *. bx -. ax *. bz
              and cz = ax *. by -. ay *. bx in
              let area2 = sqrt (cx *. cx +. cy *. cy +. cz *. cz)
              and lengths2 =
                (dx01 /. scale) ** 2. +. (dy01 /. scale) ** 2.
                +. (dz01 /. scale) ** 2. +. (dx12 /. scale) ** 2.
                +. (dy12 /. scale) ** 2. +. (dz12 /. scale) ** 2.
                +. (dx20 /. scale) ** 2. +. (dy20 /. scale) ** 2.
                +. (dz20 /. scale) ** 2. in
              if lengths2 = 0. then 0.
              else Float.min 1. (2. *. sqrt 3. *. area2 /. lengths2)) in
        Result.bind (Attribute.create_owned ~owner:Attribute.Primitive ~name
            (Attribute.Float quality)) (fun attribute ->
          Geometry.with_attribute attribute geometry))))

let run ?cancel ?(grain = 16_384) ?(iterations = 3) ?(smoothing = 0.5)
    ?(project = true) ?(use_input_points_only = false) ?hard_points ?hard_edges
    ?target_size_attribute ?(preserve_uv_seams = true) ?(uv_attribute = "uv")
    ?output_hard_edges ?output_mesh_size ?output_quality
    ?(recompute_point_normals = true) ~target_length geometry =
  Error.guard ~operation:"remesh" ~code:"invalid_remesh" @@ fun () ->
  try
    if grain <= 0 then fail "grain must be positive"
    else if iterations < 0 then fail "iterations must be non-negative"
    else if not (Float.is_finite target_length) || target_length <= 0. then
      fail "target length must be finite and positive"
    else if not (Float.is_finite smoothing) || smoothing < 0. || smoothing > 1. then
      fail "smoothing must be finite and in [0, 1]"
    else Result.bind (validate_name "output hard-edge group" output_hard_edges)
      (fun () -> Result.bind (validate_name "output mesh-size attribute"
        output_mesh_size) (fun () -> Result.bind
      (validate_name "output quality attribute" output_quality) (fun () ->
    Result.bind (validate_point_group geometry hard_points) (fun () ->
    Result.bind (validate_edge_group geometry hard_edges) (fun () ->
    Result.bind (validate_positions ?cancel geometry) (fun () ->
    Result.bind (target_values ?cancel target_size_attribute geometry) (fun _ ->
      let point_name = fresh_group_name geometry Group.Point
          "__rdk_remesh_hard_points" None
      and edge_name = fresh_edge_group_name geometry "__rdk_remesh_features"
          output_hard_edges in
      Result.bind (install_feature_groups ?cancel ~grain ~hard_points ~hard_edges
          ~preserve_uv_seams ~uv_attribute ~point_name ~edge_name geometry)
        (fun prepared ->
      Result.bind (Error.unguard (Triangulate.run ?cancel ~grain prepared)) (fun initial ->
      let current = ref initial in
      let failure = ref None and iteration = ref 0 in
      let projection_scratch = ref None in
      let projection_surface = if project && iterations > 0 then
          match Surface_index.create ?cancel ~grain geometry with
          | Ok surface -> Some surface
          | Error error when Error.code error = "cancelled" ->
              raise Cancel.Cancelled
          | Error error ->
              failure := Some (Error.to_string error); None
        else None in
      (* A structure whose iteration changed nothing topologically is kept
         for the next one: smoothing and projection only move points. *)
      let cached = ref None in
      while !iteration < iterations && !failure = None do
        Cancel.check_opt cancel;
        (* One local structure per iteration: split, collapse and flip edit it
           in place and the result is materialized once. *)
        let l, cached_ranking = match !cached with
          | Some (l, ranking) ->
              let positions = Packed.Float3.View.view
                  (Geometry.positions !current) in
              Array.blit positions.x 0 l.x 0 l.points;
              Array.blit positions.y 0 l.y 0 l.points;
              Array.blit positions.z 0 l.z 0 l.points;
              l.start <- !current;
              l, Some ranking
          | None ->
              local_of_geometry ?cancel ~grain ~target_size_attribute !current,
              None in
        cached := None;
        let hard = point_group_index l point_name
        and feature = edge_group_index l edge_name in
        if hard < 0 then
          failure := Some (operation ^ ": internal hard-point ancestry was lost")
        else if feature < 0 then
          failure := Some (operation ^ ": internal feature-edge ancestry was lost")
        else begin
          let changed = ref false in
          if not use_input_points_only then begin
            let selected, count = select_long l ~target_length in
            if count > 0 then begin split l selected count; changed := true end;
            let ranking = rank l in
            let chosen, count = select_short l ranking ~target_length ~hard ~feature in
            if count > 0 then begin collapse l chosen; changed := true end
          end;
          let ranking = match cached_ranking with
            | Some ranking when not !changed -> ranking
            | Some _ | None -> rank l in
          let chosen, count = select_flip l ranking ~hard ~feature in
          if count > 0 then begin flip l ranking chosen; changed := true end
          else if not !changed then cached := Some (l, ranking);
          let point_map = if not !changed then Some (Array.init l.points Fun.id)
            else match materialize l with
              | Error message -> failure := Some message; None
              | Ok (geometry, point_map) -> current := geometry; Some point_map in
          (match point_map with
           | Some point_map when smoothing > 0. ->
               let constrained = constraint_group l point_map ~hard ~feature
                   ~name:"__rdk_remesh_constraints"
                   (Geometry.point_count !current) in
               (match Smooth.run ?cancel ~grain ~constrained_points:constrained
                   ~boundary:Smooth.Smooth_unshared ~iterations:1
                   ~mode:(Attribute_ops.Laplacian smoothing)
                   ~recompute_normals:false ~attributes:"P" !current with
                | Error error when Error.code error = "cancelled" ->
                    raise Cancel.Cancelled
                | Error error -> failure := Some (Error.to_string error)
                | Ok smoothed -> current := smoothed)
           | Some _ | None -> ());
          if !failure = None && project then begin
            match project_to_reference ?cancel ~grain
                ~surface:(Option.get projection_surface)
                ~scratch:projection_scratch geometry !current with
            | Error message -> failure := Some message
            | Ok projected -> current := projected
          end
        end;
        incr iteration
      done;
      match !failure with
      | Some message -> Error message
      | None ->
          let output = !current
              |> Geometry.without_attribute ~owner:Attribute.Point "N"
              |> Geometry.without_attribute ~owner:Attribute.Vertex "N" in
          let with_normals = if not recompute_point_normals then Ok output
            else Deform.normals ?cancel ~grain output in
          Result.bind with_normals (fun output ->
          Result.bind (install_outputs ?cancel ~grain ~target_length
              ~target_size_attribute ~edge_name ~output_hard_edges
              ~output_mesh_size ~output_quality output) (fun output ->
            Ok (output
              |> Geometry.without_group ~owner:Group.Point point_name
              |> Geometry.without_edge_group edge_name))))))))))))
  with
  | Invalid_argument message -> fail message
