open Rays_math

type mode = Separate_pieces_separate | Separate_pieces_move_back

exception Separate_pieces_error of string
let fail message = raise (Separate_pieces_error message)

module Int_table = Hashtbl.Make (struct
  type t = int
  let equal (left : int) right = left = right
  let hash value = value land max_int
end)

module Text_table = Hashtbl.Make (struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

let block_count length grain =
  if length = 0 then 0 else 1 + ((length - 1) / grain)

let block_bounds length grain block =
  let first = block * grain in
  first, min length (first + grain)

let validate_name label name =
  if String.trim name = "" then fail (label ^ " must not be empty")

let normalized_axis axis =
  if not (Float.is_finite axis.Rays_math.Vec3.x && Float.is_finite axis.y && Float.is_finite axis.z) then
    fail "packing axis must be finite";
  let scale = max (abs_float axis.x)
      (max (abs_float axis.y) (abs_float axis.z)) in
  if scale = 0. then fail "packing axis must be non-zero";
  let x = axis.x /. scale and y = axis.y /. scale
  and z = axis.z /. scale in
  let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
  Rays_math.Vec3.create (x /. length) (y /. length) (z /. length)

let compile_piece_ids ?cancel owner name geometry =
  let attribute = match Geometry.find_attribute ~owner name geometry with
    | Some attribute -> attribute
    | None -> fail (Printf.sprintf "%s piece attribute %S was not found"
        (match owner with Attribute.Point -> "point"
          | Attribute.Primitive -> "primitive"
          | Attribute.Vertex -> "vertex" | Attribute.Detail -> "detail") name) in
  let assign_int values =
    let table = Int_table.create (min (Array.length values) 65_536) in
    let next = ref 0 in
    let ids = Array.init (Array.length values) (fun index ->
      if index land 4095 = 0 then Cancel.check_opt cancel;
      let value = Array.unsafe_get values index in
      match Int_table.find_opt table value with
      | Some id -> id
      | None ->
          let id = !next in
          incr next;
          Int_table.add table value id;
          id) in
    ids, !next in
  let assign_text values =
    let table = Text_table.create (min (Array.length values) 65_536) in
    let next = ref 0 in
    let ids = Array.init (Array.length values) (fun index ->
      if index land 4095 = 0 then Cancel.check_opt cancel;
      let value = Array.unsafe_get values index in
      match Text_table.find_opt table value with
      | Some id -> id
      | None ->
          let id = !next in
          incr next;
          Text_table.add table value id;
          id) in
    ids, !next in
  match Attribute.Private.storage attribute with
  | Attribute.Int values -> assign_int values
  | Attribute.Text values -> assign_text values
  | _ -> fail (Printf.sprintf
      "piece attribute %S must use integer or text storage, not %s"
      name (Attribute.kind_name attribute))

let first_recorded errors =
  match Array.find_opt (fun value -> value >= 0) errors with
  | None -> None
  | Some value -> Some value

let validate_point_piece_primitives ?cancel ~grain point_piece geometry =
  let topology = Topology.Private.view (Geometry.topology geometry) in
  let primitive_count = Geometry.primitive_count geometry in
  let blocks = block_count primitive_count (max 1 (grain / 8)) in
  let errors = Array.make blocks (-1) in
  if blocks > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(blocks - 1)
      (fun block ->
    Cancel.check_opt cancel;
    let first, last = block_bounds primitive_count (max 1 (grain / 8)) block in
    let first_error = ref (-1) in
    for primitive = first to last - 1 do
      if primitive land 1023 = 0 then Cancel.check_opt cancel;
      let vertex_first = Array.unsafe_get topology.primitive_offsets primitive
      and vertex_last = Array.unsafe_get topology.primitive_offsets
          (primitive + 1) in
      if vertex_first < vertex_last then begin
        let point = Array.unsafe_get topology.vertex_points vertex_first in
        let piece = Array.unsafe_get point_piece point in
        let vertex = ref (vertex_first + 1) in
        while !first_error < 0 && !vertex < vertex_last do
          let point = Array.unsafe_get topology.vertex_points !vertex in
          if Array.unsafe_get point_piece point <> piece then
            first_error := primitive;
          incr vertex
        done
      end
    done;
    Array.unsafe_set errors block !first_error);
  match first_recorded errors with
  | None -> ()
  | Some primitive -> fail (Printf.sprintf
      "primitive %d references points from different pieces" primitive)

let primitive_pieces_to_points ?cancel ~grain primitive_piece geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create ?cancel topology in
  let index = Topology_index.Private.view index in
  let point_count = Geometry.point_count geometry in
  let point_piece = Array.make point_count (-1) in
  let blocks = block_count point_count grain in
  let errors = Array.make blocks (-1) in
  if blocks > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(blocks - 1)
      (fun block ->
    Cancel.check_opt cancel;
    let first, last = block_bounds point_count grain block in
    let first_error = ref (-1) in
    for point = first to last - 1 do
      if point land 4095 = 0 then Cancel.check_opt cancel;
      let corner_first = Array.unsafe_get index.point_offsets point
      and corner_last = Array.unsafe_get index.point_offsets (point + 1) in
      if corner_first < corner_last then begin
        let vertex = Array.unsafe_get index.point_vertices corner_first in
        let piece = Array.unsafe_get primitive_piece
            (Array.unsafe_get index.primitive_of_vertex vertex) in
        let corner = ref (corner_first + 1) in
        while !first_error < 0 && !corner < corner_last do
          let vertex = Array.unsafe_get index.point_vertices !corner in
          let other = Array.unsafe_get primitive_piece
              (Array.unsafe_get index.primitive_of_vertex vertex) in
          if other <> piece then first_error := point;
          incr corner
        done;
        Array.unsafe_set point_piece point piece
      end
    done;
    Array.unsafe_set errors block !first_error);
  (match first_recorded errors with
   | None -> point_piece
   | Some point -> fail (Printf.sprintf
       "point %d is shared by primitives from different pieces" point))

let validate_positions_and_project ?cancel ~grain axis point_piece geometry =
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let point_count = Geometry.point_count geometry in
  let projection = Array.make point_count 0. in
  let blocks = block_count point_count grain in
  let errors = Array.make blocks (-1) in
  if blocks > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(blocks - 1)
      (fun block ->
    Cancel.check_opt cancel;
    let first, last = block_bounds point_count grain block in
    let first_error = ref (-1) in
    for point = first to last - 1 do
      if point land 4095 = 0 then Cancel.check_opt cancel;
      if Array.unsafe_get point_piece point >= 0 then begin
        let x = Array.unsafe_get positions.x point
        and y = Array.unsafe_get positions.y point
        and z = Array.unsafe_get positions.z point in
        let value = (x *. axis.Rays_math.Vec3.x) +. (y *. axis.y) +. (z *. axis.z) in
        if !first_error < 0
            && not (Float.is_finite x && Float.is_finite y && Float.is_finite z && Float.is_finite value) then
          first_error := point;
        Array.unsafe_set projection point value
      end
    done;
    Array.unsafe_set errors block !first_error);
  (match first_recorded errors with
   | None -> projection
   | Some point -> fail (Printf.sprintf
       "operated position or axis projection is non-finite at point %d" point))

let piece_bounds ?cancel ~piece_count point_piece projection =
  let point_count = Array.length point_piece in
  let minimum = Array.make piece_count Float.infinity
  and maximum = Array.make piece_count Float.neg_infinity in
  for point = 0 to point_count - 1 do
    if point land 4095 = 0 then Cancel.check_opt cancel;
    let piece = Array.unsafe_get point_piece point in
    if piece >= 0 then begin
      let value = Array.unsafe_get projection point in
      if value < Array.unsafe_get minimum piece then
        Array.unsafe_set minimum piece value;
      if value > Array.unsafe_get maximum piece then
        Array.unsafe_set maximum piece value
    end
  done;
  minimum, maximum

let piece_translations ?cancel ~axis ~gap minimum maximum =
  let piece_count = Array.length minimum in
  let x = Array.make piece_count 0. and y = Array.make piece_count 0.
  and z = Array.make piece_count 0. in
  let last_nonempty = ref (-1) in
  for piece = 0 to piece_count - 1 do
    if Array.unsafe_get minimum piece <> Float.infinity then
      last_nonempty := piece
  done;
  let cursor = ref 0. and placed = ref false in
  for piece = 0 to piece_count - 1 do
    if piece land 4095 = 0 then Cancel.check_opt cancel;
    let low = Array.unsafe_get minimum piece
    and high = Array.unsafe_get maximum piece in
    if low <> Float.infinity then begin
      let initial_offset = if not !placed then 0. else !cursor -. low in
      let rec lift_to_cursor remaining offset =
        let translated_low = low +. offset in
        if !placed && translated_low < !cursor && remaining > 0 then
          lift_to_cursor (remaining - 1) (Float.next_after offset Float.infinity)
        else offset, translated_low in
      let offset, translated_low = lift_to_cursor 8 initial_offset in
      let translated_high = high +. offset in
      let has_next = piece < !last_nonempty in
      let next = if has_next then translated_high +. gap else translated_high in
      if not (Float.is_finite offset && Float.is_finite translated_low && Float.is_finite translated_high
          && Float.is_finite next)
          || (!placed && translated_low < !cursor)
          || (has_next && gap > 0. && next <= translated_high) then fail (Printf.sprintf
          "packing translation is not representable for piece %d" piece);
      Array.unsafe_set x piece (offset *. axis.Rays_math.Vec3.x);
      Array.unsafe_set y piece (offset *. axis.Rays_math.Vec3.y);
      Array.unsafe_set z piece (offset *. axis.Rays_math.Vec3.z);
      cursor := next;
      placed := true
    end
  done;
  x, y, z

let element_translations ?cancel ~grain element_piece piece_x piece_y piece_z =
  let count = Array.length element_piece in
  let x = Array.make count 0. and y = Array.make count 0.
  and z = Array.make count 0. in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
      (fun element ->
    if element land 4095 = 0 then Cancel.check_opt cancel;
    let piece = Array.unsafe_get element_piece element in
    Array.unsafe_set x element (Array.unsafe_get piece_x piece);
    Array.unsafe_set y element (Array.unsafe_get piece_y piece);
    Array.unsafe_set z element (Array.unsafe_get piece_z piece));
  x, y, z

let point_translations_of_primitive ?cancel ~grain values geometry =
  let values = Packed.Float3.Private.view values in
  let topology = Geometry.topology geometry in
  let index = Topology_index.create ?cancel topology |> Topology_index.Private.view in
  let point_count = Geometry.point_count geometry in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. and assigned = Bytes.make point_count '\000' in
  let blocks = block_count point_count grain in
  let errors = Array.make blocks (-1) in
  if blocks > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(blocks - 1)
      (fun block ->
    Cancel.check_opt cancel;
    let first, last = block_bounds point_count grain block in
    let first_error = ref (-1) in
    for point = first to last - 1 do
      if point land 4095 = 0 then Cancel.check_opt cancel;
      let corner_first = Array.unsafe_get index.point_offsets point
      and corner_last = Array.unsafe_get index.point_offsets (point + 1) in
      if corner_first < corner_last then begin
        let vertex = Array.unsafe_get index.point_vertices corner_first in
        let primitive = Array.unsafe_get index.primitive_of_vertex vertex in
        let tx = Array.unsafe_get values.x primitive
        and ty = Array.unsafe_get values.y primitive
        and tz = Array.unsafe_get values.z primitive in
        if not (Float.is_finite tx && Float.is_finite ty && Float.is_finite tz) then first_error := point;
        let corner = ref (corner_first + 1) in
        while !first_error < 0 && !corner < corner_last do
          let vertex = Array.unsafe_get index.point_vertices !corner in
          let primitive = Array.unsafe_get index.primitive_of_vertex vertex in
          if Array.unsafe_get values.x primitive <> tx
              || Array.unsafe_get values.y primitive <> ty
              || Array.unsafe_get values.z primitive <> tz then
            first_error := point;
          incr corner
        done;
        Array.unsafe_set x point tx;
        Array.unsafe_set y point ty;
        Array.unsafe_set z point tz;
        Bytes.unsafe_set assigned point '\001'
      end
    done;
    Array.unsafe_set errors block !first_error);
  (match first_recorded errors with
   | None -> x, y, z, assigned
   | Some point -> fail (Printf.sprintf
       "point %d has non-finite or conflicting primitive translations" point))

let translated_positions ?cancel ~grain ?assigned ~direction tx ty tz geometry =
  let source = Packed.Float3.Private.view (Geometry.positions geometry) in
  let point_count = Geometry.point_count geometry in
  let change_x = Array.exists (( <> ) 0.) tx
  and change_y = Array.exists (( <> ) 0.) ty
  and change_z = Array.exists (( <> ) 0.) tz in
  if not (change_x || change_y || change_z) then None else
  let x = if change_x then Array.copy source.x else source.x
  and y = if change_y then Array.copy source.y else source.y
  and z = if change_z then Array.copy source.z else source.z in
  let blocks = block_count point_count grain in
  let errors = Array.make blocks (-1) in
  if blocks > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(blocks - 1)
      (fun block ->
    Cancel.check_opt cancel;
    let first, last = block_bounds point_count grain block in
    let first_error = ref (-1) in
    for point = first to last - 1 do
      if point land 4095 = 0 then Cancel.check_opt cancel;
      if match assigned with None -> true
          | Some assigned -> Bytes.unsafe_get assigned point <> '\000' then begin
        let dx = direction *. Array.unsafe_get tx point
        and dy = direction *. Array.unsafe_get ty point
        and dz = direction *. Array.unsafe_get tz point in
        let px = Array.unsafe_get source.x point +. dx
        and py = Array.unsafe_get source.y point +. dy
        and pz = Array.unsafe_get source.z point +. dz in
        if !first_error < 0 && not (Float.is_finite px && Float.is_finite py && Float.is_finite pz) then
          first_error := point;
        if change_x then Array.unsafe_set x point px;
        if change_y then Array.unsafe_set y point py;
        if change_z then Array.unsafe_set z point pz
      end
    done;
    Array.unsafe_set errors block !first_error);
  (match first_recorded errors with
   | Some point -> fail (Printf.sprintf
       "translated position is non-finite at point %d" point)
   | None -> Some (Packed.Float3.Private.of_shared_exn ~x ~y ~z))

let translated_positions_by_piece ?cancel ~grain point_piece piece_x piece_y
    piece_z geometry =
  let source = Packed.Float3.Private.view (Geometry.positions geometry) in
  let point_count = Geometry.point_count geometry in
  let change_x = Array.exists (( <> ) 0.) piece_x
  and change_y = Array.exists (( <> ) 0.) piece_y
  and change_z = Array.exists (( <> ) 0.) piece_z in
  if not (change_x || change_y || change_z) then None else
  let x = if change_x then Array.copy source.x else source.x
  and y = if change_y then Array.copy source.y else source.y
  and z = if change_z then Array.copy source.z else source.z in
  let blocks = block_count point_count grain in
  let errors = Array.make blocks (-1) in
  if blocks > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(blocks - 1)
      (fun block ->
    Cancel.check_opt cancel;
    let first, last = block_bounds point_count grain block in
    let first_error = ref (-1) in
    for point = first to last - 1 do
      if point land 4095 = 0 then Cancel.check_opt cancel;
      let piece = Array.unsafe_get point_piece point in
      if piece >= 0 then begin
        let px = Array.unsafe_get source.x point
            +. Array.unsafe_get piece_x piece
        and py = Array.unsafe_get source.y point
            +. Array.unsafe_get piece_y piece
        and pz = Array.unsafe_get source.z point
            +. Array.unsafe_get piece_z piece in
        if !first_error < 0 && not (Float.is_finite px && Float.is_finite py && Float.is_finite pz) then
          first_error := point;
        if change_x then Array.unsafe_set x point px;
        if change_y then Array.unsafe_set y point py;
        if change_z then Array.unsafe_set z point pz
      end
    done;
    Array.unsafe_set errors block !first_error);
  (match first_recorded errors with
   | Some point -> fail (Printf.sprintf
       "translated position is non-finite at point %d" point)
   | None -> Some (Packed.Float3.Private.of_shared_exn ~x ~y ~z))

let separate ?cancel ~grain ~owner ~translation_attribute ~axis ~gap
    ~piece_attribute geometry =
  let element_piece, piece_count =
    compile_piece_ids ?cancel owner piece_attribute geometry in
  let point_piece = match owner with
    | Attribute.Point ->
        validate_point_piece_primitives ?cancel ~grain element_piece geometry;
        element_piece
    | Attribute.Primitive ->
        primitive_pieces_to_points ?cancel ~grain element_piece geometry
    | Attribute.Vertex | Attribute.Detail -> assert false in
  let projection = validate_positions_and_project ?cancel ~grain axis
      point_piece geometry in
  let minimum, maximum = piece_bounds ?cancel ~piece_count point_piece
      projection in
  let piece_x, piece_y, piece_z = piece_translations ?cancel ~axis ~gap
      minimum maximum in
  let element_x, element_y, element_z = element_translations ?cancel ~grain
      element_piece piece_x piece_y piece_z in
  let positions = translated_positions_by_piece ?cancel ~grain point_piece
      piece_x piece_y piece_z geometry in
  let translation = Attribute.create_owned ~owner ~name:translation_attribute
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:element_x ~y:element_y ~z:element_z)) in
  match translation with
  | Error message -> Error message
  | Ok translation -> Geometry.Private.with_merged_attributes_and_groups_owned
      ?positions ~attributes:[|translation|] ~groups:[||] geometry

let move_back ?cancel ~grain ~owner ~translation_attribute geometry =
  let attribute = match Geometry.find_attribute ~owner translation_attribute geometry with
    | Some attribute -> attribute
    | None -> fail (Printf.sprintf "translation attribute %S was not found"
        translation_attribute) in
  let values = match Attribute.Private.storage attribute with
    | Attribute.Float3 values -> values
    | _ -> fail (Printf.sprintf
        "translation attribute %S must use float3 storage, not %s"
        translation_attribute (Attribute.kind_name attribute)) in
  let point_x, point_y, point_z, assigned = match owner with
    | Attribute.Point ->
        let values = Packed.Float3.Private.view values in
        values.x, values.y, values.z, None
    | Attribute.Primitive ->
        let x, y, z, assigned =
          point_translations_of_primitive ?cancel ~grain values geometry in
        x, y, z, Some assigned
    | Attribute.Vertex | Attribute.Detail -> assert false in
  match translated_positions ?cancel ~grain ?assigned ~direction:(-1.)
      point_x point_y point_z geometry with
  | None -> Ok geometry
  | Some positions -> Geometry.with_positions positions geometry

let run ?cancel ?(grain = 16_384) ?(owner = Attribute.Primitive)
    ?(translation_attribute = "piece_translation") ?(axis = Rays_math.Vec3.unit_x)
    ?(gap = 0.001) ~mode ~piece_attribute geometry =
  Error.guard ~operation:"separate_pieces" ~code:"invalid_separate_pieces" @@ fun () ->
  try
    if grain <= 0 then fail "grain must be positive";
    validate_name "piece attribute name" piece_attribute;
    validate_name "translation attribute name" translation_attribute;
    if String.equal piece_attribute translation_attribute then
      fail "piece and translation attribute names must differ";
    if not (Float.is_finite gap) || gap < 0. then
      fail "piece gap must be finite and non-negative";
    (match owner with
     | Attribute.Point | Attribute.Primitive -> ()
     | Attribute.Vertex | Attribute.Detail ->
         fail "piece owner must be point or primitive");
    Cancel.check_opt cancel;
    match mode with
    | Separate_pieces_separate ->
        let axis = normalized_axis axis in
        separate ?cancel ~grain ~owner ~translation_attribute ~axis ~gap
          ~piece_attribute geometry
    | Separate_pieces_move_back ->
        move_back ?cancel ~grain ~owner ~translation_attribute geometry
  with Separate_pieces_error message -> Error message
