
type storage = Integer | Text of { prefix : string }
type mode = Enumerate_piece_elements | Enumerate_pieces

let owner_count geometry = function
  | Attribute.Point -> Geometry.point_count geometry
  | Attribute.Vertex -> Geometry.vertex_count geometry
  | Attribute.Primitive -> Geometry.primitive_count geometry
  | Attribute.Detail -> 1

type int_piece_table = {
  mutable int_piece_keys : int array;
  mutable int_piece_values : int array;
  mutable int_piece_used : bytes;
  mutable int_piece_count : int;
}

let int_piece_table_create () = {
  int_piece_keys = Array.make 16 0;
  int_piece_values = Array.make 16 0;
  int_piece_used = Bytes.make 16 '\000';
  int_piece_count = 0;
}

let[@inline] int_piece_hash key =
  let value = key lxor (key lsr 27) in
  (value * 0x1e35a7bd) land max_int

let int_piece_insert_raw table key value =
  let mask = Array.length table.int_piece_keys - 1 in
  let slot = ref (int_piece_hash key land mask) in
  while Bytes.get table.int_piece_used !slot <> '\000' do
    slot := (!slot + 1) land mask
  done;
  Bytes.set table.int_piece_used !slot '\001';
  table.int_piece_keys.(!slot) <- key;
  table.int_piece_values.(!slot) <- value

let int_piece_grow table =
  let old_keys = table.int_piece_keys and old_values = table.int_piece_values
  and old_used = table.int_piece_used in
  let capacity = Array.length old_keys * 2 in
  table.int_piece_keys <- Array.make capacity 0;
  table.int_piece_values <- Array.make capacity 0;
  table.int_piece_used <- Bytes.make capacity '\000';
  for slot = 0 to Array.length old_keys - 1 do
    if Bytes.get old_used slot <> '\000' then
      int_piece_insert_raw table old_keys.(slot) old_values.(slot)
  done

let int_piece_find_or_add table key =
  if table.int_piece_count * 10 >= Array.length table.int_piece_keys * 7 then
    int_piece_grow table;
  let mask = Array.length table.int_piece_keys - 1 in
  let slot = ref (int_piece_hash key land mask) in
  while Bytes.get table.int_piece_used !slot <> '\000'
      && table.int_piece_keys.(!slot) <> key do
    slot := (!slot + 1) land mask
  done;
  if Bytes.get table.int_piece_used !slot <> '\000' then
    table.int_piece_values.(!slot)
  else begin
    let value = table.int_piece_count in
    Bytes.set table.int_piece_used !slot '\001';
    table.int_piece_keys.(!slot) <- key;
    table.int_piece_values.(!slot) <- value;
    table.int_piece_count <- value + 1;
    value
  end

let sequence_fits ~start ~step maximum_rank =
  if maximum_rank <= 0 || step = 0 then true
  else
    let rank = Int64.of_int maximum_rank
    and start64 = Int64.of_int start and step64 = Int64.of_int step
    and maximum = Int64.of_int max_int and minimum = Int64.of_int min_int in
    if step > 0 then
      Int64.compare rank (Int64.div (Int64.sub maximum start64) step64) <= 0
    else
      Int64.compare rank
        (Int64.div (Int64.sub start64 minimum) (Int64.neg step64)) <= 0

let piece_ranks ?cancel ~selection ~mode ~owner ~name count geometry =
  let source = match Geometry.find_attribute ~owner name geometry with
    | None -> Error (Printf.sprintf "piece attribute %s does not exist" name)
    | Some attribute ->
        (match Attribute.Private.storage attribute with
         | Attribute.Int values -> Ok (`Int values)
         | Attribute.Text values -> Ok (`Text values)
         | Attribute.Float _ | Attribute.Int_array _ | Attribute.Float_array _
         | Attribute.Float2 _ | Attribute.Float3 _ | Attribute.Float4 _ ->
             Error (Printf.sprintf
               "piece attribute %s has %s storage, expected int or text"
               name (Attribute.kind_name attribute))) in
  Result.map (fun source ->
    let ranks = Array.make count (-1) and piece_sizes = Dynarray.create ()
    and max_rank = ref (-1) in
    let integer_table = match source with
      | `Int _ -> Some (int_piece_table_create ())
      | `Text _ -> None in
    let text_table = match source with
      | `Text _ -> Some (Hashtbl.create 128)
      | `Int _ -> None in
    let piece_id element = match source with
      | `Int values -> int_piece_find_or_add (Option.get integer_table)
          values.(element)
      | `Text values ->
          let table = Option.get text_table and key = values.(element) in
          (match Hashtbl.find_opt table key with
           | Some piece -> piece
           | None ->
               let piece = Hashtbl.length table in
               Hashtbl.add table key piece;
               piece) in
    for element = 0 to count - 1 do
      if element land 4095 = 0 then Cancel.check_opt cancel;
      if (match selection with None -> true
          | Some group -> Group.mem element group) then begin
        let piece = piece_id element in
        while Dynarray.length piece_sizes <= piece do
          Dynarray.add_last piece_sizes 0
        done;
        let rank = match mode with
          | Enumerate_pieces -> piece
          | Enumerate_piece_elements ->
              let rank = Dynarray.get piece_sizes piece in
              Dynarray.set piece_sizes piece (rank + 1);
              rank in
        ranks.(element) <- rank;
        if rank > !max_rank then max_rank := rank
      end
    done;
    ranks, !max_rank) source

let enumerate_raw ?cancel ?(grain = 16_384) ?selection ?(start = 0) ?(step = 1)
    ?(storage = Integer) ?piece_attribute
    ?(mode = Enumerate_piece_elements) ~owner ~name geometry =
  if grain <= 0 then invalid_arg "Rdk.Attribute_ops.enumerate: grain must be positive";
  if String.trim name = "" then Error "attribute name must not be empty"
  else if (match piece_attribute with
      | Some value -> String.trim value = "" | None -> false) then
    Error "piece attribute name must not be empty"
  else if owner = Attribute.Detail then Error "detail ownership cannot be enumerated"
  else
    let count = owner_count geometry owner in
    let expected_group_owner = match owner with
      | Attribute.Point -> Group.Point
      | Attribute.Vertex -> Group.Vertex
      | Attribute.Primitive -> Group.Primitive
      | Attribute.Detail -> assert false in
    match selection with
    | Some group when Group.owner group <> expected_group_owner
        || Group.length group <> count ->
        Error "selection owner/length does not match the enumerated elements"
    | None | Some _ ->
        let ranges = (count + grain - 1) / grain in
        let range_offsets, selected_count = match piece_attribute with
          | Some _ -> [||], 0
          | None ->
              let range_counts = Array.make ranges 0 in
              if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0
                  ~finish:(ranges - 1) (fun range ->
                    let first = range * grain
                    and last = min count ((range + 1) * grain) in
                    let selected = ref 0 in
                    for element = first to last - 1 do
                      if element land 4095 = 0 then Cancel.check_opt cancel;
                      if (match selection with None -> true
                          | Some group -> Group.mem element group) then
                        incr selected
                    done;
                    range_counts.(range) <- !selected);
              let range_offsets = Array.make ranges 0
              and selected_count = ref 0 in
              for range = 0 to ranges - 1 do
                range_offsets.(range) <- !selected_count;
                selected_count := !selected_count + range_counts.(range)
              done;
              range_offsets, !selected_count in
        let existing = Geometry.find_attribute ~owner name geometry in
        let expected_kind = match storage with Integer -> "int" | Text _ -> "text" in
        (match existing with
         | Some attribute when Attribute.kind_name attribute <> expected_kind ->
             Error (Printf.sprintf "target attribute %s has %s storage, expected %s"
               name (Attribute.kind_name attribute) expected_kind)
         | None | Some _ ->
             let global_fill set =
               if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0
                   ~finish:(ranges - 1) (fun range ->
                     let first = range * grain
                     and last = min count ((range + 1) * grain) in
                     let rank = ref range_offsets.(range) in
                     let value = ref (Int64.to_int (Int64.add (Int64.of_int start)
                       (Int64.mul (Int64.of_int range_offsets.(range))
                          (Int64.of_int step)))) in
                     for element = first to last - 1 do
                       if element land 4095 = 0 then Cancel.check_opt cancel;
                       if (match selection with None -> true
                           | Some group -> Group.mem element group) then begin
                         set element !value;
                         incr rank;
                       if !rank < selected_count then value := !value + step
                       end
                     done) in
             let ranks = match piece_attribute with
               | None -> Ok None
               | Some piece_attribute -> Result.map Option.some
                   (piece_ranks ?cancel ~selection ~mode ~owner
                      ~name:piece_attribute count geometry) in
             Result.bind ranks (fun ranks ->
               let maximum_rank = match ranks with
                 | None -> selected_count - 1
                 | Some (_, maximum_rank) -> maximum_rank in
               if not (sequence_fits ~start ~step maximum_rank) then
                 invalid_arg "Rdk.Attribute_ops.enumerate: integer sequence overflows";
               let value_of_rank rank = Int64.to_int (Int64.add
                   (Int64.of_int start)
                   (Int64.mul (Int64.of_int rank) (Int64.of_int step))) in
               let piece_fill ranks set =
                 if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                     ~finish:(count - 1) (fun element ->
                       if element land 16_383 = 0 then Cancel.check_opt cancel;
                       let rank = ranks.(element) in
                       if rank >= 0 then set element (value_of_rank rank)) in
               let fill set = match ranks with
                 | None -> global_fill set
                 | Some (ranks, _) -> piece_fill ranks set in
               let attribute = match storage with
                 | Integer ->
                     let values = match existing with
                       | Some attribute ->
                           (match Attribute.Private.storage attribute with
                            | Attribute.Int values -> Array.copy values
                            | _ -> assert false)
                       | None -> Array.make count 0 in
                     fill (fun element value -> values.(element) <- value);
                     Attribute.create_owned ~name ~owner (Attribute.Int values)
                 | Text { prefix } ->
                     let values = match existing with
                       | Some attribute ->
                           (match Attribute.Private.storage attribute with
                            | Attribute.Text values -> Array.copy values
                            | _ -> assert false)
                       | None -> Array.make count "" in
                     fill (fun element value ->
                       values.(element) <- prefix ^ string_of_int value);
                     Attribute.create_owned ~name ~owner (Attribute.Text values) in
               Result.bind attribute (fun attribute ->
                 Geometry.with_attribute attribute geometry)))

let run ?cancel ?grain ?selection ?start ?step ?storage ?piece_attribute
    ?mode ~owner ~name geometry =
  try Result.map_error
      (Error.of_string ~operation:"enumerate" ~code:"invalid_enumeration")
      (enumerate_raw ?cancel ?grain ?selection ?start ?step ?storage
         ?piece_attribute ?mode ~owner ~name geometry)
  with
  | Cancel.Cancelled -> Error (Error.make ~operation:"enumerate" ~code:"cancelled"
      "enumeration was cancelled")
  | Invalid_argument message -> Error (Error.make ~operation:"enumerate"
      ~code:"invalid_enumeration" message)
