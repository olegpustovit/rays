let same_length name expected actual =
  if expected = actual then Ok ()
  else Error (Printf.sprintf "Packed.%s: plane lengths differ (%d and %d)" name expected actual)

let validate_offsets name offsets value_count =
  if Array.length offsets = 0 then
    Error (Printf.sprintf "Packed.%s: offsets must contain the initial zero" name)
  else if offsets.(0) <> 0 then Error (Printf.sprintf "Packed.%s: first offset must be zero" name)
  else begin
    let failure = ref None in
    for index = 1 to Array.length offsets - 1 do
      if !failure = None && (offsets.(index) < offsets.(index - 1) || offsets.(index) > value_count)
      then
        failure :=
          Some
            (Printf.sprintf "Packed.%s: offsets must be monotone and within the value buffer" name)
    done;
    match !failure with
    | Some message -> Error message
    | None when offsets.(Array.length offsets - 1) <> value_count ->
        Error (Printf.sprintf "Packed.%s: final offset must equal value count" name)
    | None -> Ok ()
  end

module Int_array = struct
  type t = { offsets : int array; values : int array }
  type buffer = t

  let length value = Array.length value.offsets - 1
  let value_count value = Array.length value.values

  let payload_bytes value =
    (Array.length value.offsets + Array.length value.values) * (Sys.word_size / 8)

  let row_range value index =
    if index < 0 || index >= length value then
      invalid_arg "Packed.Int_array.row_range: index out of bounds";
    (value.offsets.(index), value.offsets.(index + 1))

  let get value index =
    let first, last = row_range value index in
    Array.sub value.values first (last - first)

  let create_owned ~offsets ~values =
    Result.map
      (fun () -> { offsets; values })
      (validate_offsets "Int_array" offsets (Array.length values))

  module View = struct
    type view = { offsets : int array; values : int array }

    let view (value : buffer) = { offsets = value.offsets; values = value.values }
    let create_validated_owned ~offsets ~values : buffer = { offsets; values }
  end
end

module Float_array = struct
  type t = { offsets : int array; values : float array }
  type buffer = t

  let length value = Array.length value.offsets - 1
  let value_count value = Array.length value.values

  let payload_bytes value =
    (Array.length value.offsets * (Sys.word_size / 8)) + (Array.length value.values * 8)

  let row_range value index =
    if index < 0 || index >= length value then
      invalid_arg "Packed.Float_array.row_range: index out of bounds";
    (value.offsets.(index), value.offsets.(index + 1))

  let get value index =
    let first, last = row_range value index in
    Array.sub value.values first (last - first)

  let create_owned ~offsets ~values =
    Result.map
      (fun () -> { offsets; values })
      (validate_offsets "Float_array" offsets (Array.length values))

  module View = struct
    type view = { offsets : int array; values : float array }

    let view (value : buffer) = { offsets = value.offsets; values = value.values }
    let create_validated_owned ~offsets ~values : buffer = { offsets; values }
  end
end

module Float2 = struct
  type t = { x : float array; y : float array }
  type buffer = t

  let length value = Array.length value.x
  let payload_bytes value = length value * 2 * 8
  let get value index = (value.x.(index), value.y.(index))

  let of_owned ~x ~y =
    Result.map (fun () -> { x; y }) (same_length "Float2" (Array.length x) (Array.length y))

  module View = struct
    type view = { x : float array; y : float array }

    let view (value : buffer) = { x = value.x; y = value.y }
  end
end

module Float3 = struct
  type t = { x : float array; y : float array; z : float array; data_id : int }
  type buffer = t

  let length value = Array.length value.x
  let data_id value = value.data_id
  let payload_bytes value = length value * 3 * 8
  let get value index = (value.x.(index), value.y.(index), value.z.(index))

  let of_owned ~x ~y ~z =
    match
      ( same_length "Float3" (Array.length x) (Array.length y),
        same_length "Float3" (Array.length x) (Array.length z) )
    with
    | Ok (), Ok () -> Ok { x; y; z; data_id = Support.fresh_data_id () }
    | Error message, _ | _, Error message -> Error message

  module Builder = struct
    type nonrec t = { x : float array; y : float array; z : float array; mutable frozen : bool }

    let create count =
      if count < 0 then invalid_arg "Packed.Float3.Builder.create: negative length";
      { x = Array.make count 0.; y = Array.make count 0.; z = Array.make count 0.; frozen = false }

    let set value index x y z =
      if value.frozen then invalid_arg "Packed.Float3.Builder.set: builder is frozen";
      value.x.(index) <- x;
      value.y.(index) <- y;
      value.z.(index) <- z

    let freeze value =
      if value.frozen then invalid_arg "Packed.Float3.Builder.freeze: already frozen";
      value.frozen <- true;
      { x = value.x; y = value.y; z = value.z; data_id = Support.fresh_data_id () }
  end

  module View = struct
    type view = { x : float array; y : float array; z : float array }

    let view (value : buffer) = { x = value.x; y = value.y; z = value.z }

    let of_owned_exn ~x ~y ~z =
      match of_owned ~x ~y ~z with Ok value -> value | Error message -> invalid_arg message

    let of_shared_exn = of_owned_exn
  end
end

module Float4 = struct
  type t = { x : float array; y : float array; z : float array; w : float array }
  type buffer = t

  let length value = Array.length value.x
  let payload_bytes value = length value * 4 * 8
  let get value index = (value.x.(index), value.y.(index), value.z.(index), value.w.(index))

  let of_owned ~x ~y ~z ~w =
    let count = Array.length x in
    match
      ( same_length "Float4" count (Array.length y),
        same_length "Float4" count (Array.length z),
        same_length "Float4" count (Array.length w) )
    with
    | Ok (), Ok (), Ok () -> Ok { x; y; z; w }
    | Error message, _, _ | _, Error message, _ | _, _, Error message -> Error message

  module View = struct
    type view = { x : float array; y : float array; z : float array; w : float array }

    let view (value : buffer) = { x = value.x; y = value.y; z = value.z; w = value.w }
  end
end
