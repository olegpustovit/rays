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

module Deferred = struct
  type holder = ..

  (* the thunk and its holder, dropped together once forced *)
  type 'a t = {
    make : ((unit -> 'a) * holder option) option Atomic.t;
    value : 'a option Atomic.t;
    release : (unit -> unit) option;
  }

  let create ?holder ?release make =
    { make = Atomic.make (Some (make, holder)); value = Atomic.make None; release }

  (* From any domain, holding no lock (the thunk may run parallel work whose tasks force the same
     value): domains that race may each run the thunk, the first result wins and every reader sees
     it; the thunk (and what it holds, a device buffer) is dropped once forced. *)
  let force value =
    match Atomic.get value.value with
    | Some forced -> forced
    | None -> (
        match Atomic.get value.make with
        | None -> Option.get (Atomic.get value.value)
        | Some (make, _) ->
            let forced = make () in
            ignore (Atomic.compare_and_set value.value None (Some forced));
            Atomic.set value.make None;
            Option.get (Atomic.get value.value))

  let forced value = Option.is_some (Atomic.get value.value)

  let holder value =
    if forced value then None else Option.bind (Atomic.get value.make) (fun (_, holder) -> holder)

  let releaser value = if forced value then None else value.release
end

module Float3 = struct
  type view = { x : float array; y : float array; z : float array }

  type t = {
    planes : view;
    length : int;
    data_id : int;
    deferred : view Deferred.t option;
        (* device-resident planes, materialised when first read; [planes] is then unused *)
  }

  type buffer = t

  let planes value = match value.deferred with None -> value.planes | Some d -> Deferred.force d
  let length value = value.length
  let data_id value = value.data_id
  let payload_bytes value = length value * 3 * 8

  let get value index =
    let p = planes value in
    (p.x.(index), p.y.(index), p.z.(index))

  let make x y z =
    {
      planes = { x; y; z };
      length = Array.length x;
      data_id = Support.fresh_data_id ();
      deferred = None;
    }

  let of_owned ~x ~y ~z =
    match
      ( same_length "Float3" (Array.length x) (Array.length y),
        same_length "Float3" (Array.length x) (Array.length z) )
    with
    | Ok (), Ok () -> Ok (make x y z)
    | Error message, _ | _, Error message -> Error message

  let deferred ?holder ?release ~length materialise =
    if length < 0 then invalid_arg "Packed.Float3.deferred: negative length";
    {
      planes = { x = [||]; y = [||]; z = [||] };
      length;
      data_id = Support.fresh_data_id ();
      deferred =
        Some
          (Deferred.create ?holder ?release (fun () ->
               match materialise () with
               | [| x; y; z |]
                 when Array.length x = length && Array.length y = length && Array.length z = length
                 ->
                   { x; y; z }
               | _ -> invalid_arg "Packed.Float3.deferred: materialised planes changed shape"));
    }

  let resident value = match value.deferred with Some d -> not (Deferred.forced d) | None -> false
  let holder value = Option.bind value.deferred Deferred.holder
  let releaser value = Option.bind value.deferred Deferred.releaser

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
      make value.x value.y value.z
  end

  module View = struct
    type nonrec view = view = { x : float array; y : float array; z : float array }

    let view = planes

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
