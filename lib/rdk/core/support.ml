module Bits = struct
  let byte_count length = (length + 7) / 8

  let[@inline always] mem bits index =
    Char.code (Bytes.unsafe_get bits (index lsr 3))
    land (1 lsl (index land 7)) <> 0

  let[@inline always] set bits index =
    let byte = index lsr 3 and mask = 1 lsl (index land 7) in
    Bytes.unsafe_set bits byte
      (Char.chr (Char.code (Bytes.unsafe_get bits byte) lor mask))
end

module Key_map = struct
  type keep = First | Last

  let capacity length =
    if length < 0 || length > (Sys.max_array_length - 1) / 3 * 2 then None
    else
      let wanted = max 16 (length + (length / 2) + 1) in
      let capacity = ref 16 in
      while !capacity < wanted && !capacity <= Sys.max_array_length / 2 do
        capacity := !capacity lsl 1
      done;
      if !capacity < wanted then None else Some !capacity

  let[@inline always] integer_hash value mask =
    let value = if Sys.word_size > 32 then value lxor (value lsr 32) else value in
    let value = value lxor (value lsr 16) in
    (value * 0x45d9f3b) land mask

  (* Specialized per key type: a shared higher-order builder would call
     [hash]/[equal] through closures in the probe loops. *)
  let ints keep (values : int array) =
    Option.map (fun capacity ->
      let indices = Array.make capacity (-1) and mask = capacity - 1 in
      Array.iteri (fun index key ->
        let slot = ref (integer_hash key mask) in
        while indices.(!slot) >= 0 && values.(indices.(!slot)) <> key do
          slot := (!slot + 1) land mask
        done;
        if keep = Last || indices.(!slot) < 0 then indices.(!slot) <- index) values;
      fun key ->
        let slot = ref (integer_hash key mask) and searching = ref true
        and result = ref (-1) in
        while !searching do
          let index = indices.(!slot) in
          if index < 0 then searching := false
          else if values.(index) = key then begin
            result := index; searching := false
          end else slot := (!slot + 1) land mask
        done;
        !result) (capacity (Array.length values))

  let strings keep (values : string array) =
    Option.map (fun capacity ->
      let indices = Array.make capacity (-1) and mask = capacity - 1 in
      Array.iteri (fun index key ->
        let slot = ref (Hashtbl.hash key land mask) in
        while indices.(!slot) >= 0 && not (String.equal values.(indices.(!slot)) key) do
          slot := (!slot + 1) land mask
        done;
        if keep = Last || indices.(!slot) < 0 then indices.(!slot) <- index) values;
      fun key ->
        let slot = ref (Hashtbl.hash key land mask) and searching = ref true
        and result = ref (-1) in
        while !searching do
          let index = indices.(!slot) in
          if index < 0 then searching := false
          else if String.equal values.(index) key then begin
            result := index; searching := false
          end else slot := (!slot + 1) land mask
        done;
        !result) (capacity (Array.length values))
end

module Identity_cache = struct
  type ('k, 'v) slot = Empty | Entry of int * ('k, 'v) Ephemeron.K1.t

  type ('k, 'v) t = {
    id : 'k -> int;
    slots : ('k, 'v) slot array;
    mutable next : int;
    mutex : Mutex.t;
  }

  let create ~id capacity =
    if capacity <= 0 then invalid_arg "Identity_cache.create: capacity";
    { id; slots = Array.make capacity Empty; next = 0; mutex = Mutex.create () }

  let find cache id key =
    let rec scan slot =
      if slot = Array.length cache.slots then None
      else match cache.slots.(slot) with
        | Entry (entry_id, entry) when entry_id = id ->
            (match Ephemeron.K1.query entry key with
             | Some _ as found -> found
             | None -> scan (slot + 1))
        | Empty | Entry _ -> scan (slot + 1) in
    scan 0

  let find_or_add cache key build =
    let id = cache.id key in
    match Mutex.protect cache.mutex (fun () -> find cache id key) with
    | Some value -> value
    | None ->
        let built = build () in
        Mutex.protect cache.mutex (fun () ->
          match find cache id key with
          | Some existing -> existing
          | None ->
              cache.slots.(cache.next) <- Entry (id, Ephemeron.K1.make key built);
              cache.next <- (cache.next + 1) mod Array.length cache.slots;
              built)
end

let[@inline always] compare keys left right =
  let compared = Float.compare keys.(left) keys.(right) in
  if compared <> 0 then compared else Int.compare left right

let median_pivot keys order first middle last =
  let a = order.(first) and b = order.(middle) and c = order.(last) in
  if compare keys a b < 0 then
    if compare keys b c < 0 then b
    else if compare keys a c < 0 then c else a
  else if compare keys a c < 0 then a
  else if compare keys b c < 0 then c else b

let select ?cancel keys order first last selected =
  let lower = ref first and upper = ref last in
  while !lower < !upper do
    Cancel.check_opt cancel;
    let middle = !lower + ((!upper - !lower) / 2) in
    let pivot = median_pivot keys order !lower middle !upper in
    let left = ref !lower and right = ref !upper in
    while !left <= !right do
      while compare keys order.(!left) pivot < 0 do incr left done;
      while compare keys order.(!right) pivot > 0 do decr right done;
      if !left <= !right then begin
        let value = order.(!left) in
        order.(!left) <- order.(!right);
        order.(!right) <- value;
        incr left; decr right
      end
    done;
    if selected <= !right then upper := !right
    else if selected >= !left then lower := !left
    else begin lower := selected; upper := selected end
  done

let get_ok = function Ok value -> value | Error message -> invalid_arg message

let ceiling_div value divisor =
  (value / divisor) + if value mod divisor = 0 then 0 else 1

module Parallel = Parallel

let parallel_for ?cancel ~grain count work =
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
    (fun element ->
      if element land 4095 = 0 then Cancel.check_opt cancel;
      work element)
