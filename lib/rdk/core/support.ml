module Bits = struct

  let[@inline always] mem bits index =
    Char.code (Bytes.unsafe_get bits (index lsr 3))
    land (1 lsl (index land 7)) <> 0

  let[@inline always] set bits index =
    let byte = index lsr 3 and mask = 1 lsl (index land 7) in
    Bytes.unsafe_set bits byte
      (Char.chr (Char.code (Bytes.unsafe_get bits byte) lor mask))
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
