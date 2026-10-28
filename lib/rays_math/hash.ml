let[@inline always] mix_with multiplier hash value = hash * multiplier lxor value
let[@inline always] mix hash value = mix_with 65599 hash value
let fnv_int_mix hash value = hash lxor value * 0x100000001b3

let fnv_int_range ~seed text first last =
  let hash = ref seed in
  for index = first to last - 1 do
    hash := fnv_int_mix !hash (Char.code (String.unsafe_get text index))
  done;
  !hash

let fnv64_mix hash value = Int64.mul (Int64.logxor hash (Int64.of_int value)) 0x100000001b3L

let float_sequence32 values =
  let to_uint32 value =
    if not (Float.is_finite value) then 0
    else
      let value = Float.rem (Float.trunc value) 4_294_967_296. in
      int_of_float (if value < 0. then value +. 4_294_967_296. else value)
  in
  let imul a b = a * b land 0xFFFFFFFF in
  let hash = ref 0x9e3779b9 in
  List.iter
    (fun value ->
      let key = to_uint32 (Float.floor (value *. 1_000_003.)) in
      hash := imul (!hash lxor key) 0x85ebca6b;
      hash := !hash lxor (!hash lsr 13);
      hash := imul !hash 0xc2b2ae35;
      hash := !hash lxor (!hash lsr 16))
    values;
  !hash

let string text =
  let hash = ref 0xcbf29ce484222325L in
  String.iter (fun character -> hash := fnv64_mix !hash (Char.code character)) text;
  !hash
