type t = int64

let seed64 value = value
let seed value = Int64.of_int value

let next state =
  let open Int64 in
  let state = add state 0x9E3779B97F4A7C15L in
  let value = logxor state (shift_right_logical state 30) in
  let value = mul value 0xBF58476D1CE4E5B9L in
  let value = logxor value (shift_right_logical value 27) in
  let value = mul value 0x94D049BB133111EBL in
  logxor value (shift_right_logical value 31), state

let float state =
  let value, state = next state in
  let mantissa = Int64.shift_right_logical value 11 in
  Int64.to_float mantissa /. 9_007_199_254_740_992., state

let[@inline always] mix_index value =
  let value = value lxor (value lsr 30) in
  let value = value * 0x3f58476d1ce4e5b9 in
  let value = value lxor (value lsr 27) in
  let value = value * 0x14d049bb133111eb in
  value lxor (value lsr 31)

let[@inline always] float_at_int seed ~index =
  let keyed = seed lxor
      ((index + 0x11b54a32d192ed03) * 0x1e3779b97f4a7c15) in
  float_of_int (mix_index keyed lsr 10)
  *. (1. /. 9_007_199_254_740_992.)

let[@inline always] float_at state ~index = float_at_int (Int64.to_int state) ~index

let int ~bound state =
  if bound <= 0 then invalid_arg "Rand.int: bound must be positive";
  let value, state = next state in
  Int64.(unsigned_rem value (of_int bound) |> to_int), state

let shuffle values state =
  let values = Array.of_list values in
  let state = ref state in
  for index = Array.length values - 1 downto 1 do
    let other, next_state = int ~bound:(index + 1) !state in
    state := next_state;
    let value = values.(index) in
    values.(index) <- values.(other);
    values.(other) <- value
  done;
  Array.to_list values, !state
