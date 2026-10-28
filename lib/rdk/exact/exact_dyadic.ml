(* Magnitudes use little-endian base-2^30 limbs. The largest binary64
   exponent separation needs fewer than 72 limbs, so exact fallback remains
   small and deterministically bounded without a general bignum dependency. *)
let limb_bits = 30
let limb_base = 1 lsl limb_bits
let limb_mask = limb_base - 1
let limb_mask64 = Int64.of_int limb_mask

type t = {
  negative : bool;
  magnitude : int array;
  exponent : int;
}

let zero = { negative = false; magnitude = [||]; exponent = 0 }
let is_zero value = Array.length value.magnitude = 0

let trim magnitude =
  let last = ref (Array.length magnitude - 1) in
  while !last >= 0 && magnitude.(!last) = 0 do decr last done;
  if !last < 0 then [||]
  else if !last + 1 = Array.length magnitude then magnitude
  else Array.sub magnitude 0 (!last + 1)

let of_magnitude ~negative ~exponent magnitude =
  let magnitude = trim magnitude in
  if Array.length magnitude = 0 then zero
  else { negative; magnitude; exponent }

let magnitude_of_int64 value =
  if value = 0L then [||]
  else begin
    let low = Int64.(to_int (logand value limb_mask64)) in
    let high = Int64.(to_int (shift_right_logical value limb_bits)) in
    if high = 0 then [|low|] else [|low; high|]
  end

let of_float value =
  if not (Float.is_finite value) then
    invalid_arg "Rdk exact arithmetic: values must be finite";
  let bits = Int64.bits_of_float value in
  let negative = Int64.compare bits 0L < 0 in
  let absolute = Int64.logand bits 0x7fff_ffff_ffff_ffffL in
  let encoded_exponent = Int64.(to_int (shift_right_logical absolute 52)) in
  let fraction = Int64.logand absolute 0x000f_ffff_ffff_ffffL in
  if encoded_exponent = 0 then
    if fraction = 0L then zero
    else {
      negative;
      magnitude = magnitude_of_int64 fraction;
      exponent = -1074;
    }
  else {
    negative;
    magnitude = magnitude_of_int64
        (Int64.logor fraction (Int64.shift_left 1L 52));
    exponent = encoded_exponent - 1023 - 52;
  }

let shift_left magnitude shift =
  if shift = 0 || Array.length magnitude = 0 then magnitude
  else begin
    let words = shift / limb_bits and bits = shift mod limb_bits in
    let extra = if bits = 0 then 0 else 1 in
    let output = Array.make (Array.length magnitude + words + extra) 0 in
    if bits = 0 then Array.blit magnitude 0 output words (Array.length magnitude)
    else begin
      let carry = ref 0L in
      for index = 0 to Array.length magnitude - 1 do
        let value = Int64.logor
            (Int64.shift_left (Int64.of_int magnitude.(index)) bits) !carry in
        output.(words + index) <- Int64.(to_int (logand value limb_mask64));
        carry := Int64.shift_right_logical value limb_bits
      done;
      output.(Array.length magnitude + words) <- Int64.to_int !carry
    end;
    trim output
  end

let compare_magnitude left right =
  let left_length = Array.length left and right_length = Array.length right in
  if left_length <> right_length then Int.compare left_length right_length
  else begin
    let result = ref 0 and index = ref (left_length - 1) in
    while !result = 0 && !index >= 0 do
      result := Int.compare left.(!index) right.(!index);
      decr index
    done;
    !result
  end

let add_magnitude left right =
  let count = max (Array.length left) (Array.length right) in
  let output = Array.make (count + 1) 0 and carry = ref 0 in
  for index = 0 to count - 1 do
    let a = if index < Array.length left then left.(index) else 0
    and b = if index < Array.length right then right.(index) else 0 in
    let value = a + b + !carry in
    output.(index) <- value land limb_mask;
    carry := value lsr limb_bits
  done;
  output.(count) <- !carry;
  trim output

let subtract_magnitude left right =
  let output = Array.make (Array.length left) 0 and borrow = ref 0 in
  for index = 0 to Array.length left - 1 do
    let subtrahend =
      (if index < Array.length right then right.(index) else 0) + !borrow in
    let value = left.(index) - subtrahend in
    if value < 0 then begin
      output.(index) <- value + limb_base;
      borrow := 1
    end else begin
      output.(index) <- value;
      borrow := 0
    end
  done;
  assert (!borrow = 0);
  trim output

let add left right =
  if is_zero left then right
  else if is_zero right then left
  else begin
    let exponent = min left.exponent right.exponent in
    let left_magnitude = shift_left left.magnitude (left.exponent - exponent)
    and right_magnitude = shift_left right.magnitude (right.exponent - exponent) in
    if left.negative = right.negative then
      of_magnitude ~negative:left.negative ~exponent
        (add_magnitude left_magnitude right_magnitude)
    else
      match compare_magnitude left_magnitude right_magnitude with
      | 0 -> zero
      | comparison when comparison > 0 ->
          of_magnitude ~negative:left.negative ~exponent
            (subtract_magnitude left_magnitude right_magnitude)
      | _ ->
          of_magnitude ~negative:right.negative ~exponent
            (subtract_magnitude right_magnitude left_magnitude)
  end

let negate value =
  if is_zero value then value else { value with negative = not value.negative }

let subtract left right = add left (negate right)

let multiply_magnitude left right =
  if Array.length left = 0 || Array.length right = 0 then [||]
  else begin
    let output = Array.make (Array.length left + Array.length right) 0 in
    for left_index = 0 to Array.length left - 1 do
      let carry = ref 0L in
      for right_index = 0 to Array.length right - 1 do
        let output_index = left_index + right_index in
        let value = Int64.add
            (Int64.add
               (Int64.mul (Int64.of_int left.(left_index))
                  (Int64.of_int right.(right_index)))
               (Int64.of_int output.(output_index)))
            !carry in
        output.(output_index) <- Int64.(to_int (logand value limb_mask64));
        carry := Int64.shift_right_logical value limb_bits
      done;
      let output_index = ref (left_index + Array.length right) in
      while !carry <> 0L do
        let value = Int64.add (Int64.of_int output.(!output_index)) !carry in
        output.(!output_index) <- Int64.(to_int (logand value limb_mask64));
        carry := Int64.shift_right_logical value limb_bits;
        incr output_index
      done
    done;
    trim output
  end

let multiply left right =
  if is_zero left || is_zero right then zero
  else of_magnitude ~negative:(left.negative <> right.negative)
      ~exponent:(left.exponent + right.exponent)
      (multiply_magnitude left.magnitude right.magnitude)

let compare_zero value =
  if is_zero value then 0 else if value.negative then -1 else 1

let to_scaled_float value =
  if is_zero value then (0., 0)
  else begin
    let count = Array.length value.magnitude in
    let first = max 0 (count - 4) in
    let accumulator = ref 0. and scale = ref (1. /. float_of_int limb_base) in
    for index = count - 1 downto first do
      accumulator :=
        !accumulator +. (float_of_int value.magnitude.(index) *. !scale);
      scale := !scale /. float_of_int limb_base
    done;
    let mantissa = if value.negative then -. !accumulator else !accumulator in
    (mantissa, value.exponent + (count * limb_bits))
  end

let downward value = Float.next_after value Float.neg_infinity
let upward value = Float.next_after value Float.infinity

let to_scaled_interval value =
  if is_zero value then (0., 0., 0)
  else begin
    let count = Array.length value.magnitude in
    let lower = ref 0. and upper = ref 0.
    and scale = ref (1. /. float_of_int limb_base) in
    let lost_positive_tail = ref false in
    for index = count - 1 downto 0 do
      let limb = value.magnitude.(index) in
      if !scale = 0. then begin
        if limb <> 0 then lost_positive_tail := true
      end else if limb <> 0 then begin
        let term = float_of_int limb *. !scale in
        lower := downward (!lower +. term);
        upper := upward (!upper +. term)
      end;
      scale := !scale /. float_of_int limb_base
    done;
    if !lost_positive_tail then upper := upward !upper;
    let exponent = value.exponent + (count * limb_bits) in
    if value.negative then (-. !upper, -. !lower, exponent)
    else (!lower, !upper, exponent)
  end

module Scratch = struct
  type arena = {
    mutable limbs : int array;
    mutable limb_count : int;
    mutable negative : bool array;
    mutable exponents : int array;
    mutable offsets : int array;
    mutable lengths : int array;
    mutable value_count : int;
    mutable busy : bool;
  }

  let create () = {
    limbs = Array.make 4_096 0;
    limb_count = 0;
    negative = Array.make 256 false;
    exponents = Array.make 256 0;
    offsets = Array.make 256 0;
    lengths = Array.make 256 0;
    value_count = 0;
    busy = false;
  }

  let grow_int values capacity =
    let output = Array.make capacity 0 in
    Array.blit values 0 output 0 (Array.length values);
    output

  let grow_bool values capacity =
    let output = Array.make capacity false in
    Array.blit values 0 output 0 (Array.length values);
    output

  let ensure_values arena needed =
    if needed > Array.length arena.lengths then begin
      let capacity = ref (Array.length arena.lengths) in
      while !capacity < needed do
        if !capacity > Sys.max_array_length / 2 then
          invalid_arg "Rdk exact scratch value plane exceeds array limits";
        capacity := !capacity * 2
      done;
      arena.negative <- grow_bool arena.negative !capacity;
      arena.exponents <- grow_int arena.exponents !capacity;
      arena.offsets <- grow_int arena.offsets !capacity;
      arena.lengths <- grow_int arena.lengths !capacity
    end

  let ensure_limbs arena needed =
    if needed > Array.length arena.limbs then begin
      let capacity = ref (Array.length arena.limbs) in
      while !capacity < needed do
        if !capacity > Sys.max_array_length / 2 then
          invalid_arg "Rdk exact scratch limb plane exceeds array limits";
        capacity := !capacity * 2
      done;
      arena.limbs <- grow_int arena.limbs !capacity
    end

  let[@inline always] value arena ~negative ~exponent ~offset ~length =
    ensure_values arena (arena.value_count + 1);
    let result = arena.value_count in
    arena.value_count <- result + 1;
    arena.negative.(result) <- negative && length > 0;
    arena.exponents.(result) <- if length = 0 then 0 else exponent;
    arena.offsets.(result) <- offset;
    arena.lengths.(result) <- length;
    result

  let[@inline always] reserve_limbs arena count =
    let offset = arena.limb_count in
    let needed = offset + count in
    if needed < offset then invalid_arg "Rdk exact scratch limb count overflow";
    ensure_limbs arena needed;
    arena.limb_count <- needed;
    offset

  let[@inline always] trim arena offset length =
    let length = ref length in
    while !length > 0 && arena.limbs.(offset + !length - 1) = 0 do
      decr length
    done;
    !length

  let of_float arena number =
    let bits = Int64.bits_of_float number in
    let negative = Int64.compare bits 0L < 0 in
    let absolute = Int64.logand bits 0x7fff_ffff_ffff_ffffL in
    let encoded_exponent = Int64.(to_int (shift_right_logical absolute 52)) in
    let fraction = Int64.logand absolute 0x000f_ffff_ffff_ffffL in
    let magnitude, exponent = if encoded_exponent = 0 then fraction, -1074
      else Int64.logor fraction (Int64.shift_left 1L 52),
        encoded_exponent - 1023 - 52 in
    if magnitude = 0L then value arena ~negative:false ~exponent:0
        ~offset:arena.limb_count ~length:0
    else begin
      let low = Int64.(to_int (logand magnitude limb_mask64))
      and high = Int64.(to_int (shift_right_logical magnitude limb_bits)) in
      let length = if high = 0 then 1 else 2 in
      let offset = reserve_limbs arena length in
      arena.limbs.(offset) <- low;
      if length = 2 then arena.limbs.(offset + 1) <- high;
      value arena ~negative ~exponent ~offset ~length
    end

  let of_exact arena number =
    let length = Array.length number.magnitude in
    if length = 0 then value arena ~negative:false ~exponent:0
        ~offset:arena.limb_count ~length:0
    else begin
      let offset = reserve_limbs arena length in
      Array.blit number.magnitude 0 arena.limbs offset length;
      value arena ~negative:number.negative ~exponent:number.exponent
        ~offset ~length
    end

  let[@inline always] is_zero arena number = arena.lengths.(number) = 0

  let[@inline always] negate arena number =
    value arena ~negative:(not arena.negative.(number))
      ~exponent:arena.exponents.(number) ~offset:arena.offsets.(number)
      ~length:arena.lengths.(number)

  let[@inline always] shifted_limb arena number shift index =
    let word_shift = shift / limb_bits and bit_shift = shift mod limb_bits in
    let source = index - word_shift and length = arena.lengths.(number)
    and offset = arena.offsets.(number) in
    if bit_shift = 0 then
      if source >= 0 && source < length then arena.limbs.(offset + source) else 0
    else begin
      let low = if source >= 0 && source < length then
          Int64.shift_left (Int64.of_int arena.limbs.(offset + source)) bit_shift
        else 0L
      and carry = if source > 0 && source - 1 < length then
          Int64.shift_right_logical
            (Int64.of_int arena.limbs.(offset + source - 1))
            (limb_bits - bit_shift)
        else 0L in
      Int64.(to_int (logand (logor low carry) limb_mask64))
    end

  let[@inline always] shifted_length arena number shift =
    let length = arena.lengths.(number) in
    if length = 0 then 0 else begin
      let words = shift / limb_bits and bits = shift mod limb_bits in
      let extra = if bits = 0 then 0 else
          let high = arena.limbs.(arena.offsets.(number) + length - 1) in
          if high lsr (limb_bits - bits) = 0 then 0 else 1 in
      words + length + extra
    end

  let compare_shifted arena left left_shift right right_shift =
    let left_length = shifted_length arena left left_shift
    and right_length = shifted_length arena right right_shift in
    if left_length <> right_length then Int.compare left_length right_length
    else begin
      let comparison = ref 0 and index = ref (left_length - 1) in
      while !comparison = 0 && !index >= 0 do
        comparison := Int.compare
            (shifted_limb arena left left_shift !index)
            (shifted_limb arena right right_shift !index);
        decr index
      done;
      !comparison
    end

  let add arena left right =
    if is_zero arena left then right
    else if is_zero arena right then left
    else begin
      let exponent = min arena.exponents.(left) arena.exponents.(right) in
      let left_shift = arena.exponents.(left) - exponent
      and right_shift = arena.exponents.(right) - exponent in
      let left_length = shifted_length arena left left_shift
      and right_length = shifted_length arena right right_shift in
      if arena.negative.(left) = arena.negative.(right) then begin
        let count = max left_length right_length in
        let offset = reserve_limbs arena (count + 1) and carry = ref 0 in
        for index = 0 to count - 1 do
          let sum = shifted_limb arena left left_shift index
              + shifted_limb arena right right_shift index + !carry in
          arena.limbs.(offset + index) <- sum land limb_mask;
          carry := sum lsr limb_bits
        done;
        arena.limbs.(offset + count) <- !carry;
        value arena ~negative:arena.negative.(left) ~exponent ~offset
          ~length:(if !carry = 0 then count else count + 1)
      end else begin
        let comparison = compare_shifted arena left left_shift right right_shift in
        if comparison = 0 then value arena ~negative:false ~exponent:0
            ~offset:arena.limb_count ~length:0
        else begin
          let larger,larger_shift,smaller,smaller_shift,negative =
            if comparison > 0 then
              left,left_shift,right,right_shift,arena.negative.(left)
            else right,right_shift,left,left_shift,arena.negative.(right) in
          let count = shifted_length arena larger larger_shift in
          let offset = reserve_limbs arena count and borrow = ref 0 in
          for index = 0 to count - 1 do
            let difference = shifted_limb arena larger larger_shift index
                - shifted_limb arena smaller smaller_shift index - !borrow in
            if difference < 0 then begin
              arena.limbs.(offset + index) <- difference + limb_base;
              borrow := 1
            end else begin
              arena.limbs.(offset + index) <- difference;
              borrow := 0
            end
          done;
          assert (!borrow = 0);
          value arena ~negative ~exponent ~offset
            ~length:(trim arena offset count)
        end
      end
    end

  let subtract arena left right = add arena left (negate arena right)

  let multiply arena left right =
    if is_zero arena left || is_zero arena right then
      value arena ~negative:false ~exponent:0 ~offset:arena.limb_count ~length:0
    else begin
      let left_length = arena.lengths.(left) and right_length = arena.lengths.(right) in
      let count = left_length + right_length in
      let offset = reserve_limbs arena count in
      Array.fill arena.limbs offset count 0;
      let left_offset = arena.offsets.(left) and right_offset = arena.offsets.(right) in
      for left_index = 0 to left_length - 1 do
        let carry = ref 0L in
        for right_index = 0 to right_length - 1 do
          let output_index = offset + left_index + right_index in
          let product = Int64.add
              (Int64.add
                 (Int64.mul (Int64.of_int arena.limbs.(left_offset + left_index))
                    (Int64.of_int arena.limbs.(right_offset + right_index)))
                 (Int64.of_int arena.limbs.(output_index))) !carry in
          arena.limbs.(output_index) <-
            Int64.(to_int (logand product limb_mask64));
          carry := Int64.shift_right_logical product limb_bits
        done;
        let output_index = ref (offset + left_index + right_length) in
        while !carry <> 0L do
          let sum = Int64.add (Int64.of_int arena.limbs.(!output_index)) !carry in
          arena.limbs.(!output_index) <- Int64.(to_int (logand sum limb_mask64));
          carry := Int64.shift_right_logical sum limb_bits;
          incr output_index
        done
      done;
      value arena
        ~negative:(arena.negative.(left) <> arena.negative.(right))
        ~exponent:(arena.exponents.(left) + arena.exponents.(right))
        ~offset ~length:(trim arena offset count)
    end

  let[@inline always] compare_zero arena number =
    if is_zero arena number then 0 else if arena.negative.(number) then -1 else 1

  let pool = Domain.DLS.new_key create

  let acquire () =
    let arena = Domain.DLS.get pool in
    let arena = if arena.busy then create () else arena in
    arena.busy <- true;
    arena.limb_count <- 0;
    arena.value_count <- 0;
    arena

  let release arena = arena.busy <- false

  let orient2d ~ax ~ay ~bx ~by ~cx ~cy =
    let arena = acquire () in
    let ax = of_float arena ax and ay = of_float arena ay
    and bx = of_float arena bx and by = of_float arena by
    and cx = of_float arena cx and cy = of_float arena cy in
    let result = compare_zero arena (subtract arena
        (multiply arena (subtract arena ax cx) (subtract arena by cy))
        (multiply arena (subtract arena ay cy) (subtract arena bx cx))) in
    release arena;
    result

  let polygon_area_packed ~x ~y ~points ~first ~count =
    let arena = acquire () in
    let sum = ref (of_float arena 0.) in
    for local = 0 to count - 1 do
      let next = if local + 1 = count then 0 else local + 1 in
      let a = points.(first + local) and b = points.(first + next) in
      let ax = of_float arena x.(a) and ay = of_float arena y.(a)
      and bx = of_float arena x.(b) and by = of_float arena y.(b) in
      sum := add arena !sum
          (subtract arena (multiply arena ax by) (multiply arena bx ay))
    done;
    let result = compare_zero arena !sum in
    release arena;
    result

  let orient3d ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~dx ~dy ~dz =
    let arena = acquire () in
    let ax = of_float arena ax and ay = of_float arena ay and az = of_float arena az
    and bx = of_float arena bx and by = of_float arena by and bz = of_float arena bz
    and cx = of_float arena cx and cy = of_float arena cy and cz = of_float arena cz
    and dx = of_float arena dx and dy = of_float arena dy and dz = of_float arena dz in
    let adx = subtract arena ax dx and ady = subtract arena ay dy
    and adz = subtract arena az dz and bdx = subtract arena bx dx
    and bdy = subtract arena by dy and bdz = subtract arena bz dz
    and cdx = subtract arena cx dx and cdy = subtract arena cy dy
    and cdz = subtract arena cz dz in
    let first = multiply arena adx
        (subtract arena (multiply arena bdy cdz) (multiply arena bdz cdy))
    and second = multiply arena ady
        (subtract arena (multiply arena bdz cdx) (multiply arena bdx cdz))
    and third = multiply arena adz
        (subtract arena (multiply arena bdx cdy) (multiply arena bdy cdx)) in
    let result = compare_zero arena (add arena (add arena first second) third) in
    release arena;
    result

  let incircle ~ax ~ay ~bx ~by ~cx ~cy ~dx ~dy =
    let arena = acquire () in
    let ax = of_float arena ax and ay = of_float arena ay
    and bx = of_float arena bx and by = of_float arena by
    and cx = of_float arena cx and cy = of_float arena cy
    and dx = of_float arena dx and dy = of_float arena dy in
    let adx = subtract arena ax dx and ady = subtract arena ay dy
    and bdx = subtract arena bx dx and bdy = subtract arena by dy
    and cdx = subtract arena cx dx and cdy = subtract arena cy dy in
    let alift = add arena (multiply arena adx adx) (multiply arena ady ady)
    and blift = add arena (multiply arena bdx bdx) (multiply arena bdy bdy)
    and clift = add arena (multiply arena cdx cdx) (multiply arena cdy cdy) in
    let bcdet = subtract arena (multiply arena bdx cdy)
        (multiply arena cdx bdy)
    and cadet = subtract arena (multiply arena cdx ady)
        (multiply arena adx cdy)
    and abdet = subtract arena (multiply arena adx bdy)
        (multiply arena bdx ady) in
    let result = compare_zero arena
        (add arena (add arena (multiply arena alift bcdet)
          (multiply arena blift cadet)) (multiply arena clift abdet)) in
    release arena;
    result

  let compare_products left_first left_second right_first right_second =
    let arena = acquire () in
    let left_first = of_exact arena left_first
    and left_second = of_exact arena left_second
    and right_first = of_exact arena right_first
    and right_second = of_exact arena right_second in
    let result = compare_zero arena (subtract arena
        (multiply arena left_first left_second)
        (multiply arena right_first right_second)) in
    release arena;
    result

  let compare_homogeneous_float ~numerator ~denominator ~value =
    let arena = acquire () in
    let numerator = of_exact arena numerator
    and denominator = of_exact arena denominator
    and value = of_float arena value in
    let result = compare_zero arena
        (subtract arena numerator (multiply arena value denominator)) in
    release arena;
    result

  let orient2d_ee_homogeneous
      ~ax ~ay ~bx ~by ~cx ~cy ~cw =
    let arena = acquire () in
    let ax = of_float arena ax and ay = of_float arena ay
    and bx = of_float arena bx and by = of_float arena by
    and cx = of_exact arena cx and cy = of_exact arena cy
    and cw = of_exact arena cw in
    let acx = subtract arena (multiply arena ax cw) cx
    and acy = subtract arena (multiply arena ay cw) cy
    and bcx = subtract arena (multiply arena bx cw) cx
    and bcy = subtract arena (multiply arena by cw) cy in
    let result = compare_zero arena (subtract arena
        (multiply arena acx bcy) (multiply arena acy bcx)) in
    release arena;
    result

  let scaled_mantissa arena number =
    let count = arena.lengths.(number) in
    if count = 0 then 0.
    else begin
      let first = max 0 (count - 4) and offset = arena.offsets.(number) in
      let accumulator = ref 0. and scale = ref (1. /. float_of_int limb_base) in
      for index = count - 1 downto first do
        accumulator := !accumulator
            +. (float_of_int arena.limbs.(offset + index) *. !scale);
        scale := !scale /. float_of_int limb_base
      done;
      if arena.negative.(number) then -. !accumulator else !accumulator
    end

  let scaled_exponent arena number =
    arena.exponents.(number) + (arena.lengths.(number) * limb_bits)

  let approximate_ratio arena numerator denominator =
    if is_zero arena numerator then 0.
    else Float.ldexp
        (scaled_mantissa arena numerator /. scaled_mantissa arena denominator)
        (scaled_exponent arena numerator - scaled_exponent arena denominator)

  let barycentric_tri_homogeneous
      ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~px ~py ~pz ~pw =
    let arena = acquire () in
    let ax = of_float arena ax and ay = of_float arena ay and az = of_float arena az
    and bx = of_float arena bx and by = of_float arena by and bz = of_float arena bz
    and cx = of_float arena cx and cy = of_float arena cy and cz = of_float arena cz
    and px = of_exact arena px and py = of_exact arena py
    and pz = of_exact arena pz and pw = of_exact arena pw in
    let apx = subtract arena (multiply arena ax pw) px
    and apy = subtract arena (multiply arena ay pw) py
    and apz = subtract arena (multiply arena az pw) pz
    and bpx = subtract arena (multiply arena bx pw) px
    and bpy = subtract arena (multiply arena by pw) py
    and bpz = subtract arena (multiply arena bz pw) pz
    and cpx = subtract arena (multiply arena cx pw) px
    and cpy = subtract arena (multiply arena cy pw) py
    and cpz = subtract arena (multiply arena cz pw) pz in
    let coplanar = add arena
        (add arena
           (multiply arena apx (subtract arena
              (multiply arena bpy cpz) (multiply arena bpz cpy)))
           (multiply arena apy (subtract arena
              (multiply arena bpz cpx) (multiply arena bpx cpz))))
        (multiply arena apz (subtract arena
           (multiply arena bpx cpy) (multiply arena bpy cpx))) in
    if compare_zero arena coplanar <> 0 then begin
      release arena;
      invalid_arg "Rdk barycentric point is not coplanar with its source triangle"
    end;
    let weights af as_ bf bs cf cs pf ps =
      let base = subtract arena
          (multiply arena (subtract arena af cf) (subtract arena bs cs))
          (multiply arena (subtract arena as_ cs) (subtract arena bf cf)) in
      if is_zero arena base then None
      else begin
        let denominator = multiply arena base (multiply arena pw pw) in
        let oriented ux us vx vs = subtract arena
            (multiply arena (subtract arena (multiply arena ux pw) pf)
               (subtract arena (multiply arena vs pw) ps))
            (multiply arena (subtract arena (multiply arena us pw) ps)
               (subtract arena (multiply arena vx pw) pf)) in
        Some (approximate_ratio arena (oriented bf bs cf cs) denominator,
          approximate_ratio arena (oriented cf cs af as_) denominator,
          approximate_ratio arena (oriented af as_ bf bs) denominator)
      end in
    let result = match weights ax ay bx by cx cy px py with
      | Some result -> result
      | None ->
          (match weights ay az by bz cy cz py pz with
           | Some result -> result
           | None ->
               (match weights az ax bz bx cz cx pz px with
                | Some result -> result
                | None ->
                    release arena;
                    invalid_arg "Rdk barycentric source triangle is degenerate")) in
    release arena;
    result

  let homogeneous_difference arena coordinate weight other_coordinate other_weight =
    subtract arena (multiply arena coordinate other_weight)
      (multiply arena other_coordinate weight)

  let dot3 arena ax ay az bx by bz =
    add arena (add arena (multiply arena ax bx) (multiply arena ay by))
      (multiply arena az bz)

  let orient2d_homogeneous ~ax ~ay ~aw ~bx ~by ~bw ~cx ~cy ~cw =
    let arena = acquire () in
    let ax = of_exact arena ax and ay = of_exact arena ay and aw = of_exact arena aw
    and bx = of_exact arena bx and by = of_exact arena by and bw = of_exact arena bw
    and cx = of_exact arena cx and cy = of_exact arena cy and cw = of_exact arena cw in
    let acx = homogeneous_difference arena ax aw cx cw
    and acy = homogeneous_difference arena ay aw cy cw
    and bcx = homogeneous_difference arena bx bw cx cw
    and bcy = homogeneous_difference arena by bw cy cw in
    let result = compare_zero arena (subtract arena
        (multiply arena acx bcy) (multiply arena acy bcx)) in
    release arena;
    result

  let orient3d_homogeneous
      ~ax ~ay ~az ~aw ~bx ~by ~bz ~bw ~cx ~cy ~cz ~cw ~dx ~dy ~dz ~dw =
    let arena = acquire () in
    let ax = of_exact arena ax and ay = of_exact arena ay
    and az = of_exact arena az and aw = of_exact arena aw
    and bx = of_exact arena bx and by = of_exact arena by
    and bz = of_exact arena bz and bw = of_exact arena bw
    and cx = of_exact arena cx and cy = of_exact arena cy
    and cz = of_exact arena cz and cw = of_exact arena cw
    and dx = of_exact arena dx and dy = of_exact arena dy
    and dz = of_exact arena dz and dw = of_exact arena dw in
    let adx = homogeneous_difference arena ax aw dx dw
    and ady = homogeneous_difference arena ay aw dy dw
    and adz = homogeneous_difference arena az aw dz dw
    and bdx = homogeneous_difference arena bx bw dx dw
    and bdy = homogeneous_difference arena by bw dy dw
    and bdz = homogeneous_difference arena bz bw dz dw
    and cdx = homogeneous_difference arena cx cw dx dw
    and cdy = homogeneous_difference arena cy cw dy dw
    and cdz = homogeneous_difference arena cz cw dz dw in
    let first = multiply arena adx
        (subtract arena (multiply arena bdy cdz) (multiply arena bdz cdy))
    and second = multiply arena ady
        (subtract arena (multiply arena bdz cdx) (multiply arena bdx cdz))
    and third = multiply arena adz
        (subtract arena (multiply arena bdx cdy) (multiply arena bdy cdx)) in
    let result = compare_zero arena (add arena (add arena first second) third) in
    release arena;
    result

  let incircle_homogeneous
      ~ax ~ay ~aw ~bx ~by ~bw ~cx ~cy ~cw ~dx ~dy ~dw =
    let arena = acquire () in
    let ax = of_exact arena ax and ay = of_exact arena ay and aw = of_exact arena aw
    and bx = of_exact arena bx and by = of_exact arena by and bw = of_exact arena bw
    and cx = of_exact arena cx and cy = of_exact arena cy and cw = of_exact arena cw
    and dx = of_exact arena dx and dy = of_exact arena dy and dw = of_exact arena dw in
    let adx = homogeneous_difference arena ax aw dx dw
    and ady = homogeneous_difference arena ay aw dy dw
    and bdx = homogeneous_difference arena bx bw dx dw
    and bdy = homogeneous_difference arena by bw dy dw
    and cdx = homogeneous_difference arena cx cw dx dw
    and cdy = homogeneous_difference arena cy cw dy dw in
    let adenominator = multiply arena aw dw
    and bdenominator = multiply arena bw dw
    and cdenominator = multiply arena cw dw in
    let row_ax = multiply arena adx adenominator
    and row_ay = multiply arena ady adenominator
    and row_bx = multiply arena bdx bdenominator
    and row_by = multiply arena bdy bdenominator
    and row_cx = multiply arena cdx cdenominator
    and row_cy = multiply arena cdy cdenominator
    and row_alift = add arena (multiply arena adx adx) (multiply arena ady ady)
    and row_blift = add arena (multiply arena bdx bdx) (multiply arena bdy bdy)
    and row_clift = add arena (multiply arena cdx cdx) (multiply arena cdy cdy) in
    let first = multiply arena row_ax
        (subtract arena (multiply arena row_by row_clift)
          (multiply arena row_blift row_cy))
    and second = multiply arena row_ay
        (subtract arena (multiply arena row_blift row_cx)
          (multiply arena row_bx row_clift))
    and third = multiply arena row_alift
        (subtract arena (multiply arena row_bx row_cy)
          (multiply arena row_by row_cx)) in
    let result = compare_zero arena (add arena (add arena first second) third) in
    release arena;
    result

  let normal_dot_symbolic_homogeneous
      ~fx ~fy ~fz ~fw ~sx ~sy ~sz ~sw ~tx ~ty ~tz ~tw =
    let arena = acquire () in
    let fx = of_exact arena fx and fy = of_exact arena fy
    and fz = of_exact arena fz and fw = of_exact arena fw
    and sx = of_exact arena sx and sy = of_exact arena sy
    and sz = of_exact arena sz and sw = of_exact arena sw
    and tx = of_exact arena tx and ty = of_exact arena ty
    and tz = of_exact arena tz and tw = of_exact arena tw in
    let second_x = homogeneous_difference arena sx sw fx fw
    and second_y = homogeneous_difference arena sy sw fy fw
    and second_z = homogeneous_difference arena sz sw fz fw
    and third_x = homogeneous_difference arena tx tw fx fw
    and third_y = homogeneous_difference arena ty tw fy fw
    and third_z = homogeneous_difference arena tz tw fz fw in
    let cross_x = subtract arena (multiply arena second_y third_z)
        (multiply arena second_z third_y)
    and cross_y = subtract arena (multiply arena second_z third_x)
        (multiply arena second_x third_z)
    and cross_z = subtract arena (multiply arena second_x third_y)
        (multiply arena second_y third_x) in
    let result =
      let first = compare_zero arena cross_x in
      if first <> 0 then first else
      let second = compare_zero arena cross_y in
      if second <> 0 then second else compare_zero arena cross_z in
    release arena;
    result

  let directed_axis_sign arena axis positive x y z =
    let sign = compare_zero arena
        (if axis = 0 then x else if axis = 1 then y else if axis = 2 then z
         else invalid_arg "Rdk exact ray axis is out of bounds") in
    if positive then sign else -sign

  let axis_ray_tri_homogeneous
      ~axis ~positive
      ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~qx ~qy ~qz ~qw =
    let arena = acquire () in
    let ax = of_float arena ax and ay = of_float arena ay
    and az = of_float arena az and bx = of_float arena bx
    and by = of_float arena by and bz = of_float arena bz
    and cx = of_float arena cx and cy = of_float arena cy
    and cz = of_float arena cz and qx = of_exact arena qx
    and qy = of_exact arena qy and qz = of_exact arena qz
    and qw = of_exact arena qw and one = of_float arena 1. in
    let bax = subtract arena bx ax and bay = subtract arena by ay
    and baz = subtract arena bz az and cax = subtract arena cx ax
    and cay = subtract arena cy ay and caz = subtract arena cz az in
    let normal_x = subtract arena (multiply arena bay caz)
        (multiply arena baz cay)
    and normal_y = subtract arena (multiply arena baz cax)
        (multiply arena bax caz)
    and normal_z = subtract arena (multiply arena bax cay)
        (multiply arena bay cax) in
    let normal = directed_axis_sign arena axis positive
        normal_x normal_y normal_z in
    let result = if normal = 0 then 4 else begin
      let aqx = homogeneous_difference arena ax one qx qw
      and aqy = homogeneous_difference arena ay one qy qw
      and aqz = homogeneous_difference arena az one qz qw
      and bqx = homogeneous_difference arena bx one qx qw
      and bqy = homogeneous_difference arena by one qy qw
      and bqz = homogeneous_difference arena bz one qz qw
      and cqx = homogeneous_difference arena cx one qx qw
      and cqy = homogeneous_difference arena cy one qy qw
      and cqz = homogeneous_difference arena cz one qz qw in
      let edge_sign ux uy uz vx vy vz =
        let cross_x = subtract arena (multiply arena uy vz)
            (multiply arena uz vy)
        and cross_y = subtract arena (multiply arena uz vx)
            (multiply arena ux vz)
        and cross_z = subtract arena (multiply arena ux vy)
            (multiply arena uy vx) in
        directed_axis_sign arena axis positive cross_x cross_y cross_z in
      let first = edge_sign aqx aqy aqz bqx bqy bqz
      and second = edge_sign bqx bqy bqz cqx cqy cqz
      and third = edge_sign cqx cqy cqz aqx aqy aqz
      and plane = compare_zero arena
          (dot3 arena normal_x normal_y normal_z aqx aqy aqz) in
      let opposite left right = left <> 0 && right <> 0 && left <> right in
      if opposite first normal || opposite second normal
          || opposite third normal then 0
      else if plane = 0 then 3
      else if plane <> normal then 0
      else if first = 0 || second = 0 || third = 0 then 3
      else if normal < 0 then 1 else 2
    end in
    release arena;
    result

  let symbolic_ray_tri_homogeneous
      ~ax ~ay ~az ~bx ~by ~bz ~cx ~cy ~cz ~qx ~qy ~qz ~qw =
    let arena = acquire () in
    let ax = of_float arena ax and ay = of_float arena ay
    and az = of_float arena az and bx = of_float arena bx
    and by = of_float arena by and bz = of_float arena bz
    and cx = of_float arena cx and cy = of_float arena cy
    and cz = of_float arena cz and qx = of_exact arena qx
    and qy = of_exact arena qy and qz = of_exact arena qz
    and qw = of_exact arena qw and one = of_float arena 1. in
    let bax = subtract arena bx ax and bay = subtract arena by ay
    and baz = subtract arena bz az and cax = subtract arena cx ax
    and cay = subtract arena cy ay and caz = subtract arena cz az in
    let normal_x = subtract arena (multiply arena bay caz)
        (multiply arena baz cay)
    and normal_y = subtract arena (multiply arena baz cax)
        (multiply arena bax caz)
    and normal_z = subtract arena (multiply arena bax cay)
        (multiply arena bay cax) in
    let first_nonzero x y z =
      let first = compare_zero arena x in
      if first <> 0 then first else
      let second = compare_zero arena y in
      if second <> 0 then second else compare_zero arena z in
    let normal = first_nonzero normal_x normal_y normal_z in
    let result = if normal = 0 then 4 else begin
      let aqx = homogeneous_difference arena ax one qx qw
      and aqy = homogeneous_difference arena ay one qy qw
      and aqz = homogeneous_difference arena az one qz qw
      and bqx = homogeneous_difference arena bx one qx qw
      and bqy = homogeneous_difference arena by one qy qw
      and bqz = homogeneous_difference arena bz one qz qw
      and cqx = homogeneous_difference arena cx one qx qw
      and cqy = homogeneous_difference arena cy one qy qw
      and cqz = homogeneous_difference arena cz one qz qw in
      let edge_sign ux uy uz vx vy vz =
        let cross_x = subtract arena (multiply arena uy vz)
            (multiply arena uz vy)
        and cross_y = subtract arena (multiply arena uz vx)
            (multiply arena ux vz)
        and cross_z = subtract arena (multiply arena ux vy)
            (multiply arena uy vx) in
        first_nonzero cross_x cross_y cross_z in
      let first = edge_sign aqx aqy aqz bqx bqy bqz
      and second = edge_sign bqx bqy bqz cqx cqy cqz
      and third = edge_sign cqx cqy cqz aqx aqy aqz
      and plane = compare_zero arena
          (dot3 arena normal_x normal_y normal_z aqx aqy aqz) in
      let opposite left right = left <> 0 && right <> 0 && left <> right in
      if opposite first normal || opposite second normal
          || opposite third normal then 0
      else if plane = 0 || first = 0 || second = 0 || third = 0 then 3
      else if plane = normal then if normal < 0 then 1 else 2
      else 0
    end in
    release arena;
    result

  let radial_dot_homogeneous
      ~ex0 ~ey0 ~ez0 ~ew0 ~ex1 ~ey1 ~ez1 ~ew1
      ~lx ~ly ~lz ~lw ~rx ~ry ~rz ~rw =
    let arena = acquire () in
    let ex0 = of_exact arena ex0 and ey0 = of_exact arena ey0
    and ez0 = of_exact arena ez0 and ew0 = of_exact arena ew0
    and ex1 = of_exact arena ex1 and ey1 = of_exact arena ey1
    and ez1 = of_exact arena ez1 and ew1 = of_exact arena ew1
    and lx = of_exact arena lx and ly = of_exact arena ly
    and lz = of_exact arena lz and lw = of_exact arena lw
    and rx = of_exact arena rx and ry = of_exact arena ry
    and rz = of_exact arena rz and rw = of_exact arena rw in
    let edge_x = homogeneous_difference arena ex1 ew1 ex0 ew0
    and edge_y = homogeneous_difference arena ey1 ew1 ey0 ew0
    and edge_z = homogeneous_difference arena ez1 ew1 ez0 ew0
    and left_x = homogeneous_difference arena lx lw ex0 ew0
    and left_y = homogeneous_difference arena ly lw ey0 ew0
    and left_z = homogeneous_difference arena lz lw ez0 ew0
    and right_x = homogeneous_difference arena rx rw ex0 ew0
    and right_y = homogeneous_difference arena ry rw ey0 ew0
    and right_z = homogeneous_difference arena rz rw ez0 ew0 in
    let lr = dot3 arena left_x left_y left_z right_x right_y right_z
    and ee = dot3 arena edge_x edge_y edge_z edge_x edge_y edge_z
    and le = dot3 arena left_x left_y left_z edge_x edge_y edge_z
    and re = dot3 arena right_x right_y right_z edge_x edge_y edge_z in
    let result = compare_zero arena (subtract arena
        (multiply arena lr ee) (multiply arena le re)) in
    release arena;
    result
end
