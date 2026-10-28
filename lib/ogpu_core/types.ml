type buffer_usage = Copy_src | Copy_dst | Uniform | Storage | Vertex | Index
type memory = Shared | Device_local
type buffer_descriptor = { label : string option; size : int64; usage : buffer_usage list }

type texture_usage =
  | Texture_binding
  | Storage_binding
  | Render_attachment
  | Texture_copy_src
  | Texture_copy_dst

type texture_format =
  | R8_unorm
  | Rgba8_unorm
  | Bgra8_unorm
  | Bgra8_unorm_srgb
  | Rgba16_float
  | Rgba32_float
  | Depth32_float

type texture_descriptor = {
  label : string option;
  width : int;
  height : int;
  depth : int;
  mip_levels : int;
  sample_count : int;
  format : texture_format;
  usage : texture_usage list;
}

let texel_bytes = function
  | R8_unorm -> 1
  | Rgba8_unorm | Bgra8_unorm | Bgra8_unorm_srgb | Depth32_float -> 4
  | Rgba16_float -> 8
  | Rgba32_float -> 16

(* Texel regions for blit copies. *)
type origin = { x : int; y : int; z : int }
type extent = { width : int; height : int; depth : int }
type sampler_filter = Nearest | Linear
type mip_filter = No_mip | Nearest_mip | Linear_mip
type address_mode = Clamp_to_edge | Repeat | Mirror_repeat

type sampler_descriptor = {
  label : string option;
  min_filter : sampler_filter;
  mag_filter : sampler_filter;
  mip_filter : mip_filter;
  address_u : address_mode;
  address_v : address_mode;
  lod_min : float;
  lod_max : float;
  max_anisotropy : int;
}

(* IEEE 754 binary16 with round-to-nearest-even; NaN maps to a quiet NaN.
   Integer arithmetic on the double's bits: no allocation, since World maps
   convert millions of channels. A mantissa carry rolls into the exponent
   (and past 65504 into infinity) by plain addition. *)
let half_of_float x =
  let bits = Int64.bits_of_float x in
  let sign = if Int64.compare bits 0L < 0 then 0x8000 else 0 in
  let e = Int64.to_int (Int64.shift_right_logical bits 52) land 0x7ff
  and m = Int64.to_int (Int64.logand bits 0xfffffffffffffL) in
  let round v shift =
    let q = v lsr shift and rest = v land ((1 lsl shift) - 1) and half = 1 lsl (shift - 1) in
    if rest > half || (rest = half && q land 1 = 1) then q + 1 else q
  in
  if e = 0x7ff then sign lor if m = 0 then 0x7c00 else 0x7e00
  else if e - 1008 >= 31 then sign lor 0x7c00
  else if e - 1008 > 0 then sign lor (((e - 1008) lsl 10) + round m 42)
  else if 1051 - e >= 54 then sign
  else sign lor round (m lor (1 lsl 52)) (1051 - e)
