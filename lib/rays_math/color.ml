(* Color Module - RGBA Color System *)

(* Color type with 8-bit per channel (0-255) *)
type t = { r : int; g : int; b : int; a : int }

(* Helper function to clamp values to 0-255 range *)
let clamp_byte x = max 0 (min 255 x)

(* Constructors *)
let rgb r g b = { r = clamp_byte r; g = clamp_byte g; b = clamp_byte b; a = 255 }
let rgba r g b a = { r = clamp_byte r; g = clamp_byte g; b = clamp_byte b; a = clamp_byte a }

let of_unit_floats ?(alpha = 1.) r g b =
  let byte value = clamp_byte (int_of_float (Float.round (value *. 255.))) in
  rgba (byte r) (byte g) (byte b) (byte alpha)

let gray x =
  let clamped = clamp_byte x in
  { r = clamped; g = clamped; b = clamped; a = 255 }

let hex value =
  let value =
    if String.length value > 0 && value.[0] = '#' then String.sub value 1 (String.length value - 1)
    else value
  in
  let expand_short value = String.init (String.length value * 2) (fun index -> value.[index / 2]) in
  let value = match String.length value with 3 | 4 -> expand_short value | _ -> value in
  if String.length value <> 6 && String.length value <> 8 then
    Error "Color.hex: expected #rgb, #rgba, #rrggbb, or #rrggbbaa"
  else
    try
      let channel offset = int_of_string ("0x" ^ String.sub value offset 2) in
      let alpha = if String.length value = 8 then channel 6 else 255 in
      Ok (rgba (channel 0) (channel 2) (channel 4) alpha)
    with Failure _ -> Error "Color.hex: invalid hexadecimal digit"

let hex_exn value = match hex value with Ok color -> color | Error message -> invalid_arg message
let clamp_unit value = max 0. (min 1. value)

let wrap_hue hue =
  let wrapped = mod_float hue 360. in
  if wrapped < 0. then wrapped +. 360. else wrapped

let byte value = int_of_float ((clamp_unit value *. 255.) +. 0.5)

let hsl ?(alpha = 1.) hue saturation lightness =
  let hue = wrap_hue hue /. 60. in
  let saturation = clamp_unit saturation in
  let lightness = clamp_unit lightness in
  let chroma = (1. -. abs_float ((2. *. lightness) -. 1.)) *. saturation in
  let x = chroma *. (1. -. abs_float (mod_float hue 2. -. 1.)) in
  let r, g, b =
    if hue < 1. then (chroma, x, 0.)
    else if hue < 2. then (x, chroma, 0.)
    else if hue < 3. then (0., chroma, x)
    else if hue < 4. then (0., x, chroma)
    else if hue < 5. then (x, 0., chroma)
    else (chroma, 0., x)
  in
  let m = lightness -. (chroma /. 2.) in
  rgba (byte (r +. m)) (byte (g +. m)) (byte (b +. m)) (byte alpha)

let hsl_channels hue saturation lightness =
  let modulo x y = Float.rem (Float.rem x y +. y) y
  and unit value = Float.max 0. (Float.min 1. value) in
  let hue = modulo hue 360. /. 60.
  and saturation = unit saturation
  and lightness = unit lightness in
  let chroma = (1. -. Float.abs ((2. *. lightness) -. 1.)) *. saturation in
  let x = chroma *. (1. -. Float.abs (modulo hue 2. -. 1.)) in
  let r, g, b =
    if hue < 1. then (chroma, x, 0.)
    else if hue < 2. then (x, chroma, 0.)
    else if hue < 3. then (0., chroma, x)
    else if hue < 4. then (0., x, chroma)
    else if hue < 5. then (x, 0., chroma)
    else (chroma, 0., x)
  in
  let m = lightness -. (chroma /. 2.) in
  let byte value = float_of_int (int_of_float ((unit (value +. m) *. 255.) +. 0.5)) /. 255. in
  (byte r, byte g, byte b)

let hsv hue saturation value =
  let hue = Float.rem (Float.rem hue 1. +. 1.) 1. in
  let h6 = Sys.opaque_identity (hue *. 6.) in
  let sector = int_of_float (Float.floor h6) in
  let fraction = h6 -. float_of_int sector in
  let p = value *. (1. -. saturation)
  and q = value *. (1. -. Sys.opaque_identity (fraction *. saturation))
  and t = value *. (1. -. Sys.opaque_identity ((1. -. fraction) *. saturation)) in
  match sector mod 6 with
  | 0 -> (value, t, p)
  | 1 -> (q, value, p)
  | 2 -> (p, value, t)
  | 3 -> (p, q, value)
  | 4 -> (t, p, value)
  | _ -> (value, p, q)

let aces value = value *. ((2.51 *. value) +. 0.03) /. ((value *. ((2.43 *. value) +. 0.59)) +. 0.14)

let to_hex_string color =
  if color.a = 255 then Printf.sprintf "#%02x%02x%02x" color.r color.g color.b
  else Printf.sprintf "#%02x%02x%02x%02x" color.r color.g color.b color.a

module Linear = struct
  type t = { r : float; g : float; b : float }

  let rgb r g b = { r; g; b }
end

(* Predefined Colors *)
let black = { r = 0; g = 0; b = 0; a = 255 }
let white = { r = 255; g = 255; b = 255; a = 255 }
let red = { r = 255; g = 0; b = 0; a = 255 }

(* Additional useful colors *)
let transparent = { r = 0; g = 0; b = 0; a = 0 }

(* Color Operations *)
let with_alpha c a = { c with a = clamp_byte a }
let to_tuple c = (c.r, c.g, c.b, c.a)

let blend c1 c2 ~pct =
  let pct = max 0.0 (min 1.0 pct) in
  (* clamp pct to [0,1] *)
  let inv_pct = 1.0 -. pct in
  {
    r = int_of_float ((float_of_int c1.r *. inv_pct) +. (float_of_int c2.r *. pct) +. 0.5);
    g = int_of_float ((float_of_int c1.g *. inv_pct) +. (float_of_int c2.g *. pct) +. 0.5);
    b = int_of_float ((float_of_int c1.b *. inv_pct) +. (float_of_int c2.b *. pct) +. 0.5);
    a = int_of_float ((float_of_int c1.a *. inv_pct) +. (float_of_int c2.a *. pct) +. 0.5);
  }

let gradient colors position =
  match colors with
  | [] -> invalid_arg "Color.gradient: empty palette"
  | [ color ] -> color
  | colors ->
      let colors = Array.of_list colors in
      let position = clamp_unit position *. float (Array.length colors - 1) in
      let left = min (Array.length colors - 2) (int_of_float position) in
      blend colors.(left) colors.(left + 1) ~pct:(position -. float left)

(* Utility functions *)

let to_string c = Printf.sprintf "Color(r=%d, g=%d, b=%d, a=%d)" c.r c.g c.b c.a
let to_hex c = (c.r lsl 16) lor (c.g lsl 8) lor c.b

(* Additional constructors for convenience *)
let of_floats r g b a =
  rgba
    (int_of_float ((r *. 255.0) +. 0.5))
    (int_of_float ((g *. 255.0) +. 0.5))
    (int_of_float ((b *. 255.0) +. 0.5))
    (int_of_float ((a *. 255.0) +. 0.5))

let to_floats c =
  ( float_of_int c.r /. 255.0,
    float_of_int c.g /. 255.0,
    float_of_int c.b /. 255.0,
    float_of_int c.a /. 255.0 )
