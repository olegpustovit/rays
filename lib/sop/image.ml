type storage = Floats of float array | Rgba8 of bytes
type t = {width : int; height : int; storage : storage; data_id : int}
let width image = image.width
let height image = image.height
let data_id image = image.data_id
let float_samples image = match image.storage with
  | Floats rgba -> rgba
  | Rgba8 bytes -> Array.init (Bytes.length bytes) (fun i -> float (Char.code (Bytes.get bytes i)) /. 255.)
let payload_bytes image = match image.storage with
  | Floats rgba -> Array.length rgba * 8
  | Rgba8 bytes -> Bytes.length bytes
  let storage = float_samples
  let rgba8 image = match image.storage with Rgba8 bytes -> Some bytes | Floats _ -> None
  let error message = Error (Diagnostic.error ~code:"E_IMAGE" message)
  let dimensions ~width ~height ~limit =
    if width <= 0 || height <= 0 then error "Image dimensions must be positive."
    else if width > limit / 4 / height then error "Image dimensions exceed native storage bounds."
    else Ok ()
  let of_owned_rgba8 ~width ~height bytes =
    Result.bind (dimensions ~width ~height ~limit:Sys.max_string_length) (fun () ->
      if Bytes.length bytes <> width * height * 4 then
        error "Image RGBA8 storage must contain four bytes per pixel."
      else Ok {width; height; storage=Rgba8 bytes; data_id=Rdk.Data_id.fresh ()})
  let of_owned_rgba ~width ~height rgba =
    Result.bind (dimensions ~width ~height ~limit:Sys.max_floatarray_length) (fun () ->
    if Array.length rgba <> width * height * 4 then
      error "Image RGBA storage must contain four samples per pixel."
    else if not (Array.for_all (fun sample -> Float.is_finite sample && sample >= 0. && sample <= 1.) rgba) then
      error "Image RGBA samples must be finite and within [0,1]."
    else Ok {width; height; storage=Floats rgba; data_id=Rdk.Data_id.fresh ()})
  let of_vec4 ~context ~width ~height rgba =
    Result.bind (dimensions ~width ~height ~limit:(min Sys.max_string_length Sys.max_floatarray_length)) (fun () ->
    if Array.length rgba <> width * height * 4 then
      error "Image kernel must return four channels per pixel."
    else if Context.cancelled context then Error (Diagnostic.error ~code:"E_CANCELLED" "Image kernel cancelled.")
    else
      let bytes = Bytes.create (Array.length rgba) in
      let count = width * height and grain = 16_384 in
      let invalid = Atomic.make false in
      let chunk k =
        if not (Context.cancelled context) then
          for i = k * grain * 4 to min (Array.length rgba) ((k + 1) * grain * 4) - 1 do
            let sample = rgba.(i) in
            if not (Float.is_finite sample) then Atomic.set invalid true
            else begin
              let q = Float.max 0. (Float.min 1. sample) *. 255. in
              let n = int_of_float (Float.floor q) in
              let fraction = q -. float n in
              let n = if fraction > 0.5 || (fraction = 0.5 && n land 1 = 1) then n + 1 else n in
              Bytes.set bytes i (Char.chr n)
            end
          done in
      let last = (count - 1) / grain in
      if count < 2 * grain then for k = 0 to last do chunk k done
      else Parallel.for_ ~chunk_size:1 ~start:0 ~finish:last chunk;
      if Context.cancelled context then Error (Diagnostic.error ~code:"E_CANCELLED" "Image kernel cancelled.")
      else if Atomic.get invalid then error "Image kernel channels must be finite."
      else of_owned_rgba8 ~width ~height bytes)
let create ~width ~height ~rgba =
  Result.map (fun image -> {image with storage=Floats (Array.copy rgba)})
    (of_owned_rgba ~width ~height rgba)
