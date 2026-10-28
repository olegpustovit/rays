type error_kind = Decoder_error | Wrong_domain | Invalid_argument | Incompatible_version
type error = { operation : string; kind : error_kind; message : string }

let pp_error formatter error = Format.fprintf formatter "%s: %s" error.operation error.message
let error operation kind message = Error { operation; kind; message }

type decoded = Sdl3.rgba = { width : int; height : int; pixels : bytes }

external linked_version_number : unit -> int = "caml_sdl3_image_version"
external compiled_version_number : unit -> int = "caml_sdl3_image_compiled_version"

external decode_bytes_raw : bytes -> string option -> (decoded, string) result
  = "caml_sdl3_image_decode_bytes"

let linked_version () : Sdl3.version =
  let number = linked_version_number () in
  { major = number / 1_000_000; minor = number / 1_000 mod 1_000; patch = number mod 1_000 }

(* The headers' own version macro, not a checked-in constant. *)
let compiled_version : Sdl3.version =
  let number = compiled_version_number () in
  { major = number / 1_000_000; minor = number / 1_000 mod 1_000; patch = number mod 1_000 }

let check_version ?(release = true) () =
  match
    Sdl3.validate_version ~library:"SDL3_image" ~compiled:compiled_version ~release
      ~linked:(linked_version ()) ()
  with
  | Ok () -> Ok ()
  | Error source -> error "SDL3_image.check_version" Incompatible_version source.message

let contains_nul value =
  try
    ignore (String.index value '\x00');
    true
  with Not_found -> false

let byte bytes offset = Bytes.get_uint8 bytes offset
let range bytes offset length = offset >= 0 && length >= 0 && offset <= Bytes.length bytes - length

let u16 bytes endian offset =
  if not (range bytes offset 2) then None
  else
    let first = byte bytes offset and second = byte bytes (offset + 1) in
    Some
      (match endian with `Little -> first lor (second lsl 8) | `Big -> (first lsl 8) lor second)

let u32 bytes endian offset =
  if not (range bytes offset 4) then None
  else
    let a = byte bytes offset
    and b = byte bytes (offset + 1)
    and c = byte bytes (offset + 2)
    and d = byte bytes (offset + 3) in
    let value =
      match endian with
      | `Little ->
          Int64.logor (Int64.of_int a)
            (Int64.logor
               (Int64.shift_left (Int64.of_int b) 8)
               (Int64.logor
                  (Int64.shift_left (Int64.of_int c) 16)
                  (Int64.shift_left (Int64.of_int d) 24)))
      | `Big ->
          Int64.logor
            (Int64.shift_left (Int64.of_int a) 24)
            (Int64.logor
               (Int64.shift_left (Int64.of_int b) 16)
               (Int64.logor (Int64.shift_left (Int64.of_int c) 8) (Int64.of_int d)))
    in
    if value > Int64.of_int max_int then None else Some (Int64.to_int value)

let exif_orientation bytes offset length =
  if
    length < 14
    || (not (range bytes offset length))
    || Bytes.sub_string bytes offset 6 <> "Exif\x00\x00"
  then None
  else
    let tiff = offset + 6 in
    let endian =
      match (byte bytes tiff, byte bytes (tiff + 1)) with
      | 0x49, 0x49 -> Some `Little
      | 0x4d, 0x4d -> Some `Big
      | _ -> None
    in
    match endian with
    | None -> None
    | Some endian -> (
        match (u16 bytes endian (tiff + 2), u32 bytes endian (tiff + 4)) with
        | Some 42, Some directory_offset when directory_offset <= length - 8 -> (
            let directory = tiff + directory_offset in
            match u16 bytes endian directory with
            | None -> None
            | Some count ->
                let rec entry index =
                  if index >= count then None
                  else
                    let position = directory + 2 + (index * 12) in
                    if (not (range bytes position 12)) || position + 12 > offset + length then None
                    else
                      match
                        ( u16 bytes endian position,
                          u16 bytes endian (position + 2),
                          u32 bytes endian (position + 4),
                          u16 bytes endian (position + 8) )
                      with
                      | Some 0x0112, Some 3, Some 1, Some orientation
                        when orientation >= 1 && orientation <= 8 ->
                          Some orientation
                      | _ -> entry (index + 1)
                in
                entry 0)
        | _ -> None)

let jpeg_orientation bytes =
  if (not (range bytes 0 4)) || byte bytes 0 <> 0xff || byte bytes 1 <> 0xd8 then 1
  else
    let rec marker offset =
      if not (range bytes offset 4) then 1
      else if byte bytes offset <> 0xff then marker (offset + 1)
      else
        let rec marker_code position =
          if not (range bytes position 1) then None
          else if byte bytes position = 0xff then marker_code (position + 1)
          else Some (position, byte bytes position)
        in
        match marker_code (offset + 1) with
        | None -> 1
        | Some (_, (0xd9 | 0xda)) -> 1
        | Some (position, code) ->
            let size_offset = position + 1 in
            if not (range bytes size_offset 2) then 1
            else
              let size = (byte bytes size_offset lsl 8) lor byte bytes (size_offset + 1) in
              if size < 2 || not (range bytes (size_offset + 2) (size - 2)) then 1
              else if code = 0xe1 then
                match exif_orientation bytes (size_offset + 2) (size - 2) with
                | Some orientation -> orientation
                | None -> marker (size_offset + size)
              else marker (size_offset + size)
    in
    marker 2

let orient (decoded : decoded) orientation =
  if orientation = 1 then decoded
  else
    let source_width = decoded.width and source_height = decoded.height in
    let width, height =
      if orientation >= 5 then (source_height, source_width) else (source_width, source_height)
    in
    let pixels = Bytes.create (Bytes.length decoded.pixels) in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let source_x, source_y =
          match orientation with
          | 2 -> (source_width - 1 - x, y)
          | 3 -> (source_width - 1 - x, source_height - 1 - y)
          | 4 -> (x, source_height - 1 - y)
          | 5 -> (y, x)
          | 6 -> (y, source_height - 1 - x)
          | 7 -> (source_width - 1 - y, source_height - 1 - x)
          | 8 -> (source_width - 1 - y, x)
          | _ -> (x, y)
        in
        let source = ((source_y * source_width) + source_x) * 4 in
        let target = ((y * width) + x) * 4 in
        Bytes.unsafe_set pixels target (Bytes.unsafe_get decoded.pixels source);
        Bytes.unsafe_set pixels (target + 1) (Bytes.unsafe_get decoded.pixels (source + 1));
        Bytes.unsafe_set pixels (target + 2) (Bytes.unsafe_get decoded.pixels (source + 2));
        Bytes.unsafe_set pixels (target + 3) (Bytes.unsafe_get decoded.pixels (source + 3))
      done
    done;
    { width; height; pixels }

let decode operation ?kind bytes =
  match decode_bytes_raw bytes kind with
  | Error message -> error operation Decoder_error message
  | Ok decoded ->
      let source_orientation_number = jpeg_orientation bytes in
      Ok (orient decoded source_orientation_number)

let read_file operation path =
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> Ok (Bytes.of_string (really_input_string channel (in_channel_length channel))))
  with
  | Sys_error message -> error operation Decoder_error message
  | End_of_file -> error operation Decoder_error "image file changed while it was being read"

let kind_of_path path =
  match String.lowercase_ascii (Filename.extension path) with
  | ".ani" -> Some "ANI"
  | ".avif" | ".avifs" -> Some "AVIF"
  | ".bmp" -> Some "BMP"
  | ".cur" -> Some "CUR"
  | ".gif" -> Some "GIF"
  | ".ico" -> Some "ICO"
  | ".jpg" | ".jpeg" | ".jpe" -> Some "JPG"
  | ".jxl" -> Some "JXL"
  | ".lbm" | ".iff" -> Some "LBM"
  | ".pcx" -> Some "PCX"
  | ".png" -> Some "PNG"
  | ".pnm" | ".pbm" | ".pgm" | ".ppm" -> Some "PNM"
  | ".qoi" -> Some "QOI"
  | ".svg" | ".svgz" -> Some "SVG"
  | ".tga" -> Some "TGA"
  | ".tif" | ".tiff" -> Some "TIF"
  | ".webp" -> Some "WEBP"
  | ".xcf" -> Some "XCF"
  | ".xpm" -> Some "XPM"
  | ".xv" -> Some "XV"
  | _ -> None

let on_main operation callback =
  if (not (Sdl3.Thread.is_initial_domain ())) || not (Sdl3.Thread.is_sdl_main_thread ()) then
    error operation Wrong_domain
      "SDL3_image decoding must run on the initial OCaml domain and SDL main thread"
  else match check_version () with Error _ as failure -> failure | Ok () -> callback ()

let load_file path =
  let operation = "SDL3_image.load_file" in
  if contains_nul path then error operation Invalid_argument "image path contains a NUL byte"
  else
    on_main operation (fun () ->
        match read_file operation path with
        | Error _ as failure -> failure
        | Ok bytes -> decode operation ?kind:(kind_of_path path) bytes)
