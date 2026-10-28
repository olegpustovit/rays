open Sdl3
open Sdl3_image

let fail message = failwith ("SDL3_image test: " ^ message)

let get_image = function
  | Ok value -> value
  | Error error -> fail (Format.asprintf "%a" Sdl3_image.pp_error error)

let get_sdl = function
  | Ok value -> value
  | Error error -> fail (Format.asprintf "%a" Sdl3.pp_error error)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> Bytes.of_string (really_input_string channel (in_channel_length channel)))

let write_file path bytes =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_bytes channel bytes)

let fixture root name = Filename.concat root name

type fixture = { file : string; width : int; height : int }

(* Every still format the distribution enables. *)
let fixtures =
  [
    { file = "sample.avif"; width = 23; height = 42 };
    { file = "sample.bmp"; width = 23; height = 42 };
    { file = "sample.cur"; width = 23; height = 42 };
    { file = "palette.gif"; width = 23; height = 42 };
    { file = "sample.ico"; width = 23; height = 42 };
    { file = "sample.jpg"; width = 23; height = 42 };
    { file = "sample.jxl"; width = 23; height = 42 };
    { file = "sample.lbm"; width = 16; height = 1 };
    { file = "sample.pcx"; width = 23; height = 42 };
    { file = "sample.png"; width = 23; height = 42 };
    { file = "sample.pnm"; width = 23; height = 42 };
    { file = "sample.qoi"; width = 23; height = 42 };
    { file = "svg.svg"; width = 32; height = 32 };
    { file = "sample.tga"; width = 23; height = 42 };
    { file = "sample.tif"; width = 23; height = 42 };
    { file = "sample.webp"; width = 23; height = 42 };
    { file = "sample.xcf"; width = 23; height = 42 };
    { file = "sample.xpm"; width = 23; height = 42 };
    { file = "sample.xv"; width = 2; height = 1 };
  ]

let checked_rgba (rgba : Sdl3.rgba) ~width ~height =
  if rgba.width <> width || rgba.height <> height || Bytes.length rgba.pixels <> width * height * 4
  then
    fail
      (Printf.sprintf "decoded %dx%d with %d bytes, expected tightly packed %dx%d RGBA8" rgba.width
         rgba.height (Bytes.length rgba.pixels) width height);
  rgba

let check_fixture root value =
  let path = fixture root value.file in
  let decoded =
    match load_file path with
    | Ok rgba -> rgba
    | Error error -> fail (Format.asprintf "%s decode: %a" value.file Sdl3_image.pp_error error)
  in
  ignore (checked_rgba decoded ~width:value.width ~height:value.height);
  (* each decode owns its buffer: scribbling on one never shows in the next *)
  let first = Bytes.copy decoded.pixels in
  Bytes.fill decoded.pixels 0 (Bytes.length decoded.pixels) '\xAA';
  let again = get_image (load_file path) in
  if again.pixels != decoded.pixels && again.pixels <> first then
    fail (value.file ^ " decoded differently the second time");
  if again.pixels == decoded.pixels then fail (value.file ^ " shared a buffer between two decodes")

let pixel pixels width x y =
  let offset = ((y * width) + x) * 4 in
  Bytes.sub pixels offset 4

(* A reload that fails leaves the caller's previous pixels alone; one that
   succeeds replaces them whole. *)
let check_reload root =
  let path = Filename.temp_file "rays-sdl3-image-watch-" ".png" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      write_file path (read_file (fixture root "sample.png"));
      let current = ref (get_image (load_file path)) in
      let valid = Bytes.copy !current.pixels in
      write_file path (Bytes.of_string "malformed replacement");
      (match load_file path with
      | Error { kind = Decoder_error; message; _ } when message <> "" -> ()
      | Ok _ | Error _ -> fail "malformed reload returned the wrong result");
      if !current.pixels <> valid then fail "a failed reload changed the old pixels";
      write_file path (read_file (fixture root "rgbrgb.png"));
      current := get_image (load_file path);
      if (!current.width, !current.height) <> (256, 256) then
        fail "a successful reload did not replace the content")

let () =
  if Array.length Sys.argv <> 2 || not (Sys.file_exists Sys.argv.(1)) then
    fail "expected the SDL3_image fixture directory";
  let root = Sys.argv.(1) in
  let triple (v : Sdl3.version) = (v.major, v.minor, v.patch) in
  (match
     Sdl3_lock.check_installed
       ~lock:(Sdl3_lock.read "../../packaging/sdl3.lock")
       ~key:"sdl3_image" ~compiled:(triple compiled_version)
       ~linked:(triple (linked_version ()))
   with
  | Ok () -> ()
  | Error message -> fail message);
  (match check_version ~release:true () with
  | Ok () -> ()
  | Error error -> fail (Format.asprintf "%a" pp_error error));
  get_sdl (Init.init [ Init.Events ]);
  List.iter (check_fixture root) fixtures;

  let alpha = checked_rgba (get_image (load_file (fixture root "alpha.svg"))) ~width:2 ~height:1 in
  let saw_transparent = ref false and saw_opaque = ref false in
  for index = 3 to Bytes.length alpha.pixels - 1 do
    if index mod 4 = 3 then begin
      let value = Bytes.get_uint8 alpha.pixels index in
      if value < 255 then saw_transparent := true;
      if value = 255 then saw_opaque := true
    end
  done;
  if not (!saw_transparent && !saw_opaque) then
    fail "alpha fixture lost transparent or opaque pixels";

  let source =
    checked_rgba (get_image (load_file (fixture root "sample.jpg"))) ~width:23 ~height:42
  in
  let rotated =
    checked_rgba (get_image (load_file (fixture root "orientation-6.jpg"))) ~width:42 ~height:23
  in
  for y = 0 to 22 do
    for x = 0 to 41 do
      if pixel rotated.pixels 42 x y <> pixel source.pixels 23 y (41 - x) then
        fail "EXIF orientation-6 pixel mapping changed"
    done
  done;

  (* malformed input is a typed error for every decoder the extension picks *)
  List.iter
    (fun value ->
      let path = Filename.temp_file "rays-sdl3-image-bad-" (Filename.extension value.file) in
      Fun.protect
        ~finally:(fun () -> Sys.remove path)
        (fun () ->
          write_file path (Bytes.of_string "not an image");
          match load_file path with
          | Error { kind = Decoder_error; message; _ } when message <> "" -> ()
          | Ok _ -> fail (value.file ^ " malformed bytes decoded")
          | Error _ -> fail (value.file ^ " malformed bytes returned wrong error")))
    fixtures;
  (match load_file "/definitely/missing/rays-image.png" with
  | Error { kind = Decoder_error; message; _ } when message <> "" -> ()
  | Ok _ -> fail "missing image loaded"
  | Error _ -> fail "missing image returned the wrong error");
  (match load_file "bad\x00path" with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok _ -> fail "NUL path loaded"
  | Error _ -> fail "NUL path returned the wrong error");
  (match Domain.spawn (fun () -> load_file (fixture root "sample.png")) |> Domain.join with
  | Error { kind = Wrong_domain; _ } -> ()
  | Ok _ -> fail "wrong-domain decode succeeded"
  | Error _ -> fail "wrong-domain decode returned the wrong error");
  check_reload root;

  for _ = 1 to 10_000 do
    ignore (get_image (load_file (fixture root "sample.png")))
  done;
  get_sdl (Init.quit_subsystems [ Init.Events ]);
  Printf.printf "SDL3_image %d.%d.%d CPU decode conformance passed (%d formats)\n%!"
    (linked_version ()).major (linked_version ()).minor (linked_version ()).patch
    (List.length fixtures)
