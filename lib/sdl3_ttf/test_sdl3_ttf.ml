open Sdl3
open Sdl3_ttf

let fail message = failwith ("SDL3_ttf test: " ^ message)

let get_ttf = function
  | Ok value -> value
  | Error error -> fail (Format.asprintf "%a" Sdl3_ttf.pp_error error)

let has_visible_alpha pixels =
  let rec loop index =
    index < Bytes.length pixels && (Char.code (Bytes.get pixels index) <> 0 || loop (index + 4))
  in
  Bytes.length pixels >= 4 && loop 3

let run () =
  if Array.length Sys.argv <> 2 then fail "unexpected command-line arguments";
  let font_path = get_ttf (Font.system_path ()) in
  if not (Sys.file_exists font_path) then
    fail "installed system font discovery returned a missing path";
  let triple (v : Sdl3.version) = (v.major, v.minor, v.patch) in
  (match
     Sdl3_lock.check_installed
       ~lock:(Sdl3_lock.read "../../packaging/sdl3.lock")
       ~key:"sdl3_ttf" ~compiled:(triple compiled_version)
       ~linked:(triple (linked_version ()))
   with
  | Ok () -> ()
  | Error message -> fail message);
  (match check_version ~release:true () with
  | Ok () -> ()
  | Error error -> fail (Format.asprintf "%a" pp_error error));
  (match Font.open_file ~path:font_path ~size:18. with
  | Error { kind = Not_initialized; _ } -> ()
  | Ok font ->
      ignore (Font.destroy font);
      fail "font opened before TTF init"
  | Error _ -> fail "pre-init font open returned the wrong error");
  get_ttf (Init.init ());
  if not (get_ttf (Init.initialized ())) then fail "TTF initialization was not retained";
  (match Domain.spawn Init.initialized |> Domain.join with
  | Error { kind = Wrong_domain; _ } -> ()
  | Ok _ | Error _ -> fail "wrong-domain TTF init query was not rejected");
  (match Font.open_file ~path:font_path ~size:nan with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok font ->
      ignore (Font.destroy font);
      fail "NaN font size was accepted"
  | Error _ -> fail "NaN font size returned the wrong error");
  (match Font.open_file ~path:"" ~size:18. with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok font ->
      ignore (Font.destroy font);
      fail "empty font path opened"
  | Error _ -> fail "empty font path returned the wrong error");
  (match Font.open_file ~path:"/definitely/missing/font.ttf" ~size:18. with
  | Error ({ kind = Ttf_error; message; _ } as captured) when message <> "" ->
      let original = captured.message in
      ignore (linked_version ());
      if captured.message <> original then
        fail "TTF error text changed after a subsequent native call"
  | Ok font ->
      ignore (Font.destroy font);
      fail "missing font opened"
  | Error _ -> fail "missing font returned the wrong error");

  let font = get_ttf (Font.open_file ~path:font_path ~size:18.) in
  let metrics = get_ttf (Font.metrics font) in
  if metrics.height <= 0 || metrics.ascent <= 0 || metrics.line_skip <= 0 then
    fail "font metrics are not positive";
  (match Font.render_blended font ~color:(256, 0, 0, 255) "bad" with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok _ | Error _ -> fail "out-of-range text color was accepted");
  (match Font.render_blended font ~color:(0, 0, 0, 255) "bad\x00text" with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok _ | Error _ -> fail "NUL raster text was accepted");
  (match get_ttf (Font.render_blended font ~color:(12, 34, 56, 200) "") with
  | None -> ()
  | Some _ -> fail "empty text allocated a raster");
  let rgba =
    match get_ttf (Font.render_blended font ~color:(12, 34, 56, 200) "Rays ž") with
    | Some rgba -> rgba
    | None -> fail "non-empty text did not rasterize"
  in
  if
    rgba.width <= 0 || rgba.height <= 0
    || Bytes.length rgba.pixels <> rgba.width * rgba.height * 4
    || not (has_visible_alpha rgba.pixels)
  then fail "CPU glyph rasterization disagrees with text metrics or alpha";
  (* every render owns its buffer *)
  let again =
    match get_ttf (Font.render_blended font ~color:(12, 34, 56, 200) "Rays ž") with
    | Some rgba -> rgba
    | None -> fail "second raster of the same text failed"
  in
  if again.pixels == rgba.pixels || again.pixels <> rgba.pixels then
    fail "two renders of one text share a buffer or differ";

  (* Setters used by the runtime and glyph metrics. *)
  get_ttf (Font.set_hinting font Font.Mono_hinting);
  get_ttf (Font.set_hinting font Font.Normal_hinting);
  let glyph = get_ttf (Font.glyph_metrics font (Char.code 'A')) in
  if glyph.advance <= 0 || glyph.max_x < glyph.min_x || glyph.max_y < glyph.min_y then
    fail "ASCII glyph metrics are invalid";
  List.iter
    (fun codepoint ->
      match Font.glyph_metrics font codepoint with
      | Error { kind = Invalid_argument; _ } -> ()
      | Ok _ | Error _ -> fail "invalid Unicode scalar was accepted")
    [ -1; 0xd800; 0x110000 ];
  get_ttf (Font.set_wrap_alignment font Font.Center);
  let wrapped =
    match
      get_ttf
        (Font.render_blended_wrapped font ~color:(200, 180, 160, 255) ~wrap_width:80
           "a deterministic wrapped line with several words")
    with
    | Some rgba -> rgba
    | None -> fail "wrapped non-empty text did not rasterize"
  in
  if
    wrapped.width <= 0 || wrapped.width > 80
    || wrapped.height <= metrics.line_skip
    || Bytes.length wrapped.pixels <> wrapped.width * wrapped.height * 4
    || not (has_visible_alpha wrapped.pixels)
  then fail "wrapped raster disagrees with wrapped metrics";
  (match
     Font.render_blended_wrapped font ~color:(0, 0, 0, 255) ~wrap_width:80 "bad\xed\xa0\x80"
   with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok _ | Error _ -> fail "malformed wrapped UTF-8 was accepted");

  (* density: the same text at 144 DPI is about twice the size of 72 DPI *)
  get_ttf (Font.set_size_dpi font ~size:18. ~horizontal:72 ~vertical:72);
  let one_x =
    match get_ttf (Font.render_blended font ~color:(12, 34, 56, 200) "Density") with
    | Some rgba -> rgba
    | None -> fail "72-DPI density raster was empty"
  in
  get_ttf (Font.set_size_dpi font ~size:18. ~horizontal:144 ~vertical:144);
  let two_x =
    match get_ttf (Font.render_blended font ~color:(12, 34, 56, 200) "Density") with
    | Some rgba -> rgba
    | None -> fail "144-DPI density raster was empty"
  in
  let width_scale = float_of_int two_x.width /. float_of_int one_x.width
  and height_scale = float_of_int two_x.height /. float_of_int one_x.height in
  if width_scale < 1.8 || width_scale > 2.2 || height_scale < 1.8 || height_scale > 2.2 then
    fail
      (Printf.sprintf
         "144-DPI raster metrics are not approximately twice 72-DPI metrics: %dx%d -> %dx%d"
         one_x.width one_x.height two_x.width two_x.height);
  (match Font.set_size_dpi font ~size:18. ~horizontal:0 ~vertical:144 with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok () | Error _ -> fail "non-positive font DPI was accepted");

  let wrong_domain = Domain.spawn (fun () -> Font.metrics font) |> Domain.join in
  (match wrong_domain with
  | Error { kind = Wrong_domain; _ } -> ()
  | Ok _ | Error _ -> fail "wrong-domain font access was not rejected");
  (match Init.quit () with
  | Error { kind = Fonts_still_open; _ } -> ()
  | Ok () | Error _ -> fail "TTF quit ignored a live font");

  get_ttf (Font.destroy font);
  get_ttf (Font.destroy font);
  (match Font.metrics font with
  | Error { kind = Destroyed; _ } -> ()
  | Ok _ | Error _ -> fail "stale font access was not rejected");
  for _ = 1 to 10_000 do
    let transient = get_ttf (Font.open_file ~path:font_path ~size:8.) in
    get_ttf (Font.destroy transient)
  done;
  get_ttf (Init.quit ());
  get_ttf (Init.quit ());
  Printf.printf "SDL3_ttf %d.%d.%d CPU font conformance passed\n%!" (linked_version ()).major
    (linked_version ()).minor (linked_version ()).patch
