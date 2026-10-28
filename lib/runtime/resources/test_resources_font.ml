open Runtime_resources
open Ogpu.Error

let get = function Ok x -> x | Error e -> failwith (Format.asprintf "%a" pp_error e)

let run () =
  let font = get (Font.open_system ~size:14.) in
  get (Font.set_hinting font Font.Light_hinting);
  if get (Font.render font ~density:1 ~color:(255, 255, 255, 255) "") <> None then
    failwith "empty text";
  (match Font.render font ~density:1 ~color:(255, 255, 255, 255) "\xc0\x80" with
  | Error { kind = Invalid_argument; _ } -> ()
  | _ -> failwith "UTF-8");
  let transferred =
    Option.get (get (Font.render font ~density:1 ~color:(255, 255, 255, 255) "transfer"))
  in
  let expected = get (Text.pixels transferred) in
  let image = get (Text.into_image transferred) in
  if (not (Text.destroyed transferred)) || get (Image.pixels image) <> expected then
    failwith "text-to-image ownership transfer changed pixels";
  Bytes.fill expected 0 (Bytes.length expected) '\000';
  if get (Image.pixels image) = expected then
    failwith "public text pixels still alias the transferred image";
  get (Text.destroy transferred);
  get (Image.destroy image);
  let advance density = (get (Font.glyph_metrics_at font ~density (Char.code 'A'))).advance in
  if advance 1 <= 0 || advance 2 <= advance 1 then failwith "glyph metrics";
  (match
     ( Font.glyph_metrics_at font ~density:0 (Char.code 'A'),
       Font.render font ~density:17 ~color:(0, 0, 0, 255) "x" )
   with
  | Error { kind = Invalid_argument; _ }, Error { kind = Invalid_argument; _ } -> ()
  | _ -> failwith "density outside 1..16 accepted");
  (* Rendered text is its own picture: twice the density is about twice the
     size, and neither a font mutation nor the font's teardown shortens it. *)
  let size density =
    let text = Option.get (get (Font.render font ~density ~color:(255, 0, 0, 255) "explicit")) in
    let size = get (Text.size text) in
    get (Text.destroy text);
    size
  in
  let w1, h1 = size 1 in
  let explicit =
    Option.get (get (Font.render font ~density:2 ~color:(255, 0, 0, 255) "explicit"))
  in
  let width, height = get (Text.size explicit) in
  if width <= 0 || height <= 0 || abs (width - (2 * w1)) > 4 || abs (height - (2 * h1)) > 2 then
    failwith "explicit text";
  let generation = Text.generation explicit and pixels = get (Text.pixels explicit) in
  get (Font.set_hinting font Font.Mono_hinting);
  get (Font.destroy font);
  if
    Text.destroyed explicit
    || get (Text.size explicit) <> (width, height)
    || Text.generation explicit <> generation
    || get (Text.pixels explicit) <> pixels
  then failwith "font mutation or teardown shortened text lifetime";
  get (Text.destroy explicit);
  if not (Text.destroyed explicit) then failwith "text survived its own destroy";
  get (Font.destroy font);
  (match Font.render font ~density:1 ~color:(0, 0, 0, 0) "dead" with
  | Error { kind = Destroyed; _ } -> ()
  | _ -> failwith "stale font");
  print_endline "runtime_resources Font: UTF8 density owned text lifetime passed"
