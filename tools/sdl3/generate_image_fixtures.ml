let fail format = Printf.ksprintf failwith format

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> Bytes.of_string (really_input_string channel (in_channel_length channel)))

let write_file path bytes =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_bytes channel bytes)

(* EXIF orientation 6 is a clockwise quarter turn.  The APP1 payload is a
   minimal little-endian TIFF directory containing only tag 0x0112. *)
let orientation_6_app1 =
  Bytes.of_string
    "\xff\xe1\x00\x22Exif\x00\x00II\x2a\x00\x08\x00\x00\x00\x01\x00\x12\x01\x03\x00\x01\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00"

let oriented_jpeg source =
  if Bytes.length source < 4 || Bytes.get_uint8 source 0 <> 0xff || Bytes.get_uint8 source 1 <> 0xd8
  then fail "source is not a JPEG stream";
  let output = Bytes.create (Bytes.length source + Bytes.length orientation_6_app1) in
  Bytes.blit source 0 output 0 2;
  Bytes.blit orientation_6_app1 0 output 2 (Bytes.length orientation_6_app1);
  Bytes.blit source 2 output (2 + Bytes.length orientation_6_app1) (Bytes.length source - 2);
  output

(* A 16x1, one-plane, uncompressed ILBM with alternating black/white pixels. *)
let sample_lbm =
  Bytes.of_string
    "FORM\x00\x00\x00\x38ILBMBMHD\x00\x00\x00\x14\x00\x10\x00\x01\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x01\x01\x00\x10\x00\x01CMAP\x00\x00\x00\x06\x00\x00\x00\xff\xff\xffBODY\x00\x00\x00\x02\xaa\xaa"

(* A 2x1 XV 3-3-2 thumbnail containing one red and one blue pixel. *)
let sample_xv = Bytes.of_string "P7 332\n#END_OF_COMMENTS\n2 1\n\xe0\x03"

let () =
  if Array.length Sys.argv <> 3 then fail "usage: %s <source.jpg> <output-directory>" Sys.argv.(0);
  let source = read_file Sys.argv.(1) and output = Sys.argv.(2) in
  if not (Sys.file_exists output && Sys.is_directory output) then
    fail "%s is not an output directory" output;
  write_file (Filename.concat output "orientation-6.jpg") (oriented_jpeg source);
  write_file (Filename.concat output "sample.lbm") sample_lbm;
  write_file (Filename.concat output "sample.xv") sample_xv
