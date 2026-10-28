open Runtime_resources

let get = function Ok x -> x | Error e -> failwith (Format.asprintf "%a" pp_error e)

let read path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* The writer deliberately emits stored DEFLATE blocks. Inspect every decoded
   scanline, across block boundaries, not just the PNG signature. *)
let scanlines png =
  let n = String.length png in
  let data = Buffer.create n in
  let rec chunks i =
    if i < n then begin
      let len = Int32.to_int (String.get_int32_be png i) in
      if String.sub png (i + 4) 4 = "IDAT" then Buffer.add_substring data png (i + 8) len;
      chunks (i + 12 + len)
    end
  in
  chunks 8;
  let z = Buffer.contents data and raw = Buffer.create n in
  let rec blocks i =
    let header = Char.code z.[i] in
    assert (header land 6 = 0);
    let len = String.get_uint16_le z (i + 1) in
    assert (String.get_uint16_le z (i + 3) = lnot len land 65535);
    Buffer.add_substring raw z (i + 5) len;
    if header land 1 = 0 then blocks (i + 5 + len)
  in
  blocks 2;
  Buffer.contents raw

let run () =
  let width = 257 and height = 129 in
  let pixels = Bytes.init (257 * 129 * 4) (fun i -> Char.chr (((i * 37) + 19) land 255)) in
  let canvas = get (Canvas.create ~width ~height) in
  let path = Filename.temp_file "rays-png-rows" ".png" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove path;
      ignore (Canvas.destroy canvas))
    (fun () ->
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          get (Canvas.set_pixel canvas ~x ~y (Bytes.get_int32_be pixels (((y * width) + x) * 4)))
        done
      done;
      let previous = ref None in
      for round = 0 to 15 do
        let junk =
          Array.init 32 (fun _ -> Bytes.make (height * ((width * 4) + 1)) (Char.chr (50 + round)))
        in
        ignore (Sys.opaque_identity junk);
        Gc.full_major ();
        get (Canvas.save_png canvas path);
        let png = read path in
        let raw = scanlines png in
        assert (String.length raw = height * ((width * 4) + 1));
        for y = 0 to height - 1 do
          let offset = y * ((width * 4) + 1) in
          if raw.[offset] <> '\000' then failwith "PNG filter byte is not initialized to None";
          for x = 0 to (width * 4) - 1 do
            assert (raw.[offset + 1 + x] = Bytes.get pixels ((y * width * 4) + x))
          done
        done;
        Option.iter (fun expected -> assert (expected = png)) !previous;
        previous := Some png
      done);
  print_endline "PNG rows: initialized filters, exact RGBA, repeated byte-identical output"
