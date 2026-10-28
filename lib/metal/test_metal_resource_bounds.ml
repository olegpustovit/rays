open Metal

let fail format = Printf.ksprintf failwith format

let get = function
  | Ok value -> value
  | Error error -> fail "%s" (Format.asprintf "%a" pp_error error)

let expect_invalid name = function
  | Error { kind = Invalid_argument; _ } -> ()
  | Error error ->
      fail "%s returned %s, expected Invalid_argument" name (Format.asprintf "%a" pp_error error)
  | Ok _ -> fail "%s accepted a malformed range" name

let stats_equal name before after =
  if
    before.Release_queue.total_created <> after.Release_queue.total_created
    || before.total_released <> after.total_released
    || before.live_handles <> after.live_handles
  then
    fail "%s crossed the native boundary (created %Ld -> %Ld, released %Ld -> %Ld, live %d -> %d)"
      name before.total_created after.total_created before.total_released after.total_released
      before.live_handles after.live_handles

let reject_without_native_work name operation =
  ignore (get (Release_queue.drain ()));
  ignore (get (Release_queue.drain ()));
  let before = get (Release_queue.stats ()) in
  expect_invalid name (operation ());
  let after = get (Release_queue.stats ()) in
  stats_equal name before after

let run () =
  match Device.system_default () with
  | Error _ -> print_endline "Metal resource bounds: skipped (no device)"
  | Ok device ->
      let info = get (Device.info device) in
      reject_without_native_work "negative buffer length" (fun () ->
          Buffer.create ~device ~length:(-1L) ~storage:Buffer.Shared ());
      reject_without_native_work "oversized buffer length" (fun () ->
          Buffer.create ~device
            ~length:(Int64.succ info.max_buffer_length)
            ~storage:Buffer.Shared ());
      let buffer = get (Buffer.create ~device ~length:32L ~storage:Buffer.Shared ()) in
      let bytes = Bytes.make 16 '\001' in
      [
        ("buffer negative destination", fun () -> Buffer.write_bytes buffer ~dst_offset:(-1L) bytes);
        ("buffer destination overflow", fun () -> Buffer.write_bytes buffer ~dst_offset:24L bytes);
        ( "buffer destination integer overflow",
          fun () -> Buffer.write_bytes buffer ~dst_offset:Int64.max_int bytes );
        ( "buffer negative read",
          fun () -> Result.map ignore (Buffer.read_bytes buffer ~offset:(-1L) ~length:1) );
        ( "buffer negative read length",
          fun () -> Result.map ignore (Buffer.read_bytes buffer ~offset:0L ~length:(-1)) );
        ( "buffer oversized read",
          fun () -> Result.map ignore (Buffer.read_bytes buffer ~offset:31L ~length:2) );
        ( "buffer integer-overflow read",
          fun () ->
            Result.map ignore (Buffer.read_bytes buffer ~offset:Int64.max_int ~length:max_int) );
      ]
      |> List.iter (fun (name, operation) -> reject_without_native_work name operation);
      (* The last admitted ranges: the final 16 bytes written, the final byte read. *)
      get (Buffer.write_bytes buffer ~dst_offset:16L bytes);
      if
        get (Buffer.read_bytes buffer ~offset:0L ~length:32)
        <> Bytes.cat (Bytes.make 16 '\000') bytes
      then fail "an admitted write did not land at its offset";
      if get (Buffer.read_bytes buffer ~offset:31L ~length:1) <> Bytes.make 1 '\001' then
        fail "the last byte was not readable";
      get (Buffer.destroy buffer);
      get (Device.destroy device);
      ignore (get (Release_queue.drain ()));
      ignore (get (Release_queue.drain ()));
      print_endline "Metal resource bounds: pre-native rejection table passed"
