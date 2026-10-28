open Metal
open Test_metal_support

(* Device facts, the buffer a device owns, and the release accounting of a
   whole process: every handle made here is gone at the end. *)
let run () =
  match Device.system_default () with
  | Error _ -> print_endline "Metal device safe: skipped (no device)"
  | Ok device ->
      let before = stats () in
      let info = get (Device.info device) in
      let after = stats () in
      if after.total_created <> before.total_created then
        fail "Device.info allocated a native handle";
      if info.max_threadgroup_memory_length <= 0L then
        fail "device maximum threadgroup-memory length is not positive";
      if info.name = "Apple M1" && info.max_threadgroup_memory_length <> 32_768L then
        fail "Apple M1 maximum threadgroup-memory length is %Ld, expected 32768"
          info.max_threadgroup_memory_length;
      if info.name = "" || info.registry_id = 0L then fail "default device identity is incomplete";
      if info.max_buffer_length < 16L then fail "device buffer limit is invalid";
      if not (Device.same device device) then fail "a device is not itself";
      finalizer_releases "buffer" ~count:1 (fun () ->
          ignore (get (Buffer.create ~device ~length:16L ~storage:Buffer.Shared ())));
      reject Wrong_domain (Domain.spawn (fun () -> Device.info device) |> Domain.join);
      reject Wrong_domain
        (Domain.spawn (fun () -> Buffer.create ~device ~length:16L ~storage:Buffer.Shared ())
        |> Domain.join);
      let buffer =
        get
          (Buffer.create ~device ~length:16L ~storage:Buffer.Shared
             ~label:"Metal conformance values" ())
      in
      if Buffer.length buffer <> 16L then fail "buffer length did not round-trip";
      let values = int32_bytes [| 1l; 41l; 99l; -2l |] in
      get (Buffer.write_bytes buffer ~dst_offset:0L values);
      if get (Buffer.read_bytes buffer ~offset:0L ~length:16) <> values then
        fail "buffer bytes did not round-trip";
      if get (Buffer.read_bytes buffer ~offset:4L ~length:4) <> int32_bytes [| 41l |] then
        fail "a buffer range read the wrong offset";
      reject Parent_has_dependents (Device.destroy device);
      reject Invalid_argument (Buffer.read_bytes buffer ~offset:12L ~length:8);
      reject Invalid_argument (Buffer.read_bytes buffer ~offset:0L ~length:max_int);
      without_handles "invalid buffer labels" (fun () ->
          reject Invalid_argument
            (Buffer.create ~device ~length:16L ~storage:Buffer.Shared ~label:"invalid\000label" ()));
      let private_buffer = get (Buffer.create ~device ~length:16L ~storage:Buffer.Private ()) in
      reject Unsupported (Buffer.read_bytes private_buffer ~offset:0L ~length:4);
      reject Unsupported (Buffer.write_bytes private_buffer ~dst_offset:0L values);
      get (Buffer.destroy private_buffer);
      (* Creation reads the cache mode back from Metal and refuses a mismatch. *)
      let configured =
        get
          (Buffer.create ~device ~length:16L ~storage:Buffer.Shared ~cpu_cache:Buffer.Write_combined
             ())
      in
      get (Buffer.write_bytes configured ~dst_offset:0L values);
      if get (Buffer.read_bytes configured ~offset:0L ~length:16) <> values then
        fail "write-combined buffer bytes did not round-trip";
      get (Buffer.destroy configured);
      reject Invalid_argument (Device.supports_texture_sample_count device 0);
      if not (get (Device.supports_texture_sample_count device 1)) then
        fail "device refuses single-sample textures";
      get (Buffer.destroy buffer);
      get (Buffer.destroy buffer);
      reject Destroyed (Buffer.read_bytes buffer ~offset:0L ~length:1);
      reject Destroyed (Buffer.write_bytes buffer ~dst_offset:0L values);
      get (Device.destroy device);
      reject Destroyed (Device.info device);
      reject Destroyed (Buffer.create ~device ~length:16L ~storage:Buffer.Shared ());
      let final = settle_finalizers ~expected_live:0 in
      if final.pending <> 0 || final.external_deallocation_mismatches <> 0L then
        fail "Metal release accounting did not settle (%d pending, %Ld mismatches)" final.pending
          final.external_deallocation_mismatches;
      Printf.printf "Metal device safe: info/domain/buffer/finalizer accounting passed on %s\n%!"
        info.name
