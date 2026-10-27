open Ogpu_metal_native

let get = function Ok value -> value | Error error -> failwith (Ogpu.Error.to_string error)
let get_metal = function Ok value -> value | Error error -> failwith (Format.asprintf "%a" Metal.pp_error error)
let expect kind = function Error error when error.Ogpu.Error.kind = kind -> () | _ -> failwith "wrong rejection"

let descriptor ?(size = 64L) usage : Ogpu.Types.buffer_descriptor =
  { label = Some "ogpu-metal-foundation"; size; usage }

let run () =
  if Ogpu.Impl.backend <> "metal" then failwith "Metal implementation tag changed";
  let unsupported = {Ogpu.Caps.minimum_m1 with limits={Ogpu.Caps.minimum_m1.limits with max_buffer_size=1_024L;max_texture_dimension_2d=1024}} in
  expect Ogpu.Error.Invalid_argument (Ogpu.Caps.validate
    {unsupported with limits={unsupported.limits with max_buffer_size=0L}});
  match Device.system_default () with
  | Error _ -> print_endline "ogpu_metal foundation: skipped (no Metal device)"
  | Ok device ->
      let other = get (Device.system_default ()) in
      let before = get_metal (Metal.Release_queue.stats ()) in
      expect Ogpu.Error.Invalid_argument
        (Buffer.create device ~memory:Buffer.Shared (descriptor ~size:0L [Ogpu.Types.Copy_src]));
      let values =
        [ Buffer.Device_local, [Ogpu.Types.Storage]
        ; Buffer.Shared, [Ogpu.Types.Uniform]
        ; Buffer.Upload, [Ogpu.Types.Copy_src]
        ; Buffer.Readback, [Ogpu.Types.Copy_dst] ]
        |> List.map (fun (memory, usage) -> get (Buffer.create device ~memory (descriptor usage)))
      in
      expect Ogpu.Error.Invalid_argument
        (Buffer.create device ~memory:Buffer.Readback (descriptor [Ogpu.Types.Uniform]));
      List.iter (fun value ->
        ignore (get (Buffer.descriptor device value))) values;
      expect Ogpu.Error.Cross_device (Buffer.descriptor other (List.hd values));
      expect Ogpu.Error.Invalid_state (Device.destroy device);
      List.iter (fun value ->
        get (Buffer.destroy value)) values;
      List.iter (fun value -> expect Ogpu.Error.Stale_handle (Buffer.descriptor device value)) values;
      get (Device.destroy device);
      get (Device.destroy other);
      ignore (get_metal (Metal.Release_queue.drain ()));
      let after = get_metal (Metal.Release_queue.stats ()) in
      if after.live_handles <> before.live_handles - 2 then failwith "Metal live-handle delta";
      Printf.printf "ogpu_metal foundation: four memory classes, zero live-handle delta\n%!"
