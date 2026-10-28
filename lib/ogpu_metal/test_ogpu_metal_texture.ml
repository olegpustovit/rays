open Ogpu_metal_native

let get = function Ok x -> x | Error e -> failwith (Ogpu.Error.to_string e)
let get_metal = function Ok x -> x | Error e -> failwith (Format.asprintf "%a" Metal.pp_error e)

let expect kind = function
  | Error e when e.Ogpu.Error.kind = kind -> ()
  | _ -> failwith "wrong rejection"

let descriptor : Ogpu.Types.texture_descriptor =
  {
    label = Some "ogpu-metal-texture";
    width = 4;
    height = 4;
    depth = 1;
    mip_levels = 1;
    sample_count = 1;
    format = Rgba8_unorm;
    usage = [ Texture_binding; Texture_copy_src; Texture_copy_dst ];
  }

let run () =
  match Device.system_default () with
  | Error _ -> print_endline "ogpu_metal texture: skipped (no device)"
  | Ok device ->
      let other = get (Device.system_default ()) in
      let before = get_metal (Metal.Release_queue.stats ()) in
      let invalid = { descriptor with mip_levels = 8 } in
      expect Ogpu.Error.Invalid_argument
        (Texture.create device ~memory:Texture.Shared ~format:Texture.Rgba8_unorm invalid);
      let texture =
        get (Texture.create device ~memory:Texture.Shared ~format:Texture.Rgba8_unorm descriptor)
      in
      let bytes = get (Texture.read_bytes device texture ~mip_level:0 ~bytes_per_row:16) in
      if Bytes.length bytes <> 64 then failwith "texture readback length";
      let direct = Bytes.make 64 '\xff' in
      get
        (Texture.read_bytes_into device texture ~mip_level:0 ~bytes_per_row:16 ~destination:direct);
      if direct <> bytes then failwith "texture direct read-into mismatch";
      (match
         Texture.read_bytes_into device texture ~mip_level:0 ~bytes_per_row:16
           ~destination:(Bytes.create 63)
       with
      | Error { Ogpu.Error.kind = Invalid_argument; _ } -> ()
      | _ -> failwith "texture direct read-into accepted a short destination");
      expect Ogpu.Error.Cross_device (Texture.descriptor other texture);
      expect Ogpu.Error.Invalid_state (Device.destroy device);
      get (Texture.destroy texture);
      get (Device.destroy device);
      get (Device.destroy other);
      ignore (get_metal (Metal.Release_queue.drain ()));
      let after = get_metal (Metal.Release_queue.stats ()) in
      if after.live_handles <> before.live_handles - 2 then failwith "texture live-handle delta";
      print_endline
        "ogpu_metal texture: both host read paths agree, typed rejections, zero live-handle delta"
