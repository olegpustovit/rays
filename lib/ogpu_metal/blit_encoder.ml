let create (context : Command_control.t) =
  let native = context.native in
  let open_encoder = context.open_encoder in
  let native_of = context.native_of in
  let recording = context.recording in
  let keep_buffer = context.keep_buffer in
  let find_buffer = context.find_buffer in
  let find_texture = context.find_texture in
  let keep_texture = context.keep_texture in
  let region (o : Ogpu_core.Types.origin) (e : Ogpu_core.Types.extent) : Metal.Texture.region =
    { x = o.x; y = o.y; z = o.z; width = e.width; height = e.height; depth = e.depth }
  in
  let blit_encoder () =
    match recording () with
    | Error _ as failure -> failure
    | Ok () -> (
        let created = native_of (Metal.Blit_encoder.create native) in
        match created with
        | Error _ as failure -> failure
        | Ok encoder ->
            open_encoder := Some (fun () -> ignore (Metal.Blit_encoder.end_encoding encoder));
            Ok
              {
                Ogpu_core.Backend.buffer_to_texture =
                  (fun ~src ~offset ~bytes_per_row ~bytes_per_image ~dst ~mip ~origin ~extent ->
                    match (find_buffer src, find_texture dst) with
                    | Error e, _ | _, Error e -> Error e
                    | Ok source, Ok destination -> (
                        match (keep_buffer source, keep_texture destination) with
                        | Error e, _ | _, Error e -> Error e
                        | Ok (), Ok () ->
                            native_of
                              (Metal.Blit_encoder.copy_buffer_to_texture encoder
                                 ~source:(Buffer.Private.metal source) ~source_offset:offset
                                 ~source_bytes_per_row:(Int64.to_int bytes_per_row)
                                 ~source_bytes_per_image:(Int64.to_int bytes_per_image)
                                 ~destination:(Texture.Private.metal destination)
                                 ~destination_slice:0 ~destination_level:mip
                                 ~destination_region:(region origin extent))));
                end_blit =
                  (fun () ->
                    open_encoder := None;
                    native_of (Metal.Blit_encoder.end_encoding encoder));
              })
  in
  blit_encoder ()
