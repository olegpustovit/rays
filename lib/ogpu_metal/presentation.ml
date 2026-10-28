type t = {
  device : Metal.Device.t;
  library : Metal.Library.t;
  pipeline : Metal.Render_pipeline.t;
  mutable dead : bool;
}

let operation = "Ogpu_metal.Presentation"
let error kind message = Error (Ogpu_core.Error.make operation kind message)
let metal value = Error (Device.of_metal_error ~operation value)

let fail_encode encoder value =
  Option.iter
    (fun encoder ->
      if not (Metal.Render_encoder.destroyed encoder) then
        ignore (Metal.Render_encoder.end_encoding encoder))
    encoder;
  metal value

let source = Present_source.source

let create device =
  let native = Device.Private.metal device in
  if Device.destroyed device then error Ogpu_core.Error.Stale_handle "device is destroyed"
  else
    match Metal.Library.compile_source ~label:"Rays presentation" ~device:native source with
    | Error value -> metal value
    | Ok library -> (
        match Metal.Compiler.create native with
        | Error value ->
            ignore (Metal.Library.destroy library);
            metal value
        | Ok compiler -> (
            let compiled =
              Metal.Compiler.create_render_pipeline ~label:"Rays presentation"
                ~fragment:"present_fragment" ~color_formats:[ Metal.Texture.Bgra8_unorm ]
                ~primitive_topology:Metal.Render_pipeline.Triangle compiler ~library
                ~vertex:"present_vertex"
            in
            ignore (Metal.Compiler.destroy compiler);
            match compiled with
            | Error value ->
                ignore (Metal.Library.destroy library);
                metal value
            | Ok pipeline -> Ok { device = native; library; pipeline; dead = false }))

let validate_resources value ~device ~source ~target =
  if value.dead then error Ogpu_core.Error.Stale_handle "presentation helper is destroyed"
  else if not (Metal.Device.same value.device device) then
    error Ogpu_core.Error.Cross_device "presentation command belongs to another device"
  else if Metal.Texture.destroyed source || Metal.Texture.destroyed target then
    error Ogpu_core.Error.Stale_handle "presentation texture is destroyed"
  else if
    (not (Metal.Device.same value.device (Metal.Texture.device source)))
    || not (Metal.Device.same value.device (Metal.Texture.device target))
  then error Ogpu_core.Error.Cross_device "presentation texture belongs to another device"
  else
    let source_descriptor = Metal.Texture.descriptor source
    and target_descriptor = Metal.Texture.descriptor target in
    if
      source_descriptor.kind <> Metal.Texture.Texture_2d
      || source_descriptor.format <> Metal.Texture.Rgba8_unorm
      || source_descriptor.depth <> 1 || source_descriptor.mip_levels <> 1
      || source_descriptor.sample_count <> 1
      || source_descriptor.array_length <> 1
    then
      error Ogpu_core.Error.Invalid_argument
        "presentation source must be a single-sample 2D RGBA8 texture"
    else if not (List.mem Metal.Texture.Shader_read source_descriptor.usage) then
      error Ogpu_core.Error.Invalid_argument "presentation source must be shader-readable"
    else if
      target_descriptor.kind <> Metal.Texture.Texture_2d
      || target_descriptor.format <> Metal.Texture.Bgra8_unorm
      || target_descriptor.depth <> 1 || target_descriptor.mip_levels <> 1
      || target_descriptor.sample_count <> 1
      || target_descriptor.array_length <> 1
    then
      error Ogpu_core.Error.Invalid_argument
        "presentation target must be a single-sample 2D BGRA8 texture"
    else if not (List.mem Metal.Texture.Render_target target_descriptor.usage) then
      error Ogpu_core.Error.Invalid_argument "presentation target must be renderable"
    else if
      source_descriptor.width <> target_descriptor.width
      || source_descriptor.height <> target_descriptor.height
    then error Ogpu_core.Error.Invalid_argument "presentation source and target extents differ"
    else Ok ()

let encode value commands ?present ~source ~target () =
  if Metal.Command_buffer.destroyed commands then
    error Ogpu_core.Error.Stale_handle "presentation command buffer is destroyed"
  else
    match
      validate_resources value ~device:(Metal.Command_buffer.device commands) ~source ~target
    with
    | Error _ as failure -> failure
    | Ok () -> (
        match Metal.Render_encoder.create commands ~target () with
        | Error value -> metal value
        | Ok encoder -> (
            match Metal.Render_encoder.set_pipeline encoder value.pipeline with
            | Error value -> fail_encode (Some encoder) value
            | Ok () -> (
                match Metal.Render_encoder.set_fragment_texture encoder ~index:0 source with
                | Error value -> fail_encode (Some encoder) value
                | Ok () -> (
                    match Metal.Render_encoder.draw_triangles encoder ~first:0 ~count:3 () with
                    | Error value -> fail_encode (Some encoder) value
                    | Ok () -> (
                        match Metal.Render_encoder.end_encoding encoder with
                        | Error value -> fail_encode (Some encoder) value
                        | Ok () -> (
                            match present with
                            | None -> Ok ()
                            | Some drawable -> (
                                match Metal.Command_buffer.present commands drawable () with
                                | Ok () -> Ok ()
                                | Error value -> metal value)))))))

let destroy value =
  if not value.dead then begin
    value.dead <- true;
    ignore (Metal.Render_pipeline.destroy value.pipeline);
    ignore (Metal.Library.destroy value.library)
  end
