open Ogpu_core.Pipeline

type native = Compute of Metal.Compute_pipeline.t | Render of Metal.Render_pipeline.t
type pipeline_kind

type t = {
  native : native;
  library : Metal.Library.t;
  shared_library : Library.t option;
  mutable functions : Metal.Function.t list;
  descriptor_destroy : (unit -> unit) option;
  handle : pipeline_kind Ogpu_core.Handle.t;
  device : Device.t;
  mutable dead : bool;
  mutable submission_uses : int;
  mutable destroy_requested : bool;
}

let error op kind message = Error (Ogpu_core.Error.make op kind message)

let finish_destroy value =
  let operation = "Ogpu_metal.Pipeline.destroy" in
  let native_result =
    match value.native with
    | Compute x -> Metal.Compute_pipeline.destroy x
    | Render x -> Metal.Render_pipeline.destroy x
  in
  match native_result with
  | Error e -> Error (Device.of_metal_error ~operation e)
  | Ok () -> (
      let first =
        List.fold_left
          (fun failure function_ ->
            match (failure, Metal.Function.destroy function_) with
            | Some _, _ -> failure
            | None, Ok () -> None
            | None, Error e -> Some e)
          None value.functions
      in
      let first =
        match value.shared_library with
        | Some shared ->
            Library.Private.detach_pipeline shared;
            first
        | None -> (
            match (first, Metal.Library.destroy value.library) with
            | Some e, _ -> Some e
            | None, Ok () -> None
            | None, Error e -> Some e)
      in
      match first with
      | Some e -> Error (Device.of_metal_error ~operation e)
      | None ->
          Option.iter (fun f -> f ()) value.descriptor_destroy;
          Device.Private.detach_resource value.device;
          Ok ())

let destroy value =
  if value.dead then Ok ()
  else if value.submission_uses > 0 then (
    Ogpu_core.Handle.destroy value.handle;
    value.dead <- true;
    value.destroy_requested <- true;
    Ok ())
  else
    match finish_destroy value with
    | Error _ as failure -> failure
    | Ok () ->
        Ogpu_core.Handle.destroy value.handle;
        value.dead <- true;
        Ok ()

let metal_kind = function
  | Ogpu_core.Shader.Uniform_buffer | Storage_buffer -> `Buffer
  | Sampled_texture | Storage_texture -> `Texture
  | Sampler -> `Sampler
  | Acceleration_structure -> `Accel

let reflected_kind (value : Metal.Binding.t) =
  match value.kind with
  | Buffer_binding _ -> `Buffer
  | Texture_binding _ -> `Texture
  | Sampler_binding -> `Sampler
  | Primitive_acceleration_structure_binding | Instance_acceleration_structure_binding -> `Accel
  | _ -> `Other

let validate_reflection op shader bindings =
  let expected = Ogpu_core.Shader.bindings shader in
  if List.exists (fun (b : Ogpu_core.Shader.binding) -> b.group <> 0) expected then
    error op Ogpu_core.Error.Invalid_argument "Metal foundation maps only bind group zero"
  else
    let expected =
      List.sort compare
        (List.map (fun (b : Ogpu_core.Shader.binding) -> (b.binding, metal_kind b.kind)) expected)
    and actual =
      bindings
      |> List.filter (fun (b : Metal.Binding.t) -> b.used)
      |> List.map (fun (b : Metal.Binding.t) -> (Int64.to_int b.index, reflected_kind b))
      |> List.sort compare
    in
    let show =
      List.map (fun (index, kind) ->
          Printf.sprintf "%d:%s" index
            (match kind with
            | `Buffer -> "buffer"
            | `Texture -> "texture"
            | `Sampler -> "sampler"
            | `Accel -> "accel"
            | `Other -> "other"))
    in
    if expected <> actual then
      error op Ogpu_core.Error.Invalid_argument
        (Printf.sprintf
           "native Metal reflection does not match OGPU slots and resource classes (interface %s, \
            reflected %s)"
           (String.concat "," (show expected))
           (String.concat "," (show actual)))
    else Ok ()

let compile_library = Library.compile_native

let metal_constants constants =
  List.map
    (fun (name, value) ->
      ( name,
        match value with
        | Ogpu_core.Shader.Bool value -> Metal.Function.Bool_constant value
        | Int32 value -> Metal.Function.Int32_constant value
        | Uint32 value ->
            Metal.Function.Uint32_constant (Int64.logand (Int64.of_int32 value) 0xffff_ffffL)
        | Float32 value -> Metal.Function.Float32_constant value ))
    constants

let create_compute_from_library device (library : Library.t) ~entry ~constants ~interface =
  let op = "Ogpu_metal.Pipeline.create_compute_from_library" in
  match Library.validate device library with
  | Error _ as e -> e
  | Ok () -> (
      let native = Library.Private.metal library in
      let found =
        if constants = [] then Metal.Function.find ~library:native entry
        else Metal.Function.specialize ~library:native entry ~constants:(metal_constants constants)
      in
      match found with
      | Error e -> Error (Device.of_metal_error ~operation:op e)
      | Ok function_ -> (
          match Metal.Compute_pipeline.create ~label:entry ~reflection:true function_ with
          | Error e ->
              ignore (Metal.Function.destroy function_);
              Error (Device.of_metal_error ~operation:op e)
          | Ok pipeline -> (
              let declared =
                Ogpu_core.Shader.create
                  {
                    backend = "metal";
                    label = None;
                    bytes = Bytes.of_string "x";
                    entry_points = [ { name = entry; stage = Compute } ];
                    bindings = interface;
                  }
              in
              let checked =
                match declared with
                | Error _ as e -> e
                | Ok declared ->
                    Option.value (Metal.Compute_pipeline.bindings pipeline) ~default:[]
                    |> validate_reflection op declared
              in
              match checked with
              | Error e ->
                  ignore (Metal.Compute_pipeline.destroy pipeline);
                  ignore (Metal.Function.destroy function_);
                  Error e
              | Ok () ->
                  Library.Private.attach_pipeline library;
                  Device.Private.attach_resource device;
                  Ok
                    {
                      native = Compute pipeline;
                      library = native;
                      shared_library = Some library;
                      functions = [ function_ ];
                      descriptor_destroy = None;
                      handle = Ogpu_core.Handle.create ~device:(Device.Private.handle device);
                      device;
                      dead = false;
                      submission_uses = 0;
                      destroy_requested = false;
                    })))

let attachment blend format =
  let open Metal.Render_pipeline in
  match blend with
  | Ogpu_core.Pipeline.Replace -> color_attachment format
  | Alpha ->
      color_attachment ~blending:Blend_enabled ~source_rgb:Blend_source_alpha
        ~destination_rgb:Blend_one_minus_source_alpha ~source_alpha:Blend_one
        ~destination_alpha:Blend_one_minus_source_alpha format
  | Add ->
      color_attachment ~blending:Blend_enabled ~source_rgb:Blend_one ~destination_rgb:Blend_one
        ~source_alpha:Blend_one ~destination_alpha:Blend_one format
  | Multiply ->
      color_attachment ~blending:Blend_enabled ~source_rgb:Blend_destination_color
        ~destination_rgb:Blend_zero ~source_alpha:Blend_one
        ~destination_alpha:Blend_one_minus_source_alpha format
  | Screen ->
      color_attachment ~blending:Blend_enabled ~source_rgb:Blend_one_minus_destination_color
        ~destination_rgb:Blend_one ~source_alpha:Blend_one
        ~destination_alpha:Blend_one_minus_source_alpha format
  | Subtract ->
      color_attachment ~blending:Blend_enabled ~source_rgb:Blend_one ~destination_rgb:Blend_one
        ~rgb_operation:Blend_reverse_subtract ~source_alpha:Blend_one
        ~destination_alpha:Blend_one_minus_source_alpha format

let build_render ~support_indirect_command_buffers ~primitive_topology ~blend ~op device descriptor
    =
  match compile_library op device descriptor.vertex with
  | Error _ as e -> e
  | Ok library -> (
      let color =
        match descriptor.color_format with
        | Rgba8_unorm -> Metal.Texture.Rgba8_unorm
        | Bgra8_unorm -> Metal.Texture.Bgra8_unorm
      in
      match Metal.Compiler.create (Device.Private.metal device) with
      | Error e ->
          ignore (Metal.Library.destroy library);
          Error (Device.of_metal_error ~operation:op e)
      | Ok compiler -> (
          let fragment =
            Option.map
              (fun (_ : Ogpu_core.Shader.t) -> Option.get descriptor.fragment_entry)
              descriptor.fragment
          in
          match
            Metal.Compiler.create_render_pipeline ?label:descriptor.label ?fragment ~reflection:true
              ~raster_sample_count:descriptor.sample_count
              ~color_attachments:[ attachment blend color ]
              ~primitive_topology ~support_indirect_command_buffers compiler ~library
              ~vertex:descriptor.vertex_entry
          with
          | Error e ->
              ignore (Metal.Compiler.destroy compiler);
              ignore (Metal.Library.destroy library);
              Error (Device.of_metal_error ~operation:op e)
          | Ok pipeline -> (
              ignore (Metal.Compiler.destroy compiler);
              let reflection =
                Option.value
                  (Metal.Render_pipeline.reflection pipeline)
                  ~default:{ vertex = []; fragment = [] }
              in
              match validate_reflection op descriptor.vertex reflection.vertex with
              | Error e ->
                  ignore (Metal.Render_pipeline.destroy pipeline);
                  ignore (Metal.Library.destroy library);
                  Error e
              | Ok () ->
                  let value =
                    {
                      native = Render pipeline;
                      library;
                      shared_library = None;
                      functions = [];
                      descriptor_destroy = None;
                      handle = Ogpu_core.Handle.create ~device:(Device.Private.handle device);
                      device;
                      dead = false;
                      submission_uses = 0;
                      destroy_requested = false;
                    }
                  in
                  Device.Private.attach_resource device;
                  Ok value)))

(* An owned, uncached render pipeline for the portable driver. With
   [indirect], the fragment function is retained for argument encoders and
   indirect command buffers. *)
let create_render_owned ?(indirect = false) ?(primitive_topology = Metal.Render_pipeline.Triangle)
    ?(blend = Ogpu_core.Pipeline.Replace) device descriptor =
  let op = "Ogpu_metal.Pipeline.create_render_owned" in
  if Device.destroyed device then error op Ogpu_core.Error.Stale_handle "device is destroyed"
  else
    match Ogpu_core.Pipeline.create_render (Device.capabilities device) descriptor with
    | Error _ as e -> e
    | Ok () -> (
        match
          build_render ~support_indirect_command_buffers:indirect ~primitive_topology ~blend ~op
            device descriptor
        with
        | Error _ as e -> e
        | Ok value -> (
            if not indirect then Ok value
            else
              match (value.native, descriptor.fragment_entry) with
              | Render native, Some entry -> (
                  match Metal.Render_pipeline.supports_indirect_command_buffers native with
                  | Error e ->
                      ignore (destroy value);
                      Error (Device.of_metal_error ~operation:op e)
                  | Ok false ->
                      ignore (destroy value);
                      error op Ogpu_core.Error.Unsupported
                        "native pipeline does not support indirect commands"
                  | Ok true -> (
                      match Metal.Function.find ~library:value.library entry with
                      | Error e ->
                          ignore (destroy value);
                          Error (Device.of_metal_error ~operation:op e)
                      | Ok function_ ->
                          value.functions <- [ function_ ];
                          Ok value))
              | _ -> Ok value))

module Private = struct
  type nonrec native = native =
    | Compute of Metal.Compute_pipeline.t
    | Render of Metal.Render_pipeline.t

  let native value = value.native

  let argument_function value =
    match (value.native, value.functions) with
    | Render _, function_ :: _ -> Some function_
    | _ -> None

  let argument_encoder value ~buffer_index =
    match argument_function value with
    | None ->
        error "Ogpu_metal.Pipeline.argument_encoder" Ogpu_core.Error.Invalid_state
          "fragment function was not retained"
    | Some function_ -> (
        match Metal.Function.argument_encoder function_ ~buffer_index with
        | Error e ->
            Error (Device.of_metal_error ~operation:"Ogpu_metal.Pipeline.argument_encoder" e)
        | Ok encoder -> Ok encoder)

  let retain_submission value =
    if value.dead then
      Error
        (Ogpu_core.Error.make "Ogpu_metal.Pipeline.retain_submission" Ogpu_core.Error.Stale_handle
           "pipeline is destroyed")
    else (
      value.submission_uses <- value.submission_uses + 1;
      Ok ())

  let release_submission value =
    value.submission_uses <- value.submission_uses - 1;
    if value.submission_uses = 0 && value.destroy_requested then ignore (finish_destroy value)
end
