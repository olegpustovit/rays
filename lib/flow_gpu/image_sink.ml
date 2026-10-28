module B = Ogpu.Backend
type storage = {buffer:B.buffer; texture:B.texture; width:int; height:int}
type t = {device:B.device; queue:B.queue; library:B.library; pipeline:B.pipeline;
  uniforms:bytes; domain:Domain.id; mutable storage:storage option;
  mutable generation:int; mutable closed:bool; mutable buffers:int; mutable textures:int}
type output = {owner:t; texture:B.texture; generation:int}
let interface = List.init 3 (fun binding -> Ogpu.Shader.{group=0;binding;
  kind=(if binding=2 then Uniform_buffer else Storage_buffer);visibility=[Compute]})
let diagnostic e = Diagnostic.error ~code:"E_GPU" (Ogpu.Error.to_string e)
let gpu result = Result.map_error diagnostic result
let error code message = Error (Diagnostic.error ~code message)
let create_owned device queue =
  if not(Domain.is_main_domain()) then error "E_GPU" "Image sinks require the initial domain."
  else
    let (let*) = Result.bind in
    let* () = gpu (Ogpu.Caps.require ~operation:"Image_sink.create" (B.capabilities device) Compute_pipeline) in
    let* shader = gpu (Ogpu.Shader.of_source {backend="metal";label=Some"Flow image RGBA8";
      bytes=Bytes.of_string Emit.image_rgba8_source;entry_points=[{name="rays_flow_image_rgba8";stage=Compute}];bindings=interface}) in
    let* library = gpu (B.create_library device shader) in
    match B.create_compute_pipeline_from library ~entry:"rays_flow_image_rgba8" ~interface () with
    | Error e -> ignore(B.destroy_library library); Error(diagnostic e)
    | Ok pipeline -> Ok {device;queue;library;pipeline;uniforms=Bytes.create 12;domain=Domain.self();
        storage=None;generation=0;closed=false;buffers=0;textures=0}
let create gpu = create_owned (Renderer.gpu_device gpu) (Renderer.gpu_queue gpu)
let release (storage:storage) = ignore(B.destroy_texture storage.texture);ignore(B.destroy_buffer storage.buffer)
let allocate t ~width ~height ~pitch =
  let (let*) = Result.bind in
  let buffer_descriptor:Ogpu.Types.buffer_descriptor={label=Some"Flow image padded RGBA8";
    size=Int64.of_int(pitch*height);usage=[Storage;Copy_src]} in
  let texture_descriptor:Ogpu.Types.texture_descriptor={label=Some"Flow image RGBA8";width;height;depth=1;
    mip_levels=1;sample_count=1;format=Rgba8_unorm;
    usage=[Texture_binding;Texture_copy_src;Texture_copy_dst]} in
  let caps=B.capabilities t.device in
  let* ()=gpu(Ogpu.Validation.validate_buffer caps buffer_descriptor) in
  let* ()=gpu(Ogpu.Validation.validate_texture caps texture_descriptor) in
  let* buffer = gpu (B.create_buffer t.device buffer_descriptor) in
  t.buffers<-t.buffers+1;
  match B.create_texture t.device texture_descriptor with
  | Error e -> ignore(B.destroy_buffer buffer);Error(diagnostic e)
  | Ok texture -> t.textures<-t.textures+1;Ok {buffer;texture;width;height}
let get = function Ok value -> value | Error e -> raise(Diagnostic.Error(diagnostic e))
let convert t ~width ~height input =
  if t.closed || Domain.self()<>t.domain then error "E_GPU" "Image sink is closed or called from another domain."
  else if width<=0 || height<=0 || width>Sys.max_string_length/4/height then
    error "E_IMAGE" "Image dimensions exceed native storage bounds."
  else if Run.width input<>4 || Run.count input<>width*height then
    error "E_IMAGE" "Image sink needs one vec4 output per pixel."
  else match Run.buffer input with
  | None -> error "E_GPU" "Image producer output was closed or superseded."
  | Some source ->
    let pitch=(width*4+255)/256*256 in
    if pitch>Sys.max_string_length/height then error "E_IMAGE" "Padded image exceeds native storage bounds."
    else
    let (let*) = Result.bind in
    (* Invalidate all borrowed images before any allocation, write or submission. *)
    t.generation<-t.generation+1;
    let* storage,fresh = match t.storage with
      | Some old when old.width=width && old.height=height -> Ok(old,false)
      | _ -> Result.map(fun storage->storage,true)(allocate t ~width ~height ~pitch) in
    let published=ref false in
    Fun.protect ~finally:(fun()->if fresh && not !published then release storage) (fun()->
      try
        Bytes.set_int32_le t.uniforms 0 (Int32.of_int(width*height));
        Bytes.set_int32_le t.uniforms 4 (Int32.of_int width);
        Bytes.set_int32_le t.uniforms 8 (Int32.of_int(pitch/4));
        let commands=get(B.begin_commands t.queue) in
        let committed=ref false in
        let receipt=Fun.protect ~finally:(fun()->if not !committed then ignore(B.abandon commands)) (fun()->
          let compute=get(B.compute_encoder commands) in
          let ended=ref false in
          Fun.protect ~finally:(fun()->if not !ended then ignore(B.end_compute compute)) (fun()->
            get(B.set_pipeline compute t.pipeline);
            get(B.set_buffer compute ~index:0 source);
            get(B.set_buffer compute ~index:1 storage.buffer);
            get(B.set_bytes compute ~index:2 t.uniforms);
            get(B.dispatch_threads compute ~threads:(width*height,1,1) ~threadgroup:(256,1,1));
            get(B.end_compute compute);ended:=true);
          let blit=get(B.blit_encoder commands) in
          let ended=ref false in
          Fun.protect ~finally:(fun()->if not !ended then ignore(B.end_blit blit)) (fun()->
            get(B.buffer_to_texture blit ~src:storage.buffer ~bytes_per_row:(Int64.of_int pitch)
              ~bytes_per_image:(Int64.of_int(pitch*height)) ~dst:storage.texture
              ~extent:{width;height;depth=1} ());
            get(B.end_blit blit);ended:=true);
          let receipt=get(B.commit commands) in committed:=true;receipt) in
        get(B.complete_through t.queue receipt.epoch);
        if fresh then Option.iter release t.storage;
        t.storage<-Some storage;published:=true;
        Ok {owner=t;texture=storage.texture;generation=t.generation}
      with Diagnostic.Error e -> Error e
        | Out_of_memory -> error "E_IMAGE" "Image conversion exceeds available storage.")
let texture output =
  let t=output.owner in
  if t.closed || Domain.self()<>t.domain || t.generation<>output.generation then None
  else Some output.texture
let close t =
  if Domain.self()<>t.domain then invalid_arg "Image_sink.close: creating domain required";
  if not t.closed then begin
    Option.iter release t.storage;t.storage<-None;
    ignore(B.destroy_pipeline t.pipeline);ignore(B.destroy_library t.library);t.closed<-true
  end
let buffer_creations t = t.buffers
let texture_creations t = t.textures

module Private = struct
  let create_owned = create_owned
end
