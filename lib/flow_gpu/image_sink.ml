module B = Ogpu.Backend
type storage = {texture:B.texture; width:int; height:int}
type t = {device:B.device; queue:B.queue; pipeline:B.pipeline; size:B.buffer;
  domain:Domain.id; mutable storage:storage option;
  mutable generation:int; mutable closed:bool; mutable textures:int}
type output = {owner:t; texture:B.texture; generation:int}
(* image.rays: the packed colours at fragment buffer 0, the width at 1 *)
let resources = [0; 1]
let diagnostic e = Diagnostic.error ~code:"E_GPU" (Ogpu.Error.to_string e)
let gpu result = Result.map_error diagnostic result
let error code message = Error (Diagnostic.error ~code message)
let create_owned device queue =
  if not(Domain.is_main_domain()) then error "E_GPU" "Image sinks require the initial domain."
  else
    let (let*) = Result.bind in
    let shader stage bindings = gpu (Ogpu.Shader.create {backend="metal";label=Some"Flow image RGBA8";
      bytes=Bytes.of_string Image_rgba8_source.source;
      entry_points=[{name="image_vertex";stage=Vertex};{name="image_fragment";stage=Fragment}];
      bindings=List.map (fun binding -> Ogpu.Shader.{group=0;binding;kind=Storage_buffer;visibility=[stage]}) bindings}) in
    let* vertex = shader Vertex [] in
    let* fragment = shader Fragment resources in
    let* bindings = gpu (Ogpu.Binding.create_layout
      (List.map (fun binding -> {Ogpu.Binding.binding;kind=Buffer;visibility=[Fragment]}) resources)) in
    let* layout = gpu (Ogpu.Binding.create_pipeline_layout ~device:(B.device_handle device)
      ~capabilities:(B.capabilities device) [0, bindings]) in
    let* size = gpu (B.create_buffer device {label=Some"Flow image width";size=16L;usage=[Storage]}) in
    match B.create_render_pipeline ~blend:Replace ~topology:Triangle_list ~indirect:false device
      {backend="metal";label=Some"Flow image RGBA8";layout;vertex;vertex_entry="image_vertex";
       fragment=Some fragment;fragment_entry=Some"image_fragment";color_format=Rgba8_unorm;
       depth_format=No_depth;sample_count=1} with
    | Error e -> ignore(B.destroy_buffer size); Error(diagnostic e)
    | Ok pipeline -> Ok {device;queue;pipeline;size;domain=Domain.self();
        storage=None;generation=0;closed=false;textures=0}
let create gpu = create_owned (Renderer.gpu_device gpu) (Renderer.gpu_queue gpu)
let release (storage:storage) = ignore(B.destroy_texture storage.texture)
let allocate t ~width ~height =
  let (let*) = Result.bind in
  let texture_descriptor:Ogpu.Types.texture_descriptor={label=Some"Flow image RGBA8";width;height;depth=1;
    mip_levels=1;sample_count=1;format=Rgba8_unorm;
    usage=[Texture_binding;Render_attachment;Texture_copy_src]} in
  let* ()=gpu(Ogpu.Validation.validate_texture (B.capabilities t.device) texture_descriptor) in
  let* texture = gpu (B.create_texture t.device texture_descriptor) in
  t.textures<-t.textures+1;Ok {texture;width;height}
let get = function Ok value -> value | Error e -> raise(Diagnostic.Error(diagnostic e))
(* The conversion pass: [source]'s width x height vec4s drawn into [texture]. *)
let encode_pass t commands ~source ~width ~height texture =
  let size=Bytes.create 4 in Bytes.set_int32_le size 0 (Int32.bits_of_float(float width));
  get(B.write_buffer t.size ~offset:0L size);
  let render=get(B.render_encoder commands {colors=[{texture;resolve=None;
    load=Dont_care;store=Store;clear=(0.,0.,0.,0.)}];depth=None}) in
  let ended=ref false in
  Fun.protect ~finally:(fun()->if not !ended then ignore(B.end_render render)) (fun()->
    get(B.set_render_pipeline render t.pipeline);
    get(B.set_viewport render {x=0;y=0;width;height});
    get(B.set_stage_buffer render Fragment ~index:0 source);
    get(B.set_stage_buffer render Fragment ~index:1 t.size);
    get(B.draw render ~primitive:Triangle_list ~first:0 ~count:3 ());
    get(B.end_render render);ended:=true)
let encode t commands ~source ~width ~height texture =
  try Ok (encode_pass t commands ~source ~width ~height texture) with Diagnostic.Error e -> Error e
let convert t ~width ~height input =
  if t.closed || Domain.self()<>t.domain then error "E_GPU" "Image sink is closed or called from another domain."
  else if width<=0 || height<=0 || width>Sys.max_string_length/4/height then
    error "E_IMAGE" "Image dimensions exceed native storage bounds."
  else if Run.width input<>4 || Run.count input<>width*height then
    error "E_IMAGE" "Image sink needs one vec4 output per pixel."
  else match Run.buffer input with
  | None -> error "E_GPU" "Image producer output was closed or superseded."
  | Some source ->
    let (let*) = Result.bind in
    (* Invalidate all borrowed images before any allocation, write or submission. *)
    t.generation<-t.generation+1;
    let* storage,fresh = match t.storage with
      | Some old when old.width=width && old.height=height -> Ok(old,false)
      | _ -> Result.map(fun storage->storage,true)(allocate t ~width ~height) in
    let published=ref false in
    Fun.protect ~finally:(fun()->if fresh && not !published then release storage) (fun()->
      try
        let commands=get(B.begin_commands t.queue) in
        let committed=ref false in
        let receipt=Fun.protect ~finally:(fun()->if not !committed then ignore(B.abandon commands)) (fun()->
          encode_pass t commands ~source ~width ~height storage.texture;
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
    ignore(B.destroy_pipeline t.pipeline);ignore(B.destroy_buffer t.size);t.closed<-true
  end
let buffer_creations _ = 1
let texture_creations t = t.textures

module Private = struct
  let create_owned = create_owned
end
