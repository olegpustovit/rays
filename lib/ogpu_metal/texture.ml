type format = Ogpu_core.Types.texture_format = R8_unorm | Rgba8_unorm | Bgra8_unorm | Bgra8_unorm_srgb | Rgba16_float | Rgba32_float | Depth32_float
type memory = Ogpu_core.Types.memory = Shared | Device_local
type t =
  { metal : Metal.Texture.t; handle : unit Ogpu_core.Handle.t; device : Device.t
  ; descriptor : Ogpu_core.Types.texture_descriptor; format : format
  ;  parent : t option; mutable live_views : int;
    mutable submission_uses:int;mutable destroy_requested:bool }

let error operation kind message = Error (Ogpu_core.Error.make operation kind message)
let metal_format = function R8_unorm->Metal.Texture.R8_unorm|Rgba8_unorm->Metal.Texture.Rgba8_unorm|Bgra8_unorm->Metal.Texture.Bgra8_unorm|Bgra8_unorm_srgb->Metal.Texture.Bgra8_unorm_srgb|Rgba16_float->Metal.Texture.Rgba16_float|Rgba32_float->Metal.Texture.Rgba32_float|Depth32_float->Metal.Texture.Depth32_float
let bytes_per_pixel = function R8_unorm->1|Rgba8_unorm|Bgra8_unorm|Bgra8_unorm_srgb|Depth32_float->4|Rgba16_float->8|Rgba32_float->16
let add_unique x xs = if List.mem x xs then xs else x :: xs
let validate_descriptor (value : Ogpu_core.Types.texture_descriptor) format =
  let operation="Ogpu_metal.Texture.create" in
  let max_mips = 1 + int_of_float (Float.floor (Float.log2 (float_of_int (max value.width (max value.height value.depth))))) in
  if value.mip_levels > max_mips then error operation Ogpu_core.Error.Invalid_argument "mip count exceeds texture extent"
  else if not (List.mem value.sample_count [1;4;9;16]) then error operation Ogpu_core.Error.Invalid_argument "sample count must be 1, 4, 9, or 16"
  else if value.sample_count > 1 && (value.depth <> 1 || value.mip_levels <> 1) then error operation Ogpu_core.Error.Invalid_argument "multisample textures are 2D and single-mip"
  else if (format=Depth32_float) && List.mem Ogpu_core.Types.Texture_binding value.usage then error operation Ogpu_core.Error.Invalid_argument "depth/stencil sampling is not exposed by this foundation"
  else Ok ()

let usage values =
  let native=List.fold_left (fun result -> function
    | Ogpu_core.Types.Texture_binding -> add_unique Metal.Texture.Shader_read result
    | Storage_binding -> add_unique Metal.Texture.Shader_write result
    | Render_attachment -> add_unique Metal.Texture.Render_target result
    | Texture_copy_src | Texture_copy_dst -> result) [] values in
  (* Metal expands an empty usage mask during creation; supply one explicit
     native bit for copy-only textures so descriptor validation stays exact. *)
  if native=[] then [Metal.Texture.Shader_read] else native

(* The validated native descriptor for an OGPU texture; shared by device and
   heap creation and by heap placement queries. *)
let native_descriptor ~operation device ~memory ~format (descriptor:Ogpu_core.Types.texture_descriptor) =
  if Device.destroyed device then error operation Ogpu_core.Error.Stale_handle "device is destroyed"
  else match Ogpu_core.Validation.validate_texture (Device.capabilities device) descriptor with Error _ as e->e|Ok()->
    let profile:Ogpu_core.Validation.texture_profile={format;storage=memory;
      width=descriptor.width;height=descriptor.height;depth=descriptor.depth;mip_levels=descriptor.mip_levels;
      sample_count=descriptor.sample_count;usage=descriptor.usage}in
    match Ogpu_core.Validation.validate_texture_profile(Device.capabilities device)profile with Error _ as e->e|Ok()->
    match validate_descriptor descriptor format with Error _ as e->e|Ok()->
    let storage=match memory with Device_local->Metal.Buffer.Private|Shared->Metal.Buffer.Shared in
    let native_usage=usage descriptor.usage in
    let base=Metal.Texture.descriptor_2d ~storage ~usage:native_usage ?label:descriptor.label ~format:(metal_format format) ~width:descriptor.width ~height:descriptor.height () in
    Ok{base with kind=(if descriptor.sample_count>1 then Metal.Texture.Texture_2d_multisample else if descriptor.depth>1 then Texture_3d else Texture_2d);depth=descriptor.depth;mip_levels=descriptor.mip_levels;sample_count=descriptor.sample_count}

let wrap device ~operation ~format descriptor created=
  match created with Error e->Error(Device.of_metal_error~operation e)|Ok metal->
    let value={metal;handle=Ogpu_core.Handle.create~device:(Device.Private.handle device);device;descriptor;format;parent=None;live_views=0;submission_uses=0;destroy_requested=false}in
    Device.Private.attach_resource device;Ok value

let create device ~memory ~format descriptor =
  let operation="Ogpu_metal.Texture.create" in
  match native_descriptor ~operation device ~memory ~format descriptor with Error _ as e->e|Ok native->
  wrap device ~operation ~format descriptor (Metal.Texture.create ~device:(Device.Private.metal device) native)

let validate operation device value=Ogpu_core.Handle.validate_for~operation(Device.Private.handle device)value.handle
let destroyed value=Ogpu_core.Handle.destroyed value.handle
let descriptor device value=Result.map(fun()->value.descriptor)(validate"Ogpu_metal.Texture.descriptor"device value)
let format device value=Result.map(fun()->value.format)(validate"Ogpu_metal.Texture.format"device value)

let read_bytes device value ~mip_level ~bytes_per_row =
  let operation="Ogpu_metal.Texture.read_bytes"in match validate operation device value with Error _ as e->e|Ok()->
  if mip_level<0||mip_level>=value.descriptor.mip_levels then error operation Ogpu_core.Error.Invalid_argument "mip level is outside the texture"
  else let width=max 1(value.descriptor.width lsr mip_level)and height=max 1(value.descriptor.height lsr mip_level)in
    let minimum=width*bytes_per_pixel value.format in
    if bytes_per_row<minimum||bytes_per_row>max_int/height then error operation Ogpu_core.Error.Invalid_argument "bytes_per_row is invalid"
    else match Metal.Texture.read_bytes value.metal~region:{x=0;y=0;z=0;width;height;depth=1}~mip_level~slice:0~bytes_per_row~bytes_per_image:(bytes_per_row*height)with Ok x->Ok x|Error e->Error(Device.of_metal_error~operation e)

let read_bytes_into device value ~mip_level ~bytes_per_row ~destination =
  let operation="Ogpu_metal.Texture.read_bytes_into"in
  match validate operation device value with Error _ as e->e|Ok()->
  if mip_level<0||mip_level>=value.descriptor.mip_levels then
    error operation Ogpu_core.Error.Invalid_argument "mip level is outside the texture"
  else
    let width=max 1(value.descriptor.width lsr mip_level)
    and height=max 1(value.descriptor.height lsr mip_level)in
    let minimum=width*bytes_per_pixel value.format in
    if bytes_per_row<minimum||bytes_per_row>max_int/height then
      error operation Ogpu_core.Error.Invalid_argument "bytes_per_row is invalid"
    else
      let required=bytes_per_row*height in
      if Bytes.length destination<>required then
        error operation Ogpu_core.Error.Invalid_argument
          "destination length does not match the texture row layout"
      else
        match Metal.Texture.read_bytes_into value.metal
          ~region:{x=0;y=0;z=0;width;height;depth=1}~mip_level~slice:0
          ~bytes_per_row~bytes_per_image:required~destination with
        |Ok()->Ok()|Error e->Error(Device.of_metal_error~operation e)

let destroy value =
  let operation="Ogpu_metal.Texture.destroy"in if destroyed value then Ok()else if value.live_views<>0 then error operation Ogpu_core.Error.Invalid_state "texture still owns live views"else
  if value.submission_uses>0 then(Ogpu_core.Handle.destroy value.handle;value.destroy_requested<-true;Ok())else
  match Metal.Texture.destroy value.metal with Error e->Error(Device.of_metal_error~operation e)|Ok()->Ogpu_core.Handle.destroy value.handle;Option.iter(fun parent->parent.live_views<-parent.live_views-1)value.parent;Device.Private.detach_resource value.device;Ok()

module Private=struct
  let metal value=value.metal
  let retain_submission value=if destroyed value then Error(Ogpu_core.Error.make"Ogpu_metal.Texture.retain_submission"Ogpu_core.Error.Stale_handle"texture is destroyed")else(value.submission_uses<-value.submission_uses+1;Ok())
  let release_submission value=value.submission_uses<-value.submission_uses-1;if value.submission_uses=0&&value.destroy_requested then(match Metal.Texture.destroy value.metal with Ok()->Option.iter(fun parent->parent.live_views<-parent.live_views-1)value.parent;Device.Private.detach_resource value.device|Error _->())
end
