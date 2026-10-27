type texture_profile={format:Types.texture_format;storage:Types.memory;width:int;height:int;depth:int;mip_levels:int;sample_count:int;usage:Types.texture_usage list}
let invalid operation message=Error(Error.make operation Error.Invalid_argument message)
let validate_label ~operation=function None->Ok()|Some label when String.contains label '\000'->invalid operation"label contains NUL"|Some _->Ok()
let validate_buffer_shape ~operation ~max_size ~size ~usage_count=
  if max_size<=0L||size<=0L||size>max_size then invalid operation"buffer size is outside device limits"
  else if usage_count<=0 then invalid operation"buffer usage is empty"else Ok()
let mul_overflows a b=a<>0L&&b>Int64.div Int64.max_int a
let validate_texture_shape ~operation ~max_dimension ~max_samples ~width ~height ~depth ~mip_levels ~sample_count ~usage_count=
  if max_dimension<=0||max_samples<=0||width<=0||height<=0||depth<=0||width>max_dimension||height>max_dimension then invalid operation"texture extent is outside device limits"
  else if mip_levels<=0||sample_count<=0||sample_count>max_samples||sample_count land(sample_count-1)<>0 then invalid operation"texture cardinality is invalid"
  else if usage_count<=0 then invalid operation"texture usage is empty"
  else let w=Int64.of_int width and h=Int64.of_int height and d=Int64.of_int depth in
    if mul_overflows w h||mul_overflows(Int64.mul w h)d then invalid operation"texture texel cardinality overflows"else
    let rec maximum_mips n levels=if n<=1 then levels else maximum_mips(n/2)(levels+1)in
    if mip_levels>maximum_mips(max width(max height depth))1 then invalid operation"texture mip cardinality exceeds its extent"else Ok()
let unique values=List.sort_uniq compare values=values
let validate_texture_profile capabilities value=
  let operation="Ogpu.Validation.validate_texture_profile"in let limits=capabilities.Caps.limits in
  match validate_texture_shape~operation~max_dimension:limits.max_texture_dimension_2d~max_samples:limits.max_sample_count
    ~width:value.width~height:value.height~depth:value.depth~mip_levels:value.mip_levels~sample_count:value.sample_count~usage_count:(List.length value.usage)with
  |Error _ as failure->failure|Ok()->
  if not(unique value.usage)then invalid operation"texture usage must be sorted and unique"
  else if value.sample_count>1&&(value.mip_levels<>1||value.depth<>1||value.usage<>[Types.Render_attachment])then invalid operation"multisample textures require one attachment-only mip and depth slice"
  else if value.storage=Types.Shared&&value.sample_count>1 then invalid operation"shared multisample textures are unsupported"
  else Ok()
let validate_buffer (caps:Caps.t) (value:Types.buffer_descriptor)=
  let operation="Ogpu.Types.validate_buffer"in
  match validate_label~operation value.label with Error _ as failure->failure|Ok()->
  validate_buffer_shape~operation~max_size:caps.Caps.limits.max_buffer_size~size:value.size~usage_count:(List.length value.usage)
let validate_texture (caps:Caps.t) (value:Types.texture_descriptor)=
  let operation="Ogpu.Types.validate_texture"and limits=caps.Caps.limits in
  match validate_label~operation value.label with Error _ as failure->failure|Ok()->
  match validate_texture_shape~operation~max_dimension:limits.max_texture_dimension_2d~max_samples:limits.max_sample_count
    ~width:value.width~height:value.height~depth:value.depth~mip_levels:value.mip_levels~sample_count:value.sample_count~usage_count:(List.length value.usage)with
  |Error _ as failure->failure
  |Ok() when value.format=Types.R8_unorm||value.format=Types.Bgra8_unorm||value.format=Types.Bgra8_unorm_srgb||value.format=Types.Depth32_float->
    invalid operation"OGPU texture descriptors require an RGBA color format"
  |Ok() when value.format<>Types.Rgba8_unorm&&(List.mem Types.Render_attachment value.usage||value.sample_count>1)->
    Error(Error.make operation Error.Unsupported"float textures are not render attachments")
  |Ok()->Ok()
let validate_sampler (value:Types.sampler_descriptor)=
  let operation="Ogpu.Types.validate_sampler"in
  match validate_label~operation value.label with Error _ as failure->failure|Ok()->
  if not(Float.is_finite value.lod_min)||not(Float.is_finite value.lod_max)||value.lod_min<0.||value.lod_max<value.lod_min||value.max_anisotropy<1||value.max_anisotropy>16
  then Error(Error.make operation Error.Invalid_argument"sampler LOD/anisotropy limits are invalid")else Ok()
let validate_layout ~operation entries=
  let entries=List.sort(fun(a,_)(b,_)->Int.compare a b)entries in
  let rec loop previous=function
    |[]->Ok()
    |(binding,_)::_ when binding<0->invalid operation"binding is negative"
    |(_,visibility)::_ when visibility=[]->invalid operation"visibility is empty"
    |(_,visibility)::_ when List.sort_uniq Int.compare visibility<>visibility->invalid operation"visibility must be sorted and unique"
    |(binding,_)::_ when previous=Some binding->invalid operation"duplicate binding"
    |(binding,_)::rest->loop(Some binding)rest
  in loop None entries
let validate_groups ~operation ~max_groups groups=
  let groups=List.sort Int.compare groups in
  if max_groups<0||List.length groups>max_groups then Error(Error.make operation Error.Capacity"bind-group limit exceeded")else
  let rec loop expected=function []->Ok()|group::_ when group<>expected->invalid operation"groups must be contiguous from zero"|_::rest->loop(expected+1)rest in loop 0 groups
