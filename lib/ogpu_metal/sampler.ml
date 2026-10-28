type filter = Ogpu_core.Types.sampler_filter = Nearest | Linear
type mip_filter = Ogpu_core.Types.mip_filter = No_mip | Nearest_mip | Linear_mip
type address_mode = Ogpu_core.Types.address_mode = Clamp_to_edge | Repeat | Mirror_repeat
type descriptor = Ogpu_core.Types.sampler_descriptor
type sampler_kind

type t = {
  metal : Metal.Sampler.t;
  handle : sampler_kind Ogpu_core.Handle.t;
  device : Device.t;
  mutable submissions : int;
}

let filter = function Nearest -> Metal.Sampler.Nearest | Linear -> Metal.Sampler.Linear

let mip = function
  | No_mip -> Metal.Sampler.Not_mipmapped
  | Nearest_mip -> Mip_nearest
  | Linear_mip -> Mip_linear

let address = function
  | Clamp_to_edge -> Metal.Sampler.Clamp_to_edge
  | Repeat -> Metal.Sampler.Repeat
  | Mirror_repeat -> Metal.Sampler.Mirror_repeat

let create device (descriptor : descriptor) =
  let operation = "Ogpu_metal.Sampler.create" in
  if Device.destroyed device then
    Error (Ogpu_core.Error.make operation Ogpu_core.Error.Stale_handle "device is destroyed")
  else if
    descriptor.max_anisotropy < 1 || descriptor.max_anisotropy > 16
    || (not (Float.is_finite descriptor.lod_min))
    || (not (Float.is_finite descriptor.lod_max))
    || descriptor.lod_min < 0.
    || descriptor.lod_max < descriptor.lod_min
  then
    Error
      (Ogpu_core.Error.make operation Ogpu_core.Error.Invalid_argument "sampler limits are invalid")
  else
    let native =
      {
        (Metal.Sampler.default ?label:descriptor.label ()) with
        min_filter = filter descriptor.min_filter;
        mag_filter = filter descriptor.mag_filter;
        mip_filter = mip descriptor.mip_filter;
        max_anisotropy = descriptor.max_anisotropy;
        s_address = address descriptor.address_u;
        t_address = address descriptor.address_v;
        r_address = address descriptor.address_v;
        lod_min_clamp = descriptor.lod_min;
        lod_max_clamp = descriptor.lod_max;
        support_argument_buffers = true;
      }
    in
    match Metal.Sampler.create ~device:(Device.Private.metal device) native with
    | Error e -> Error (Device.of_metal_error ~operation e)
    | Ok metal ->
        let value =
          {
            metal;
            handle = Ogpu_core.Handle.create ~device:(Device.Private.handle device);
            device;
            submissions = 0;
          }
        in
        Device.Private.attach_resource device;
        Ok value

let destroyed value = Ogpu_core.Handle.destroyed value.handle

let destroy value =
  let operation = "Ogpu_metal.Sampler.destroy" in
  if destroyed value then Ok ()
  else if value.submissions > 0 then
    Error
      (Ogpu_core.Error.make operation Ogpu_core.Error.Invalid_state
         "sampler is retained by submitted work")
  else
    match Metal.Sampler.destroy value.metal with
    | Error e -> Error (Device.of_metal_error ~operation e)
    | Ok () ->
        Ogpu_core.Handle.destroy value.handle;
        Device.Private.detach_resource value.device;
        Ok ()

module Private = struct
  let metal value = value.metal

  let retain_submission value =
    if destroyed value then
      Error
        (Ogpu_core.Error.make "Ogpu_metal.Sampler.retain" Ogpu_core.Error.Stale_handle
           "sampler is destroyed")
    else (
      value.submissions <- value.submissions + 1;
      Ok ())

  let release_submission value =
    if value.submissions > 0 then value.submissions <- value.submissions - 1
end
