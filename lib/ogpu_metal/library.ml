type t = {
  metal : Metal.Library.t;
  device : Device.t;
  handle : unit Ogpu_core.Handle.t;
  mutable pipelines : int;
  mutable dead : bool;
}

let error op kind message = Error (Ogpu_core.Error.make op kind message)

let compile_native op device shader =
  if Ogpu_core.Shader.backend shader <> "metal" then
    error op Ogpu_core.Error.Invalid_argument "shader backend is not metal"
  else
    let bytes = Bytes.to_string (Ogpu_core.Shader.bytes shader) in
    let result =
      Metal.Library.compile_source ~device:(Device.Private.metal device)
        ?label:(Ogpu_core.Shader.label shader) bytes
    in
    Result.map_error (Device.of_metal_error ~operation:op) result

let create device shader =
  let op = "Ogpu_metal.Library.create" in
  if Device.destroyed device then error op Ogpu_core.Error.Stale_handle "device is destroyed"
  else
    match compile_native op device shader with
    | Error _ as e -> e
    | Ok metal ->
        Device.Private.attach_resource device;
        Ok
          {
            metal;
            device;
            handle = Ogpu_core.Handle.create ~device:(Device.Private.handle device);
            pipelines = 0;
            dead = false;
          }

let validate device value =
  Ogpu_core.Handle.validate_for ~operation:"Ogpu_metal.Library.validate"
    (Device.Private.handle device) value.handle

let destroy value =
  let op = "Ogpu_metal.Library.destroy" in
  if value.dead then Ok ()
  else if value.pipelines > 0 then
    error op Ogpu_core.Error.Invalid_state "library has live pipelines"
  else
    match Metal.Library.destroy value.metal with
    | Error e -> Error (Device.of_metal_error ~operation:op e)
    | Ok () ->
        value.dead <- true;
        Ogpu_core.Handle.destroy value.handle;
        Device.Private.detach_resource value.device;
        Ok ()

module Private = struct
  let metal value = value.metal
  let attach_pipeline value = value.pipelines <- value.pipelines + 1
  let detach_pipeline value = value.pipelines <- value.pipelines - 1
end
