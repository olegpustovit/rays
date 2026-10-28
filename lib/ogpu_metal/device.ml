type t =
  { metal : Metal.Device.t
  ; handle : Ogpu_core.Handle.device
  ; profile : Ogpu_core.Caps.t
  ; mutable generation : int64
  ; mutable live_resources : int
  }

let error operation kind message = Error (Ogpu_core.Error.make operation kind message)

let error_kind ~operation ~source = function
    | Metal.Invalid_argument -> Ogpu_core.Error.Invalid_argument
    | Metal.Invalid_state | Metal.Parent_has_dependents -> Ogpu_core.Error.Invalid_state
    | Metal.Destroyed -> Ogpu_core.Error.Stale_handle
    | Metal.Device_mismatch -> Ogpu_core.Error.Cross_device
    | Metal.Unsupported -> Ogpu_core.Error.Unsupported
    | Metal.Native_error when operation = "Ogpu_metal.Device.system_default" &&
        source = "Metal.Device.system_default" -> Ogpu_core.Error.No_adapter
    | Metal.Native_error | Metal.Wrong_domain ->
        Ogpu_core.Error.Device_lost
let of_metal_error ~operation (value : Metal.error) =
  let kind = error_kind ~operation ~source:value.operation value.kind in
  Ogpu_core.Error.make operation kind value.message

let system_default () =
  let operation = "Ogpu_metal.Device.system_default" in
  match Metal.Device.system_default () with
  | Error value -> Error (of_metal_error ~operation value)
  | Ok metal ->
      (match Metal.Device.info metal with
       | Error value ->
           ignore (Metal.Device.destroy metal);
           Error (of_metal_error ~operation value)
       | Ok info ->
           let rec maximum_sample current=function
             |[]->Ok current
             |sample::rest->(match Metal.Device.supports_texture_sample_count metal sample with
               |Error value->Error(of_metal_error~operation value)
               |Ok true->maximum_sample sample rest|Ok false->maximum_sample current rest)in
           match maximum_sample 1[4;9;16]with Error _ as failure->ignore(Metal.Device.destroy metal);failure|Ok max_sample_count->
           (* Curve primitives intersect only on Apple GPU family 9 and later. *)
           let family f = match Metal.Device.supports_family metal f with Ok true -> true | _ -> false in
           let curves = family Metal.Device.Apple9 in
           let capabilities : Ogpu_core.Caps.t =
             { limits =
                 { max_buffer_size = info.max_buffer_length
                 ; max_texture_dimension_2d = 16_384
                 ; max_bind_groups = 4
                 ; max_sample_count }
             ; compute_pipeline = true
             ; render_pipeline = true
             ; ray_tracing = info.raytracing
             ; ray_tracing_curves = info.raytracing && curves
             ; ray_tracing_motion = info.raytracing }
           in
           match Ogpu_core.Caps.validate capabilities with
           | Error _ as failure -> ignore (Metal.Device.destroy metal); failure
           | Ok () ->
               Ok { metal; handle = Ogpu_core.Handle.create_device (); profile = capabilities;
                    generation = 1L; live_resources = 0 })

let id value = Ogpu_core.Handle.device_id value.handle
let capabilities value = value.profile
let destroyed value = Ogpu_core.Handle.device_destroyed value.handle

let destroy value =
  let operation = "Ogpu_metal.Device.destroy" in
  if destroyed value then Ok ()
  else if value.live_resources <> 0 then
    error operation Ogpu_core.Error.Invalid_state "device still owns live resources"
  else
    match Metal.Device.destroy value.metal with
    | Error metal -> Error (of_metal_error ~operation metal)
    | Ok () ->
        Ogpu_core.Handle.destroy_device value.handle;
        value.generation <- Int64.succ value.generation;
        Ok ()

module Private = struct
  let error_kind = error_kind
  let metal value = value.metal
  let handle value = value.handle
  let attach_resource value = value.live_resources <- value.live_resources + 1
  let detach_resource value = value.live_resources <- value.live_resources - 1
end
