let error operation kind message = Error (Ogpu_core.Error.make operation kind message)
module Build = Metal.Acceleration_structure.Build

type t =
  { device : Device.t; descriptor : Build.t option; metal : Metal.Acceleration_structure.t
  ; sizes : Metal.Acceleration_structure.sizes;  buffers : Buffer.t list; structures : t list
  ; mutable dead : bool; mutable submission_uses : int; mutable destroy_requested : bool }

let validate_scratch_plan ~buffer_size ~offset ~required =
  let operation = "Ogpu_metal.Acceleration.validate_scratch_plan" in
  if buffer_size < 0L || offset < 0L || required < 0L
     || Int64.rem offset 256L <> 0L || offset > Int64.sub buffer_size required
  then error operation Ogpu_core.Error.Invalid_argument "scratch range is invalid or insufficient"
  else Ok ()

let validate operation device value =
  if value.dead then error operation Ogpu_core.Error.Stale_handle "acceleration structure is destroyed"
  else if Device.destroyed device then error operation Ogpu_core.Error.Stale_handle "device is destroyed"
  else if Device.id device <> Device.id value.device then error operation Ogpu_core.Error.Cross_device "acceleration structure belongs to another device"
  else Ok ()

let allocate operation device descriptor sizes ~buffers ~structures =
  match Metal.Acceleration_structure.create ~device:(Device.Private.metal device)
          ~size:sizes.Metal.Acceleration_structure.acceleration_structure_size with
  | Error metal -> Error (Device.of_metal_error ~operation metal)
  | Ok metal ->
      Device.Private.attach_resource device;
      Ok { device; descriptor; metal; sizes;  buffers; structures; dead = false
         ; submission_uses = 0; destroy_requested = false }

let keyframes (frames : (Buffer.t * int64) array) : Build.keyframe list =
  Array.to_list (Array.map (fun (buffer, offset) -> { Build.buffer = Buffer.Private.metal buffer; offset }) frames)

(* Metal's [Build] validates every range; the caller already resolved tokens. *)
let build_geometry (geometry : Ogpu_core.Backend.driver_geometry)
    ~(resolve : int64 -> (Buffer.t, Ogpu_core.Error.t) result) =
  let frames tokens =
    let resolved = Array.map (fun (token, offset) -> Result.map (fun b -> (b, offset)) (resolve token)) tokens in
    match Array.to_list resolved |> List.find_opt Result.is_error with
    | Some (Error e) -> Error e
    | _ -> Ok (Array.map Result.get_ok resolved) in
  let common ~opaque ~duplicate : Build.common =
    { opaque; allow_duplicate_intersection = duplicate } in
  match geometry with
  | Ogpu_core.Backend.Driver_triangles { vertices; offset; length = _; vertex_stride; vertex_count } ->
      Result.map (fun vertices ->
        (Build.Triangles { vertices = keyframes [| (vertices, offset) |]; vertex_stride = Int64.of_int vertex_stride
                         ; triangle_count = Int64.of_int (vertex_count / 3); index = None
                         ; common = Build.default_common }, [ vertices ])) (resolve vertices)
  | Driver_motion_triangles { keyframes = frames_tokens; vertex_stride; vertex_count } ->
      Result.map (fun frames_resolved ->
        (Build.Triangles { vertices = keyframes frames_resolved; vertex_stride = Int64.of_int vertex_stride
                         ; triangle_count = Int64.of_int (vertex_count / 3); index = None
                         ; common = Build.default_common }, List.map fst (Array.to_list frames_resolved)))
        (frames frames_tokens)
  | Driver_boxes { boxes; stride; count; opaque; duplicate } ->
      Result.map (fun frames_resolved ->
        (Build.Bounding_boxes { boxes = keyframes frames_resolved; stride = Int64.of_int stride
                              ; count = Int64.of_int count; common = common ~opaque ~duplicate }
        , List.map fst (Array.to_list frames_resolved))) (frames boxes)
  | Driver_curves { control; control_stride; control_count; radii; radius_stride; indices; index_offset
                  ; segment_count; per_segment; curve_type; basis; caps } -> (
      match frames control, frames radii, resolve indices with
      | Ok control, Ok radii, Ok indices ->
          Ok (Build.Curves
                { control_points = keyframes control; control_stride = Int64.of_int control_stride
                ; control_point_count = Int64.of_int control_count; radii = keyframes radii
                ; radius_stride = Int64.of_int radius_stride
                ; index = { index_buffer = Buffer.Private.metal indices; index_offset; index_uint16 = false }
                ; segment_count = Int64.of_int segment_count; control_points_per_segment = per_segment
                ; curve_type = (if curve_type = 0 then Build.Round else Build.Flat)
                ; basis = (match basis with 0 -> Build.Bspline | 1 -> Build.Catmull_rom | 2 -> Build.Linear | _ -> Build.Bezier)
                ; end_caps = (match caps with 0 -> Build.No_caps | 1 -> Build.Disk | _ -> Build.Sphere)
                ; common = Build.default_common }
             , List.map fst (Array.to_list control) @ List.map fst (Array.to_list radii) @ [ indices ])
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)

let create device (descriptor : Ogpu_core.Backend.driver_accel_descriptor)
    ~(resolve_buffer : int64 -> (Buffer.t, Ogpu_core.Error.t) result)
    ~(resolve_structure : int64 -> (t, Ogpu_core.Error.t) result) =
  let operation = "Ogpu_metal.Acceleration.create" in
  if not (Device.capabilities device).Ogpu_core.Caps.ray_tracing then
    error operation Ogpu_core.Error.Unsupported "ray tracing is unavailable on this adapter"
  else
    let native = Device.Private.metal device in
    let sizes_of build = Result.map_error (Device.of_metal_error ~operation) (Build.sizes ~device:native build) in
    match descriptor with
    | Driver_blas { geometries; motion } -> (
        let rec convert acc buffers = function
          | [] -> Ok (List.rev acc, buffers)
          | geometry :: rest -> (
              match build_geometry geometry ~resolve:resolve_buffer with
              | Error _ as e -> e
              | Ok (g, bs) -> convert (g :: acc) (buffers @ bs) rest) in
        match convert [] [] (Array.to_list geometries) with
        | Error _ as e -> e
        | Ok (geometries, buffers) -> (
            let motion = Option.map (fun (m : Ogpu_core.Backend.driver_motion) : Build.motion ->
              { keyframe_count = m.motion_keyframes; start_time = m.motion_start; end_time = m.motion_end
              ; start_border = (if m.motion_start_border = 0 then Build.Clamp else Build.Vanish)
              ; end_border = (if m.motion_end_border = 0 then Build.Clamp else Build.Vanish) }) motion in
            match Build.primitive native ?motion geometries with
            | Error metal -> Error (Device.of_metal_error ~operation metal)
            | Ok build -> (
                match sizes_of build with
                | Error _ as e -> ignore (Build.destroy build); e
                | Ok sizes -> allocate operation device (Some build) sizes ~buffers ~structures:[])))
    | Driver_tlas { instances; offset; instance_count; kind; structures; motion_transforms } -> (
        match resolve_buffer instances with
        | Error _ as e -> e
        | Ok instances -> (
            let resolved = Array.map resolve_structure structures in
            match Array.to_list resolved |> List.find_opt Result.is_error with
            | Some (Error e) -> Error e
            | _ -> (
                let structures = Array.to_list (Array.map Result.get_ok resolved) in
                if List.exists (fun s -> match s.descriptor with Some d -> Build.instance_kind d <> None | None -> false) structures then
                  error operation Ogpu_core.Error.Invalid_argument "instances must reference primitive structures"
                else
                  let transforms = match motion_transforms with
                    | None -> Ok None
                    | Some (token, offset, count) ->
                        Result.map (fun b -> Some (b, offset, count)) (resolve_buffer token) in
                  match transforms with
                  | Error _ as e -> e
                  | Ok transforms -> (
                      let kind = match kind with
                        | Ogpu_core.Backend.User_id_instances -> Build.User_id_instances
                        | Motion_instances -> Build.Motion_instances in
                      match Build.instances native ~buffer:(Buffer.Private.metal instances) ~offset
                              ~count:(Int64.of_int instance_count) ~kind
                              ?motion_transforms:(Option.map (fun ((b : Buffer.t), o, c) -> (Buffer.Private.metal b, o, Int64.of_int c)) transforms)
                              (Array.of_list (List.map (fun s -> s.metal) structures)) with
                      | Error metal -> Error (Device.of_metal_error ~operation metal)
                      | Ok build -> (
                          match sizes_of build with
                          | Error _ as e -> ignore (Build.destroy build); e
                          | Ok sizes ->
                              allocate operation device (Some build) sizes
                                ~buffers:(instances :: Option.fold ~none:[] ~some:(fun (b, _, _) -> [ b ]) transforms)
                                ~structures)))))
    | Driver_sized { size; template } -> (
        match resolve_structure template with
        | Error _ as e -> e
        | Ok template ->
            if size <= 0L then error operation Ogpu_core.Error.Invalid_argument "structure size must be positive"
            else
              allocate operation device None
                { template.sizes with acceleration_structure_size = size }
                ~buffers:template.buffers ~structures:template.structures)

let sizes value = value.sizes

let validate_scratch operation device value scratch scratch_offset required =
  match validate operation device value with Error _ as failure -> failure | Ok () ->
  match Buffer.descriptor device scratch with Error _ as failure -> failure | Ok descriptor ->
  validate_scratch_plan ~buffer_size:descriptor.size ~offset:scratch_offset ~required

let with_descriptor operation value f =
  match value.descriptor with
  | None -> error operation Ogpu_core.Error.Invalid_state "a sized structure is filled by copy or compaction"
  | Some descriptor -> f descriptor

let encode_build encoder device value ~scratch ~scratch_offset =
  let operation = "Ogpu_metal.Acceleration.encode_build" in
  match validate_scratch operation device value scratch scratch_offset value.sizes.build_scratch_buffer_size with
  | Error _ as failure -> failure
  | Ok () -> with_descriptor operation value (fun descriptor ->
      Result.map_error (Device.of_metal_error ~operation)
        (Metal.Acceleration_encoder.build_with encoder ~destination:value.metal ~descriptor
           ~scratch:(Buffer.Private.metal scratch) ~scratch_offset))

let encode_pair operation encoder device ~src ~dst encode =
  match validate operation device src with Error _ as failure -> failure | Ok () ->
  match validate operation device dst with Error _ as failure -> failure | Ok () ->
  Result.map_error (Device.of_metal_error ~operation) (encode encoder ~source:src.metal ~destination:dst.metal)

let encode_compact encoder device ~src ~dst =
  encode_pair "Ogpu_metal.Acceleration.encode_compact" encoder device ~src ~dst
    Metal.Acceleration_encoder.copy_and_compact

let encode_compacted_size encoder device value ~destination ~offset =
  let operation = "Ogpu_metal.Acceleration.encode_compacted_size" in
  match validate operation device value with Error _ as failure -> failure | Ok () ->
  Result.map_error (Device.of_metal_error ~operation)
    (Metal.Acceleration_encoder.write_compacted_size_typed encoder ~source:value.metal
       ~destination:(Buffer.Private.metal destination) ~offset Metal.Acceleration_encoder.Uint64)

let finish_destroy value =
  let operation = "Ogpu_metal.Acceleration.destroy" in
  let released = match value.descriptor with None -> Ok () | Some descriptor -> Build.destroy descriptor in
  match released, Metal.Acceleration_structure.destroy value.metal with
  | Error metal, _ | Ok (), Error metal -> Error (Device.of_metal_error ~operation metal)
  | Ok (), Ok () -> Device.Private.detach_resource value.device; Ok ()

let destroy value =
  if value.dead then Ok ()
  else if value.submission_uses > 0 then (value.dead <- true; value.destroy_requested <- true; Ok ())
  else match finish_destroy value with Error _ as failure -> failure | Ok () -> value.dead <- true; Ok ()

module Private = struct
  let metal value = value.metal
  let rec retain_structure value = retain_submission value
  and retain_submission value =
    if value.dead then error "Ogpu_metal.Acceleration.retain_submission" Ogpu_core.Error.Stale_handle "acceleration structure is destroyed"
    else
      let retained = ref [] in
      let keep retain release = match retain () with
        | Error _ as failure -> List.iter (fun release -> release ()) !retained; failure
        | Ok () -> retained := release :: !retained; Ok () in
      let rec buffers = function
        | [] -> Ok ()
        | buffer :: rest -> (match keep (fun () -> Buffer.Private.retain_submission buffer) (fun () -> Buffer.Private.release_submission buffer) with Error _ as failure -> failure | Ok () -> buffers rest) in
      let rec structures = function
        | [] -> Ok ()
        | structure :: rest -> (match keep (fun () -> Result.map (fun release -> retained := release :: !retained) (retain_structure structure)) (fun () -> ()) with Error _ as failure -> failure | Ok () -> structures rest) in
      match buffers value.buffers with
      | Error _ as failure -> failure
      | Ok () ->
          match structures value.structures with
          | Error _ as failure -> failure
          | Ok () ->
          value.submission_uses <- value.submission_uses + 1;
          Ok (fun () ->
            List.iter (fun release -> release ()) !retained;
            value.submission_uses <- value.submission_uses - 1;
            if value.submission_uses = 0 && value.destroy_requested then ignore (finish_destroy value))
end
