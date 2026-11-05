(* OGPU's driver records over the Vulkan bridge. Tokens name native objects; the checks live in
   Ogpu_core.Backend, as for Metal. *)
module B = Ogpu_core.Backend
module E = Ogpu_core.Error
module V = Vulkan

let live = ref 0
let live_handles () = !live
let error op kind text = Error (E.make op kind text)

(* Every VkResult becomes an Ogpu error here, and nowhere else. *)
let of_result op code =
  let code = if code < -10 && code > -30 then code + 10 else code in
  if code = V.no_device then error op E.No_adapter "no discrete Vulkan 1.3 device"
  else if code = V.missing_feature then
    error op E.No_adapter "the Vulkan device lacks a feature Rays requires"
  else if code = V.error_device_lost then error op E.Device_lost "VK_ERROR_DEVICE_LOST"
  else if code = V.error_out_of_device_memory then
    error op E.Capacity "VK_ERROR_OUT_OF_DEVICE_MEMORY"
  else if code = V.error_out_of_host_memory then error op E.Capacity "VK_ERROR_OUT_OF_HOST_MEMORY"
  else error op E.Io (Printf.sprintf "VkResult %d" code)

let handle op value = if value < 0 then of_result op value else Ok value
let checked op code = if code < 0 then of_result op code else Ok ()

type buffer = { buffer : V.handle; size : int64 }

type texture = {
  texture : V.handle;
  descriptor : Ogpu_core.Types.texture_descriptor;
}

(* ponytail: an indirect command buffer is its recorded calls, replayed into the encoder; secondary
   command buffers if replay shows in a frame's CPU time. *)
type icb_command = {
  mutable command_pipeline : int64 option;
  mutable command_buffers : (int * int64 * int64) list;
  mutable command_draw : (B.driver_render_encoder -> (unit, E.t) result) option;
}

let format_index : Ogpu_core.Types.texture_format -> int = function
  | R8_unorm -> 0
  | Rgba8_unorm -> 1
  | Bgra8_unorm -> 2
  | Bgra8_unorm_srgb -> 3
  | Rgba16_float -> 4
  | Rgba32_float -> 5
  | Depth32_float -> 6

let primitive_index : Ogpu_core.Render_pass.primitive -> int = function
  | Point_list -> 0
  | Line_list -> 1
  | Triangle_list -> 2
  | Triangle_strip -> 3

let index_kind : Ogpu_core.Render_pass.index_type -> int = function Uint16 -> 0 | Uint32 -> 1
let load_index : Ogpu_core.Render_pass.load -> int = function Load -> 0 | Clear -> 1 | Dont_care -> 2
let store_index : Ogpu_core.Render_pass.store -> int = function Store -> 0 | Discard -> 1 | Resolve -> 2

let compare_index : Ogpu_core.Render_pass.comparison -> int = function
  | Never -> 0
  | Less -> 1
  | Equal -> 2
  | Less_equal -> 3
  | Greater -> 4
  | Not_equal -> 5
  | Greater_equal -> 6
  | Always -> 7

let blend_index : Ogpu_core.Pipeline.blend -> int = function
  | Replace -> 0
  | Alpha -> 1
  | Add -> 2
  | Multiply -> 3
  | Screen -> 4
  | Subtract -> 5

let validation_mode () =
  match Sys.getenv_opt "RAYS_VULKAN_VALIDATION" with
  | Some "fatal" -> (true, true)
  | Some "on" -> (true, false)
  | _ -> (false, false)

let create () =
  let device_live = ref false in
  let create_device () =
    if !device_live then
      error "Ogpu_vulkan.create_device" E.Invalid_state "adapter already owns a live device"
    else
      let validation, fatal = validation_mode () in
      match handle "Ogpu_vulkan.create_device" (V.device_create validation fatal) with
      | Error _ as e -> e
      | Ok device ->
          device_live := true;
          let next = ref 1L in
          let token () =
            let x = !next in
            next := Int64.succ x;
            x
          in
          let buffers : (int64, buffer) Hashtbl.t = Hashtbl.create 32
          and textures : (int64, texture) Hashtbl.t = Hashtbl.create 32
          and pipelines : (int64, V.handle * (int * int * int)) Hashtbl.t = Hashtbl.create 16
          and samplers : (int64, V.handle) Hashtbl.t = Hashtbl.create 8
          and icbs : (int64, icb_command array) Hashtbl.t = Hashtbl.create 8
          (* An argument slice is a (texture, sampler) pair: binding the buffer at its offset binds
             those at the slot and the next one, the shape SPIR-V's fragments declare. *)
          and arguments : (int64 * int64, (V.handle * V.handle) ref) Hashtbl.t = Hashtbl.create 16
          and frames : (int64, V.handle) Hashtbl.t = Hashtbl.create 4
          and accels : (int64, V.handle) Hashtbl.t = Hashtbl.create 8
          and accel_resources : (int64, B.driver_resource list) Hashtbl.t = Hashtbl.create 8
          and accel_geometries : (int64, int array array) Hashtbl.t = Hashtbl.create 8 in
          let queues = ref 0 in
          let handle_id = Ogpu_core.Handle.create_device () in
          let max_buffer, max_texture, max_samples, max_invocations, _local, _api =
            V.device_facts device
          in
          let capabilities : Ogpu_core.Caps.t =
            {
              limits =
                {
                  max_buffer_size = Int64.of_int max_buffer;
                  max_texture_dimension_2d = max_texture;
                  max_bind_groups = 4;
                  max_sample_count = max_samples;
                };
              compute_pipeline = true;
              render_pipeline = true;
              ray_tracing = true;
              ray_tracing_curves = false;
              ray_tracing_motion = true; (* only after device_create enables rayQuery and accelerationStructure *)
              ray_tracing_refit = true;
              memoryless_attachments = false;
              unified_memory = false;
            }
          in
          let register_buffer op created size =
            match handle op created with
            | Error _ as e -> e
            | Ok native ->
                incr live;
                let id = token () in
                Hashtbl.add buffers id { buffer = native; size };
                let in_range offset length =
                  offset >= 0L && length >= 0 && Int64.add offset (Int64.of_int length) <= size
                in
                let read_into offset destination at length =
                  if at < 0 || length > Bytes.length destination - at || not (in_range offset length)
                  then error op E.Invalid_argument "read range is invalid"
                  else
                    checked op (V.buffer_read native (Int64.to_int offset) destination at length)
                in
                Ok
                  ( native,
                    {
                      B.token = id;
                      write =
                        (fun offset bytes ->
                          if not (in_range offset (Bytes.length bytes)) then
                            error op E.Invalid_argument "write range is invalid"
                          else checked op (V.buffer_write native (Int64.to_int offset) bytes));
                      read =
                        (fun offset length ->
                          let bytes = Bytes.create length in
                          Result.map (fun () -> bytes) (read_into offset bytes 0 length));
                      read_into;
                      destroy =
                        (fun () ->
                          Hashtbl.remove buffers id;
                          decr live;
                          V.buffer_destroy native;
                          Ok ());
                    } )
          in
          let create_buffer memory (descriptor : Ogpu_core.Types.buffer_descriptor) =
            let op = "Ogpu_vulkan.create_buffer" in
            let size = Int64.to_int descriptor.size in
            (* Both kinds are device-local: a Shared buffer in host memory is read by kernels over PCIe, 100x
               slower than VRAM (isogrid_city's triangle kernel: 160 ms to 4 ms). Host writes stage into the next
               submit and reads go through the transfer queue; Shared reads as zeros, so it is filled like a mapped
               buffer is. ponytail: a host-visible tier for a buffer the host rewrites every frame and the GPU reads
               once, if a measurement ever shows the staging copy. *)
            match register_buffer op (V.buffer_create device size 1) descriptor.size with
            | Error _ as e -> e
            | Ok (native, resource) when memory = Ogpu_core.Types.Shared -> (
                match checked op (V.buffer_zero native) with
                | Ok () -> Ok resource
                | Error _ as e ->
                    ignore (resource.B.destroy ());
                    e)
            | Ok (_, resource) -> Ok resource
          in
          let create_mapped_buffer (descriptor : Ogpu_core.Types.buffer_descriptor) =
            let op = "Ogpu_vulkan.create_mapped_buffer" in
            let size = Int64.to_int descriptor.size in
            match register_buffer op (V.buffer_create device size 1) descriptor.size with
            | Error _ as e -> e
            | Ok (native, resource) -> (
                (* The host copy exists only while the host uses it: made on its first ask (any
                   domain; a loser's copy is collected), dropped on release and destroy, and then
                   the GC frees it as any Bigarray's data. *)
                let cell = Atomic.make None in
                let rec view () =
                  match Atomic.get cell with
                  | Some _ as v -> v
                  | None ->
                      let v = Some (V.zeroed_view (size / 4)) in
                      if Atomic.compare_and_set cell None v then v else view ()
                in
                match checked op (V.buffer_zero native) with
                | Error _ as e -> e
                | Ok () ->
                    Ok
                      {
                        B.mapped_resource =
                          { resource with destroy = (fun () -> Atomic.set cell None; resource.destroy ()) };
                        view;
                        release_view = (fun () -> Atomic.set cell None);
                        flush =
                          (fun length ->
                            match Atomic.get cell with
                            | Some v -> checked op (V.buffer_flush native v length)
                            | None -> error op E.Invalid_argument "no host view was written");
                        invalidate =
                          (fun length ->
                            checked op (V.buffer_invalidate native (Option.get (view ())) length));
                      })
          in
          let register_texture ~format (descriptor : Ogpu_core.Types.texture_descriptor) =
            let op = "Ogpu_vulkan.create_texture" in
            let flag x = if x then 1 else 0 in
            let created =
              (* OCaml bools and ints share a representation, so an int array crosses as is. *)
              V.texture_create device
                [|
                  descriptor.width;
                  descriptor.height;
                  max 1 descriptor.depth;
                  max 1 descriptor.mip_levels;
                  max 1 descriptor.sample_count;
                  format_index format;
                  flag (List.mem Ogpu_core.Types.Storage_binding descriptor.usage);
                  flag (List.mem Ogpu_core.Types.Render_attachment descriptor.usage);
                |]
            in
            match handle op created with
            | Error _ as e -> e
            | Ok native ->
                incr live;
                let id = token () in
                let value = { texture = native; descriptor = { descriptor with format } } in
                Hashtbl.add textures id value;
                let read_into offset destination at length =
                  if
                    offset <> 0L || at <> 0
                    || length <> Bytes.length destination
                    || descriptor.height <= 0
                    || length mod descriptor.height <> 0
                  then error op E.Invalid_argument "destination must match the texture read range"
                  else
                    let row = length / descriptor.height in
                    let texel = Ogpu_core.Types.texel_bytes format in
                    if row < descriptor.width * texel || row mod texel <> 0 then
                      error op E.Invalid_argument "row pitch is invalid"
                    else checked op (V.texture_read native destination row texel)
                in
                Ok
                  {
                    B.token = id;
                    write =
                      (fun _ _ -> error op E.Unsupported "use a transfer pass for textures");
                    read =
                      (fun offset length ->
                        let bytes = Bytes.create length in
                        Result.map (fun () -> bytes) (read_into offset bytes 0 length));
                    read_into;
                    destroy =
                      (fun () ->
                        Hashtbl.remove textures id;
                        decr live;
                        V.texture_destroy native;
                        Ok ());
                  }
          in
          let own_pipeline ?(local = (1, 1, 1)) native ~max_threads =
            incr live;
            let id = token () in
            Hashtbl.add pipelines id (native, local);
            {
              B.pipeline_token = id;
              max_threads;
              destroy_pipeline =
                (fun () ->
                  Hashtbl.remove pipelines id;
                  decr live;
                  V.pipeline_destroy native;
                  Ok ());
            }
          in
          let create_library shader =
            let op = "Ogpu_vulkan.create_library" in
            let bytes = Ogpu_core.Shader.bytes shader in
            match handle op (V.shader_create device bytes) with
            | Error _ as e -> e
            | Ok modul ->
                incr live;
                
                Ok
                  {
                    B.create_compute_pipeline_in =
                      (fun ~entry ~constants ~interface:declared ->
                        let space : Ogpu_core.Shader.binding_kind -> int = function
                          | Uniform_buffer | Storage_buffer -> 0
                          | Sampled_texture | Storage_texture -> 1
                          | Sampler -> 2
                          | Acceleration_structure -> 3
                        and reflected_space = function 0 | 1 -> 0 | 2 | 3 -> 1 | 4 -> 2 | _ -> 3 in
                        if constants <> [] then
                          error op E.Unsupported "SPIR-V kernels take no named function constants"
                        else
                          match V.reflect_entry bytes entry with
                          | None -> error op E.Invalid_argument "the module has no such entry point"
                          | Some (reflected, local) ->
                              let classes f l = List.sort compare (List.map f l) in
                              if
                                classes (fun (b : Ogpu_core.Shader.binding) -> (b.binding, space b.kind)) declared
                                <> classes (fun (b, k) -> (b, reflected_space k)) reflected
                              then error op E.Invalid_argument "SPIR-V reflection does not match the interface"
                              else
                                Result.map
                                  (fun native -> own_pipeline ~local native ~max_threads:max_invocations)
                                  (handle op
                                     (V.compute_pipeline_create device modul entry
                                        (Array.of_list reflected))));
                    destroy_library =
                      (fun () ->
                        decr live;
                        V.shader_destroy device modul;
                        Ok ());
                  }
          in
          let create_render_pipeline (options : B.render_pipeline_options)
              (descriptor : Ogpu_core.Pipeline.render_descriptor) =
            let op = "Ogpu_vulkan.create_render_pipeline" in
            let modules_of shader = handle op (V.shader_create device (Ogpu_core.Shader.bytes shader)) in
            let ( let* ) = Result.bind in
            let* vertex = modules_of descriptor.vertex in
            let fragment =
              match descriptor.fragment with
              | None -> Ok 0
              | Some shader -> modules_of shader
            in
            match fragment with
            | Error _ as e ->
                V.shader_destroy device vertex;
                e
            | Ok fragment ->
                let interface =
                  List.sort_uniq compare
                    (V.reflect (Ogpu_core.Shader.bytes descriptor.vertex)
                    @
                    match descriptor.fragment with
                    | None -> []
                    | Some shader -> V.reflect (Ogpu_core.Shader.bytes shader))
                in
                let topology =
                  match options.topology with
                  | Point_list -> 0
                  | Line_list -> 1
                  | Triangle_list | Triangle_strip -> 2
                in
                let color =
                  match descriptor.color_format with Rgba8_unorm -> 1 | Bgra8_unorm -> 2
                in
                let has_depth =
                  match descriptor.depth_format with No_depth -> 0 | Depth32_float -> 1
                in
                let created =
                  V.render_pipeline_create device vertex fragment (Array.of_list interface)
                    [|
                      blend_index options.blend;
                      topology;
                      color;
                      has_depth;
                      max 1 descriptor.sample_count;
                    |]
                in
                V.shader_destroy device vertex;
                if fragment <> 0 then V.shader_destroy device fragment;
                Result.map (fun native -> own_pipeline native ~max_threads:0) (handle op created)
          in
          let create_sampler (descriptor : Ogpu_core.Types.sampler_descriptor) =
            let op = "Ogpu_vulkan.create_sampler" in
            let filter : Ogpu_core.Types.sampler_filter -> int = function
              | Nearest -> 0
              | Linear -> 1
            in
            let address : Ogpu_core.Types.address_mode -> int = function
              | Clamp_to_edge -> 0
              | Repeat -> 1
              | Mirror_repeat -> 2
            in
            let mip =
              match descriptor.mip_filter with No_mip -> 0 | Nearest_mip -> 1 | Linear_mip -> 2
            in
            match
              handle op
                (V.sampler_create device
                   [|
                     filter descriptor.min_filter;
                     filter descriptor.mag_filter;
                     mip;
                     address descriptor.address_u;
                     address descriptor.address_v;
                     descriptor.max_anisotropy;
                   |]
                   descriptor.lod_min descriptor.lod_max)
            with
            | Error _ as e -> e
            | Ok native ->
                incr live;
                let id = token () in
                Hashtbl.add samplers id native;
                Ok
                  {
                    B.sampler_token = id;
                    destroy_sampler =
                      (fun () ->
                        Hashtbl.remove samplers id;
                        decr live;
                        V.sampler_destroy device native;
                        Ok ());
                  }
          in
          let find_buffer op id =
            match Hashtbl.find_opt buffers id with
            | Some buffer -> Ok buffer
            | None -> error op E.Invalid_argument "buffer graph is incomplete"
          in
          let find_texture op id =
            match Hashtbl.find_opt textures id with
            | Some texture -> Ok texture
            | None -> error op E.Invalid_argument "texture graph is incomplete"
          in
          let find_pipeline op id =
            match Hashtbl.find_opt pipelines id with
            | Some pipeline -> Ok pipeline
            | None -> error op E.Invalid_argument "pipeline graph is incomplete"
          in
          let find_handle op id = Result.map fst (find_pipeline op id) in
          let create_icb ~max_commands =
            let id = token () in
            incr live;
            let commands =
              Array.init max_commands (fun _ ->
                  { command_pipeline = None; command_buffers = []; command_draw = None })
            in
            Hashtbl.add icbs id commands;
            let op = "Ogpu_vulkan.icb" in
            let at index k =
              if index < 0 || index >= max_commands then
                error op E.Invalid_argument "indirect command index is out of range"
              else k commands.(index)
            in
            Ok
              {
                B.icb_token = id;
                icb_set_pipeline =
                  (fun ~index pipeline ->
                    at index (fun c ->
                        c.command_pipeline <- Some pipeline;
                        Ok ()));
                icb_set_buffer =
                  (fun ~index _stage ~slot ~offset buffer ->
                    at index (fun c ->
                        c.command_buffers <- (slot, offset, buffer) :: c.command_buffers;
                        Ok ()));
                icb_draw =
                  (fun ~index ~primitive ~first ~count ~instances ->
                    at index (fun c ->
                        c.command_draw <- Some (fun e -> e.draw ~primitive ~first ~count ~instances);
                        Ok ()));
                icb_draw_indexed =
                  (fun ~index ~primitive ~index_type buffer ~offset ~count ~instances ->
                    at index (fun c ->
                        c.command_draw <-
                          Some (fun e -> e.draw_indexed ~primitive ~index_type buffer ~offset ~count ~instances);
                        Ok ()));
                destroy_icb =
                  (fun () ->
                    Hashtbl.remove icbs id;
                    decr live;
                    Ok ());
              }
          in
          let create_argument ~pipeline:_ _stage ~index:_ =
            let op = "Ogpu_vulkan.argument" in
            let slice buffer offset =
              match Hashtbl.find_opt arguments (buffer, offset) with
              | Some cell -> cell
              | None ->
                  let cell = ref (0, 0) in
                  Hashtbl.replace arguments (buffer, offset) cell;
                  cell
            in
            Ok
              {
                B.argument_length = 16;
                argument_alignment = 16;
                argument_texture =
                  (fun ~buffer ~offset ~slot:_ texture ->
                    Result.map
                      (fun (t : texture) ->
                        let cell = slice buffer offset in
                        cell := (t.texture, snd !cell))
                      (find_texture op texture));
                argument_sampler =
                  (fun ~buffer ~offset ~slot:_ sampler ->
                    match Hashtbl.find_opt samplers sampler with
                    | None -> error op E.Invalid_argument "sampler graph is incomplete"
                    | Some native ->
                        let cell = slice buffer offset in
                        cell := (fst !cell, native);
                        Ok ());
                destroy_argument = (fun () -> Ok ());
              }
          in
          let read_bytes op token offset length =
            match find_buffer op token with
            | Error _ as e -> e
            | Ok buffer ->
                if
                  offset < 0L || length < 0 || offset > buffer.size
                  || Int64.of_int length > Int64.sub buffer.size offset
                then error op E.Invalid_argument "read range is invalid"
                else
                  let bytes = Bytes.create length in
                  Result.map (fun () -> bytes)
                    (checked op
                       (V.buffer_read buffer.buffer (Int64.to_int offset) bytes 0 length))
          in
          let generated_buffer op generated bytes =
            match
              create_buffer Ogpu_core.Types.Device_local
                {
                  label = Some "Vulkan ray bounds";
                  size = Int64.of_int (Bytes.length bytes);
                  usage = [ Ogpu_core.Types.Storage; Copy_dst ];
                }
            with
            | Error _ as e -> e
            | Ok resource -> (
                match resource.B.write 0L bytes with
                | Ok () ->
                    generated := resource :: !generated;
                    Ok resource.B.token
                | Error _ as e ->
                    ignore (resource.B.destroy ());
                    e)
          in
          let put_float bytes at value =
            Bytes.set_int32_le bytes at (Int32.bits_of_float value)
          in
          let padded_bounds minimum maximum =
            Array.init 3 (fun axis ->
                let epsilon =
                  1e-5 *. max 1. (max (Float.abs minimum.(axis)) (Float.abs maximum.(axis)))
                in
                (minimum.(axis) -. epsilon, maximum.(axis) +. epsilon))
          in
          let motion_triangle_boxes op generated keyframes stride vertices =
            let length = ((vertices - 1) * stride) + 12 in
            let ( let* ) = Result.bind in
            let* frames =
              Array.fold_right
                (fun (token, offset) rest ->
                  let* rest = rest in
                  Result.map (fun bytes -> bytes :: rest) (read_bytes op token offset length))
                keyframes (Ok [])
            in
            let boxes = Bytes.create ((vertices / 3) * 24) in
            for triangle = 0 to (vertices / 3) - 1 do
              let minimum = [| infinity; infinity; infinity |]
              and maximum = [| neg_infinity; neg_infinity; neg_infinity |] in
              List.iter
                (fun frame ->
                  for vertex = 0 to 2 do
                    for axis = 0 to 2 do
                      let at = ((triangle * 3 + vertex) * stride) + axis * 4 in
                      let value = Int32.float_of_bits (Bytes.get_int32_le frame at) in
                      minimum.(axis) <- min minimum.(axis) value;
                      maximum.(axis) <- max maximum.(axis) value
                    done
                  done)
                frames;
              let padded = padded_bounds minimum maximum in
              for axis = 0 to 2 do
                let low, high = padded.(axis) in
                put_float boxes ((triangle * 24) + (axis * 4)) low;
                put_float boxes ((triangle * 24) + 12 + (axis * 4)) high
              done
            done;
            let* token = generated_buffer op generated boxes in
            let* (b : buffer) = find_buffer op token in
            Ok [| 1; b.buffer; 0; 24; vertices / 3; 0; 0 |]
          in
          let geometry_records generated geometries =
            let op = "Ogpu_vulkan.accel" in
            let flag x = if x then 1 else 0 in
            let record (geometry : B.driver_geometry) =
              match geometry with
              | Driver_triangles { vertices; offset; vertex_stride; vertex_count; length = _ } ->
                  Result.map
                    (fun (b : buffer) ->
                      [| 0; b.buffer; Int64.to_int offset; vertex_stride; vertex_count; 1; 0 |])
                    (find_buffer op vertices)
              | Driver_motion_triangles { keyframes; vertex_stride; vertex_count } ->
                  motion_triangle_boxes op generated keyframes vertex_stride vertex_count
              | Driver_boxes { boxes = [| (id, offset) |]; stride; count; opaque; duplicate } ->
                  Result.map
                    (fun (b : buffer) ->
                      [| 1; b.buffer; Int64.to_int offset; stride; count; flag opaque; flag duplicate |])
                    (find_buffer op id)
              | Driver_boxes _ | Driver_curves _ ->
                  error op E.Unsupported "Vulkan ray queries do not expose native curves or multi-keyframe boxes"
            in
            let rec all index records =
              if index = Array.length geometries then Ok (Array.of_list (List.rev records))
              else
                Result.bind (record geometries.(index)) (fun geometry ->
                    all (index + 1) (geometry :: records))
            in
            all 0 []
          in
          let motion_instance_geometries op generated instances offset count structures transforms =
            let ( let* ) = Result.bind in
            let* records = read_bytes op instances offset (count * 44) in
            let transform_token, transform_offset, transform_count = transforms in
            let* transform_data = read_bytes op transform_token transform_offset (transform_count * 48) in
            let read_float bytes at = Int32.float_of_bits (Bytes.get_int32_le bytes at) in
            let read_u32 bytes at = Int32.to_int (Bytes.get_int32_le bytes at) in
            let local_bounds (geometry : int array) =
              if geometry.(0) <> 0 then error op E.Unsupported "motion instances require triangle prototypes"
              else
                let buffer = geometry.(1)
                and byte_offset = geometry.(2)
                and stride = geometry.(3)
                and vertices = geometry.(4) in
                let bytes = Bytes.create (((vertices - 1) * stride) + 12) in
                match checked op (V.buffer_read buffer byte_offset bytes 0 (Bytes.length bytes)) with
                | Error _ as e -> e
                | Ok () ->
                    let minimum = [| infinity; infinity; infinity |]
                    and maximum = [| neg_infinity; neg_infinity; neg_infinity |] in
                    for vertex = 0 to vertices - 1 do
                      for axis = 0 to 2 do
                        let value = read_float bytes ((vertex * stride) + (axis * 4)) in
                        minimum.(axis) <- min minimum.(axis) value;
                        maximum.(axis) <- max maximum.(axis) value
                      done
                    done;
                    Ok (minimum, maximum)
            in
            let transform_point data key point =
              let at = key * 48 in
              Array.init 3 (fun row ->
                  read_float data (at + (row * 4))
                  *. point.(0)
                  +. (read_float data (at + ((3 + row) * 4)) *. point.(1))
                  +. (read_float data (at + ((6 + row) * 4)) *. point.(2))
                  +. read_float data (at + ((9 + row) * 4)))
            in
            let build_instance index =
              let base = index * 44 in
              let mask = read_u32 records base
              and structure = read_u32 records (base + 4)
              and user_id = read_u32 records (base + 8)
              and start = read_u32 records (base + 12)
              and keys = read_u32 records (base + 16) in
              if structure >= Array.length structures then
                error op E.Invalid_argument "motion instance structure index is out of range"
              else if keys < 2 || start < 0 || keys > transform_count || start > transform_count - keys then
                error op E.Invalid_argument "motion instance transform range is invalid"
              else
                match Hashtbl.find_opt accel_geometries structures.(structure) with
                | None -> error op E.Invalid_argument "motion instance prototype is not a triangle BLAS"
                | Some [| triangle |] ->
                    (match local_bounds triangle with
                    | Error _ as e -> e
                    | Ok (minimum, maximum) ->
                        let bytes = Bytes.create ((keys - 1) * 24) in
                        for interval = 0 to keys - 2 do
                          let world_min = [| infinity; infinity; infinity |]
                          and world_max = [| neg_infinity; neg_infinity; neg_infinity |] in
                          for key = 0 to 1 do
                            for corner = 0 to 7 do
                              let point =
                                Array.init 3 (fun axis ->
                                    if corner land (1 lsl axis) = 0 then minimum.(axis) else maximum.(axis))
                              in
                              let world = transform_point transform_data (start + interval + key) point in
                              for axis = 0 to 2 do
                                world_min.(axis) <- min world_min.(axis) world.(axis);
                                world_max.(axis) <- max world_max.(axis) world.(axis)
                              done
                            done
                          done;
                          let padded = padded_bounds world_min world_max
                          and box_at = interval * 24 in
                          for axis = 0 to 2 do
                            let low, high = padded.(axis) in
                            put_float bytes (box_at + axis * 4) low;
                            put_float bytes (box_at + 12 + axis * 4) high
                          done
                        done;
                        let* token = generated_buffer op generated bytes in
                        let* (box : buffer) = find_buffer op token in
                        Ok ([| 1; box.buffer; 0; 24; keys - 1; 0; 0 |], structure, mask, user_id))
                | Some _ ->
                    error op E.Unsupported "motion instances require one triangle geometry per prototype"
            in
            let rec all index geometries =
              if index = count then Ok (List.rev geometries)
              else
                Result.bind (build_instance index) (fun geometry ->
                    all (index + 1) (geometry :: geometries))
            in
            let* records = all 0 [] in
            let instances = Bytes.make (count * 68) '\000' in
            List.iteri
              (fun index (_, structure, mask, user_id) ->
                let base = index * 68 in
                List.iter
                  (fun at -> Bytes.set_int32_le instances (base + at) (Int32.bits_of_float 1.))
                  [ 0; 16; 32 ];
                Bytes.set_int32_le instances (base + 48) (Int32.of_int user_id);
                Bytes.set_int32_le instances (base + 52) (Int32.of_int mask);
                Bytes.set_int32_le instances (base + 56) (Int32.of_int index);
                Bytes.set_int32_le instances (base + 60) (Int32.of_int structure))
              records;
            Ok (Array.of_list (List.map (fun (geometry, _, _, _) -> geometry) records), instances)
          in
          let create_accel (descriptor : B.driver_accel_descriptor) =
            let op = "Ogpu_vulkan.create_accel" in
            let generated = ref [] and stored_geometries = ref None and children = ref [] in
            let cleanup () =
              List.iter (fun resource -> ignore (resource.B.destroy ())) !generated;
              List.iter V.accel_destroy !children
            in
            let ( let* ) = Result.bind in
            let created =
              match descriptor with
              | Driver_blas { geometries; refit; _ } ->
                  let* records = geometry_records generated geometries in
                  stored_geometries := Some records;
                  Ok (V.accel_create_blas device records refit)
              | Driver_tlas
                  {
                    instances;
                    offset;
                    instance_count;
                    kind = Motion_instances;
                    structures;
                    motion_transforms = Some transforms;
                  } ->
                  let* (proxies, instance_records) =
                    motion_instance_geometries op generated instances offset instance_count structures
                      transforms
                  in
                  let* instance_token = generated_buffer op generated instance_records in
                  let* (instance_buffer : buffer) = find_buffer op instance_token in
                  let rec build_proxies index handles =
                    if index = Array.length proxies then Ok (Array.of_list (List.rev handles))
                    else
                      let* proxy =
                        handle op (V.accel_create_blas device [| proxies.(index) |] false)
                      in
                      children := proxy :: !children;
                      build_proxies (index + 1) (proxy :: handles)
                  in
                  let* proxy_handles = build_proxies 0 [] in
                  Ok
                    (V.accel_create_tlas device instance_buffer.buffer 0 instance_count (proxy_handles, true))
              | Driver_tlas { kind = Motion_instances; _ } ->
                  error op E.Invalid_argument "motion instances require transform keyframes"
              | Driver_tlas { instances; offset; instance_count; structures; _ } ->
                  let* b = find_buffer op instances in
                  let* handles =
                    Array.fold_right
                      (fun id rest ->
                        let* rest = rest in
                        match Hashtbl.find_opt accels id with
                        | Some native -> Ok (native :: rest)
                        | None -> error op E.Invalid_argument "acceleration structure graph is incomplete")
                      structures (Ok [])
                  in
                  Ok
                    (V.accel_create_tlas device b.buffer (Int64.to_int offset) instance_count
                       (Array.of_list handles, false))
              | Driver_sized { size; template } -> (
                  match Hashtbl.find_opt accels template with
                  | Some native -> Ok (V.accel_create_sized device (Int64.to_int size) native)
                  | None -> error op E.Invalid_argument "acceleration structure graph is incomplete")
            in
            match created with
            | Error _ as e ->
                cleanup ();
                e
            | Ok created -> (
                match handle op created with
                | Error _ as e ->
                    cleanup ();
                    e
                | Ok native ->
                    incr live;
                    let id = token () in
                    Hashtbl.add accels id native;
                    Hashtbl.add accel_resources id !generated;
                    Option.iter (Hashtbl.add accel_geometries id) !stored_geometries;
                    let structure, build, update = V.accel_sizes native in
                    Ok
                      {
                        B.accel_token = id;
                        accel_sizes =
                          {
                            structure_size = Int64.of_int structure;
                            build_scratch_size = Int64.of_int build;
                            refit_scratch_size = Int64.of_int update;
                          };
                        destroy_accel =
                          (fun () ->
                            Hashtbl.remove accels id;
                            Hashtbl.remove accel_geometries id;
                            List.iter
                              (fun resource -> ignore (resource.B.destroy ()))
                              (Hashtbl.find_opt accel_resources id |> Option.value ~default:[]);
                            Hashtbl.remove accel_resources id;
                            decr live;
                            V.accel_destroy native;
                            Ok ());
                      })
          in
          let create_queue () =
            let op = "Ogpu_vulkan.queue" in
            match handle op (V.queue_create device) with
            | Error _ as e -> e
            | Ok queue ->
                incr live;
                incr queues;
                let next_epoch = ref 1 and completed = ref 0 in
                let active () = !next_epoch - 1 - !completed in
                let bound_epoch epoch =
                  let epoch = Int64.to_int epoch in
                  if epoch <= 0 || epoch >= !next_epoch then
                    error op E.Invalid_argument "epoch was not submitted"
                  else Ok epoch
                in
                let complete_through epoch =
                  match bound_epoch epoch with
                  | Error _ as e -> e
                  | Ok epoch when epoch <= !completed -> Ok ()
                  | Ok epoch -> (
                      match checked op (V.queue_wait queue epoch) with
                      | Error _ as e -> e
                      | Ok () ->
                          completed := epoch;
                          Ok ())
                in
                let poll_through epoch =
                  match bound_epoch epoch with
                  | Error _ as e -> e
                  | Ok epoch when epoch <= !completed -> Ok true
                  | Ok epoch ->
                      let done_ = V.queue_completed queue in
                      if done_ < 0 then of_result op done_
                      else if done_ >= epoch then (
                        V.queue_poll queue;
                        completed := epoch;
                        Ok true)
                      else Ok false
                in
                let begin_commands () =
                  match handle op (V.commands_begin queue) with
                  | Error _ as e -> e
                  | Ok commands ->
                      let finished = ref false in
                      let check_slot code = checked op code in
                      let commit_with submit =
                        if !finished then error op E.Invalid_state "commands are finished"
                        else if active () >= 3 then
                          error op E.Capacity "frames-in-flight capacity reached"
                        else
                          let epoch = submit () in
                          if epoch < 0 then of_result op epoch
                          else (
                            finished := true;
                            next_epoch := epoch + 1;
                            Ok { B.epoch = Int64.of_int epoch })
                      in
                      let compute_encoder () =
                        V.compute_begin commands;
                        (* A kernel runs in groups of its own local size: the threadgroup a caller names is Metal's, and
                           SPIR-V cannot change it. *)
                        let local = ref (1, 1, 1) in
                        Ok
                          {
                            B.set_pipeline =
                              (fun id ->
                                Result.map
                                  (fun (pipeline, size) ->
                                    local := size;
                                    V.set_pipeline commands pipeline)
                                  (find_pipeline op id));
                            set_buffer =
                              (fun ~index ~offset id ->
                                Result.bind (find_buffer op id) (fun b ->
                                    check_slot
                                      (V.set_buffer commands index (Int64.to_int offset) b.buffer)));
                            set_bytes =
                              (fun ~index bytes -> check_slot (V.set_bytes commands index bytes));
                            set_texture =
                              (fun ~index id ->
                                Result.bind (find_texture op id) (fun t ->
                                    check_slot (V.set_texture commands index t.texture)));
                            set_accel =
                              (fun ~index id ->
                                match Hashtbl.find_opt accels id with
                                | None -> error op E.Invalid_argument "acceleration structure graph is incomplete"
                                | Some native -> check_slot (V.set_accel commands index native));
                            dispatch_threads =
                              (fun ~threads:(x, y, z) ~threadgroup:_ ->
                                let gx, gy, gz = !local in
                                let groups t g = (t + g - 1) / g in
                                V.dispatch commands (groups x gx) (groups y gy) (groups z gz);
                                Ok ());
                            dispatch_indirect =
                              (fun id ~offset ~threadgroup:_ ->
                                Result.map
                                  (fun (b : buffer) ->
                                    V.dispatch_indirect commands b.buffer (Int64.to_int offset))
                                  (find_buffer op id));
                            compute_use_accels = (fun _ -> Ok ());
                            end_compute =
                              (fun () ->
                                V.encoder_end commands;
                                Ok ());
                          }
                      in
                      let accel_encoder () =
                        let ( let* ) = Result.bind in
                        let find id =
                          match Hashtbl.find_opt accels id with
                          | Some native -> Ok native
                          | None -> error op E.Invalid_argument "acceleration structure graph is incomplete"
                        in
                        Ok
                          {
                            B.build =
                              (fun id ~scratch ~scratch_offset ->
                                let* a = find id in
                                let* s = find_buffer op scratch in
                                checked op
                                  (V.accel_build commands a s.buffer (Int64.to_int scratch_offset)));
                            compact =
                              (fun ~src ~dst ->
                                let* s = find src in
                                let* d = find dst in
                                V.accel_compact commands s d;
                                Ok ());
                            write_compacted_size =
                              (fun id ~dst ~offset ->
                                let* a = find id in
                                let* b = find_buffer op dst in
                                checked op
                                  (V.accel_write_compacted commands a b.buffer (Int64.to_int offset)));
                            refit =
                              (fun id descriptor ~scratch ~scratch_offset ->
                                let* a = find id in
                                let* s = find_buffer op scratch in
                                match descriptor with
                                | B.Driver_blas { geometries; _ } ->
                                    let generated = ref [] in
                                    let cleanup () =
                                      List.iter (fun resource -> ignore (resource.B.destroy ())) !generated
                                    in
                                    (match geometry_records generated geometries with
                                    | Error _ as e ->
                                        cleanup ();
                                        e
                                    | Ok geometries -> (
                                        match
                                          checked op
                                            (V.accel_refit commands a geometries s.buffer
                                               (Int64.to_int scratch_offset))
                                        with
                                        | Error _ as e ->
                                            cleanup ();
                                            e
                                        | Ok () ->
                                            List.iter
                                              (fun resource -> ignore (resource.B.destroy ()))
                                              (Hashtbl.find_opt accel_resources id |> Option.value ~default:[]);
                                            Hashtbl.replace accel_resources id !generated;
                                            Hashtbl.replace accel_geometries id geometries;
                                            Ok ()))
                                | B.Driver_tlas _ | B.Driver_sized _ ->
                                    error op E.Invalid_argument "Vulkan refit requires a BLAS descriptor"
                              );
                            end_accel =
                              (fun () ->
                                V.encoder_end commands;
                                Ok ());
                          }
                      in
                      let blit_encoder () =
                        Ok
                          {
                            B.buffer_to_texture =
                              (fun ~src ~offset ~bytes_per_row ~bytes_per_image:_ ~dst ~mip
                                   ~origin ~extent ->
                                Result.bind (find_buffer op src) (fun b ->
                                    Result.map
                                      (fun (t : texture) ->
                                        let texel =
                                          Ogpu_core.Types.texel_bytes t.descriptor.format
                                        in
                                        V.blit_buffer_to_texture commands b.buffer t.texture
                                          [|
                                            Int64.to_int offset;
                                            Int64.to_int bytes_per_row;
                                            texel;
                                            mip;
                                            origin.x;
                                            origin.y;
                                            origin.z;
                                            extent.width;
                                            extent.height;
                                            extent.depth;
                                          |])
                                      (find_texture op dst)));
                            end_blit =
                              (fun () ->
                                V.encoder_end commands;
                                Ok ());
                          }
                      in
                      let render_encoder (target : B.driver_render_target) =
                        let ( let* ) = Result.bind in
                        let handle_of id = Result.map (fun (t : texture) -> t.texture) (find_texture op id) in
                        let colors =
                          Array.fold_right
                            (fun (color : B.driver_color_attachment) rest ->
                              let* rest = rest in
                              let* texture = handle_of color.color in
                              let* resolve =
                                match color.color_resolve with
                                | None -> Ok 0
                                | Some id -> handle_of id
                              in
                              let r, g, b, a = color.color_clear in
                              Ok
                                (( [|
                                     texture;
                                     resolve;
                                     load_index color.color_load;
                                     store_index color.color_store;
                                   |],
                                   [| r; g; b; a |] )
                                :: rest))
                            target.target_colors (Ok [])
                        in
                        let depth =
                          match target.target_depth with
                          | None -> Ok None
                          | Some d ->
                              let* texture = handle_of d.depth in
                              Ok
                                (Some
                                   ( [| texture; load_index d.depth_load; store_index d.depth_store |],
                                     d.depth_clear ))
                        in
                        let* colors = colors in
                        let* depth = depth in
                        let* () =
                          checked op
                            (V.render_begin commands (Array.of_list colors) depth
                               (target.target_width, target.target_height))
                        in
                        let set_stage_buffer _stage ~index ~offset id =
                          match Hashtbl.find_opt arguments (id, offset) with
                          | Some { contents = texture, sampler } ->
                              let* () = check_slot (V.set_texture commands index texture) in
                              check_slot (V.set_sampler commands (index + 1) sampler)
                          | None ->
                              let* b = find_buffer op id in
                              check_slot
                                (V.set_buffer commands index (Int64.to_int offset) b.buffer)
                        in
                        let render_set_pipeline id =
                          Result.map (V.set_pipeline commands) (find_handle op id)
                        in
                        let rec encoder : B.driver_render_encoder =
                          {
                            B.render_set_pipeline;
                            set_stage_buffer;
                            set_stage_texture =
                              (fun _stage ~index id ->
                                let* t = find_texture op id in
                                check_slot (V.set_texture commands index t.texture));
                            set_stage_sampler =
                              (fun _stage ~index id ->
                                match Hashtbl.find_opt samplers id with
                                | None -> error op E.Invalid_argument "sampler graph is incomplete"
                                | Some native -> check_slot (V.set_sampler commands index native));
                            set_viewport =
                              (fun rect ->
                                V.render_viewport commands (rect.x, rect.y, rect.width, rect.height);
                                Ok ());
                            set_scissor =
                              (fun rect ->
                                V.render_scissor commands (rect.x, rect.y, rect.width, rect.height);
                                Ok ());
                            set_cull =
                              (fun cull ->
                                V.render_cull commands
                                  (match cull with Cull_none -> 0 | Cull_front -> 1 | Cull_back -> 2);
                                Ok ());
                            set_winding =
                              (fun winding ->
                                V.render_winding commands (winding = B.Clockwise);
                                Ok ());
                            set_depth_state =
                              (fun state ->
                                (match state with
                                | None -> V.render_depth commands (-1) false
                                | Some s ->
                                    V.render_depth commands (compare_index s.depth_compare)
                                      s.depth_write);
                                Ok ());
                            draw =
                              (fun ~primitive ~first ~count ~instances ->
                                V.render_draw commands (primitive_index primitive) first count
                                  instances;
                                Ok ());
                            draw_indexed =
                              (fun ~primitive ~index_type id ~offset ~count ~instances ->
                                let* b = find_buffer op id in
                                V.render_draw_indexed commands b.buffer
                                  [|
                                    primitive_index primitive;
                                    index_kind index_type;
                                    Int64.to_int offset;
                                    Int64.to_int count;
                                    instances;
                                  |];
                                Ok ());
                            draw_indexed_indirect =
                              (fun ~primitive ~index_type id ~offset arguments_buffer ~arguments_offset ->
                                let* b = find_buffer op id in
                                let* a = find_buffer op arguments_buffer in
                                V.render_draw_indexed_indirect commands b.buffer a.buffer
                                  [|
                                    primitive_index primitive;
                                    index_kind index_type;
                                    Int64.to_int offset;
                                    Int64.to_int arguments_offset;
                                  |];
                                Ok ());
                            draw_batch =
                              (fun draws ->
                                Array.fold_left
                                  (fun result (draw : B.driver_batch_draw) ->
                                    let* () = result in
                                    let* () = render_set_pipeline draw.batch_pipeline in
                                    let* () =
                                      Array.fold_left
                                        (fun result (stage, index, buffer, offset) ->
                                          let* () = result in
                                          set_stage_buffer stage ~index ~offset buffer)
                                        (Ok ()) draw.batch_buffers
                                    in
                                    match draw.batch_index with
                                    | Some (index_type, buffer, offset, count) ->
                                        encoder.draw_indexed ~primitive:draw.batch_primitive
                                          ~index_type buffer ~offset ~count
                                          ~instances:draw.batch_instances
                                    | None ->
                                        encoder.draw ~primitive:draw.batch_primitive
                                          ~first:draw.batch_vertex_start
                                          ~count:draw.batch_vertex_count
                                          ~instances:draw.batch_instances)
                                  (Ok ()) draws);
                            use_resources = (fun _ -> Ok ());
                            execute_icb =
                              (fun id ~location ~length ->
                                match Hashtbl.find_opt icbs id with
                                | None -> error op E.Invalid_argument "indirect commands are unknown"
                                | Some recorded ->
                                    let result = ref (Ok ()) in
                                    for i = location to location + length - 1 do
                                      let c = recorded.(i) in
                                      if !result = Ok () then
                                        result :=
                                          let* () =
                                            match c.command_pipeline with
                                            | Some p -> render_set_pipeline p
                                            | None -> Ok ()
                                          in
                                          let* () =
                                            List.fold_left
                                              (fun result (slot, offset, buffer) ->
                                                let* () = result in
                                                set_stage_buffer B.Vertex ~index:slot ~offset buffer)
                                              (Ok ()) c.command_buffers
                                          in
                                          match c.command_draw with
                                          | Some draw -> draw encoder
                                          | None -> Ok ()
                                    done;
                                    !result);
                            end_render =
                              (fun () ->
                                V.render_end commands;
                                Ok ());
                          }
                        in
                        Ok encoder
                      in
                      let commit () = commit_with (fun () -> V.commands_commit commands) in
                      let commit_present ~source (frame : B.driver_frame) =
                        match (find_texture op source, Hashtbl.find_opt frames frame.frame_token) with
                        | Error e, _ -> Error e
                        | _, None -> error op E.Invalid_state "frame token is stale"
                        | Ok source, Some surface ->
                            let result =
                              commit_with (fun () ->
                                  V.commit_present commands surface source.texture)
                            in
                            if Result.is_ok result then Hashtbl.remove frames frame.frame_token;
                            result
                      in
                      let abandon () =
                        if !finished then error op E.Invalid_state "commands are finished"
                        else (
                          finished := true;
                          V.commands_abandon commands;
                          Ok ())
                      in
                      Ok
                        {
                          B.compute_encoder;
                          accel_encoder;
                          blit_encoder;
                          render_encoder;
                          commit;
                          commit_present;
                          abandon;
                        }
                in
                Ok
                  {
                    B.complete_through;
                    poll_through;
                    completed_epoch = (fun () -> Int64.of_int !completed);
                    begin_commands;
                    gpu_duration =
                      (fun epoch ->
                        let seconds = V.queue_duration queue (Int64.to_int epoch) in
                        if seconds < 0. then None else Some seconds);
                    gpu_timing = (fun () -> { B.gpu_seconds = V.queue_total_seconds queue });
                    destroy_queue =
                      (fun () ->
                        if active () > 0 && V.queue_completed queue < !next_epoch - 1 then
                          error op E.Invalid_state "queue has commands in flight"
                        else (
                          decr queues;
                          decr live;
                          V.queue_destroy queue;
                          Ok ()));
                  }
          in
          let create_surface (configuration : Ogpu_core.Surface.configuration) =
            let op = "Ogpu_vulkan.surface" in
            let immediate (c : Ogpu_core.Surface.configuration) = c.present_mode = Immediate in
            match configuration.layer with
            | None -> error op E.Unsupported "surface needs a native layer token"
            | Some layer -> (
                let created =
                  V.surface_create device layer
                    (Native_layer_token.owner_id layer)
                    (Native_layer_token.generation layer)
                    (immediate configuration)
                in
                match handle op created with
                | Error _ as e -> e
                | Ok surface ->
                    incr live;
                    Ok
                      {
                        B.configure =
                          (fun c -> checked op (V.surface_configure surface (immediate c)));
                        acquire_sync =
                          (fun () ->
                            let image = V.surface_acquire surface in
                            if image >= 0 then (
                              let frame_token = token () in
                              Hashtbl.add frames frame_token surface;
                              Ok (`Acquired { B.frame_token }))
                            else if image = -1 then Ok `Timeout
                            else if image = -2 then Ok `Occluded
                            else if image = V.error_device_lost - 10 then Ok `Device_lost
                            else of_result op image);
                        discard =
                          (fun frame ->
                            if Hashtbl.mem frames frame.frame_token then (
                              Hashtbl.remove frames frame.frame_token;
                              V.surface_discard surface;
                              Ok ())
                            else error op E.Invalid_state "frame token is stale");
                        destroy_surface =
                          (fun () ->
                            decr live;
                            V.surface_destroy surface;
                            Ok ());
                      })
          in
          Ok
            {
              B.device_handle = handle_id;
              capabilities;
              allocated_size = (fun () -> Int64.of_int (fst (V.device_memory device)));
              recommended_working_set = (fun () -> Int64.of_int (snd (V.device_memory device)));
              create_buffer;
              create_mapped_buffer;
              create_texture =
                (fun descriptor -> register_texture ~format:descriptor.format descriptor);
              create_depth_texture = (fun descriptor -> register_texture ~format:Depth32_float descriptor);
              create_memoryless_texture =
                (fun ~depth:_ _ ->
                  error "Ogpu_vulkan.create_memoryless_texture" E.Unsupported
                    "this device has no tile memory");
              create_library;
              create_accel;
              (* Vulkan stores a 24-bit user id and 8-bit mask together; motion records are the portable
                 44-byte keyframe layout consumed when swept instance AABBs are built. *)
              instance_layout = (function
                | B.User_id_instances -> [| 68; 0; -1; 52; 56; 48; -1; -1; -1; -1; -1; -1 |]
                | Motion_instances -> [| 44; -1; -1; 0; 4; 8; 12; 16; 20; 24; 28; 32 |]);
              create_sampler;
              create_render_pipeline;
              create_icb;
              create_argument;
              create_queue;
              create_surface;
              destroy_device =
                (fun () ->
                  if !queues <> 0 then
                    error "Ogpu_vulkan.destroy_device" E.Invalid_state "device has active queues"
                  else (
                    device_live := false;
                    V.device_destroy device;
                    Ogpu_core.Handle.destroy_device handle_id;
                    Ok ()));
            }
  in
  { B.create_device }
