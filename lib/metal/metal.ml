module Enum = Metal_gen.Enum
module Data_type = Metal_gen.Enum.Mtl_data_type
module Feature_set = Metal_gen.Enum.Mtl_feature_set

type error_kind =
  | Native_error
  | Wrong_domain
  | Destroyed
  | Parent_has_dependents
  | Invalid_argument
  | Invalid_state
  | Unsupported
  | Device_mismatch

type error = { operation : string; kind : error_kind; message : string }

let pp_error formatter error = Format.fprintf formatter "%s: %s" error.operation error.message
let error operation kind message = Error { operation; kind; message }
let native_error operation message = error operation Native_error message

(* Adapters for generated [Metal_raw.Registry] calls, which return
   [(_, string) result]: [native] types the failure, [probe] reads an
   internal capability bit whose native failure means "unsupported". *)
let native operation = function
  | Ok value -> Ok value
  | Error message -> native_error operation message

let native_result = native

(* The Metal build lowers these binds to matches without continuations. *)
let ( let* ) = Result.bind
let probe = function Ok value -> value | Error _ -> false
let contains_nul value = String.contains value '\000'
let option_exists predicate = function Some value -> predicate value | None -> false

module Vertex_descriptor = struct end

module Thread = struct
  let is_initial_domain = Domain.is_main_domain
  let is_platform_main_thread = Metal_raw.is_main_thread

  let require operation =
    if not (is_initial_domain ()) then
      error operation Wrong_domain "Metal operation must run on the initial OCaml domain"
    else if not (is_platform_main_thread ()) then
      error operation Wrong_domain "Metal operation must run on the platform main thread"
    else Ok ()
end

let before_main operation =
  let* () = Thread.require operation in
  ignore (Metal_raw.drain_releases ());
  Ok ()

let on_main operation callback =
  let* () = before_main operation in
  callback ()

module Release_queue = struct
  type stats = {
    pending : int;
    live_handles : int;
    total_created : int64;
    total_released : int64;
    external_deallocations : int64;
    external_deallocation_mismatches : int64;
    resident_bytes : int64;
  }

  let drain () =
    let* () = Thread.require "Metal.Release_queue.drain" in
    Ok (Metal_raw.drain_releases ())

  let stats () =
    let* () = Thread.require "Metal.Release_queue.stats" in
    let resident_bytes = Metal_raw.resident_bytes () in
    if resident_bytes < 0L then
      native_error "Metal.Release_queue.stats" "mach task_info could not read resident memory"
    else
      Ok
        {
          pending = Metal_raw.pending_releases ();
          live_handles = Metal_raw.live_handles ();
          total_created = Metal_raw.total_created ();
          total_released = Metal_raw.total_released ();
          external_deallocations = Metal_raw.external_deallocations ();
          external_deallocation_mismatches = Metal_raw.external_deallocation_mismatches ();
          resident_bytes;
        }
end

type lifetime = { identity : int; destroyed : bool Atomic.t; dependents : int Atomic.t }

let lifetime_identity_counter = Atomic.make 0

let rec next_lifetime_identity () =
  let identity = Atomic.get lifetime_identity_counter in
  if identity = Stdlib.max_int then failwith "Metal lifetime identity space exhausted"
  else if Atomic.compare_and_set lifetime_identity_counter identity (identity + 1) then identity
  else next_lifetime_identity ()

let lifetime () =
  {
    identity = next_lifetime_identity ();
    destroyed = Atomic.make false;
    dependents = Atomic.make 0;
  }

let is_destroyed lifetime = Atomic.get lifetime.destroyed
let dependent_count lifetime = Atomic.get lifetime.dependents
let attach lifetime = Atomic.incr lifetime.dependents
let detach lifetime = Atomic.decr lifetime.dependents

let ensure_live operation lifetime =
  if is_destroyed lifetime then error operation Destroyed "handle is destroyed" else Ok ()

let destroy_leaf operation lifetime raw detach_parent =
  on_main operation (fun () ->
      if Atomic.compare_and_set lifetime.destroyed false true then begin
        ignore (Metal_raw.destroy raw);
        detach_parent ()
      end;
      Ok ())

let destroy_parent operation lifetime raw detach_parent =
  on_main operation (fun () ->
      if is_destroyed lifetime then Ok ()
      else
        let dependents = dependent_count lifetime in
        if dependents <> 0 then
          error operation Parent_has_dependents
            (Printf.sprintf "handle still owns %d live dependent(s)" dependents)
        else begin
          Atomic.set lifetime.destroyed true;
          ignore (Metal_raw.destroy raw);
          detach_parent ();
          Ok ()
        end)

type device = { raw : Metal_raw.handle; lifetime : lifetime; registry_id : int64 }
type buffer_storage_mode = Shared | Managed | Private

type texture_kind =
  | Texture_1d
  | Texture_1d_array
  | Texture_2d
  | Texture_2d_array
  | Texture_2d_multisample
  | Texture_cube
  | Texture_cube_array
  | Texture_3d
  | Texture_2d_multisample_array
  | Texture_buffer

type shader_scalar_type =
  | Float
  | Half
  | Int
  | Uint
  | Short
  | Ushort
  | Char
  | Uchar
  | Bool
  | Long
  | Ulong
  | Bfloat

type shader_data_type =
  | No_type
  | Struct
  | Array
  | Scalar of shader_scalar_type
  | Vector of shader_scalar_type * int
  | Matrix of shader_scalar_type * int * int
  | Texture_type
  | Sampler_type
  | Pointer
  | Other_data_type of int

type shader_binding_access = Read_only | Read_write | Write_only | Unknown_access of int
type buffer_binding_layout = { alignment : int64; data_size : int64; data_type : shader_data_type }

type texture_binding_layout = {
  texture_kind : texture_kind;
  data_type : shader_data_type;
  depth : bool;
  array_length : int64;
}

type sized_binding_layout = { alignment : int64; data_size : int64 }

type shader_binding_kind =
  | Buffer_binding of buffer_binding_layout
  | Threadgroup_memory_binding of sized_binding_layout
  | Texture_binding of texture_binding_layout
  | Sampler_binding
  | Imageblock_data_binding
  | Imageblock_binding
  | Visible_function_table_binding
  | Primitive_acceleration_structure_binding
  | Instance_acceleration_structure_binding
  | Intersection_function_table_binding
  | Object_payload_binding of sized_binding_layout
  | Tensor_binding
  | Unknown_binding of int

type shader_binding = {
  name : string;
  index : int64;
  access : shader_binding_access;
  used : bool;
  argument : bool;
  kind : shader_binding_kind;
  reflection : Metal_argument_reflection_snapshot.reflected_type option;
}

type function_kind =
  | Vertex
  | Fragment
  | Kernel
  | Visible
  | Intersection
  | Mesh
  | Object
  | Unknown_function_kind of int

type function_constant_value =
  | Bool_constant of bool
  | Int8_constant of int
  | Uint8_constant of int
  | Int16_constant of int
  | Uint16_constant of int
  | Int32_constant of int32
  | Uint32_constant of int64
  | Int64_constant of int64
  | Uint64_bits_constant of int64
  | Float16_constant of float
  | Float32_constant of float

type pixel_format = Metal_format.t
type resource_cpu_cache_mode = Default_cache | Write_combined
type resource_hazard_tracking_mode = Default_hazard_tracking | Untracked | Tracked
type purgeable_state = Nonvolatile | Volatile | Empty
type resource_state = { relinquished : bool Atomic.t; purgeable : purgeable_state Atomic.t }

type texture_usage =
  | Shader_read
  | Shader_write
  | Render_target
  | Pixel_format_view
  | Shader_atomic

type texture_compression_type = Lossless | Lossy
type texture_swizzle_channel = Zero | One | Red | Green | Blue | Alpha

type texture_swizzle = {
  red : texture_swizzle_channel;
  green : texture_swizzle_channel;
  blue : texture_swizzle_channel;
  alpha : texture_swizzle_channel;
}

type texture_descriptor = {
  kind : texture_kind;
  format : pixel_format;
  width : int;
  height : int;
  depth : int;
  mip_levels : int;
  sample_count : int;
  array_length : int;
  storage : buffer_storage_mode;
  cpu_cache : resource_cpu_cache_mode;
  hazard_tracking : resource_hazard_tracking_mode;
  usage : texture_usage list;
  allow_gpu_optimized_contents : bool;
  compression : texture_compression_type;
  swizzle : texture_swizzle;
  label : string option;
}

type heap_allocation = { active : bool Atomic.t }

type heap = {
  lifetime : lifetime;
  purgeable : purgeable_state Atomic.t;
  active_uses : int Atomic.t;
}

and metal_layer = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  device : device;
  mutable drawable_descriptor : texture_descriptor option;
}

and metal_drawable = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  layer : metal_layer;
  mutable drawable_texture : texture option;
  mutable presentation_scheduled : bool;
}

and render_pass_descriptor = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  pass_width : int;
  pass_height : int;
  pass_array_length : int;
  pass_sample_count : int;
  mutable pass_color : texture option;
  mutable pass_depth : texture option;
  mutable pass_stencil : texture option;
  mutable pass_visibility : buffer option;
  mutable pass_resolve : texture option;
  pass_samples : render_pass_sample_state option array;
}

and render_pass_sample_state = { sample_buffer : resource100_sample_buffer }
and resource100_sample_buffer = { lifetime : lifetime; device : device }
and resource_parent = Device_resource of device

and buffer = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  device : device;
  length : int64;
  storage : buffer_storage_mode;
  parent : resource_parent;
  allocation : heap_allocation option;
  state : resource_state;
}

and acceleration_structure = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  device : device;
  size : int64;
  heap : heap option;
  allocation : heap_allocation option;
}

and texture = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  device : device;
  descriptor : texture_descriptor;
  parent : texture_parent;
  allocation : heap_allocation option;
  state : resource_state;
}

and texture_parent =
  | Texture_resource of resource_parent
  | Texture_drawable_resource of metal_drawable

type sampler_filter = Nearest | Linear
type sampler_mip_filter = Not_mipmapped | Mip_nearest | Mip_linear

type sampler_address_mode =
  | Clamp_to_edge
  | Mirror_clamp_to_edge
  | Repeat
  | Mirror_repeat
  | Clamp_to_zero
  | Clamp_to_border_color

type sampler_border_color = Transparent_black | Opaque_black | Opaque_white
type sampler_reduction_mode = Weighted_average | Minimum | Maximum

type sampler_compare_function =
  | Never
  | Less
  | Equal
  | Less_equal
  | Greater
  | Not_equal
  | Greater_equal
  | Always

type stencil_operation =
  | Keep
  | Zero
  | Replace
  | Increment_clamp
  | Decrement_clamp
  | Invert
  | Increment_wrap
  | Decrement_wrap

type stencil_face = {
  compare : sampler_compare_function;
  stencil_fail : stencil_operation;
  depth_fail : stencil_operation;
  pass : stencil_operation;
  read_mask : int32;
  write_mask : int32;
}

type sampler_descriptor = {
  min_filter : sampler_filter;
  mag_filter : sampler_filter;
  mip_filter : sampler_mip_filter;
  max_anisotropy : int;
  s_address : sampler_address_mode;
  t_address : sampler_address_mode;
  r_address : sampler_address_mode;
  border_color : sampler_border_color;
  reduction_mode : sampler_reduction_mode;
  normalized_coordinates : bool;
  lod_min_clamp : float;
  lod_max_clamp : float;
  lod_average : bool;
  lod_bias : float;
  compare_function : sampler_compare_function;
  support_argument_buffers : bool;
  label : string option;
}

type sampler = { raw : Metal_raw.handle; lifetime : lifetime; device : device }
type depth_stencil = { raw : Metal_raw.handle; lifetime : lifetime; device : device }
type library = { raw : Metal_raw.handle; lifetime : lifetime; device : device }
type function_handle = { raw : Metal_raw.handle; lifetime : lifetime; library : library }

type shader_argument_encoder = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  function_ : function_handle option;
  device : device;
  encoded_length : int64;
  alignment : int64;
  retained : (int64, lifetime) Hashtbl.t;
  parent_encoder : shader_argument_encoder option;
}

type compiler = { raw : Metal_raw.handle; lifetime : lifetime; device : device }

type compute_pipeline = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  device : device;
  bindings : shader_binding array option;
  max_total_threads : int;
}

type render_color_attachment_mapping = Identity | Inherited
type render_blend_state = Blend_disabled | Blend_enabled

type render_blend_factor =
  | Blend_zero
  | Blend_one
  | Blend_source_color
  | Blend_one_minus_source_color
  | Blend_source_alpha
  | Blend_one_minus_source_alpha
  | Blend_destination_color
  | Blend_one_minus_destination_color
  | Blend_destination_alpha
  | Blend_one_minus_destination_alpha
  | Blend_source_alpha_saturated
  | Blend_color
  | Blend_one_minus_color
  | Blend_alpha
  | Blend_one_minus_alpha
  | Blend_source1_color
  | Blend_one_minus_source1_color
  | Blend_source1_alpha
  | Blend_one_minus_source1_alpha

type render_blend_operation =
  | Blend_add
  | Blend_subtract
  | Blend_reverse_subtract
  | Blend_min
  | Blend_max

type render_color_write = Write_red | Write_green | Write_blue | Write_alpha

type render_color_attachment = {
  format : pixel_format;
  blending : render_blend_state;
  source_rgb : render_blend_factor;
  destination_rgb : render_blend_factor;
  rgb_operation : render_blend_operation;
  source_alpha : render_blend_factor;
  destination_alpha : render_blend_factor;
  alpha_operation : render_blend_operation;
  write_mask : render_color_write list;
}

type render_pipeline_reflection = {
  vertex_bindings : shader_binding array;
  fragment_bindings : shader_binding array;
}

type render_pipeline = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  device : device;
  raster_sample_count : int;
  color_formats : pixel_format list;
  reflection : render_pipeline_reflection option;
}

type residency_set = { lifetime : lifetime }

type command_queue = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  device : device;
  residency_sets : residency_set list ref;
}

type command_phase = Recording | Submitted | Failed

type indirect_command_kind =
  | Indirect_draw
  | Indirect_draw_indexed
  | Indirect_concurrent_dispatch
  | Indirect_concurrent_dispatch_threads

type indirect_command_buffer_descriptor = {
  command_types : indirect_command_kind list;
  inherit_buffers : bool;
  inherit_pipeline_state : bool;
  max_vertex_buffer_bind_count : int;
  max_fragment_buffer_bind_count : int;
  max_kernel_buffer_bind_count : int;
  support_ray_tracing : bool;
  support_dynamic_attribute_stride : bool;
  max_kernel_threadgroup_memory_bind_count : int;
  max_object_buffer_bind_count : int;
  max_mesh_buffer_bind_count : int;
  max_object_threadgroup_memory_bind_count : int;
  inherit_depth_stencil_state : bool;
  inherit_depth_bias : bool;
  inherit_depth_clip_mode : bool;
  inherit_cull_mode : bool;
  inherit_front_facing_winding : bool;
  inherit_triangle_fill_mode : bool;
  support_color_attachment_mapping : bool;
}

type indirect_retained = Indirect_buffer of buffer | Indirect_render_pipeline of render_pipeline

type indirect_command_buffer = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  device : device;
  max_command_count : int;
  command_types : indirect_command_kind list;
  retained : indirect_retained list ref;
}

type indirect_command_buffer_handle = indirect_command_buffer

type indirect_render_command = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  parent : indirect_command_buffer;
}

type render_encoder_prepared_resources = {
  lifetime : lifetime;
  prepared_buffers : buffer array;
  prepared_textures : texture array;
}

type prepared_command_resources = {
  prepared_pipeline_roots : render_pipeline array;
  prepared_buffer_roots : buffer array;
  prepared_texture_roots : texture array;
  prepared_depth_stencil_roots : depth_stencil array;
  prepared_sample_roots : resource100_sample_buffer array;
  prepared_indirect_roots : indirect_command_buffer array;
  prepared_resource_roots : render_encoder_prepared_resources array;
}

let empty_prepared_command_resources =
  {
    prepared_pipeline_roots = [||];
    prepared_buffer_roots = [||];
    prepared_texture_roots = [||];
    prepared_depth_stencil_roots = [||];
    prepared_sample_roots = [||];
    prepared_indirect_roots = [||];
    prepared_resource_roots = [||];
  }

type command_resource =
  | Command_buffer_buffer of buffer
  | Command_buffer_acceleration_structure of acceleration_structure
  | Command_buffer_texture of texture
  | Command_buffer_sampler of sampler
  | Command_buffer_depth_stencil of depth_stencil
  | Command_buffer_render_pipeline of render_pipeline
  | Command_buffer_indirect of indirect_command_buffer
  | Command_buffer_drawable of metal_drawable
  | Command_buffer_prepared_command of prepared_command_resources

type prepared_resource_slot = { mutable prepared_active : bool; prepared_lifetime : lifetime }

type prepared_command_slot = {
  mutable prepared_command_active : bool;
  mutable prepared_command : prepared_command_resources;
}

type command_buffer = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  queue : command_queue;
  mutable phase : command_phase;
  resources : command_resource list ref;
  (* Lifetime identities already in [resources]: dedup in O(1) per bind. *)
  retained_identities : (int, unit) Hashtbl.t;
  prepared_resource : prepared_resource_slot option;
  prepared_command_slot : prepared_command_slot;
  mutable scoped_prepared_active : bool;
  scoped_prepared_lifetime : lifetime;
  callback_tokens : nativeint list ref;
  presentation_events : lifetime list ref;
}

type compute_encoder = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  command_buffer : command_buffer;
  mutable pipeline : compute_pipeline option;
}

type render_encoder = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  command_buffer : command_buffer;
  target : texture;
  mutable pipeline : render_pipeline option;
}

type blit_encoder = { raw : Metal_raw.handle; lifetime : lifetime; command_buffer : command_buffer }

type acceleration_encoder = {
  raw : Metal_raw.handle;
  lifetime : lifetime;
  command_buffer : command_buffer;
}

(* MTLCommandBufferStatus, or -1 when Metal cannot report it *)
let command_buffer_status raw =
  match Metal_raw.Registry.command_buffer_status raw with
  | Ok status -> Int64.to_int status
  | Error _ -> -1

let make_device raw =
  let* registry_id =
    native_result "Metal.Device.registry_id" (Metal_raw.Registry.device_registry_id raw)
  in
  Ok ({ raw; lifetime = lifetime (); registry_id } : device)

(* [Gc.finalise_last], and a closure that holds the [destroyed] flag instead of
   the value: a child destroyed in the frame that made it then dies in the
   minor heap. [Gc.finalise] promoted every command buffer, encoder, drawable
   and pass descriptor and kept each for two major cycles, and since a cycle's
   length follows the heap size the heap grew with the frame count (0.35 KiB
   per frame, 65 MB after 96,000 frames). [on_finalize] must not capture the
   value either, or the value is never collected. *)
let finalize_after destroyed dependents on_finalize () =
  if Atomic.compare_and_set destroyed false true then begin
    on_finalize ();
    Atomic.decr dependents
  end

let attach_finalizer ?(on_finalize = fun () -> ()) value lifetime parent =
  Gc.finalise_last (finalize_after lifetime.destroyed parent.dependents on_finalize) value

let attach_lifetime_finalizer ?(on_finalize = fun () -> ()) lifetime parent =
  Gc.finalise_last (finalize_after lifetime.destroyed parent.dependents on_finalize) lifetime

let command_resource_lifetime = function
  | Command_buffer_buffer buffer -> buffer.lifetime
  | Command_buffer_acceleration_structure value -> value.lifetime
  | Command_buffer_texture texture -> texture.lifetime
  | Command_buffer_sampler sampler -> sampler.lifetime
  | Command_buffer_depth_stencil value -> value.lifetime
  | Command_buffer_render_pipeline pipeline -> pipeline.lifetime
  | Command_buffer_indirect value -> value.lifetime
  | Command_buffer_drawable value -> value.lifetime
  | Command_buffer_prepared_command _ ->
      invalid_arg "prepared command has multiple resource lifetimes"

let command_texture_heap (value : texture) =
  match value.parent with
  | Texture_resource (Device_resource _) -> None
  | Texture_drawable_resource _ -> None

let command_resource_heap = function
  | Command_buffer_buffer { parent = Device_resource _; _ } -> None
  | Command_buffer_acceleration_structure _ -> None
  | Command_buffer_texture texture -> command_texture_heap texture
  | Command_buffer_sampler _ -> None
  | Command_buffer_depth_stencil _ -> None
  | Command_buffer_render_pipeline _ | Command_buffer_indirect _ -> None
  | Command_buffer_drawable _ -> None
  | Command_buffer_prepared_command _ -> None

let release_prepared_command_resources prepared =
  for index = 0 to Array.length prepared.prepared_pipeline_roots - 1 do
    detach (Array.unsafe_get prepared.prepared_pipeline_roots index).lifetime
  done;
  for index = 0 to Array.length prepared.prepared_buffer_roots - 1 do
    let buffer = Array.unsafe_get prepared.prepared_buffer_roots index in
    detach buffer.lifetime;
    match buffer.parent with Device_resource _ -> ()
  done;
  for index = 0 to Array.length prepared.prepared_texture_roots - 1 do
    let texture = Array.unsafe_get prepared.prepared_texture_roots index in
    detach texture.lifetime;
    Option.iter (fun heap -> Atomic.decr heap.active_uses) (command_texture_heap texture)
  done;
  for index = 0 to Array.length prepared.prepared_depth_stencil_roots - 1 do
    detach (Array.unsafe_get prepared.prepared_depth_stencil_roots index).lifetime
  done;
  for index = 0 to Array.length prepared.prepared_sample_roots - 1 do
    detach (Array.unsafe_get prepared.prepared_sample_roots index).lifetime
  done;
  for index = 0 to Array.length prepared.prepared_indirect_roots - 1 do
    detach (Array.unsafe_get prepared.prepared_indirect_roots index).lifetime
  done;
  for index = 0 to Array.length prepared.prepared_resource_roots - 1 do
    let resources = Array.unsafe_get prepared.prepared_resource_roots index in
    for buffer_index = 0 to Array.length resources.prepared_buffers - 1 do
      match (Array.unsafe_get resources.prepared_buffers buffer_index).parent with
      | Device_resource _ -> ()
    done;
    for texture_index = 0 to Array.length resources.prepared_textures - 1 do
      Option.iter
        (fun heap -> Atomic.decr heap.active_uses)
        (command_texture_heap (Array.unsafe_get resources.prepared_textures texture_index))
    done;
    detach resources.lifetime
  done

let release_command_resources resources identities =
  let retained = !resources in
  resources := [];
  Hashtbl.reset identities;
  List.iter
    (function
      | Command_buffer_prepared_command prepared -> release_prepared_command_resources prepared
      | resource ->
          detach (command_resource_lifetime resource);
          Option.iter (fun heap -> Atomic.decr heap.active_uses) (command_resource_heap resource))
    retained

let release_prepared_resource slot =
  if slot.prepared_active then begin
    slot.prepared_active <- false;
    detach slot.prepared_lifetime
  end

let release_prepared_command_slot slot =
  if slot.prepared_command_active then begin
    let prepared = slot.prepared_command in
    slot.prepared_command_active <- false;
    slot.prepared_command <- empty_prepared_command_resources;
    release_prepared_command_resources prepared
  end

let release_finalized_command_buffer_resources resources identities prepared_resource
    prepared_command_slot =
  release_command_resources resources identities;
  release_prepared_resource prepared_resource;
  release_prepared_command_slot prepared_command_slot

let release_command_buffer_resources command_buffer =
  release_command_resources command_buffer.resources command_buffer.retained_identities;
  (match command_buffer.prepared_resource with
  | Some slot -> release_prepared_resource slot
  | None when command_buffer.scoped_prepared_active ->
      command_buffer.scoped_prepared_active <- false;
      detach command_buffer.scoped_prepared_lifetime
  | None -> ());
  release_prepared_command_slot command_buffer.prepared_command_slot

(* True when [lifetime] is already retained by this command buffer; otherwise
   records it and returns false. Every retain below goes through this table, so
   binding the same resources in thousands of draws costs O(1) each instead of
   rescanning the retained list. *)
let already_retained (command_buffer : command_buffer) (lifetime : lifetime) =
  Hashtbl.mem command_buffer.retained_identities lifetime.identity
  ||
  (Hashtbl.add command_buffer.retained_identities lifetime.identity ();
   false)

let retain_command_buffer_buffer (command_buffer : command_buffer) (buffer : buffer) =
  if not (already_retained command_buffer buffer.lifetime) then begin
    attach buffer.lifetime;
    Option.iter
      (fun heap -> Atomic.incr heap.active_uses)
      (match buffer.parent with Device_resource _ -> None);
    command_buffer.resources := Command_buffer_buffer buffer :: !(command_buffer.resources)
  end

let rec command_resources_retain_prepared_command prepared = function
  | [] -> false
  | Command_buffer_prepared_command candidate :: _ -> candidate == prepared
  | _ :: rest -> command_resources_retain_prepared_command prepared rest

let retain_prepared_command_resources prepared =
  for index = 0 to Array.length prepared.prepared_pipeline_roots - 1 do
    attach (Array.unsafe_get prepared.prepared_pipeline_roots index).lifetime
  done;
  for index = 0 to Array.length prepared.prepared_buffer_roots - 1 do
    let buffer = Array.unsafe_get prepared.prepared_buffer_roots index in
    attach buffer.lifetime;
    match buffer.parent with Device_resource _ -> ()
  done;
  for index = 0 to Array.length prepared.prepared_texture_roots - 1 do
    let texture = Array.unsafe_get prepared.prepared_texture_roots index in
    attach texture.lifetime;
    Option.iter (fun heap -> Atomic.incr heap.active_uses) (command_texture_heap texture)
  done;
  for index = 0 to Array.length prepared.prepared_depth_stencil_roots - 1 do
    attach (Array.unsafe_get prepared.prepared_depth_stencil_roots index).lifetime
  done;
  for index = 0 to Array.length prepared.prepared_sample_roots - 1 do
    attach (Array.unsafe_get prepared.prepared_sample_roots index).lifetime
  done;
  for index = 0 to Array.length prepared.prepared_indirect_roots - 1 do
    attach (Array.unsafe_get prepared.prepared_indirect_roots index).lifetime
  done;
  for index = 0 to Array.length prepared.prepared_resource_roots - 1 do
    let resources = Array.unsafe_get prepared.prepared_resource_roots index in
    attach resources.lifetime;
    for buffer_index = 0 to Array.length resources.prepared_buffers - 1 do
      match (Array.unsafe_get resources.prepared_buffers buffer_index).parent with
      | Device_resource _ -> ()
    done;
    for texture_index = 0 to Array.length resources.prepared_textures - 1 do
      Option.iter
        (fun heap -> Atomic.incr heap.active_uses)
        (command_texture_heap (Array.unsafe_get resources.prepared_textures texture_index))
    done
  done

let retain_command_buffer_prepared_command_overflow command_buffer prepared =
  if not (command_resources_retain_prepared_command prepared !(command_buffer.resources)) then begin
    retain_prepared_command_resources prepared;
    command_buffer.resources :=
      Command_buffer_prepared_command prepared :: !(command_buffer.resources)
  end

let retain_command_buffer_prepared_command command_buffer prepared =
  let primary = command_buffer.prepared_command_slot in
  if primary.prepared_command_active then begin
    if primary.prepared_command != prepared then
      retain_command_buffer_prepared_command_overflow command_buffer prepared
  end
  else begin
    retain_prepared_command_resources prepared;
    primary.prepared_command <- prepared;
    primary.prepared_command_active <- true
  end

let retain_command_buffer_acceleration_structure (command_buffer : command_buffer)
    (value : acceleration_structure) =
  if not (already_retained command_buffer value.lifetime) then begin
    attach value.lifetime;
    command_buffer.resources :=
      Command_buffer_acceleration_structure value :: !(command_buffer.resources)
  end

let retain_command_buffer_texture (command_buffer : command_buffer) (texture : texture) =
  if not (already_retained command_buffer texture.lifetime) then begin
    attach texture.lifetime;
    Option.iter (fun heap -> Atomic.incr heap.active_uses) (command_texture_heap texture);
    command_buffer.resources := Command_buffer_texture texture :: !(command_buffer.resources)
  end

let retain_command_buffer_sampler (command_buffer : command_buffer) (sampler : sampler) =
  if not (already_retained command_buffer sampler.lifetime) then begin
    attach sampler.lifetime;
    command_buffer.resources := Command_buffer_sampler sampler :: !(command_buffer.resources)
  end

let retain_command_buffer_depth_stencil command_buffer (value : depth_stencil) =
  if not (already_retained command_buffer value.lifetime) then begin
    attach value.lifetime;
    command_buffer.resources := Command_buffer_depth_stencil value :: !(command_buffer.resources)
  end

let retain_command_buffer_indirect (command_buffer : command_buffer)
    (value : indirect_command_buffer) =
  if not (already_retained command_buffer value.lifetime) then begin
    attach value.lifetime;
    command_buffer.resources := Command_buffer_indirect value :: !(command_buffer.resources)
  end

let retain_command_buffer_render_pipeline (command_buffer : command_buffer)
    (pipeline : render_pipeline) =
  if not (already_retained command_buffer pipeline.lifetime) then begin
    attach pipeline.lifetime;
    command_buffer.resources :=
      Command_buffer_render_pipeline pipeline :: !(command_buffer.resources)
  end

let retain_command_buffer_drawable (command_buffer : command_buffer) (value : metal_drawable) =
  if not (already_retained command_buffer value.lifetime) then begin
    attach value.lifetime;
    command_buffer.resources := Command_buffer_drawable value :: !(command_buffer.resources)
  end

let release_queue_residency_sets residency_sets =
  let retained = !residency_sets in
  residency_sets := [];
  List.iter (fun (value : residency_set) -> detach value.lifetime) retained

let deactivate_allocation (value : heap_allocation option) =
  match value with None -> () | Some allocation -> Atomic.set allocation.active false

let resource_parent_lifetime = function Device_resource device -> device.lifetime
let resource_parent_extra_device (_device : device) = function Device_resource _ -> None

let texture_parent_lifetime = function
  | Texture_resource parent -> resource_parent_lifetime parent
  | Texture_drawable_resource drawable -> drawable.lifetime

let texture_parent_extra_device (device : device) = function
  | Texture_resource parent -> resource_parent_extra_device device parent
  | Texture_drawable_resource _ -> None

let resource_state () = { relinquished = Atomic.make false; purgeable = Atomic.make Nonvolatile }
let parent_heap = function Device_resource _ -> None

let texture_heap (value : texture) =
  match value.parent with
  | Texture_resource parent -> parent_heap parent
  | Texture_drawable_resource _ -> None

let ensure_heap_nonvolatile operation = function
  | Some heap when Atomic.get heap.purgeable <> Nonvolatile ->
      error operation Invalid_state "resource heap must be nonvolatile"
  | None | Some _ -> Ok ()

let ensure_resource_usable operation state heap =
  if Atomic.get state.relinquished then
    error operation Invalid_state "resource has relinquished its storage for aliasing"
  else
    match Atomic.get state.purgeable with
    | Volatile ->
        error operation Invalid_state "resource is volatile and must be restored before access"
    | Empty ->
        error operation Invalid_state
          "resource contents are empty and must be restored before access"
    | Nonvolatile ->
        let* () = ensure_heap_nonvolatile operation heap in
        Ok ()

let ensure_buffer_usable operation (value : buffer) =
  let* () = ensure_live operation value.lifetime in
  ensure_resource_usable operation value.state (parent_heap value.parent)

let ensure_texture_usable operation (value : texture) =
  let* () = ensure_live operation value.lifetime in
  ensure_resource_usable operation value.state (texture_heap value)

let same_device (left : device) (right : device) = Int64.equal left.registry_id right.registry_id

let ensure_same_device operation expected actual =
  if same_device expected actual then Ok ()
  else error operation Device_mismatch "resources belong to different Metal devices"

let storage_code = function Shared -> 0 | Managed -> 1 | Private -> 2
let cache_code = function Default_cache -> 0 | Write_combined -> 1
let hazard_code = function Default_hazard_tracking -> 0 | Untracked -> 1 | Tracked -> 2

let resource_options_code ~storage ~cpu_cache ~hazard_tracking =
  cache_code cpu_cache lor (storage_code storage lsl 4) lor (hazard_code hazard_tracking lsl 8)

let concrete_hazard_tracking ~heap = function
  | Default_hazard_tracking -> if heap then Untracked else Tracked
  | (Untracked | Tracked) as mode -> mode

module Device = struct
  type t = device

  type family =
    | Apple1
    | Apple2
    | Apple3
    | Apple4
    | Apple5
    | Apple6
    | Apple7
    | Apple8
    | Apple9
    | Apple10
    | Mac2
    | Common1
    | Common2
    | Common3
    | Metal3
    | Metal4

  type info = {
    name : string;
    registry_id : int64;
    low_power : bool;
    removable : bool;
    headless : bool;
    unified_memory : bool;
    recommended_max_working_set_size : int64;
    current_allocated_size : int64;
    max_buffer_length : int64;
    max_threadgroup_memory_length : int64;
    raytracing : bool;
    raytracing_from_render : bool;
    dynamic_libraries : bool;
  }

  let system_default () =
    on_main "Metal.Device.system_default" (fun () ->
        let* raw = native_result "Metal.Device.system_default" (Metal_raw.default_device ()) in
        make_device raw)

  let same = same_device

  let info (value : t) =
    let operation = "Metal.Device.info" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let* max_threadgroup_memory_length =
          native operation (Metal_raw.Registry.device_max_threadgroup_memory_length value.raw)
        in
        let* name = native operation (Metal_raw.Registry.device_name value.raw) in
        let* low_power = native operation (Metal_raw.Registry.device_is_low_power value.raw) in
        let* removable = native operation (Metal_raw.Registry.device_is_removable value.raw) in
        let* headless = native operation (Metal_raw.Registry.device_is_headless value.raw) in
        let* unified_memory =
          native operation (Metal_raw.Registry.device_has_unified_memory value.raw)
        in
        let* recommended_max_working_set_size =
          native operation (Metal_raw.Registry.device_recommended_max_working_set_size value.raw)
        in
        let* current_allocated_size =
          native operation (Metal_raw.Registry.device_current_allocated_size value.raw)
        in
        let* max_buffer_length =
          native operation (Metal_raw.Registry.device_max_buffer_length value.raw)
        in
        let* raytracing =
          native operation (Metal_raw.Registry.device_supports_raytracing value.raw)
        in
        let* raytracing_from_render =
          native operation (Metal_raw.Registry.device_supports_raytracing_from_render value.raw)
        in
        let* dynamic_libraries =
          native operation (Metal_raw.Registry.device_supports_dynamic_libraries value.raw)
        in
        Ok
          {
            name = (if name = "" then "Unnamed Metal device" else name);
            registry_id = value.registry_id;
            low_power;
            removable;
            headless;
            unified_memory;
            recommended_max_working_set_size;
            current_allocated_size;
            max_buffer_length;
            max_threadgroup_memory_length;
            raytracing;
            raytracing_from_render;
            dynamic_libraries;
          })

  let family_code = function
    | Apple1 -> 1001
    | Apple2 -> 1002
    | Apple3 -> 1003
    | Apple4 -> 1004
    | Apple5 -> 1005
    | Apple6 -> 1006
    | Apple7 -> 1007
    | Apple8 -> 1008
    | Apple9 -> 1009
    | Apple10 -> 1010
    | Mac2 -> 2002
    | Common1 -> 3001
    | Common2 -> 3002
    | Common3 -> 3003
    | Metal3 -> 5001
    | Metal4 -> 5002

  let supports_family (value : t) family =
    on_main "Metal.Device.supports_family" (fun () ->
        let* () = ensure_live "Metal.Device.supports_family" value.lifetime in
        let* supported =
          native_result "Metal.Device.supports_family"
            (Metal_raw.Registry.device_supports_family value.raw
               (Int64.of_int (family_code family)))
        in
        Ok supported)

  let supports_texture_sample_count (value : t) sample_count =
    on_main "Metal.Device.supports_texture_sample_count" (fun () ->
        let* () = ensure_live "Metal.Device.supports_texture_sample_count" value.lifetime in
        if sample_count <= 0 then
          error "Metal.Device.supports_texture_sample_count" Invalid_argument
            "texture sample count must be positive"
        else
          native "Metal.Device.supports_texture_sample_count"
            (Metal_raw.Registry.device_supports_texture_sample_count value.raw
               (Int64.of_int sample_count)))

  let destroy (value : t) =
    destroy_parent "Metal.Device.destroy" value.lifetime value.raw (fun () -> ())
end

module Buffer = struct
  type t = buffer
  type storage_mode = buffer_storage_mode = Shared | Managed | Private
  type cpu_cache_mode = resource_cpu_cache_mode = Default_cache | Write_combined

  type hazard_tracking_mode = resource_hazard_tracking_mode =
    | Default_hazard_tracking
    | Untracked
    | Tracked

  let validate_create operation (device : Device.t) ~length ~label =
    if length <= 0L then error operation Invalid_argument "buffer length must be positive"
    else if
      match Metal_raw.Registry.device_max_buffer_length device.raw with
      | Ok limit -> length > limit
      | Error _ -> true
    then error operation Invalid_argument "buffer length exceeds the device limit"
    else if option_exists contains_nul label then
      error operation Invalid_argument "label contains a NUL byte"
    else Ok ()

  let finish_create operation ~(device : Device.t) ~parent ~length ~storage ~cpu_cache
      ~hazard_tracking ~heap_offset ~allocation ~label raw =
    let actual_length, actual_storage, actual_cache, actual_hazard, actual_offset =
      Metal_raw.buffer_info raw
    in
    let expected_hazard =
      concrete_hazard_tracking
        ~heap:(match parent with Device_resource _ -> false)
        hazard_tracking
    in
    if
      actual_length <> length
      || actual_storage <> storage_code storage
      || actual_cache <> cache_code cpu_cache
      || actual_hazard <> hazard_code expected_hazard
      || option_exists (fun expected -> expected <> actual_offset) heap_offset
    then begin
      ignore (Metal_raw.destroy raw);
      native_error operation "Metal changed checked buffer properties during creation"
    end
    else
      let label_result =
        match label with None -> Ok () | Some label -> Metal_raw.buffer_set_label raw label
      in
      match label_result with
      | Error message ->
          ignore (Metal_raw.destroy raw);
          native_error operation message
      | Ok () ->
          let parent_lifetime = resource_parent_lifetime parent in
          let value : t =
            {
              raw;
              lifetime = lifetime ();
              device;
              length;
              storage;
              parent;
              allocation;
              state = resource_state ();
            }
          in
          attach parent_lifetime;
          let extra_device = resource_parent_extra_device device parent in
          Option.iter attach extra_device;
          attach_finalizer
            ~on_finalize:(fun () ->
              deactivate_allocation allocation;
              Option.iter detach extra_device)
            value value.lifetime parent_lifetime;
          Ok value

  let create ~(device : Device.t) ~length ~storage ?(cpu_cache = Default_cache) ?label () =
    let hazard_tracking = Default_hazard_tracking in
    on_main "Metal.Buffer.create" (fun () ->
        let* () = ensure_live "Metal.Buffer.create" device.lifetime in
        let* () = validate_create "Metal.Buffer.create" device ~length ~label in
        let options = resource_options_code ~storage ~cpu_cache ~hazard_tracking in
        let* raw =
          native_result "Metal.Buffer.create" (Metal_raw.buffer_create device.raw length options)
        in
        finish_create "Metal.Buffer.create" ~device ~parent:(Device_resource device) ~length
          ~storage ~cpu_cache ~hazard_tracking ~heap_offset:None ~allocation:None ~label raw)

  let length (value : t) = value.length

  let validate_range operation ~total ~offset ~length =
    if offset < 0L || length < 0 then error operation Invalid_argument "range is negative"
    else
      let length64 = Int64.of_int length in
      if offset > total || length64 > Int64.sub total offset then
        error operation Invalid_argument "range exceeds the buffer"
      else Ok ()

  let write_bytes (value : t) ~dst_offset bytes =
    let src_offset = 0 in
    on_main "Metal.Buffer.write_bytes" (fun () ->
        let* () = ensure_buffer_usable "Metal.Buffer.write_bytes" value in
        if value.storage = Private then
          error "Metal.Buffer.write_bytes" Unsupported "private buffers have no CPU mapping"
        else
          let source_length = Bytes.length bytes in
          if src_offset < 0 || src_offset > source_length then
            error "Metal.Buffer.write_bytes" Invalid_argument
              "source offset is outside the byte buffer"
          else
            let length = source_length - src_offset in
            let* () =
              validate_range "Metal.Buffer.write_bytes" ~total:value.length ~offset:dst_offset
                ~length
            in
            let* () =
              native_result "Metal.Buffer.write_bytes"
                (Metal_raw.buffer_write value.raw dst_offset bytes src_offset length)
            in
            Ok ())

  let read_bytes (value : t) ~offset ~length =
    on_main "Metal.Buffer.read_bytes" (fun () ->
        let* () = ensure_buffer_usable "Metal.Buffer.read_bytes" value in
        if value.storage = Private then
          error "Metal.Buffer.read_bytes" Unsupported "private buffers have no CPU mapping"
        else if length > Sys.max_string_length then
          error "Metal.Buffer.read_bytes" Invalid_argument
            "read length exceeds the maximum OCaml byte-buffer size"
        else
          let* () = validate_range "Metal.Buffer.read_bytes" ~total:value.length ~offset ~length in
          let* bytes =
            native_result "Metal.Buffer.read_bytes" (Metal_raw.buffer_read value.raw offset length)
          in
          Ok bytes)

  let destroy (value : t) =
    destroy_parent "Metal.Buffer.destroy" value.lifetime value.raw (fun () ->
        deactivate_allocation value.allocation;
        detach (resource_parent_lifetime value.parent);
        Option.iter detach (resource_parent_extra_device value.device value.parent))
end

module Acceleration_structure = struct
  type t = acceleration_structure
  type structure = t

  type sizes = {
    acceleration_structure_size : int64;
    build_scratch_buffer_size : int64;
    refit_scratch_buffer_size : int64;
  }

  (* Generic build descriptors: triangles, bounding boxes, and
     curves, each static or keyframed, under one primitive descriptor; and
     instance descriptors of the default, user-id, or motion record kinds. *)
  module Build = struct
    type keyframe = { buffer : buffer; offset : int64 }
    type border = Clamp | Vanish

    type motion = {
      keyframe_count : int;
      start_time : float;
      end_time : float;
      start_border : border;
      end_border : border;
    }

    type index = { index_buffer : buffer; index_offset : int64; index_uint16 : bool }
    type common = { opaque : bool; allow_duplicate_intersection : bool }

    let default_common = { opaque = true; allow_duplicate_intersection = false }

    type curve_type = Round | Flat
    type curve_basis = Bspline | Catmull_rom | Linear | Bezier
    type end_caps = No_caps | Disk | Sphere

    type geometry =
      | Triangles of {
          vertices : keyframe list;
          vertex_stride : int64;
          triangle_count : int64;
          index : index option;
          common : common;
        }
      | Bounding_boxes of { boxes : keyframe list; stride : int64; count : int64; common : common }
      | Curves of {
          control_points : keyframe list;
          control_stride : int64;
          control_point_count : int64;
          radii : keyframe list;
          radius_stride : int64;
          index : index;
          segment_count : int64;
          control_points_per_segment : int;
          curve_type : curve_type;
          basis : curve_basis;
          end_caps : end_caps;
          common : common;
        }

    type instance_kind = User_id_instances | Motion_instances

    type instance_layout = {
      size : int;
      transform : int;
      options : int;
      mask : int;
      structure_index : int;
      user_id : int;
      transforms_start : int;
      transforms_count : int;
      start_border_offset : int;
      end_border_offset : int;
      start_time_offset : int;
      end_time_offset : int;
    }

    type t = {
      raw : Metal_raw.handle;
      lifetime : lifetime;
      device : device;
      buffers : buffer list;
      structures : structure list;
      kind : instance_kind option;
    }

    let kind_code = function User_id_instances -> 1 | Motion_instances -> 2

    let instance_layout kind =
      let layout = Metal_raw.accel_instance_layout (kind_code kind) in
      {
        size = layout.(0);
        transform = layout.(1);
        options = layout.(2);
        mask = layout.(3);
        structure_index = layout.(4);
        user_id = layout.(5);
        transforms_start = layout.(6);
        transforms_count = layout.(7);
        start_border_offset = layout.(8);
        end_border_offset = layout.(9);
        start_time_offset = layout.(10);
        end_time_offset = layout.(11);
      }

    let keyframe_raw (k : keyframe) : Metal_raw.accel_keyframe =
      { keyframe_buffer = k.buffer.raw; keyframe_offset = k.offset }

    let common_raw (c : common) = (c.opaque, c.allow_duplicate_intersection)

    (* The element at the last index of a strided range must fit its buffer. *)
    let range_fits (buffer : buffer) offset stride count element =
      offset >= 0L && stride >= element && element > 0L && count > 0L && offset <= buffer.length
      && Int64.sub count 1L <= Int64.div (Int64.sub (Int64.sub buffer.length offset) element) stride
      && element <= Int64.sub buffer.length offset

    let validate_keyframes operation (device : device) name keyframes ~motion ~check =
      let expected = match motion with None -> 1 | Some m -> m.keyframe_count in
      if List.length keyframes <> expected then
        error operation Invalid_argument
          (name ^ " keyframe count must match the descriptor's motion keyframes (or be one)")
      else
        let rec loop = function
          | [] -> Ok ()
          | (k : keyframe) :: rest ->
              let* () = ensure_buffer_usable operation k.buffer in
              if not (same_device device k.buffer.device) then
                error operation Device_mismatch (name ^ " buffer belongs to another device")
              else if not (check k) then
                error operation Invalid_argument (name ^ " range exceeds its buffer")
              else loop rest
        in
        loop keyframes

    let validate_index operation (device : device) = function
      | None -> Ok ()
      | Some (i : index) ->
          let* () = ensure_buffer_usable operation i.index_buffer in
          if not (same_device device i.index_buffer.device) then
            error operation Device_mismatch "index buffer belongs to another device"
          else if i.index_offset < 0L || i.index_offset > i.index_buffer.length then
            error operation Invalid_argument "index offset exceeds its buffer"
          else Ok ()

    let geometry_raw (g : geometry) : Metal_raw.accel_geometry_raw =
      match g with
      | Triangles { vertices; vertex_stride; triangle_count; index; common } ->
          let opaque, allow_duplicate = common_raw common in
          let first = List.hd vertices in
          Raw_triangles
            {
              vertex = first.buffer.raw;
              vertex_offset = first.offset;
              vertex_stride;
              triangle_count;
              index = Option.map (fun (i : index) -> i.index_buffer.raw) index;
              index_offset = Option.fold ~none:0L ~some:(fun (i : index) -> i.index_offset) index;
              index_uint16 = Option.fold ~none:false ~some:(fun (i : index) -> i.index_uint16) index;
              keyframes =
                (match vertices with
                | [ _ ] -> [||]
                | _ -> Array.of_list (List.map keyframe_raw vertices));
              opaque;
              allow_duplicate;
            }
      | Bounding_boxes { boxes; stride; count; common } ->
          let box_opaque, box_allow_duplicate = common_raw common in
          let first = List.hd boxes in
          Raw_boxes
            {
              boxes = first.buffer.raw;
              box_offset = first.offset;
              box_stride = stride;
              box_count = count;
              box_keyframes =
                (match boxes with
                | [ _ ] -> [||]
                | _ -> Array.of_list (List.map keyframe_raw boxes));
              box_opaque;
              box_allow_duplicate;
            }
      | Curves
          {
            control_points;
            control_stride;
            control_point_count;
            radii;
            radius_stride;
            index;
            segment_count;
            control_points_per_segment;
            curve_type;
            basis;
            end_caps;
            common;
          } ->
          let curve_opaque, curve_allow_duplicate = common_raw common in
          let control = List.hd control_points and radius = List.hd radii in
          let motion = match control_points with [ _ ] -> false | _ -> true in
          Raw_curves
            {
              control = control.buffer.raw;
              control_offset = control.offset;
              control_stride;
              control_count = control_point_count;
              radius = radius.buffer.raw;
              radius_offset = radius.offset;
              radius_stride;
              curve_index = index.index_buffer.raw;
              curve_index_offset = index.index_offset;
              curve_index_uint16 = index.index_uint16;
              segment_count;
              segment_control_points = Int64.of_int control_points_per_segment;
              curve_type = (match curve_type with Round -> 0 | Flat -> 1);
              curve_basis =
                (match basis with Bspline -> 0 | Catmull_rom -> 1 | Linear -> 2 | Bezier -> 3);
              end_caps = (match end_caps with No_caps -> 0 | Disk -> 1 | Sphere -> 2);
              control_keyframes =
                (if motion then Array.of_list (List.map keyframe_raw control_points) else [||]);
              radius_keyframes =
                (if motion then Array.of_list (List.map keyframe_raw radii) else [||]);
              curve_opaque;
              curve_allow_duplicate;
            }

    let validate_geometry operation (device : device) ~motion = function
      | Triangles { vertices; vertex_stride; triangle_count; index; _ } ->
          if
            vertex_stride < 12L
            || Int64.rem vertex_stride 4L <> 0L
            || triangle_count <= 0L
            || triangle_count > Int64.div Int64.max_int 3L
          then
            error operation Invalid_argument
              "triangle stride must be at least 12 and four-byte aligned with a positive count"
          else
            let count = match index with None -> Int64.mul triangle_count 3L | Some _ -> 1L in
            let* () =
              validate_keyframes operation device "vertex" vertices ~motion ~check:(fun k ->
                  range_fits k.buffer k.offset vertex_stride count 12L)
            in
            validate_index operation device
              (Option.map
                 (fun (i : index) ->
                   ignore
                     (range_fits i.index_buffer i.index_offset
                        (if i.index_uint16 then 2L else 4L)
                        (Int64.mul triangle_count 3L)
                        (if i.index_uint16 then 2L else 4L));
                   i)
                 index)
      | Bounding_boxes { boxes; stride; count; _ } ->
          if stride < 24L || Int64.rem stride 4L <> 0L || count <= 0L then
            error operation Invalid_argument
              "bounding box stride must be at least 24 and four-byte aligned with a positive count"
          else
            validate_keyframes operation device "bounding box" boxes ~motion ~check:(fun k ->
                range_fits k.buffer k.offset stride count 24L)
      | Curves
          {
            control_points;
            control_stride;
            control_point_count;
            radii;
            radius_stride;
            index;
            segment_count;
            control_points_per_segment;
            _;
          } ->
          if
            control_stride < 12L || radius_stride < 4L || control_point_count < 2L
            || segment_count <= 0L || control_points_per_segment < 2
            || control_points_per_segment > 4
          then
            error operation Invalid_argument "curve strides, counts, or segment shape are invalid"
          else if List.length radii <> List.length control_points then
            error operation Invalid_argument
              "curve radius keyframes must match control point keyframes"
          else
            let* () =
              validate_keyframes operation device "control point" control_points ~motion
                ~check:(fun k ->
                  range_fits k.buffer k.offset control_stride control_point_count 12L)
            in
            let* () =
              validate_keyframes operation device "radius" radii ~motion ~check:(fun k ->
                  range_fits k.buffer k.offset radius_stride control_point_count 4L)
            in
            validate_index operation device (Some index)

    let geometry_buffers = function
      | Triangles { vertices; index; _ } ->
          List.map (fun (k : keyframe) -> k.buffer) vertices
          @ Option.fold ~none:[] ~some:(fun (i : index) -> [ i.index_buffer ]) index
      | Bounding_boxes { boxes; _ } -> List.map (fun (k : keyframe) -> k.buffer) boxes
      | Curves { control_points; radii; index; _ } ->
          List.map (fun (k : keyframe) -> k.buffer) (control_points @ radii)
          @ [ index.index_buffer ]

    (* A descriptor borrows its buffers and structures: like [Triangle.t] it
       pins nothing, and every build revalidates them and retains them on the
       command buffer for the GPU's lifetime of the work. *)
    let finish operation (device : device) raw ~buffers ~structures ~instance_count:_ ~kind =
      attach device.lifetime;
      let value = { raw; lifetime = lifetime (); device; buffers; structures; kind } in
      Gc.finalise
        (fun (value : t) ->
          if Atomic.compare_and_set value.lifetime.destroyed false true then begin
            ignore (Metal_raw.destroy value.raw);
            detach value.device.lifetime
          end)
        value;
      ignore operation;
      Ok value

    let primitive (device : Device.t) ?motion geometries =
      let operation = "Metal.Acceleration_structure.Build.primitive" in
      on_main operation (fun () ->
          let* () = ensure_live operation device.lifetime in
          if geometries = [] then
            error operation Invalid_argument "descriptor requires at least one geometry"
          else if
            match motion with
            | Some m ->
                m.keyframe_count < 2
                || (not (m.start_time < m.end_time))
                || not (Float.is_finite m.start_time && Float.is_finite m.end_time)
            | None -> false
          then
            error operation Invalid_argument
              "motion requires at least two keyframes and an increasing finite time range"
          else
            let rec validate = function
              | [] -> Ok ()
              | g :: rest ->
                  let* () = validate_geometry operation device ~motion g in
                  validate rest
            in
            let* () = validate geometries in
            let raw : Metal_raw.accel_primitive_raw =
              {
                geometries = Array.of_list (List.map geometry_raw geometries);
                motion =
                  Option.map
                    (fun (m : motion) : Metal_raw.accel_motion_raw ->
                      {
                        keyframe_count = Int64.of_int m.keyframe_count;
                        start_time = m.start_time;
                        end_time = m.end_time;
                        start_border = (match m.start_border with Clamp -> 0 | Vanish -> 1);
                        end_border = (match m.end_border with Clamp -> 0 | Vanish -> 1);
                      })
                    motion;
                primitive_refit = false;
                fast_build = false;
              }
            in
            let* raw = native_result operation (Metal_raw.accel_descriptor_primitive raw) in
            finish operation device raw
              ~buffers:(List.concat_map geometry_buffers geometries)
              ~structures:[] ~instance_count:0L ~kind:None)

    let instances (device : Device.t) ~(buffer : buffer) ?(offset = 0L) ~count ~kind
        ?motion_transforms (primitives : structure array) =
      let stride = None in
      let operation = "Metal.Acceleration_structure.Build.instances" in
      on_main operation (fun () ->
          let layout = instance_layout kind in
          let stride = Option.value stride ~default:(Int64.of_int layout.size) in
          let* () = ensure_live operation device.lifetime in
          let* () = ensure_buffer_usable operation buffer in
          if not (same_device device buffer.device) then
            error operation Device_mismatch "instance buffer belongs to another device"
          else if Array.length primitives = 0 then
            error operation Invalid_argument "instances reference no structures"
          else if
            count <= 0L
            || stride < Int64.of_int layout.size
            || Int64.rem stride 4L <> 0L
            || Int64.rem offset 4L <> 0L
            || not (range_fits buffer offset stride count (Int64.of_int layout.size))
          then
            error operation Invalid_argument
              "instance range is invalid, unaligned, or exceeds its buffer"
          else if kind = Motion_instances && motion_transforms = None then
            error operation Invalid_argument "motion instances require a transform buffer"
          else
            let rec check index =
              if index = Array.length primitives then Ok ()
              else
                let* () = ensure_live operation primitives.(index).lifetime in
                if not (same_device device primitives.(index).device) then
                  error operation Device_mismatch "primitive belongs to another device"
                else check (index + 1)
            in
            let* () = check 0 in
            let transforms =
              match motion_transforms with
              | None -> Ok None
              | Some ((b : buffer), transform_offset, transform_count) ->
                  let* () = ensure_buffer_usable operation b in
                  if not (same_device device b.device) then
                    error operation Device_mismatch
                      "motion transform buffer belongs to another device"
                  else if not (range_fits b transform_offset 48L transform_count 48L) then
                    error operation Invalid_argument "motion transform range exceeds its buffer"
                  else Ok (Some (b, transform_offset, transform_count))
            in
            let* transforms = transforms in
            let raw : Metal_raw.accel_instances_raw =
              {
                instances_buffer = buffer.raw;
                instances_offset = offset;
                instances_stride = stride;
                instances_count = count;
                instance_kind = kind_code kind;
                instanced = Array.map (fun (x : structure) -> x.raw) primitives;
                motion_transforms = Option.map (fun ((b : buffer), _, _) -> b.raw) transforms;
                motion_transform_offset = Option.fold ~none:0L ~some:(fun (_, o, _) -> o) transforms;
                motion_transform_count = Option.fold ~none:0L ~some:(fun (_, _, c) -> c) transforms;
                instances_refit = false;
              }
            in
            let* raw = native_result operation (Metal_raw.accel_descriptor_instances raw) in
            finish operation device raw
              ~buffers:(buffer :: Option.fold ~none:[] ~some:(fun (b, _, _) -> [ b ]) transforms)
              ~structures:(Array.to_list primitives) ~instance_count:count ~kind:(Some kind))

    let sizes ~(device : Device.t) (value : t) =
      let operation = "Metal.Acceleration_structure.Build.sizes" in
      on_main operation (fun () ->
          let* () = ensure_live operation value.lifetime in
          if not (same_device device value.device) then
            error operation Device_mismatch "descriptor belongs to another device"
          else
            let* acceleration_structure_size, build_scratch_buffer_size, refit_scratch_buffer_size =
              native_result operation (Metal_raw.accel_descriptor_sizes device.raw value.raw)
            in
            Ok { acceleration_structure_size; build_scratch_buffer_size; refit_scratch_buffer_size })

    let instance_kind (value : t) = value.kind

    let destroy (value : t) =
      destroy_parent "Metal.Acceleration_structure.Build.destroy" value.lifetime value.raw
        (fun () -> detach value.device.lifetime)
  end

  let create ~(device : device) ~size =
    let operation = "Metal.Acceleration_structure.create" in
    on_main operation (fun () ->
        if size <= 0L then error operation Invalid_argument "size must be positive"
        else
          let* () = ensure_live operation device.lifetime in
          let* raw =
            native_result operation (Metal_raw.acceleration_structure_create device.raw size)
          in
          let value =
            { raw; lifetime = lifetime (); device; size; heap = None; allocation = None }
          in
          attach device.lifetime;
          attach_finalizer value value.lifetime device.lifetime;
          Ok value)

  let destroy (value : t) =
    destroy_parent "Metal.Acceleration_structure.destroy" value.lifetime value.raw (fun () ->
        match value.heap with
        | None -> detach value.device.lifetime
        | Some heap ->
            deactivate_allocation value.allocation;
            detach heap.lifetime)
end

module Texture = struct
  type t = texture

  type kind = texture_kind =
    | Texture_1d
    | Texture_1d_array
    | Texture_2d
    | Texture_2d_array
    | Texture_2d_multisample
    | Texture_cube
    | Texture_cube_array
    | Texture_3d
    | Texture_2d_multisample_array
    | Texture_buffer

  type format = Metal_format.t =
    | R8_unorm
    | Rgba8_unorm
    | Bgra8_unorm
    | Bgra8_unorm_srgb
    | Rgba16_float
    | Rgba32_float
    | Depth32_float

  type format_layout = Metal_format.layout = {
    block_width : int;
    block_height : int;
    bytes_per_block : int;
  }

  type cpu_cache_mode = resource_cpu_cache_mode = Default_cache | Write_combined

  type hazard_tracking_mode = resource_hazard_tracking_mode =
    | Default_hazard_tracking
    | Untracked
    | Tracked

  type usage = texture_usage =
    | Shader_read
    | Shader_write
    | Render_target
    | Pixel_format_view
    | Shader_atomic

  type compression_type = texture_compression_type = Lossless | Lossy
  type swizzle_channel = texture_swizzle_channel = Zero | One | Red | Green | Blue | Alpha

  type swizzle = texture_swizzle = {
    red : swizzle_channel;
    green : swizzle_channel;
    blue : swizzle_channel;
    alpha : swizzle_channel;
  }

  type descriptor = texture_descriptor = {
    kind : kind;
    format : format;
    width : int;
    height : int;
    depth : int;
    mip_levels : int;
    sample_count : int;
    array_length : int;
    storage : Buffer.storage_mode;
    cpu_cache : cpu_cache_mode;
    hazard_tracking : hazard_tracking_mode;
    usage : usage list;
    allow_gpu_optimized_contents : bool;
    compression : compression_type;
    swizzle : swizzle;
    label : string option;
  }

  let compression_code = function Lossless -> 0 | Lossy -> 1

  let swizzle_channel_code = function
    | Zero -> 0
    | One -> 1
    | Red -> 2
    | Green -> 3
    | Blue -> 4
    | Alpha -> 5

  let swizzle_codes (value : swizzle) =
    ( swizzle_channel_code value.red,
      swizzle_channel_code value.green,
      swizzle_channel_code value.blue,
      swizzle_channel_code value.alpha )

  let default_swizzle = { red = Red; green = Green; blue = Blue; alpha = Alpha }

  let has_writable_usage =
    List.exists (function
      | Shader_write | Shader_atomic -> true
      | Shader_read | Render_target | Pixel_format_view -> false)

  let has_lossy_incompatible_usage =
    List.exists (function
      | Pixel_format_view | Shader_write | Shader_atomic -> true
      | Shader_read | Render_target -> false)

  type region = { x : int; y : int; z : int; width : int; height : int; depth : int }

  let descriptor_2d ?(storage = Private) ?(usage = [ Shader_read ]) ?label ~format ~width ~height ()
      =
    let compression = Lossless in
    let mipmapped = false in
    let swizzle = default_swizzle in
    let max_dimension = max width height in
    let rec mip_count dimension count =
      if dimension <= 1 then count else mip_count (dimension / 2) (count + 1)
    in
    {
      kind = Texture_2d;
      format;
      width;
      height;
      depth = 1;
      mip_levels = (if mipmapped then mip_count max_dimension 1 else 1);
      sample_count = 1;
      array_length = 1;
      storage;
      cpu_cache = Default_cache;
      hazard_tracking = Default_hazard_tracking;
      usage;
      allow_gpu_optimized_contents = true;
      compression;
      swizzle;
      label;
    }

  let kind_code = function
    | Texture_1d -> 0
    | Texture_1d_array -> 1
    | Texture_2d -> 2
    | Texture_2d_array -> 3
    | Texture_2d_multisample -> 4
    | Texture_cube -> 5
    | Texture_cube_array -> 6
    | Texture_3d -> 7
    | Texture_2d_multisample_array -> 8
    | Texture_buffer -> 9

  let format_code = Metal_format.code
  let format_layout = Metal_format.layout

  let usage_bit = function
    | Shader_read -> 0x1
    | Shader_write -> 0x2
    | Render_target -> 0x4
    | Pixel_format_view -> 0x10
    | Shader_atomic -> 0x20

  let usage_bits usages = List.fold_left (fun bits usage -> bits lor usage_bit usage) 0 usages

  let max_mip_levels (descriptor : descriptor) =
    let largest = max descriptor.width (max descriptor.height descriptor.depth) in
    let rec count dimension levels =
      if dimension <= 1 then levels else count (dimension / 2) (levels + 1)
    in
    count largest 1

  let is_array = function
    | Texture_1d_array | Texture_2d_array | Texture_cube_array | Texture_2d_multisample_array ->
        true
    | Texture_1d | Texture_2d | Texture_2d_multisample | Texture_cube | Texture_3d | Texture_buffer
      ->
        false

  let is_multisample = function
    | Texture_2d_multisample | Texture_2d_multisample_array -> true
    | Texture_1d | Texture_1d_array | Texture_2d | Texture_2d_array | Texture_cube
    | Texture_cube_array | Texture_3d | Texture_buffer ->
        false

  let validate_descriptor operation (device : Device.t) (descriptor : descriptor) =
    let invalid message = error operation Invalid_argument message in
    if
      descriptor.width <= 0 || descriptor.height <= 0 || descriptor.depth <= 0
      || descriptor.mip_levels <= 0 || descriptor.sample_count <= 0 || descriptor.array_length <= 0
    then invalid "texture dimensions and counts must be positive"
    else if
      (descriptor.kind = Texture_buffer && descriptor.width > 268_435_456)
      || descriptor.kind <> Texture_buffer
         && (descriptor.width > 16_384 || descriptor.height > 16_384 || descriptor.depth > 2_048)
    then invalid "texture dimensions exceed the binding's checked Metal limits"
    else if descriptor.mip_levels > max_mip_levels descriptor then
      invalid "texture mip count exceeds its dimensions"
    else if descriptor.array_length >= 2_048 then invalid "texture array length must be below 2048"
    else if List.length descriptor.usage <> List.length (List.sort_uniq compare descriptor.usage)
    then invalid "texture usage contains duplicates"
    else if descriptor.swizzle <> default_swizzle && has_writable_usage descriptor.usage then
      invalid "texture swizzling is incompatible with shader-write and shader-atomic usage"
    else if descriptor.compression = Lossy && descriptor.storage <> Private then
      invalid "lossy texture compression requires private storage"
    else if descriptor.compression = Lossy && not descriptor.allow_gpu_optimized_contents then
      invalid "lossy texture compression requires GPU-optimized contents"
    else if descriptor.compression = Lossy && has_lossy_incompatible_usage descriptor.usage then
      invalid
        "lossy texture compression is incompatible with pixel-format views, shader writes, and \
         shader atomics"
    else if
      descriptor.compression = Lossy
      &&
      match descriptor.kind with
      | Texture_1d | Texture_1d_array | Texture_buffer -> true
      | Texture_2d | Texture_2d_array | Texture_2d_multisample | Texture_cube | Texture_cube_array
      | Texture_3d | Texture_2d_multisample_array ->
          false
    then invalid "lossy texture compression is incompatible with 1D and buffer textures"
    else if descriptor.compression = Lossy then
      invalid "pixel format does not support lossy texture compression"
    else
      match descriptor.label with
      | Some label when contains_nul label -> invalid "texture label contains a NUL byte"
      | _ -> (
          let structural_error =
            match descriptor.kind with
            | (Texture_1d | Texture_buffer) when descriptor.height <> 1 || descriptor.depth <> 1 ->
                Some "1D and buffer textures require height=depth=1"
            | Texture_1d_array when descriptor.height <> 1 || descriptor.depth <> 1 ->
                Some "1D-array textures require height=depth=1"
            | Texture_2d | Texture_2d_array | Texture_2d_multisample | Texture_2d_multisample_array
            | Texture_cube | Texture_cube_array
              when descriptor.depth <> 1 ->
                Some "2D, cube, and multisample textures require depth=1"
            | (Texture_cube | Texture_cube_array) when descriptor.width <> descriptor.height ->
                Some "cube textures must be square"
            | Texture_3d when descriptor.array_length <> 1 -> Some "3D textures cannot be arrays"
            | _ -> None
          in
          match structural_error with
          | Some message -> invalid message
          | None when is_array descriptor.kind && descriptor.array_length < 2 ->
              invalid "array textures require at least two elements"
          | None when (not (is_array descriptor.kind)) && descriptor.array_length <> 1 ->
              invalid "non-array textures require array_length=1"
          | None when is_multisample descriptor.kind && descriptor.mip_levels <> 1 ->
              invalid "multisample textures require exactly one mip level"
          | None when is_multisample descriptor.kind && descriptor.sample_count = 1 ->
              invalid "multisample textures require more than one sample"
          | None when (not (is_multisample descriptor.kind)) && descriptor.sample_count <> 1 ->
              invalid "non-multisample textures require sample_count=1"
          | None when descriptor.kind = Texture_buffer && descriptor.mip_levels <> 1 ->
              invalid "buffer textures require exactly one mip level"
          | None -> (
              if descriptor.sample_count = 1 then Ok ()
              else
                match Device.supports_texture_sample_count device descriptor.sample_count with
                | Error _ as failure -> failure
                | Ok true -> Ok ()
                | Ok false -> invalid "device does not support the texture sample count"))

  let raw_descriptor (descriptor : descriptor) =
    let swizzle_red, swizzle_green, swizzle_blue, swizzle_alpha =
      swizzle_codes descriptor.swizzle
    in
    {
      Metal_raw.texture_type = kind_code descriptor.kind;
      pixel_format = format_code descriptor.format;
      width = descriptor.width;
      height = descriptor.height;
      depth = descriptor.depth;
      mip_levels = descriptor.mip_levels;
      sample_count = descriptor.sample_count;
      array_length = descriptor.array_length;
      storage_mode = storage_code descriptor.storage;
      cpu_cache_mode = cache_code descriptor.cpu_cache;
      hazard_tracking_mode = hazard_code descriptor.hazard_tracking;
      usage = usage_bits descriptor.usage;
      allow_gpu_optimized_contents = descriptor.allow_gpu_optimized_contents;
      compression_type = compression_code descriptor.compression;
      swizzle_red;
      swizzle_green;
      swizzle_blue;
      swizzle_alpha;
    }

  let verify_info operation raw descriptor ~heap =
    let info = Metal_raw.texture_info raw in
    let actual_hazard = concrete_hazard_tracking ~heap descriptor.hazard_tracking in
    if Array.length info <> 18 then
      native_error operation "Metal returned malformed texture properties"
    else
      let swizzle_red, swizzle_green, swizzle_blue, swizzle_alpha =
        swizzle_codes descriptor.swizzle
      in
      let expected =
        [|
          kind_code descriptor.kind;
          format_code descriptor.format;
          descriptor.width;
          descriptor.height;
          descriptor.depth;
          descriptor.mip_levels;
          descriptor.sample_count;
          descriptor.array_length;
          usage_bits descriptor.usage;
          storage_code descriptor.storage;
          cache_code descriptor.cpu_cache;
          hazard_code actual_hazard;
          (if descriptor.allow_gpu_optimized_contents then 1 else 0);
          compression_code descriptor.compression;
          swizzle_red;
          swizzle_green;
          swizzle_blue;
          swizzle_alpha;
        |]
      in
      if info = expected then Ok { descriptor with hazard_tracking = actual_hazard }
      else begin
        let fields =
          [|
            "texture type";
            "pixel format";
            "width";
            "height";
            "depth";
            "mip levels";
            "sample count";
            "array length";
            "usage";
            "storage mode";
            "CPU cache mode";
            "hazard tracking mode";
            "GPU-optimized contents";
            "compression type";
            "red swizzle";
            "green swizzle";
            "blue swizzle";
            "alpha swizzle";
          |]
        in
        let rec mismatch index =
          if index = Array.length expected then None
          else if info.(index) <> expected.(index) then Some index
          else mismatch (index + 1)
        in
        match mismatch 0 with
        | None ->
            native_error operation "Metal changed a checked texture descriptor during creation"
        | Some index ->
            native_error operation
              (Printf.sprintf "Metal changed texture %s from %d to %d" fields.(index)
                 expected.(index) info.(index))
      end

  let finish_create ?expected_shareable operation ~device ~descriptor ~parent ~heap_offset:_
      ~allocation raw =
    let heap =
      match parent with
      | Texture_resource (Device_resource _) | Texture_drawable_resource _ -> false
    in
    match verify_info operation raw descriptor ~heap with
    | Error _ as failure ->
        ignore (Metal_raw.destroy raw);
        failure
    | Ok _
      when option_exists
             (fun expected -> expected <> Metal_raw.texture_is_shareable raw)
             expected_shareable ->
        ignore (Metal_raw.destroy raw);
        native_error operation "Metal changed the checked texture sharing mode during creation"
    | Ok descriptor ->
        let parent_lifetime = texture_parent_lifetime parent in
        let state =
          match parent with
          | Texture_resource _ -> resource_state ()
          | Texture_drawable_resource _ -> resource_state ()
        in
        let value : t =
          { raw; lifetime = lifetime (); device; descriptor; parent; allocation; state }
        in
        attach parent_lifetime;
        let extra_device = texture_parent_extra_device device parent in
        Option.iter attach extra_device;
        attach_finalizer
          ~on_finalize:(fun () ->
            deactivate_allocation allocation;
            Option.iter detach extra_device)
          value value.lifetime parent_lifetime;
        Ok value

  let create ~(device : Device.t) descriptor =
    on_main "Metal.Texture.create" (fun () ->
        let* () = ensure_live "Metal.Texture.create" device.lifetime in
        let* () = validate_descriptor "Metal.Texture.create" device descriptor in
        if descriptor.kind = Texture_buffer then
          error "Metal.Texture.create" Invalid_argument
            "texture-buffer resources must be created from a buffer"
        else
          let* raw =
            native_result "Metal.Texture.create"
              (Metal_raw.texture_create device.raw (raw_descriptor descriptor) descriptor.label)
          in
          finish_create ~expected_shareable:false "Metal.Texture.create" ~device ~descriptor
            ~parent:(Texture_resource (Device_resource device)) ~heap_offset:None ~allocation:None
            raw)

  let device (value : t) = value.device
  let descriptor (value : t) = value.descriptor
  let destroyed (value : t) = is_destroyed value.lifetime
  let mip_dimension dimension level = max 1 (dimension lsr level)

  let total_slices (descriptor : descriptor) =
    match descriptor.kind with
    | Texture_1d_array | Texture_2d_array | Texture_2d_multisample_array -> descriptor.array_length
    | Texture_cube -> 6
    | Texture_cube_array -> descriptor.array_length * 6
    | Texture_1d | Texture_2d | Texture_2d_multisample | Texture_3d | Texture_buffer -> 1

  let checked_mul left right =
    if left = 0 || right = 0 then Some 0
    else if left > max_int / right then None
    else Some (left * right)

  let validate_transfer operation (value : t) ~region ~mip_level ~slice ~bytes_per_row
      ~bytes_per_image =
    let invalid message = error operation Invalid_argument message in
    if value.descriptor.storage = Private then
      error operation Unsupported "private textures have no CPU transfer mapping"
    else if is_multisample value.descriptor.kind then
      error operation Unsupported "multisample textures do not support CPU transfer"
    else if mip_level < 0 || mip_level >= value.descriptor.mip_levels then
      invalid "mip level is outside the texture"
    else if slice < 0 || slice >= total_slices value.descriptor then
      invalid "slice is outside the texture"
    else if
      region.x < 0 || region.y < 0 || region.z < 0 || region.width <= 0 || region.height <= 0
      || region.depth <= 0
    then invalid "texture region coordinates and dimensions are invalid"
    else
      let width = mip_dimension value.descriptor.width mip_level in
      let height = mip_dimension value.descriptor.height mip_level in
      let depth = mip_dimension value.descriptor.depth mip_level in
      if
        region.x > width
        || region.width > width - region.x
        || region.y > height
        || region.height > height - region.y
        || region.z > depth
        || region.depth > depth - region.z
      then invalid "texture region exceeds the selected mip level"
      else
        let layout = format_layout value.descriptor.format in
        let aligned origin length limit block =
          origin mod block = 0 && (length mod block = 0 || origin + length = limit)
        in
        if
          not
            (aligned region.x region.width width layout.block_width
            && aligned region.y region.height height layout.block_height)
        then invalid "texture region is not aligned to its format blocks"
        else
          let blocks value block = 1 + ((value - 1) / block) in
          match checked_mul (blocks region.width layout.block_width) layout.bytes_per_block with
          | None -> invalid "texture row cardinality overflows an OCaml integer"
          | Some minimum_row
            when bytes_per_row < minimum_row || bytes_per_row mod layout.bytes_per_block <> 0 ->
              invalid "texture row pitch is too small or not block-aligned"
          | Some _ -> (
              match checked_mul bytes_per_row (blocks region.height layout.block_height) with
              | None -> invalid "texture image cardinality overflows an OCaml integer"
              | Some minimum_image when bytes_per_image < minimum_image ->
                  invalid "texture image pitch is smaller than its rows"
              | Some _ -> (
                  match checked_mul bytes_per_image region.depth with
                  | None -> invalid "texture transfer cardinality overflows an OCaml integer"
                  | Some total when total > Sys.max_string_length ->
                      invalid "texture transfer exceeds the maximum OCaml byte buffer"
                  | Some total -> Ok total))

  let transfer_tuple region ~mip_level ~slice ~source_offset ~bytes_per_row ~bytes_per_image =
    ( (region.x, region.y, region.z, region.width, region.height, region.depth),
      mip_level,
      slice,
      source_offset,
      bytes_per_row,
      bytes_per_image )

  let read_bytes (value : t) ~region ~mip_level ~slice ~bytes_per_row ~bytes_per_image =
    on_main "Metal.Texture.read_bytes" (fun () ->
        let* () = ensure_texture_usable "Metal.Texture.read_bytes" value in
        let* _ =
          validate_transfer "Metal.Texture.read_bytes" value ~region ~mip_level ~slice
            ~bytes_per_row ~bytes_per_image
        in
        let* bytes =
          native_result "Metal.Texture.read_bytes"
            (Metal_raw.texture_read value.raw
               (transfer_tuple region ~mip_level ~slice ~source_offset:0 ~bytes_per_row
                  ~bytes_per_image))
        in
        Ok bytes)

  let read_bytes_into (value : t) ~region ~mip_level ~slice ~bytes_per_row ~bytes_per_image
      ~destination =
    on_main "Metal.Texture.read_bytes_into" (fun () ->
        let* () = ensure_texture_usable "Metal.Texture.read_bytes_into" value in
        let* total =
          validate_transfer "Metal.Texture.read_bytes_into" value ~region ~mip_level ~slice
            ~bytes_per_row ~bytes_per_image
        in
        if Bytes.length destination <> total then
          error "Metal.Texture.read_bytes_into" Invalid_argument
            "destination length does not match the complete pitched region"
        else
          let* () =
            native_result "Metal.Texture.read_bytes_into"
              (Metal_raw.texture_read_into value.raw
                 (transfer_tuple region ~mip_level ~slice ~source_offset:0 ~bytes_per_row
                    ~bytes_per_image)
                 destination)
          in
          Ok ())

  let destroy (value : t) =
    destroy_parent "Metal.Texture.destroy" value.lifetime value.raw (fun () ->
        deactivate_allocation value.allocation;
        detach (texture_parent_lifetime value.parent);
        Option.iter detach (texture_parent_extra_device value.device value.parent))
end

module Metal_layer = struct
  type t = metal_layer

  type config = {
    width : int;
    height : int;
    format : Texture.format;
    framebuffer_only : bool;
    maximum_drawables : int;
    allows_timeout : bool;
    display_sync : bool;
    presents_with_transaction : bool;
  }

  let default ~width ~height =
    {
      width;
      height;
      format = Texture.Bgra8_unorm;
      framebuffer_only = false;
      maximum_drawables = 3;
      allows_timeout = true;
      display_sync = false;
      presents_with_transaction = false;
    }

  let adopt_borrowed (device : Device.t) token config =
    let operation = "Metal.Metal_layer.adopt_borrowed" in
    on_main operation (fun () ->
        let* () = ensure_live operation device.lifetime in
        if not (Native_layer_token.alive token) then
          error operation Destroyed "native layer token is stale"
        else if
          config.width <= 0 || config.height <= 0 || config.maximum_drawables < 2
          || config.maximum_drawables > 3
        then error operation Invalid_argument "invalid drawable size or maximum count"
        else
          let* raw =
            native_result operation
              (Metal_raw.layer_adopt_borrowed device.raw token
                 (Native_layer_token.owner_id token)
                 (Native_layer_token.generation token))
          in
          match
            Metal_raw.layer_configure raw config.width config.height
              (Metal_format.code config.format)
              ( config.framebuffer_only,
                config.maximum_drawables,
                config.allows_timeout,
                config.display_sync,
                config.presents_with_transaction )
          with
          | Error m ->
              ignore (Metal_raw.destroy raw);
              native_error operation m
          | Ok () ->
              let value : t = { raw; lifetime = lifetime (); device; drawable_descriptor = None } in
              attach device.lifetime;
              attach_finalizer value value.lifetime device.lifetime;
              Ok value)

  let device (value : t) = value.device

  let configure (value : t) config =
    let operation = "Metal.Metal_layer.configure" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        if
          config.width <= 0 || config.height <= 0 || config.maximum_drawables < 2
          || config.maximum_drawables > 3
        then error operation Invalid_argument "invalid drawable size or maximum count"
        else if
          not
            (List.mem config.format
               [ Texture.Bgra8_unorm; Texture.Bgra8_unorm_srgb; Texture.Rgba16_float ])
        then error operation Unsupported "pixel format is not supported by CAMetalLayer"
        else
          let* () =
            native_result operation
              (Metal_raw.layer_configure value.raw config.width config.height
                 (Metal_format.code config.format)
                 ( config.framebuffer_only,
                   config.maximum_drawables,
                   config.allows_timeout,
                   config.display_sync,
                   config.presents_with_transaction ))
          in
          ();
          ();
          ();
          ();
          ();
          ();
          ();
          ();
          value.drawable_descriptor <- None;
          Ok ())

  let destroy (value : t) =
    destroy_parent "Metal.Metal_layer.destroy" value.lifetime value.raw (fun () ->
        detach value.device.lifetime)
end

module Drawable = struct
  type t = metal_drawable
  type loss = Timeout_or_unavailable

  let acquire_owned ~finalize (layer : metal_layer) =
    let operation = "Metal.Drawable.acquire" in
    let* () = before_main operation in
    let* () = ensure_live operation layer.lifetime in
    match Metal_raw.layer_next_drawable layer.raw with
    | Error message -> native_error operation message
    | Ok None -> Ok (Error Timeout_or_unavailable)
    | Ok (Some raw) -> (
        match Metal_raw.drawable10_snapshot raw with
        | Error message ->
            ignore (Metal_raw.destroy raw);
            native_error operation message
        | Ok (_drawable_id, _) ->
            let value : t =
              {
                raw;
                lifetime = lifetime ();
                layer;
                drawable_texture = None;
                presentation_scheduled = false;
              }
            in
            attach layer.lifetime;
            if finalize then attach_finalizer value value.lifetime layer.lifetime;
            Ok (Ok value))

  let acquire layer = acquire_owned ~finalize:true layer

  let texture_owned ~finalize (value : t) =
    let operation = "Metal.Drawable.texture" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    match value.drawable_texture with
    | Some texture -> Ok texture
    | None -> (
        let* raw, width, height, format_code =
          native_result operation (Metal_raw.drawable_texture value.raw)
        in
        match
          match format_code with
          | 80 -> Some Texture.Bgra8_unorm
          | 81 -> Some Texture.Bgra8_unorm_srgb
          | 115 -> Some Texture.Rgba16_float
          | _ -> None
        with
        | None ->
            ignore (Metal_raw.destroy raw);
            error operation Unsupported "drawable returned an unsupported pixel format"
        | Some format ->
            let descriptor =
              match value.layer.drawable_descriptor with
              | Some descriptor
                when descriptor.width = width && descriptor.height = height
                     && descriptor.format = format ->
                  descriptor
              | _ ->
                  let descriptor =
                    Texture.descriptor_2d ~storage:Buffer.Private ~usage:[ Texture.Render_target ]
                      ~format ~width ~height ()
                  in
                  value.layer.drawable_descriptor <- Some descriptor;
                  descriptor
            in
            let texture : texture =
              {
                raw;
                lifetime = lifetime ();
                device = value.layer.device;
                descriptor;
                parent = Texture_drawable_resource value;
                allocation = None;
                state = { relinquished = Atomic.make false; purgeable = Atomic.make Nonvolatile };
              }
            in
            attach value.lifetime;
            if finalize then attach_finalizer texture texture.lifetime value.lifetime;
            value.drawable_texture <- Some texture;
            Ok texture)

  let texture value = texture_owned ~finalize:true value
  let acquire_scoped layer = acquire_owned ~finalize:false layer
  let texture_scoped value = texture_owned ~finalize:false value

  let destroy (value : t) =
    destroy_parent "Metal.Drawable.destroy" value.lifetime value.raw (fun () ->
        detach value.layer.lifetime)
end

module Render_pass_descriptor = struct
  type color_load_action = Load_dont_care | Load | Clear
  type t = render_pass_descriptor

  let create ~width ~height ?(sample_count = 1) () =
    let array_length = 1 in
    let operation = "Metal.Render_pass_descriptor.create" in
    on_main operation (fun () ->
        if width <= 0 || height <= 0 || array_length <= 0 || sample_count <= 0 then
          error operation Invalid_argument "render pass sizes must be positive"
        else
          let* raw = native_result operation (Metal_raw.render_pass_descriptor_create ()) in
          match
            Metal_raw.render_pass_descriptor_set_sizes raw width height array_length sample_count
          with
          | Error message ->
              ignore (Metal_raw.destroy raw);
              native_error operation message
          | Ok () ->
              Ok
                {
                  raw;
                  lifetime = lifetime ();
                  pass_width = width;
                  pass_height = height;
                  pass_array_length = array_length;
                  pass_sample_count = sample_count;
                  pass_color = None;
                  pass_depth = None;
                  pass_stencil = None;
                  pass_visibility = None;
                  pass_resolve = None;
                  pass_samples = Array.make 4 None;
                })

  let set_resolve_texture (value : t) (next : texture option) =
    let operation = "Metal.Render_pass_descriptor.set_resolve_texture" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        match next with
        | Some texture when is_destroyed texture.lifetime ->
            error operation Destroyed "resolve texture is destroyed"
        | Some _ when value.pass_sample_count <= 1 ->
            error operation Invalid_argument "resolve texture requires a multisample render pass"
        | Some texture
          when texture.descriptor.width <> value.pass_width
               || texture.descriptor.height <> value.pass_height
               || texture.descriptor.sample_count <> 1
               || not (List.mem Render_target texture.descriptor.usage) ->
            error operation Invalid_argument
              "resolve texture dimensions, samples, or usage are incompatible"
        | Some texture
          when option_exists
                 (fun (color : texture) -> not (same_device color.device texture.device))
                 value.pass_color ->
            error operation Device_mismatch "resolve texture belongs to another device"
        | _ ->
            let* actual =
              native_result operation
                (Metal_raw.render_pass_resolve_texture value.raw
                   (Option.map (fun (texture : texture) -> texture.raw) next)
                   true)
            in
            Option.iter (fun raw -> ignore (Metal_raw.destroy raw)) actual;
            if Option.is_some actual <> Option.is_some next then
              error operation Native_error "resolve texture round-trip changed nullability"
            else begin
              Option.iter (fun (texture : texture) -> attach texture.lifetime) next;
              Option.iter (fun (texture : texture) -> detach texture.lifetime) value.pass_resolve;
              value.pass_resolve <- next;
              Ok ()
            end)

  let set_color_store_action (value : t) ~store ~resolve =
    let operation = "Metal.Render_pass_descriptor.set_color_store_action" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        (* MTLStoreAction: DontCare, Store, MultisampleResolve, StoreAndMultisampleResolve *)
        let code =
          match (store, resolve) with
          | false, false -> 0
          | true, false -> 1
          | false, true -> 2
          | true, true -> 3
        in
        if resolve && value.pass_sample_count = 1 then
          error operation Invalid_argument "resolve store actions require multisampling"
        else
          let* () =
            native_result operation (Metal_raw.render_pass_color_store_action value.raw code)
          in
          Ok ())

  type store_action = Store_dont_care | Store

  let set_depth_stencil_actions (value : t) ~depth:(depth_load, depth_store, clear_depth)
      ~stencil:(stencil_load, stencil_store, clear_stencil) =
    let operation = "Metal.Render_pass_descriptor.set_depth_stencil_actions" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        if
          (not (Float.is_finite clear_depth))
          || clear_depth < 0. || clear_depth > 1. || clear_stencil < 0 || clear_stencil > 255
        then
          error operation Invalid_argument
            "depth clear must be in [0,1] and stencil clear in [0,255]"
        else
          let load = function Load_dont_care -> 0 | Load -> 1 | Clear -> 2
          and store = function Store_dont_care -> 0 | Store -> 1 in
          let* () =
            native_result operation
              (Metal_raw.render_pass_depth_stencil_actions value.raw (load depth_load)
                 (store depth_store) clear_depth (load stencil_load) (store stencil_store)
                 clear_stencil)
          in
          Ok ())

  let set_color_load_action (value : t) action =
    let operation = "Metal.Render_pass_descriptor.set_color_load_action" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let code = match action with Load_dont_care -> 0 | Load -> 1 | Clear -> 2 in
        let* () =
          native_result operation (Metal_raw.render_pass_color_load_action value.raw code)
        in
        Ok ())

  let detach_option get = Option.iter (fun value -> detach (get value))
  let depth_capable = function Texture.Depth32_float -> true | _ -> false
  let stencil_capable _ = false

  let set_attachments (value : t) ~(color : texture) ?(clear = (0., 0., 0., 1.))
      ?(depth : texture option) ?(stencil : texture option) () =
    let visibility_result : buffer option = None in
    let operation = "Metal.Render_pass_descriptor.set_attachments" in
    on_main operation (fun () ->
        let textures = color :: List.filter_map Fun.id [ depth; stencil ] in
        let* () = ensure_live operation value.lifetime in
        if value.pass_array_length <> 1 then
          error operation Unsupported
            "classic attachment configuration supports one render-target slice"
        else
          match
            List.find_opt (fun (texture : texture) -> is_destroyed texture.lifetime) textures
          with
          | Some _ -> error operation Destroyed "render pass attachment is destroyed"
          | None -> (
              let device = color.device in
              if
                List.exists
                  (fun (texture : texture) -> not (same_device device texture.device))
                  textures
              then
                error operation Device_mismatch
                  "render pass attachments belong to different devices"
              else if
                option_exists
                  (fun (texture : texture) -> not (same_device device texture.device))
                  value.pass_resolve
                || Array.exists
                     (function
                       | None -> false
                       | Some (state : render_pass_sample_state) ->
                           not (same_device device state.sample_buffer.device))
                     value.pass_samples
              then
                error operation Device_mismatch
                  "retained resolve/counter attachments belong to another device"
              else
                match visibility_result with
                | Some buffer when is_destroyed buffer.lifetime ->
                    error operation Destroyed "visibility buffer is destroyed"
                | Some buffer when not (same_device device buffer.device) ->
                    error operation Device_mismatch "visibility buffer belongs to another device"
                | Some buffer when buffer.length < 8L ->
                    error operation Invalid_argument
                      "visibility buffer must contain at least eight bytes"
                | _ ->
                    let compatible (texture : texture) =
                      texture.descriptor.width = value.pass_width
                      && texture.descriptor.height = value.pass_height
                      && texture.descriptor.sample_count = value.pass_sample_count
                      && List.mem Render_target texture.descriptor.usage
                    in
                    let r, g, b, a = clear in
                    if not (compatible color) then
                      error operation Invalid_argument
                        "color attachment dimensions, samples, or usage are incompatible"
                    else if
                      depth_capable color.descriptor.format
                      || stencil_capable color.descriptor.format
                    then
                      error operation Invalid_argument
                        "color attachment uses a depth/stencil pixel format"
                    else if
                      option_exists
                        (fun texture ->
                          (not (compatible texture))
                          || not (depth_capable texture.descriptor.format))
                        depth
                    then
                      error operation Invalid_argument
                        "depth attachment format or geometry is incompatible"
                    else if
                      option_exists
                        (fun texture ->
                          (not (compatible texture))
                          || not (stencil_capable texture.descriptor.format))
                        stencil
                    then
                      error operation Invalid_argument
                        "stencil attachment format or geometry is incompatible"
                    else if not (List.for_all Float.is_finite [ r; g; b; a ]) then
                      error operation Invalid_argument "clear color must be finite"
                    else
                      let* () =
                        native_result operation
                          (Metal_raw.render_pass_descriptor_set_attachments value.raw color.raw
                             (Option.map (fun (texture : texture) -> texture.raw) depth)
                             (Option.map (fun (texture : texture) -> texture.raw) stencil)
                             (Option.map (fun (buffer : buffer) -> buffer.raw) visibility_result)
                             clear)
                      in
                      Option.iter (fun (texture : texture) -> attach texture.lifetime) (Some color);
                      Option.iter (fun (texture : texture) -> attach texture.lifetime) depth;
                      Option.iter (fun (texture : texture) -> attach texture.lifetime) stencil;
                      Option.iter
                        (fun (buffer : buffer) -> attach buffer.lifetime)
                        visibility_result;
                      detach_option (fun (texture : texture) -> texture.lifetime) value.pass_color;
                      detach_option (fun (texture : texture) -> texture.lifetime) value.pass_depth;
                      detach_option (fun (texture : texture) -> texture.lifetime) value.pass_stencil;
                      detach_option (fun (buffer : buffer) -> buffer.lifetime) value.pass_visibility;
                      value.pass_color <- Some color;
                      value.pass_depth <- depth;
                      value.pass_stencil <- stencil;
                      value.pass_visibility <- visibility_result;
                      Ok ()))

  let destroy (value : t) =
    destroy_parent "Metal.Render_pass_descriptor.destroy" value.lifetime value.raw (fun () ->
        detach_option (fun (texture : texture) -> texture.lifetime) value.pass_color;
        detach_option (fun (texture : texture) -> texture.lifetime) value.pass_depth;
        detach_option (fun (texture : texture) -> texture.lifetime) value.pass_stencil;
        detach_option (fun (buffer : buffer) -> buffer.lifetime) value.pass_visibility;
        detach_option (fun (texture : texture) -> texture.lifetime) value.pass_resolve;
        Array.iter
          (Option.iter (fun (state : render_pass_sample_state) ->
               detach state.sample_buffer.lifetime))
          value.pass_samples)
end

module Sampler = struct
  type t = sampler
  type filter = sampler_filter = Nearest | Linear
  type mip_filter = sampler_mip_filter = Not_mipmapped | Mip_nearest | Mip_linear

  type address_mode = sampler_address_mode =
    | Clamp_to_edge
    | Mirror_clamp_to_edge
    | Repeat
    | Mirror_repeat
    | Clamp_to_zero
    | Clamp_to_border_color

  type border_color = sampler_border_color = Transparent_black | Opaque_black | Opaque_white
  type reduction_mode = sampler_reduction_mode = Weighted_average | Minimum | Maximum

  type compare_function = sampler_compare_function =
    | Never
    | Less
    | Equal
    | Less_equal
    | Greater
    | Not_equal
    | Greater_equal
    | Always

  type descriptor = sampler_descriptor = {
    min_filter : filter;
    mag_filter : filter;
    mip_filter : mip_filter;
    max_anisotropy : int;
    s_address : address_mode;
    t_address : address_mode;
    r_address : address_mode;
    border_color : border_color;
    reduction_mode : reduction_mode;
    normalized_coordinates : bool;
    lod_min_clamp : float;
    lod_max_clamp : float;
    lod_average : bool;
    lod_bias : float;
    compare_function : compare_function;
    support_argument_buffers : bool;
    label : string option;
  }

  let default ?label () =
    {
      min_filter = Nearest;
      mag_filter = Nearest;
      mip_filter = Not_mipmapped;
      max_anisotropy = 1;
      s_address = Clamp_to_edge;
      t_address = Clamp_to_edge;
      r_address = Clamp_to_edge;
      border_color = Transparent_black;
      reduction_mode = Weighted_average;
      normalized_coordinates = true;
      lod_min_clamp = 0.;
      lod_max_clamp = 3.402823466e38;
      lod_average = false;
      lod_bias = 0.;
      compare_function = Never;
      support_argument_buffers = false;
      label;
    }

  let filter_code = function Nearest -> 0 | Linear -> 1
  let mip_code = function Not_mipmapped -> 0 | Mip_nearest -> 1 | Mip_linear -> 2

  let address_code = function
    | Clamp_to_edge -> 0
    | Mirror_clamp_to_edge -> 1
    | Repeat -> 2
    | Mirror_repeat -> 3
    | Clamp_to_zero -> 4
    | Clamp_to_border_color -> 5

  let border_code = function Transparent_black -> 0 | Opaque_black -> 1 | Opaque_white -> 2
  let reduction_code = function Weighted_average -> 0 | Minimum -> 1 | Maximum -> 2

  let compare_code = function
    | Never -> 0
    | Less -> 1
    | Equal -> 2
    | Less_equal -> 3
    | Greater -> 4
    | Not_equal -> 5
    | Greater_equal -> 6
    | Always -> 7

  let validate descriptor =
    let invalid message = error "Metal.Sampler.create" Invalid_argument message in
    if descriptor.max_anisotropy < 1 || descriptor.max_anisotropy > 16 then
      invalid "sampler anisotropy must be in [1, 16]"
    else if
      (not (Float.is_finite descriptor.lod_min_clamp))
      || (not (Float.is_finite descriptor.lod_max_clamp))
      || descriptor.lod_min_clamp < 0.
      || descriptor.lod_max_clamp < descriptor.lod_min_clamp
      || descriptor.lod_max_clamp > 3.402823466e38
    then invalid "sampler LOD clamps are invalid or exceed float32 range"
    else if
      (not (Float.is_finite descriptor.lod_bias))
      || descriptor.lod_bias < -16. || descriptor.lod_bias > 15.999
    then invalid "sampler LOD bias must be finite and in [-16, 15.999]"
    else if option_exists contains_nul descriptor.label then
      invalid "sampler label contains a NUL byte"
    else if
      (not descriptor.normalized_coordinates)
      && (descriptor.s_address <> Clamp_to_edge
         || descriptor.t_address <> Clamp_to_edge
         || descriptor.r_address <> Clamp_to_edge
         || descriptor.mip_filter <> Not_mipmapped
         || descriptor.max_anisotropy <> 1)
    then invalid "unnormalized coordinates require clamp-to-edge, no mip filter, and anisotropy=1"
    else Ok ()

  let descriptor_tuple descriptor =
    ( filter_code descriptor.min_filter,
      filter_code descriptor.mag_filter,
      mip_code descriptor.mip_filter,
      descriptor.max_anisotropy,
      address_code descriptor.s_address,
      address_code descriptor.t_address,
      address_code descriptor.r_address,
      border_code descriptor.border_color,
      reduction_code descriptor.reduction_mode,
      descriptor.normalized_coordinates,
      descriptor.lod_min_clamp,
      descriptor.lod_max_clamp,
      descriptor.lod_average,
      descriptor.lod_bias,
      compare_code descriptor.compare_function,
      descriptor.support_argument_buffers )

  let requires_sampler_reduction descriptor =
    descriptor.reduction_mode <> Weighted_average || descriptor.lod_bias <> 0.

  let create ~(device : Device.t) descriptor =
    on_main "Metal.Sampler.create" (fun () ->
        let* () = ensure_live "Metal.Sampler.create" device.lifetime in
        let* () = validate descriptor in
        if
          requires_sampler_reduction descriptor
          && not (Metal_raw.device_supports_sampler_reduction device.raw)
        then
          error "Metal.Sampler.create" Unsupported
            "sampler reduction modes and LOD bias require macOS 26 and Apple GPU family 10"
        else
          let* raw =
            native_result "Metal.Sampler.create"
              (Metal_raw.sampler_create device.raw (descriptor_tuple descriptor) descriptor.label)
          in
          let value : t = { raw; lifetime = lifetime (); device } in
          attach device.lifetime;
          attach_finalizer value value.lifetime device.lifetime;
          Ok value)

  let destroy (value : t) =
    destroy_parent "Metal.Sampler.destroy" value.lifetime value.raw (fun () ->
        detach value.device.lifetime)
end

module Depth_stencil = struct
  type t = depth_stencil

  type compare_function = sampler_compare_function =
    | Never
    | Less
    | Equal
    | Less_equal
    | Greater
    | Not_equal
    | Greater_equal
    | Always

  type operation = stencil_operation =
    | Keep
    | Zero
    | Replace
    | Increment_clamp
    | Decrement_clamp
    | Invert
    | Increment_wrap
    | Decrement_wrap

  type face = stencil_face = {
    compare : compare_function;
    stencil_fail : operation;
    depth_fail : operation;
    pass : operation;
    read_mask : int32;
    write_mask : int32;
  }

  let operation_code = function
    | Keep -> 0
    | Zero -> 1
    | Replace -> 2
    | Increment_clamp -> 3
    | Decrement_clamp -> 4
    | Invert -> 5
    | Increment_wrap -> 6
    | Decrement_wrap -> 7

  let raw_face (value : face) =
    ({
       Metal_raw.compare_function = Sampler.compare_code value.compare;
       stencil_failure_operation = operation_code value.stencil_fail;
       depth_failure_operation = operation_code value.depth_fail;
       pass_operation = operation_code value.pass;
       read_mask = value.read_mask;
       write_mask = value.write_mask;
     }
      : Metal_raw.depth_stencil_face_descriptor)

  let create_owned ~finalize ?label ?(depth_compare = Always) ?(depth_write = false) ?front_face
      ?back_face (device : Device.t) () =
    let operation = "Metal.Depth_stencil.create" in
    on_main operation (fun () ->
        let* () = ensure_live operation device.lifetime in
        if option_exists contains_nul label then
          error operation Invalid_argument "depth/stencil label contains a NUL byte"
        else
          let descriptor : Metal_raw.depth_stencil_descriptor =
            {
              depth_compare_function = Sampler.compare_code depth_compare;
              depth_write_enabled = depth_write;
              front_face_stencil = Option.map raw_face front_face;
              back_face_stencil = Option.map raw_face back_face;
              label;
            }
          in
          let* raw =
            native_result operation (Metal_raw.depth_stencil_create device.raw descriptor)
          in
          let value : t = { raw; lifetime = lifetime (); device } in
          attach device.lifetime;
          if finalize then attach_finalizer value value.lifetime device.lifetime;
          Ok value)

  let create ?label ?depth_compare ?depth_write ?front_face ?back_face device () =
    create_owned ~finalize:true ?label ?depth_compare ?depth_write ?front_face ?back_face device ()

  module Private = struct end

  let destroy (value : t) =
    destroy_parent "Metal.Depth_stencil.destroy" value.lifetime value.raw (fun () ->
        detach value.device.lifetime)
end

module Shader_type = struct
  type scalar = shader_scalar_type =
    | Float
    | Half
    | Int
    | Uint
    | Short
    | Ushort
    | Char
    | Uchar
    | Bool
    | Long
    | Ulong
    | Bfloat

  type t = shader_data_type =
    | No_type
    | Struct
    | Array
    | Scalar of scalar
    | Vector of scalar * int
    | Matrix of scalar * int * int
    | Texture_type
    | Sampler_type
    | Pointer
    | Other_data_type of int

  let of_code = function
    | 0 -> No_type
    | 1 -> Struct
    | 2 -> Array
    | 3 -> Scalar Float
    | 4 -> Vector (Float, 2)
    | 5 -> Vector (Float, 3)
    | 6 -> Vector (Float, 4)
    | 7 -> Matrix (Float, 2, 2)
    | 8 -> Matrix (Float, 2, 3)
    | 9 -> Matrix (Float, 2, 4)
    | 10 -> Matrix (Float, 3, 2)
    | 11 -> Matrix (Float, 3, 3)
    | 12 -> Matrix (Float, 3, 4)
    | 13 -> Matrix (Float, 4, 2)
    | 14 -> Matrix (Float, 4, 3)
    | 15 -> Matrix (Float, 4, 4)
    | 16 -> Scalar Half
    | 17 -> Vector (Half, 2)
    | 18 -> Vector (Half, 3)
    | 19 -> Vector (Half, 4)
    | 20 -> Matrix (Half, 2, 2)
    | 21 -> Matrix (Half, 2, 3)
    | 22 -> Matrix (Half, 2, 4)
    | 23 -> Matrix (Half, 3, 2)
    | 24 -> Matrix (Half, 3, 3)
    | 25 -> Matrix (Half, 3, 4)
    | 26 -> Matrix (Half, 4, 2)
    | 27 -> Matrix (Half, 4, 3)
    | 28 -> Matrix (Half, 4, 4)
    | 29 -> Scalar Int
    | 30 -> Vector (Int, 2)
    | 31 -> Vector (Int, 3)
    | 32 -> Vector (Int, 4)
    | 33 -> Scalar Uint
    | 34 -> Vector (Uint, 2)
    | 35 -> Vector (Uint, 3)
    | 36 -> Vector (Uint, 4)
    | 37 -> Scalar Short
    | 38 -> Vector (Short, 2)
    | 39 -> Vector (Short, 3)
    | 40 -> Vector (Short, 4)
    | 41 -> Scalar Ushort
    | 42 -> Vector (Ushort, 2)
    | 43 -> Vector (Ushort, 3)
    | 44 -> Vector (Ushort, 4)
    | 45 -> Scalar Char
    | 46 -> Vector (Char, 2)
    | 47 -> Vector (Char, 3)
    | 48 -> Vector (Char, 4)
    | 49 -> Scalar Uchar
    | 50 -> Vector (Uchar, 2)
    | 51 -> Vector (Uchar, 3)
    | 52 -> Vector (Uchar, 4)
    | 53 -> Scalar Bool
    | 54 -> Vector (Bool, 2)
    | 55 -> Vector (Bool, 3)
    | 56 -> Vector (Bool, 4)
    | 58 -> Texture_type
    | 59 -> Sampler_type
    | 60 -> Pointer
    | 81 -> Scalar Long
    | 82 -> Vector (Long, 2)
    | 83 -> Vector (Long, 3)
    | 84 -> Vector (Long, 4)
    | 85 -> Scalar Ulong
    | 86 -> Vector (Ulong, 2)
    | 87 -> Vector (Ulong, 3)
    | 88 -> Vector (Ulong, 4)
    | 121 -> Scalar Bfloat
    | 122 -> Vector (Bfloat, 2)
    | 123 -> Vector (Bfloat, 3)
    | 124 -> Vector (Bfloat, 4)
    | code -> Other_data_type code
end

module Reflection = Metal_argument_reflection_snapshot

module Binding = struct
  type access = shader_binding_access =
    | Read_only
    | Read_write
    | Write_only
    | Unknown_access of int

  type buffer = buffer_binding_layout = {
    alignment : int64;
    data_size : int64;
    data_type : Shader_type.t;
  }

  type texture = texture_binding_layout = {
    texture_kind : Texture.kind;
    data_type : Shader_type.t;
    depth : bool;
    array_length : int64;
  }

  type sized = sized_binding_layout = { alignment : int64; data_size : int64 }

  type kind = shader_binding_kind =
    | Buffer_binding of buffer
    | Threadgroup_memory_binding of sized
    | Texture_binding of texture
    | Sampler_binding
    | Imageblock_data_binding
    | Imageblock_binding
    | Visible_function_table_binding
    | Primitive_acceleration_structure_binding
    | Instance_acceleration_structure_binding
    | Intersection_function_table_binding
    | Object_payload_binding of sized
    | Tensor_binding
    | Unknown_binding of int

  type t = shader_binding = {
    name : string;
    index : int64;
    access : access;
    used : bool;
    argument : bool;
    kind : kind;
    reflection : Reflection.reflected_type option;
  }

  let access_of_code = function
    | 0 -> Read_only
    | 1 -> Read_write
    | 2 -> Write_only
    | code -> Unknown_access code

  let texture_kind_of_code = function
    | 0 -> Some Texture.Texture_1d
    | 1 -> Some Texture.Texture_1d_array
    | 2 -> Some Texture.Texture_2d
    | 3 -> Some Texture.Texture_2d_array
    | 4 -> Some Texture.Texture_2d_multisample
    | 5 -> Some Texture.Texture_cube
    | 6 -> Some Texture.Texture_cube_array
    | 7 -> Some Texture.Texture_3d
    | 8 -> Some Texture.Texture_2d_multisample_array
    | 9 -> Some Texture.Texture_buffer
    | _ -> None

  let of_raw
      ( name,
        kind_code,
        access_code,
        index,
        used,
        argument,
        buffer_alignment,
        buffer_data_size,
        buffer_data_type,
        texture_kind,
        texture_data_type,
        depth,
        array_length,
        threadgroup_alignment,
        threadgroup_data_size,
        object_alignment,
        object_data_size,
        reflection ) =
    let kind =
      match kind_code with
      | 0 ->
          Buffer_binding
            {
              alignment = buffer_alignment;
              data_size = buffer_data_size;
              data_type = Shader_type.of_code buffer_data_type;
            }
      | 1 ->
          Threadgroup_memory_binding
            { alignment = threadgroup_alignment; data_size = threadgroup_data_size }
      | 2 -> (
          match texture_kind_of_code texture_kind with
          | Some texture_kind ->
              Texture_binding
                {
                  texture_kind;
                  data_type = Shader_type.of_code texture_data_type;
                  depth;
                  array_length;
                }
          | None -> Unknown_binding kind_code)
      | 3 -> Sampler_binding
      | 16 -> Imageblock_data_binding
      | 17 -> Imageblock_binding
      | 24 -> Visible_function_table_binding
      | 25 -> Primitive_acceleration_structure_binding
      | 26 -> Instance_acceleration_structure_binding
      | 27 -> Intersection_function_table_binding
      | 34 -> Object_payload_binding { alignment = object_alignment; data_size = object_data_size }
      | 37 -> Tensor_binding
      | code -> Unknown_binding code
    in
    { name; index; access = access_of_code access_code; used; argument; kind; reflection }
end

module Library = struct
  type t = library

  let make device raw =
    let value : t = { raw; lifetime = lifetime (); device } in
    attach device.lifetime;
    attach_finalizer value value.lifetime device.lifetime;
    value

  let compile_source ?label ~(device : Device.t) source =
    on_main "Metal.Library.compile_source" (fun () ->
        let* () = ensure_live "Metal.Library.compile_source" device.lifetime in
        if source = "" then
          error "Metal.Library.compile_source" Invalid_argument "shader source is empty"
        else if contains_nul source then
          error "Metal.Library.compile_source" Invalid_argument "shader source contains a NUL byte"
        else if option_exists contains_nul label then
          error "Metal.Library.compile_source" Invalid_argument "library label contains a NUL byte"
        else
          let* raw =
            native_result "Metal.Library.compile_source"
              (Metal_raw.library_compile device.raw source label)
          in
          Ok (make device raw))

  let destroy (value : t) =
    destroy_parent "Metal.Library.destroy" value.lifetime value.raw (fun () ->
        detach value.device.lifetime)
end

module Function = struct
  type t = function_handle

  type kind = function_kind =
    | Vertex
    | Fragment
    | Kernel
    | Visible
    | Intersection
    | Mesh
    | Object
    | Unknown_function_kind of int

  type constant_value = function_constant_value =
    | Bool_constant of bool
    | Int8_constant of int
    | Uint8_constant of int
    | Int16_constant of int
    | Uint16_constant of int
    | Int32_constant of int32
    | Uint32_constant of int64
    | Int64_constant of int64
    | Uint64_bits_constant of int64
    | Float16_constant of float
    | Float32_constant of float

  let kind_of_code = function
    | 1 -> Vertex
    | 2 -> Fragment
    | 3 -> Kernel
    | 5 -> Visible
    | 6 -> Intersection
    | 7 -> Mesh
    | 8 -> Object
    | code -> Unknown_function_kind code

  let validate_constant_name operation name =
    if name = "" || contains_nul name then
      error operation Invalid_argument
        "function-constant names must be nonempty and contain no NUL byte"
    else Ok ()

  let raw_constant operation (name, value) =
    let* () = validate_constant_name operation name in
    let integral tag value = Ok (name, tag, value, 0.) in
    match value with
    | Bool_constant value -> integral 0 (if value then 1L else 0L)
    | Int8_constant value when value >= -128 && value <= 127 -> integral 1 (Int64.of_int value)
    | Uint8_constant value when value >= 0 && value <= 255 -> integral 2 (Int64.of_int value)
    | Int16_constant value when value >= -32_768 && value <= 32_767 ->
        integral 3 (Int64.of_int value)
    | Uint16_constant value when value >= 0 && value <= 65_535 -> integral 4 (Int64.of_int value)
    | Int32_constant value -> integral 5 (Int64.of_int32 value)
    | Uint32_constant value when value >= 0L && value <= 0xffff_ffffL -> integral 6 value
    | Int64_constant value -> integral 7 value
    | Uint64_bits_constant value -> integral 8 value
    | Float16_constant value -> Ok (name, 9, 0L, value)
    | Float32_constant value -> Ok (name, 10, 0L, value)
    | Int8_constant _ -> error operation Invalid_argument "int8 function constant is out of range"
    | Uint8_constant _ -> error operation Invalid_argument "uint8 function constant is out of range"
    | Int16_constant _ -> error operation Invalid_argument "int16 function constant is out of range"
    | Uint16_constant _ ->
        error operation Invalid_argument "uint16 function constant is out of range"
    | Uint32_constant _ ->
        error operation Invalid_argument "uint32 function constant is out of range"

  let raw_constants operation constants =
    let rec loop seen reversed = function
      | [] -> Ok (Array.of_list (List.rev reversed))
      | ((name, _) as constant) :: rest ->
          if List.mem name seen then
            error operation Invalid_argument "function-constant list contains a duplicate name"
          else
            let* raw = raw_constant operation constant in
            loop (name :: seen) (raw :: reversed) rest
    in
    loop [] [] constants

  let find ~(library : Library.t) name =
    on_main "Metal.Function.find" (fun () ->
        let* () = ensure_live "Metal.Function.find" library.lifetime in
        if name = "" || contains_nul name then
          error "Metal.Function.find" Invalid_argument
            "function name must be nonempty and contain no NUL byte"
        else
          let* raw =
            native_result "Metal.Function.find" (Metal_raw.function_find library.raw name)
          in
          let value : t = { raw; lifetime = lifetime (); library } in
          attach library.lifetime;
          attach_finalizer value value.lifetime library.lifetime;
          Ok value)

  let specialize ~(library : Library.t) ~constants name =
    let label = None in
    let operation = "Metal.Function.specialize" in
    on_main operation (fun () ->
        let* () = ensure_live operation library.lifetime in
        if name = "" || contains_nul name then
          error operation Invalid_argument "function name must be nonempty and contain no NUL byte"
        else if option_exists contains_nul label then
          error operation Invalid_argument "function label contains a NUL byte"
        else
          let* raw_constants = raw_constants operation constants in
          let* raw =
            native_result operation
              (Metal_raw.function_specialize library.raw name raw_constants label)
          in
          let value : t = { raw; lifetime = lifetime (); library } in
          attach library.lifetime;
          attach_finalizer value value.lifetime library.lifetime;
          Ok value)

  let query operation raw (value : t) =
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let* x = native_result operation (raw value.raw) in
        Ok x)

  let argument_encoder (value : t) ~buffer_index =
    let operation = "Metal.Function.argument_encoder" in
    if buffer_index < 0L then error operation Invalid_argument "buffer index must be nonnegative"
    else
      let* raw =
        query operation
          (fun raw -> Metal_raw.shader_function_argument_encoder raw buffer_index)
          value
      in
      match Metal_raw.argument_encoder_snapshot raw with
      | Error m ->
          ignore (Metal_raw.destroy raw);
          native_error operation m
      | Ok (_, _, _, registry) when registry <> value.library.device.registry_id ->
          ignore (Metal_raw.destroy raw);
          error operation Device_mismatch "argument encoder device disagrees with function"
      | Ok (_argument_label, encoded_length, alignment, _) ->
          let x : shader_argument_encoder =
            {
              raw;
              lifetime = lifetime ();
              function_ = Some value;
              device = value.library.device;
              encoded_length;
              alignment;
              retained = Hashtbl.create 17;
              parent_encoder = None;
            }
          in
          attach value.lifetime;
          let retained = x.retained in
          attach_finalizer
            ~on_finalize:(fun () ->
              Hashtbl.iter (fun _ lifetime -> detach lifetime) retained;
              Hashtbl.clear retained)
            x x.lifetime value.lifetime;
          Ok x

  let destroy (value : t) =
    destroy_parent "Metal.Function.destroy" value.lifetime value.raw (fun () ->
        detach value.library.lifetime)
end

module Shader_argument_encoder = struct
  type t = shader_argument_encoder

  type resource =
    | Buffer of buffer
    | Texture of texture
    | Sampler of sampler
    | Acceleration_structure of acceleration_structure
    | Indirect_command_buffer of indirect_command_buffer
    | Render_pipeline of render_pipeline
    | Compute_pipeline of compute_pipeline
    | Depth_stencil of depth_stencil

  let encoded_length (value : t) = value.encoded_length
  let alignment (value : t) = value.alignment

  let parts = function
    | Buffer x -> (0, x.raw, x.lifetime, x.device)
    | Texture x -> (1, x.raw, x.lifetime, x.device)
    | Sampler x -> (2, x.raw, x.lifetime, x.device)
    | Acceleration_structure x -> (3, x.raw, x.lifetime, x.device)
    | Indirect_command_buffer x -> (4, x.raw, x.lifetime, x.device)
    | Render_pipeline x -> (7, x.raw, x.lifetime, x.device)
    | Compute_pipeline x -> (8, x.raw, x.lifetime, x.device)
    | Depth_stencil x -> (9, x.raw, x.lifetime, x.device)

  let retain_at (value : t) index (lifetime : lifetime) =
    match Hashtbl.find_opt value.retained index with
    | Some old when old == lifetime -> ()
    | old ->
        Option.iter detach old;
        attach lifetime;
        Hashtbl.replace value.retained index lifetime

  let set (value : t) ~index resource =
    let offset = 0L in
    let op = "Metal.Shader_argument_encoder.set" in
    on_main op (fun () ->
        let* () = ensure_live op value.lifetime in
        if index < 0L || offset < 0L then
          error op Invalid_argument "argument index or offset is negative"
        else
          let tag, raw, lifetime, device = parts resource in
          let* () = ensure_live op lifetime in
          if not (same_device value.device device) then
            error op Device_mismatch "argument resource belongs to another device"
          else
            let* () =
              native_result op (Metal_raw.argument_encoder_single value.raw tag raw offset index)
            in
            retain_at value index lifetime;
            Ok ())

  let set_argument_buffer (value : t) (buffer : buffer) ~offset () =
    let array_element = 0L in
    let start_offset = 0L in
    let op = "Metal.Shader_argument_encoder.set_argument_buffer" in
    on_main op (fun () ->
        let* () = ensure_live op value.lifetime in
        let* () = ensure_buffer_usable op buffer in
        if not (same_device value.device buffer.device) then
          error op Device_mismatch "argument buffer belongs to another device"
        else if offset < 0L || offset > buffer.length || start_offset < 0L || array_element < 0L
        then error op Invalid_argument "argument buffer range is invalid"
        else
          let* () =
            native_result op
              (Metal_raw.argument_encoder_set_buffer value.raw buffer.raw offset start_offset
                 array_element)
          in
          retain_at value (-1L) buffer.lifetime;
          Ok ())

  let destroy (value : t) =
    destroy_parent "Metal.Shader_argument_encoder.destroy" value.lifetime value.raw (fun () ->
        Hashtbl.iter (fun _ lifetime -> detach lifetime) value.retained;
        Hashtbl.clear value.retained;
        match value.parent_encoder with
        | Some parent -> detach parent.lifetime
        | None ->
            Option.iter (fun (f : function_handle) -> detach f.lifetime) value.function_;
            if Option.is_none value.function_ then detach value.device.lifetime)
end

module Compute_pipeline = struct
  type t = compute_pipeline

  let make device ~reflection raw raw_bindings =
    let bindings = if reflection then Some (Array.map Binding.of_raw raw_bindings) else None in
    let value : t =
      {
        raw;
        lifetime = lifetime ();
        device;
        bindings;
        max_total_threads = Metal_raw.compute_pipeline_max_total_threads raw;
      }
    in
    attach device.lifetime;
    attach_finalizer value value.lifetime device.lifetime;
    value

  let create ?label ?(reflection = false) (function_value : Function.t) =
    let operation = "Metal.Compute_pipeline.create" in
    on_main operation (fun () ->
        let* () = ensure_live operation function_value.lifetime in
        if option_exists contains_nul label then
          error operation Invalid_argument "pipeline label contains a NUL byte"
        else
          let device = function_value.library.device in
          if Function.kind_of_code (Metal_raw.function_kind function_value.raw) <> Function.Kernel
          then
            error operation Invalid_argument
              "the pipeline entry point must be a Metal kernel function"
          else
            let descriptor_required = label <> None || reflection in
            let creation =
              if not descriptor_required then
                Result.map
                  (fun raw -> (raw, [||]))
                  (Metal_raw.compute_pipeline_create device.raw function_value.raw)
              else
                let descriptor : Metal_raw.compute_pipeline_descriptor =
                  {
                    label;
                    reflection;
                    preloaded_libraries = [||];
                    binary_archives = [||];
                    fail_on_binary_archive_miss = false;
                    support_indirect_command_buffers = false;
                    buffer_mutabilities = Array.make 31 0;
                  }
                in
                Metal_raw.compute_pipeline_create_descriptor device.raw function_value.raw
                  descriptor
            in
            let* raw, raw_bindings = native_result operation creation in
            Ok (make device ~reflection raw raw_bindings))

  let bindings (value : t) = Option.map Array.to_list value.bindings
  let max_total_threads (value : t) = value.max_total_threads

  let destroy (value : t) =
    destroy_parent "Metal.Compute_pipeline.destroy" value.lifetime value.raw (fun () ->
        detach value.device.lifetime)
end

module Render_pipeline = struct
  type t = render_pipeline
  type primitive_topology = Point | Line | Triangle
  type color_attachment_mapping = render_color_attachment_mapping = Identity | Inherited
  type blend_state = render_blend_state = Blend_disabled | Blend_enabled

  type blend_factor = render_blend_factor =
    | Blend_zero
    | Blend_one
    | Blend_source_color
    | Blend_one_minus_source_color
    | Blend_source_alpha
    | Blend_one_minus_source_alpha
    | Blend_destination_color
    | Blend_one_minus_destination_color
    | Blend_destination_alpha
    | Blend_one_minus_destination_alpha
    | Blend_source_alpha_saturated
    | Blend_color
    | Blend_one_minus_color
    | Blend_alpha
    | Blend_one_minus_alpha
    | Blend_source1_color
    | Blend_one_minus_source1_color
    | Blend_source1_alpha
    | Blend_one_minus_source1_alpha

  type blend_operation = render_blend_operation =
    | Blend_add
    | Blend_subtract
    | Blend_reverse_subtract
    | Blend_min
    | Blend_max

  type color_write = render_color_write = Write_red | Write_green | Write_blue | Write_alpha

  type color_attachment = render_color_attachment = {
    format : Texture.format;
    blending : blend_state;
    source_rgb : blend_factor;
    destination_rgb : blend_factor;
    rgb_operation : blend_operation;
    source_alpha : blend_factor;
    destination_alpha : blend_factor;
    alpha_operation : blend_operation;
    write_mask : color_write list;
  }

  type reflection = { vertex : Binding.t list; fragment : Binding.t list }

  let color_attachment ?(blending = Blend_disabled) ?(source_rgb = Blend_one)
      ?(destination_rgb = Blend_zero) ?(rgb_operation = Blend_add) ?(source_alpha = Blend_one)
      ?(destination_alpha = Blend_zero) ?(alpha_operation = Blend_add)
      ?(write_mask = [ Write_red; Write_green; Write_blue; Write_alpha ]) format =
    {
      format;
      blending;
      source_rgb;
      destination_rgb;
      rgb_operation;
      source_alpha;
      destination_alpha;
      alpha_operation;
      write_mask;
    }

  let blend_state_code = function Blend_disabled -> 0 | Blend_enabled -> 1
  let color_attachment_mapping_code = function Identity -> 0 | Inherited -> 1

  let blend_factor_code = function
    | Blend_zero -> 0
    | Blend_one -> 1
    | Blend_source_color -> 2
    | Blend_one_minus_source_color -> 3
    | Blend_source_alpha -> 4
    | Blend_one_minus_source_alpha -> 5
    | Blend_destination_color -> 6
    | Blend_one_minus_destination_color -> 7
    | Blend_destination_alpha -> 8
    | Blend_one_minus_destination_alpha -> 9
    | Blend_source_alpha_saturated -> 10
    | Blend_color -> 11
    | Blend_one_minus_color -> 12
    | Blend_alpha -> 13
    | Blend_one_minus_alpha -> 14
    | Blend_source1_color -> 15
    | Blend_one_minus_source1_color -> 16
    | Blend_source1_alpha -> 17
    | Blend_one_minus_source1_alpha -> 18

  let blend_operation_code = function
    | Blend_add -> 0
    | Blend_subtract -> 1
    | Blend_reverse_subtract -> 2
    | Blend_min -> 3
    | Blend_max -> 4

  let color_write_bit = function
    | Write_red -> 8
    | Write_green -> 4
    | Write_blue -> 2
    | Write_alpha -> 1

  let write_mask_code values =
    List.fold_left (fun mask value -> mask lor color_write_bit value) 0 values

  let raw_color_attachment (value : color_attachment) =
    ({
       Metal_raw.pixel_format = Metal_format.code value.format;
       blending_state = blend_state_code value.blending;
       source_rgb_blend_factor = blend_factor_code value.source_rgb;
       destination_rgb_blend_factor = blend_factor_code value.destination_rgb;
       rgb_blend_operation = blend_operation_code value.rgb_operation;
       source_alpha_blend_factor = blend_factor_code value.source_alpha;
       destination_alpha_blend_factor = blend_factor_code value.destination_alpha;
       alpha_blend_operation = blend_operation_code value.alpha_operation;
       write_mask = write_mask_code value.write_mask;
     }
      : Metal_raw.metal4_render_color_attachment_descriptor)

  let topology_code = function Point -> 1 | Line -> 2 | Triangle -> 3

  let make ?mesh_constraints:_ ?tile_constraints:_ ?color_attachments:_ ?vertex_descriptor:_
      ?alpha_to_coverage:_ ?alpha_to_one:_ ?max_vertex_amplification_count:_
      ?color_attachment_mapping:_ device ~raster_sample_count ~color_formats ~reflection raw
      (raw_reflection : Metal_raw.render_pipeline_reflection) =
    let map values = Array.map Binding.of_raw values in
    let reflection =
      if reflection then
        Some
          {
            vertex_bindings = map raw_reflection.vertex_bindings;
            fragment_bindings = map raw_reflection.fragment_bindings;
          }
      else None
    in
    let value : t =
      { raw; lifetime = lifetime (); device; raster_sample_count; color_formats; reflection }
    in
    attach device.lifetime;
    attach_finalizer value value.lifetime device.lifetime;
    value

  let state_query operation raw (value : t) =
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let* x = native_result operation (raw value.raw) in
        Ok x)

  let supports_indirect_command_buffers value =
    state_query "Metal.Render_pipeline.supports_indirect_command_buffers"
      Metal_raw.pipeline_render_indirect value

  let reflection (value : t) =
    Option.map
      (fun reflection ->
        {
          vertex = Array.to_list reflection.vertex_bindings;
          fragment = Array.to_list reflection.fragment_bindings;
        })
      value.reflection

  let destroy (value : t) =
    destroy_parent "Metal.Render_pipeline.destroy" value.lifetime value.raw (fun () ->
        detach value.device.lifetime)
end

let ensure_metal4 operation (device : Device.t) =
  let* () = ensure_live operation device.lifetime in
  if
    not
      (Result.value ~default:false
         (Metal_raw.Registry.device_supports_family device.raw
            (Int64.of_int (Device.family_code Device.Metal4))))
  then error operation Unsupported "the Metal device does not support Metal 4"
  else Ok ()

module Compiler = struct
  type t = compiler

  let make device raw =
    let value : t = { raw; lifetime = lifetime (); device } in
    attach device.lifetime;
    attach_finalizer value value.lifetime device.lifetime;
    value

  let create device =
    let operation = "Metal.Compiler.create" in
    on_main operation (fun () ->
        let* () = ensure_metal4 operation device in
        let* raw = native_result operation (Metal_raw.compiler_create device.raw None None) in
        Ok (make device raw))

  let validate_pipeline_entry operation (library : Library.t) ~stage name =
    if name = "" || contains_nul name then
      error operation Invalid_argument
        (stage ^ " function name must be nonempty and contain no NUL byte")
    else if not (Array.exists (String.equal name) (Metal_raw.library_function_names library.raw))
    then error operation Invalid_argument (stage ^ " function is absent from the source library")
    else Ok ()

  let validate_optional_pipeline_entry operation library ~stage = function
    | None -> Ok ()
    | Some name -> validate_pipeline_entry operation library ~stage name

  let resolve_color_attachments operation ?color_formats ?color_attachments () =
    match (color_formats, color_attachments) with
    | Some _, Some _ ->
        error operation Invalid_argument
          "color_formats and color_attachments are mutually exclusive"
    | Some formats, None -> Ok (List.map Render_pipeline.color_attachment formats)
    | None, Some attachments -> Ok attachments
    | None, None -> Ok [ Render_pipeline.color_attachment Texture.Bgra8_unorm ]

  let validate_render_target operation (device : Device.t) ~has_fragment ~raster_sample_count
      ~color_attachments ~rasterization_enabled =
    if rasterization_enabled <> has_fragment then
      error operation Invalid_argument "rasterization requires exactly one fragment function"
    else if
      (rasterization_enabled && color_attachments = [])
      || ((not rasterization_enabled) && color_attachments <> [])
      || List.length color_attachments > 8
    then
      error operation Invalid_argument
        "render color attachments must match rasterization and not exceed eight"
    else if
      List.exists
        (fun (attachment : Render_pipeline.color_attachment) ->
          Metal_format.is_depth_or_stencil attachment.format)
        color_attachments
    then error operation Invalid_argument "render color attachments require color pixel formats"
    else if
      List.exists
        (fun (attachment : Render_pipeline.color_attachment) ->
          List.length attachment.write_mask
          <> List.length (List.sort_uniq compare attachment.write_mask))
        color_attachments
    then error operation Invalid_argument "render color-write masks contain duplicate channels"
    else if raster_sample_count <= 0 then
      error operation Invalid_argument "render raster sample count must be positive"
    else if
      not
        (probe
           (Metal_raw.Registry.device_supports_texture_sample_count device.raw
              (Int64.of_int raster_sample_count)))
    then error operation Unsupported "the Metal device does not support the render sample count"
    else
      Ok
        ( Int64.of_int raster_sample_count,
          List.map
            (fun (attachment : Render_pipeline.color_attachment) -> attachment.format)
            color_attachments,
          Array.of_list (List.map Render_pipeline.raw_color_attachment color_attachments) )

  let with_render_descriptor operation callback ?label ?fragment ?(reflection = false)
      ?(raster_sample_count = 1) ?color_formats ?color_attachments
      ?(primitive_topology = Render_pipeline.Triangle) ?(support_indirect_command_buffers = false)
      (value : t) ~(library : Library.t) ~vertex =
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let* () = ensure_live operation library.lifetime in
        let* () = ensure_same_device operation value.device library.device in
        let* () = validate_pipeline_entry operation library ~stage:"vertex" vertex in
        let* () = validate_optional_pipeline_entry operation library ~stage:"fragment" fragment in
        if option_exists contains_nul label then
          error operation Invalid_argument "render-pipeline label contains a NUL byte"
        else
          let* color_attachments =
            resolve_color_attachments operation ?color_formats ?color_attachments ()
          in
          let* raster_sample_count, color_formats, raw_color_attachments =
            validate_render_target operation value.device ~has_fragment:(Option.is_some fragment)
              ~raster_sample_count ~color_attachments ~rasterization_enabled:true
          in
          let descriptor : Metal_raw.metal4_render_descriptor =
            {
              label;
              library = library.raw;
              vertex_function = vertex;
              fragment_function = fragment;
              reflection;
              raster_sample_count;
              color_attachments = raw_color_attachments;
              rasterization_enabled = true;
              primitive_topology = Render_pipeline.topology_code primitive_topology;
              support_indirect_commands = support_indirect_command_buffers;
              lookup_archives = [||];
              vertex_descriptor = None;
              support_vertex_binary_linking = false;
              support_fragment_binary_linking = false;
              vertex_dynamic_linking = None;
              fragment_dynamic_linking = None;
              vertex_static_linking = None;
              fragment_static_linking = None;
              alpha_to_coverage = false;
              alpha_to_one = false;
              max_vertex_amplification_count = 1L;
              color_attachment_mapping =
                Render_pipeline.color_attachment_mapping_code Render_pipeline.Identity;
            }
          in
          callback reflection color_attachments color_formats descriptor)

  let create_render_pipeline ?label ?fragment ?(reflection = false) ?(raster_sample_count = 1)
      ?color_formats ?color_attachments ?(primitive_topology = Render_pipeline.Triangle)
      ?(support_indirect_command_buffers = false) (value : t) ~(library : Library.t) ~vertex =
    let operation = "Metal.Compiler.create_render_pipeline" in
    with_render_descriptor operation
      (fun reflection color_attachments color_formats descriptor ->
        let* raw, raw_reflection =
          native_result operation (Metal_raw.compiler_create_render_pipeline value.raw descriptor)
        in
        Ok
          (Render_pipeline.make value.device ~raster_sample_count ~alpha_to_coverage:false
             ~alpha_to_one:false ~max_vertex_amplification_count:1
             ~color_attachment_mapping:Render_pipeline.Identity ~color_formats ~color_attachments
             ~reflection raw raw_reflection))
      ?label ?fragment ~reflection ~raster_sample_count ?color_formats ?color_attachments
      ~primitive_topology ~support_indirect_command_buffers value ~library ~vertex

  let destroy (value : t) =
    destroy_parent "Metal.Compiler.destroy" value.lifetime value.raw (fun () ->
        detach value.device.lifetime)
end

module Indirect_command_buffer = struct
  type command_type = indirect_command_kind =
    | Indirect_draw
    | Indirect_draw_indexed
    | Indirect_concurrent_dispatch
    | Indirect_concurrent_dispatch_threads

  type descriptor = indirect_command_buffer_descriptor
  type t = indirect_command_buffer
  type buffer = t

  let descriptor ?(inherit_buffers = false) ?(inherit_pipeline_state = false)
      ?(max_vertex_buffer_bind_count = 0) ?(max_fragment_buffer_bind_count = 0) ~command_types () =
    let inherit_cull_mode = true in
    let inherit_depth_bias = true in
    let inherit_depth_clip_mode = true in
    let inherit_depth_stencil_state = true in
    let inherit_front_facing_winding = true in
    let inherit_triangle_fill_mode = true in
    let max_kernel_buffer_bind_count = 0 in
    let max_kernel_threadgroup_memory_bind_count = 0 in
    let max_mesh_buffer_bind_count = 0 in
    let max_object_buffer_bind_count = 0 in
    let max_object_threadgroup_memory_bind_count = 0 in
    let support_color_attachment_mapping = false in
    let support_dynamic_attribute_stride = false in
    let support_ray_tracing = false in
    {
      command_types;
      inherit_buffers;
      inherit_pipeline_state;
      max_vertex_buffer_bind_count;
      max_fragment_buffer_bind_count;
      max_kernel_buffer_bind_count;
      support_ray_tracing;
      support_dynamic_attribute_stride;
      max_kernel_threadgroup_memory_bind_count;
      max_object_buffer_bind_count;
      max_mesh_buffer_bind_count;
      max_object_threadgroup_memory_bind_count;
      inherit_depth_stencil_state;
      inherit_depth_bias;
      inherit_depth_clip_mode;
      inherit_cull_mode;
      inherit_front_facing_winding;
      inherit_triangle_fill_mode;
      support_color_attachment_mapping;
    }

  let command_bit = function
    | Indirect_draw -> 1L
    | Indirect_draw_indexed -> 2L
    | Indirect_concurrent_dispatch -> 32L
    | Indirect_concurrent_dispatch_threads -> 64L

  let has kind kinds = List.exists (( = ) kind) kinds

  let release_retained retained =
    let values = !retained in
    retained := [];
    List.iter
      (function
        | Indirect_buffer value -> detach value.lifetime
        | Indirect_render_pipeline value -> detach value.lifetime)
      values

  let retain value retained =
    let lifetime =
      match retained with
      | Indirect_buffer value -> value.lifetime
      | Indirect_render_pipeline value -> value.lifetime
    in
    attach lifetime;
    value.retained := retained :: !(value.retained)

  let raw_descriptor (value : descriptor) =
    ({
       Metal_raw.command_types =
         List.fold_left
           (fun bits kind -> Int64.logor bits (command_bit kind))
           0L value.command_types;
       inherit_buffers = value.inherit_buffers;
       inherit_pipeline_state = value.inherit_pipeline_state;
       max_vertex_buffer_bind_count = Int64.of_int value.max_vertex_buffer_bind_count;
       max_fragment_buffer_bind_count = Int64.of_int value.max_fragment_buffer_bind_count;
       max_kernel_buffer_bind_count = Int64.of_int value.max_kernel_buffer_bind_count;
       support_ray_tracing = value.support_ray_tracing;
       support_dynamic_attribute_stride = value.support_dynamic_attribute_stride;
       max_kernel_threadgroup_memory_bind_count =
         Int64.of_int value.max_kernel_threadgroup_memory_bind_count;
       max_object_buffer_bind_count = Int64.of_int value.max_object_buffer_bind_count;
       max_mesh_buffer_bind_count = Int64.of_int value.max_mesh_buffer_bind_count;
       max_object_threadgroup_memory_bind_count =
         Int64.of_int value.max_object_threadgroup_memory_bind_count;
       inherit_depth_stencil_state = value.inherit_depth_stencil_state;
       inherit_depth_bias = value.inherit_depth_bias;
       inherit_depth_clip_mode = value.inherit_depth_clip_mode;
       inherit_cull_mode = value.inherit_cull_mode;
       inherit_front_facing_winding = value.inherit_front_facing_winding;
       inherit_triangle_fill_mode = value.inherit_triangle_fill_mode;
       support_color_attachment_mapping = value.support_color_attachment_mapping;
     }
      : Metal_raw.indirect_command_buffer_descriptor)

  let create ~(device : Device.t) ~max_command_count (descriptor : descriptor) =
    let cpu_cache = Buffer.Default_cache in
    let hazard_tracking = Buffer.Default_hazard_tracking in
    let storage = Buffer.Private in
    let operation = "Metal.Indirect_command_buffer.create" in
    on_main operation (fun () ->
        let counts =
          [
            descriptor.max_vertex_buffer_bind_count;
            descriptor.max_fragment_buffer_bind_count;
            descriptor.max_kernel_buffer_bind_count;
            descriptor.max_kernel_threadgroup_memory_bind_count;
            descriptor.max_object_buffer_bind_count;
            descriptor.max_mesh_buffer_bind_count;
            descriptor.max_object_threadgroup_memory_bind_count;
          ]
        in
        let* () = ensure_live operation device.lifetime in
        if descriptor.command_types = [] then
          error operation Invalid_argument "at least one indirect command type is required"
        else if List.sort_uniq compare descriptor.command_types <> descriptor.command_types then
          error operation Invalid_argument "indirect command types must be unique and ordered"
        else if max_command_count <= 0 then
          error operation Invalid_argument "maximum command count must be positive"
        else if List.exists (fun count -> count < 0 || count > 31) counts then
          error operation Invalid_argument "indirect binding counts must be in [0, 31]"
        else
          let options = resource_options_code ~storage ~cpu_cache ~hazard_tracking in
          let* raw =
            native_result operation
              (Metal_raw.indirect_command_buffer_create device.raw (raw_descriptor descriptor)
                 (Int64.of_int max_command_count) (Int64.of_int options))
          in
          let value =
            {
              raw;
              lifetime = lifetime ();
              device;
              max_command_count;
              command_types = descriptor.command_types;
              retained = ref [];
            }
          in
          attach device.lifetime;
          let retained = value.retained in
          attach_finalizer
            ~on_finalize:(fun () -> release_retained retained)
            value value.lifetime device.lifetime;
          Ok value)

  let validate_range operation (value : t) ~location ~length =
    if
      location < 0 || length < 0
      || location > value.max_command_count
      || length > value.max_command_count - location
    then error operation Invalid_argument "range exceeds the indirect command buffer"
    else Ok ()

  let destroy (value : t) =
    destroy_parent "Metal.Indirect_command_buffer.destroy" value.lifetime value.raw (fun () ->
        release_retained value.retained;
        detach value.device.lifetime)

  module Render_command = struct
    type t = indirect_render_command
    type primitive = Point | Line | Line_strip | Triangle | Triangle_strip
    type index_type = Uint16 | Uint32

    let primitive_code = function
      | Point -> 0
      | Line -> 1
      | Line_strip -> 2
      | Triangle -> 3
      | Triangle_strip -> 4

    let at (value : buffer) index =
      let operation = "Metal.Indirect_command_buffer.Render_command.at" in
      on_main operation (fun () ->
          let* () = ensure_live operation value.lifetime in
          if
            not
              (has Indirect_draw value.command_types
              || has Indirect_draw_indexed value.command_types)
          then error operation Invalid_state "descriptor does not enable render commands"
          else
            let* () = validate_range operation value ~location:index ~length:1 in
            let* raw =
              native_result operation
                (Metal_raw.indirect_render_command value.raw (Int64.of_int index))
            in
            let command : t = { raw; lifetime = lifetime (); parent = value } in
            attach value.lifetime;
            attach_finalizer command command.lifetime value.lifetime;
            Ok command)

    let set_pipeline (value : t) (pipeline : Render_pipeline.t) =
      let operation = "Metal.Indirect_command_buffer.Render_command.set_pipeline" in
      on_main operation (fun () ->
          let* () = ensure_live operation value.lifetime in
          let* () = ensure_live operation pipeline.lifetime in
          let* () = ensure_same_device operation value.parent.device pipeline.device in
          match
            Metal_raw.Registry.render_pipeline_state_support_indirect_command_buffers pipeline.raw
          with
          | Error message -> native_error operation message
          | Ok false ->
              error operation Unsupported "pipeline was not compiled for indirect command buffers"
          | Ok true ->
              let* () =
                native_result operation
                  (Metal_raw.indirect_render_command_set_pipeline value.raw pipeline.raw)
              in
              retain value.parent (Indirect_render_pipeline pipeline);
              Ok ())

    let set_buffer operation native (value : t) ~index ~offset (buffer : Buffer.t) =
      on_main operation (fun () ->
          let* () = ensure_live operation value.lifetime in
          let* () = ensure_buffer_usable operation buffer in
          if index < 0 || index >= 31 then
            error operation Invalid_argument "buffer index must be in [0, 31)"
          else if offset < 0L || offset > buffer.length then
            error operation Invalid_argument "buffer offset is outside the resource"
          else
            let* () = ensure_same_device operation value.parent.device buffer.device in
            let* () = native_result operation (native value.raw buffer.raw offset index) in
            retain value.parent (Indirect_buffer buffer);
            Ok ())

    let set_vertex_buffer value ~index ~offset buffer =
      set_buffer "Metal.Indirect_command_buffer.Render_command.set_vertex_buffer"
        Metal_raw.indirect_render_command_set_vertex_buffer value ~index ~offset buffer

    let set_fragment_buffer value ~index ~offset buffer =
      set_buffer "Metal.Indirect_command_buffer.Render_command.set_fragment_buffer"
        Metal_raw.indirect_render_command_set_fragment_buffer value ~index ~offset buffer

    let draw_indexed (value : t) ~primitive ~index_type ~(index_buffer : Buffer.t) ~index_offset
        ~index_count ?(instance_count = 1L) () =
      let base_instance = 0L in
      let base_vertex = 0L in
      let operation = "Metal.Indirect_command_buffer.Render_command.draw_indexed" in
      let type_code, element = match index_type with Uint16 -> (0, 2L) | Uint32 -> (1, 4L) in
      on_main operation (fun () ->
          let* () = ensure_live operation value.lifetime in
          let* () = ensure_buffer_usable operation index_buffer in
          if
            index_count <= 0L || instance_count <= 0L || base_instance < 0L || index_offset < 0L
            || Int64.rem index_offset element <> 0L
            || index_offset > index_buffer.length
            || index_count > Int64.div (Int64.sub index_buffer.length index_offset) element
          then error operation Invalid_argument "indexed indirect draw range is invalid"
          else
            let* () = ensure_same_device operation value.parent.device index_buffer.device in
            let* () =
              native_result operation
                (Metal_raw.indirect_render_draw_indexed value.raw (primitive_code primitive)
                   index_count type_code index_buffer.raw index_offset instance_count base_vertex
                   base_instance value.parent.device.registry_id)
            in
            retain value.parent (Indirect_buffer index_buffer);
            Ok ())

    let draw_primitives (value : t) ~primitive ~vertex_start ~vertex_count ?(instance_count = 1) ()
        =
      let base_instance = 0 in
      let operation = "Metal.Indirect_command_buffer.Render_command.draw_primitives" in
      on_main operation (fun () ->
          let* () = ensure_live operation value.lifetime in
          if vertex_start < 0 || vertex_count <= 0 || instance_count <= 0 || base_instance < 0 then
            error operation Invalid_argument "draw ranges must be nonnegative and counts positive"
          else
            let* () =
              native_result operation
                (Metal_raw.indirect_render_command_draw_primitives value.raw
                   (primitive_code primitive) (Int64.of_int vertex_start)
                   (Int64.of_int vertex_count) (Int64.of_int instance_count)
                   (Int64.of_int base_instance))
            in
            Ok ())

    let destroy (value : t) =
      destroy_leaf "Metal.Indirect_command_buffer.Render_command.destroy" value.lifetime value.raw
        (fun () -> detach value.parent.lifetime)
  end
end

module Command_queue = struct
  type t = command_queue

  let create (device : Device.t) =
    on_main "Metal.Command_queue.create" (fun () ->
        let* () = ensure_live "Metal.Command_queue.create" device.lifetime in
        let* raw =
          native_result "Metal.Command_queue.create"
            (Metal_raw.Registry.command_queue_create device.raw)
        in
        let residency_sets = ref [] in
        let value : t = { raw; lifetime = lifetime (); device; residency_sets } in
        attach device.lifetime;
        attach_finalizer
          ~on_finalize:(fun () -> release_queue_residency_sets residency_sets)
          value value.lifetime device.lifetime;
        Ok value)

  let destroy (value : t) =
    destroy_parent "Metal.Command_queue.destroy" value.lifetime value.raw (fun () ->
        release_queue_residency_sets value.residency_sets;
        detach value.device.lifetime)
end

module Command_buffer = struct
  type t = command_buffer
  type present_time = Immediate | At_time of float | After_minimum_duration of float

  type status =
    | Not_enqueued
    | Enqueued
    | Committed
    | Scheduled
    | Completed
    | Error of string
    | Unknown of int

  type diagnostics = {
    error_options : int64;
    gpu_start_time : float;
    gpu_end_time : float;
    kernel_start_time : float;
    kernel_end_time : float;
    retained_references : bool;
  }

  let release_callback_tokens tokens =
    let retained = !tokens in
    tokens := [];
    List.iter Metal_raw.command_buffer_cancel_handler retained

  let create_owned ~finalize (queue : Command_queue.t) ?label () =
    let* () = before_main "Metal.Command_buffer.create" in
    let* () = ensure_live "Metal.Command_buffer.create" queue.lifetime in
    match label with
    | Some label when contains_nul label ->
        error "Metal.Command_buffer.create" Invalid_argument "label contains a NUL byte"
    | _ -> (
        let* raw =
          native_result "Metal.Command_buffer.create"
            (Metal_raw.Registry.command_buffer_create queue.raw)
        in
        let prepared_resource =
          if finalize then Some { prepared_active = false; prepared_lifetime = queue.lifetime }
          else None
        in
        let prepared_command_slot =
          { prepared_command_active = false; prepared_command = empty_prepared_command_resources }
        in
        let value : t =
          {
            raw;
            lifetime = lifetime ();
            queue;
            phase = Recording;
            resources = ref [];
            retained_identities = Hashtbl.create 16;
            prepared_resource;
            prepared_command_slot;
            scoped_prepared_active = false;
            scoped_prepared_lifetime = queue.lifetime;
            callback_tokens = ref [];
            presentation_events = ref [];
          }
        in
        attach queue.lifetime;
        let resources = value.resources
        and identities = value.retained_identities
        and callback_tokens = value.callback_tokens
        and presentation_events = value.presentation_events in
        if finalize then
          attach_lifetime_finalizer
            ~on_finalize:(fun () ->
              Option.iter
                (fun prepared_resource ->
                  release_finalized_command_buffer_resources resources identities prepared_resource
                    prepared_command_slot)
                prepared_resource;
              release_callback_tokens callback_tokens;
              List.iter detach !presentation_events;
              presentation_events := [])
            value.lifetime queue.lifetime;
        match label with
        | None -> Ok value
        | Some label -> (
            match Metal_raw.Registry.set_command_buffer_label raw label with
            | Ok () -> Ok value
            | Error message ->
                ignore (Metal_raw.destroy raw);
                if Atomic.compare_and_set value.lifetime.destroyed false true then
                  detach queue.lifetime;
                native_error "Metal.Command_buffer.create" message))

  let create queue () =
    let label = None in
    create_owned ~finalize:true queue ?label ()

  module Private = struct
    (* Scoped callers must arrange [destroy] on every exit.  In particular a
       submitted buffer must first be brought to a terminal state with
       [wait_until_completed].  Omitting the finalizer prevents a blocking
       synchronous submission from depending on a later major collection. *)
  
  end

  let device (value : t) = value.queue.device
  let destroyed (value : t) = is_destroyed value.lifetime

  let release_presentation_events (value : t) =
    List.iter detach !(value.presentation_events);
    value.presentation_events := []

  let diagnostics (value : t) =
    let operation = "Metal.Command_buffer.diagnostics" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let* ( queue_id,
               device_id,
               error_options,
               gpu_start_time,
               gpu_end_time,
               kernel_start_time,
               kernel_end_time,
               retained_references ) =
          native_result operation (Metal_raw.presentation_command_snapshot value.raw)
        in
        if queue_id <> value.queue.device.registry_id || device_id <> value.queue.device.registry_id
        then error operation Device_mismatch "native command-buffer device identity changed"
        else if
          not
            (List.for_all Float.is_finite
               [ gpu_start_time; gpu_end_time; kernel_start_time; kernel_end_time ])
        then error operation Native_error "native command-buffer timestamps are non-finite"
        else
          Ok
            {
              error_options;
              gpu_start_time;
              gpu_end_time;
              kernel_start_time;
              kernel_end_time;
              retained_references;
            })

  let status (value : t) =
    on_main "Metal.Command_buffer.status" (fun () ->
        let* () = ensure_live "Metal.Command_buffer.status" value.lifetime in
        let status = command_buffer_status value.raw in
        if status = 4 || status = 5 then begin
          release_command_buffer_resources value;
          release_callback_tokens value.callback_tokens;
          release_presentation_events value
        end;
        Ok
          (match status with
          | 0 -> Not_enqueued
          | 1 -> Enqueued
          | 2 -> Committed
          | 3 -> Scheduled
          | 4 -> Completed
          | 5 ->
              Error
                (Option.value
                   (Metal_raw.command_buffer_error value.raw)
                   ~default:"Metal command buffer failed without NSError")
          | value -> Unknown value))

  let present (value : t) (drawable : metal_drawable) () =
    let at = Immediate in
    let operation = "Metal.Command_buffer.present" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    if value.phase <> Recording then
      error operation Invalid_state "command buffer is no longer recording"
    else
      let* () = ensure_live operation drawable.lifetime in
      if drawable.presentation_scheduled then
        error operation Invalid_state "drawable is already scheduled for presentation"
      else
        let* () = ensure_same_device operation value.queue.device drawable.layer.device in
        let mode, time =
          match at with
          | Immediate -> (0, 0.)
          | At_time t -> (1, t)
          | After_minimum_duration t -> (2, t)
        in
        if (not (Float.is_finite time)) || time < 0. then
          error operation Invalid_argument "presentation time must be finite and nonnegative"
        else
          let* () =
            native_result operation
              (Metal_raw.command_buffer_present_drawable value.raw drawable.raw mode time)
          in
          drawable.presentation_scheduled <- true;
          retain_command_buffer_drawable value drawable;
          Ok ()

  let commit (value : t) =
    let* () = before_main "Metal.Command_buffer.commit" in
    let* () = ensure_live "Metal.Command_buffer.commit" value.lifetime in
    if value.phase <> Recording then
      error "Metal.Command_buffer.commit" Invalid_state "command buffer was already submitted"
    else if dependent_count value.lifetime <> 0 then
      error "Metal.Command_buffer.commit" Invalid_state "a command encoder is still open"
    else
      let* () =
        native_result "Metal.Command_buffer.commit"
          (Metal_raw.Registry.command_buffer_commit value.raw)
      in
      value.phase <- Submitted;
      Ok ()

  let wait_until_completed (value : t) =
    let* () = before_main "Metal.Command_buffer.wait_until_completed" in
    let* () = ensure_live "Metal.Command_buffer.wait_until_completed" value.lifetime in
    if value.phase <> Submitted then
      error "Metal.Command_buffer.wait_until_completed" Invalid_state
        "command buffer has not been committed"
    else (
      Metal_raw.command_buffer_wait value.raw;
      let status = command_buffer_status value.raw in
      if status = 4 || status = 5 then begin
        release_command_buffer_resources value;
        release_callback_tokens value.callback_tokens;
        release_presentation_events value
      end;
      if status = 4 then Ok ()
      else
        native_error "Metal.Command_buffer.wait_until_completed"
          (Option.value
             (Metal_raw.command_buffer_error value.raw)
             ~default:(Printf.sprintf "command buffer ended with status %d" status)))

  let destroy (value : t) =
    if
      value.phase = Submitted
      &&
      let status = command_buffer_status value.raw in
      status <> 4 && status <> 5
    then
      error "Metal.Command_buffer.destroy" Parent_has_dependents
        "submitted command buffer has not reached a terminal state"
    else
      let* () = before_main "Metal.Command_buffer.destroy" in
      if is_destroyed value.lifetime then Ok ()
      else
        let dependents = dependent_count value.lifetime in
        if dependents <> 0 then
          error "Metal.Command_buffer.destroy" Parent_has_dependents
            (Printf.sprintf "handle still owns %d live dependent(s)" dependents)
        else begin
          Atomic.set value.lifetime.destroyed true;
          (* Metal may synchronously run completion blocks while deallocating
               an uncommitted command buffer.  Cancel and unroot callbacks
               before releasing the native object so those blocks become
               harmless no-ops instead of re-entering the OCaml runtime. *)
          release_callback_tokens value.callback_tokens;
          ignore (Metal_raw.destroy value.raw);
          release_command_buffer_resources value;
          release_presentation_events value;
          detach value.queue.lifetime;
          Ok ()
        end
end

module Acceleration_encoder = struct
  type t = acceleration_encoder
  type compacted_size_type = Uint32 | Uint64

  let create (command_buffer : Command_buffer.t) =
    let operation = "Metal.Acceleration_encoder.create" in
    on_main operation (fun () ->
        let* () = ensure_live operation command_buffer.lifetime in
        if command_buffer.phase <> Recording then
          error operation Invalid_state "command buffer is no longer recording"
        else if dependent_count command_buffer.lifetime <> 0 then
          error operation Invalid_state "command buffer already has an open encoder"
        else
          let* raw =
            native_result operation
              (Metal_raw.Registry.command_buffer_acceleration_encoder command_buffer.raw)
          in
          let value = { raw; lifetime = lifetime (); command_buffer } in
          attach command_buffer.lifetime;
          attach_finalizer value value.lifetime command_buffer.lifetime;
          Ok value)

  (* Build or refit through a generic [Build.t] descriptor; every buffer and
     structure it references stays alive until the command buffer completes. *)
  let with_descriptor operation (value : t) ~(descriptor : Acceleration_structure.Build.t)
      ~(scratch : buffer) ~scratch_offset ~required ~destinations run =
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let device = value.command_buffer.queue.device in
        let* () = ensure_live operation descriptor.lifetime in
        if not (same_device device descriptor.device) then
          error operation Device_mismatch "descriptor belongs to another device"
        else
          let buffers = scratch :: descriptor.buffers in
          let rec validate = function
            | [] -> Ok ()
            | (buffer : buffer) :: rest ->
                let* () = ensure_live operation buffer.lifetime in
                if buffer.device.lifetime != device.lifetime then
                  error operation Device_mismatch "build resource belongs to another device"
                else validate rest
          in
          let rec validate_structures = function
            | [] -> Ok ()
            | (x : Acceleration_structure.t) :: rest ->
                let* () = ensure_live operation x.lifetime in
                if x.device.lifetime != device.lifetime then
                  error operation Device_mismatch "structure belongs to another device"
                else validate_structures rest
          in
          let* () = validate buffers in
          let* () = validate_structures (destinations @ descriptor.structures) in
          if scratch_offset < 0L || scratch_offset > scratch.length then
            error operation Invalid_argument "scratch offset exceeds its buffer"
          else
            let* sizes =
              native_result operation (Metal_raw.accel_descriptor_sizes device.raw descriptor.raw)
            in
            let structure_size, build_scratch, refit_scratch = sizes in
            let scratch_needed = required build_scratch refit_scratch in
            if
              List.exists
                (fun (x : Acceleration_structure.t) -> x.size < structure_size)
                destinations
            then error operation Invalid_argument "destination acceleration structure is too small"
            else if scratch_needed > Int64.sub scratch.length scratch_offset then
              error operation Invalid_argument "scratch range is too small"
            else
              let* () = native_result operation (run ()) in
              List.iter (retain_command_buffer_buffer value.command_buffer) buffers;
              List.iter
                (retain_command_buffer_acceleration_structure value.command_buffer)
                (destinations @ descriptor.structures);
              Ok ())

  let build_with (value : t) ~(destination : Acceleration_structure.t) ~descriptor ~scratch
      ~scratch_offset =
    with_descriptor "Metal.Acceleration_encoder.build_with" value ~descriptor ~scratch
      ~scratch_offset
      ~required:(fun build _ -> build)
      ~destinations:[ destination ]
      (fun () ->
        Metal_raw.accel_encoder_build_descriptor value.raw destination.raw
          descriptor.Acceleration_structure.Build.raw scratch.raw scratch_offset)

  let validate_acceleration operation (device : device) (value : acceleration_structure) =
    let* () = ensure_live operation value.lifetime in
    if value.device.lifetime != device.lifetime then
      error operation Device_mismatch "acceleration structure belongs to another device"
    else Ok ()

  let copy_common operation native (value : t) ~(source : acceleration_structure)
      ~(destination : acceleration_structure) =
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let device = value.command_buffer.queue.device in
        let* () = validate_acceleration operation device source in
        let* () = validate_acceleration operation device destination in
        let* () = native_result operation (native value.raw source.raw destination.raw) in
        retain_command_buffer_acceleration_structure value.command_buffer source;
        retain_command_buffer_acceleration_structure value.command_buffer destination;
        Ok ())

  let copy_and_compact value ~source ~destination =
    copy_common "Metal.Acceleration_encoder.copy_and_compact"
      Metal_raw.acceleration_encoder_copy_and_compact value ~source ~destination

  let write_compacted_size_typed (value : t) ~source ~destination ~offset kind =
    let operation = "Metal.Acceleration_encoder.write_compacted_size_typed" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let bytes, code = match kind with Uint32 -> (4L, 0) | Uint64 -> (8L, 1) in
        let* () = validate_acceleration operation value.command_buffer.queue.device source in
        let* () = ensure_buffer_usable operation destination in
        let* () =
          ensure_same_device operation value.command_buffer.queue.device destination.device
        in
        if
          offset < 0L
          || Int64.rem offset bytes <> 0L
          || offset > destination.length
          || bytes > Int64.sub destination.length offset
        then error operation Invalid_argument "compacted-size output range is invalid"
        else
          let* () =
            native_result operation
              (Metal_raw.acceleration_encoder_write_type value.raw source.raw destination.raw offset
                 code)
          in
          retain_command_buffer_acceleration_structure value.command_buffer source;
          retain_command_buffer_buffer value.command_buffer destination;
          Ok ())

  let end_encoding (value : acceleration_encoder) =
    let operation = "Metal.Acceleration_encoder.end_encoding" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let* () = native_result operation (Metal_raw.Registry.acceleration_encoder_end value.raw) in
        if Atomic.compare_and_set value.lifetime.destroyed false true then begin
          ignore (Metal_raw.destroy value.raw);
          detach value.command_buffer.lifetime
        end;
        Ok ())
end

module Render_encoder = struct
  type t = render_encoder
  type cull_mode = No_cull | Cull_front | Cull_back
  type winding = Clockwise | Counter_clockwise
  type stage = Vertex | Fragment
  type resource_usage = Read | Write | Sample
  type resource = Buffer_resource of Buffer.t | Texture_resource of Texture.t
  type primitive = Point | Line | Line_strip | Triangle | Triangle_strip
  type index_type = Uint16 | Uint32

  type viewport = {
    x : float;
    y : float;
    width : float;
    height : float;
    znear : float;
    zfar : float;
  }

  type scissor = { x : int; y : int; width : int; height : int }

  let bits code values = List.fold_left (fun mask value -> mask lor code value) 0 values
  let stage_code = function Vertex -> 1 | Fragment -> 2
  let usage_code = function Read -> 1 | Write -> 2 | Sample -> 4
  let depth_attachment_formats = [ Texture.Depth32_float ]
  let stencil_attachment_formats = []

  let validate_attachment operation (command_buffer : command_buffer) (target : texture) formats
      (texture : texture) =
    let* () = ensure_texture_usable operation texture in
    if not (List.mem Render_target texture.descriptor.usage) then
      error operation Invalid_argument "attachment lacks Render_target usage"
    else if not (List.mem texture.descriptor.format formats) then
      error operation Invalid_argument "attachment pixel format is incompatible"
    else if
      texture.descriptor.width <> target.descriptor.width
      || texture.descriptor.height <> target.descriptor.height
      || texture.descriptor.sample_count <> target.descriptor.sample_count
    then error operation Invalid_argument "attachment dimensions or sample count differ"
    else ensure_same_device operation command_buffer.queue.device texture.device

  let create_owned ~finalize (command_buffer : Command_buffer.t) ~(target : Texture.t)
      ?(clear = (0., 0., 0., 1.)) ?(depth : Texture.t option) ?(stencil : Texture.t option) () =
    let operation = "Metal.Render_encoder.create" in
    let* () = before_main operation in
    let* () = ensure_live operation command_buffer.lifetime in
    if command_buffer.phase <> Recording then
      error operation Invalid_state "command buffer is no longer recording"
    else if dependent_count command_buffer.lifetime <> 0 then
      error operation Invalid_state "command buffer already has an open encoder"
    else
      let* () = ensure_texture_usable operation target in
      if not (List.mem Render_target target.descriptor.usage) then
        error operation Invalid_argument "render target texture lacks Render_target usage"
      else if target.descriptor.sample_count <> 1 then
        error operation Invalid_argument "classic render encoder currently requires one sample"
      else if not (same_device command_buffer.queue.device target.device) then
        error operation Device_mismatch "render target belongs to another device"
      else
        let attachment_check =
          match depth with
          | Some texture ->
              validate_attachment operation command_buffer target depth_attachment_formats texture
          | None -> Ok ()
        in
        let attachment_check =
          let* () = attachment_check in
          match stencil with
          | Some texture ->
              validate_attachment operation command_buffer target stencil_attachment_formats texture
          | None -> Ok ()
        in
        let* () = attachment_check in
        let r, g, b, a = clear in
        if not (List.for_all Float.is_finite [ r; g; b; a ]) then
          error operation Invalid_argument "clear color must be finite"
        else
          let* raw =
            native_result operation
              (Metal_raw.command_buffer_render_encoder_attachments command_buffer.raw target.raw
                 (Option.map (fun (x : texture) -> x.raw) depth)
                 (Option.map (fun (x : texture) -> x.raw) stencil)
                 clear)
          in
          let value : t =
            { raw; lifetime = lifetime (); command_buffer; target; pipeline = None }
          in
          attach command_buffer.lifetime;
          retain_command_buffer_texture command_buffer target;
          Option.iter (retain_command_buffer_texture command_buffer) depth;
          Option.iter (retain_command_buffer_texture command_buffer) stencil;
          if finalize then attach_lifetime_finalizer value.lifetime command_buffer.lifetime;
          Ok value

  let create command_buffer ~target () =
    let clear = None in
    let depth = None in
    let stencil = None in
    create_owned ~finalize:true command_buffer ~target ?clear ?depth ?stencil ()

  let create_from_pass_owned ~finalize (command_buffer : Command_buffer.t)
      (pass : render_pass_descriptor) =
    let operation = "Metal.Render_encoder.create_from_pass" in
    let* () = before_main operation in
    let* () = ensure_live operation command_buffer.lifetime in
    if command_buffer.phase <> Recording then
      error operation Invalid_state "command buffer is no longer recording"
    else if dependent_count command_buffer.lifetime <> 0 then
      error operation Invalid_state "command buffer already has an open encoder"
    else
      let* () = ensure_live operation pass.lifetime in
      match pass.pass_color with
      | None -> error operation Invalid_state "render pass has no color attachment"
      | Some target ->
          let* () = ensure_same_device operation command_buffer.queue.device target.device in
          let* raw =
            native_result operation
              (Metal_raw.Registry.command_buffer_render_encoder_from_pass command_buffer.raw
                 pass.raw)
          in
          let value : t =
            { raw; lifetime = lifetime (); command_buffer; target; pipeline = None }
          in
          attach command_buffer.lifetime;
          retain_command_buffer_texture command_buffer target;
          Option.iter (retain_command_buffer_texture command_buffer) pass.pass_depth;
          Option.iter (retain_command_buffer_texture command_buffer) pass.pass_stencil;
          Option.iter (retain_command_buffer_buffer command_buffer) pass.pass_visibility;
          Option.iter (retain_command_buffer_texture command_buffer) pass.pass_resolve;
          Array.iter
            (Option.iter (fun (state : render_pass_sample_state) ->
                 if
                   not
                     (List.exists
                        (( == ) state.sample_buffer.lifetime)
                        !(command_buffer.presentation_events))
                 then begin
                   attach state.sample_buffer.lifetime;
                   command_buffer.presentation_events :=
                     state.sample_buffer.lifetime :: !(command_buffer.presentation_events)
                 end))
            pass.pass_samples;
          if finalize then attach_lifetime_finalizer value.lifetime command_buffer.lifetime;
          Ok value

  type prepared_indexed_binding = {
    prepared_stage : stage;
    prepared_index : int;
    prepared_offset : int64;
    prepared_buffer : Buffer.t;
  }

  type prepared_indexed_draw = {
    prepared_pipeline : Render_pipeline.t;
    prepared_bindings : prepared_indexed_binding array;
    prepared_primitive : primitive;
    prepared_index_type : index_type;
    prepared_index_buffer : Buffer.t;
    prepared_index_offset : int64;
    prepared_index_count : int64;
  }

  type prepared_indexed_draws = {
    prepared_device : device;
    prepared_draws : prepared_indexed_draw array;
    prepared_pipeline_raws : Metal_raw.handle array;
    prepared_buffer_raws : Metal_raw.handle array array;
    prepared_stages : int array array;
    prepared_offsets : int64 array array;
    prepared_slots : int array array;
    prepared_primitives : int array;
    prepared_counts : int64 array;
    prepared_index_types : int array;
    prepared_index_raws : Metal_raw.handle array;
    prepared_index_offsets : int64 array;
    prepared_command_resources : prepared_command_resources;
  }

  let distinct_roots lifetime_of values =
    let length = Array.length values in
    if length = 0 then [||]
    else
      let seen = Hashtbl.create length
      and roots = Array.make length (Array.unsafe_get values 0)
      and root_count = ref 0 in
      Array.iter
        (fun value ->
          let identity = (lifetime_of value).identity in
          if not (Hashtbl.mem seen identity) then begin
            Hashtbl.add seen identity ();
            Array.unsafe_set roots !root_count value;
            incr root_count
          end)
        values;
      Array.sub roots 0 !root_count

  let create_from_pass_scoped command_buffer pass =
    create_from_pass_owned ~finalize:false command_buffer pass

  let prepare_indexed_draws (device : Device.t) (draws : prepared_indexed_draw array) =
    let operation = "Metal.Render_encoder.prepare_indexed_draws" in
    let* () = before_main operation in
    let* () = ensure_live operation device.lifetime in
    if Array.length draws = 0 then error operation Invalid_argument "indexed draw array is empty"
    else
      let draws =
        Array.map
          (fun draw -> { draw with prepared_bindings = Array.copy draw.prepared_bindings })
          draws
      in
      let duplicate_binding bindings index =
        let candidate = Array.unsafe_get bindings index in
        let rec scan other =
          if other = index then false
          else
            let binding = Array.unsafe_get bindings other in
            binding.prepared_stage = candidate.prepared_stage
            && binding.prepared_index = candidate.prepared_index
            || scan (other + 1)
        in
        scan 0
      in
      let validate_binding bindings index =
        let binding : prepared_indexed_binding = Array.unsafe_get bindings index in
        if binding.prepared_stage <> Vertex && binding.prepared_stage <> Fragment then
          error operation Invalid_argument "indexed draw binding must target vertex or fragment"
        else if binding.prepared_index < 0 || binding.prepared_index >= 31 then
          error operation Invalid_argument "buffer index must be in [0, 31)"
        else if duplicate_binding bindings index then
          error operation Invalid_argument "buffer binding is duplicated"
        else
          let* () = ensure_buffer_usable operation binding.prepared_buffer in
          if
            binding.prepared_offset < 0L || binding.prepared_offset > binding.prepared_buffer.length
          then error operation Invalid_argument "buffer offset is outside the resource"
          else ensure_same_device operation device binding.prepared_buffer.device
      in
      let validate_draw draw =
        let* () = ensure_live operation draw.prepared_pipeline.lifetime in
        let* () = ensure_same_device operation device draw.prepared_pipeline.device in
        let rec bindings index =
          if index = Array.length draw.prepared_bindings then Ok ()
          else
            let* () = validate_binding draw.prepared_bindings index in
            bindings (index + 1)
        in
        let* () = bindings 0 in
        let* () = ensure_buffer_usable operation draw.prepared_index_buffer in
        let* () = ensure_same_device operation device draw.prepared_index_buffer.device in
        let width = match draw.prepared_index_type with Uint16 -> 2L | Uint32 -> 4L in
        if
          draw.prepared_index_count <= 0L || draw.prepared_index_offset < 0L
          || Int64.rem draw.prepared_index_offset width <> 0L
          || width > Int64.div Int64.max_int draw.prepared_index_count
        then error operation Invalid_argument "indexed draw range is invalid"
        else
          let required = Int64.mul width draw.prepared_index_count in
          if
            draw.prepared_index_offset > draw.prepared_index_buffer.length
            || required > Int64.sub draw.prepared_index_buffer.length draw.prepared_index_offset
          then error operation Invalid_argument "indexed draw exceeds the index buffer"
          else Ok ()
      in
      let rec validate index =
        if index = Array.length draws then Ok ()
        else
          let* () = validate_draw draws.(index) in
          validate (index + 1)
      in
      let* () = validate 0 in
      let rec count_resources draw_index count =
        if draw_index = Array.length draws then Ok count
        else
          let draw = Array.unsafe_get draws draw_index in
          let additional = Array.length draw.prepared_bindings + 2 in
          if count > Sys.max_array_length - additional then
            error operation Invalid_argument
              "indexed draw resources exceed the supported array size"
          else count_resources (draw_index + 1) (count + additional)
      in
      let* resource_count = count_resources 0 0 in
      let prepared_buffer_raws =
        Array.map
          (fun draw ->
            Array.map (fun binding -> binding.prepared_buffer.raw) draw.prepared_bindings)
          draws
      and prepared_stages =
        Array.map
          (fun draw ->
            Array.map
              (fun binding -> if binding.prepared_stage = Vertex then 0 else 1)
              draw.prepared_bindings)
          draws
      and prepared_offsets =
        Array.map
          (fun draw -> Array.map (fun binding -> binding.prepared_offset) draw.prepared_bindings)
          draws
      and prepared_slots =
        Array.map
          (fun draw -> Array.map (fun binding -> binding.prepared_index) draw.prepared_bindings)
          draws
      in
      let prepared_pipeline_roots =
        distinct_roots
          (fun (pipeline : render_pipeline) -> pipeline.lifetime)
          (Array.map (fun draw -> draw.prepared_pipeline) draws)
      and all_buffer_roots =
        Array.make (resource_count - Array.length draws) draws.(0).prepared_index_buffer
      in
      let buffer_index = ref 0 in
      let add_buffer_root buffer =
        Array.unsafe_set all_buffer_roots !buffer_index buffer;
        incr buffer_index
      in
      for draw_index = 0 to Array.length draws - 1 do
        let draw = Array.unsafe_get draws draw_index in
        for binding_index = 0 to Array.length draw.prepared_bindings - 1 do
          let buffer = (Array.unsafe_get draw.prepared_bindings binding_index).prepared_buffer in
          add_buffer_root buffer
        done;
        let index_buffer = draw.prepared_index_buffer in
        add_buffer_root index_buffer
      done;
      let prepared_buffer_roots =
        distinct_roots (fun (buffer : buffer) -> buffer.lifetime) all_buffer_roots
      in
      let prepared_command_resources =
        {
          prepared_pipeline_roots;
          prepared_buffer_roots;
          prepared_texture_roots = [||];
          prepared_depth_stencil_roots = [||];
          prepared_sample_roots = [||];
          prepared_indirect_roots = [||];
          prepared_resource_roots = [||];
        }
      in
      Ok
        {
          prepared_device = device;
          prepared_draws = draws;
          prepared_pipeline_raws = Array.map (fun draw -> draw.prepared_pipeline.raw) draws;
          prepared_buffer_raws;
          prepared_stages;
          prepared_offsets;
          prepared_slots;
          prepared_primitives =
            Array.map
              (fun draw ->
                match draw.prepared_primitive with
                | Point -> 0
                | Line -> 1
                | Line_strip -> 2
                | Triangle -> 3
                | Triangle_strip -> 4)
              draws;
          prepared_counts = Array.map (fun draw -> draw.prepared_index_count) draws;
          prepared_index_types =
            Array.map
              (fun draw -> match draw.prepared_index_type with Uint16 -> 0 | Uint32 -> 1)
              draws;
          prepared_index_raws = Array.map (fun draw -> draw.prepared_index_buffer.raw) draws;
          prepared_index_offsets = Array.map (fun draw -> draw.prepared_index_offset) draws;
          prepared_command_resources;
        }

  let rec validate_prepared_bindings operation bindings index =
    if index = Array.length bindings then Ok ()
    else
      let* () = ensure_buffer_usable operation bindings.(index).prepared_buffer in
      validate_prepared_bindings operation bindings (index + 1)

  let rec validate_prepared_execution operation (target : texture) draws index =
    if index = Array.length draws then Ok ()
    else
      let draw = draws.(index) in
      let* () = ensure_live operation draw.prepared_pipeline.lifetime in
      if
        draw.prepared_pipeline.raster_sample_count <> target.descriptor.sample_count
        || draw.prepared_pipeline.color_formats = []
        || List.hd draw.prepared_pipeline.color_formats <> target.descriptor.format
      then error operation Invalid_argument "pipeline differs from the render target"
      else
        let* () = ensure_buffer_usable operation draw.prepared_index_buffer in
        let* () = validate_prepared_bindings operation draw.prepared_bindings 0 in
        validate_prepared_execution operation target draws (index + 1)

  let close_failed_prepared_encoder (value : t) =
    ignore (Metal_raw.Registry.render_encoder_end value.raw);
    if Atomic.compare_and_set value.lifetime.destroyed false true then begin
      ignore (Metal_raw.destroy value.raw);
      detach value.command_buffer.lifetime
    end;
    value.command_buffer.phase <- Failed

  let execute_prepared_indexed_draws (value : t) prepared =
    let operation = "Metal.Render_encoder.execute_prepared_indexed_draws" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    let* () =
      ensure_same_device operation value.command_buffer.queue.device prepared.prepared_device
    in
    let* () = validate_prepared_execution operation value.target prepared.prepared_draws 0 in
    match
      Metal_raw.render_encoder_execute_indexed_draws value.raw prepared.prepared_pipeline_raws
        prepared.prepared_buffer_raws prepared.prepared_stages prepared.prepared_offsets
        prepared.prepared_slots prepared.prepared_primitives prepared.prepared_counts
        prepared.prepared_index_types prepared.prepared_index_raws prepared.prepared_index_offsets
    with
    | Error message ->
        close_failed_prepared_encoder value;
        native_error operation message
    | Ok () ->
        retain_command_buffer_prepared_command value.command_buffer
          prepared.prepared_command_resources;
        value.pipeline <-
          Some prepared.prepared_draws.(Array.length prepared.prepared_draws - 1).prepared_pipeline;
        Ok ()

  let destroyed (value : t) = is_destroyed value.lifetime

  let set_pipeline (value : t) (pipeline : Render_pipeline.t) =
    let operation = "Metal.Render_encoder.set_pipeline" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    let* () = ensure_live operation pipeline.lifetime in
    let* () = ensure_same_device operation value.command_buffer.queue.device pipeline.device in
    if pipeline.raster_sample_count <> value.target.descriptor.sample_count then
      error operation Invalid_argument "pipeline sample count differs from the render target"
    else if
      pipeline.color_formats = []
      || List.hd pipeline.color_formats <> value.target.descriptor.format
    then error operation Invalid_argument "pipeline color format differs from the render target"
    else
      let* () =
        native_result operation
          (Metal_raw.Registry.render_encoder_set_pipeline value.raw pipeline.raw)
      in
      (match value.pipeline with
      | Some current when current == pipeline -> ()
      | None | Some _ -> value.pipeline <- Some pipeline);
      retain_command_buffer_render_pipeline value.command_buffer pipeline;
      Ok ()

  let set_buffer operation raw_call (value : t) ~index ~offset (buffer : Buffer.t) =
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    let* () = ensure_buffer_usable operation buffer in
    if index < 0 || index >= 31 then
      error operation Invalid_argument "buffer index must be in [0, 31)"
    else if offset < 0L || offset > buffer.length then
      error operation Invalid_argument "buffer offset is outside the resource"
    else
      let* () = ensure_same_device operation value.command_buffer.queue.device buffer.device in
      let* () = native_result operation (raw_call value.raw buffer.raw offset index) in
      retain_command_buffer_buffer value.command_buffer buffer;
      Ok ()

  let set_vertex_buffer =
    set_buffer "Metal.Render_encoder.set_vertex_buffer" (fun e b o i ->
        Metal_raw.Registry.render_encoder_set_vertex_buffer e b o (Int64.of_int i))

  let set_fragment_buffer =
    set_buffer "Metal.Render_encoder.set_fragment_buffer" (fun e b o i ->
        Metal_raw.Registry.render_encoder_set_fragment_buffer e b o (Int64.of_int i))

  let set_texture operation raw_call (value : t) ~index (texture : Texture.t) =
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let* () = ensure_texture_usable operation texture in
        if index < 0 || index >= 31 then
          error operation Invalid_argument "texture index must be in [0, 31)"
        else
          let* () = ensure_same_device operation value.command_buffer.queue.device texture.device in
          let* () = native_result operation (raw_call value.raw texture.raw index) in
          retain_command_buffer_texture value.command_buffer texture;
          Ok ())

  let set_vertex_texture =
    set_texture "Metal.Render_encoder.set_vertex_texture" (fun e t i ->
        Metal_raw.Registry.render_encoder_set_vertex_texture e t (Int64.of_int i))

  let set_fragment_texture =
    set_texture "Metal.Render_encoder.set_fragment_texture" (fun e t i ->
        Metal_raw.Registry.render_encoder_set_fragment_texture e t (Int64.of_int i))

  let set_sampler operation raw_call (value : t) ~index (sampler : Sampler.t) =
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let* () = ensure_live operation sampler.lifetime in
        if index < 0 || index >= 31 then
          error operation Invalid_argument "sampler index must be in [0, 31)"
        else
          let* () = ensure_same_device operation value.command_buffer.queue.device sampler.device in
          let result = raw_call value.raw sampler.raw index in
          let* () = native_result operation result in
          retain_command_buffer_sampler value.command_buffer sampler;
          Ok ())

  let set_vertex_sampler =
    set_sampler "Metal.Render_encoder.set_vertex_sampler" (fun e s i ->
        Metal_raw.Registry.render_encoder_set_vertex_sampler e s (Int64.of_int i))

  let set_fragment_sampler =
    set_sampler "Metal.Render_encoder.set_fragment_sampler" (fun e s i ->
        Metal_raw.Registry.render_encoder_set_fragment_sampler e s (Int64.of_int i))

  let set_validated operation validate raw_call (value : t) argument =
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    let* raw_argument = validate value argument in
    let* () = native_result operation (raw_call value.raw raw_argument) in
    Ok ()

  let set_viewport (value : t) (viewport : viewport) =
    let operation = "Metal.Render_encoder.set_viewport" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    if
      not
        (Float.is_finite viewport.x && Float.is_finite viewport.y && Float.is_finite viewport.width
       && Float.is_finite viewport.height && Float.is_finite viewport.znear
       && Float.is_finite viewport.zfar)
    then error operation Invalid_argument "viewport values must be finite"
    else if
      (* The origin and extent may leave the target: a viewport maps clip space
         and Metal rasterizes only the part on the attachment. *)
      viewport.width <= 0. || viewport.height <= 0. || viewport.znear < 0. || viewport.znear > 1.
      || viewport.zfar < 0. || viewport.zfar > 1. || viewport.znear > viewport.zfar
    then error operation Invalid_argument "viewport is empty or outside the depth range"
    else
      let* () =
        native_result operation
          (Metal_raw.Registry.render_encoder_set_viewport value.raw
             ({
                originX = viewport.x;
                originY = viewport.y;
                width = viewport.width;
                height = viewport.height;
                znear = viewport.znear;
                zfar = viewport.zfar;
              }
               : Metal_gen.Record.Mtl_viewport.t))
      in
      Ok ()

  let set_scissor (value : t) (scissor : scissor) =
    let operation = "Metal.Render_encoder.set_scissor" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    if
      scissor.x < 0 || scissor.y < 0 || scissor.width <= 0 || scissor.height <= 0
      || scissor.x > value.target.descriptor.width - scissor.width
      || scissor.y > value.target.descriptor.height - scissor.height
    then error operation Invalid_argument "scissor rectangle is outside the render target"
    else
      let* () =
        native_result operation
          (Metal_raw.Registry.render_encoder_set_scissor value.raw
             ({
                x = Int64.of_int scissor.x;
                y = Int64.of_int scissor.y;
                width = Int64.of_int scissor.width;
                height = Int64.of_int scissor.height;
              }
               : Metal_gen.Record.Mtl_scissor_rect.t))
      in
      Ok ()

  let set_cull_mode (value : t) mode =
    let operation = "Metal.Render_encoder.set_cull_mode" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    let mode = match mode with No_cull -> 0 | Cull_front -> 1 | Cull_back -> 2 in
    let* () =
      native_result operation
        (Metal_raw.Registry.render_encoder_set_cull_mode value.raw (Int64.of_int mode))
    in
    Ok ()

  let set_front_facing_winding =
    set_validated "Metal.Render_encoder.set_front_facing_winding"
      (fun _ winding -> Ok (match winding with Clockwise -> 0 | Counter_clockwise -> 1))
      (fun e w -> Metal_raw.Registry.render_encoder_set_winding e (Int64.of_int w))

  let validate_nonempty operation what values =
    if values = [] then error operation Invalid_argument (what ^ " must be nonempty") else Ok ()

  let validate_resource operation device = function
    | Buffer_resource b ->
        let* () = ensure_buffer_usable operation b in
        ensure_same_device operation device b.device
    | Texture_resource t ->
        let* () = ensure_texture_usable operation t in
        ensure_same_device operation device t.device

  let resource_raw = function Buffer_resource b -> b.raw | Texture_resource t -> t.raw

  let resource_lifetime = function
    | Buffer_resource b -> b.lifetime
    | Texture_resource t -> t.lifetime

  let has_duplicate_lifetimes lifetimes =
    let rec loop seen = function
      | [] -> false
      | value :: rest ->
          List.exists (fun current -> current == value) seen || loop (value :: seen) rest
    in
    loop [] lifetimes

  let retain_resource command_buffer = function
    | Buffer_resource b -> retain_command_buffer_buffer command_buffer b
    | Texture_resource t -> retain_command_buffer_texture command_buffer t

  let use_resources (value : t) resources ~usage ~stages =
    let operation = "Metal.Render_encoder.use_resources" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let* () = validate_nonempty operation "resources" resources in
        let* () = validate_nonempty operation "usage" usage in
        let* () = validate_nonempty operation "stages" stages in
        let device = value.command_buffer.queue.device in
        if has_duplicate_lifetimes (List.map resource_lifetime resources) then
          error operation Invalid_argument "resources contain duplicate identities"
        else
          match
            List.find_map
              (fun r ->
                match validate_resource operation device r with Ok () -> None | Error e -> Some e)
              resources
          with
          | Some e -> Error e
          | None ->
              let* () =
                native_result operation
                  (Metal_raw.render_encoder_use_resources value.raw
                     (Array.of_list (List.map resource_raw resources))
                     (bits usage_code usage) (bits stage_code stages))
              in
              List.iter (retain_resource value.command_buffer) resources;
              Ok ())

  let set_depth_stencil_state (value : t) (state : Depth_stencil.t option) =
    let operation = "Metal.Render_encoder.set_depth_stencil_state" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    match state with
    | Some x ->
        let* () = ensure_live operation x.lifetime in
        let* () = ensure_same_device operation value.command_buffer.queue.device x.device in
        let* () =
          native_result operation (Metal_raw.Registry.render_depth_stencil value.raw (Some x.raw))
        in
        retain_command_buffer_depth_stencil value.command_buffer x;
        Ok ()
    | None ->
        let* () =
          native_result operation (Metal_raw.Registry.render_depth_stencil value.raw None)
        in
        Ok ()

  let primitive_code = function
    | Point -> 0
    | Line -> 1
    | Line_strip -> 2
    | Triangle -> 3
    | Triangle_strip -> 4

  let index_type_code = function Uint16 -> 0 | Uint32 -> 1
  let index_width = function Uint16 -> 2L | Uint32 -> 4L

  let validate_draw_buffer operation (value : t) (buffer : buffer) ~offset ~required =
    let* () = ensure_buffer_usable operation buffer in
    let* () = ensure_same_device operation value.command_buffer.queue.device buffer.device in
    if
      offset < 0L || required < 0L || offset > buffer.length
      || required > Int64.sub buffer.length offset
    then error operation Invalid_argument "draw buffer range is outside the resource"
    else Ok ()

  let checked_product operation a b =
    if a < 0L || b < 0L || (a <> 0L && b > Int64.div Int64.max_int a) then
      error operation Invalid_argument "draw range overflows"
    else Ok (Int64.mul a b)

  let draw_indexed_basic (value : t) ~primitive ~index_type ~(index_buffer : Buffer.t) ~index_offset
      ~index_count =
    let operation = "Metal.Render_encoder.draw_indexed_basic" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    if Option.is_none value.pipeline then
      error operation Invalid_state "no render pipeline is bound"
    else if index_count <= 0L then error operation Invalid_argument "index count must be positive"
    else
      let width = index_width index_type in
      if index_offset < 0L || Int64.rem index_offset width <> 0L then
        error operation Invalid_argument "index offset is misaligned"
      else if index_count <> 0L && width > Int64.div Int64.max_int index_count then
        error operation Invalid_argument "draw range overflows"
      else
        let required = Int64.mul index_count width in
        let* () =
          validate_draw_buffer operation value index_buffer ~offset:index_offset ~required
        in
        let* () =
          native_result operation
            (Metal_raw.Registry.render_draw_indexed_basic value.raw
               (Int64.of_int (primitive_code primitive))
               index_count
               (Int64.of_int (index_type_code index_type))
               index_buffer.raw index_offset)
        in
        retain_command_buffer_buffer value.command_buffer index_buffer;
        Ok ()

  let draw_indexed_instances (value : t) ~primitive ~index_type ~(index_buffer : Buffer.t)
      ~index_offset ~index_count ~instances =
    let operation = "Metal.Render_encoder.draw_indexed_instances" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        if Option.is_none value.pipeline then
          error operation Invalid_state "no render pipeline is bound"
        else if index_count <= 0L || instances <= 0L then
          error operation Invalid_argument "draw counts must be positive"
        else
          let width = index_width index_type in
          if index_offset < 0L || Int64.rem index_offset width <> 0L then
            error operation Invalid_argument "index offset is misaligned"
          else
            let* required = checked_product operation index_count width in
            let* () =
              validate_draw_buffer operation value index_buffer ~offset:index_offset ~required
            in
            let* () =
              native_result operation
                (Metal_raw.Registry.render_draw_indexed_instances value.raw
                   (Int64.of_int (primitive_code primitive))
                   index_count
                   (Int64.of_int (index_type_code index_type))
                   index_buffer.raw index_offset instances)
            in
            retain_command_buffer_buffer value.command_buffer index_buffer;
            Ok ())

  (* An indexed draw whose index count and instance count the device wrote: [arguments] holds
     five uints at [arguments_offset] (index count, instance count, first index, base vertex,
     base instance), read when the draw runs. *)
  let draw_indexed_indirect (value : t) ~primitive ~index_type ~(index_buffer : Buffer.t)
      ~index_offset ~(arguments : Buffer.t) ~arguments_offset =
    let operation = "Metal.Render_encoder.draw_indexed_indirect" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        if Option.is_none value.pipeline then
          error operation Invalid_state "no render pipeline is bound"
        else
          let width = index_width index_type in
          if index_offset < 0L || Int64.rem index_offset width <> 0L then
            error operation Invalid_argument "index offset is misaligned"
          else if arguments_offset < 0L || Int64.rem arguments_offset 4L <> 0L then
            error operation Invalid_argument "indirect arguments offset is misaligned"
          else
            let* () =
              validate_draw_buffer operation value index_buffer ~offset:index_offset ~required:width
            in
            let* () =
              validate_draw_buffer operation value arguments ~offset:arguments_offset ~required:20L
            in
            let* () =
              native_result operation
                (Metal_raw.Registry.render_draw_indexed_indirect value.raw
                   (Int64.of_int (primitive_code primitive))
                   (Int64.of_int (index_type_code index_type))
                   index_buffer.raw index_offset arguments.raw arguments_offset)
            in
            retain_command_buffer_buffer value.command_buffer index_buffer;
            retain_command_buffer_buffer value.command_buffer arguments;
            Ok ())

  let pipeline_supports_icb operation (value : t) =
    match value.pipeline with
    | None -> error operation Invalid_state "no render pipeline is bound"
    | Some p ->
        let* b =
          native_result operation
            (Metal_raw.Registry.render_pipeline_state_support_indirect_command_buffers p.raw)
        in
        Ok b

  let execute_indirect_commands (value : t) (commands : indirect_command_buffer) ~location ~length =
    let operation = "Metal.Render_encoder.execute_indirect_commands" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    let* () = ensure_live operation commands.lifetime in
    let* () = ensure_same_device operation value.command_buffer.queue.device commands.device in
    let* () = Indirect_command_buffer.validate_range operation commands ~location ~length in
    match pipeline_supports_icb operation value with
    | Error _ as error -> error
    | Ok false -> error operation Unsupported "pipeline lacks indirect-command-buffer support"
    | Ok true ->
        let* () =
          native_result operation
            (Metal_raw.render_encoder_execute_icb_range value.raw commands.raw location length)
        in
        retain_command_buffer_indirect value.command_buffer commands;
        Ok ()

  let draw_primitives (value : t) ~primitive ~first ~count ?(instances = 1) () =
    let operation = "Metal.Render_encoder.draw_primitives" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    if Option.is_none value.pipeline then
      error operation Invalid_state "no render pipeline is bound"
    else if first < 0 || count <= 0 || instances <= 0 then
      error operation Invalid_argument "draw range must be positive"
    else
      let code =
        match primitive with
        | Point -> 0
        | Line -> 1
        | Line_strip -> 2
        | Triangle -> 3
        | Triangle_strip -> 4
      in
      let* () =
        native_result operation
          (Metal_raw.Registry.render_encoder_draw_primitives value.raw (Int64.of_int code)
             (Int64.of_int first) (Int64.of_int count) (Int64.of_int instances))
      in
      Ok ()

  let draw_triangles (value : t) ~first ~count () =
    let instances = 1 in
    let operation = "Metal.Render_encoder.draw_triangles" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    if Option.is_none value.pipeline then
      error operation Invalid_state "no render pipeline is bound"
    else if first < 0 || count <= 0 || instances <= 0 then
      error operation Invalid_argument "draw range must be positive"
    else
      let* () =
        native_result operation
          (Metal_raw.Registry.render_encoder_draw_primitives value.raw
             3L (* MTLPrimitiveTypeTriangle *)
             (Int64.of_int first) (Int64.of_int count) (Int64.of_int instances))
      in
      Ok ()

  let end_encoding (value : t) =
    let operation = "Metal.Render_encoder.end_encoding" in
    let* () = before_main operation in
    let* () = ensure_live operation value.lifetime in
    let* () = native_result operation (Metal_raw.Registry.render_encoder_end value.raw) in
    if Atomic.compare_and_set value.lifetime.destroyed false true then begin
      ignore (Metal_raw.destroy value.raw);
      detach value.command_buffer.lifetime
    end;
    Ok ()
end

module Compute_encoder = struct
  type t = compute_encoder

  let create (command_buffer : Command_buffer.t) =
    on_main "Metal.Compute_encoder.create" (fun () ->
        let* () = ensure_live "Metal.Compute_encoder.create" command_buffer.lifetime in
        if command_buffer.phase <> Recording then
          error "Metal.Compute_encoder.create" Invalid_state "command buffer is no longer recording"
        else if dependent_count command_buffer.lifetime <> 0 then
          error "Metal.Compute_encoder.create" Invalid_state
            "command buffer already has an open encoder"
        else
          let* raw =
            native_result "Metal.Compute_encoder.create"
              (Metal_raw.Registry.command_buffer_compute_encoder command_buffer.raw)
          in
          let value : t = { raw; lifetime = lifetime (); command_buffer; pipeline = None } in
          attach command_buffer.lifetime;
          attach_finalizer value value.lifetime command_buffer.lifetime;
          Ok value)

  let set_pipeline (value : t) (pipeline : Compute_pipeline.t) =
    on_main "Metal.Compute_encoder.set_pipeline" (fun () ->
        let* () = ensure_live "Metal.Compute_encoder.set_pipeline" value.lifetime in
        let* () = ensure_live "Metal.Compute_encoder.set_pipeline" pipeline.lifetime in
        let* () =
          ensure_same_device "Metal.Compute_encoder.set_pipeline" value.command_buffer.queue.device
            pipeline.device
        in
        let* () =
          native_result "Metal.Compute_encoder.set_pipeline"
            (Metal_raw.compute_encoder_set_pipeline value.raw pipeline.raw)
        in
        value.pipeline <- Some pipeline;
        Ok ())

  let set_buffer (value : t) ~index ~offset (buffer : Buffer.t) =
    on_main "Metal.Compute_encoder.set_buffer" (fun () ->
        let* () = ensure_live "Metal.Compute_encoder.set_buffer" value.lifetime in
        let* () = ensure_buffer_usable "Metal.Compute_encoder.set_buffer" buffer in
        if index < 0 || index >= 31 then
          error "Metal.Compute_encoder.set_buffer" Invalid_argument
            "buffer index must be in [0, 31)"
        else if offset < 0L || offset > buffer.length then
          error "Metal.Compute_encoder.set_buffer" Invalid_argument
            "buffer offset is outside the resource"
        else
          let* () =
            ensure_same_device "Metal.Compute_encoder.set_buffer" value.command_buffer.queue.device
              buffer.device
          in
          let* () =
            native_result "Metal.Compute_encoder.set_buffer"
              (Metal_raw.compute_encoder_set_buffer value.raw buffer.raw offset index)
          in
          retain_command_buffer_buffer value.command_buffer buffer;
          Ok ())

  let set_texture (value : t) ~index (texture : Texture.t) =
    on_main "Metal.Compute_encoder.set_texture" (fun () ->
        let* () = ensure_live "Metal.Compute_encoder.set_texture" value.lifetime in
        let* () = ensure_texture_usable "Metal.Compute_encoder.set_texture" texture in
        if index < 0 || index >= 31 then
          error "Metal.Compute_encoder.set_texture" Invalid_argument
            "texture index must be in [0, 31)"
        else
          let* () =
            ensure_same_device "Metal.Compute_encoder.set_texture" value.command_buffer.queue.device
              texture.device
          in
          let* () =
            native_result "Metal.Compute_encoder.set_texture"
              (Metal_raw.compute_encoder_set_texture value.raw texture.raw index)
          in
          retain_command_buffer_texture value.command_buffer texture;
          Ok ())

  let set_acceleration_structure (value : t) ~index (x : Acceleration_structure.t option) =
    let operation = "Metal.Compute_encoder.set_acceleration_structure" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        if index < 0 || index >= 31 then
          error operation Invalid_argument "buffer index is outside [0, 31)"
        else
          match x with
          | Some a ->
              let* () = ensure_live operation a.lifetime in
              let* () = ensure_same_device operation value.command_buffer.queue.device a.device in
              let* () =
                native_result operation
                  (Metal_raw.compute35_acceleration value.raw (Some a.raw) (Int64.of_int index))
              in
              retain_command_buffer_acceleration_structure value.command_buffer a;
              Ok ()
          | None ->
              let* () =
                native_result operation
                  (Metal_raw.compute35_acceleration value.raw None (Int64.of_int index))
              in
              Ok ())

  let set_bytes (value : t) ~index bytes =
    let operation = "Metal.Compute_encoder.set_bytes" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        if index < 0 || index >= 31 || Bytes.length bytes = 0 then
          error operation Invalid_argument "byte binding index or length is invalid"
        else
          let* () =
            native_result operation
              (Metal_raw.compute35_bytes_plain value.raw bytes (Int64.of_int index))
          in
          Ok ())

  let use_acceleration_structures (value : t) structures =
    let operation = "Metal.Compute_encoder.use_acceleration_structures" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        if structures = [] then error operation Invalid_argument "structures must not be empty"
        else
          let device = value.command_buffer.queue.device in
          let rec check = function
            | [] -> Ok ()
            | (a : Acceleration_structure.t) :: xs ->
                let* () = ensure_live operation a.lifetime in
                let* () = ensure_same_device operation device a.device in
                check xs
          in
          let* () = check structures in
          let* () =
            native_result operation
              (Metal_raw.compute35_accelerations value.raw
                 (Array.of_list (List.map (fun (a : Acceleration_structure.t) -> a.raw) structures)))
          in
          List.iter (retain_command_buffer_acceleration_structure value.command_buffer) structures;
          Ok ())

  let positive_size (x, y, z) = x > 0 && y > 0 && z > 0

  let product3 x y z =
    if x > max_int / y then None
    else
      let xy = x * y in
      if xy > max_int / z then None else Some (xy * z)

  let dispatch_threads (value : t) ~threads ~threadgroup =
    on_main "Metal.Compute_encoder.dispatch_threads" (fun () ->
        let* () = ensure_live "Metal.Compute_encoder.dispatch_threads" value.lifetime in
        if Option.is_none value.pipeline then
          error "Metal.Compute_encoder.dispatch_threads" Invalid_state
            "no compute pipeline is bound"
        else if not (positive_size threads && positive_size threadgroup) then
          error "Metal.Compute_encoder.dispatch_threads" Invalid_argument
            "thread and threadgroup dimensions must be positive"
        else
          let tx, ty, tz = threadgroup in
          let pipeline = Option.get value.pipeline in
          match product3 tx ty tz with
          | None ->
              error "Metal.Compute_encoder.dispatch_threads" Invalid_argument
                "threadgroup cardinality overflows an OCaml integer"
          | Some product when product > pipeline.max_total_threads ->
              error "Metal.Compute_encoder.dispatch_threads" Invalid_argument
                "threadgroup exceeds the pipeline's maximum total thread count"
          | Some _ ->
              let* () =
                native_result "Metal.Compute_encoder.dispatch_threads"
                  (Metal_raw.compute_encoder_dispatch value.raw threads threadgroup)
              in
              Ok ())

  let end_encoding (value : t) =
    on_main "Metal.Compute_encoder.end_encoding" (fun () ->
        let* () = ensure_live "Metal.Compute_encoder.end_encoding" value.lifetime in
        let* () =
          native_result "Metal.Compute_encoder.end_encoding"
            (Metal_raw.Registry.compute_encoder_end value.raw)
        in
        if Atomic.compare_and_set value.lifetime.destroyed false true then begin
          ignore (Metal_raw.destroy value.raw);
          detach value.command_buffer.lifetime
        end;
        Ok ())
end

module Blit_encoder = struct
  type t = blit_encoder

  let create (command_buffer : Command_buffer.t) =
    on_main "Metal.Blit_encoder.create" (fun () ->
        let* () = ensure_live "Metal.Blit_encoder.create" command_buffer.lifetime in
        if command_buffer.phase <> Recording then
          error "Metal.Blit_encoder.create" Invalid_state "command buffer is no longer recording"
        else if dependent_count command_buffer.lifetime <> 0 then
          error "Metal.Blit_encoder.create" Invalid_state
            "command buffer already has an open encoder"
        else
          let* raw =
            native_result "Metal.Blit_encoder.create"
              (Metal_raw.Registry.command_buffer_blit_encoder command_buffer.raw)
          in
          let value : t = { raw; lifetime = lifetime (); command_buffer } in
          attach command_buffer.lifetime;
          attach_finalizer value value.lifetime command_buffer.lifetime;
          Ok value)

  let copy_buffer_to_texture (value : t) ~(source : Buffer.t) ~source_offset ~source_bytes_per_row
      ~source_bytes_per_image ~(destination : Texture.t) ~destination_slice ~destination_level
      ~(destination_region : Texture.region) =
    let operation = "Metal.Blit_encoder.copy_buffer_to_texture" in
    on_main operation (fun () ->
        let* () = ensure_live operation value.lifetime in
        let* () = ensure_buffer_usable operation source in
        let* () = ensure_texture_usable operation destination in
        let descriptor = destination.descriptor in
        if source_offset < 0L || source_bytes_per_row <= 0 || source_bytes_per_image <= 0 then
          error operation Invalid_argument
            "blit source offset and pitches must be nonnegative and positive"
        else if descriptor.sample_count <> 1 then
          error operation Unsupported "multisample textures do not accept buffer blits"
        else if destination_level < 0 || destination_level >= descriptor.mip_levels then
          error operation Invalid_argument "blit destination mip level is outside the texture"
        else if destination_slice < 0 || destination_slice >= Texture.total_slices descriptor then
          error operation Invalid_argument "blit destination slice is outside the texture"
        else if
          destination_region.x < 0 || destination_region.y < 0 || destination_region.z < 0
          || destination_region.width <= 0 || destination_region.height <= 0
          || destination_region.depth <= 0
        then error operation Invalid_argument "blit destination region is invalid"
        else
          let mip_width = Texture.mip_dimension descriptor.width destination_level
          and mip_height = Texture.mip_dimension descriptor.height destination_level
          and mip_depth = Texture.mip_dimension descriptor.depth destination_level in
          if
            destination_region.x > mip_width
            || destination_region.width > mip_width - destination_region.x
            || destination_region.y > mip_height
            || destination_region.height > mip_height - destination_region.y
            || destination_region.z > mip_depth
            || destination_region.depth > mip_depth - destination_region.z
          then
            error operation Invalid_argument
              "blit destination region exceeds the selected mip level"
          else
            let layout = Texture.format_layout descriptor.format in
            let aligned origin length limit block =
              origin mod block = 0 && (length mod block = 0 || origin + length = limit)
            in
            if
              not
                (aligned destination_region.x destination_region.width mip_width layout.block_width
                && aligned destination_region.y destination_region.height mip_height
                     layout.block_height)
            then
              error operation Invalid_argument "blit destination region is not format-block aligned"
            else
              let blocks value block = 1 + ((value - 1) / block) in
              let row_blocks = blocks destination_region.width layout.block_width in
              match Texture.checked_mul row_blocks layout.bytes_per_block with
              | None ->
                  error operation Invalid_argument "blit row cardinality overflows an OCaml integer"
              | Some minimum_row
                when source_bytes_per_row < minimum_row
                     || source_bytes_per_row mod layout.bytes_per_block <> 0 ->
                  error operation Invalid_argument
                    "blit source row pitch is too small or not block-aligned"
              | Some _ -> (
                  match
                    Texture.checked_mul source_bytes_per_row
                      (blocks destination_region.height layout.block_height)
                  with
                  | None ->
                      error operation Invalid_argument
                        "blit image cardinality overflows an OCaml integer"
                  | Some minimum_image when source_bytes_per_image < minimum_image ->
                      error operation Invalid_argument
                        "blit source image pitch is smaller than its rows"
                  | Some _ ->
                      let image_bytes = Int64.of_int source_bytes_per_image
                      and depth = Int64.of_int destination_region.depth in
                      if image_bytes > Int64.div Int64.max_int depth then
                        error operation Invalid_argument "blit source cardinality overflows 64 bits"
                      else
                        let total = Int64.mul image_bytes depth in
                        if
                          source_offset > source.length
                          || total > Int64.sub source.length source_offset
                        then error operation Invalid_argument "blit source range exceeds the buffer"
                        else
                          let* () =
                            ensure_same_device operation value.command_buffer.queue.device
                              source.device
                          in
                          let* () =
                            ensure_same_device operation value.command_buffer.queue.device
                              destination.device
                          in
                          let copy =
                            ( source_offset,
                              source_bytes_per_row,
                              source_bytes_per_image,
                              ( destination_region.width,
                                destination_region.height,
                                destination_region.depth ),
                              destination_slice,
                              destination_level,
                              (destination_region.x, destination_region.y, destination_region.z) )
                          in
                          let* () =
                            native_result operation
                              (Metal_raw.blit_encoder_copy_buffer_to_texture value.raw source.raw
                                 destination.raw copy)
                          in
                          retain_command_buffer_buffer value.command_buffer source;
                          retain_command_buffer_texture value.command_buffer destination;
                          Ok ()))

  let end_encoding (value : t) =
    on_main "Metal.Blit_encoder.end_encoding" (fun () ->
        let* () = ensure_live "Metal.Blit_encoder.end_encoding" value.lifetime in
        let* () =
          native_result "Metal.Blit_encoder.end_encoding"
            (Metal_raw.Registry.blit_encoder_end value.raw)
        in
        if Atomic.compare_and_set value.lifetime.destroyed false true then begin
          ignore (Metal_raw.destroy value.raw);
          detach value.command_buffer.lifetime
        end;
        Ok ())
end
