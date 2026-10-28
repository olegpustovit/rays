(** Ownership-aware bindings to Metal.framework on macOS. *)

(** Registry-generated enum values used by the safe Metal API. *)
module Enum : module type of Metal_gen.Enum

(** Typed Metal data-type values, including packed formats and resource kinds. *)
module Data_type : module type of Metal_gen.Enum.Mtl_data_type
module Feature_set : module type of Metal_gen.Enum.Mtl_feature_set

type error_kind =
  | Native_error
  | Wrong_domain
  | Destroyed
  | Parent_has_dependents
  | Invalid_argument
  | Invalid_state
  | Unsupported
  | Device_mismatch

type error = private
  { operation : string
  ; kind : error_kind
  ; message : string
  }

type acceleration_structure
type indirect_command_buffer
type render_pipeline
type compute_pipeline
type depth_stencil

type purgeable_state =
  | Nonvolatile
  | Volatile
  | Empty

val pp_error : Format.formatter -> error -> unit

module Release_queue : sig
  type stats =
    { pending : int
    ; live_handles : int
    ; total_created : int64
    ; total_released : int64
    ; external_deallocations : int64
    ; external_deallocation_mismatches : int64
    ; resident_bytes : int64
    }

  val drain : unit -> (int, error) result
  val stats : unit -> (stats, error) result
end

type device

type compute_encoder
type acceleration_encoder
type blit_encoder
type command_queue
module Device : sig
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

  type info =
    { name : string
    ; registry_id : int64
    ; low_power : bool
    ; removable : bool
    ; headless : bool
    ; unified_memory : bool
    ; recommended_max_working_set_size : int64
    ; current_allocated_size : int64
    ; max_buffer_length : int64
    ; max_threadgroup_memory_length : int64
    ; raytracing : bool
    ; raytracing_from_render : bool
    ; dynamic_libraries : bool
    }

  val system_default : unit -> (t, error) result
  val same : t -> t -> bool
  val info : t -> (info, error) result
  val supports_family : t -> family -> (bool, error) result
  val supports_texture_sample_count : t -> int -> (bool, error) result

  val destroy : t -> (unit, error) result
end

module Buffer : sig
  type t

  type storage_mode =
    | Shared
    | Managed
    | Private

  type cpu_cache_mode =
    | Default_cache
    | Write_combined

  type hazard_tracking_mode =
    | Default_hazard_tracking
    | Untracked
    | Tracked

  val create :
    device:Device.t -> length:int64 -> storage:storage_mode ->
    ?cpu_cache:cpu_cache_mode -> ?label:string -> unit -> (t, error) result

  val length : t -> int64

  val write_bytes :
    t -> dst_offset:int64 -> bytes -> (unit, error) result
  val read_bytes : t -> offset:int64 -> length:int -> (bytes, error) result

  val destroy : t -> (unit, error) result
end

module Acceleration_structure : sig
  type t

  type sizes =
    { acceleration_structure_size : int64
    ; build_scratch_buffer_size : int64
    ; refit_scratch_buffer_size : int64
    }

  type structure = t

  (** Generic build descriptors. A geometry lists one keyframe for a
      static structure or exactly [motion.keyframe_count] keyframes; ranges are
      validated against their buffers, and the descriptor keeps every buffer and
      instanced structure alive until it is destroyed. *)
  module Build : sig
    type keyframe = { buffer : Buffer.t; offset : int64 }
    type border = Clamp | Vanish
    type motion =
      { keyframe_count : int; start_time : float; end_time : float
      ; start_border : border; end_border : border }
    type index = { index_buffer : Buffer.t; index_offset : int64; index_uint16 : bool }
    type common =
      { opaque : bool; allow_duplicate_intersection : bool }
    val default_common : common
    type curve_type = Round | Flat
    type curve_basis = Bspline | Catmull_rom | Linear | Bezier
    type end_caps = No_caps | Disk | Sphere
    type geometry =
      | Triangles of
          { vertices : keyframe list; vertex_stride : int64; triangle_count : int64
          ; index : index option; common : common }
      | Bounding_boxes of
          { boxes : keyframe list; stride : int64; count : int64; common : common }
      | Curves of
          { control_points : keyframe list; control_stride : int64; control_point_count : int64
          ; radii : keyframe list; radius_stride : int64; index : index
          ; segment_count : int64; control_points_per_segment : int
          ; curve_type : curve_type; basis : curve_basis; end_caps : end_caps
          ; common : common }
    type instance_kind = User_id_instances | Motion_instances

    (** Byte offsets inside one native instance record; [-1] where the kind
        lacks the field. [transform] is a packed 4x3 (48 bytes), [user_id] and
        the motion fields exist for the user-id and motion kinds. *)
    type instance_layout =
      { size : int; transform : int; options : int; mask : int
      ; structure_index : int; user_id : int; transforms_start : int; transforms_count : int
      ; start_border_offset : int; end_border_offset : int; start_time_offset : int
      ; end_time_offset : int }
    val instance_layout : instance_kind -> instance_layout
    type t
    val primitive : Device.t -> ?motion:motion -> geometry list ->
      (t, error) result

    (** [motion_transforms] is (buffer, offset, count) of packed 4x3 keyframe
        transforms and is required for [Motion_instances]. *)
    val instances : Device.t -> buffer:Buffer.t -> ?offset:int64 -> count:int64 -> kind:instance_kind -> ?motion_transforms:(Buffer.t * int64 * int64) ->
      structure array -> (t, error) result
    val sizes : device:Device.t -> t -> (sizes, error) result

    val instance_kind : t -> instance_kind option
    val destroy : t -> (unit, error) result
  end

  val create : device:Device.t -> size:int64 -> (t, error) result
  val destroy : t -> (unit, error) result
end

module Texture : sig
  type t

  type kind =
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

  type format =
    | R8_unorm
    | Rgba8_unorm
    | Bgra8_unorm
    | Bgra8_unorm_srgb
    | Rgba16_float
    | Rgba32_float
    | Depth32_float
  type format_layout =
    { block_width : int
    ; block_height : int
    ; bytes_per_block : int
    }

  type cpu_cache_mode =
    | Default_cache
    | Write_combined

  type hazard_tracking_mode =
    | Default_hazard_tracking
    | Untracked
    | Tracked

  type usage =
    | Shader_read
    | Shader_write
    | Render_target
    | Pixel_format_view
    | Shader_atomic

  type compression_type =
    | Lossless
    | Lossy

  type swizzle_channel =
    | Zero
    | One
    | Red
    | Green
    | Blue
    | Alpha

  type swizzle =
    { red : swizzle_channel
    ; green : swizzle_channel
    ; blue : swizzle_channel
    ; alpha : swizzle_channel
    }

  type descriptor =
    { kind : kind
    ; format : format
    ; width : int
    ; height : int
    ; depth : int
    ; mip_levels : int
    ; sample_count : int
    ; array_length : int
    ; storage : Buffer.storage_mode
    ; cpu_cache : cpu_cache_mode
    ; hazard_tracking : hazard_tracking_mode
    ; usage : usage list
    ; allow_gpu_optimized_contents : bool
    ; compression : compression_type
    ; swizzle : swizzle
    ; label : string option
    }

  type region =
    { x : int
    ; y : int
    ; z : int
    ; width : int
    ; height : int
    ; depth : int
    }

  val descriptor_2d :
    ?storage:Buffer.storage_mode -> ?usage:usage list ->
    ?label:string ->
    format:format -> width:int -> height:int -> unit -> descriptor
  val create : device:Device.t -> descriptor -> (t, error) result

  val device : t -> Device.t
  val descriptor : t -> descriptor

  val destroyed : t -> bool
  val read_bytes :
    t -> region:region -> mip_level:int -> slice:int -> bytes_per_row:int ->
    bytes_per_image:int -> (bytes, error) result
  (* Read directly into caller-owned storage.  The destination must have the
     exact pitched size of the requested region. *)
  val read_bytes_into :
    t -> region:region -> mip_level:int -> slice:int -> bytes_per_row:int ->
    bytes_per_image:int -> destination:bytes -> (unit, error) result
  val destroy : t -> (unit, error) result
end

module Metal_layer : sig
  type t
  type config =
    { width:int; height:int; format:Texture.format; framebuffer_only:bool
    ; maximum_drawables:int; allows_timeout:bool; display_sync:bool
    ; presents_with_transaction:bool }
  val default : width:int -> height:int -> config
  val adopt_borrowed : Device.t -> Native_layer_token.t -> config -> (t,error) result
  val configure : t -> config -> (unit,error) result
  val device : t -> Device.t
  val destroy : t -> (unit,error) result
end

module Drawable : sig
  type t
  type loss = Timeout_or_unavailable
  val acquire : Metal_layer.t -> ((t,loss) result,error) result
  val texture : t -> (Texture.t,error) result
  val destroy : t -> (unit,error) result
    (** Acquires an explicitly-owned drawable without an OCaml finalizer.
        Destroy it on every exit, after destroying its texture wrapper.  A
        drawable scheduled on a retained-reference command buffer remains
        natively owned through terminal presentation. *)
    val acquire_scoped : Metal_layer.t -> ((t,loss) result,error) result
    (* Returns the explicitly-owned drawable texture wrapper without an OCaml
        finalizer.  Use this with [acquire_scoped], and destroy the texture
        before destroying its drawable. *)
    val texture_scoped : t -> (Texture.t,error) result
end

module Render_pass_descriptor : sig
  type t
  type color_load_action = Load_dont_care | Load | Clear
  val create : width:int -> height:int -> ?sample_count:int -> unit -> (t,error) result
  val set_resolve_texture : t -> Texture.t option -> (unit,error) result
  (** [store] keeps the attachment's contents after the pass; [resolve] writes
      the resolve texture (multisampled passes only). *)
  val set_color_store_action : t -> store:bool -> resolve:bool -> (unit,error) result
  val set_color_load_action : t -> color_load_action -> (unit,error) result
  type store_action = Store_dont_care | Store

  (** Load/store actions and clear values for the depth and stencil
      attachments set by [set_attachments]; absent attachments are left
      untouched. *)
  val set_depth_stencil_actions : t -> depth:(color_load_action * store_action * float) ->
    stencil:(color_load_action * store_action * int) -> (unit,error) result
  val set_attachments :
    t -> color:Texture.t -> ?clear:float * float * float * float ->
    ?depth:Texture.t -> ?stencil:Texture.t -> unit -> (unit,error) result
  val destroy : t -> (unit,error) result
end

module Sampler : sig
  type t
  type filter = Nearest | Linear
  type mip_filter = Not_mipmapped | Mip_nearest | Mip_linear
  type address_mode =
    | Clamp_to_edge
    | Mirror_clamp_to_edge
    | Repeat
    | Mirror_repeat
    | Clamp_to_zero
    | Clamp_to_border_color

  type border_color = Transparent_black | Opaque_black | Opaque_white
  type reduction_mode = Weighted_average | Minimum | Maximum
  type compare_function =
    | Never
    | Less
    | Equal
    | Less_equal
    | Greater
    | Not_equal
    | Greater_equal
    | Always

  type descriptor =
    { min_filter : filter
    ; mag_filter : filter
    ; mip_filter : mip_filter
    ; max_anisotropy : int
    ; s_address : address_mode
    ; t_address : address_mode
    ; r_address : address_mode
    ; border_color : border_color
    ; reduction_mode : reduction_mode
    ; normalized_coordinates : bool
    ; lod_min_clamp : float
    ; lod_max_clamp : float
    ; lod_average : bool
    ; lod_bias : float
    ; compare_function : compare_function
    ; support_argument_buffers : bool
    ; label : string option
    }

  val default : ?label:string -> unit -> descriptor
  val create : device:Device.t -> descriptor -> (t, error) result
  val destroy : t -> (unit, error) result

end

module Depth_stencil : sig
  type t
  type compare_function =
    | Never
    | Less
    | Equal
    | Less_equal
    | Greater
    | Not_equal
    | Greater_equal
    | Always

  type operation =
    | Keep
    | Zero
    | Replace
    | Increment_clamp
    | Decrement_clamp
    | Invert
    | Increment_wrap
    | Decrement_wrap

  type face = private
    { compare : compare_function
    ; stencil_fail : operation
    ; depth_fail : operation
    ; pass : operation
    ; read_mask : int32
    ; write_mask : int32
    }

  (** Creates immutable depth-test state. The default is always-pass with depth
      writes disabled and with stencil testing disabled for both faces. *)
  val create :
    ?label:string -> ?depth_compare:compare_function -> ?depth_write:bool ->
    ?front_face:face -> ?back_face:face -> Device.t -> unit ->
    (t, error) result

  val destroy : t -> (unit, error) result

  module Private : sig
  end
end

module Shader_type : sig
  type scalar =
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

  type t =
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
end

module Reflection : module type of Metal_argument_reflection_snapshot

module Binding : sig
  type access =
    | Read_only
    | Read_write
    | Write_only
    | Unknown_access of int

  type buffer =
    { alignment : int64
    ; data_size : int64
    ; data_type : Shader_type.t
    }

  type texture =
    { texture_kind : Texture.kind
    ; data_type : Shader_type.t
    ; depth : bool
    ; array_length : int64
    }

  type sized =
    { alignment : int64
    ; data_size : int64
    }

  type kind =
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

  type t =
    { name : string
    ; index : int64
    ; access : access
    ; used : bool
    ; argument : bool
    ; kind : kind
    ; reflection : Reflection.reflected_type option
    }

end

module Library : sig
  type t

  val compile_source :
    ?label:string -> device:Device.t -> string -> (t, error) result

  (* Calls the deprecated path-based SDK constructor exactly. New code should
     prefer [load_file], which uses the URL-based constructor. *)

  val destroy : t -> (unit, error) result
end

module rec Function : sig
  type t

  type kind =
    | Vertex
    | Fragment
    | Kernel
    | Visible
    | Intersection
    | Mesh
    | Object
    | Unknown_function_kind of int

  type constant_value =
    | Bool_constant of bool
    | Int8_constant of int
    | Uint8_constant of int
    | Int16_constant of int
    | Uint16_constant of int
    | Int32_constant of int32
    | Uint32_constant of int64
    | Int64_constant of int64
    (** The [int64] bit pattern is reinterpreted as an unsigned Metal value. *)
    | Uint64_bits_constant of int64
    | Float16_constant of float
    | Float32_constant of float

  val find : library:Library.t -> string -> (t, error) result
  val specialize :
    library:Library.t -> constants:(string * constant_value) list -> string -> (t, error) result
  val argument_encoder : t -> buffer_index:int64 -> (Shader_argument_encoder.t,error) result
  val destroy : t -> (unit, error) result
end

and Shader_argument_encoder : sig
  type t
  type resource =
    | Buffer of Buffer.t | Texture of Texture.t | Sampler of Sampler.t
    | Acceleration_structure of acceleration_structure
    | Indirect_command_buffer of indirect_command_buffer
    | Render_pipeline of render_pipeline
    | Compute_pipeline of compute_pipeline | Depth_stencil of depth_stencil
  val encoded_length : t -> int64
  val alignment : t -> int64
  val set : t -> index:int64 -> resource -> (unit,error) result
  val set_argument_buffer : t -> Buffer.t -> offset:int64 -> unit -> (unit,error) result
  val destroy : t -> (unit,error) result
end

module Compute_pipeline : sig
  type t

  val create :
    ?label:string ->
    ?reflection:bool -> Function.t ->
    (t, error) result
  val bindings : t -> Binding.t list option
  val max_total_threads : t -> int
  (** [maxTotalThreadsPerThreadgroup]: Metal lowers it for a kernel that needs more registers. *)
  val destroy : t -> (unit, error) result
end

module Vertex_descriptor : sig

end

module Render_pipeline : sig
  type t

  type primitive_topology =
    | Point
    | Line
    | Triangle

  (** [Identity] maps logical color outputs to the same physical indices.
      [Inherited] takes the mapping from the render encoder at draw time. *)
  type color_attachment_mapping =
    | Identity
    | Inherited

  type blend_state =
    | Blend_disabled
    | Blend_enabled

  type blend_factor =
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

  type blend_operation =
    | Blend_add
    | Blend_subtract
    | Blend_reverse_subtract
    | Blend_min
    | Blend_max

  type color_write =
    | Write_red
    | Write_green
    | Write_blue
    | Write_alpha

  type color_attachment = private
    { format : Texture.format
    ; blending : blend_state
    ; source_rgb : blend_factor
    ; destination_rgb : blend_factor
    ; rgb_operation : blend_operation
    ; source_alpha : blend_factor
    ; destination_alpha : blend_factor
    ; alpha_operation : blend_operation
    ; write_mask : color_write list
    }

  (** Builds one concrete color-attachment pipeline policy. *)
  val color_attachment :
    ?blending:blend_state -> ?source_rgb:blend_factor ->
    ?destination_rgb:blend_factor -> ?rgb_operation:blend_operation ->
    ?source_alpha:blend_factor -> ?destination_alpha:blend_factor ->
    ?alpha_operation:blend_operation -> ?write_mask:color_write list ->
    Texture.format -> color_attachment

  type reflection =
    { vertex : Binding.t list
    ; fragment : Binding.t list
    }

  val supports_indirect_command_buffers : t -> (bool,error) result
  val reflection : t -> reflection option
  val destroy : t -> (unit, error) result
end

module Compiler : sig
  type t

  (** Creates a synchronous Metal 4 compiler. *)
  val create :
    Device.t ->
    (t, error) result

  (** Compiles a conventional vertex/fragment Metal 4 render pipeline.
      Rasterized pipelines require a fragment function and one to eight color
      formats or typed color attachments, but not both. Vertex-only pipelines
      use a void-returning vertex function, disable rasterization, and use no
      color attachments. *)
  val create_render_pipeline :
    ?label:string -> ?fragment:string -> ?reflection:bool ->
    ?raster_sample_count:int -> ?color_formats:Texture.format list ->
    ?color_attachments:Render_pipeline.color_attachment list ->
    ?primitive_topology:Render_pipeline.primitive_topology ->
    ?support_indirect_command_buffers:bool ->
    t -> library:Library.t ->
    vertex:string -> (Render_pipeline.t, error) result

  val destroy : t -> (unit, error) result
end

(** Handle for indirect commands recorded through the legacy Metal command
    encoders below. *)
type indirect_command_buffer_handle

module Indirect_command_buffer : sig
  type command_type =
    | Indirect_draw
    | Indirect_draw_indexed
    | Indirect_concurrent_dispatch
    | Indirect_concurrent_dispatch_threads

  type descriptor
  type t = indirect_command_buffer_handle
  type buffer = t

  val descriptor :
    ?inherit_buffers:bool -> ?inherit_pipeline_state:bool ->
    ?max_vertex_buffer_bind_count:int -> ?max_fragment_buffer_bind_count:int ->
    command_types:command_type list ->
    unit -> descriptor

  val create :
    device:Device.t -> max_command_count:int ->
    descriptor -> (t, error) result
  val destroy : t -> (unit, error) result

  module Render_command : sig
    type t
    type primitive = Point | Line | Line_strip | Triangle | Triangle_strip
    type index_type = Uint16 | Uint32
    val at : buffer -> int -> (t, error) result
    val set_pipeline : t -> Render_pipeline.t -> (unit, error) result
    val set_vertex_buffer : t -> index:int -> offset:int64 -> Buffer.t -> (unit, error) result
    val set_fragment_buffer : t -> index:int -> offset:int64 -> Buffer.t -> (unit, error) result
    val draw_indexed : t -> primitive:primitive -> index_type:index_type ->
      index_buffer:Buffer.t -> index_offset:int64 -> index_count:int64 ->
      ?instance_count:int64 -> unit -> (unit,error) result
    val draw_primitives : t -> primitive:primitive -> vertex_start:int ->
      vertex_count:int -> ?instance_count:int -> unit ->
      (unit, error) result
    val destroy : t -> (unit, error) result
  end

end

module Command_queue : sig
  type t = command_queue
  val create : Device.t -> (t, error) result
  val destroy : t -> (unit, error) result
end

module Command_buffer : sig
  type t
  type present_time = Immediate | At_time of float | After_minimum_duration of float
  type diagnostics =
    { error_options:int64; gpu_start_time:float; gpu_end_time:float
    ; kernel_start_time:float; kernel_end_time:float; retained_references:bool }

  type status =
    | Not_enqueued
    | Enqueued
    | Committed
    | Scheduled
    | Completed
    | Error of string
    | Unknown of int

  val create : Command_queue.t -> unit -> (t, error) result
  val device : t -> Device.t
  val status : t -> (status, error) result
  val diagnostics : t -> (diagnostics,error) result

  val present :
    t -> Drawable.t -> unit -> (unit, error) result
  val commit : t -> (unit, error) result
  val wait_until_completed : t -> (unit, error) result
  val destroyed : t -> bool
  val destroy : t -> (unit, error) result

  module Private : sig

  end
end

module Acceleration_encoder : sig
  type t = acceleration_encoder
  type compacted_size_type = Uint32 | Uint64

  val create : Command_buffer.t -> (t, error) result

  (** Build or refit through a generic descriptor; the destination(s) must fit
      the descriptor's sizes and the scratch range its build/refit scratch. *)
  val build_with :
    t -> destination:Acceleration_structure.t -> descriptor:Acceleration_structure.Build.t ->
    scratch:Buffer.t -> scratch_offset:int64 -> (unit, error) result
  val copy_and_compact :
    t -> source:Acceleration_structure.t ->
    destination:Acceleration_structure.t -> (unit, error) result
  val write_compacted_size_typed : t -> source:Acceleration_structure.t -> destination:Buffer.t -> offset:int64 -> compacted_size_type -> (unit,error) result
  val end_encoding : t -> (unit, error) result

end

module Compute_encoder : sig
  type t = compute_encoder

  val create : Command_buffer.t -> (t, error) result
  val set_pipeline : t -> Compute_pipeline.t -> (unit, error) result
  val set_buffer :
    t -> index:int -> offset:int64 -> Buffer.t -> (unit, error) result
  val set_texture : t -> index:int -> Texture.t -> (unit, error) result
  val set_acceleration_structure : t -> index:int -> Acceleration_structure.t option -> (unit,error) result
  val set_bytes : t -> index:int -> bytes -> (unit,error) result

  (** [useResource:usage:MTLResourceUsageRead] on each structure: an instance
      structure bound with [set_acceleration_structure] only references its
      bottom-level structures, which must be declared here to stay resident
      for the dispatch. Rejects an empty list, a destroyed structure, or one
      from another device. *)
  val use_acceleration_structures : t -> Acceleration_structure.t list -> (unit,error) result

  val dispatch_threads :
    t -> threads:int * int * int -> threadgroup:int * int * int ->
    (unit, error) result
  val end_encoding : t -> (unit, error) result
end

module Render_encoder : sig
  type t

  type cull_mode = No_cull | Cull_front | Cull_back
  type winding = Clockwise | Counter_clockwise
  type stage = Vertex | Fragment
  type resource_usage = Read | Write | Sample
  type resource = Buffer_resource of Buffer.t | Texture_resource of Texture.t
  type primitive = Point | Line | Line_strip | Triangle | Triangle_strip
  type index_type = Uint16 | Uint32
  type viewport =
    { x : float; y : float; width : float; height : float
    ; znear : float; zfar : float }
  type scissor = { x : int; y : int; width : int; height : int }

  val create :
    Command_buffer.t -> target:Texture.t ->
    unit -> (t, error) result
  val set_pipeline : t -> Render_pipeline.t -> (unit, error) result
  val set_vertex_buffer :
    t -> index:int -> offset:int64 -> Buffer.t -> (unit, error) result
  val set_fragment_buffer :
    t -> index:int -> offset:int64 -> Buffer.t -> (unit, error) result
  val set_vertex_texture : t -> index:int -> Texture.t -> (unit, error) result
  val set_fragment_texture : t -> index:int -> Texture.t -> (unit, error) result
  val set_vertex_sampler :
    t -> index:int -> Sampler.t ->
    (unit, error) result
  val set_fragment_sampler :
    t -> index:int -> Sampler.t ->
    (unit, error) result
  val set_viewport : t -> viewport -> (unit, error) result
  val set_scissor : t -> scissor -> (unit, error) result
  val set_cull_mode : t -> cull_mode -> (unit, error) result
  val set_front_facing_winding : t -> winding -> (unit, error) result
  val use_resources : t -> resource list -> usage:resource_usage list -> stages:stage list -> (unit,error) result
  val set_depth_stencil_state : t -> Depth_stencil.t option -> (unit,error) result
  val draw_indexed_basic : t -> primitive:primitive -> index_type:index_type -> index_buffer:Buffer.t -> index_offset:int64 -> index_count:int64 -> (unit,error) result
  val draw_indexed_instances : t -> primitive:primitive -> index_type:index_type -> index_buffer:Buffer.t -> index_offset:int64 -> index_count:int64 -> instances:int64 -> (unit,error) result
  val draw_indexed_indirect : t -> primitive:primitive -> index_type:index_type -> index_buffer:Buffer.t ->
    index_offset:int64 -> arguments:Buffer.t -> arguments_offset:int64 -> (unit,error) result
  (** Index count, instance count, first index, base vertex and base instance are five uints the
      device wrote at [arguments_offset] of [arguments]; both buffers stay retained with the
      command buffer. *)
  val execute_indirect_commands : t -> Indirect_command_buffer.t -> location:int -> length:int -> (unit,error) result

  val draw_triangles :
    t -> first:int -> count:int -> unit ->
    (unit, error) result
  val draw_primitives :
    t -> primitive:primitive -> first:int -> count:int -> ?instances:int -> unit ->
    (unit, error) result
  val end_encoding : t -> (unit, error) result
  val destroyed : t -> bool

    type prepared_indexed_binding=
      { prepared_stage:stage
      ; prepared_index:int
      ; prepared_offset:int64
      ; prepared_buffer:Buffer.t }
    type prepared_indexed_draw=
      { prepared_pipeline:Render_pipeline.t
      ; prepared_bindings:prepared_indexed_binding array
      ; prepared_primitive:primitive
      ; prepared_index_type:index_type
      ; prepared_index_buffer:Buffer.t
      ; prepared_index_offset:int64
      ; prepared_index_count:int64 }
    type prepared_indexed_draws

    (* Creates an explicitly-ended pass encoder without an OCaml finalizer. *)
    val create_from_pass_scoped :
      Command_buffer.t -> Render_pass_descriptor.t -> (t, error) result
    val prepare_indexed_draws :
      Device.t -> prepared_indexed_draw array ->
      (prepared_indexed_draws,error) result
    val execute_prepared_indexed_draws :
      t -> prepared_indexed_draws -> (unit,error) result
end

module Blit_encoder : sig
  type t = blit_encoder

  val create : Command_buffer.t -> (t, error) result
  val copy_buffer_to_texture :
    t -> source:Buffer.t -> source_offset:int64 -> source_bytes_per_row:int ->
    source_bytes_per_image:int -> destination:Texture.t ->
    destination_slice:int -> destination_level:int ->
    destination_region:Texture.region -> (unit, error) result
  val end_encoding : t -> (unit, error) result
end
