(* The bridge's operations (vulkan_bridge.c). A handle is a C pointer as an int, a negative int a
   VkResult. Backend types stay here and in lib/ogpu_vulkan. *)

type handle = int
type floats = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

let no_device = -100000
let missing_feature = -100001
let error_device_lost = -4
let error_out_of_host_memory = -1
let error_out_of_device_memory = -2

external device_create : bool -> bool -> int = "rv_device_create"
external device_destroy : handle -> unit = "rv_device_destroy"
external validation_messages : unit -> int = "rv_validation_messages"

external device_facts : handle -> int * int * int * int * int * int = "rv_device_facts"
(** max buffer bytes, max texture dimension, max samples, max invocations, device-local bytes, api *)

external device_memory : handle -> int * int = "rv_device_memory"
external queue_create : handle -> int = "rv_queue_create"
external queue_destroy : handle -> unit = "rv_queue_destroy"
external queue_completed : handle -> int = "rv_queue_completed"
external queue_wait : handle -> int -> int = "rv_queue_wait"
external queue_poll : handle -> unit = "rv_queue_poll"
external queue_duration : handle -> int -> float = "rv_queue_duration"
external queue_total_seconds : handle -> float = "rv_queue_total_seconds"
external commands_begin : handle -> int = "rv_commands_begin"
external commands_commit : handle -> int = "rv_commands_commit"
external commands_abandon : handle -> unit = "rv_commands_abandon"
external buffer_create : handle -> int -> int -> int = "rv_buffer_create"
external buffer_zero : handle -> int = "rv_buffer_zero"
external buffer_destroy : handle -> unit = "rv_buffer_destroy"
external buffer_write : handle -> int -> bytes -> int = "rv_buffer_write"
external buffer_read : handle -> int -> bytes -> int -> int -> int = "rv_buffer_read"
external buffer_flush : handle -> floats -> int -> int = "rv_buffer_flush"
external buffer_invalidate : handle -> floats -> int -> int = "rv_buffer_invalidate"
external shader_create : handle -> bytes -> int = "rv_shader_create"
external shader_destroy : handle -> handle -> unit = "rv_shader_destroy"

external compute_pipeline_create : handle -> handle -> string -> (int * int) array -> int
  = "rv_compute_pipeline_create"

external render_pipeline_create : handle -> handle -> handle -> (int * int) array -> int array -> int
  = "rv_render_pipeline_create"

external pipeline_destroy : handle -> unit = "rv_pipeline_destroy"
external set_pipeline : handle -> handle -> unit = "rv_cmd_set_pipeline"
external set_buffer : handle -> int -> int -> handle -> int = "rv_cmd_set_buffer"
external set_bytes : handle -> int -> bytes -> int = "rv_cmd_set_bytes"
external set_texture : handle -> int -> handle -> int = "rv_cmd_set_texture"
external set_sampler : handle -> int -> handle -> int = "rv_cmd_set_sampler"
external compute_begin : handle -> unit = "rv_compute_begin"
external dispatch : handle -> int -> int -> int -> unit = "rv_compute_dispatch"
external dispatch_indirect : handle -> handle -> int -> unit = "rv_compute_dispatch_indirect"
external encoder_end : handle -> unit = "rv_encoder_end"
external texture_create : handle -> int array -> int = "rv_texture_create"
external texture_destroy : handle -> unit = "rv_texture_destroy"
external texture_read : handle -> bytes -> int -> int -> int = "rv_texture_read"

external blit_buffer_to_texture : handle -> handle -> handle -> int array -> unit
  = "rv_blit_buffer_to_texture"

external sampler_create : handle -> int array -> float -> float -> int = "rv_sampler_create"
external sampler_destroy : handle -> handle -> unit = "rv_sampler_destroy"

external render_begin :
  handle -> (int array * float array) array -> (int array * float) option -> int * int -> int
  = "rv_render_begin"

external render_end : handle -> unit = "rv_render_end"
external render_viewport : handle -> int * int * int * int -> unit = "rv_render_viewport"
external render_scissor : handle -> int * int * int * int -> unit = "rv_render_scissor"
external render_cull : handle -> int -> unit = "rv_render_cull"
external render_winding : handle -> bool -> unit = "rv_render_winding"
external render_depth : handle -> int -> bool -> unit = "rv_render_depth"
external render_draw : handle -> int -> int -> int -> int -> unit = "rv_render_draw"
external render_draw_indexed : handle -> handle -> int array -> unit = "rv_render_draw_indexed"

external render_draw_indexed_indirect : handle -> handle -> handle -> int array -> unit
  = "rv_render_draw_indexed_indirect"

external surface_create : handle -> Native_layer_token.t -> int64 -> int64 -> bool -> int
  = "rv_surface_create"

external surface_configure : handle -> bool -> int = "rv_surface_configure"
external surface_acquire : handle -> int = "rv_surface_acquire"
external surface_discard : handle -> unit = "rv_surface_discard"
external commit_present : handle -> handle -> handle -> int = "rv_commit_present"
external surface_destroy : handle -> unit = "rv_surface_destroy"

(* The bindings a SPIR-V module declares in set 0, as (binding, kind): 0 uniform buffer, 1 storage
   buffer, 2 sampled image, 3 storage image, 4 sampler, 5 acceleration structure. *)
let reflect bytes =
  let words = Bytes.length bytes / 4 in
  let word i = Int32.to_int (Bytes.get_int32_le bytes (i * 4)) land 0xffffffff in
  let bindings = Hashtbl.create 16
  and block = Hashtbl.create 16
  and pointers = Hashtbl.create 16
  and kinds = Hashtbl.create 16
  and variables = ref [] in
  let i = ref 5 in
  while !i < words do
    let head = word !i in
    let length = head lsr 16 and op = head land 0xffff in
    if length = 0 then invalid_arg "Vulkan.reflect: malformed module";
    (match op with
    | 71 when word (!i + 2) = 33 -> Hashtbl.replace bindings (word (!i + 1)) (word (!i + 3))
    | 71 when word (!i + 2) = 2 -> Hashtbl.replace block (word (!i + 1)) ()
    | 32 -> Hashtbl.replace pointers (word (!i + 1)) (word (!i + 3))
    | 25 -> Hashtbl.replace kinds (word (!i + 1)) (if word (!i + 7) = 2 then 3 else 2)
    | 26 -> Hashtbl.replace kinds (word (!i + 1)) 4
    | 5341 -> Hashtbl.replace kinds (word (!i + 1)) 5
    | 59 -> variables := (word (!i + 2), word (!i + 1), word (!i + 3)) :: !variables
    | _ -> ());
    i := !i + length
  done;
  List.filter_map
    (fun (id, pointer, storage) ->
      match Hashtbl.find_opt bindings id with
      | None -> None
      | Some binding ->
          let pointee = Option.value ~default:(-1) (Hashtbl.find_opt pointers pointer) in
          let kind =
            match Hashtbl.find_opt kinds pointee with
            | Some kind -> kind
            | None -> if storage = 2 && Hashtbl.mem block pointee then 0 else 1
          in
          Some (binding, kind))
    !variables
