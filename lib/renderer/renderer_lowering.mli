val default_state : int * int * int * int -> int * int * int * int -> Scene_execution.state
val write_affine : bytes -> Scene_command.Render_ir.transform -> unit
val affine_uniforms : Scene_command.Render_ir.transform -> bytes
val identity_transform : Scene_command.Render_ir.transform -> bool

val mesh_of_geometry :
  int ->
  Scene_command.Render_ir.transform ->
  viewport:int * int * int * int ->
  int * int * int * int ->
  Scene_command.Render_ir.geometry ->
  Scene_execution.sampled_draw

val mesh_of_geometry_run :
  int ->
  Scene_command.Render_ir.transform ->
  viewport:int * int * int * int ->
  int * int * int * int ->
  Scene_command.Render_ir.command array ->
  int ->
  int ->
  Scene_execution.sampled_draw

val compose_raster :
  Scene_command.Render_ir.transform ->
  Scene_command.Render_ir.transform ->
  Scene_command.Render_ir.transform

val ui_sampler : Ogpu.Types.sampler_descriptor
val ui_white_texture : Scene_execution.sampled_texture
val same_bytes_range : bytes -> int -> bytes -> bool

type t

val ui_index_bytes : t -> int -> bytes
val write_ui_affine : t -> Scene_command.Render_ir.transform -> bytes

type ui_slot = {
  mutable ui_vertices : bytes;
  mutable ui_indices : bytes;
  mutable ui_affine : bytes;
  mutable ui_source : bytes;
}

val create : unit -> t
val clear : t -> unit
val find_ui_slot : t -> int -> ui_slot
val snapshot_cache_entry_byte_capacity : int
val sampled_texture_bytes : Scene_execution.sampled_texture -> int

val lower_scene2 :
  t ->
  facts:Runtime.presentation_facts ->
  density:int ->
  snapshot:
    (density:int ->
    Renderer_retained_plans.resource ->
    (int * int * Scene_execution.sampled_texture, Ogpu.Error.t) result) ->
  resolve_gpu:(Scene_command.Ui_batch.t -> (string option, string) result) ->
  rollback:(unit -> unit) ->
  resource:(int -> Renderer_retained_plans.resource option) ->
  Scene_command.Render_ir.command array ->
  (Scene_execution.sampled_draw list, Ogpu.Error.t) result
