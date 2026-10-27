type resource =
  | Image of Runtime_resources.Image.t
  | Text of Runtime_resources.Text.t
  | Canvas of Runtime_resources.Canvas.t

type resource_stamp
type plan = {
  mutable plan_ir_id : int;
  plan_command_count : int;
  plan_source_bytes : int;
  plan_density : int;
  plan_extent : int * int * int * int;
  plan_resources : resource_stamp list;
  mutable plan_ir : Scene_command.Render_ir.t;
  plan_draws : Scene_execution.sampled_draw list;
}
type segment = {
  segment_version : int64;
  segment_density : int;
  segment_width : int;
  segment_height : int;
  segment_draws : Scene_execution.sampled_draw list;
}
type t

val create : unit -> t
val find : t -> int -> plan
val find_ir : t -> int -> plan option
val add : t -> int -> plan -> unit
val rebind : t -> plan -> ir_id:int -> ir:Scene_command.Render_ir.t -> unit
val find_segment : t -> int64 -> segment
val add_segment : t -> int64 -> bytes:int -> segment -> unit
val segment_count : t -> int
val clear : t -> unit
val plan_source_bytes : Scene_command.Render_ir.command array -> int
val same_resources : resource_stamp list -> resource_stamp list -> bool
val resource_stamps : resolve:(int -> resource option) ->
  Scene_command.Render_ir.command array -> bool * resource_stamp list
