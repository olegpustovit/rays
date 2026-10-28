type t

val create : Device.t -> (t, Ogpu_core.Error.t) result

val encode :
  t ->
  Metal.Command_buffer.t ->
  ?present:Metal.Drawable.t ->
  source:Metal.Texture.t ->
  target:Metal.Texture.t ->
  unit ->
  (unit, Ogpu_core.Error.t) result
(** [encode value commands ~source ~target] appends the presentation render pass (and the drawable's
    presentation when [present] is given) to a caller-owned command buffer, which is never committed
    or destroyed here. *)

val destroy : t -> unit
