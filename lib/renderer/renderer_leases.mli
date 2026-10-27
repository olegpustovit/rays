type t
type checkpoint
type policy = Copy_image_snapshots | Retain_image_snapshots of t

val create : unit -> t
val checkpoint : policy -> checkpoint option
val retain : t -> Runtime_resources.Image.Private.lease -> unit
val release : t -> unit
val rollback : policy -> checkpoint option -> unit
