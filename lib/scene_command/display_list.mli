(** Packed, renderer-neutral retained 2D command segments. *)

type t
type segment = t

val fresh_id : unit -> int64

module Builder : sig
  type t

  val create : unit -> t
  val reset : t -> unit
  val push_clip : t -> x:float -> y:float -> width:float -> height:float -> unit
  val pop_clip : t -> unit
  val geometry : t -> Render_ir.geometry -> unit
  val image : t -> resource_id:int -> source:Render_ir.rect -> destination:Render_ir.rect -> unit
  val publish : t -> id:int64 -> version:int64 -> (segment, Render_ir.error) result
end

val id : t -> int64
val version : t -> int64
val source_bytes : t -> int
val render_ir : t -> Render_ir.t
