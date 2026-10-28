val fuse : Core.Region.t -> Core.Region.t
(** Absorb the [Collect] regions of equal count or origin that feed this one. *)

val merge : Core.Region.t list -> Core.Region.t option
(** Sibling Collect regions of equal count or origin, fused into one region with an output each (in
    list order). [None] when they disagree or the register limit declines. *)
