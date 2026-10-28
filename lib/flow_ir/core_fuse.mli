val fuse : ?through_exact:bool -> Core.Region.t -> Core.Region.t
(** Absorb the [Collect] regions of equal count or origin that feed this one. [~through_exact:true]
    also absorbs a producer an [(exact x)] wraps, for a device region read back as a whole (a fused
    SOP write's); otherwise [exact] is a boundary. *)

val merge : Core.Region.t list -> Core.Region.t option
(** Sibling Collect regions of equal count or origin, fused into one region with an output each (in
    list order). [None] when they disagree or the register limit declines. *)
