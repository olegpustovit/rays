val fuse : ?through_exact:bool -> Core.Region.t -> Core.Region.t
(** Absorb the [Collect] regions of equal count or origin that feed this one. [~through_exact:true]
    also absorbs a producer an [(exact x)] wraps, for a device region read back as a whole (a fused
    SOP write's); otherwise [exact] is a boundary. *)

val merge : Core.Region.t list -> Core.Region.t option
(** Sibling Collect regions of equal count or origin, fused into one region with an output each (in
    list order). [None] when they disagree or the register limit declines. *)

val compact_sources : Core.Region.t -> Core.Region.t
(** For a device: a source that fills one constant becomes that constant, and sources reading the
    same attribute become one. *)
