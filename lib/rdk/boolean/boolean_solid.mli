(** View build-once exact solid Boolean arrangement. *)

type t
type treatment = Solid | Surface
type operation = Union | Intersection | Difference | Reverse_difference | Xor | Shatter

val prepare :
  ?cancel:Cancel.t ->
    ?resolve_left_self_intersections:bool ->
    ?resolve_right_self_intersections:bool ->
    ?left_treatment:treatment -> ?right_treatment:treatment ->
  grain:int -> left:Geometry.t -> right:Geometry.t ->
  unit -> (t, Error.t) result
(** Construct and classify the shared two-operand arrangement once. Degenerate
    source triangle pairs are rejected until their repair policy is explicit.
    Self-intersection resolution is opt-in per operand so known-clean solids do
    not pay for self broad phases. [Surface] operands remain exact arrangement
    constraints and seam contributors but add zero to volumetric winding. *)

val extract_product_with_ancestry :
  ?cancel:Cancel.t -> ?require_closed:bool -> ?defer_rounded_slivers:bool ->
  ?corner_payload:bool ->
  operation:operation -> t ->
  (Boolean_extract.ancestry, Error.t) result
(** Treatment-aware solid/surface product extraction. Sheet facets remain
    zero-volume arrangement constraints and are emitted explicitly, including
    paired opposite-facing walls for a solid-minus-surface cut. *)

val seams :
  ?cancel:Cancel.t -> ?grain:int -> ?parallel_cutoff:int -> ?materialize:bool ->
  t -> (Boolean_seam.t, Error.t) result
(** Materialize deterministic self/intersection curves and coincident facets
    from the already prepared exact complex without repeating arrangement. *)

val shatter_with_ancestry :
  ?cancel:Cancel.t -> ?require_closed:bool -> ?defer_rounded_slivers:bool ->
  ?corner_payload:bool -> t ->
  (Boolean_extract.ancestry array, Error.t) result
(** Extract stable A-only, overlap, and B-only closed region boundaries from
    one prepared arrangement. Shared walls are intentionally duplicated
    between products. *)

module View : sig
  val weiler : t -> Boolean_weiler.t
end
