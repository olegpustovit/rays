(** View post-rounding Boolean materialization policy. *)

(* O(Ec + Ic + Es + Is) time and O(Fc) auxiliary storage for complex edges
    [Ec], complex incidence [Ic], extracted surface edges [Es], surface
    incidence [Is], and complex facets [Fc]. Exact seam-facet adjacency
    restricts candidates; the threshold applies only to rounded length. *)

(* O(F log F + K) time and O(F + K) auxiliary storage for [F] triangles and
    broad-phase candidate work [K]. Exact binary64 predicates distinguish
    ordinary shared topology from non-adjacent contact. *)

val verify_unchanged_extraction :
  ?cancel:Cancel.t -> grain:int -> require_closed:bool ->
  allow_opposite_duplicates:bool -> Boolean_extract.ancestry -> Geometry.t ->
  (bool, Error.t) result
(** Fast exact verification for geometry that still physically shares its
    extraction positions and topology. [false] requests the ordinary bounded
    rounding-repair path; no weakened result is published. *)

type cleanup
val cleanup_geometry : cleanup -> Geometry.t
(* Number of rounded zero-area facets removed or ULP-scale contact edges
   contracted by the mandatory certified representability repair. *)
val cleanup_primitive_source : cleanup -> int -> int

val collapse_tiny_seams :
  ?cancel:Cancel.t -> grain:int -> threshold:float -> require_closed:bool ->
  ?allow_opposite_duplicates:bool -> ?max_batches:int -> ?strict:bool ->
  Boolean_extract.ancestry -> Boolean_seam.t -> Geometry.t ->
  (cleanup, Error.t) result
(** Continue deterministic independent contraction batches until no tiny
    seam-adjacent candidate remains, the explicit batch bound is reached, or
    no candidate has a certified contraction. Strict mode returns the stable
    [unresolved_cleanup] diagnostic instead of publishing a partial cleanup.
    Before optional threshold cleanup, a separately bounded mandatory pass
    removes only closed/pair-canceling zero-area subcomplexes and contracts
    only ULP-scale edges. Conservative link/orientation batches run first;
    a bounded speculative fallback is retained only when complete rounded
    degeneracy, incidence, and self-contact verification accepts the result. *)

val split_seam_points :
  ?cancel:Cancel.t -> grain:int -> cleanup -> (cleanup, Error.t) result
(** Duplicate each seam point once per primitive component obtained after
    removing exact seam edges. Corner/primitive order is unchanged; point
    payload and native edge groups follow exact packed ancestry. *)

type detriangulation = Triangles | All_polygons | Unchanged_polygons
val detriangulate :
  ?cancel:Cancel.t -> grain:int -> assume_flat:bool -> mode:detriangulation ->
  Boolean_extract.ancestry -> cleanup -> (cleanup, Error.t) result
(** Merge adjacent triangles only when their exact extraction ancestry names
    the same source polygon and the edge is not a Boolean seam. *)
