(** Seeded coherent gradient noise, safe to share between domains. *)

type t

val create : int -> t
val sample3 : t -> x:float -> y:float -> z:float -> float
(** Samples are approximately in the range 0..1. *)

val fbm2 :
  ?octaves:int ->
  t -> x:float -> y:float -> float
val fbm3 :
  ?octaves:int ->
  t -> x:float -> y:float -> z:float -> float
(** Fractal Brownian motion composed from multiple noise frequencies. *)

val permutation : t -> int array
(* Owned snapshot of the seeded 512-entry table for the GPU noise ABI. *)
type fbm3_scratch
val create_fbm3_scratch : unit -> fbm3_scratch
val fbm3_with_scratch :
  fbm3_scratch -> t -> octaves:int -> lacunarity:float -> gain:float ->
  x:float -> y:float -> z:float -> float
(** Allocation-free fractal sample for an already validated octave profile.
    Scratch is caller-owned and must not be shared concurrently. Non-finite
    intermediate coordinates return [nan]. *)

val sample2_into :
  t -> first:int -> last:int -> frequency:float ->
  x:float array -> y:float array -> output:float array -> unit
(** Fill the half-open range in [output] from packed coordinate planes.
    Arrays are borrowed, ranges must be in bounds, and disjoint ranges may
    be called concurrently with the same immutable noise value. *)

val sample3_into :
  t -> first:int -> last:int ->
  ?x_offset:int -> ?y_offset:int -> ?z_offset:int -> ?output_offset:int ->
  ?x_stride:int -> ?y_stride:int -> ?z_stride:int ->
  ?frequency:float ->
  x:float array -> y:float array -> z:float array -> output:float array -> unit -> unit
(** Packed [sample3], exactly matching scalar samples. Offsets default to zero
    and permit registers stored in one scratch array. Input/output spans must
    be disjoint, or have identical per-index addresses. Non-finite coordinates
    produce [nan]; callers retain their typed error boundary. [frequency]
    defaults to 1 and multiplies each coordinate before sampling. Input strides
    default to 1; larger strides read components from interleaved arrays. *)
