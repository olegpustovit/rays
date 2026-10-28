(** Streaming deterministic isosurface extraction into packed RDK geometry.
    O(lattice cells + emitted vertices) time; O(x*y + z + emitted vertices)
    auxiliary storage for callback fields, excluding the caller's sampled
    volume. Sampled extraction owns O(workers*x*y + z + emitted vertices)
    scratch with rotating planes per active worker. Sampling and cell
    work use the shared Parallel pool, with stable disjoint ranges and exact
    one-domain/multi-domain ordering. Grain defaults to 16,384; fewer than two
    chunks per plane/slab stay sequential for callbacks. Sampled volumes use
    stable consecutive slab chunks containing at least one grain of cells;
    fewer than two chunks retain plane/slab scheduling. Cancellation is checked per plane
    and slab; invalid parameters, nonfinite samples and cancellation are typed
    errors. *)

type sample = float array
(** Borrowed XYZ sample. Custom callbacks must not retain or mutate it. *)

module Field : sig
  type t
  val custom : (sample -> float) -> t
  (** Fields must be deterministic and concurrency-safe: sufficiently large
      planes sample concurrently in disjoint chunks of the shared pool. *)
end

val extract_dense : resolution:int * int * int -> min:Rays_math.Vec3.t ->
  max:Rays_math.Vec3.t -> iso:float -> field:Field.t -> unit ->
  (Geometry.t, Error.t) result

module View : sig
end
