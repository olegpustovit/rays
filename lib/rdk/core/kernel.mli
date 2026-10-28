(** Packed point kernels. Built-in kernels allocate only their exact output
    planes. User callbacks may allocate according to ordinary OCaml closure and
    floating-point boxing rules. [Writer.set] may only target the current stable
    point index passed to the callback. *)

val generate_point_ranges :
  int ->
  (first:int -> last:int ->
   x:float array -> y:float array -> z:float array -> unit) ->
  Geometry.t
(** High-throughput O(n) generator. The callback is invoked once per stable
    disjoint half-open range, not once per point. It may mutate only
    indices from [first] inclusive to [last] exclusive in the supplied owned
    output planes and must not retain the
    arrays. This makes tight user loops allocation-free beyond exact output. *)
