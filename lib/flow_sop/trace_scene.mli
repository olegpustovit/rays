(** The CPU host for [scene/trace]: a polygon geometry's {!Rdk.Surface_index} answers the ray queries of a
    [Flow.Ray_host] scene id. Distances are world units along the normalised direction; [normal] is the
    geometric triangle normal; [u] and [v] are the barycentric weights of triangle corners 1 and 2. *)
val register : int -> Rdk.Geometry.t -> (unit, string) result
val unregister : int -> unit
