(** Host ray queries for [scene/trace]: a scene id names a closure the host registered. [flow] never sees
    geometry; the host (flow_sop over rdk, or the path tracer's acceleration structure) supplies the closure.
    A query is [|ox; oy; oz; dx; dy; dz; tmin; tmax; time|]. Closures run on any domain and must be pure. *)
type hit = { t : float; prim : int; u : float; v : float; nx : float; ny : float; nz : float; hit : bool; instance : int }
(** [instance] is the index in an instanced scene (0 for a flat scene's triangles, -1 for a sphere or a miss). *)
val miss : hit
val register : int -> (float array -> hit) -> unit
val unregister : int -> unit
val query : int -> float array -> hit
(** Raises the typed diagnostic [E_TRACE] when no scene has this id. *)
