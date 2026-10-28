(** Assign a surface to a primitive group, preserving unselected surfaces.
    One sequential packed pass; O(primitives) time and storage. Deterministic
    across domain counts. Cancellation is checked every 4096 primitives. *)
val run :
  ?cancel:Rdk_core.Cancel.t -> ?group:string ->
  name:string -> color:(float * float * float) -> roughness:float -> emission:(float * float * float) -> Rdk_core.Geometry.t ->
  (Rdk_core.Geometry.t, Rdk_core.Error.t) result
