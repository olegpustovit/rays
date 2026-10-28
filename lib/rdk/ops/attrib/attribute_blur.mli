type mode = Laplacian of float | Custom_steps of { odd : float; even : float }

val run :
  ?cancel:Cancel.t -> ?grain:int -> ?selection:Group.t ->
  ?iterations:int -> ?mode:mode ->
  pattern:string -> Geometry.t -> (Geometry.t, Error.t) result
