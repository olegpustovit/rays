type mode = Laplacian of float | Custom_steps of { odd : float; even : float }

type params = {
  cancel : Cancel.t option;
  grain : int option;
  selection : Group.t option;
  iterations : int;
  mode : mode;
  pattern : string;
}

val run : params -> Geometry.t -> (Geometry.t, Error.t) result
