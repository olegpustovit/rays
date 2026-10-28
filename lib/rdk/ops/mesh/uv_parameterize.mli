type boundary_mode = Circle | Preserve

type params = {
  cancel : Cancel.t option;
  grain : int option;
  name : string;
  seams : Group.t option;
  edge_seams : Edge_group.t option;
  uv_tolerance : float;
  iterations : int;
  tolerance : float;
}

val solve : params -> boundary_mode -> Geometry.t -> (Geometry.t, string) result