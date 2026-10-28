
type params = {
  cancel : Cancel.t option;
  grain : int option;
  selection : Element_selection.t option;
  pre_compute_normals : bool option;
  make_normals_unit_length : bool option;
  unique_points : bool option;
  consolidate_distance : float option;
  consolidate_normals_distance : float option;
  remove_inline_points : bool option;
  inline_distance : float option;
  orient_polygons : bool option;
  cusp_angle : float option;
  remove_degenerate : bool option;
  make_planar : bool option;
  post_compute_normals : bool option;
  reverse_normals : bool option;
}

val run : params -> Geometry.t -> (Geometry.t, Error.t) result
