(** Incremental isotropic remeshing: per iteration, split long edges,
    collapse short ones, flip to even out valence, then optionally smooth and
    project back onto the input surface.

    The three topology stages edit one local triangle structure in place and
    materialize a single geometry per iteration; only [triangulate] is a
    caller-supplied kernel. [collapse] and [flip] are no longer invoked and
    stay in the record only so callers compile unchanged until it shrinks.

    Per iteration the cost is O(points + corners + edges) for the structure,
    the selections and the materialization, plus O(local valence) per edit;
    candidate collapses are radix sorted. Payload interpolation runs through
    [Parallel] with [grain]; every other pass is sequential, so output is
    byte-identical between one and many domains. Cancellation is checked at
    least every 4,096 elements. *)
type params = {
  cancel : Cancel.t option;
  grain : int;
  iterations : int;
  smoothing : float;
  project : bool;
  use_input_points_only : bool;
  hard_points : Group.t option;
  hard_edges : Edge_group.t option;
  target_size_attribute : string option;
  preserve_uv_seams : bool;
  uv_attribute : string;
  output_hard_edges : string option;
  output_mesh_size : string option;
  output_quality : string option;
  recompute_point_normals : bool;
  target_length : float;
}

val run : params -> Geometry.t -> (Geometry.t, Error.t) result
