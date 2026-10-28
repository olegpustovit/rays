(** Packed mesh cleanup. *)

val delete_degenerate :
  ?cancel:Cancel.t -> grain:int -> ?primitives:Group.t -> epsilon:float ->
  Geometry.t -> (Geometry.t, string) result

type overlap_policy = Keep_first_overlap | Delete_overlap_pairs

type params = {
  cancel : Cancel.t option;
  grain : int;
  epsilon : float;
  remove_degenerate : bool;
  consolidate_distance : float option;
  overlaps : overlap_policy option;
  reverse_winding : bool;
  remove_nan_points : bool;
  remove_unused_points : bool;
  delete_unused_groups : bool;
  point_attributes : string option;
  vertex_attributes : string option;
  primitive_attributes : string option;
  detail_attributes : string option;
  point_groups : string option;
  vertex_groups : string option;
  primitive_groups : string option;
  edge_groups : string option;
}

val run : params -> Geometry.t -> (Geometry.t, Error.t) result
