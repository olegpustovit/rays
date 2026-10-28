val apply :
  ?cancel:Cancel.t ->
  grain:int ->
  remove_degenerate_primitives:bool ->
  remove_unused_points_from_degenerate_primitives:bool ->
  remove_all_unused_points:bool ->
  Geometry.t ->
  (Geometry.t, string) result
(** Remove consecutive duplicate point references after Fuse, discard polygon
    and closed-curve primitives with fewer than three remaining vertices and
    open curves with fewer than two, and optionally compact points. All output
    ordering and attribute/group ancestry are stable. *)

val apply_with_mapping :
  ?cancel:Cancel.t ->
  grain:int ->
  remove_degenerate_primitives:bool ->
  remove_unused_points_from_degenerate_primitives:bool ->
  remove_all_unused_points:bool ->
  Geometry.t ->
  (Geometry.t * int array, string) result
(** The cleanup result plus a source-point-to-output-point map. Deleted source
    points map to [-1]. This private fused boundary lets a caller defer
    topology-affine edge-group remapping until the final topology exists. *)
