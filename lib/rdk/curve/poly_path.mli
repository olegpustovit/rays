val run :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  ?edges:Rdk_core.Edge_group.t ->
  ?preserve_source_payload:bool ->
  ?connect_end_points:bool ->
  ?maximum_distance:float ->
  ?connect_only_to_other_end_points:bool ->
  ?make_isolated_loops_closed:bool ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, string) result

val chains :
  ?cancel:Rdk_core.Cancel.t ->
  point_count:int ->
  edge_a:int array ->
  edge_b:int array ->
  (int -> bool) ->
  (int array * int array * int array, string) result
(** Maximal point chains of the selected non-self edges of a unique-edge graph,
    in the order [run] emits its polylines. Returns [(offsets, values, closed)]:
    ragged point rows, with [closed.(i) = 1] for an isolated loop (its first
    point repeats last). *)
