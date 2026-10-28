val run :
  ?edges:Rdk_core.Edge_group.t ->
  ?make_isolated_loops_closed:bool ->
  Rdk_core.Geometry.t -> (Rdk_core.Geometry.t, string) result

val chains :
  point_count:int ->
  edge_a:int array ->
  edge_b:int array ->
  (int -> bool) ->
  (int array * int array * int array, string) result
(** Maximal point chains of the selected non-self edges of a unique-edge graph,
    in the order [run] emits its polylines. Returns [(offsets, values, closed)]:
    ragged point rows, with [closed.(i) = 1] for an isolated loop (its first
    point repeats last). *)
