val chains :
  point_count:int ->
  edge_a:int array ->
  edge_b:int array ->
  (int -> bool) ->
  (int array * int array * int array, string) result
(** Maximal point chains of the selected non-self edges of a unique-edge graph,
    in tracing order. Returns [(offsets, values, closed)]:
    ragged point rows, with [closed.(i) = 1] for an isolated loop (its first
    point repeats last). *)
