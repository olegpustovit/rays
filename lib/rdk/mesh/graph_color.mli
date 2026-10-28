type connectivity =
  | Graph_primitives_by_point
  | Graph_points_by_primitive
  | Graph_primitives_by_edge

type worksets = {
  begin_attribute : string;
  length_attribute : string;
}

val run :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?selection:Transform_ops.deform_selection ->
  ?connectivity:connectivity ->
  ?color_attribute:string ->
  ?sort_output:bool ->
  ?worksets:worksets ->
  Geometry.t -> (Geometry.t, Error.t) result

val greedy : offsets:int array -> neighbors:int array -> int array -> int array
(** [greedy ~offsets ~neighbors order] first-fit colors a graph given as neighbour
    rows (CSR). Elements are colored in [order]; each takes the smallest color
    unused by its already colored neighbours. Elements outside [order] stay [-1].
    This is the rule [run] applies per component. Raises [Error.Fail]
    on an internal color-bound failure. *)
