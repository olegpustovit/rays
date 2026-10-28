type end_mode = Open | Close | Close_straight | Unroll | Unroll_new
type cut_mode = Inside | Outside | Inside_and_outside
type parameter_attribute_mode = Replace | Scale

type curve_join_end = Join_curve_start | Join_curve_end

type curve_join_pick = {
  primitive : int;
  end_ : curve_join_end;
}

val interpolate_attribute :
  ?cancel:Cancel.t ->
  ?grain:int ->
  int array -> int array -> float array ->
  int array -> int array -> float array ->
  Attribute.t -> Attribute.t
(** Shared packed linear interpolation for point/vertex topology generators.
    Integer, text, and ragged payload use the nearest endpoint. *)

val join :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?primitives:Group.t ->
  ?picked_ends:curve_join_pick array ->
  ?orient_closest:bool ->
  ?connect_closest_ends:bool ->
  ?only_connected:bool ->
  ?group_size:int ->
  ?keep_originals:bool ->
  ?tolerance:float ->
  ?wrap:bool ->
  Geometry.t ->
  (Geometry.t, string) result

