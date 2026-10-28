type curve_join_end = Join_curve_start | Join_curve_end
type curve_join_pick = { primitive : int; end_ : curve_join_end }

val interpolate_attribute :
  ?cancel:Cancel.t ->
  ?grain:int ->
  int array ->
  int array ->
  float array ->
  int array ->
  int array ->
  float array ->
  Attribute.t ->
  Attribute.t
(** Shared packed linear interpolation for point/vertex topology generators. Integer, text, and
    ragged payload use the nearest endpoint. *)

type params = {
  cancel : Cancel.t option;
  grain : int option;
  primitives : Group.t option;
  picked_ends : curve_join_pick array option;
  orient_closest : bool;
  connect_closest_ends : bool;
  only_connected : bool;
  group_size : int option;
  keep_originals : bool;
  tolerance : float;
  wrap : bool;
}

val join : params -> Geometry.t -> (Geometry.t, string) result
