(** Checked packed curve topology operations. *)

val join_curves :
  ?cancel:Cancel.t -> ?grain:int -> ?primitives:Group.t ->
  ?picked_ends:Curve_join.curve_join_pick array ->
  ?orient_closest:bool -> ?connect_closest_ends:bool ->
  ?only_connected:bool -> ?group_size:int -> ?keep_originals:bool ->
  ?tolerance:float -> ?wrap:bool -> Geometry.t -> (Geometry.t, Error.t) result
