(** Checked packed curve topology operations. *)

type curve_join_end =
  | Join_curve_start
  | Join_curve_end

type curve_join_pick = {
  primitive : int;
  end_ : curve_join_end;
}

val join_curves :
  ?cancel:Cancel.t -> ?grain:int -> ?primitives:Group.t ->
  ?picked_ends:curve_join_pick array ->
  ?orient_closest:bool -> ?connect_closest_ends:bool ->
  ?only_connected:bool -> ?group_size:int -> ?keep_originals:bool ->
  ?tolerance:float -> ?wrap:bool -> Geometry.t -> (Geometry.t, Error.t) result
