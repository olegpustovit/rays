(** Checked packed curve topology operations. *)

type params = {
  cancel : Cancel.t option;
  grain : int option;
  primitives : Group.t option;
  picked_ends : Curve_join.curve_join_pick array option;
  orient_closest : bool option;
  connect_closest_ends : bool option;
  only_connected : bool option;
  group_size : int option;
  keep_originals : bool option;
  tolerance : float option;
  wrap : bool option;
}

val join_curves : params -> Geometry.t -> (Geometry.t, Error.t) result