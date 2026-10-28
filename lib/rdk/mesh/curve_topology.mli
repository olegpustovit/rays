(** Checked packed curve topology operations. *)

type centroid_piece_owner =
  | Centroid_piece_points
  | Centroid_piece_primitives

type centroid_run_over =
  | Centroid_detail
  | Centroid_primitives
  | Centroid_pieces of {
      owner : centroid_piece_owner;
      attribute : string;
    }

type centroid_method =
  | Centroid_point_mass
  | Centroid_bounding_box
  | Centroid_convex_hull

type extract_curve_cut =
  | Extract_cut_constant of float
  | Extract_cut_primitive_attribute of string

type ends_mode =
  | Ends_open
  | Ends_close_straight
  | Ends_unroll_shared
  | Ends_unroll_new

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
