let protected operation code work = Error.guard ~operation ~code work

type centroid_piece_owner = Extract_centroid.piece_owner =
  | Centroid_piece_points
  | Centroid_piece_primitives

type centroid_run_over = Extract_centroid.run_over =
  | Centroid_detail
  | Centroid_primitives
  | Centroid_pieces of {
      owner : centroid_piece_owner;
      attribute : string;
    }

type centroid_method = Extract_centroid.method_ =
  | Centroid_point_mass
  | Centroid_bounding_box
  | Centroid_convex_hull

type extract_curve_cut = Extract_point_curve.cut =
  | Extract_cut_constant of float
  | Extract_cut_primitive_attribute of string

type ends_mode =
  | Ends_open
  | Ends_close_straight
  | Ends_unroll_shared
  | Ends_unroll_new

type curve_join_end = Curve_ops.curve_join_end =
  | Join_curve_start
  | Join_curve_end

type curve_join_pick = Curve_ops.curve_join_pick = {
  primitive : int;
  end_ : curve_join_end;
}

let join_curves ?cancel ?grain ?primitives ?picked_ends ?orient_closest
    ?connect_closest_ends ?only_connected ?group_size ?keep_originals
    ?tolerance ?wrap geometry =
  protected "join_curves" "invalid_geometry" (fun () ->
    Curve_ops.join ?cancel ?grain ?primitives ?picked_ends ?orient_closest
      ?connect_closest_ends ?only_connected ?group_size ?keep_originals
      ?tolerance ?wrap geometry)

