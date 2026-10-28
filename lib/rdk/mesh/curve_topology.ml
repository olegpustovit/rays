let protected operation code work = Error.guard ~operation ~code work

type curve_join_end = Curve_join.curve_join_end =
  | Join_curve_start
  | Join_curve_end

type curve_join_pick = Curve_join.curve_join_pick = {
  primitive : int;
  end_ : curve_join_end;
}

let join_curves ?cancel ?grain ?primitives ?picked_ends ?orient_closest
    ?connect_closest_ends ?only_connected ?group_size ?keep_originals
    ?tolerance ?wrap geometry =
  protected "join_curves" "invalid_geometry" (fun () ->
    Curve_join.join ?cancel ?grain ?primitives ?picked_ends ?orient_closest
      ?connect_closest_ends ?only_connected ?group_size ?keep_originals
      ?tolerance ?wrap geometry)
