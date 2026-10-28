let protected operation code work = Error.guard ~operation ~code work



let join_curves ?cancel ?grain ?primitives ?picked_ends ?orient_closest
    ?connect_closest_ends ?only_connected ?group_size ?keep_originals
    ?tolerance ?wrap geometry =
  protected "join_curves" "invalid_geometry" (fun () ->
    Curve_join.join ?cancel ?grain ?primitives ?picked_ends ?orient_closest
      ?connect_closest_ends ?only_connected ?group_size ?keep_originals
      ?tolerance ?wrap geometry)
