let protected operation code work = Error.guard ~operation ~code work

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

let join_curves
    ({
       cancel;
       grain;
       primitives;
       picked_ends;
       orient_closest;
       connect_closest_ends;
       only_connected;
       group_size;
       keep_originals;
       tolerance;
       wrap;
     } :
      params) geometry =
  protected "join_curves" "invalid_geometry" (fun () ->
      Curve_join.join
        {
          cancel;
          grain;
          primitives;
          picked_ends;
          orient_closest = Option.value ~default:true orient_closest;
          connect_closest_ends = Option.value ~default:false connect_closest_ends;
          only_connected = Option.value ~default:false only_connected;
          group_size;
          keep_originals = Option.value ~default:false keep_originals;
          tolerance = Option.value ~default:0. tolerance;
          wrap = Option.value ~default:false wrap;
        }
        geometry)
