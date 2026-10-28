(** Circle and grid generators over packed geometry. *)

type circle_arc =
    Circle_closed
  | Circle_open_arc of { start_angle : float; end_angle : float; }
  | Circle_closed_arc of { start_angle : float; end_angle : float; }
  | Circle_sliced_arc of { start_angle : float; end_angle : float; }
type circle_orientation =
    Circle_xy
  | Circle_xz
  | Circle_yz
  | Circle_axes of { horizontal : Rays_math.Vec3.t;
      vertical : Rays_math.Vec3.t;
    }
type grid_counts = Grid_divisions | Grid_point_counts
type grid_orientation =
    Grid_xy
  | Grid_xz
  | Grid_yz
  | Grid_axes of { horizontal : Rays_math.Vec3.t;
      vertical : Rays_math.Vec3.t;
    }
type grid_connectivity =
    Grid_points
  | Grid_rows
  | Grid_columns
  | Grid_rows_and_columns
  | Grid_quads
  | Grid_triangles
  | Grid_alternating_triangles
  | Grid_reverse_triangles
val normalize_plane_axis :
  string ->
  string -> Rays_math.Vec3.t -> (Rays_math.Vec3.t, string) result

val grid :
  ?cancel:Rdk_core.Cancel.t -> ?grain:int -> ?counts:grid_counts ->
  ?connectivity:grid_connectivity -> ?orientation:grid_orientation ->
  ?center:Rays_math.Vec3.t -> ?width:float -> ?height:float ->
  ?rotation:float -> ?uv_attribute:String.t ->
  columns:int -> rows:int -> size:float -> unit ->
  (Rdk_core.Geometry.t, Rdk_core.Error.t) result
