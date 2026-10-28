type torus_connectivity =
  | Torus_triangles
  | Torus_alternating_triangles
  | Torus_quads
  | Torus_rows
  | Torus_columns
  | Torus_rows_and_columns
  | Torus_points

type torus_normals =
  | Torus_no_normals
  | Torus_point_normals
  | Torus_vertex_normals

type torus_orientation =
  | Torus_x
  | Torus_y
  | Torus_z
  | Torus_axis of Rays_math.Vec3.t

type torus_rotation_order =
  | Torus_xyz
  | Torus_xzy
  | Torus_yxz
  | Torus_yzx
  | Torus_zxy
  | Torus_zyx

val torus :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?connectivity:torus_connectivity ->
  ?normals:torus_normals ->
  ?orientation:torus_orientation ->
  ?center:Rays_math.Vec3.t ->
  ?rotation:Rays_math.Vec3.t ->
  ?rotation_order:torus_rotation_order ->
  ?uniform_scale:float ->
  ?u_start:float ->
  ?u_end:float ->
  ?v_start:float ->
  ?v_end:float ->
  ?u_wrap:bool ->
  ?v_wrap:bool ->
  ?u_end_caps:bool ->
  ?v_end_cap:bool ->
  ?uv_attribute:string ->
  ?rows:int ->
  ?columns:int ->
  major_radius:float ->
  minor_radius:float ->
  unit ->
  (Geometry.t, Error.t) result
