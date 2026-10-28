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

type tube_connectivity =
  | Tube_triangles
  | Tube_alternating_triangles
  | Tube_quads
  | Tube_rows
  | Tube_columns
  | Tube_rows_and_columns
  | Tube_points

type tube_normals =
  | Tube_no_normals
  | Tube_point_normals
  | Tube_vertex_normals

type tube_orientation =
  | Tube_x
  | Tube_y
  | Tube_z
  | Tube_axis of Rays_math.Vec3.t

type tube_rotation_order =
  | Tube_xyz
  | Tube_xzy
  | Tube_yxz
  | Tube_yzx
  | Tube_zxy
  | Tube_zyx

type platonic_kind =
  | Platonic_tetrahedron
  | Platonic_cube
  | Platonic_octahedron
  | Platonic_icosahedron
  | Platonic_dodecahedron
  | Platonic_soccer_ball

type platonic_normals =
  | Platonic_no_normals
  | Platonic_point_normals
  | Platonic_vertex_normals

type platonic_orientation =
  | Platonic_x
  | Platonic_y
  | Platonic_z
  | Platonic_axis of Rays_math.Vec3.t

type platonic_rotation_order =
  | Platonic_xyz
  | Platonic_xzy
  | Platonic_yxz
  | Platonic_yzx
  | Platonic_zxy
  | Platonic_zyx

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
