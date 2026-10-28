type sphere_connectivity =
  | Sphere_triangles
  | Sphere_alternating_triangles
  | Sphere_quads
  | Sphere_rows
  | Sphere_columns
  | Sphere_rows_and_columns
  | Sphere_points

type sphere_normals =
  | Sphere_no_normals
  | Sphere_point_normals
  | Sphere_vertex_normals

type sphere_orientation =
  | Sphere_x
  | Sphere_y
  | Sphere_z
  | Sphere_axis of Rays_math.Vec3.t

type sphere_rotation_order =
  | Sphere_xyz
  | Sphere_xzy
  | Sphere_yxz
  | Sphere_yzx
  | Sphere_zxy
  | Sphere_zyx

val run :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?connectivity:sphere_connectivity ->
  ?unique_points_per_pole:bool ->
  ?triangular_poles:bool ->
  ?normals:sphere_normals ->
  ?orientation:sphere_orientation ->
  ?center:Rays_math.Vec3.t ->
  ?rotation:Rays_math.Vec3.t ->
  ?rotation_order:sphere_rotation_order ->
  ?uniform_scale:float ->
  ?radius_x:float ->
  ?radius_y:float ->
  ?radius_z:float ->
  ?uv_attribute:string ->
  ?segments:int ->
  ?rings:int ->
  radius:float ->
  unit ->
  (Geometry.t, Error.t) result
(** Generate a latitude/longitude sphere or ellipsoid as regular/alternating
    triangles, quads, open row/column curves, combined curves, or points. The
    compatible default retains shared poles, triangles, +Y pole orientation,
    and smooth point normals. Quad poles may be triangles or logically
    degenerate quads; [unique_points_per_pole] gives every meridian its own
    coincident endpoint.

    Radius overrides name the local radial-X, pole-Y, and radial-Z axes before
    pole orientation and Euler rotation. [Sphere_axis] uses a deterministic
    scale-safe orthonormal frame. Rotation components are radians and the
    rotation-order constructor names their application order. Optional UVs
    are vertex-owned for topology and point-owned for point output, with a
    non-crossing surface seam.

    Cardinalities are checked before allocation. Positions, normals, UVs,
    topology, offsets, and kinds use exact packed planes and deterministic
    disjoint fills. Work/output are O(segments * rings); auxiliary storage is
    O(segments + rings + parallel ranges). *)
