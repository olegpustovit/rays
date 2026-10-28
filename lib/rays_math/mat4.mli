(** Immutable 4x4 matrices for three-dimensional affine and projective transforms. Matrices multiply
    column vectors and compose right-to-left. *)

type t

val identity : t

val of_rows :
  float * float * float * float ->
  float * float * float * float ->
  float * float * float * float ->
  float * float * float * float ->
  t

val get : t -> row:int -> column:int -> float
val mul : t -> t -> t
val transpose : t -> t
val inverse : t -> t option
val translation : Vec3.t -> t
val scaling : Vec3.t -> t
val rotation_x : float -> t
val rotation_y : float -> t
val rotation_z : float -> t
val rotation : axis:Vec3.t -> float -> t

val perspective : fov_y:float -> aspect:float -> near:float -> far:float -> t
(** Right-handed perspective projection. [fov_y] is in radians. *)

val orthographic :
  left:float -> right:float -> bottom:float -> top:float -> near:float -> far:float -> t

val look_at : eye:Vec3.t -> target:Vec3.t -> up:Vec3.t -> t
val transform : t -> float * float * float * float -> float * float * float * float

(* Transform a point and perform homogeneous division when possible. *)
val transform_point : t -> Vec3.t -> Vec3.t
val transform_direction : t -> Vec3.t -> Vec3.t
val nearly_equal : t -> t -> eps:float -> bool

val to_rows :
  t ->
  (float * float * float * float)
  * (float * float * float * float)
  * (float * float * float * float)
  * (float * float * float * float)
