(** Functional pointer-controlled orbit camera inspired by openFrameworks' [ofEasyCam]. All
    positions and control areas use logical points. *)

type t

val create :
  ?target:Rays_math.Vec3.t ->
  ?distance:float ->
  ?azimuth:float ->
  ?elevation:float ->
  ?fov_y:float ->
  ?inertia:bool ->
  unit ->
  t

val update : t -> Frame.t -> t
(** Consume the ordered pointer facts in one frame. Captured drags continue outside the control
    area. Double-clicking resets the original target, distance, and orientation. Vertical
    wheel/trackpad motion dollies and horizontal wheel/trackpad motion is ignored. Left drags orbit
    and middle/right drags pan. *)

val camera : t -> Camera.t
val target : t -> Rays_math.Vec3.t
val distance : t -> float
val fov_y : t -> float
val near : t -> float
val far : t -> float
val control_area : t -> (int * int * int * int) option
val inertia : t -> bool

val of_view : eye:Rays_math.Vec3.t -> target:Rays_math.Vec3.t -> t -> t
(** Orbit to look from [eye] at [target] about the current up axis (elevation clamps just short of
    the poles). Lens, clipping, and input settings are kept; inertia stops. *)

val frame_bounds : min:Rays_math.Vec3.t -> max:Rays_math.Vec3.t -> t -> t
(** Target the box center at [radius / tan (fov_y / 2) * 1.2] (radius: half the box diagonal),
    keeping the viewing direction. *)

val with_distance : float -> t -> t
val with_fov_y : float -> t -> t
val with_clip : near:float -> far:float -> t -> t
val with_control_area : (int * int * int * int) option -> t -> t
val with_inertia : bool -> t -> t
val reset : t -> t

val fly : speed:float -> t -> Frame.t -> t * float
(** One frame of WASD fly navigation: held W/S, D/A, and Q/E move forward, right, and up at [speed]
    units per second (Shift four times faster); [Frame.mouse_delta] yaws about the up axis and
    pitches within 89 degrees; each wheel step scales the returned speed by 1.2. The result is an
    ordinary orbit camera, so orbiting resumes seamlessly. *)
