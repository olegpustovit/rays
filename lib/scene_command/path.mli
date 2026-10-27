(** Backend-neutral paths, deterministic adaptive flattening, and fill tessellation. *)

type point = { x : float; y : float }

type command =
  | Move_to of point
  | Line_to of point
  | Quadratic_to of point * point
  | Cubic_to of point * point * point
  | Close

type t = private command array
type fill_rule = Even_odd | Non_zero
type mesh = { vertices : point array; indices : int array }
type cap = Butt | Square | Round
type join = Miter | Bevel | Round

type error =
  | Empty_path
  | Invalid_tolerance of float
  | Invalid_width of float
  | Invalid_miter_limit of float
  | Non_finite_point of point
  | Missing_current_point
  | Open_contour
  | Complexity_limit

val of_commands : command array -> t
val tessellate : tolerance:float -> fill_rule:fill_rule -> t -> (mesh, error) result

(** Tessellates the centerline. Degenerate segments are ignored. *)
val stroke :
  tolerance:float ->
  width:float ->
  cap:cap ->
  join:join ->
  miter_limit:float ->
  t ->
  (mesh, error) result
