type point = Scene_command.Path.point = { x : float; y : float }
type command = Scene_command.Path.command =
  | Move_to of point
  | Line_to of point
  | Quadratic_to of point * point
  | Cubic_to of point * point * point
  | Close
type t = Scene_command.Path.t
type fill_rule = Scene_command.Path.fill_rule = Even_odd | Non_zero
type mesh = Scene_command.Path.mesh = { vertices : point array; indices : int array }
type cap = Scene_command.Path.cap = Butt | Square | Round
type join = Scene_command.Path.join = Miter | Bevel | Round
type error = Scene_command.Path.error =
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
val stroke : tolerance:float -> width:float -> cap:cap -> join:join ->
  miter_limit:float -> t -> (mesh, error) result
