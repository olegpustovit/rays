(** Packed median-split bounds shared by the linear-piece and proximity trees. *)

type t = {
  order : int array;
  min_x : float array;
  min_y : float array;
  min_z : float array;
  max_x : float array;
  max_y : float array;
  max_z : float array;
  left : int array;
  right : int array;
  first : int array;
  count : int array;
}

val create :
  ?cancel:Cancel.t ->
  ?extend:(int -> int -> int array -> int -> int -> unit) ->
  grain:int ->
  centroid_x:float array ->
  centroid_y:float array ->
  centroid_z:float array ->
  item_min_x:float array ->
  item_min_y:float array ->
  item_min_z:float array ->
  item_max_x:float array ->
  item_max_y:float array ->
  item_max_z:float array ->
  unit ->
  t
