(** Deterministic point clustering and representative order. *)
type metric = Euclidean | Componentwise

type clusters = {
  count : int;
  of_point : int array;
  offsets : int array;
  members : int array;
  representatives : int array;
}

type t = Identity | Clusters of clusters

val of_links :
  ?cancel:Cancel.t -> operation:string -> int array -> (t, string) result

val create :
  ?cancel:Cancel.t -> ?selection:Group.t -> ?metric:metric ->
  ?inclusive:bool -> operation:string ->
  tolerance:float -> compatible:(int -> int -> bool) ->
  Geometry.t -> (t, string) result
