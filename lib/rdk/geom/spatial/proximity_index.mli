(** Spatial nearest-entity queries over packed primitive and edge features. *)
type features
type t

val primitive_features :
  ?cancel:Cancel.t -> ?grain:int -> Geometry.t -> (features, string) result
val edge_features :
  ?cancel:Cancel.t -> ?grain:int -> Geometry.t -> (features, string) result
val create : ?cancel:Cancel.t -> ?grain:int -> features -> t
val nearest_entities_with_distances :
  ?cancel:Cancel.t -> ?grain:int -> max_distance:float ->
  t -> features -> (int array * float array, string) result
