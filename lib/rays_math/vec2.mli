type t = { x : float; y : float }

val create : float -> float -> t
val zero : t
val add : t -> t -> t
val sub : t -> t -> t
val to_string : t -> string
