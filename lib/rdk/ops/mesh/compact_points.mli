val run :
  ?cancel:Cancel.t -> ?grain:int -> Geometry.t -> (Geometry.t, Error.t) result
