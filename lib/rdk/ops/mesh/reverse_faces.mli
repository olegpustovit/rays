type operation = Reverse_vertices | Shift_vertices of int

val run : ?cancel:Cancel.t -> Geometry.t -> (Geometry.t, Error.t) result
