(** Test support: a SOP graph written as Rays Lisp text, compiled to a
    {!Sop.Node.t} by the same lowering the editor uses. *)

val node_result : ?with_:(string * Sop.Node.t) list -> string ->
  (Sop.Node.t, string) result
(** [node_result text] lowers the SOP expression [text] (for example
    ["(-> (sop/grid :columns 8) (sop/dissolve))"]) as the result of a [sop]
    graph. [~with_:["src", n]] makes [(sop/ext_src)] stand for the existing
    node [n], for geometry only OCaml can build (see {!snapshot}). A binding
    ["image/pixels", n] makes [(image/ext_pixels)] an image-typed external.
    Both kinds retain the original node and its inputs without cooking it
    during checking or lowering. *)

val node : ?with_:(string * Sop.Node.t) list -> string -> Sop.Node.t
(** As {!node_result}; a lowering error fails the test with its message. *)

val workspace_node : ?with_:(string * Sop.Node.t) list -> string -> Sop.Node.t
(** Compile a complete workspace, including shared definitions, and return the
    first default SOP graph's result. External nodes work as in {!node_result}.
    Checking or lowering errors fail the test with their message. *)

val snapshot : Rdk.Geometry.t -> Sop.Node.t
(** A generator node holding an already built geometry: the one OCaml-only
    source, used through [~with_]. *)

val float : float -> string
(** A float as a Lisp literal that reads back as the same float. *)

val vec3 : Rays_math.Vec3.t -> string
val curve_points : (float * float * float) array -> string
(** [[x y z] ...] for [(sop/curve ...)]. *)
