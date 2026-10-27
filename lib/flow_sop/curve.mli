(** The lowered [sop/curve]: authored [points] use a lossless text encoding
    for parameter views; live drives carry the Vec3 array directly.
    [closed] joins the last point to the first. *)

val parameter : string
(** ["points"]. *)

val closed_parameter : string
(** ["closed"]. *)

val filled_parameter : string
(** ["filled"]: the closed curve is one polygon surface, not a polyline. *)

val encode : (float * float * float) array -> string
(** Lossless (hexadecimal floats), deterministic. *)

val factory : Sop.Edit_graph.factory

val replace_points : source:Sop.Node.t -> closed:bool -> filled:bool ->
  (float * float * float) array -> Sop.Node.t
(** Rebuild from typed points while retaining the source node's identity and label. *)
