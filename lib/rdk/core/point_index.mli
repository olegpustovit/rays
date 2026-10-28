(** Cached point-to-vertex incidence for a packed topology. *)
type t
val create : ?cancel:Cancel.t -> Topology.t -> t

type index = t
module View : sig
  type view = {
    primitive_of_vertex : int array;
    point_offsets : int array;
    point_vertices : int array;
  }
  val view : index -> view
end
