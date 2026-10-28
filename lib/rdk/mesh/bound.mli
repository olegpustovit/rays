(** Selected geometric bounds and generated box or sphere surfaces. *)

type deform_selection = Deform.selection =
  | Selected_points of Group.t
  | Selected_vertices of Group.t
  | Selected_primitives of Group.t
  | Selected_edges of Edge_group.t

type bound_shape =
  | Bound_box of { divisions : int * int * int }
  | Bound_sphere of { segments : int; rings : int; minimum_radius : float }

module Private : sig
  val selected_bounds :
    ?cancel:Cancel.t -> grain:int -> operation:string ->
    deform_selection option -> Geometry.t -> (Analysis.bounds, string) result
end
