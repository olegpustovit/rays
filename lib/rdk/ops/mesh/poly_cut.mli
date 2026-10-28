type element = Poly_cut_points | Poly_cut_edges
type strategy = Poly_cut_remove | Poly_cut_cut

type detection =
  | Poly_cut_all
  | Poly_cut_crossing of { attribute : string; value : float }
  | Poly_cut_change of { attribute : string; threshold : float }

type params = {
  cancel : Cancel.t option;
  grain : int option;
  primitives : Group.t option;
  cut_points : Group.t option;
  cut_edges : Edge_group.t option;
  element : element;
  strategy : strategy;
  detection : detection;
  keep_closed : bool;
}

val cut : params -> Geometry.t -> (Geometry.t, Error.t) result
(** Packed polygon-curve cutting kernel. *)
