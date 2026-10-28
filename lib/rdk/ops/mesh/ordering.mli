val sort_primitives :
  ?cancel:Rdk_core.Cancel.t ->
  ?grain:int ->
  name:string ->
  Rdk_core.Geometry.t ->
  (Rdk_core.Geometry.t, Rdk_core.Error.t) result
(** Stable reorder of primitives by an integer primitive attribute. *)
