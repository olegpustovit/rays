(** The document reducers: every edit of a {!Document.t} is rewritten, checked and lowered
    atomically, or refused and changes nothing. *)

val update_geometry :
  (Sop.Edit_graph.t -> (Sop.Edit_graph.t, string) result) ->
  Flow_sop.Network.t -> (Flow_sop.Network.t, string) result

val cook_effects : Param.effects

val apply_parameters :
  Flow_sop.Network.t -> node_id:int -> (string * Param.value) list ->
  (Flow_sop.Network.t * Param.effects, string) result

val relabel : Flow_sop.Network.t -> node_id:int -> string -> (Flow_sop.Network.t, string) result

val syntax_edit_result :
  factories:Sop.Edit_graph.factory list -> Document.t -> Flow_graph.Flow_edit.op ->
  (Document.t, Diagnostic.t) result

val syntax_edit :
  factories:Sop.Edit_graph.factory list -> Document.t -> Flow_graph.Flow_edit.op ->
  (Document.t, string) result
(** One gesture on the text. *)

val syntax_batch :
  factories:Sop.Edit_graph.factory list -> Document.t -> Flow_graph.Flow_edit.op list ->
  (Document.t, string) result
(** Several gestures, all or none. *)

val text_edit :
  ?imports:(string * string) list -> factories:Sop.Edit_graph.factory list -> Document.t -> string ->
  (Document.t, Diagnostic.t list) result

val layout_edit : Document.t -> (Layout_by_path.t -> Layout_by_path.t) -> Document.t
(** A change to the layout keys only: the source and the lowering are untouched. *)

val select_layout :
  factories:Sop.Edit_graph.factory list -> Document.t -> string -> (Document.t, string) result

val reconcile :
  factories:Sop.Edit_graph.factory list -> ?adopt:bool -> Document.t -> Document.t ->
  (Document.t, string) result

val panel_node :
  factories:Sop.Edit_graph.factory list -> ?loop_message:string -> Document.t ->
  Editor_core.Panels.path -> (Document.t * string list, string) result
