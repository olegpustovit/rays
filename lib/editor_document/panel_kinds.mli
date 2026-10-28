(** The editor's panel kinds, as the forms the layout gestures of
    {!Flow_graph.Flow_edit} ([Split_panel], [Set_panel_kind], [Layout_window]) write. *)

val expr : Flow.Syntax.t list -> string -> (Flow.Syntax.t, string) result
(** [expr source kind]: [outline], [graph], [list], [lisp], [inspector], [spreadsheet],
    [timeline], [viewport] (over the first scene graph of the workspace [source]), [canvas] (over
    its first draw graph), or ["graph:name"] for a graph panel showing that graph. *)

val beside : string -> string
(** The kind a split puts beside a panel of this kind: a graph beside a lisp panel, else a lisp
    panel. *)
