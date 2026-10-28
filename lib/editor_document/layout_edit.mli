(** The editor graph's layout edits: a gesture on the panels of its [(ui/workspace ...)] as a pure
    rewrite of the workspace source, built on {!Flow_graph.Edit_tree}. They address a binding of an
    editor graph by its path, as the {!Flow_graph.Flow_edit} ops do; panels made by a loop have no
    binding, so they are edited on the loop. {!Workspace_doc.edit_layout} applies one. *)

type path = Flow.Workspace.path

type op =
  | Set_layout_size of { node : path; size : [ `Ratio of float | `First of int | `Second of int ] }
      (** the binding [node] of an editor graph is a [ui/split] or [ui/split-at]; it becomes
          [ui/split-at] with this ratio (clamped to 0.1-0.9, four decimals), or a [ui/split] with
          [:first_size] or [:second_size] in whole points (1 or more) *)
  | Split_panel of { node : path; axis : [ `H | `V ]; ratio : float; panel : Flow.Syntax.t }
      (** the panel [node] becomes the first half, [ratio] of the extent (clamped to 0.1-0.9), of a
          new [ui/split-at]; the second is [panel] *)
  | Close_panel of { node : path }
      (** the split holding panel [node] becomes the other half; a panel outside a split does not
          close *)
  | Dock_panel of { node : path; target : path; side : [ `Left | `Right | `Top | `Bottom ] }
      (** Move a panel beside another in the same scope; collapse the split/tile it leaves. *)
  | Set_panel_kind of { node : path; panel : Flow.Syntax.t }
      (** the binding [node] becomes [panel]. The panel forms are the editor's ({!Panel_kinds}):
          this module knows no panel kind. *)
  | Set_layout of { graph : string; index : int }
      (** the editor graph [graph] has a [(ui/switch ...)] at the root of its workspace: its
          [:active] becomes [index]. One history entry: repeats merge ({!gesture}). *)
  | Layout_new of { graph : string }
      (** a copy of the active layout (its splits, tiles and floats copied, its panels shared) joins
          the switch and becomes active; a workspace without a switch gets one around its tree
          first. Ten layouts at most. *)
  | Merge_layouts of { graph : string }
      (** an older file with several [:context editor] graphs: the others join [graph]'s switch
          (made around its tree when it has none) as layouts, their bindings renamed [other_name],
          and are removed. Ten layouts at most. *)
  | Merge_then_new of { graph : string }  (** {!Merge_layouts}, then {!Layout_new}, as one edit *)
  | Layout_remove of { graph : string }
      (** the active layout leaves the switch (and the bindings only it read); the last stays *)
  | Layout_window of { graph : string; panel : Flow.Syntax.t }
      (** [panel] in a new [(ui/floating ...)] window of the active layout *)
  | Layout_float of { graph : string; at : int list }
      (** the docked panel at tree path [at] of the active layout becomes a floating window, and a
          floating one docks beside the rest *)

val label : op -> string
(** The history label: "Split panel", "New layout", ... *)

val gesture : op -> string option
(** A history merge key for an edit that repeats ({!Set_layout}), [None] for the others. *)

val paths : op -> path list
(** The paths the edit reads, for the read-only imported-form check. *)

val merges : op -> bool
(** The edit folds the other editor graphs into one: their layout keys are gone. *)

val remap : op -> path -> path option
(** Where a layout key goes after the edit: a closed panel's key goes ([None]), the rest stay. *)

val apply_checked :
  ?ops:Flow.Op.t list ->
  ?library:bool ->
  ?checker:
    (Flow.Catalog.catalog -> Flow.Syntax.t list -> Flow.Workspace.t option * Diagnostic.t list) ->
  Flow.Catalog.catalog ->
  Flow.Syntax.t list ->
  op ->
  (Flow.Syntax.t list * Flow.Workspace.t, Diagnostic.t) result
(** The rewritten source (printed and parsed again) and its check, as
    {!Flow_graph.Flow_edit.apply_checked}. *)

val changes_nodes : op -> bool
(** The edit adds or removes nodes, so layout keys are pruned after it (a resize does not). *)
