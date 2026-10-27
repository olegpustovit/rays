(** The workspace document: a checked workspace, its layout keyed by path and the
    sketch settings.  UI-free.  The source text is the authored
    truth; [checked] is derived from it and every edit validates atomically.
    Same-type catalog literals reuse the term's check and validate its schema;
    other edits run the workspace check.

    The persisted text is the workspace form itself followed by optional
    [(layout ...)] ({!Layout_by_path}) and [(settings :name value ...)] forms
    (only settings that differ from their default). A [.rays] sketch can
    include those trailing forms as well. *)

type literal = { base : Flow.Workspace.t; edits : Literal_edit.change Layout_by_path.Path_map.t }
(** A span/term patch since [base], bounded to 4,096 distinct catalog arguments.
    It is derived data; it is never persisted. *)

type t = {
  source : Flow.Syntax.t list;  (** the one [(workspace ...)] form *)
  checked : Flow.Workspace.t;
  layout : Layout_by_path.t;
  settings : Settings.t;
  extra : Flow.Syntax.t list;
      (** the other root forms as they were written, for {!to_text}: a [(view ...)] is kept
          verbatim; [layout] and [settings] are kept for the comments above and inside them *)
  inputs : (string * (string * Flow.Eval.value) list) list;
      (** Host-supplied graph inputs; retained across edits and reloads,
          validated by lowering, never written into the source. *)
  literal : literal option;
  imports : (string * Flow.Syntax.t list) list;
      (** Read-only children spliced from each relative imported file. *)
  import_sources : (string * string) list;
      (** Original imported texts, retained for reload digests and rechecking. *)
  fragment : bool;
      (** A library of bare graph, definition or macro forms, printed without a workspace wrapper. *)
}

val import_paths : string -> (string list, Diagnostic.t list) result
val import_texts : t -> (string * string) list
val imported_file : t -> Flow.Workspace.path -> string option

val name : t -> string

val literal_changes : previous:t -> t -> Literal_edit.change list option
(** Changes on one literal-patch lineage, including several edits between frames.
    [None] means that structural checking/lowering must run. *)

val editor_graph : t -> Flow.Workspace.graph option
(** The selected named editor layout, or the first editor graph when none is selected. *)

val of_text :
  ?ops:Flow.Op.t list ->
  ?imports:(string * string) list ->
  ?inputs:(string * (string * Flow.Eval.value) list) list ->
  ?settings:Settings.t -> ?layout:Layout_by_path.t -> Flow.Check.catalog -> string -> (t, Diagnostic.t list) result
(** Parse and check. [ops] defaults to [Flow_sop.Operators.all]; an explicit
    list replaces those extensions. Absent settings and layout forms retain the supplied values.
    Bare graph, definition and macro forms load as a library fragment; its
    save writes those forms only, leaving editor layout and settings outside the library.
    An explicit settings form starts from its schema defaults; an explicit empty layout clears it.
    Errors are the checker's diagnostics, a parse error, or a layout or
    settings form that does not read ([E_LAYOUT], [E_SETTINGS], with the form's span). Unknown or
    duplicate document forms give [E_DOCUMENT_FORM].  A layout entry whose path is not in the
    checked workspace is dropped, and so is a selected editor layout that names no editor graph. *)

val check_text :
  ?ops:Flow.Op.t list ->
  ?imports:(string * string) list ->
  ?inputs:(string * (string * Flow.Eval.value) list) list ->
  ?settings:Settings.t -> ?layout:Layout_by_path.t -> Flow.Check.catalog -> string ->
  (t * Diagnostic.t list, Diagnostic.t list) result
(** {!of_text} with the checker's warnings. *)

val to_text : t -> string
(** Canonical text; read with the same settings schema at its defaults to
    recover the complete saved document.  Comments between and after the root forms and a
    [(view ...)] form are kept. *)

val print : t -> string * Flow.Lisp.spans
(** {!to_text} with the printer's authored form spans, for text gestures. *)

val edit : Flow.Check.catalog -> t -> Flow_graph.Flow_edit.op -> (t, Diagnostic.t) result
(** One gesture, atomically: the source is rewritten and re-checked, layout
    keys follow {!Flow_graph.Flow_edit.remap} and are pruned as in {!of_text}, and on an error
    nothing changes. *)
