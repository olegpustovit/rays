(** The workspace text pane: Selection, Graph and Document tabs over
    the Lisp text.  Pure state and text plus [view], which draws and returns
    intents for [Editor_frame] to apply after the frame. *)

type tab = Selection | Graph | Document
type path = Flow.Workspace.path

type shown = {
  graph : string;  (** the graph the Selection and Graph tabs read *)
  text : string;
  mark : (int * int) option;  (** byte span of the selected binding in [text] *)
  key : path;  (** what the Selection tab shows and its apply writes: the selected binding's
                   path, else [[graph]] *)
  applied : string Lazy.t;  (** the whole document's text (the draft's dirty mark) *)
  body : (Flow.Syntax.t * (int * Diagnostic.span) list) option;
      (** the Graph tab's graph body and the spans of [text]: what the caret is looked up in *)
  source : Flow.Syntax.t list;
  spans : Flow.Lisp.spans;
  applied_spans : Flow.Lisp.spans Lazy.t;
}

type find = { query : string; replace : string; case : bool; replacing : bool; at : int; grab : bool }
(** The find bar: what to find and its replacement, whether case counts, whether the replace row is
    open, the byte the current match is the first at or after, and whether the query field takes
    the keyboard this frame. *)

type command = Find | Find_replace | Find_selection | Find_next | Find_previous | Definition | References
  | Select of int * int
(** What the keys ask of the pane: the find bar (seeded from the selection for [Find] and
    [Find_selection], the latter without taking the keyboard; [Find_replace] opens the replace row),
    the next or previous match, the definition or the references of the symbol under the caret, or a selection of the text. *)

type request = { select : int * int; splice : (int * int * string) list }
(** What the text area is asked once: replace the ranges (one undo step), then select. *)

type checked = { at : string; found : Diagnostic.t list }
(** The diagnostics found in the text [at]. A later text shows one only while the text up to the end
    of its line (and its span) still reads the same, see {!visible}: no mark names a line it has
    drifted from. *)

type slot = { gtext : string; gbase : Editor_document.Workspace_doc.t option; gerrors : checked }
(** One graph's Graph tab draft, the document it began from and the errors of its last apply. *)

type live = { seen : string; since : float; found : Diagnostic.t list option }
(** The parse of a draft: [found] is [None] until [seen] has rested for the debounce, and belongs
    to [seen] alone (another text drops it). *)

type state = {
  tab : tab;
  draft : string option;  (** the Document tab's unapplied text *)
  binding_draft : (path * string) option;  (** the Selection tab's unapplied text, keyed by [shown.key] *)
  graphs : (string * slot) list;  (** the Graph tab's unapplied texts: one slot per graph *)
  doc_base : Editor_document.Workspace_doc.t option; (** base document of the draft *)
  binding_base : Editor_document.Workspace_doc.t option;
  doc_errors : checked;  (** of the last refused apply *)
  binding_errors : checked;
  live : live option;
  wrap : bool;  (** long lines continue on the next row (the right-click menu toggles it) *)
  parinfer : bool;  (** the closing brackets follow indentation, {!Lisp_text.parinfer_text} (the
                        right-click menu toggles it; on by default) *)
  menu : (float * float) option;  (** the right-click menu while it is open *)
  picker : (int * int * bool) option;  (** the colour literal being edited: byte range, edited yet *)
  scrubbing : bool;  (** a successfully applied token scrub; retain printed line breaks until release *)
  target : path option;  (** a definition revealed in the Document tab *)
  find : find option;  (** the find bar while it is open *)
  pending : command list;  (** commands {!view} resolves this frame, where the caret and selection are known *)
  request : request option;
  nonce : int;  (** raised to scroll the same offset into view again *)
  cache : ((Flow.Syntax.t list * Editor_document.Workspace_doc.t option * string * path option * tab) * shown) option;
}

val initial : state

val no_errors : checked

val graph_slot : state -> string -> slot option

val visible : checked -> string -> Diagnostic.t list
(** The diagnostics of [checked] that still hold for a text. *)

val live_delay : float
(** Seconds a draft rests before {!view} parses it. *)

val binding : Flow.Syntax.t list -> path -> (Flow.Syntax.t option * Flow.Syntax.t) option
(** The binding a path names in the source: its pattern (none for a [@result]) and expression. *)

val graph_op : Flow.Syntax.t list -> graph:string -> ?selection:path -> string ->
  (Flow_graph.Flow_edit.op, Diagnostic.t) result
(** The [Set_graph] of the graph form a Graph tab holds, or (with [selection], the shown binding's
    path) of the Selection closure patched into the graph; the error of text that is neither. *)

val paste_ops : Flow.Syntax.t list -> graph:string -> scope:path -> string ->
  (Flow_graph.Flow_edit.op list, string) result
(** Pasted text as [Add_node]s of [scope]: [name expr] pairs, or bare expressions named by their
    head.  Names pair by position and are made free of the graph's and of each other; a pasted
    binding that reads an earlier pasted one reads its new name (all renames at once). *)

val merge3 : base:string -> mine:string -> theirs:string -> string option
(** A draft ([mine]) begun from [base], against the text the document has now ([theirs]): the
    three merged on the syntax tree, graphs and [let*] bindings paired by name.  [None] when both
    changed the same value or the draft does not read. *)

val make_shown : Flow.Syntax.t list -> string -> path option -> tab -> shown
(** [make_shown source graph selected tab]: Selection prints the selected
    binding's top-level ancestor as a [let*] over the root bindings it reads
    (a note names the count and the graph inputs used) with the binding marked;
    Graph the whole graph form; Document the workspace form. *)

val scrub_op : shown -> int * int -> string -> Flow_graph.Flow_edit.op option
(** A numeric token reported by PXUI, looked up through printed spans and mapped
    to its innermost source card's [Set_arg]. No parsing or textual search. *)

val shown : ?workspace:Editor_document.Workspace_doc.t -> state -> source:Flow.Syntax.t list -> graph:string -> selected:path option ->
  state * shown
(** {!make_shown} for the state's tab, cached by source, optional workspace, graph,
    selection and tab. A workspace includes saved layout and settings in the Document tab. *)

val line_of : string -> Diagnostic.t -> int option
(** The 1-based line of a diagnostic in the text it was checked from: its
    position, else its span. *)

val binding_at : shown -> int -> path option
(** The innermost binding or inline node at a byte in any tab. *)

val reference_target : Flow.Syntax.t list -> from:path -> string -> path option
(** Resolve a read in its lexical scope, including dotted record outputs. *)

val definition_at : shown -> int -> path option
(** The definition of the symbol at a printed byte, including call heads. *)

val cameras : Flow.Syntax.t list -> string -> string list
(** The bindings of a scene graph that are [scene/camera] calls. *)

val first_error : state -> Diagnostic.t option

val summary : state -> string
(** One line for crash reports: tab, draft, first error and its line. *)

type intent =
  | Tab of tab
  | Doc_draft of string
  | Doc_apply of string  (** Check & apply the whole workspace text *)
  | Doc_discard
  | Binding_draft of path * string
  | Binding_apply of path * string
      (** Check & apply of Selection: patch the shown closure's named bindings
          into one candidate graph, preserving omitted bindings and refusing
          changed results or duplicate names. Check and lower once via
          {!Flow_graph.Flow_edit.Set_graph}. *)
  | Binding_discard
  | Graph_draft of string * string  (** the graph's name and its draft text *)
  | Graph_apply of string * string  (** Check & apply: the graph's form is replaced ({!Flow_graph.Flow_edit.Set_graph}) *)
  | Graph_discard of string  (** the graph whose draft is dropped *)
  | Live of live  (** the debounced parse of the draft *)
  | Menu of (float * float) option  (** the right-click menu opened here, or closed *)
  | Toggle_wrap
  | Doc_scrub of string * bool
      (** the Document text while a number in it is dragged ([true] on the frame the drag ends):
          applied live, one history entry *)
  | Graph_scrub of string * string * bool
  | Binding_scrub of path * string * bool
  | Literal_scrub of { source : Flow.Syntax.t list; op : Flow_graph.Flow_edit.op;
      position : Diagnostic.position; fallback : intent }
      (** A token edit against the source shown by the pane. A stale source or
          unapplied draft takes the ordinary scrub/merge path. *)
  | Toggle_parinfer
  | Picker of (int * int * bool) option
      (** the colour literal (byte range with its quotes) whose control is open, and whether an edit was made *)
  | Open_graph of string  (** Command-click on a [(ref name)]: show that graph (the back stack remembers this one) *)
  | Select_binding of path  (** the caret moved into this binding of the Graph tab: select its node *)
  | Jump_definition of path  (** follow and reveal a definition *)
  | Carry_over of int * bool
      (** a payload is held over the text at this byte of the shown text; [true] on the frame it is released *)
  | Find_set of find option * bool  (** the bar's new state, and whether the view goes to its current match *)
  | Request of request option  (** a request given to the area, or taken *)
  | References_of of string  (** Shift-F12: the symbol under the caret, whose uses the editor lists *)
  | Resolved  (** the pending commands were resolved *)

val find_all : case:bool -> string -> string -> (int * int) list
(** [find_all ~case query text]: the (start, stop) of every non-overlapping match, in order;
    ASCII letters fold together unless [case]. *)

val find_current : find -> (int * int) list -> (int * int) option
(** The match the bar is on: the first at or after [at], else the first. *)

val find_step : find -> (int * int) list -> [ `Next | `Previous ] -> find
(** The bar after Next or Previous (wrapping at either end). *)

val find_replace : find -> (int * int) list -> all:bool -> (request * find) option
(** Replace the current match, or all, as one request, and the bar after it. *)

val symbols : string -> (string * string * int) list
(** The bindings of a text as (name, kind, byte of the name): graphs, functions, macros and
    [let*] names, in order; empty for text that does not read. *)

val symbol_at : string -> int -> string option
(** The symbol the byte [at] touches; [None] for anything else or text that does not read. *)

val references : string -> string -> (int * string) list
(** [references text name]: the byte and the line's text of every use of the symbol [name], in order. *)

val line_target : string -> string -> int option
(** [line_target text "12:5"]: the byte of that 1-based line and character column ([":5"] is
    optional; past the end is clamped); [None] for anything else. *)

val current_text : state -> shown -> string
(** What the tab shows and edits: its draft when it has one. *)

val command : state -> command -> state

val tabs_width : Pxui.Ui.t -> width:int -> state -> shown -> float
(** The room the Selection / Graph / Document tabs of {!view} take in the header of a panel
    [width] wide. *)

val view : Pxui.Ui.t -> bounds:int * int * int * int -> tabs_right:float option -> vocab:Lisp_text.vocab -> names:Lisp_text.names -> time:float -> state -> shown -> intent list
