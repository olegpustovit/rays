(** The graph pane's view of a checked workspace: nodes, zones,
    rows, chips and their layout, with no drawing.  A port of the study's
    [buildView] / [buildScope] / [mkNode] / [argRows] (e1.js) and
    [layoutScope] / [nodeSize] / [place] (e2.js).

    Identity is the [Flow.Workspace.path] of a binding, so a selection, a
    probe or a layout position survives every edit that keeps the path.  Every
    row carries the {!Flow_edit.arg_key} of the input it shows: a wire dropped
    on a row is [Flow_edit.Connect { node; key; ... }], a scrubbed number is
    [Flow_edit.Set_arg], the [Add] row's key is where the new input goes.

    Structure comes from the authored syntax (the keys and the notes live
    there); types, liveness and loop invariance come from the checker.
    A call of a node kind written in an input ({!Flow_edit.node_call}: the steps of a
    [->], a call inside a call) is a node like a bound one, placed before the node that
    holds it and wired to its row; its path ends in {!Flow_edit.nested_leaf} and it has
    no binding until it is renamed.
    Inline higher-order calls are cards and their [fn] inputs are zones with
    parameter rails, call selectors and editable body cards. Inline loops and
    [let*] scopes remain chips until unfolded ({!Flow_edit.Unfold}), as do
    folded expressions, [ref]s and calls of functions and macros. *)

type path = Flow.Workspace.path

type chip =
  | No_value  (** nothing written: the row shows its default *)
  | Const  (** a number, text, flag or vector: the row's [expr] is scrubbable *)
  | Name of string
      (** a wire from a binding, loop variable or input ([a], [a.field]), or from the nested
          node written in the row (its leaf; the row's [expr] is still the call) *)
  | Inline of { glyph : string; text : string }
      (** a nested call [ƒ], loop [for] / [Σ] / [⟲], function [λ], record
          [{}] or macro call [◆], with its flat Lisp text *)

type row_kind =
  | Arg
  | Rest  (** one item of a repeating input *)
  | Add  (** the [+ input] / [+ field] row; its key is where the new one goes *)
  | Hole  (** a macro parameter *)
  | Binder  (** a macro parameter the template uses as a loop or scope name *)
  | Group_reader
  | Group_writer

type control =
  | Plain
  | Range of float * float  (** a number with a soft range: the field draws its position line *)
  | Choice  (** one of a list of names: the field ends in a chevron *)

type row = {
  label : string;
  key : Flow_edit.arg_key;
  ty : Flow.Ty.t option;
  expr : Flow.Syntax.t option;  (** what is written there *)
  chip : chip;
  default : string option;  (** the default a missing argument takes, as text *)
  socket : bool;  (** a wire can land here (literal-only text and choices: no) *)
  kind : row_kind;
  control : control;
  folder : string;  (** the schema folder the row sits in (shown at level [Full]); empty for none *)
  head : bool;  (** the first geometry slot of a node kind: the header's in-port, not a row of the body *)
}

type zone_kind = For | Fold | Scan | Sum | Let | Fn | State | Branch
type role = Var | Acc | Param | Capture

type rail_row = {
  name : string;  (** the pattern's text *)
  names : string list;  (** what it declares *)
  role : role;
  ty : Flow.Ty.t option;
  expr : Flow.Syntax.t option;  (** the collection or initial value; [None] for [Param] and [Capture] *)
  key : Flow_edit.arg_key option;  (** [Bv] into the zone form; [None] for [Param] and [Capture] *)
}

type input = { path : path; name : string; ty : Flow.Ty.t; default : Flow.Syntax.t option }

type lens = { steps : string array; error : string option; template : string }
(** A macro call and its expansion, one step at a time: [steps.(0)] is the call as
    written, [steps.(k)] the text after [k] {!Flow.Macro.expand_once} steps (at most 12);
    [error] is the diagnostic that stopped them.  [template] is the macro's definition, which
    the lens shows as one step more than [steps] ({!lens_height}'s [step] past the last). *)

type node = {
  path : path;
  name : string;
      (** the binding's text; [@result] for a synthetic result node; the leaf of a nested node *)
  binds : string list;  (** the names it declares: [name], or a pattern's names *)
  head : string;  (** [record], [number], [link], [vector], a call head, a zone keyword *)
  rows : row list;  (** empty for a zone *)
  outputs : (string * Flow.Ty.t) list;  (** record fields, or the names of a pattern *)
  ty : Flow.Ty.t;
  note : string option;
  bypass : bool;
  macro : string option;  (** the macro this node calls *)
  lens : lens option;  (** the expansion, for a macro call *)
  live : bool;  (** depends on [t] (◷) *)
  invariant : bool;  (** the same each iteration of its loop (↥) *)
  synthetic : bool;
  zone : zone option;
  expr : bool;  (** an operator call written in an input, drawn as a card while its leaf is not folded *)
}

and zone = {
  kind : zone_kind;
  rail : rail_row list;
  scope : scope;
  yield_label : string;  (** collect, next, add, result, return *)
  order : string option;
      (** a loop over [sop/point_list] or [sop/piece_list] says how its elements are ordered:
          ["by index"], or ["by id"] for [:key "id"] *)
}

and scope = {
  path : path;
  inputs : input list;  (** graph inputs; empty below the root *)
  nodes : node list;  (** in authored order, the synthetic result last *)
  result : result;
}

and result =
  | Link of string
  | Node of path  (** the synthetic [@result] node *)
  | Literal of Flow.Syntax.t

val find_graph : Flow.Workspace.t -> string -> Flow.Workspace.graph option
(** The graph [of_graph] would project (a [def:] name finds a definition), or [None]. *)

val of_graph : ?folded:(path -> bool) -> Flow.Catalog.catalog -> Flow.Workspace.t -> string -> scope
(** The root scope of a graph or, for ["def:name"] or a [defn] name, of a
    definition.  Raises [Invalid_argument] when there is none.  An operator call written in an input
    ({!Flow_edit.value_call}) is a card ([expr]) unless [folded] its leaf (default: all are), and
    its whole call tree with it. *)

val same_graph : Flow.Workspace.t -> Flow.Workspace.t -> string -> bool
(** Whether the graph's authored content, checked types, live/invariant paths
    and macro environment still describe the same projection. *)

val anonymous : node -> bool
(** A nested node: a call written in an input, with no binding. *)

val title : node -> string
(** What the card is called: the binding, [result], or the kind of a nested node. *)

val sources : row -> string list
(** The names a row is wired from: the nested node written in it, else every name its
    expression reads. *)

val with_arguments : (path * Flow_edit.arg_key * Flow.Syntax.t) list -> scope -> scope
(** Patch existing same-type literal rows. Node identities, port types, liveness,
    row order and layout stay unchanged; structural edits use [of_graph]. *)

val bypassable : node -> bool
(** A call whose first input fits its result, or one already bypassed: it can carry the
    [B] flag (the checker refuses the rest with [E_BYPASS]). *)

(** A node whose positional inputs are an ordered list the user can move items in: a [list], a
    [str] or a [scene/merge] (the order of the merge is the order of the scene). *)
val reorderable : node -> bool

val find : scope -> path -> node option
(** The node at a path, searching zones. *)

val zones : scope -> node list
(** Every zone below the scope, outer first. *)

(** {2 Levels}
    See flow.md 6.4. *)

type level = Point | Chip | Card | Full
(** Point: a 14-point disc and the name; Chip: the header with a [+N] count; Card: the rows the
    exposure rule shows; Full: every row under its folder labels. *)

val level_name : level -> string
val level_of_name : string -> level option

val value_card : node -> bool
(** A literal binding: a header-only card with the value in a field (the sheet's value node). *)

type line =
  | Folder of int * string  (** the n-th folder label row of a [Full] card *)
  | Row of int * row  (** [n.rows.(i)] *)

val row_shown : ?pin:bool -> row -> bool
(** Is the row on its node's card ({!Exposure.shown}): written, driven, primary, or pinned
    by [pin] (a pin of [false] hides it); structural rows always show. *)

val lines : ?pin:(string -> bool option) -> level -> node -> line array
(** The body of a card at a level, in order; the header slot, and every row of a value card, a
    zone, a chip or a point, are not lines.  [pin] is a row's pin by label ([layout.rows]). *)

val set_count : node -> int
(** What the chip's [+N] says: the rows with something written. *)

val point_title : node -> string

(** {2 Layout}
    Logical points on the 24-point dot lattice. *)

type item = Input of input | Item of node | Return

type placed = {
  item : item;
  path : path;
  x : float;
  y : float;  (** relative to the scope's origin *)
  w : float;
  h : float;
  collapsed : bool;
  inner : layout option;  (** an expanded zone's own layout *)
  level : level;  (** the requested level *)
  lines : line array;  (** the card's body at that level *)
  shown : level;
  (** the level it is drawn at: [level], except a zone and the return card (the pane sets it, and then
      [w] and [h] are the box of that level); [layout] returns it equal to [level] *)
}

and layout = { placed : placed list; w : float; h : float }

val layout :
  ?foot:bool -> ?at:(path -> (float * float) option) -> ?prev:(path -> (float * float * float * float) option) -> ?collapsed:(path -> bool) ->
  ?lens:(path -> int option) -> ?level:(path -> level) -> ?pin:(path -> string -> bool option) ->
  ?inner:bool -> scope -> layout
(** Columns by dependency depth, inputs first and the return last; a node
    stacks below its column's previous one.  [at] overrides a node's position
    ([Layout_by_path.at]), [collapsed] folds a zone to its card, [lens] gives the step of
    a macro call whose expansion panel is open (the card grows by {!lens_height} and
    widens to {!lens_width}), [foot] reserves a footer row under every card and zone (the
    host has probe records to show; default none).  A zone's size
    comes from its inner layout, recursively.  ponytail: no crossing
    minimisation (like [automatic_layout]). *)

val lens_width : float
val lens_height : lens -> step:int -> float
(** The expansion panel under a macro call: a row of step buttons, the printed step
    (at most 16 lines of 20 points) and the button row. *)

val rail_top : node -> float
(** Where a zone's cards start below its top: the label row and 28 points of air, plus a row
    for each rail row beyond the label ({!extra_rails}). *)

val label_row : zone -> rail_row option
(** The rail row the zone's label row shows: its first loop variable. *)

val extra_rails : zone -> rail_row list
(** The rail rows drawn under the label row (accumulators, further variables, parameters): a
    plain [for] has none, so a loop adds nothing it does not use. *)

val lattice : float
(** The dot grid (24): cards and columns sit on it. *)

val snap : float -> float
val zone_pad_x : float
val point_size : float

val row_height : float
val head_height : float
val foot_height : float
val body_top : float
(** Where a card's rows start: one point above the header's bottom (kit rev 3). *)

val card_pad : float
(** The 4 points of padding and the border under a card's last row. *)

val card_height : rows:float -> extra:float -> float
(** A card of [rows] 24-point rows and [extra] points of footer or panel: the 24-point header
    alone when it has neither. *)

val node_width : float
