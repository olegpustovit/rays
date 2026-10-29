(** Every graph gesture as a pure rewrite of the workspace source (a port of the study's [e1.js] /
    [e2.js] primitives).

    A gesture names its target by a {!type:Flow.Workspace.path}: the scope path of the binding, then
    its name ([["flower"; "ring"; "u"]], or [["g"; "@result"]] for a body result). Nothing is stored
    beside the text, so layout keyed by path survives every edit that keeps the path.

    [apply] rewrites the syntax tree, prints it canonically, parses the text again (fresh ids and
    spans) and checks it with {!Flow.Workspace.check}: an edit that does not check is refused whole
    and nothing changes. Comments (notes) travel with the binding name they precede, so reordering,
    rewiring, renaming, folding and wrapping keep them.

    A call of a node kind or a higher-order call/function written in an argument ({!node_call}) is a
    node too, with no binding: its leaf is the leaf of the node that holds it, then [#] and the
    input ({!nested_leaf}: [["g"; "result#0#:cutters"]]). Every op that edits an input takes such a
    path; [Rename] binds the node under the new name, [Delete_nodes] hands its place to its first
    input, and a wire that leaves it or lands where it is written ([Connect]) binds it first, so a
    new wire never deletes a node. [Disconnect] removes what is written, a nested node included:
    [Unfold] it first to keep it. A nested [fn] is a scope: body edits use its nested leaf as a
    scope segment.

    Child indices ([sub], [Bv]) count {!Flow.Syntax.children}: for a list the head is child 0. *)

type path = Flow.Workspace.path

(** The input a row of the graph edits. *)
type arg_key =
  | Whole  (** the whole binding expression *)
  | Pos of int  (** positional argument *)
  | Kw of string  (** [:keyword] argument of a call *)
  | Field of string  (** field of a [{...}] record *)
  | Bv of int * int  (** child [j] of child [i], for a zone's binding vectors *)
  | Arm of int  (** Conditional arm: zero-based then arm, [-1] for else. *)

type loop =
  | For
  | Fold
  | If
  | Map
  | Filter
      (** Repeat, Iterate, a conditional, or the selection as the body of a [map] / [filter] over
          [range]. *)

(** ponytail: one variant and one [apply]; no command objects. The editor graph's layout edits are
    [Editor_document.Layout_edit]. *)
type op =
  | Set_arg of { node : path; key : arg_key; sub : int list; value : Flow.Syntax.t }
      (** [sub] addresses a part of the argument (a vector component); a missing keyword or field is
          added *)
  | Connect of { node : path; key : arg_key; src : string; iter : bool }
      (** [src] is a name; [iter] marks a loop variable, which makes a literal step [n] into the
          product [src * n]. With [Whole] on a [@result] node it makes [src] the result of that
          scope. *)
  | Disconnect of { node : path; key : arg_key; fallback : Flow.Syntax.t option }
      (** [fallback] replaces the argument (see {!default_for}; the pane writes a named argument's
          schema default, so the row stays on its card); [None] removes it *)
  | Set_input_default of { form : string; input : string; value : Flow.Syntax.t }
      (** [form] is a graph name or ["def:name"] *)
  | Unfold of { node : path; key : arg_key; sub : int list }
      (** a nested call, loop or scope becomes its own binding, named by {!fresh_name} *)
  | Fold_into of { node : path }  (** a binding used once is inlined into its use *)
  | Add_arm of { node : path; after : int }
  | Delete_arm of { node : path; index : int }
  | Wrap of { nodes : path list; loop : loop }
      (** the selected bindings (one scope, at most one result leaving) become a loop body; geometry
          is merged, numbers summed, and [Fold] feeds the result back into an outside input of the
          same type *)
  | Hoist of { node : path }  (** a loop-invariant binding moves out of its loop or scope *)
  | Rename of { node : path; to_ : string }
  | Make_local_fn of { nodes : path list }
  | Make_defn of {
      nodes : path list;
      name : string;
      context : string;
      params : (string * string) list;
    }
      (** the selected bindings (one scope, one result leaving) become a new top-level
          [(defn name :context context [(p : type) ...] body)]; [params] types every name the
          selection reads from outside ([defn_draft]), and a call replaces the bindings *)
  | Make_macro of { nodes : path list; name : string; holes : (int list * string) list }
      (** [holes] are child paths into the template (the binding expression, or [let*] over several
          bindings, see [literals]) *)
  | Inline_macro of { node : path }
  | Inline_defn of { node : path }
      (** a call of a [defn] is replaced by the function's body, its parameters by the call's
          arguments *)
  | Move_binding of { node : path; by : int }
      (** the binding swaps with its neighbour [by] places away ([-1] up, [1] down): text order
          only, refused when it reads or is read by the neighbour *)
  | Move_step of { node : path; earlier : bool }
      (** a step of a chain swaps with the step before it ([earlier]) or after it: the two calls
          trade places along their {!chain_in} inputs, each keeping its other arguments *)
  | Toggle_bypass of { node : path }
  | Set_note of { node : path; text : string }
      (** a root path (a graph name) notes the form; [""] removes the note *)
  | Add_item of { node : path }
      (** a [list] literal continues a numeric step; a [str] adds a part *)
  | Move_item of { node : path; pos : int }  (** item [pos] swaps with [pos - 1] *)
  | Add_field of { node : path; name : string; value : Flow.Syntax.t }
  | Add_node of { scope : path; name : string; expr : Flow.Syntax.t }
      (** [scope] is a scope path, [["g"]] for the graph body *)
  | Delete_nodes of { nodes : path list }
  | Set_graph of { name : string; form : Flow.Syntax.t }
      (** the whole [(graph name ...)] form, replacing the graph or appended; the scene and World
          graphs written from the editor's own objects *)
  | Duplicate of { nodes : path list }
      (** the bindings of one scope, copied with fresh names (the copies read each other where the
          originals did, and notes stay with the originals) *)
  | Remove_graph of { name : string }
      (** the whole graph goes (the World graph of a deleted World); refused, naming the readers,
          while a [(ref ...)] still reads it *)
  | Rename_graph of { name : string; to_ : string }
      (** the graph and every [(ref name ...)] that reads it, in one rewrite; a taken name is
          refused *)
  | Group_merge of { nodes : path list; name : string }
      (** scene objects of one scope leave their [scene/merge] for a new merge bound to [name],
          which takes the place of the first of them *)

val positional : Flow.Syntax.t list -> Flow.Syntax.t list
(** The positional arguments of a call, in written order: [:keyword value] pairs may stand anywhere
    among them, so [(sop/transform :translate [1 2 3] a)] has [a] as input 0. *)

val keywords : Flow.Syntax.t list -> (string * Flow.Syntax.t) list
(** The [:keyword value] pairs of a call's arguments, in written order. *)

val node_call : Flow.Syntax.t -> bool
(** A call of a node kind (a head with a [/]): written in an argument, it is a nested node. *)

val value_call : Flow.Syntax.t -> bool
(** A plain operator call ([(+ a b)]): a card of its own only while written in its reader, see
    {!Connect}. *)

val context_sym : Flow.Syntax.t -> bool
(** [t], the frame time: written in an input it is a value card (like {!value_call}) wired to the
    row; disconnecting it writes the row's fallback literal. *)

val value_base : string -> string
(** The name a binding of this operator starts with: [+] is [sum], a comparison [test]. *)

val nested : string -> bool
(** The leaf names a nested node. *)

val nested_leaf : string -> arg_key -> string
(** The leaf of the node written in input [key] ([Pos] or [Kw]) of the node at this leaf. *)

val nested_nodes : Flow.Syntax.t -> (arg_key * Flow.Syntax.t) list
(** The node calls written in the inputs of a call, with the input each is written in. *)

val leaf_keys : string -> (string * arg_key list) option
(** A leaf as the binding (or [@result]) and the inputs down to the nested node; no inputs for a
    bound node. *)

val label : op -> string
(** The history label: "Repeat", "Iterate", "Unfold", "Make macro", ... *)

type macro_draft = {
  literals : (int list * Flow.Syntax.t) list;
      (** the candidate holes of the template ([literals]) *)
  free : string list;  (** outside names the template reads: they always become holes *)
  name : string;  (** a suggested macro name *)
}

val macro_draft : Flow.Syntax.t list -> path list -> (macro_draft, Diagnostic.t) result
(** What [Make_macro] over these nodes would template, for the dialog that picks its holes. The
    errors are [Make_macro]'s (one scope, one result leaving). *)

type defn_draft = {
  free : string list;  (** the names the selection reads from outside *)
  name : string;  (** a suggested function name *)
}

val defn_draft : Flow.Syntax.t list -> path list -> (defn_draft, Diagnostic.t) result
(** What [Make_defn] over these nodes needs typed: the outside names. The errors are [Make_defn]'s.
*)

val macro_op : macro_draft -> nodes:path list -> name:string -> (bool * string) array -> op
(** The [Make_macro] a dialog's answer means: entry [i] says whether literal [i] is a hole and how
    it is named. Unchecked entries are copied into the template. *)

val gesture : op -> string option
(** A history merge key for a gesture that repeats while dragged (scrubbing an argument), [None] for
    everything else. *)

val apply_checked :
  ?ops:Flow.Op.t list ->
  ?library:bool ->
  ?checker:
    (Flow.Catalog.catalog -> Flow.Syntax.t list -> Flow.Workspace.t option * Diagnostic.t list) ->
  Flow.Catalog.catalog ->
  Flow.Syntax.t list ->
  op ->
  (Flow.Syntax.t list * Flow.Workspace.t, Diagnostic.t) result
(** The rewritten source (printed and parsed again) and its check. An op that has alternatives
    ([Wrap]) tries them in order and reports the first diagnostic when none checks. [checker]
    supplies a host's library-aware checker for both branch-default discovery and rewritten
    candidates. *)

val apply :
  Flow.Catalog.catalog -> Flow.Syntax.t list -> op -> (Flow.Syntax.t list, Diagnostic.t) result

val remap : op -> path -> path option
(** Where a layout key goes after the op: [Rename] rewrites the prefix, [Hoist] drops the owner
    segment, [Delete_nodes] drops the deleted nodes' keys ([None]), everything else keeps it.
    ponytail: bindings that Wrap, Make_local_fn or Make_macro move under a new scope lose their
    layout key. *)

val fresh_name : Flow.Syntax.t list -> root:string -> string -> string
(** A name unused in the graph or definition [root] and not taken by an operator or special form; a
    slash prefix and other characters are dropped ([sop/transform] gives [transform], then
    [transform_2]). *)

val fresh_among : string list ref -> string -> string
(** {!fresh_name} against the names in [used], which the new name joins: several bindings named in
    one batch (a graph written whole, before any source has it) stay distinct. *)

val rename_ref : string -> string -> Flow.Syntax.t -> Flow.Syntax.t
(** [rename_ref old new form]: every symbol [old] (or [old.field]) in [form] reads [new]. *)

val default_for : ?fresh:(string -> string) -> Flow.Ty.t -> string -> Flow.Syntax.t option
(** The value a disconnected input of this type falls back to (the study's [defaultFor]); [None] for
    types with none. The label ["color"] makes text a colour. [fresh] chooses function parameter
    names against the destination scope (use {!fresh_name}). *)

val literals : Flow.Syntax.t -> (int list * Flow.Syntax.t) list
(** Number and text leaves of an expression with their child paths, the candidates for macro holes.
*)

val duplicated : Flow.Syntax.t list -> path list -> path list
(** The paths the copies of [Duplicate { nodes }] get, in order; [[]] when it cannot apply. *)

val arg_text : Flow.Syntax.t list -> path -> arg_key -> Flow.Syntax.t option
(** The argument [key] of the binding [node], as written (what an {!Unfold} put there). *)

val arg_of : Flow.Syntax.t -> arg_key -> Flow.Syntax.t option
(** The argument [key] of an expression, as written. *)

val skip_value : int list list -> Flow.Syntax.t
(** A [:skip] value (register L16) for these tuples: bare integers when each has one. *)

val free_names : Flow.Syntax.t -> string list
(** Names a form reads and does not declare, first use first (field access [a.b] counts as [a]).
    Projection wiring uses it. *)

val pat_names : Flow.Syntax.t -> string list
(** The names a binding pattern declares. *)

val pat_key : Flow.Syntax.t -> string
(** A pattern's text, the last segment of its path ([a], [[a b]], [{:keys [a b]}]). *)
