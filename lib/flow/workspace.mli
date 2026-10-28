(** The workspace language checker: resolves, types and validates a [(workspace ...)] and produces
    the typed IR that lowering and evaluation read (port of the static pass of the study's
    [compile]).

    Static only: no value is evaluated. A count that is a literal (a literal [range], [linspace] or
    [list]) is bounded here ([E_ITER_BOUND], destructuring lengths); a count that comes from an
    input, [t] or geometry is bounded by [Eval] when it runs. Every body is typed once (both [if]
    branches, zero-iteration loops) and a [fn] or [defn] body is typed again at each call site with
    the argument types, unannotated [fn] parameters starting as [Any] (register F2). A definition
    called from several sites reports each diagnostic once.

    Codes: [E_SHADOW] [E_FN_ESCAPES] [E_ACC_TYPE] [E_ITER_BOUND] [E_NO_ELSE] [E_PATTERN]
    [E_INPUT_DEFAULT] [E_TIME_COUNT] [E_TIME_BRANCH] [W_UNKNOWN_GROUP], the [E_MACRO_*] codes of
    {!Macro}, and the study's other diagnostics ([E_TYPE], [E_UNBOUND], [E_ARITY],
    [E_UNKNOWN_PARAM], [E_RECURSION], [E_GRAPH_CYCLE], ...) with the study's wording. *)

type path = string list
(** Lexical identity, stable across edits: [["flower"; "ring"; "u"]] is the binding [u] in the zone
    bound to [ring] in graph [flower]. Reserved segments: [":x"] a loop variable or parameter,
    ["@result"] a body result, ["~for"] / ["~let"] / ["~fn"] an unbound inline form, ["def:name"]
    the root of a [defn]. *)

module Paths : Set.S with type elt = path

type context = Context.t

val context_name : context -> string

type pattern =
  | Name of string
  | Seq of pattern list
  | Keys of string list  (** [a], [[a b]] (also over a vec3), [{:keys [a b]}]. *)

type diagnostic_policy = First | Range_last

type term = { path : path option; ty : Ty.t; node : node; form : Syntax.t }
(** [path] is set on bindings, zones, results and graph/defn bodies. [form] is the authored form
    (with its notes); for a macro call it is the call. *)

and node =
  | Lit of Param.value  (** int, float or bool *)
  | Text of string
  | Nil  (** empty geometry *)
  | Time  (** [t] *)
  | Vec of term list  (** vec3 literal *)
  | Ref_binding of string * string list  (** [name.field.field] *)
  | Call of { kind : string; ctx : Context.t; args : (string * term) list }
      (** catalog kind, qualified, with its resolved context; slots and keyword parameters in
          written order. [sop/merge] names every input [input]. *)
  | Op of { op : string; args : (string * term) list; skip : int list list }
      (** built-in operator ([+], [range], [value/rand], [scene/object], [ui/split-at], ...) with
          its slot names; a rest slot repeats its name. [skip] lists the argument tuples a
          [scene/merge] leaves out (register L16), [[]] otherwise. *)
  | Call_fn of { fn : string; args : term list; body : term option }
      (** a [defn] (arguments in parameter order, defaults filled in) or a local [fn]. [body]
          retains this call site's checked specialization. *)
  | Fn_ref of string  (** a defn, operator or kind name used as a function value *)
  | Graph_ref of { graph : string; inputs : (string * term) list }
  | Let of (pattern * term) list * term
  | State of { binder : pattern; init : term; step : term; zone : path }
      (** [(state [s init] step)]: one fold step per logical frame. *)
  | Loop of {
      kind : [ `For | `Fold | `Scan | `Sum ];
      accs : (pattern * term) list;
      clauses : (pattern * term) list;
      skip : int list list;
      body : term;
      zone : path;
    }  (** [skip]: the iteration tuples a [for] leaves out (register L16); [[]] for other kinds *)
  | With_error of term * term
  | Error_rows of { rows : term; policy : diagnostic_policy; mapper : term }
  | If of term * term * term
  | Cond of (term * term) list * term
  | Case of term * (Syntax.t * term) list * term
  | Fn of { params : (pattern * Ty.t option) list; body : term; zone : path; capture : term option }
      (** A function port specializes [body] while [capture] retains its original callable's lexical
          environment and call identity. *)
  | Hof of [ `Map | `Filter | `Reduce | `Sort_by | `Iterate ] * term list
  | List_lit of term list
  | Record of (string * term) list
  | Get of term * string
  | Assoc of term * (string * term) list
  | Str of term list
  | List_op of string * term list  (** [concat] *)
  | Bypass of term  (** [^:bypass] on a call *)
  | Expanded of { macro : string; body : term }  (** a macro call and its expansion *)

val children : term -> term list
(** Immediate checked-term children in authored traversal order. *)

val map_children : (term -> term) -> term -> term
(** Map immediate children, preserving all other checked-term and source-form data. *)

type graph = {
  name : string;
  context : context;
  inputs : (string * Ty.t * term option) list;  (** graph inputs always have a default *)
  body : term;
  form : Syntax.t;
}

type node_definition = {
  kind : Catalog.kind;
  slots : Catalog.slot list;  (** complete physical input order *)
  keyword_inputs : string list;
  fields : Param.field_view list;
  label : string;
  category : string list;
  description : string option;
  operation : string;
  version : int;  (** Nonnegative runtime version from the declaration. *)
  body : graph;
  validations : (term * string * string) list;
  normalizers : (string * term) list;
}
(** A Lisp catalog declaration. Calls retain [Call] identity; its body is checked once using the
    declared input types. Validation predicates run against parameter values before cooking. *)

val kind_calls : term -> string list
(** Unique catalog calls reachable in a checked term, including specialized function and macro
    bodies. Used for node dependency metadata. *)

val bypass_input : term -> term option
(** Resolve the first authored positional input against checked arguments, which may be in
    declaration order. Shared by checking, evaluation and lowering. *)

type packed_form =
  | Candidate of Paths.t
  | Refused of Diagnostic.t list
      (** A candidate links an authored path to the producers that require actual capture
          qualification. Structural refusals do not prevent CPU evaluation. *)

type t = {
  name : string;
  graphs : graph list;
  defs : graph list;  (** [defn]s *)
  nodes : node_definition list;  (** [defnode]s *)
  macros : Syntax.t list;  (** the [defmacro] forms *)
  source : Syntax.t list;
  live : Paths.t;
      (** terms that depend on [t]: they mention it, or depend on a live binding, capture, fold
          accumulator, [ref] override, graph or function (register T1). A [defn] body is live when
          it is at any call site. *)
  invariant : Paths.t;
      (** bindings directly in a zone body that do not depend on the loop variables or the
          accumulator (register L7, the "same each time" mark) *)
  approx : Paths.t;
      (** Definitive eligibility, empty after check until actual-capture qualification above Flow.
          Independent precision taint still enforces statically visible exact-only sinks; execution
          precision belongs to IR. *)
  packed : (path * packed_form) list;
      (** Immutable candidate/refusal facts, including aliases and kernel body participation.
          Holding an unused callable does not qualify it. *)
  packed_roots : (Syntax.t * path) list;
      (** Physical authored form identities and their checker producer paths. Syntax IDs may restart
          across imports or be zero on generated forms; qualifier traversal supplies evaluator
          ownership without rewriting terms. *)
  approx_reasons : (path * Diagnostic.t list) list;
      (** Pending/structural refusals after check; actual reasons after qualification. *)
  kind_fns : (string * (string * Context.t * Catalog.slot list)) list;
      (** A catalog kind used as a function value: its qualified name, context and complete ordered
          slot signature, including a repeated final slot. *)
  ops : Op.t list;  (** immutable extra operators retained by this checked workspace *)
}

val max_iterations : int
(** Iterations per zone: 4,096. *)

val body_context_ops : string list
(** Operators a defnode body reads from its cook context ([sop/context_seed], [sop/provenance_base],
    [sop/context_frame], [sop/context_time]); a body elsewhere is [E_NODE_CONTEXT]. *)

val check :
  ?ops:Op.t list ->
  ?library:bool ->
  ?call_sites:bool ->
  Catalog.catalog ->
  Syntax.t list ->
  t option * Diagnostic.t list
(** Check one [(workspace name ...)] form. The workspace is returned only without errors; warnings
    never block it. [library] allows definitions and macros without a runnable graph, for editing
    imported source libraries. [call_sites] types a [defn] body only where it is called (and on its
    own when it is named as a function value), not on its own as well: for a host's embedded
    declarations, which a test checks whole. An unused [defn] then goes unchecked. *)

val skip_tuples : Syntax.t -> int list list option
(** The tuples of a [:skip] value (register L16): a list of tuples of non-negative integers, a bare
    integer being a tuple of one; [None] when it is not one. *)

val special_forms : string list
(** The special forms of the language, as the checker knows them (an editor's completions and
    indentation read this list, so they offer no form the language lacks). *)

val name_taken : string -> bool
(** A name a binding may not take: a special form, built-in operator or type name. Editors use it to
    pick fresh names. *)

type op_signature = Op.signature = {
  pos : (string * Ty.t) list;
  opt : (string * Ty.t) list;
  rest : (string * Ty.t) option;
  kw : (string * Ty.t) list;
}
(** Positional inputs, optional positional inputs, the repeating input and keyword inputs of a
    built-in operator. *)

val op_signature : ?ops:Op.t list -> context -> string -> op_signature option
(** The built-in operator a call head names in a graph of this context ([sin], [value/rand],
    [scene/object], ...); [None] for kinds and definitions. *)

val op_choices : ?ops:Op.t list -> string -> string -> string list
(** The texts an operator's argument takes ([ui/graph]'s [view], a split's [axis]); empty when it
    takes any. *)

val value_ops : string list
(** The built-in operators any graph may call ([+], [sin], [value/rand], [range], ...): the value
    part of the add menu. *)

val slot_ty : ?slot:Catalog.slot -> Catalog.kind -> Ty.t
(** The type a catalog kind's slots take (geometry; a World layer; the World of [scene/world]; the
    scene of [scene/root]). *)

val group_reader : Catalog.parameter -> bool

val group_writer : Catalog.kind -> Catalog.parameter -> bool
(** Which catalog parameters read or write a primitive group (inferred: the manifest has no
    markers). *)

val validate_parameter :
  ?ty:Ty.t -> (Diagnostic.severity -> string -> string -> unit) -> Catalog.parameter -> term -> unit
(** Validate one already typed keyword argument using the workspace's coercion, colour, choice and
    range rules. *)
