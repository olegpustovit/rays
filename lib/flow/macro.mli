(** Hygienic quasiquote macros of the workspace language (port of the study's [checkMacro],
    [expandOnce], [expand] and [macroParams]).

    [(defmacro name [a b & rest] `template)]: [~a] fills a hole, [~@rest] splices the rest
    parameter, [x#] is a fresh name [x__N]. Every other name in a template must be global, so a
    caller's names arrive only through holes.

    Errors: [E_MACRO_CAPTURE] (a template names something from the call site), [E_MACRO_UNQUOTE] (an
    unquote of anything but a parameter, or a misused [~]/[~@]), [E_MACRO_DEPTH], [E_MACRO_SIZE],
    plus [E_MACRO_SHAPE], [E_MACRO_PARAM], [E_MACRO_ARITY] and [E_MACRO_TEMPLATE]. *)

val max_depth : int
(** Nested expansions allowed: 32. *)

val max_forms : int
(** Forms one expansion may produce: 5,000. *)

val valid_name : string -> bool
(** Workspace names: [[a-z][a-z0-9_-]*]. *)

val params : Syntax.t -> (string list * string option, Diagnostic.t) result
(** Required parameters and the optional [& rest] parameter of a [defmacro] form. *)

val check : known:(head:bool -> string -> bool) -> Syntax.t -> Diagnostic.t list
(** Validate one [defmacro] form. [known] says which names a template may use freely (operators,
    kinds, special forms, types and workspace names), as the [head] of a call or as an argument: a
    name a binding could take is not free as an argument. *)

type state
(** The fresh-name counter and size budget of one expansion. *)

val state : unit -> state

val expand_once : ?state:state -> Syntax.t list -> Syntax.t -> (Syntax.t, Diagnostic.t) result
(** [expand_once macros form] expands the leftmost-outermost macro call one step; [Ok form]
    (physically the same) when none is left. Pass one [state] through all steps of a stepper to get
    the names [expand] gives. [macros] are [defmacro] forms; the result's new forms take the span of
    the call. *)

val expand : Syntax.t list -> Syntax.t -> (Syntax.t, Diagnostic.t) result
(** Every macro call in the form, recursively. Deterministic: the same call always expands to the
    same text. *)
