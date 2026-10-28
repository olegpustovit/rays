(** The workspace language's static types (port of the study's type strings).
    {!Port_type} is the smaller set a driven parameter port carries at run time. *)

type t =
  | Named of string | Float | Int | Bool | Vec2 | Vec3 | Vec4 | Text
  | Color  (** text or vec3; only catalog parameters ask for it *)
  | List of t
  | Array of t  (** int, float and vector storage, or interpreter-only text data *)
  | Record of (string * t) list  (** fields in written order *)
  | Fn of fn_signature option
  | Any  (** unknown or unconstrained: unannotated fn parameters, empty lists *)
and fn_signature = { params : t list; result : t }
(** [Fn None] is an uninstantiated callable. A function port certifies the
    input types and checked result in [Fn (Some signature)]. *)

type color = [ `Geometry | `Float | `Int | `Bool | `Vec3 | `Text | `Fn | `Record | `Output | `Compound ]
val geometry : t
val image : t
val drawing : t
val scene : t
val world : t
val settings : t
val panel : t
val editor : t
val material : t
val is_geometry : t -> bool
val is_cooked : t -> bool

val shape : t -> bool
val color : t -> color
val default : t -> Syntax.t option

val of_syntax : Syntax.t -> t option
(** A type annotation as written in a parameter: a type name, [fn],
    [(list T)] or [{:field T ...}] (unique lowercase field names).  [Any] and
    [Color] are not writable. *)

val names : unit -> (string * t) list
val to_string : t -> string
(** Diagnostic spelling: [float], [list:int], [rec{a:int,b:float}],
    [fn(vec3)->float] for an instantiated function port. *)

val of_string : string -> t option
(** Inverse of [to_string]. *)

val has_fn : t -> bool
(** A function type anywhere inside; such values cannot be stored (E_FN_ESCAPES). *)

val fits : t -> t -> bool
(** [fits have want]: [have] may be used where [want] is expected, possibly
    through {!coerce}. Numbers and bool interconvert; numbers widen to vectors
    without changing a vector's width;
    records fit when they have every wanted field. *)

val coerce : t -> t -> t
(** [coerce have want] is the type of a [have] value after conversion to [want]. *)

val join : t -> t -> t option
(** The least type both fit into ([int]+[float] is [float]); [Any] is neutral. *)

val unify : t -> t -> t option
(** [join], else [have] when the two fit each other. *)

val elem : t -> t option
(** Element type of a list. *)
val numeric_record : t -> bool
(** A nonempty record whose leaves are numbers, booleans or vectors (nested records allowed). *)
