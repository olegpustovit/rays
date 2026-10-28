(** Immutable built-in operator declarations. Checker, evaluator and editor share these records. *)
type signature = {
  pos : (string * Ty.t) list; opt : (string * Ty.t) list;
  rest : (string * Ty.t) option; kw : (string * Ty.t) list;
}
type shape = Scalar | Struct of { splice : bool }
type arithmetic = { apply : 'f 'r. ('f, 'r) Value.t -> ('f, 'r) Value.t -> ('f, 'r) Value.t }
type packed_kind = Binary of Packed_ops.binary | Unary of Packed_ops.unary
  | Integer of Packed_ops.integer | Integer_compare of Packed_ops.comparison
  | Convert of Packed_ops.conversion
  | Noise3 | Fma | Checked_mul | Checked_fma | Rand_at | Trace | Stage | Length | Dot | Cross | Normalize | Exact | Gather | Frame | Constant_only
type t = {
  name : string; ctx : Context.t; signature : signature;
  out : Ty.t list -> Ty.t;
  out_args : ((string * Ty.t * Param.value option) list -> Ty.t) option;
  (** Optional result typing from named argument types and scalar literals.
      A missing literal denotes a computed argument. Clear this callback when
      copying a declaration with different result semantics. *)
  any_num : bool;
  choices : (string * string list) list; shape : shape;
  live : bool;
  check : 'f 'r. (string * ('f, 'r) Value.t) list -> unit;
  body : 'f 'r. live:Input.t -> node:(string -> (string * ('f, 'r) Value.t) list -> ('f, 'r) Value.t) ->
    (string * ('f, 'r) Value.t) list -> ('f, 'r) Value.t;
  category : string;
  arithmetic : arithmetic option;
  packed_extension : packed_kind option;
  (** Asserts the intrinsic's semantics, not just its signature. Clear this
      capability when copying a declaration and changing its behavior.
      Only [Noise3] supports explicit declarations; other kinds are inferred
      from canonical built-ins. *)
}
val all : t list
val noise3 : t
val text_float : t
val noise_table : int -> Rays_math.Noise.t
val noise_settings : (string * ('f, 'r) Value.t) list -> int * int * float * float
val drawing_color : ('f, 'r) Value.t -> Rays_math.Color.t
val hsl_channels : float -> float -> float -> float * float * float
val find : ?extra:t list -> string -> Context.t -> t option
(** Exact head, then bare value head, then a head qualified by the given context. *)

val finder : t list -> string -> Context.t -> t option
(** [finder extra] is [find ~extra] with the extras indexed once, for a caller that looks up many
    names against the same list: [find] scans the list at each of its three tries. *)

val of_context : ?extra:t list -> Context.t -> t list
(** Context-specific operators followed by the value operators, in declaration order. *)

val result_type : t -> (string * Ty.t * Param.value option) list -> Ty.t
(** Use [out_args] when supplied, otherwise [out] on positional argument types. *)

val arith : string -> arithmetic option

val packed_kind : t -> packed_kind option
(** Canonical scalar built-ins or a validated explicit intrinsic capability.
    [Constant_only] permits existing constant folding without promising a
    dynamic register instruction. *)

val is_display_kind : string -> bool
(** Drawing and UI consumers may display approximate values. *)

val validate : t list -> Diagnostic.t option
(** Check an immutable extension list; declarations cannot shadow built-ins. *)

val max_iterations : int

val range_length : ?limit:int -> int -> int -> int
(** The checked length of [range], shared with structured region loops.
    The default limit is 4,096; subtraction cannot overflow native Int. *)

module Private : sig
  val constant_array_descriptor : string -> (string * ('f, 'r) Value.t) list -> ('f, 'r) Value.t
  (** Validate the actual count and one used fill element at the constructor's
      authored evaluation site, retaining immutable metadata for a cooked
      storage consumer. Empty arrays do not validate the unused fill. *)

  val array_count : ?limit:int -> string -> ('f, 'r) Value.t -> int
  (** Checked array length shared by storage allocation and compiled source
      views that do not materialize their arrays. *)

  type reduction = Sum | Minimum | Maximum
  val reduce_array :
    ?rows:(start:int -> finish:int -> (int -> unit) -> unit) -> reduction ->
    ?offsets:('f, 'r) Value.t -> ('f, 'r) Value.t -> ('f, 'r) Value.t
  (** Shared numeric bulk reduction. [rows] dispatches inclusive, disjoint
      output row indices; callers may use a pooled executor. Input storage is
      borrowed immutably. Offsets are checked before allocation or dispatch. *)

  val segment_argmax :
    ?rows:(start:int -> finish:int -> (int -> unit) -> unit) ->
    offsets:('f, 'r) Value.t -> ('f, 'r) Value.t -> ('f, 'r) Value.t
  (** Index into the values of the first maximum of each segment (-1 when the
      segment is empty), for int or finite float values. *)
end
