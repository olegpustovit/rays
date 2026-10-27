(** Serializable structured frame programs. Locals and constants are numbered in
    authored traversal order. Values are immutable after publication. *)
type literal =
  | Int of int | Float of float | Bool of bool | Text of string
  | Vector of float array | List of literal array
  | Record of (string * literal) list
  | Array of { width : int; data : float array }
  | Int_array of int array
  | Text_array of string array
  | Empty_geometry

type integer = Flow.Packed_ops.integer = Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ishl | Ishr
type byte_mode = Clamped | Unclamped
type primitive =
  | Numeric of Flow.Packed_ops.binary | Math of Flow.Packed_ops.unary
  | Remainder | Exp | Floor | Ceil | Round | Int_convert | Float_convert
  | Integer of integer | Integer_compare of Flow.Packed_ops.comparison | Equal | Show | Text_concat | Text_blank
  | Text_trim | Text_lower | Text_repr | Text_table_decode | Text_table_encode
  | Text_split | Text_tokens | Text_to_float | Text_to_int
  | Sequence_length | Sequence_get | Sequence_build | Sequence_slice | Sequence_concat
  | Array_max_length | Array_range | Array_int | Array_text | Array_float | Array_vec3 | Array_vec4 | Array_nth | Array_count | Array_slice | Array_concat
  | Array_reduce of Flow.Op.Private.reduction | Array_segment of Flow.Op.Private.reduction
  | Array_segment_argmax | Next_after
  | Array_scatter_add | Array_compact | Array_scan_sum | Array_grade | Array_index_of
  | Rand | Rand_at | Fma | Checked_mul | Checked_fma | Noise3 | Color_parse | Color_byte of byte_mode
  | Coerce of Flow.Ty.t | Truthy | Error | Fail of string * string | Fail_diagnostic of Diagnostic.t

type frame_field = Time | Dt | Index | Tick | Width | Height | Pointer_x | Pointer_y | Input

type expr =
  | Constant of int | Local of int | Frame of frame_field
  | Call_index of int
  | Source of Flow.Eval.value
      (** Immutable host-owned source capture, materialized only when this
          expression executes. Native hosts supply the resolver; the current
          2D web exporter refuses these captures. *)
  | Host_source of { name : string; ty : Flow.Ty.t; args : (string * int) list }
      (** Construct a typed host source from computed arguments. The ordinary
          resolver materializes it only when this expression executes. *)
  | Source_descriptor of { name : string; ty : Flow.Ty.t; args : (string * int) list }
      (** Immutable host-array query retained for a cooked storage consumer.
          Numeric consumers still resolve and validate their array inputs. *)
  | Prim of primitive * (string * int) list
  | Vec of int list | Fields of (string * int) list | Items of int list
  | Field of int * string

type iteration = Static_index of int | Local_index of int
type source = {
  site : Flow.Workspace.path; owner : Flow.Workspace.path;
  instance : int; prefix : Flow.Workspace.path;
  iter : iteration list; span : Diagnostic.span option;
}

(** Drawing statements: operands are locals holding the operator's own inputs, with every
    default already resolved by the lowering ([Core_draw]); the executor checks and encodes them. *)
type mark = Rect | Circle | Line
type transform = Translate | Rotate | Scale
type path = Rounded_rect | Polygon | Polyline
type draw =
  | Background of int  (** colour *)
  | Point of { at : int; ink : int }
  | Mark of { mark : mark; args : (string * int) list }
      (** One mark; the arguments are the operator's own input names. *)
  | Batch of { mark : mark; args : (string * int) list }
      (** A mark set; the count is that of [positions] (of [from] for lines). *)
  | Points of { positions : int; ink : int }
  | Path of { path : path; args : (string * int) list }
      (** Indexed path geometry from the shared tessellation. *)
  | Text_run of { at : int; text : int; ink : int; size : int }
  | Image of { source : Flow.Ty.t * int; at : int; scale : int; angle : int }
      (** [source] is the plan node of the image, resolved by the host. *)
  | Push of transform * int  (** open a view transform on its offset, angle or factor *)
  | Pop

type statement = { id : int; source : source; node : statement_node }
and statement_node =
  | Draw of draw
  | Assign of int * expr
  | Reset_call of int
  | Protect of { body : statement list; value : int; failure : int;
      handler : statement list; mapped : int; result : int }
  | Diagnose_rows of { rows : int; policy : Flow.Workspace.diagnostic_policy; index : int;
      handler : statement list; diagnostic : int; result : int }
  | If of { condition : int; yes : statement list; no : statement list }
  | Iterate of { sequence : int; index : int; item : int; limit : int; body : statement list }
  | Counted of { first : int; last : int; count : int; index : int; item : int;
      limit : int; stop : int option; init : statement list; body : statement list }
      (** Exact integer induction after the source's endpoint conversion.
          Assign max(0,last-first) to count, check limit, run init once, then
          run body with ordinal index and Int(first+index) item. A true [stop]
          local terminates before the next pass. *)
  | Store of { sequence : int; index : int; value : int }
  | State_read of { slot : int; result : int; init : statement list; initial : int }
  | State_peek of { slot : int; found : int; value : int }
      (** [found] := whether this frame already holds a pending write; [value] := it. *)
  | State_write of { slot : int; value : int }
  | Invoke of { region : int; args : int list; result : int }

type state_slot = {
  source : source; prefix : Flow.Workspace.path; zone : Flow.Workspace.path;
  plane : [ `Frame | `Host ]; ty : Flow.Ty.t;
}
(* A region: a pure numeric loop body over packed arrays, as a register program
    (instruction [n] defines register [n] from earlier registers). *)
module Region : sig
  type projection = Value | Valid
  type conversion = Flow.Packed_ops.conversion = To_float | Truncate | Floor | Ceil | Round | Coerce_int | Integer_result of string
  type loop = {first:int; last:int; finish:int; initial:int array; limit:int option;
    span:Diagnostic.span}
  type instruction =
    | Const of float | Input of int * int * int  (** source, width, component *)
    | Uniform of int * int | Frame of string | Accumulator of int
    | Binary of Flow.Packed_ops.binary * int * int | Unary of Flow.Packed_ops.unary * int
    | Noise3 of int * int * int * int * int * float * float | Fma of int * int * int | Rand_at of int * int | Select of int * int * int
    | Checked_mul of projection * int * int | Checked_fma of projection * int * int * int
    | Trace of int * int array
      (** One field (0 t, 1 prim, 2 u, 3 v, 4-6 normal, 7 hit) of a host ray query: registers
          scene id, origin, direction, tmin, tmax, time. *)
    | Array_prepare of int * Flow.Ty.t
      (** Execution-local immutable table capture, resolved before its index.
          Its register is a private table handle and cannot be a scalar output. *)
    | Gather of int * int * int * int
      (** Prepared table register, exact integer index, width and component. *)
    | Int_gather of int * int
    | Text_const of string | Text_input of int | Text_uniform of int
    | Text_gather of int * int | Text_select of int * int * int
    | Int_const of int | Int_input of int | Int_uniform of int | Int_frame of string | Int_accumulator
    | Int_binary of Flow.Packed_ops.integer * int * int
    | Int_compare of Flow.Packed_ops.comparison * int * int | Int_abs of int
    | Convert of conversion * int | Int_select of int * int * int
    | Guarded of (int * bool) list * instruction
    | Raise of Flow.Ty.t * string * string * string option
    | Loop_begin of loop
    | Loop_carry of int * int * Flow.Ty.t
    | Loop_end of int * int array * int option
    | Loop_result of int * int * Flow.Ty.t
      (** Structured carried loops: [Loop_begin] defines the Int index,
          contiguous [Loop_carry] registers define its scalar state, and
          [Loop_end] supplies simultaneous next values and an optional Bool
          register tested before the update; a true value stops after committing
          those next values. Only [Loop_result]
          exposes the final state outside the body. *)
  val integer_register : instruction -> bool
  val text_register : instruction -> bool
  (* Text leaf registers are separate from numeric and array-handle registers. *)

  val operands : instruction -> int list
  type iteration = Zip | Product
  type result = Collect | Sum of bool | Accumulate of Flow.Workspace.term * bool
  type origin = Flow.Workspace.path * int list
  type count_source = Flow.Eval.residual -> Flow.Workspace.term -> origin option
  type t = private {
    residual : Flow.Eval.residual; term : Flow.Workspace.term;
    sources : (Flow.Eval.residual * Flow.Workspace.term) array; widths : int array;
    uniforms : Flow.Eval.value array; uniform_names : (string * string list) array;
    code : instruction array; dependent : bool array option Atomic.t; outputs : int array array;
    count : int option; origin : origin option;
    iteration : iteration; result : result; skip : int array;
    stages : (Flow.Eval.residual * Flow.Workspace.term) list;
    fusion : bool; dynamic : bool; count_source : count_source;
    body : Flow.Workspace.term; binding_fn : string option;
    sites : (int * Flow.Workspace.path * int list) list }
  val make : residual:Flow.Eval.residual -> term:Flow.Workspace.term ->
    sources:(Flow.Eval.residual * Flow.Workspace.term) array -> widths:int array ->
    uniforms:Flow.Eval.value array -> uniform_names:(string * string list) array ->
    code:instruction array -> outputs:int array array -> count:int option -> origin:origin option ->
    iteration:iteration -> result:result -> skip:int array ->
    stages:(Flow.Eval.residual * Flow.Workspace.term) list -> fusion:bool -> dynamic:bool ->
    count_source:count_source -> body:Flow.Workspace.term -> binding_fn:string option ->
    sites:(int * Flow.Workspace.path * int list) list -> (t, Diagnostic.t list) Stdlib.result
  val dependent_flags : t -> bool array
  val rebuild : t -> ?residual:Flow.Eval.residual -> ?term:Flow.Workspace.term ->
    ?sources:(Flow.Eval.residual * Flow.Workspace.term) array -> ?widths:int array ->
    ?uniforms:Flow.Eval.value array -> ?uniform_names:(string * string list) array ->
    ?code:instruction array -> ?outputs:int array array -> ?count:int option ->
    ?origin:origin option -> ?iteration:iteration -> ?result:result -> ?skip:int array ->
    ?stages:(Flow.Eval.residual * Flow.Workspace.term) list -> ?fusion:bool -> ?dynamic:bool ->
    ?count_source:count_source -> ?body:Flow.Workspace.term -> ?binding_fn:string option ->
    ?sites:(int * Flow.Workspace.path * int list) list -> unit -> (t, Diagnostic.t list) Stdlib.result
  module Private : sig
    val with_captures : t -> residual:Flow.Eval.residual -> uniforms:Flow.Eval.value array ->
      sources:(Flow.Eval.residual * Flow.Workspace.term) array ->
      stages:(Flow.Eval.residual * Flow.Workspace.term) list -> count:int option ->
      (t, Diagnostic.t) Stdlib.result
  end
end
type region = Region.t

type program = {
  constants : literal array; locals : Flow.Ty.t array;
  body : statement list; results : int array;
  state_slots : state_slot array; regions : region array;
  call_slots : int;
}

val reads : statement -> int list
(** Local operands read directly by one statement, in operand order. *)
val writes : statement -> int list
(** Local slots written directly by one statement. Nested statements are not included. *)
val children : statement -> statement list
(** Immediate nested statements in execution/source order: body before handler,
    yes before no, and loop initialization before its body. *)
val iter : (statement -> unit) -> statement list -> unit
(** Visit statements in preorder, recursively following [children]. *)

val primitive_name : primitive -> string
val literal_of_value : Flow.Eval.value -> literal option
val value_of_literal : literal -> Flow.Eval.value
val pp : Format.formatter -> program -> unit
(** Deterministic diagnostic printer, including every statement's source identity. *)
