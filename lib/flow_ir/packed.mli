(** Specialized numeric packed maps, loops and reductions with scalar registers
    in 1,024-element blocks. Accumulators retain reference evaluation order. *)
type t = Core.Region.t
type binary = Flow.Packed_ops.binary = Add | Sub | Mul | Div | Mod | Pow | Min | Max | Lt | Le | Gt | Ge | Eq | And | Or | Atan2 | Hypot
type unary = Flow.Packed_ops.unary = Sin | Cos | Sqrt | Abs | Not | Tan | Asin | Acos | Exp | Log | Sign
type conversion = Core.Region.conversion = To_float | Truncate | Floor | Ceil | Round | Coerce_int | Integer_result of string
type projection = Core.Region.projection = Value | Valid
type loop = Core.Region.loop = {first:int; last:int; finish:int; initial:int array; limit:int option;
  span:Diagnostic.span}
type instruction = Core.Region.instruction = Const of float | Input of int * int * int
  | Uniform of int * int | Frame of string | Accumulator of int
  | Binary of binary * int * int | Unary of unary * int
  | Noise3 of int * int * int * int * int * float * float | Fma of int * int * int | Rand_at of int * int
  | Select of int * int * int
  | Checked_mul of projection * int * int | Checked_fma of projection * int * int * int
  | Trace of int * int array
  | Array_prepare of int * Flow.Ty.t
  | Gather of int * int * int * int | Int_gather of int * int
  | Text_const of string | Text_input of int | Text_uniform of int
  | Text_gather of int * int | Text_select of int * int * int
  | Int_const of int | Int_input of int | Int_uniform of int | Int_frame of string | Int_accumulator
  | Int_binary of Flow.Packed_ops.integer * int * int
  | Int_compare of Flow.Packed_ops.comparison * int * int | Int_abs of int
  | Convert of conversion * int | Int_select of int * int * int
  | Guarded of (int * bool) list * instruction | Raise of Flow.Ty.t * string * string * string option
  | Loop_begin of loop | Loop_carry of int * int * Flow.Ty.t
  | Loop_end of int * int array * int option | Loop_result of int * int * Flow.Ty.t
type origin = Flow.Workspace.path * int list
type count_source = Flow.Eval.residual -> Flow.Workspace.term -> origin option
(** Hosts may prove equal source cardinality from instantiated domain facts.
    [None] retains a materialization boundary for dynamic multi-input maps. *)

val compile_result : ?fusion:bool -> ?count_source:count_source ->
  Flow.Eval.residual -> Flow.Workspace.term -> (t, Diagnostic.t list) Stdlib.result
(** The first actual compilation refusal, with its source location. Declined
    fusion retains a valid unfused program and is not a refusal. *)

val compile : ?fusion:bool -> ?count_source:count_source ->
  Flow.Eval.residual -> Flow.Workspace.term -> t option
(** Unsupported bodies remain on the reference interpreter. Fusion defaults to
    true; false retains intermediate arrays for parity checks and profiling. *)

val compile_template : ?count_source:count_source -> Flow.Eval.residual -> Flow.Workspace.term -> t option
val rebind : t -> Flow.Eval.residual -> t option
(** A frame-fold template keeps captures as uniforms and reuses its register
    code with the next step's immutable bindings. A changed function body or a
    fused lexical scope requires a fresh specialization. *)

val static_count : t -> int option
val count_origin : t -> origin option
val elementwise : t -> bool
(** False for sums and accumulator steps; they must not fuse as independent elements. *)

val gpu_refusals : ?fused:bool -> ?fast:bool -> t -> Diagnostic.t list
(** Pure form restrictions shared by placement, display execution and emission.
    [fused] admits fused and checked multiply-add, which the native device runs in 32 bits within
    the owner's device tolerance; the default keeps the binary64 forms (the web export).
    Constants/noise are checked only when reachable from outputs; ordered
    accumulators are refused even when unreachable. *)

val stage_count : t -> int
(** Number of packed stages executed in this register program without intermediate arrays. *)

val provenance : t -> (int * Flow.Workspace.path * int list) list
(** Graph instance, authored site and iteration tuple for every fused stage. *)

val site : t -> int * Flow.Workspace.path * int list
(** The consuming stage, where the group's single timing belongs. *)

val force : ?dispatch:(Flow.Eval.residual -> Flow.Workspace.term -> (Flow.Eval.value, Diagnostic.t) result) ->
  ?state:Flow.Eval.state -> ?elems:(string * Flow.Eval.value) list ->
  ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
  ?measure:((unit -> float) * (t -> seconds:float -> reference:bool -> unit)) ->
  ?on_walk:(unit -> unit) ->
  ?strict:bool ->
  t -> live:Flow.Eval.live -> (Flow.Eval.value, Diagnostic.t) result
val force_all : ?dispatch:(Flow.Eval.residual -> Flow.Workspace.term -> (Flow.Eval.value, Diagnostic.t) result) ->
  ?state:Flow.Eval.state -> ?elems:(string * Flow.Eval.value) list ->
  ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
  ?measure:((unit -> float) * (t -> seconds:float -> reference:bool -> unit)) ->
  ?on_walk:(unit -> unit) ->
  ?strict:bool ->
  t -> live:Flow.Eval.live -> (Flow.Eval.value list, Diagnostic.t) result
(** One packed array per output, in output order. A program with several outputs
    only collects and has no reference fallback; [force] is the single-output
    accessor and refuses such a program. [strict:true] returns kernel errors
    directly instead of rerunning the reference interpreter. *)

val reference : ?state:Flow.Eval.state -> ?elems:(string * Flow.Eval.value) list ->
  ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
  t -> live:Flow.Eval.live -> (Flow.Eval.value, Diagnostic.t) result

module Private : sig
  val argument_types : t -> Flow.Ty.t array
  (** Declared types of the structured Core arguments captured by this region. *)

  val bind_arguments : ?validate:bool -> t -> Flow.Eval.value array -> (t,Diagnostic.t) result
  (** Borrow checked immutable Core arguments in place of typed source markers.
      Refuses wrong arity/storage; rewrites closure and residual captures without
      re-evaluating their checked bodies. [~validate:false] skips the finite-value scan, for a
      device that checks what it uploads (a resident fold's placeholders are zeros). *)

  val trace : ((t, Diagnostic.t list) Stdlib.result -> unit) ref
  (** Called with every public compilation result; tests record snapshots through it. *)

  val ordered_add : t -> bool
  (** Recognizes the exact component-wise non-collecting ordered-add program. *)

  val output_reachable : ?whole_loops:bool -> t -> bool array
  (** Fresh instruction mask used by GPU qualification and code generation. [whole_loops] (the
      default) keeps every instruction of a reached loop body, for per-operation finite checks;
      [false] keeps only what reaches the outputs, a loop's next values or its stop. *)

  val finite_checks : t -> bool array
  (** After [gpu_refusals] succeeds, instructions whose result needs a finite
      check. Other instructions cannot introduce nonfinite values; checks on
      their operand producers retain intermediate overflow detection. *)

  type view = {code : instruction array; widths : int array; outputs : int array array;
    uniform_widths : int array; collecting : bool; }
  val view : t -> view
  (* Borrowed immutable arrays for downstream compilation. *)
  type inputs = {arrays : float array array; uniforms : float array array;
    tables : float array array; frame : float array; count : int}
  (* [tables.(u)] is array-valued uniform [u] flattened (empty for a scalar uniform), the table a
     gather reads; integer arrays and scalars are floats. *)
  type value_inputs = {arrays : Flow.Eval.value array; uniforms : Flow.Eval.value array;
    frame : Flow.Eval.value array; count : int}
  (* Materialize CPU inputs while preserving exact integer storage. [lengths i] gives source [i]'s
     length instead of building it (an Int or Float range the caller binds on the device); its
     array is then empty. [source] lets the caller supply an immutable cached
     source; its storage type is checked as usual. *)
  val prepare_values : ?state:Flow.Eval.state ->
    ?resolve:(Flow.Eval.value -> (Flow.Eval.value,Diagnostic.t) result) ->
    ?measure:((unit -> float) * (t -> seconds:float -> reference:bool -> unit)) ->
    ?lengths:(int -> int option) ->
    ?source:(Flow.Eval.residual -> Flow.Workspace.term -> Flow.Eval.value option) ->
    t -> live:Flow.Eval.live -> (value_inputs,Diagnostic.t) result
  type shape = Value of Flow.Eval.value | Range of Flow.Eval.value * int * int
    | Opaque of Flow.Eval.value list option
  (* What a source or uniform reads: a value itself; [array/range] of an array's count times [mul]
     over [div]; or a term over the listed values ([None]: a form not traversed). A resident fold
     keeps an array's count on the device when every read of it is one of the first two. *)
  val source_shapes : t -> shape array
  val uniform_shapes : t -> shape array
  val device_inputs : value_inputs -> inputs
  (* [prepare]'s conversion of materialized inputs. *)
  val prepare : ?fast:bool -> ?state:Flow.Eval.state -> ?resolve:(Flow.Eval.value -> (Flow.Eval.value, Diagnostic.t) result) ->
    ?measure:((unit -> float) * (t -> seconds:float -> reference:bool -> unit)) ->
    ?source:(Flow.Eval.residual -> Flow.Workspace.term -> Flow.Eval.value option) ->
    t -> live:Flow.Eval.live -> (inputs, Diagnostic.t) result
end

type column = {data:float array;offset:int;stride:int}

val force_into : ?state:Flow.Eval.state -> ?elems:(string * Flow.Eval.value) list ->
  ?resolve:(Flow.Eval.value -> (Flow.Eval.value,Diagnostic.t) result) ->
  ?measure:((unit -> float) * (t -> seconds:float -> reference:bool -> unit)) ->
  ?strict:bool ->
  column_source:(Flow.Eval.residual -> Flow.Workspace.term -> (column array option,Diagnostic.t) result) ->
  count:int -> outputs:float array option array array -> t -> live:Flow.Eval.live -> (unit,Diagnostic.t) result
(** Execute a Collect region into caller-owned contiguous output planes.
    Borrowed source columns retain their offset/stride and are never written.
    [None] sources use ordinary packed materialization; [None] output lanes skip
    writes while retaining arithmetic and finite checks. Every plane must have
    [count] elements, including when all output lanes are skipped. *)
