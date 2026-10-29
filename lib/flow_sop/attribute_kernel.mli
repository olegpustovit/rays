val sources : Flow.Eval.value -> int list
(** Cook-time connection between immutable geometry and packed Flow data. *)

val reads : Flow.Eval.value -> string list
(** Attribute dependencies of retained data, including topology-only sources; opaque geometry uses
    conservatively read every attribute. *)

val owner : string -> (Rdk.Attribute.owner, Diagnostic.t) result
val value_type : string -> (Flow.Ty.t, Diagnostic.t) result

val seed_for : ?salt:int -> seed:int64 -> namespace:string -> label:string -> unit -> int
(** {!Operators.seed_for}. *)

val context_label : string
(** {!Operators.context_label}. *)

val dependencies : Flow.Eval.value -> Sop.Context.Dependencies.t
(** What a kernel over this data reads of its cook context: the input slices {!input_reads} names, the
    seed, frame and time an inlined body's context sources read. *)

val input_reads : Flow.Eval.value -> Sop.Context.Dependencies.t
(** The host-input slices a frame-dependent value reads ([Time] for [t], [Pointer], [Keys], [Size],
    [Step], and [Input] for the rest), not the whole frame. *)

val context_reads : Flow.Eval.value -> Sop.Context.Dependencies.t
(** The seed, frame and time alone. *)

val specialize : Sop.Context.t -> Flow.Eval.value -> Flow.Eval.value
(** The data with its context sources replaced by the context's values, through captures. *)

val resolve :
  ?context:Sop.Context.t ->
  geometry:(int -> Rdk.Geometry.t option) ->
  Flow.Eval.value ->
  (Flow.Eval.value, Diagnostic.t) result
(** Resolve immutable cooked attributes and owner/type predicates, shared by kernels and reference
    probes. CSR reads expose typed offsets and values. *)

val source_origins : sources:int list -> Sop.Node.t list -> ((int * int) list, Diagnostic.t) result
(** The ordered source-to-topology-origin proof used by [prepare]. *)

val materialized_source : Sop.Node.t -> Sop.Node.t
(** A normal geometry consumer: cooking it through Session expands upstream packed instances at the
    existing input boundary. Retain it with its source. *)

val write_source :
  geometry:(int -> Rdk.Geometry.t option) ->
  owner:Rdk.Attribute.owner ->
  kind:string ->
  string ->
  Flow.Eval.value ->
  Rdk.Geometry.t ->
  (Rdk.Geometry.t option, Diagnostic.t) result
(** Share an unchanged typed attribute query's immutable storage after checking
    owner/type/cardinality. [None] retains the ordinary computed-array path. *)

val write :
  grain:int ->
  owner:Rdk.Attribute.owner ->
  kind:string ->
  string ->
  Flow.Eval.value ->
  Rdk.Geometry.t ->
  (Rdk.Geometry.t, Diagnostic.t) result
(** Write concrete typed arrays into fresh owned storage, checking owner count, storage layout and
    finite numeric values. *)

val with_planes :
  grain:int ->
  Rdk.Geometry.t ->
  (string * Flow.Eval.value) list ->
  (Rdk.Geometry.t, Diagnostic.t) result
(** Write [:attribute], [:owner] and the [:planes] record (width and x/y/z/w planes, as
    [sop/attr_planes] reads them) with the matching float, vec2, vec3 or vec4 storage. *)

val write_selected :
  grain:int ->
  owner:Rdk.Attribute.owner ->
  kind:string ->
  string ->
  values:Flow.Eval.value ->
  selection:Flow.Eval.value ->
  default:Flow.Eval.value ->
  Rdk.Geometry.t ->
  (Rdk.Geometry.t, Diagnostic.t) result
(** Write finite numeric replacements at a zero/one integer selection. Unselected rows retain
    matching immutable native storage, including nonfinite bytes; missing or differently typed
    storage uses the explicit finite default row. Input and selection cardinality, types and finite
    computed values are checked. *)

val write_constant_source :
  grain:int ->
  owner:Rdk.Attribute.owner ->
  kind:string ->
  string ->
  ?selection:Flow.Eval.value ->
  ?default:Flow.Eval.value ->
  Flow.Eval.value ->
  Rdk.Geometry.t ->
  (Rdk.Geometry.t option, Diagnostic.t) result
(** Consume validated constant-array metadata without dense intermediate storage. Other values
    return [None] and retain the ordinary writer path. *)

val prepare :
  ?profile:Flow_ir.Profile.t ->
  ?strict:bool ->
  ?approx:Flow.Workspace.Paths.t ->
  ?sink:Flow_ir.sink ->
  sources:int list ->
  Sop.Node.t list ->
  Flow.Eval.value ->
  (Flow_ir.Executor.program, Diagnostic.t) result
(** Use instantiated, parameter-sensitive topology and elementwise facts to prove equal owner-array
    counts before dynamic multi-input fusion. CSR row offsets and flattened values use separate
    count origins. *)

val delete_after :
  (string * string) list -> Rdk.Cancel.t -> Rdk.Geometry.t -> (Rdk.Geometry.t, Diagnostic.t) result
(** Delete each (owner, pattern) in order, as [sop/delete_attr] does. *)

val node :
  ?state:Flow.Eval.state ->
  ?reference:bool ->
  ?strict:bool ->
  ?elems:(string * Flow.Eval.value) list ->
  ?profile:Flow_ir.Profile.t ->
  ?approx:Flow.Workspace.Paths.t ->
  ?gpu:Flow_ir.Gpu.config ->
  ?selection:Flow.Eval.value ->
  ?default:Flow.Eval.value ->
  ?guard:Flow.Eval.value ->
  ?prior:(string * string * Flow.Eval.value) list ->
  ?owner:Rdk.Attribute.owner ->
  ?kind:string ->
  ?deletes:(string * string) list ->
  source:string ->
  name:string ->
  values:Flow.Eval.value ->
  sources:int list ->
  Sop.Node.t list ->
  Sop.Node.t

val resident_vec3 : Rdk.Geometry.t -> string -> float array option
(** The placeholder ({!Flow_ir.Gpu.planes}[.resident]) of point P or a point vec3 attribute [name]
    that a device still holds unread, or [None]: asked without materialising it. *)
