(** Packed pixel functions; procedural images never depend on the Flow compiler. *)
type t
val prepare : ?site:(int * Flow.Workspace.path * int list) -> ?path:Flow.Workspace.path ->
  ?approx:Flow.Workspace.Paths.t -> identity:int -> width:int -> height:int -> fn:Flow.Eval.fn ->
  sources:int list -> Sop.Node.t list -> (t, Diagnostic.t) result
(** Retains one immutable pixel-center UV grid and the actual compiled body.
    Display qualification uses the authored [path]; absent provenance stays CPU-only.
    [node] always cooks an exact CPU snapshot. *)

val node : ?state:Flow.Eval.state -> t -> Sop.Node.t
(** Cook through [Sop.Session] to install the context's shared pool. *)

val program : t -> Flow_ir.Executor.program

val with_inputs : t -> Sop.Node.t list -> (t, Diagnostic.t) result
(** Rebind current compiled inputs while sharing the UV grid and program.
    A changed point-origin proof or source count returns [E_DATA_SOURCE]. *)
