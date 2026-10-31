type prepared
(** Native Scene lowering of Flow's typed deferred drawing plan. Uses the workspace evaluator and
    the environment's fold state. *)

val prepare :
  ?gpu:Flow_ir.Gpu.config ->
  ?profile:Flow_ir.Profile.t ->
  ?approx:Flow.Workspace.Paths.t ->
  ?states:Flow.Eval.value list ->
  Flow.Eval.plan ->
  Flow.Eval.value ->
  (prepared, Diagnostic.t) result
(** Prepare each drawing argument's IR program once for playback or export. *)

val borrows_gpu : prepared -> bool
(** The last rendered scene binds GPU outputs (a mark set's [Device] attributes) that the next
    render replaces: after a failed render it is stale and must not be shown again. *)

val render_prepared :
  ?state:Flow.Eval.state ->
  ?image:(Flow.Eval.value -> (Rays.Image.t, Diagnostic.t) result) ->
  ?gpu:Marks_scene.gpu ->
  ?resident:
    Flow_ir.Core_exec.resident
    * (Flow_ir.Packed.t -> live:Input.t -> Flow_ir.Gpu.value option option) ->
  prepared ->
  live:Input.t ->
  size:int * int ->
  (Rays.Scene.t, Diagnostic.t) result
(** Runs the core draw path ({!Flow_ir.Core_exec}) into a mark table and adapts it with
    {!Marks_scene}; a lowering error is the diagnostic. With a configured GPU, a region that feeds
    only the positions, sizes, radius, from or to of a mark set stays on the GPU when its backend
    places it there, and that mark set is drawn by the [Marks] family with the region's buffer bound
    directly. Without a configured GPU, or off the backend, nothing differs from the CPU path.
    [resident] is a resident fold and how a mark attribute over the arrays it kept after its loop is
    drawn ({!Workspace_gpu.resident}): one host does both. *)
