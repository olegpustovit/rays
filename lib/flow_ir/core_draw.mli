val lower :
  ?region_allowed:(Core.Region.t -> bool) ->
  Flow.Eval.plan ->
  Flow.Eval.value ->
  (Core.program * Flow.Eval.residual option, string) result
(** Lowering rules of the draw operators: a draw graph's plan and root value become a core program
    of draw statements, with no result, in painter order. The second component is a residual to run
    it with (state reads need one), absent when nothing is live. A drawing chosen by a live value is
    refused as ["draw_dynamic"], an image that is not a plan node as ["draw_image"]. *)

val device_regions : Core.program -> width:(int -> int option) -> bool array * int list list
(** Regions read exclusively as compatible mark-set attributes, and their per-batch groups in
    argument order. [width] supplies each target's eligible single-output region width. Other
    attributes can remain on the CPU. *)

type group = { members : int list; results : int list; args : int list; region : Core.Region.t }

val static_invokes : Core.program -> bool array

val device_groups :
  ?approx:Flow.Workspace.Paths.t ->
  ?fast:bool ->
  Core.program ->
  width:(int -> int option) ->
  legal:(Core.Region.t -> bool) ->
  bool array * group list
(** Device placement and sibling fusion, shared by native drawing and web export. Outputs follow
    invocation order. Groups preserve scope, rate, precision and authored error order; incompatible
    groups remain single-output regions. *)
