val lower :
  ?trace:((Packed.t, Diagnostic.t list) result -> unit) ->
  ?strict:bool ->
  ?budget:int ->
  ?lowered:(Flow.Eval.residual -> unit) ->
  Flow.Eval.residual ->
  (Core.program, string) result
(** Frame lowering: literals, captured reads, scalar operations, vectors, patterned lets, branches,
    specialized/inlined callables, macros, bypasses, ordered record fields, text/display/equality,
    bounded lists, higher-order forms and structured product/accumulator loops. Unsupported
    forms/operators are named; no opaque operation survives. *)
(* [budget] is the lowered-term limit (default 2048; cook-time interpreted geometry folds raise it).
   [strict] requires every packed numeric subterm to qualify as a region;
   structured scalar control and bounded list work remain compiled core. [lowered] is told each
   residual the program lowers (the root and the captures it reads), once. *)

type draw_context = {
  value : Core.source -> Flow.Eval.value -> int;
      (** the local holding a constant or residual value *)
  local : Flow.Ty.t -> int;
  emit : Core.source -> Core.statement_node -> unit;
}

val lower_draw :
  ?region_allowed:(Core.Region.t -> bool) ->
  root:Flow.Eval.residual Lazy.t ->
  (draw_context -> unit) ->
  (Core.program, string) result
(** A program whose statements the callback writes, with no result. [root], forced only when a
    residual is lowered, stands for the evaluation (operators, definitions, state). *)
