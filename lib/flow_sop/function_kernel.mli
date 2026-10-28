val node :
  ?state:Flow.Eval.state ->
  ?reference:bool ->
  ?elems:(string * Flow.Eval.value) list ->
  identity:int ->
  signature:Flow.Ty.fn_signature ->
  fn:Flow.Eval.fn ->
  sources:int list ->
  Sop.Node.t list ->
  Sop.Node.t
(** Language-owned compiled bulk functions behind the Sop payload boundary. *)
