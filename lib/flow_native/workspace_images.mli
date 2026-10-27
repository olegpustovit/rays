type t
val bind : t -> Flow_sop.Lower.t -> unit
val peek : t -> Flow.Eval.plan -> int -> Rays.Image.t option
val create : ?seed:int64 -> ?grain:int -> ?domains:int -> gpu:Workspace_gpu.t -> Workspace_resources.t -> t
val close : t -> unit
val render_stats : t -> int * int * int * int
val capture_stats : t -> int * int * int * int * int
val image : ?gpu:Flow_ir.Gpu.config -> ?display:bool -> t -> state:Flow.Eval.state -> live:Input.t -> Flow.Eval.plan -> Flow.Eval.value ->
  (Rays.Image.t,Diagnostic.t) result
val payload : t -> Flow_sop.Lower.image_resolver
val texture : ?gpu:Flow_ir.Gpu.config -> t -> state:Flow.Eval.state -> live:Input.t -> Flow.Eval.plan -> Flow.Eval.value ->
  (Rays.Texture.t,Diagnostic.t) result
