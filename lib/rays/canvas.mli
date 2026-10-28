type t
val create : width:int -> height:int -> (t,string) result
val create_exn : width:int -> height:int -> t
val size : t -> (int * int,string) result
(* Replace the canvas contents through a reusable, layerless native Metal
   target owned by the canvas. *)
val render : ?density:int -> t -> Scene.t -> (unit,string) result
(* [density] (default 1) renders the scene at that many canvas pixels a logical point, as a
   Retina window does: the scene is [width / density] by [height / density] points.  A canvas
   keeps the density of its first render. *)
val pixel : t -> x:int -> y:int -> (Rays_math.Color.t option,string) result
val pixels : t -> (Rays_math.Color.t array,string) result
val to_image : t -> (Image.t,string) result
val gpu_seconds : t -> (float,string) result
(* The device's GPU time so far, summed over completed command buffers: the difference across a
   [render] is that frame's GPU time (tools/ui_shot.exe's UI_SHOT_BENCH). *)
module Private : sig
  val invalidate : t -> (unit,string) result
  val gpu_source : t -> ((int * int * (unit -> Ogpu.Backend.texture option)),string) result
  (** Borrow the completed GPU frame until another render attempt, explicit
      invalidation, CPU mutation or destruction. Requires the initial SDL domain
      and platform main thread. *)

  val pixel_stats : t -> int * int
  (** Successful captures and actual successful GPU pixel readbacks, including
      operations through private copying and implicit CPU synchronization. *)

  type native_stats = Renderer.stats
  val native_stats : t -> (native_stats,string) result
end
val save_png : t -> string -> (unit,string) result
val save_screen_png : string -> (unit,string) result
val destroy : t -> (unit,string) result
