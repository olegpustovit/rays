(** Initial-domain conversion of validated packed vec4 output to a resident
    RGBA8 texture, drawn by the generated image.rays raster stages: the unorm
    store clamps channels to [0,1] and rounds to nearest, ties to even. No pixels
    are read back by the converter. *)
type t
type output
val create : Renderer.gpu -> (t, Diagnostic.t) result
val convert : t -> width:int -> height:int -> Run.output -> (output, Diagnostic.t) result
(** Completes the conversion pass before publication. The
    producer must be current and contain exactly width*height vec4 pixels.
    Unchanged dimensions reuse one texture. Resize allocates a replacement,
    retaining the old texture until successful completion.
    Every attempted write invalidates previously borrowed images, including
    failed conversions. Producer finite validation supplies the finite guard. *)

val encode : t -> Ogpu.Backend.commands -> source:Ogpu.Backend.buffer -> width:int -> height:int ->
  Ogpu.Backend.texture -> (unit, Diagnostic.t) result
(** The conversion pass alone, in the caller's [commands]: [source]'s width*height packed vec4s into
    an RGBA8 texture with [Render_attachment] usage (the path tracer's film). *)

val texture : output -> Ogpu.Backend.texture option
(** Borrow until the next conversion or close. Another domain returns None. *)

val close : t -> unit
(** Idempotent on the creating initial domain. Close before releasing its GPU. *)

val buffer_creations : t -> int
(** One: the width the stages read, created with the sink. *)
val texture_creations : t -> int

module Private : sig
  val create_owned : Ogpu.Backend.device -> Ogpu.Backend.queue -> (t, Diagnostic.t) result
end
