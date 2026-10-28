(** Initial-domain conversion of validated packed vec4 output to a resident
    RGBA8 texture. Channels clamp to [0,1] and round to nearest, ties to even.
    Rows use 256-byte alignment. No pixels are read back by the converter. *)
type t
type output
val create : Renderer.gpu -> (t, Diagnostic.t) result
val convert : t -> width:int -> height:int -> Run.output -> (output, Diagnostic.t) result
(** Completes conversion and buffer-to-texture copy before publication. The
    producer must be current and contain exactly width*height vec4 pixels.
    Unchanged dimensions reuse one buffer and texture. Resize allocates a
    replacement pair, retaining the old pair until successful completion.
    Every attempted write invalidates previously borrowed images, including
    failed conversions. Producer finite validation supplies the finite guard. *)

val texture : output -> Ogpu.Backend.texture option
(** Borrow until the next conversion or close. Another domain returns None. *)

val close : t -> unit
(** Idempotent on the creating initial domain. Close before releasing its GPU. *)

val buffer_creations : t -> int
val texture_creations : t -> int

module Private : sig
  val create_owned : Ogpu.Backend.device -> Ogpu.Backend.queue -> (t, Diagnostic.t) result
end
