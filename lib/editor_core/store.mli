(** Atomic file writes and the s-expression state stored beside documents: a sketch's or the
    editor's saved settings and the viewport of a preset. Documents (presets) are workspace text,
    see [Editor_document.Preset]. *)

val write_text : filename:string -> string -> (unit, string) result
(** Write atomically through the shared filesystem helper; creates the directory. *)

val read_text : filename:string -> (string, string) result

val user_folder : Input.platform -> pref:string option -> home:string -> string
(** The editor's per-user folder for presets and preferences. Apple keeps [home/.rays]; elsewhere
    it is the platform's preference folder [pref] (SDL's), and [home/.rays] when SDL names none. *)

module Viewport : sig
  val encode : Rays.Easy_camera.t -> look_through:bool -> Flow.Syntax.t
  val decode : Rays.Easy_camera.t -> Flow.Syntax.t -> Rays.Easy_camera.t * bool
end

module Settings : sig
  type value =
    | Bool of bool
    | Float of float
    | Int of int
    | Text of string
    | Choice of string
    | Pair of float * float

  type t = (string * value) list

  val save : sketch:string -> string -> t -> (unit, string) result
  (** One [(settings :sketch "name" :key value ...)] form, written atomically. *)

  val load : sketch:string -> string -> (t, string) result
  val bool : t -> string -> bool option
  val float : t -> string -> float option
  val int : t -> string -> int option
end
