(** CPU-only image decoding through the pinned stable SDL3_image library. *)

type error_kind = Decoder_error | Wrong_domain | Invalid_argument | Incompatible_version
type error = private { operation : string; kind : error_kind; message : string }

val pp_error : Format.formatter -> error -> unit

val compiled_version : Sdl3.version
(** The version macro of the headers compiled against, and the library linked; both are checked
    against packaging/sdl3.lock by the tests. *)

val linked_version : unit -> Sdl3.version
val check_version : ?release:bool -> unit -> (unit, error) result

val load_file : string -> (Sdl3.rgba, error) result
(** Decode an image file to tightly packed RGBA8, upright (EXIF orientation applied). The buffer is
    the caller's. *)
