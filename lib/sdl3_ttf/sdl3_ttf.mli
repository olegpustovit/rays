(** Ownership-aware CPU font metrics and rasterization through SDL3_ttf. *)

type error_kind =
  | Ttf_error
  | Wrong_domain
  | Destroyed
  | Invalid_argument
  | Incompatible_version
  | Not_initialized
  | Font_not_found
  | Fonts_still_open

type error = private {
  operation : string;
  kind : error_kind;
  message : string;
}

val pp_error : Format.formatter -> error -> unit

(** The version macro of the headers compiled against, and the library
    linked; both are checked against packaging/sdl3.lock by the tests. *)
val compiled_version : Sdl3.version
val linked_version : unit -> Sdl3.version
val check_version : ?release:bool -> unit -> (unit, error) result

module Init : sig
  val init : unit -> (unit, error) result
  val initialized : unit -> (bool, error) result
  val quit : unit -> (unit, error) result
end

module Font : sig
  type t

  type metrics = {
    height : int;
    ascent : int;
    descent : int;
    line_skip : int;
  }
  type alignment = Left | Center | Right
  type hinting = Normal_hinting | Light_hinting | Mono_hinting
    | None_hinting | Light_subpixel_hinting
  type glyph_metrics = {
    min_x : int; max_x : int; min_y : int; max_y : int; advance : int;
  }

  val open_file : path:string -> size:float -> (t, error) result

  (** Locate an installed UI font. [RAYS_UI_FONT] is authoritative when
      non-empty; otherwise the macOS system fonts are tried in stable order. *)
  val system_path : unit -> (string, error) result
  val metrics : t -> (metrics, error) result
  val set_size_dpi :
    t -> size:float -> horizontal:int -> vertical:int -> (unit, error) result
  val set_hinting : t -> hinting -> (unit, error) result
  val set_wrap_alignment : t -> alignment -> (unit, error) result
  (** Line alignment inside [render_blended_wrapped] output. *)

  val glyph_metrics : t -> int -> (glyph_metrics, error) result

  (** Render UTF-8 to tightly packed RGBA8 rows. Empty text is [Ok None], preserving
      the high-level no-op contract instead of asking SDL_ttf for a 0-width
      bitmap. Color channels are straight RGBA values in [0,255]. *)
  val render_blended :
    t -> color:int * int * int * int -> string ->
    (Sdl3.rgba option, error) result
  val render_blended_wrapped :
    t -> color:int * int * int * int -> wrap_width:int -> string ->
    (Sdl3.rgba option, error) result

  val destroy : t -> (unit, error) result
end
