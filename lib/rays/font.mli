type t
type render_mode=Blended of Rays_math.Color.t
type hinting=Normal_hinting|Light_hinting|Mono_hinting|None_hinting
type alignment=Left|Center|Right
val load:string->int->(t,[`Msg of string])result
val system : ?size:int -> unit -> (t,[`Msg of string]) result
val cached_text : ?wrap:int -> ?align:alignment -> ?density:int -> t -> string -> render_mode -> (Image.t,[`Msg of string]) result
module Private : sig
  type cache_context
  val create_cache_context : unit -> cache_context
  val with_cache_context : cache_context -> (unit -> 'a) -> 'a
  val clear_cache_context : cache_context -> unit
  type automatic
  val borrow_automatic : ?wrap:int -> ?align:alignment -> ?density:int -> size:int -> string -> render_mode -> (automatic,[`Msg of string]) result
  val automatic_image : automatic -> Image.t
  val release_automatic : automatic -> unit
  val automatic_counts : unit -> int * int * int
  val generation : t -> int
  type glyph = { glyph_width:int; glyph_height:int; glyph_advance:int;
    glyph_alpha:bytes }
  val glyph : ?density:int -> t -> int -> (glyph,[`Msg of string]) result
  (** Rasterize one code point at [density] exactly as it appears inside a
      rendered string; [glyph_advance] is the pen advance in backing pixels
      and [glyph_alpha] holds one coverage byte per pixel. *)

end
(* Measurements are logical points and allocate no image; [wrap] measures
   wrapped text. Invalid UTF-8 is measured and drawn as U+FFFD. *)
(* Vertical metrics in logical points. Raise [Invalid_argument] on a
   destroyed font. *)
val get_ascent : t -> int
val set_hinting:t->hinting->(unit,[`Msg of string])result
val destroy:t->unit
