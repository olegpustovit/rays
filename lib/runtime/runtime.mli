(** SDL presentation shell and logical/drawable frame facts. GPU execution is
    owned by [Renderer]; window-only operations return [Unsupported] offscreen. *)
type t
val clipboard_set_text : string -> (unit, Ogpu.Error.t) result
val clipboard_get_text : unit -> (string, Ogpu.Error.t) result

type frame_facts = { logical_width:int; logical_height:int;
  drawable_width:int; drawable_height:int; pixel_scale_x:float; pixel_scale_y:float }

(** An offscreen target has no [position] or [refresh_rate], never vsyncs,
    and reports its drawable/logical ratio as both density and display scale. *)
type presentation_facts = {
   logical_width : int; logical_height : int;
  drawable_width : int; drawable_height : int;
  pixel_density : float;

}
val create : ?vsync:bool -> ?hidden:bool -> ?high_density:bool -> ?title:string ->
  width:int -> height:int -> unit -> (t, Ogpu.Error.t) result
(** [high_density] (default true) asks for a drawable at the display's native density; false
    keeps the drawable at the logical size, so a capture is the same bytes on every display. *)

(** An offscreen frame-facts source; GPU resources are owned by [Renderer]. *)
val create_offscreen :
  logical_width:int -> logical_height:int -> width:int -> height:int -> unit ->
  (t,Ogpu.Error.t) result
(** Resizes to [width]x[height] logical points. A window's drawable follows
    its display, so [?drawable] is rejected there; an offscreen target uses
    [?drawable] pixels, 1x by default. *)
val resize : t -> width:int -> height:int ->
  (unit, Ogpu.Error.t) result
val frame_facts : t -> frame_facts
val map_logical_rect : frame_facts -> int * int * int * int ->
  int * int * int * int

(** Cached presentation facts, requeried when the drawable changes and after
    resize or show. *)
val presentation_facts : t -> (presentation_facts, Ogpu.Error.t) result

val set_resizable : t -> bool -> (unit, Ogpu.Error.t) result
val set_window_background : t -> float * float * float -> (unit, Ogpu.Error.t) result
(** The native window's own background (sRGB, 0..1): what the title bar shows,
    since a Rays window hides its title and keeps only the traffic lights. *)

val set_relative_mouse : t -> bool -> (unit, Ogpu.Error.t) result
(** Hide and capture the pointer, reporting relative motion (fly cameras). *)

val set_cursor : t -> [`Default|`Horizontal_resize|`Vertical_resize|`Text] ->
  (unit, Ogpu.Error.t) result

(** Text input runs only while a text field has focus: [Some (region, cursor)]
    (logical points) starts it and places the input method there, [None] stops
    it. Repeating the same answer costs no SDL call. *)
val set_text_input : t -> ((int * int * int * int) * int) option ->
  (unit, Ogpu.Error.t) result

(** Tell a window target that SDL announced a size or density change, so the
    next frame re-reads its facts. The window is not polled between
    announcements. *)
val window_changed : t -> unit

val show : t -> (unit, Ogpu.Error.t) result

(** Native file dialogs. [show_dialog] returns at once with the dialog's id;
    the outcome arrives as a [Input.File_dialog] with that id, in
    the next event poll on the initial domain. [pattern] lists extensions
    separated by semicolons ("png;jpg"), or "*". At most 8 dialogs are open at
    once; a ninth is an error. *)
type dialog_kind = Open_file | Open_files | Save_file | Open_folder
type dialog_filter = { name : string; pattern : string }
val show_dialog : t -> ?filters:dialog_filter list -> ?default_location:string ->
  dialog_kind -> (int, Ogpu.Error.t) result

(** Shown and neither minimized nor covered. SDL announces the start of
    occlusion and not its end: ask once per frame while hidden. *)
val visible : t -> (bool, Ogpu.Error.t) result
val destroy : t -> (unit, Ogpu.Error.t) result
module Private : sig
  val surface_configuration : t -> (Ogpu.Surface.configuration, Ogpu.Error.t) result
  val sync_facts : t -> (unit, Ogpu.Error.t) result
  val is_window : t -> bool
  (** One announcement bit per window: [announce] when SDL says the size or
      density changed, [refresh flag query] runs [query] once and only if a
      change was announced since the last refresh. *)
  module Change_flag : sig
    type t
    val create : unit -> t
    val announce : t -> unit
    val refresh : t -> (unit -> 'a) -> 'a option
  end

  (** The SDL window of a window target, for qualification tests that change
      its size behind the runtime's back. *)
  val window_handle : t -> Sdl3.Window.t option

end
