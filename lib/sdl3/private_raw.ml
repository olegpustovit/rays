type version = { major : int; minor : int; patch : int }

external linked_version_number : unit -> int = "caml_sdl3_linked_version"
external compiled_version_number : unit -> int = "caml_sdl3_compiled_version"
external get_error : unit -> string = "caml_sdl3_get_error"
external clear_error : unit -> unit = "caml_sdl3_clear_error"
external is_main_thread : unit -> bool = "caml_sdl3_is_main_thread"
external init_subsystem : int -> bool = "caml_sdl3_init_subsystem"
external quit_subsystem : int -> unit = "caml_sdl3_quit_subsystem"

external create_window : string -> int -> int -> int -> nativeint
  = "caml_sdl3_create_window"
external set_window_background : nativeint -> float -> float -> float -> unit
  = "caml_sdl3_set_window_background"
external destroy_window : nativeint -> unit = "caml_sdl3_destroy_window"
external window_size : nativeint -> (int * int) option = "caml_sdl3_window_size"
external window_size_in_pixels : nativeint -> (int * int) option
  = "caml_sdl3_window_size_in_pixels"
external window_state : nativeint -> int = "caml_sdl3_window_state"
external show_window : nativeint -> bool = "caml_sdl3_show_window"
external raise_window : nativeint -> bool = "caml_sdl3_raise_window"
external hide_window : nativeint -> bool = "caml_sdl3_hide_window"
external window_id : nativeint -> int64 = "caml_sdl3_window_id"
external window_pixel_density : nativeint -> float
  = "caml_sdl3_window_pixel_density"
external window_position : nativeint -> (int * int) option
  = "caml_sdl3_window_position"
external window_title : nativeint -> string = "caml_sdl3_window_title"
external center_window : nativeint -> bool = "caml_sdl3_center_window"
external set_window_resizable : nativeint -> bool -> bool
  = "caml_sdl3_set_window_resizable"
external set_window_relative_mouse : nativeint -> bool -> bool
  = "caml_sdl3_set_window_relative_mouse"
external create_system_cursor : int -> nativeint = "caml_sdl3_create_system_cursor"
external set_cursor : nativeint -> bool = "caml_sdl3_set_cursor"
external destroy_cursor : nativeint -> unit = "caml_sdl3_destroy_cursor"
external set_window_size : nativeint -> int -> int -> bool
  = "caml_sdl3_set_window_size"
external restore_window : nativeint -> bool = "caml_sdl3_restore_window"
external sync_window : nativeint -> bool = "caml_sdl3_sync_window"
external show_dialog : nativeint -> int -> (string * string) list -> string option -> int
  = "caml_sdl3_show_dialog"
external set_control_click_right_click : bool -> bool
  = "caml_sdl3_set_control_click_right_click"

external clipboard_set_text : string -> bool = "caml_sdl3_clipboard_set_text"
external clipboard_get_text : unit -> string option = "caml_sdl3_clipboard_get_text"

external start_text_input : nativeint -> bool = "caml_sdl3_start_text_input"
external stop_text_input : nativeint -> bool = "caml_sdl3_stop_text_input"
external set_text_input_area : nativeint -> int -> int -> int -> int -> int -> bool
  = "caml_sdl3_set_text_input_area_bytecode" "caml_sdl3_set_text_input_area"

external create_metal_view : nativeint -> nativeint = "caml_sdl3_create_metal_view"
external destroy_metal_view : nativeint -> unit = "caml_sdl3_destroy_metal_view"
external metal_layer_token : nativeint -> int64 -> int64 -> Native_layer_token.t
  = "caml_sdl3_metal_layer_token"
external invalidate_metal_layer_token : Native_layer_token.t -> unit
  = "caml_sdl3_invalidate_metal_layer_token"

(* The SDL_Event union never crosses this boundary: the stub builds the typed
   event itself (see the enums in sdl3_stubs.c), or skips what Rays does
   not read. *)
type 'event poll = 'event option

external poll_event : unit -> 'event option = "caml_sdl3_poll_event"
external wait_event_timeout : int -> bool = "caml_sdl3_wait_event_timeout"
