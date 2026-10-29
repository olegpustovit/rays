(** Ergonomic functional sketch lifecycle. *)

type clock = Realtime | Fixed of float
type error = Ogpu.Error.t

val error_to_string : error -> string

type config = {
  width : int;
  height : int;
  title : string;
  fps : int option;
  domains : int option;
  clock : clock;
}
(** [default_config] enables native resizing. *)

val default_config : config

val run_state :
  ?config:config ->
  ?max_frames:int ->
  ?crash_dir:string ->
  ?profile:bool ->
  init:(Frame.t -> 'model) ->
  update:('model -> Frame.t -> 'model) ->
  view:('model -> Frame.t -> Scene.t) ->
  ?after_present:('model -> Frame.t -> 'model) ->
  ?crash_dump:('model -> string -> unit) ->
  ?idle:('model -> bool) ->
  ?on_stop:('model -> unit) ->
  unit ->
  ('model, error) result
(** Run a sketch with immutable user state threaded through every frame. Native startup, runtime,
    validation and export failures are returned as [Ogpu.Error.t]. Exceptions raised by sketch
    callbacks are fatal: they write a crash report and propagate. A fatal exception in [init],
    [update], [view], or [after_present] writes a crash report folder under the platform temporary
    directory's [rays-crash] (or [crash_dir]) before it propagates: [crash.txt] holds the exception,
    backtrace, the last 120 frames' input, GC and environment facts; [crash_dump model directory]
    adds the sketch's own files there (e.g. its document). The folder path is printed to stderr.
    [max_frames] keeps one runtime alive for exactly that many frames unless [quit] is requested
    first (the process environment is never read; bin/rays.ml passes RAYS_MAX_FRAMES, RAYS_CRASH_DIR
    and RAYS_PROFILE). [after_present] runs after the native frame is rendered, can capture that
    frame, and returns the model for the next frame and [on_stop]. [idle model] runs about every
    millisecond while an unchanged picture waits for the next frame (input, or at most 16 ms): it
    serves background work for the main domain (a cook's device requests, which otherwise wait for
    the next frame) and answers whether more is in flight; the wait ends when it answers [false]. *)

val export :
  ?config:config ->
  ?prefix:string ->
  directory:string ->
  frames:int ->
  (Frame.t -> Scene.t) ->
  (unit, error) result
(** Renders a deterministic PNG sequence named [prefix-NNNNNN.png]. Captured frames use the
    renderer's native backing dimensions. *)

val export_state :
  ?config:config ->
  ?fps:int ->
  ?prefix:string ->
  directory:string ->
  frames:int ->
  init:(Frame.t -> 'model) ->
  update:('model -> Frame.t -> 'model) ->
  view:('model -> Frame.t -> Scene.t) ->
  ?after_present:('model -> Frame.t -> 'model) ->
  ?on_stop:('model -> unit) ->
  unit ->
  ('model, error) result
(** Stateful deterministic PNG-sequence export. Startup and file-write errors are returned;
    exceptions from sketch callbacks propagate with crash reports. *)

val quit : unit -> unit

val set_relative_mouse : bool -> (unit, string) result
(** Capture and hide the pointer: [Frame.mouse_delta] then reports device motion even at the window
    edge (fly cameras). Released when the sketch stops; an error when no sketch is running. *)

val set_cursor :
  [ `Default | `Horizontal_resize | `Vertical_resize | `Text ] -> (unit, string) result
(** Set the active native pointer cursor; an error when no sketch is running. *)

val set_window_background : Rays_math.Color.t -> (unit, string) result
(** The native window's own background. A Rays window keeps the traffic lights, hides its title and
    has a transparent title bar, so this is the title bar's colour: a host sets it to the colour its
    top edge is painted with. An error when no sketch is running; nothing happens off macOS. *)

type dialog = Open_file | Open_files | Save_file | Open_folder

val show_file_dialog :
  ?filters:(string * string list) list -> ?default_location:string -> dialog -> (int, string) result
(** Open the system's file dialog and return at once with its id. When the user chooses or cancels,
    [Input.File_dialog] arrives in a later frame with the same id: the chosen absolute paths
    ([Open_files] may choose several), or an empty list for a cancel. [filters] are a label with the
    extensions it lists, without dots ([("Images", ["png"; "jpg"])]); an empty list means every
    file. No more than 8 dialogs can be open; an error when no sketch is running. The dialog is
    native and modal to the window; Rays never reads the files. *)
