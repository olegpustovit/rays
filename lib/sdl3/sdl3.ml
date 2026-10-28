type error_kind =
  | Sdl_error
  | Wrong_domain
  | Destroyed
  | Parent_has_dependents
  | Incompatible_version
  | Invalid_argument
  | Unsupported

type error = { operation : string; kind : error_kind; message : string }
type rect = { x : int; y : int; width : int; height : int }

let pp_error formatter error = Format.fprintf formatter "%s: %s" error.operation error.message
let error operation kind message = Error { operation; kind; message }

let contains_nul value =
  try
    ignore (String.index value '\x00');
    true
  with Not_found -> false

let sdl_error operation =
  let message = Private_raw.get_error () in
  let message = if message = "" then "SDL call failed without an error" else message in
  error operation Sdl_error message

type version = { major : int; minor : int; patch : int }

let version_of_number value =
  { major = value / 1_000_000; minor = value / 1_000 mod 1_000; patch = value mod 1_000 }

(* The headers' own version macro, not a checked-in constant. *)
let compiled_version : version = version_of_number (Private_raw.compiled_version_number ())
let version_number value = (value.major * 1_000_000) + (value.minor * 1_000) + value.patch
let stable_version value = value.minor mod 2 = 0 && value.patch mod 2 = 0
let linked_version () = version_of_number (Private_raw.linked_version_number ())
let version_string value = Printf.sprintf "%d.%d.%d" value.major value.minor value.patch

let validate_version ?(library = "SDL") ?(compiled = compiled_version) ~release ~linked () =
  if release && not (stable_version compiled) then
    error "SDL3.validate_version" Incompatible_version
      ("compiled against prerelease " ^ library ^ " headers " ^ version_string compiled)
  else if version_number linked < version_number compiled then
    error "SDL3.validate_version" Incompatible_version
      (Printf.sprintf "linked %s %s is older than compiled headers %s" library
         (version_string linked) (version_string compiled))
  else if release && not (stable_version linked) then
    error "SDL3.validate_version" Incompatible_version
      ("linked " ^ library ^ " is a development release: " ^ version_string linked)
  else Ok ()

let check_version ?(release = true) () = validate_version ~release ~linked:(linked_version ()) ()

module Thread = struct
  let is_initial_domain = Domain.is_main_domain
  let is_sdl_main_thread = Private_raw.is_main_thread

  let require operation =
    if not (is_initial_domain ()) then
      error operation Wrong_domain "SDL operation must run on the initial OCaml domain"
    else if not (is_sdl_main_thread ()) then
      error operation Wrong_domain "SDL operation must run on the platform main thread"
    else Ok ()
end

(* A finalized native object must never leak because the main domain has not
   drained yet, so the queues are unbounded and mutex-protected; a finalizer
   only appends the raw pointer, and [before_main] drains on every SDL call,
   children before parents. *)
module Release_queue = struct
  type kind = { pending : nativeint Queue.t; destroy : nativeint -> unit }

  let mutex = Mutex.create ()

  (* Finalizers run on any domain: the count lets the common empty drain skip
     the lock and allocate nothing. *)
  let pending_count = Atomic.make 0
  let kind destroy = { pending = Queue.create (); destroy }
  let metal_views = kind Private_raw.destroy_metal_view
  let windows = kind Private_raw.destroy_window
  let cursors = kind Private_raw.destroy_cursor

  let enqueue kind raw =
    Mutex.lock mutex;
    Queue.add raw kind.pending;
    Atomic.incr pending_count;
    Mutex.unlock mutex

  let metal_view = enqueue metal_views
  let window = enqueue windows
  let cursor = enqueue cursors

  let drain_kind kind =
    Mutex.lock mutex;
    let batch = List.of_seq (Queue.to_seq kind.pending) in
    Queue.clear kind.pending;
    Atomic.fetch_and_add pending_count (-List.length batch) |> ignore;
    Mutex.unlock mutex;
    List.iter kind.destroy batch

  let drain () =
    if Atomic.get pending_count <> 0 then begin
      drain_kind metal_views;
      drain_kind windows;
      drain_kind cursors
    end
end

let before_main operation =
  match Thread.require operation with
  | Error _ as failure -> failure
  | Ok () ->
      Release_queue.drain ();
      Ok ()

let on_main operation callback =
  match before_main operation with Error _ as failure -> failure | Ok () -> callback ()

module Init = struct
  type subsystem = Video | Events

  (* The bits stubs.c maps to SDL_INIT_VIDEO and SDL_INIT_EVENTS. *)
  let bit = function Video -> 1 | Events -> 2
  let mask values = List.fold_left (fun mask value -> mask lor bit value) 0 values

  let init subsystems =
    on_main "SDL3.Init.init" (fun () ->
        match check_version () with
        | Error _ as failure -> failure
        | Ok () ->
            Private_raw.clear_error ();
            if Private_raw.init_subsystem (mask subsystems) then Ok ()
            else sdl_error "SDL3.Init.init")

  let quit_subsystems subsystems =
    on_main "SDL3.Init.quit_subsystems" (fun () ->
        Private_raw.quit_subsystem (mask subsystems);
        Ok ())
end

module Hint = struct
  let control_click_is_right_click enabled =
    let operation = "SDL3.Hint.control_click_is_right_click" in
    on_main operation (fun () ->
        Private_raw.clear_error ();
        if Private_raw.set_control_click_right_click enabled then Ok () else sdl_error operation)
end

module Window = struct
  type flag = Hidden | High_pixel_density | Metal
  type t = { raw : nativeint; mutable destroyed : bool; metal_views : int Atomic.t }

  (* The bits stubs.c maps to SDL_WINDOW_HIDDEN, SDL_WINDOW_HIGH_PIXEL_DENSITY
     and SDL_WINDOW_METAL. *)
  let flag = function Hidden -> 1 | High_pixel_density -> 2 | Metal -> 4
  let flags_mask values = List.fold_left (fun bits value -> bits lor flag value) 0 values

  let live operation value callback =
    on_main operation (fun () ->
        if value.destroyed then error operation Destroyed "window is destroyed"
        else callback value.raw)

  let create ~title ~width ~height ?(flags = []) () =
    if contains_nul title then
      error "SDL3.Window.create" Invalid_argument "window title contains a NUL byte"
    else if width <= 0 || height <= 0 then
      error "SDL3.Window.create" Invalid_argument "window dimensions must be positive"
    else
      on_main "SDL3.Window.create" (fun () ->
          Private_raw.clear_error ();
          let raw = Private_raw.create_window title width height (flags_mask flags) in
          if raw = Nativeint.zero then sdl_error "SDL3.Window.create"
          else
            let value = { raw; destroyed = false; metal_views = Atomic.make 0 } in
            Gc.finalise
              (fun value ->
                if not value.destroyed then begin
                  value.destroyed <- true;
                  Release_queue.window value.raw
                end)
              value;
            Ok value)

  let size value =
    let operation = "SDL3.Window.size" in
    match before_main operation with
    | Error _ as failure -> failure
    | Ok () -> (
        if value.destroyed then error operation Destroyed "window is destroyed"
        else
          match Private_raw.window_size value.raw with
          | Some size -> Ok size
          | None -> sdl_error operation)

  let size_in_pixels value =
    let operation = "SDL3.Window.size_in_pixels" in
    match before_main operation with
    | Error _ as failure -> failure
    | Ok () -> (
        if value.destroyed then error operation Destroyed "window is destroyed"
        else
          match Private_raw.window_size_in_pixels value.raw with
          | Some size -> Ok size
          | None -> sdl_error operation)

  let positive_scale operation call value =
    live operation value (fun raw ->
        Private_raw.clear_error ();
        let scale = call raw in
        if Float.is_finite scale && scale > 0. then Ok scale else sdl_error operation)

  let pixel_density = positive_scale "SDL3.Window.pixel_density" Private_raw.window_pixel_density

  let position value =
    live "SDL3.Window.position" value (fun raw ->
        Private_raw.clear_error ();
        match Private_raw.window_position raw with
        | Some position -> Ok position
        | None -> sdl_error "SDL3.Window.position")

  let title value = live "SDL3.Window.title" value (fun raw -> Ok (Private_raw.window_title raw))

  let center value =
    live "SDL3.Window.center" value (fun raw ->
        Private_raw.clear_error ();
        if Private_raw.center_window raw then Ok () else sdl_error "SDL3.Window.center")

  let set_size value ~width ~height =
    let operation = "SDL3.Window.set_size" in
    if width <= 0 || height <= 0 then
      error operation Invalid_argument "window dimensions must be positive"
    else
      live operation value (fun raw ->
          Private_raw.clear_error ();
          if Private_raw.set_window_size raw width height then Ok () else sdl_error operation)

  type state = { hidden : bool; minimized : bool; occluded : bool }

  (* Occlusion has a start event and no matching end, so a caller that saw
     [Occluded] reads this to learn when the window is uncovered again. *)
  let state value =
    live "SDL3.Window.state" value (fun raw ->
        let bits = Private_raw.window_state raw in
        Ok { hidden = bits land 1 <> 0; minimized = bits land 2 <> 0; occluded = bits land 4 <> 0 })

  (* Occlusion has a start event and no matching end, so a caller that sees
     [Occluded] reads this to learn when the window is uncovered again. *)

  let bool_call operation call value =
    live operation value (fun raw ->
        Private_raw.clear_error ();
        if call raw then Ok () else sdl_error operation)

  let set_background value ~red ~green ~blue =
    live "SDL3.Window.set_background" value (fun raw ->
        Ok (Private_raw.set_window_background raw red green blue))

  let set_resizable value enabled =
    live "SDL3.Window.set_resizable" value (fun raw ->
        Private_raw.clear_error ();
        if Private_raw.set_window_resizable raw enabled then Ok ()
        else sdl_error "SDL3.Window.set_resizable")

  let set_relative_mouse value enabled =
    live "SDL3.Window.set_relative_mouse" value (fun raw ->
        Private_raw.clear_error ();
        if Private_raw.set_window_relative_mouse raw enabled then Ok ()
        else
          error "SDL3.Window.set_relative_mouse" Unsupported
            (let message = Private_raw.get_error () in
             if message = "" then "relative mouse mode is unavailable" else message))

  let show = bool_call "SDL3.Window.show" Private_raw.show_window
  let raise_window = bool_call "SDL3.Window.raise_window" Private_raw.raise_window
  let hide = bool_call "SDL3.Window.hide" Private_raw.hide_window
  let restore = bool_call "SDL3.Window.restore" Private_raw.restore_window
  let sync = bool_call "SDL3.Window.sync" Private_raw.sync_window

  let destroy value =
    on_main "SDL3.Window.destroy" (fun () ->
        if value.destroyed then Ok ()
        else if Atomic.get value.metal_views <> 0 then
          error "SDL3.Window.destroy" Parent_has_dependents
            (Printf.sprintf "window still owns %d Metal view(s)" (Atomic.get value.metal_views))
        else begin
          value.destroyed <- true;
          Private_raw.destroy_window value.raw;
          Ok ()
        end)
end

module Cursor = struct
  type shape = Default | Text | Ew_resize | Ns_resize
  type t = { raw : nativeint; mutable destroyed : bool }

  (* The order of the shapes table in the stub. *)
  let code = function Default -> 0 | Text -> 1 | Ew_resize -> 2 | Ns_resize -> 3

  let create shape =
    on_main "SDL3.Cursor.create" (fun () ->
        Private_raw.clear_error ();
        let raw = Private_raw.create_system_cursor (code shape) in
        if raw = Nativeint.zero then
          error "SDL3.Cursor.create" Unsupported
            (let message = Private_raw.get_error () in
             if message = "" then "system cursors are unavailable" else message)
        else
          let value = { raw; destroyed = false } in
          Gc.finalise
            (fun value ->
              if not value.destroyed then begin
                value.destroyed <- true;
                Release_queue.cursor value.raw
              end)
            value;
          Ok value)

  let set value =
    on_main "SDL3.Cursor.set" (fun () ->
        if value.destroyed then error "SDL3.Cursor.set" Destroyed "cursor is destroyed"
        else (
          Private_raw.clear_error ();
          if Private_raw.set_cursor value.raw then Ok () else sdl_error "SDL3.Cursor.set"))

  let destroy value =
    on_main "SDL3.Cursor.destroy" (fun () ->
        if value.destroyed then Ok ()
        else begin
          value.destroyed <- true;
          Private_raw.destroy_cursor value.raw;
          Ok ()
        end)
end

module Metal_view : sig
  type layer = Native_layer_token.t
  type t

  val create : Window.t -> (t, error) result
  val layer : t -> (layer, error) result
  val destroy : t -> (unit, error) result
end = struct
  type layer = Native_layer_token.t

  type t = {
    raw : nativeint;
    generation : int;
    window : Window.t;
    mutable layer_token : layer option;
    mutable destroyed : bool;
  }

  let next_generation = Atomic.make 1

  let create window =
    on_main "SDL3.Metal_view.create" (fun () ->
        if window.Window.destroyed then
          error "SDL3.Metal_view.create" Destroyed "parent window is destroyed"
        else begin
          Private_raw.clear_error ();
          let raw = Private_raw.create_metal_view window.raw in
          if raw = Nativeint.zero then sdl_error "SDL3.Metal_view.create"
          else begin
            Atomic.incr window.metal_views;
            let value =
              {
                raw;
                generation = Atomic.fetch_and_add next_generation 1;
                window;
                layer_token = None;
                destroyed = false;
              }
            in
            Gc.finalise
              (fun value ->
                if not value.destroyed then begin
                  value.destroyed <- true;
                  Option.iter Private_raw.invalidate_metal_layer_token value.layer_token;
                  Atomic.decr value.window.metal_views;
                  Release_queue.metal_view value.raw
                end)
              value;
            Ok value
          end
        end)

  let layer value =
    on_main "SDL3.Metal_view.layer" (fun () ->
        if value.destroyed then error "SDL3.Metal_view.layer" Destroyed "Metal view is destroyed"
        else if value.window.destroyed then
          error "SDL3.Metal_view.layer" Destroyed "parent window is destroyed"
        else
          match value.layer_token with
          | Some token when Native_layer_token.alive token -> Ok token
          | _ ->
              let token =
                Private_raw.metal_layer_token value.raw
                  (Private_raw.window_id value.window.raw)
                  (Int64.of_int value.generation)
              in
              if Native_layer_token.alive token then (
                value.layer_token <- Some token;
                Ok token)
              else sdl_error "SDL3.Metal_view.layer")

  let destroy value =
    on_main "SDL3.Metal_view.destroy" (fun () ->
        if value.destroyed then Ok ()
        else begin
          value.destroyed <- true;
          Option.iter Private_raw.invalidate_metal_layer_token value.layer_token;
          Atomic.decr value.window.metal_views;
          Private_raw.destroy_metal_view value.raw;
          Ok ()
        end)
end

module Clipboard = struct
  let set_text text =
    let operation = "SDL3.Clipboard.set_text" in
    if contains_nul text then error operation Invalid_argument "clipboard text contains a NUL byte"
    else
      on_main operation (fun () ->
          Private_raw.clear_error ();
          if Private_raw.clipboard_set_text text then Ok () else sdl_error operation)

  let get_text () =
    on_main "SDL3.Clipboard.get_text" (fun () ->
        Private_raw.clear_error ();
        match Private_raw.clipboard_get_text () with
        | Some text -> Ok text
        | None -> sdl_error "SDL3.Clipboard.get_text")
end

module Text_input = struct
  let live operation window callback =
    on_main operation (fun () ->
        if window.Window.destroyed then error operation Destroyed "text input window is destroyed"
        else callback window.Window.raw)

  let bool_call operation call window =
    live operation window (fun raw ->
        Private_raw.clear_error ();
        if call raw then Ok () else sdl_error operation)

  let start = bool_call "SDL3.Text_input.start" Private_raw.start_text_input
  let stop = bool_call "SDL3.Text_input.stop" Private_raw.stop_text_input

  let set_area window { x; y; width; height } ~cursor =
    let operation = "SDL3.Text_input.set_area" in
    if cursor < 0 then
      error operation Invalid_argument "text-input cursor offset must be non-negative"
    else if width <= 0 || height <= 0 then
      error operation Invalid_argument "text-input area dimensions must be positive"
    else
      live operation window (fun raw ->
          Private_raw.clear_error ();
          if Private_raw.set_text_input_area raw x y width height cursor then Ok ()
          else sdl_error operation)
end

module Key = struct
  (* Chosen in the stubs with SDL's own key macros; a letter or digit is
     [Char] in lower case whatever the layout, by the key's position. *)
  type t =
    | Char of char
    | Arrow_up
    | Arrow_down
    | Arrow_left
    | Arrow_right
    | Space
    | Enter
    | Escape
    | Backspace
    | Tab
    | Shift
    | Control
    | Alt
    | Meta
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | Home
    | End
    | Page_up
    | Page_down
    | Insert
    | Delete
    | Unknown of int

  type modifier =
    | Shift_held
    | Control_held
    | Alt_held
    | Meta_held
    | Num_lock
    | Caps_lock
    | Scroll_lock
end

module Dialog = struct
  type kind = Open_file | Open_files | Save_file | Open_folder
  type filter = { name : string; pattern : string }

  (* the order of the kinds in the stub *)
  let code = function Open_file -> 0 | Open_files -> 1 | Save_file -> 2 | Open_folder -> 3
  let max_filters = 32

  let show (window : Window.t) ?(filters = []) ?default_location kind =
    let operation = "SDL3.Dialog.show" in
    let no_nul =
      List.for_all
        (fun { name; pattern } -> not (contains_nul name || contains_nul pattern))
        filters
      && not (Option.fold ~none:false ~some:contains_nul default_location)
    in
    if not no_nul then
      error operation Invalid_argument "a dialog filter or location contains a NUL byte"
    else if List.length filters > max_filters then
      error operation Invalid_argument "too many dialog filters"
    else if List.exists (fun { name; pattern } -> name = "" || pattern = "") filters then
      error operation Invalid_argument "a dialog filter needs a name and a pattern"
    else
      on_main operation (fun () ->
          if window.Window.destroyed then error operation Destroyed "window is destroyed"
          else begin
            Private_raw.clear_error ();
            let id =
              Private_raw.show_dialog window.Window.raw (code kind)
                (List.map (fun { name; pattern } -> (name, pattern)) filters)
                default_location
            in
            if id = 0 then sdl_error operation else Ok id
          end)
end

module Event = struct
  type mouse_button = Left | Middle | Right | X1 | X2
  type wheel_direction = Normal | Flipped
  type pinch_phase = Began | Updated | Ended
  type scroll_phase = Scroll_began | Scroll_changed | Scroll_ended | Scroll_momentum

  type window_change =
    | Shown
    | Hidden
    | Minimized
    | Restored
    | Occluded
    | Focus_gained
    | Focus_lost
    | Close_requested
    | Resized of int * int
    | Pixel_size_changed of int * int

  type drop_change = Drop_begin | Drop_position | Drop_complete | File of string
  type dialog_outcome = Chosen of string list | Cancelled | Failed of string

  (* The order of these constructors is the order of the tags in the stubs. *)
  type t =
    | Quit
    | Window of window_change
    | Key of { key : Key.t; modifiers : Key.modifier list; down : bool; repeat : bool }
    | Text_input of string
    | Text_editing of { text : string; start : int; length : int }
    | Mouse_motion of { x : float; y : float; dx : float; dy : float }
    | Mouse_button of { button : mouse_button; down : bool; x : float; y : float }
    | Mouse_wheel of {
        x : float;
        y : float;
        direction : wheel_direction;
        mouse_x : float;
        mouse_y : float;
        integer_x : int;
        integer_y : int;
      }
    | Pinch of { phase : pinch_phase; scale : float }
    | Drop of { change : drop_change; x : float; y : float }
    | Dialog of { id : int; outcome : dialog_outcome }
    | Scroll of { x : float; y : float; phase : scroll_phase; seconds : float }

  let poll () : t option = Private_raw.poll_event ()

  (* Motion and window-size events are floods: one frame keeps the latest
     position (with the summed relative motion) and the latest size of each
     kind, and everything else keeps its order. *)
  let flush motion sizes events =
    let events = List.fold_left (fun events size -> size :: events) events (List.rev sizes) in
    match motion with None -> events | Some motion -> motion :: events

  let same_size_kind a b =
    match (a, b) with
    | Window (Resized _), Window (Resized _)
    | Window (Pixel_size_changed _), Window (Pixel_size_changed _) ->
        true
    | _ -> false

  (* [next] is the event just polled; an empty queue ends the frame. *)
  let rec collect motion sizes events next =
    match next with
    | None -> List.rev (flush motion sizes events)
    | Some (Mouse_motion m) ->
        let merged =
          match motion with
          | Some (Mouse_motion p) -> Mouse_motion { m with dx = m.dx +. p.dx; dy = m.dy +. p.dy }
          | Some _ | None -> Mouse_motion m
        in
        collect (Some merged) sizes events (poll ())
    | Some (Window (Resized _ | Pixel_size_changed _) as size) ->
        let sizes =
          if List.exists (same_size_kind size) sizes then
            List.map (fun old -> if same_size_kind size old then size else old) sizes
          else size :: sizes
        in
        collect motion sizes events (poll ())
    | Some event -> collect None [] (event :: flush motion sizes events) (poll ())

  let poll_coalesced () =
    on_main "SDL3.Event.poll_coalesced" (fun () -> Ok (collect None [] [] (poll ())))

  let wait ~timeout_ms =
    on_main "SDL3.Event.wait" (fun () -> Ok (Private_raw.wait_event_timeout (max 0 timeout_ms)))
end

(* Decoded pixels: tightly packed RGBA8 rows, ownership with the caller. *)
type rgba = { width : int; height : int; pixels : bytes }
