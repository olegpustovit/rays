type frame_facts = {
  logical_width : int;
  logical_height : int;
  drawable_width : int;
  drawable_height : int;
  pixel_scale_x : float;
  pixel_scale_y : float;
}

type presentation_facts = {

  logical_width : int;
  logical_height : int;
  drawable_width : int;
  drawable_height : int;

  pixel_density : float;

}

(* SDL announces a change of size or density with an event instead of making
   every frame ask. The announcement is one bit: set by the event, taken by the
   next frame, which re-reads the window only if it was set. *)
module Change_flag = struct
  type t = { mutable announced : bool }

  let create () = { announced = false }
  let announce flag = flag.announced <- true

  (* [query] runs once per announcement, however many arrived. *)
  let refresh flag query =
    if flag.announced then begin
      flag.announced <- false;
      Some (query ())
    end else None
end

type cursor_shape = [ `Default | `Horizontal_resize | `Vertical_resize | `Text ]

type window = {
  handle : Sdl3.Window.t;
  view : Sdl3.Metal_view.t;
  vsync : bool;
  mutable cursors : (cursor_shape * Sdl3.Cursor.t) list;
  mutable cursor_shape : cursor_shape option;
  mutable text_input : bool;
  changed : Change_flag.t;
}

(* One render target: a presenting window, or ([window = None]) an owned
   offscreen texture. *)
type t = {
  window : window option;

  mutable facts : frame_facts;
  mutable presentation : presentation_facts option;
  mutable dead : bool;
}

let stale operation = Error (Ogpu.Error.make operation Ogpu.Error.Stale_handle "runtime is destroyed")

let error op text = Error (Ogpu.Error.make op Ogpu.Error.Invalid_state text)
let sdl op = function Ok x -> Ok x | Error e -> error op (Format.asprintf "%a" Sdl3.pp_error e)
let clipboard_set_text text = sdl "Runtime.clipboard_set_text" (Sdl3.Clipboard.set_text text)
let clipboard_get_text () = sdl "Runtime.clipboard_get_text" (Sdl3.Clipboard.get_text ())

let facts window =
  match (Sdl3.Window.size window, Sdl3.Window.size_in_pixels window) with
  | Ok (lw, lh), Ok (dw, dh) when lw > 0 && lh > 0 && dw > 0 && dh > 0 ->
      Ok
        {
          logical_width = lw;
          logical_height = lh;
          drawable_width = dw;
          drawable_height = dh;
          pixel_scale_x = float dw /. float lw;
          pixel_scale_y = float dh /. float lh;
        }
  | Error e, _ -> sdl "Runtime.facts" (Error e)
  | _, Error e -> sdl "Runtime.facts" (Error e)
  | _ -> error "Runtime.facts" "window dimensions are invalid"

 (* Every step is checked: the first failure is the result. *)
let reveal window =
  let ( let* ) = Result.bind in
  let* () = Sdl3.Window.show window in
  let* () = Sdl3.Window.restore window in
  let* () = Sdl3.Window.raise_window window in
  let* () = Sdl3.Window.center window in
  Sdl3.Window.sync window

let create ?(vsync = true) ?(hidden = true) ?(high_density = true) ?(title = "Rays") ~width ~height () =
  let op = "Runtime.create" in
  if width <= 0 || height <= 0 then
    Error (Ogpu.Error.make op Invalid_argument "dimensions must be positive")
  else
    (* Each acquired SDL resource pushes its release; any later failure
       unwinds them newest first. *)
    let undo = ref [] in
    let unwind () =
      List.iter (fun release -> ignore (release ())) !undo;
      undo := []
    in
    let acquire result release =
      Result.map
        (fun value ->
          undo := (fun () -> release value) :: !undo;
          value)
        (sdl op result)
    in
    let ( let* ) = Result.bind in
    let created =
      let* () = acquire (Sdl3.Init.init [ Sdl3.Init.Video ]) (fun () ->
        Sdl3.Init.quit_subsystems [ Sdl3.Init.Video ]) in
      (* SDL itself turns a Control-click into the secondary click. *)
      let* () = sdl op (Sdl3.Hint.control_click_is_right_click true) in
      let flags : Sdl3.Window.flag list =
        Sdl3.Window.Metal :: (if high_density then [ Sdl3.Window.High_pixel_density ] else []) @ (if hidden then [ Sdl3.Window.Hidden ] else [])
      in
      let* window = acquire (Sdl3.Window.create ~title ~width ~height ~flags ()) Sdl3.Window.destroy in
      let* () = if hidden then Ok () else sdl op (reveal window) in
      let* view = acquire (Sdl3.Metal_view.create window) Sdl3.Metal_view.destroy in
      let* _token = sdl op (Sdl3.Metal_view.layer view) in
      (* Size the renderer for the actual drawable once, rather than
         allocating a logical-size target and immediately resizing it. *)
      let* facts = facts window in
      Ok
        {
          window =
              Some
              { handle = window; view; vsync; cursors = []; cursor_shape = None;
                text_input = false; changed = Change_flag.create () };

          facts;
          presentation = None;
          dead = false;
        }
    in
    (match created with Ok _ -> undo := [] | Error _ -> unwind ());
    created

let offscreen_facts ~logical_width ~logical_height ~width ~height =
  {
    logical_width;
    logical_height;
    drawable_width = width;
    drawable_height = height;
    pixel_scale_x = float width /. float logical_width;
    pixel_scale_y = float height /. float logical_height;
  }

let create_offscreen ~logical_width ~logical_height ~width ~height () =
  let op = "Runtime.create_offscreen" in
  if width <= 0 || height <= 0 || logical_width <= 0 || logical_height <= 0 then
    Error (Ogpu.Error.make op Invalid_argument "dimensions must be positive")
  else begin
    Ok
      {
        window = None;
        facts = offscreen_facts ~logical_width ~logical_height ~width ~height;
        presentation = None;
        dead = false;
      }
  end

let scale_rect (facts : frame_facts) (x, y, w, h) =
  let edge value logical drawable = value * drawable / logical in
  let l = edge x facts.logical_width facts.drawable_width
  and t = edge y facts.logical_height facts.drawable_height
  and r = edge (x + w) facts.logical_width facts.drawable_width
  and b = edge (y + h) facts.logical_height facts.drawable_height in
  (l, t, r - l, b - t)

let map_logical_rect = scale_rect

let apply_facts (value : t) (facts : frame_facts) =
  value.facts <- facts;
  Ok ()

(* A window's drawable follows SDL, which announces each change as a size
   event ([window_changed]); an offscreen target changes only through
   [resize]. A frame costs no SDL call while nothing was announced. *)
let sync_facts (value : t) =
  match value.window with
  | None -> Ok ()
  | Some window -> (
      match Change_flag.refresh window.changed (fun () -> facts window.handle) with
      | None -> Ok ()
      | Some (Error _ as error) -> error
      | Some (Ok live)
        when live.logical_width = value.facts.logical_width
             && live.logical_height = value.facts.logical_height
             && live.drawable_width = value.facts.drawable_width
             && live.drawable_height = value.facts.drawable_height ->
          Ok ()
      | Some (Ok live) -> apply_facts value live)

(* Runs one frame against the synced target; only a window presents. *)
let resize (value : t) ~width ~height =
  let op = "Runtime.resize" in
  if value.dead then stale op
  else if width <= 0 || height <= 0 then
    Error (Ogpu.Error.make op Invalid_argument "dimensions must be positive")
  else
    match value.window with
    | Some window -> (
        match sdl op (Sdl3.Window.set_size window.handle ~width ~height) with
        | Error _ as e -> e
        | Ok () ->
            value.presentation <- None;
            Result.bind (facts window.handle) (apply_facts value))
    | None ->
        value.presentation <- None;
        apply_facts value
          (offscreen_facts ~logical_width:width ~logical_height:height ~width ~height)

let frame_facts (value : t) = value.facts

let live_window operation (value : t) callback =
  if value.dead then stale operation
  else
    match value.window with
    | None -> Error (Ogpu.Error.make operation Unsupported "operation requires a window")
    | Some window -> callback window

let query_presentation (value : t) =
  match value.window with
  | None ->
      let facts = value.facts in
      Ok
        {

          logical_width = facts.logical_width;
          logical_height = facts.logical_height;
          drawable_width = facts.drawable_width;
          drawable_height = facts.drawable_height;

          pixel_density = facts.pixel_scale_x;

        }
  | Some { handle = window; _ } -> (
      let op = "Runtime.presentation_facts" in
      match sdl op (Sdl3.Window.pixel_density window) with
      | Ok pixel_density ->
          let facts = value.facts in
          Ok
            {
              logical_width = facts.logical_width;
              logical_height = facts.logical_height;
              drawable_width = facts.drawable_width;
              drawable_height = facts.drawable_height;
              pixel_density;
            }
      | Error e -> Error e)

(* Queried once and reused while the target's drawable facts are unchanged;
   a drawable change, resize, or show queries again (so a new display's scale
   is read, never guessed). *)
let presentation_facts (value : t) =
  if value.dead then stale "Runtime.presentation_facts"
  else
    let live = value.facts in
    match value.presentation with
    | Some cached
      when cached.logical_width = live.logical_width
           && cached.logical_height = live.logical_height
           && cached.drawable_width = live.drawable_width
           && cached.drawable_height = live.drawable_height ->
        Ok cached
    | Some _ | None ->
        Result.map
          (fun facts ->
            value.presentation <- Some facts;
            facts)
          (query_presentation value)

let window_call operation call value =
  live_window operation value (fun window -> sdl operation (call window.handle))

let window_changed (value : t) =
  match value.window with
  | Some window when not value.dead -> Change_flag.announce window.changed
  | Some _ | None -> ()

let set_resizable value enabled =
  window_call "Runtime.set_resizable"
    (fun window -> Sdl3.Window.set_resizable window enabled)
    value

let set_window_background value (red, green, blue) =
  window_call "Runtime.set_window_background"
    (fun window -> Sdl3.Window.set_background window ~red ~green ~blue)
    value

let set_relative_mouse value enabled =
  window_call "Runtime.set_relative_mouse"
    (fun window -> Sdl3.Window.set_relative_mouse window enabled)
    value

let set_cursor value (shape : cursor_shape) =
  live_window "Runtime.set_cursor" value (fun window ->
      if window.cursor_shape = Some shape then Ok ()
      else
        let cursor =
          match List.assoc_opt shape window.cursors with
          | Some cursor -> Ok cursor
          | None ->
              let native =
                match shape with
                | `Default -> Sdl3.Cursor.Default
                | `Horizontal_resize -> Sdl3.Cursor.Ew_resize
                | `Vertical_resize -> Sdl3.Cursor.Ns_resize
                | `Text -> Sdl3.Cursor.Text
              in
              Result.map
                (fun cursor ->
                  window.cursors <- (shape, cursor) :: window.cursors;
                  cursor)
                (sdl "Runtime.set_cursor" (Sdl3.Cursor.create native))
        in
        match cursor with
        | Error _ as error -> error
        | Ok cursor -> (
            match sdl "Runtime.set_cursor" (Sdl3.Cursor.set cursor) with
            | Error _ as error -> error
            | Ok () ->
                window.cursor_shape <- Some shape;
                Ok ()))

(* Text input runs only while a text field has focus: a focused region starts
   it (the input method and on-screen keyboard then follow that region), no
   region stops it. A frame with the same answer makes no SDL call. *)
let set_text_input value area =
  let operation = "Runtime.set_text_input" in
  live_window operation value (fun window ->
      let ( let* ) = Result.bind in
      match area with
      | None ->
          if not window.text_input then Ok ()
          else
            let* () = sdl operation (Sdl3.Text_input.stop window.handle) in
            window.text_input <- false;
            Ok ()
      | Some ((x, y, width, height), cursor) ->
          let* () =
            if window.text_input then Ok ()
            else
              let* () = sdl operation (Sdl3.Text_input.start window.handle) in
              window.text_input <- true;
              Ok ()
          in
          sdl operation
            (Sdl3.Text_input.set_area window.handle { Sdl3.x; y; width; height } ~cursor))

let show (value : t) =
  live_window "Runtime.show" value (fun window ->
      match sdl "Runtime.show" (reveal window.handle) with
      | Error _ as error -> error
      | Ok () ->
          Change_flag.announce window.changed;
          let synced = sync_facts value in
          if Result.is_ok synced then value.presentation <- None;
          synced)

type dialog_kind = Open_file | Open_files | Save_file | Open_folder
type dialog_filter = { name : string; pattern : string }

(* Opens a native file dialog and returns at once with its id; the outcome is
   a [Input.File_dialog] carrying the same id. *)
let show_dialog value ?(filters = []) ?default_location kind =
  window_call "Runtime.show_dialog"
    (fun window ->
      Sdl3.Dialog.show window
        ~filters:(List.map (fun { name; pattern } -> { Sdl3.Dialog.name; pattern }) filters)
        ?default_location
        (match kind with
         | Open_file -> Sdl3.Dialog.Open_file
         | Open_files -> Open_files
         | Save_file -> Save_file
         | Open_folder -> Open_folder))
    value

(* Shown, and neither minimized nor covered. SDL announces the start of
   occlusion and not its end, so a caller that saw the window hidden asks this
   once per frame to learn when it is back. *)
let visible value =
  live_window "Runtime.visible" value (fun window ->
      Result.map
        (fun (state : Sdl3.Window.state) ->
          not (state.hidden || state.minimized || state.occluded))
        (sdl "Runtime.visible" (Sdl3.Window.state window.handle)))

let destroy (value : t) =
  if value.dead then Ok ()
  else (
    value.dead <- true;
    let failure = ref None in
    let record = function
      | Ok () -> ()
      | Error error -> if !failure = None then failure := Some error
    in
    Option.iter
      (fun window ->
        if window.text_input then
          record (sdl "Runtime.destroy" (Sdl3.Text_input.stop window.handle));
        List.iter
          (fun (_, cursor) -> record (sdl "Runtime.destroy" (Sdl3.Cursor.destroy cursor)))
          window.cursors;
        window.cursors <- [];
        record (sdl "Runtime.destroy" (Sdl3.Metal_view.destroy window.view));
        record (sdl "Runtime.destroy" (Sdl3.Window.destroy window.handle));
        record (sdl "Runtime.destroy" (Sdl3.Init.quit_subsystems [ Sdl3.Init.Video ])))
      value.window;
    match !failure with None -> Ok () | Some error -> Error error)

module Private = struct
  module Change_flag = Change_flag
  let is_window (value : t) = Option.is_some value.window
  let sync_facts = sync_facts
  let surface_configuration (value : t) =
    let layer = match value.window with
      | None -> Ok None
      | Some window -> Result.map Option.some (sdl "Runtime.surface_configuration"
          (Sdl3.Metal_view.layer window.view)) in
    Result.map (fun layer ->
      let facts = value.facts in
      let vsync = Option.fold ~none:false ~some:(fun window -> window.vsync) value.window in
      { Ogpu.Surface.logical_width=facts.logical_width;
        logical_height=facts.logical_height;
        physical_width=facts.drawable_width;
        physical_height=facts.drawable_height;
        format=Ogpu.Surface.Bgra8_unorm;
        present_mode=(if vsync then Ogpu.Surface.Fifo else Ogpu.Surface.Immediate);
        max_acquired=2; layer }) layer
  let window_handle (value : t) = Option.map (fun window -> window.handle) value.window
end
