module Store = Store
module Panels = Panels

module History = struct
  type merge =
    | Step
    | Gesture of string
    | Burst of { key : string; at : float; window : float }
    | Repair

  (* Each entry carries the label of the edit that produced it. *)
  type 'a t = {
    past : ('a * string) list;
    present : 'a;
    label : string;
    future : ('a * string) list;
    capacity : int;
    depth : int;
    merge : merge option;
  }

  let create ?(capacity = 128) present =
    if capacity < 1 then invalid_arg "Editor_core.History.create: capacity must be positive";
    { past = []; present; label = ""; future = []; capacity; depth = 0; merge = None }

  let present t = t.present
  let label t = t.label
  let redo_label t = match t.future with (_, label) :: _ -> Some label | [] -> None
  let can_undo t = t.past <> []
  let can_redo t = t.future <> []
  let depth t = t.depth

  (* ponytail: the bound is enforced by walking the list, O(capacity) per
     commit; switch to a deque if capacities grow past a few hundred. *)
  let commit value label t =
    if value == t.present then t
    else
      let past = (t.present, t.label) :: t.past in
      let past, depth =
        if t.depth < t.capacity then (past, t.depth + 1) else (List.take t.capacity past, t.capacity)
      in
      { t with past; present = value; label; future = []; depth; merge = None }

  (* the present replaced in place: a continued gesture, or a repair that is no edit of the
     user's (camera bookkeeping), which leaves what redo would restore *)
  let amend value t = if value == t.present then t else { t with present = value }

  let record ?(merge = Step) ?(label = "Edit") value t =
    let continuation =
      match (merge, t.merge) with
      | Repair, _ -> true
      | Gesture id, Some (Gesture previous) -> id = previous
      | Burst { key; at; window }, Some (Burst previous) ->
          String.equal key previous.key && at >= previous.at && at -. previous.at < window
      | _ -> false
    in
    let t = if continuation then amend value t else commit value label t in
    { t with merge = (if merge = Repair then t.merge else Some merge) }

  let seal ?(gesture_only = false) t =
    match t.merge with
    | None -> t
    | Some (Gesture _) -> { t with merge = None }
    | Some _ when not gesture_only -> { t with merge = None }
    | Some _ -> t

  let undo t =
    match t.past with
    | [] -> None
    | (previous, label) :: past ->
        Some
          {
            t with
            past;
            present = previous;
            label;
            future = (t.present, t.label) :: t.future;
            depth = t.depth - 1;
            merge = None;
          }

  let redo t =
    match t.future with
    | [] -> None
    | (next, label) :: future ->
        Some
          {
            t with
            past = (t.present, t.label) :: t.past;
            present = next;
            label;
            future;
            depth = t.depth + 1;
            merge = None;
          }
end

module Keymap = struct
  type trigger = Leader of string | Chord of Input.key * Input.key list

  (* What a symbol key types with Shift held (the US layout): the key event names the key, a
     binding names the symbol. *)
  let shifted = function
    | '/' -> Some '?'
    | '=' -> Some '+'
    | '-' -> Some '_'
    | ';' -> Some ':'
    | '\'' -> Some '"'
    | ',' -> Some '<'
    | '.' -> Some '>'
    | '[' -> Some '{'
    | ']' -> Some '}'
    | '\\' -> Some '|'
    | '`' -> Some '~'
    | '1' -> Some '!'
    | '2' -> Some '@'
    | '3' -> Some '#'
    | '4' -> Some '$'
    | '5' -> Some '%'
    | '6' -> Some '^'
    | '7' -> Some '&'
    | '8' -> Some '*'
    | '9' -> Some '('
    | '0' -> Some ')'
    | _ -> None

  let label = function
    | Leader sequence ->
        (* the sheets write each key apart: / o v *)
        "/ "
        ^ String.concat " "
            (List.init (String.length sequence) (fun i -> String.make 1 sequence.[i]))
    | Chord (key, modifiers) ->
        let open Input in
        let key, modifiers =
          match key with
          | KeyChar c when List.mem Shift modifiers && shifted c <> None ->
              (KeyChar (Option.get (shifted c)), List.filter (( <> ) Shift) modifiers)
          | _ -> (key, modifiers)
        in
        (* the sheets' vocabulary: modifier glyphs before the key with no dash, a letter under a
           modifier in capitals (⌘G, ⇧D), ↵ ⇥ ⌫ and esc *)
        let key =
          match key with
          | KeyChar c ->
              String.make 1
                (if modifiers = [] then Char.lowercase_ascii c else Char.uppercase_ascii c)
          | ArrowUp -> "↑"
          | ArrowDown -> "↓"
          | ArrowLeft -> "←"
          | ArrowRight -> "→"
          | Space -> "Space"
          | Enter -> "↵"
          | Escape -> "esc"
          | Backspace -> "⌫"
          | Tab -> "⇥"
          | Home -> "Home"
          | End -> "End"
          | PageUp -> "PgUp"
          | PageDown -> "PgDn"
          | Insert -> "Ins"
          | Delete -> "⌦"
          | Shift -> "⇧"
          | Ctrl -> "⌃"
          | Alt -> "⌥"
          | Meta -> "⌘"
          | F1 -> "F1"
          | F2 -> "F2"
          | F3 -> "F3"
          | F4 -> "F4"
          | F5 -> "F5"
          | F6 -> "F6"
          | F7 -> "F7"
          | F8 -> "F8"
          | F9 -> "F9"
          | F10 -> "F10"
          | F11 -> "F11"
          | F12 -> "F12"
          | Unknown code -> "Key " ^ string_of_int code
        in
        List.fold_left
          (fun label (modifier, name) ->
            if List.mem modifier modifiers then label ^ name else label)
          ""
          [ (Meta, "⌘"); (Ctrl, "⌃"); (Alt, "⌥"); (Shift, "⇧") ]
        ^ key
end

module Guide_context = struct
  type t = Canvas | Node | Multi | Hints | Leader | Search | List | Text

  let name = function
    | Canvas -> "Canvas"
    | Node -> "Node"
    | Multi -> "Selection"
    | Hints -> "Hints"
    | Leader -> "Leader"
    | Search -> "Search"
    | List -> "List"
    | Text -> "Text"
end

module Command = struct
  type ('scope, 'action) t = {
    id : string;
    label : string;
    trigger : Keymap.trigger option;
    scope : 'scope option;
    guide : Guide_context.t list;
    action : 'action;
  }

  let make ?trigger ?scope ?(guide = []) ~id ~label action =
    { id; label; trigger; scope; guide; action }

  let for_guide commands ~focus ~context =
    List.filter
      (fun command ->
        command.trigger <> None
        && (command.scope = None || command.scope = Some focus)
        && List.mem context command.guide)
      commands
end

module Router = struct
  open Command
  open Keymap

  type state = Idle | Pending of string (* the leader keys typed so far *)

  let visible commands focus =
    List.filter (fun command -> command.scope = None || command.scope = Some focus) commands

  let same_key a b =
    match (a, b) with
    | Input.KeyChar a, Input.KeyChar b -> Char.lowercase_ascii a = Char.lowercase_ascii b
    | _ -> a = b

  let chord commands focus keys key =
    visible commands focus
    |> List.filter_map (fun command ->
        match command.trigger with
        | Some (Chord (bound, modifiers))
          when same_key bound key
               && List.for_all (fun modifier -> List.mem modifier keys) modifiers
               && (bound <> Input.Tab
                  || List.for_all
                       (fun modifier -> List.mem modifier keys = List.mem modifier modifiers)
                       [ Input.Shift; Input.Alt ])
               && List.for_all
                    (fun modifier -> (not (List.mem modifier keys)) || List.mem modifier modifiers)
                    [ Input.Meta; Input.Ctrl ] ->
            Some (List.length modifiers, command)
        | _ -> None)
    |> List.fold_left
         (fun best candidate ->
           match best with
           | Some (count, _) when count >= fst candidate -> best
           | _ -> Some candidate)
         None
    |> Option.map snd

  let fly (frame : Rays.Frame.t) =
    let open Rays in
    let exits =
      List.exists
        (function
          | Input.Key_pressed (Input.Escape | Input.Space | Input.KeyChar '/') | Input.Focus_lost ->
              true
          | _ -> false)
        frame.input.events
    in
    ( exits,
      {
        frame with
        input =
          {
            frame.input with
            events =
              List.filter
                (function
                  | Input.Key_pressed (Input.KeyChar '/') | Input.Focus_lost -> true
                  | Input.Key_pressed _ | Input.Key_released _ | Input.Text_input _
                  | Input.Text_editing _ ->
                      false
                  | _ -> true)
                frame.input.events;
          };
      } )

  let modifier = function Input.Shift | Input.Ctrl | Input.Alt | Input.Meta -> true | _ -> false

  (* A text field owns the keyboard except the Command/Ctrl chords it has no use for (its own are
     select all, copy, cut, paste, undo and redo): Command-S saves with the caret in a field.
     Only commands of every pane run; a pane's own keys wait for the field to close. *)
  let text_chords keymap ~focus ~previous_keys ~(frame : Rays.Frame.t) =
    let open Rays in
    let global = List.filter (fun command -> command.scope = None) keymap in
    let owned = function
      | Input.KeyChar c -> List.mem (Char.lowercase_ascii c) [ 'a'; 'c'; 'v'; 'x'; 'z'; 'y' ]
      | Input.F12 -> false
      | _ -> true
    in
    let modifiers =
      ref (Input.keys_before ~previous:previous_keys ~held:frame.input.keys frame.input.events)
    in
    let actions, passed =
      List.fold_left
        (fun (actions, passed) event ->
          modifiers := Input.keys_after !modifiers event;
          match event with
          | Input.Key_pressed key
            when (not (owned key))
                 && (List.mem Input.Meta !modifiers || List.mem Input.Ctrl !modifiers
                   || key = Input.F12) -> (
              match chord global focus !modifiers key with
              | Some command -> (command :: actions, passed)
              | None -> (actions, event :: passed))
          | _ -> (actions, event :: passed))
        ([], []) frame.input.events
    in
    (List.rev actions, { frame with input = { frame.input with events = List.rev passed } })

  let step ?(previous_keys = []) keymap ~focus ~text_focus ~(frame : Rays.Frame.t) state =
    let open Rays in
    if text_focus then
      let actions, frame = text_chords keymap ~focus ~previous_keys ~frame in
      (Idle, actions, frame)
    else
      let modifiers =
        ref (Input.keys_before ~previous:previous_keys ~held:frame.input.keys frame.input.events)
      in
      let traversing = ref false in
      let state, actions, passed =
        List.fold_left
          (fun (state, actions, passed) event ->
            modifiers := Input.keys_after !modifiers event;
            let command = List.mem Input.Meta !modifiers || List.mem Input.Ctrl !modifiers in
            if !traversing then (Idle, actions, event :: passed)
            else
              match (state, event) with
              | _, Input.Key_pressed Input.Tab when not command -> (
                  traversing := true;
                  match chord keymap focus !modifiers Input.Tab with
                  | None -> (Idle, actions, event :: passed)
                  | Some action -> (Idle, action :: actions, passed))
              | Idle, Input.Key_pressed (Input.KeyChar '/')
                when (not text_focus) && (not command) && not (List.mem Input.Shift !modifiers) ->
                  (Pending "", actions, passed)
              | Idle, Input.Key_pressed key when not text_focus -> (
                  (* Shift and a symbol key is the symbol it types, when a chord is bound to that *)
                  let typed =
                    match key with
                    | Input.KeyChar c when List.mem Input.Shift !modifiers ->
                        Option.bind (shifted c) (fun s ->
                            chord keymap focus !modifiers (Input.KeyChar s))
                    | _ -> None
                  in
                  match
                    match typed with Some _ -> typed | None -> chord keymap focus !modifiers key
                  with
                  | Some action -> (Idle, action :: actions, passed)
                  | None -> (Idle, actions, event :: passed))
              | Idle, Input.Text_input _ when actions <> [] -> (Idle, actions, passed)
              | Idle, _ -> (Idle, actions, event :: passed)
              | Pending _, Input.Key_pressed key when modifier key -> (state, actions, passed)
              | Pending prefix, Input.Key_pressed (Input.KeyChar character) -> (
                  (* An exact sequence runs; a proper prefix opens the next page. *)
                  (* / then Shift-/ is the sequence "?", as the sheets write it *)
                  let character =
                    if List.mem Input.Shift !modifiers then
                      Option.value (shifted character) ~default:character
                    else character
                  in
                  let typed = prefix ^ String.make 1 (Char.lowercase_ascii character) in
                  let commands = visible keymap focus in
                  match
                    List.find_opt (fun command -> command.trigger = Some (Leader typed)) commands
                  with
                  | Some command -> (Idle, command :: actions, passed)
                  | None
                    when List.exists
                           (fun command ->
                             match command.trigger with
                             | Some (Leader sequence) -> String.starts_with ~prefix:typed sequence
                             | _ -> false)
                           commands ->
                      (Pending typed, actions, passed)
                  | None -> (Idle, actions, passed))
              | Pending _, (Input.Text_input _ | Input.Text_editing _) -> (state, actions, passed)
              | Pending _, (Input.Key_pressed _ | Input.Pointer_pressed _) -> (Idle, actions, passed)
              | Pending _, Input.Focus_lost -> (Idle, actions, event :: passed)
              | Pending _, _ -> (state, actions, event :: passed))
          (state, [], []) frame.input.events
      in
      (state, List.rev actions, { frame with input = { frame.input with events = List.rev passed } })
end
