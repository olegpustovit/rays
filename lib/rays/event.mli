(** Input events delivered, in order, through [Frame.events]. *)

(** Where a trackpad scroll gesture is: the fingers touched, moved, lifted, or
    the system is sending its own inertia after the lift. *)
type scroll_phase = Touched | Moved | Lifted | Momentum

type t =
  | KeyPressed of Input.key
  | KeyReleased of Input.key
  | MouseMoved of (float * float)                            (* logical-point position *)
  | MousePressed of Input.mouse_button * (float * float)    (* button and position *)
  | MouseReleased of Input.mouse_button * (float * float)   (* button and position *)
  | PointerCancelled of Input.mouse_button                  (* OS cancelled the pointer *)
  | MouseScrolled of (float * float)                         (* scroll delta x,y; trackpads send fractions *)
  | TextInput of string
  | TextEditing of { text : string; start : int; length : int }
  | FileDropped of string                                   (* absolute path; not read *)
  | FileDragMoved of (float * float)                         (* files from the OS are over the window at this point *)
  | FileDragEnded                                            (* the drag left the window or finished *)
  | MousePinched of float                                    (* trackpad pinch: zoom factor since the last event; above 1 zooms in *)
  | FileDialog of { id : int; result : (string list, string) result }
      (** a native file dialog from {!Sketch.show_file_dialog} finished: the
          chosen paths (empty when the user cancelled) or what went wrong *)
  | WindowResized of (int * int)                             (* new width and height *)
  | WindowFocusLost
  | WindowFocusGained                                        (* the window is the key window again *)
  | WindowClosed                                             (* user attempted to close *)
  | TrackpadScrolled of { delta : float * float; phase : scroll_phase; time : float }
      (** macOS: a trackpad (or Magic Mouse) scroll as the system phases it.
          [delta] is in points with [MouseScrolled]'s directions and [time] is
          the event's own clock in seconds, for velocities. Every one that moves
          is also delivered as a [MouseScrolled] of a tenth of [delta] in the same
          frame, so code that reads only [MouseScrolled] sees what it always
          did; code that reads these ignores that frame's [MouseScrolled]. *)

module Private : sig
  val key_of_runtime : Runtime_input.key -> Input.key
  (** The one move from the binding's key to Rays's; exposed for tests. *)

  val keys_before : previous:Input.key list -> held:Input.key list ->
    t list -> Input.key list
  (** Initial held keys: changed keys and focus cancellation take [previous];
      unchanged keys use the final [held] snapshot. These hooks are internal
      input plumbing; their output can be advanced through ordered events. *)

  val keys_after : Input.key list -> t -> Input.key list
end
