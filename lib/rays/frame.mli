(** Immutable facts about one sketch frame. *)

type t = {
  input : Input.t;
  (** Canonical logical input and frame facts. *)
  pixel_scale : float * float;
  (** Backing pixels per logical point on each axis. *)
  fps : float;
  mouse_delta : float * float;
  (** Accumulated logical pointer movement for this frame. *)
}

val mouse_down : Input.mouse_button -> t -> bool
val has_event : (Input.event -> bool) -> t -> bool
