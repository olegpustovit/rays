(** The PXUI design kit: palette and typography shared by every PXUI host. *)

type t = {
  panel : Rays_math.Color.t;
  foreground : Rays_math.Color.t;
  control : Rays_math.Color.t;
  input : Rays_math.Color.t;
  track : Rays_math.Color.t;
  accent : Rays_math.Color.t;
  ink_ground : Rays_math.Color.t option;
      (** the ground the inks blend against when it is not [panel] (a window sheet paints [input] as
          [panel] and keeps the inks of the ground); [None] means [panel] *)
}

val default : t

val font_size : int
(** Type sizes in logical points: body (the default kit text), the upper-case label, a window title
    or viewport readout, and the selected thing's name (once per panel). *)

val label_size : int
val title_size : int
val display_size : int

val ink_2 : t -> Rays_math.Color.t
(** Derived kit colours. Text: the foreground, [ink_2] for labels and secondary text (4.8:1 on the
    panel) and [ink_3] for disabled text, placeholders and keys. [muted] is [ink_2]. *)

val ink_3 : t -> Rays_math.Color.t
val muted : t -> Rays_math.Color.t

val border : t -> Rays_math.Color.t
(** Lines, the foreground at 30, 15 and 8 percent: [border] (line-3) for the one outlined button and
    switches, [edge] (line-2) between panes and under fields, [faint_border] (line-1) inside a list.
*)

val edge : t -> Rays_math.Color.t
val faint_border : t -> Rays_math.Color.t

val hover_fill : t -> Rays_math.Color.t
(** Fills: hover, pressed, and the accent wash of a text selection or a dock target. The current row
    and an active button are [control]. *)

val pressed_fill : t -> Rays_math.Color.t
val tint : t -> Rays_math.Color.t
val invalid : Rays_math.Color.t

type ports = {
  geometry : Rays_math.Color.t;
  float : Rays_math.Color.t;
  int : Rays_math.Color.t;
  vec3 : Rays_math.Color.t;
  bool : Rays_math.Color.t;
  compound : Rays_math.Color.t;
  output : Rays_math.Color.t;
  hint : Rays_math.Color.t;
  text : Rays_math.Color.t;
  fn : Rays_math.Color.t;
  record : Rays_math.Color.t;
}

val ports : t -> ports
(** [text], [fn] (diamond socket) and [record] colour the workspace types; a list draws its element
    colour on a stacked socket. *)

type zone = { fill : Rays_math.Color.t; edge : Rays_math.Color.t; dashed : bool }
(** Zone tokens of the graph pane: [fill] tints the region, [edge] is its border, [dashed] draws the
    border as dashes (functions and [let*] scopes are hollow, a [fn] zone keeps a faint tint). *)

val zone_for : t -> zone
val zone_fold : t -> zone
val zone_sum : t -> zone
val zone_fn : t -> zone
val zone_let : t -> zone
val zone_branch : t -> taken:bool -> zone

val dark : t -> bool
(** The panel is a dark colour. *)
