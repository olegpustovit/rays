type t = {
  panel : Rays_math.Color.t;
  foreground : Rays_math.Color.t;
  control : Rays_math.Color.t;
  input : Rays_math.Color.t;
  track : Rays_math.Color.t;
  accent : Rays_math.Color.t;
  ink_ground : Rays_math.Color.t option;
}

let default =
  {
    panel = Rays_math.Color.hex_exn "#f4f5f0";
    foreground = Rays_math.Color.hex_exn "#222b2b";
    control = Rays_math.Color.hex_exn "#dce3de";
    input = Rays_math.Color.hex_exn "#ffffff";
    track = Rays_math.Color.hex_exn "#e3e8e4";
    accent = Rays_math.Color.hex_exn "#f0481f";
    ink_ground = None;
  }

(* Kit rev 3 (specification/pxui.md): four type sizes, three text inks, three line strengths.
   Separation comes from space first, a hairline second, a rectangle last; nothing is filled
   with ink. *)
let font_size = 13 (* body: values, names, code, menu rows *)
let label_size = 11 (* upper-case captions, keys, units *)
let title_size = 20
let display_size = 40

(* the inks are blended against the ground, which a sheet (a window) keeps when it paints its own *)
let ground theme = Option.value theme.ink_ground ~default:theme.panel

let ink_2 theme =
  Rays_math.Color.blend theme.foreground (ground theme) ~pct:0.32 (* labels, secondary *)

let ink_3 theme =
  Rays_math.Color.blend theme.foreground (ground theme) ~pct:0.521 (* disabled, placeholder, keys *)

let muted = ink_2

let border theme =
  Rays_math.Color.with_alpha theme.foreground 77 (* line-3: the primary button, switch *)

let edge theme =
  Rays_math.Color.with_alpha theme.foreground 38 (* line-2: panes, cards, windows, fields *)

let faint_border theme = Rays_math.Color.with_alpha theme.foreground 20 (* line-1: inside a list *)
let hover_fill theme = Rays_math.Color.blend theme.input theme.accent ~pct:0.11
let pressed_fill theme = Rays_math.Color.blend theme.control theme.accent ~pct:0.18
let tint theme = Rays_math.Color.with_alpha theme.accent 41 (* selection in text, dock targets *)
let invalid = Rays_math.Color.hex_exn "#c2255c"

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

(* the port colours, light and dark, parsed once: a canvas asks for them per port every frame *)
let port_colors =
  let pair light night = (Rays_math.Color.hex_exn light, Rays_math.Color.hex_exn night) in
  [|
    pair "#6b50ae" "#a98cf5";
    pair "#285f77" "#72b3cf";
    pair "#3b7d4e" "#74c28e";
    pair "#b0435f" "#f08aa3";
    pair "#a3407a" "#e88ac0";
    pair "#4f6a5f" "#9cc2b2";
  |]

let port_hint = Rays_math.Color.hex_exn "#f5cf4f"

let ports theme =
  let dark = theme.panel.Rays_math.Color.r + theme.panel.g + theme.panel.b < 384 in
  let color i =
    let light, night = port_colors.(i) in
    if dark then night else light
  in
  let vec3 = color 0 in
  {
    geometry = theme.accent;
    float = color 1;
    int = color 2;
    vec3;
    bool = color 3;
    compound = vec3;
    output = theme.foreground;
    hint = port_hint;
    text = muted theme;
    fn = color 4;
    record = color 5;
  }

(* Graph zones: a tinted rectangle with a border; [dashed] draws the
   border as dashes, and a hollow zone has no fill worth the name.  The
   colours are the study's [--z-*] tokens. *)
type zone = { fill : Rays_math.Color.t; edge : Rays_math.Color.t; dashed : bool }

let dark theme = theme.panel.Rays_math.Color.r + theme.panel.g + theme.panel.b < 384

let zone theme ~light ~dark:night ~dashed =
  let r, g, b, fill, edge = if dark theme then night else light in
  { fill = Rays_math.Color.rgba r g b fill; edge = Rays_math.Color.rgba r g b edge; dashed }

let zone_for theme =
  zone theme ~dashed:false ~light:(240, 72, 31, 15, 92) ~dark:(255, 107, 69, 16, 100)

let zone_fold theme =
  zone theme ~dashed:false ~light:(107, 80, 174, 19, 128) ~dark:(169, 140, 245, 20, 128)

let zone_sum theme =
  zone theme ~dashed:false ~light:(40, 95, 119, 20, 128) ~dark:(114, 179, 207, 20, 128)

let zone_fn theme =
  zone theme ~dashed:true ~light:(163, 64, 122, 9, 140) ~dark:(232, 138, 192, 10, 128)

let zone_let theme = zone theme ~dashed:true ~light:(34, 43, 43, 0, 87) ~dark:(225, 231, 227, 0, 77)

let zone_branch theme ~taken =
  zone theme ~dashed:false
    ~light:(176, 104, 15, (if taken then 26 else 5), if taken then 140 else 70)
    ~dark:(240, 175, 80, (if taken then 30 else 6), if taken then 150 else 80)
