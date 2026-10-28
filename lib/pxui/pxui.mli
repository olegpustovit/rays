(** PXUI: Rays's creative-coding UI kit.

    {!Ui} is the immediate-mode core: build the interface every frame inside [Ui.frame], read widget
    values straight back into the sketch model, and compose [Ui.scene] into the view. {!Theme} is
    the design kit's palette and typography; the camera modules are reusable panels. *)

type theme = Theme.t = {
  panel : Rays_math.Color.t;
  foreground : Rays_math.Color.t;
  control : Rays_math.Color.t;
  input : Rays_math.Color.t;
  track : Rays_math.Color.t;
  accent : Rays_math.Color.t;
  ink_ground : Rays_math.Color.t option;
}

val default_theme : theme

module Theme = Theme
module Ui = Ui
