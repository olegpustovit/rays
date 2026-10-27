type theme = Theme.t = {
  panel : Rays_math.Color.t;
  foreground : Rays_math.Color.t;
  control : Rays_math.Color.t;
  input : Rays_math.Color.t;
  track : Rays_math.Color.t;
  accent : Rays_math.Color.t;
  ink_ground : Rays_math.Color.t option;
}

let default_theme = Theme.default

module Theme = Theme
module Ui = Ui
