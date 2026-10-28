type numeric_value =
  | Scalar of float
  | Vec2 of Rays_math.Vec2.t
  | Vec3 of Rays_math.Vec3.t
  | Vec4 of float * float * float * float

type noise_location = Noise_position | Noise_element_number | Noise_attribute of string

type noise_range =
  | Noise_positive
  | Noise_zero_centered
  | Noise_min_max of numeric_value * numeric_value

type random_distribution =
  | Random_constant of numeric_value
  | Random_two_values of { a : numeric_value; b : numeric_value; probability_b : float }
  | Random_uniform of { min : numeric_value; max : numeric_value }
  | Random_uniform_discrete of { min : numeric_value; max : numeric_value; step : numeric_value }
  | Random_normal of { middle : numeric_value; scale : numeric_value }
  | Random_exponential of { median : numeric_value }
  | Random_log_normal of { median : numeric_value; stddev : numeric_value }
  | Random_cauchy of { median : numeric_value; scale : numeric_value }
  | Random_direction of { direction : numeric_value; cone_angle : float }
  | Random_inside_sphere of { dimensions : int }
  | Random_inside_sphere_cone of { direction : numeric_value; cone_angle : float }
  | Random_custom_ramp of {
      ramp : (float * float) list;
      fit_min : numeric_value;
      fit_max : numeric_value;
    }
  | Random_custom_discrete of (numeric_value * float) list
  | Random_custom_discrete_text of (string * float) list
