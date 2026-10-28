
type numeric_value =
  | Scalar of float
  | Vec2 of Rays_math.Vec2.t
  | Vec3 of Rays_math.Vec3.t
  | Vec4 of float * float * float * float

type random_operation =
  | Random_set
  | Random_add
  | Random_minimum
  | Random_maximum
  | Random_multiply

type noise_kind = Noise_float | Noise_vector | Noise_quaternion

type noise_location =
  | Noise_position
  | Noise_element_number
  | Noise_attribute of string

type noise_range =
  | Noise_positive
  | Noise_zero_centered
  | Noise_min_max of numeric_value * numeric_value

type noise_operation =
  | Noise_set_initial
  | Noise_set
  | Noise_add
  | Noise_subtract
  | Noise_multiply
  | Noise_minimum
  | Noise_maximum

type random_selection =
  | Random_points of Group.t
  | Random_vertices of Group.t
  | Random_primitives of Group.t
  | Random_edges of Edge_group.t

type random_distribution =
  | Random_constant of numeric_value
  | Random_two_values of {
      a : numeric_value;
      b : numeric_value;
      probability_b : float;
    }
  | Random_uniform of { min : numeric_value; max : numeric_value }
  | Random_uniform_discrete of {
      min : numeric_value;
      max : numeric_value;
      step : numeric_value;
    }
  | Random_normal of { middle : numeric_value; scale : numeric_value }
  | Random_exponential of { median : numeric_value }
  | Random_log_normal of { median : numeric_value; stddev : numeric_value }
  | Random_cauchy of { median : numeric_value; scale : numeric_value }
  | Random_direction of {
      direction : numeric_value;
      cone_angle : float;
    }
  | Random_inside_sphere of { dimensions : int }
  | Random_inside_sphere_cone of {
      direction : numeric_value;
      cone_angle : float;
    }
  | Random_custom_ramp of {
      ramp : (float * float) list;
      fit_min : numeric_value;
      fit_max : numeric_value;
    }
  | Random_custom_discrete of (numeric_value * float) list
  | Random_custom_discrete_text of (string * float) list

type remap_input = Remap_explicit of {
  min : numeric_value;
  max : numeric_value;
} | Remap_auto

type remap_policy = Remap_clamp | Remap_cycle | Remap_extrapolate

type storage_kind = Position | Float | Float2 | Float3 | Float4

type direction_plan = {
  axis : float array;
  householder : float array;
  householder_norm2 : float;
  cap_angle : float;
  bias : float;
}

type prepared_distribution =
  | Prepared_constant of float array
  | Prepared_two_values of {
      a : float array;
      b : float array;
      probability_b : float;
    }
  | Prepared_uniform of { min : float array; span : float array }
  | Prepared_uniform_discrete of {
      min : float array;
      max : float array;
      step : float array;
      choices : float array;
    }
  | Prepared_normal of { middle : float array; scale : float array }
  | Prepared_exponential of { median : float array }
  | Prepared_log_normal of { median : float array; sigma : float array }
  | Prepared_cauchy of {
      median : float array;
      scale : float;
      direction : direction_plan option;
    }
  | Prepared_direction of direction_plan
  | Prepared_inside_sphere of { dimensions : int }
  | Prepared_inside_sphere_cone of direction_plan
  | Prepared_custom_ramp of {
      knots : (float * float) array;
      fit_min : float array;
      fit_span : float array;
    }
  | Prepared_custom_discrete of {
      values : float array array;
      cumulative : float array;
      total : float;
      last_positive : int;
    }
  | Prepared_custom_discrete_text of {
      values : string array;
      cumulative : float array;
      total : float;
      last_positive : int;
    }

type sample_limits =
  | No_limits
  | Minimum_only of float array
  | Maximum_only of float array
  | Minimum_and_maximum of float array * float array

(* Peter J. Acklam's inverse-normal rational approximation. The tails remain
   deliberately infinite at exact quantiles zero and one, so the operation's
   ordinary finite-output validation reports an unusable fraction. *)

type noise_coordinates = { nx : float array; ny : float array; nz : float array }
