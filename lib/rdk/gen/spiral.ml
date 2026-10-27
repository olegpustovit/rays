open Rays_math

exception Invalid_spiral of string

type extent =
  | Spiral_turns of { turns : float; height : float }
  | Spiral_height_pitch of { height : float; pitch : float }

type radius =
  | Spiral_archimedean_change of {
      start_radius : float; increase_per_turn : float;
    }
  | Spiral_archimedean_end of { start_radius : float; end_radius : float }
  | Spiral_logarithmic_change of {
      start_radius : float; scale_per_turn : float;
    }
  | Spiral_logarithmic_end of { start_radius : float; end_radius : float }

type direction = Spiral_counterclockwise | Spiral_clockwise
type divisions =
  | Spiral_divisions_per_curve of int
  | Spiral_divisions_per_turn of int
type orientation = Spiral_x | Spiral_y | Spiral_z | Spiral_axis of Rays_math.Vec3.t
type rotation_order =
  | Spiral_xyz | Spiral_xzy | Spiral_yxz
  | Spiral_yzx | Spiral_zxy | Spiral_zyx

type ramp = {
  positions : float array;
  values : float array;
  slopes : float array;
}

type profile = {
  turns : float;
  height : float;
  angular_rate : float;
  radius : radius;
  radius_scale : float;
  height_ramp : ramp option;
  radius_ramp : ramp option;
  uniform_scale : float;
}

let get_ok = function Ok value -> value | Error message -> invalid_arg message

let compile_ramp label source =
  match source with
  | [] -> Ok None
  | [position, value] ->
      if not (Float.is_finite position && Float.is_finite value && position >= 0. && position <= 1.)
      then Error ("Rdk.Spiral.spiral: " ^ label
          ^ " ramp point must be finite and inside [0, 1]")
      else Ok (Some { positions = [|position|]; values = [|value|]; slopes = [||] })
  | _ ->
      let count = List.length source in
      let positions = Array.make count 0. and values = Array.make count 0. in
      let valid = ref true and previous = ref (-1.) in
      List.iteri (fun index (position, value) ->
        if not (Float.is_finite position && Float.is_finite value && position >= 0.
            && position <= 1. && position > !previous) then valid := false;
        positions.(index) <- position; values.(index) <- value;
        previous := position) source;
      if not !valid || positions.(0) <> 0. || positions.(count - 1) <> 1. then
        Error ("Rdk.Spiral.spiral: " ^ label
          ^ " ramp positions must be finite, strictly increasing, and span 0 through 1")
      else
        let slopes = Array.init (count - 1) (fun index ->
          (values.(index + 1) -. values.(index))
          /. (positions.(index + 1) -. positions.(index))) in
        Ok (Some { positions; values; slopes })

let[@inline always] ramp_segment ramp t =
  let count = Array.length ramp.positions in
  if count <= 1 then 0
  else if t <= 0. then 0
  else if t >= 1. then count - 2
  else begin
    let low = ref 0 and high = ref (count - 1) in
    while !low + 1 < !high do
      let middle = (!low + !high) lsr 1 in
      if ramp.positions.(middle) <= t then low := middle else high := middle
    done;
    !low
  end

let[@inline always] ramp_value ramp t = match ramp with
  | None -> 1.
  | Some ramp when Array.length ramp.positions = 1 -> ramp.values.(0)
  | Some ramp ->
      let segment = ramp_segment ramp t in
      ramp.values.(segment)
        +. ((t -. ramp.positions.(segment)) *. ramp.slopes.(segment))

let[@inline always] ramp_slope ramp t = match ramp with
  | None -> 0.
  | Some ramp when Array.length ramp.positions = 1 -> 0.
  | Some ramp -> ramp.slopes.(ramp_segment ramp t)

let[@inline always] base_radius profile t = match profile.radius with
  | Spiral_archimedean_change { start_radius; increase_per_turn } ->
      start_radius +. (increase_per_turn *. profile.turns *. t)
  | Spiral_archimedean_end { start_radius; end_radius } ->
      start_radius +. ((end_radius -. start_radius) *. t)
  | Spiral_logarithmic_change { start_radius; scale_per_turn } ->
      start_radius *. exp (log scale_per_turn *. profile.turns *. t)
  | Spiral_logarithmic_end { start_radius; end_radius } ->
      start_radius *. exp (log (end_radius /. start_radius) *. t)

let[@inline always] base_radius_derivative profile t = match profile.radius with
  | Spiral_archimedean_change { increase_per_turn; _ } ->
      increase_per_turn *. profile.turns
  | Spiral_archimedean_end { start_radius; end_radius } ->
      end_radius -. start_radius
  | Spiral_logarithmic_change { scale_per_turn; _ } ->
      base_radius profile t *. log scale_per_turn *. profile.turns
  | Spiral_logarithmic_end { start_radius; end_radius } ->
      base_radius profile t *. log (end_radius /. start_radius)

let[@inline always] final_radius profile t =
  base_radius profile t *. profile.radius_scale *. ramp_value profile.radius_ramp t

let[@inline always] final_height profile t =
  profile.height *. t *. ramp_value profile.height_ramp t

let[@inline always] speed profile t =
  let base = base_radius profile t
  and derivative = base_radius_derivative profile t
  and radius_ramp = ramp_value profile.radius_ramp t
  and radius_slope = ramp_slope profile.radius_ramp t
  and height_ramp = ramp_value profile.height_ramp t
  and height_slope = ramp_slope profile.height_ramp t in
  let radius = base *. profile.radius_scale *. radius_ramp
  and dr = profile.radius_scale
      *. ((derivative *. radius_ramp) +. (base *. radius_slope))
  and dh = profile.height *. (height_ramp +. (t *. height_slope)) in
  profile.uniform_scale *. sqrt ((dr *. dr)
    +. ((radius *. profile.angular_rate) *. (radius *. profile.angular_rate))
    +. (dh *. dh))

let[@inline always] gauss_length profile lower upper =
  if upper <= lower then 0.
  else
    let midpoint = (lower +. upper) *. 0.5
    and half = (upper -. lower) *. 0.5 in
    let n1 = 0.5384693101056831 and n2 = 0.9061798459386640
    and w0 = 0.5688888888888889 and w1 = 0.4786286704993665
    and w2 = 0.2369268850561891 in
    half *. ((w0 *. speed profile midpoint)
      +. (w1 *. (speed profile (midpoint -. (half *. n1))
          +. speed profile (midpoint +. (half *. n1))))
      +. (w2 *. (speed profile (midpoint -. (half *. n2))
          +. speed profile (midpoint +. (half *. n2)))))

let normalize_axis label value =
  if not (Float.is_finite value.Rays_math.Vec3.x && Float.is_finite value.y && Float.is_finite value.z) then
    Error ("Rdk.Spiral.spiral: " ^ label ^ " axis must be finite")
  else
    let scale = max (abs_float value.x)
        (max (abs_float value.y) (abs_float value.z)) in
    if scale = 0. then Error ("Rdk.Spiral.spiral: " ^ label ^ " axis must be non-zero")
    else
      let x = value.x /. scale and y = value.y /. scale
      and z = value.z /. scale in
      let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
      Ok (Rays_math.Vec3.create (x /. length) (y /. length) (z /. length))

let rotation_matrix order rotation =
  let x = Rays_math.Mat4.rotation_x rotation.Rays_math.Vec3.x
  and y = Rays_math.Mat4.rotation_y rotation.y
  and z = Rays_math.Mat4.rotation_z rotation.z in
  match order with
  | Spiral_xyz -> Rays_math.Mat4.mul z (Rays_math.Mat4.mul y x)
  | Spiral_xzy -> Rays_math.Mat4.mul y (Rays_math.Mat4.mul z x)
  | Spiral_yxz -> Rays_math.Mat4.mul z (Rays_math.Mat4.mul x y)
  | Spiral_yzx -> Rays_math.Mat4.mul x (Rays_math.Mat4.mul z y)
  | Spiral_zxy -> Rays_math.Mat4.mul y (Rays_math.Mat4.mul x z)
  | Spiral_zyx -> Rays_math.Mat4.mul x (Rays_math.Mat4.mul y z)

let frame orientation rotation_order (rotation : Rays_math.Vec3.t) =
  let oriented = match orientation with
    | Spiral_x -> Ok (Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_x, Rays_math.Vec3.create 0. 0. (-1.))
    | Spiral_y -> Ok (Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_y, Rays_math.Vec3.unit_z)
    | Spiral_z -> Ok (Rays_math.Vec3.unit_x, Rays_math.Vec3.unit_z, Rays_math.Vec3.create 0. (-1.) 0.)
    | Spiral_axis axis ->
        Result.bind (normalize_axis "central" axis) (fun axis ->
          let ax = abs_float axis.x and ay = abs_float axis.y
          and az = abs_float axis.z in
          let reference = if ax <= ay && ax <= az then Rays_math.Vec3.unit_x
            else if ay <= az then Rays_math.Vec3.unit_y else Rays_math.Vec3.unit_z in
          let projection = Rays_math.Vec3.dot reference axis in
          let radial = Rays_math.Vec3.create
              (reference.x -. (projection *. axis.x))
              (reference.y -. (projection *. axis.y))
              (reference.z -. (projection *. axis.z)) in
          Result.bind (normalize_axis "radial" radial) (fun radial ->
            Result.map (fun tangent -> radial, axis, tangent)
              (normalize_axis "tangent" (Rays_math.Vec3.cross radial axis)))) in
  Result.map (fun (x_axis, y_axis, z_axis) ->
    if rotation.x = 0. && rotation.y = 0. && rotation.z = 0. then
      x_axis, y_axis, z_axis
    else
      let matrix = rotation_matrix rotation_order rotation in
      Rays_math.Mat4.transform_direction matrix x_axis,
      Rays_math.Mat4.transform_direction matrix y_axis,
      Rays_math.Mat4.transform_direction matrix z_axis) oriented

let validate_names names =
  let names = List.filter_map Fun.id names in
  if List.exists (fun name -> String.trim name = "" || String.equal name "P") names
  then Error "Rdk.Spiral.spiral: output attribute names must be non-empty and cannot be P"
  else
    let sorted = List.sort String.compare names in
    let rec duplicate = function
      | left :: (right :: _ as tail) ->
          String.equal left right || duplicate tail
      | _ -> false in
    if duplicate sorted then Error "Rdk.Spiral.spiral: output attribute names must be unique"
    else Ok ()

let[@inline always] write_normalized_quaternion (qx, qy, qz, qw)
    index x y z w =
  let length = sqrt ((x *. x) +. (y *. y) +. (z *. z) +. (w *. w)) in
  qx.(index) <- x /. length; qy.(index) <- y /. length;
  qz.(index) <- z /. length; qw.(index) <- w /. length

let generate ?cancel ?(grain = 16_384)
    ?(extent = Spiral_turns { turns = 3.; height = 2. })
    ?(radius = Spiral_archimedean_change {
      start_radius = 1.; increase_per_turn = 0. })
    ?(height_ramp = []) ?(radius_scale = 1.) ?(radius_ramp = [])
    ?(direction = Spiral_counterclockwise) ?(start_angle = 0.)
    ?(divisions = Spiral_divisions_per_turn 32) ?(uniform_angle = true)
    ?(spiral_count = 1) ?(orientation = Spiral_y) ?(center = Rays_math.Vec3.zero)
    ?(rotation = Rays_math.Vec3.zero) ?(rotation_order = Spiral_xyz)
    ?(uniform_scale = 1.) ?angle_attribute ?x_axis_attribute ?y_axis_attribute
    ?tangent_attribute ?orient_attribute ?distance_attribute () =
  let turns, height, extent_valid = match extent with
    | Spiral_turns { turns; height } -> turns, height,
        Float.is_finite turns && turns > 0. && Float.is_finite height
    | Spiral_height_pitch { height; pitch } ->
        let turns = height /. pitch in
        turns, height, Float.is_finite height && Float.is_finite pitch && pitch <> 0.
          && Float.is_finite turns && turns > 0. in
  let radius_valid, base_end_radius = match radius with
    | Spiral_archimedean_change { start_radius; increase_per_turn } ->
        let end_radius = start_radius +. (increase_per_turn *. turns) in
        Float.is_finite start_radius && start_radius >= 0.
          && Float.is_finite increase_per_turn && Float.is_finite end_radius && end_radius >= 0.,
        end_radius
    | Spiral_archimedean_end { start_radius; end_radius } ->
        Float.is_finite start_radius && start_radius >= 0. && Float.is_finite end_radius
          && end_radius >= 0., end_radius
    | Spiral_logarithmic_change { start_radius; scale_per_turn } ->
        let end_radius = start_radius *. exp (log scale_per_turn *. turns) in
        Float.is_finite start_radius && start_radius > 0. && Float.is_finite scale_per_turn
          && scale_per_turn > 0. && Float.is_finite end_radius && end_radius > 0.,
        end_radius
    | Spiral_logarithmic_end { start_radius; end_radius } ->
        Float.is_finite start_radius && start_radius > 0. && Float.is_finite end_radius
          && end_radius > 0., end_radius in
  let segment_count = match divisions with
    | Spiral_divisions_per_curve count -> if count > 0 then Some count else None
    | Spiral_divisions_per_turn count ->
        if count <= 0 then None
        else
          let segments = ceil (turns *. float_of_int count) in
          if not (Float.is_finite segments) || segments > float_of_int max_int then None
          else Some (max 1 (int_of_float segments)) in
  if grain <= 0 then Error "Rdk.Spiral.spiral: grain must be positive"
  else if not extent_valid then
    Error "Rdk.Spiral.spiral: turns must be finite/positive; height and pitch must be finite with height/pitch positive"
  else if not radius_valid then
    Error "Rdk.Spiral.spiral: radius profile must remain finite and non-negative; logarithmic radii/scales must be positive"
  else if not (Float.is_finite radius_scale && radius_scale > 0.
      && Float.is_finite uniform_scale && uniform_scale > 0. && Float.is_finite start_angle) then
    Error "Rdk.Spiral.spiral: radius scale and uniform scale must be finite/positive and start angle finite"
  else if not (Float.is_finite center.x && Float.is_finite center.y && Float.is_finite center.z
      && Float.is_finite rotation.x && Float.is_finite rotation.y && Float.is_finite rotation.z) then
    Error "Rdk.Spiral.spiral: center and rotation must be finite"
  else if spiral_count <= 0 then Error "Rdk.Spiral.spiral: spiral count must be positive"
  else match segment_count with
    | None -> Error "Rdk.Spiral.spiral: divisions must be positive and output cardinality finite"
    | Some segment_count ->
      Result.bind (validate_names [angle_attribute; x_axis_attribute;
          y_axis_attribute; tangent_attribute; orient_attribute;
          distance_attribute]) (fun () ->
      Result.bind (compile_ramp "height" height_ramp) (fun height_ramp ->
      Result.bind (compile_ramp "radius" radius_ramp) (fun radius_ramp ->
      Result.bind (frame orientation rotation_order rotation)
        (fun (x_axis, y_axis, z_axis) ->
      let point_limit = Sys.max_array_length
      and primitive_limit = min (Sys.max_array_length - 1) Sys.max_string_length in
      if segment_count >= point_limit || spiral_count > primitive_limit then
        Error "Rdk.Spiral.spiral: output cardinality exceeds OCaml array limits"
      else
        let points_per_curve = segment_count + 1 in
        if spiral_count > point_limit / points_per_curve then
          Error "Rdk.Spiral.spiral: output cardinality exceeds OCaml array limits"
        else
          let point_count = spiral_count * points_per_curve in
          let direction_sign = match direction with
            | Spiral_counterclockwise -> 1. | Spiral_clockwise -> -1. in
          let angular_rate = direction_sign *. turns *. 2. *. Float.pi in
          let profile = { turns; height; angular_rate; radius; radius_scale;
            height_ramp; radius_ramp; uniform_scale } in
          let _ = base_end_radius in
          let t_values = Array.make points_per_curve 0. in
          if uniform_angle then
            Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(points_per_curve - 1) (fun point ->
                if point land 4095 = 0 then Cancel.check_opt cancel;
                t_values.(point) <- float_of_int point
                    /. float_of_int segment_count)
          else begin
            let ramp_break_count = function
              | Some ramp -> max 0 (Array.length ramp.positions - 2)
              | None -> 0 in
            let break_candidate_count = ramp_break_count height_ramp
                + ramp_break_count radius_ramp in
            let breaks = Array.make break_candidate_count 0.
            and break_cursor = ref 0 in
            let append_ramp_breaks = function
              | Some ramp when Array.length ramp.positions > 2 ->
                  for index = 1 to Array.length ramp.positions - 2 do
                    breaks.(!break_cursor) <- ramp.positions.(index);
                    incr break_cursor
                  done
              | Some _ | None -> () in
            append_ramp_breaks height_ramp; append_ramp_breaks radius_ramp;
            assert (!break_cursor = break_candidate_count);
            Array.sort Float.compare breaks;
            let break_count = ref 0 in
            for index = 0 to break_candidate_count - 1 do
              if !break_count = 0
                  || breaks.(index) <> breaks.(!break_count - 1) then begin
                breaks.(!break_count) <- breaks.(index);
                incr break_count
              end
            done;
            let candidates = Array.make
                (segment_count + 1 + !break_count) 0. in
            let grid_boundary = ref 0 and break_index = ref 0
            and boundary_count = ref 0 in
            while !grid_boundary <= segment_count
                || !break_index < !break_count do
              if !boundary_count land 4095 = 0 then Cancel.check_opt cancel;
              let use_grid = if !grid_boundary > segment_count then false
                else if !break_index >= !break_count then true
                else
                  float_of_int !grid_boundary /. float_of_int segment_count
                    <= breaks.(!break_index) in
              if use_grid then begin
                let value = float_of_int !grid_boundary
                    /. float_of_int segment_count in
                candidates.(!boundary_count) <- value;
                incr boundary_count; incr grid_boundary;
                if !break_index < !break_count
                    && breaks.(!break_index) = value then incr break_index
              end else begin
                candidates.(!boundary_count) <- breaks.(!break_index);
                incr boundary_count; incr break_index
              end
            done;
            let integration_count = !boundary_count - 1 in
            let lengths = Array.make integration_count 0. in
            Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(integration_count - 1)
              (fun segment ->
                if segment land 4095 = 0 then Cancel.check_opt cancel;
                let lower = candidates.(segment)
                and upper = candidates.(segment + 1) in
                lengths.(segment) <- gauss_length profile lower upper);
            let prefix = Array.make (integration_count + 1) 0. in
            let invalid = ref (-1) in
            for segment = 0 to integration_count - 1 do
              if segment land 4095 = 0 then Cancel.check_opt cancel;
              let length = lengths.(segment) in
              if not (Float.is_finite length && length >= 0.) && !invalid < 0 then
                invalid := segment;
              prefix.(segment + 1) <- prefix.(segment) +. length
            done;
            let total = prefix.(integration_count) in
            if !invalid >= 0 || not (Float.is_finite total) || total <= 0. then
              raise (Invalid_spiral
                "Rdk.Spiral.spiral: equal-arc integration produced a non-finite or zero curve length");
            t_values.(0) <- 0.; t_values.(segment_count) <- 1.;
            Parallel.for_ ~chunk_size:grain ~start:1
              ~finish:(segment_count - 1) (fun point ->
                if point land 4095 = 0 then Cancel.check_opt cancel;
                let target = total *. float_of_int point
                    /. float_of_int segment_count in
                let low = ref 0 and high = ref integration_count in
                while !low + 1 < !high do
                  let middle = (!low + !high) lsr 1 in
                  if prefix.(middle) <= target then low := middle
                  else high := middle
                done;
                let bin = !low
                and lower = candidates.(!low)
                and upper = candidates.(!low + 1) in
                let fraction = if lengths.(bin) = 0. then 0.
                  else (target -. prefix.(bin)) /. lengths.(bin) in
                let estimate = ref (lower +. ((upper -. lower) *. fraction)) in
                for _ = 0 to 3 do
                  let current_length = prefix.(bin)
                      +. gauss_length profile lower !estimate
                  and current_speed = speed profile !estimate in
                  if current_speed > 0. && Float.is_finite current_speed then
                    estimate := Float.max lower (Float.min upper
                      (!estimate -. ((current_length -. target) /. current_speed)))
                done;
                t_values.(point) <- !estimate)
          end;
          let local_radius = Array.make points_per_curve 0.
          and local_height = Array.make points_per_curve 0.
          and base_sine = Array.make points_per_curve 0.
          and base_cosine = Array.make points_per_curve 0.
          and base_angle = Array.make points_per_curve 0. in
          let table_errors = Array.make
              (((points_per_curve - 1) / grain) + 1) (-1) in
          Parallel.for_ ~chunk_size:1 ~start:0
            ~finish:(Array.length table_errors - 1) (fun range ->
              let first = range * grain
              and last = min points_per_curve ((range + 1) * grain) in
              for point = first to last - 1 do
                if point land 4095 = 0 then Cancel.check_opt cancel;
                let t = t_values.(point) in
                let angle = start_angle +. (angular_rate *. t)
                and radius = final_radius profile t *. uniform_scale
                and height = final_height profile t *. uniform_scale in
                if Float.is_finite angle && Float.is_finite radius && Float.is_finite height then begin
                  base_angle.(point) <- angle;
                  base_sine.(point) <- sin angle;
                  base_cosine.(point) <- cos angle;
                  local_radius.(point) <- radius;
                  local_height.(point) <- height
                end else if table_errors.(range) < 0 then
                  table_errors.(range) <- point
              done);
          let table_invalid = Array.fold_left (fun first point ->
              if point < 0 then first else if first < 0 || point < first
              then point else first) (-1) table_errors in
          if table_invalid >= 0 then Error (Printf.sprintf
              "Rdk.Spiral.spiral: generated sample %d is not finite" table_invalid)
          else begin
            let phase_sine = Array.make spiral_count 0.
            and phase_cosine = Array.make spiral_count 0. in
            for spiral = 0 to spiral_count - 1 do
              let phase = 2. *. Float.pi *. float_of_int spiral
                  /. float_of_int spiral_count in
              phase_sine.(spiral) <- sin phase; phase_cosine.(spiral) <- cos phase
            done;
            let px = Array.make point_count 0. and py = Array.make point_count 0.
            and pz = Array.make point_count 0.
            and vertex_points = Array.make point_count 0
            and angle_values = Option.map (fun _ -> Array.make point_count 0.)
                angle_attribute in
            let range_count = ((point_count - 1) / grain) + 1
            and position_errors = Array.make (((point_count - 1) / grain) + 1) (-1) in
            Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
              (fun range ->
                let first = range * grain
                and last = min point_count ((range + 1) * grain) in
                for point = first to last - 1 do
                  if point land 4095 = 0 then Cancel.check_opt cancel;
                  let spiral = point / points_per_curve
                  and local = point mod points_per_curve in
                  let cosine = (base_cosine.(local) *. phase_cosine.(spiral))
                      -. (base_sine.(local) *. phase_sine.(spiral))
                  and sine = (base_sine.(local) *. phase_cosine.(spiral))
                      +. (base_cosine.(local) *. phase_sine.(spiral)) in
                  let lx = local_radius.(local) *. cosine
                  and ly = local_height.(local)
                  and lz = local_radius.(local) *. sine in
                  let x = center.x +. (x_axis.x *. lx) +. (y_axis.x *. ly)
                      +. (z_axis.x *. lz)
                  and y = center.y +. (x_axis.y *. lx) +. (y_axis.y *. ly)
                      +. (z_axis.y *. lz)
                  and z = center.z +. (x_axis.z *. lx) +. (y_axis.z *. ly)
                      +. (z_axis.z *. lz) in
                  if Float.is_finite x && Float.is_finite y && Float.is_finite z then begin
                    px.(point) <- x; py.(point) <- y; pz.(point) <- z;
                    vertex_points.(point) <- point;
                    (match angle_values with
                     | Some values -> values.(point) <- base_angle.(local)
                         +. (2. *. Float.pi *. float_of_int spiral
                           /. float_of_int spiral_count)
                     | None -> ())
                  end else if position_errors.(range) < 0 then
                    position_errors.(range) <- point
                done);
            let position_invalid = Array.fold_left (fun first point ->
                if point < 0 then first else if first < 0 || point < first
                then point else first) (-1) position_errors in
            if position_invalid >= 0 then Error (Printf.sprintf
                "Rdk.Spiral.spiral: generated point %d is not finite" position_invalid)
            else begin
              let distance_values = Option.map (fun _ -> Array.make point_count 0.)
                  distance_attribute in
              (match distance_values with
               | None -> ()
               | Some distances ->
                   let block_size = 4096
                   and blocks_per_curve = (segment_count + 4095) / 4096 in
                   let block_count = spiral_count * blocks_per_curve
                   and block_bases = Array.make
                       (spiral_count * blocks_per_curve) 0. in
                   Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(block_count - 1)
                     (fun block ->
                       if block land 255 = 0 then Cancel.check_opt cancel;
                       let spiral = block / blocks_per_curve
                       and local_block = block mod blocks_per_curve in
                       let first = (spiral * points_per_curve)
                           + (local_block * block_size) + 1
                       and last = min ((spiral + 1) * points_per_curve - 1)
                           ((spiral * points_per_curve)
                             + ((local_block + 1) * block_size)) in
                       let total = ref 0. in
                       for point = first to last do
                         let dx = px.(point) -. px.(point - 1)
                         and dy = py.(point) -. py.(point - 1)
                         and dz = pz.(point) -. pz.(point - 1) in
                         total := !total +. sqrt ((dx *. dx) +. (dy *. dy)
                           +. (dz *. dz))
                       done;
                       block_bases.(block) <- !total);
                   for spiral = 0 to spiral_count - 1 do
                     let accumulated = ref 0. in
                     for local_block = 0 to blocks_per_curve - 1 do
                       let block = (spiral * blocks_per_curve) + local_block
                       and length = block_bases.((spiral * blocks_per_curve)
                           + local_block) in
                       block_bases.(block) <- !accumulated;
                       accumulated := !accumulated +. length
                     done
                   done;
                   Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(block_count - 1)
                     (fun block ->
                       if block land 255 = 0 then Cancel.check_opt cancel;
                       let spiral = block / blocks_per_curve
                       and local_block = block mod blocks_per_curve in
                       let first = (spiral * points_per_curve)
                           + (local_block * block_size) + 1
                       and last = min ((spiral + 1) * points_per_curve - 1)
                           ((spiral * points_per_curve)
                             + ((local_block + 1) * block_size)) in
                       let accumulated = ref block_bases.(block) in
                       for point = first to last do
                         let dx = px.(point) -. px.(point - 1)
                         and dy = py.(point) -. py.(point - 1)
                         and dz = pz.(point) -. pz.(point - 1) in
                         accumulated := !accumulated +. sqrt ((dx *. dx)
                           +. (dy *. dy) +. (dz *. dz));
                         distances.(point) <- !accumulated
                       done));
              let frame_requested = Option.is_some x_axis_attribute
                  || Option.is_some y_axis_attribute
                  || Option.is_some tangent_attribute
                  || Option.is_some orient_attribute in
              let x_values = Option.map (fun _ -> Array.make point_count 0.,
                  Array.make point_count 0., Array.make point_count 0.)
                  x_axis_attribute
              and y_values = Option.map (fun _ -> Array.make point_count 0.,
                  Array.make point_count 0., Array.make point_count 0.)
                  y_axis_attribute
              and tangent_values = Option.map (fun _ -> Array.make point_count 0.,
                  Array.make point_count 0., Array.make point_count 0.)
                  tangent_attribute
              and orient_values = Option.map (fun _ -> Array.make point_count 0.,
                  Array.make point_count 0., Array.make point_count 0.,
                  Array.make point_count 0.) orient_attribute in
              (* Each range owns its tiny scratch buffer; the point loop does not
                 share writable state with another domain. *)
              let frame_errors = Array.make range_count (-1) in
              if frame_requested then
                Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(range_count - 1)
                  (fun range ->
                    let first = range * grain
                    and last = min point_count ((range + 1) * grain) in
                    let up = Array.make 3 0. in
                    for point = first to last - 1 do
                      if point land 4095 = 0 then Cancel.check_opt cancel;
                      let local = point mod points_per_curve in
                      let previous = if local = 0 then point else point - 1
                      and next = if local = segment_count then point else point + 1 in
                      let dx = px.(next) -. px.(previous)
                      and dy = py.(next) -. py.(previous)
                      and dz = pz.(next) -. pz.(previous) in
                      let adx = abs_float dx and ady = abs_float dy
                      and adz = abs_float dz in
                      let tangent_scale = if adx >= ady then
                          if adx >= adz then adx else adz
                        else if ady >= adz then ady else adz in
                      if tangent_scale = 0. || not (Float.is_finite tangent_scale) then
                        frame_errors.(range) <- if frame_errors.(range) < 0
                          then point else frame_errors.(range)
                      else begin
                        let sx = dx /. tangent_scale and sy = dy /. tangent_scale
                        and sz = dz /. tangent_scale in
                        let inverse = 1. /. sqrt ((sx *. sx) +. (sy *. sy)
                            +. (sz *. sz)) in
                        let tx = sx *. inverse and ty = sy *. inverse
                        and tz = sz *. inverse in
                        let projection = (y_axis.x *. tx) +. (y_axis.y *. ty)
                            +. (y_axis.z *. tz) in
                        let uy0 = y_axis.x -. (projection *. tx)
                        and uy1 = y_axis.y -. (projection *. ty)
                        and uy2 = y_axis.z -. (projection *. tz) in
                        let up_length = sqrt ((uy0 *. uy0) +. (uy1 *. uy1)
                            +. (uy2 *. uy2)) in
                        if up_length > 1e-12 then begin
                          up.(0) <- uy0 /. up_length;
                          up.(1) <- uy1 /. up_length;
                          up.(2) <- uy2 /. up_length
                        end
                        else begin
                          let spiral = point / points_per_curve in
                          let phase = 2. *. Float.pi *. float_of_int spiral
                              /. float_of_int spiral_count in
                          let cosine = cos (base_angle.(local) +. phase)
                          and sine = sin (base_angle.(local) +. phase) in
                          let rx = (x_axis.x *. cosine) +. (z_axis.x *. sine)
                          and ry = (x_axis.y *. cosine) +. (z_axis.y *. sine)
                          and rz = (x_axis.z *. cosine) +. (z_axis.z *. sine) in
                          let dot = (rx *. tx) +. (ry *. ty) +. (rz *. tz) in
                          let xx = rx -. (dot *. tx)
                          and xy = ry -. (dot *. ty)
                          and xz = rz -. (dot *. tz) in
                          let length = sqrt ((xx *. xx) +. (xy *. xy)
                              +. (xz *. xz)) in
                          let xx = xx /. length and xy = xy /. length
                          and xz = xz /. length in
                          up.(0) <- (tz *. xy) -. (ty *. xz);
                          up.(1) <- (tx *. xz) -. (tz *. xx);
                          up.(2) <- (ty *. xx) -. (tx *. xy)
                        end;
                        let uy0 = up.(0) and uy1 = up.(1) and uy2 = up.(2) in
                        let xx = (uy1 *. tz) -. (uy2 *. ty)
                        and xy = (uy2 *. tx) -. (uy0 *. tz)
                        and xz = (uy0 *. ty) -. (uy1 *. tx) in
                        (match x_values with Some (x, y, z) ->
                          x.(point) <- xx; y.(point) <- xy; z.(point) <- xz
                         | None -> ());
                        (match y_values with Some (x, y, z) ->
                          x.(point) <- uy0; y.(point) <- uy1; z.(point) <- uy2
                         | None -> ());
                        (match tangent_values with Some (x, y, z) ->
                          x.(point) <- tx; y.(point) <- ty; z.(point) <- tz
                         | None -> ());
                        (match orient_values with
                         | Some orientation ->
                             let trace = xx +. uy1 +. tz in
                             if trace > 0. then begin
                               let scale = sqrt (trace +. 1.) *. 2. in
                               write_normalized_quaternion orientation point
                                 ((uy2 -. ty) /. scale) ((tx -. xz) /. scale)
                                 ((xy -. uy0) /. scale) (0.25 *. scale)
                             end else if xx > uy1 && xx > tz then begin
                               let scale = sqrt (1. +. xx -. uy1 -. tz) *. 2. in
                               write_normalized_quaternion orientation point
                                 (0.25 *. scale) ((uy0 +. xy) /. scale)
                                 ((xz +. tx) /. scale) ((uy2 -. ty) /. scale)
                             end else if uy1 > tz then begin
                               let scale = sqrt (1. +. uy1 -. xx -. tz) *. 2. in
                               write_normalized_quaternion orientation point
                                 ((uy0 +. xy) /. scale) (0.25 *. scale)
                                 ((ty +. uy2) /. scale) ((tx -. xz) /. scale)
                             end else begin
                               let scale = sqrt (1. +. tz -. xx -. uy1) *. 2. in
                               write_normalized_quaternion orientation point
                                 ((xz +. tx) /. scale) ((ty +. uy2) /. scale)
                                 (0.25 *. scale) ((xy -. uy0) /. scale)
                             end
                         | None -> ())
                      end
                    done);
              let frame_invalid = Array.fold_left (fun first point ->
                  if point < 0 then first else if first < 0 || point < first
                  then point else first) (-1) frame_errors in
              if frame_invalid >= 0 then Error (Printf.sprintf
                  "Rdk.Spiral.spiral: cannot construct a frame at stationary point %d"
                  frame_invalid)
              else begin
                let attributes = ref [] in
                let add_float name values = match name, values with
                  | Some name, Some values ->
                      attributes := (Attribute.create_owned ~owner:Attribute.Point
                        ~name (Attribute.Float values) |> get_ok) :: !attributes
                  | None, None -> () | _ -> assert false
                and add_float3 name values = match name, values with
                  | Some name, Some (x, y, z) ->
                      let values = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
                      attributes := (Attribute.create_owned ~owner:Attribute.Point
                        ~name (Attribute.Float3 values) |> get_ok) :: !attributes
                  | None, None -> () | _ -> assert false
                and add_float4 name values = match name, values with
                  | Some name, Some (x, y, z, w) ->
                      let values = Packed.Float4.of_owned ~x ~y ~z ~w |> get_ok in
                      attributes := (Attribute.create_owned ~owner:Attribute.Point
                        ~name (Attribute.Float4 values) |> get_ok) :: !attributes
                  | None, None -> () | _ -> assert false in
                add_float angle_attribute angle_values;
                add_float3 x_axis_attribute x_values;
                add_float3 y_axis_attribute y_values;
                add_float3 tangent_attribute tangent_values;
                add_float4 orient_attribute orient_values;
                add_float distance_attribute distance_values;
                let topology = Topology.Private.create_validated_owned
                    ~point_count ~vertex_points
                    ~primitive_offsets:(Array.init (spiral_count + 1)
                      (fun spiral -> spiral * points_per_curve))
                    ~primitive_kinds:(Bytes.make spiral_count '\001') in
                let positions = Packed.Float3.Private.of_owned_exn
                    ~x:px ~y:py ~z:pz in
                Geometry.create ~positions ~topology
                  ~attributes:(List.rev !attributes) ()
              end
            end
          end))))

let run ?cancel ?grain ?extent ?radius ?height_ramp ?radius_scale
    ?radius_ramp ?direction ?start_angle ?divisions ?uniform_angle
    ?spiral_count ?orientation ?center ?rotation ?rotation_order ?uniform_scale
    ?angle_attribute ?x_axis_attribute ?y_axis_attribute ?tangent_attribute
    ?orient_attribute ?distance_attribute () =
  Error.guard ~operation:"spiral" ~code:"invalid_parameter" (fun () ->
    try generate ?cancel ?grain ?extent ?radius ?height_ramp ?radius_scale
        ?radius_ramp ?direction ?start_angle ?divisions ?uniform_angle
        ?spiral_count ?orientation ?center ?rotation ?rotation_order
        ?uniform_scale ?angle_attribute ?x_axis_attribute ?y_axis_attribute
        ?tangent_attribute ?orient_attribute ?distance_attribute ()
    with Invalid_spiral message -> Error message)
