type interaction = Orbit | Pan
type settings = { target : Rays_math.Vec3.t; distance : float; azimuth : float; elevation : float }
type motion = { interaction : interaction; dx : float; dy : float }

type camera_snapshot = {
  snapshot_target : Rays_math.Vec3.t;
  snapshot_distance : float;
  snapshot_azimuth : float;
  snapshot_elevation : float;
  snapshot_fov_y : float;
  snapshot_near : float;
  snapshot_far : float;
  snapshot_camera : Camera.t;
}

type t = {
  target : Rays_math.Vec3.t;
  distance : float;
  azimuth : float;
  elevation : float;
  fov_y : float;
  near : float;
  far : float;
  control_area : (int * int * int * int) option;
  inertia : bool;
  drag : (Input.mouse_button * interaction * (float * float)) option;
  velocity : motion option;
  last_press : (Input.mouse_button * (float * float) * float) option;
  pointer : (float * float) option;
  initial : settings;
  mutable camera_cache : camera_snapshot option;
}

let validate_distance distance =
  if (not (Float.is_finite distance)) || distance <= 0. then
    invalid_arg "Easy_camera: distance must be finite and positive"

let validate_control_area = function
  | None -> ()
  | Some (_, _, width, height) ->
      if width < 0 || height < 0 then
        invalid_arg "Easy_camera: control-area dimensions must be non-negative"

let rotation_sensitivity = 0.01
let dolly_sensitivity = 0.01
let drag_coefficient = 0.9

let clamp_elevation elevation =
  let limit = (Float.pi /. 2.) -. 1e-4 in
  Float.max (-.limit) (Float.min limit elevation)

let create ?(target = Rays_math.Vec3.zero) ?(distance = 10.) ?(azimuth = 0.) ?(elevation = 0.)
    ?(fov_y = Float.pi /. 3.) ?(inertia = true) () =
  let near = 0.1 and far = 1000. in
  validate_distance distance;
  ignore
    (Camera.perspective ~fov_y ~near ~far ~at:(Rays_math.Vec3.create 0. 0. distance) ~target ());
  let elevation = clamp_elevation elevation in
  {
    target;
    distance;
    azimuth;
    elevation;
    fov_y;
    near;
    far;
    control_area = None;
    inertia;
    drag = None;
    velocity = None;
    last_press = None;
    pointer = None;
    initial = { target; distance; azimuth; elevation };
    camera_cache = None;
  }

let orbit_basis up =
  let candidate =
    if abs_float (Rays_math.Vec3.dot up Rays_math.Vec3.unit_z) < 0.999 then Rays_math.Vec3.unit_z
    else Rays_math.Vec3.unit_x
  in
  let forward =
    Rays_math.Vec3.sub candidate (Rays_math.Vec3.scale up (Rays_math.Vec3.dot candidate up))
    |> Rays_math.Vec3.normalize
  in
  let right = Rays_math.Vec3.cross up forward |> Rays_math.Vec3.normalize in
  (right, forward)

let camera value =
  match value.camera_cache with
  | Some cached
    when cached.snapshot_target = value.target
         && cached.snapshot_distance = value.distance
         && cached.snapshot_azimuth = value.azimuth
         && cached.snapshot_elevation = value.elevation
         && cached.snapshot_fov_y = value.fov_y
         && cached.snapshot_near = value.near && cached.snapshot_far = value.far ->
      cached.snapshot_camera
  | _ ->
      let up = Rays_math.Vec3.unit_y in
      let right, forward = orbit_basis up in
      let cosine = cos value.elevation in
      let horizontal =
        Rays_math.Vec3.add
          (Rays_math.Vec3.scale right (cosine *. sin value.azimuth))
          (Rays_math.Vec3.scale forward (cosine *. cos value.azimuth))
      in
      let offset =
        Rays_math.Vec3.add horizontal (Rays_math.Vec3.scale up (sin value.elevation))
        |> Fun.flip Rays_math.Vec3.scale value.distance
      in
      let camera =
        Camera.perspective ~fov_y:value.fov_y ~near:value.near ~far:value.far
          ~at:(Rays_math.Vec3.add value.target offset)
          ~target:value.target ()
        |> Camera.with_up up
      in
      value.camera_cache <-
        Some
          {
            snapshot_target = value.target;
            snapshot_distance = value.distance;
            snapshot_azimuth = value.azimuth;
            snapshot_elevation = value.elevation;
            snapshot_fov_y = value.fov_y;
            snapshot_near = value.near;
            snapshot_far = value.far;
            snapshot_camera = camera;
          };
      camera

let target value = value.target
let distance value = value.distance
let fov_y value = value.fov_y
let near value = value.near
let far value = value.far
let control_area value = value.control_area
let inertia value = value.inertia

(* Inverse of [camera]'s orbit placement; keeps the up axis, lens, and input
   settings and drops any in-flight inertia. *)
let of_view ~eye ~target value =
  let offset = Rays_math.Vec3.sub eye target in
  let distance = Rays_math.Vec3.length offset in
  if (not (Float.is_finite distance)) || distance <= 1e-9 then
    { value with target; drag = None; velocity = None }
  else
    let unit = Rays_math.Vec3.scale offset (1. /. distance) in
    let right, forward = orbit_basis Rays_math.Vec3.unit_y in
    let height = Float.max (-1.) (Float.min 1. (Rays_math.Vec3.dot unit Rays_math.Vec3.unit_y)) in
    {
      value with
      target;
      distance;
      elevation = clamp_elevation (asin height);
      azimuth = atan2 (Rays_math.Vec3.dot unit right) (Rays_math.Vec3.dot unit forward);
      drag = None;
      velocity = None;
    }

(* Aim at the box center from the current direction, far enough that the
   bounding sphere fills the vertical field of view with 20% margin. *)
let frame_bounds ~min ~max value =
  let center = Rays_math.Vec3.scale (Rays_math.Vec3.add min max) 0.5 in
  let radius = 0.5 *. Rays_math.Vec3.length (Rays_math.Vec3.sub max min) in
  let distance = Float.max 1e-3 (radius /. tan (value.fov_y /. 2.) *. 1.2) in
  { value with target = center; distance; drag = None; velocity = None }

let with_distance distance value =
  validate_distance distance;
  if distance = value.distance then value else { value with distance }

let with_fov_y fov_y value =
  if fov_y = value.fov_y then value
  else begin
    ignore
      (Camera.perspective ~fov_y ~near:value.near ~far:value.far
         ~at:(Rays_math.Vec3.create 0. 0. value.distance)
         ~target:Rays_math.Vec3.zero ());
    { value with fov_y }
  end

let with_clip ~near ~far value =
  if near = value.near && far = value.far then value
  else begin
    ignore
      (Camera.perspective ~fov_y:value.fov_y ~near ~far
         ~at:(Rays_math.Vec3.create 0. 0. value.distance)
         ~target:Rays_math.Vec3.zero ());
    { value with near; far }
  end

let with_control_area area value =
  validate_control_area area;
  if area = value.control_area then value else { value with control_area = area; drag = None }

let with_inertia inertia value =
  if inertia = value.inertia then value
  else { value with inertia; velocity = (if inertia then value.velocity else None) }

let reset value =
  {
    value with
    target = value.initial.target;
    distance = value.initial.distance;
    azimuth = value.initial.azimuth;
    elevation = value.initial.elevation;
    drag = None;
    velocity = None;
  }

let contains value (x, y) =
  match value.control_area with
  | None -> true
  | Some (left, top, width, height) ->
      x >= float left && y >= float top && x < float (left + width) && y < float (top + height)

let pan_axes value =
  let camera = camera value in
  let forward =
    Rays_math.Vec3.normalize (Rays_math.Vec3.sub (Camera.target camera) (Camera.position camera))
  in
  let up = Camera.up camera in
  let right = Rays_math.Vec3.normalize (Rays_math.Vec3.cross forward up) in
  let up = Rays_math.Vec3.normalize (Rays_math.Vec3.cross right forward) in
  (right, up)

let apply_delta (frame : Frame.t) value interaction dx dy =
  match interaction with
  | Orbit ->
      {
        value with
        azimuth = value.azimuth -. (dx *. rotation_sensitivity);
        elevation = clamp_elevation (value.elevation +. (dy *. rotation_sensitivity));
      }
  | Pan ->
      let right, up = pan_axes value in
      let height = max 1 (snd frame.input.size) in
      let units_per_pixel =
        2. *. value.distance *. tan (value.fov_y /. 2.) /. float_of_int height
      in
      let delta =
        Rays_math.Vec3.add
          (Rays_math.Vec3.scale right (-.dx *. units_per_pixel))
          (Rays_math.Vec3.scale up (dy *. units_per_pixel))
      in
      { value with target = Rays_math.Vec3.add value.target delta }

let interaction_for_button = function
  | Input.LeftButton -> Some Orbit
  | MiddleButton | RightButton -> Some Pan
  | MouseX1 | MouseX2 -> None

let double_click value button point time =
  match value.last_press with
  | Some (previous_button, (previous_x, previous_y), previous_time) ->
      let x, y = point in
      previous_button = button && time >= previous_time
      && time -. previous_time <= 0.3
      && ((x -. previous_x) *. (x -. previous_x)) +. ((y -. previous_y) *. (y -. previous_y)) <= 25.
  | None -> false

let apply_inertia (frame : Frame.t) value =
  match (value.drag, value.velocity) with
  | None, Some motion when value.inertia && frame.input.dt > 0. ->
      let frames = frame.input.dt *. 60. in
      let decay = drag_coefficient ** frames in
      let value =
        apply_delta frame value motion.interaction (motion.dx *. decay) (motion.dy *. decay)
      in
      if abs_float motion.dx *. decay < 1e-4 && abs_float motion.dy *. decay < 1e-4 then
        { value with velocity = None }
      else
        {
          value with
          velocity = Some { motion with dx = motion.dx *. decay; dy = motion.dy *. decay };
        }
  | None, Some _ -> { value with velocity = None }
  | _ -> value

let update value (frame : Frame.t) =
  if frame.input.events = [] then apply_inertia frame value
  else
    let has_pointer =
      List.exists
        (function
          | Input.Pointer_pressed _ | Pointer_moved _ | Pointer_released _ -> true | _ -> false)
        frame.input.events
    in
    let pointer =
      ref
        (if has_pointer then Option.value ~default:frame.input.pointer value.pointer
         else frame.input.pointer)
    in
    let value =
      List.fold_left
        (fun value event ->
          match event with
          | Input.Pointer_pressed (button, point) -> (
              pointer := point;
              if not (contains value point) then value
              else if double_click value button point frame.input.t then
                let value = reset value in
                { value with last_press = Some (button, point, frame.input.t) }
              else
                match interaction_for_button button with
                | None -> { value with last_press = Some (button, point, frame.input.t) }
                | Some interaction ->
                    {
                      value with
                      drag = Some (button, interaction, point);
                      velocity = None;
                      last_press = Some (button, point, frame.input.t);
                    })
          | Pointer_moved (x, y) -> (
              let point = (x, y) in
              pointer := point;
              match value.drag with
              | None -> value
              | Some (button, interaction, (previous_x, previous_y)) ->
                  let dx = x -. previous_x and dy = y -. previous_y in
                  apply_delta frame value interaction dx dy |> fun value ->
                  {
                    value with
                    drag = Some (button, interaction, point);
                    velocity = Some { interaction; dx; dy };
                  })
          | Pointer_released (button, point) -> (
              pointer := point;
              match value.drag with
              | Some (captured, _, _) when captured = button ->
                  {
                    value with
                    drag = None;
                    velocity = (if value.inertia then value.velocity else None);
                  }
              | _ -> value)
          | Scrolled (horizontal, vertical) when contains value !pointer ->
              ignore horizontal;
              {
                value with
                distance =
                  Float.max 1e-4 (value.distance *. exp (-.vertical *. dolly_sensitivity *. 12.));
                velocity = None;
                pointer = Some !pointer;
              }
          | Pointer_cancelled button -> (
              match value.drag with
              | Some (captured, _, _) when captured = button ->
                  { value with drag = None; velocity = None }
              | _ -> value)
          | Focus_lost -> { value with drag = None; velocity = None; last_press = None }
          | _ -> value)
        value frame.input.events
    in
    let value =
      if not has_pointer then value
      else
        match value.pointer with
        | Some previous when previous = !pointer -> value
        | _ -> { value with pointer = Some !pointer }
    in
    apply_inertia frame value

(* One fly-camera step: held W/S/A/D/Q/E move along view forward, right,
   and the up axis at [speed] units/s (Shift x4); pointer motion yaws about
   the up axis and pitches within +-89 degrees; each wheel step scales the
   speed by 1.2. The orbit target stays [distance] ahead. *)
let fly ~speed value (frame : Frame.t) =
  let view = camera value and up = Rays_math.Vec3.unit_y in
  let eye = Camera.position view in
  let forward = Rays_math.Vec3.normalize (Rays_math.Vec3.sub (Camera.target view) eye) in
  let dx, dy = frame.mouse_delta in
  let yaw = -0.003 *. dx and pitch = -0.003 *. dy in
  let along = Rays_math.Vec3.dot up forward in
  let yawed =
    Rays_math.Vec3.add
      (Rays_math.Vec3.add
         (Rays_math.Vec3.scale forward (cos yaw))
         (Rays_math.Vec3.scale (Rays_math.Vec3.cross up forward) (sin yaw)))
      (Rays_math.Vec3.scale up (along *. (1. -. cos yaw)))
  in
  let limit = 89. *. Float.pi /. 180. in
  let elevation =
    Float.max (-.limit)
      (Float.min limit
         (asin (Float.max (-1.) (Float.min 1. (Rays_math.Vec3.dot yawed up))) +. pitch))
  in
  let horizontal =
    Rays_math.Vec3.normalize
      (Rays_math.Vec3.sub yawed (Rays_math.Vec3.scale up (Rays_math.Vec3.dot yawed up)))
  in
  let forward =
    Rays_math.Vec3.add
      (Rays_math.Vec3.scale horizontal (cos elevation))
      (Rays_math.Vec3.scale up (sin elevation))
  in
  let right = Rays_math.Vec3.normalize (Rays_math.Vec3.cross forward up) in
  let held key = List.mem (Input.KeyChar key) frame.input.keys in
  let axis positive negative =
    (if held positive then 1. else 0.) -. if held negative then 1. else 0.
  in
  let move =
    Rays_math.Vec3.add
      (Rays_math.Vec3.add
         (Rays_math.Vec3.scale forward (axis 'w' 's'))
         (Rays_math.Vec3.scale right (axis 'd' 'a')))
      (Rays_math.Vec3.scale up (axis 'q' 'e'))
  in
  let step = speed *. frame.input.dt *. if List.mem Input.Shift frame.input.keys then 4. else 1. in
  let eye =
    if Float.is_finite step then Rays_math.Vec3.add eye (Rays_math.Vec3.scale move step) else eye
  in
  let wheel =
    List.fold_left
      (fun total -> function Input.Scrolled (_, steps) -> total +. steps | _ -> total)
      0. frame.input.events
  in
  let speed = Float.max 0.01 (Float.min 1e4 (speed *. Float.pow 1.2 wheel)) in
  let target = Rays_math.Vec3.add eye (Rays_math.Vec3.scale forward (distance value)) in
  (of_view ~eye ~target value, speed)
