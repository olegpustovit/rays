type projection =
  | Perspective of { fov_y : float; near : float; far : float }
  | Orthographic of { height : float; near : float; far : float }

type lens = { aperture : float; focus_distance : float option }

let pinhole = { aperture = 0.; focus_distance = None }

type t = {
  position : Rays_math.Vec3.t;
  target : Rays_math.Vec3.t;
  up : Rays_math.Vec3.t;
  projection : projection;
  lens : lens;
}

let validate_view position target up =
  if Rays_math.Vec3.length_sq (Rays_math.Vec3.sub target position) = 0. then
    invalid_arg "Camera: position and target must differ";
  if Rays_math.Vec3.length_sq up = 0. then invalid_arg "Camera: up must be non-zero"

let create ~position ~target ~up ~projection =
  validate_view position target up;
  { position; target; up = Rays_math.Vec3.normalize up; projection; lens = pinhole }

let perspective ?(fov_y = Float.pi /. 3.) ?(near = 0.1) ?(far = 1000.) ~at ~target () =
  ignore (Rays_math.Mat4.perspective ~fov_y ~aspect:1. ~near ~far);
  create ~position:at ~target ~up:Rays_math.Vec3.unit_y
    ~projection:(Perspective { fov_y; near; far })

let orthographic ~height ~at ~target () =
  let near = 0.1 and far = 1000. in
  if (not (Float.is_finite height)) || height <= 0. then
    invalid_arg "Camera.orthographic: height must be finite and positive";
  if near = far then invalid_arg "Camera.orthographic: near and far must differ";
  create ~position:at ~target ~up:Rays_math.Vec3.unit_y
    ~projection:(Orthographic { height; near; far })

let position camera = camera.position
let target camera = camera.target
let up camera = camera.up
let projection camera = camera.projection
let lens camera = camera.lens

let focus_distance camera =
  match camera.lens.focus_distance with
  | Some distance -> distance
  | None -> Rays_math.Vec3.length (Rays_math.Vec3.sub camera.target camera.position)

let with_lens lens camera =
  if (not (Float.is_finite lens.aperture)) || lens.aperture < 0. then
    invalid_arg "Camera.with_lens: aperture must be finite and non-negative";
  Option.iter
    (fun distance ->
      if (not (Float.is_finite distance)) || distance <= 0. then
        invalid_arg "Camera.with_lens: focus distance must be finite and positive")
    lens.focus_distance;
  { camera with lens }

let with_position position camera =
  validate_view position camera.target camera.up;
  { camera with position }

let with_up up camera =
  validate_view camera.position camera.target up;
  { camera with up = Rays_math.Vec3.normalize up }

let viewport_size (_, _, width, height) =
  if width <= 0 || height <= 0 then invalid_arg "Camera: viewport dimensions must be positive";
  (float_of_int width, float_of_int height)

let view_matrix camera =
  Rays_math.Mat4.look_at ~eye:camera.position ~target:camera.target ~up:camera.up

let projection_matrix ~viewport camera =
  let width, height = viewport_size viewport in
  let aspect = width /. height in
  let projection =
    match camera.projection with
    | Perspective { fov_y; near; far } -> Rays_math.Mat4.perspective ~fov_y ~aspect ~near ~far
    | Orthographic { height; near; far } ->
        let half_height = height /. 2. in
        let half_width = half_height *. aspect in
        Rays_math.Mat4.orthographic ~left:(-.half_width) ~right:half_width ~bottom:(-.half_height)
          ~top:half_height ~near ~far
  in
  projection

let view_projection_matrix ~viewport camera =
  Rays_math.Mat4.mul (projection_matrix ~viewport camera) (view_matrix camera)

let world_to_screen ~viewport:((vx, vy, _, _) as viewport) camera point =
  let width, height = viewport_size viewport in
  let matrix = view_projection_matrix ~viewport camera in
  let x, y, z, w = Rays_math.Mat4.transform matrix (point.Rays_math.Vec3.x, point.y, point.z, 1.) in
  if w <= 1e-12 then None
  else
    let ndc_x = x /. w and ndc_y = y /. w and ndc_z = z /. w in
    Some
      (Rays_math.Vec3.create
         (float_of_int vx +. ((ndc_x +. 1.) *. 0.5 *. width))
         (float_of_int vy +. ((1. -. ndc_y) *. 0.5 *. height))
         ((ndc_z +. 1.) *. 0.5))

let screen_to_world ~viewport:((vx, vy, _, _) as viewport) camera point =
  let width, height = viewport_size viewport in
  match Rays_math.Mat4.inverse (view_projection_matrix ~viewport camera) with
  | None -> None
  | Some inverse ->
      let ndc_x = (2. *. (point.Rays_math.Vec3.x -. float_of_int vx) /. width) -. 1. in
      let ndc_y = 1. -. (2. *. (point.y -. float_of_int vy) /. height) in
      let ndc_z = (2. *. point.z) -. 1. in
      let x, y, z, w = Rays_math.Mat4.transform inverse (ndc_x, ndc_y, ndc_z, 1.) in
      if abs_float w <= 1e-12 then None else Some (Rays_math.Vec3.create (x /. w) (y /. w) (z /. w))

let world_to_camera camera point = Rays_math.Mat4.transform_point (view_matrix camera) point

let screen_ray ~viewport camera ~at:(x, y) =
  match
    ( screen_to_world ~viewport camera (Rays_math.Vec3.create x y 0.),
      screen_to_world ~viewport camera (Rays_math.Vec3.create x y 1.) )
  with
  | Some near, Some far -> Some (near, Rays_math.Vec3.normalize (Rays_math.Vec3.sub far near))
  | _ -> None
