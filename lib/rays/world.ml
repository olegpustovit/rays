type rgb = Rays_math.Color.Linear.t = { r : float; g : float; b : float }

let rgb = Rays_math.Color.Linear.rgb

type emit = Dome | Light
type shape = Rect | Strip | Circle | Ring

type layer =
  | Gradient of { zenith : rgb; horizon : rgb; nadir : rgb; sharpness : float }
  | Sky of { turbidity : float; intensity : float }
  | Sun of { angular_radius : float; intensity : float; tint : rgb }
  | Shape of {
      shape : shape;
      azimuth : float;
      elevation : float;
      width : float;
      height : float;
      softness : float;
      color : rgb;
      intensity : float;
      emit : emit;
    }
  | Scatter of {
      seed : int;
      count : int;
      elevation_min : float;
      elevation_max : float;
      size_min : float;
      size_max : float;
      palette : rgb list;
      intensity : float;
      mirror : bool;
      emit : emit;
    }
  | Room of {
      width : float;
      depth : float;
      height : float;
      rows : int;
      columns : int;
      panel : float;
      intensity : float;
      wall : rgb;
      cove : float;
      emit : emit;
    }

type entry = { name : string; layer : layer; visible : bool }
type background = Environment | Color of rgb | Transparent

type t = {
  layers : entry list;
  background : background;
  rotation : float;
  exposure : float;
  time_of_day : float;
  latitude : float;
  day_of_year : int;
  sun : [ `Linked | `Manual of float * float ];
}

type map = { width : int; height : int; pixels : Float.Array.t }

type light = {
  position : Rays_math.Vec3.t;
  normal : Rays_math.Vec3.t;
  u : Rays_math.Vec3.t;
  v : Rays_math.Vec3.t;
  radiance : rgb;
}

type sun = { direction : Rays_math.Vec3.t; radiance : rgb; angular_radius : float }
type cdf = { marginal : Float.Array.t; conditional : Float.Array.t }

type baked = {
  camera : map;
  lighting : map;
  cdf : cdf;
  sh9 : rgb array Lazy.t;
  specular : map array Lazy.t;
  lights : light list;
  sun : sun option;
  background : background;
  exposure : float;
}

let pi = Float.pi
let two_pi = 2. *. Float.pi
let clamp1 x = Float.min 1. (Float.max (-1.) x)
let white = rgb 1. 1. 1.
let scale_rgb k c = { r = k *. c.r; g = k *. c.g; b = k *. c.b }

(* ---- mapping ---- *)

let direction_of_uv u v =
  let phi = (u -. 0.5) *. two_pi and theta = v *. pi in
  let s = sin theta in
  Rays_math.Vec3.create (s *. sin phi) (cos theta) (-.s *. cos phi)

let direction ~azimuth ~elevation =
  let c = cos elevation in
  Rays_math.Vec3.create (c *. sin azimuth) (sin elevation) (-.c *. cos azimuth)

(* Turns azimuth by [a] (toward +X). *)
let rotate a (d : Rays_math.Vec3.t) =
  let c = cos a and s = sin a in
  Rays_math.Vec3.create ((d.x *. c) -. (d.z *. s)) d.y ((d.z *. c) +. (d.x *. s))

let sun_direction ~latitude ~day_of_year ~hours =
  let dec = -0.40910518 *. cos (two_pi *. float (day_of_year + 10) /. 365.) in
  let h = (hours -. 12.) *. pi /. 12. in
  let east = -.cos dec *. sin h
  and north = (sin dec *. cos latitude) -. (cos dec *. sin latitude *. cos h)
  and up = (sin dec *. sin latitude) +. (cos dec *. cos latitude *. cos h) in
  Rays_math.Vec3.create east up (-.north)

let[@inline] smooth_edge soft dist =
  if dist <= 0. then 1.
  else if dist >= soft then 0.
  else
    let t = dist /. soft in
    1. -. (t *. t *. (3. -. (2. *. t)))

(* ---- bilinear sampling ---- *)

let[@inline] sample_into m u v (out : Float.Array.t) o =
  let fx = (u *. float m.width) -. 0.5 and fy = (v *. float m.height) -. 0.5 in
  let x0 = Float.floor fx and y0 = Float.floor fy in
  let tx = fx -. x0 and ty = fy -. y0 in
  let w = m.width and x0 = int_of_float x0 and y0 = int_of_float y0 in
  let xa = ((x0 mod w) + w) mod w in
  let xb = (xa + 1) mod w in
  let ya = max 0 (min (m.height - 1) y0) * w and yb = max 0 (min (m.height - 1) (y0 + 1)) * w in
  let p = m.pixels in
  let aa = (ya + xa) * 3 and ba = (ya + xb) * 3 and ab = (yb + xa) * 3 and bb = (yb + xb) * 3 in
  for c = 0 to 2 do
    let top =
      Float.Array.unsafe_get p (aa + c)
      +. (tx *. (Float.Array.unsafe_get p (ba + c) -. Float.Array.unsafe_get p (aa + c)))
    and bottom =
      Float.Array.unsafe_get p (ab + c)
      +. (tx *. (Float.Array.unsafe_get p (bb + c) -. Float.Array.unsafe_get p (ab + c)))
    in
    Float.Array.unsafe_set out (o + c) (top +. (ty *. (bottom -. top)))
  done

(* ---- Nishita sky ---- *)

let earth = 6360e3
and atmosphere = 6420e3

let beta_r = [| 5.8e-6; 13.5e-6; 33.1e-6 |]

(* Distance from a point inside a sphere to where the ray leaves it. *)
let[@inline] exit_distance ox oy oz dx dy dz radius =
  let b = (ox *. dx) +. (oy *. dy) +. (oz *. dz)
  and c = (ox *. ox) +. (oy *. oy) +. (oz *. oz) -. (radius *. radius) in
  -.b +. sqrt ((b *. b) -. c)

let nishita ~turbidity ~intensity (s : Rays_math.Vec3.t) dx dy dz out o =
  let views = 12 and steps = 4 and g = 0.76 and oy = earth +. 1. in
  let seg = exit_distance 0. oy 0. dx dy dz atmosphere /. float views in
  let mu = (dx *. s.x) +. (dy *. s.y) +. (dz *. s.z) in
  let phase_r = 3. /. (16. *. pi) *. (1. +. (mu *. mu))
  and phase_m =
    3. /. (8. *. pi)
    *. ((1. -. (g *. g)) *. (1. +. (mu *. mu)))
    /. ((2. +. (g *. g)) *. Float.pow (1. +. (g *. g) -. (2. *. g *. mu)) 1.5)
  in
  let beta_m = 21e-6 *. turbidity in
  let sum_r = Array.make 3 0. and sum_m = Array.make 3 0. in
  let od_r = ref 0. and od_m = ref 0. in
  for i = 0 to views - 1 do
    let t = seg *. (float i +. 0.5) in
    let px = dx *. t and py = oy +. (dy *. t) and pz = dz *. t in
    let h = sqrt ((px *. px) +. (py *. py) +. (pz *. pz)) -. earth in
    let hr = exp (-.h /. 7994.) *. seg and hm = exp (-.h /. 1200.) *. seg in
    od_r := !od_r +. hr;
    od_m := !od_m +. hm;
    let seg_l = exit_distance px py pz s.x s.y s.z atmosphere /. float steps in
    let lr = ref 0. and lm = ref 0. and lit = ref true and j = ref 0 in
    while !lit && !j < steps do
      let t = seg_l *. (float !j +. 0.5) in
      let qx = px +. (s.x *. t) and qy = py +. (s.y *. t) and qz = pz +. (s.z *. t) in
      let hl = sqrt ((qx *. qx) +. (qy *. qy) +. (qz *. qz)) -. earth in
      if hl < 0. then lit := false
      else begin
        lr := !lr +. (exp (-.hl /. 7994.) *. seg_l);
        lm := !lm +. (exp (-.hl /. 1200.) *. seg_l)
      end;
      incr j
    done;
    if !lit then
      for c = 0 to 2 do
        let tau = (beta_r.(c) *. (!od_r +. !lr)) +. (beta_m *. 1.1 *. (!od_m +. !lm)) in
        let a = exp (-.tau) in
        sum_r.(c) <- sum_r.(c) +. (a *. hr);
        sum_m.(c) <- sum_m.(c) +. (a *. hm)
      done
  done;
  for c = 0 to 2 do
    Float.Array.set out (o + c)
      (((sum_r.(c) *. beta_r.(c) *. phase_r) +. (sum_m.(c) *. beta_m *. phase_m))
      *. 20. *. intensity)
  done

(* ponytail: the sky is baked at a fixed 128x64 and upsampled; a sharper
   Mie halo or horizon needs a larger sky map. *)
let sky_map ~turbidity ~intensity sun =
  let w = 128 and h = 64 in
  let pixels = Float.Array.make (w * h * 3) 0. in
  let horizon = Float.Array.make (w * 3) 0. in
  let column i = (((float i +. 0.5) /. float w) -. 0.5) *. two_pi in
  Parallel.for_ ~chunk_size:8 ~start:0 ~finish:(w - 1) (fun i ->
      let phi = column i in
      nishita ~turbidity ~intensity sun (sin phi) 0. (-.cos phi) horizon (i * 3));
  Parallel.for_ ~chunk_size:2 ~start:0 ~finish:(h - 1) (fun j ->
      let theta = (float j +. 0.5) /. float h *. pi in
      let y = cos theta and s = sin theta in
      for i = 0 to w - 1 do
        let o = ((j * w) + i) * 3 in
        if y >= 0. then begin
          let phi = column i in
          nishita ~turbidity ~intensity sun (s *. sin phi) y (-.s *. cos phi) pixels o
        end
        else begin
          let k = 1. -. (0.7 *. (1. -. smooth_edge 0.1 (-.y))) in
          for c = 0 to 2 do
            Float.Array.set pixels (o + c) (k *. Float.Array.get horizon ((i * 3) + c))
          done
        end
      done);
  { width = w; height = h; pixels }

(* ---- layer preparation ---- *)

(* A soft round emitter: disc (ring = false) or annulus, in local space. *)
type round = {
  cx : float;
  cy : float;
  cz : float;
  theta : float;
  bound : float;
  cos_bound : float;
  radius : float;
  half : float;
  ring : bool;
  soft : float;
  color : rgb;
  promote : bool;
}

type prepared =
  | Fill of Float.Array.t (* one rgb per row *)
  | Sky_map of map
  | Disc of { x : float; y : float; z : float; cos_r : float; color : rgb }
  | Rounds of round array
  | Box of {
      c : Rays_math.Vec3.t;
      t : Rays_math.Vec3.t;
      b : Rays_math.Vec3.t;
      hw : float;
      hh : float;
      soft : float;
      cos_bound : float;
      color : rgb;
      promote : bool;
    }
  | Band of {
      azimuth : float;
      hw : float;
      elevation : float;
      hh : float;
      soft : float;
      y_lo : float;
      y_hi : float;
      color : rgb;
      promote : bool;
    }
  | Box_room of {
      hw : float;
      hh : float;
      hd : float;
      rows : int;
      columns : int;
      panel : float;
      intensity : float;
      ambient : rgb;
      cove : float;
      promote : bool;
    }

let is_base = function Gradient _ | Sky _ | Room _ -> true | _ -> false

(* Visible entries from the topmost visible base layer up. *)
let effective world =
  List.fold_left
    (fun kept e ->
      if not e.visible then kept else if is_base e.layer then [ e.layer ] else e.layer :: kept)
    [] world.layers
  |> List.rev

let local_sun (world : t) =
  match world.sun with
  | `Linked ->
      sun_direction ~latitude:world.latitude ~day_of_year:world.day_of_year ~hours:world.time_of_day
  | `Manual (azimuth, elevation) -> direction ~azimuth ~elevation

let tangent azimuth = Rays_math.Vec3.create (cos azimuth) 0. (sin azimuth)

let bitangent azimuth elevation =
  Rays_math.Vec3.create
    (-.sin elevation *. sin azimuth)
    (cos elevation)
    (sin elevation *. cos azimuth)

let round ~azimuth ~elevation ~radius ~half ~ring ~soft ~color ~promote =
  let c = direction ~azimuth ~elevation in
  let bound = Float.min pi (radius +. half +. soft) in
  {
    cx = c.x;
    cy = c.y;
    cz = c.z;
    theta = Float.acos (clamp1 c.y);
    bound;
    cos_bound = cos bound;
    radius;
    half;
    ring;
    soft;
    color;
    promote;
  }

(* Blob [k] of a scatter, keyed only by seed and index. *)
let blobs ~seed ~count ~elevation_min ~elevation_max ~size_min ~size_max ~palette =
  let rng = Rays_math.Rand.seed seed and palette = Array.of_list palette in
  List.init (max 0 count) (fun k ->
      let at n = Rays_math.Rand.float_at rng ~index:((4 * k) + n) in
      let color =
        if Array.length palette = 0 then white
        else
          palette.(min
                     (Array.length palette - 1)
                     (int_of_float (at 3 *. float (Array.length palette))))
      in
      ( two_pi *. at 0,
        elevation_min +. ((elevation_max -. elevation_min) *. at 1),
        size_min +. ((size_max -. size_min) *. at 2),
        color ))

let room_ambient ~intensity ~panel = intensity *. panel *. panel *. 0.5

let prepare ~height ~sun layer =
  match layer with
  | Gradient { zenith; horizon; nadir; sharpness } ->
      let rows = Float.Array.make (height * 3) 0. in
      for j = 0 to height - 1 do
        let y = cos ((float j +. 0.5) /. float height *. pi) in
        let w = 1. -. Float.pow (1. -. Float.abs y) sharpness in
        let far = if y >= 0. then zenith else nadir in
        Float.Array.set rows (j * 3) (horizon.r +. (w *. (far.r -. horizon.r)));
        Float.Array.set rows ((j * 3) + 1) (horizon.g +. (w *. (far.g -. horizon.g)));
        Float.Array.set rows ((j * 3) + 2) (horizon.b +. (w *. (far.b -. horizon.b)))
      done;
      Some (Fill rows)
  | Sky { turbidity; intensity } -> Some (Sky_map (sky_map ~turbidity ~intensity sun))
  | Sun { angular_radius; intensity; tint } ->
      if sun.Rays_math.Vec3.y <= 0. then None
      else
        (* The camera disc is at least 1.5 texels wide so it never vanishes
           at preview resolution. *)
        let r = Float.max angular_radius (1.5 *. pi /. float height) in
        Some
          (Disc { x = sun.x; y = sun.y; z = sun.z; cos_r = cos r; color = scale_rgb intensity tint })
  | Shape { shape; azimuth; elevation; width; height = sh; softness; color; intensity; emit } -> (
      let color = scale_rgb intensity color and promote = emit = Light in
      let soft = Float.max 0. softness in
      match shape with
      | Circle ->
          Some
            (Rounds
               [|
                 round ~azimuth ~elevation ~radius:(width /. 2.) ~half:0. ~ring:false ~soft ~color
                   ~promote;
               |])
      | Ring ->
          Some
            (Rounds
               [|
                 round ~azimuth ~elevation ~radius:(width /. 2.) ~half:(sh /. 2.) ~ring:true ~soft
                   ~color ~promote;
               |])
      | Rect ->
          let a = (width /. 2.) +. soft and b = (sh /. 2.) +. soft in
          let cos_bound =
            if a >= pi /. 2. || b >= pi /. 2. then 0.
            else cos (Float.atan (Float.hypot (tan a) (tan b)))
          in
          Some
            (Box
               {
                 c = direction ~azimuth ~elevation;
                 t = tangent azimuth;
                 b = bitangent azimuth elevation;
                 hw = width /. 2.;
                 hh = sh /. 2.;
                 soft;
                 cos_bound;
                 color;
                 promote;
               })
      | Strip ->
          let lo = elevation -. (sh /. 2.) -. soft and hi = elevation +. (sh /. 2.) +. soft in
          Some
            (Band
               {
                 azimuth;
                 hw = width /. 2.;
                 elevation;
                 hh = sh /. 2.;
                 soft;
                 y_lo = sin (Float.max (-.pi /. 2.) lo);
                 y_hi = sin (Float.min (pi /. 2.) hi);
                 color;
                 promote;
               }))
  | Scatter
      {
        seed;
        count;
        elevation_min;
        elevation_max;
        size_min;
        size_max;
        palette;
        intensity;
        mirror;
        emit;
      } ->
      let promote = emit = Light in
      let items = blobs ~seed ~count ~elevation_min ~elevation_max ~size_min ~size_max ~palette in
      Some
        (Rounds
           (Array.of_list
              (List.concat_map
                 (fun (azimuth, elevation, size, c) ->
                   let blob elevation k promote =
                     round ~azimuth ~elevation ~radius:0. ~half:0. ~ring:false ~soft:size
                       ~color:(scale_rgb (k *. intensity) c)
                       ~promote
                   in
                   if mirror then [ blob elevation 1. promote; blob (-.elevation) 0.25 false ]
                   else [ blob elevation 1. promote ])
                 items)))
  | Room { width; depth; height = rh; rows; columns; panel; intensity; wall; cove; emit } ->
      Some
        (Box_room
           {
             hw = width /. 2.;
             hh = rh /. 2.;
             hd = depth /. 2.;
             rows = max 1 rows;
             columns = max 1 columns;
             panel;
             intensity;
             cove;
             promote = emit = Light;
             ambient = scale_rgb (room_ambient ~intensity ~panel) wall;
           })

(* ---- per-texel evaluation ---- *)

(* acc: 0..2 lighting, 3..5 promoted (camera-only) radiance. *)
let[@inline] add acc promote k (c : rgb) =
  let o = if promote then 3 else 0 in
  Float.Array.unsafe_set acc o (Float.Array.unsafe_get acc o +. (k *. c.r));
  Float.Array.unsafe_set acc (o + 1) (Float.Array.unsafe_get acc (o + 1) +. (k *. c.g));
  Float.Array.unsafe_set acc (o + 2) (Float.Array.unsafe_get acc (o + 2) +. (k *. c.b))

let[@inline] base acc r g b =
  Float.Array.unsafe_set acc 0 r;
  Float.Array.unsafe_set acc 1 g;
  Float.Array.unsafe_set acc 2 b;
  Float.Array.unsafe_set acc 3 0.;
  Float.Array.unsafe_set acc 4 0.;
  Float.Array.unsafe_set acc 5 0.

(* The texel arrives in acc 6..11 (u, v, phi, x, y, z) so no float is boxed
   per call: minor GCs stop every domain. *)
let eval acc j prep =
  let u = Float.Array.unsafe_get acc 6
  and v = Float.Array.unsafe_get acc 7
  and phi = Float.Array.unsafe_get acc 8
  and x = Float.Array.unsafe_get acc 9
  and y = Float.Array.unsafe_get acc 10
  and z = Float.Array.unsafe_get acc 11 in
  match prep with
  | Fill rows ->
      base acc
        (Float.Array.unsafe_get rows (j * 3))
        (Float.Array.unsafe_get rows ((j * 3) + 1))
        (Float.Array.unsafe_get rows ((j * 3) + 2))
  | Sky_map m ->
      sample_into m u v acc 0;
      base acc (Float.Array.unsafe_get acc 0) (Float.Array.unsafe_get acc 1)
        (Float.Array.unsafe_get acc 2)
  | Disc d -> if (x *. d.x) +. (y *. d.y) +. (z *. d.z) >= d.cos_r then add acc true 1. d.color
  | Rounds items ->
      for k = 0 to Array.length items - 1 do
        let it = Array.unsafe_get items k in
        let dot = (x *. it.cx) +. (y *. it.cy) +. (z *. it.cz) in
        if dot >= it.cos_bound then begin
          let a = Float.acos (clamp1 dot) in
          let dist = if it.ring then Float.abs (a -. it.radius) -. it.half else a -. it.radius in
          let m = smooth_edge it.soft dist in
          if m > 0. then add acc it.promote m it.color
        end
      done
  | Box r ->
      let dc = (x *. r.c.x) +. (y *. r.c.y) +. (z *. r.c.z) in
      if dc > 0. && dc >= r.cos_bound then begin
        let a = Float.atan (((x *. r.t.x) +. (y *. r.t.y) +. (z *. r.t.z)) /. dc)
        and b = Float.atan (((x *. r.b.x) +. (y *. r.b.y) +. (z *. r.b.z)) /. dc) in
        let m = smooth_edge r.soft (Float.max (Float.abs a -. r.hw) (Float.abs b -. r.hh)) in
        if m > 0. then add acc r.promote m r.color
      end
  | Band s ->
      if y >= s.y_lo && y <= s.y_hi then begin
        let el = Float.asin (clamp1 y) in
        let da = Float.rem (phi -. s.azimuth +. (3. *. pi)) two_pi -. pi in
        let along = if s.hw >= pi then neg_infinity else (Float.abs da -. s.hw) *. cos el in
        let m = smooth_edge s.soft (Float.max along (Float.abs (el -. s.elevation) -. s.hh)) in
        if m > 0. then add acc s.promote m s.color
      end
  | Box_room r ->
      let ax = Float.abs x and ay = Float.abs y and az = Float.abs z in
      let tx = if ax > 0. then r.hw /. ax else infinity
      and ty = if ay > 0. then r.hh /. ay else infinity
      and tz = if az > 0. then r.hd /. az else infinity in
      let t = Float.min tx (Float.min ty tz) in
      let a = r.ambient in
      if ty <= t && y > 0. then begin
        base acc (0.5 *. a.r) (0.5 *. a.g) (0.5 *. a.b);
        let cell v half n =
          let f = (v +. half) /. (2. *. half) *. float n in
          let f = f -. Float.floor f in
          Float.abs (f -. 0.5) < r.panel /. 2.
        in
        if cell (x *. t) r.hw r.columns && cell (z *. t) r.hd r.rows then
          add acc r.promote r.intensity white
      end
      else begin
        base acc a.r a.g a.b;
        if ty > t && y *. t > r.hh *. 0.9 then add acc false r.cove white
      end

(* ---- extracted lights ---- *)

let rect_light ~rotation ~radius ~azimuth ~elevation ~half_w ~half_h radiance =
  let c = direction ~azimuth ~elevation and cap a = Float.min a 1.4 in
  let rot = rotate rotation in
  {
    position = rot (Rays_math.Vec3.scale c radius);
    normal = rot (Rays_math.Vec3.neg c);
    u = rot (Rays_math.Vec3.scale (tangent azimuth) (radius *. tan (cap half_w)));
    v = rot (Rays_math.Vec3.scale (bitangent azimuth elevation) (radius *. tan (cap half_h)));
    radiance;
  }

(* ponytail: every emitter becomes rect lights of its angular extent; round
   shapes scale radiance by area ratio (small-angle), softness is ignored. *)
let extract ~rotation ~radius layer =
  let rect = rect_light ~rotation ~radius in
  match layer with
  | Shape { emit = Light; shape; azimuth; elevation; width; height; color; intensity; _ } -> (
      let c = scale_rgb intensity color in
      match shape with
      | Rect -> [ rect ~azimuth ~elevation ~half_w:(width /. 2.) ~half_h:(height /. 2.) c ]
      | Circle ->
          [
            rect ~azimuth ~elevation ~half_w:(width /. 2.) ~half_h:(width /. 2.)
              (scale_rgb (pi /. 4.) c);
          ]
      | Ring ->
          let outer = (width /. 2.) +. (height /. 2.) in
          let k = two_pi *. (width /. 2.) *. height /. (4. *. outer *. outer) in
          [ rect ~azimuth ~elevation ~half_w:outer ~half_h:outer (scale_rgb k c) ]
      | Strip ->
          let span = Float.min width two_pi in
          let n = max 1 (int_of_float (Float.ceil (span /. (pi /. 3.)))) in
          List.init n (fun k ->
              let step = span /. float n in
              rect
                ~azimuth:(azimuth -. (span /. 2.) +. ((float k +. 0.5) *. step))
                ~elevation ~half_w:(step /. 2.) ~half_h:(height /. 2.) c))
  | Scatter
      {
        emit = Light;
        seed;
        count;
        elevation_min;
        elevation_max;
        size_min;
        size_max;
        palette;
        intensity;
        _;
      } ->
      blobs ~seed ~count ~elevation_min ~elevation_max ~size_min ~size_max ~palette
      |> List.map (fun (azimuth, elevation, size, c) ->
          rect ~azimuth ~elevation ~half_w:(size /. 2.) ~half_h:(size /. 2.)
            (scale_rgb (0.3 *. pi *. intensity) c))
  | Room { emit = Light; width; depth; height; rows; columns; panel; intensity; _ } ->
      let rows = max 1 rows and columns = max 1 columns in
      let cw = width /. float columns and cd = depth /. float rows in
      let rot = rotate rotation in
      List.init (rows * columns) (fun k ->
          let i = k mod columns and j = k / columns in
          {
            position =
              rot
                (Rays_math.Vec3.create
                   ((-.width /. 2.) +. ((float i +. 0.5) *. cw))
                   (height /. 2.)
                   ((-.depth /. 2.) +. ((float j +. 0.5) *. cd)));
            normal = rot (Rays_math.Vec3.create 0. (-1.) 0.);
            u = rot (Rays_math.Vec3.create (cw *. panel /. 2.) 0. 0.);
            v = rot (Rays_math.Vec3.create 0. 0. (cd *. panel /. 2.));
            radiance = rgb intensity intensity intensity;
          })
  | _ -> []

(* ---- derived products ---- *)

let texel_solid_angle ~width ~height j =
  two_pi /. float width *. (pi /. float height) *. sin ((float j +. 0.5) /. float height *. pi)

let sh_basis (d : Rays_math.Vec3.t) (out : float array) =
  let x = d.x and y = d.y and z = d.z in
  out.(0) <- 0.282095;
  out.(1) <- 0.488603 *. y;
  out.(2) <- 0.488603 *. z;
  out.(3) <- 0.488603 *. x;
  out.(4) <- 1.092548 *. x *. y;
  out.(5) <- 1.092548 *. y *. z;
  out.(6) <- 0.315392 *. ((3. *. z *. z) -. 1.);
  out.(7) <- 1.092548 *. x *. z;
  out.(8) <- 0.546274 *. ((x *. x) -. (y *. y))

let lobe =
  [|
    pi;
    2. *. pi /. 3.;
    2. *. pi /. 3.;
    2. *. pi /. 3.;
    pi /. 4.;
    pi /. 4.;
    pi /. 4.;
    pi /. 4.;
    pi /. 4.;
  |]

(* Per-row partial sums, then a fixed-order total: same bits on any domain
   count. *)
let project_sh m =
  let rows =
    Parallel.init_array ~grain:4 m.height (fun j ->
        let sums = Array.make 27 0. and y = Array.make 9 0. in
        let dw = texel_solid_angle ~width:m.width ~height:m.height j in
        let v = (float j +. 0.5) /. float m.height in
        for i = 0 to m.width - 1 do
          sh_basis (direction_of_uv ((float i +. 0.5) /. float m.width) v) y;
          let o = ((j * m.width) + i) * 3 in
          for c = 0 to 2 do
            let l = Float.Array.get m.pixels (o + c) *. dw in
            for k = 0 to 8 do
              sums.((k * 3) + c) <- sums.((k * 3) + c) +. (l *. y.(k))
            done
          done
        done;
        sums)
  in
  let total = Array.make 27 0. in
  Array.iter (fun sums -> Array.iteri (fun k s -> total.(k) <- total.(k) +. s) sums) rows;
  Array.init 9 (fun k ->
      rgb
        (lobe.(k) *. total.(k * 3))
        (lobe.(k) *. total.((k * 3) + 1))
        (lobe.(k) *. total.((k * 3) + 2)))

let luminance p o =
  Float.max 0.
    ((0.2126 *. Float.Array.get p o)
    +. (0.7152 *. Float.Array.get p (o + 1))
    +. (0.0722 *. Float.Array.get p (o + 2)))

(* Running sum normalised in place; uniform when the total is zero. *)
let normalise a o n total =
  for k = 0 to n do
    Float.Array.set a (o + k)
      (if total > 0. then Float.Array.get a (o + k) /. total else float k /. float n)
  done;
  Float.Array.set a (o + n) 1.

let build_cdf m =
  let w = m.width and h = m.height in
  let conditional = Float.Array.make (h * (w + 1)) 0. and row_sums = Float.Array.make h 0. in
  Parallel.for_ ~chunk_size:8 ~start:0 ~finish:(h - 1) (fun j ->
      let s = sin ((float j +. 0.5) /. float h *. pi) and o = j * (w + 1) and sum = ref 0. in
      for i = 0 to w - 1 do
        sum := !sum +. (luminance m.pixels (((j * w) + i) * 3) *. s);
        Float.Array.set conditional (o + i + 1) !sum
      done;
      Float.Array.set row_sums j !sum;
      normalise conditional o w !sum);
  let marginal = Float.Array.make (h + 1) 0. and sum = ref 0. in
  for j = 0 to h - 1 do
    sum := !sum +. Float.Array.get row_sums j;
    Float.Array.set marginal (j + 1) !sum
  done;
  let total = !sum in
  normalise marginal 0 h total;
  { marginal; conditional }

(* Box average when shrinking, bilinear when growing. *)
let resample src w h =
  let pixels = Float.Array.make (w * h * 3) 0. in
  Parallel.for_ ~chunk_size:8 ~start:0 ~finish:(h - 1) (fun j ->
      let px = Float.Array.make 3 0. in
      for i = 0 to w - 1 do
        let o = ((j * w) + i) * 3 in
        if src.width >= w && src.height >= h then begin
          let x0 = i * src.width / w
          and x1 = max ((i * src.width / w) + 1) ((i + 1) * src.width / w)
          and y0 = j * src.height / h
          and y1 = max ((j * src.height / h) + 1) ((j + 1) * src.height / h) in
          let n = float ((x1 - x0) * (y1 - y0)) in
          for c = 0 to 2 do
            let s = ref 0. in
            for y = y0 to y1 - 1 do
              for x = x0 to x1 - 1 do
                s := !s +. Float.Array.get src.pixels ((((y * src.width) + x) * 3) + c)
              done
            done;
            Float.Array.set pixels (o + c) (!s /. n)
          done
        end
        else begin
          sample_into src ((float i +. 0.5) /. float w) ((float j +. 0.5) /. float h) px 0;
          Float.Array.blit px 0 pixels o 3
        end
      done);
  { width = w; height = h; pixels }

let radical_inverse i =
  let rec go i bit acc =
    if i = 0 then acc else go (i lsr 1) (bit *. 0.5) (if i land 1 = 1 then acc +. bit else acc)
  in
  go i 0.5 0.

(* ponytail: 32 fixed Hammersley GGX samples per texel from the previous box
   level (Karis split-sum, N = V = R); fine-grained lobes alias above that. *)
let prefilter src ~roughness w h =
  let samples = 32 and a = roughness *. roughness in
  (* Half vectors in the (t, b, n) frame. *)
  let hx = Float.Array.make samples 0.
  and hy = Float.Array.make samples 0.
  and hz = Float.Array.make samples 0. in
  for s = 0 to samples - 1 do
    let e1 = (float s +. 0.5) /. float samples and e2 = radical_inverse s in
    let cos_t = sqrt ((1. -. e2) /. (1. +. (((a *. a) -. 1.) *. e2))) in
    let sin_t = sqrt (1. -. (cos_t *. cos_t)) and phi = two_pi *. e1 in
    Float.Array.set hx s (sin_t *. cos phi);
    Float.Array.set hy s (sin_t *. sin phi);
    Float.Array.set hz s cos_t
  done;
  let pixels = Float.Array.make (w * h * 3) 0. in
  Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(h - 1) (fun j ->
      let px = Float.Array.make 3 0. in
      let theta = (float j +. 0.5) /. float h *. pi in
      let ny = cos theta and st = sin theta in
      for i = 0 to w - 1 do
        let phi = (((float i +. 0.5) /. float w) -. 0.5) *. two_pi in
        let nx = st *. sin phi and nz = -.st *. cos phi in
        (* t = normalize (Y x n), b = n x t; texel centres never hit a pole. *)
        let tx = -.cos phi and tz = -.sin phi in
        let bx = ny *. tz and by = (nz *. tx) -. (nx *. tz) and bz = -.ny *. tx in
        let sr = ref 0. and sg = ref 0. and sb = ref 0. and sw = ref 0. in
        for s = 0 to samples - 1 do
          let a = Float.Array.unsafe_get hx s
          and c = Float.Array.unsafe_get hy s
          and d = Float.Array.unsafe_get hz s in
          let k = 2. *. d in
          let lx = (k *. ((a *. tx) +. (c *. bx) +. (d *. nx))) -. nx
          and ly = (k *. ((c *. by) +. (d *. ny))) -. ny
          and lz = (k *. ((a *. tz) +. (c *. bz) +. (d *. nz))) -. nz in
          let nl = (2. *. d *. d) -. 1. in
          if nl > 0. then begin
            let u = (Float.atan2 lx (-.lz) /. two_pi) +. 0.5 and v = Float.acos (clamp1 ly) /. pi in
            sample_into src u v px 0;
            sr := !sr +. (nl *. Float.Array.get px 0);
            sg := !sg +. (nl *. Float.Array.get px 1);
            sb := !sb +. (nl *. Float.Array.get px 2);
            sw := !sw +. nl
          end
        done;
        let o = ((j * w) + i) * 3 and k = if !sw > 0. then 1. /. !sw else 0. in
        Float.Array.set pixels o (!sr *. k);
        Float.Array.set pixels (o + 1) (!sg *. k);
        Float.Array.set pixels (o + 2) (!sb *. k)
      done);
  { width = w; height = h; pixels }

let specular_chain box =
  let boxes = Array.make 6 box in
  for k = 1 to 5 do
    boxes.(k) <- resample boxes.(k - 1) (128 lsr k) (64 lsr k)
  done;
  Array.init 6 (fun k ->
      if k = 0 then boxes.(0)
      else prefilter boxes.(k - 1) ~roughness:(0.2 *. float k) (128 lsr k) (64 lsr k))

(* ---- bake ---- *)

let bake ?domains ?(light_radius = 10.) ~width ~height (world : t) =
  if width < 1 || height < 1 then invalid_arg "World.bake: empty map";
  let sun = local_sun world in
  let layers = effective world in
  let rotation = world.rotation in
  let lights = List.concat_map (extract ~rotation ~radius:light_radius) layers in
  let sun_out =
    List.fold_left
      (fun found -> function
        | Sun { angular_radius; intensity; tint } when sun.y > 0. ->
            Some
              {
                direction = rotate rotation sun;
                angular_radius;
                radiance = scale_rgb intensity tint;
              }
        | _ -> found)
      None layers
  in
  let camera = Float.Array.make (width * height * 3) 0.
  and lighting = Float.Array.make (width * height * 3) 0. in
  (* Local azimuth per column: layers are evaluated at phi - rotation. *)
  let phis =
    Float.Array.init width (fun i ->
        let phi = ((((float i +. 0.5) /. float width) -. 0.5) *. two_pi) -. rotation in
        phi -. (two_pi *. Float.floor ((phi +. pi) /. two_pi)))
  in
  let sin_phis = Float.Array.map sin phis and cos_phis = Float.Array.map cos phis in
  Parallel.run ?domains (fun () ->
      let prepared = Array.of_list (List.filter_map (prepare ~height ~sun) layers) in
      Parallel.for_ ~chunk_size:4 ~start:0 ~finish:(height - 1) (fun j ->
          let v = (float j +. 0.5) /. float height in
          let theta = v *. pi in
          let y = cos theta and s = sin theta in
          (* Cull round items whose polar band misses this row. *)
          let row =
            Array.map
              (function
                | Rounds items ->
                    Rounds
                      (Array.of_list
                         (List.filter
                            (fun it -> Float.abs (theta -. it.theta) <= it.bound)
                            (Array.to_list items)))
                | p -> p)
              prepared
          in
          let acc = Float.Array.make 12 0. in
          Float.Array.set acc 7 v;
          Float.Array.set acc 10 y;
          for i = 0 to width - 1 do
            let phi = Float.Array.unsafe_get phis i in
            Float.Array.fill acc 0 6 0.;
            Float.Array.unsafe_set acc 6 ((phi /. two_pi) +. 0.5);
            Float.Array.unsafe_set acc 8 phi;
            Float.Array.unsafe_set acc 9 (s *. Float.Array.unsafe_get sin_phis i);
            Float.Array.unsafe_set acc 11 (-.s *. Float.Array.unsafe_get cos_phis i);
            for k = 0 to Array.length row - 1 do
              eval acc j (Array.unsafe_get row k)
            done;
            let o = ((j * width) + i) * 3 in
            for c = 0 to 2 do
              let l = Float.Array.unsafe_get acc c in
              Float.Array.unsafe_set lighting (o + c) l;
              Float.Array.unsafe_set camera (o + c) (l +. Float.Array.unsafe_get acc (c + 3))
            done
          done);
      let lighting = { width; height; pixels = lighting } in
      (* ponytail: SH9 is projected from the 128x64 box level, not the full
       map; exact enough for 9 coefficients. *)
      let cdf = build_cdf lighting in
      (* ponytail: the raster products are forced on one domain at a time (the raster lowering); the path
         tracer never reads them *)
      let box = lazy (resample lighting 128 64) in
      let sh9 = lazy (Parallel.run ?domains (fun () -> project_sh (Lazy.force box))) in
      let specular = lazy (Parallel.run ?domains (fun () -> specular_chain (Lazy.force box))) in
      {
        camera = { width; height; pixels = camera };
        lighting;
        cdf;
        sh9;
        specular;
        lights;
        sun = sun_out;
        background = world.background;
        exposure = world.exposure;
      })

module Key = struct
  type nonrec t = t * int * int * float

  let equal = ( = )
  let hash = Hashtbl.hash_param 64 256
end

module Cache = Lru.Make (Key)

type cache_context = { cache : baked Cache.t }

let current_cache = Domain.DLS.new_key (fun () -> None)
let create_cache_context () = { cache = Cache.create 4 }

let with_cache_context context f =
  let previous = Domain.DLS.get current_cache in
  Domain.DLS.set current_cache (Some context);
  Fun.protect ~finally:(fun () -> Domain.DLS.set current_cache previous) f

module Private = struct
  type nonrec cache_context = cache_context

  let create_cache_context = create_cache_context
  let with_cache_context = with_cache_context
end

let bake_cached ~width ~height (world : t) =
  let light_radius = 10. in
  let plain = { world with background = Environment; exposure = 0. } in
  let key = (plain, width, height, light_radius) in
  let baked =
    match Domain.DLS.get current_cache with
    | None -> bake ~light_radius ~width ~height plain
    | Some context -> (
        match Cache.find context.cache key with
        | baked -> baked
        | exception Not_found ->
            let baked = bake ~light_radius ~width ~height plain in
            Cache.add context.cache key baked;
            baked)
  in
  { baked with background = world.background; exposure = world.exposure }

(* ---- presets ---- *)

let entry name layer = { name; layer; visible = true }

let base_world layers =
  {
    layers;
    background = Environment;
    rotation = 0.;
    exposure = 0.;
    time_of_day = 14.;
    latitude = 0.7;
    day_of_year = 172;
    sun = `Linked;
  }

let default =
  base_world
    [
      entry "studio"
        (Gradient
           {
             zenith = rgb 0.8 0.8 0.8;
             horizon = rgb 0.5 0.5 0.5;
             nadir = rgb 0.2 0.2 0.2;
             sharpness = 1.;
           });
    ]

let strip name elevation color emit =
  entry name
    (Shape
       {
         shape = Strip;
         azimuth = 0.;
         elevation;
         width = two_pi;
         height = 0.03;
         softness = 0.02;
         color;
         intensity = 6.;
         emit;
       })

let presets =
  [
    ( "neon corridor",
      base_world
        [
          entry "dark"
            (Gradient
               {
                 zenith = rgb 0.01 0.005 0.02;
                 horizon = rgb 0.02 0.01 0.03;
                 nadir = rgb 0.005 0.005 0.01;
                 sharpness = 2.;
               });
          strip "magenta high" 0.45 (rgb 1. 0.1 0.6) Light;
          strip "cyan high" 0.25 (rgb 0.1 0.8 1.) Light;
          strip "cyan low" (-0.25) (rgb 0.1 0.8 1.) Dome;
          strip "magenta low" (-0.45) (rgb 1. 0.1 0.6) Dome;
        ] );
    ( "soft blobs",
      base_world
        [
          entry "dusk"
            (Gradient
               {
                 zenith = rgb 0.05 0.04 0.08;
                 horizon = rgb 0.12 0.08 0.1;
                 nadir = rgb 0.03 0.03 0.04;
                 sharpness = 1.5;
               });
          entry "blobs"
            (Scatter
               {
                 seed = 7;
                 count = 24;
                 elevation_min = 0.05;
                 elevation_max = 1.2;
                 size_min = 0.08;
                 size_max = 0.3;
                 palette = [ rgb 1. 0.6 0.7; rgb 0.6 0.8 1.; rgb 1. 0.9 0.6; rgb 0.7 1. 0.8 ];
                 intensity = 4.;
                 mirror = true;
                 emit = Dome;
               });
        ] );
    ( "white room",
      base_world
        [
          entry "room"
            (Room
               {
                 width = 8.;
                 depth = 12.;
                 height = 4.;
                 rows = 3;
                 columns = 2;
                 panel = 0.6;
                 intensity = 8.;
                 wall = rgb 0.8 0.8 0.8;
                 cove = 2.;
                 emit = Light;
               });
        ] );
    ( "daylight",
      base_world
        [
          entry "sky" (Sky { turbidity = 1.; intensity = 1. });
          entry "sun" (Sun { angular_radius = 0.02; intensity = 2000.; tint = rgb 1. 0.95 0.9 });
        ] );
  ]
