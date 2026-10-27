(** A World: an ordered stack of procedural environment layers, and the
    deterministic CPU bake that turns it into equirectangular HDR maps plus
    extracted lights. Pure data: no GPU values. The raster renderer and the
    path tracer read the same {!baked} products.

    {b Conventions.} +Y is up. Colors are linear-light floats, HDR, never
    clamped. A unit direction [d = (x, y, z)] maps to the equirectangular
    coordinates [u = atan2 x (-z) / 2pi + 0.5] in \[0, 1) and
    [v = acos y / pi] in \[0, 1\]: -Z (the front, azimuth 0) is [u = 0.5],
    azimuth grows toward +X, row 0 is the zenith. Texel [(i, j)] of a
    [w x h] map holds the direction at [((i + 0.5) / w, (j + 0.5) / h)].
    Azimuth/elevation pairs mean
    [(cos el * sin az, sin el, -. cos el * cos az)]. [rotation] (radians,
    about +Y) turns the whole World, lights and sun included: layers are
    evaluated at azimuth [phi - rotation]. *)

type rgb = Rays_math.Color.Linear.t = { r : float; g : float; b : float }
val rgb : float -> float -> float -> rgb

type emit =
  | Dome  (** Baked into both maps. *)
  | Light
  (** Also extracted as rect area lights ({!baked.lights}); it stays in the
      camera map and is left out of the lighting map. *)

type shape =
  | Rect  (** [width] x [height] angular box around its centre. *)
  | Strip
  (** A band along the parallel at [elevation]: [width] radians of azimuth
      (>= 2pi is a full ring), [height] radians thick. *)
  | Circle  (** Disc of angular diameter [width]. *)
  | Ring  (** Annulus of angular diameter [width], [height] thick. *)

type layer =
  | Gradient of { zenith : rgb; horizon : rgb; nadir : rgb; sharpness : float }
  (** Base. Weight [1 - (1 - |y|) ** sharpness] from the horizon color
      toward zenith (y > 0) or nadir. *)
  | Sky of { turbidity : float; intensity : float }
  (** Base. Nishita single scattering (Rayleigh + Mie, 12 view and 4 light
      samples) lit by the World sun; [turbidity] scales Mie density
      (1 = clear). Below the horizon it fades to a ground of 0.3 x the
      horizon color. Baked at 128x64 and bilinearly upsampled. *)
  | Sun of { angular_radius : float; intensity : float; tint : rgb }
  (** A disc of radiance [intensity * tint] in the camera map only; lighting
      gets it through {!baked.sun}. Dropped when the sun is below the
      horizon. *)
  | Shape of { shape : shape; azimuth : float; elevation : float;
               width : float; height : float; softness : float; color : rgb;
               intensity : float; emit : emit }
  (** Adds [color * intensity], falling off with a smoothstep over
      [softness] radians outside the edge. *)
  | Scatter of { seed : int; count : int; elevation_min : float;
                 elevation_max : float; size_min : float; size_max : float;
                 palette : rgb list; intensity : float; mirror : bool;
                 emit : emit }
  (** [count] soft round blobs of angular radius in \[size_min, size_max\],
      fully soft from the centre, azimuth uniform, elevation uniform in its
      range, color from [palette] (white if empty), all from [seed] only.
      [mirror] adds each blob reflected below the horizon at 0.25 x
      radiance; mirrors are always [Dome]. *)
  | Room of { width : float; depth : float; height : float; rows : int;
              columns : int; panel : float; intensity : float; wall : rgb;
              cove : float; emit : emit }
  (** Base. The interior of the box \[-width/2, width/2\] x
      \[-height/2, height/2\] x \[-depth/2, depth/2\] seen from the origin.
      The ceiling is a [columns] (along X) x [rows] (along Z) grid; the
      centre [panel] fraction of each cell side is a white panel of
      radiance [intensity]. Walls and floor are [wall] lit by a constant
      ambient [intensity * panel^2 * 0.5] (the unlit ceiling at half that);
      a cove strip of radiance [cove] runs along the walls' top 5%.
      [emit] applies to the panels only. *)

type entry = { name : string; layer : layer; visible : bool }

type background =
  | Environment  (** Show the camera map. *)
  | Color of rgb
  | Transparent

type t = {
  layers : entry list;
  (** Bottom to top. A base layer (Gradient, Sky, Room) replaces everything
      below it; the others add radiance. Invisible entries are skipped, and
      anything below the topmost visible base layer is dropped (no maps, no
      lights). *)
  background : background;  (** Not baked: renderers read it at bounce 0. *)
  rotation : float;
  exposure : float;  (** Stops, not baked: carried to the renderers. *)
  time_of_day : float;  (** Hours, 0..24, local solar time. *)
  latitude : float;  (** Radians, north positive. *)
  day_of_year : int;
  sun : [ `Linked | `Manual of float * float ];
  (** [`Linked] uses the linked sun direction; [`Manual (azimuth, elevation)]. *)
}

val default : t
(** A neutral grey studio gradient. *)

val presets : (string * t) list
(** ["neon corridor"], ["soft blobs"], ["white room"], ["daylight"]. *)

val direction_of_uv : float -> float -> Rays_math.Vec3.t

(** {1 Bake} *)

type map = { width : int; height : int; pixels : Float.Array.t }
(** Row-major RGB, 3 floats per texel, equirectangular as above. *)

type light = {
  position : Rays_math.Vec3.t;
  normal : Rays_math.Vec3.t;  (** Emitting side; faces the origin. *)
  u : Rays_math.Vec3.t;  (** Half-extent axes: the rect is [position +- u +- v]. *)
  v : Rays_math.Vec3.t;
  radiance : rgb;
}

type sun = { direction : Rays_math.Vec3.t; radiance : rgb; angular_radius : float }
(** [direction] points toward the sun; [radiance] is the disc's. *)

type cdf = {
  marginal : Float.Array.t;  (** [height + 1] entries, 0 .. 1. *)
  conditional : Float.Array.t;  (** [height] rows of [width + 1], 0 .. 1. *)
}
(** Importance sampling of the lighting map by luminance x sin theta. A
    row or map with zero weight is uniform. *)

type baked = {
  camera : map;  (** Everything, sun disc included: what the eye sees. *)
  lighting : map;
  (** [camera] minus the [Light] emitters and the sun disc. *)
  cdf : cdf;
  sh9 : rgb array;
  (** Irradiance-ready SH: the lighting map's coefficients on the real basis
      [0.282095; 0.488603 y; 0.488603 z; 0.488603 x; 1.092548 xy;
       1.092548 yz; 0.315392 (3z^2 - 1); 1.092548 xz; 0.546274 (x^2 - y^2)],
      already multiplied by the cosine-lobe factors pi, 2pi/3, pi/4
      (Ramamoorthi-Hanrahan). *)
  specular : map array;
  (** 6 GGX-prefiltered lighting mips for roughness 0, 0.2, .., 1: 128x64,
      64x32, .., 4x2. Mip 0 is a box downsample. *)
  lights : light list;
  sun : sun option;
  background : background;
  exposure : float;
}

val bake : ?domains:int -> ?light_radius:float -> width:int -> height:int ->
  t -> baked
(** Byte-identical for every [domains] (default: recommended). [Light]
    shapes and blobs become rect lights at distance [light_radius]
    (default 10.) of angular size preserved ([2 R tan half-angle]); strips
    split into segments of at most 60 degrees; room panels stay at their
    true position in the box. *)

val bake_cached : width:int ->
  height:int -> t -> baked
(** [bake] behind a run-owned LRU of capacity 4 keyed structurally;
    changing only [background] or [exposure] is a hit. *)

module Private : sig
  type cache_context
  val create_cache_context : unit -> cache_context
  val with_cache_context : cache_context -> (unit -> 'a) -> 'a
end
