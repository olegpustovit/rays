type t = {
  diffuse : Rays_math.Color.t;
  ambient : Rays_math.Color.t;
  specular : Rays_math.Color.t;
  emissive : Rays_math.Color.t;
  shininess : float;
  round : float;
}

let create ?(diffuse = Rays_math.Color.white) ?(ambient = Rays_math.Color.white)
    ?(specular = Rays_math.Color.white) ?(emissive = Rays_math.Color.black) ?(shininess = 32.)
    ?(round = 0.) () =
  if (not (Float.is_finite shininess)) || shininess < 0. then
    invalid_arg "Material.create: shininess must be finite and non-negative";
  if (not (Float.is_finite round)) || round < 0. then
    invalid_arg "Material.create: round must be finite and non-negative";
  { diffuse; ambient; specular; emissive; shininess; round }

let default = create ()

let matte diffuse =
  create ~diffuse ~ambient:diffuse ~specular:Rays_math.Color.black ~shininess:0. ()

let unlit emissive =
  let diffuse = Rays_math.Color.with_alpha Rays_math.Color.black emissive.Rays_math.Color.a in
  create ~diffuse ~ambient:Rays_math.Color.black ~specular:Rays_math.Color.black ~emissive
    ~shininess:0. ()
