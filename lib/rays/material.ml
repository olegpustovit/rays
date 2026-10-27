type t = {
  diffuse : Color.t;
  ambient : Color.t;
  specular : Color.t;
  emissive : Color.t;
  shininess : float;
  round : float;
}

let create ?(diffuse = Color.white) ?(ambient = Color.white)
    ?(specular = Color.white) ?(emissive = Color.black)
    ?(shininess = 32.) ?(round = 0.) () =
  if not (Float.is_finite shininess) || shininess < 0. then
    invalid_arg "Material.create: shininess must be finite and non-negative";
  if not (Float.is_finite round) || round < 0. then
    invalid_arg "Material.create: round must be finite and non-negative";
  { diffuse; ambient; specular; emissive; shininess; round }

let default = create ()

let matte diffuse =
  create ~diffuse ~ambient:diffuse ~specular:Color.black ~shininess:0. ()

let unlit emissive =
  let diffuse = Color.with_alpha Color.black emissive.Color.a in
  create ~diffuse ~ambient:Color.black ~specular:Color.black
    ~emissive ~shininess:0. ()
