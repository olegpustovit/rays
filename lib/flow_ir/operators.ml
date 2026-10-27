let noise_tables = Domain.DLS.new_key (fun () -> ref [])
let noise_table seed =
  let cache=Domain.DLS.get noise_tables in
  match List.assoc_opt seed !cache with
  |Some table->table
  |None->let table=Rays_math.Noise.create seed in
      cache:=(seed,table)::List.filteri(fun i _->i<15)!cache;table
let noise_settings args =
  let integer key default = Option.fold ~none:default ~some:(function
    | Flow.Value.Int n -> n | value -> Flow.Value.int_of value) (List.assoc_opt key args) in
  let number key default = Option.fold ~none:default ~some:Flow.Value.num (List.assoc_opt key args) in
  let seed=integer "seed" 0 and octaves=integer "octaves" 1 in
  let lacunarity=number "lacunarity" 2. and gain=number "gain" 0.5 in
  if octaves<1 || octaves>64 then Flow.Value.fail "E_NOISE_RANGE" "Noise octaves are 1 to 64.";
  if not (Float.is_finite lacunarity && lacunarity>0.) then
    Flow.Value.fail "E_NOISE_RANGE" "Noise lacunarity must be finite and positive.";
  if not (gain>=0. && gain<=1.) then Flow.Value.fail "E_NOISE_RANGE" "Noise gain is 0 to 1.";
  seed,octaves,lacunarity,gain
let noise3 : Flow.Op.t = {
  name = "noise3"; ctx = Flow.Context.value;
  signature = {pos = ["position", Flow.Ty.Vec3]; opt = []; rest = None;
    kw = ["seed",Flow.Ty.Int;"octaves",Flow.Ty.Int;"lacunarity",Flow.Ty.Float;"gain",Flow.Ty.Float]};
  out = (fun _ -> Flow.Ty.Float); out_args = None; any_num = false; choices = [];
  shape = Flow.Op.Scalar; live = false; category = "Noise"; arithmetic = None;
  packed_extension = Some Flow.Packed_ops.Noise3;
  check = (fun args ->
    if not(List.exists(fun (_,v)->match v with Flow.Value.Residual _->true|_->false)args)
    then ignore(noise_settings args));
  body = (fun ~live:_ ~node:_ args -> match List.assoc "position" args with
    | Flow.Value.Vec3 (x, y, z) ->
        ignore (Flow.Value.fin "noise3 position" x);
        ignore (Flow.Value.fin "noise3 position" y);
        ignore (Flow.Value.fin "noise3 position" z);
        let seed,octaves,lacunarity,gain=noise_settings args in
        Flow.Value.Float (Flow.Value.fin "noise3"
          (if octaves=1 then Rays_math.Noise.sample3 (noise_table seed) ~x ~y ~z
           else if lacunarity=2. && gain=0.5 then Rays_math.Noise.fbm3 ~octaves (noise_table seed) ~x ~y ~z
           else Rays_math.Noise.Private.fbm3_with_scratch (Rays_math.Noise.Private.create_fbm3_scratch ())
             (noise_table seed) ~octaves ~lacunarity ~gain ~x ~y ~z))
    | _ -> Flow.Value.fail "E_TYPE" "noise3 takes a vec3 position.");
}
let drawing_color = function
  |Flow.Value.Text text->(match Rays_math.Color.hex text with
      |Ok color->color|Error message->Flow.Value.fail "E_DRAW_COLOR" message)
  |Vec3(r,g,b)->Rays_math.Color.of_floats (Flow.Value.fin "red" r)
      (Flow.Value.fin "green" g) (Flow.Value.fin "blue" b) 1.
  |List values when Array.length values=4->
      let channel i=Flow.Value.fin "color" (Flow.Value.num values.(i))in
      Rays_math.Color.of_floats (channel 0) (channel 1) (channel 2) (channel 3)
  |_->Flow.Value.fail "E_TYPE" "A color is hex text, vec3 or (list r g b a)."
let color_value color =
  let r,g,b,a=Rays_math.Color.to_floats color in
  if color.Rays_math.Color.a=255 then Flow.Value.Vec3(r,g,b)
  else Flow.Value.List [|Float r;Float g;Float b;Float a|]
(* HSL to bytes with the Lisp [mod] (floored, as Core and packed kernels compute it), so the walker,
   Core and a packed kernel give the same bits; each channel is a byte over 255, as a color holds it *)
let hsl_channels h s l =
  let modulo x y = Float.rem (Float.rem x y +. y) y and unit v = Float.max 0. (Float.min 1. v) in
  let hue = modulo h 360. /. 60. and s = unit s and l = unit l in
  let chroma = (1. -. Float.abs (2. *. l -. 1.)) *. s in
  let x = chroma *. (1. -. Float.abs (modulo hue 2. -. 1.)) in
  let r, g, b =
    if hue < 1. then chroma, x, 0. else if hue < 2. then x, chroma, 0.
    else if hue < 3. then 0., chroma, x else if hue < 4. then 0., x, chroma
    else if hue < 5. then x, 0., chroma else chroma, 0., x in
  let m = l -. chroma /. 2. in
  let byte v = float_of_int (int_of_float (unit (v +. m) *. 255. +. 0.5)) /. 255. in
  byte r, byte g, byte b
let hsl : Flow.Op.t = {noise3 with name="color/hsl"; category="Color"; packed_extension=None;
  signature={pos=["h",Flow.Ty.Float;"s",Flow.Ty.Float;"l",Flow.Ty.Float];opt=[];rest=None;kw=[]};
  out=(fun _->Flow.Ty.Vec3);check=(fun _->());
  body=(fun ~live:_ ~node:_ args->
    let number key=Flow.Value.fin key (Flow.Value.num(List.assoc key args))in
    let r,g,b=hsl_channels (number "h") (number "s") (number "l") in Flow.Value.Vec3(r,g,b))}
let hsla : Flow.Op.t = {hsl with name="color/hsla";
  signature={hsl.signature with pos=hsl.signature.pos@["a",Flow.Ty.Float]};
  out=(fun _->Flow.Ty.List Flow.Ty.Float);
  body=(fun ~live:_ ~node:_ args->
    let number key=Flow.Value.fin key (Flow.Value.num(List.assoc key args))in
    let r,g,b,a=Rays_math.Color.to_floats (Rays_math.Color.hsl ~alpha:(number "a")
      (number "h") (number "s") (number "l"))in
    Flow.Value.List [|Float r;Float g;Float b;Float a|])}
let gradient : Flow.Op.t = {hsl with name="color/gradient";
  signature={pos=["stops",Flow.Ty.List Flow.Ty.Color;"x",Flow.Ty.Float];opt=[];rest=None;kw=[]};
  out=(fun _->Flow.Ty.Vec3);
  body=(fun ~live:_ ~node:_ args->
    let stops=match List.assoc "stops" args with
      |Flow.Value.List xs when Array.length xs>0->Array.to_list(Array.map drawing_color xs)
      |_->Flow.Value.fail "E_DRAW_COLOR" "A gradient needs at least one color stop."in
    if List.exists(fun color->color.Rays_math.Color.a<>255)stops then
      Flow.Value.fail "E_DRAW_COLOR" "A packed RGB gradient requires opaque stops.";
    color_value(Rays_math.Color.gradient stops (Flow.Value.fin "gradient position"
      (Flow.Value.num(List.assoc "x" args)))))}
let all = [noise3;hsl;hsla;gradient]
