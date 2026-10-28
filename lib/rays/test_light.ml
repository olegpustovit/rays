open Rays

let camera=Camera.perspective~fov_y:1.~near:0.1~far:100.
  ~at:(Rays_math.Vec3.create 0. 0. 10.)~target:Rays_math.Vec3.zero()
let triangle=Result.get_ok(Mesh.View.create_owned
  [|Rays_math.Vec3.zero;Rays_math.Vec3.unit_x;Rays_math.Vec3.unit_y|]) |> Mesh.recalculate_normals
let lights samples=
  let attenuation=Light.attenuation~constant:2.~quadratic:3.()in
  let diffuse=Rays_math.Color.rgb 51 102 153 and at=Rays_math.Vec3.create 1. 2. 3.
  and direction=Rays_math.Vec3.create 0. (-2.) 0. in
  [Light.directional~diffuse~intensity:3.~direction:Rays_math.Vec3.unit_y();
   Light.point~diffuse~intensity:4.~attenuation~at();
   Light.spot~diffuse~intensity:5.~attenuation~at~direction~cutoff:0.5~concentration:3.();
   Light.area~diffuse~intensity:6.~attenuation~at~direction~width:4.~height:5.~samples()]
let stage lights=
  let scene=Scene.D3.create~lights[Scene.D3.mesh triangle]in
  let staged=Result.get_ok(Scene.Layer.stage_native~width:320~height:240
    [Scene.view3d~camera scene])in
  Option.get (List.hd staged.scene3).entries.(0).draw.state.transform_uniforms

let run ()=
  let defaults=Light.attenuation()and physical=Light.attenuation~constant:0.~quadratic:1.()in
  assert(defaults.constant=1.&&defaults.quadratic=0.);
  assert(physical.constant=0.&&physical.quadratic=1.);
  List.iter(fun(constant,quadratic)->
    match Light.attenuation~constant~quadratic()with
    |exception Invalid_argument _->()|_->failwith"invalid attenuation accepted")
    [0.,0.;(-1.),1.;1.,(-1.);nan,1.;1.,nan;infinity,1.;1.,infinity];
  List.iter(fun samples->
    let bytes=stage(lights samples)in
    assert(Int32.float_of_bits(Bytes.get_int32_le bytes(73*4))=4.);
    let rows=[|
      [0.;0.;1.;0.;0.2;0.4;0.6;1.;3.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.;0.];
      [1.;1.;2.;3.;0.2;0.4;0.6;1.;4.;2.;0.;3.;0.;0.;0.;0.;0.;0.;0.;0.];
      [2.;1.;2.;3.;0.2;0.4;0.6;1.;5.;0.;(-1.);0.;cos 0.5;cos 0.5;3.;2.;0.;3.;0.;0.];
      [3.;1.;2.;3.;0.2;0.4;0.6;1.;6.;0.;(-1.);0.;4.;5.;float samples;2.;0.;3.;0.;0.]|]in
    Array.iteri(fun row expected->List.iteri(fun word value->
      assert(Bytes.get_int32_le bytes((84+row*20+word)*4)=Int32.bits_of_float value))expected)rows)
    [1;4;9;16];
  print_endline "Lights: finite constant/quadratic attenuation, exact four-kind uniform ABI"

let dump ()=
  Marshal.to_channel stdout (List.map(fun samples->stage(lights samples))[1;4;9;16])[]

let bench ()=
  let authored=Array.of_list(lights 9)in
  let many=List.init 64(fun i->authored.(i mod 4))and count=5000 in
  let sample ()=
    Gc.full_major();
    let allocated=Gc.allocated_bytes()and started=Unix.gettimeofday()in
    for _=1 to count do ignore(stage many)done;
    Unix.gettimeofday()-.started,Gc.allocated_bytes()-.allocated in
  ignore(sample());
  let samples=Array.init 3(fun _->sample())in
  let times=Array.map fst samples and bytes=Array.map snd samples in
  Array.sort Float.compare times;Array.sort Float.compare bytes;
  Printf.printf "name,lights,frames,median_s,allocated_bytes\nlight_uniform_staging,64,%d,%.9f,%.0f\n%!"
    count times.(1)bytes.(1)
