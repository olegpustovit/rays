open Rays

let camera=Camera.orthographic ~height:2. ~at:(Rays_math.Vec3.create 0. 0. 2.) ~target:Rays_math.Vec3.zero ()
let stage mesh=
  let scene=Scene.D3.create [Scene.D3.mesh ~material:(Material.unlit Rays_math.Color.white) mesh]in
  let staged=Result.get_ok(Scene.Layer.stage_native ~width:128 ~height:128 [Scene.view3d ~camera scene])in
  (List.hd staged.scene3).entries.(0)
let attributes (entry:Renderer.Scene_execution.scene3_entry)=Option.get entry.vertex_attributes|>snd
let float bytes offset=Int32.float_of_bits(Bytes.get_int32_le bytes offset)
let make colors=
  let vertices:Mesh.View.vec3_view={
    x=[|-.0.70000001;0.60000001;0.10000001|];
    y=[|-.0.60000001;-.0.50000001;0.80000001|];z=[|0.;0.;0.|]}in
  let normals:Mesh.View.vec3_view={x=[|0.;0.;0.|];y=[|0.;0.;0.|];z=[|1.;1.;1.|]}in
  vertices,normals,[|0;1;2|],colors
let mesh (vertices,normals,indices,colors)=
  Result.get_ok(Mesh.View.create_packed_shared ~normals ~indices ~colors vertices)

let native source second_source first second=
  let configuration={Renderer.default_configuration with logical_width=128;logical_height=128;
    drawable_width=128;drawable_height=128}in
  match Renderer.create_offscreen configuration with
  |Error error->failwith(Format.asprintf "%a" Renderer.pp_error error)
  |Ok runtime->Fun.protect ~finally:(fun()->ignore(Renderer.destroy runtime))(fun()->
      let get=function Ok value->value|Error error->failwith(Format.asprintf "%a" Renderer.pp_error error)in
      let draw (entry:Renderer.Scene_execution.sampled_draw)=
        Renderer.prepared_draw ~family:entry.family ~blend:entry.blend
          ?texture:entry.texture ?auxiliary:entry.auxiliary
          ?vertex_attributes:entry.vertex_attributes ~samples:entry.samples entry.draw in
      let render entry=
        ignore(get(Renderer.draw ~clear:(0.,0.,0.,1.) runtime [draw entry]));
        get(Renderer.read_pixels runtime ~bytes_per_row:512)in
      let current=render first in
      let before=(get(Renderer.stats runtime)).uploaded_bytes in
      let changed=render second in
      let uploaded=Int64.sub(get(Renderer.stats runtime)).uploaded_bytes before in
      if changed=current then failwith "recolour did not change visible triangle pixels";
      if uploaded<>36L then failwith(Printf.sprintf "color edit reuploaded geometry (%Ld bytes)"uploaded);
      ignore(get(Renderer.draw ~clear:(0.,0.,0.,1.) runtime
        (List.init 65(fun _->draw second))));
      let combined=get(Renderer.read_pixels runtime ~bytes_per_row:512)in
      if combined<>changed then failwith "coalescing lost the float32 attribute stream")

let pair ()=
  let colors=[|Rays_math.Color.rgb 255 0 0;Rays_math.Color.rgb 0 255 0;Rays_math.Color.rgb 0 0 255|]in
  let vertices,normals,indices,_=make colors in
  let source=mesh(vertices,normals,indices,colors)
  and changed=mesh(vertices,normals,indices,Array.make 3(Rays_math.Color.rgb 255 255 0))in
  source,changed,stage source,stage changed

let with_caches f=Scene.Layer.with_cache_context
  (Scene.Layer.create_cache_context())f
let run ()=with_caches(fun()->
  let source,_,first,second=pair()in
  let packed=first.draw.mesh.vertices and attrs=attributes first in
  assert(Bytes.length packed=72&&Bytes.length attrs=36);
  let original=Mesh.View.view source in
  Array.iteri(fun i(position:Rays_math.Vec3.t)->
    let normal=(Option.get original.normals).(i)in
    List.iteri(fun word expected->
      assert(Bytes.get_int32_le packed(i*24+word*4)=Int32.bits_of_float expected))
      [position.x;position.y;position.z;normal.x;normal.y;normal.z];
    assert(Bytes.get_int32_le first.draw.mesh.indices(i*4)=Int32.of_int i);
    let color=(Option.get original.colors).(i)in
    assert(Bytes.get_int32_le attrs(i*12)=
      Int32.of_int((color.r lsl 24)lor(color.g lsl 16)lor(color.b lsl 8)lor color.a));
    assert(float attrs(i*12+4)=0.&&float attrs(i*12+8)=0.))original.vertices;
  assert(first.draw.mesh.vertices==second.draw.mesh.vertices);
  assert(first.draw.mesh.indices==second.draw.mesh.indices);
  assert(attrs!=attributes second&&attrs<>attributes second);
  Array.iteri(fun i _->assert(Bytes.get_int32_le(attributes second)(i*12)=0xffff00ffl))
    original.vertices;
  assert(float(Option.get first.draw.state.transform_uniforms)(82*4)=1.);
  let repeated=stage source in
  assert(repeated.draw.mesh.vertices==packed&&attributes repeated==attrs);
  assert(repeated.draw.mesh.indices==first.draw.mesh.indices);
  assert(Scene.D3.Walk.depth_clear Scene.D3.empty=1.);
  List.iter(fun samples->
    let scene=Scene.D3.create~samples[Scene.D3.mesh source]in
    assert(Scene.D3.Walk.depth_clear scene=1.);
    let staged=Result.get_ok(Scene.Layer.stage_native~width:128~height:128[Scene.view3d~camera scene])in
    Array.iter(fun(entry:Renderer.Scene_execution.scene3_entry)->
      assert(entry.samples=samples&&entry.draw.state.depth_clear=1.))
      (List.hd staged.scene3).entries)[1;4;9;16];
  let vertices,normals,indices,colors=make(Array.make 3 Rays_math.Color.white)in
  vertices.x.(0)<-1e100;
  let oversized=mesh(vertices,normals,indices,colors)in
  (match Scene.Layer.stage_native~width:128~height:128
      [Scene.view3d~camera(Scene.D3.create[Scene.D3.mesh oversized])]with
   |Error error->assert(error="native View3d lowering failed: mesh")
   |Ok _->failwith"non-finite float32 narrowing was accepted");
  print_endline "Scene3 float32: 36-byte split mirror, immutable component reuse, color invalidation, finite narrowing")

let run_native ()=with_caches(fun()->
  let source,changed,first,second=pair()in
  native source changed first second)
