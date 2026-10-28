(* Tuple shorthand for the record-based [Scene_execution.submit]. *)
let sampled ?texture ?auxiliary ?(samples=1) family blend draw : Scene_execution.sampled_draw =
  {family;blend;texture;auxiliary;vertex_attributes=None;samples;draw}
let render_resources ?clear r draws=Scene_execution.submit ?clear r
  (List.map(fun(family,blend,texture,auxiliary,draw)->sampled ?texture ?auxiliary family blend draw)draws)
let render_textured ?clear r draws=Scene_execution.submit ?clear r
  (List.map(fun(family,blend,texture,draw)->sampled ?texture family blend draw)draws)
let render_family ?clear r draws=Scene_execution.submit ?clear r
  (List.map(fun(family,blend,draw)->sampled family blend draw)draws)
let render_blended ?clear r draws=Scene_execution.submit ?clear r
  (List.map(fun(blend,draw)->sampled Scene_execution.Scene2 blend draw)draws)
let get=function Ok x->x|Error e->failwith(Ogpu.Error.to_string e)
let frames = List.init 600 succ
(* Scene2 and Scene2_textured share the argument-buffer pipeline: vertex
   color times the bound texel (white for untextured draws). *)
let source2={|#include <metal_stdlib>
using namespace metal;
struct Out { float4 position [[position]]; float4 color; float2 uv; };
inline float scene_double(const device uchar *p){uint lo=*reinterpret_cast<const device uint*>(p);uint hi=*reinterpret_cast<const device uint*>(p+4);ulong bits=(ulong(hi)<<32)|ulong(lo);float sign=(hi>>31)==0?1.:-1.;int exponent=int((bits>>52)&0x7fful);ulong fraction=bits&0xffffffffffffful;if(exponent==0)return sign*ldexp(float(fraction)/4503599627370496.,-1022);return sign*ldexp(1.+float(fraction)/4503599627370496.,exponent-1023);}
vertex Out scene_vertex(uint i [[vertex_id]],const device uchar *input [[buffer(0)]],const device float *affine [[buffer(6)]]){constexpr float2 p[3]={{-1.,-1.},{3.,-1.},{-1.,3.}};const device uchar*v=input+i*68;Out o;o.position=float4(p[i%3]+affine[0]*0.,0.,1.);o.color=unpack_unorm4x8_to_float(*reinterpret_cast<const device uint*>(v+48)).abgr;o.uv=float2(scene_double(v+52),scene_double(v+60));return o;}
struct Scene2_arguments { texture2d<float, access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };
fragment float4 scene_fragment(Out value [[stage_in]],constant Scene2_arguments&args [[buffer(1)]]){return value.color*args.image.sample(args.sampling,value.uv);}
|}
let source3={|#include <metal_stdlib>
using namespace metal;
struct Out { float4 position [[position]]; };
inline float scene_double(const device uchar *p){uint lo=*reinterpret_cast<const device uint*>(p);uint hi=*reinterpret_cast<const device uint*>(p+4);ulong bits=(ulong(hi)<<32)|ulong(lo);float sign=(hi>>31)==0?1.:-1.;int exponent=int((bits>>52)&0x7fful);ulong fraction=bits&0xffffffffffffful;if(exponent==0)return sign*ldexp(float(fraction)/4503599627370496.,-1022);return sign*ldexp(1.+float(fraction)/4503599627370496.,exponent-1023);}
vertex Out scene_vertex(uint i [[vertex_id]],const device uchar *input [[buffer(0)]]){const device uchar*p=input+i*68;Out v;v.position=float4(scene_double(p),scene_double(p+8),scene_double(p+16),1.);return v;}
fragment float4 scene_fragment(){return float4(1.,0.,0.5,1.);}
|}
let source3_textured={|#include <metal_stdlib>
using namespace metal;
struct Out { float4 position [[position]]; float2 uv; };
inline float scene_double(const device uchar *p){uint lo=*reinterpret_cast<const device uint*>(p);uint hi=*reinterpret_cast<const device uint*>(p+4);ulong bits=(ulong(hi)<<32)|ulong(lo);float sign=(hi>>31)==0?1.:-1.;int exponent=int((bits>>52)&0x7fful);ulong fraction=bits&0xffffffffffffful;if(exponent==0)return sign*ldexp(float(fraction)/4503599627370496.,-1022);return sign*ldexp(1.+float(fraction)/4503599627370496.,exponent-1023);}
vertex Out scene_vertex(uint i [[vertex_id]],const device uchar *input [[buffer(0)]]){const device uchar*p=input+i*68;Out v;v.position=float4(scene_double(p),scene_double(p+8),scene_double(p+16),1.);v.uv=float2(scene_double(p+52),scene_double(p+60));return v;}
fragment float4 scene_fragment(Out value [[stage_in]],texture2d<float> image [[texture(1)]],sampler sampling [[sampler(2)]]){return image.sample(sampling,value.uv);}
|}
let run () =
  let driver = Ogpu.Impl.create_driver () in let live_handles = Ogpu.Impl.live_handles in
  let before=live_handles()in
  let configuration:Ogpu.Surface.configuration={logical_width=4;logical_height=4;physical_width=4;physical_height=4;format=Bgra8_unorm;present_mode=Fifo;max_acquired=2;layer=None}in
  let renderer=get(Scene_execution.create~offscreen:true driver configuration(fun device family blend samples->let bytes,vertex_bindings,fragment_bindings,groups=match family with
    |Scene_execution.Scene2|Scene2_textured->source2,[{Ogpu.Shader.group=0;binding=0;kind=Storage_buffer;visibility=[Vertex]};{group=0;binding=6;kind=Storage_buffer;visibility=[Vertex]}],[{Ogpu.Shader.group=0;binding=1;kind=Storage_buffer;visibility=[Fragment]}],[0,[{Ogpu.Binding.binding=0;kind=Buffer;visibility=[Vertex]};{binding=6;kind=Buffer;visibility=[Vertex]};{binding=1;kind=Buffer;visibility=[Fragment]}]]
    |Scene3|Scene3_points->source3,[{Ogpu.Shader.group=0;binding=0;kind=Storage_buffer;visibility=[Vertex]}],[],[0,[{Ogpu.Binding.binding=0;kind=Buffer;visibility=[Vertex]}]]
    |Scene3_textured|Ui|Marks->source3_textured,[{Ogpu.Shader.group=0;binding=0;kind=Storage_buffer;visibility=[Vertex]}],[{Ogpu.Shader.group=0;binding=1;kind=Sampled_texture;visibility=[Fragment]};{group=0;binding=2;kind=Sampler;visibility=[Fragment]}],[0,[{Ogpu.Binding.binding=0;kind=Buffer;visibility=[Vertex]};{binding=1;kind=Texture;visibility=[Fragment]};{binding=2;kind=Sampler;visibility=[Fragment]}]]
    |Scene3_world->source3,[{Ogpu.Shader.group=0;binding=0;kind=Storage_buffer;visibility=[Vertex]}],[],[0,[{Ogpu.Binding.binding=0;kind=Buffer;visibility=[Vertex]}]]in
    let vertex=get(Ogpu.Shader.create{backend="metal";label=Some"scene-execution-metal-vertex";bytes=Bytes.of_string bytes;entry_points=[{name="scene_vertex";stage=Vertex}];bindings=vertex_bindings})and fragment=get(Ogpu.Shader.create{backend="metal";label=Some"scene-execution-metal-fragment";bytes=Bytes.of_string bytes;entry_points=[{name="scene_fragment";stage=Fragment}];bindings=fragment_bindings})in
    let layouts=List.map(fun(group,entries)->group,get(Ogpu.Binding.create_layout entries))groups in let layout=get(Ogpu.Binding.create_pipeline_layout~device:(Ogpu.Backend.device_handle device)~capabilities:(Ogpu.Backend.capabilities device)layouts)in let descriptor:Ogpu.Pipeline.render_descriptor={backend="metal";label=Some"scene-execution-metal";layout;vertex;vertex_entry="scene_vertex";fragment=Some fragment;fragment_entry=Some"scene_fragment";color_format=Rgba8_unorm;depth_format=(match family with Scene_execution.Scene2|Scene2_textured|Ui|Marks->Ogpu.Pipeline.No_depth|Scene3|Scene3_points|Scene3_textured|Scene3_world->Depth32_float);sample_count=samples}in Ogpu.Backend.create_render_pipeline~blend~topology:(if family=Scene3_points then Ogpu.Render_pass.Point_list else Triangle_list)~indirect:(family=Scene2||family=Scene2_textured)device descriptor))in
  let supported=List.filter(fun samples->samples<=(Ogpu.Backend.capabilities(Scene_execution.device renderer)).Ogpu.Caps.limits.max_sample_count)[1;4;9;16]in
  let indices=Bytes.make 12 '\000'in Bytes.set_int32_le indices 4 1l;Bytes.set_int32_le indices 8 2l;let plain=Bytes.make 48 '\000'in for index=0 to 2 do Bytes.set_int32_le plain(index*16+8)0xff0080ffl done;let mesh:Scene_execution.mesh={key=Host "fullscreen";vertices=plain;vertex_count=3;indices;index_count=3; primitive=Ogpu.Render_pass.Triangle_list}and state:Scene_execution.state={viewport=(0,0,4,4);scissor=(0,0,4,4);cull=Ogpu.Render_pass.Cull_none;depth_compare=Ogpu.Render_pass.Always;depth_write=false;depth_load=Ogpu.Render_pass.Clear;depth_clear=1.;transform_uniforms=None}in
  let sampled samples draw:Scene_execution.sampled_draw={family=Scene3;blend=Ogpu.Pipeline.Replace;texture=None;auxiliary=None;vertex_attributes=None;samples;draw}in
  ignore(get(Scene_execution_fixtures.render renderer[{mesh;state};{mesh;state}]));let pixels=get(Scene_execution.read_pixels renderer~bytes_per_row:16)in
  let r=Char.code(Bytes.get pixels 0)and g=Char.code(Bytes.get pixels 1)and b=Char.code(Bytes.get pixels 2)in if r<>255||g<>0||b<>128 then failwith(Printf.sprintf"scene execution Metal pixel mismatch %d,%d,%d"r g b);
  List.iter(fun(blend,expected)->List.iter(fun _frame->
    ignore(get(render_blended renderer[blend,{mesh;state}]));
    let bytes=get(Scene_execution.read_pixels renderer~bytes_per_row:16)in
    let actual=Char.code(Bytes.get bytes 0),Char.code(Bytes.get bytes 1),Char.code(Bytes.get bytes 2)in
    if actual<>expected then failwith"scene execution Metal blend pixel") frames)
    [Ogpu.Pipeline.Replace,(255,0,128);Alpha,(255,0,128);Add,(255,0,128);
     Multiply,(0,0,0);Screen,(255,0,128);Subtract,(0,0,0)];
  let vertices3=Bytes.make(68*3)'\000'in for index=0 to 2 do Bytes.set_int32_le vertices3(index*68+48)0xffffffffl done;
  List.iteri(fun index(x,y)->let offset=index*68 in Bytes.set_int64_le vertices3 offset(Int64.bits_of_float x);Bytes.set_int64_le vertices3(offset+8)(Int64.bits_of_float y);Bytes.set_int64_le vertices3(offset+16)(Int64.bits_of_float 0.))[-1.,-1.;3.,-1.;-1.,3.];
  let mesh3:Scene_execution.mesh={key=Host "fullscreen-scene3-double68";vertices=vertices3;vertex_count=3;indices;index_count=3; primitive=Ogpu.Render_pass.Triangle_list}in
  List.iter(fun _frame->ignore(get(render_family renderer[Scene_execution.Scene3,Ogpu.Pipeline.Replace,{mesh=mesh3;state}]));let bytes=get(Scene_execution.read_pixels renderer~bytes_per_row:16)in if Char.code(Bytes.get bytes 0)<>255||Char.code(Bytes.get bytes 2)<>128 then failwith"scene execution Metal Scene3 double68 pixel")frames;
  if List.mem 4 supported then List.iter(fun _frame->ignore(get(Scene_execution.submit renderer[sampled 4 {mesh=mesh3;state}]));let bytes=get(Scene_execution.read_pixels renderer~bytes_per_row:16)in if Char.code(Bytes.get bytes 0)<>255||Char.code(Bytes.get bytes 2)<>128 then failwith"scene execution Metal four-sample resolve pixel")frames;
  List.iter(fun samples->if not(List.mem samples supported)then let before=Scene_execution.upload_bytes renderer in match Scene_execution.submit renderer[sampled samples {mesh=mesh3;state}]with Error e when e.Ogpu.Error.kind=Unsupported&&Scene_execution.upload_bytes renderer=before->()|_->failwith"unsupported native sample count was not rejected atomically")[9;16];
  let mesh_uv u v=let bytes=Bytes.copy vertices3 in for index=0 to 2 do let offset=index*68 in Bytes.set_int64_le bytes(offset+52)(Int64.bits_of_float u);Bytes.set_int64_le bytes(offset+60)(Int64.bits_of_float v)done;{mesh3 with Scene_execution.key=Host(Printf.sprintf"textured-%g-%g"u v);vertices=bytes}in
  let levels=[|{Scene_execution.width=2;height=2;bytes=Bytes.of_string"\255\000\000\255\000\255\000\255\000\000\255\255\255\255\255\255"};{width=1;height=1;bytes=Bytes.of_string"\255\255\000\255"}|]in
  let sampler ?(min_filter=Ogpu.Types.Nearest)?(mag_filter=Ogpu.Types.Nearest)?(mip_filter=Ogpu.Types.No_mip)?(address_u=Ogpu.Types.Clamp_to_edge)?(address_v=Ogpu.Types.Clamp_to_edge)?(lod_min=0.)?(lod_max=1.)() : Ogpu.Types.sampler_descriptor={label=Some"scene-texture-test";min_filter;mag_filter;mip_filter;address_u;address_v;lod_min;lod_max;max_anisotropy=1}in
  let cases=[mesh_uv 0.1 0.1,sampler(),(255,0,0);mesh_uv 0.5 0.5,sampler~min_filter:Linear~mag_filter:Linear(),(128,128,128);mesh_uv 1.1 0.1,sampler~address_u:Repeat(),(255,0,0);mesh_uv 1.9 0.1,sampler~address_u:Mirror_repeat(),(255,0,0);mesh_uv 0.1 0.1,sampler~mip_filter:Nearest_mip~lod_min:1.~lod_max:1.(),(255,255,0)]in
  let mixed_texture:Scene_execution.sampled_texture={key="native-mixed-scene2";levels;sampler=sampler();gpu=None}in
  List.iter(fun _frame->
    ignore(get(render_textured renderer[
      Scene_execution.Scene2,Ogpu.Pipeline.Replace,None,{mesh;state};
      Scene_execution.Scene2_textured,Ogpu.Pipeline.Replace,Some mixed_texture,{mesh=mesh_uv 0.1 0.1;state}
    ]));
    let bytes=get(Scene_execution.read_pixels renderer~bytes_per_row:16)in
    if Char.code(Bytes.get bytes 0)<>255||Char.code(Bytes.get bytes 1)<>0||Char.code(Bytes.get bytes 2)<>0 then
      failwith"mixed Scene2/Scene2_textured native pixel") frames;
  let film=get(Ogpu.Backend.create_texture(Scene_execution.device renderer)
    {Ogpu.Types.label=Some"gpu-film-test";width=1;height=1;depth=1;
      mip_levels=1;sample_count=1;format=Rgba8_unorm;usage=[Texture_binding;Storage_binding;Texture_copy_dst]})in
  (* Produce the film on the GPU, as a path tracer would. *)
  let staging=get(Ogpu.Backend.create_buffer(Scene_execution.device renderer)
    {Ogpu.Types.label=Some"gpu-film-staging";size=256L;usage=[Copy_src]})in
  get(Ogpu.Backend.write_buffer staging~offset:0L(Bytes.of_string"\x31\x82\xdd\xff"));
  let commands=get(Ogpu.Backend.begin_commands(Scene_execution.queue renderer))in
  let blit=get(Ogpu.Backend.blit_encoder commands)in
  get(Ogpu.Backend.buffer_to_texture blit~src:staging~bytes_per_row:256L~bytes_per_image:256L
    ~dst:film~extent:{width=1;height=1;depth=1}());
  get(Ogpu.Backend.end_blit blit);
  let receipt=get(Ogpu.Backend.commit commands)in
  get(Ogpu.Backend.complete_through(Scene_execution.queue renderer)receipt.epoch);
  get(Ogpu.Backend.destroy_buffer staging);
  let gpu_texture:Scene_execution.sampled_texture={key="gpu-film-test";
    levels=[|{width=1;height=1;bytes=Bytes.empty}|];sampler=sampler();gpu=Some film}in
  (* Warm the transform ring for this one-draw frame; the measured frame
     must then upload nothing at all. *)
  ignore(get(render_textured renderer[Scene_execution.Scene2_textured,
    Ogpu.Pipeline.Replace,Some gpu_texture,{mesh=mesh_uv 0.1 0.1;state}]));
  let before_film=Scene_execution.upload_bytes renderer in
  ignore(get(render_textured renderer[Scene_execution.Scene2_textured,
    Ogpu.Pipeline.Replace,Some gpu_texture,{mesh=mesh_uv 0.1 0.1;state}]));
  let pixel=get(Scene_execution.read_pixels renderer~bytes_per_row:16)in
  if Char.code(Bytes.get pixel 0)<>0x31||Char.code(Bytes.get pixel 1)<>0x82||
     Char.code(Bytes.get pixel 2)<>0xdd||Scene_execution.upload_bytes renderer<>before_film
  then failwith"GPU film was copied through CPU staging or sampled incorrectly";
  let bad_film={gpu_texture with Scene_execution.levels=[|{width=2;height=1;bytes=Bytes.empty}|]}in
  (match render_textured renderer[Scene_execution.Scene2_textured,
      Ogpu.Pipeline.Replace,Some bad_film,{mesh=mesh_uv 0.1 0.1;state}]with
   |Error error when error.Ogpu.Error.kind=Invalid_argument->()
   |_->failwith"mismatched GPU film extent was accepted");
  let foreign_driver = Ogpu.Impl.create_driver () in
  let foreign_device=get(Ogpu.Backend.create_device foreign_driver)in
  let foreign=get(Ogpu.Backend.create_texture foreign_device
    {Ogpu.Types.label=None;width=1;height=1;depth=1;mip_levels=1;
      sample_count=1;format=Rgba8_unorm;usage=[Texture_binding]})in
  let foreign_film={gpu_texture with Scene_execution.gpu=Some foreign}in
  (match render_textured renderer[Scene_execution.Scene2_textured,
      Ogpu.Pipeline.Replace,Some foreign_film,{mesh=mesh_uv 0.1 0.1;state}]with
   |Error error when error.Ogpu.Error.kind=Cross_device->()
   |_->failwith"foreign GPU film was accepted");
  get(Ogpu.Backend.destroy_texture foreign);
  get(Ogpu.Backend.destroy_device foreign_device);
  get(Ogpu.Backend.destroy_texture film);
  (match render_textured renderer[Scene_execution.Scene2_textured,
      Ogpu.Pipeline.Replace,Some gpu_texture,{mesh=mesh_uv 0.1 0.1;state}]with
   |Error error when error.Ogpu.Error.kind=Invalid_argument->()
   |_->failwith"destroyed GPU film was accepted");
  List.iteri(fun index(mesh,sampler,expected)->let texture:Scene_execution.sampled_texture={key="native-mip-chain";levels;sampler;gpu=None}and stable=ref None in List.iter(fun _frame->ignore(get(render_textured renderer[Scene_execution.Scene3_textured,Ogpu.Pipeline.Replace,Some texture,{mesh;state}]));let bytes=get(Scene_execution.read_pixels renderer~bytes_per_row:16)in let actual=Char.code(Bytes.get bytes 0),Char.code(Bytes.get bytes 1),Char.code(Bytes.get bytes 2)in begin if actual<>expected then let r,g,b=actual in failwith(Printf.sprintf"scene execution Metal texture case %d: %d,%d,%d"index r g b)end;let uploaded=Scene_execution.upload_bytes renderer in match !stable with None->stable:=Some uploaded|Some value when value=uploaded->()|Some _->failwith"stable native mip chain reuploaded")frames)cases;
  for index=0 to 64 do
    let sampler={ (sampler()) with Ogpu.Types.label=Some(Printf.sprintf"sampler-eviction-%d"index)}in
    let texture:Scene_execution.sampled_texture={key="native-sampler-eviction";levels;sampler;gpu=None}in
    ignore(get(render_textured renderer[Scene_execution.Scene2_textured,Ogpu.Pipeline.Replace,Some texture,{mesh=mesh_uv 0.1 0.1;state}]))
  done;

  let compacted_texture:Scene_execution.sampled_texture={key="native-sampler-eviction";levels;sampler=sampler~min_filter:Linear~mag_filter:Linear();gpu=None}in
  List.iter(fun _->ignore(get(render_textured renderer[
    Scene_execution.Scene2_textured,Ogpu.Pipeline.Replace,Some compacted_texture,
      {mesh=mesh_uv 0.5 0.5;state}]));
    let bytes=get(Scene_execution.read_pixels renderer~bytes_per_row:16)in
    if (Char.code(Bytes.get bytes 0),Char.code(Bytes.get bytes 1),Char.code(Bytes.get bytes 2))<>(128,128,128) then
      failwith"sampler cache eviction changed the linear sampler result")frames;
  let replacement=[|{Scene_execution.width=2;height=2;bytes=Bytes.make 16 '\255'};{width=1;height=1;bytes=Bytes.of_string"\000\000\255\255"}|]in let replaced:Scene_execution.sampled_texture={key="native-mip-chain";levels=replacement;sampler=sampler();gpu=None}in let before_reload=Scene_execution.upload_bytes renderer in ignore(get(render_textured renderer[Scene_execution.Scene3_textured,Ogpu.Pipeline.Replace,Some replaced,{mesh=mesh_uv 0.1 0.1;state}]));if Scene_execution.upload_bytes renderer<=before_reload then failwith"texture generation replacement was not uploaded";
  (let bytes=get(Scene_execution.read_pixels renderer~bytes_per_row:16)in if (Char.code(Bytes.get bytes 0),Char.code(Bytes.get bytes 1),Char.code(Bytes.get bytes 2))<>(255,255,255) then failwith"replaced texture generation did not sample the new levels");
  let malformed={replaced with Scene_execution.key="malformed";levels=[|{Scene_execution.width=2;height=2;bytes=Bytes.make 15 '\000'}|]}and before_reject=Scene_execution.upload_bytes renderer in begin match render_textured renderer[Scene_execution.Scene3_textured,Ogpu.Pipeline.Replace,Some malformed,{mesh=mesh_uv 0.1 0.1;state}]with Error _ when Scene_execution.upload_bytes renderer=before_reject->()|_->failwith"malformed texture was not rejected atomically"end;
  let corners label=let bytes=get(Scene_execution.read_pixels renderer~bytes_per_row:32)in
    List.iter(fun at->if (Char.code(Bytes.get bytes at),Char.code(Bytes.get bytes(at+1)),Char.code(Bytes.get bytes(at+2)))<>(255,0,128) then failwith("resized pixel "^label))[0;28;7*32;7*32+28]in
  get(Scene_execution.resize renderer{configuration with physical_width=8;physical_height=8});ignore(get(Scene_execution_fixtures.render renderer[{mesh;state={state with viewport=(0,0,8,8);scissor=(0,0,8,8)}}]));
  corners"8x8 draw";
  ignore(get(Scene_execution_fixtures.render renderer[{mesh;state={state with viewport=(0,0,16,16);scissor=(-4,0,32,32)}}]));
  corners"oversized viewport and scissor";
  if List.mem 4 supported then(ignore(get(Scene_execution.submit renderer[sampled 4 {mesh=mesh3;state={state with viewport=(0,0,8,8);scissor=(0,0,8,8)}}]));let captured=get(Scene_execution.read_pixels renderer~bytes_per_row:32)in if Bytes.length captured<>256||Char.code(Bytes.get captured 0)<>255||Char.code(Bytes.get captured 2)<>128 then failwith"resized multisample capture pixel");
  get(Scene_execution.destroy renderer);if live_handles()<>before then failwith"scene execution Metal live delta";print_endline"scene execution Metal: SDL-free draw/resize, exact pixel, zero delta"
