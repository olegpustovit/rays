(* Sampler semantics through the portable render encoder: nearest and linear
   filtering, clamp/repeat/mirror addressing, and an explicit mip level. *)
let get=function Ok value->value|Error error->failwith(Ogpu.Error.to_string error)

let source={|#include <metal_stdlib>
using namespace metal;
struct V { float4 position [[position]]; };
vertex V sample_vertex(uint i [[vertex_id]]) {
  constexpr float2 p[3]={{-1.,-1.},{3.,-1.},{-1.,3.}};
  V v; v.position=float4(p[i],0.,1.); return v;
}
fragment float4 sample_clamp(texture2d<float> t [[texture(0)]], sampler s [[sampler(1)]]) { return t.sample(s,float2(-0.25,0.25)); }
fragment float4 sample_repeat(texture2d<float> t [[texture(0)]], sampler s [[sampler(1)]]) { return t.sample(s,float2(-0.25,0.25)); }
fragment float4 sample_mirror(texture2d<float> t [[texture(0)]], sampler s [[sampler(1)]]) { return t.sample(s,float2(1.75,0.25)); }
fragment float4 sample_linear(texture2d<float> t [[texture(0)]], sampler s [[sampler(1)]]) { return t.sample(s,float2(0.5,0.5)); }
fragment float4 sample_mip(texture2d<float> t [[texture(0)]], sampler s [[sampler(1)]]) { return t.sample(s,float2(0.5),level(1.0)); }
|}

let shader entries bindings=get(Ogpu.Shader.create{backend="metal";label=Some"sampler-pixels";bytes=Bytes.of_string source;entry_points=entries;bindings})
let rgba bytes=[Char.code(Bytes.get bytes 0);Char.code(Bytes.get bytes 1);Char.code(Bytes.get bytes 2);Char.code(Bytes.get bytes 3)]
let close expected actual=List.for_all2(fun a b->abs(a-b)<=1)expected actual

let run () =
  let open Ogpu in
  let driver = Impl.create_driver () in let live_handles = Impl.live_handles in
  let before=live_handles()in
  let device=get(Backend.create_device driver)in
  let vertex=shader[{name="sample_vertex";stage=Shader.Vertex}][]
  and fragment name=shader[{name;stage=Shader.Fragment}][{group=0;binding=0;kind=Sampled_texture;visibility=[Fragment]};{group=0;binding=1;kind=Sampler;visibility=[Fragment]}]in
  let texture_layout=get(Binding.create_layout[{binding=0;kind=Binding.Texture;visibility=[Binding.Fragment]};{binding=1;kind=Binding.Sampler;visibility=[Binding.Fragment]}])in
  let layout=get(Binding.create_pipeline_layout~device:(Backend.device_handle device)~capabilities:(Backend.capabilities device)[0,texture_layout])in
  let make_pipeline entry=
    let descriptor:Pipeline.render_descriptor={backend="metal";label=Some entry;layout;vertex;vertex_entry="sample_vertex";fragment=Some(fragment entry);fragment_entry=Some entry;color_format=Rgba8_unorm;depth_format=No_depth;sample_count=1}in
    entry,get(Backend.create_render_pipeline device descriptor)in
  let pipelines=List.map make_pipeline["sample_clamp";"sample_repeat";"sample_mirror";"sample_linear";"sample_mip"]in
  let sampled=get(Backend.create_texture device{label=Some"sampled";width=2;height=2;depth=1;mip_levels=2;sample_count=1;format=Rgba8_unorm;usage=[Texture_binding;Texture_copy_dst]})
  and target=get(Backend.create_texture device{label=Some"target";width=1;height=1;depth=1;mip_levels=1;sample_count=1;format=Rgba8_unorm;usage=[Texture_binding;Render_attachment;Texture_copy_src]})
  and upload=get(Backend.create_buffer device{label=None;size=768L;usage=[Copy_src;Copy_dst]})
  and queue=get(Backend.create_queue device)in
  let bytes=Bytes.make 768 '\000'in
  Bytes.blit(Bytes.of_string"\xff\x00\x00\xff\x00\xff\x00\xff")0 bytes 0 8;
  Bytes.blit(Bytes.of_string"\x00\x00\xff\xff\xff\xff\xff\xff")0 bytes 256 8;
  Bytes.blit(Bytes.of_string"\xff\xff\x00\xff")0 bytes 512 4;
  get(Backend.write_buffer upload~offset:0L bytes);
  let wait receipt=get(Backend.complete_through queue receipt.Backend.epoch)in
  let commands=get(Backend.begin_commands queue)in
  let blit=get(Backend.blit_encoder commands)in
  get(Backend.buffer_to_texture blit~src:upload~bytes_per_row:256L~bytes_per_image:512L~dst:sampled~extent:{width=2;height=2;depth=1}());
  get(Backend.buffer_to_texture blit~src:upload~offset:512L~bytes_per_row:256L~bytes_per_image:256L~dst:sampled~mip:1~extent:{width=1;height=1;depth=1}());
  get(Backend.end_blit blit);
  wait(get(Backend.commit commands));
  let base:Types.sampler_descriptor={label=None;min_filter=Types.Nearest;mag_filter=Nearest;mip_filter=No_mip;address_u=Clamp_to_edge;address_v=Clamp_to_edge;lod_min=0.;lod_max=2.;max_anisotropy=1}in
  let cases=["sample_clamp",[255;0;0;255],base;
    "sample_repeat",[0;255;0;255],{base with address_u=Repeat};
    "sample_mirror",[255;0;0;255],{base with address_u=Mirror_repeat};
    "sample_linear",[128;128;128;255],{base with min_filter=Linear;mag_filter=Linear};
    "sample_mip",[255;255;0;255],{base with mip_filter=Nearest_mip}]in
  List.iter(fun(entry,expected,descriptor)->
    let pipeline=List.assoc entry pipelines in
    let sampler=get(Backend.create_sampler device descriptor)in
    let commands=get(Backend.begin_commands queue)in
    let encoder=get(Backend.render_encoder commands
      {colors=[{texture=target;resolve=None;load=Clear;store=Store;clear=(0.,0.,0.,0.)}];depth=None})in
    get(Backend.set_render_pipeline encoder pipeline);
    get(Backend.set_stage_texture encoder Fragment~index:0 sampled);
    get(Backend.set_stage_sampler encoder Fragment~index:1 sampler);
    get(Backend.draw encoder~primitive:Triangle_list~first:0~count:3());
    get(Backend.end_render encoder);
    wait(get(Backend.commit commands));
    let actual=rgba(get(Backend.read_texture target~bytes_per_row:4))in
    if not(close expected actual)then failwith(Printf.sprintf"%s sampled %s"entry(String.concat","(List.map string_of_int actual)));
    get(Backend.destroy_sampler sampler))cases;
  get(Backend.destroy_queue queue);get(Backend.destroy_buffer upload);get(Backend.destroy_texture sampled);get(Backend.destroy_texture target);
  List.iter(fun(_,pipeline)->get(Backend.destroy_pipeline pipeline))pipelines;get(Backend.destroy_device device);
  if live_handles()<>before then failwith"sampler pixels live-handle delta";
  print_endline"ogpu_metal sampler pixels: nearest/linear clamp/repeat/mirror/mip exact, zero delta"
