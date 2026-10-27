(* Minimal MSL pipelines for every family and blend so the caches under test
   run on a real device. Scene2 families use the argument-buffer
   fragment; textured families sample a texture and sampler directly. *)
let source = {|#include <metal_stdlib>
using namespace metal;
struct Out { float4 position [[position]]; float2 uv; };
vertex Out scene_vertex(uint i [[vertex_id]], const device uchar *input [[buffer(0)]], const device float *affine [[buffer(6)]]) {
  constexpr float2 p[3]={{-1.,-1.},{3.,-1.},{-1.,3.}};
  float nudge = float(input[0]) * 0. + affine[0] * 0.;
  Out v; v.position=float4(p[i % 3] + nudge,0.,1.); v.uv=(p[i % 3]+1.)*.5; return v; }
fragment float4 scene_fragment(Out v [[stage_in]]){return float4(0.25,0.5,0.75,1.);}
struct Scene2_arguments { texture2d<float, access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };
fragment float4 scene_fragment_argument(Out v [[stage_in]], constant Scene2_arguments &args [[buffer(1)]]) { return args.image.sample(args.sampling, v.uv); }
fragment float4 scene_fragment_textured(Out v [[stage_in]], texture2d<float> image [[texture(1)]], sampler sampling [[sampler(2)]]) { return image.sample(sampling, v.uv); }
fragment float4 scene_fragment_shadow(Out v [[stage_in]], const device float *p [[buffer(3)]], texture2d<float> d [[texture(4)]], sampler s [[sampler(5)]]) { return d.sample(s, v.uv) * p[0]; }
|}

let get = function Ok value -> value | Error error -> failwith (Ogpu.Error.to_string error)

let render ?clear renderer draws =
  let draws = List.map (fun draw ->
    { Scene_execution.family = Scene_execution.Scene2;
      blend = Ogpu.Pipeline.Replace; texture = None; auxiliary = None;
      vertex_attributes = None; samples = 1; draw }) draws in
  Scene_execution.submit ?clear renderer draws

let make device family blend samples =
  let module S = Ogpu.Shader in
  let module B = Ogpu.Binding in
  let fragment_entry, fragment_bindings, layout_entries =
    match family with
    | Scene_execution.Scene2 | Scene2_textured ->
        "scene_fragment_argument",
        [ { S.group = 0; binding = 1; kind = Storage_buffer; visibility = [ Fragment ] } ],
        [ { B.binding = 1; kind = Buffer; visibility = [ Fragment ] } ]
    | Scene3_textured | Ui ->
        "scene_fragment_textured",
        [ { S.group = 0; binding = 1; kind = Sampled_texture; visibility = [ Fragment ] };
          { group = 0; binding = 2; kind = Sampler; visibility = [ Fragment ] } ],
        [ { B.binding = 1; kind = Texture; visibility = [ Fragment ] };
          { binding = 2; kind = Sampler; visibility = [ Fragment ] } ]
    | Scene3_world ->
        "scene_fragment_shadow",
        [ { S.group = 0; binding = 3; kind = Storage_buffer; visibility = [ Fragment ] };
          { group = 0; binding = 4; kind = Sampled_texture; visibility = [ Fragment ] };
          { group = 0; binding = 5; kind = Sampler; visibility = [ Fragment ] } ],
        [ { B.binding = 3; kind = Buffer; visibility = [ Fragment ] };
          { binding = 4; kind = Texture; visibility = [ Fragment ] };
          { binding = 5; kind = Sampler; visibility = [ Fragment ] } ]
    | Scene3 | Scene3_points | Marks -> "scene_fragment", [], []
  in
  let vertex_bindings =
    [ { S.group = 0; binding = 0; kind = Storage_buffer; visibility = [ Vertex ] };
      { group = 0; binding = 6; kind = Storage_buffer; visibility = [ Vertex ] } ]
  in
  let vertex = get (S.create { backend = "metal"; label = Some "test-vertex"; bytes = Bytes.of_string source;
    entry_points = [ { name = "scene_vertex"; stage = Vertex } ]; bindings = vertex_bindings }) in
  let fragment = get (S.create { backend = "metal"; label = Some "test-fragment"; bytes = Bytes.of_string source;
    entry_points = [ { name = fragment_entry; stage = Fragment } ]; bindings = fragment_bindings }) in
  let group = get (B.create_layout
    ([ { B.binding = 0; kind = Buffer; visibility = [ Vertex ] }; { binding = 6; kind = Buffer; visibility = [ Vertex ] } ]
     @ layout_entries)) in
  let layout = get (B.create_pipeline_layout ~device:(Ogpu.Backend.device_handle device)
    ~capabilities:(Ogpu.Backend.capabilities device) [ 0, group ]) in
  let depth_format = match family with
    | Scene_execution.Scene2 | Scene2_textured | Ui | Marks -> Ogpu.Pipeline.No_depth
    | Scene3 | Scene3_points | Scene3_textured | Scene3_world -> Depth32_float
    in
  Ogpu.Backend.create_render_pipeline ~blend
    ~topology:(if family = Scene3_points then Ogpu.Render_pass.Point_list else Triangle_list)
    ~indirect:(match family with
      | Scene2 | Scene2_textured | Scene3 | Scene3_points -> true
      | _ -> false) device
    { backend = "metal"; label = Some "test-pipelines"; layout; vertex; vertex_entry = "scene_vertex";
      fragment = Some fragment; fragment_entry = Some fragment_entry; color_format = Rgba8_unorm;
      depth_format; sample_count = samples }

let create_offscreen driver configuration =
  Scene_execution.create ~offscreen:true driver configuration make
