(* Minimal MSL pipelines for every family and blend so the caches under test
   run on a real device. Scene2 families use the argument-buffer
   fragment; textured families sample a texture and sampler directly. *)
let source =
  {|#include <metal_stdlib>
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
  let draws =
    List.map
      (fun draw ->
        {
          Scene_execution.family = Scene_execution.Scene2;
          blend = Ogpu.Pipeline.Replace;
          texture = None;
          auxiliary = None;
          vertex_attributes = None;
          samples = 1;
          draw;
        })
      draws
  in
  Scene_execution.submit ?clear renderer draws

let make device family blend samples =
  let module S = Ogpu.Shader in
  let module B = Ogpu.Binding in
  let fragment_entry, fragment_bindings, layout_entries =
    match family with
    | Scene_execution.Scene2 | Scene2_textured ->
        ( "scene_fragment_argument",
          [ { S.group = 0; binding = 1; kind = Storage_buffer; visibility = [ Fragment ] } ],
          [ { B.binding = 1; kind = Buffer; visibility = [ Fragment ] } ] )
    | Scene3_textured | Ui ->
        ( "scene_fragment_textured",
          [
            { S.group = 0; binding = 1; kind = Sampled_texture; visibility = [ Fragment ] };
            { group = 0; binding = 2; kind = Sampler; visibility = [ Fragment ] };
          ],
          [
            { B.binding = 1; kind = Texture; visibility = [ Fragment ] };
            { binding = 2; kind = Sampler; visibility = [ Fragment ] };
          ] )
    | Scene3_world ->
        ( "scene_fragment_shadow",
          [
            { S.group = 0; binding = 3; kind = Storage_buffer; visibility = [ Fragment ] };
            { group = 0; binding = 4; kind = Sampled_texture; visibility = [ Fragment ] };
            { group = 0; binding = 5; kind = Sampler; visibility = [ Fragment ] };
          ],
          [
            { B.binding = 3; kind = Buffer; visibility = [ Fragment ] };
            { binding = 4; kind = Texture; visibility = [ Fragment ] };
            { binding = 5; kind = Sampler; visibility = [ Fragment ] };
          ] )
    | Scene3 | Scene3_points | Marks -> ("scene_fragment", [], [])
  in
  let vertex_bindings =
    [
      { S.group = 0; binding = 0; kind = Storage_buffer; visibility = [ Vertex ] };
      { group = 0; binding = 6; kind = Storage_buffer; visibility = [ Vertex ] };
    ]
  in
  let vertex =
    get
      (S.create
         {
           backend = "metal";
           label = Some "test-vertex";
           bytes = Bytes.of_string source;
           entry_points = [ { name = "scene_vertex"; stage = Vertex } ];
           bindings = vertex_bindings;
         })
  in
  let fragment =
    get
      (S.create
         {
           backend = "metal";
           label = Some "test-fragment";
           bytes = Bytes.of_string source;
           entry_points = [ { name = fragment_entry; stage = Fragment } ];
           bindings = fragment_bindings;
         })
  in
  let group =
    get
      (B.create_layout
         ([
            { B.binding = 0; kind = Buffer; visibility = [ Vertex ] };
            { binding = 6; kind = Buffer; visibility = [ Vertex ] };
          ]
         @ layout_entries))
  in
  let layout =
    get
      (B.create_pipeline_layout
         ~device:(Ogpu.Backend.device_handle device)
         ~capabilities:(Ogpu.Backend.capabilities device)
         [ (0, group) ])
  in
  let depth_format =
    match family with
    | Scene_execution.Scene2 | Scene2_textured | Ui | Marks -> Ogpu.Pipeline.No_depth
    | Scene3 | Scene3_points | Scene3_textured | Scene3_world -> Depth32_float
  in
  Ogpu.Backend.create_render_pipeline ~blend
    ~topology:(if family = Scene3_points then Ogpu.Render_pass.Point_list else Triangle_list)
    ~indirect:
      (match family with Scene2 | Scene2_textured | Scene3 | Scene3_points -> true | _ -> false)
    device
    {
      backend = "metal";
      label = Some "test-pipelines";
      layout;
      vertex;
      vertex_entry = "scene_vertex";
      fragment = Some fragment;
      fragment_entry = Some fragment_entry;
      color_format = Rgba8_unorm;
      depth_format;
      sample_count = samples;
    }

(* The production pipelines (generated shaders, production binding slots), so a
   Metal test cannot drift from what the renderer binds. *)
let create_offscreen driver configuration =
  Renderer_pipeline.create_renderer ~backend:"metal" ~offscreen:true ~driver ~configuration ()

(* Production-ABI geometry covering the whole target with one triangle.
   Scene2_textured: 68-byte records (doubles x, y at 0, 8; colour word at 48; uv doubles at 52, 60). *)
let corners = [ (-1., -1.); (3., -1.); (-1., 3.) ]

let indices =
  let b = Bytes.make 12 '\000' in
  Bytes.set_int32_le b 4 1l;
  Bytes.set_int32_le b 8 2l;
  b

let mesh key vertices : Scene_execution.mesh =
  {
    key = Host key;
    vertices;
    vertex_count = 3;
    indices;
    index_count = 3;
    primitive = Ogpu.Render_pass.Triangle_list;
  }

let scene2_vertices ?(color = 0xffffffffl) ?(uv = fun _ _ -> (0., 0.)) () =
  let b = Bytes.make (68 * 3) '\000' in
  let double at x = Bytes.set_int64_le b at (Int64.bits_of_float x) in
  List.iteri
    (fun i (x, y) ->
      let o = i * 68 in
      double o x;
      double (o + 8) y;
      Bytes.set_int32_le b (o + 48) color;
      let u, v = uv x y in
      double (o + 52) u;
      double (o + 60) v)
    corners;
  b

(* A plain [Scene2] draw carries 16-byte records (float x, y, colour word) that the renderer expands. *)
let scene2_plain_vertices ?(color = 0xffffffffl) () =
  let b = Bytes.make (16 * 3) '\000' in
  List.iteri
    (fun i (x, y) ->
      Bytes.set_int32_le b (i * 16) (Int32.bits_of_float x);
      Bytes.set_int32_le b ((i * 16) + 4) (Int32.bits_of_float y);
      Bytes.set_int32_le b ((i * 16) + 8) color)
    corners;
  b

let scene2_mesh ?color ?uv key = mesh key (scene2_vertices ?color ?uv ())

(* Scene3: 24-byte records (position, normal as floats) plus the 12-byte attribute record
   (colour word, uv floats) a vertex. *)
let scene3_mesh ?(color = 0xffffffffl) ?(uv = (0., 0.)) key =
  let v = Bytes.make (24 * 3) '\000' and a = Bytes.make (12 * 3) '\000' in
  let float b at x = Bytes.set_int32_le b at (Int32.bits_of_float x) in
  List.iteri
    (fun i (x, y) ->
      float v (i * 24) x;
      float v ((i * 24) + 4) y;
      float v ((i * 24) + 20) 1.;
      Bytes.set_int32_le a (i * 12) color;
      float a ((i * 12) + 4) (fst uv);
      float a ((i * 12) + 8) (snd uv))
    corners;
  (mesh key v, (key ^ ":attributes", a))

(* A Scene3 block: identity transforms, unlit white material, so a draw shows its vertex colour times texel. *)
let scene3_surface () =
  let b = Bytes.make 5456 '\000' in
  let set i x = Bytes.set_int32_le b (i * 4) (Int32.bits_of_float x) in
  for m = 0 to 2 do
    for d = 0 to 3 do
      set ((m * 16) + (d * 5)) 1.
    done
  done;
  List.iter (fun i -> set i 1.) [ 52; 53; 54; 55; 59; 69; 70; 71; 72 ];
  set 50 (-1.);
  b
