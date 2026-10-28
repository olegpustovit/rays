open Ogpu_metal_native

let get = function Ok x -> x | Error e -> failwith (Ogpu.Error.to_string e)
let metal = function Ok x -> x | Error e -> failwith (Format.asprintf "%a" Metal.pp_error e)

let source =
  {|#include <metal_stdlib>
using namespace metal;
struct V { float4 position [[position]]; float2 uv; };
vertex V scene_vertex(uint i [[vertex_id]]) { constexpr float2 p[3]={{-1.,-1.},{3.,-1.},{-1.,3.}}; V v; v.position=float4(p[i],0.,1.); v.uv=(p[i]+1.)*.5; return v; }
struct Args { texture2d<float, access::sample> image [[id(0)]]; sampler sampling [[id(1)]]; };
fragment float4 scene_fragment_argument(V v [[stage_in]], constant Args &args [[buffer(1)]]) { return args.image.sample(args.sampling, v.uv); }
|}

let run () =
  match Device.system_default () with
  | Error _ -> print_endline "pipeline argument buffer: skipped (no device)"
  | Ok device ->
      let before = metal (Metal.Release_queue.stats ()) in
      let shader entries bindings =
        get
          (Ogpu.Shader.create
             {
               backend = "metal";
               label = Some "argument";
               bytes = Bytes.of_string source;
               entry_points = entries;
               bindings;
             })
      in
      let vertex = shader [ { name = "scene_vertex"; stage = Vertex } ] []
      and fragment =
        shader
          [ { name = "scene_fragment_argument"; stage = Fragment } ]
          [ { group = 0; binding = 1; kind = Storage_buffer; visibility = [ Fragment ] } ]
      in
      let group =
        get
          (Ogpu.Binding.create_layout
             [ { binding = 1; kind = Ogpu.Binding.Buffer; visibility = [ Ogpu.Binding.Fragment ] } ])
      in
      let layout =
        get
          (Ogpu.Binding.create_pipeline_layout ~device:(Device.Private.handle device)
             ~capabilities:(Device.capabilities device)
             [ (0, group) ])
      in
      let descriptor : Ogpu.Pipeline.render_descriptor =
        {
          backend = "metal";
          label = Some "argument";
          layout;
          vertex;
          vertex_entry = "scene_vertex";
          fragment = Some fragment;
          fragment_entry = Some "scene_fragment_argument";
          color_format = Rgba8_unorm;
          depth_format = No_depth;
          sample_count = 1;
        }
      in
      (* Without [indirect] no fragment function is retained for encoders. *)
      let direct = get (Pipeline.create_render_owned device descriptor) in
      (match Pipeline.Private.argument_encoder direct ~buffer_index:1L with
      | Error e when e.Ogpu.Error.kind = Ogpu.Error.Invalid_state -> ()
      | _ -> failwith "direct pipeline offered an argument encoder");
      let pipeline = get (Pipeline.create_render_owned ~indirect:true device descriptor) in
      let encoder = get (Pipeline.Private.argument_encoder pipeline ~buffer_index:1L) in
      if Metal.Shader_argument_encoder.encoded_length encoder <= 0L then
        failwith "argument encoder length";
      (match Pipeline.Private.argument_encoder pipeline ~buffer_index:(-1L) with
      | Error e when e.Ogpu.Error.kind = Ogpu.Error.Invalid_argument -> ()
      | _ -> failwith "malformed argument binding accepted");
      metal (Metal.Shader_argument_encoder.destroy encoder);
      get (Pipeline.destroy pipeline);
      get (Pipeline.destroy direct);
      get (Device.destroy device);
      ignore (metal (Metal.Release_queue.drain ()));
      let after = metal (Metal.Release_queue.stats ()) in
      if after.live_handles <> before.live_handles - 1 then
        failwith "argument pipeline live-handle delta";
      print_endline
        "pipeline argument buffer: owned indirect pipeline retains its fragment encoder, zero delta"
