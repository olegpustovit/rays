open Ogpu_metal_native

let get = function Ok x -> x | Error e -> failwith (Ogpu.Error.to_string e)
let get_metal = function Ok x -> x | Error e -> failwith (Format.asprintf "%a" Metal.pp_error e)

let expect kind = function
  | Error e when e.Ogpu.Error.kind = kind -> ()
  | Error e -> failwith ("wrong rejection: " ^ Ogpu.Error.to_string e)
  | Ok _ -> failwith "wrong rejection: accepted"

let layout device entries =
  let group = get (Ogpu.Binding.create_layout entries) in
  get
    (Ogpu.Binding.create_pipeline_layout ~device:(Device.Private.handle device)
       ~capabilities:(Device.capabilities device)
       [ (0, group) ])

let shader ~label ~bytes ~entries ~bindings =
  get
    (Ogpu.Shader.create
       {
         backend = "metal";
         label = Some label;
         bytes = Bytes.of_string bytes;
         entry_points = entries;
         bindings;
       })

let compute_source =
  {|#include <metal_stdlib>
using namespace metal;
kernel void mapped_compute(device uint *values [[buffer(0)]], uint i [[thread_position_in_grid]]) { values[i] = values[i] * 2 + 3; }
|}

let render_source =
  {|#include <metal_stdlib>
using namespace metal;
struct V { float4 position [[position]]; };
vertex V mapped_vertex(uint i [[vertex_id]]) { constexpr float2 p[3]={{-1.,-1.},{3.,-1.},{-1.,3.}}; V v; v.position=float4(p[i],0.,1.); return v; }
fragment float4 mapped_fragment() { return float4(0.125,0.5,0.875,1.); }
|}

let run () =
  match Device.system_default () with
  | Error _ -> print_endline "ogpu_metal pipeline: skipped (no device)"
  | Ok device ->
      let other = get (Device.system_default ()) in
      let before = get_metal (Metal.Release_queue.stats ()) in
      let compute_binding : Ogpu.Shader.binding =
        { group = 0; binding = 0; kind = Storage_buffer; visibility = [ Compute ] }
      in
      let compute_shader =
        shader ~label:"mapped-compute" ~bytes:compute_source
          ~entries:[ { name = "mapped_compute"; stage = Compute } ]
          ~bindings:[ compute_binding ]
      in
      let library = get (Library.create device compute_shader) in
      let compute =
        get
          (Pipeline.create_compute_from_library device library ~entry:"mapped_compute" ~constants:[]
             ~interface:[ compute_binding ])
      in
      expect Ogpu.Error.Invalid_argument
        (Pipeline.create_compute_from_library device library ~entry:"mapped_compute" ~constants:[]
           ~interface:[ { compute_binding with kind = Sampled_texture } ]);
      (match
         Pipeline.create_compute_from_library device library ~entry:"absent" ~constants:[]
           ~interface:[]
       with
      | Ok _ -> failwith "absent entry accepted"
      | Error _ -> ());
      let invalid_shader =
        shader ~label:"invalid-msl" ~bytes:"this is not Metal"
          ~entries:[ { name = "bad"; stage = Compute } ]
          ~bindings:[]
      in
      (match Library.create device invalid_shader with
      | Ok _ -> failwith "invalid MSL compiled"
      | Error _ -> ());
      let render_shader =
        shader ~label:"mapped-render" ~bytes:render_source
          ~entries:
            [
              { name = "mapped_vertex"; stage = Vertex };
              { name = "mapped_fragment"; stage = Fragment };
            ]
          ~bindings:[]
      in
      let render_layout = layout device [] in
      let render_descriptor : Ogpu.Pipeline.render_descriptor =
        {
          backend = "metal";
          label = Some "mapped-render";
          layout = render_layout;
          vertex = render_shader;
          vertex_entry = "mapped_vertex";
          fragment = Some render_shader;
          fragment_entry = Some "mapped_fragment";
          color_format = Rgba8_unorm;
          depth_format = No_depth;
          sample_count = 1;
        }
      in
      let expected =
        [
          ( Ogpu.Pipeline.Replace,
            ( Metal.Render_pipeline.Blend_disabled,
              Metal.Render_pipeline.Blend_one,
              Metal.Render_pipeline.Blend_zero,
              Metal.Render_pipeline.Blend_add ) );
          ( Alpha,
            ( Metal.Render_pipeline.Blend_enabled,
              Metal.Render_pipeline.Blend_source_alpha,
              Metal.Render_pipeline.Blend_one_minus_source_alpha,
              Metal.Render_pipeline.Blend_add ) );
          ( Add,
            ( Metal.Render_pipeline.Blend_enabled,
              Metal.Render_pipeline.Blend_one,
              Metal.Render_pipeline.Blend_one,
              Metal.Render_pipeline.Blend_add ) );
          ( Multiply,
            ( Metal.Render_pipeline.Blend_enabled,
              Metal.Render_pipeline.Blend_destination_color,
              Metal.Render_pipeline.Blend_zero,
              Metal.Render_pipeline.Blend_add ) );
          ( Screen,
            ( Metal.Render_pipeline.Blend_enabled,
              Metal.Render_pipeline.Blend_one_minus_destination_color,
              Metal.Render_pipeline.Blend_one,
              Metal.Render_pipeline.Blend_add ) );
          ( Subtract,
            ( Metal.Render_pipeline.Blend_enabled,
              Metal.Render_pipeline.Blend_one,
              Metal.Render_pipeline.Blend_one,
              Metal.Render_pipeline.Blend_reverse_subtract ) );
        ]
      in
      List.iter
        (fun (blend, (_enabled, _source, _destination, _operation)) ->
          let value = get (Pipeline.create_render_owned ~blend device render_descriptor) in
          get (Pipeline.destroy value))
        expected;
      get (Pipeline.destroy compute);
      get (Library.destroy library);
      get (Device.destroy device);
      get (Device.destroy other);
      ignore (get_metal (Metal.Release_queue.drain ()));
      let after = get_metal (Metal.Release_queue.stats ()) in
      if after.live_handles <> before.live_handles - 2 then failwith "pipeline live-handle delta";
      print_endline
        "ogpu_metal pipeline: reflected compute from library, six owned render blends, typed \
         rejections, zero live-handle delta"
