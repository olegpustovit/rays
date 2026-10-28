open Metal
open Test_metal_support

let render_shader_source =
  {|
#include <metal_stdlib>
using namespace metal;

struct PrismelVertexOut {
  float4 position [[position]];
};

vertex PrismelVertexOut prismel_vertex(
    uint vertex_id [[vertex_id]],
    constant float2 &offset [[buffer(0)]]) {
  constexpr float2 positions[3] = {
    float2(-1.0f, -1.0f),
    float2(3.0f, -1.0f),
    float2(-1.0f, 3.0f)
  };
  PrismelVertexOut result;
  result.position = float4(positions[vertex_id] + offset, 0.0f, 1.0f);
  return result;
}

vertex void prismel_vertex_only(uint vertex_id [[vertex_id]]) {
  (void)vertex_id;
}

fragment float4 prismel_fragment(constant float4 &tint [[buffer(1)]]) {
  return tint;
}

kernel void prismel_kernel(device uint *out [[buffer(0)]]) { out[0] = 113; }
|}

let buffer_binding (binding : Binding.t) =
  match binding.kind with
  | Binding.Buffer_binding layout -> (binding.name, binding.index, binding.access, layout.data_type)
  | _ -> fail "render reflection reports %s as something other than a buffer" binding.name

(* The Metal 4 compiler and the render pipelines it makes: what it refuses
   before compiling, what Metal refuses, the reflection it copies out, and
   the pixel a compiled pipeline draws. *)
let run () =
  match Device.system_default () with
  | Error _ -> print_endline "metal render pipeline: skipped (no device)"
  | Ok device ->
      if not (get (Device.supports_family device Device.Metal4)) then begin
        without_handles "Metal 4 compiler on an older device" (fun () ->
          reject Unsupported (Compiler.create device));
        get (Device.destroy device);
        print_endline "metal render pipeline: Metal 4 unsupported, typed rejection"
      end
      else begin
        finalizer_releases "compiler" ~count:1 (fun () -> ignore (get (Compiler.create device)));
        let compiler = get (Compiler.create device) in
        reject Parent_has_dependents (Device.destroy device);
        reject Wrong_domain (Domain.spawn (fun () -> Compiler.create device) |> Domain.join);
        let library = get (Library.compile_source ~device render_shader_source) in
        let create ?label ?fragment ?reflection ?raster_sample_count ?color_formats
            ?color_attachments ?primitive_topology ?support_indirect_command_buffers vertex =
          Compiler.create_render_pipeline ?label ?fragment ?reflection ?raster_sample_count
            ?color_formats ?color_attachments ?primitive_topology
            ?support_indirect_command_buffers compiler ~library ~vertex
        in
        without_handles "invalid Metal 4 render inputs" (fun () ->
          reject Invalid_argument (create "");
          reject Invalid_argument (create "missing_vertex");
          reject Invalid_argument
            (create ~label:"invalid\000render" ~fragment:"prismel_fragment" "prismel_vertex");
          reject Invalid_argument (create ~fragment:"missing_fragment" "prismel_vertex");
          (* Rasterization needs a fragment function and one to eight colours. *)
          reject Invalid_argument (create "prismel_vertex");
          reject Invalid_argument (create ~color_formats:[] "prismel_vertex_only");
          reject Invalid_argument
            (create ~color_formats:[] ~fragment:"prismel_fragment" "prismel_vertex");
          reject Invalid_argument
            (create ~raster_sample_count:0 ~fragment:"prismel_fragment" "prismel_vertex");
          reject Invalid_argument
            (create
               ~color_formats:(List.init 9 (fun _ -> Texture.Bgra8_unorm))
               ~fragment:"prismel_fragment" "prismel_vertex");
          reject Invalid_argument
            (create ~color_formats:[ Texture.Depth32_float ] ~fragment:"prismel_fragment"
               "prismel_vertex");
          reject Invalid_argument
            (create ~color_formats:[ Texture.Bgra8_unorm ]
               ~color_attachments:[ Render_pipeline.color_attachment Texture.Bgra8_unorm ]
               ~fragment:"prismel_fragment" "prismel_vertex");
          reject Invalid_argument
            (create
               ~color_attachments:
                 [ Render_pipeline.color_attachment
                     ~write_mask:[ Render_pipeline.Write_red; Render_pipeline.Write_red ]
                     Texture.Bgra8_unorm
                 ]
               ~fragment:"prismel_fragment" "prismel_vertex");
          (* A stage given a function of another stage is Metal's to refuse. *)
          reject Native_error (create ~fragment:"prismel_fragment" "prismel_fragment");
          reject Native_error (create ~fragment:"prismel_vertex" "prismel_vertex");
          reject Native_error (create ~fragment:"prismel_fragment" "prismel_kernel"));
        let render_pipeline =
          get
            (create ~label:"metal4 reflected render" ~fragment:"prismel_fragment" ~reflection:true
               ~primitive_topology:Render_pipeline.Triangle ~support_indirect_command_buffers:true
               "prismel_vertex")
        in
        if not (get (Render_pipeline.supports_indirect_command_buffers render_pipeline)) then
          fail "a pipeline compiled for indirect command buffers denies it";
        (match Render_pipeline.reflection render_pipeline with
         | Some reflection ->
             (match List.map buffer_binding reflection.vertex with
              | [ ("offset", 0L, Binding.Read_only, Shader_type.Vector (Shader_type.Float, 2)) ] -> ()
              | _ -> fail "Metal 4 vertex reflection is not the one float2 buffer at index 0");
             (match List.map buffer_binding reflection.fragment with
              | [ ("tint", 1L, Binding.Read_only, Shader_type.Vector (Shader_type.Float, 4)) ] -> ()
              | _ -> fail "Metal 4 fragment reflection is not the one float4 buffer at index 1")
         | None -> fail "Metal 4 render reflection is missing");
        let plain_pipeline =
          get (create ~fragment:"prismel_fragment" ~color_formats:[ Texture.Rgba8_unorm ] "prismel_vertex")
        in
        if Render_pipeline.reflection plain_pipeline <> None then
          fail "render reflection was kept without being asked for";
        if get (Render_pipeline.supports_indirect_command_buffers plain_pipeline) then
          fail "a pipeline compiled without indirect command buffers claims them";
        finalizer_releases "render pipeline" ~count:1 (fun () ->
          ignore (get (create ~fragment:"prismel_fragment" "prismel_vertex")));
        (* One covering triangle, the tint in a fragment buffer: the pipeline's
           output is the tint, to the byte. *)
        let queue = get (Command_queue.create device) in
        let target =
          get
            (Texture.create ~device
               (Texture.descriptor_2d ~storage:Buffer.Shared ~usage:[ Texture.Render_target ]
                  ~format:Texture.Rgba8_unorm ~width:2 ~height:2 ()))
        in
        let offset = get (Buffer.create ~device ~length:16L ~storage:Buffer.Shared ()) in
        get (Buffer.write_bytes offset ~dst_offset:0L (float_bytes [| 0.; 0.; 0.; 0. |]));
        let tint = get (Buffer.create ~device ~length:16L ~storage:Buffer.Shared ()) in
        get (Buffer.write_bytes tint ~dst_offset:0L (float_bytes [| 0.25; 0.5; 0.75; 1. |]));
        let commands = get (Command_buffer.create queue ()) in
        let encoder = get (Render_encoder.create commands ~target ()) in
        (* The reflected pipeline targets Bgra8, this pass Rgba8. *)
        reject Invalid_argument (Render_encoder.set_pipeline encoder render_pipeline);
        get (Render_encoder.set_pipeline encoder plain_pipeline);
        reject Parent_has_dependents (Render_pipeline.destroy plain_pipeline);
        get (Render_encoder.set_vertex_buffer encoder ~index:0 ~offset:0L offset);
        get (Render_encoder.set_fragment_buffer encoder ~index:1 ~offset:0L tint);
        get (Render_encoder.draw_triangles encoder ~first:0 ~count:3 ());
        (* The same triangle again, its counts read from a buffer when the draw runs (index
           count 3, one instance); a misaligned argument offset is refused before encoding. *)
        let int_bytes values = let b = Bytes.create (4 * Array.length values) in
          Array.iteri (fun i v -> Bytes.set_int32_le b (4 * i) (Int32.of_int v)) values; b in
        let indices = get (Buffer.create ~device ~length:12L ~storage:Buffer.Shared ()) in
        get (Buffer.write_bytes indices ~dst_offset:0L (int_bytes [| 0; 1; 2 |]));
        let arguments = get (Buffer.create ~device ~length:24L ~storage:Buffer.Shared ()) in
        get (Buffer.write_bytes arguments ~dst_offset:0L (int_bytes [| 0; 3; 1; 0; 0; 0 |]));
        reject Invalid_argument (Render_encoder.draw_indexed_indirect encoder ~primitive:Render_encoder.Triangle
          ~index_type:Uint32 ~index_buffer:indices ~index_offset:0L ~arguments ~arguments_offset:2L);
        get (Render_encoder.draw_indexed_indirect encoder ~primitive:Render_encoder.Triangle
          ~index_type:Uint32 ~index_buffer:indices ~index_offset:0L ~arguments ~arguments_offset:4L);
        get (Render_encoder.end_encoding encoder);
        complete commands;
        get (Buffer.destroy arguments);
        get (Buffer.destroy indices);
        let pixels =
          get
            (Texture.read_bytes target ~region:(full_region 2 2) ~mip_level:0 ~slice:0
               ~bytes_per_row:8 ~bytes_per_image:16)
        in
        for pixel = 0 to 3 do
          if Bytes.sub_string pixels (pixel * 4) 4 <> "\x40\x80\xbf\xff" then
            fail "compiled pipeline drew %02x,%02x,%02x,%02x at pixel %d"
              (Bytes.get_uint8 pixels (pixel * 4))
              (Bytes.get_uint8 pixels ((pixel * 4) + 1))
              (Bytes.get_uint8 pixels ((pixel * 4) + 2))
              (Bytes.get_uint8 pixels ((pixel * 4) + 3))
              pixel
        done;
        get (Buffer.destroy tint);
        get (Buffer.destroy offset);
        get (Texture.destroy target);
        get (Command_queue.destroy queue);
        get (Render_pipeline.destroy plain_pipeline);
        get (Render_pipeline.destroy render_pipeline);
        reject Destroyed (Render_pipeline.supports_indirect_command_buffers render_pipeline);
        get (Render_pipeline.destroy render_pipeline);
        (* A pipeline outlives the library and compiler that made it. *)
        let survivor = get (create ~fragment:"prismel_fragment" "prismel_vertex") in
        get (Library.destroy library);
        reject Destroyed (create ~fragment:"prismel_fragment" "prismel_vertex");
        get (Compiler.destroy compiler);
        get (Compiler.destroy compiler);
        ignore (get (Render_pipeline.supports_indirect_command_buffers survivor));
        get (Render_pipeline.destroy survivor);
        get (Device.destroy device);
        ignore (settle_finalizers ~expected_live:0);
        print_endline "metal render pipeline safe: ok"
      end
