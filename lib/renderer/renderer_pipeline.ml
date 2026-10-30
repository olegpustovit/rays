open Ogpu.Types
open Ogpu.Shader
open Ogpu.Binding
open Ogpu.Pipeline
open Ogpu.Render_pass
open Scene_execution

let source_scene3 = Metal_scene3_source.source
let source_scene3_points = Metal_scene3_points_source.source
let source_scene3_textured = Metal_scene3_textured_source.source
let source_scene3_world = Metal_scene3_world_source.source
let source_scene3_sun_depth = Metal_scene3_sun_depth_source.source

let make_sun_depth_pipeline backend backend_device =
  let buffer binding =
    { Ogpu.Shader.group = 0; binding; kind = Storage_buffer; visibility = [ Vertex ] }
  in
  let artifact bindings =
    Ogpu.Shader.create
      {
        backend;
        label = Some "scene-sun-depth";
        bytes = Bytes.of_string source_scene3_sun_depth;
        entry_points =
          [ { name = "sun_vertex"; stage = Vertex }; { name = "sun_fragment"; stage = Fragment } ];
        bindings;
      }
  in
  let ( let* ) = Result.bind in
  let* vertex = artifact (List.map buffer [ 0; 2; 3; 4 ]) in
  let* fragment = artifact [] in
  let* bindings =
    Ogpu.Binding.create_layout
      (List.map
         (fun binding -> { Ogpu.Binding.binding; kind = Buffer; visibility = [ Vertex ] })
         [ 0; 2; 3; 4 ])
  in
  let* layout =
    Ogpu.Binding.create_pipeline_layout
      ~device:(Ogpu.Backend.device_handle backend_device)
      ~capabilities:(Ogpu.Backend.capabilities backend_device)
      [ (0, bindings) ]
  in
  Ogpu.Backend.create_render_pipeline ~blend:Replace ~topology:Triangle_list ~indirect:false
    backend_device
    {
      backend;
      label = Some "scene-sun-depth";
      layout;
      vertex;
      vertex_entry = "sun_vertex";
      fragment = Some fragment;
      fragment_entry = Some "sun_fragment";
      color_format = Rgba8_unorm;
      depth_format = Depth32_float;
      sample_count = 1;
    }

let create_renderer ~backend ?device ?gpu_vertices ~offscreen ~driver ~configuration () =
  if backend <> "metal" then
    Error
      (Ogpu.Error.make "Renderer.create" Ogpu.Error.Unsupported
         ("no renderer shaders for backend " ^ backend))
  else
    let make_sun_depth_pipeline device = make_sun_depth_pipeline backend device in
    let make_pipeline backend_device family blend samples =
      (* Every pipeline is generated (shaders/*.rays) and binds in its Lisp declaration order: buffer 0
       and the vertex buffers, the one buffer both stages read, then the fragment's resources. *)
      let texture t = [ (t, `Texture); (t + 1, `Sampler) ] in
      let scene3 = ([ 1; 3; 4; 5 ], [ 2 ]) in
      let source, (vertex_entry, fragment_entry), (vertex_buffers, shared), fragment_resources =
        match family with
        | Scene_execution.Scene2 ->
            (* the vertex words 0, the constants 1 *)
            (Renderer_shaders.geometry, ("geometry_vertex", "geometry_fragment"), ([ 1 ], []), [])
        | Scene2_textured ->
            (* the constants 0, the texture's argument buffer 7 *)
            (Renderer_shaders.quad, ("quad_vertex", "quad_fragment"), ([], []), [ (7, `Buffer) ])
        (* records as floats 0 (and normals 1: the records again, or a device's normals), surface 2,
           instances 3, attributes 4 and 5 *)
        | Scene3 -> (source_scene3, ("scene3_vertex", "scene3_fragment"), scene3, [])
        | Scene3_points ->
            (source_scene3_points, ("scene3_points_vertex", "scene3_fragment"), scene3, [])
        | Scene3_textured ->
            ( source_scene3_textured,
              ("scene3_vertex", "scene3_textured_fragment"),
              scene3,
              texture 6 )
        | Scene3_world ->
            (* the World block 6, image 7 (8), specular 9 (10), camera map 11 (12), sun map 13 *)
            ( source_scene3_world,
              ("scene3_vertex", "world_fragment"),
              scene3,
              ((6, `Buffer) :: texture 7) @ texture 9 @ texture 11 @ [ (13, `Texture) ] )
        | Ui -> (Renderer_shaders.ui, ("ui_vertex", "ui_fragment"), ([ 1; 2 ], []), texture 3)
        | Marks ->
            ( Renderer_shaders.marks,
              ("marks_vertex", "marks_fragment"),
              ([ 1; 2; 3; 4; 5; 6 ], []),
              [] )
      in
      let resources =
        List.map (fun binding -> (binding, `Buffer, `Vertex)) (0 :: vertex_buffers)
        @ List.map (fun binding -> (binding, `Buffer, `Both)) shared
        @ List.map (fun (binding, kind) -> (binding, kind, `Fragment)) fragment_resources
      in
      let artifact stage =
        Ogpu.Shader.create
          {
            backend;
            label = Some "runtime-next";
            bytes = Bytes.of_string source;
            entry_points =
              [
                { name = vertex_entry; stage = Vertex }; { name = fragment_entry; stage = Fragment };
              ];
            bindings =
              List.filter_map
                (fun (binding, kind, visibility) ->
                  if visibility = `Fragment <> (stage = Fragment) then None
                  else
                    Some
                      {
                        Ogpu.Shader.group = 0;
                        binding;
                        kind =
                          (match kind with
                          | `Buffer -> Storage_buffer
                          | `Texture -> Sampled_texture
                          | `Sampler -> Sampler);
                        visibility =
                          (match visibility with
                          | `Vertex -> [ Vertex ]
                          | `Both -> [ Vertex; Fragment ]
                          | `Fragment -> [ Fragment ]);
                      })
                resources;
          }
      in
      let ( let* ) = Result.bind in
      let* vertex = artifact Vertex in
      let* fragment = artifact Fragment in
      let* bindings =
        Ogpu.Binding.create_layout
          (List.map
             (fun (binding, kind, visibility) ->
               {
                 Ogpu.Binding.binding;
                 kind =
                   (match kind with `Buffer -> Buffer | `Texture -> Texture | `Sampler -> Sampler);
                 visibility =
                   (match visibility with
                   | `Vertex -> [ Vertex ]
                   | `Both -> [ Vertex; Fragment ]
                   | `Fragment -> [ Fragment ]);
               })
             resources)
      in
      let* layout =
        Ogpu.Binding.create_pipeline_layout
          ~device:(Ogpu.Backend.device_handle backend_device)
          ~capabilities:(Ogpu.Backend.capabilities backend_device)
          [ (0, bindings) ]
      in
      Ogpu.Backend.create_render_pipeline ~blend
        ~topology:(if family = Scene3_points then Ogpu.Render_pass.Point_list else Triangle_list)
        ~indirect:
          (match family with
          | Scene2 | Scene2_textured | Scene3 | Scene3_points -> true
          | Scene3_textured | Scene3_world | Ui | Marks -> false)
        backend_device
        {
          backend;
          label = Some "runtime-next";
          layout;
          vertex;
          vertex_entry;
          fragment = Some fragment;
          fragment_entry = Some fragment_entry;
          color_format = Rgba8_unorm;
          depth_format =
            (match family with
            | Scene_execution.Scene2 | Scene2_textured | Ui | Marks -> Ogpu.Pipeline.No_depth
            | Scene3 | Scene3_points | Scene3_textured | Scene3_world -> Depth32_float);
          sample_count = samples;
        }
    in
    Scene_execution.create ?device ?gpu_vertices ~sun_depth:make_sun_depth_pipeline ~offscreen
      driver configuration make_pipeline
