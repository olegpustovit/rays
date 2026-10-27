open Ogpu.Types
open Ogpu.Shader
open Ogpu.Binding
open Ogpu.Pipeline
open Ogpu.Render_pass
open Scene_execution

let source_scene3 = Metal_scene3_source.source
let source_scene3_points = "#define RAYS_POINTS\n" ^ source_scene3
let source_scene3_textured = Metal_scene3_textured_source.source
let source_scene3_world = Metal_scene3_world_source.source
let source_scene3_sun_depth = Metal_scene3_sun_depth_source.source

let make_sun_depth_pipeline backend backend_device =
  let buffer binding = { Ogpu.Shader.group = 0; binding; kind = Storage_buffer; visibility = [ Vertex ] } in
  let artifact bindings =
    Ogpu.Shader.create
      {
        backend;
        label = Some "scene-sun-depth";
        bytes = Bytes.of_string source_scene3_sun_depth;
        entry_points = [ { name = "sun_vertex"; stage = Vertex }; { name = "sun_fragment"; stage = Fragment } ];
        bindings;
      }
  in
  let ( let* ) = Result.bind in
  let* vertex = artifact (List.map buffer [ 0; 3; 6; 7 ]) in
  let* fragment = artifact [] in
  let* bindings =
    Ogpu.Binding.create_layout
      (List.map (fun binding -> { Ogpu.Binding.binding; kind = Buffer; visibility = [ Vertex ] }) [ 0; 3; 6; 7 ])
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
    Error(Ogpu.Error.make "Renderer.create" Ogpu.Error.Unsupported
      ("no renderer shaders for backend " ^ backend))
  else
  let make_sun_depth_pipeline device=make_sun_depth_pipeline backend device in
  let make_pipeline backend_device family blend samples =
    let source, extra =
      match family with
      | Scene_execution.Scene2 | Scene2_textured ->
          ( Renderer_shaders.scene2_textured_argument,
            [
              {
                Ogpu.Shader.group = 0;
                binding = 1;
                kind = Storage_buffer;
                visibility = [ Fragment ];
              };
            ] )
      | Scene3 -> (source_scene3, [])
      | Scene3_points -> (source_scene3_points, [])
      | Scene3_textured ->
          ( source_scene3_textured,
            [
              {
                Ogpu.Shader.group = 0;
                binding = 1;
                kind = Sampled_texture;
                visibility = [ Fragment ];
              };
              { group = 0; binding = 2; kind = Sampler; visibility = [ Fragment ] };
            ] )
      | Scene3_world ->
          ( source_scene3_world,
            [
              { Ogpu.Shader.group = 0; binding = 1; kind = Sampled_texture; visibility = [ Fragment ] };
              { group = 0; binding = 2; kind = Sampler; visibility = [ Fragment ] };
              { group = 0; binding = 3; kind = Storage_buffer; visibility = [ Fragment ] };
              { group = 0; binding = 4; kind = Sampled_texture; visibility = [ Fragment ] };
              { group = 0; binding = 5; kind = Sampler; visibility = [ Fragment ] };
              { group = 0; binding = 8; kind = Sampled_texture; visibility = [ Fragment ] };
              { group = 0; binding = 9; kind = Sampled_texture; visibility = [ Fragment ] };
              { group = 0; binding = 10; kind = Sampler; visibility = [ Fragment ] };
            ] )
      | Ui ->
          ( Renderer_shaders.ui,
            [
              {
                Ogpu.Shader.group = 0;
                binding = 1;
                kind = Sampled_texture;
                visibility = [ Fragment ];
              };
              { group = 0; binding = 2; kind = Sampler; visibility = [ Fragment ] };
            ] )
      | Marks -> (Renderer_shaders.marks, [])
    in
    let artifact bindings =
      Ogpu.Shader.create
        {
          backend;
          label = Some "runtime-next";
          bytes = Bytes.of_string source;
          entry_points =
            [
              { name = "scene_vertex"; stage = Vertex };
              {
                name =
                  (if family = Scene2 || family = Scene2_textured then "scene_fragment_argument"
                   else "scene_fragment");
                stage = Fragment;
              };
            ];
          bindings;
        }
    in
    let vertex_schema =
      { Ogpu.Shader.group = 0; binding = 0; kind = Storage_buffer; visibility = [ Vertex ] }
      ::
      (match family with
      | Scene_execution.Scene2 | Scene2_textured | Ui ->
          [ { Ogpu.Shader.group = 0; binding = 6; kind = Storage_buffer; visibility = [ Vertex ] } ]
      | Marks ->
          List.map
            (fun binding ->
              { Ogpu.Shader.group = 0; binding; kind = Storage_buffer; visibility = [ Vertex ] })
            [ 1; 2; 3; 4; 5; 6 ]
      | Scene3 | Scene3_points | Scene3_textured | Scene3_world ->
          [
            {
              Ogpu.Shader.group = 0;
              binding = 6;
              kind = Storage_buffer;
              visibility = [ Vertex; Fragment ];
            };
            { Ogpu.Shader.group = 0; binding = 7; kind = Storage_buffer; visibility = [ Vertex ] };
            { Ogpu.Shader.group = 0; binding = 11; kind = Storage_buffer; visibility = [ Vertex ] };
          ])
    in
    match (artifact vertex_schema, artifact extra) with
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Ok vertex, Ok fragment -> (
        let entries =
          { Ogpu.Binding.binding = 0; kind = Buffer; visibility = [ Vertex ] }
          ::
          (match family with
          | Scene_execution.Scene2 | Scene2_textured | Ui ->
              [ { Ogpu.Binding.binding = 6; kind = Buffer; visibility = [ Vertex ] } ]
          | Marks ->
              List.map
                (fun binding -> { Ogpu.Binding.binding; kind = Buffer; visibility = [ Vertex ] })
                [ 1; 2; 3; 4; 5; 6 ]
          | Scene3 | Scene3_points | Scene3_textured | Scene3_world ->
              [
                { Ogpu.Binding.binding = 6; kind = Buffer; visibility = [ Vertex; Fragment ] };
                { Ogpu.Binding.binding = 7; kind = Buffer; visibility = [ Vertex ] };
                { Ogpu.Binding.binding = 11; kind = Buffer; visibility = [ Vertex ] };
              ])
        in
        let bindings =
          match family with
          | Scene2 | Scene2_textured ->
              entries @ [ { Ogpu.Binding.binding = 1; kind = Buffer; visibility = [ Fragment ] } ]
          | Ui ->
              entries
              @ [
                  { Ogpu.Binding.binding = 1; kind = Texture; visibility = [ Fragment ] };
                  { binding = 2; kind = Sampler; visibility = [ Fragment ] };
                ]
          | Scene3 | Scene3_points | Marks -> entries
          | Scene3_textured ->
              entries
              @ [
                  { Ogpu.Binding.binding = 1; kind = Texture; visibility = [ Fragment ] };
                  { binding = 2; kind = Sampler; visibility = [ Fragment ] };
                ]
          | Scene3_world ->
              entries
              @ [
                  { Ogpu.Binding.binding = 1; kind = Texture; visibility = [ Fragment ] };
                  { binding = 2; kind = Sampler; visibility = [ Fragment ] };
                  { binding = 3; kind = Buffer; visibility = [ Fragment ] };
                  { binding = 4; kind = Texture; visibility = [ Fragment ] };
                  { binding = 5; kind = Sampler; visibility = [ Fragment ] };
                ]
              @ (if family = Scene3_world then
                   [
                     { Ogpu.Binding.binding = 8; kind = Texture; visibility = [ Fragment ] };
                     { binding = 9; kind = Texture; visibility = [ Fragment ] };
                     { binding = 10; kind = Sampler; visibility = [ Fragment ] };
                   ]
                 else [])
        in
        match Ogpu.Binding.create_layout bindings with
        | Error e -> Error e
        | Ok bindings -> (
            let groups = [ (0, bindings) ] in
            match
              Ogpu.Binding.create_pipeline_layout
                ~device:(Ogpu.Backend.device_handle backend_device)
                ~capabilities:(Ogpu.Backend.capabilities backend_device)
                groups
            with
            | Error _ as e -> e
            | Ok layout -> (
                let descriptor : Ogpu.Pipeline.render_descriptor =
                  {
                    backend;
                    label = Some "runtime-next";
                    layout;
                    vertex;
                    vertex_entry = "scene_vertex";
                    fragment = Some fragment;
                    fragment_entry =
                      Some
                        (if family = Scene2 || family = Scene2_textured then
                           "scene_fragment_argument"
                         else "scene_fragment");
                    color_format = Rgba8_unorm;
                    depth_format =
                      (match family with
                      | Scene_execution.Scene2 | Scene2_textured | Ui | Marks -> Ogpu.Pipeline.No_depth
                      | Scene3 | Scene3_points | Scene3_textured | Scene3_world -> Depth32_float);
                    sample_count = samples;
                  }
                in
                Ogpu.Backend.create_render_pipeline ~blend
                  ~topology:
                    (if family = Scene3_points then Ogpu.Render_pass.Point_list
                     else Triangle_list)
                  ~indirect:
                    (match family with
                     | Scene2 | Scene2_textured | Scene3 | Scene3_points -> true
                     | Scene3_textured | Scene3_world | Ui | Marks -> false)
                  backend_device descriptor)))
  in
  Scene_execution.create ?device ?gpu_vertices ~sun_depth:make_sun_depth_pipeline ~offscreen driver configuration
    make_pipeline
