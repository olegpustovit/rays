let get = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)

let create_offscreen ~logical_width ~logical_height ~width ~height =
  Renderer.create_offscreen
    {
      Renderer.default_configuration with
      logical_width;
      logical_height;
      drawable_width = width;
      drawable_height = height;
    }

let uniform red =
  let bytes = Bytes.make 5456 '\000' in
  let set index value = Bytes.set_int32_le bytes (index * 4) (Int32.bits_of_float value) in
  for matrix = 0 to 2 do
    for diagonal = 0 to 3 do
      set ((matrix * 16) + (diagonal * 5)) 1.
    done
  done;
  set 0 0.5;
  set 3 (if red then -0.5 else 0.5);
  set 50 (-1.);
  set 52 (if red then 1. else 0.);
  set 53 (if red then 0. else 1.);
  set 55 1.;
  set 69 1.;
  set 70 1.;
  set 71 1.;
  set 72 1.;
  bytes

let run () =
  match create_offscreen ~logical_width:4 ~logical_height:4 ~width:4 ~height:4 with
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)
  | Ok runtime ->
      let indices = Bytes.make 12 '\000'
      and vertices = Bytes.make (24 * 3) '\000'
      and attributes = Bytes.make (12 * 3) '\000' in
      Bytes.set_int32_le indices 4 1l;
      Bytes.set_int32_le indices 8 2l;
      List.iteri
        (fun index (x, y) ->
          let offset = index * 24 in
          Bytes.set_int32_le vertices offset (Int32.bits_of_float x);
          Bytes.set_int32_le vertices (offset + 4) (Int32.bits_of_float y);
          Bytes.set_int32_le vertices (offset + 20) (Int32.bits_of_float 1.);
          Bytes.set_int32_le attributes (index * 12) 0xffffffffl)
        [ (-1., -1.); (3., -1.); (-1., 3.) ];
      let mesh : Renderer.Scene_execution.mesh =
        {
          key = Host "scene3-shared-mesh";
          vertices;
          vertex_count = 3;
          indices;
          index_count = 3;
          primitive = Ogpu.Render_pass.Triangle_list;
        }
      in
      let state scissor transform_uniforms : Renderer.Scene_execution.state =
        {
          viewport = (0, 0, 4, 4);
          scissor;
          cull = Ogpu.Render_pass.Cull_none;
          depth_compare = Always;
          depth_write = false;
          depth_load = Clear;
          depth_clear = 1.;
          transform_uniforms = Some transform_uniforms;
        }
      in
      let draw scissor uniform : Renderer.Scene_execution.scene3_entry =
        {
          family = Scene3;
          blend = Ogpu.Pipeline.Replace;
          texture = None;
          auxiliary = None;
          vertex_attributes = Some ("test-attributes", attributes);
          samples = 1;
          draw = { mesh; state = state scissor uniform };
        }
      in
      let left = draw (0, 0, 4, 4) (uniform true) and right = draw (0, 0, 4, 4) (uniform false) in
      for _frame = 1 to 3 do
        ignore (get (Renderer.draw runtime [ left; right ]));
        let pixels = get (Renderer.read_pixels runtime ~bytes_per_row:256) in
        if
          Char.code (Bytes.get pixels 0) <> 255
          || Char.code (Bytes.get pixels 1) <> 0
          || Char.code (Bytes.get pixels 12) <> 0
          || Char.code (Bytes.get pixels 13) <> 255
        then
          failwith
            (Printf.sprintf "Scene3 draws sharing a mesh lost distinct uniforms: %d,%d / %d,%d"
               (Char.code (Bytes.get pixels 0))
               (Char.code (Bytes.get pixels 1))
               (Char.code (Bytes.get pixels 12))
               (Char.code (Bytes.get pixels 13)))
      done;
      let first = uniform true and second = uniform false in
      Bytes.set_int32_le second (52 * 4) (Int32.bits_of_float 1.);
      Bytes.set_int32_le second (53 * 4) (Int32.bits_of_float 0.);
      let reference_draws = [ draw (0, 0, 4, 4) first; draw (0, 0, 4, 4) second ] in
      let render entries =
        ignore (get (Renderer.draw runtime entries));
        get (Renderer.read_pixels runtime ~bytes_per_row:256)
      in
      let expected = render reference_draws in
      let instanced = Bytes.make (5456 + (2 * 192)) '\000' in
      Bytes.blit first 0 instanced 0 5456;
      Bytes.set_int32_le instanced (83 * 4) (Int32.bits_of_float 1.);
      Bytes.blit first 0 instanced 5456 192;
      Bytes.blit second 0 instanced (5456 + 192) 192;
      let actual = render [ draw (0, 0, 4, 4) instanced ] in
      if actual <> expected then failwith "instanced Scene3 draw differs from two indexed draws";
      let primitive_mesh primitive positions =
        let count = List.length positions in
        let vertices = Bytes.make (count * 24) '\000'
        and attributes = Bytes.make (count * 12) '\000'
        and indices = Bytes.make (count * 4) '\000' in
        List.iteri
          (fun index (x, y) ->
            let offset = index * 24 in
            Bytes.set_int32_le vertices offset (Int32.bits_of_float x);
            Bytes.set_int32_le vertices (offset + 4) (Int32.bits_of_float y);
            Bytes.set_int32_le vertices (offset + 20) (Int32.bits_of_float 1.);
            Bytes.set_int32_le attributes (index * 12) 0xffffffffl;
            Bytes.set_int32_le indices (index * 4) (Int32.of_int index))
          positions;
        ( { mesh with vertices; indices; vertex_count = count; index_count = count; primitive },
          attributes )
      in
      List.iter
        (fun (primitive, positions) ->
          let mesh, attributes = primitive_mesh primitive positions in
          let entry =
            {
              left with
              family = (if primitive = Ogpu.Render_pass.Point_list then Scene3_points else Scene3);
              draw = { left.draw with mesh };
              vertex_attributes = Some ("primitive-attributes", attributes);
            }
          in
          let pixels = render [ entry ] in
          let lit = ref false in
          for row = 0 to 3 do
            for column = 0 to 3 do
              if Char.code (Bytes.get pixels ((row * 256) + (column * 4))) > 0 then lit := true
            done
          done;
          if not !lit then
            failwith
              ("Scene3 native draw produced no pixels: "
              ^
              match primitive with
              | Ogpu.Render_pass.Point_list -> "point"
              | Line_list -> "line"
              | Triangle_list -> "triangle"
              | Triangle_strip -> "strip"))
        [
          (Ogpu.Render_pass.Line_list, [ (-1., 0.); (1., 0.) ]);
          (Ogpu.Render_pass.Point_list, [ (-0.5, -0.75) ]);
        ];
      let reference_uniform = uniform true in
      let expected = render [ draw (0, 0, 4, 4) reference_uniform ] in
      let gpu = get (Renderer.acquire_gpu ()) in
      let resident_runtime =
        get
          (Renderer.Native.create_offscreen_with_gpu gpu
             {
               Renderer.default_configuration with
               logical_width = 4;
               logical_height = 4;
               drawable_width = 4;
               drawable_height = 4;
             })
      in
      let device = Renderer.gpu_device gpu in
      let native result =
        match result with Ok v -> v | Error e -> failwith (Ogpu.Error.to_string e)
      in
      let positions =
        native
          (Ogpu.Backend.create_buffer device
             { label = Some "indexed-test-P"; size = 36L; usage = [ Vertex; Storage; Copy_dst ] })
      in
      let device_indices =
        native
          (Ogpu.Backend.create_buffer device
             {
               label = Some "indexed-test-indices";
               size = 12L;
               usage = [ Index; Storage; Copy_dst ];
             })
      in
      let position_bytes = Bytes.create 36 in
      for i = 0 to 2 do
        Bytes.blit vertices (i * 24) position_bytes (i * 12) 12
      done;
      native (Ogpu.Backend.write_buffer positions ~offset:0L position_bytes);
      native (Ogpu.Backend.write_buffer device_indices ~offset:0L indices);
      let key, unregister =
        get
          (Renderer.register_gpu_positions gpu ~positions ~indices:(device_indices, 3) ~count:3
             ~valid:(fun () -> true)
             ())
      in
      let color_buffers = ref [] in
      Fun.protect
        ~finally:(fun () ->
          unregister ();
          get (Renderer.destroy resident_runtime);
          List.iter (fun buffer -> native (Ogpu.Backend.destroy_buffer buffer)) !color_buffers;
          native (Ogpu.Backend.destroy_buffer device_indices);
          native (Ogpu.Backend.destroy_buffer positions);
          Renderer.release_gpu gpu)
        (fun () ->
          let device_uniform = Bytes.copy reference_uniform in
          Bytes.set_int32_le device_uniform (79 * 4) (Int32.bits_of_float 1.);
          let entry = draw (0, 0, 4, 4) device_uniform in
          let entry =
            {
              entry with
              draw =
                {
                  entry.draw with
                  mesh =
                    {
                      mesh with
                      key = Gpu_vertices key;
                      vertices = Bytes.empty;
                      indices = Bytes.empty;
                    };
                };
            }
          in
          for _ = 1 to 3 do
            ignore (get (Renderer.draw resident_runtime [ entry ]));
            let actual = get (Renderer.read_pixels resident_runtime ~bytes_per_row:256) in
            if actual <> expected then
              failwith "borrowed UInt32 Scene3 indices differ from host indices"
          done;
          let white_uniform = Bytes.copy device_uniform in
          Bytes.set_int32_le white_uniform (81 * 4) (Int32.bits_of_float (-1.));
          let white =
            {
              entry with
              vertex_attributes = Some ("device-white", Bytes.empty);
              draw =
                {
                  entry.draw with
                  state = { entry.draw.state with transform_uniforms = Some white_uniform };
                };
            }
          in
          ignore (get (Renderer.draw resident_runtime [ white ]));
          if get (Renderer.read_pixels resident_runtime ~bytes_per_row:256) <> expected then
            failwith "constant device white differs from the host default attribute record";
          List.iter
            (fun width ->
              let values = [| -0.2; 0.25; 0.5; 0.65; 0.8; 1.3; 0.12; 0.25; 0.1; 0.7; 0.9; 1. |] in
              let color_bytes = Bytes.create (3 * width * 4) in
              let host_attributes = Bytes.copy attributes in
              for i = 0 to 2 do
                for c = 0 to width - 1 do
                  Bytes.set_int32_le color_bytes
                    (((i * width) + c) * 4)
                    (Int32.bits_of_float values.((i * 4) + c))
                done;
                let channel c = Int32.float_of_bits (Int32.bits_of_float values.((i * 4) + c)) in
                let color =
                  Rays_math.Color.of_floats (channel 0) (channel 1) (channel 2)
                    (if width = 4 then channel 3 else 1.)
                in
                Bytes.set_int32_le host_attributes (i * 12)
                  Int32.(
                    logor
                      (shift_left (of_int color.r) 24)
                      (logor
                         (shift_left (of_int color.g) 16)
                         (logor (shift_left (of_int color.b) 8) (of_int color.a))))
              done;
              let color_buffer =
                native
                  (Ogpu.Backend.create_buffer device
                     {
                       label = Some "indexed-test-Cd";
                       size = Int64.of_int (Bytes.length color_bytes);
                       usage = [ Vertex; Storage; Copy_dst ];
                     })
              in
              color_buffers := color_buffer :: !color_buffers;
              native (Ogpu.Backend.write_buffer color_buffer ~offset:0L color_bytes);
              let color_key, unregister_color =
                get
                  (Renderer.register_gpu_positions gpu ~positions ~indices:(device_indices, 3)
                     ~colors:(color_buffer, width) ~count:3
                     ~valid:(fun () -> true)
                     ())
              in
              Fun.protect ~finally:unregister_color (fun () ->
                  let white = uniform true in
                  Bytes.set_int32_le white (53 * 4) (Int32.bits_of_float 1.);
                  Bytes.set_int32_le white (54 * 4) (Int32.bits_of_float 1.);
                  let reference =
                    {
                      (draw (0, 0, 4, 4) white) with
                      vertex_attributes =
                        Some (Printf.sprintf "host-colors-%d" width, host_attributes);
                    }
                  in
                  let expected = render [ reference ] in
                  let device_uniform = Bytes.copy white in
                  Bytes.set_int32_le device_uniform (79 * 4) (Int32.bits_of_float 1.);
                  Bytes.set_int32_le device_uniform (81 * 4) (Int32.bits_of_float (float width));
                  let colored =
                    {
                      reference with
                      vertex_attributes = Some ("resident-colors", Bytes.empty);
                      draw =
                        {
                          state =
                            { reference.draw.state with transform_uniforms = Some device_uniform };
                          mesh =
                            {
                              reference.draw.mesh with
                              key = Gpu_vertices color_key;
                              vertices = Bytes.empty;
                              indices = Bytes.empty;
                            };
                        };
                    }
                  in
                  for _ = 1 to 3 do
                    ignore (get (Renderer.draw resident_runtime [ colored ]));
                    let actual = get (Renderer.read_pixels resident_runtime ~bytes_per_row:256) in
                    if actual <> expected then begin
                      let at = ref 0 in
                      while
                        !at < Bytes.length actual && Bytes.get actual !at = Bytes.get expected !at
                      do
                        incr at
                      done;
                      failwith
                        (Printf.sprintf "resident float%d colours differ at byte %d: %d vs %d" width
                           !at
                           (Char.code (Bytes.get actual !at))
                           (Char.code (Bytes.get expected !at)))
                    end
                  done))
            [ 3; 4 ]);
      get (Renderer.destroy runtime);
      print_endline "Scene3 shared-mesh and indexed instances passed"
