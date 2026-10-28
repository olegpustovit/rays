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
      get (Renderer.destroy runtime);
      print_endline "Scene3 shared-mesh and indexed instances passed"
