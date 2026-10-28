let get = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)

let run () =
  let live_handles = Ogpu.Impl.live_handles in
  let before = live_handles () in
  let configuration =
    { Renderer.default_configuration with
      logical_width = 4; logical_height = 4; drawable_width = 4; drawable_height = 4 }
  in
  match Renderer.create configuration with
  | Error _ -> print_endline "renderer: skipped (no window/Metal device)"
  | Ok renderer ->
      let indices = Bytes.make 12 '\000' and vertices = Bytes.make 48 '\000' in
      Bytes.set_int32_le indices 4 1l;
      Bytes.set_int32_le indices 8 2l;
      List.iteri
        (fun i (x, y) ->
          let offset = i * 16 in
          Bytes.set_int32_le vertices offset (Int32.bits_of_float x);
          Bytes.set_int32_le vertices (offset + 4) (Int32.bits_of_float y);
          Bytes.set_int32_le vertices (offset + 8) 0x4080BFFFl)
        [ -1., -1.; 3., -1.; -1., 3. ];
      let mesh : Renderer.Scene_execution.mesh =
        { key = Host "runtime-next-fullscreen"; vertices; vertex_count = 3; indices;
          index_count = 3; primitive = Ogpu.Render_pass.Triangle_list }
      in
      let state : Renderer.Scene_execution.state =
        { viewport = (0, 0, 4, 4); scissor = (0, 0, 4, 4);
          cull = Ogpu.Render_pass.Cull_none; depth_compare = Ogpu.Render_pass.Always;
          depth_write = false; depth_load = Ogpu.Render_pass.Load; depth_clear = 1.;
          transform_uniforms = None }
      in
      let draw = Renderer.prepared_draw ~family:Renderer.Scene2 { mesh; state } in
      for frame = 1 to 600 do
        ignore (get (Renderer.draw renderer (if frame = 2 then [ draw; draw ] else [ draw ])));
        if List.mem frame [ 1; 2; 60; 600 ] then begin
          let facts = get (Runtime.presentation_facts (Renderer.Native.runtime renderer)) in
          let row_bytes = facts.drawable_width * 4 in
          let bytes = get (Renderer.read_pixels renderer ~bytes_per_row:row_bytes) in
          if Char.code (Bytes.get bytes 0) <> 64 || Char.code (Bytes.get bytes 1) <> 128
             || Char.code (Bytes.get bytes 2) <> 191 then failwith "renderer exact pixel"
        end
      done;
      get (Runtime.resize (Renderer.Native.runtime renderer) ~width:8 ~height:8);
      let resized_state = { state with viewport = (0, 0, 8, 8); scissor = (0, 0, 8, 8) } in
      ignore (get (Renderer.draw renderer [ Renderer.prepared_draw ~family:Renderer.Scene2
        { mesh; state = resized_state } ]));
      get (Renderer.destroy renderer);
      if live_handles () <> before then failwith "renderer Metal live-handle delta";
      print_endline "renderer: Metal frames 1/2/60/600 and resize, zero delta"
