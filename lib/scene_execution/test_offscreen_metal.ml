let get = function Ok value -> value | Error error -> failwith (Ogpu.Error.to_string error)

let run () =
  let driver = Ogpu.Impl.create_driver () in
  let live_handles = Ogpu.Impl.live_handles in
  let before = live_handles () in
  let configuration : Ogpu.Surface.configuration =
    {
      logical_width = 4;
      logical_height = 4;
      physical_width = 4;
      physical_height = 4;
      format = Bgra8_unorm;
      present_mode = Fifo;
      max_acquired = 1;
      layer = None;
    }
  in
  match Scene_execution_fixtures.create_offscreen driver configuration with
  | Error { Ogpu.Error.kind = No_adapter; _ } ->
      print_endline "offscreen Metal execution: skipped (no device)"
  | Error error -> failwith (Ogpu.Error.to_string error)
  | Ok renderer ->
      Fun.protect
        ~finally:(fun () -> get (Scene_execution.destroy renderer))
        (fun () ->
          let expect_pixels label pixel =
            let expected = Bytes.init 64 (fun i -> Bytes.get pixel (i mod 4)) in
            let actual = get (Scene_execution.read_pixels renderer ~bytes_per_row:16) in
            if not (Bytes.equal expected actual) then failwith (label ^ " full-image pixel drift")
          in
          if not (get (Scene_execution_fixtures.render ~clear:(0.125, 0.25, 0.5, 1.) renderer []))
          then failwith "layerless clear-only submission was skipped";
          expect_pixels "layerless clear-only" (Bytes.of_string "\x20\x40\x80\xff");
          let indices = Bytes.make 12 '\000' in
          Bytes.set_int32_le indices 4 1l;
          Bytes.set_int32_le indices 8 2l;
          let mesh : Scene_execution.mesh =
            {
              key = Host "offscreen-fullscreen";
              vertices = Scene_execution_fixtures.scene2_plain_vertices ();
              vertex_count = 3;
              indices;
              index_count = 3;
              primitive = Ogpu.Render_pass.Triangle_list;
            }
          and state : Scene_execution.state =
            {
              viewport = (0, 0, 4, 4);
              scissor = (0, 0, 4, 4);
              cull = Ogpu.Render_pass.Cull_none;
              depth_compare = Always;
              depth_write = false;
              depth_load = Load;
              depth_clear = 1.;
              transform_uniforms = None;
            }
          in
          let expected = Bytes.of_string "\xff\xff\xff\xff" in
          let clear = (0., 0., 0., 1.) in
          for frame = 1 to 600 do
            if not (get (Scene_execution_fixtures.render ~clear renderer [ { mesh; state } ])) then
              failwith "layerless offscreen submission was skipped";
            if List.mem frame [ 1; 2; 60; 600 ] then
              expect_pixels (Printf.sprintf "offscreen frame %d" frame) expected
          done;
          let uploaded = Scene_execution.upload_bytes renderer in
          if uploaded <= 0L || Scene_execution.Private.cache_count_for_report renderer > 2 then
            failwith "offscreen cache accounting is unbounded or empty";
          let retained : Scene_execution.sampled_draw =
            {
              family = Scene2;
              blend = Ogpu.Pipeline.Replace;
              texture = None;
              auxiliary = None;
              vertex_attributes = None;
              samples = 1;
              draw = { mesh; state };
            }
          in
          if
            not
              (get
                 (Scene_execution.submit ~clear ~prepared:("offscreen-retained", 1L) renderer
                    [ retained ]))
          then failwith "offscreen retained preparation was skipped";
          let steady_uploads = ref None in
          for frame = 1 to 60 do
            (match
               get
                 (Scene_execution.replay_prepared_sampled_resources ~clear
                    ~identity:"offscreen-retained" ~version:1L renderer)
             with
            | Some (true, 1) -> ()
            | _ -> failwith "offscreen retained replay missed");
            expect_pixels "offscreen retained replay" expected;
            if frame = 2 then steady_uploads := Some (Scene_execution.upload_bytes renderer)
            else if frame > 2 && Some (Scene_execution.upload_bytes renderer) <> !steady_uploads
            then failwith "offscreen retained replay uploaded unchanged data"
          done;
          (match
             get
               (Scene_execution.replay_prepared_sampled_resources ~clear
                  ~identity:"offscreen-retained" ~version:2L renderer)
           with
          | None -> ()
          | Some _ -> failwith "offscreen retained replay accepted a wrong version");
          (* Distinct scissors prevent coalescing: this graph crosses the mesh
         cache limit and must rebuild after completion-time eviction. *)
          let dense =
            List.init 257 (fun index ->
                {
                  Scene_execution.family = Scene2;
                  blend = Ogpu.Pipeline.Replace;
                  texture = None;
                  auxiliary = None;
                  vertex_attributes = None;
                  samples = 1;
                  draw =
                    {
                      mesh = { mesh with key = Host (Printf.sprintf "dense-%d" index) };
                      state = { state with scissor = (index mod 2, 0, 3, 4) };
                    };
                })
          in
          for frame = 1 to 60 do
            if
              not
                (get
                   (Scene_execution.submit ~clear ~prepared:("dense-eviction", 1L) renderer dense))
            then failwith "dense eviction submission was skipped";
            (match
               get
                 (Scene_execution.replay_prepared_sampled_resources ~clear
                    ~identity:"dense-eviction" ~version:1L renderer)
             with
            | None -> ()
            | Some _ -> failwith "evicted native graph retained");
            if List.mem frame [ 1; 2; 60 ] then expect_pixels "dense eviction" expected;
            if Scene_execution.Private.cache_count_for_report renderer > 256 then
              failwith "dense native cache overflow"
          done);
      let after = live_handles () in
      if after <> before then
        failwith (Printf.sprintf "offscreen native live-handle delta %d -> %d" before after);
      print_endline
        "offscreen Metal execution: exact clear/frame1/2/60/600 + retained replay + 257-draw \
         eviction, bounded, zero delta"
