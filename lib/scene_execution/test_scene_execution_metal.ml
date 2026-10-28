(* Tuple shorthand for the record-based [Scene_execution.submit]. *)
let sampled ?texture ?auxiliary ?vertex_attributes ?(samples = 1) family blend draw :
    Scene_execution.sampled_draw =
  { family; blend; texture; auxiliary; vertex_attributes; samples; draw }

let render_resources ?clear r draws =
  Scene_execution.submit ?clear r
    (List.map
       (fun (family, blend, texture, auxiliary, draw) ->
         sampled ?texture ?auxiliary family blend draw)
       draws)

let render_textured ?clear r draws =
  Scene_execution.submit ?clear r
    (List.map (fun (family, blend, texture, draw) -> sampled ?texture family blend draw) draws)

let render_family ?clear r draws =
  Scene_execution.submit ?clear r
    (List.map (fun (family, blend, draw) -> sampled family blend draw) draws)

let render_blended ?clear r draws =
  Scene_execution.submit ?clear r
    (List.map (fun (blend, draw) -> sampled Scene_execution.Scene2 blend draw) draws)

let get = function Ok x -> x | Error e -> failwith (Ogpu.Error.to_string e)
let frames = List.init 600 succ

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
      max_acquired = 2;
      layer = None;
    }
  in
  let renderer = get (Scene_execution_fixtures.create_offscreen driver configuration) in
  let supported =
    List.filter
      (fun samples ->
        samples
        <= (Ogpu.Backend.capabilities (Scene_execution.device renderer)).Ogpu.Caps.limits
             .max_sample_count)
      [ 1; 4; 9; 16 ]
  in
  let indices = Bytes.make 12 '\000' in
  Bytes.set_int32_le indices 4 1l;
  Bytes.set_int32_le indices 8 2l;
  let plain = Scene_execution_fixtures.scene2_plain_vertices ~color:0xff0080ffl () in
  let mesh : Scene_execution.mesh =
    {
      key = Host "fullscreen";
      vertices = plain;
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
      depth_compare = Ogpu.Render_pass.Always;
      depth_write = false;
      depth_load = Ogpu.Render_pass.Clear;
      depth_clear = 1.;
      transform_uniforms = None;
    }
  in
  let surface = Scene_execution_fixtures.scene3_surface () in
  let state3 = { state with transform_uniforms = Some surface } in
  let mesh3, attrs3 = Scene_execution_fixtures.scene3_mesh ~color:0xff0080ffl "fullscreen-scene3" in
  let sampled3 samples draw =
    sampled ~vertex_attributes:attrs3 ~samples Scene_execution.Scene3 Ogpu.Pipeline.Replace draw
  in
  ignore (get (Scene_execution_fixtures.render renderer [ { mesh; state }; { mesh; state } ]));
  let pixels = get (Scene_execution.read_pixels renderer ~bytes_per_row:16) in
  let r = Char.code (Bytes.get pixels 0)
  and g = Char.code (Bytes.get pixels 1)
  and b = Char.code (Bytes.get pixels 2) in
  if r <> 255 || g <> 0 || b <> 128 then
    failwith (Printf.sprintf "scene execution Metal pixel mismatch %d,%d,%d" r g b);
  List.iter
    (fun (blend, expected) ->
      List.iter
        (fun _frame ->
          ignore (get (render_blended renderer [ (blend, { mesh; state }) ]));
          let bytes = get (Scene_execution.read_pixels renderer ~bytes_per_row:16) in
          let actual =
            ( Char.code (Bytes.get bytes 0),
              Char.code (Bytes.get bytes 1),
              Char.code (Bytes.get bytes 2) )
          in
          if actual <> expected then failwith "scene execution Metal blend pixel")
        frames)
    [
      (Ogpu.Pipeline.Replace, (255, 0, 128));
      (Alpha, (255, 0, 128));
      (Add, (255, 0, 128));
      (Multiply, (0, 0, 0));
      (Screen, (255, 0, 128));
      (Subtract, (0, 0, 0));
    ];
  List.iter
    (fun _frame ->
      ignore (get (Scene_execution.submit renderer [ sampled3 1 { mesh = mesh3; state = state3 } ]));
      let bytes = get (Scene_execution.read_pixels renderer ~bytes_per_row:16) in
      if Char.code (Bytes.get bytes 0) <> 255 || Char.code (Bytes.get bytes 2) <> 128 then
        failwith "scene execution Metal Scene3 pixel")
    frames;
  if List.mem 4 supported then
    List.iter
      (fun _frame ->
        ignore
          (get (Scene_execution.submit renderer [ sampled3 4 { mesh = mesh3; state = state3 } ]));
        let bytes = get (Scene_execution.read_pixels renderer ~bytes_per_row:16) in
        if Char.code (Bytes.get bytes 0) <> 255 || Char.code (Bytes.get bytes 2) <> 128 then
          failwith "scene execution Metal four-sample resolve pixel")
      frames;
  List.iter
    (fun samples ->
      if not (List.mem samples supported) then
        let before = Scene_execution.upload_bytes renderer in
        match
          Scene_execution.submit renderer [ sampled3 samples { mesh = mesh3; state = state3 } ]
        with
        | Error e
          when e.Ogpu.Error.kind = Unsupported && Scene_execution.upload_bytes renderer = before ->
            ()
        | _ -> failwith "unsupported native sample count was not rejected atomically")
    [ 9; 16 ];
  let mesh_uv u v =
    Scene_execution_fixtures.scene2_mesh
      ~uv:(fun _ _ -> (u, v))
      (Printf.sprintf "textured-%g-%g" u v)
  in
  let mesh3_uv u v =
    Scene_execution_fixtures.scene3_mesh ~uv:(u, v) (Printf.sprintf "textured3-%g-%g" u v)
  in
  let levels =
    [|
      {
        Scene_execution.width = 2;
        height = 2;
        bytes = Bytes.of_string "\255\000\000\255\000\255\000\255\000\000\255\255\255\255\255\255";
      };
      { width = 1; height = 1; bytes = Bytes.of_string "\255\255\000\255" };
    |]
  in
  let sampler ?(min_filter = Ogpu.Types.Nearest) ?(mag_filter = Ogpu.Types.Nearest)
      ?(mip_filter = Ogpu.Types.No_mip) ?(address_u = Ogpu.Types.Clamp_to_edge)
      ?(address_v = Ogpu.Types.Clamp_to_edge) ?(lod_min = 0.) ?(lod_max = 1.) () :
      Ogpu.Types.sampler_descriptor =
    {
      label = Some "scene-texture-test";
      min_filter;
      mag_filter;
      mip_filter;
      address_u;
      address_v;
      lod_min;
      lod_max;
      max_anisotropy = 1;
    }
  in
  let cases =
    [
      (mesh3_uv 0.1 0.1, sampler (), (255, 0, 0));
      (mesh3_uv 0.5 0.5, sampler ~min_filter:Linear ~mag_filter:Linear (), (128, 128, 128));
      (mesh3_uv 1.1 0.1, sampler ~address_u:Repeat (), (255, 0, 0));
      (mesh3_uv 1.9 0.1, sampler ~address_u:Mirror_repeat (), (255, 0, 0));
      (mesh3_uv 0.1 0.1, sampler ~mip_filter:Nearest_mip ~lod_min:1. ~lod_max:1. (), (255, 255, 0));
    ]
  in
  let mixed_texture : Scene_execution.sampled_texture =
    { key = "native-mixed-scene2"; levels; sampler = sampler (); gpu = None }
  in
  List.iter
    (fun _frame ->
      ignore
        (get
           (render_textured renderer
              [
                (Scene_execution.Scene2, Ogpu.Pipeline.Replace, None, { mesh; state });
                ( Scene_execution.Scene2_textured,
                  Ogpu.Pipeline.Replace,
                  Some mixed_texture,
                  { mesh = mesh_uv 0.1 0.1; state } );
              ]));
      let bytes = get (Scene_execution.read_pixels renderer ~bytes_per_row:16) in
      if
        Char.code (Bytes.get bytes 0) <> 255
        || Char.code (Bytes.get bytes 1) <> 0
        || Char.code (Bytes.get bytes 2) <> 0
      then failwith "mixed Scene2/Scene2_textured native pixel")
    frames;
  let film =
    get
      (Ogpu.Backend.create_texture (Scene_execution.device renderer)
         {
           Ogpu.Types.label = Some "gpu-film-test";
           width = 1;
           height = 1;
           depth = 1;
           mip_levels = 1;
           sample_count = 1;
           format = Rgba8_unorm;
           usage = [ Texture_binding; Storage_binding; Texture_copy_dst ];
         })
  in
  (* Produce the film on the GPU, as a path tracer would. *)
  let staging =
    get
      (Ogpu.Backend.create_buffer (Scene_execution.device renderer)
         { Ogpu.Types.label = Some "gpu-film-staging"; size = 256L; usage = [ Copy_src ] })
  in
  get (Ogpu.Backend.write_buffer staging ~offset:0L (Bytes.of_string "\x31\x82\xdd\xff"));
  let commands = get (Ogpu.Backend.begin_commands (Scene_execution.queue renderer)) in
  let blit = get (Ogpu.Backend.blit_encoder commands) in
  get
    (Ogpu.Backend.buffer_to_texture blit ~src:staging ~bytes_per_row:256L ~bytes_per_image:256L
       ~dst:film
       ~extent:{ width = 1; height = 1; depth = 1 }
       ());
  get (Ogpu.Backend.end_blit blit);
  let receipt = get (Ogpu.Backend.commit commands) in
  get (Ogpu.Backend.complete_through (Scene_execution.queue renderer) receipt.epoch);
  get (Ogpu.Backend.destroy_buffer staging);
  let gpu_texture : Scene_execution.sampled_texture =
    {
      key = "gpu-film-test";
      levels = [| { width = 1; height = 1; bytes = Bytes.empty } |];
      sampler = sampler ();
      gpu = Some film;
    }
  in
  (* Warm the transform ring for this one-draw frame; the measured frame
     must then upload nothing at all. *)
  ignore
    (get
       (render_textured renderer
          [
            ( Scene_execution.Scene2_textured,
              Ogpu.Pipeline.Replace,
              Some gpu_texture,
              { mesh = mesh_uv 0.1 0.1; state } );
          ]));
  let before_film = Scene_execution.upload_bytes renderer in
  ignore
    (get
       (render_textured renderer
          [
            ( Scene_execution.Scene2_textured,
              Ogpu.Pipeline.Replace,
              Some gpu_texture,
              { mesh = mesh_uv 0.1 0.1; state } );
          ]));
  let pixel = get (Scene_execution.read_pixels renderer ~bytes_per_row:16) in
  if
    Char.code (Bytes.get pixel 0) <> 0x31
    || Char.code (Bytes.get pixel 1) <> 0x82
    || Char.code (Bytes.get pixel 2) <> 0xdd
    || Scene_execution.upload_bytes renderer <> before_film
  then failwith "GPU film was copied through CPU staging or sampled incorrectly";
  let bad_film =
    {
      gpu_texture with
      Scene_execution.levels = [| { width = 2; height = 1; bytes = Bytes.empty } |];
    }
  in
  (match
     render_textured renderer
       [
         ( Scene_execution.Scene2_textured,
           Ogpu.Pipeline.Replace,
           Some bad_film,
           { mesh = mesh_uv 0.1 0.1; state } );
       ]
   with
  | Error error when error.Ogpu.Error.kind = Invalid_argument -> ()
  | _ -> failwith "mismatched GPU film extent was accepted");
  let foreign_driver = Ogpu.Impl.create_driver () in
  let foreign_device = get (Ogpu.Backend.create_device foreign_driver) in
  let foreign =
    get
      (Ogpu.Backend.create_texture foreign_device
         {
           Ogpu.Types.label = None;
           width = 1;
           height = 1;
           depth = 1;
           mip_levels = 1;
           sample_count = 1;
           format = Rgba8_unorm;
           usage = [ Texture_binding ];
         })
  in
  let foreign_film = { gpu_texture with Scene_execution.gpu = Some foreign } in
  (match
     render_textured renderer
       [
         ( Scene_execution.Scene2_textured,
           Ogpu.Pipeline.Replace,
           Some foreign_film,
           { mesh = mesh_uv 0.1 0.1; state } );
       ]
   with
  | Error error when error.Ogpu.Error.kind = Cross_device -> ()
  | _ -> failwith "foreign GPU film was accepted");
  get (Ogpu.Backend.destroy_texture foreign);
  get (Ogpu.Backend.destroy_device foreign_device);
  get (Ogpu.Backend.destroy_texture film);
  (match
     render_textured renderer
       [
         ( Scene_execution.Scene2_textured,
           Ogpu.Pipeline.Replace,
           Some gpu_texture,
           { mesh = mesh_uv 0.1 0.1; state } );
       ]
   with
  | Error error when error.Ogpu.Error.kind = Invalid_argument -> ()
  | _ -> failwith "destroyed GPU film was accepted");
  List.iteri
    (fun index ((mesh, attrs), sampler, expected) ->
      let texture : Scene_execution.sampled_texture =
        { key = "native-mip-chain"; levels; sampler; gpu = None }
      and stable = ref None in
      List.iter
        (fun _frame ->
          ignore
            (get
               (Scene_execution.submit renderer
                  [
                    sampled ~texture ~vertex_attributes:attrs Scene_execution.Scene3_textured
                      Ogpu.Pipeline.Replace { mesh; state = state3 };
                  ]));
          let bytes = get (Scene_execution.read_pixels renderer ~bytes_per_row:16) in
          let actual =
            ( Char.code (Bytes.get bytes 0),
              Char.code (Bytes.get bytes 1),
              Char.code (Bytes.get bytes 2) )
          in
          begin if actual <> expected then
            let r, g, b = actual in
            failwith (Printf.sprintf "scene execution Metal texture case %d: %d,%d,%d" index r g b)
          end;
          let uploaded = Scene_execution.upload_bytes renderer in
          match !stable with
          | None -> stable := Some uploaded
          | Some value when value = uploaded -> ()
          | Some _ -> failwith "stable native mip chain reuploaded")
        frames)
    cases;
  for index = 0 to 64 do
    let sampler =
      { (sampler ()) with Ogpu.Types.label = Some (Printf.sprintf "sampler-eviction-%d" index) }
    in
    let texture : Scene_execution.sampled_texture =
      { key = "native-sampler-eviction"; levels; sampler; gpu = None }
    in
    ignore
      (get
         (render_textured renderer
            [
              ( Scene_execution.Scene2_textured,
                Ogpu.Pipeline.Replace,
                Some texture,
                { mesh = mesh_uv 0.1 0.1; state } );
            ]))
  done;

  let compacted_texture : Scene_execution.sampled_texture =
    {
      key = "native-sampler-eviction";
      levels;
      sampler = sampler ~min_filter:Linear ~mag_filter:Linear ();
      gpu = None;
    }
  in
  List.iter
    (fun _ ->
      ignore
        (get
           (render_textured renderer
              [
                ( Scene_execution.Scene2_textured,
                  Ogpu.Pipeline.Replace,
                  Some compacted_texture,
                  { mesh = mesh_uv 0.5 0.5; state } );
              ]));
      let bytes = get (Scene_execution.read_pixels renderer ~bytes_per_row:16) in
      if
        (Char.code (Bytes.get bytes 0), Char.code (Bytes.get bytes 1), Char.code (Bytes.get bytes 2))
        <> (128, 128, 128)
      then failwith "sampler cache eviction changed the linear sampler result")
    frames;
  let replacement =
    [|
      { Scene_execution.width = 2; height = 2; bytes = Bytes.make 16 '\255' };
      { width = 1; height = 1; bytes = Bytes.of_string "\000\000\255\255" };
    |]
  in
  let replaced : Scene_execution.sampled_texture =
    { key = "native-mip-chain"; levels = replacement; sampler = sampler (); gpu = None }
  in
  let before_reload = Scene_execution.upload_bytes renderer in
  (let mesh, attrs = mesh3_uv 0.1 0.1 in
   ignore
     (get
        (Scene_execution.submit renderer
           [
             sampled ~texture:replaced ~vertex_attributes:attrs Scene_execution.Scene3_textured
               Ogpu.Pipeline.Replace { mesh; state = state3 };
           ])));
  if Scene_execution.upload_bytes renderer <= before_reload then
    failwith "texture generation replacement was not uploaded";
  (let bytes = get (Scene_execution.read_pixels renderer ~bytes_per_row:16) in
   if
     (Char.code (Bytes.get bytes 0), Char.code (Bytes.get bytes 1), Char.code (Bytes.get bytes 2))
     <> (255, 255, 255)
   then failwith "replaced texture generation did not sample the new levels");
  let malformed =
    {
      replaced with
      Scene_execution.key = "malformed";
      levels = [| { Scene_execution.width = 2; height = 2; bytes = Bytes.make 15 '\000' } |];
    }
  and before_reject = Scene_execution.upload_bytes renderer in
  begin match
    let mesh, attrs = mesh3_uv 0.1 0.1 in
    Scene_execution.submit renderer
      [
        sampled ~texture:malformed ~vertex_attributes:attrs Scene_execution.Scene3_textured
          Ogpu.Pipeline.Replace { mesh; state = state3 };
      ]
  with
  | Error _ when Scene_execution.upload_bytes renderer = before_reject -> ()
  | _ -> failwith "malformed texture was not rejected atomically"
  end;
  let corners label =
    let bytes = get (Scene_execution.read_pixels renderer ~bytes_per_row:32) in
    List.iter
      (fun at ->
        if
          ( Char.code (Bytes.get bytes at),
            Char.code (Bytes.get bytes (at + 1)),
            Char.code (Bytes.get bytes (at + 2)) )
          <> (255, 0, 128)
        then failwith ("resized pixel " ^ label))
      [ 0; 28; 7 * 32; (7 * 32) + 28 ]
  in
  get
    (Scene_execution.resize renderer { configuration with physical_width = 8; physical_height = 8 });
  ignore
    (get
       (Scene_execution_fixtures.render renderer
          [ { mesh; state = { state with viewport = (0, 0, 8, 8); scissor = (0, 0, 8, 8) } } ]));
  corners "8x8 draw";
  ignore
    (get
       (Scene_execution_fixtures.render renderer
          [ { mesh; state = { state with viewport = (0, 0, 16, 16); scissor = (-4, 0, 32, 32) } } ]));
  corners "oversized viewport and scissor";
  if List.mem 4 supported then (
    ignore
      (get
         (Scene_execution.submit renderer
            [
              sampled3 4
                {
                  mesh = mesh3;
                  state = { state3 with viewport = (0, 0, 8, 8); scissor = (0, 0, 8, 8) };
                };
            ]));
    let captured = get (Scene_execution.read_pixels renderer ~bytes_per_row:32) in
    if
      Bytes.length captured <> 256
      || Char.code (Bytes.get captured 0) <> 255
      || Char.code (Bytes.get captured 2) <> 128
    then failwith "resized multisample capture pixel");
  get (Scene_execution.destroy renderer);
  if live_handles () <> before then failwith "scene execution Metal live delta";
  print_endline "scene execution Metal: SDL-free draw/resize, exact pixel, zero delta"
