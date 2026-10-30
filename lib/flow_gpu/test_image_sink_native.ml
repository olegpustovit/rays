module G = Flow_gpu
module B = Ogpu.Backend
module P = Sop
module I = Runtime_resources.Image

let get = Test_program.ok

let resource = function
  | Ok x -> x
  | Error e -> failwith (Format.asprintf "%a" Runtime_resources.pp_error e)

let execution = function
  | Ok x -> x
  | Error e -> failwith (Format.asprintf "%a" Renderer.pp_error e)

let native = function Ok x -> x | Error e -> failwith (Ogpu.Error.to_string e)
let render canvas scene = Result.get_ok (Rays.Canvas.render canvas scene)
let canvas_stats canvas = Result.get_ok (Rays.Canvas.Private.native_stats canvas)

let packed ~width ~height body =
  let forms =
    Flow.Syntax.parse
      ("(workspace image (graph g :context image (image/map (fn [uv] " ^ body ^ "))))")
    |> get
  in
  let w =
    match Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } forms with
    | Some w, [] -> w
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let evaluated = Flow.Eval.static w |> get in
  let fn =
    match List.assoc "function" evaluated.plan.nodes.(0).args with
    | Flow.Eval.Fn f -> f
    | _ -> assert false
  in
  let kernel = Flow_sop.Image_kernel.prepare ~identity:0 ~width ~height ~fn ~sources:[] [] |> get in
  let program = Flow_sop.Image_kernel.program kernel in
  match Flow_ir.Executor.packed_region program with Some p -> (p, program) | None -> assert false

let () =
  let gpu =
    match Renderer.acquire_gpu () with
    | Ok gpu -> gpu
    | Error e -> failwith (Format.asprintf "%a" Renderer.pp_error e)
  in
  Fun.protect
    ~finally:(fun () -> Renderer.release_gpu gpu)
    (fun () ->
      let cache = G.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device gpu) in
      let sink = G.Image_sink.create gpu |> get in
      Fun.protect
        ~finally:(fun () ->
          G.Image_sink.close sink;
          G.Pipelines.close cache)
        (fun () ->
          print_endline
            "test,fixture,width,height,time,max_channel_difference,differing_channels,differing_pixels";
          List.iter
            (fun (name, body) ->
              let p, program = packed ~width:65 ~height:3 body in
              let run = G.Run.create gpu cache (Flow_shader.Emit.kernel p |> get) in
              Fun.protect
                ~finally:(fun () -> G.Run.close run)
                (fun () ->
                  let previous = ref None in
                  List.iter
                    (fun time ->
                      let live = Input.at_time time in
                      let input = Flow_ir.Packed.Host.prepare p ~live |> get in
                      let output = G.Run.dispatch run input |> get in
                      let converted = G.Image_sink.convert sink ~width:65 ~height:3 output |> get in
                      Option.iter (fun old -> assert (G.Image_sink.texture old = None)) !previous;
                      previous := Some converted;
                      let texture = G.Image_sink.texture converted |> Option.get in
                      let actual = B.read_texture texture ~bytes_per_row:260 |> native in
                      let values =
                        match Flow_ir.Executor.force program ~live |> get with
                        | Flow.Eval.Vec4_array xs -> xs
                        | _ -> assert false
                      in
                      let context = P.Context.create () |> Result.get_ok in
                      let expected =
                        P.Image.of_vec4 ~context ~width:65 ~height:3 values
                        |> Result.get_ok |> P.Image.rgba8 |> Option.get
                      in
                      let maximum = ref 0 and channels = ref 0 and pixels = ref 0 in
                      for pixel = 0 to 194 do
                        let different = ref false in
                        for channel = 0 to 3 do
                          let i = (4 * pixel) + channel in
                          let d =
                            abs (Char.code (Bytes.get actual i) - Char.code (Bytes.get expected i))
                          in
                          maximum := max !maximum d;
                          if d <> 0 then (
                            incr channels;
                            different := true)
                        done;
                        if !different then incr pixels
                      done;
                      Printf.printf "image_sink,%s,65,3,%.2f,%d,%d,%d\n%!" name time !maximum
                        !channels !pixels;
                      assert (!maximum <= 1);
                      if name = "ties" then
                        for i = 0 to 194 do
                          List.iteri
                            (fun c n -> assert (Char.code (Bytes.get actual ((4 * i) + c)) = n))
                            [ 0; 2; 2; 128 ]
                        done;
                      if name = "clip" then assert (actual = expected);
                      let creates =
                        (G.Image_sink.buffer_creations sink, G.Image_sink.texture_creations sink)
                      in
                      ignore (G.Image_sink.convert sink ~width:65 ~height:3 output |> get);
                      assert (
                        creates
                        = (G.Image_sink.buffer_creations sink, G.Image_sink.texture_creations sink));
                      assert (G.Image_sink.texture converted = None);
                      let stale = output in
                      ignore (G.Run.dispatch run input |> get);
                      assert (Result.is_error (G.Image_sink.convert sink ~width:65 ~height:3 stale)))
                    [ 0.; 0.5; 1. ]);
              assert (
                G.Image_sink.buffer_creations sink = 1 && G.Image_sink.texture_creations sink = 1))
            [
              ("gradient", "[uv.x uv.y 0.5 1]");
              ("live", "[(+ uv.x (* t 0.25)) uv.y 0.5 1]");
              ("ties", "[(/ 0.5 255) (/ 1.5 255) (/ 2.5 255) (/ 127.5 255)]");
              ("clip", "[(- uv.x 1) (+ uv.y 1) -1 2]");
            ];
          let p, _ = packed ~width:17 ~height:5 "[uv.x uv.y t 1]" in
          let run = G.Run.create gpu cache (Flow_shader.Emit.kernel p |> get) in
          Fun.protect
            ~finally:(fun () -> G.Run.close run)
            (fun () ->
              let inputs = Flow_ir.Packed.Host.prepare p ~live:(Input.at_time 0.25) |> get in
              let output = G.Run.dispatch run inputs |> get in
              let resized = G.Image_sink.convert sink ~width:17 ~height:5 output |> get in
              let texture = G.Image_sink.texture resized |> Option.get in
              let d = B.texture_descriptor texture in
              assert (d.width = 17 && d.height = 5);
              assert (
                G.Image_sink.buffer_creations sink = 1 && G.Image_sink.texture_creations sink = 2);
              assert (Domain.join (Domain.spawn (fun () -> G.Image_sink.texture resized = None)));
              G.Image_sink.close sink;
              G.Image_sink.close sink;
              assert (
                G.Image_sink.texture resized = None
                && Result.is_error (G.Image_sink.convert sink ~width:17 ~height:5 output))));
      Printf.eprintf
        "GPU image sink: odd-width pixels, ties-even/clipping, live colors, reuse, resize and \
         stale generations pass\n\
         %!")

let () =
  let forms =
    Flow.Syntax.parse
      "(workspace image\n\
      \    (defn render :context image [(bias : float)]\n\
      \      (let* [picture (image/map (fn [uv] [(+ uv.x bias) uv.y (* t 0.5) 1]))] picture))\n\
      \    (defn invoke :context image [(alias : fn)] (alias 0.125))\n\
      \    (graph g :context image (invoke :alias render)))"
    |> get
  in
  let w =
    match Flow.Workspace.check Flow.Catalog.{ version = 1; kinds = [] } forms with
    | Some w, [] -> w
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let lowered = Flow_sop.Lower.of_checked ~factories:[] w |> get in
  let node =
    Array.find_opt (fun (n : Flow.Eval.node) -> n.kind = "image/map") lowered.plan.nodes
    |> Option.get
  in
  let path = lowered.image_sites.(node.id) |> Option.get in
  assert (path = [ "def:render"; "picture" ] && path <> node.site);
  let fn = match List.assoc "function" node.args with Flow.Eval.Fn fn -> fn | _ -> assert false in
  let program =
    Flow_sop.Image_kernel.prepare ~path ~approx:lowered.approx
      ~site:(node.inst, node.site, node.iter) ~identity:0 ~width:65 ~height:17 ~fn ~sources:[] []
    |> get |> Flow_sop.Image_kernel.program
  in
  let gpu =
    match Renderer.acquire_gpu () with
    | Ok gpu -> gpu
    | Error e -> failwith (Format.asprintf "%a" Renderer.pp_error e)
  in
  Fun.protect
    ~finally:(fun () -> Renderer.release_gpu gpu)
    (fun () ->
      let host = G.Host.create ~clock:Unix.gettimeofday gpu in
      let sink = G.Image_sink.create gpu |> get in
      let image = ref None and snapshots = ref [] in
      Fun.protect
        ~finally:(fun () ->
          Option.iter (fun image -> resource (I.destroy image)) !image;
          List.iter (fun (_, _, lease) -> I.release_snapshot lease) !snapshots;
          G.Image_sink.close sink;
          G.Host.close host)
        (fun () ->
          let gpu_config =
            {
              Flow_ir.Gpu.default with
              backend = Some (G.Host.backend host);
              policy = Qualification;
            }
          in
          List.iter
            (fun time ->
              let live = Input.at_time time in
              let output =
                match Flow_ir.Executor.try_display ~gpu:gpu_config program ~live |> get with
                | Some (Gpu output) -> output
                | _ -> assert false
              in
              let converted =
                G.Image_sink.convert sink ~width:65 ~height:17
                  (G.Host.output host output |> get |> Option.get)
                |> get
              in
              let source () = G.Image_sink.texture converted in
              let current =
                match !image with
                | None ->
                    let before = Gc.allocated_bytes () in
                    let current = resource (I.of_gpu ~width:65 ~height:17 ~source) in
                    assert (Gc.allocated_bytes () -. before < 4096.);
                    image := Some current;
                    current
                | Some current ->
                    assert (Result.is_error (I.gpu_snapshot current));
                    let identity = I.identity current
                    and generation = I.generation current
                    and readbacks = I.readbacks current in
                    resource (I.replace_gpu_source current ~width:65 ~height:17 ~source);
                    assert (I.identity current = identity && I.generation current = generation + 1);
                    assert (I.readbacks current = readbacks);
                    current
              in
              assert (I.cpu_storage_bytes current = 0);
              (match resource (I.gpu_snapshot current) with
              | Some (w, h, _, texture) ->
                  assert (
                    (w, h) = (65, 17) && Option.fold ~none:false ~some:(( == ) texture) (source ()))
              | None -> assert false);
              let actual =
                B.read_texture (G.Image_sink.texture converted |> Option.get) ~bytes_per_row:260
                |> native
              in
              let values =
                match Flow_ir.Executor.force program ~live |> get with
                | Flow.Eval.Vec4_array xs -> xs
                | _ -> assert false
              in
              let context = P.Context.create () |> Result.get_ok in
              let expected =
                P.Image.of_vec4 ~context ~width:65 ~height:17 values
                |> Result.get_ok |> P.Image.rgba8 |> Option.get
              in
              let maximum = ref 0 and channels = ref 0 and pixels = ref 0 in
              for pixel = 0 to (65 * 17) - 1 do
                let different = ref false in
                for channel = 0 to 3 do
                  let i = (4 * pixel) + channel in
                  let d = abs (Char.code (Bytes.get actual i) - Char.code (Bytes.get expected i)) in
                  maximum := max !maximum d;
                  if d <> 0 then (
                    incr channels;
                    different := true)
                done;
                if !different then incr pixels
              done;
              Printf.printf "image_sink,qualified_named,65,17,%.2f,%d,%d,%d\n%!" time !maximum
                !channels !pixels;
              assert (!maximum <= 1);
              let before = I.readbacks current in
              let published = resource (I.pixels current) in
              assert (published = actual && I.readbacks current = before + 1);
              let _, _, _, bytes, lease = resource (I.borrow_snapshot current) in
              assert (bytes = actual && I.readbacks current = before + 2);
              snapshots := (bytes, Bytes.copy bytes, lease) :: !snapshots;
              assert (I.cpu_storage_bytes current = 0);
              List.iter (fun (bytes, saved, _) -> assert (bytes = saved)) !snapshots;
              Printf.printf "image_sink,published_named,65,17,%.2f,%d,%d,%d\n%!" time !maximum
                !channels !pixels)
            [ 0.; 0.5; 1. ];
          let current = Option.get !image in
          let canvas = Rays.Canvas.create ~width:65 ~height:17 |> Result.get_ok in
          Fun.protect
            ~finally:(fun () -> ignore (Rays.Canvas.destroy canvas))
            (fun () ->
              let image =
                match Rays.Image.of_resource current with
                | Ok image -> image
                | Error error -> failwith ("Image.of_resource: " ^ error)
              in
              (* [image] aliases [current]; the outer test scope owns and destroys that resource. *)
              let node = Rays.Scene.image image ~at:(0, 0) () in
              let builder = Scene_command.Display_list.Builder.create () in
              Scene_command.Display_list.Builder.image builder ~resource_id:1
                ~source:{ x = 0.; y = 0.; width = 65.; height = 17. }
                ~destination:{ x = 0.; y = 0.; width = 65.; height = 17. };
              let segment =
                Scene_command.Display_list.Builder.publish builder
                  ~id:(Scene_command.Display_list.fresh_id ())
                  ~version:0L
                |> Result.get_ok
              in
              let snapshot canvas =
                let image = Rays.Canvas.to_image canvas |> Result.get_ok in
                Fun.protect
                  ~finally:(fun () -> ignore (Rays.Image.destroy image))
                  (fun () -> Rays.Image.pixels image |> Result.get_ok)
              in
              List.iteri
                (fun index scene ->
                  let rendering =
                    Rays.Scene.Layer.stage_native_render ~width:65 ~height:17 scene |> Result.get_ok
                  in
                  if
                    List.length scene > 1
                    || match rendering.layers with [ Scene2_segment _ ] -> true | _ -> false
                  then assert (rendering.resources = []);
                  let coordinator =
                    execution
                      (Renderer.create_offscreen
                         {
                           Renderer.default_configuration with
                           logical_width = 65;
                           logical_height = 17;
                           drawable_width = 65;
                           drawable_height = 17;
                         })
                  in
                  Fun.protect
                    ~finally:(fun () -> execution (Renderer.destroy coordinator))
                    (fun () ->
                      let staged =
                        Rays.Scene.Layer.stage_native ~width:65 ~height:17 scene |> Result.get_ok
                      in
                      let lower () =
                        Renderer.lower_scene2 coordinator ~density:1
                          ~resource:(fun id -> List.assoc_opt id staged.resources)
                          staged.scene2
                      in
                      ignore (execution (lower ()));
                      ignore (execution (lower ()));
                      render canvas scene;
                      render canvas scene;
                      let old_pixels = snapshot canvas in
                      let before = I.readbacks current and generation = I.generation current in
                      let output =
                        match
                          Flow_ir.Executor.try_display ~gpu:gpu_config program
                            ~live:(Input.at_time (0.25 +. (float index *. 0.125)))
                          |> get
                        with
                        | Some (Gpu output) -> output
                        | _ -> assert false
                      in
                      let converted =
                        G.Image_sink.convert sink ~width:65 ~height:17
                          (G.Host.output host output |> get |> Option.get)
                        |> get
                      in
                      assert (I.generation current = generation);
                      assert (
                        match lower () with
                        | Error { Ogpu.Error.kind = Ogpu.Error.Invalid_state; _ } -> true
                        | _ -> false);
                      let failed = Result.is_error (Rays.Canvas.render canvas scene) in
                      assert (failed && I.readbacks current = before);
                      resource
                        (I.replace_gpu_source current ~width:65 ~height:17 ~source:(fun () ->
                             G.Image_sink.texture converted));
                      assert (I.generation current = generation + 1);
                      ignore (execution (lower ()));
                      render canvas scene;
                      let pixels = snapshot canvas in
                      assert (pixels <> old_pixels);
                      let fresh = Rays.Canvas.create ~width:65 ~height:17 |> Result.get_ok in
                      Fun.protect
                        ~finally:(fun () -> ignore (Rays.Canvas.destroy fresh))
                        (fun () ->
                          render fresh scene;
                          assert (snapshot fresh = pixels))))
                [
                  [ node ];
                  [ Rays.Scene.Layer.display_list ~images:[ (1, image) ] segment ];
                  [ node; Rays.Scene.Layer.layer_break; node ];
                ]);
          let p, _ = packed ~width:17 ~height:5 "[uv.x uv.y t 1]" in
          let cache = G.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device gpu) in
          let run = G.Run.create gpu cache (Flow_shader.Emit.kernel p |> get) in
          Fun.protect
            ~finally:(fun () ->
              G.Run.close run;
              G.Pipelines.close cache)
            (fun () ->
              let output =
                G.Run.dispatch run (Flow_ir.Packed.Host.prepare p ~live:(Input.at_time 0.25) |> get)
                |> get
              in
              let converted = G.Image_sink.convert sink ~width:17 ~height:5 output |> get in
              let identity = I.identity current in
              resource
                (I.replace_gpu_source current ~width:17 ~height:5 ~source:(fun () ->
                     G.Image_sink.texture converted));
              assert (I.identity current = identity && resource (I.size current) = (17, 5));
              assert (I.cpu_storage_bytes current = 0);
              G.Image_sink.close sink;
              assert (Result.is_error (I.gpu_snapshot current) && Result.is_error (I.pixels current)))));
  Printf.eprintf
    "GPU image qualification/publication: named-call selection, exact channel parity, zero \
     publication CPU storage/reads and retained snapshots pass\n\
     %!"

let () =
  let handles = Ogpu.Impl.live_handles in
  let before = handles () in
  let gpu = execution (Renderer.acquire_gpu ()) in
  Fun.protect
    ~finally:(fun () -> Renderer.release_gpu gpu)
    (fun () ->
      let cache = G.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device gpu) in
      let sink = G.Image_sink.create gpu |> get in
      let image = ref None and leases = ref [] in
      Fun.protect
        ~finally:(fun () ->
          Option.iter (fun image -> resource (I.destroy image)) !image;
          List.iter (fun (_, _, lease) -> I.release_snapshot lease) !leases;
          G.Image_sink.close sink;
          G.Pipelines.close cache)
        (fun () ->
          let produce width height time =
            let p, _ = packed ~width ~height "[uv.x uv.y t 1]" in
            let run = G.Run.create gpu cache (Flow_shader.Emit.kernel p |> get) in
            Fun.protect
              ~finally:(fun () -> G.Run.close run)
              (fun () ->
                let output =
                  G.Run.dispatch run
                    (Flow_ir.Packed.Host.prepare p ~live:(Input.at_time time) |> get)
                  |> get
                in
                G.Image_sink.convert sink ~width ~height output |> get)
          in
          let first = produce 65 17 0.25 in
          let current =
            resource (I.of_gpu ~width:65 ~height:17 ~source:(fun () -> G.Image_sink.texture first))
          in
          image := Some current;
          let view = Rays.Texture.of_image current |> Result.get_ok in
          let identity = Rays.Texture.Private.identity view in
          assert (Rays.Texture.size view = (65, 17));
          let wrong = Domain.spawn (fun () -> Rays.Texture.of_image current) in
          assert (Result.is_error (Domain.join wrong));
          let refuses operation =
            try
              operation ();
              false
            with Invalid_argument _ -> true
          in
          assert (refuses (fun () -> ignore (Rays.Texture.pixels view)));
          assert (refuses (fun () -> ignore (Rays.Texture.sample view ~u:0.5 ~v:0.5)));
          assert (refuses (fun () -> ignore (Rays.Texture.generate_mipmaps view)));
          assert (refuses (fun () -> ignore (Rays.Texture.Private.levels view)));
          assert (Result.is_error (Rays.Texture.subsection ~x:0 ~y:0 ~width:1 ~height:1 view));
          assert (I.cpu_storage_bytes current = 0 && I.readbacks current = 0);
          let canvas = Rays.Canvas.create ~width:65 ~height:17 |> Result.get_ok in
          Fun.protect
            ~finally:(fun () -> ignore (Rays.Canvas.destroy canvas))
            (fun () ->
              let mesh = Rays.Mesh.plane ~width:(2. *. 65. /. 17.) ~height:2. () in
              let camera =
                Rays.Camera.orthographic ~height:2. ~at:(Rays_math.Vec3.create 0. 0. 2.)
                  ~target:Rays_math.Vec3.zero ()
              in
              let mesh_scene ?viewport texture =
                Rays.Scene.
                  [
                    clear Rays_math.Color.black;
                    view3d ?viewport ~camera
                      (Rays.Scene.D3.create ~ambient:Rays_math.Color.white
                         [
                           Rays.Scene.D3.mesh
                             ~material:(Rays.Material.matte Rays_math.Color.white)
                             ~cull:Cull_none
                             ~texture:(Rays.Scene.D3.textured ~filter:Nearest texture)
                             mesh;
                         ]);
                  ]
              in
              let image_scene source_image =
                Rays.Scene.[ clear Rays_math.Color.black; image source_image ~at:(0, 0) () ]
              in
              let displayed = image_scene (Rays.Image.of_resource current |> Result.get_ok)
              and meshed = mesh_scene view in
              let mixed source_image texture =
                Rays.Scene.
                  [
                    clear Rays_math.Color.black;
                    group [ image source_image ~at:(0, 0) () ];
                    Layer.layer_break;
                    group (List.tl (mesh_scene ~viewport:(33, 0, 32, 17) texture));
                  ]
              in
              let mixed_scene = mixed (Rays.Image.of_resource current |> Result.get_ok) view in
              let scenes = ref [ displayed; meshed; mixed_scene ] in
              let snapshot target =
                let image = Rays.Canvas.to_image target |> Result.get_ok in
                Fun.protect
                  ~finally:(fun () -> ignore (Rays.Image.destroy image))
                  (fun () -> Rays.Image.pixels image |> Result.get_ok)
              in
              let verify converted width height time =
                let bytes =
                  B.read_texture
                    (G.Image_sink.texture converted |> Option.get)
                    ~bytes_per_row:(width * 4)
                  |> native
                in
                let reference_image =
                  Rays.Image.upload_rgba ~width ~height ~rgba:bytes () |> Result.get_ok
                in
                Fun.protect
                  ~finally:(fun () -> ignore (Rays.Image.destroy reference_image))
                  (fun () ->
                    let reference_texture =
                      Rays.Texture.create_owned ~width ~height
                        (Array.init (width * height) (fun i ->
                             let o = i * 4 in
                             Rays_math.Color.rgba
                               (Char.code (Bytes.get bytes o))
                               (Char.code (Bytes.get bytes (o + 1)))
                               (Char.code (Bytes.get bytes (o + 2)))
                               (Char.code (Bytes.get bytes (o + 3)))))
                      |> Result.get_ok
                    in
                    let references =
                      [
                        image_scene reference_image;
                        mesh_scene reference_texture;
                        mixed reference_image reference_texture;
                      ]
                    in
                    List.iteri
                      (fun index (scene, reference) ->
                        let reads = I.readbacks current in
                        render canvas scene;
                        render canvas scene;
                        let uploaded = (canvas_stats canvas).uploaded_bytes in
                        render canvas scene;
                        assert ((canvas_stats canvas).uploaded_bytes = uploaded);
                        assert (I.readbacks current = reads);
                        let actual = snapshot canvas in
                        let fresh = Rays.Canvas.create ~width:65 ~height:17 |> Result.get_ok in
                        Fun.protect
                          ~finally:(fun () -> ignore (Rays.Canvas.destroy fresh))
                          (fun () ->
                            render fresh reference;
                            assert (snapshot fresh = actual));
                        Printf.printf "image_sink,resident_%s_source%dx%d,65,17,%.2f,0,0,0\n%!"
                          [| "image"; "mesh"; "mixed" |].(index)
                          width height time)
                      (List.combine !scenes references))
              in
              let staged =
                Rays.Scene.Layer.stage_native_render_checked ~width:65 ~height:17
                  (List.nth !scenes 1)
                |> Result.get_ok
              in
              assert (match staged.mesh_images with [ image ] -> image == current | _ -> false);
              let sampled = (List.hd staged.scene3).entries.(0).texture |> Option.get in
              assert (Option.is_some sampled.gpu && sampled.levels.(0).bytes = Bytes.empty);
              verify first 65 17 0.25;
              assert (I.readbacks current = 0);
              let _, _, _, saved, lease = resource (I.borrow_snapshot current) in
              leases := [ (saved, Bytes.copy saved, lease) ];
              let expire () =
                List.iter
                  (fun scene ->
                    (match
                       Rays.Scene.Layer.stage_native_render_checked ~width:65 ~height:17 scene
                     with
                    | Error (Resource error) -> assert (error.kind = Destroyed)
                    | Ok _ -> ()
                    | Error error ->
                        failwith
                          (Format.asprintf "resident expiry staging: %a"
                             Rays.Scene.Layer.pp_native_error error));
                    let reads = I.readbacks current in
                    assert (Result.is_error (Rays.Canvas.render canvas scene));
                    assert (I.readbacks current = reads))
                  !scenes
              in
              let generation = I.generation current in
              let second = produce 65 17 0.75 in
              assert (I.generation current = generation);
              expire ();
              resource
                (I.replace_gpu_source current ~width:65 ~height:17 ~source:(fun () ->
                     G.Image_sink.texture second));
              verify second 65 17 0.75;
              let resized = produce 17 5 0.5 in
              expire ();
              resource
                (I.replace_gpu_source current ~width:17 ~height:5 ~source:(fun () ->
                     G.Image_sink.texture resized));
              resource
                (I.replace current ~width:17 ~height:5 ~rgba:(Bytes.make (17 * 5 * 4) '\255'));
              assert (Result.is_error (Rays.Texture.of_image current));
              assert (
                match Rays.Scene.Layer.stage_native_render_checked ~width:65 ~height:17 meshed with
                | Error (Message message) ->
                    String.ends_with ~suffix:"borrowed GPU texture needs an explicit CPU snapshot"
                      message
                | _ -> false);
              assert (I.readbacks current = 1);
              resource
                (I.replace_gpu_source current ~width:17 ~height:5 ~source:(fun () ->
                     G.Image_sink.texture resized));
              assert (
                Rays.Texture.Private.identity view = identity && Rays.Texture.size view = (17, 5));
              let resized_image = Rays.Image.of_resource current |> Result.get_ok in
              scenes := [ image_scene resized_image; meshed; mixed resized_image view ];
              verify resized 17 5 0.5;
              List.iter (fun (bytes, saved, _) -> assert (bytes = saved)) !leases;
              let foreign_driver = Ogpu.Impl.create_driver () in
              let device = native (B.create_device foreign_driver) in
              Fun.protect
                ~finally:(fun () -> native (B.destroy_device device))
                (fun () ->
                  let texture =
                    native
                      (B.create_texture device
                         {
                           Ogpu.Types.label = None;
                           width = 17;
                           height = 5;
                           depth = 1;
                           mip_levels = 1;
                           sample_count = 1;
                           format = Rgba8_unorm;
                           usage = [ Texture_binding; Texture_copy_src ];
                         })
                  in
                  Fun.protect
                    ~finally:(fun () -> native (B.destroy_texture texture))
                    (fun () ->
                      resource
                        (I.replace_gpu_source current ~width:17 ~height:5 ~source:(fun () ->
                             Some texture));
                      List.iter
                        (fun scene ->
                          assert (
                            match Rays.Canvas.render canvas scene with
                            | Error message ->
                                String.ends_with ~suffix:"GPU image belongs to another renderer"
                                  message
                            | Ok () -> false))
                        !scenes;
                      assert (I.readbacks current = 1)));
              resource
                (I.replace_gpu_source current ~width:17 ~height:5 ~source:(fun () ->
                     G.Image_sink.texture resized));
              let texture = G.Image_sink.texture resized |> Option.get in
              resource (I.destroy current);
              assert (not (B.texture_destroyed texture));
              expire ();
              List.iter (fun (bytes, saved, _) -> assert (bytes = saved)) !leases)));
  assert (handles () = before);
  Printf.eprintf
    "Resident image consumers: offscreen image/mesh parity, zero warm uploads/readbacks, expiry, \
     resize, cross-device rejection and ownership pass\n\
     %!"
