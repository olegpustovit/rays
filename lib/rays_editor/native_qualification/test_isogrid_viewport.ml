module Gpu = Flow_native.Workspace_gpu
module G = Rdk.Geometry

let ok = function Ok v -> v | Error d -> failwith (Diagnostic.to_string d)
let string_ok = function Ok v -> v | Error s -> failwith s

let stats gpu =
  let host, _, _, _, _ = Gpu.image_stats gpu in
  host

let run gpu config workspace recipe =
  let catalog = ok (Rays_editor.workspace_catalog ()) in
  let value = List.hd (ok (Flow.Syntax.parse (string_of_int recipe))) in
  let workspace =
    ok
      (Editor_document.Workspace_doc.edit catalog workspace
         (Flow_graph.Flow_edit.Set_arg
            { node = [ "volume"; "recipe-switch" ]; key = Whole; sub = []; value }))
  in
  let publication = stats gpu in
  let lowered =
    ok
      (Flow_sop.Lower.of_checked ~gpu:config ~defer:true ~inputs:workspace.inputs workspace.checked)
  in
  let graph =
    List.find (fun (g : Flow_sop.Lower.graph) -> g.default && g.name = "sculpture") lowered.graphs
  in
  let node =
    string_ok (Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root))
  in
  let session = string_ok (Sop.Session.create ~max_entries:0 ~max_payload_bytes:(1 lsl 28)) in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      (* Ordinary measured policy, as the viewport's display root is cooked (no host reader): each
         route runs until the shared comparison has numbers, so cook again until the whole mesh is
         resident. *)
      let cook () =
        let output =
          ok
            (Sop.Session.cook ~cpu_consumers:0 session
               ~context:(string_ok (Sop.Context.create ~domains:1 ()))
               node)
        in
        ok (Sop.Payload.geometry output.payload)
      in
      let resident geometry =
        let attribute name =
          Option.fold ~none:false ~some:Rdk.Attribute.resident
            (G.find_attribute ~owner:Point name geometry)
        in
        Rdk.Packed.Float3.resident (G.positions geometry)
        && attribute "N" && attribute "Cd"
        && Rdk.Topology.resident (G.topology geometry)
      in
      let rec cook_until_resident attempts =
        let geometry = cook () in
        if resident geometry then geometry
        else if attempts = 0 then failwith "measured policy did not keep the IsoGrid mesh resident"
        else cook_until_resident (attempts - 1)
      in
      let geometry = cook_until_resident 8 in
      let before = stats gpu in
      assert (before.payload_readback_bytes = publication.payload_readback_bytes);
      let surface = string_ok (Rdk_rays.Surface.of_geometry geometry) in
      assert ((stats gpu).payload_readback_bytes = before.payload_readback_bytes);
      let scene = Rdk_rays.Surface.scene3 ~device:(Gpu.display gpu) surface in
      assert (Gpu.display_refusal gpu = None);
      let mesh = (List.hd (Rays.Scene.D3.Walk.drawings scene)).mesh in
      assert (Rays.Mesh.View.device mesh <> None);
      assert (Rays.Mesh.View.device_index_count mesh = Some (G.primitive_count geometry * 6));
      assert (Rays.Mesh.View.device_color_width mesh = 3);
      let bound = stats gpu in
      assert (bound.payload_readback_bytes = before.payload_readback_bytes);
      assert (bound.input_uploaded_bytes = before.input_uploaded_bytes);
      assert (bound.validation_readback_bytes = before.validation_readback_bytes + 4);
      let camera =
        Rays.Camera.perspective
          ~fov_y:(36. *. Float.pi /. 180.)
          ~at:(Rays_math.Vec3.create 10. 8. 10.)
          ~target:Rays_math.Vec3.zero ()
      in
      let frame = Rays.Scene.[ clear Rays_math.Color.black; view3d ~camera scene ] in
      let staged = string_ok (Rays.Scene.Layer.stage_native ~width:256 ~height:256 frame) in
      let entry = (List.hd staged.scene3).entries.(0) in
      assert (Bytes.length entry.draw.mesh.vertices = 0 && Bytes.length entry.draw.mesh.indices = 0);
      assert (
        match entry.vertex_attributes with Some (_, bytes) -> Bytes.length bytes = 0 | _ -> false);
      let canvas = Rays.Canvas.create_exn ~width:256 ~height:256
      and host_canvas = Rays.Canvas.create_exn ~width:256 ~height:256 in
      Fun.protect
        ~finally:(fun () ->
          string_ok (Rays.Canvas.destroy canvas);
          string_ok (Rays.Canvas.destroy host_canvas);
          Gpu.frame ~keep:[] gpu;
          Gpu.frame gpu;
          Gpu.frame gpu)
        (fun () ->
          string_ok (Rays.Canvas.render canvas frame);
          let warm = stats gpu and renderer = string_ok (Rays.Canvas.Private.native_stats canvas) in
          assert (renderer.uploaded_bytes < 65536L);
          let allocated = Gc.allocated_bytes () in
          for _ = 1 to 3 do
            string_ok (Rays.Canvas.render canvas frame)
          done;
          let frame_allocation = (Gc.allocated_bytes () -. allocated) /. 3. in
          assert (frame_allocation < 131072.);
          let after = stats gpu
          and rendered = string_ok (Rays.Canvas.Private.native_stats canvas) in
          assert (rendered.uploaded_bytes = renderer.uploaded_bytes);
          assert (rendered.pipeline_cache_entries = renderer.pipeline_cache_entries);
          assert (after.payload_readback_bytes = before.payload_readback_bytes);
          assert (
            after.buffer_creations = warm.buffer_creations
            && after.pipeline_compilations = warm.pipeline_compilations);
          assert (after.validation_readback_bytes = warm.validation_readback_bytes);
          assert (Rdk.Topology.resident (G.topology geometry));
          assert (Rdk.Packed.Float3.resident (G.positions geometry));
          let pixels = string_ok (Rays.Canvas.pixels canvas) in
          assert (
            Array.fold_left
              (fun n (c : Rays_math.Color.t) -> n + if c.r + c.g + c.b > 0 then 1 else 0)
              0 pixels
            > 1024);
          let bounds = Rdk_rays.Geometry_view.bounds geometry in
          assert ((stats gpu).payload_readback_bytes = before.payload_readback_bytes);
          assert ((stats gpu).bounds_readback_bytes - after.bounds_readback_bytes <= 24576);
          let orbit =
            Rays.Camera.perspective
              ~at:(Rays_math.Vec3.create (-10.) 5. 7.)
              ~target:Rays_math.Vec3.zero ()
          in
          let lo, hi = Option.get bounds in
          let framed =
            Rays.Easy_camera.create ()
            |> Rays.Easy_camera.frame_bounds ~min:lo ~max:hi
            |> Rays.Easy_camera.camera
          in
          let world =
            {
              (Rays.World.bake ~domains:1 ~width:32 ~height:16 Rays.World.default) with
              sun =
                Some
                  {
                    direction = Rays_math.Vec3.normalize (Rays_math.Vec3.create 0.3 1. 0.2);
                    radiance = Rays.World.rgb 4. 4. 4.;
                    angular_radius = 0.01;
                  };
            }
          in
          let variants =
            [
              ("orbit", orbit, Fun.id);
              ("frame", framed, Fun.id);
              ("shadow", camera, Rays.Scene.D3.with_world world);
            ]
          in
          let variants =
            List.map
              (fun (name, camera, adapt) ->
                let frame =
                  Rays.Scene.[ clear Rays_math.Color.black; view3d ~camera (adapt scene) ]
                in
                string_ok (Rays.Canvas.render canvas frame);
                let first = string_ok (Rays.Canvas.Private.native_stats canvas) in
                string_ok (Rays.Canvas.render canvas frame);
                let next = string_ok (Rays.Canvas.Private.native_stats canvas) in
                assert (first.uploaded_bytes = next.uploaded_bytes);
                assert (first.sun_shadow_passes = next.sun_shadow_passes);
                (name, camera, adapt, string_ok (Rays.Canvas.pixels canvas)))
              variants
          in
          assert ((stats gpu).payload_readback_bytes = before.payload_readback_bytes);
          let summary = Rdk_rays.Geometry_view.summary geometry in
          assert (summary.points = G.point_count geometry);
          assert (summary.prims = G.primitive_count geometry);
          for owner = 0 to 3 do
            ignore (Rdk_rays.Geometry_view.count owner geometry)
          done;
          assert ((stats gpu).payload_readback_bytes = before.payload_readback_bytes);
          (* Exercise the actual Pick/Spreadsheet readers before the tracer can
             materialise them: each recipe starts with a different CPU reader. *)
          let consumer_start = (stats gpu).payload_readback_bytes in
          let consumer =
            if recipe = 0 then begin
              let index = Rdk_rays.Geometry_view.surface geometry in
              assert (index <> None);
              assert (not (Rdk.Topology.resident (G.topology geometry)));
              assert (not (Rdk.Packed.Float3.resident (G.positions geometry)));
              let topology = G.topology geometry in
              let first, last = Rdk.Topology.primitive_vertex_range topology 0 in
              assert (last - first = 4);
              let point i =
                let x, y, z =
                  Rdk.Packed.Float3.get (G.positions geometry)
                    (Rdk.Topology.point_of_vertex topology (first + i))
                in
                Rays_math.Vec3.create x y z
              in
              let open Rays_math.Vec3 in
              let center = scale (add (add (point 0) (point 1)) (add (point 2) (point 3))) 0.25 in
              let normal = normalize (cross (sub (point 1) (point 0)) (sub (point 2) (point 0))) in
              let cast () =
                Rdk_rays.Geometry_view.cast_material index geometry
                  ~origin:(add center (scale normal 0.001))
                  ~direction:(scale normal (-1.))
              in
              let hit = cast () in
              assert (
                match hit with
                | Some (distance, _) -> abs_float (distance -. 0.001) < 1e-6
                | None -> false);
              let read = (stats gpu).payload_readback_bytes in
              assert (cast () = hit);
              assert ((stats gpu).payload_readback_bytes = read);
              "picking"
            end
            else begin
              let cells () =
                List.init 4 (fun owner ->
                    let count = Rdk_rays.Geometry_view.count owner geometry in
                    Array.map
                      (fun (name, cell) ->
                        (name, if count = 0 then [] else List.map cell [ 0; count / 2; count - 1 ]))
                      (Rdk_rays.Geometry_view.columns owner geometry))
              in
              let first = cells () in
              assert (Array.exists (fun (name, _) -> name = "Cd.x") (List.hd first));
              assert (not (Rdk.Packed.Float3.resident (G.positions geometry)));
              assert (Rdk.Topology.resident (G.topology geometry));
              let read = (stats gpu).payload_readback_bytes in
              assert (cells () = first);
              assert ((stats gpu).payload_readback_bytes = read);
              "inspection"
            end
          in
          let consumer_bytes = (stats gpu).payload_readback_bytes - consumer_start in
          assert (consumer_bytes > 0);
          Printf.printf "IsoGrid recipe %d: %s materialises %d payload bytes; repeated read +0\n%!"
            recipe consumer consumer_bytes;
          let host_scene =
            let trace_start = (stats gpu).payload_readback_bytes in
            let prepared_trace =
              string_ok
                (Rays_pathtracer.mesh
                   [
                     (mesh, Rays_pathtracer.material (Rays_pathtracer.Linear_color.rgb 0.8 0.7 0.6));
                   ])
            in
            ignore
              (string_ok (Rays_pathtracer.scene_mesh [ (Rays_math.Mat4.identity, prepared_trace) ]));
            assert ((stats gpu).payload_readback_bytes > trace_start);
            Rdk_rays.Surface.scene3 (string_ok (Rdk_rays.Surface.of_geometry geometry))
          in
          string_ok
            (Rays.Canvas.render host_canvas
               Rays.Scene.[ clear Rays_math.Color.black; view3d ~camera host_scene ]);
          assert ((stats gpu).payload_readback_bytes > before.payload_readback_bytes);
          let host_pixels = string_ok (Rays.Canvas.pixels host_canvas) in
          let worst = ref 0 in
          Array.iteri
            (fun i (c : Rays_math.Color.t) ->
              let d : Rays_math.Color.t = host_pixels.(i) in
              worst :=
                max !worst
                  (max
                     (max (abs (c.r - d.r)) (abs (c.g - d.g)))
                     (max (abs (c.b - d.b)) (abs (c.a - d.a)))))
            pixels;
          assert (!worst = 0);
          let read_for_trace = (stats gpu).payload_readback_bytes in
          let host_mesh = (List.hd (Rays.Scene.D3.Walk.drawings host_scene)).mesh in
          List.iter
            (fun mode ->
              let draw source canvas =
                let picture =
                  Rays.Scene.D3.create
                    [ Rays.Scene.D3.mesh ~mode ~cull:Rays.Scene.D3.Cull_none source ]
                in
                string_ok
                  (Rays.Canvas.render canvas
                     Rays.Scene.[ clear Rays_math.Color.black; view3d ~camera picture ]);
                string_ok (Rays.Canvas.pixels canvas)
              in
              assert (draw mesh canvas = draw host_mesh host_canvas))
            [ Rays.Scene.D3.Wireframe; Rays.Scene.D3.Vertices ];
          assert ((stats gpu).payload_readback_bytes = read_for_trace);
          List.iter
            (fun (name, camera, adapt, expected) ->
              string_ok
                (Rays.Canvas.render host_canvas
                   Rays.Scene.[ clear Rays_math.Color.black; view3d ~camera (adapt host_scene) ]);
              let actual = string_ok (Rays.Canvas.pixels host_canvas) in
              if actual <> expected then
                failwith (Printf.sprintf "recipe %d %s pixels differ" recipe name))
            variants;
          assert (bounds = Rdk_rays.Geometry_view.bounds geometry);
          let read = (stats gpu).payload_readback_bytes in
          string_ok (Rays.Canvas.render canvas frame);
          assert (string_ok (Rays.Canvas.pixels canvas) = pixels);
          assert ((stats gpu).payload_readback_bytes = read);
          string_ok
            (Rays.Canvas.render host_canvas
               Rays.Scene.[ clear Rays_math.Color.black; view3d ~camera host_scene ]);
          assert (string_ok (Rays.Canvas.pixels host_canvas) = pixels);
          Option.iter
            (fun directory ->
              if not (Sys.file_exists directory) then Unix.mkdir directory 0o755;
              string_ok
                (Rays.Canvas.save_png canvas
                   (Filename.concat directory (Printf.sprintf "recipe-%d-device.png" recipe)));
              string_ok
                (Rays.Canvas.save_png host_canvas
                   (Filename.concat directory (Printf.sprintf "recipe-%d-host.png" recipe))))
            (Sys.getenv_opt "RAYS_S9_IMAGES");
          Printf.printf
            "IsoGrid recipe %d: %d quads, pixel difference %d, publication/display payload %d/%d, \
             validation +4, renderer upload %Ld, %.0f allocated bytes/warm frame, \
             orbit/frame/shadow/modes parity, trace preparation materialises\n\
             %!"
            recipe (G.primitive_count geometry) !worst
            (before.payload_readback_bytes - publication.payload_readback_bytes)
            (after.payload_readback_bytes - before.payload_readback_bytes)
            renderer.uploaded_bytes frame_allocation))

let () =
  Printexc.record_backtrace true;
  let file = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sketches/isogrid.rays" in
  let text = In_channel.with_open_bin file In_channel.input_all in
  let frozen = "0fdb9b0ebec8f5fe84227d7c0963dd3ec8c446c6145e145555b4fa4f80744e69" in
  assert (Editor_document.Lowering.sha256 text = frozen);
  let workspace =
    match Rays_editor.Workspace.load text with
    | Ok w -> w
    | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let baseline = Ogpu.Impl.live_handles () in
  let gpu = Gpu.create () in
  Fun.protect
    ~finally:(fun () -> Gpu.close gpu)
    (fun () ->
      Gpu.with_backend ~fast:true gpu (fun config -> List.iter (run gpu config workspace) [ 0; 3 ]));
  assert (Ogpu.Impl.live_handles () = baseline);
  assert (
    Editor_document.Lowering.sha256 (In_channel.with_open_bin file In_channel.input_all) = frozen)
