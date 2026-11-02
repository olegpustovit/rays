open Rays

let render canvas scene = Result.get_ok (Canvas.render canvas scene)
let stats canvas = Result.get_ok (Canvas.Private.native_stats canvas)
let mesh_of = Test_meshes.facing_z

let run () =
  Scene.Layer.with_cache_context (Scene.Layer.create_cache_context ()) (fun () ->
      let camera =
        Camera.orthographic ~height:2. ~at:(Rays_math.Vec3.create 0. 0. 2.)
          ~target:Rays_math.Vec3.zero ()
      in
      let prepare ~width ~height scene =
        match Scene.Layer.stage_native ~width ~height [ Scene.view3d ~camera scene ] with
        | Ok { scene3 = [ prepared ]; _ } -> prepared
        | Ok _ -> failwith "native Scene3 staging cardinality"
        | Error message -> failwith message
      in
      begin match
        Scene.Layer.stage_native ~width:0 ~height:16 [ Scene.view3d ~camera Scene.D3.empty ]
      with
      | Error _ -> print_endline "native Scene3 lowering: typed viewport rejection"
      | _ -> failwith "native Scene3 accepted empty viewport"
      end;
      let mesh =
        mesh_of ~indices:[| 0; 1; 2 |]
          ~colors:
            [| Rays_math.Color.red; Rays_math.Color.rgb 0 255 0; Rays_math.Color.rgb 0 0 255 |]
          [|
            Rays_math.Vec3.create (-0.5) (-0.5) 0.;
            Rays_math.Vec3.create 0.5 (-0.5) 0.;
            Rays_math.Vec3.create 0. 0.5 0.;
          |]
      in
      let scene =
        Scene.D3.create [ Scene.D3.mesh ~material:(Material.unlit Rays_math.Color.white) mesh ]
      in
      let lights n = List.init n (fun _ -> Light.directional ~direction:Rays_math.Vec3.unit_z ()) in
      ignore
        (prepare ~width:16 ~height:16 (Scene.D3.create ~lights:(lights 64) [ Scene.D3.mesh mesh ]));
      (match
         Scene.Layer.stage_native ~width:16 ~height:16
           [ Scene.view3d ~camera (Scene.D3.create ~lights:(lights 65) [ Scene.D3.mesh mesh ]) ]
       with
      | Error message when message = "native View3d lowering failed: more than 64 lights" -> ()
      | _ -> failwith "native Scene3 accepted more than 64 lights");
      let retained_frame = Scene.[ clear Rays_math.Color.black; view3d ~camera scene ] in
      let retained_first =
        Result.get_ok (Scene.Layer.stage_native ~width:16 ~height:16 retained_frame)
      in
      let retained_second =
        Result.get_ok (Scene.Layer.stage_native ~width:16 ~height:16 retained_frame)
      in
      if retained_first != retained_second then
        failwith "resource-free retained frame did not reuse native staging";
      (match retained_first.retained with
      | Some (identity, 1L) when identity <> "" -> ()
      | _ -> failwith "resource-free retained frame has no stable submission identity");
      let reusable_view = Scene.[ view3d ~camera scene ] in
      let before_release =
        Result.get_ok (Scene.Layer.stage_native ~width:16 ~height:16 reusable_view)
      in
      Scene.Layer.release reusable_view;
      let after_release =
        Result.get_ok (Scene.Layer.stage_native ~width:16 ~height:16 reusable_view)
      in
      if before_release.scene3 <> after_release.scene3 then
        failwith "released reusable View3d changed prepared native data";
      let prepared = prepare ~width:16 ~height:16 scene in
      let with_ui ~x scene =
        let builder = Scene_command.Ui_batch.Builder.create () in
        Scene_command.Ui_batch.Builder.rect builder ~x ~y:0. ~width:1. ~height:1. ();
        let batch = Scene_command.Ui_batch.Builder.publish builder in
        Result.get_ok
          (Scene.Layer.stage_native_render ~width:16 ~height:16
             [ Scene.view3d ~camera scene; Scene.Layer.ui batch ])
      in
      let first_ui = with_ui ~x:0. scene in
      let equivalent =
        Scene.D3.create [ Scene.D3.mesh ~material:(Material.unlit Rays_math.Color.white) mesh ]
      in
      let second_ui = with_ui ~x:1. equivalent in
      if List.hd first_ui.scene3 != List.hd second_ui.scene3 then
        failwith "changed UI rebuilt an equivalent 3D layer's uniforms";
      let changed_material =
        with_ui ~x:2.
          (Scene.D3.create [ Scene.D3.mesh ~material:(Material.unlit Rays_math.Color.red) mesh ])
      in
      if List.hd second_ui.scene3 == List.hd changed_material.scene3 then
        failwith "changed material reused stale 3D uniforms";
      let module Native = Native_scene_lowering in
      Native.with_cache_context (Native.create_cache_context ()) (fun () ->
          let first = Native.draws_of_prepared retained_first prepared in
          let second =
            Native.draws_of_prepared
              { retained_first with retained = Some ("different frame", 2L) }
              prepared
          in
          if first != second then
            failwith "unchanged 3D layer rebuilt its draws for a different UI frame");
      if Array.length prepared.entries <> 1 then failwith "native triangle draw count";
      let entry = prepared.entries.(0) and vertices = prepared.entries.(0).draw.mesh.vertices in
      if Bytes.length vertices <> 72 || entry.draw.mesh.index_count <> 3 then
        failwith "native Scene3 ABI cardinality";
      (* blending an opaque draw keeps a tile-based GPU from dropping its hidden fragments *)
      if entry.blend <> Ogpu.Pipeline.Replace then
        failwith "an opaque default-blend draw kept blending";
      if
        Int32.float_of_bits (Bytes.get_int32_le vertices 0) <> -0.5
        ||
        match entry.vertex_attributes with
        | Some (_, bytes) -> Bytes.get_int32_le bytes 0 <> 0xff0000ffl
        | None -> true
      then failwith "native Scene3 vertex ABI";
      (match entry.draw.state.transform_uniforms with
      | Some bytes
        when Bytes.length bytes = 5456
             && Int32.float_of_bits (Bytes.get_int32_le bytes (64 * 4)) = 1. ->
          ()
      | _ -> failwith "native Scene3 uniform ABI");
      let check_clip_depth label offset bytes =
        let word index = Int32.float_of_bits (Bytes.get_int32_le bytes ((offset + index) * 4)) in
        let depth z = ((word 10 *. z) +. word 11) /. ((word 14 *. z) +. word 15) in
        if Float.abs (depth 1.9) > 1e-6 || Float.abs (depth (-998.) -. 1.) > 1e-6 then
          failwith (label ^ ": camera near/far must map to native depth 0/1")
      in
      check_clip_depth "ordinary MVP" 0 (Option.get entry.draw.state.transform_uniforms);
      let instanced =
        prepare ~width:16 ~height:16
          (Scene.D3.create
             [
               Scene.D3.instances_array mesh [| Rays_math.Mat4.identity; Rays_math.Mat4.identity |];
             ])
      in
      check_clip_depth "instance MVP" 1364
        (Option.get instanced.entries.(0).draw.state.transform_uniforms);
      let staged =
        Result.get_ok (Scene.Layer.stage_native ~width:16 ~height:16 [ Scene.view3d ~camera scene ])
      in
      if List.length staged.scene3 <> 1 || Array.length (List.hd staged.scene3).entries <> 1 then
        failwith "View3d did not reach structured native staging";
      let texture =
        Texture.init ~width:2 ~height:2 (fun ~x ~y ->
            if x = y then Rays_math.Color.red else Rays_math.Color.rgb 0 0 255)
        |> Texture.generate_mipmaps
      in
      let textured =
        Scene.D3.create
          [ Scene.D3.mesh ~texture:(Scene.D3.textured ~filter:Texture.Trilinear texture) mesh ]
      in
      let textured_frame = Scene.[ clear Rays_math.Color.black; view3d ~camera textured ] in
      let textured_first =
        Result.get_ok (Scene.Layer.stage_native ~width:16 ~height:16 textured_frame)
      and textured_second =
        Result.get_ok (Scene.Layer.stage_native ~width:16 ~height:16 textured_frame)
      in
      if
        textured_first == textured_second || textured_first.retained <> None
        || textured_second.retained <> None
      then failwith "resource-bearing retained frame reused stale native staging";
      let textured_stage =
        Result.get_ok
          (Scene.Layer.stage_native ~width:16 ~height:16 [ Scene.view3d ~camera textured ])
      in
      let textured_entry = (List.hd textured_stage.scene3).entries.(0) in
      (match (textured_entry.family, textured_entry.texture) with
      | Renderer.Scene_execution.Scene3_textured, Some value
        when Array.length value.levels = 2 && Bytes.length value.levels.(0).bytes = 16 ->
          ()
      | _ -> failwith "native texture mip/sampler staging");
      let light = Light.directional ~direction:(Rays_math.Vec3.create 0. 0. (-1.)) () in
      let lit = Scene.D3.create ~lights:[ light ] [ Scene.D3.mesh mesh ] in
      let baked = World.bake ~domains:1 ~width:64 ~height:32 World.default in
      let world_stage = prepare ~width:16 ~height:16 (Scene.D3.with_world baked lit) in
      let inverse = Option.get world_stage.entries.(0).draw.state.transform_uniforms in
      let word index = Int32.float_of_bits (Bytes.get_int32_le inverse ((84 + index) * 4)) in
      let world_z depth = ((word 10 *. depth) +. word 11) /. ((word 14 *. depth) +. word 15) in
      if Float.abs (world_z 0. -. 1.9) > 1e-5 || Float.abs (world_z 1. +. 998.) > 1e-3 then
        failwith "World background inverse did not use native depth endpoints";
      let strip =
        mesh_of ~mode:Mesh.Triangle_strip ~indices:[| 0; 1; 2; 3 |]
          [|
            Rays_math.Vec3.zero;
            Rays_math.Vec3.unit_x;
            Rays_math.Vec3.unit_y;
            Rays_math.Vec3.create 1. 1. 0.;
          |]
      in
      let strip_scene = Scene.D3.create [ Scene.D3.mesh strip ] in
      let strip_prepared = prepare ~width:16 ~height:16 strip_scene in
      let encoded = strip_prepared.entries.(0).draw.mesh.indices in
      let actual =
        Array.init 6 (fun index -> Int32.to_int (Bytes.get_int32_le encoded (index * 4)))
      in
      if actual <> [| 0; 1; 2; 2; 1; 3 |] then failwith "native triangle strip winding";
      let first = prepare ~width:16 ~height:16 scene in
      let second = prepare ~width:16 ~height:16 scene in
      if first != second then
        failwith "stable texture-free Scene3 did not reuse its prepared payload";
      if
        first.entries.(0).draw.mesh.vertices != second.entries.(0).draw.mesh.vertices
        || first.entries.(0).draw.mesh.key <> second.entries.(0).draw.mesh.key
      then failwith "stable Scene3 mesh was repacked for a camera-only prepare";
      print_endline
        "Scene3 lowering: exact released View3d reuse, geometry, uniforms, texture mips, World \
         depth, stable pack")

let run_native () =
  let canvas = Canvas.create_exn ~width:64 ~height:64 in
  Fun.protect
    ~finally:(fun () -> ignore (Canvas.destroy canvas))
    (fun () ->
      let count = 4096 in
      let points =
        Array.init count (fun i ->
            Rays_math.Vec3.create
              ((float (i mod 64) /. 32.) -. 1.)
              ((float (i / 64) /. 32.) -. 1.)
              0.)
      in
      let indices =
        Array.init ((count - 2) * 3) (fun i -> if i mod 3 = 0 then 0 else (i / 3) + (i mod 3))
      in
      let heavy = mesh_of ~indices points in
      let world =
        Scene.D3.create [ Scene.D3.mesh ~material:(Material.unlit Rays_math.Color.red) heavy ]
      in
      let cam_at position =
        Camera.orthographic ~height:2. ~at:position ~target:Rays_math.Vec3.zero ()
      in
      render canvas
        Scene.
          [
            clear Rays_math.Color.black;
            view3d ~viewport:(0, 0, 64, 64) ~camera:(cam_at (Rays_math.Vec3.create 0. 0. 2.)) world;
          ];
      let after_first = (stats canvas).uploaded_bytes in
      render canvas
        Scene.
          [
            clear Rays_math.Color.black;
            view3d ~viewport:(0, 0, 64, 64) ~camera:(cam_at (Rays_math.Vec3.create 0.2 0. 2.)) world;
          ];
      let delta = Int64.sub (stats canvas).uploaded_bytes after_first in
      if delta > 32_768L then
        failwith (Printf.sprintf "camera orbit reuploaded Scene3 vertices (%Ld bytes)" delta));
  print_endline "native Scene3 lowering: camera orbit preserves uploaded geometry"

(* A device mesh's host copy is made once, only for a reader of vertices: the raster lowering reads
   its own arrays (unread) and a sun fit its bounds, a path tracer its host copy. *)
let device_host () =
  let made = ref 0 in
  let host () =
    incr made;
    Test_meshes.box ~width:1. ~height:1. ~depth:1. ()
  in
  let box = Some (Rays_math.Vec3.create (-1.) 0. 0., Rays_math.Vec3.create 1. 2. 3.) in
  let bounds () = box in
  let mesh =
    Mesh.View.create_device ~indices:[| 0; 1; 2 |] ~host ~bounds ~key:"k" ~normals:true ~keep:ignore
      3
    |> Result.get_ok
  in
  let unread = Mesh.View.unread mesh in
  if !made <> 0 || Array.length unread.vertices.x <> 0 || unread.indices <> [| 0; 1; 2 |] then
    failwith "a device mesh's own arrays made its host copy";
  if Mesh.bounds mesh <> box || !made <> 0 then failwith "a device mesh's bounds made its host copy";
  if Array.length (Mesh.View.view mesh).vertices <> 24 || !made <> 1 then
    failwith "a device mesh's view is not its host copy";
  ignore (Mesh.View.packed_view mesh);
  ignore (Mesh.centroid mesh);
  if !made <> 1 then failwith "a device mesh made its host copy twice";
  print_endline "device meshes: host copy made once, for a reader of vertices"

let () = device_host ()

let resident_streams () =
  Scene.Layer.with_cache_context (Scene.Layer.create_cache_context ()) (fun () ->
      let made = ref 0 in
      let host () =
        incr made;
        Test_meshes.box ~width:1. ~height:1. ~depth:1. ()
      in
      let create ?indices ?colors ?tex_coords ?(mode = Mesh.Triangles) ?(device_index_count = 36)
          ?(device_color_width = 3) () =
        Mesh.View.create_device ~mode ?indices ?colors ?tex_coords ~host ~device_index_count
          ~device_color_width ~key:"resident-streams" ~normals:true ~keep:ignore 24
      in
      let mesh = Result.get_ok (create ()) in
      assert (Mesh.index_count mesh = 36 && Mesh.View.triangle_count mesh = 12);
      assert (Mesh.View.device_index_count mesh = Some 36 && Mesh.View.device_color_width mesh = 3);
      assert (Array.length (Mesh.View.unread mesh).indices = 0 && !made = 0);
      List.iter
        (fun result -> assert (Result.is_error result))
        [
          create ~device_index_count:35 ();
          create ~device_index_count:0 ();
          create ~device_color_width:2 ();
          create ~mode:Mesh.Points ();
          create ~indices:[| 0; 1; 2 |] ();
          create ~colors:(Array.make 24 Rays_math.Color.white) ();
          create ~tex_coords:(Array.make 24 Rays_math.Vec2.zero) ();
        ];
      assert (not (Mesh.same mesh (Result.get_ok (create ~device_color_width:4 ()))));
      let camera =
        Camera.orthographic ~height:2. ~at:(Rays_math.Vec3.create 0. 0. 2.)
          ~target:Rays_math.Vec3.zero ()
      in
      let stage mode =
        Result.get_ok
          (Scene.Layer.stage_native ~width:16 ~height:16
             [ Scene.view3d ~camera (Scene.D3.create [ Scene.D3.mesh ~mode mesh ]) ])
      in
      let face_frame = [ Scene.view3d ~camera (Scene.D3.create [ Scene.D3.mesh mesh ]) ] in
      let stage_faces () =
        Result.get_ok (Scene.Layer.stage_native ~width:16 ~height:16 face_frame)
      in
      let faces = stage_faces () in
      assert (stage_faces () == faces);
      let entry = (List.hd faces.scene3).entries.(0) in
      assert (Bytes.length entry.draw.mesh.vertices = 0 && Bytes.length entry.draw.mesh.indices = 0);
      assert (entry.draw.mesh.index_count = 36);
      assert (
        match entry.vertex_attributes with
        | Some (_, bytes) -> Bytes.length bytes = 0
        | None -> false);
      let uniforms = Option.get entry.draw.state.transform_uniforms in
      assert (Int32.float_of_bits (Bytes.get_int32_le uniforms (81 * 4)) = 3.);
      assert (!made = 0);
      ignore (stage Scene.D3.Wireframe);
      assert (!made = 1);
      ignore (stage Scene.D3.Vertices);
      ignore (Mesh.View.packed_view mesh);
      assert (!made = 1);
      let indices = (Mesh.View.packed_view mesh).indices in
      let legacy =
        Result.get_ok
          (Mesh.View.create_device ~indices ~host ~key:"legacy-texture" ~normals:true ~keep:ignore
             24)
      in
      let texture = Texture.init ~width:1 ~height:1 (fun ~x:_ ~y:_ -> Rays_math.Color.white) in
      List.iter
        (fun source ->
          let frame =
            [
              Scene.view3d ~camera
                (Scene.D3.create [ Scene.D3.mesh ~texture:(Scene.D3.textured texture) source ]);
            ]
          in
          let staged = Result.get_ok (Scene.Layer.stage_native ~width:16 ~height:16 frame) in
          let entry = (List.hd staged.scene3).entries.(0) in
          assert (match entry.draw.mesh.key with Host _ -> true | Gpu_vertices _ -> false);
          assert (Bytes.length entry.draw.mesh.vertices > 0))
        [ mesh; legacy ];
      assert (!made = 2);
      print_endline
        "resident mesh streams: nonforcing counts/face staging, checked metadata and single CPU \
         fallback")

let () = resident_streams ()

(* A live cook's display is a new device mesh each frame: the staging caches keep the newest and let
   the older frames go (each holds its scene, its geometry and the device outputs under it), and the
   newest staged again is a hit. *)
let device_frames () =
  Scene.Layer.with_cache_context (Scene.Layer.create_cache_context ()) (fun () ->
      let camera =
        Camera.orthographic ~height:2. ~at:(Rays_math.Vec3.create 0. 0. 2.)
          ~target:Rays_math.Vec3.zero ()
      in
      let meshes = Weak.create 12 in
      let frame i =
        let mesh =
          Mesh.View.create_device ~indices:[| 0; 1; 2 |]
            ~host:(fun () -> Test_meshes.box ~width:1. ~height:1. ~depth:1. ())
            ~bounds:(fun () -> None)
            ~key:("frame " ^ string_of_int i)
            ~normals:true ~keep:ignore 3
          |> Result.get_ok
        in
        Weak.set meshes i (Some mesh);
        [ Scene.view3d ~camera (Scene.D3.create [ Scene.D3.mesh mesh ]) ]
      in
      let stage frame = Result.get_ok (Scene.Layer.stage_native ~width:16 ~height:16 frame) in
      let newest = ref [] in
      for i = 0 to 11 do
        newest := frame i;
        ignore (stage !newest)
      done;
      if stage !newest != stage !newest then failwith "the newest device frame was not cached";
      Gc.full_major ();
      let alive = ref 0 in
      for i = 0 to 11 do
        if Weak.check meshes i then incr alive
      done;
      if !alive > 1 then failwith (Printf.sprintf "staging kept %d of 12 device frames alive" !alive);
      ignore (Sys.opaque_identity !newest));
  print_endline "native Scene3 lowering: older device frames are let go, the newest is a hit"

let () = device_frames ()
