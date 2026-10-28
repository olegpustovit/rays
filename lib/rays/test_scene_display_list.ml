open Rays

let run () =
  Scene.Layer.with_cache_context (Scene.Layer.create_cache_context ()) (fun () ->
      let builder = Scene_command.Display_list.Builder.create () in
      (* The rectangle (2, 3) 8 x 9 in opaque red, as two triangles. *)
      let rectangle =
        {
          Scene_command.Render_ir.vertices = [| 2.; 3.; 10.; 3.; 10.; 12.; 2.; 12. |];
          indices = [| 0; 1; 2; 0; 2; 3 |];
          color = 0xff0000ffl;
        }
      in
      Scene_command.Display_list.Builder.geometry builder rectangle;
      let segment =
        Result.get_ok (Scene_command.Display_list.Builder.publish builder ~id:41L ~version:3L)
      in
      let node = Scene.Layer.display_list segment in
      let staged_ir, staged_resources =
        Result.get_ok (Scene.Layer.stage ~width:32 ~height:32 [ node ])
      in
      if staged_ir != Scene_command.Display_list.render_ir segment || staged_resources <> [] then
        failwith "root display list was copied during scene staging";
      let direct = Result.get_ok (Scene.Layer.stage_native ~width:32 ~height:32 [ node ]) in
      (match (direct.layers, direct.retained) with
      | [ Scene.Layer.Scene2_segment (retained, []) ], Some ("scene2-segment:41", 3L)
        when retained == segment ->
          ()
      | _ -> failwith "root display list did not retain direct native identity");
      let transformed =
        Result.get_ok
          (Scene.Layer.stage_native ~width:32 ~height:32 [ Scene.translate 5 7 [ node ] ])
      in
      (match (transformed.layers, transformed.retained) with
      | [ Scene.Layer.Scene2_layer (ir, []) ], Some (identity, 1L)
        when String.starts_with ~prefix:"scene-stage:" identity -> (
          match Array.to_list (Scene_command.Render_ir.commands_readonly ir) with
          | [
           Scene_command.Render_ir.Push_transform _;
           Scene_command.Render_ir.Geometry geometry;
           Scene_command.Render_ir.Pop_transform;
          ]
            when geometry = rectangle ->
              ()
          | _ -> failwith "transformed display-list command order drift")
      | _ -> failwith "transformed display list bypassed retained transform lowering");
      let equivalent () =
        [
          Scene.clear Rays_math.Color.black;
          Scene.rect ~at:(3, 4) ~w:12 ~h:9 ~fill:Rays_math.Color.white ();
        ]
      in
      let first_equivalent =
        Result.get_ok (Scene.Layer.stage_native_render ~width:32 ~height:32 (equivalent ()))
      and second_equivalent =
        Result.get_ok (Scene.Layer.stage_native_render ~width:32 ~height:32 (equivalent ()))
      in
      if first_equivalent != second_equivalent then
        failwith "equivalent shallow scene composition missed native stage reuse";
      let changed =
        Result.get_ok
          (Scene.Layer.stage_native_render ~width:32 ~height:32
             [
               Scene.clear Rays_math.Color.black;
               Scene.rect ~at:(3, 4) ~w:13 ~h:9 ~fill:Rays_math.Color.white ();
             ])
      in
      if changed == first_equivalent then
        failwith "changed shallow scene composition reused stale native stage";
      let layered width =
        [
          Scene.rect ~at:(1, 1) ~w:8 ~h:8 ~fill:Rays_math.Color.white ();
          Scene.Layer.layer_break;
          Scene.rect ~at:(16, 1) ~w:width ~h:8 ~fill:Rays_math.Color.red ();
        ]
      in
      let first_layered =
        Result.get_ok (Scene.Layer.stage_native_render ~width:32 ~height:32 (layered 8))
      and changed_layered =
        Result.get_ok (Scene.Layer.stage_native_render ~width:32 ~height:32 (layered 9))
      in
      (match (first_layered.layers, changed_layered.layers) with
      | ( [ Scene.Layer.Scene2_layer (stable, []); Scene.Layer.Scene2_layer (first_dynamic, []) ],
          [ Scene.Layer.Scene2_layer (reused, []); Scene.Layer.Scene2_layer (changed_dynamic, []) ]
        )
        when stable == reused && first_dynamic != changed_dynamic ->
          ()
      | _ -> failwith "changed layer invalidated unrelated retained Scene2 IR");

      let image_builder = Scene_command.Display_list.Builder.create () in
      Scene_command.Display_list.Builder.image image_builder ~resource_id:7
        ~source:{ x = 0.; y = 0.; width = 1.; height = 1. }
        ~destination:{ x = 4.; y = 5.; width = 6.; height = 7. };
      let image_segment =
        Result.get_ok (Scene_command.Display_list.Builder.publish image_builder ~id:42L ~version:1L)
      in
      (match Scene.Layer.display_list image_segment with
      | _ -> failwith "display list accepted an unbound image resource"
      | exception Invalid_argument _ -> ());
      let image =
        Image.create ~width:1 ~height:1 ~color:Rays_math.Color.white () |> Result.get_ok
      in
      Fun.protect
        ~finally:(fun () -> ignore (Image.destroy image))
        (fun () ->
          (match Scene.Layer.display_list ~images:[ (7, image); (7, image) ] image_segment with
          | _ -> failwith "display list accepted a duplicate image resource"
          | exception Invalid_argument _ -> ());
          let image_node = Scene.Layer.display_list ~images:[ (7, image) ] image_segment in
          let first =
            Result.get_ok (Scene.Layer.stage_native ~width:32 ~height:32 [ image_node ])
          in
          (match first.layers with
          | [ Scene.Layer.Scene2_segment (retained, [ (7, Renderer.Image resource) ]) ]
            when retained == image_segment && resource == Image.resource image ->
              ()
          | _ -> failwith "display-list image binding lost managed identity");
          let peer =
            Image.create ~width:1 ~height:1 ~color:Rays_math.Color.white () |> Result.get_ok
          in
          Fun.protect
            ~finally:(fun () -> ignore (Image.destroy peer))
            (fun () ->
              let rebound =
                Result.get_ok
                  (Scene.Layer.stage_native ~width:32 ~height:32
                     [ Scene.Layer.display_list ~images:[ (7, peer) ] image_segment ])
              in
              if first.retained = rebound.retained then
                failwith "equal-generation image identities collided in retained replay");
          let replacement =
            Image.create ~width:1 ~height:1 ~color:Rays_math.Color.black () |> Result.get_ok
          in
          Result.get_ok (Image.replace image replacement);
          let second =
            Result.get_ok (Scene.Layer.stage_native ~width:32 ~height:32 [ image_node ])
          in
          if first.retained = second.retained then
            failwith "display-list resource generation did not invalidate replay");
      print_endline "Scene display list: direct retained layer and transformed parity")
