let tests =
  [
    ("test_rays", Test_rays.run);
    ("test_sop_ui", Test_sop_ui.run);
    ("test_custom_sop", Test_custom_sop.run);
    ("test_rays_editor", Test_rays_editor.run);
    ("test_rays_editor_logic", Test_rays_editor.run_logic);
    ("test_editor_document", Test_editor_document.run);
    ("test_editor_input", Test_editor_input.run);
    ("test_editor_commands", Test_editor_commands.run);
    ("test_editor_canvas_native", Test_editor_commands.canvas_frame_size);
    ("test_world_command", Test_editor_commands.world_command);
    ("test_editor_cook", Test_editor_cook.run);
    ("test_editor_transactions", Test_editor_transactions.run);
    ( "test_scene_tree",
      fun () ->
        Test_scene_tree.run ();
        Test_scene_tree.run_host () );
    ( "test_scene_sync",
      fun () ->
        Test_scene_sync.run ();
        Test_scene_sync.run_loops ();
        Test_scene_sync.run_nested_loops ();
        Test_scene_sync.run_compose ();
        Test_scene_sync.run_instances () );
    ("test_scene_sync_root", Test_scene_sync.run_root);
    ("test_sop_catalog", fun () -> Test_sop_catalog.run ());
    ("test_sop_catalog_inputs", Test_sop_catalog.run_inputs);
    ( "test_sop_catalog_exhaustive",
      fun () ->
        Test_sop_catalog.run ~exhaustive:true ();
        Test_sop_catalog.run_inputs () );
    ("test_packed_pieces", Test_packed_pieces.run);
    ("test_terminal_boolean_normals", Test_terminal_boolean_normals.run);
    ("test_workspace_live", Test_workspace_live.run);
    ("test_text_pane", Test_text_pane.run);
    ("test_editor_consistency", Test_editor_consistency.run);
    ("test_editor_consistency_native", Test_editor_consistency.run_native);
    ("test_probe", Test_probe.run);
    ("test_viewport_pick", Test_viewport_pick.run);
    ("test_workspace_doc_a", Test_workspace_doc.run ~part:`A);
    ("test_workspace_doc_b", Test_workspace_doc.run ~part:`B);
    ("test_workspace_doc_c", Test_workspace_doc.run ~part:`C);
    ( "test_workspace_shell",
      fun () ->
        Test_workspace_shell.run ();
        Test_workspace_shell.run_roots () );
    ("test_workspace_shell_a", Test_workspace_shell.run ~part:`A);
    ("test_workspace_shell_b", Test_workspace_shell.run ~part:`B);
    ( "test_workspace_shell_rest",
      fun () ->
        Test_workspace_shell.run ~part:`Rest ();
        Test_workspace_shell.run_roots () );
    ("test_workspace_shell_native", Test_workspace_shell.run_native);
    ("test_workspace_view_native", Test_workspace_shell.run_view_native);
    ( "test_workspace_roots_native",
      fun () ->
        Test_workspace_shell.run_roots_native ();
        Test_workspace_shell.run_budget_native () );
    ("test_workspace_source", Test_workspace_source.run);
    ( "test_workspace_vocabulary",
      fun () ->
        Test_workspace_source.run_library_navigation ();
        Test_workspace_source.run_vocabulary_refresh () );
    ("test_bloom_studio", Test_bloom_studio.run);
    ("sop_render_parity", Sop_render_parity.run);
  ]

let () =
  (* the editor's pointer positions are the 11-point kit's, not its 13-point default *)
  match Array.to_list Sys.argv with
  | _ :: name :: _ -> (
      match List.assoc_opt name tests with
      | Some run -> run ()
      | None -> failwith ("unknown test: " ^ name))
  | _ -> failwith "usage: test_main <name>"
