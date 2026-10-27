(* The kernel register programs of the shared fixtures and four workspaces. *)
module E = Flow.Eval
let catalog = fst (Test_program.ok (Flow.Check.catalog_of_manifest
  (In_channel.with_open_bin "../sop_catalog/flow_manifest.sexp" In_channel.input_all)))

let run checked =
  let _, evaluation = Test_program.ok (Flow_ir.qualify_workspace ~observe:(fun _ residual ->
    let term = (E.Private.residual_view residual).term in
    List.iter (fun fusion -> ignore (Flow_ir.Packed.compile_result ~fusion residual term)) [true; false];
    ignore (Flow_ir.Packed.compile_template residual term)) checked) in
  (* Frame folds compile their kernels as templates while they run. *)
  List.iter (fun value ->
    let state = E.create_state () in
    let execute r live =
      Option.map (fun p -> Flow_ir.Packed.force ~state p ~live)
        (Flow_ir.Packed.compile_template r (E.Private.residual_view r).term) in
    for frame = 0 to 1 do
      let live = {(Frame_input.at_time (float frame /. 60.)) with frame; dt = 1. /. 60.; size = 800, 600} in
      ignore (E.Private.force_with_executor ~state ~execute value ~live)
    done) evaluation.states

let workspace path =
  Region_snapshot.start ();
  let source = In_channel.with_open_text ("../../" ^ path ^ ".rays") In_channel.input_all in
  (* The file's layout form is editor chrome, not part of the checked workspace. *)
  (match Flow.Workspace.check ~ops:Flow_ir.Operators.all catalog [List.hd (Test_program.ok (Flow.Syntax.parse source))] with
   | Some workspace, ds when not (List.exists (fun (d : Flow.Diagnostic.t) -> d.severity = Error) ds) -> run workspace
   | _, ds ->
       (* A workspace holding graphs of kinds outside flow_ir (sop) cannot be checked here. *)
       Printf.bprintf Region_snapshot.buffer "skipped: %s\n" (Flow.Diagnostic.to_string (List.hd ds)));
  Region_snapshot.finish ("workspace_" ^ Filename.basename path)

let () =
  Region_snapshot.start ();
  ignore (Test_program.fixtures 1024);
  ignore (Test_program.vector_fixtures 1024);
  Region_snapshot.finish "test_program";
  List.iter workspace ["sketches/particles"; "sketches/flow_shader"; "sketches/flow_particles_gpu";
    "sketches/flow_image_kernel"]
