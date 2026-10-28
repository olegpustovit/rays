(* The kernel register programs of the shared fixtures and four workspaces. *)
module E = Flow.Eval
let catalog = fst (Test_program.ok (Flow.Check.catalog_of_manifest
  (In_channel.with_open_bin "../flow_sop/flow_manifest.sexp" In_channel.input_all)))

let run ?(require_state_kernel = false) checked =
  let _, evaluation = Test_program.ok (Flow_ir.qualify_workspace ~observe:(fun _ residual ->
    let term = (E.Residual.residual_view residual).term in
    List.iter (fun fusion -> ignore (Flow_ir.Packed.compile_result ~fusion residual term)) [true; false];
    ignore (Flow_ir.Packed.compile_template residual term)) checked) in
  (* Frame folds compile their kernels as templates while they run. *)
  List.iter (fun value ->
    (match value with
     | E.Residual residual when require_state_kernel ->
         let core = Result.get_ok (Flow_ir.Core_lower.lower residual) in
         if Array.length core.Flow_ir.Core.regions = 0 then
           failwith "particle state residual did not qualify to a Core kernel"
     | _ -> ());
    let state = E.create_state () and reference_state = E.create_state () in
    let program = Result.get_ok (Flow_ir.Executor.compile value) in
    for frame = 0 to 1 do
      let live = {(Input.at_time (float frame /. 60.)) with frame; dt = 1. /. 60.; size = 800, 600} in
      let actual = Flow_ir.Executor.force ~state program ~live in
      let expected = E.Private.force_reference ~state:reference_state value ~live in
      let same = match actual, expected with
        | Ok actual, Ok expected -> Marshal.to_string actual [Marshal.No_sharing] = Marshal.to_string expected [Marshal.No_sharing]
        | Error actual, Error expected -> Diagnostic.to_string actual = Diagnostic.to_string expected
        | _ -> false in
      let state_same = Marshal.to_string (E.Private.state_values state) [Marshal.No_sharing] =
        Marshal.to_string (E.Private.state_values reference_state) [Marshal.No_sharing] in
      if not same || not state_same then failwith "state residual differs from the reference walker"
    done) evaluation.states

let workspace path =
  Region_snapshot.start ();
  let source = In_channel.with_open_text ("../../" ^ path ^ ".rays") In_channel.input_all in
  (* The file's layout form is editor chrome, not part of the checked workspace. *)
  (match Flow.Workspace.check ~ops:[] catalog [List.hd (Test_program.ok (Flow.Syntax.parse source))] with
   | Some workspace, ds when not (List.exists (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity) ds) ->
       run ~require_state_kernel:(path = "sketches/particles") workspace
   | _, ds ->
       (* A workspace holding graphs of kinds outside flow_ir (sop) cannot be checked here. *)
       Printf.bprintf Region_snapshot.buffer "skipped: %s\n" (Diagnostic.to_string (List.hd ds)));
  Region_snapshot.finish ("workspace_" ^ Filename.basename path)

let state_branch_laziness () =
  let source = {|(workspace lazy_state
    (graph g :context value
      (let* [held (state [s (array/range 3)]
        (if (> t 100.)
          (map (fn [x] (error "E_UNTAKEN" "inactive state map")) s)
          (map (fn [x] (+ x 1)) s)))] (+ (array/sum held) 0.))))|} in
  let forms = Test_program.ok (Flow.Syntax.parse source) in
  let checked = match Flow.Workspace.check ~ops:[] {Flow.Check.version=1;kinds=[]} forms with
    | Some workspace, [] -> workspace
    | _, diagnostics -> failwith (String.concat "; " (List.map Diagnostic.to_string diagnostics)) in
  let evaluation = Test_program.ok (E.static checked) in
  let value = List.hd evaluation.states in
  let program = Test_program.ok (Flow_ir.Executor.compile value) in
  let reference_state = E.create_state () and core_state = E.create_state () in
  List.iter (fun time ->
    let live = Input.at_time time in
    let expected = E.Private.force_reference ~state:reference_state value ~live in
    let actual = Flow_ir.Executor.force ~state:core_state program ~live in
    let same = match actual, expected with
      | Ok actual, Ok expected -> actual = expected
      | Error actual, Error expected -> Diagnostic.to_string actual = Diagnostic.to_string expected
      | _ -> false in
    if not same then failwith (Printf.sprintf "state branch laziness differs at t=%g" time);
    if Marshal.to_string (E.Private.state_values core_state) [Marshal.No_sharing]
       <> Marshal.to_string (E.Private.state_values reference_state) [Marshal.No_sharing] then
      failwith (Printf.sprintf "state branch changed inactive state at t=%g" time)) [0.; 101.]

let () =
  Region_snapshot.start ();
  state_branch_laziness ();
  ignore (Test_program.fixtures 1024);
  ignore (Test_program.vector_fixtures 1024);
  Region_snapshot.finish "test_program";
  List.iter workspace ["sketches/particles"; "sketches/flow_shader"; "sketches/flow_particles_gpu";
    "sketches/flow_image_kernel"]
