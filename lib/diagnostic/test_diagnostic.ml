let () =
  let diagnostic = Diagnostic.error ~span:{Diagnostic.start=2;finish=5}
      ~position:{line=2;col=3} ~cause:"backend" ~hints:["retry"]
      ~trace:[{node_id=7;label="image";operation="image/map"}]
      ~code:"E_TEST" "failed" in
  assert (Diagnostic.to_string diagnostic = "E_TEST at 2:3: failed");
  assert (Diagnostic.error_to_string diagnostic =
    "image: [E_TEST] failed\n  cause: backend\n  hint: retry");
  assert ((Diagnostic.prepend_trace {node_id=8;label="parent";operation="scene/root"} diagnostic).trace =
    [{node_id=8;label="parent";operation="scene/root"};
     {node_id=7;label="image";operation="image/map"}]);
  assert (Diagnostic.report ~file:"x.rays" ~source:"a\nbcde" diagnostic =
    "File \"x.rays\", line 2, characters 0-3:\nError [E_TEST]: failed");
  assert (match Diagnostic.warning ~code:"W_TEST" "warning" with
    | {severity=Diagnostic.Warning; cause=None; hints=[]; trace=[]; _} -> true
    | _ -> false);
  let raised = try raise (Diagnostic.Error diagnostic) with Diagnostic.Error value -> value in
  assert (raised = diagnostic)
