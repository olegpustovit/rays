let check condition message = if not condition then failwith message

let cook node =
  let context = Result.get_ok (Sop.Context.create ()) in
  let session = Result.get_ok (Sop.Session.create ~max_entries:8
    ~max_payload_bytes:1_000_000) in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
    match Sop.Session.cook session ~context node with
    | Ok output -> Result.get_ok (Sop.Payload.geometry output.payload)
    | Error error -> failwith (Diagnostic.error_to_string error))

let () =
  let workspace = {|(workspace shared
    (defn size :context value [] 3)
    (graph g :context sop (sop/grid :columns (size) :rows 2)))|} in
  check (Rdk.Geometry.point_count (cook (Lisp_sop.workspace_node workspace)) = 12)
    "workspace definitions were not available while lowering";
  let source = Lisp_sop.node "(sop/grid :columns 3 :rows 2)" in
  let workspace = {|(workspace shared
    (graph g :context sop (sop/ext_src)))|} in
  check (Lisp_sop.workspace_node ~with_:["src", source] workspace == source)
    "workspace external marker was not restored";
  check (Rdk.Geometry.point_count (cook (Lisp_sop.node
    "(sop/grid :columns 3 :rows 2)")) = 12) "expression helper changed";
  List.iter (fun text ->
    check (try ignore (Lisp_sop.workspace_node text); false with Failure _ -> true)
      "invalid workspace did not fail with a lowering diagnostic")
    ["("; "(workspace empty (graph g :context sop nil))";
     "(workspace value_only (graph g :context value 1))";
     "(workspace bad (graph g :context sop (sop/not_a_node)))"]
