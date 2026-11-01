(* S6 spike: the frozen sculpture's actual numeric record, with the host
   volume captured and its compact/count/range chain still unmaterialised.
   No evaluator deferral policy or GPU backend is installed by this check. *)
module E = Flow.Eval
module W = Flow.Workspace

let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)

let () =
  let file = ref "sketches/isogrid.rays" and resolution = ref None in
  Arg.parse [ ("--resolution", Arg.Int (fun n -> resolution := Some n), "in-memory volume resolution") ]
    (fun path -> file := path) "test_isogrid_defer [FILE] [--resolution N]";
  let text = In_channel.with_open_bin !file In_channel.input_all in
  assert (
    Editor_document.Lowering.sha256 text
    = "75686b00542de41aba26b5ff4cc63f88805924f4a625e8ad44c033d66664eeda");
  let workspace =
    match Rays_editor.Workspace.load text with
    | Ok workspace -> workspace
    | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let workspace = match !resolution with
    | None -> workspace
    | Some resolution ->
        ok (Editor_document.Workspace_doc.edit (ok (Rays_editor.workspace_catalog ())) workspace
          (Flow_graph.Flow_edit.Set_input_default { form = "volume"; input = "resolution";
            value = List.hd (ok (Flow.Syntax.parse (string_of_int resolution))) }))
  in
  let captured = ref None in
  let bulk residual =
    let view = E.Residual.residual_view residual in
    if Option.is_none !captured && match view.site with "sculpture" :: _ -> true | _ -> false then
      captured := Some residual;
    match Flow_ir.Packed.compile residual view.term with
    | None -> None
    | Some packed -> Result.to_option (Flow_ir.Packed.force packed ~live:(Input.at_time 0.))
  in
  ignore
    (ok
       (E.Residual.handle ~trusted:true ~bulk (fun () ->
            E.Host.static_with_requests ~inputs:workspace.inputs workspace.checked)));
  let residual = Option.get !captured in
  let graph =
    List.find (fun (graph : W.graph) -> graph.name = "sculpture") workspace.checked.graphs
  in
  let bindings, body =
    match graph.body.node with W.Let (bindings, body) -> (bindings, body) | _ -> assert false
  in
  let binding name =
    List.find_map (function W.Name key, term when key = name -> Some term | _ -> None) bindings
    |> Option.get
  in
  let arguments (term : W.term) =
    match term.node with W.Call { args; _ } | W.Op { args; _ } -> args | _ -> assert false
  in
  let mesh = binding "mesh" in
  let fields =
    arguments mesh
    @ [
        ("N", List.assoc "values" (arguments (binding "shaded")));
        ("Cd", List.assoc "values" (arguments body));
      ]
  in
  let ty = Flow.Ty.Record (List.map (fun (name, (term : W.term)) -> (name, term.ty)) fields) in
  let result = { mesh with W.node = W.Record fields; ty } in
  let bindings =
    List.filter
      (function W.Name ("volume" | "side" | "mesh" | "shaded"), _ -> false | _ -> true)
      bindings
  in
  let term = { graph.body with W.node = W.Let (bindings, result); ty } in
  let residual = E.Residual.with_bindings residual ~term [] in
  let program =
    match Flow_ir.Core_lower.lower ~strict:true residual with
    | Ok program -> program
    | Error name -> failwith ("S6 strict sculpture record refused: " ^ name)
  in
  assert (Array.length program.regions > 0);
  let compact = ref false and count = ref false and range = ref false and kinds = ref false in
  Flow_ir.Core.iter
    (fun (statement : Flow_ir.Core.statement) ->
      match statement.node with
      | Assign (_, Prim (Array_compact, _)) -> compact := true
      | Assign (_, Prim (Array_count, _)) -> count := true
      | Assign (_, Prim (Array_range, _)) -> range := true
      | Assign (_, Prim (Array_int, _)) -> kinds := true
      | _ -> ())
    program.body;
  assert (!compact && !count && !range && !kinds);
  Array.iter
    (fun (region : Flow_ir.Core.region) ->
      let name = Option.value ~default:[] region.term.path |> String.concat "/" in
      let origin =
        match region.origin with None -> "none" | Some (path, _) -> String.concat "/" path
      in
      Printf.printf "S6 region %s origin=%s\n%!" name origin)
    program.regions;
  let origin name =
    Array.find_opt
      (fun (region : Flow_ir.Core.region) ->
        region.term.path = Some [ "sculpture"; name ])
      program.regions
    |> Option.get |> fun region -> region.origin
  in
  assert (origin "positions" <> None);
  assert (origin "positions" = origin "normals" && origin "normals" = origin "colors");
  let other = Result.get_ok (Flow_ir.Core_lower.lower ~strict:true residual) in
  let other_positions =
    Array.find_opt
      (fun (region : Flow_ir.Core.region) -> region.term.path = Some [ "sculpture"; "positions" ])
      other.regions |> Option.get
  in
  assert (other_positions.origin <> origin "positions");
  let expected = ok (E.Host.force_reference (E.Residual residual) ~live:(Input.at_time 0.)) in
  let bytes value = Marshal.to_bytes value [ Marshal.No_sharing ] in
  let expected = bytes expected in
  let prepared = Flow_ir.Core_exec.prepare program in
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          let calls = Hashtbl.create 16 and lock = Mutex.create () in
          let measure packed ~seconds:_ ~reference =
            assert (not reference);
            let site = Flow_ir.Packed.site packed in
            Mutex.protect lock (fun () ->
                Hashtbl.replace calls site (1 + Option.value ~default:0 (Hashtbl.find_opt calls site)))
          in
          let actual =
            ok
              (Flow_ir.Core_exec.execute ~root:residual ~strict:true ~rerun_on_error:false
                 ~force:(fun packed -> Flow_ir.Packed.force ~strict:true
                     ~measure:((fun () -> 0.), measure) packed ~live:(Input.at_time 0.)) prepared
                 ~live:(Input.at_time 0.))
          in
          Hashtbl.iter
            (fun (_, path, _) count ->
              if count <> 1 then failwith (Printf.sprintf "CPU source %s executed %d times"
                  (String.concat "/" path) count)) calls;
          assert (Hashtbl.length calls = Array.length program.regions);
          Printf.printf "S6 CPU materialisation: domains=%d, %d packed sources, each once, no reference walk\n%!"
            domains (Hashtbl.length calls);
          assert (bytes actual = expected)))
    [ 1; 8 ];
  Printf.printf
    "S6 strict sculpture record: %d regions, exact reference/one/eight-domain fields\n%!"
    (Array.length program.regions)
