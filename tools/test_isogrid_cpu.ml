(* The complete frozen workspace, including the mesh cook, with no GPU backend.
   This executable selects ogpu_mock and creates neither a renderer nor a device. *)
module E = Flow.Eval

let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)

let same_vectors expected actual =
  let actual = Rdk.Packed.Float3.View.view actual in
  assert (Array.length expected = 3 * Array.length actual.x);
  Array.iteri
    (fun component values ->
      Array.iteri
        (fun index value ->
          assert (Int64.bits_of_float value = Int64.bits_of_float expected.((index * 3) + component)))
        values)
    [| actual.x; actual.y; actual.z |]

let () =
  let text = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  assert (
    Editor_document.Lowering.sha256 text
    = "0fdb9b0ebec8f5fe84227d7c0963dd3ec8c446c6145e145555b4fa4f80744e69");
  let workspace =
    match Rays_editor.Workspace.load text with
    | Ok workspace -> workspace
    | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let eager = ref None in
  List.iter
    (fun (empty, defer) ->
      let workspace =
        if not empty then workspace
        else
          ok
            (Editor_document.Workspace_doc.edit
               (ok (Rays_editor.workspace_catalog ()))
               workspace
               (Flow_graph.Flow_edit.Set_arg
                  {
                    node = [ "volume"; "voxels" ];
                    key = Whole;
                    sub = [];
                    value = List.hd (ok (Flow.Syntax.parse "(array/int 13824 0)"));
                  }))
      in
      let gpu = Flow_ir.Gpu.{ default with backend = None } in
      let lowered =
        ok (Flow_sop.Lower.of_checked ~gpu ~defer ~inputs:workspace.inputs workspace.checked)
      in
      assert (lowered.gpu.backend = None);
      assert (lowered.defer = defer);
      if not empty && not defer then eager := Some lowered.evaluated;
      let graph =
        List.find
          (fun (graph : Flow_sop.Lower.graph) -> graph.default && graph.name = "sculpture")
          lowered.graphs
      in
      let node =
        Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)
        |> Result.get_ok
      in
      let session =
        Sop.Session.create ~max_entries:16 ~max_payload_bytes:(1 lsl 28) |> Result.get_ok
      in
      Fun.protect
        ~finally:(fun () -> Sop.Session.close session)
        (fun () ->
          let output =
            ok
              (Sop.Session.cook session
                 ~context:(Sop.Context.create ~domains:1 () |> Result.get_ok)
                 node)
          in
          let geometry = ok (Sop.Payload.geometry output.payload) in
          let nodes = Array.to_list lowered.plan.nodes in
          let mesh = List.find (fun (node : E.node) -> node.kind = "sop/mesh") nodes in
          let attribute name =
            List.find
              (fun (node : E.node) ->
                node.kind = "sop/with_attr"
                && List.assoc_opt "attribute" node.args = Some (E.Text name))
              nodes
            |> fun node -> List.assoc "values" node.args
          in
          (match List.assoc "points" mesh.args with
          | E.Residual _ -> assert defer
          | _ -> assert (not defer));
          (* One reference force shares the compacted mask between all fields. *)
          let expected =
            match
              ok
                (E.force
                   (E.Record (mesh.args @ [ ("N", attribute "N"); ("Cd", attribute "Cd") ]))
                   ~live:(Input.at_time 0.))
            with
            | E.Record fields -> fields
            | _ -> assert false
          in
          (match List.assoc "points" expected with
          | E.Vec3_array values -> same_vectors values (Rdk.Geometry.positions geometry)
          | _ -> assert false);
          let topology = Rdk.Topology.View.view (Rdk.Geometry.topology geometry) in
          List.iter
            (fun (name, actual) ->
              match List.assoc name expected with
              | E.Int_array expected -> assert (expected = actual)
              | _ -> assert false)
            [
              ("vertex_points", topology.vertex_points);
              ("primitive_offsets", topology.primitive_offsets);
              ( "kinds",
                Array.init (Bytes.length topology.primitive_kinds) (fun i ->
                    Char.code (Bytes.get topology.primitive_kinds i)) );
            ];
          List.iter
            (fun name ->
              match
                (List.assoc name expected, Rdk.Geometry.find_attribute ~owner:Point name geometry)
              with
              | E.Vec3_array expected, Some actual -> (
                  match Rdk.Attribute.View.storage actual with
                  | Float3 actual -> same_vectors expected actual
                  | _ -> assert false)
              | _ -> assert false)
            [ "N"; "Cd" ];
          if empty then begin
            assert (
              Rdk.Geometry.point_count geometry = 0 && Rdk.Geometry.primitive_count geometry = 0);
            assert (topology.primitive_offsets = [| 0 |])
          end
          else
            assert (
              Rdk.Geometry.point_count geometry > 0 && Rdk.Geometry.primitive_count geometry > 0);
          if defer && not empty then begin
            let force ?state ?elems ?resolve value ~live =
              Result.bind (Flow_ir.Executor.compile value) (fun program ->
                  Flow_ir.Executor.force ?state ?elems ?resolve program ~live)
            in
            let baseline = Option.get !eager in
            let before = Flow_graph.Probe.make ~force baseline
            and after = Flow_graph.Probe.make ~force lowered.evaluated in
            let compare path probes =
              let a = Flow_graph.Probe.at before path ~probes
              and b = Flow_graph.Probe.at after path ~probes in
              if a <> b || a = None then
                failwith ("Deferred IsoGrid probe differs at " ^ String.concat "/" path)
            in
            List.iter (fun name -> compare [ "sculpture"; name ] []) [ "faces"; "positions" ];
            List.iter
              (fun (name, count) ->
                let path =
                  let rec find (term : Flow.Workspace.term) =
                    match term.path with
                    | Some path when List.hd (List.rev path) = name -> Some path
                    | _ -> List.find_map find (Flow.Workspace.children term)
                  in
                  let graph =
                    List.find (fun (g : Flow.Workspace.graph) -> g.name = "sculpture")
                      workspace.checked.graphs
                  in
                  Option.get (find graph.body)
                in
                List.iter (fun k -> compare path [ k ]) [ 0; 4095; 4096; count - 1 ])
              [ ("index", 82944); ("corner", Rdk.Geometry.point_count geometry) ];
            print_endline "Deferred IsoGrid faces/positions and selected body probes match eager values"
          end;
          Printf.printf
            "CPU-only IsoGrid empty=%b defer=%b: %d points, %d primitives; exact P/topology/N/Cd\n\
             %!"
            empty defer
            (Rdk.Geometry.point_count geometry)
            (Rdk.Geometry.primitive_count geometry)))
    [ (false, false); (true, false); (false, true); (true, true) ]
