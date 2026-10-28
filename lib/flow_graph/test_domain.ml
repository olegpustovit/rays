open Flow
open Flow_graph

let catalog = { Catalog.version = 1; kinds = [] }

open Test_support

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace lookup (graph g :context value (let* [table (array/int 3 9007199254740993) \
          picked (array/nth table 1)] picked)))")
  in
  let workspace = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let card =
    Option.get (Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "picked" ])
  in
  assert (card.head = "array/nth" && card.ty = Ty.Int);
  let _, edited =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = card.path; key = Pos 1; sub = []; value = Syntax.make (Num "2") }))
  in
  assert (List.assoc "g" (ok (Eval.run ~time:0. edited)).results = Eval.Int 9007199254740993)

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace lookup (graph g :context host (let* [table (array/range 3 :type int) picked \
          (map (fn [i] (array/nth table i)) (array/range 3 :type int))] picked)))")
  in
  let workspace = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let rec lookup (scope : Projection.scope) =
    List.find_map
      (fun (node : Projection.node) ->
        if node.head = "array/nth" then Some node
        else Option.bind node.zone (fun zone -> lookup zone.scope))
      scope.nodes
  in
  let card = Option.get (lookup (Projection.of_graph catalog workspace "g")) in
  let _, edited =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = card.path; key = Pos 1; sub = []; value = Syntax.make (Num "2") }))
  in
  assert (List.assoc "g" (ok (Eval.run ~time:0. edited)).results = Eval.Int_array [| 2; 2; 2 |])

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace lookup (graph g :context host (let* [table (array/text 3 \"é日本😀\") picked \
          (map (fn [(i : int)] (array/nth table i)) (array/range 3 :type int))] picked)))")
  in
  let workspace = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let rec lookup (scope : Projection.scope) =
    List.find_map
      (fun (node : Projection.node) ->
        if node.head = "array/nth" then Some node
        else Option.bind node.zone (fun zone -> lookup zone.scope))
      scope.nodes
  in
  let card = Option.get (lookup (Projection.of_graph catalog workspace "g")) in
  assert (card.ty = Ty.Text);
  let _, edited =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = card.path; key = Pos 1; sub = []; value = Syntax.make (Num "2") }))
  in
  assert (
    List.assoc "g" (ok (Eval.run ~time:0. edited)).results
    = Eval.Text_array [| "é日本😀"; "é日本😀"; "é日本😀" |])

let () =
  List.iter
    (fun (name, args, index) ->
      let source =
        ok
          (Syntax.parse
             ("(workspace checked (graph g :context value (let* [status (" ^ name ^ " " ^ args
            ^ ")] status.value)))"))
      in
      let workspace =
        match Workspace.check catalog source with
        | Some w, [] -> w
        | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
      in
      let pane = Projection.of_graph catalog workspace "g" in
      let card = Option.get (Projection.find pane [ "g"; "status" ]) in
      assert (card.ty = Ty.Record [ ("value", Ty.Float); ("valid", Ty.Bool) ]);
      let _, edited =
        ok
          (Flow_edit.apply_checked catalog source
             (Set_arg
                { node = card.path; key = Pos index; sub = []; value = Syntax.make (Num "4.0") }))
      in
      let result = List.assoc "g" (ok (Eval.run ~time:2. edited)).results in
      assert (result = Eval.Float 8.))
    [ ("float/mul_checked", "t 2.0", 1); ("float/fma_checked", "t 2.0 1.0", 2) ]

let () =
  let source =
    ok (Syntax.parse "(workspace integers (graph g :context host (let* [v (array/int 1)] v)))")
  in
  let workspace = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let card =
    Option.get (Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "v" ])
  in
  let probe = Probe.make (ok (Eval.static ~record:true workspace)) in
  assert ((Probe.footer probe card ~probes:[]).value = "1 × int")

let () =
  let source =
    ok
      (Syntax.parse "(workspace texts (graph g :context host (let* [v (array/text 5001 \"猫\")] v)))")
  in
  let workspace = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let card =
    Option.get (Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "v" ])
  in
  assert (card.ty = Ty.Array Ty.Text);
  let default = Option.get (Flow_edit.default_for (Ty.Array Ty.Text) "texts") in
  assert (Syntax.head default = Some "array/text");
  let probe = Probe.make (ok (Eval.static ~record:true workspace)) in
  assert ((Probe.footer probe card ~probes:[]).value = "5001 × text")

let () =
  List.iter
    (fun (literal, ty, width, expected) ->
      let source =
        ok
          (Syntax.parse
             ("(workspace vectors (graph g :context host (let* [v " ^ literal ^ "] v)))"))
      in
      let workspace =
        match Workspace.check catalog source with
        | Some w, [] -> w
        | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
      in
      let card =
        Option.get (Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "v" ])
      in
      assert (card.ty = ty && (List.hd card.rows).ty = Some ty);
      let default = Option.get (Flow_edit.default_for ty "v") in
      (match default.node with
      | Syntax.Vec cs -> assert (List.length cs = width)
      | _ -> assert false);
      let _, edited =
        ok
          (Flow_edit.apply_checked catalog source
             (Set_arg
                {
                  node = card.path;
                  key = Whole;
                  sub = [ width - 1 ];
                  value = Syntax.make (Num "9.5");
                }))
      in
      let probe = Probe.make (ok (Eval.static ~record:true edited)) in
      assert ((Probe.footer probe card ~probes:[]).value = expected))
    [ ("[1 2]", Ty.Vec2, 2, "[1 9.5]"); ("[1 2 3 4]", Ty.Vec4, 4, "[1 2 3 9.5]") ]

let () =
  let text =
    "(workspace live (graph g :context value (let* [x (if (< t 1) 1 2) y (cond (< t 0) 1 (< t 1) 2 \
     :else 3) z (case (< t 1) 0 4 true 5 :else 6)] (+ x (+ y z)))))"
  in
  let source = ok (Syntax.parse text) in
  let workspace = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let scope = Projection.of_graph catalog workspace "g" in
  let evaluated = ok (Eval.static ~record:true workspace) in
  List.iter
    (fun (time, expected) ->
      let probe = Probe.make ~time evaluated in
      List.iter2
        (fun name expected ->
          let node = Option.get (Projection.find scope [ "g"; name ]) in
          let actual = Probe.taken_arm probe node ~probes:[] in
          if actual <> Some expected then
            failwith
              (Printf.sprintf "taken %s at %g: expected %d got %s" name time expected
                 (Option.fold ~none:"none" ~some:string_of_int actual)))
        [ "x"; "y"; "z" ] expected)
    [ (0., [ 0; 1; 1 ]); (2., [ 1; 2; 0 ]) ];
  List.iter
    (fun (expression, result) ->
      let source =
        ok
          (Syntax.parse
             ("(workspace wrap (graph g :context value (let* [x " ^ expression ^ "] " ^ result
            ^ ")))"))
      in
      let before =
        match Workspace.check catalog source with
        | Some w, _ -> ok (Eval.run ~time:0. w)
        | _, ds ->
            failwith (expression ^ ": " ^ String.concat "; " (List.map Diagnostic.to_string ds))
      in
      let _, workspace =
        ok (Flow_edit.apply_checked catalog source (Wrap { nodes = [ [ "g"; "x" ] ]; loop = If }))
      in
      assert ((ok (Eval.run ~time:0. workspace)).results = before.results))
    [
      ("{:weight 2 :name \"point\"}", "x.weight");
      ("(array/float 6 3.0)", "(array/count x)");
      ("[1 2 3]", "x.x");
      ("(list 1 2)", "(count x)");
    ];
  let source =
    ok (Syntax.parse "(workspace bypass (graph g :context value (let* [x (if false 3 9)] x)))")
  in
  let _, workspace =
    ok (Flow_edit.apply_checked catalog source (Toggle_bypass { node = [ "g"; "x" ] }))
  in
  assert (List.assoc "g" (ok (Eval.run ~time:0. workspace)).results = Eval.Int 3)

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace tuples (graph g :context value (sum [i (range 2)] (if (< i 1) 1 2))))")
  in
  let workspace = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let scope = Projection.of_graph catalog workspace "g" in
  let node = Option.get (Projection.find scope [ "g"; "@result"; "@result" ]) in
  let probe = Probe.make (ok (Eval.static ~record:true workspace)) in
  List.iter
    (fun (tuple, arm) -> assert (Probe.taken_arm probe node ~probes:tuple = Some arm))
    [ ([ 0 ], 0); ([ 1 ], 1); ([ 0 ], 0); ([ 1 ], 1) ];
  let empty = Probe.make (ok (Eval.static workspace)) in
  assert (Probe.taken_arm empty node ~probes:[ 0 ] = None);
  assert (Probe.taken_arm empty node ~probes:[ 0 ] = None);
  let path = [ "g"; "pick" ] in
  let evaluated =
    {
      (ok (Eval.static workspace)) with
      records =
        [
          ( path,
            [ ([ 0 ], Eval.Deferred (Ty.geometry, 1)); ([ 1 ], Eval.Deferred (Ty.geometry, 2)) ] );
        ];
    }
  in
  let forced = ref [] in
  let probe =
    Probe.make
      ~geometry:(fun id ->
        forced := id :: !forced;
        None)
      evaluated
  in
  let at tuple id =
    assert (Probe.at probe path ~probes:tuple = Some (Probe.Value (Eval.Deferred (Ty.geometry, id))))
  in
  at [ 0 ] 1;
  at [ 0 ] 1;
  assert (!forced = [ 1 ]);
  let geometry = { node with path; head = "geometry"; ty = Ty.geometry; invariant = false } in
  let footer = Probe.footer probe geometry ~probes:[ 0 ] in
  assert (footer.runs = Some 2 && footer.spark = None && !forced = [ 1 ]);
  assert ((Probe.footer probe geometry ~probes:[ 2 ]).value = "not run here" && !forced = [ 1 ]);
  at [ 1 ] 2;
  assert (!forced = [ 2; 1 ]);
  assert (Array.length (Probe.records probe path) = 2)

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace branches (graph g :context value (let* [x (if true (+ 1 2) (* 3 4)) y (cond \
          false 1 true 2 :else 3) z (case 1 0 4 1 5 :else 6)] (+ x (+ y z)))))")
  in
  let workspace = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let scope = Projection.of_graph ~folded:(fun _ -> false) catalog workspace "g" in
  List.iter
    (fun path ->
      (* an arm is an input of its conditional: a call written there is a card, not a zone *)
      assert ((Option.get (Projection.find scope path)).zone = None))
    [ [ "g"; "x#then" ]; [ "g"; "x#else" ] ];
  let source, workspace =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = [ "g"; "x#then" ]; key = Pos 0; sub = []; value = Syntax.make (Num "8") }))
  in
  let probe = Probe.make (ok (Eval.static ~record:true workspace)) in
  let scope = Projection.of_graph ~folded:(fun _ -> false) catalog workspace "g" in
  let node = Option.get (Projection.find scope [ "g"; "x#then" ]) in
  assert ((Probe.footer probe node ~probes:[]).value = "10");
  let source, _ =
    ok (Flow_edit.apply_checked catalog source (Add_arm { node = [ "g"; "y" ]; after = 0 }))
  in
  assert (
    Flow_edit.remap (Add_arm { node = [ "g"; "y" ]; after = 0 }) [ "g"; "y#then~2"; "@result" ]
    = Some [ "g"; "y#then~3"; "@result" ]);
  let _, _ =
    ok (Flow_edit.apply_checked catalog source (Delete_arm { node = [ "g"; "y" ]; index = 0 }))
  in
  assert (
    Flow_edit.remap (Delete_arm { node = [ "g"; "y" ]; index = 0 }) [ "g"; "y#then"; "@result" ]
    = None)

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace toy (graph g :context draw (let* [elapsed (state [s 0.0] (+ s (frame/dt))) \
          dot (draw/circle [12 20 0] (+ 1 elapsed) :fill \"#00ffff\")] (draw/merge dot))))")
  in
  let source, workspace =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg
            {
              node = [ "g"; "elapsed" ];
              key = Bv (1, 1);
              sub = [];
              value = Syntax.make (Num "2.0");
            }))
  in
  ignore source;
  let scope = Projection.of_graph catalog workspace "g" in
  let state = Option.get (Projection.find scope [ "g"; "elapsed" ]) in
  assert ((Option.get state.zone).kind = Projection.State);
  let dot = Option.get (Projection.find scope [ "g"; "dot" ]) in
  assert (dot.ty = Flow.Ty.drawing && List.length dot.rows = 4);
  let evaluated = ok (Eval.static ~record:true workspace) in
  assert (Array.length evaluated.plan.nodes = 2);
  let session = Eval.create_state () in
  let live = { (Input.at_time 0.) with dt = 0.25 } in
  let probes = Probe.make ~state:session ~live evaluated in
  ignore (Probe.records probes [ "g"; "elapsed" ]);
  let record = List.assoc [ "g"; "elapsed" ] evaluated.records |> List.hd |> snd in
  assert (ok (Eval.force ~state:session record ~live) = Eval.Float 2.25)

let () =
  let source =
    ok (Syntax.parse "(workspace readback (graph g :context value (let* [x (exact (sin t))] x)))")
  in
  let workspace =
    match Workspace.check catalog source with
    | Some w, [] -> w
    | _ -> failwith "exact did not check"
  in
  let scope = Projection.of_graph catalog workspace "g" in
  let card = Option.get (Projection.find scope [ "g"; "x" ]) in
  assert (card.ty = Ty.Float && List.length card.rows = 1);
  let source, edited =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = [ "g"; "x" ]; key = Pos 0; sub = []; value = Syntax.make (Num "2.5") }))
  in
  ignore source;
  let result = ok (Eval.run ~time:7. edited) in
  assert (List.assoc "g" result.results = Eval.Float 2.5)

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace nested_exact (graph g :context draw\n\
         \    (let* [output (draw/circles (exact (map (fn [x] [(+ x t) 0 0]) (array/float 4))))] \
          output)))")
  in
  let workspace =
    match Workspace.check catalog source with
    | Some w, [] -> w
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let scope = Projection.of_graph catalog workspace "g" in
  let exact = Option.get (Projection.find scope [ "g"; "output#0" ]) in
  assert (exact.head = "exact");
  assert (Option.is_some (Projection.find scope [ "g"; "output#0#0#0" ]));
  let replacement = ok (Syntax.parse "(array/vec3 4)") |> List.hd in
  let _, edited =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = exact.path; key = Pos 0; sub = []; value = replacement }))
  in
  assert (
    Option.is_none
      (Projection.find (Projection.of_graph catalog edited "g") [ "g"; "output#0#0#0" ]))

let () =
  let source =
    ok
      (Syntax.parse
         {|(workspace frozen (graph g :context image
    (let* [img (image/noise :width 3 :height 2) snapshot (exact img)] snapshot)))|})
  in
  let check source =
    match Workspace.check catalog source with
    | Some workspace, [] -> workspace
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let workspace = check source in
  let card =
    Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "snapshot" ] |> Option.get
  in
  assert (card.head = "exact" && card.ty = Ty.image && List.length card.rows = 1);
  let evaluation = ok (Eval.static workspace) in
  let node =
    Array.find_opt (fun (n : Eval.node) -> n.kind = "exact") evaluation.plan.nodes |> Option.get
  in
  assert (
    node.ty = Ty.image && List.mem_assoc "value" node.args
    && List.assoc "g" evaluation.results = Eval.Deferred (Ty.image, node.id));
  let replacement = ok (Syntax.parse "(image/noise :width 5 :height 4)") |> List.hd in
  let source, edited =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = card.path; key = Pos 0; sub = []; value = replacement }))
  in
  let nested =
    Projection.find (Projection.of_graph catalog edited "g") [ "g"; "snapshot#0" ] |> Option.get
  in
  assert (nested.head = "image/noise");
  let text, _ = Lisp.print source in
  let roundtrip = check (ok (Syntax.parse text)) in
  let result = ok (Eval.static roundtrip) in
  let node =
    Array.find_opt (fun (n : Eval.node) -> n.kind = "exact") result.plan.nodes |> Option.get
  in
  let child =
    match List.assoc "value" node.args with
    | Eval.Deferred (_, id) -> result.plan.nodes.(id)
    | _ -> assert false
  in
  assert (List.assoc "width" child.args = Eval.Int 5 && List.assoc "height" child.args = Eval.Int 4)

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace norm (graph g :context value\n    (let* [radius (length [3 4 0])] radius)))")
  in
  let checked =
    match Workspace.check catalog source with
    | Some w, [] -> w
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let card =
    Projection.find (Projection.of_graph catalog checked "g") [ "g"; "radius" ] |> Option.get
  in
  assert (card.ty = Ty.Float && (List.hd card.rows).ty = Some Ty.Vec3);
  let vector = ok (Syntax.parse "[0 0 12]") |> List.hd in
  let _, edited =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = card.path; key = Pos 0; sub = []; value = vector }))
  in
  assert (List.assoc "g" (ok (Eval.run ~time:0. edited)).results = Eval.Float 12.)

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace maps (graph g :context value (let* [amp 2.0 out (map (fn [(x : float)] (let* \
          [shift (+ x amp)] (* shift 3.0))) (array/float 10003 0.5))] (array/count out))))")
  in
  let workspace =
    match Workspace.check catalog source with
    | Some w, [] -> w
    | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let scope = Projection.of_graph catalog workspace "g" in
  let fn = Option.get (Projection.find scope [ "g"; "out#0" ]) in
  assert (
    fn.ty = Ty.Fn (Some { params = [ Ty.Float ]; result = Ty.Float })
    && (Option.get fn.zone).kind = Projection.Fn);
  assert (
    List.exists
      (fun (rail : Projection.rail_row) -> rail.name = "x" && rail.ty = Some Ty.Float)
      (Option.get fn.zone).rail);
  let card = Option.get (Projection.find scope [ "g"; "out" ]) in
  assert ((List.hd card.rows).chip = Projection.Name "out#0");
  let chain = Probe.chains scope in
  assert (Hashtbl.find chain [ "g"; "out#0"; "shift" ] = [ [ "g"; "out#0" ] ]);
  let probe = Probe.make (ok (Eval.static ~record:true workspace)) in
  assert (List.assoc [ "g"; "out#0" ] (Probe.counts probe scope ~probe:(fun _ -> 0)) = 10003);
  let shift = Option.get (Projection.find scope [ "g"; "out#0"; "shift" ]) in
  assert ((Probe.footer probe shift ~probes:[ 9999 ]).value = "2.5");
  let source, edited =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = shift.path; key = Pos 1; sub = []; value = Syntax.make (Num "4.0") }))
  in
  ignore source;
  let scope = Projection.of_graph catalog edited "g" in
  let shift = Option.get (Projection.find scope shift.path) in
  let probe = Probe.make (ok (Eval.static ~record:true edited)) in
  assert ((Probe.footer probe shift ~probes:[ 9999 ]).value = "4.5");
  let source =
    ok
      (Syntax.parse
         "(workspace nested (graph g :context value (array/count (map (fn [(x : float)] (+ x 2.0)) \
          (array/float 3 1.0)))))")
  in
  let workspace = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let scope = Projection.of_graph catalog workspace "g" in
  let fn = Option.get (Projection.find scope [ "g"; "@result#0#0" ]) in
  let body = Option.get (Projection.find scope (fn.path @ [ "@result" ])) in
  let _, edited =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = body.path; key = Pos 1; sub = []; value = Syntax.make (Num "5.0") }))
  in
  let probe = Probe.make (ok (Eval.static ~record:true edited)) in
  assert ((Probe.footer probe body ~probes:[ 2 ]).value = "6")

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace scatter (graph g :context host (let* [added (array/scatter_add (array/int 3 \
          0) (array/vec3 3 [1 2 3]) 2)] added)))")
  in
  let workspace = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let card =
    Option.get (Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "added" ])
  in
  assert (card.head = "array/scatter_add" && card.ty = Ty.Array Ty.Vec3);
  let op = Option.get (Op.find card.head Context.value) in
  assert (
    op.signature.pos
    = [ ("indices", Ty.Array Ty.Int); ("values", Ty.Array Ty.Any); ("size", Ty.Int) ]);
  let edited_source, edited =
    ok
      (Flow_edit.apply_checked catalog source
         (Set_arg { node = card.path; key = Pos 2; sub = []; value = Syntax.make (Num "3") }))
  in
  assert (
    List.assoc "g" (ok (Eval.run ~time:0. edited)).results
    = Eval.Vec3_array [| 3.; 6.; 9.; 0.; 0.; 0.; 0.; 0.; 0. |]);
  let saved, _ = Lisp.print edited_source in
  let restored = ok (Syntax.parse saved) in
  assert (match Workspace.check catalog restored with Some _, [] -> true | _ -> false);
  List.iter
    (fun (index, replacement) ->
      assert (
        Result.is_error
          (Flow_edit.apply_checked catalog source
             (Set_arg
                {
                  node = card.path;
                  key = Pos index;
                  sub = [];
                  value = List.hd (ok (Syntax.parse replacement));
                }))))
    [ (0, "(array/float 3)"); (1, "(array/text 3)"); (2, "\"bad\""); (2, "2.5") ]

let () =
  List.iter
    (fun (name, argument, expected, edited_argument, edited_expected) ->
      let source =
        ok
          (Syntax.parse
             ("(workspace integer_arrays (graph g :context host (let* [tested (" ^ name ^ " "
            ^ argument ^ ")] tested)))"))
      in
      let workspace =
        match Workspace.check catalog source with Some w, [] -> w | _ -> assert false
      in
      let card =
        Option.get (Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "tested" ])
      in
      assert (card.head = name && card.ty = Ty.Array Ty.Int);
      let signature = (Option.get (Op.find name Context.value)).signature in
      assert (
        signature.pos = [ ((if name = "array/compact" then "mask" else "array"), Ty.Array Ty.Int) ]);
      assert (List.assoc "g" (ok (Eval.run ~time:0. workspace)).results = Eval.Int_array expected);
      let changed, checked =
        ok
          (Flow_edit.apply_checked catalog source
             (Set_arg
                {
                  node = card.path;
                  key = Pos 0;
                  sub = [];
                  value = List.hd (ok (Syntax.parse edited_argument));
                }))
      in
      assert (
        List.assoc "g" (ok (Eval.run ~time:0. checked)).results = Eval.Int_array edited_expected);
      let saved, _ = Lisp.print changed in
      assert (
        match Workspace.check catalog (ok (Syntax.parse saved)) with
        | Some _, [] -> true
        | _ -> false);
      List.iter
        (fun value ->
          assert (
            Result.is_error
              (Flow_edit.apply_checked catalog source
                 (Set_arg
                    {
                      node = card.path;
                      key = Pos 0;
                      sub = [];
                      value = List.hd (ok (Syntax.parse value));
                    }))))
        [ "(array/float 3)"; "(array/text 3)"; "3" ])
    [
      ("array/compact", "(array/int 3 1)", [| 0; 1; 2 |], "(array/int 5 0)", [||]);
      ( "array/scan_sum",
        "(array/int 3 2)",
        [| 0; 2; 4; 6 |],
        "(array/int 5 -1)",
        [| 0; -1; -2; -3; -4; -5 |] );
    ]

let () =
  List.iter
    (fun (name, args, expected) ->
      let source =
        ok
          (Syntax.parse
             ("(workspace ordering (graph g :context host (let* [tested (" ^ name ^ " " ^ args
            ^ ")] tested)))"))
      in
      let workspace =
        match Workspace.check catalog source with Some w, [] -> w | _ -> assert false
      in
      let card =
        Option.get (Projection.find (Projection.of_graph catalog workspace "g") [ "g"; "tested" ])
      in
      assert (card.head = name && card.ty = Ty.Array Ty.Int);
      assert ((List.hd card.rows).ty = Some (Ty.Array Ty.Any) && (List.hd card.rows).socket);
      if name = "array/grade" then
        assert (
          List.exists
            (fun (row : Projection.row) -> row.key = Kw "descending" && row.ty = Some Ty.Bool)
            card.rows);
      let signature = (Option.get (Op.find name Context.value)).signature in
      assert (
        signature.pos
        =
        if name = "array/grade" then [ ("keys", Ty.Array Ty.Any) ]
        else [ ("keys", Ty.Array Ty.Any); ("queries", Ty.Array Ty.Any) ]);
      assert (List.assoc "g" (ok (Eval.run ~time:0. workspace)).results = Eval.Int_array expected);
      let key, value, expected =
        if name = "array/grade" then (Flow_edit.Kw "descending", "true", [| 0; 1; 2 |])
        else (Flow_edit.Pos 1, "(array/int 5 2)", [| -1; -1; -1; -1; -1 |])
      in
      let changed, checked =
        ok
          (Flow_edit.apply_checked catalog source
             (Set_arg { node = card.path; key; sub = []; value = List.hd (ok (Syntax.parse value)) }))
      in
      assert (List.assoc "g" (ok (Eval.run ~time:0. checked)).results = Eval.Int_array expected);
      let saved, _ = Lisp.print changed in
      assert (
        match Workspace.check catalog (ok (Syntax.parse saved)) with
        | Some _, [] -> true
        | _ -> false);
      let _, checked =
        ok
          (Flow_edit.apply_checked catalog source
             (Set_arg
                {
                  node = card.path;
                  key = Pos 0;
                  sub = [];
                  value = List.hd (ok (Syntax.parse "(array/int 0)"));
                }))
      in
      assert (
        List.assoc "g" (ok (Eval.run ~time:0. checked)).results
        = if name = "array/grade" then Eval.Int_array [||] else Eval.Int_array [| -1; -1; -1; -1 |]);
      List.iter
        (fun (key, value) ->
          assert (
            Result.is_error
              (Flow_edit.apply_checked catalog source
                 (Set_arg
                    { node = card.path; key; sub = []; value = List.hd (ok (Syntax.parse value)) }))))
        (if name = "array/grade" then
           [ (Pos 0, "(array/vec3 3)"); (Pos 0, "3"); (Kw "descending", "\"bad\"") ]
         else [ (Pos 0, "(array/text 3)"); (Pos 1, "(array/float 3)"); (Pos 1, "3") ]))
    [
      ("array/grade", "(array/int 3 2)", [| 0; 1; 2 |]);
      ("array/index_of", "(array/int 3 1) (array/int 4 1)", [| 0; 0; 0; 0 |]);
    ]

let () =
  let source =
    ok
      (Syntax.parse
         "(workspace capacities (graph g :context host (let* [limit (array/max_length)] limit)))")
  in
  let checked = match Workspace.check catalog source with Some w, [] -> w | _ -> assert false in
  let card =
    Option.get (Projection.find (Projection.of_graph catalog checked "g") [ "g"; "limit" ])
  in
  assert (card.ty = Ty.Int && card.head = "array/max_length");
  let selector =
    List.find (fun (row : Projection.row) -> row.key = Flow_edit.Kw "type") card.rows
  in
  assert (selector.ty = Some Ty.Text && selector.control = Projection.Choice && not selector.socket);
  List.iter
    (fun (kind, expected) ->
      let changed, checked =
        ok
          (Flow_edit.apply_checked catalog source
             (Set_arg
                { node = card.path; key = Kw "type"; sub = []; value = Syntax.make (Sym kind) }))
      in
      assert (List.assoc "g" (ok (Eval.run ~time:0. checked)).results = Eval.Int expected);
      let printed, _ = Lisp.print changed in
      let saved =
        match Workspace.check catalog (ok (Syntax.parse printed)) with
        | Some w, [] -> w
        | _ -> assert false
      in
      assert (List.assoc "g" (ok (Eval.run ~time:0. saved)).results = Eval.Int expected))
    [
      ("int", Sys.max_array_length);
      ("text", Sys.max_array_length);
      ("float", Sys.max_floatarray_length);
      ("vec2", Sys.max_floatarray_length / 2);
      ("vec3", Sys.max_floatarray_length / 3);
      ("vec4", Sys.max_floatarray_length / 4);
    ];
  List.iter
    (fun text ->
      assert (
        Result.is_error
          (Flow_edit.apply_checked catalog source
             (Set_arg
                {
                  node = card.path;
                  key = Kw "type";
                  sub = [];
                  value = List.hd (ok (Syntax.parse text));
                }))))
    [ "\"unknown\""; "1"; "(if (> t 0.) \"int\" \"float\")" ];
  assert (
    List.assoc "g" (ok (Eval.run ~time:0. checked)).results = Eval.Int Sys.max_floatarray_length)
