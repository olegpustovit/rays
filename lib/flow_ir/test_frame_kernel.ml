module E = Flow.Eval
module I = Flow_ir
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let checked text =
  match Flow.Workspace.check {Flow.Catalog.version = 1; kinds = []} (ok (Flow.Syntax.parse text)) with
  | Some w, [] -> w | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let read path = In_channel.with_open_text path In_channel.input_all
let bytes v = Marshal.to_string v [Marshal.No_sharing]
let same a b = match a, b with
  | Ok a, Ok b -> bytes a = bytes b
  | Error a, Error b -> Diagnostic.to_string a = Diagnostic.to_string b
  | _ -> false
let live frame dt = {(Input.at_time (float frame /. 60.)) with frame; dt; size = 800, 600}
let compare_frames value frames =
  let program = ok (I.Executor.compile value) in
  let one = E.create_state () and eight = E.create_state () and reference = E.create_state () in
  List.iter (fun (frame, dt) ->
    let live = live frame dt in
    let expected = E.Private.force_reference ~state:reference value ~live in
    List.iter (fun (domains, state) -> Parallel.run ~domains (fun () ->
      assert (same (I.Executor.force ~state program ~live) expected);
      assert (bytes (E.Private.state_values state) = bytes (E.Private.state_values reference)))) [1, one; 8, eight]) frames

let compare_prepared value frames =
  let r=match value with E.Residual r -> r | _ -> failwith "Expected a frame residual." in
  let core=match I.Core_lower.lower r with Ok p -> p | Error name ->
    failwith ("Prepared kernel refused "^name^": "^fst (Flow.Lisp.print [(E.Residual.residual_view r).term.form])) in
  let prepared=I.Core_exec.prepare core in
  let state=E.create_state () and reference=E.create_state () in
  List.iter (fun (frame,dt) ->
    let live=live frame dt in
    let expected=E.Private.force_reference ~state:reference value ~live in
    let actual=Result.bind (E.Residual.bind_live ~state r ~live)
      (fun () -> I.Core_exec.execute ~root:r prepared ~live) in
    assert (same actual expected)) frames

let () =
  let source = ok (Flow.Syntax.parse (read "../../sketches/particles.rays")) in
  let source = match source with
    | [{node = Flow.Syntax.List (head :: name :: graphs); _} as workspace] ->
        let picture = List.find (fun (graph : Flow.Syntax.t) -> match graph.node with
          | List (_ :: {node = Sym "picture"; _} :: _) -> true | _ -> false) graphs in
        [{workspace with node = List [head; name; picture]}]
    | _ -> assert false in
  let workspace = match Flow.Workspace.check {Flow.Catalog.version = 1; kinds = []} source with
    | Some w, [] -> w | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
  let evaluation = ok (E.static workspace) in
  let particles = List.hd evaluation.states in
  compare_frames particles [0, 1. /. 60.; 1, 0.125; 2, 20.; 2, 20.; 3, 20.; 4, 0.125; 1, 0.125];
  let value body =
    let ws = checked ("(workspace w (graph g :context value (let* [tested " ^ body ^ "] 0.0)))") in
    let evaluation = ok (E.static ~record:true ws) in
    List.assoc ["g"; "tested"] evaluation.records |> List.hd |> snd in
  let bound=value "(state [p {:xs (array/float 2051) :scale 0.125}] (let* [f (fn [x] (+ x p.scale))] {:xs (map f p.xs) :scale (+ p.scale (frame/dt))}))" in
  compare_frames bound [0,0.125;1,0.125;2,0.125;3,0.125;4,0.125;5,0.125];
  List.iter (fun body -> compare_frames (value body)
    [0, 0.125; 1, 0.5; 2, 1.; 2, 1.; 1024, 0.125; 1025, 0.125; 1, 0.25])
    ["(state [p {:xs (array/float 2051) :scale 0.125}] {:xs (map (fn [x] (+ x p.scale)) p.xs) :scale (+ p.scale (frame/dt))})";
     "(state [p (array/float 0)] (map (fn [x] (+ x (frame/dt))) (array/range (frame/index))))";
     "(state [p (array/range 16385)] (map (fn [x] (if (not (and (>= x 0) (< x (frame/width)))) (- 0 x) (+ x (frame/dt)))) p))";
     "(state [p (array/float 2051)] (scan [a 0.0] [x p] (+ a (+ x (frame/dt)))))";
     "(state [p (array/float 2051)] (map (fn [x] (if (< (frame/dt) 1) (+ x (frame/dt)) (pow (+ x 2) 10000000))) p))"];
  let v = value "(let* [a (state [p (array/float 2051)] (map (fn [x] (+ x (frame/dt))) p))] {:xs a :error (array/nth (array/float 2) (frame/index))})" in
  compare_frames v [0, 0.125; 1, 0.125; 2, 0.125; 1, 0.125; 3, 0.125];
  List.iter (fun body -> compare_prepared (value body)
    [0,0.125;1,0.5;2,1.;2,1.;3,20.;4,0.125;1,0.25]) [
    "(state [p (exact (map (fn [x] [x (- 0 x)]) (array/range 2051)))] (map (fn [v] (+ (* v (frame/dt)) 0.5)) p))";
    "(state [p (exact (map (fn [x] [x (- 0 x) 1 2]) (array/range 2051)))] (map (fn [v] (+ (* v (frame/dt)) [1 2 3 4])) p))";
    "(state [p {:xs (array/range 2051) :scale 0.125}] {:xs (map (fn [x] (if (> x 1024) (+ x p.scale) (sin x))) p.xs) :scale (+ p.scale (frame/dt))})";
    "(state [p (array/range 2051)] (map (fn [x] (if (< (frame/dt) 1) (+ x 1) (pow (+ x 2) 10000000))) p))";
    "(state [p (array/range 2051)] (filter (fn [x] (< x (frame/width))) p))";
    "(state [p (array/range 2051)] (map (fn [x] (let* [f (fn [y] (+ y (frame/dt)))] (+ (f x) (f x)))) p))" ];
  (* This is an execution check: the old per-element boxed path allocates several
     megabytes here. A warm prepared block map retains only its packed output. *)
  let v=value "(state [p (array/range 32771)] (map (fn [x] (+ (* x (frame/dt)) 1.)) p))" in
  let r=match v with E.Residual r -> r | _ -> assert false in
  let prepared=I.Core_exec.prepare (Result.get_ok (I.Core_lower.lower r)) and state=E.create_state () in
  let force frame =
    let live=live frame 0.125 in
    ignore (ok (E.Residual.bind_live ~state r ~live));
    ignore (ok (I.Core_exec.execute ~root:r prepared ~live)) in
  force 0;Gc.full_major ();
  let before=Gc.allocated_bytes () in
  for frame=1 to 3 do force frame done;
  let bytes=(Gc.allocated_bytes ()-.before)/.3. in
  if bytes>700_000. then failwith (Printf.sprintf "Prepared map still boxes its elements: %.0f bytes/frame" bytes);
  print_endline "Packed frame folds: template reuse, particle branches, domains, resets, repeated reads and rollback passed"
