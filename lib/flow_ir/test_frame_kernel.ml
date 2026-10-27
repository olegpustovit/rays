module E = Flow.Eval
module I = Flow_ir
let ok = function Ok x -> x | Error d -> failwith (Flow.Diagnostic.to_string d)
let checked text =
  match Flow.Workspace.check {Flow.Check.version = 1; kinds = []} (ok (Flow.Syntax.parse text)) with
  | Some w, [] -> w | _, ds -> failwith (String.concat "; " (List.map Flow.Diagnostic.to_string ds))
let read path = In_channel.with_open_text path In_channel.input_all
let bytes v = Marshal.to_string v [Marshal.No_sharing]
let same a b = match a, b with
  | Ok a, Ok b -> bytes a = bytes b
  | Error a, Error b -> Flow.Diagnostic.to_string a = Flow.Diagnostic.to_string b
  | _ -> false
let live frame dt = {(Frame_input.at_time (float frame /. 60.)) with frame; dt; size = 800, 600}
let compare_frames value frames =
  let program = ok (I.Executor.compile value) in
  let one = E.create_state () and eight = E.create_state () and reference = E.create_state () in
  List.iter (fun (frame, dt) ->
    let live = live frame dt in
    let expected = E.Private.force_reference ~state:reference value ~live in
    List.iter (fun (domains, state) -> Rays_math.Parallel.run ~domains (fun () ->
      assert (same (I.Executor.force ~state program ~live) expected);
      assert (E.state_stamp state = E.state_stamp reference))) [1, one; 8, eight]) frames

let compare_prepared value frames =
  let r=match value with E.Residual r -> r | _ -> failwith "Expected a frame residual." in
  let core=match I.Core_lower.lower r with Ok p -> p | Error name ->
    failwith ("Prepared kernel refused "^name^": "^fst (Flow.Lisp.print [(E.Private.residual_view r).term.form])) in
  let prepared=I.Core_exec.prepare core and boxed=I.Core_exec.prepare ~fused:false core in
  let state=E.create_state () and general=E.create_state () in
  let force state prepared live =
    let execute r live=Some (I.Core_exec.execute ~root:r prepared ~live) in
    E.Private.force_with_executor ~state ~execute value ~live in
  List.iter (fun (frame,dt) ->
    let live=live frame dt in
    assert (same (force state prepared live) (force general boxed live));
    assert (E.state_stamp state=E.state_stamp general)) frames

let () =
  let source = ok (Flow.Syntax.parse (read "../../sketches/particles.rays")) in
  let source = match source with
    | [{node = Flow.Syntax.List (head :: name :: graphs); _} as workspace] ->
        let picture = List.find (fun (graph : Flow.Syntax.t) -> match graph.node with
          | List (_ :: {node = Sym "picture"; _} :: _) -> true | _ -> false) graphs in
        [{workspace with node = List [head; name; picture]}]
    | _ -> assert false in
  let workspace = match Flow.Workspace.check {Flow.Check.version = 1; kinds = []} source with
    | Some w, [] -> w | _, ds -> failwith (String.concat "; " (List.map Flow.Diagnostic.to_string ds)) in
  let evaluation = ok (E.static workspace) in
  let particles = List.hd evaluation.states in
  compare_frames particles [0, 1. /. 60.; 1, 0.125; 2, 20.; 2, 20.; 3, 20.; 4, 0.125; 1, 0.125];
  (* Prove template dispatch and reuse, then compare the public executor as well. *)
  let templates = ref [] and compiled = ref 0 and rebound = ref 0 and executed = ref 0 in
  let state = E.create_state () and reference = E.create_state () in
  let execute r live =
    let program = match List.find_map (fun p -> I.Packed.rebind p r) !templates with
      | Some p -> incr rebound; Some p
      | None ->
          let p = I.Packed.compile_template r (E.Private.residual_view r).term in
          Option.iter (fun p -> incr compiled; templates := p :: !templates) p; p in
    Option.map (fun p -> incr executed; I.Packed.force ~state p ~live) program in
  for frame = 0 to 5 do
    let live = live frame (if frame > 1 then 20. else 0.125) in
    assert (same (E.Private.force_with_executor ~state ~execute particles ~live)
      (E.Private.force_reference ~state:reference particles ~live))
  done;
  if !compiled <> 2 || !rebound <> 10 || !executed <> 12 then
    failwith (Printf.sprintf "template counts: %d compiled, %d rebound, %d executed" !compiled !rebound !executed);
  let value body =
    let ws = checked ("(workspace w (graph g :context value (let* [tested " ^ body ^ "] 0.0)))") in
    let evaluation = ok (E.static ~record:true ws) in
    List.assoc ["g"; "tested"] evaluation.records |> List.hd |> snd in
  let bound=value "(state [p {:xs (array/float 2051) :scale 0.125}] (let* [f (fn [x] (+ x p.scale))] {:xs (map f p.xs) :scale (+ p.scale (frame/dt))}))" in
  templates:=[];compiled:=0;rebound:=0;executed:=0;
  let state=E.create_state () and reference=E.create_state () in
  for frame=0 to 5 do
    let live=live frame 0.125 in
    assert (same (E.Private.force_with_executor ~state ~execute bound ~live)
      (E.Private.force_reference ~state:reference bound ~live))
  done;
  assert (!compiled=1 && !rebound=5 && !executed=6);
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
    let execute r live=Some (I.Core_exec.execute ~root:r prepared ~live) in
    ignore (ok (E.Private.force_with_executor ~state ~execute v ~live:(live frame 0.125))) in
  force 0;Gc.full_major ();
  let before=Gc.allocated_bytes () in
  for frame=1 to 3 do force frame done;
  let bytes=(Gc.allocated_bytes ()-.before)/.3. in
  if bytes>700_000. then failwith (Printf.sprintf "Prepared map still boxes its elements: %.0f bytes/frame" bytes);
  print_endline "Packed frame folds: template reuse, particle branches, domains, resets, repeated reads and rollback passed"

let () =
  let module C=I.Core in
  let source : C.source={site=[];owner=[];instance=0;prefix=[];iter=[];span=Some {start=1;finish=2}} in
  let s id node : C.statement={id;source;node} in
  let core body results : C.program={constants=[|C.Int 0;Int 3;Int 1;Vector [|1.;2.;3.|]|];
    locals=[|Flow.Ty.Int;Int;Int;Array Float;Int;Int;Int;Float;Vec3|];body;results;
    state_slots=[||];regions=[||];call_slots=0} in
  let setup=[s 0 (Assign (0,Constant 0));s 1 (Assign (1,Constant 1));s 2 (Assign (2,Constant 2));
    s 3 (Assign (3,Prim (Sequence_build,["count",1;"width",2])))] in
  let loop body=s 4 (Counted {first=0;last=1;count=4;index=5;item=6;limit=10;init=[];body}) in
  let check program expected =
    let live=live 0 0.125 in
    let fast=I.Core_exec.execute (I.Core_exec.prepare program) ~live
    and boxed=I.Core_exec.execute (I.Core_exec.prepare ~fused:false program) ~live in
    assert (same fast boxed);assert (same fast expected) in
  (* Loop counters remain observable after a block run. *)
  check (core (setup @ [loop [s 5 (Assign (7,Frame Dt));s 6 (Store {sequence=3;index=6;value=7})]]) [|5|]) (Ok (E.Int 2));
  (* A vector cannot be silently narrowed by a scalar conversion. *)
  let program=core (setup @ [loop [s 5 (Assign (8,Constant 3));
    s 6 (Assign (7,Prim (Float_convert,["x",8])));s 7 (Store {sequence=3;index=6;value=7})]]) [|3|] in
  let live=live 0 0.125 in
  assert (same (I.Core_exec.execute (I.Core_exec.prepare program) ~live)
    (I.Core_exec.execute (I.Core_exec.prepare ~fused:false program) ~live));
  let program=core (setup @ [loop [s 5 (Assign (7,Frame Index));s 6 (Assign (8,Frame Tick));
    s 7 (Assign (9,Prim (Numeric Lt,["a",7;"b",8])));s 8 (Store {sequence=3;index=6;value=9})]]) [|3|] in
  let program={program with locals=[|Flow.Ty.Int;Int;Int;Array Float;Int;Int;Int;Int;Int;Bool|]} in
  let live={live with frame=9007199254740993;tick=9007199254740992} in
  assert (same (I.Core_exec.execute (I.Core_exec.prepare program) ~live)
    (I.Core_exec.execute (I.Core_exec.prepare ~fused:false program) ~live));
  (* Reading a fresh input builder publishes it in the general executor; the
     block tier must preserve that ownership transition, including untaken reads. *)
  List.iter (fun selected ->
    let read=[s 6 (Assign (7,Prim (Array_nth,["array",3;"index",6])));
      s 7 (Store {sequence=10;index=6;value=7})] in
    let loop_body=if selected then read else [s 13 (If {condition=12;yes=read;no=[]})] in
    let program=core (setup @ [s 5 (Assign (10,Prim (Sequence_build,["count",1;"width",2])));
      s 8 (Assign (12,Constant 4));loop loop_body;s 9 (Assign (11,Frame Dt));
      s 10 (Store {sequence=3;index=0;value=11})]) [|10|] in
    let program={program with constants=Array.append program.constants [|C.Bool false|];
      locals=Array.append program.locals [|Flow.Ty.Bool;Array Float;Float;Bool|]} in
    let live={(Frame_input.at_time 0.) with dt=0.125;size=800,600} in
    assert (same (I.Core_exec.execute (I.Core_exec.prepare program) ~live)
      (I.Core_exec.execute (I.Core_exec.prepare ~fused:false program) ~live))) [true;false]
