open Flow
let checked body =
  let source = "(workspace w (graph g :context value " ^ body ^ "))" in
  let forms = Result.get_ok (Syntax.parse source) in
  match Workspace.check {Catalog.version = 1; kinds = []} forms with
  | Some w, [] -> w
  | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let value r = List.assoc "g" (Result.get_ok r).Eval.results
let () =
  let workspace=checked "(state [n 0.0] (+ n t))" in
  let run state frame time = Eval.run ~state
    ~live:{(Input.at_time time) with frame} ~time workspace in
  let initial=Eval.create_state() in
  assert(value(run initial 0 0.25)=Eval.Float 0.25);
  let left=Eval.fork_state initial and right=Eval.fork_state initial in
  assert(Eval.state_stamp left=Eval.state_stamp right);
  assert(value(run left 1 0.75)=Eval.Float 1.0);
  assert(value(run right 1 1.25)=Eval.Float 1.5);
  assert(Eval.state_stamp left<>Eval.state_stamp right)
let () =
  let forms=Result.get_ok(Syntax.parse {|(workspace references
    (graph level :context value [(step : float 2)] (state [n 0.0] (+ n step)))
    (graph host :context host (state [n 0] (+ n 3)))
    (graph inline :context value (if (< (ref level) 4) 2 4))
    (graph bound :context value (let* [n (ref level)] (if (< n 4) 2 4)))
    (graph override_inline :context value (if (< (ref level :step 4) 8) 4 8))
    (graph override_bound :context value
      (let* [n (ref level :step 4.0)] (if (< n 8) 4 8)))
    (graph other :context value (if (< (ref level :step 6) 12) 6 12))
    (graph shared :context value (+ (ref level :step 4) (ref level :step 4.0)))
    (graph host_inline :context value (if (< (ref host) 6) 3 6))
    (graph host_bound :context value (let* [n (ref host)] (if (< n 6) 3 6))))|})in
  let workspace=match Workspace.check {Catalog.version=1;kinds=[]} forms with
    |Some w,[]->w|_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds))in
  let evaluation=Result.get_ok(Eval.static workspace)in
  let identities()=Array.map(fun(i:Eval.instance)->
    i.graph,i.default,List.map(fun(n,v)->n,Eval.show v)i.inputs)evaluation.plan.instances in
  let original=identities()in
  assert(Array.length original=12);
  List.iter(fun force->
    let caller=Eval.create_state()in
    List.iter(fun(tick,frame,level,host)->
      let live={(Input.at_time 0.)with tick;frame}in
      List.iter(fun v->ignore(Result.get_ok(force ~state:caller v ~live)))evaluation.states;
      let stamp=Eval.state_stamp caller in
      let expected=["level",Eval.Float(float(2*level));"host",Eval.Int(3*host);
        "inline",Eval.Int(2*level);"bound",Eval.Int(2*level);
        "override_inline",Eval.Int(4*level);"override_bound",Eval.Int(4*level);
        "other",Eval.Int(6*level);"shared",Eval.Float(float(8*level));
        "host_inline",Eval.Int(3*host);"host_bound",Eval.Int(3*host)]in
      List.iter(fun results->
        let private_state=Eval.fork_state caller in
        for _=1 to 2 do
          List.iter(fun(name,residual)->
            let actual=Result.get_ok(force ~state:private_state residual ~live)in
            let wanted=List.assoc name expected in
            if actual<>wanted then failwith(Printf.sprintf
              "Graph state %s at tick%d/frame%d: expected %s, got %s"
              name tick frame (Eval.show wanted)(Eval.show actual)))results
        done;
        assert(Eval.state_stamp private_state=stamp && Eval.state_stamp caller=stamp))
        [evaluation.results;List.rev evaluation.results];
      assert(identities()=original))
      [0,0,1,1;1,0,1,2;1,1,2,2])
    [(fun ~state v ~live->Eval.force ~state v ~live);
     (fun ~state v ~live->Eval.Host.force_reference ~state v ~live)];
  print_endline "Graph state: inline/bound known tuples, coercion sharing, opposite orders, private snapshots and host/frame clocks agree"

let () =
  let w = checked "(let* [s (state [previous 0.0] (+ previous (frame/dt)))] (+ s s))" in
  let state = Eval.create_state () in
  let run frame = Eval.run ~state ~live:{(Input.at_time (float frame)) with frame; dt = 0.25} ~time:(float frame) w in
  assert (value (run 0) = Eval.Float 0.5);
  assert (value (run 0) = Eval.Float 0.5);
  assert (value (run 1) = Eval.Float 1.);
  assert (value (run 2) = Eval.Float 1.5);
  (* Seeking backwards and explicit reload reset both begin a fresh fold. *)
  assert (value (run 0) = Eval.Float 0.5);
  Eval.reset_state state;
  assert (value (run 1) = Eval.Float 0.5);
  (* Independent playback sessions have independent snapshots. *)
  let export () =
    let state = Eval.create_state () in
    Array.init 8 (fun frame -> value (Eval.run ~state
      ~live:{(Input.at_time (float frame *. 0.125)) with frame; dt = 0.125}
      ~time:(float frame *. 0.125) w)) in
  assert (Marshal.to_string (export ()) [] = Marshal.to_string (export ()) []);
  (* Every state reads the previous snapshot, independent of forcing order. *)
  let w = checked "(let* [a (state [x 0] (+ x 1)) b (state [y 0] (+ y a))] (+ a b))" in
  let s = Eval.create_state () in
  let run frame = value (Eval.run ~state:s ~live:{(Input.at_time 0.) with frame} ~time:0. w) in
  assert (run 0 = Eval.Int 2 && run 1 = Eval.Int 5 && run 1 = Eval.Int 5);
  (* An error after the fold step must not commit a partial frame. *)
  let w = checked "(let* [a (state [x 0] (+ x 1))] (+ a (nth (list 0) (frame/index))))" in
  let s = Eval.create_state () in
  let run frame = Eval.run ~state:s ~live:{(Input.at_time 0.) with frame} ~time:0. w in
  assert (value (run 0) = Eval.Int 1);
  assert (Result.is_error (run 1));
  assert (value (run 0) = Eval.Int 1);
  let forms = Result.get_ok (Syntax.parse "(workspace w (graph g :context value (state [s t] s)))") in
  let _, ds = Workspace.check {Catalog.version = 1; kinds = []} forms in
  assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_STATE_INIT") ds)

let () =
  let forms=Result.get_ok(Syntax.parse "(workspace w (graph host :context host (state [n 0] (+ n 1))) (graph animated :context value (state [n 0] (+ n 1))))")in
  let workspace=match Workspace.check {Catalog.version=1;kinds=[]} forms with
    |Some workspace,[]->workspace|_,ds->failwith(String.concat "; " (List.map Diagnostic.to_string ds))in
  let state=Eval.create_state()in
  let run tick frame=Result.get_ok(Eval.run ~state ~live:{(Input.at_time 0.)with tick;frame} ~time:0. workspace)in
  let result=run 0 0 in
  assert(List.assoc "host" result.results=Eval.Int 1 && List.assoc "animated" result.results=Eval.Int 1);
  let result=run 1 0 in
  assert(List.assoc "host" result.results=Eval.Int 2 && List.assoc "animated" result.results=Eval.Int 1);
  assert(List.assoc "host" (run 1 0).results=Eval.Int 2);
  let snapshot=Eval.fork_state state in
  assert(List.assoc "host" (run 2 1).results=Eval.Int 3);
  let result=Result.get_ok(Eval.run ~state:snapshot ~live:{(Input.at_time 0.)with tick=2;frame=1} ~time:0. workspace)in
  assert(List.assoc "host" result.results=Eval.Int 3 && List.assoc "animated" result.results=Eval.Int 2);
  Eval.reset_state ~host_state:false state;
  let result=run 3 0 in
  assert(List.assoc "host" result.results=Eval.Int 4 && List.assoc "animated" result.results=Eval.Int 1);
  Eval.reset_state state;
  assert(List.assoc "host" (run 4 0).results=Eval.Int 1)

let () =
  let catalog=Catalog.{version=1;kinds=[]} in
  let reject expression =
    let forms=Result.get_ok(Syntax.parse("(workspace images (graph g :context value "^expression^"))"))in
    let workspace,ds=Workspace.check catalog forms in
    assert(workspace=None && List.exists(fun(d:Diagnostic.t)->d.code="E_STATE_TYPE")ds)in
  List.iter reject
    ["(state [s (exact (image/noise))] s)";
     "(let* [img (exact (image/noise)) alias img] (state [s alias] s))";
     "(state [s (list (exact (image/noise)))] s)";
     "(state [s {:image (exact (image/noise))}] s)";
     "(let* [snapshot (fn [] (exact (image/noise)))] (state [s (snapshot)] s))"];
  let forms=Result.get_ok(Syntax.parse {|(workspace images
    (graph img :context image (exact (image/noise)))
    (graph g :context value (state [s (ref img)] s)))|})in
  let workspace,ds=Workspace.check catalog forms in
  assert(workspace=None && List.exists(fun(d:Diagnostic.t)->d.code="E_STATE_TYPE")ds);
  assert(value(Eval.run ~time:0. (checked "(state [s (exact 1.0)] (+ s 1))"))=Eval.Float 2.);
  let array_state=Eval.create_state()in
  assert(value(Eval.run ~state:array_state ~time:0. (checked
    "(array/sum (state [s (exact (array/range 3))] s))"))=Eval.Float 3.);
  assert(List.mem(Eval.Float_array [|0.;1.;2.|])(Eval.Host.state_values array_state));
  (* An opaque extension can conceal resources from static types. Check the
     seed before the step reads it, and the step before it reaches next. *)
  List.iter(fun wrap->
    let calls=ref 0 in
    let operator={(Option.get(Op.find "sin" Context.value))with
      name="test/opaque";signature={pos=["resource",Ty.Bool];opt=[];rest=None;kw=[]};
      out=(fun _->Ty.Any);any_num=false;arithmetic=None;packed_extension=None;
      body=(fun ~live:_ ~node:_ args->
        incr calls;
        let v=if Value.truthy(List.assoc "resource" args) then Value.Deferred(Ty.image,0)else Value.Int 0 in
        match wrap with
        |0->v|1->Value.List [|v|]|2->Value.Record ["image",v]
        |_->Value.Struct("opaque",Ty.Any,["image",v]))}in
    List.iter(fun (seed,step,expected_calls)->
      let forms=Result.get_ok(Syntax.parse(Printf.sprintf
        "(workspace opaque (graph g :context value (state [s %s] %s)))"seed step))in
      let workspace=match Workspace.check ~ops:[operator] catalog forms with
        |Some w,[]->w|_,ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds))in
      let evaluation=Result.get_ok(Eval.static workspace)in
      let residual=List.assoc "g" evaluation.results in
      List.iter(fun force->
        let state=Eval.create_state()in
        let stamp=Eval.state_stamp state in
        calls:=0;
        (match force ~state residual ~live:(Input.at_time 0.)with
         |Error d->assert(d.Diagnostic.code="E_STATE_TYPE")|Ok _->assert false);
        assert(!calls=expected_calls && Eval.state_stamp state=stamp
          && Eval.Host.state_values state=[]))
        [(fun ~state v ~live->Eval.force ~state v ~live);
         (fun ~state v ~live->Eval.Host.force_reference ~state v ~live)])
      ["(test/opaque true)","(test/opaque false)",1;
       "(test/opaque false)","(test/opaque true)",2]) [0;1;2;3];
  print_endline "Exact images: static state refusal and opaque seed/step backstops preserve caller state; numeric and packed exact state remains data"
