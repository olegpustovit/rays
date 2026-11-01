(* Flow_ir.static against Flow.Eval.static: a static packed map of more than 4,096 elements is
   made by its CPU kernel after the walker's first 4,096 elements, and the evaluation is the same
   value bit for bit, the same records and the same failure. *)
module E = Flow.Eval
open Test_support

let checked body =
  let forms = ok (Flow.Syntax.parse ("(workspace w (graph g :context host " ^ body ^ "))")) in
  match Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } forms with
  | Some ws, [] -> ws
  | _, ds -> failwith (body ^ ": " ^ String.concat "; " (List.map Diagnostic.to_string ds))

let bits (v : E.value) =
  match v with
  | E.Float_array d | Vec2_array d | Vec3_array d | Vec4_array d -> Array.map Int64.bits_of_float d
  | _ -> failwith "a packed array"

let rec same (a : E.value) (b : E.value) =
  match (a, b) with
  | (E.Float_array _ | Vec2_array _ | Vec3_array _ | Vec4_array _), _ ->
      Flow.Value.ty_of a = Flow.Value.ty_of b && bits a = bits b
  | Record x, Record y ->
      List.length x = List.length y && List.for_all2 (fun (n, v) (m, w) -> n = m && same v w) x y
  | List x, List y -> Array.length x = Array.length y && Array.for_all2 same x y
  | Residual _, Residual _ -> true
  | _ -> a = b

let maps =
  [
    (* the hash of sketches/million_marks, without its drift *)
    "(let* [indices (array/range 20000) extent 300.0]\n\
    \     (map (fn [i] (let* [h (fn [s] (abs (mod (* (sin (+ (* i 12.9898) (* s 78.233))) \
     43758.5453) 1.0)))\n\
    \                         x (- (+ (h 1.0) (h 2.0)) 1.0) y (- (h 3.0) 0.5)]\n\
    \                    [(* x extent) (* y extent) 0])) indices))";
    "(map (fn [i] (* (sqrt i) 0.5)) (array/range 4097))";
    "(let* [a (array/range 9000) b (map (fn [i] (+ i 0.5)) a)] (map (fn [x y] [(- y x) (* x y)]) a \
     b))";
    "(let* [a (array/range 5000)] {:first (map (fn [i] (if (> (mod i 3) 1) (- 0 i) (/ i 7))) a)\n\
    \                                 :second (map (fn [i] [i (cos i) (pow i 0.5) 1]) a)})";
    (* 4,096 elements and fewer are the walker's alone *)
    "(map (fn [i] (* i 3)) (array/range 4096))";
    (* forms the kernel refuses are walked to the end *)
    "(map (fn [i] (value/rand i 7)) (array/range 4200))";
    (* a map that reads the frame stays a residual *)
    "(map (fn [i] (+ i t)) (array/range 5000))";
  ]

let () =
  List.iter
    (fun body ->
      let ws = checked body in
      let walked = ok (E.static ~record:true ws) and fast = ok (Flow_ir.static ~record:true ws) in
      if not (same (List.assoc "g" walked.results) (List.assoc "g" fast.results)) then
        failwith ("Flow_ir.static differs from the walker for " ^ body);
      (* The same paths are recorded. A path every element reaches has the same 4,096 records; one
       inside a branch has those of the first 4,096 elements, where the walker alone would go on
       to later elements until that path had 4,096. *)
      let counts (r : E.t) =
        List.map (fun (path, values) -> (path, List.length values)) r.records
      in
      if
        List.map fst (counts walked) <> List.map fst (counts fast)
        || not (List.for_all2 (fun (_, w) (_, f) -> f <= w) (counts walked) (counts fast))
      then failwith ("records differ for " ^ body);
      let branches =
        let rec at i =
          i + 4 <= String.length body && (String.sub body i 4 = "(if " || at (i + 1))
        in
        at 0
      in
      if (not branches) && counts walked <> counts fast then
        failwith ("records of a map without a branch differ for " ^ body);
      let qualified = snd (ok (Flow_ir.qualify_workspace ~record:true ws)) in
      if not (same (List.assoc "g" walked.results) (List.assoc "g" qualified.results)) then
        failwith ("qualify_workspace differs from the walker for " ^ body))
    maps;
  (* A failure, before or after the walked elements, is the walker's diagnostic. *)
  List.iter
    (fun body ->
      let ws = checked body in
      match (E.static ws, Flow_ir.static ws) with
      | Error a, Error b -> if a <> b then failwith ("another diagnostic for " ^ body)
      | _ -> failwith ("expected a failure for " ^ body))
    [
      "(map (fn [i] (exp (- i 4300))) (array/range 5200))";
      "(map (fn [i] (exp (+ i 700))) (array/range 5200))";
    ];
  (* It is the kernel that makes the elements past 4,096: the walker is not asked for them. The
     walker costs about 22 microseconds an element, so 200,000 elements in well under a second
     of processor time can only be the kernel. *)
  let ws = checked "(map (fn [i] [(sin i) (cos i) 0]) (array/range 200000))" in
  let started = Sys.time () in
  let fast = ok (Flow_ir.static ws) in
  let seconds = Sys.time () -. started in
  (match List.assoc "g" fast.results with
  | E.Vec3_array d -> assert (Array.length d = 600000 && d.(3 * 199999) = sin 199999.)
  | _ -> failwith "a vec3 array");
  if seconds > 2. then
    failwith (Printf.sprintf "a 200,000-element static map took %.2f s: it was walked" seconds);
  (* Allocation: past the walked head each element costs its 24 result bytes and little else. Two
     sizes on one domain (the counter is the calling domain's) give the marginal bytes per element,
     which a closure or a boxed value made per element would multiply: the output loop of
     Packed.force_all once allocated about 112 of them (155 bytes in all, now 43). *)
  let allocated n =
    let ws = checked (Printf.sprintf "(map (fn [i] [(sin i) (cos i) 0]) (array/range %d))" n) in
    Parallel.run ~domains:1 (fun () ->
        let before = Gc.allocated_bytes () in
        ignore (ok (Flow_ir.static ws));
        Gc.allocated_bytes () -. before)
  in
  ignore (allocated 20000);
  let small = allocated 100000 and large = allocated 300000 in
  let per_element = (large -. small) /. 200000. in
  if per_element > 64. then
    failwith
      (Printf.sprintf "a large static map allocates %.0f bytes per element (24 are the result)"
         per_element);
  print_endline "Static maps: kernel-made values, records and failures equal the walker's"

let () =
  let ticks = ref 0. in
  let profile =
    Flow_ir.Profile.create ~clock:(fun () ->
        ticks := !ticks +. 1.;
        !ticks)
  in
  let body = "(map (fn [i] (* i 0.5)) (array/range 4100))" in
  let workspace = checked body in
  let observed = Flow_ir.Profile.sample profile (fun () -> ok (Flow_ir.static workspace)) in
  assert (same (List.assoc "g" observed.results) (List.assoc "g" (ok (E.static workspace)).results));
  (match Flow_ir.Profile.static_maps profile with
  | [ map ] ->
      assert (map.site = [ "g"; "@result" ] && map.count = 4100);
      assert (map.tier = Flow_ir.Cpu_kernel && map.refusal = []);
      assert (map.head_elements = 0 && map.tail_elements = 0);
      assert (map.seconds = map.head_seconds +. map.kernel_seconds +. map.tail_seconds)
  | _ -> failwith "static map profile missing");
  let refused = checked "(map (fn [i] (value/rand i 7)) (array/range 4100))" in
  ignore (Flow_ir.Profile.sample profile (fun () -> ok (Flow_ir.static refused)));
  let map = List.hd (List.rev (Flow_ir.Profile.static_maps profile)) in
  assert (map.tier = Flow_ir.Interp && map.refusal <> []);
  assert (map.head_elements = 4096 && map.tail_elements = 4);
  let failing = checked "(map (fn [i] (exp (- i 4300))) (array/range 5200))" in
  assert (Flow_ir.Profile.sample profile (fun () -> Flow_ir.static failing) = E.static failing);
  let map = List.hd (List.rev (Flow_ir.Profile.static_maps profile)) in
  assert (map.tier = Flow_ir.Interp && map.tail_elements > 0);
  assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_NONFINITE") map.refusal);
  (* A failed scope must restore its predecessor; no platform clock or waiting in this check. *)
  (try Flow_ir.Profile.sample profile (fun () -> failwith "scope") with Failure _ -> ());
  let small = checked "(map (fn [i] (* i 0.5)) (array/range 2))" in
  for _ = 1 to 513 do
    ignore (Flow_ir.Profile.sample profile (fun () -> ok (Flow_ir.static small)))
  done;
  assert (List.length (Flow_ir.Profile.static_maps profile) = 512);
  ignore (ok (Flow_ir.static workspace));
  assert (List.length (Flow_ir.Profile.static_maps profile) = 512);
  let concurrent = Flow_ir.Profile.create ~clock:(fun () -> 0.) in
  let run domains =
    Parallel.run ~domains (fun () ->
        Parallel.init_array ~grain:1 8 (fun _ ->
            Flow_ir.Profile.sample concurrent (fun () -> ok (Flow_ir.static small))))
  in
  let sequential = run 1 and parallel = run 8 in
  Array.iter2
    (fun (a : E.t) (b : E.t) -> assert (same (List.assoc "g" a.results) (List.assoc "g" b.results)))
    sequential parallel;
  assert (List.length (Flow_ir.Profile.static_maps concurrent) = 16);
  print_endline
    "Static map profiles: actual head/tail counts, refusal, scoped clock and bounded history"

let () =
  let workspace ?(table = "(array/int 3 1)") bindings body =
    let source =
      Printf.sprintf
        "(workspace gathers (graph source :context host {:values %s}) (graph g :context host (let* \
         [data (ref source) %s] (map (fn [(i : int)] %s) (array/range 4101 :type int)))))"
        table bindings body
    in
    match
      Flow.Workspace.check Flow.Catalog.{ version = 1; kinds = [] } (ok (Flow.Syntax.parse source))
    with
    | Some workspace, [] -> workspace
    | _, diagnostics -> failwith (String.concat "; " (List.map Diagnostic.to_string diagnostics))
  in
  let parity workspace =
    let profile = Flow_ir.Profile.create ~clock:(fun () -> 0.) in
    match
      (E.static workspace, Flow_ir.Profile.sample profile (fun () -> Flow_ir.static workspace))
    with
    | Ok reference, Ok compiled ->
        assert (same (List.assoc "g" reference.results) (List.assoc "g" compiled.results));
        let map =
          List.find
            (fun (m : Flow_ir.Profile.static_map) -> m.count = 4101)
            (Flow_ir.Profile.static_maps profile)
        in
        if map.tier <> Flow_ir.Cpu_kernel || map.tail_elements <> 0 then
          failwith
            ("gather walked: " ^ String.concat "; " (List.map Diagnostic.to_string map.refusal))
    | Error reference, Error compiled ->
        assert (reference = compiled && reference.code = "E_ARRAY_RANGE");
        let map = List.hd (List.rev (Flow_ir.Profile.static_maps profile)) in
        assert (map.head_elements = 4096 && map.tail_elements > 0)
    | _ -> failwith "gather result differs from walker"
  in
  List.iter
    (fun (bindings, body) -> parity (workspace bindings body))
    [
      ("", "(if (= (array/nth data.values (int/mod i 3)) 1) i 0)");
      ( "lookup (fn [table index] (array/nth table index))",
        "(if (= (lookup data.values (int/mod i 3)) 1) i 0)" );
      ( "lookup (fn [(table : (array int)) (index : int)] (array/nth table index))",
        "(if (= (lookup data.values (int/mod i 3)) 1) i 0)" );
      ("", "(if (< i 0) (array/nth data.values 100) (array/nth data.values (int/mod i 3)))");
      ("", "(array/nth data.values (if (= i 4099) 3 (int/mod i 3)))");
    ];
  let text_workspace =
    workspace ~table:"(array/text 3 \"red\")"
      "lookup (fn [(i : int)] (array/nth data.values (int/mod i 3)))" "i"
  in
  let text_evaluated = ok (E.static ~record:true text_workspace) in
  let fn =
    match List.assoc [ "g"; "lookup" ] text_evaluated.records with
    | [ (_, E.Fn fn) ] -> fn
    | _ -> failwith "missing Text lookup function"
  in
  let value =
    ok
      (E.Host.map_function
         ~signature:Flow.Ty.{ params = [ Int ]; result = Text }
         fn
         [ E.Int_array (Array.init 4101 Fun.id) ])
  in
  (match value with
  | E.Residual residual ->
      let packed =
        Flow_ir.Packed.compile_result residual (E.Residual.residual_view residual).term
        |> Result.map_error (fun ds -> List.hd ds)
        |> ok
      in
      assert (
        Flow_ir.Packed.force ~strict:true packed ~live:(Input.at_time 0.)
        = E.Host.force_reference value ~live:(Input.at_time 0.))
  | _ -> failwith "expected Text gather residual");
  (* An inconsistent concrete annotation is a compiler refusal at the gather, not a checker
     change. Corrupt one checked type to exercise a boundary ordinary source cannot manufacture. *)
  let residual = ref None in
  let checked = workspace "" "(if (= (array/nth data.values (int/mod i 3)) 1) i 0)" in
  ignore
    (ok
       (E.Residual.handle
          ~bulk:(fun r ->
            residual := Some r;
            None)
          (fun () -> E.Host.static_with_requests checked)));
  let residual = Option.get !residual in
  let view = E.Residual.residual_view residual in
  let rec corrupt (term : Flow.Workspace.term) =
    match term.node with
    | Op { op = "array/nth"; _ } -> { term with ty = Flow.Ty.Float }
    | _ -> Flow.Workspace.map_children corrupt term
  in
  let term = corrupt view.term in
  let residual = E.Residual.with_bindings residual ~term view.bindings in
  (match Flow_ir.Packed.compile_result residual term with
  | Error [ diagnostic ] ->
      assert (diagnostic.code = "E_PACKED_TYPE" && diagnostic.span <> None);
      let words = String.split_on_char ' ' diagnostic.message in
      List.iter (fun word -> assert (List.mem word words)) [ "(array/nth"; "float,"; "int." ]
  | _ -> failwith "inconsistent gather type was not refused");
  print_endline
    "Static gathers: reference fields, helpers, Text, lazy/tail errors and concrete mismatch"

let () =
  List.iter
    (fun index ->
      List.iter
        (fun bad ->
          let source =
            Printf.sprintf
              "(let* [table (list 1 2 3)] (map (fn [(i : int)] (nth table (if (= i %d) %s 0))) \
               (array/range 4101 :type int)))"
              index bad
          in
          let workspace = checked source in
          match (E.static workspace, Flow_ir.static workspace) with
          | Error a, Error b -> assert (a = b && a.code = "E_LIST_RANGE")
          | _ -> failwith "list range failure differs from walker")
        [ "-1"; "3"; "0.5" ])
    [ 0; 4100 ];
  let workspace =
    checked
      "(let* [table (list)] (map (fn [(i : int)] (nth table i)) (array/range 4101 :type int)))"
  in
  (match (E.static workspace, Flow_ir.static workspace) with
  | Error a, Error b -> assert (a = b && a.code = "E_LIST_RANGE")
  | _ -> failwith "empty list error differs");
  print_endline
    "Static list guards: negative, range and fractional indices before/after 4,096 keep walker \
     diagnostics"

let () =
  let workspace =
    checked
      "(let* [f (fn [(i : int)] (let* [y (+ i 7)] (* y 2))) a (map f (array/range 4101 :type int)) \
       b (map f (array/range 4101 :type int))] {:a a :b b :last (f 7)})"
  in
  let eager = ok (Flow_ir.static ~record:true ~verify:true workspace) in
  let profile = Flow_ir.Profile.create ~clock:(fun () -> 0.) in
  let replay =
    Flow_ir.Profile.sample profile (fun () ->
        ok (Flow_ir.static ~record:true ~replay:true ~verify:true workspace))
  in
  assert (same (List.assoc "g" eager.results) (List.assoc "g" replay.results));
  List.iter
    (fun (map : Flow_ir.Profile.static_map) ->
      assert (map.tier = Flow_ir.Cpu_kernel && map.head_elements = 0 && map.tail_elements = 0))
    (Flow_ir.Profile.static_maps profile);
  let eager_probe = Flow_graph.Probe.make eager and replay_probe = Flow_graph.Probe.make replay in
  let path =
    List.find_map
      (fun (path, _) -> if List.hd (List.rev path) = "y" then Some path else None)
      eager.records
    |> Option.get
  in
  List.iter
    (fun k ->
      let a = Flow_graph.Probe.at eager_probe path ~probes:[ k ] in
      let b = Flow_graph.Probe.at replay_probe path ~probes:[ k ] in
      if a <> b || Option.is_none b then
        failwith
          (Printf.sprintf "replay probe differs or is absent at call %d (%s)" k
             (String.concat "/" path)))
    [ 0; 4095; 4096; 4100; 4101; 8196; 8201 ];
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          let compiled = ok (Flow_ir.static ~replay:true ~verify:true workspace) in
          assert (same (List.assoc "g" eager.results) (List.assoc "g" compiled.results))))
    [ 1; 8 ];
  print_endline
    "Trusted static maps: full bit verification, replayed body probes and consecutive call indices"

let () =
  List.iter
    (fun body ->
      let workspace = checked body in
      let expected = E.static ~record:true workspace in
      if Result.is_ok expected then failwith ("expected replay failure for " ^ body);
      assert (Flow_ir.static ~record:true ~replay:true workspace = expected))
    [
      "(map (fn [(i : int)] (exp (if (= i 4100) 1000. 0.))) (array/range 4101 :type int))";
      "(let* [table (list 1 2 3)] (map (fn [(i : int)] (nth table (if (= i 4100) 0.5 0.))) \
       (array/range 4101 :type int)))";
    ];
  let forms =
    ok
      (Flow.Syntax.parse
         "(workspace w (defn f :context host [(i : int)] (+ i 1)) (graph g :context host (map f \
          (array/range 4101 :type int))))")
  in
  let workspace =
    match Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } forms with
    | Some workspace, [] -> workspace
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let profile = Flow_ir.Profile.create ~clock:(fun () -> 0.) in
  let replay =
    Flow_ir.Profile.sample profile (fun () ->
        ok (Flow_ir.static ~record:true ~replay:true ~verify:true workspace))
  in
  let eager = ok (Flow_ir.static ~record:true workspace) in
  assert (same (List.assoc "g" eager.results) (List.assoc "g" replay.results));
  List.iter2
    (fun (path, values) (other, replayed) ->
      if path <> other || List.length values <> List.length replayed then
        failwith
          (Printf.sprintf "named recording differs: %s (%d), %s (%d)" (String.concat "/" path)
             (List.length values) (String.concat "/" other) (List.length replayed));
      List.iter2
        (fun (i, a) (j, b) ->
          if i <> j || not (same a b) then
            failwith
              ("named record differs at " ^ String.concat "/" path ^ ": " ^ E.show a ^ " / "
             ^ E.show b))
        values replayed)
    eager.records replay.records;
  List.iter
    (fun (map : Flow_ir.Profile.static_map) -> assert (map.head_elements = 4096))
    (Flow_ir.Profile.static_maps profile);
  print_endline
    "Replay failures retain full diagnostics; named-function recording retains its eager head"

let () =
  let workspace =
    checked
      "(let* [indices (array/range 7 :type int) seed (map (fn [(i : int)] (int/mod (int/add i (int \
       t)) 7)) indices) jumped (exact (iterate 4 seed (fn [(p : (array int))] (exact (map (fn [(x \
       : int)] (array/nth p x)) p))) :until (fn [(a : (array int)) (b : (array int))] (int/eq \
       (array/sum a) (array/sum b)))))] (map (fn [(i : int)] (array/nth jumped i)) indices))"
  in
  let value = List.assoc "g" (ok (E.static workspace)).results in
  let compiled = ok (Flow_ir.Executor.compile ~strict:true value) in
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          List.iter
            (fun time ->
              let live = Input.at_time time in
              assert (
                same
                  (ok (E.force value ~live))
                  (ok (Flow_ir.Executor.force ~strict:true compiled ~live))))
            [ 0.; 1.; 2. ]))
    [ 1; 8 ];
  print_endline
    "Strict captured iterate validates maps and until reductions with their carried array bindings"

let () =
  let ticks = ref 0 in
  let profile =
    Flow_ir.Profile.create ~clock:(fun () ->
        incr ticks;
        if !ticks < 3 then 0. else 1.)
  in
  let workspace = checked "(map (fn [i] (* i 0.5)) (array/range 4100))" in
  ignore (Flow_ir.Profile.sample profile (fun () -> ok (Flow_ir.static workspace)));
  match Flow_ir.Profile.static_maps profile with
  | [ map ] ->
      assert (map.head_elements = 0 && map.tail_elements = 0);
      assert (map.head_seconds = 0. && map.kernel_seconds = 1. && map.tail_seconds = 0.);
      assert (map.seconds = 1.)
  | _ -> failwith "kernel-first profile missing"

let () =
  let check source =
    match
      Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } (ok (Flow.Syntax.parse source))
    with
    | Some checked, [] -> checked
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let checked =
    check
      {|(workspace staged
        (defn build :context value [(n : int)]
          (map (fn [(i : int)] (+ i 1)) (array/range n :type int)))
        (graph volume :context host (build 4101))
        (graph sculpture :context sop
          (let* [mask (map (fn [(i : int)] (if (< i 2000) 1 0)) (array/range 4101 :type int))
                 faces (array/compact mask)
                 count (array/count faces)
                 vertices (array/range (* count 4) :type int)
                 points (map (fn [(i : int)]
                            [(float (array/nth faces (int/div i 4)))
                             (float (int/mod i 4)) 0.]) vertices)
                 tiny (array/slice faces 0 2)
                 pairs (for [a tiny b tiny] [a b 0])
                 copied (map (fn [(i : int)] [i 0 0]) tiny)
                 dropped (array/compact (map (fn [(bit : int)] (- 1 bit)) mask))
                 face_copy (map (fn [(i : int)] [i 0 0]) faces)
                 drop_copy (map (fn [(i : int)] [i 0 0]) dropped)
                 small (build 3)] nil)))|}
  in
  let _, eager = ok (Flow_ir.qualify_workspace ~record:true ~replay:true checked) in
  let profile = Flow_ir.Profile.create ~clock:(fun () -> 0.) in
  let _, deferred =
    ok
      (Flow_ir.Profile.sample profile (fun () ->
           Flow_ir.qualify_workspace ~defer:true ~record:true ~replay:true checked))
  in
  assert (same (List.assoc "volume" eager.results) (List.assoc "volume" deferred.results));
  let recorded evaluation name =
    List.assoc [ "sculpture"; name ] evaluation.E.records |> List.hd |> snd
  in
  List.iter
    (fun name ->
      let value = recorded deferred name in
      (match value with E.Residual _ -> () | _ -> failwith ("not deferred: " ^ name));
      let program = ok (Flow_ir.Executor.compile ~strict:true value) in
      List.iter
        (fun domains ->
          Parallel.run ~domains (fun () ->
              assert (
                same (recorded eager name)
                  (ok (Flow_ir.Executor.force ~strict:true program ~live:(Input.at_time 0.))))))
        [ 1; 8 ])
    [ "mask"; "faces"; "count"; "vertices"; "points" ];
  let points = recorded deferred "points" in
  let program = ok (Flow_ir.Executor.compile ~strict:true points) in
  assert (Flow_ir.Executor.packed_root program <> None);
  let core =
    match points with
    | E.Residual residual -> Result.get_ok (Flow_ir.Core_lower.lower residual)
    | _ -> assert false
  in
  assert (Array.length core.regions >= 2);
  Flow_ir.Core.iter
    (fun (statement : Flow_ir.Core.statement) ->
      match statement.node with
      | Assign (_, Source value) -> assert (not (E.is_live value))
      | _ -> ())
    core.body;
  let production = ok (Flow_ir.Executor.compile points) in
  let prepared = Flow_ir.Core_exec.prepare core in
  let names = [ "mask"; "faces"; "count"; "vertices"; "points" ] in
  let record evaluation = E.Record (List.map (fun name -> (name, recorded evaluation name)) names) in
  let shared = record deferred in
  let shared_core =
    match points with
    | E.Residual root -> Result.get_ok (Flow_ir.Core_lower.lower ~value:shared ~strict:true root)
    | _ -> assert false
  in
  assert (Array.length shared_core.regions = 2);
  let origins =
    let fields = List.map (fun name -> (name, recorded deferred name))
        [ "copied"; "pairs"; "face_copy"; "drop_copy" ] in
    let root = match points with E.Residual root -> root | _ -> assert false in
    let core =
      Result.get_ok (Flow_ir.Core_lower.lower ~value:(E.Record fields) ~strict:true root)
    in
    fun name ->
      Array.find_opt
        (fun (region : Flow_ir.Core.Region.t) -> region.term.path = Some [ "sculpture"; name ])
        core.regions
      |> Option.get |> fun region -> region.origin
  in
  assert (origins "copied" <> None && origins "pairs" = None);
  assert (origins "face_copy" <> None && origins "drop_copy" <> None);
  assert (origins "face_copy" <> origins "drop_copy");
  let shared_profile = Flow_ir.Profile.create ~clock:(fun () -> 0.) in
  let shared_program = ok (Flow_ir.Executor.compile ~profile:shared_profile ~strict:true shared) in
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          assert (
            same (recorded eager "points")
              (ok (Flow_ir.Core_exec.execute ~strict:true prepared ~live:(Input.at_time 0.))));
          assert (
            same (recorded eager "points")
              (ok (Flow_ir.Executor.force production ~live:(Input.at_time 0.))));
          let runs = ref 0 in
          let actual =
            ok
              (Flow_ir.Core_exec.execute
                 ~force:(fun packed ->
                   incr runs;
                   Flow_ir.Packed.force ~strict:true packed ~live:(Input.at_time 0.))
                 (Flow_ir.Core_exec.prepare shared_core) ~live:(Input.at_time 0.))
          in
          assert (!runs = 2 && same (record eager) actual);
          assert (
            same (record eager)
              (ok (Flow_ir.Executor.force ~strict:true shared_program ~live:(Input.at_time 0.))));
          assert (
            List.length
              (List.filter
                 (fun (event : Flow_ir.execution) -> event.tier = Cpu_kernel)
                 (Flow_ir.Profile.executions shared_profile))
            = 2);
          let outputs = Array.init 3 (fun _ -> Some (Array.make 8000 nan)) in
          assert (
            ok
              (Flow_ir.Executor.force_into ~strict:true
                 ~column_source:(fun _ _ -> Ok None) ~count:8000 ~outputs:[| outputs |]
                 program ~live:(Input.at_time 0.))
            = Some ());
          let interleaved =
            E.Vec3_array
              (Array.init 24000 (fun i -> (Option.get outputs.(i mod 3)).(i / 3)))
          in
          assert (same (recorded eager "points") interleaved)))
    [ 1; 8 ];
  assert (same (recorded eager "small") (recorded deferred "small"));
  let maps = Flow_ir.Profile.static_maps profile in
  assert (
    List.exists
      (fun (map : Flow_ir.Profile.static_map) ->
        map.deferred && map.head_elements = 0 && map.tail_elements = 0)
      maps);
  assert (
    List.exists
      (fun (map : Flow_ir.Profile.static_map) ->
        map.count = 4101
        && (match map.site with "volume" :: _ -> true | _ -> false)
        && not map.deferred)
      maps);
  let bad =
    check
      {|(workspace bad
        (graph sculpture :context sop
          (let* [bad_data (map (fn [(i : int)]
                         (array/nth (array/int 1 17) (if (< i 4096) 0 1)))
                       (array/range 4101 :type int))] nil)))|}
  in
  let expected =
    match Flow_ir.qualify_workspace bad with Error d -> d | Ok _ -> failwith "eager error missing"
  in
  let _, deferred = ok (Flow_ir.qualify_workspace ~defer:true ~record:true ~replay:true bad) in
  let value = recorded deferred "bad_data" in
  let program = ok (Flow_ir.Executor.compile ~strict:true value) in
  (* Production force restores the authored error span by rerunning a failed
     compiled leaf. Qualification above still requires a fully compiled form. *)
  (match Flow_ir.Executor.force program ~live:(Input.at_time 0.) with
  | Error actual ->
      if actual <> expected then
        failwith
          ("deferred diagnostic expected " ^ Diagnostic.to_string expected ^ " but got "
         ^ Diagnostic.to_string actual)
  | Ok _ -> failwith "deferred late error missing");
  let record = E.Record [ ("before", E.Int 17); ("failed", value) ] in
  let program = ok (Flow_ir.Executor.compile ~strict:true record) in
  assert (Flow_ir.Executor.force program ~live:(Input.at_time 0.) = Error expected);
  print_endline
    "SOP deferral: host/small maps eager, compiled arrays/counts exact, late diagnostics preserved"
