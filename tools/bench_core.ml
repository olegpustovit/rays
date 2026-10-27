(* Scalar rows by default; `kernels` times packed kernels, `attributes` fused against separate mark attributes (the core-region switch is gone: compare against commit 035c1506^).
   One scalar residual evaluated by the tree walker and the core executor. Median of three samples per row, one domain. *)
module E = Flow.Eval
module I = Flow_ir

let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let catalog = { Flow.Check.version = 0; kinds = [] }
let body = "(+ (* (sin t) 0.5) (* t t))"
let iterations = 200_000
let live = Input.at_time 0.37

let check source = match Flow.Workspace.check catalog (ok (Flow.Syntax.parse source)) with
  | Some ws, ds when not (List.exists (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity) ds) -> ws
  | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))

let median values =
  let sorted = Array.copy values in
  Array.sort Float.compare sorted;
  sorted.(Array.length sorted / 2)

let measure force =
  let sample () =
    Gc.full_major ();
    let bytes = Gc.allocated_bytes () and start = Unix.gettimeofday ()
    and cpu = (Unix.times ()).Unix.tms_utime in
    for _ = 1 to iterations do ignore (force ()) done;
    (Unix.gettimeofday () -. start) /. float iterations,
    ((Unix.times ()).Unix.tms_utime -. cpu) /. float iterations,
    (Gc.allocated_bytes () -. bytes) /. float iterations in
  let samples = Array.init 3 (fun _ -> sample ()) in
  median (Array.map (fun (wall, _, _) -> wall) samples),
  median (Array.map (fun (_, cpu, _) -> cpu) samples),
  median (Array.map (fun (_, _, bytes) -> bytes) samples)

let scalar () =
  let value = List.assoc "g" (ok (E.static (check ("(workspace w (graph g :context value " ^ body ^ "))")))).results in
  let program = ok (I.Executor.compile value) in
  let residual = match value with E.Residual residual -> residual | _ -> failwith "scalar did not defer" in
  let core = match I.Core_lower.lower residual with
    | Ok core -> I.Core_exec.prepare core
    | Error reason -> failwith reason in
  let fast = Option.get (I.Core_exec.fast core) in
  let reference = E.Private.force_reference value ~live in
  let rows = List.map (fun (label, force) ->
    (match force (), reference with
     | Ok (E.Float a), Ok (E.Float b) when Int64.bits_of_float a = Int64.bits_of_float b -> ()
     | _ -> failwith (label ^ ": result differs from the reference walker"));
    let wall, cpu, bytes = measure force in
    label, wall, cpu, bytes) [
      "tree walker", (fun () -> E.Private.force_reference value ~live);
      "core exec", (fun () -> I.Core_exec.execute core ~live);
      "core fast", (fun () -> fast live);
      "core", (fun () -> I.Executor.force program ~live) ] in
  Printf.printf "Scalar residual %s; %d evaluations per sample; domains 1; median of 3 samples\n"
    body iterations;
  List.iter (fun (label, wall, cpu, bytes) ->
    Printf.printf "%-12s %12.1f wall ns/eval %12.1f CPU ns/eval %10.1f allocated bytes/eval\n"
      label (wall *. 1e9) (cpu *. 1e9) bytes) rows;
  let seconds label = let _,wall,_,_ = List.find (fun (name,_,_,_) -> name=label) rows in wall in
  Printf.printf "walker/core-exec ratio %.2f; core-fast/dispatcher ratio %.2f\n"
    (seconds "tree walker" /. seconds "core exec") (seconds "core fast" /. seconds "core")

(* Packed kernels: compile cost (microseconds) and force time per call, switch off and on, 1 and 8 domains. *)
let kernel_residual body =
  let ws = Flow.Workspace.check ~ops:[] catalog
      (ok (Flow.Syntax.parse ("(workspace w (graph g :context value (let* [tested " ^ body ^ "] 0.0)))"))) in
  match ws with
  | Some ws, [] ->
      (match List.assoc ["g"; "tested"] (ok (E.static ~record:true ws)).records |> List.hd |> snd with
       | E.Residual r -> r
       | _ -> failwith "kernel did not defer")
  | _ -> failwith "kernel does not check"

let time_median ~repeat f =
  let sample () =
    let start = Unix.gettimeofday () in
    for _ = 1 to repeat do f () done;
    (Unix.gettimeofday () -. start) /. float repeat in
  median (Array.init 3 (fun _ -> sample ()))

let kernels () =
  let map n = Printf.sprintf "(map (fn [x] (+ (* x 2) t)) (array/range %d))" n in
  let cases = [
    "map 1,024", map 1024, 4000;
    "map 1,000,000", map 1_000_000, 5;
    "fused maps 1,000,000", Printf.sprintf "(map (fn [y] (* y 0.5)) %s)" (map 1_000_000), 5;
    "sum 1,000,000", "(sum [x (array/range 1000000)] (* x t))", 5 ] in
  Printf.printf "Packed kernels; median of 3 samples; compile in microseconds, force in milliseconds\n";
  Printf.printf "%-22s %-6s %12s %12s\n" "case" "domains" "compile us" "force ms";
  List.iter (fun (label, body, repeat) ->
    let r = kernel_residual body in
    let term = (E.Residual.residual_view r).term in
    let compile = time_median ~repeat:2000 (fun () -> ignore (I.Packed.compile r term)) in
      let packed = Option.get (I.Packed.compile r term) in
      List.iter (fun domains ->
        let force = Parallel.run ~domains (fun () ->
          time_median ~repeat (fun () -> ignore (ok (I.Packed.force packed ~live)))) in
        Printf.printf "%-22s %-6d %12.2f %12.3f\n%!" label domains
          (compile *. 1e6) (force *. 1e3)) [1; 8]) cases

let scratch () =
  let body op initial passes =
    List.fold_left (fun value _ -> "(" ^ op ^ " " ^ value ^ " x)") initial (List.init passes Fun.id) in
  let measure repeat force =
    let samples = Array.init 3 (fun _ ->
      Gc.full_major ();
      let allocated = Gc.allocated_bytes () and started = Unix.gettimeofday () in
      for _ = 1 to repeat do ignore (force ()) done;
      (Unix.gettimeofday () -. started) /. float repeat,
      (Gc.allocated_bytes () -. allocated) /. float repeat) in
    Array.sort (fun (a, _) (b, _) -> Float.compare a b) samples;
    samples.(1) in
  Printf.printf "Small-map scratch; 47 instructions, median of 3 samples; seconds and allocated bytes per force\n%!";
  List.iter (fun (label, op, type_suffix) ->
    List.iter (fun count ->
      let initial, passes = if type_suffix = "" then "(+ x t)", 44
        else "(int/add x (int t))", 43 in
      let residual = kernel_residual (Printf.sprintf
        "(map (fn [x] %s) (array/range %d%s))" (body op initial passes) count type_suffix) in
      let packed = Option.get (I.Packed.compile residual (E.Residual.residual_view residual).term) in
      let reference = I.Packed.reference packed ~live in
      List.iter (fun domains -> Parallel.run ~domains (fun () ->
        let actual = I.Packed.force ~strict:true packed ~live in
        if Marshal.to_string actual [Marshal.No_sharing] <> Marshal.to_string reference [Marshal.No_sharing] then
          failwith (label ^ ": packed output differs from reference");
        let seconds, bytes = measure 100 (fun () -> I.Packed.force ~strict:true packed ~live) in
        Printf.printf "%s count=%d registers=%d domains=%d seconds=%.9f allocated_bytes=%.0f\n%!"
          label count (Array.length (I.Packed.Private.view packed).code) domains seconds bytes)) [1; 8])
      [1; 11; 65; 1024]) ["float", "+", ""; "int", "int/add", " :type int"]

(* Attribute fusion: one two-output region against the two kernels it replaces. *)
let attributes () =
  let n = 1_000_000 in
  let ws = match Flow.Workspace.check catalog (ok (Flow.Syntax.parse (Printf.sprintf
    "(workspace w (graph g :context draw (let* [p (map (fn [i] [(* i t) (sin (+ i t)) 0]) (array/range %d))
      c (map (fn [i] [(sin t) (/ i %d) (cos (* i t))]) (array/range %d))] (draw/circles p :fill c))))" n n n))) with
    | Some ws, [] -> ws | _ -> failwith "graph does not check" in
  let evaluated = ok (E.static ~record:true ws) in
  let record name = match List.assoc ["g"; name] evaluated.records |> List.hd |> snd with
    | E.Residual r -> r | _ -> failwith "attribute did not defer" in
  let compile r = Option.get (I.Packed.compile r (E.Residual.residual_view r).term) in
  let separate = [compile (record "p"); compile (record "c")] in
  let regions = List.map compile [record "p"; record "c"] in
  let fused = match I.Core_fuse.merge regions with
    | Some fused -> fused | None -> failwith "Core declined equal-count attribute fusion" in
  Printf.printf "Attribute fusion at %d; median of 3 samples, milliseconds per frame\n" n;
  List.iter (fun domains ->
    let time f = Parallel.run ~domains (fun () -> time_median ~repeat:5 f) *. 1e3 in
    let sep = time (fun () -> List.iter (fun p -> ignore (ok (I.Packed.force p ~live))) separate)
    and fus = time (fun () -> ignore (ok (I.Packed.force_all fused ~live))) in
    Printf.printf "domains %d  separate %.3f  fused %.3f  (%.2fx)\n%!" domains sep fus (sep /. fus)) [1; 8]

(* Whole-frame core execution without a window or renderer; unchanged authored workspace. *)
let frames path =
  let source = ok (Flow.Syntax.parse (In_channel.with_open_bin path In_channel.input_all)) in
  let source = List.map (fun (form : Flow.Syntax.t) -> match form.node with
    | List (head :: name :: declarations) ->
        let declarations = List.filter (fun (declaration : Flow.Syntax.t) -> match declaration.node with
          | List (_ :: _ :: {node=Kw "context";_} :: {node=Sym ("editor"|"settings");_} :: _) -> false
          | _ -> true) declarations in
        {form with node=List (head :: name :: declarations)}
    | _ -> form) source in
  let ws = match Flow.Workspace.check ~ops:[] catalog source with
    | Some ws, [] -> ws
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)) in
  let evaluation = ok (E.static ws) in
  let program, root = match I.Core_draw.lower evaluation.plan (snd (List.hd evaluation.results)) with
    | Ok p -> p | Error name -> failwith name in
  if Sys.getenv_opt "RAYS_BENCH_PRINT_CORE" = Some "1" then Format.printf "%a%!" I.Core.pp program;
  let prepared=I.Core_exec.prepare program in
  let sample () =
    let state=E.create_state () and buffers=I.Core_exec.Buffers.create () in
    let step i =
      let live={(Input.at_time (float i /. 60.)) with dt=1. /. 60.; frame=i; tick=i; size=800,600} in
      I.Core_exec.Buffers.clear buffers;
      let drawn=ref None in
      (match root with
       | None -> drawn:=Some (I.Core_exec.draw prepared ~live ~size:live.size ~buffers)
       | Some root ->
           ignore (ok (E.Residual.bind_live ~state root ~live));
           drawn:=Some (I.Core_exec.draw ~root prepared ~live ~size:live.size ~buffers));
      ignore (ok (Option.get !drawn)) in
    step 0;
    Gc.full_major ();
    let bytes=Gc.allocated_bytes () and start=Unix.gettimeofday () in
    for i=1 to 600 do step i done;
    (Unix.gettimeofday () -. start) /. 600., (Gc.allocated_bytes () -. bytes) /. 600. in
  let samples=Array.init 3 (fun _ -> sample ()) in
  Printf.printf "Core frame %s; 600 frames per sample, 800x600, domains 1; median of 3\n%.3f ms/frame %.0f allocated bytes/frame\n%!"
    path (median (Array.map fst samples) *. 1e3) (median (Array.map snd samples))

let measure_loop force =
    let samples=Array.init 3(fun _->
      Gc.full_major();
      let bytes=Gc.allocated_bytes() and started=Sys.time()in
      ignore(ok(force()));Sys.time()-.started,Gc.allocated_bytes()-.bytes)in
    Array.sort(fun(a,_)(b,_)->Float.compare a b)samples;samples.(1)

let loops () =
  List.iter(fun(name,body)->
    let residual=kernel_residual("(map (fn [x] "^body^") (array/range 1024))")in
    let packed=match I.Packed.compile_result residual (E.Residual.residual_view residual).term with
      |Ok packed->packed|Error errors->failwith(String.concat "; "(List.map Diagnostic.to_string errors))in
    let reference()=E.Private.force_reference(E.Residual residual)~live in
    let expected=ok(reference())in
    let run domains force=Parallel.run ~domains(fun()->
      if Marshal.to_string(ok(force()))[Marshal.No_sharing]<>Marshal.to_string expected[Marshal.No_sharing]then
        failwith(name^": loop benchmark differs from reference");
      measure_loop force)in
    let cpu,bytes=run 1 reference in
    Printf.printf "%s walker domains=1 cpu_seconds=%.6f allocated_bytes=%.0f\n%!" name cpu bytes;
    List.iter(fun domains->let cpu,bytes=run domains(fun()->I.Packed.force ~strict:true packed ~live)in
      Printf.printf "%s packed domains=%d cpu_seconds=%.6f allocated_bytes=%.0f\n%!" name domains cpu bytes)[1;8])[
    "scalar","(fold [a t] [i (range 16)] (+ a (* x 0.125)))";
    "record","(let* [s (fold [s {:a t :b x}] [i (range 16)] {:a s.b :b (+ s.a 0.125)})] (+ s.a s.b))";
    "early-exit","(let* [s (iterate 4611686018427387903 {:n 0 :a t}
      (fn [a] {:n (int/add a.n 1) :a (+ a.a (* x 0.125))})
      :until (fn [prev next] (>= next.n 3)))] s.a)"]

let loop_fusion () = Parallel.run ~domains:1(fun()->
  List.iter(fun(name,body)->
    let residual=kernel_residual body in
    let term=(E.Residual.residual_view residual).term in
    let expected=ok(E.Private.force_reference(E.Residual residual)~live)in
    List.iter(fun fusion->
      let packed=Option.get(I.Packed.compile ~fusion residual term)in
      let force()=I.Packed.force ~strict:true packed ~live in
      if Marshal.to_string(ok(force()))[Marshal.No_sharing]<>Marshal.to_string expected[Marshal.No_sharing]then
        failwith(name^": loop fusion differs from reference");
      assert(I.Packed.stage_count packed=(if fusion then 2 else 1));
      let cpu,bytes=measure_loop force in
      Printf.printf "%s fusion=%b rows=4096 domains=1 cpu_seconds=%.6f allocated_bytes=%.0f\n%!"
        name fusion cpu bytes)[false;true])[
    "producer","(map (fn [y] (* y 2.)) (map (fn [x]
      (fold [a t] [i (range 16)] (+ a (* x 0.125)))) (array/range 4096)))";
    "consumer","(map (fn [y] (fold [a y] [i (range 16)] (+ a (* y 0.125))))
      (map (fn [x] (+ x t)) (array/range 4096)))";
    "both","(map (fn [y] (iterate 3 y (fn [a] (+ a 1.))))
      (map (fn [x] (iterate 3 (+ x t) (fn [a] (+ a 1.)))) (array/range 4096)))";
  ])

let () = match Sys.argv with
  | [|_; "frames"; path|] -> frames path
  | [|_; "kernels"|] -> kernels ()
  | [|_; "scratch"|] -> scratch ()
  | [|_; "attributes"|] -> attributes ()
  | [|_; "loops"|] -> loops ()
  | [|_; "loop-fusion"|] -> loop_fusion ()
  | _ -> scalar ()
