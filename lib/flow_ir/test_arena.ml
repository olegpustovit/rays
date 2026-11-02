(* Memory plan S5: a program's region results die at their last read. *)
module C = Flow_ir.Core
module X = Flow_ir.Core_exec
module P = Flow_ir.Packed

let () =
  let plan = X.arena_buffers in
  (* a chain of one size alternates two buffers; alone in its size, a local has none *)
  let first = [| 0; 1; 2; 3 |] and last = [| 1; 2; 3; 4 |] in
  assert (plan ~first ~last ~size:[| 8; 8; 8; 8 |] = [| 0; 1; 0; 1 |]);
  assert (plan ~first ~last ~size:[| 8; 6; 4; 2 |] = [| -1; -1; -1; -1 |]);
  (* no two overlapping locals share; unplanned locals take nothing *)
  assert (plan ~first ~last ~size:[| 8; -1; 8; 4 |] = [| 0; -1; 0; -1 |]);
  assert (plan ~first:[| 0; 0 |] ~last:[| 5; 5 |] ~size:[| 8; 8 |] = [| -1; -1 |])

(* 20 regions over each other: an array/nth keeps them from fusing into one. *)
let chain ~count ~size =
  let buffer = Buffer.create 256 in
  Printf.bprintf buffer "a0 (map (fn [(i : int)] (+ (float i) t)) (array/range %d :type int))\n"
    (size 0);
  for k = 1 to count - 1 do
    Printf.bprintf buffer
      "a%d (map (fn [(i : int)] (+ (array/nth a%d (int/mod i (array/count a%d))) 1.0)) (array/range %d :type int))\n"
      k (k - 1) (k - 1) (size k)
  done;
  Printf.bprintf buffer "tested (map (fn [(x : float)] (* x 2.0)) a%d)" (count - 1);
  Buffer.contents buffer

let core body =
  let source = "(workspace w (graph g :context value (let* [" ^ body ^ "] 0.)))" in
  let ok = function Ok v -> v | Error d -> failwith (Diagnostic.to_string d) in
  let ws =
    match
      Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } (ok (Flow.Syntax.parse source))
    with
    | Some ws, [] -> ws
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  match
    snd (List.hd (List.assoc [ "g"; "tested" ] (ok (Flow.Eval.static ~record:true ws)).records))
  with
  | Flow.Eval.Residual r ->
      (match Flow_ir.Core_lower.lower ~value:(Flow.Eval.Residual r) ~strict:true ~budget:32768 r with
      | Ok program -> program
      | Error name -> failwith ("refused " ^ name))
  | _ -> failwith "expected a residual"

let live = Input.at_time 0.25
let word = Sys.word_size / 8
let live_bytes () =
  Gc.full_major ();
  (Gc.stat ()).live_words * word

let run prepared =
  let peak = ref 0 in
  let baseline = live_bytes () in
  let result =
    X.execute ~strict:true
      ~force:(fun packed ->
        let result = P.force ~strict:true packed ~live in
        peak := max !peak (live_bytes () - baseline);
        result)
      prepared ~live
  in
  ((match result with Ok v -> v | Error d -> failwith (Diagnostic.error_to_string d)), !peak)

let bits (value : Flow.Eval.value) = Marshal.to_string value [ Marshal.No_sharing ]

let () =
  let count = 20 in
  (* distinct sizes: nothing can be shared, and every array dies as its reader finishes *)
  let distinct k = 2_000 + (((k * 7) mod 20) * 1_000) in
  let program = core (chain ~count ~size:distinct) in
  let invokes = ref 0 in
  C.iter
    (fun (s : C.statement) -> match s.node with Invoke _ -> incr invokes | _ -> ())
    program.body;
  if !invokes < count then failwith (Format.asprintf "the chain fused into %d regions: %a" !invokes C.pp program);
  let bytes k = distinct k * word in
  let all = List.fold_left (fun sum k -> sum + bytes k) 0 (List.init count Fun.id) in
  let neighbours =
    List.fold_left
      (fun best k -> max best (bytes k + bytes (k + 1)))
      0
      (List.init (count - 1) Fun.id)
  in
  let largest = List.fold_left (fun best k -> max best (bytes k)) 0 (List.init count Fun.id) in
  let prepared = X.prepare program in
  let _, peak = run prepared in
  if peak > neighbours + largest then
    failwith
      (Printf.sprintf "core chain peak %d > neighbours %d + largest %d (sum of all %d)" peak
         neighbours largest all);
  Printf.printf "core chain: peak %d KiB, neighbours+one %d KiB, sum %d KiB\n%!" (peak / 1024)
    ((neighbours + largest) / 1024)
    (all / 1024);
  (* one size: the arena reuses, and a dead array read past its last use would be NaN *)
  let same _ = 30_000 in
  let program = core (chain ~count ~size:same) in
  let plain = X.prepare program and clobbered = X.prepare ~clobber:true program in
  let result prepared = bits (Result.get_ok (X.execute ~strict:true ~force:(P.force ~strict:true ~live) prepared ~live)) in
  let reference = result plain in
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          for _ = 1 to 3 do
            if result plain <> reference then failwith "arena changed a chain's result";
            if result clobbered <> reference then failwith "a dead array was read"
          done))
    [ 1; 8 ];
  let allocated prepared =
    let before = Gc.allocated_bytes () in
    ignore (X.execute ~strict:true ~force:(P.force ~strict:true ~live) prepared ~live);
    Gc.allocated_bytes () -. before
  in
  (* the third run takes every array from its buffers *)
  let fresh = float (count * 30_000 * word) in
  ignore (allocated plain);
  let spent = allocated plain in
  if spent > 0.5 *. fresh then
    failwith (Printf.sprintf "chain allocated %.0f bytes a run of %.0f fresh" spent fresh);
  Printf.printf "core chain, one size: %.0f KiB a run, %.0f KiB fresh\n%!" (spent /. 1024.)
    (fresh /. 1024.)

(* A long body over few live values takes few slots, so it keeps its 1024 lanes; a guarded one keeps its
   results' own slots; a dead register read in place of a live one would come back NaN. *)
let () =
  let links = 200 in
  let buffer = Buffer.create 4096 in
  Buffer.add_string buffer "tested (map (fn [(i : int)] (let* [x (+ (float i) t) v0 (+ x 1.0)\n";
  for k = 1 to links do
    Printf.bprintf buffer "v%d (+ (* v%d 0.5) (sin x))\n" k (k - 1)
  done;
  Printf.bprintf buffer "] (if (> x 100.0) v%d (- 0.0 v%d)))) (array/range 4096 :type int))" links links;
  let program = core (Buffer.contents buffer) in
  let region = program.regions.(0) in
  let registers = C.Region.registers region in
  let code = Array.length region.code in
  if code < links || registers.slots > code / 10 then
    failwith (Printf.sprintf "%d registers took %d slots" code registers.slots);
  Printf.printf "registers: %d take %d slots, %d lanes a block\n%!" code registers.slots
    (min 1024 (65536 / registers.slots));
  let result prepared = bits (Result.get_ok (X.execute ~strict:true ~force:(P.force ~strict:true ~live) prepared ~live)) in
  let plain = result (X.prepare program) in
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          if result (X.prepare ~clobber:true program) <> plain then
            failwith "a register was read past its last use"))
    [ 1; 8 ]
