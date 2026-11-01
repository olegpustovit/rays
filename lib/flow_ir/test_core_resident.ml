module E = Flow.Eval
module I = Flow_ir
module C = I.Core_exec
open Test_support

let compile body =
  let body = "(if (>= t 0.) " ^ body ^ " " ^ body ^ ")" in
  let source = "(workspace w (graph g :context value (let* [tested " ^ body ^ "] 0.)))" in
  let workspace =
    match
      Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } (ok (Flow.Syntax.parse source))
    with
    | Some workspace, [] -> workspace
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  let value =
    snd (List.hd (List.assoc [ "g"; "tested" ] (ok (E.static ~record:true workspace)).records))
  in
  let residual = match value with E.Residual r -> r | _ -> failwith "expected residual" in
  let program =
    match I.Core_lower.lower ~strict:true residual with
    | Ok program -> program
    | Error name -> failwith ("Core refused " ^ name)
  in
  (value, program, C.prepare ~remember_loops:true program)

(* A device emulator: placeholders have excess capacity and deliberately wrong contents.
   Only the device callbacks may read its actual values without a payload materialisation. *)
let device ?(decline = false) ?(overflow = false) ?(error = false) () =
  let held = ref [] and invokes = ref 0 and reads = ref 0 and closes = ref 0 in
  let overflow = ref overflow in
  let actual value =
    Option.value ~default:value
      (List.find_map (fun (key, v) -> if key == value then Some v else None) !held)
  in
  let keep value =
    let count = Flow.Value.array_length value + 3 in
    let placeholder =
      match value with
      | E.Int_array _ -> E.Int_array (Array.make count (-999))
      | E.Vec3_array _ -> E.Vec3_array (Array.make (count * 3) (-999.))
      | _ -> failwith "unexpected device value"
    in
    held := (placeholder, value) :: !held;
    placeholder
  in
  let finish value =
    let resolved = actual value in
    if resolved != value then incr reads;
    resolved
  in
  let settle () =
    if !overflow then begin
      overflow := false;
      raise C.Resident_overflow
    end;
    if error then raise (Diagnostic.Error (Diagnostic.error ~code:"E_TEST_STATUS" "device status"))
  in
  let resident : C.resident =
    {
      invoke =
        (fun (packed : I.Packed.t) args ~locals:_ ~live ->
          if match packed.term.ty with Flow.Ty.Array _ -> false | _ -> true then None
          else (
            incr invokes;
            if decline && !invokes mod 2 = 0 then None
            else
              let packed = ok (I.Packed.Host.bind_arguments packed (Array.map actual args)) in
              Some (keep (ok (I.Packed.force ~strict:true packed ~live)))));
      compact =
        (fun mask ->
          match actual mask with
          | E.Int_array mask ->
              Some
                (keep
                   (E.Int_array
                      (Array.to_list (Array.init (Array.length mask) Fun.id)
                      |> List.filter (fun k -> mask.(k) <> 0)
                      |> Array.of_list)))
          | _ -> None);
      coerce = (fun _ _ -> None);
      finish;
      count =
        (fun value ->
          if actual value == value then None
          else (
            settle ();
            Some (Flow.Value.array_length (actual value))));
      close =
        (fun () ->
          incr closes;
          held := []);
      settle;
      range = (fun _ _ ~div:_ ~length:_ -> None);
      fill = (fun _ ~length:_ _ -> None);
    }
  in
  let rec materialise = function
    | E.Record fields -> E.Record (List.map (fun (k, v) -> (k, materialise v)) fields)
    | value -> finish value
  in
  (resident, materialise, invokes, reads, closes)

let body ?(cpu = false) () =
  {|(let* [mask (map (fn [(i : int)] (if (= (int/mod i 2) 0) 1 0))
                         (array/range (int t) :type int))
           faces (array/compact mask)
           n (array/count faces)
           points (map (fn [(i : int)] [(float i) 0.0 0.0]) faces)]
      {:points points :indices faces :count n|}
  ^ (if cpu then {| :words (fold [s ""] [i faces] (str s i ",")) :sum (array/sum faces)|} else "")
  ^ "})"

let () =
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          List.iter
            (fun cpu ->
              let value, core, prepared = compile (body ~cpu ()) in
              assert (Array.length core.regions >= 2);
              List.iter
                (fun decline ->
                  List.iter
                    (fun time ->
                      let live = Input.at_time time in
                      let resident, materialise, invokes, reads, closes = device ~decline () in
                      Fun.protect ~finally:resident.close (fun () ->
                          let result = ok (C.execute ~strict:true ~resident prepared ~live) in
                          assert (!invokes >= 2 && !closes = 0);
                          if not (cpu || decline) then assert (!reads = 0);
                          if cpu then assert (!reads > 0);
                          assert (materialise result = ok (E.Host.force_reference value ~live))))
                    [ 17.; 17.; 0.; 257.; 3. ])
                [ false; true ])
            [ false; true ]))
    [ 1; 4 ];
  let value, _, prepared =
    compile
      {|(let* [p (map (fn [i] [i 0.0 0.0]) (array/range (int t)))
             out (fold [b p] [d (range 2)]
               (map (fn [(v : vec3)] [(+ v.x 1.0) v.y v.z]) b))]
        {:before p :after out})|}
  in
  let resident, materialise, _, reads, closes = device () in
  let live = Input.at_time 17. in
  Fun.protect ~finally:resident.close (fun () ->
      let result = ok (C.execute ~strict:true ~resident prepared ~live) in
      assert (!closes = 0 && !reads = 0);
      assert (materialise result = ok (E.Host.force_reference value ~live)));
  let value, _, prepared = compile (body ()) in
  let resident, materialise, _, reads, closes = device ~overflow:true () in
  let live = Input.at_time 17. in
  Fun.protect ~finally:resident.close (fun () ->
      let result = ok (C.execute ~strict:true ~resident prepared ~live) in
      assert (!closes = 1 && !reads = 0);
      assert (materialise result = ok (E.Host.force_reference value ~live)));
  let resident, _, _, _, closes = device ~error:true () in
  (match C.execute ~strict:true ~resident prepared ~live with
  | Error d -> assert (d.code = "E_TEST_STATUS" && !closes = 1)
  | Ok _ -> failwith "device status error was ignored");
  let _, _, prepared = compile "(state [a 0.] (+ a t))" in
  let resident, _, invokes, _, closes = device () in
  (match C.execute ~strict:true ~resident prepared ~live with
  | Error d -> assert (d.code = "E_CORE_RESIDENT" && !invokes = 0 && !closes = 1)
  | Ok _ -> failwith "resident stateful value execution accepted");
  print_endline
    "Resident value execution: counts, escaping records, CPU readers, errors and overflow match"

let () =
  let record, _, _ = compile
      "{:first (map (fn [(i : int)] (int/add i 1)) (array/range (int t) :type int)) :second (map (fn [(i : int)] (int/mul i 2)) (array/range (int t) :type int))}" in
  let root = match record with E.Residual r -> r | _ -> assert false in
  let view = E.Residual.residual_view root in
  let rec fields (term : Flow.Workspace.term) = match term.node with
    | Record fields -> Some fields
    | _ -> List.find_map fields (Flow.Workspace.children term) in
  let fields = Option.get (fields view.term) in
  let value = E.Record (List.map (fun (name, term) ->
      name, E.Residual (E.Residual.with_bindings root ~term view.bindings)) fields) in
  let program = ok (I.Executor.compile value) in
  let live = Input.at_time 5001. in
  let resident, _, invokes, reads, closes = device () in
  let resident = { resident with settle = (fun () ->
          if !invokes >= 2 then
            raise (Diagnostic.Error (Diagnostic.error ~code:"E_TEST_STATUS" "second field failed"))) } in
  Fun.protect ~finally:resident.close (fun () ->
      let actual = ok (I.Executor.force ~strict:true ~resident program ~live) in
      assert (!invokes = 2 && !closes > 0 && !reads = 0);
      assert (actual = ok (E.Host.force_reference value ~live)));
  print_endline "Resident field failure discards earlier placeholders and reruns the whole CPU value"
