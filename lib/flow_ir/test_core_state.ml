module E = Flow.Eval
module I = Flow_ir
open Test_support

let check ?(extra = "") body =
  let forms =
    ok (Flow.Syntax.parse ("(workspace w " ^ extra ^ " (graph g :context value " ^ body ^ "))"))
  in
  match Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } forms with
  | Some ws, [] -> ws
  | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))

let bits x y = Int64.bits_of_float x = Int64.bits_of_float y

let rec same_bits (a : E.value) (b : E.value) =
  match (a, b) with
  | Float x, Float y -> bits x y
  | List xs, List ys -> Array.length xs = Array.length ys && Array.for_all2 same_bits xs ys
  | Record fs, Record gs ->
      List.length fs = List.length gs
      && List.for_all2 (fun (n, a) (m, b) -> n = m && same_bits a b) fs gs
  | a, b -> a = b

let same a b =
  match (a, b) with
  | Ok a, Ok b -> same_bits a b
  | Error (a : Diagnostic.t), Error (b : Diagnostic.t) -> a.code = b.code && a.span = b.span
  | _ -> false

let cases = Core_fixtures.state_cases

(* A [ref] re-runs the referenced graph in its own instance: the state it reads is shared. *)
let refs = Core_fixtures.state_refs

let live frame =
  {
    (Input.at_time (float_of_int frame *. 0.25)) with
    frame;
    tick = frame;
    events =
      (if frame mod 2 = 1 then [ Key_pressed (Input.KeyChar 'a'); Key_pressed (Input.KeyChar 'b') ]
       else []);
  }

let () =
  let used = ref 0 and refused = ref [] and failed = ref [] in
  List.iter
    (fun (name, extra, body) ->
      let evaluation = ok (E.static (check ~extra ("(let* [v " ^ body ^ "] 0.)"))) in
      let values = List.map snd evaluation.results @ evaluation.states in
      let reference = E.create_state () and core = E.create_state () in
      let programs = List.map (fun value -> (value, I.Executor.compile value)) values in
      List.iter
        (function
          | _, Ok _ -> incr used
          | _, Error (d : Diagnostic.t) ->
              refused := (name ^ ": " ^ d.code ^ " " ^ d.message) :: !refused)
        programs;
      for frame = 0 to 7 do
        let live = live frame in
        List.iter
          (fun (v, program) ->
            let core_before = E.state_stamp core and reference_before = E.state_stamp reference in
            let expected = E.Host.force_reference ~state:reference v ~live in
            let actual =
              match program with
              | Ok program -> I.Executor.force ~state:core program ~live
              | Error _ -> expected
            in
            if not (same actual expected) then failwith (Printf.sprintf "%s frame %d" name frame);
            if Result.is_error expected then begin
              if E.state_stamp core <> core_before || E.state_stamp reference <> reference_before
              then
                failwith
                  (Printf.sprintf "%s frame %d: failed evaluation changed a state epoch" name frame)
            end
            else if
              Marshal.to_string (E.Host.state_values core) [ Marshal.No_sharing ]
              <> Marshal.to_string (E.Host.state_values reference) [ Marshal.No_sharing ]
            then failwith (Printf.sprintf "%s frame %d: state values differ" name frame))
          programs
      done)
    (List.map (fun (name, body) -> (name, "", body)) cases @ refs);
  if !refused <> [] || !failed <> [] then failwith (String.concat "; " (!refused @ !failed));
  assert (!used > 0);
  Printf.printf
    "Core state: slots, pending writes, keys, errors and rollback match the walker (%d executions)\n"
    !used

(* Kernels that read state: a result map over a state value, a step map over its previous
   array, and a step map reading a second state. Each runs as a Core region with the state
   lowered before it, bit-identical to the walker in values and state at one and eight domains. *)
let () =
  let kernels =
    [
      ( "result",
        "",
        "(let* [s (state [a 0.] (+ a (frame/dt)))] (array/sum (map (fn [x] (+ x s)) (array/range \
         5000))))" );
      ("previous", "", "(array/sum (state [p (exact (array/range 5000))] (map (fn [x] (+ x t)) p)))");
      ( "two-states",
        "",
        "(let* [a (state [s 0.] (+ s t))] (array/sum (state [p (exact (array/range 5000))] (map \
         (fn [x] (+ x a)) p))))" );
      ( "sum",
        "",
        "(let* [s (state [a 1] (+ a 1))] (array/sum (map (fn [x] (* x s)) (array/range 5000))))" );
    ]
  in
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          List.iter
            (fun (name, extra, body) ->
              let evaluation = ok (E.static (check ~extra body)) in
              let values = List.map snd evaluation.results @ evaluation.states in
              if
                not
                  (List.exists
                     (function
                       | E.Residual r -> (
                           match I.Core_lower.lower r with
                           | Ok core -> Array.length core.I.Core.regions > 0
                           | Error _ -> false)
                       | _ -> false)
                     values)
              then failwith (name ^ ": state kernel did not lower to a Core region");
              let reference = E.create_state () and core = E.create_state () in
              let programs = List.map (fun v -> (v, ok (I.Executor.compile v))) values in
              for frame = 0 to 5 do
                let live = live frame in
                List.iter
                  (fun (v, program) ->
                    let expected = E.Host.force_reference ~state:reference v ~live in
                    if not (same (I.Executor.force ~state:core program ~live) expected) then
                      failwith (Printf.sprintf "%s frame %d domains %d" name frame domains);
                    if
                      Marshal.to_string (E.Host.state_values core) [ Marshal.No_sharing ]
                      <> Marshal.to_string (E.Host.state_values reference) [ Marshal.No_sharing ]
                    then
                      failwith
                        (Printf.sprintf "%s frame %d domains %d: state values differ" name frame
                           domains))
                  programs
              done)
            kernels))
    [ 1; 8 ];
  print_endline
    "Core state kernels: result, previous, two-state and sum regions match the walker at one and \
     eight domains"
