module E = Flow.Eval
module I = Flow_ir
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let check ?(extra="") body =
  let forms = ok (Flow.Syntax.parse ("(workspace w " ^ extra ^ " (graph g :context value " ^ body ^ "))")) in
  match Flow.Workspace.check {Flow.Catalog.version=1;kinds=[]} forms with
  | Some ws,[] -> ws | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let bits x y = Int64.bits_of_float x = Int64.bits_of_float y
let rec same_bits (a : E.value) (b : E.value) = match a,b with
  | Float x,Float y -> bits x y
  | List xs,List ys -> Array.length xs=Array.length ys && Array.for_all2 same_bits xs ys
  | Record fs,Record gs -> List.length fs=List.length gs && List.for_all2 (fun (n,a) (m,b) -> n=m && same_bits a b) fs gs
  | a,b -> a=b
let same a b = match a,b with
  | Ok a,Ok b -> same_bits a b
  | Error (a : Diagnostic.t),Error (b : Diagnostic.t) -> a.code=b.code && a.span=b.span | _ -> false
let cases = Core_fixtures.state_cases
(* A [ref] re-runs the referenced graph in its own instance: the state it reads is shared. *)
let refs = Core_fixtures.state_refs
let live frame =
  {(Input.at_time (float_of_int frame *. 0.25)) with frame; tick=frame;
   events=if frame mod 2=1 then [Key_pressed (Input.KeyChar 'a'); Key_pressed (Input.KeyChar 'b')] else []}
let () =
  let used=ref 0 and refused=ref [] and failed=ref [] in
  List.iter (fun (name,extra,body) ->
    let evaluation=ok (E.static (check ~extra ("(let* [v " ^ body ^ "] 0.)"))) in
    let values=List.map snd evaluation.results @ evaluation.states in
    let reference=E.create_state () and core=E.create_state () in
    let programs = List.map (fun value -> value, I.Executor.compile value) values in
    List.iter (function _, Ok _ -> incr used | _, Error (d : Diagnostic.t) ->
      refused := (name ^ ": " ^ d.code ^ " " ^ d.message) :: !refused) programs;
    for frame=0 to 7 do
      let live=live frame in
      List.iter (fun (v, program) ->
        let core_before=E.state_stamp core and reference_before=E.state_stamp reference in
        let expected=E.Private.force_reference ~state:reference v ~live in
        let actual=match program with
          | Ok program -> I.Executor.force ~state:core program ~live
          | Error _ -> expected in
        if not (same actual expected) then failwith (Printf.sprintf "%s frame %d" name frame);
        if Result.is_error expected then begin
          if E.state_stamp core<>core_before || E.state_stamp reference<>reference_before then
            failwith (Printf.sprintf "%s frame %d: failed evaluation changed a state epoch" name frame)
        end else if Marshal.to_string (E.Private.state_values core) [Marshal.No_sharing]
          <> Marshal.to_string (E.Private.state_values reference) [Marshal.No_sharing] then
          failwith (Printf.sprintf "%s frame %d: state values differ" name frame)) programs
    done) (List.map (fun (name,body) -> name,"",body) cases @ refs);
  if !refused<>[] || !failed<>[] then failwith (String.concat "; " (!refused @ !failed));
  assert (!used>0);
  Printf.printf "Core state: slots, pending writes, keys, errors and rollback match the walker (%d executions)\n" !used
