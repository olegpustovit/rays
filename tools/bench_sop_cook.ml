(* SOP lowering and cook latency of a workspace: check, lower and cook every default SOP graph,
   then an edit (the first FROM in the text becomes TO) checked, relowered with the previous ids
   and recooked in the same session. Prints CSV: phase,seconds (median of ROUNDS).
   usage: bench_sop_cook FILE.rays [FROM TO [ROUNDS]] *)
open Sop

let get = function Ok value -> value | Error message -> failwith message

let load text =
  match Rays_editor.Workspace.load text with
  | Ok workspace -> workspace
  | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))

let replace text ~from ~by =
  let n = String.length from in
  let rec find i =
    if i + n > String.length text then failwith ("no " ^ from)
    else if String.sub text i n = from then i
    else find (i + 1)
  in
  let i = find 0 in
  String.sub text 0 i ^ by ^ String.sub text (i + n) (String.length text - i - n)

let time f =
  let start = Unix.gettimeofday () in
  let result = f () in
  (result, Unix.gettimeofday () -. start)

let lower ?previous (workspace : Editor_document.Workspace_doc.t) =
  Flow_sop.Lower.of_checked ~factories:Nodes.factories ~inputs:workspace.inputs
    ~reference:(Sys.getenv_opt "BENCH_REFERENCE" <> None)
    ?compiled_ids:(Option.map (fun (l : Flow_sop.Lower.t) -> l.compiled_ids) previous)
    ?sites:(Option.map (fun (l : Flow_sop.Lower.t) -> l.sites) previous)
    workspace.checked
  |> function
  | Ok lowered -> lowered
  | Error d -> failwith (Diagnostic.to_string d)

let cook session (lowered : Flow_sop.Lower.t) =
  let context = get (Context.create ()) in
  List.iter
    (fun (graph : Flow_sop.Lower.graph) ->
      match graph.root with
      | Some root when graph.default -> (
          let node = get (Edit_graph.compile_node graph.network.geometry ~node_id:root) in
          match Session.cook session ~context node with
          | Ok _ -> ()
          | Error d -> failwith (Diagnostic.error_to_string d))
      | _ -> ())
    lowered.graphs

let median values =
  let values = List.sort Float.compare values in
  List.nth values (List.length values / 2)

let () =
  (* BENCH_NESTED=1: Lisp cards as nodes of their own, the former lowering *)
  if Sys.getenv_opt "BENCH_NESTED" = Some "1" then Atomic.set Flow.Eval.inline_nodes false;
  let path, edit, rounds =
    match List.tl (Array.to_list Sys.argv) with
    | [ path ] -> (path, None, 5)
    | [ path; from; by ] -> (path, Some (from, by), 5)
    | [ path; from; by; rounds ] -> (path, Some (from, by), int_of_string rounds)
    | _ -> failwith "usage: bench_sop_cook FILE.rays [FROM TO [ROUNDS]]"
  in
  let text = In_channel.with_open_bin path In_channel.input_all in
  let edited = Option.fold ~none:text ~some:(fun (from, by) -> replace text ~from ~by) edit in
  let samples = Hashtbl.create 8 in
  let note phase seconds =
    Hashtbl.replace samples phase
      (seconds :: Option.value ~default:[] (Hashtbl.find_opt samples phase))
  in
  for _ = 1 to rounds do
    let session = get (Session.create ~max_entries:4096 ~max_payload_bytes:(1 lsl 30)) in
    let workspace, check = time (fun () -> load text) in
    let lowered, lower_s = time (fun () -> lower workspace) in
    let (), cook_s = time (fun () -> cook session lowered) in
    let ((), phases), edit_s =
      time (fun () ->
          Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
              let workspace = load edited in
              cook session (lower ~previous:lowered workspace)))
    in
    if Sys.getenv_opt "BENCH_PHASES" <> None then prerr_endline (Flow.Phase_timer.summary phases);
    note "check" check;
    note "lower" lower_s;
    note "cook" cook_s;
    note "edit_to_cook" edit_s;
    Session.close session
  done;
  print_endline "phase,seconds";
  List.iter
    (fun phase -> Printf.printf "%s,%.4f\n" phase (median (Hashtbl.find samples phase)))
    [ "check"; "lower"; "cook"; "edit_to_cook" ]
