(* Single-program design, step A: a Lisp node call evaluates its body in the calling program
   ([Flow.Eval.inline_nodes]), so chained nodes are one program for the region passes. The bytes equal
   the nested lowering's; the region count is printed for the step report. *)
open Rdk_test_support
module L = Flow_sop.Lower
module Edit = Sop.Edit_graph
module Session = Sop.Session

let flow_ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)

let chains =
  [
    ( "set_orient > color_by_height",
      "(sop/color_by_height (sop/set_orient (sop/grid :counts \"Point counts\" :connectivity \
       \"Points\" :columns 1025 :rows 2) :x 0.5))" );
    ( "color_by_height > color_by_height",
      "(sop/color_by_height (sop/color_by_height (sop/grid :counts \"Point counts\" :columns 65 \
       :rows 33)) :low_red 7)" );
  ]

let run inline body =
  Atomic.set Flow.Eval.inline_nodes inline;
  let profiles = ref [] in
  let before = Atomic.get L.Private.nested_lowerings in
  let forms =
    Flow.Syntax.parse (Printf.sprintf "(workspace w (graph g :context sop %s))" body) |> flow_ok
  in
  let l =
    match L.workspace ~on_profile:(fun p -> profiles := p :: !profiles) forms with
    | Ok l -> l
    | Error d -> failwith (Diagnostic.to_string d)
  in
  let graph = List.hd l.graphs in
  let compiled = Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) in
  let session =
    Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
  in
  let context = Sop.Context.create ~domains:1 ~time:0. ~grain:257 () |> get_string_ok in
  let output = Session.cook session ~context (get_string_ok compiled) in
  Session.close session;
  let bytes =
    match output with
    | Ok (o : Session.output) -> geometry_bytes (Result.get_ok (Sop.Payload.geometry o.payload))
    | Error d -> failwith (Diagnostic.error_to_string d)
  in
  let runs =
    List.fold_left
      (fun n p ->
        n
        + List.length
            (List.filter
               (fun (e : Flow_ir.execution) -> e.tier = Cpu_kernel)
               (Flow_ir.Profile.executions p)))
      0 (l.profile :: !profiles)
  in
  (bytes, runs, Atomic.get L.Private.nested_lowerings - before)

let () =
  List.iter
    (fun (name, body) ->
      let nested_bytes, nested_runs, nested = run false body in
      let inline_bytes, inline_runs, inlined = run true body in
      Printf.printf "inline %s: kernel runs %d -> %d, nested lowerings %d -> %d, bytes equal\n" name
        nested_runs inline_runs nested inlined;
      (* the two chained bodies are one program: one kernel run, no nested lowering (the grid,
         which has validators, still lowers its own) *)
      assert (nested_bytes = inline_bytes);
      assert (nested - inlined = 2 && inline_runs = 1))
    chains;
  Atomic.set Flow.Eval.inline_nodes false
