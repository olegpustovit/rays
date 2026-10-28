(* What the graph pane's probes run on: the exact frame/region compiler the canvas uses, and the
   profile of its groups read as the footer's "ran on" line. *)

type reports = Flow_ir.execution list

let reports profile = Flow_ir.Profile.executions profile

let no_reports : reports = []

let same_count (a : reports) (b : reports) = List.compare_lengths a b = 0

(* Prepared programs survive live frames (64, most recent), while values and state remain local to
   each probe snapshot. *)
let force () : Flow_graph.Probe.force =
  let programs = Lru.Identity_list.create ~equal:( == ) 64 in
  fun ?state ?elems ?resolve value ~live ->
    let prepared = match value with
      | Flow.Eval.Residual residual ->
          (match Lru.Identity_list.peek programs residual with
           | Some program -> program
           | None ->
               let program = Flow_ir.Executor.compile value in
               Lru.Identity_list.add programs residual program;
               program)
      | _ -> Flow_ir.Executor.compile value in
    match prepared with
    | Ok program -> Flow_ir.Executor.force ?state ?elems ?resolve program ~live
    | Error _ -> Flow.Eval.Host.force_reference ?state ?elems ?resolve value ~live

let rec covers site path = match site, path with
  | [a], b :: _ -> a = b || String.starts_with ~prefix:(a ^ "#") b
  | a :: rest, b :: tail when a = b -> covers rest tail
  | _ -> false

let rec prefix xs ys = match xs, ys with
  | [], _ -> true | x :: xs, y :: ys -> x = y && prefix xs ys | _ -> false

let find (reports : reports) ~instance path ~probes =
  let found = List.fold_left (fun best (report : Flow_ir.execution) ->
    List.fold_left (fun best (origin, site, iter) ->
      if not ((origin < 0 || instance = Some origin) && covers site path && prefix iter probes) then best else
      let score = List.fold_left (fun n s -> n + String.length s + 1) 0 site in
      match best with Some (previous, _) when previous >= score -> best
        | _ -> Some (score, report)) best report.sites) None reports in
  match found with
  | Some (_, (report : Flow_ir.execution)) -> Some Flow_graph.Probe.{
      tier = (match report.tier with Interp -> "Interp" | Core -> "Core" | Cpu_kernel -> "CPU"
        | Gpu -> "GPU" | Gpu_compile -> "Gpu_compile" | Gpu_readback -> "Readback");
      group = fst report.owner;
      seconds = if fst report.owner = path then Some report.seconds else None }
  | None -> None
