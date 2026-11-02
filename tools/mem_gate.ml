(* Memory gate: one process per scenario, one domain, no window. Each runs the real frame through
   Rays_editor.Measure (update, offscreen render, after_present), the runner behind [rays mem], and
   checks live bytes after a full collection at two frames, the footprint, what no owner claims, and
   the route: a scenario that means the device must stay on it. Budgets are in
   specification/memory-budgets.sexp and only go down. A failure names the owners that moved and runs
   the scenario again with Memprof, printing the sites still live. *)

module Doc = Editor_document.Workspace_doc
module E3 = Rays_editor.Editor
module M = Rays_editor.Measure
module F = Flow_graph.Flow_edit

let fail fmt =
  Printf.ksprintf
    (fun m ->
      prerr_endline ("mem_gate: " ^ m);
      exit 1)
    fmt

let ok = function Ok v -> v | Error d -> fail "%s" (Diagnostic.to_string d)
let string_ok = function Ok v -> v | Error m -> fail "%s" m
let read path = In_channel.with_open_bin path In_channel.input_all
let parse_one text = List.hd (ok (Flow.Syntax.parse text))

(* the budgets file is forms the language's own reader reads *)
type budget = {
  name : string;
  frames : int;
  early : int;
  live_flat_pct : float;
  max_live_bytes : int option;
  max_footprint_bytes : int option;
  max_unattributed_pct : float option;
  max_readback_bytes : int option;
  route : [ `Device | `Any ];
  ceiling : int option;  (** RAYS_MEM_BUDGET for this scenario: it passes or fails with E_MEMORY *)
  known_red : bool;  (** fails today; passing fails, so the marker is removed *)
}

let parse_budget (form : Flow.Syntax.t) =
  let fields =
    match form.node with
    | List ({ node = Sym "scenario"; _ } :: { node = Sym name; _ } :: fields) ->
        ( name,
          List.filter_map
            (fun (field : Flow.Syntax.t) ->
              match field.node with
              | List [ { node = Sym key; _ }; { node = Num value | Sym value; _ } ] -> Some (key, value)
              | _ -> None)
            fields )
    | _ -> fail "memory-budgets.sexp: a scenario is (scenario name (field value) ...)"
  in
  let name, fields = fields in
  let int key = Option.map int_of_string (List.assoc_opt key fields) in
  let float key = Option.map float_of_string (List.assoc_opt key fields) in
  {
    name;
    frames = Option.value ~default:120 (int "frames");
    early = Option.value ~default:30 (int "early");
    live_flat_pct = Option.value ~default:1. (float "live_flat_pct");
    max_live_bytes = int "max_live_bytes";
    max_footprint_bytes = int "max_footprint_bytes";
    max_unattributed_pct = float "max_unattributed_pct";
    max_readback_bytes = int "max_readback_bytes";
    route = (if List.assoc_opt "route" fields = Some "device" then `Device else `Any);
    ceiling = int "ceiling";
    known_red = List.mem_assoc "known_red" fields;
  }

let load_budgets path =
  match ok (Flow.Syntax.parse (read path)) with
  | [ { node = List ({ node = Sym "memory_budgets"; _ } :: forms); _ } ] -> List.map parse_budget forms
  | _ -> fail "memory-budgets.sexp: expected (memory_budgets ...)"

let catalog = lazy (ok (Rays_editor.workspace_catalog ()))

let load text =
  match Rays_editor.Workspace.load text with
  | Ok workspace -> workspace
  | Error diagnostics ->
      fail "%s" (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let sketch name =
  let path = Filename.concat "sketches" (name ^ ".rays") in
  let text = read path in
  match
    Result.bind
      (Rays_editor.Source.read_imports ~file:path text)
      (fun imports -> Rays_editor.Workspace.load ~imports text)
  with
  | Ok workspace -> workspace
  | Error diagnostics ->
      fail "%s" (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let set_input form input value workspace =
  ok
    (Doc.edit (Lazy.force catalog) workspace
       (F.Set_input_default { form; input; value = parse_one (string_of_int value) }))

let edit editor op = string_ok (E3.edit editor op)

let sea side = set_input "sea" "side" side (sketch "gpu_sea")

(* a large document: the bindings an edit lowers again *)
let large =
  let bindings =
    String.concat " " (List.init 300 (fun i -> Printf.sprintf "b%d (+ %d.0 1.0)" i i))
  in
  Printf.sprintf
    {|(workspace large
  (graph picture :context draw
    (let* [%s]
      (draw/background "#14161c"))))|}
    bindings

let no_step _ editor = editor

let scenario = function
  | "idle" ->
      ( load {|(workspace idle (graph picture :context draw (draw/background "#14161c")))|},
        no_step )
  | "ws_layout" -> (sketch "ws_layout", no_step)
  | "pxui" -> (sketch "pxui", no_step)
  | "recursive_signals" -> (sketch "recursive_signals", no_step)
  | "sea_128" | "sea_128_200m" -> (sea 128, no_step)
  | "isogrid_small" -> (set_input "volume" "resolution" 12 (sketch "isogrid"), no_step)
  | "image_graph" -> (sketch "flow_image", no_step)
  | "scrubs_100" ->
      ( sea 128,
        fun tick editor ->
          (* one literal, as a drag writes it, on the fast path *)
          edit editor
            (F.Set_input_default
               { form = "sea"; input = "height"; value = parse_one (Printf.sprintf "0.%d" (10 + (tick mod 80))) }) )
  | "edits_200" ->
      ( load large,
        fun tick editor ->
          edit editor
            (F.Add_node
               {
                 scope = [ "picture" ];
                 name = Printf.sprintf "added_%d" tick;
                 expr = parse_one (string_of_int tick);
               }) )
  | name -> fail "unknown scenario %s" name

let growth (a : M.sample) (b : M.sample) =
  if a.live <= 0 then 100. else 100. *. float (b.live - a.live) /. float a.live

(* the owners that moved between the samples, largest first; what none claims is its own line *)
let moved (a : M.sample) (b : M.sample) =
  let before (e : Mem.Report.entry) =
    List.fold_left
      (fun found (o : Mem.Report.entry) -> if o.name = e.name then o.bytes else found)
      0 a.report.owners
  in
  let owners =
    List.filter_map
      (fun (e : Mem.Report.entry) ->
        let d = e.bytes - before e in
        if d = 0 then None else Some (d, e.name))
      b.report.owners
  in
  let unclaimed = b.report.unattributed - a.report.unattributed in
  (unclaimed, "unattributed") :: owners
  |> List.sort (fun (x, _) (y, _) -> compare (abs y) (abs x))
  |> List.filteri (fun i _ -> i < 6)
  |> List.map (fun (d, name) -> Printf.sprintf "%s %+d" name d)
  |> String.concat ", "

let check (budget : budget) (o : M.outcome) =
  let problems = ref [] in
  let note fmt = Printf.ksprintf (fun m -> problems := m :: !problems) fmt in
  let flat = growth o.early o.final in
  if budget.ceiling = None && abs_float flat > budget.live_flat_pct then
    note "live moved %.2f%% from frame %d (%d B) to %d (%d B), limit %.2f%%; moved: %s" flat
      o.early.frame o.early.live o.final.frame o.final.live budget.live_flat_pct
      (moved o.early o.final);
  Option.iter
    (fun max ->
      if o.final.live > max then
        note "live %d B over max_live_bytes %d; top: %s" o.final.live max (moved o.early o.final))
    budget.max_live_bytes;
  Option.iter
    (fun max ->
      if o.final.footprint > max then
        note "footprint %d B over max_footprint_bytes %d" o.final.footprint max)
    budget.max_footprint_bytes;
  let report = o.final.report in
  Option.iter
    (fun max_pct ->
      let explicit = report.ocaml_live + report.device in
      let pct = if explicit <= 0 then 0. else 100. *. float report.unattributed /. float explicit in
      if pct > max_pct then note "unattributed %.1f%% of explicit over %.1f%%" pct max_pct)
    budget.max_unattributed_pct;
  Option.iter
    (fun max ->
      if o.readback_bytes > max then note "readback %d B over %d: the route left the device" o.readback_bytes max)
    budget.max_readback_bytes;
  (match budget.route with
  | `Any -> ()
  | `Device ->
      if o.fallbacks <> [] then
        note "left the device: %s"
          (String.concat "; "
             (List.map (fun (site, why, n) -> Printf.sprintf "%s: %s (%d)" site why n) o.fallbacks)));
  (* over its ceiling a scenario may only fail with the typed error, and never swap *)
  (match budget.ceiling with
  | None ->
      Option.iter (fun e -> note "unexpected %s" e) o.memory_error;
      Option.iter (fun e -> note "cook failed: %s" e) o.cook_error
  | Some ceiling ->
      let typed =
        let says text =
          Option.fold ~none:false
            ~some:(fun e ->
              let word = "E_MEMORY" in
              let n = String.length word in
              Seq.exists
                (fun i -> String.sub e i n = word)
                (Seq.init (max 0 (String.length e - n + 1)) Fun.id))
            text
        in
        says o.memory_error || says o.cook_error
      in
      if o.final.footprint > ceiling && not typed then
        note "footprint %d B over its ceiling %d with no E_MEMORY" o.final.footprint ceiling);
  Printf.printf
    "scenario %s: live %d (frame %d) -> %d (frame %d) %.2f%%, device %d, footprint %d, alloc/frame %.0f, promoted/frame %.0f, readback %d\n%s%!"
    budget.name o.early.live o.early.frame o.final.live o.final.frame flat o.final.device
    o.final.footprint
    (o.final.allocated /. float budget.frames)
    (o.final.promoted /. float budget.frames)
    o.readback_bytes (Mem.Report.to_string report);
  List.rev !problems

let run_one ?(sites = false) (budget : budget) =
  Option.iter (fun ceiling -> Mem.set_budget ceiling) budget.ceiling;
  let workspace, step = scenario budget.name in
  let outcome =
    string_ok (M.run ~step ~sites ~early:(min budget.early budget.frames) ~frames:budget.frames workspace)
  in
  if sites then
    List.iter (fun (bytes, site) -> Printf.printf "live %10d B  %s\n" bytes site) outcome.sites;
  match check budget outcome with
  | [] when budget.known_red ->
      prerr_endline ("scenario " ^ budget.name ^ " passes: remove its known_red");
      exit 3
  | [] -> ()
  | problems when budget.known_red ->
      Printf.printf "scenario %s: known red: %s\n" budget.name (String.concat "; " problems)
  | problems when not sites ->
      prerr_endline (Printf.sprintf "scenario %s: %s" budget.name (String.concat "; " problems));
      exit 3
  | _ -> ()

let self = Sys.executable_name
let child args = Unix.system (Filename.quote_command self args)

let run_child budget =
  match child [ "run"; budget.name ] with
  | WEXITED 0 -> ()
  | WEXITED 3 ->
      print_endline ("mem_gate: " ^ budget.name ^ " failed; the same run with Memprof:");
      ignore (child [ "sites"; budget.name ]);
      fail "scenario %s failed its budget" budget.name
  | WEXITED n -> fail "scenario %s exited %d" budget.name n
  | _ -> fail "scenario %s signalled" budget.name

let () =
  string_ok (Mem.configure Sys.getenv_opt);
  let budgets = load_budgets "specification/memory-budgets.sexp" in
  let find name =
    match List.find_opt (fun b -> b.name = name) budgets with
    | Some budget -> budget
    | None -> fail "no budget for %s" name
  in
  match List.tl (Array.to_list Sys.argv) with
  | [ "run"; name ] -> run_one (find name)
  | [ "sites"; name ] -> run_one ~sites:true (find name)
  | names ->
      List.iter run_child (if names = [] then budgets else List.map find names);
      print_endline "mem_gate: ok"
