(* Memory gate: one process per scenario, window-free, one domain. Samples live bytes after a full
   collection at frames 30 and 120, plus allocated/promoted per frame, device bytes and footprint.
   Budgets live in specification/memory-budgets.sexp and may only go down. *)

module Doc = Editor_document.Workspace_doc
module E3 = Rays_editor.Editor
module F = Flow_graph.Flow_edit

let fail fmt = Printf.ksprintf (fun m -> prerr_endline ("mem_gate: " ^ m); exit 1) fmt

let ok = function Ok v -> v | Error d -> fail "%s" (Diagnostic.to_string d)

let string_ok = function Ok v -> v | Error m -> fail "%s" m

let read path = In_channel.with_open_bin path In_channel.input_all

type sexp = Atom of string | List of sexp list

let parse_sexps text =
  let len = String.length text in
  let i = ref 0 in
  let rec skip () =
    while
      !i < len
      &&
      match text.[!i] with
      | ' ' | '\n' | '\t' | '\r' ->
          incr i;
          true
      | ';' ->
          while !i < len && text.[!i] <> '\n' do
            incr i
          done;
          true
      | _ -> false
    do
      ()
    done
  in
  let rec parse () =
    skip ();
    if !i >= len then fail "unexpected end of sexp";
    match text.[!i] with
    | '(' ->
        incr i;
        let items = ref [] in
        let rec go () =
          skip ();
          if !i >= len then fail "unclosed list";
          if text.[!i] = ')' then (
            incr i;
            List (List.rev !items))
          else begin
            items := parse () :: !items;
            go ()
          end
        in
        go ()
    | '"' ->
        incr i;
        let buf = Buffer.create 16 in
        let rec go () =
          if !i >= len then fail "unclosed string";
          match text.[!i] with
          | '"' ->
              incr i;
              Atom (Buffer.contents buf)
          | '\\' ->
              incr i;
              if !i >= len then fail "bad escape";
              Buffer.add_char buf text.[!i];
              incr i;
              go ()
          | c ->
              Buffer.add_char buf c;
              incr i;
              go ()
        in
        go ()
    | c when c = ')' -> fail "unexpected )"
    | _ ->
        let start = !i in
        while
          !i < len
          &&
          match text.[!i] with ' ' | '\n' | '\t' | '\r' | '(' | ')' | ';' -> false | _ -> true
        do
          incr i
        done;
        Atom (String.sub text start (!i - start))
  in
  let items = ref [] in
  let rec go () =
    skip ();
    if !i >= len then List.rev !items
    else begin
      items := parse () :: !items;
      go ()
    end
  in
  go ()

let catalog () = ok (Rays_editor.workspace_catalog ())

let parse text = List.hd (ok (Flow.Syntax.parse text))

let load_workspace text =
  match Rays_editor.Workspace.load text with
  | Ok ws -> ws
  | Error ds -> fail "%s" (String.concat "\n" (List.map Diagnostic.to_string ds))

let set_input form input value workspace =
  ok (Doc.edit (catalog ()) workspace (F.Set_input_default { form; input; value = parse (string_of_int value) }))

let frame tick : Rays.Frame.t =
  {
    input =
      {
        (Input.at_time (float tick /. 60.)) with
        dt = 1. /. 60.;
        frame = tick;
        tick;
        size = (960, 640);
      };
    pixel_scale = (1., 1.);
    fps = 60.;
    mouse_delta = (0., 0.);
  }

type sample = {
  frame : int;
  live : int;
  device : int;
  footprint : int;
  allocated : float;
  promoted : float;
}

(* The editor already owns the OGPU device; querying another driver fails. Metal's system
   default is the same MTLDevice, so its currentAllocatedSize is the process total. *)
let device_bytes () =
  match Metal.Device.system_default () with
  | Error _ -> 0
  | Ok device -> (
      match Metal.Device.info device with
      | Ok info ->
          let n = Int64.to_int info.Metal.Device.current_allocated_size in
          ignore (Metal.Device.destroy device);
          max 0 n
      | Error _ ->
          ignore (Metal.Device.destroy device);
          0)

let sample ~frame ~allocated0 ~promoted0 =
  Gc.full_major ();
  let live = (Gc.stat ()).live_words * (Sys.word_size / 8) in
  let _, promoted, _ = Gc.counters () in
  let allocated = Gc.allocated_bytes () in
  let footprint = Option.value ~default:live (Mem.phys_footprint ()) in
  {
    frame;
    live;
    device = device_bytes ();
    footprint;
    allocated = allocated -. allocated0;
    promoted = (promoted *. float (Sys.word_size / 8)) -. promoted0;
  }

type budget = {
  name : string;
  frames : int;
  live_flat_pct : float;
  max_live_bytes : int option;
  max_footprint_bytes : int option;
  known_red : string option;
}

let assoc key = function
  | List fields ->
      let rec find = function
        | [] -> None
        | List (Atom k :: rest) :: _ when k = key -> Some rest
        | _ :: xs -> find xs
      in
      find fields
  | Atom _ -> None

let float_field key sexp default =
  match assoc key sexp with Some [ Atom s ] -> float_of_string s | None -> default | Some _ -> fail "bad float %s" key

let parse_budget = function
  | List (Atom "scenario" :: Atom name :: _) as sexp ->
      let known_red = match assoc "known_red" sexp with Some [ Atom s ] -> Some s | _ -> None in
      let max_live =
        match assoc "max_live_bytes" sexp with Some [ Atom s ] -> Some (int_of_string s) | _ -> None
      in
      let max_foot =
        match assoc "max_footprint_bytes" sexp with
        | Some [ Atom s ] -> Some (int_of_string s)
        | _ -> None
      in
      {
        name;
        frames = (match assoc "frames" sexp with Some [ Atom s ] -> int_of_string s | _ -> 120);
        live_flat_pct = float_field "live_flat_pct" sexp 5.;
        max_live_bytes = max_live;
        max_footprint_bytes = max_foot;
        known_red;
      }
  | _ -> fail "bad scenario in memory-budgets.sexp"

let load_budgets path =
  match parse_sexps (read path) with
  | [ List (Atom "memory_budgets" :: rest) ] ->
      List.filter_map
        (function List (Atom "scenario" :: _) as s -> Some (parse_budget s) | _ -> None)
        rest
  | _ -> fail "memory-budgets.sexp: expected (memory_budgets ...)"

let create_editor workspace =
  string_ok
    (E3.create ~ui_text_size:11 ~await:true ~domains:1 ~workspace ~prepare:E3.prepare ~draw:E3.draw
       ())

let run_frames editor frames =
  let allocated0 = Gc.allocated_bytes () in
  let _, promoted0, _ = Gc.counters () in
  let promoted0 = promoted0 *. float (Sys.word_size / 8) in
  let early = ref None and final = ref None in
  let editor = ref editor in
  for tick = 1 to frames do
    editor := E3.update !editor (frame tick);
    ignore (E3.scene !editor (frame tick));
    if tick = 30 then early := Some (sample ~frame:tick ~allocated0 ~promoted0);
    if tick = frames || tick = 120 then final := Some (sample ~frame:tick ~allocated0 ~promoted0)
  done;
  E3.close !editor;
  let early = Option.value ~default:(sample ~frame:0 ~allocated0 ~promoted0) !early in
  let final = Option.get !final in
  (early, final, frames)

let growth_pct (a : sample) (b : sample) =
  if a.live <= 0 then 100. else 100. *. float (b.live - a.live) /. float a.live

let check_budget (budget : budget) early final frames =
  let flat = growth_pct early final in
  let problems = ref [] in
  let note fmt = Printf.ksprintf (fun m -> problems := m :: !problems) fmt in
  if frames >= 120 && abs_float flat > budget.live_flat_pct then
    note "live grew %.2f%% from frame %d (%d B) to %d (%d B); flatness limit %.2f%%" flat early.frame
      early.live final.frame final.live budget.live_flat_pct;
  Option.iter
    (fun max -> if final.live > max then note "live %d B exceeds max_live_bytes %d" final.live max)
    budget.max_live_bytes;
  Option.iter
    (fun max ->
      if final.footprint > max then
        note "footprint %d B exceeds max_footprint_bytes %d" final.footprint max)
    budget.max_footprint_bytes;
  let report = Mem.report ~device:final.device () in
  Printf.printf
    "scenario %s: live %d→%d (%.2f%%) device=%d footprint=%d alloc/frame=%.0f promoted/frame=%.0f \
     unattributed=%d\n\
     %s%!"
    budget.name early.live final.live flat final.device final.footprint
    (final.allocated /. float frames)
    (final.promoted /. float frames) report.unattributed (Mem.Report.to_string report);
  match (!problems, budget.known_red) with
  | [], Some reason ->
      fail "scenario %s is marked known_red (%s) but passed; remove the known_red marker" budget.name
        reason
  | [], None -> ()
  | problems, Some "growth" ->
      Printf.printf "scenario %s: known_red growth (%s)\n%!" budget.name
        (String.concat "; " (List.rev problems))
  | problems, _ -> fail "scenario %s: %s" budget.name (String.concat "; " (List.rev problems))

let repo_file rel =
  let candidates =
    [
      rel;
      Filename.concat (Sys.getcwd ()) rel;
      Filename.concat (Filename.dirname Sys.argv.(0)) (Filename.concat "../../.." rel);
      Filename.concat (Filename.dirname Sys.argv.(0)) (Filename.concat "../../../.." rel);
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> fail "missing %s (cwd %s)" rel (Sys.getcwd ())

let run_idle () =
  load_workspace
    {|
(workspace idle
  (graph picture :context draw
    (draw/background "#14161c")))
|}
  |> create_editor

let run_file name edits =
  let text = read (repo_file (Filename.concat "sketches" (name ^ ".rays"))) in
  let workspace = List.fold_left (fun ws edit -> edit ws) (load_workspace text) edits in
  create_editor workspace

let scenario_editor = function
  | "idle" -> run_idle ()
  | "ws_layout" -> run_file "ws_layout" []
  | "recursive_signals" -> run_file "recursive_signals" []
  | "sea_128" -> run_file "gpu_sea" [ set_input "sea" "side" 128 ]
  | "isogrid_small" -> run_file "isogrid" [ set_input "volume" "resolution" 12 ]
  | "pxui" -> run_file "pxui" []
  | name -> fail "unknown scenario %s" name

let run_one (budget : budget) =
  Mem.reset ();
  let editor = scenario_editor budget.name in
  let early, final, frames = run_frames editor budget.frames in
  check_budget budget early final frames

let self = Sys.executable_name

let run_child budget =
  let cmd = Filename.quote_command self [ "run"; budget.name ] in
  match Unix.system cmd with
  | WEXITED 0 -> ()
  | WEXITED n -> fail "scenario %s exited %d" budget.name n
  | _ -> fail "scenario %s signalled" budget.name

let () =
  match Array.to_list Sys.argv with
  | _ :: "run" :: name :: _ ->
      let budgets = load_budgets (repo_file "specification/memory-budgets.sexp") in
      (match List.find_opt (fun b -> b.name = name) budgets with
      | None -> fail "no budget for %s" name
      | Some budget -> run_one budget)
  | _ :: names ->
      let budgets = load_budgets (repo_file "specification/memory-budgets.sexp") in
      let selected =
        match names with
        | [] | [ "all" ] -> budgets
        | names ->
            List.map
              (fun name ->
                match List.find_opt (fun b -> b.name = name) budgets with
                | Some b -> b
                | None -> fail "unknown scenario %s" name)
              names
      in
      List.iter run_child selected;
      print_endline "mem_gate: ok"
  | [] -> assert false
