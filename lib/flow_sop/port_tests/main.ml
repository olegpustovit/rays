(* Registrations run at link time in no fixed order relative to this module,
   so the work is deferred to at_exit. See port_gate.ml for usage. *)
let nodes_dir () =
  (* Prefer the source tree:
     <root>/_build/default/lib/flow_sop/port_tests/main.exe. *)
  let exe = Filename.dirname Sys.executable_name in
  let source = Filename.concat exe "../../../../../lib/flow_sop/nodes" in
  if Sys.file_exists source then source else Filename.concat exe "../nodes"

let main () =
  let dir = nodes_dir () in
  let executable_digest = Digest.file Sys.executable_name |> Digest.to_hex in
  let args = List.tl (Array.to_list Sys.argv) in
  let bench = List.mem "--bench" args in
  let profile, cache_compare, capture, command_args = match args with
    | [flag; key; fixture; case] when flag = "--profile-golden" || flag = "--profile-reference-golden" ->
        Some (flag = "--profile-reference-golden", key, fixture, case), None, None, [key]
    | flag :: key :: fixture :: cases when flag = "--compare-golden-cache" && cases <> [] ->
        None, Some (key, fixture, cases), None, [key]
    | [flag] when flag = "--check-golden-registry" -> None, None, Some (`Audit, ""), []
    | flag :: path :: rest when flag = "--capture-goldens" -> None, None, Some (`Ports, path), rest
    | flag :: path :: rest when flag = "--capture-supplemental" -> None, None, Some (`Supplemental, path), rest
    | flag :: path :: rest when flag = "--capture-all-goldens" -> None, None, Some (`All, path), rest
    | _ -> None, None, None, args in
  let all = Port_gate.registered () in
  let keys = List.filter (fun a -> a <> "--bench" && a <> "--full") command_args in
  let quick = keys <> [] && not (List.mem "--full" args) in
  let selected = match keys with
    | [] -> List.filter (fun (n : Port_gate.t) -> Sys.file_exists (Filename.concat dir (n.key ^ ".rays"))) all
    | key :: _ -> (match List.filter (fun (n : Port_gate.t) -> n.key = key) all with
        | [] -> Printf.eprintf "FAIL: no port registered for %S (registered: %s)\n" key
                  (String.concat " " (List.map (fun (n : Port_gate.t) -> n.key) all)); exit 2
        | l -> l) in
  let preflight_all_capture () =
    let definitions = match Flow_sop.Catalog.node_definitions () with
      | Ok definitions -> definitions
      | Error diagnostic -> failwith (Diagnostic.to_string diagnostic) in
    let expected = List.map (fun definition ->
      let qualified = definition.Flow.Defnode.kind.qualified in
      let prefix = "sop/" in
      if not (String.starts_with ~prefix qualified) then
        failwith ("non-SOP definition in golden catalog: " ^ qualified);
      String.sub qualified (String.length prefix) (String.length qualified - String.length prefix)) definitions
      |> List.sort_uniq String.compare in
    let generic = List.map (fun (node : Port_gate.t) -> node.key) selected |> List.sort_uniq String.compare in
    let supplemental = List.sort_uniq String.compare Supplemental_goldens.node_keys in
    let overlap = List.filter (fun key -> List.mem key generic) supplemental in
    let actual = List.sort_uniq String.compare (generic @ supplemental) in
    if overlap <> [] || actual <> expected then begin
      let missing = List.filter (fun key -> not (List.mem key actual)) expected in
      let extra = List.filter (fun key -> not (List.mem key expected)) actual in
      failwith (Printf.sprintf "golden capture registry mismatch: expected %d SOP keys, generic %d, supplemental %d, overlap [%s], missing [%s], extra [%s]"
        (List.length expected) (List.length generic) (List.length supplemental)
        (String.concat "," overlap) (String.concat "," missing) (String.concat "," extra))
    end;
    Printf.eprintf "golden capture registry preflight: %d exact SOP keys (%d generic, %d supplemental)\n%!"
      (List.length expected) (List.length generic) (List.length supplemental) in
  let write_rows path rows = Out_channel.with_open_bin path (fun channel ->
    List.iter (fun row -> output_string channel row; output_char channel '\n') rows) in
  let unique_rows rows =
    let seen = Hashtbl.create (List.length rows) in
    List.filter (fun row -> if Hashtbl.mem seen row then false else (Hashtbl.add seen row (); true)) rows in
  let read_rows path =
    In_channel.with_open_bin path (fun channel -> In_channel.input_lines channel) in
  let provenance_path out_dir = Filename.concat out_dir "provenance.sexp" in
  let provenance_sources () = Sys.readdir dir |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name ".rays")
    |> List.sort String.compare
    |> List.map (fun name -> Printf.sprintf "(source %S %S)" name
        (Digest.file (Filename.concat dir name) |> Digest.to_hex)) in
  let provenance_rows status started sources =
    [Printf.sprintf "(capture (status %S) (started_unix %.6f) (executable %S)"
       status started executable_digest;
     "  (lisp_sources";
     "    " ^ String.concat "\n    " sources;
     "  ))"] in
  let write_provenance out_dir status started sources =
    write_rows (provenance_path out_dir) (provenance_rows status started sources) in
  let finish_provenance out_dir started sources =
    if provenance_sources () <> sources then begin
      write_provenance out_dir "source_changed" started sources;
      failwith "Lisp node sources changed during golden capture; rows were retained as provisional"
    end;
    write_provenance out_dir "complete" started sources in
  let append_rows path rows = Out_channel.with_open_gen [Open_wronly; Open_creat; Open_append; Open_binary] 0o644 path
      (fun channel -> List.iter (fun row -> output_string channel row; output_char channel '\n') rows) in
  let append_new_rows path rows =
    let seen = Hashtbl.create (List.length rows) in
    if Sys.file_exists path then List.iter (fun row -> Hashtbl.replace seen row ()) (read_rows path);
    let rows = List.filter (fun row -> if Hashtbl.mem seen row then false else (Hashtbl.add seen row (); true)) rows in
    append_rows path rows in
  let capture_ports out_dir =
       if not (Sys.file_exists out_dir) then Unix.mkdir out_dir 0o755;
       if selected = [] then failwith "no registered Lisp nodes to capture";
       Printf.printf "Capturing %d registered Lisp nodes\n%!" (List.length selected);
       let started = Unix.gettimeofday () in
       let sources = provenance_sources () in
       write_provenance out_dir "running" started sources;
       let ports_path = Filename.concat out_dir "ports.sexp" in
       let coverage_path = Filename.concat out_dir "coverage.sexp" in
       write_rows ports_path [];
       write_rows coverage_path (unique_rows (Port_gate.coverage_rows selected));
       let cache = Port_gate.create_fixture_cache () in
       let row_count = ref 0 in
       List.iter (fun (index, node) ->
         Printf.eprintf "starting %s (%d/%d), %d rows checkpointed\n%!"
           node.Port_gate.key index (List.length selected) !row_count;
         let node_started = Unix.gettimeofday () in
         let rows = Port_gate.capture_goldens ~dir ~out_dir ~cache node in
         let rows = unique_rows rows in
         append_rows ports_path rows;
         row_count := !row_count + List.length rows;
         Printf.eprintf "captured %s (%d/%d): %d rows in %.2fs\n%!"
           node.Port_gate.key index (List.length selected) (List.length rows)
           (Unix.gettimeofday () -. node_started)) (List.mapi (fun i node -> i + 1, node) selected);
       Printf.printf "captured %d Lisp fixture/case rows for %d registered nodes to %s in %.2fs\n%!"
         !row_count (List.length selected) out_dir (Unix.gettimeofday () -. started);
       finish_provenance out_dir started sources in
  let capture_supplemental out_dir ~append =
    if not (Sys.file_exists out_dir) then Unix.mkdir out_dir 0o755;
    let started = Unix.gettimeofday () in
    let sources = provenance_sources () in
    write_provenance out_dir "running" started sources;
    let ports_path = Filename.concat out_dir "ports.sexp" and coverage_path = Filename.concat out_dir "coverage.sexp" in
    if not append then (write_rows ports_path []; write_rows coverage_path []);
    let count = Supplemental_goldens.capture ~dir ~out_dir
      ~on_coverage:(append_new_rows coverage_path)
      ~on_rows:(fun node rows elapsed ->
        append_new_rows ports_path rows;
        Printf.eprintf "captured supplemental %s: %d rows in %.2fs\n%!"
          node.Port_gate.key (List.length rows) elapsed) in
    Printf.printf "captured %d supplemental Lisp fixture/case rows to %s\n%!" count out_dir;
    finish_provenance out_dir started sources in
  let profile_row (initial_reference, key, fixture, case) =
    let node = List.find_opt (fun node ->
      List.mem_assoc fixture node.Port_gate.inputs
      && List.mem_assoc case (("defaults", []) :: node.Port_gate.cases)) selected in
    match node with
    | Some node ->
        let geometries = match List.assoc_opt fixture node.Port_gate.inputs with
          | Some inputs -> inputs
          | None -> failwith (Printf.sprintf "unknown fixture %s/%s" key fixture) in
        let cases = ("defaults", []) :: node.Port_gate.cases in
        let changes = match List.assoc_opt case cases with
          | Some changes -> changes
          | None -> failwith (Printf.sprintf "unknown case %s/%s" key case) in
        let ctx = Port_gate.context ~label:fixture ~dir node geometries in
        let phases = Hashtbl.create 8 in
        let phase name elapsed = Hashtbl.replace phases name elapsed in
        let before = Gc.quick_stat () and started = Unix.gettimeofday () in
        let outcome = match Port_gate.lisp_compiled ~phase ~initial_reference ctx changes with
          | Error message -> Printf.printf "outcome=refused %s\n%!" message
          | Ok ((compiled, _), _build) ->
              let cook_started = Unix.gettimeofday () in
              let cooked = Port_gate.with_capture_session (fun session -> Port_gate.cook session 4 97 compiled) in
              phase "cook" (Unix.gettimeofday () -. cook_started);
              (match cooked with
               | Error error -> Printf.printf "outcome=error %s: %s\n%!" error.Diagnostic.code error.message
               | Ok geometry ->
                   let digest_started = Unix.gettimeofday () in
                   let digest = Digest.to_hex (Digest.string (Rdk_test_support.geometry_bytes geometry)) in
                   phase "geometry_bytes_digest" (Unix.gettimeofday () -. digest_started);
                   Printf.printf "outcome=geometry p%d/v%d/f%d digest=%s\n%!"
                     (Rdk.Geometry.point_count geometry) (Rdk.Geometry.vertex_count geometry)
                     (Rdk.Geometry.primitive_count geometry) digest) in
        ignore outcome;
        let after = Gc.quick_stat () in
        Printf.printf "profile node=%s fixture=%s case=%s total=%.4fs parse=%.4fs check=%.4fs lower=%.4fs compile_node=%.4fs cook=%.4fs geometry_bytes_digest=%.4fs major_words=%.0f\n%!"
          key fixture case (Unix.gettimeofday () -. started)
          (Option.value (Hashtbl.find_opt phases "parse") ~default:0.)
          (Option.value (Hashtbl.find_opt phases "check") ~default:0.)
          (Option.value (Hashtbl.find_opt phases "lower") ~default:0.)
          (Option.value (Hashtbl.find_opt phases "compile_node") ~default:0.)
          (Option.value (Hashtbl.find_opt phases "cook") ~default:0.)
          (Option.value (Hashtbl.find_opt phases "geometry_bytes_digest") ~default:0.)
          (after.Gc.major_words -. before.Gc.major_words)
    | None -> failwith (Printf.sprintf "no registered row %s/%s/%s" key fixture case) in
  let compare_cache_rows (key, fixture, case_names) =
    let matching = List.find_opt (fun node -> List.mem_assoc fixture node.Port_gate.inputs
      && List.for_all (fun case -> List.mem_assoc case (("defaults", []) :: node.Port_gate.cases)) case_names) selected in
    let node = match matching with Some node -> node
      | None -> failwith (Printf.sprintf "no registered sequence %s/%s" key fixture) in
    let geometries = List.assoc fixture node.Port_gate.inputs in
    let ctx = Port_gate.context ~label:fixture ~dir node geometries in
    let cases = List.map (fun name -> name, List.assoc name (("defaults", []) :: node.Port_gate.cases)) case_names in
    let run ~clear_each label =
      Flow_sop.Lower.Private.clear_cache ();
      Gc.full_major ();
      let rows = List.map (fun (case, changes) ->
        let before = Gc.quick_stat () and started = Unix.gettimeofday () in
        let produce () = match Port_gate.lisp_compiled ctx changes with
          | Error message -> let code, message = Port_gate.stable_error message in
              Printf.sprintf "error:%s:%s" code message
          | Ok ((compiled, _), _) ->
              (match Port_gate.with_capture_session (fun session -> Port_gate.cook session 4 97 compiled) with
               | Error error -> Printf.sprintf "error:%s:%s" error.Diagnostic.code error.message
               | Ok geometry -> "ok:" ^ Digest.to_hex (Digest.string
                   (Rdk_test_support.geometry_bytes geometry))) in
        let outcome = if clear_each then
            Fun.protect ~finally:Flow_sop.Lower.Private.clear_cache produce
          else produce () in
        Gc.full_major ();
        let after = Gc.quick_stat () in
        Printf.printf "cache-%s node=%s fixture=%s case=%s total=%.4fs major_words=%.0f live_words_after_major=%d outcome=%s\n%!"
          label key fixture case (Unix.gettimeofday () -. started)
          (after.Gc.major_words -. before.Gc.major_words) after.Gc.live_words outcome;
        case, outcome) cases in
      Gc.full_major ();
      let live = (Gc.quick_stat ()).Gc.live_words in
      Printf.printf "cache-%s sequence_live_words_after_major=%d\n%!" label live;
      rows in
    let retained = run ~clear_each:false "retained" in
    let cleared = run ~clear_each:true "cleared" in
    Flow_sop.Lower.Private.clear_cache ();
    if retained <> cleared then failwith "cache clearing changed a golden case outcome or digest" in
  let captured = match profile, cache_compare, capture with
   | Some row, None, None -> profile_row row; true
   | None, Some sequence, None -> compare_cache_rows sequence; true
   | None, None, Some (`Audit, _) -> preflight_all_capture (); true
   | None, None, Some (`Ports, out_dir) -> capture_ports out_dir; true
   | None, None, Some (`Supplemental, out_dir) -> capture_supplemental out_dir ~append:true; true
   | None, None, Some (`All, out_dir) ->
       preflight_all_capture ();
       let started = Unix.gettimeofday () and sources = provenance_sources () in
       capture_ports out_dir;
       capture_supplemental out_dir ~append:true;
       finish_provenance out_dir started sources;
       true
   | None, None, None -> false
   | _ -> failwith "profile and capture modes are exclusive" in
  if not captured then begin
    if bench then print_endline "| key | domains | native s | lisp s | ratio |\n|---|---|---|---|---|";
    let failed = List.filter (fun (n : Port_gate.t) ->
      try if bench then not (Port_gate.run_bench ~dir n) else (Port_gate.run_gate ~quick ~dir n; false)
      with Failure m -> Printf.printf "FAIL %s\n%!" m; true
         | e -> Printf.printf "FAIL %s: exception %s\n%!" n.key (Printexc.to_string e); true) selected in
    if bench && failed <> [] then print_endline "SPEED GATE FAILED";
    if failed <> [] then (Printf.eprintf "port gate failed: %s\n" (String.concat " " (List.map (fun (n : Port_gate.t) -> n.key) failed)); exit 1)
  end

let () = at_exit (fun () -> if Array.length Sys.argv >= 0 then main ())
