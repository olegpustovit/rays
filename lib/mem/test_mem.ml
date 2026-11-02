let fail message =
  prerr_endline ("test_mem: " ^ message);
  exit 1

let check condition message = if not condition then fail message

let () =
  Mem.reset ();
  let meshes = Mem.cache "renderer.meshes" in
  let history = Mem.document "editor.history" in
  check (Mem.bytes meshes = 0 && Mem.peak meshes = 0) "fresh owner is empty";
  Mem.charge meshes 1_000_000;
  Mem.charge meshes 500_000;
  check (Mem.bytes meshes = 1_500_000 && Mem.peak meshes = 1_500_000) "charge accumulates";
  Mem.release meshes 400_000;
  check (Mem.bytes meshes = 1_100_000 && Mem.peak meshes = 1_500_000) "release keeps peak";
  Mem.release meshes 9_000_000;
  check (Mem.bytes meshes = 0) "release floors at zero";
  Mem.charge history 12;
  let same = Mem.cache "renderer.meshes" in
  check (same == meshes) "same name reuses the account";
  check (Mem.name meshes = "renderer.meshes" && Mem.kind history = Mem.Document) "name and kind";
  let report = Mem.report ~device:64 ~budget:1_000_000_000 () in
  check (report.attributed = 12) "report attributed";
  check (List.length report.owners = 2) "report lists charged owners";
  check (report.device = 64 && report.budget = 1_000_000_000) "report carries device and budget";
  check (report.ocaml_live > 0 && report.footprint > 0) "live and footprint are positive";
  (match Mem.phys_footprint () with
  | Some n -> check (n > 0) "phys_footprint on macOS"
  | None -> ());
  (match Mem.instructions () with Some n -> check (n > 0L) "instructions" | None -> ());
  Mem.reset ();
  check (Mem.bytes meshes = 0 && (Mem.report ()).attributed = 0) "reset clears accounts";
  Mem.set_budget 1_000;
  let owner = Mem.cache "test.evict" in
  let bag = ref [ 400; 400; 400 ] in
  Mem.register_evictor owner (fun () ->
      match !bag with
      | [] -> false
      | n :: rest ->
          bag := rest;
          Mem.release owner n;
          true);
  Mem.charge owner 1_200;
  let usage () = Mem.attributed () in
  check (Mem.retreat ~usage () = Mem.Within && Mem.bytes owner = 800 && !bag = [ 400; 400 ])
    "retreat drops until under budget";
  Mem.set_budget 100;
  bag := [];
  (match Mem.retreat ~usage () with
  | Over { limit = 100; top = [ ("test.evict", 800) ]; used = 800 } -> ()
  | _ -> fail "retreat is Over, naming the owner, when nothing is left to drop");
  check (Mem.overdraft () <> None) "the overdraft is latched";
  Mem.set_budget 5_000;
  check (Mem.retreat ~usage () = Mem.Within && Mem.overdraft () = None) "recovery clears it";
  Mem.reset ();
  (* the order of retreat: coldest and largest per rebuild cost, then plans, then undo, then Over *)
  let order = ref [] in
  let table name ~size ~rebuild ~last_use =
    let owner = Mem.cache name and left = ref 1 in
    Mem.charge owner size;
    Mem.register_evictor owner ~rebuild ~last_use:(fun () -> last_use) (fun () ->
        if !left = 0 then false
        else begin
          decr left;
          order := name :: !order;
          Mem.release owner size;
          true
        end)
  in
  for _ = 1 to 100 do
    ignore (Mem.retreat ())
  done;
  let now = Mem.epoch () in
  table "warm_big" ~size:1000 ~rebuild:1. ~last_use:now;
  table "cold_small" ~size:200 ~rebuild:1. ~last_use:(now - 10);
  table "costly" ~size:1000 ~rebuild:100. ~last_use:now;
  let step name = fun () ->
    if List.mem name !order then false
    else begin
      order := name :: !order;
      true
    end
  in
  Mem.set_budget 1;
  (match
     Mem.retreat
       ~usage:(fun () -> 2300)
       ~later:[ step "plans"; step "undo" ] ()
   with
  | Over _ -> ()
  | Within -> fail "nothing but evictors freed bytes");
  let got = String.concat " " (List.rev !order) in
  check (got = "cold_small warm_big costly plans undo") ("retreat order: " ^ got);
  Mem.reset ();
  (* a cache that cannot drop now does not end the retreat: the next one goes *)
  Mem.set_budget 1;
  let busy = Mem.cache "busy" and idle = Mem.cache "idle" in
  Mem.charge busy 900;
  Mem.charge idle 100;
  let tried = ref 0 in
  Mem.register_evictor busy (fun () ->
      incr tried;
      false);
  Mem.register_evictor idle (fun () ->
      Mem.release idle 100;
      true);
  ignore (Mem.retreat ~usage:(fun () -> 2) ());
  check (!tried = 1 && Mem.bytes idle = 0 && Mem.bytes busy = 900) "a busy cache is skipped once";
  Mem.reset ();
  (* an evictor whose table died leaves, and takes its bytes with it *)
  let gone = Mem.cache "gone" in
  Mem.charge gone 77;
  Mem.register_evictor gone ~live:(fun () -> false) ~forget:(fun () -> Mem.release gone 77) (fun () ->
      true);
  ignore (Mem.report ());
  check (Mem.evictor_count () = 0 && Mem.bytes gone = 0) "report prunes dead evictors";
  (* budget ranks and the process boundary *)
  Mem.reset ();
  Mem.set_budget ~source:Machine 100;
  Mem.set_budget ~source:Preference 200;
  Mem.set_budget ~source:Machine 300;
  check (Mem.budget () = 200) "a machine default never replaces a preference";
  (match Mem.configure (function "RAYS_MEM_BUDGET" -> Some "4G" | _ -> None) with
  | Ok () -> check (Mem.budget () = 4_000_000_000 && not (Mem.clobber ())) "configure reads a size"
  | Error m -> fail m);
  Mem.set_budget ~source:Preference 1;
  check (Mem.budget () = 4_000_000_000) "the environment beats a preference";
  (match Mem.configure (function "RAYS_MEM_BUDGET" -> Some "lots" | _ -> None) with
  | Error _ -> ()
  | Ok () -> fail "a malformed RAYS_MEM_BUDGET is an error");
  check (Mem.configure (function "RAYS_MEM_CLOBBER" -> Some "1" | _ -> None) = Ok () && Mem.clobber ())
    "RAYS_MEM_CLOBBER=1 is read";
  ignore (Mem.configure (fun _ -> None));
  (* the machine default *)
  (match Mem.physical_ram () with
  | 0 -> ()
  | ram ->
      check (Mem.machine_budget ~working_set:0 = ram * 6 / 10) "60% of RAM";
      check (Mem.machine_budget ~working_set:1000 = 1000) "the smaller of the two");
  (* pressure: lowers the limit below the footprint, runs the same steps; critical alone collects *)
  Mem.reset ();
  check (Mem.phys_footprint () = None || Mem.pressure_start ()) "the OS pressure source starts";
  let cached = Mem.cache "pressure.cache" in
  let big = 2_000_000_000 in
  Mem.charge cached big;
  Mem.register_evictor cached (fun () ->
      if Mem.bytes cached = 0 then false
      else begin
        Mem.release cached big;
        true
      end);
  check (Mem.budget () = 0) "no budget set";
  ignore (Mem.retreat ());
  check (Mem.bytes cached = big) "no pressure, no budget: nothing goes";
  Mem.Private.pressure_inject 1;
  let cycles = Mem.Private.major_cycles () in
  ignore (Mem.retreat ());
  check (Mem.bytes cached = 0) "warning runs the retreat";
  check (Mem.Private.major_cycles () = cycles) "warning does not collect";
  Mem.Private.pressure_inject 0;
  ignore (Mem.retreat ());
  Mem.charge cached big;
  ignore (Mem.retreat ());
  check (Mem.bytes cached = big) "normal restores the budget";
  (* two cycles: what the warning dropped is no longer credited against the footprint *)
  Gc.full_major ();
  Gc.full_major ();
  let cycles = Mem.Private.major_cycles () in
  Mem.Private.pressure_inject 2;
  ignore (Mem.retreat ());
  check (Mem.bytes cached = 0) "critical runs the retreat";
  check (Mem.Private.major_cycles () > cycles) "critical collects";
  Mem.Private.pressure_inject 0;
  ignore (Mem.retreat ());
  Mem.reset ();
  (* a cook's admission: its own shed first, then an overdraft naming the owners *)
  let session = Mem.cache "cook.session" in
  Mem.charge session 123;
  check (Mem.admit () = Ok ()) "unlimited admits";
  Mem.set_budget 1;
  let shed_calls = ref 0 in
  (match
     Mem.admit
       ~shed:(fun () ->
         incr shed_calls;
         false)
       ()
   with
  | Error { limit = 1; top = [ ("cook.session", 123) ]; _ } when !shed_calls = 1 -> ()
  | _ -> fail "admit over budget");
  Mem.reset ();
  (* every charge from four domains lands: the counters are atomic *)
  let shared = Mem.cache "shared" in
  let run () =
    for _ = 1 to 200_000 do
      Mem.charge shared 3;
      Mem.release shared 1
    done
  in
  List.iter Domain.join (List.init 4 (fun _ -> Domain.spawn run));
  check (Mem.bytes shared = 4 * 200_000 * 2) "charges race";
  (* owners and evictors made from several domains *)
  let spawn _ =
    Domain.spawn (fun () ->
        for i = 1 to 2_000 do
          let owner = Mem.cache (Printf.sprintf "shared.%d" (i mod 7)) in
          Mem.charge owner 1;
          if i mod 100 = 0 then Mem.register_evictor owner (fun () -> false);
          ignore (Mem.attributed ())
        done)
  in
  List.iter Domain.join (List.init 4 spawn);
  check (Mem.bytes (Mem.cache "shared.0") = 4 * (2_000 / 7)) "owner table races";
  Mem.reset ();
  print_endline "test_mem: ok"
