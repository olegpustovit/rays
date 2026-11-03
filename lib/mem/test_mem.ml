let fail message =
  prerr_endline ("test_mem: " ^ message);
  exit 1

let check condition message = if not condition then fail message

let () =
  Mem.reset ();
  (* a walked owner is what its roots reach beyond those before it; a charged table is not walked,
     and what it holds is on the ledger already, however a root reaches it *)
  let shared = Array.make 100_000 0 in
  let held = Array.make 50_000 0 in
  let named = Mem.plan "walk.named" and counted = Mem.cache "walk.counted" in
  let charged = Mem.cache "walk.charged" in
  Mem.root named (fun () -> [ Obj.repr (shared, held) ]);
  let own = Array.make 10_000 0 in
  let table = (shared, own) in
  Mem.walk_table counted table ~uncharged:(fun () -> true);
  Mem.walk_table charged held ~uncharged:(fun () -> false);
  Mem.charge charged 400_000;
  ignore (Mem.report ());
  check (abs (Mem.bytes named - 800_000) < 100) "a named root is its reach beyond what a table charges";
  check (abs (Mem.bytes counted - 80_000) < 100) "a table is what it reaches beyond the roots before it";
  check (Mem.bytes charged = 400_000) "a table on the ledger is not walked";
  Mem.root named (fun () -> []);
  ignore (Mem.report ());
  ignore (Sys.opaque_identity (table, held));
  check (abs (Mem.bytes counted - 880_000) < 100) "what a root let go of falls to the next";
  check (Mem.bytes named = 0) "a dropped root leaves the ledger";
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
  (* a touched 100 MB block raises the footprint by about its size: tolerance 10 MB, for the
     allocator's bookkeeping. Windows must report it (the stub answers None there until ported). *)
  (let grew () =
     let before = Mem.phys_footprint () in
     let block = Bytes.make 100_000_000 'x' in
     let after = Mem.phys_footprint () in
     ignore (Sys.opaque_identity block);
     (before, after)
   in
   match grew () with
   | Some a, Some b ->
       check
         (abs (b - a - 100_000_000) < 10_000_000)
         (Printf.sprintf "footprint rose by %d bytes for a touched 100 MB block (tolerance 10 MB)" (b - a))
   | _ when Sys.win32 -> fail "phys_footprint is unavailable on Windows"
   | _ -> ());
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
  (* a device budget sheds the device pool by the same order and leaves the host pool alone *)
  check (not Sys.win32 || Mem.physical_ram () > 0) "Windows reports installed RAM";
  let vram = Mem.cache ~pool:Device "test.vram" and ram = Mem.cache "test.ram" in
  let drops = ref 0 and ram_drops = ref 0 in
  let evictable owner count =
    Mem.register_evictor owner (fun () ->
        Mem.bytes owner > 0
        && begin
             incr count;
             Mem.release owner 400;
             true
           end)
  in
  evictable vram drops;
  evictable ram ram_drops;
  Mem.charge vram 1_200;
  Mem.charge ram 1_200;
  Mem.set_device_budget 1_000;
  let unowned = ref 0 in
  let device () = Mem.bytes vram + !unowned in
  check
    (Mem.retreat ~usage:(fun () -> 0) ~device () = Mem.Within && !drops = 1 && !ram_drops = 0)
    "over the device budget one VRAM entry goes and no host entry";
  Mem.set_device_budget 100;
  unowned := 5_000;
  (match Mem.retreat ~usage:(fun () -> 0) ~device () with
  | Over { limit = 100; _ } when !drops = 3 && !ram_drops = 0 -> ()
  | _ -> fail "device Over names the device limit once every VRAM cache is gone");
  Mem.charge vram 500;
  (match Mem.admit_bytes ~pool:Device 1 with
  | exception Mem.Refused { limit = 100; _ } -> ()
  | () -> fail "a device allocation past the device budget is refused");
  Mem.admit_bytes 1;
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
  Mem.admit_bytes max_int;
  let footprint = Option.value ~default:0 (Mem.phys_footprint ()) in
  Mem.set_budget ~source:Override (footprint + (1 lsl 30));
  Mem.admit_bytes (1 lsl 20);
  check
    (match Mem.admit_bytes (2 lsl 30) with
    | () -> false
    | exception Mem.Refused o -> o.used > o.limit)
    "an allocation that would pass the limit is refused before it is made";
  Mem.set_budget ~source:Override 0;
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
  (* a drop whose bytes something else still holds frees nothing: its credit lets that boundary
     stand, but neither the next boundary nor a cook's admission takes it for freed memory *)
  (match Mem.phys_footprint () with
  | None -> ()
  | Some footprint ->
      let held = Mem.cache "held.elsewhere" and big = 1 lsl 30 in
      let dropped () =
        Mem.reset ();
        Mem.charge held big;
        Mem.register_evictor held (fun () ->
            Mem.bytes held > 0
            && begin
                 Mem.release held big;
                 true
               end);
        Mem.set_budget ~source:Override (footprint / 2);
        Mem.retreat ()
      in
      check (dropped () = Mem.Within) "the boundary that drops stands on its credit";
      check (Mem.retreat () <> Mem.Within) "the next boundary reads what is still there";
      ignore (dropped ());
      check (Result.is_error (Mem.admit ())) "a cook is not admitted on credit";
      ignore (dropped ());
      check
        (match Mem.admit_bytes (1 lsl 20) with
        | () -> false
        | exception Mem.Refused _ -> true)
        "nor is an allocation");
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
