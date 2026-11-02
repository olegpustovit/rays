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
  print_endline "test_mem: ok"
