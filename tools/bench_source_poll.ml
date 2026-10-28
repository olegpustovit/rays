(* Opt-in comparison for the 2 Hz idle source check; no window or sleeps. *)
let () =
  let file = Sys.argv.(1) in
  let text = In_channel.with_open_bin file In_channel.input_all in
  let source = ref (Rays_editor.Source.at ~file
    ~digest:(Editor_document.Contexts.sha256 text)) in
  let iterations = 2000 in
  let allocated = Gc.allocated_bytes in
  let measure label f =
    Gc.full_major ();
    let bytes = allocated () and started = Unix.gettimeofday () in
    for i = 1 to iterations do f i done;
    let elapsed = Unix.gettimeofday () -. started and bytes = allocated () -. bytes in
    Printf.printf "%s: %.6f ms/poll, %.0f bytes/poll\n%!" label
      (elapsed *. 1000. /. float iterations) (bytes /. float iterations) in
  Printf.printf "%s: %d bytes, %d polls/sample, OCaml %s\n%!"
    file (String.length text) iterations Sys.ocaml_version;
  for _ = 1 to 5 do
    measure "before (unchanged mtime)" (fun _ -> ignore (Unix.stat file));
    measure "after (content/digest)" (fun i ->
      let next, changed = Rays_editor.Source.poll ~now:(float i) !source in
      source := next;
      assert (changed = None));
    (* Start a fresh poll clock for the next sample. *)
    source := Rays_editor.Source.at ~file ~digest:(Editor_document.Contexts.sha256 text)
  done
