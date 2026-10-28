(* Flow.Workspace.check, in process: the path tracer's accumulation kernel set ("pt", the first-frame gate),
   its declarations whole ("ptlib"), or a sketch FILE.rays checked as the editor opens it. Median of N
   runs: the "check" phase's CPU time and the whole call's allocated bytes.
   dune exec tools/bench_check.exe -- N pt ptlib sketches/sop_gallery.rays *)
let get = function Ok v -> v | Error m -> failwith m

let catalog =
  lazy
    (match Editor_document.Kinds.catalog ~version:Flow_sop.Manifest.version Sop.Nodes.factories with
    | Ok c -> c
    | Error d -> failwith (Diagnostic.to_string d))

let run = function
  | "pt" -> get (Rays_pathtracer.Private.check_sketch ())
  | "ptlib" -> get (Rays_pathtracer.Private.check_library ())
  | file -> (
      let text = In_channel.with_open_bin file In_channel.input_all in
      match Editor_document.Workspace_doc.check_text (Lazy.force catalog) text with
      | Ok _ -> ()
      | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds)))

let median a =
  let a = Array.copy a in
  Array.sort Float.compare a;
  a.(Array.length a / 2)

let () =
  let n = int_of_string Sys.argv.(1) in
  Array.iteri
    (fun i target ->
      if i >= 2 then begin
        run target;
        let samples =
          Array.init n (fun _ ->
              Gc.full_major ();
              let bytes = Gc.allocated_bytes () in
              let (), timing = Flow.Phase_timer.sample ~clock:Sys.time (fun () -> run target) in
              (Flow.Phase_timer.seconds timing "check", Gc.allocated_bytes () -. bytes))
        in
        Printf.printf "%-28s check %8.2f ms  alloc %8.1f MB  (median of %d)\n%!" target
          (1e3 *. median (Array.map fst samples))
          (median (Array.map snd samples) /. 1e6)
          n
      end)
    Sys.argv
