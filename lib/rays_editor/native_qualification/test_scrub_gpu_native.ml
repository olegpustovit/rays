(* On the real device: sketches/gpu_sea.rays at GRID_SIDE (512 in @runtest-native) lowered as the
   editor lowers it (the workspace GPU, fast precision) and scrubbed as a card's number field
   scrubs: each lowering after an edit keeps the cards it can (a live argument's body replayed, its
   kernel nodes and their compiled kernels taken again), and its cook on the device equals, byte for
   byte, the cook of the same text lowered and cooked in a new process, where nothing is kept. A
   scrub back to an earlier value compiles no kernel. Device kernel compiles per scrub are printed. *)
module Lowering = Editor_document.Lowering
module Workspace_doc = Editor_document.Workspace_doc
module E = Flow_graph.Flow_edit

let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000
let catalog = ok (Rays_editor.workspace_catalog ())

let sea () =
  let text = In_channel.with_open_bin "../../../sketches/gpu_sea.rays" In_channel.input_all in
  let at = Str.search_forward (Str.regexp_string "(side : int 1000)") text 0 in
  String.sub text 0 at
  ^ Printf.sprintf "(side : int %d)" side
  ^ String.sub text (at + 17) (String.length text - at - 17)

(* the sea graph's root cooked at t = 0.7, as bytes (positions, every attribute) *)
let cook (doc : Editor_document.Document.t) =
  let lowered = doc.workspace.lowered in
  let graph = List.find (fun (g : Flow_sop.Lower.graph) -> g.name = "sea") lowered.graphs in
  let node = Result.get_ok (Lisp_sop.root lowered graph) in
  let session = Result.get_ok (Sop.Session.create ~max_entries:64 ~max_payload_bytes:(1 lsl 30)) in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      match
        Sop.Session.cook session ~context:(Result.get_ok (Sop.Context.create ~time:0.7 ())) node
      with
      | Ok output ->
          Digest.to_hex
            (Digest.string
               (Rdk_test_support.geometry_bytes (ok (Sop.Payload.geometry output.payload))))
      | Error d -> failwith (Diagnostic.error_to_string d))

let fresh_cook gpu text =
  cook (ok (Lowering.of_workspace ~gpu (Result.get_ok (Workspace_doc.of_text catalog text))))

(* the same text in a new process: nothing kept from this one's lowerings *)
let cook_elsewhere text =
  let file = Filename.temp_file "scrub" ".rays" in
  Out_channel.with_open_bin file (fun oc -> output_string oc text);
  let ic = Unix.open_process_args_in Sys.executable_name [| Sys.executable_name; file |] in
  let digest = input_line ic in
  (match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith "the fresh cook failed");
  Sys.remove file;
  digest

let () =
  let owner = Flow_native.Workspace_gpu.create () in
  Fun.protect
    ~finally:(fun () -> Flow_native.Workspace_gpu.close owner)
    (fun () ->
      Flow_native.Workspace_gpu.with_backend ~fast:true owner (fun gpu ->
          if Array.length Sys.argv = 2 then
            print_endline
              (fresh_cook gpu (In_channel.with_open_bin Sys.argv.(1) In_channel.input_all))
          else
            let host () =
              let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats owner in
              host
            in
            let doc =
              ok
                (Lowering.of_workspace ~gpu
                   (Result.get_ok (Workspace_doc.of_text catalog (sea ()))))
            in
            ignore (cook doc);
            let waves = [ "sea"; "waves" ] in
            (* the last scrub returns to a text cooked before: no pipeline is compiled *)
            let steps =
              [
                (waves, "seed", [], "9");
                (waves, "octaves", [], "5");
                (waves, "seed", [], "11");
                (waves, "lacunarity", [ 1 ], "2.25");
                ([ "sea"; E.nested_leaf "waves" (Pos 0) ], "width", [], "7.5");
                (waves, "seed", [], "9");
                (waves, "seed", [], "11");
                (waves, "seed", [], "9");
              ]
            in
            let last = List.length steps - 1 in
            ignore
              (List.fold_left
                 (fun ((doc : Editor_document.Document.t), i) (node, key, sub, value) ->
                   let op =
                     E.Set_arg
                       { node; key = Kw key; sub; value = List.hd (ok (Flow.Syntax.parse value)) }
                   in
                   let ws = ok (Workspace_doc.edit catalog doc.workspace.ws op) in
                   let before = host () in
                   let doc = ok (Lowering.of_workspace ~previous:doc ws) in
                   let scrubbed = cook doc in
                   let after = host () in
                   let compiled = after.pipeline_compilations - before.pipeline_compilations in
                   let what = Printf.sprintf "%s %s" key value in
                   Printf.printf "scrub %s: %d pipelines compiled, %d runners created\n%!" what
                     compiled
                     (after.runners_created - before.runners_created);
                   if scrubbed <> cook_elsewhere (Workspace_doc.to_text ws) then
                     failwith (what ^ ": the scrubbed cook differs from a fresh one");
                   if i = last && compiled <> 0 then
                     failwith (what ^ ": a scrub back to a cooked text compiled a pipeline");
                   (doc, i + 1))
                 (doc, 0) steps)))
