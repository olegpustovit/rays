(* Qualification only: every successful static kernel is compared with every
   walker element, bit for bit. A mismatch aborts the run, never falls back. *)
let rec files path =
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun name -> files (Filename.concat path name))
  else if Filename.check_suffix path ".rays" then [ path ]
  else []

let () =
  let arguments, keep =
    match Array.to_list Sys.argv |> List.tl with
    | "--shard" :: shard :: paths ->
        let i, n = Scanf.sscanf shard "%d/%d" (fun i n -> (i, n)) in
        if n <= 0 || i < 0 || i >= n then invalid_arg "shard must be i/n with 0 <= i < n";
        (paths, fun index -> index mod n = i)
    | paths -> (paths, fun _ -> true)
  in
  let paths = List.concat_map files arguments |> List.filteri (fun index _ -> keep index) in
  List.iter
    (fun file ->
      let text = In_channel.with_open_bin file In_channel.input_all in
      (* Import-only files are checked when their owning workspace is loaded. *)
      let forms =
        match Flow.Syntax.parse text with
        | Ok forms -> forms
        | Error d -> failwith (Diagnostic.to_string d)
      in
      let workspace =
        List.exists
          (fun (form : Flow.Syntax.t) ->
            match form.node with List ({ node = Sym "workspace"; _ } :: _) -> true | _ -> false)
          forms
      in
      if workspace then begin
        Printf.printf "verify static %s\n%!" file;
        let loaded =
          Result.bind (Rays_editor.Source.read_imports ~file text) (fun imports ->
              Rays_editor.Workspace.load ~imports text)
        in
        let loaded =
          match loaded with
          | Ok workspace -> workspace
          | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
        in
        (* Million-point reference topology takes minutes. 73² still crosses
           the head and block boundaries. Change the in-memory default so
           graph references also use it, unlike top-level input overrides. *)
        let edits =
          let value n = List.hd (Result.get_ok (Flow.Syntax.parse (string_of_int n))) in
          match Filename.basename file with
          | "gpu_sea.rays" ->
              [
                Flow_graph.Flow_edit.Set_input_default
                  { form = "sea"; input = "side"; value = value 73 };
              ]
          | "flow_particles_gpu.rays" ->
              [
                Flow_graph.Flow_edit.Set_arg
                  { node = [ "exact_surface"; "side" ]; key = Whole; sub = []; value = value 72 };
              ]
          | _ -> []
        in
        let loaded =
          if edits = [] then loaded
          else begin
            Printf.printf "  qualification grid: 73² points\n%!";
            let catalog = Result.get_ok (Rays_editor.workspace_catalog ()) in
            List.fold_left
              (fun doc edit ->
                match Editor_document.Workspace_doc.edit catalog doc edit with
                | Ok doc -> doc
                | Error d -> failwith (Diagnostic.to_string d))
              loaded edits
          end
        in
        match Flow_ir.static ~verify:true ~inputs:loaded.inputs loaded.checked with
        | Ok _ -> ()
        | Error d -> failwith (file ^ ": " ^ Diagnostic.to_string d)
      end)
    paths;
  Printf.printf "Verified static kernels across %d source files\n%!" (List.length paths)
