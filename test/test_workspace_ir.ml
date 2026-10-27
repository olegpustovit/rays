let rec files directory = Array.to_list (Sys.readdir directory) |> List.sort String.compare
  |> List.concat_map (fun name -> let path = Filename.concat directory name in
    if Sys.is_directory path then files path else if Filename.check_suffix name ".rays" then [path] else [])
let () =
  let root = if Array.length Sys.argv > 1 then Sys.argv.(1) else ".." in
  let directory = Sys.getenv_opt "RAYS_WORKSPACE_PIXELS" in
  let paths = files (Filename.concat root "sketches")
    @ [Filename.concat root "specification/pxui-kit/kit.rays"] in
  let fixtures = Filename.concat root "specification/workspace/cases" in
  let fixtures = Array.to_list (Sys.readdir fixtures) |> List.sort String.compare
    |> List.filter (fun name -> Filename.check_suffix name ".lisp" || name = "import.rays")
    |> List.map (Filename.concat fixtures) in
  if List.length fixtures < 12 then failwith "missing generated workspace fixtures";
  let selected path = match Sys.getenv_opt "RAYS_WORKSPACE_FILTER" with
    | None -> true
    | Some suffix -> Filename.check_suffix path suffix in
  let paths = List.filter selected paths and fixtures = List.filter selected fixtures in
  (* by default the former examples, the kit, the fixtures and four small sketches; every sketch under RAYS_SLOW=1 *)
  let small = ["ws_layout.rays"; "ws_kit.rays"; "voxel_wall.rays"; "flow_kernel.rays"; "audio.rays"; "basic.rays";
    "file_dialog.rays"; "generative.rays"; "noise.rays"; "pxui.rays"; "recursive_rectangles.rays"; "pathtracer.rays"] in
  let has s sub = let n = String.length sub in
    let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in go 0 in
  (* the fourteen that cost 3-41 s each (drawing, particles, variations, gallery, wave, sunflower, bloom, tiles, orrery, garland, rosette, tree, tunnel, procedural_modeling) wait for RAYS_SLOW=1: the other files
     and fixtures take the same compiled-vs-reference path *)
  let heavy = ["sop_gallery.rays"; "wave.lisp"; "sunflower.lisp"; "bloom.lisp"; "tiles.lisp"; "orrery.lisp";
    "sketches/drawing.rays"; "sketches/particles.rays"; "variations.lisp"; "garland.lisp"; "rosette.lisp"; "tree.lisp"; "tunnel.lisp"; "procedural_modeling.rays"] in
  let slow = Sys.getenv_opt "RAYS_SLOW" = Some "1" in
  let paths = if slow then paths
    else List.filter (fun p -> (not (has p "sketches/") || List.exists (has p) small)
      && not (List.exists (has p) heavy)) paths in
  let fixtures = if slow then fixtures else List.filter (fun p -> not (List.exists (has p) heavy)) fixtures in
  let count = ref 0 in
  List.iter (fun path ->
    let name = String.sub path (String.length root + 1) (String.length path - String.length root - 1) in
    begin
      let text = In_channel.with_open_bin path In_channel.input_all in
      let workspace = match Result.bind (Rays_editor.Source.read_imports ~file:path text)
          (fun imports -> Rays_editor.Workspace.load ~imports text) with Ok w -> w
        | Error ds -> failwith (name ^ ": " ^ String.concat "\n" (List.map Diagnostic.to_string ds)) in
      let directory = Option.map (fun parent ->
        if not (Sys.file_exists parent) then Unix.mkdir parent 0o755;
        Filename.concat parent (String.map (function '/' -> '-' | c -> c) name)) directory in
      Workspace_parity.check ?directory ~factories:Sop_catalog.Editor.factories ~name workspace;
      incr count
    end) (paths @ fixtures);
  if !count <> List.length paths + List.length fixtures then failwith "incomplete workspace coverage";
  Printf.printf "Workspace IR sweep: %d files, %d fixtures\n%!" (List.length paths) (List.length fixtures)
