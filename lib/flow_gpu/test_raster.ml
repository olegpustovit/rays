(* The renderer's raster shaders are generated: lib/renderer/shaders/NAME.rays printed by Raster into
   shaders/metal/NAME.metal, which the renderer embeds. This checks the checked-in output is the
   printer's; [--write DIR] rewrites it there (DIR is the source lib/renderer/shaders). *)
let pipelines = [ "marks"; "ui" ]
let header name = Printf.sprintf "// Generated from %s.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.\n" name
let read path = In_channel.with_open_bin path In_channel.input_all

let () =
  let write = match Sys.argv with [| _; "--write"; dir |] -> Some dir | _ -> None in
  let dir = Option.value write ~default:"../renderer/shaders" in
  List.iter (fun name ->
    let declarations = read (Filename.concat dir (name ^ ".rays")) in
    let source = match Flow_gpu.Raster.msl ~declarations ~vertex:(name ^ "-vertex") ~fragment:(name ^ "-fragment") with
      | Ok source -> header name ^ source
      | Error d -> failwith (name ^ ".rays: " ^ Diagnostic.to_string d) in
    let path = Filename.concat dir (Filename.concat "metal" (name ^ ".metal")) in
    match write with
    | Some _ -> Out_channel.with_open_bin path (fun o -> output_string o source)
    | None -> if read path <> source then failwith (path ^ " differs from its .rays: rerun test_raster.exe --write lib/renderer/shaders"))
    pipelines;
  print_endline "raster: marks and ui stages are their .rays, printed"
