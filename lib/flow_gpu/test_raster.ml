(* The renderer's raster shaders are generated: lib/renderer/shaders/NAME.rays printed by Raster into
   shaders/metal/NAME.metal (which the renderer embeds) and shaders/spirv/NAME.{vert,frag}.spv (the
   Vulkan modules, validated by spirv-val when it is installed). This checks the checked-in output is
   the printers'; [--write DIR] rewrites it there (DIR is the source lib/renderer/shaders). *)
let pipelines = [ "marks"; "ui" ]
let header name = Printf.sprintf "// Generated from %s.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.\n" name
let read path = In_channel.with_open_bin path In_channel.input_all
let ok name = function Ok value -> value | Error d -> failwith (name ^ ".rays: " ^ Diagnostic.to_string d)
let validator = Sys.command "command -v spirv-val >/dev/null 2>&1" = 0

let () =
  let write = match Sys.argv with [| _; "--write"; dir |] -> Some dir | _ -> None in
  let dir = Option.value write ~default:"../renderer/shaders" in
  List.iter (fun name ->
    let declarations = read (Filename.concat dir (name ^ ".rays")) in
    let vertex = name ^ "-vertex" and fragment = name ^ "-fragment" in
    let msl = header name ^ ok name (Flow_gpu.Raster.msl ~declarations ~vertex ~fragment) in
    let vert, frag = ok name (Flow_gpu.Raster.spirv ~declarations ~vertex ~fragment) in
    let outputs = [ "metal/" ^ name ^ ".metal", msl; "spirv/" ^ name ^ ".vert.spv", Bytes.to_string vert;
      "spirv/" ^ name ^ ".frag.spv", Bytes.to_string frag ] in
    List.iter (fun (file, text) ->
      let path = Filename.concat dir file in
      if Filename.check_suffix file ".spv" && validator then begin
        let temporary = Filename.temp_file "rays-raster-" ".spv" in
        Fun.protect ~finally:(fun () -> Sys.remove temporary) (fun () ->
          Out_channel.with_open_bin temporary (fun o -> output_string o text);
          if Sys.command ("spirv-val --target-env vulkan1.1 " ^ Filename.quote temporary) <> 0 then
            failwith ("spirv-val refused " ^ file))
      end;
      match write with
      | Some _ -> Out_channel.with_open_bin path (fun o -> output_string o text)
      | None -> if read path <> text then
          failwith (path ^ " differs from its .rays: rerun test_raster.exe --write lib/renderer/shaders")) outputs)
    pipelines;
  print_endline (if validator then "raster: marks and ui stages are their .rays, printed (MSL, SPIR-V spirv-val valid)"
    else "raster: marks and ui stages are their .rays, printed (spirv-val unavailable)")
