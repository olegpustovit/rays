(* The renderer's raster shaders are generated: lib/renderer/shaders/NAME.rays printed by Raster into
   shaders/metal/NAME.metal (which the renderer embeds) and shaders/spirv/NAME.{vert,frag}.spv (the
   Vulkan modules, validated by spirv-val when it is installed). This checks the checked-in output is
   the printers'; [--write DIR] rewrites it there (DIR is the source lib/renderer/shaders). *)
(* (output name, source, vertex entry, fragment entry, textures in argument buffers) *)
let pipelines = [ "marks", "marks", "marks-vertex", "marks-fragment", false; "ui", "ui", "ui-vertex", "ui-fragment", false;
  "scene3", "scene", "scene3-vertex", "scene3-fragment", false;
  "scene3_points", "scene", "scene3-points-vertex", "scene3-fragment", false;
  "scene3_textured", "scene", "scene3-vertex", "scene3-textured-fragment", false;
  "scene3_sun_depth", "scene", "sun-vertex", "sun-fragment", false;
  "scene3_world", "scene", "scene3-vertex", "world-fragment", false;
  "scene2_textured_argument", "scene", "scene2-vertex", "scene2-fragment", true;
  "image_rgba8", "image", "image-vertex", "image-fragment", false ]
let header name = Printf.sprintf "// Generated from %s.rays by Flow_gpu.Raster (lib/flow_gpu/test_raster.exe --write); do not edit.\n" name
let read path = In_channel.with_open_bin path In_channel.input_all
let ok name = function Ok value -> value | Error d -> failwith (name ^ ".rays: " ^ Diagnostic.to_string d)
let validator = Sys.command "command -v spirv-val >/dev/null 2>&1" = 0

let () =
  let write = match Sys.argv with [| _; "--write"; dir |] -> Some dir | _ -> None in
  let dir = Option.value write ~default:"../renderer/shaders" in
  List.iter (fun (name, source, vertex, fragment, arguments) ->
    let declarations = read (Filename.concat dir (source ^ ".rays")) in
    let msl = header source ^ ok source (Flow_gpu.Raster.msl ~arguments ~declarations ~vertex ~fragment ()) in
    let vert, frag = ok source (Flow_gpu.Raster.spirv ~declarations ~vertex ~fragment) in
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
  print_endline (if validator then "raster: every renderer and image sink stage are their .rays, printed (MSL, SPIR-V spirv-val valid)"
    else "raster: every renderer and image sink stage are their .rays, printed (spirv-val unavailable)")
