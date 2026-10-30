(* The raster and compute shaders are printed at build time (lib/flow_shader/print_shader.exe,
   run by each owner's dune). This validates the printed SPIR-V modules with spirv-val when it is
   installed. *)

let validator = Sys.command "command -v spirv-val >/dev/null 2>&1" = 0

let () =
  if validator then
    List.iter
      (fun dir ->
        Array.iter
          (fun file ->
            if
              Filename.check_suffix file ".spv"
              && Sys.command
                   ("spirv-val --target-env vulkan1.1 " ^ Filename.quote (Filename.concat dir file))
                 <> 0
            then failwith ("spirv-val refused " ^ file))
          (Sys.readdir dir))
      [ "."; "../renderer"; "../ogpu_metal" ];
  print_endline
    (if validator then "raster: printed SPIR-V spirv-val valid" else "raster: spirv-val unavailable")
