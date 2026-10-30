(* [print_shader.exe OWNER] prints OWNER's shaders from its shaders/NAME.rays into the current
   directory, at build time (OWNER's dune): NAME.metal, which OWNER embeds, and the Vulkan modules
   NAME.{vert,frag,comp}.spv, which lib/flow_gpu/test_raster.exe validates with spirv-val. OWNER [web]
   prints the web's compaction and dispatch arguments as NAME.wgsl from lib/flow_gpu/shaders (web_export
   embeds them). *)

(* (output name, source, vertex entry, fragment entry, textures in argument buffers) *)
let raster = function
  | "renderer" ->
      [
        ("marks", "marks", "marks-vertex", "marks-fragment", false);
        ("ui", "ui", "ui-vertex", "ui-fragment", false);
        ("scene3", "scene", "scene3-vertex", "scene3-fragment", false);
        ("scene3_points", "scene", "scene3-points-vertex", "scene3-fragment", false);
        ("scene3_textured", "scene", "scene3-vertex", "scene3-textured-fragment", false);
        ("scene3_sun_depth", "scene", "sun-vertex", "sun-fragment", false);
        ("scene3_world", "scene", "scene3-vertex", "world-fragment", false);
        ("geometry", "marks", "geometry-vertex", "geometry-fragment", false);
        ("quad", "marks", "quad-vertex", "quad-fragment", true);
        ("image_rgba8", "image", "image-vertex", "image-fragment", false);
      ]
  | "ogpu_metal" -> [ ("present", "present", "present-vertex", "present-fragment", false) ]
  | _ -> []

(* (name, entry, grid, Metal threadgroup size, Vulkan workgroup size) *)
let compute = function
  | "flow_gpu" ->
      [
        (* the resident fold's compaction (Run.compaction) *)
        ("compact", "compact-mask", true, 1024, 256);
        (* a resident P's bounds (Run.bounds) *)
        ("bounds", "chunk-bounds", false, 1024, 256);
        (* a triangle mesh's display normals from its resident P (Host.normals) *)
        ("normals", "point-normal", true, 256, 256);
        (* a resident dispatch's threadgroup counts from a count cell (Run.dispatch_arguments) *)
        ("dispatch", "dispatch-arguments", false, 1, 1);
      ]
  | _ -> []

let ok name = function
  | Ok value -> value
  | Error d -> failwith (name ^ ".rays: " ^ Diagnostic.to_string d)

let read file = In_channel.with_open_bin file In_channel.input_all
let source name = read ("shaders/" ^ name ^ ".rays")
let write file text = Out_channel.with_open_bin file (fun o -> output_string o text)

let () =
  let owner = Sys.argv.(1) in
  List.iter
    (fun (name, source_name, vertex, fragment, arguments) ->
      let declarations = source source_name in
      write (name ^ ".metal")
        (ok source_name (Flow_shader.Raster.msl ~arguments ~declarations ~vertex ~fragment ()));
      let vert, frag = ok source_name (Flow_shader.Raster.spirv ~declarations ~vertex ~fragment) in
      write (name ^ ".vert.spv") (Bytes.to_string vert);
      write (name ^ ".frag.spv") (Bytes.to_string frag))
    (raster owner);
  List.iter
    (fun (name, entry, grid, metal, vulkan) ->
      let declarations = source name in
      write (name ^ ".metal")
        (ok name (Flow_shader.Raster.compute_msl ~grid ~size:metal ~declarations ~entry));
      write (name ^ ".comp.spv")
        (Bytes.to_string
           (ok name (Flow_shader.Raster.compute_spirv ~grid ~size:vulkan ~declarations ~entry))))
    (compute owner);
  (* the web's resident compaction (workgroups of 256, the WebGPU default) and dispatch arguments *)
  if owner = "web" then
    List.iter
      (fun (name, entry, size, kernel) ->
        let declarations = read ("../flow_gpu/shaders/" ^ name ^ ".rays") in
        let program, buffers =
          ok name (Flow_shader.Raster.compute_program ~size ~declarations ~entry)
        in
        write (name ^ ".wgsl") (ok name (Flow_web.Wgsl.compute ~size ~name:kernel ~buffers program)))
      [
        ("compact", "compact-mask", 256, "compact_mask");
        ("dispatch", "dispatch-arguments", 1, "dispatch_arguments");
      ];
  (* the presenter's pipelines (present.js): native's marks, geometry and quad, every draw's
     constants bound at 6 *)
  if owner = "web" then begin
    let marks = read "../renderer/shaders/marks.rays" in
    let binding name slot = if name = "affine" then 6 else slot in
    let stage entry (d : Flow_shader.Raster.data) =
      {
        Flow_web.Wgsl.entry;
        program = d.program;
        buffers = List.map (fun (u, name, slot, words) -> (u, binding name slot, words)) d.buffers;
        elements = d.elements;
        outputs = d.outputs;
      }
    in
    write "marks.wgsl"
      (ok "marks"
         (Flow_web.Wgsl.raster
            (List.map
               (fun (quads, vertex, fragment) ->
                 let v, f =
                   ok vertex (Flow_shader.Raster.stages ~declarations:marks ~vertex ~fragment)
                 in
                 (quads, stage vertex v, stage fragment f))
               [
                 (true, "marks-vertex", "marks-fragment");
                 (false, "geometry-vertex", "geometry-fragment");
                 (true, "quad-vertex", "quad-fragment");
               ])))
  end
