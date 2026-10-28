let fail message = raise (Failure message)
let ok = function Ok value -> value | Error error -> fail (Ogpu.Error.to_string error)

let reject = function
  | Error (error : Ogpu.Error.t) when error.kind = Invalid_argument -> ()
  | _ -> fail "expected rejection"

let caps = Ogpu.Caps.minimum_m1

let shader ~backend ~entries ~bindings =
  ok
    (Ogpu.Shader.create
       {
         backend;
         label = None;
         bytes = Bytes.of_string "artifact";
         entry_points = entries;
         bindings;
       })

let entry name stage : Ogpu.Shader.entry_point = { name; stage }

let run () =
  let device = Ogpu.Handle.create_device () in
  let vertex = shader ~backend:"mock" ~entries:[ entry "vs" Ogpu.Shader.Vertex ] ~bindings:[] in
  let fragment = shader ~backend:"mock" ~entries:[ entry "fs" Ogpu.Shader.Fragment ] ~bindings:[] in
  let empty = ok (Ogpu.Binding.create_pipeline_layout ~device ~capabilities:caps []) in
  let render sample_count fragment fragment_entry =
    Ogpu.Pipeline.create_render caps
      {
        backend = "mock";
        label = None;
        layout = empty;
        vertex;
        vertex_entry = "vs";
        fragment;
        fragment_entry;
        color_format = Ogpu.Pipeline.Bgra8_unorm;
        depth_format = Ogpu.Pipeline.Depth32_float;
        sample_count;
      }
  in
  ignore (ok (render 4 (Some fragment) (Some "fs")));
  reject (render 3 (Some fragment) (Some "fs"));
  reject (render max_int (Some fragment) (Some "fs"));
  reject (render 1 (Some fragment) None);
  reject (render 1 None (Some "fs"));
  reject
    (Ogpu.Pipeline.create_render caps
       {
         backend = "mock";
         label = Some "";
         layout = empty;
         vertex;
         vertex_entry = "vs";
         fragment = None;
         fragment_entry = None;
         color_format = Ogpu.Pipeline.Rgba8_unorm;
         depth_format = Ogpu.Pipeline.No_depth;
         sample_count = 1;
       });
  Ogpu.Handle.destroy_device device;
  print_endline "OGPU deterministic pipeline descriptions passed"
