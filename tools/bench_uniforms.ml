(* Tuple shorthand for the record-based renderer submission. *)
let sampled ?texture ?auxiliary ?(samples=1) family blend draw : Renderer.Scene_execution.sampled_draw =
  {family;blend;texture;auxiliary;vertex_attributes=None;samples;draw}
let render_blended ?clear r draws=Renderer.Scene_execution.submit ?clear r
  (List.map(fun(blend,draw)->sampled Renderer.Scene_execution.Scene2 blend draw)draws)
let get = function Ok value -> value | Error error -> failwith (Ogpu.Error.to_string error)

let configuration : Ogpu.Surface.configuration =
  { logical_width = 64; logical_height = 64; physical_width = 64;
    physical_height = 64; format = Rgba8_unorm; present_mode = Immediate;
    max_acquired = 2;layer=None }

let mesh : Renderer.Scene_execution.mesh =
  { key = "uniform-bench"; vertices = Bytes.make 48 '\000'; vertex_count = 3;
    indices = Bytes.make 12 '\000'; index_count = 3;
    primitive = Ogpu.Render_pass.Triangle_list }

let uniform value =
  let bytes = Bytes.make 24 '\000' in
  Bytes.set_int32_le bytes 0 (Int32.bits_of_float 1.);
  Bytes.set_int32_le bytes 16 (Int32.bits_of_float 1.);
  Bytes.set_int32_le bytes 8 (Int32.bits_of_float value);
  bytes

let count = match Sys.getenv_opt "RAYS_UNIFORM_BENCH_DRAWS" with
  | None -> 128 | Some value -> max 1 (int_of_string value)

let draws frame = List.init count (fun index ->
  let state : Renderer.Scene_execution.state =
    { viewport = 0, 0, 64, 64; scissor = 0, 0, 64, 64;
      cull = Ogpu.Render_pass.Cull_none; depth_compare = Always;
      depth_write = false; depth_load = Load; depth_clear = 1.;
      transform_uniforms = Some (uniform (float index +. float frame /. 1000.)) } in
  Ogpu.Pipeline.Replace, { Renderer.Scene_execution.mesh; state })

let percentile values fraction =
  let sorted = Array.copy values in
  Array.sort Float.compare sorted;
  sorted.(int_of_float (ceil (fraction *. float (Array.length sorted))) - 1)

let () =
  let driver, live_handles = Ogpu.Impl.create_driver () in
  let baseline = live_handles () in
  let renderer = get (Scene_execution_fixtures.create_offscreen driver configuration) in
  let stable = draws 0 in
  print_endline "mode,draws,frames,p50_s,p95_s,allocated_bytes_per_frame,uploaded_bytes_per_frame,peak_buffers";
  let measure name make_draws =
    for frame = 1 to 3 do
      ignore (get (render_blended renderer (make_draws frame)))
    done;
    Gc.full_major ();
    let samples = Array.make 50 0. in
    let allocated = Gc.allocated_bytes ()
    and uploaded = Renderer.Scene_execution.upload_bytes renderer
    and peak_buffers = ref 0 in
    for frame = 0 to Array.length samples - 1 do
      let started = Unix.gettimeofday () in
      ignore (get (render_blended renderer (make_draws (frame + 4))));
      samples.(frame) <- Unix.gettimeofday () -. started;
      peak_buffers := max !peak_buffers (live_handles ())
    done;
    Printf.printf "%s,%d,50,%.6f,%.6f,%.0f,%Ld,%d\n%!" name count
      (percentile samples 0.5) (percentile samples 0.95)
      ((Gc.allocated_bytes () -. allocated) /. 50.)
      (Int64.div (Int64.sub (Renderer.Scene_execution.upload_bytes renderer) uploaded) 50L)
      !peak_buffers in
  measure "static" (fun _ -> stable);
  measure "changing" draws;
  get (Renderer.Scene_execution.destroy renderer);
  if live_handles () <> baseline then
    failwith "uniform benchmark leaked mock handles"
