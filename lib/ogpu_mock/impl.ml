let backend = "mock"
(* ponytail: controls are kept for the process lifetime; they are a few counters each *)
let controls = ref []
let create_driver () =
  let driver, control = Backend_mock.create () in
  controls := control :: !controls;
  driver
let live_handles () =
  List.fold_left (fun total control ->
    let buffers, textures, pipelines, queues, surfaces = Backend_mock.live_counts control in
    total + buffers + textures + pipelines + queues + surfaces) 0 !controls
