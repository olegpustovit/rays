let backend = "mock"
let create_driver () =
  let driver, control = Backend_mock.create () in
  let live_handles () =
    let buffers, textures, pipelines, queues, surfaces =
      Backend_mock.live_counts control in
    buffers + textures + pipelines + queues + surfaces in
  driver, live_handles
