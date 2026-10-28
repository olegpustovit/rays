let () =
  let driver = Ogpu.Impl.create_driver () in
  let live_handles = Ogpu.Impl.live_handles in
  Ogpu_conformance.Conformance.run driver;
  if live_handles () <> 0 then failwith "mock backend leaked handles";
  print_endline
    "OGPU conformance (mock): capabilities, buffer round trip, texture upload and host read \
     (padded rows, inset origin), Rgba8/Rgba16_float/Rgba32_float level-0 round trips, typed \
     compute/render/ray-tracing rejection, instance record packing, abandoned commands, lifetime, \
     zero handles"
