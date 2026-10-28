let backend = "metal"
let create_driver () = Ogpu_metal_native.Backend.create ()
let live_handles () =
  match Metal.Release_queue.drain (), Metal.Release_queue.stats () with
  | Ok _, Ok stats -> stats.live_handles
  | _ -> failwith "Metal release-queue diagnostics failed"
