let backend = "vulkan"

let create_driver () =
  {
    Ogpu_core.Backend.create_device =
      (fun () ->
        Error (Ogpu_core.Error.make "Vulkan.create_device" Ogpu_core.Error.Unsupported "no device yet"));
  }

let live_handles () = 0
