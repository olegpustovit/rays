let read name =
  Bytes.of_string (In_channel.with_open_bin ("vulkan/" ^ name ^ ".spv") In_channel.input_all)

let shaders =
  {
    Ogpu_conformance.Conformance.backend = "vulkan";
    bytes =
      (function
      | ( "sample_lod" | "library" | "flat_vertex" | "flat_fragment" | "argument_fragment"
        | "textured_fragment" ) as name ->
          read name
      | name -> failwith ("no Vulkan shader " ^ name));
    (* SPIR-V has no named function constants: the two variants are two entries. *)
    scale = (fun triple -> ((if triple then "scale3" else "scale2"), []));
    instance_sizes = (64, 64);
  }

let () =
  let driver = Ogpu.Impl.create_driver () in
  match driver.Ogpu.Backend.create_device () with
  | Error { Ogpu.Error.kind = No_adapter; _ } ->
      print_endline "OGPU conformance (Vulkan): skipped (no device)"
  | Error error -> failwith (Ogpu.Error.to_string error)
  | Ok raw ->
      (match raw.destroy_device () with
      | Ok () -> ()
      | Error error -> failwith (Ogpu.Error.to_string error));
      (* A failure leaves the device alive; the driver then hangs the normal exit path for minutes, so
         report it and leave at once. *)
      (try Ogpu_conformance.Conformance.run ~shaders driver
       with exn ->
         prerr_endline ("OGPU conformance (Vulkan) failed: " ^ Printexc.to_string exn);
         Unix._exit 2);
      if Ogpu.Impl.live_handles () <> 0 then failwith "Vulkan backend leaked handles";
      let messages = Vulkan.validation_messages () in
      if messages <> 0 then failwith (Printf.sprintf "%d validation messages" messages);
      print_endline "OGPU conformance (Vulkan): suite passed, zero handles, zero validation messages"
