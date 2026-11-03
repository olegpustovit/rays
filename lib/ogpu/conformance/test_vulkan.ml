let read name =
  Bytes.of_string (In_channel.with_open_bin ("vulkan/" ^ name ^ ".spv") In_channel.input_all)

let shaders =
  {
    Ogpu_conformance.Conformance.backend = "vulkan";
    bytes =
      (function
      | ( "sample_lod" | "library" | "rays" | "rt_extra" | "flat_vertex" | "flat_fragment" | "argument_fragment"
        | "textured_fragment" ) as name ->
          read name
      | name -> failwith ("no Vulkan shader " ^ name));
    (* SPIR-V has no named function constants: the two variants are two entries. *)
    scale = (fun triple -> ((if triple then "scale3" else "scale2"), []));
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
      Ogpu_conformance.Conformance.run ~shaders driver;
      if Ogpu.Impl.live_handles () <> 0 then failwith "Vulkan backend leaked handles";
      let messages = Vulkan.validation_messages () in
      if messages <> 0 then failwith (Printf.sprintf "%d validation messages" messages);
      print_endline "OGPU conformance (Vulkan): suite passed, zero handles, zero validation messages"

(* VK_ERROR_OUT_OF_DEVICE_MEMORY is a typed [Capacity] error, never a crash. One buffer of twice the
   working set cannot fit any heap, so the driver refuses it at once; taking device-local memory
   until the driver said no starved every other test's device for that moment, and made them fail
   under a parallel [@runtest-native]. *)
let () =
  let driver = Ogpu.Impl.create_driver () in
  match driver.Ogpu.Backend.create_device () with
  | Error { Ogpu.Error.kind = No_adapter; _ } -> ()
  | Error error -> failwith (Ogpu.Error.to_string error)
  | Ok raw ->
      let size = Int64.mul 2L (raw.recommended_working_set ()) in
      (match
         raw.create_buffer Ogpu.Types.Device_local
           { Ogpu.Types.label = None; size; usage = [ Ogpu.Types.Storage ] }
       with
      | Error { Ogpu.Error.kind = Capacity; _ } -> ()
      | Error error -> failwith ("out of device memory is not Capacity: " ^ Ogpu.Error.to_string error)
      | Ok resource ->
          ignore (resource.destroy ());
          failwith "a buffer of twice the working set was created");
      (match raw.destroy_device () with Ok () -> () | Error e -> failwith (Ogpu.Error.to_string e));
      if Ogpu.Impl.live_handles () <> 0 then failwith "out-of-memory test leaked handles";
      if Vulkan.validation_messages () <> 0 then failwith "out-of-memory test: validation messages";
      print_endline "OGPU Vulkan: an oversized device-local buffer is a typed Capacity error"
