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

(* VK_ERROR_OUT_OF_DEVICE_MEMORY is a typed [Capacity] error, never a crash: device-local buffers are
   taken until the driver says no (a bounded count past the working set), and every one is released. *)
let () =
  let driver = Ogpu.Impl.create_driver () in
  match driver.Ogpu.Backend.create_device () with
  | Error { Ogpu.Error.kind = No_adapter; _ } -> ()
  | Error error -> failwith (Ogpu.Error.to_string error)
  | Ok raw ->
      let chunk = Int64.min 1_073_741_824L raw.capabilities.limits.max_buffer_size in
      let budget = raw.recommended_working_set () in
      let most = Int64.to_int (Int64.div (Int64.mul 3L budget) chunk) + 4 in
      let held = ref [] and refused = ref None in
      (try
         for _ = 1 to most do
           match
             raw.create_buffer Ogpu.Types.Device_local
               { Ogpu.Types.label = None; size = chunk; usage = [ Ogpu.Types.Storage ] }
           with
           | Ok resource -> held := resource :: !held
           | Error error ->
               refused := Some error;
               raise Exit
         done
       with Exit -> ());
      (match !refused with
      | Some { Ogpu.Error.kind = Capacity; _ } -> ()
      | Some error -> failwith ("out of device memory is not Capacity: " ^ Ogpu.Error.to_string error)
      | None -> failwith "no VK_ERROR_OUT_OF_DEVICE_MEMORY within three working sets of buffers");
      List.iter
        (fun (resource : Ogpu.Backend.driver_resource) ->
          match resource.destroy () with Ok () -> () | Error e -> failwith (Ogpu.Error.to_string e))
        !held;
      (match raw.destroy_device () with Ok () -> () | Error e -> failwith (Ogpu.Error.to_string e));
      if Ogpu.Impl.live_handles () <> 0 then failwith "out-of-memory test leaked handles";
      if Vulkan.validation_messages () <> 0 then failwith "out-of-memory test: validation messages";
      print_endline "OGPU Vulkan: device-local exhaustion is a typed Capacity error"
