let convert ?(operation = "Ogpu_metal.Device.system_default") source kind =
  Ogpu_metal_native.Device.Private.error_kind ~operation ~source kind

let () =
  let source = "Metal.Device.system_default" in
  assert (convert source Metal.Native_error = Ogpu_core.Error.No_adapter);
  assert (convert "Metal.Device.registry_id" Metal.Native_error = Ogpu_core.Error.Device_lost);
  assert (convert "Metal.Buffer.write" Metal.Native_error = Ogpu_core.Error.Device_lost);
  assert (
    convert ~operation:"Ogpu_metal.Buffer.write" source Metal.Native_error
    = Ogpu_core.Error.Device_lost);
  List.iter
    (fun (native, expected) -> assert (convert source native = expected))
    [
      (Metal.Invalid_argument, Ogpu_core.Error.Invalid_argument);
      (Invalid_state, Invalid_state);
      (Parent_has_dependents, Invalid_state);
      (Destroyed, Stale_handle);
      (Device_mismatch, Cross_device);
      (Unsupported, Unsupported);
      (Wrong_domain, Device_lost);
    ];
  print_endline
    "OGPU default adapter classification preserves registry and other typed native failures"
