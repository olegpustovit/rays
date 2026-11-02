let () =
  if Ogpu.Impl.backend <> "mock" then
    failwith "mock renderer selected the wrong OGPU implementation";
  match Renderer.create_offscreen Renderer.default_configuration with
  | Error error when error.Ogpu.Error.kind = Ogpu.Error.Unsupported ->
      print_endline "renderer: mock backend returns typed unsupported (no shader fallback)"
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)
  | Ok renderer ->
      ignore (Renderer.destroy renderer);
      failwith "renderer silently created a mock shader path"

(* the mock is a discrete device: a driver caps live allocations, as Vulkan's 4,096 *)
let () =
  let module B = Ogpu.Backend in
  let ok = function Ok v -> v | Error e -> failwith (Ogpu.Error.to_string e) in
  let device = ok (B.create_device (Ogpu.Impl.create_driver ())) in
  let descriptor = { Ogpu.Types.label = None; size = 16L; usage = [ Ogpu.Types.Storage ] } in
  let held = List.init 4096 (fun _ -> ok (B.create_buffer device descriptor)) in
  (match B.create_buffer device descriptor with
  | Error { Ogpu.Error.kind = Capacity; _ } -> ()
  | _ -> failwith "the 4,097th live allocation was accepted");
  List.iter (fun b -> ignore (B.destroy_buffer b)) held;
  ignore (B.destroy_buffer (ok (B.create_buffer device descriptor)));
  ignore (B.destroy_device device);
  print_endline "mock backend: 4,096 live allocations at most, then a typed Capacity error"
