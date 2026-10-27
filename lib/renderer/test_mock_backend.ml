let () =
  if Ogpu.Impl.backend <> "mock" then failwith "mock renderer selected the wrong OGPU implementation";
  match Renderer.create_offscreen Renderer.default_configuration with
  | Error error when error.Ogpu.Error.kind = Ogpu.Error.Unsupported ->
      print_endline "renderer: mock backend returns typed unsupported (no shader fallback)"
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)
  | Ok renderer ->
      ignore (Renderer.destroy renderer);
      failwith "renderer silently created a mock shader path"
