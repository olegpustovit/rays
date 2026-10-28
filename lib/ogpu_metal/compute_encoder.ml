let create (context : Command_control.t) =
  let operation = context.Command_control.operation in
  let native = context.native in
  let open_encoder = context.open_encoder in
  let native_of = context.native_of in
  let recording = context.recording in
  let keep = context.keep in
  let keep_buffer = context.keep_buffer in
  let find_buffer = context.find_buffer in
  let find_accel = context.find_accel in
  let keep_accel = context.keep_accel in
  let resource = context.resource in
  let error = Command_control.error in
  match recording () with
  | Error _ as failure -> failure
  | Ok () -> (
      let created = native_of (Metal.Compute_encoder.create native) in
      match created with
      | Error _ as failure -> failure
      | Ok encoder ->
          open_encoder := Some (fun () -> ignore (Metal.Compute_encoder.end_encoding encoder));
          Ok
            {
              Ogpu_core.Backend.set_pipeline =
                (fun token ->
                  match context.pipeline token with
                  | None ->
                      error operation Ogpu_core.Error.Invalid_argument
                        "pipeline graph is incomplete"
                  | Some pipeline -> (
                      match Pipeline.Private.native pipeline with
                      | Render _ ->
                          error operation Ogpu_core.Error.Invalid_argument
                            "render pipeline cannot execute compute"
                      | Compute compute -> (
                          match
                            keep
                              (fun () -> Pipeline.Private.retain_submission pipeline)
                              (fun () -> Pipeline.Private.release_submission pipeline)
                          with
                          | Error _ as failure -> failure
                          | Ok () -> native_of (Metal.Compute_encoder.set_pipeline encoder compute))
                      ));
              set_buffer =
                (fun ~index ~offset token ->
                  match find_buffer token with
                  | Error _ as failure -> failure
                  | Ok buffer -> (
                      match keep_buffer buffer with
                      | Error _ as failure -> failure
                      | Ok () ->
                          native_of
                            (Metal.Compute_encoder.set_buffer encoder ~index ~offset
                               (Buffer.Private.metal buffer))));
              set_bytes =
                (fun ~index bytes ->
                  native_of (Metal.Compute_encoder.set_bytes encoder ~index bytes));
              set_texture =
                (fun ~index token ->
                  match resource token with
                  | Some (Command_control.Texture texture) -> (
                      match
                        keep
                          (fun () -> Texture.Private.retain_submission texture)
                          (fun () -> Texture.Private.release_submission texture)
                      with
                      | Error _ as failure -> failure
                      | Ok () ->
                          native_of
                            (Metal.Compute_encoder.set_texture encoder ~index
                               (Texture.Private.metal texture)))
                  | _ ->
                      error operation Ogpu_core.Error.Invalid_argument "texture graph is incomplete");
              set_accel =
                (fun ~index token ->
                  match find_accel token with
                  | Error _ as failure -> failure
                  | Ok structure -> (
                      match keep_accel structure with
                      | Error _ as failure -> failure
                      | Ok () ->
                          native_of
                            (Metal.Compute_encoder.set_acceleration_structure encoder ~index
                               (Some (Acceleration.Private.metal structure)))));
              dispatch_threads =
                (fun ~threads ~threadgroup ->
                  native_of (Metal.Compute_encoder.dispatch_threads encoder ~threads ~threadgroup));
              compute_use_accels =
                (fun tokens ->
                  let rec collect acc = function
                    | [] -> Ok (List.rev acc)
                    | token :: rest -> (
                        match find_accel token with
                        | Error _ as failure -> failure
                        | Ok structure -> (
                            match keep_accel structure with
                            | Error _ as failure -> failure
                            | Ok () -> collect (Acceleration.Private.metal structure :: acc) rest))
                  in
                  match collect [] tokens with
                  | Error _ as failure -> failure
                  | Ok [] -> Ok ()
                  | Ok structures ->
                      native_of
                        (Metal.Compute_encoder.use_acceleration_structures encoder structures));
              end_compute =
                (fun () ->
                  open_encoder := None;
                  native_of (Metal.Compute_encoder.end_encoding encoder));
            })
