let metal_cull = function
  | Ogpu_core.Render_pass.Cull_none -> Metal.Render_encoder.No_cull
  | Cull_front -> Cull_front
  | Cull_back -> Cull_back

let metal_stage = function
  | Ogpu_core.Backend.Vertex -> Metal.Render_encoder.Vertex
  | Fragment -> Fragment

let metal_primitive = function
  | Ogpu_core.Render_pass.Point_list -> Metal.Render_encoder.Point
  | Line_list -> Line
  | Triangle_list -> Triangle
  | Triangle_strip -> Triangle_strip

let metal_index = function
  | Ogpu_core.Render_pass.Uint16 -> Metal.Render_encoder.Uint16
  | Uint32 -> Uint32

let render_pipeline (context : Command_control.t) token =
  match context.pipeline token with
  | None ->
      Command_control.error context.operation Ogpu_core.Error.Invalid_argument
        "pipeline graph is incomplete"
  | Some pipeline -> (
      match Pipeline.Private.native pipeline with
      | Compute _ ->
          Command_control.error context.operation Ogpu_core.Error.Invalid_argument
            "compute pipeline cannot draw"
      | Render native ->
          Result.map
            (fun () -> native)
            (context.keep
               (fun () -> Pipeline.Private.retain_submission pipeline)
               (fun () -> Pipeline.Private.release_submission pipeline)))

let draw_batch (context : Command_control.t) encoder render_pipeline draws =
  let operation = context.operation in
  let device = context.device in
  let native_of = context.native_of in
  let find_buffer = context.find_buffer in
  let keep_buffer = context.keep_buffer in
  let all_indexed =
    Array.for_all
      (fun (d : Ogpu_core.Backend.driver_batch_draw) ->
        Option.is_some d.batch_index && d.batch_instances = 1)
      draws
  in
  let exception Unresolved of Ogpu_core.Error.t in
  let get = function Ok x -> x | Error e -> raise (Unresolved e) in
  let resolve (d : Ogpu_core.Backend.driver_batch_draw) =
    let native = get (render_pipeline d.batch_pipeline) in
    let buffers =
      Array.map
        (fun (stage, slot, token, offset) ->
          let buffer = get (find_buffer token) in
          get (keep_buffer buffer);
          (stage, slot, buffer, offset))
        d.batch_buffers
    in
    let index =
      Option.map
        (fun (kind, token, offset, count) ->
          let index_buffer = get (find_buffer token) in
          get (keep_buffer index_buffer);
          (kind, index_buffer, offset, count))
        d.batch_index
    in
    (native, buffers, index)
  in
  match Array.map resolve draws with
  | exception Unresolved e -> Error e
  | resolved ->
      if all_indexed then begin
        let prepared =
          Array.map2
            (fun (d : Ogpu_core.Backend.driver_batch_draw) (native, buffers, index) ->
              let kind, index_buffer, offset, count = Option.get index in
              ({
                 Metal.Render_encoder.Private.prepared_pipeline = native;
                 prepared_bindings =
                   Array.map
                     (fun (stage, slot, buffer, offset) ->
                       {
                         Metal.Render_encoder.Private.prepared_stage = metal_stage stage;
                         prepared_index = slot;
                         prepared_offset = offset;
                         prepared_buffer = Buffer.Private.metal buffer;
                       })
                     buffers;
                 prepared_primitive = metal_primitive d.batch_primitive;
                 prepared_index_type = metal_index kind;
                 prepared_index_buffer = Buffer.Private.metal index_buffer;
                 prepared_index_offset = offset;
                 prepared_index_count = count;
               }
                : Metal.Render_encoder.Private.prepared_indexed_draw))
            draws resolved
        in
        match
          Metal.Render_encoder.Private.prepare_indexed_draws (Device.Private.metal device) prepared
        with
        | Error e -> Error (Device.of_metal_error ~operation e)
        | Ok prepared ->
            native_of (Metal.Render_encoder.Private.execute_prepared_indexed_draws encoder prepared)
      end
      else
        let failed e = Error (Device.of_metal_error ~operation e) in
        let bind (stage, slot, buffer, offset) =
          let native = Buffer.Private.metal buffer in
          match stage with
          | Ogpu_core.Backend.Vertex ->
              Metal.Render_encoder.set_vertex_buffer encoder ~index:slot ~offset native
          | Fragment -> Metal.Render_encoder.set_fragment_buffer encoder ~index:slot ~offset native
        in
        let rec bind_all buffers slot =
          if slot = Array.length buffers then Ok ()
          else
            match bind (Array.unsafe_get buffers slot) with
            | Error e -> failed e
            | Ok () -> bind_all buffers (slot + 1)
        in
        let rec each index previous =
          if index = Array.length draws then Ok ()
          else
            let d = draws.(index) in
            let native, buffers, indexed = resolved.(index) in
            match
              if match previous with Some p -> p == native | None -> false then Ok ()
              else Metal.Render_encoder.set_pipeline encoder native
            with
            | Error e -> failed e
            | Ok () -> (
                match bind_all buffers 0 with
                | Error _ as failure -> failure
                | Ok () -> (
                    match
                      match indexed with
                      | None ->
                          Metal.Render_encoder.draw_primitives encoder
                            ~primitive:(metal_primitive d.batch_primitive)
                            ~first:d.batch_vertex_start ~count:d.batch_vertex_count
                            ~instances:d.batch_instances ()
                      | Some (kind, index_buffer, offset, count) ->
                          Metal.Render_encoder.draw_indexed_instances encoder
                            ~primitive:(metal_primitive d.batch_primitive)
                            ~index_type:(metal_index kind)
                            ~index_buffer:(Buffer.Private.metal index_buffer)
                            ~index_offset:offset ~index_count:count
                            ~instances:(Int64.of_int d.batch_instances)
                    with
                    | Error e -> failed e
                    | Ok () ->
                        each (index + 1)
                          (match previous with
                          | Some p when p == native -> previous
                          | _ -> Some native)))
        in
        each 0 None

let use_resources (context : Command_control.t) encoder tokens =
  let split_token (buffers, textures) token =
    match context.resource token with
    | Some (Command_control.Buffer buffer) ->
        Result.map
          (fun () ->
            (Metal.Render_encoder.Buffer_resource (Buffer.Private.metal buffer) :: buffers, textures))
          (context.keep_buffer buffer)
    | Some (Command_control.Texture texture) ->
        Result.map
          (fun () ->
            ( buffers,
              Metal.Render_encoder.Texture_resource (Texture.Private.metal texture) :: textures ))
          (context.keep_texture texture)
    | None ->
        Command_control.error context.operation Ogpu_core.Error.Invalid_argument
          "resource graph is incomplete"
  in
  let rec gather acc = function
    | [] -> Ok acc
    | token :: rest -> Result.bind (split_token acc token) (fun acc -> gather acc rest)
  in
  match gather ([], []) tokens with
  | Error _ as failure -> failure
  | Ok (buffers, textures) ->
      let ( let* ) = Result.bind in
      let* () =
        if buffers = [] then Ok ()
        else
          context.native_of
            (Metal.Render_encoder.use_resources encoder (List.rev buffers)
               ~usage:[ Metal.Render_encoder.Read ]
               ~stages:[ Metal.Render_encoder.Vertex; Fragment ])
      in
      if textures = [] then Ok ()
      else
        context.native_of
          (Metal.Render_encoder.use_resources encoder (List.rev textures)
             ~usage:[ Metal.Render_encoder.Sample ] ~stages:[ Metal.Render_encoder.Fragment ])

let create (context : Command_control.t) target =
  let operation = context.Command_control.operation in
  let native = context.native in
  let open_encoder = context.open_encoder in
  let native_of = context.native_of in
  let recording = context.recording in
  let keep = context.keep in
  let keep_buffer = context.keep_buffer in
  let find_buffer = context.find_buffer in
  let find_texture = context.find_texture in
  let keep_texture = context.keep_texture in
  let depth_state_for = context.depth_state in
  let error = Command_control.error in
  let render_encoder (target : Ogpu_core.Backend.driver_render_target) =
    match recording () with
    | Error _ as failure -> failure
    | Ok () -> (
        let color = target.target_colors.(0) in
        let optional f = function
          | None -> Ok None
          | Some token -> Result.map Option.some (f token)
        in
        match
          ( find_texture color.color,
            optional find_texture color.color_resolve,
            optional
              (fun (d : Ogpu_core.Backend.driver_depth_attachment) -> find_texture d.depth)
              target.target_depth )
        with
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
        | Ok color_texture, Ok resolve, Ok depth -> (
            let retained_all =
              List.fold_left
                (fun result texture -> Result.bind result (fun () -> keep_texture texture))
                (Ok ())
                (color_texture :: List.filter_map Fun.id [ resolve; depth ])
            in
            match retained_all with
            | Error _ as failure -> failure
            | Ok () -> (
                match
                  Metal.Render_pass_descriptor.create ~width:target.target_width
                    ~height:target.target_height ~sample_count:target.target_samples ()
                with
                | Error e -> Error (Device.of_metal_error ~operation e)
                | Ok pass -> (
                    let fail e =
                      ignore (Metal.Render_pass_descriptor.destroy pass);
                      Error (Device.of_metal_error ~operation e)
                    in
                    let load = function
                      | Ogpu_core.Render_pass.Dont_care ->
                          Metal.Render_pass_descriptor.Load_dont_care
                      | Load -> Load
                      | Clear -> Clear
                    and store = function
                      | Ogpu_core.Render_pass.Store | Resolve -> Metal.Render_pass_descriptor.Store
                      | Discard -> Store_dont_care
                    in
                    let configured =
                      let ( let* ) = Result.bind in
                      let* () =
                        Metal.Render_pass_descriptor.set_attachments pass
                          ~color:(Texture.Private.metal color_texture)
                          ~clear:color.color_clear
                          ?depth:(Option.map Texture.Private.metal depth)
                          ()
                      in
                      let* () =
                        Metal.Render_pass_descriptor.set_color_load_action pass
                          (load color.color_load)
                      in
                      let keep = color.color_store = Ogpu_core.Render_pass.Store in
                      let* () =
                        match resolve with
                        | None ->
                            Metal.Render_pass_descriptor.set_color_store_action pass ~store:keep
                              ~resolve:false
                        | Some resolve ->
                            let* () =
                              Metal.Render_pass_descriptor.set_resolve_texture pass
                                (Some (Texture.Private.metal resolve))
                            in
                            Metal.Render_pass_descriptor.set_color_store_action pass ~store:keep
                              ~resolve:true
                      in
                      let depth_actions =
                        match target.target_depth with
                        | None ->
                            ( Metal.Render_pass_descriptor.Clear,
                              Metal.Render_pass_descriptor.Store,
                              1. )
                        | Some d -> (load d.depth_load, store d.depth_store, d.depth_clear)
                      in
                      Metal.Render_pass_descriptor.set_depth_stencil_actions pass
                        ~depth:depth_actions
                        ~stencil:
                          (Metal.Render_pass_descriptor.Clear, Metal.Render_pass_descriptor.Store, 0)
                    in
                    match configured with
                    | Error e -> fail e
                    | Ok () -> (
                        match Metal.Render_encoder.Private.create_from_pass_scoped native pass with
                        | Error e -> fail e
                        | Ok encoder ->
                            let finish () =
                              open_encoder := None;
                              let ended = Metal.Render_encoder.end_encoding encoder in
                              ignore (Metal.Render_pass_descriptor.destroy pass);
                              native_of ended
                            in
                            open_encoder := Some (fun () -> ignore (finish ()));
                            let resolve_pipeline token = render_pipeline context token in
                            Ok
                              {
                                Ogpu_core.Backend.render_set_pipeline =
                                  (fun token ->
                                    match resolve_pipeline token with
                                    | Error _ as failure -> failure
                                    | Ok native ->
                                        native_of (Metal.Render_encoder.set_pipeline encoder native));
                                set_stage_buffer =
                                  (fun stage ~index ~offset token ->
                                    match find_buffer token with
                                    | Error _ as failure -> failure
                                    | Ok buffer -> (
                                        match keep_buffer buffer with
                                        | Error _ as failure -> failure
                                        | Ok () ->
                                            native_of
                                              (match stage with
                                              | Ogpu_core.Backend.Vertex ->
                                                  Metal.Render_encoder.set_vertex_buffer encoder
                                                    ~index ~offset (Buffer.Private.metal buffer)
                                              | Fragment ->
                                                  Metal.Render_encoder.set_fragment_buffer encoder
                                                    ~index ~offset (Buffer.Private.metal buffer))));
                                set_stage_texture =
                                  (fun stage ~index token ->
                                    match find_texture token with
                                    | Error _ as failure -> failure
                                    | Ok texture -> (
                                        match keep_texture texture with
                                        | Error _ as failure -> failure
                                        | Ok () ->
                                            native_of
                                              (match stage with
                                              | Ogpu_core.Backend.Vertex ->
                                                  Metal.Render_encoder.set_vertex_texture encoder
                                                    ~index (Texture.Private.metal texture)
                                              | Fragment ->
                                                  Metal.Render_encoder.set_fragment_texture encoder
                                                    ~index (Texture.Private.metal texture))));
                                set_stage_sampler =
                                  (fun stage ~index token ->
                                    match context.sampler token with
                                    | None ->
                                        error operation Ogpu_core.Error.Invalid_argument
                                          "sampler graph is incomplete"
                                    | Some sampler -> (
                                        match
                                          keep
                                            (fun () -> Sampler.Private.retain_submission sampler)
                                            (fun () -> Sampler.Private.release_submission sampler)
                                        with
                                        | Error _ as failure -> failure
                                        | Ok () ->
                                            native_of
                                              (match stage with
                                              | Ogpu_core.Backend.Vertex ->
                                                  Metal.Render_encoder.set_vertex_sampler encoder
                                                    ~index (Sampler.Private.metal sampler)
                                              | Fragment ->
                                                  Metal.Render_encoder.set_fragment_sampler encoder
                                                    ~index (Sampler.Private.metal sampler))));
                                set_viewport =
                                  (fun (rect : Ogpu_core.Render_pass.rect) ->
                                    native_of
                                      (Metal.Render_encoder.set_viewport encoder
                                         {
                                           x = float rect.x;
                                           y = float rect.y;
                                           width = float rect.width;
                                           height = float rect.height;
                                           znear = 0.;
                                           zfar = 1.;
                                         }));
                                set_scissor =
                                  (fun (rect : Ogpu_core.Render_pass.rect) ->
                                    native_of
                                      (Metal.Render_encoder.set_scissor encoder
                                         {
                                           x = rect.x;
                                           y = rect.y;
                                           width = rect.width;
                                           height = rect.height;
                                         }));
                                set_cull =
                                  (fun cull ->
                                    native_of
                                      (Metal.Render_encoder.set_cull_mode encoder (metal_cull cull)));
                                set_winding =
                                  (fun winding ->
                                    native_of
                                      (Metal.Render_encoder.set_front_facing_winding encoder
                                         (match winding with
                                         | Ogpu_core.Backend.Clockwise ->
                                             Metal.Render_encoder.Clockwise
                                         | Counter_clockwise -> Counter_clockwise)));
                                set_depth_state =
                                  (function
                                  | None ->
                                      native_of
                                        (Metal.Render_encoder.set_depth_stencil_state encoder None)
                                  | Some state -> (
                                      match depth_state_for state with
                                      | Error _ as failure -> failure
                                      | Ok native ->
                                          native_of
                                            (Metal.Render_encoder.set_depth_stencil_state encoder
                                               (Some native))));
                                draw =
                                  (fun ~primitive ~first ~count ~instances ->
                                    native_of
                                      (Metal.Render_encoder.draw_primitives encoder
                                         ~primitive:(metal_primitive primitive) ~first ~count
                                         ~instances ()));
                                draw_indexed =
                                  (fun ~primitive ~index_type token ~offset ~count ~instances ->
                                    match find_buffer token with
                                    | Error _ as failure -> failure
                                    | Ok buffer -> (
                                        match keep_buffer buffer with
                                        | Error _ as failure -> failure
                                        | Ok () ->
                                            let index_buffer = Buffer.Private.metal buffer in
                                            native_of
                                              (if instances = 1 then
                                                 Metal.Render_encoder.draw_indexed_basic encoder
                                                   ~primitive:(metal_primitive primitive)
                                                   ~index_type:(metal_index index_type)
                                                   ~index_buffer ~index_offset:offset
                                                   ~index_count:count
                                               else
                                                 Metal.Render_encoder.draw_indexed_instances encoder
                                                   ~primitive:(metal_primitive primitive)
                                                   ~index_type:(metal_index index_type)
                                                   ~index_buffer ~index_offset:offset
                                                   ~index_count:count
                                                   ~instances:(Int64.of_int instances))));
                                draw_indexed_indirect =
                                  (fun ~primitive
                                    ~index_type
                                    token
                                    ~offset
                                    arguments_token
                                    ~arguments_offset
                                  ->
                                    match (find_buffer token, find_buffer arguments_token) with
                                    | (Error _ as failure), _ | _, (Error _ as failure) -> failure
                                    | Ok buffer, Ok arguments -> (
                                        match (keep_buffer buffer, keep_buffer arguments) with
                                        | (Error _ as failure), _ | _, (Error _ as failure) ->
                                            failure
                                        | Ok (), Ok () ->
                                            native_of
                                              (Metal.Render_encoder.draw_indexed_indirect encoder
                                                 ~primitive:(metal_primitive primitive)
                                                 ~index_type:(metal_index index_type)
                                                 ~index_buffer:(Buffer.Private.metal buffer)
                                                 ~index_offset:offset
                                                 ~arguments:(Buffer.Private.metal arguments)
                                                 ~arguments_offset)));
                                draw_batch =
                                  (fun draws -> draw_batch context encoder resolve_pipeline draws);
                                use_resources = (fun tokens -> use_resources context encoder tokens);
                                execute_icb =
                                  (fun token ~location ~length ->
                                    match context.icb token with
                                    | None ->
                                        error operation Ogpu_core.Error.Invalid_argument
                                          "indirect command buffer graph is incomplete"
                                    | Some entry ->
                                        native_of
                                          (Metal.Render_encoder.execute_indirect_commands encoder
                                             entry ~location ~length));
                                end_render = finish;
                              })))))
  in
  render_encoder target
