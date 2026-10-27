type resource = Buffer of Buffer.t | Texture of Texture.t

let color_format (descriptor : Ogpu_core.Types.texture_descriptor) = descriptor.format

type icb_entry = {
  icb : Metal.Indirect_command_buffer.t;
  icb_commands : Metal.Indirect_command_buffer.Render_command.t option array;
}

type active_queue = { presentations : Surface.Private.pending_presentation array }

type table_entry = {
  table : Metal.Intersection_function_table.t;
  mutable table_handles : Metal.Function_handle.t list;
  mutable table_uses : int;
  mutable table_destroy_requested : bool;
}

(* Objects retained by in-flight commands: destruction while used is
   deferred to the last release. *)

(* Encoders reuse depth-stencil states by value; the least recently used is
   released once more than [depth_state_capacity] distinct states exist. *)
module Depth_state_table = Lru.Make (struct
  type t = Ogpu_core.Backend.depth_state
  let equal = ( = )
  let hash = Hashtbl.hash
end)

let depth_state_capacity = 64

type control = {
  resources : (int64, resource) Hashtbl.t;

  pipeline_tokens : (int64, Pipeline.t) Hashtbl.t;
  tables : (int64, table_entry) Hashtbl.t;
  active_queues : (int64, active_queue) Hashtbl.t;
  libraries : (int64, Library.t) Hashtbl.t;
  accels : (int64, Acceleration.t) Hashtbl.t;
  sampler_tokens : (int64, Sampler.t) Hashtbl.t;
  icbs : (int64, icb_entry) Hashtbl.t;
  arguments : (int64, Metal.Shader_argument_encoder.t) Hashtbl.t;
  depth_states : Metal.Depth_stencil.t Depth_state_table.t;
  acquired_frames : (int64, Surface.t * Surface.frame) Hashtbl.t;
  mutable cleanup_error : Ogpu_core.Error.t option;
  mutable device_live : bool;
  mutable next : int64;
}

let error op kind text = Error (Ogpu_core.Error.make op kind text)

let token c =
  let x = c.next in
  c.next <- Int64.succ x;
  x

let pending_presentation_capacity = 3

let metal_compare = function
  | Ogpu_core.Render_pass.Never -> Metal.Depth_stencil.Never
  | Less -> Less | Equal -> Equal | Less_equal -> Less_equal | Greater -> Greater
  | Not_equal -> Not_equal | Greater_equal -> Greater_equal | Always -> Always

let create () =
  let c =
    {
      resources = Hashtbl.create 32;

      pipeline_tokens = Hashtbl.create 16;
      tables = Hashtbl.create 4;
      active_queues = Hashtbl.create 4;
      libraries = Hashtbl.create 4;
      accels = Hashtbl.create 8;
      sampler_tokens = Hashtbl.create 8;
      icbs = Hashtbl.create 8;
      arguments = Hashtbl.create 4;
      depth_states = Depth_state_table.create depth_state_capacity ~release:(fun _ native -> ignore (Metal.Depth_stencil.destroy native));
      acquired_frames = Hashtbl.create 4;
      cleanup_error = None;
      device_live = false;
      next = 1L;
    }
  in
  let record_ogpu_cleanup error =
    if Option.is_none c.cleanup_error then c.cleanup_error <- Some error
  in
  let record_cleanup failure =
    Option.iter
      (fun error ->
        record_ogpu_cleanup (Device.of_metal_error ~operation:"Ogpu_metal.Backend.cleanup" error))
      failure
  in
  let take_cleanup_error () =
    let failure = c.cleanup_error in
    c.cleanup_error <- None;
    failure
  in
  let create_device () =
    if c.device_live then
      error "Ogpu_metal.Backend.create_device" Ogpu_core.Error.Invalid_state
        "adapter already owns a live device"
    else
      match Device.system_default () with
      | Error _ as e -> e
      | Ok device ->
          c.device_live <- true;
          let buffer_memory = function
            | Ogpu_core.Types.Shared -> Buffer.Shared
            | Ogpu_core.Types.Device_local -> Buffer.Device_local
          in
          let register_buffer created =
            match created with
            | Error _ as e -> e
            | Ok buffer ->
                let id = token c in
                Hashtbl.add c.resources id (Buffer buffer);
                let read offset length = Buffer.read_bytes device buffer ~offset ~length in
                Ok
                  {
                    Ogpu_core.Backend.token = id;
                    write =
                      (fun offset bytes ->
                        Buffer.write_bytes device buffer ~dst_offset:offset bytes);
                    read;
                    read_into =
                      (fun offset destination destination_offset length ->
                        match read offset length with
                        | Error _ as e -> e
                        | Ok bytes ->
                            if
                              destination_offset < 0
                              || length > Bytes.length destination - destination_offset
                            then
                              error "Ogpu_metal.Backend.buffer.read_into"
                                Ogpu_core.Error.Invalid_argument "destination range is invalid"
                            else (
                              Bytes.blit bytes 0 destination destination_offset length;
                              Ok ()));
                    destroy =
                      (fun () ->
                        match Buffer.destroy buffer with
                        | Error _ as e -> e
                        | Ok () ->
                            Hashtbl.remove c.resources id;
                            Ok ());
                  }
          in
          let create_buffer memory descriptor =
            register_buffer (Buffer.create device ~memory:(buffer_memory memory) descriptor)
          in
          let texture_memory ~host_read (descriptor : Ogpu_core.Types.texture_descriptor) =
            if descriptor.sample_count = 1 && host_read then Texture.Shared else Device_local
          in
          let register_texture ~host_read (descriptor : Ogpu_core.Types.texture_descriptor) created =
            match created with
            | Error _ as e -> e
            | Ok texture ->
                let id = token c in
                Hashtbl.add c.resources id (Texture texture);
                let read offset length =
                  if not host_read then
                    error "Ogpu_metal.Backend.depth.read" Ogpu_core.Error.Unsupported
                      "depth textures are not host readable"
                  else if
                    offset <> 0L || descriptor.height <= 0 || length mod descriptor.height <> 0
                  then
                    error "Ogpu_metal.Backend.texture.read" Ogpu_core.Error.Invalid_argument
                      "texture read range is invalid"
                  else
                    Texture.read_bytes device texture ~mip_level:0
                      ~bytes_per_row:(length / descriptor.height)
                in
                let read_into offset destination destination_offset length =
                  if not host_read then
                    error "Ogpu_metal.Backend.depth.read_into" Ogpu_core.Error.Unsupported
                      "depth textures are not host readable"
                  else if
                    offset <> 0L || destination_offset <> 0
                    || length <> Bytes.length destination
                    || descriptor.height <= 0
                    || length mod descriptor.height <> 0
                  then
                    error "Ogpu_metal.Backend.texture.read_into" Ogpu_core.Error.Invalid_argument
                      "destination must exactly match the texture read range"
                  else
                    Texture.read_bytes_into device texture ~mip_level:0
                      ~bytes_per_row:(length / descriptor.height) ~destination
                in
                Ok
                  {
                    Ogpu_core.Backend.token = id;
                    write =
                      (fun _ _ ->
                        error "Ogpu_metal.Backend.texture.write" Ogpu_core.Error.Unsupported
                          "use a transfer pass for textures");
                    read;
                    read_into;
                    destroy =
                      (fun () ->
                        match Texture.destroy texture with
                        | Error _ as e -> e
                        | Ok () ->
                            Hashtbl.remove c.resources id;
                            Ok ());
                  }
          in
          let create_texture_format ~format ~host_read descriptor =
            register_texture ~host_read descriptor
              (Texture.create device ~memory:(texture_memory ~host_read descriptor) ~format descriptor)
          in
          let create_texture descriptor =
            create_texture_format ~format:(color_format descriptor) ~host_read:true descriptor
          in
          let create_depth_texture descriptor =
            create_texture_format ~format:Texture.Depth32_float ~host_read:false descriptor
          in
          let native_resource token = Hashtbl.find_opt c.resources token in
          let finish_table_destroy entry =
            let operation = "Ogpu_metal.Backend.destroy_table" in
            (* The table holds a dependent on every function handle it binds,
               so it goes first and releases them for destruction. *)
            let destroyed = Metal.Intersection_function_table.destroy entry.table in
            let handles = entry.table_handles in
            entry.table_handles <- [];
            let released =
              List.fold_left
                (fun result handle ->
                  match (result, Metal.Function_handle.destroy handle) with
                  | Ok (), Error e -> Error (Device.of_metal_error ~operation e)
                  | result, _ -> result)
                (Ok ()) handles
            in
            match (destroyed, released) with
            | Error e, _ -> Error (Device.of_metal_error ~operation e)
            | Ok (), (Error _ as e) -> e
            | Ok (), Ok () -> Ok ()
          in
          let create_table pipeline ~capacity =
            let operation = "Ogpu_metal.Backend.create_table" in
            match Pipeline.Private.native pipeline with
            | Render _ ->
                error operation Ogpu_core.Error.Invalid_argument
                  "function tables belong to compute pipelines"
            | Compute compute -> (
                match Metal.Intersection_function_table.create ~pipeline:compute ~capacity with
                | Error e -> Error (Device.of_metal_error ~operation e)
                | Ok table ->
                    let id = token c in
                    let entry =
                      { table; table_handles = []; table_uses = 0
                      ; table_destroy_requested = false }
                    in
                    Hashtbl.add c.tables id entry;
                    let handle_of name =
                      match Pipeline.Private.linked_function pipeline name with
                      | None ->
                          error operation Ogpu_core.Error.Invalid_argument
                            "function is not linked into the table's pipeline"
                      | Some function_ -> (
                          match Metal.Function_handle.create ~pipeline:compute ~function_ with
                          | Error e -> Error (Device.of_metal_error ~operation e)
                          | Ok handle ->
                              entry.table_handles <- handle :: entry.table_handles;
                              Ok handle)
                    in
                    Ok
                      {
                        Ogpu_core.Backend.table_token = id;
                        table_set_function =
                          (fun ~index name ->
                            match handle_of name with
                            | Error _ as e -> e
                            | Ok handle ->
                                let set = Metal.Intersection_function_table.set_function table ~index (Some handle) in
                                Result.map_error (Device.of_metal_error ~operation) set);
                        table_set_buffer =
                          (fun ~index buffer_token ~offset ->
                            match native_resource buffer_token with
                            | Some (Buffer buffer) ->
                                Result.map_error (Device.of_metal_error ~operation)
                                  (Metal.Intersection_function_table.set_buffer table ~index ~offset
                                     (Some (Buffer.Private.metal buffer)))
                            | _ ->
                                error operation Ogpu_core.Error.Invalid_argument
                                  "table buffer graph is incomplete");
                        destroy_table =
                          (fun () ->
                            if entry.table_uses > 0 then begin
                              entry.table_destroy_requested <- true;
                              Hashtbl.remove c.tables id;
                              Ok ()
                            end
                            else
                              match finish_table_destroy entry with
                              | Error _ as e -> e
                              | Ok () ->
                                  Hashtbl.remove c.tables id;
                                  Ok ());
                      })
          in
          let own_pipeline pipeline =
            let id = token c in
            Hashtbl.add c.pipeline_tokens id pipeline;
            Ok
              {
                Ogpu_core.Backend.pipeline_token = id;
                create_table = create_table pipeline;
                destroy_pipeline =
                  (fun () ->
                    match Pipeline.destroy pipeline with
                    | Error _ as failure -> failure
                    | Ok () ->
                        Hashtbl.remove c.pipeline_tokens id;
                        Ok ());
              }
          in
          let create_library shader =
            match Library.create device shader with
            | Error _ as failure -> failure
            | Ok library ->
                let id = token c in
                Hashtbl.add c.libraries id library;
                Ok
                  {

                    Ogpu_core.Backend.create_compute_pipeline_in =
                      (fun ~entry ~constants ~interface ~linked ->
                        match
                          Pipeline.create_compute_from_library ~linked device library ~entry ~constants
                            ~interface
                        with
                        | Error _ as failure -> failure
                        | Ok pipeline -> own_pipeline pipeline);
                    destroy_library =
                      (fun () ->
                        match Library.destroy library with
                        | Error _ as failure -> failure
                        | Ok () ->
                            Hashtbl.remove c.libraries id;
                            Ok ());
                  }
          in
          let create_accel descriptor =
            let operation = "Ogpu_metal.Backend.create_accel" in
            let resolve_buffer token =
              match native_resource token with
              | Some (Buffer buffer) -> Ok buffer
              | _ ->
                  error operation Ogpu_core.Error.Invalid_argument
                    "acceleration buffer graph is incomplete"
            in
            let resolve_structure token =
              match Hashtbl.find_opt c.accels token with
              | Some structure -> Ok structure
              | None ->
                  error operation Ogpu_core.Error.Invalid_argument
                    "acceleration structure graph is incomplete"
            in
            match Acceleration.create device descriptor ~resolve_buffer ~resolve_structure with
            | Error _ as failure -> failure
            | Ok structure ->
                let id = token c in
                Hashtbl.add c.accels id structure;
                let sizes = Acceleration.sizes structure in
                Ok
                  {
                    Ogpu_core.Backend.accel_token = id;
                    accel_sizes =
                      {
                        structure_size = sizes.acceleration_structure_size;
                        build_scratch_size = sizes.build_scratch_buffer_size;

                      };
                    destroy_accel =
                      (fun () ->
                        match Acceleration.destroy structure with
                        | Error _ as failure -> failure
                        | Ok () ->
                            Hashtbl.remove c.accels id;
                            Ok ());
                  }
          in
          let instance_layout kind =
            let layout =
              Metal.Acceleration_structure.Build.instance_layout
                (match kind with
                 | Ogpu_core.Backend.User_id_instances -> Metal.Acceleration_structure.Build.User_id_instances
                 | Motion_instances -> Motion_instances)
            in
            [| layout.size; layout.transform; layout.options; layout.mask; layout.table_offset
             ; layout.structure_index; layout.user_id; layout.transforms_start; layout.transforms_count
             ; layout.start_border_offset; layout.end_border_offset; layout.start_time_offset
             ; layout.end_time_offset |]
          in
          let create_sampler descriptor =
            match Sampler.create device descriptor with
            | Error _ as failure -> failure
            | Ok sampler ->
                let id = token c in
                Hashtbl.add c.sampler_tokens id sampler;
                Ok
                  {
                    Ogpu_core.Backend.sampler_token = id;
                    destroy_sampler =
                      (fun () ->
                        match Sampler.destroy sampler with
                        | Error _ as failure -> failure
                        | Ok () ->
                            Hashtbl.remove c.sampler_tokens id;
                            Ok ());
                  }
          in
          let create_render_pipeline (options : Ogpu_core.Backend.render_pipeline_options)
              descriptor =
            let primitive_topology =
              match options.topology with
              | Ogpu_core.Render_pass.Point_list -> Metal.Render_pipeline.Point
              | _ -> Metal.Render_pipeline.Triangle
            in
            match
              Pipeline.create_render_owned ~indirect:options.indirect ~primitive_topology
                ~blend:options.blend device descriptor
            with
            | Error _ as failure -> failure
            | Ok pipeline -> own_pipeline pipeline
          in
          let create_icb ~max_commands =
            let operation = "Ogpu_metal.Backend.create_icb" in
            let descriptor =
              Metal.Indirect_command_buffer.descriptor ~inherit_buffers:false
                ~inherit_pipeline_state:false ~max_vertex_buffer_bind_count:8
                ~max_fragment_buffer_bind_count:8
                ~command_types:
                  [ Metal.Indirect_command_buffer.Indirect_draw; Indirect_draw_indexed ]
                ()
            in
            match
              Metal.Indirect_command_buffer.create ~device:(Device.Private.metal device)
                ~max_command_count:max_commands descriptor
            with
            | Error e -> Error (Device.of_metal_error ~operation e)
            | Ok icb ->
                let id = token c in
                let entry = { icb; icb_commands = Array.make max_commands None } in
                Hashtbl.add c.icbs id entry;
                let native_of = function
                  | Error e -> Error (Device.of_metal_error ~operation e)
                  | Ok x -> Ok x
                in
                let command index =
                  match entry.icb_commands.(index) with
                  | Some command -> Ok command
                  | None -> (
                      match Metal.Indirect_command_buffer.Render_command.at icb index with
                      | Error e -> Error (Device.of_metal_error ~operation e)
                      | Ok command ->
                          entry.icb_commands.(index) <- Some command;
                          Ok command)
                in
                let buffer token =
                  match native_resource token with
                  | Some (Buffer buffer) -> Ok buffer
                  | _ ->
                      error operation Ogpu_core.Error.Invalid_argument
                        "buffer graph is incomplete"
                in
                let primitive = function
                  | Ogpu_core.Render_pass.Point_list ->
                      Metal.Indirect_command_buffer.Render_command.Point
                  | Line_list -> Line
                  | Triangle_list -> Triangle
                  | Triangle_strip -> Triangle_strip
                in
                Ok
                  {
                    Ogpu_core.Backend.icb_token = id;

                    icb_set_pipeline =
                      (fun ~index token ->
                        match Hashtbl.find_opt c.pipeline_tokens token with
                        | None ->
                            error operation Ogpu_core.Error.Invalid_argument
                              "pipeline graph is incomplete"
                        | Some pipeline -> (
                            match Pipeline.Private.native pipeline with
                            | Compute _ ->
                                error operation Ogpu_core.Error.Invalid_argument
                                  "compute pipeline cannot draw"
                            | Render native -> (
                                match command index with
                                | Error _ as failure -> failure
                                | Ok command ->
                                    native_of
                                      (Metal.Indirect_command_buffer.Render_command.set_pipeline
                                         command native))));
                    icb_set_buffer =
                      (fun ~index stage ~slot ~offset token ->
                        match (command index, buffer token) with
                        | Error e, _ | _, Error e -> Error e
                        | Ok command, Ok buffer ->
                            (match stage with
                             | Ogpu_core.Backend.Vertex ->
                                 native_of (Metal.Indirect_command_buffer.Render_command.set_vertex_buffer command ~index:slot ~offset (Buffer.Private.metal buffer))
                             | Fragment ->
                                 native_of (Metal.Indirect_command_buffer.Render_command.set_fragment_buffer command ~index:slot ~offset (Buffer.Private.metal buffer))));
                    icb_draw =
                      (fun ~index ~primitive:kind ~first ~count ~instances ->
                        match command index with
                        | Error _ as failure -> failure
                        | Ok command ->
                            native_of
                              (Metal.Indirect_command_buffer.Render_command.draw_primitives
                                 command ~primitive:(primitive kind) ~vertex_start:first
                                 ~vertex_count:count ~instance_count:instances ()));
                    icb_draw_indexed =
                      (fun ~index ~primitive:kind ~index_type token ~offset ~count ~instances ->
                        match (command index, buffer token) with
                        | Error e, _ | _, Error e -> Error e
                        | Ok command, Ok index_buffer ->
                            native_of
                              (Metal.Indirect_command_buffer.Render_command.draw_indexed command
                                 ~primitive:(primitive kind)
                                 ~index_type:
                                   (match index_type with
                                   | Ogpu_core.Render_pass.Uint16 -> Uint16
                                   | Uint32 -> Uint32)
                                 ~index_buffer:(Buffer.Private.metal index_buffer)
                                 ~index_offset:offset ~index_count:count
                                 ~instance_count:(Int64.of_int instances) ()));
                    destroy_icb =
                      (fun () ->
                        Array.iteri
                          (fun index command ->
                            Option.iter
                              (fun command ->
                                ignore
                                  (Metal.Indirect_command_buffer.Render_command.destroy command);
                                entry.icb_commands.(index) <- None)
                              command)
                          entry.icb_commands;
                        match Metal.Indirect_command_buffer.destroy icb with
                        | Error e -> Error (Device.of_metal_error ~operation e)
                        | Ok () ->
                            Hashtbl.remove c.icbs id;
                            Ok ());
                  }
          in
          let create_argument ~pipeline stage ~index =
            let operation = "Ogpu_metal.Backend.create_argument" in
            match Hashtbl.find_opt c.pipeline_tokens pipeline with
            | None ->
                error operation Ogpu_core.Error.Invalid_argument "pipeline graph is incomplete"
            | Some pipeline -> (
                match stage with
                | Ogpu_core.Backend.Vertex ->
                    error operation Ogpu_core.Error.Unsupported
                      "argument encoders are exposed for fragment functions"
                | Fragment -> (
                    match Pipeline.Private.argument_encoder pipeline ~buffer_index:(Int64.of_int index) with
                    | Error _ as failure -> failure
                    | Ok encoder ->
                        let id = token c in
                        Hashtbl.add c.arguments id encoder;
                        let native_of = function
                          | Error e -> Error (Device.of_metal_error ~operation e)
                          | Ok x -> Ok x
                        in
                        (* A Metal argument encoder keeps its last-set resources
                           alive; encode each slice through a short-lived
                           encoder so evicted textures can be released. *)
                        let encode buffer offset resource =
                          match native_resource buffer with
                          | Some (Buffer buffer) -> (
                              match
                                Pipeline.Private.argument_encoder pipeline
                                  ~buffer_index:(Int64.of_int index)
                              with
                              | Error _ as failure -> failure
                              | Ok scratch ->
                                  let finish result =
                                    ignore (Metal.Shader_argument_encoder.destroy scratch);
                                    native_of result
                                  in
                                  (match
                                     Metal.Shader_argument_encoder.set_argument_buffer scratch
                                       (Buffer.Private.metal buffer) ~offset ()
                                   with
                                  | Error _ as failure -> finish failure
                                  | Ok () -> finish (resource scratch)))
                          | _ ->
                              error operation Ogpu_core.Error.Invalid_argument
                                "buffer graph is incomplete"
                        in
                        Ok
                          {

                            Ogpu_core.Backend.argument_length =
                              Int64.to_int (Metal.Shader_argument_encoder.encoded_length encoder);
                            argument_alignment =
                              Int64.to_int (Metal.Shader_argument_encoder.alignment encoder);
                            argument_texture =
                              (fun ~buffer ~offset ~slot texture ->
                                match native_resource texture with
                                | Some (Texture texture) ->
                                    encode buffer offset (fun scratch ->
                                        Metal.Shader_argument_encoder.set scratch
                                          ~index:(Int64.of_int slot)
                                          (Metal.Shader_argument_encoder.Texture
                                             (Texture.Private.metal texture)))
                                | _ ->
                                    error operation Ogpu_core.Error.Invalid_argument
                                      "texture graph is incomplete");
                            argument_sampler =
                              (fun ~buffer ~offset ~slot sampler ->
                                match Hashtbl.find_opt c.sampler_tokens sampler with
                                | Some sampler ->
                                    encode buffer offset (fun scratch ->
                                        Metal.Shader_argument_encoder.set scratch
                                          ~index:(Int64.of_int slot)
                                          (Metal.Shader_argument_encoder.Sampler
                                             (Sampler.Private.metal sampler)))
                                | None ->
                                    error operation Ogpu_core.Error.Invalid_argument
                                      "sampler graph is incomplete");
                            destroy_argument =
                              (fun () ->
                                match Metal.Shader_argument_encoder.destroy encoder with
                                | Error e -> Error (Device.of_metal_error ~operation e)
                                | Ok () ->
                                    Hashtbl.remove c.arguments id;
                                    Ok ());
                          }))
          in
          let depth_state_for (state : Ogpu_core.Backend.depth_state) =
            let operation = "Ogpu_metal.Backend.depth_state" in
            match Depth_state_table.find c.depth_states state with
            | native -> Ok native
            | exception Not_found ->
                (match
                   Metal.Depth_stencil.create ~label:"ogpu-metal-depth-state"
                     ~depth_compare:(metal_compare state.depth_compare)
                     ~depth_write:state.depth_write
                     (Device.Private.metal device) ()
                 with
                 | Error e -> Error (Device.of_metal_error ~operation e)
                 | Ok native ->
                     Depth_state_table.add c.depth_states state native;
                     Ok native)
          in
          let create_queue () =
            match Queue.create device with
            | Error _ as e -> e
            | Ok queue ->
                let queue_token = token c in
                let active =
                  {
                    presentations =
                      Array.init pending_presentation_capacity (fun _ ->
                          Surface.Private.create_pending_presentation ());
                  }
                in
                Hashtbl.add c.active_queues queue_token active;
                let commit_completed_epoch _epoch completion =
                  let cleanup = take_cleanup_error () in
                  match (completion, cleanup) with
                  | (Error _ as e), _ -> e
                  | Ok (), Some error -> Error error
                  | Ok (), None -> Ok ()
                in
                let complete_through epoch =
                  let waited = Queue.wait_through queue epoch in
                  let completed = Queue.completed_epoch queue in
                  Surface.Private.complete_presentations_through active.presentations completed;
                  commit_completed_epoch epoch waited
                in
                let poll_through epoch =
                  let polled = Queue.poll_through queue epoch in
                  let completed = Queue.completed_epoch queue in
                  if epoch > 0L && completed >= epoch then begin
                    Surface.Private.complete_presentations_through active.presentations completed;
                    Result.map
                      (fun () -> true)
                      (commit_completed_epoch completed (Result.map (fun _ -> ()) polled))
                  end
                  else polled
                in
                let begin_commands () =
                  let operation = "Ogpu_metal.Backend.begin_commands" in
                  let native_of = function
                    | Error e -> Error (Device.of_metal_error ~operation e)
                    | Ok x -> Ok x
                  in
                  match Metal.Command_buffer.create (Queue.Private.metal queue) () with
                  | Error e -> Error (Device.of_metal_error ~operation e)
                  | Ok native ->
                      let commands_token = token c in
                      let retained = ref [] in
                      let open_encoder : (unit -> unit) option ref = ref None in
                      let finished = ref false in
                      let keep retain release =
                        match retain () with
                        | Error _ as failure -> failure
                        | Ok () ->
                            retained := release :: !retained;
                            Ok ()
                      in
                      (* One retain per buffer per command buffer, however many
                         bindings reference it. *)
                      let keep_buffer buffer =
                        match Buffer.Private.retain_for ~commands:commands_token buffer with
                        | Error _ as failure -> failure
                        | Ok false -> Ok ()
                        | Ok true ->
                            retained := (fun () -> Buffer.Private.release_submission buffer) :: !retained;
                            Ok ()
                      in
                      let find_buffer token =
                        match native_resource token with
                        | Some (Buffer buffer) -> Ok buffer
                        | _ ->
                            error operation Ogpu_core.Error.Invalid_argument
                              "buffer graph is incomplete"
                      in
                      let find_accel token =
                        match Hashtbl.find_opt c.accels token with
                        | Some structure -> Ok structure
                        | None ->
                            error operation Ogpu_core.Error.Invalid_argument
                              "acceleration structure graph is incomplete"
                      in
                      let keep_accel structure =
                        match Acceleration.Private.retain_submission structure with
                        | Error _ as failure -> failure
                        | Ok release ->
                            retained := release :: !retained;
                            Ok ()
                      in
                      let recording () =
                        if !finished then
                          error operation Ogpu_core.Error.Invalid_state "commands are finished"
                        else Ok ()
                      in
                      let accel_encoder () =
                        match recording () with
                        | Error _ as failure -> failure
                        | Ok () -> (
                            match Metal.Acceleration_encoder.create native with
                            | Error e -> Error (Device.of_metal_error ~operation e)
                            | Ok encoder ->
                                open_encoder :=
                                  Some
                                    (fun () ->
                                      ignore (Metal.Acceleration_encoder.end_encoding encoder));
                                let operate encode token ~scratch ~scratch_offset =
                                  match find_accel token with
                                  | Error _ as failure -> failure
                                  | Ok structure -> (
                                      match find_buffer scratch with
                                      | Error _ as failure -> failure
                                      | Ok scratch -> (
                                          match keep_accel structure with
                                          | Error _ as failure -> failure
                                          | Ok () -> (
                                              match keep_buffer scratch with
                                              | Error _ as failure -> failure
                                              | Ok () ->
                                                  encode encoder device structure ~scratch
                                                    ~scratch_offset)))
                                in
                                let pair encode ~src ~dst =
                                  match (find_accel src, find_accel dst) with
                                  | Error e, _ | _, Error e -> Error e
                                  | Ok src, Ok dst -> (
                                      match (keep_accel src, keep_accel dst) with
                                      | Error e, _ | _, Error e -> Error e
                                      | Ok (), Ok () -> encode encoder device ~src ~dst)
                                in
                                Ok
                                  {
                                    Ogpu_core.Backend.build = operate Acceleration.encode_build;

                                    compact = pair Acceleration.encode_compact;
                                    write_compacted_size =
                                      (fun token ~dst ~offset ->
                                        match (find_accel token, find_buffer dst) with
                                        | Error e, _ | _, Error e -> Error e
                                        | Ok structure, Ok destination -> (
                                            match (keep_accel structure, keep_buffer destination) with
                                            | Error e, _ | _, Error e -> Error e
                                            | Ok (), Ok () ->
                                                Acceleration.encode_compacted_size encoder device
                                                  structure ~destination ~offset));
                                    end_accel =
                                      (fun () ->
                                        open_encoder := None;
                                        native_of (Metal.Acceleration_encoder.end_encoding encoder));
                                  })
                      in
                      let find_texture token =
                        match native_resource token with
                        | Some (Texture texture) -> Ok texture
                        | _ ->
                            error operation Ogpu_core.Error.Invalid_argument
                              "texture graph is incomplete"
                      in
                      let keep_texture texture =
                        keep
                          (fun () -> Texture.Private.retain_submission texture)
                          (fun () -> Texture.Private.release_submission texture)
                      in
                      let encoder_context =
                        { Command_control.operation; device; native; native_of; open_encoder; recording;
                          keep; keep_buffer; find_buffer; find_accel; keep_accel; find_texture; keep_texture;
                          resource = (fun token ->
                            match native_resource token with
                            | Some (Buffer buffer) -> Some (Command_control.Buffer buffer)
                            | Some (Texture texture) -> Some (Command_control.Texture texture)
                            | None -> None);
                          pipeline = (fun token -> Hashtbl.find_opt c.pipeline_tokens token);
                          sampler = (fun token -> Hashtbl.find_opt c.sampler_tokens token);
                          table = (fun token ->
                            match Hashtbl.find_opt c.tables token with
                            | None -> error operation Ogpu_core.Error.Invalid_argument
                                "function table graph is incomplete"
                            | Some entry ->
                                (match keep
                                  (fun () -> entry.table_uses <- entry.table_uses + 1; Ok ())
                                  (fun () ->
                                    entry.table_uses <- entry.table_uses - 1;
                                    if entry.table_uses = 0 && entry.table_destroy_requested then
                                      ignore (finish_table_destroy entry))
                                with
                                | Error _ as failure -> failure
                                | Ok () -> Ok entry.table));
                          icb = (fun token -> Option.map (fun entry -> entry.icb) (Hashtbl.find_opt c.icbs token));
                          depth_state = depth_state_for;
                        }
                      in
                      let compute_encoder () = Compute_encoder.create encoder_context in
                      let blit_encoder () = Blit_encoder.create encoder_context in
                      let render_encoder target = Render_encoder.create encoder_context target in
                      let commit_present ~source (frame : Ogpu_core.Backend.driver_frame) =
                        match recording () with
                        | Error _ as failure -> failure
                        | Ok () -> (
                            match (find_texture source, Hashtbl.find_opt c.acquired_frames frame.frame_token) with
                            | Error e, _ -> Error e
                            | _, None ->
                                error operation Ogpu_core.Error.Invalid_state "frame token is stale"
                            | Ok source, Some (surface, native_frame) -> (
                                match
                                  Array.find_opt Surface.Private.pending_presentation_available
                                    active.presentations
                                with
                                | None ->
                                    error operation Ogpu_core.Error.Capacity
                                      "bounded presentation slots are all in flight"
                                | Some pending -> (
                                    match Surface.Private.prepare_present pending surface native_frame ~source with
                                    | Error _ as failure -> failure
                                    | Ok () -> (
                                        match Surface.Private.presentation_encoder pending native with
                                        | Error _ as failure ->
                                            Surface.Private.rollback_present pending;
                                            failure
                                        | Ok () -> (
                                            match Queue.submit_native queue native ~retained:!retained with
                                            | Error _ as failure ->
                                                Surface.Private.rollback_present pending;
                                                failure
                                            | Ok (receipt : Queue.receipt) ->
                                                finished := true;
                                                retained := [];
                                                Surface.Private.commit_present pending ~epoch:receipt.epoch;
                                                Hashtbl.remove c.acquired_frames frame.frame_token;
                                                Ok { Ogpu_core.Backend.epoch = receipt.epoch })))))
                      in
                      let release_all () =
                        List.iter (fun release -> release ()) !retained;
                        retained := []
                      in
                      let commit () =
                        match recording () with
                        | Error _ as failure -> failure
                        | Ok () -> (
                            match Queue.submit_native queue native ~retained:!retained with
                            | Error _ as failure -> failure
                            | Ok (receipt : Queue.receipt) ->
                                finished := true;
                                retained := [];
                                Ok { Ogpu_core.Backend.epoch = receipt.epoch })
                      in
                      let abandon () =
                        match recording () with
                        | Error _ as failure -> failure
                        | Ok () ->
                            Option.iter (fun end_open -> end_open ()) !open_encoder;
                            open_encoder := None;
                            finished := true;
                            release_all ();
                            native_of (Metal.Command_buffer.destroy native)
                      in
                      Ok
                        {

                          Ogpu_core.Backend.compute_encoder;
                          accel_encoder;
                          blit_encoder;
                          render_encoder;

                          commit;
                          commit_present;
                          abandon;
                        }
                in
                let gpu_duration epoch = Queue.gpu_duration queue epoch in
                let gpu_timing () =
                  let timing = Queue.gpu_timing_for_device device in
                  {
                    Ogpu_core.Backend.gpu_seconds = timing.duration_seconds }
                in
                let destroy_queue () =
                  if
                    not
                      (Array.for_all Surface.Private.pending_presentation_available
                         active.presentations)
                  then
                    error "Ogpu_metal.Backend.destroy_queue" Ogpu_core.Error.Invalid_state
                      "queue has presentations in flight"
                  else
                    match Queue.destroy queue with
                    | Error _ as e -> e
                    | Ok () -> (
                        Surface.Private.clear_pending_presentations active.presentations;
                        Hashtbl.remove c.active_queues queue_token;
                        match take_cleanup_error () with None -> Ok () | Some error -> Error error)
                in
                Ok
                  {

                    Ogpu_core.Backend.complete_through;
                    poll_through;
                    completed_epoch = (fun () -> Queue.completed_epoch queue);
                    begin_commands;

                    gpu_duration;
                    gpu_timing;
                    destroy_queue;
                  }
          in
          let create_surface (configuration : Ogpu_core.Surface.configuration) =
            let adopted =
              match configuration.layer with
              | Some token -> (
                  match
                    Metal.Metal_layer.adopt_borrowed (Device.Private.metal device) token
                      (Metal.Metal_layer.default ~width:configuration.physical_width
                         ~height:configuration.physical_height)
                  with
                  | Error e -> Error (Device.of_metal_error ~operation:"Ogpu_metal.Backend.create_surface" e)
                  | Ok layer -> Ok (Some layer))
              | None -> Ok None
            in
            match adopted with
            | Error _ as e -> e
            | Ok adopted -> (
            match adopted with
            | None ->
                error "Ogpu_metal.Backend.create_surface" Ogpu_core.Error.Unsupported
                  "surface needs a native layer token"
            | Some layer -> (
                let release_adopted () =
                  Option.iter (fun layer -> ignore (Metal.Metal_layer.destroy layer)) adopted
                in
                match Surface.create device ~layer configuration with
                | Error _ as e -> release_adopted (); e
                | Ok surface ->
                    let frames_of_surface () =
                      Hashtbl.fold
                        (fun _ (owner, _) count -> if owner == surface then count + 1 else count)
                        c.acquired_frames 0
                    in
                    let find_frame token =
                      Option.map snd (Hashtbl.find_opt c.acquired_frames token)
                    in
                    let acquire_with acquire =
                      match acquire surface with
                      | Error _ as e -> e
                      | Ok Surface.Timeout -> Ok `Timeout
                      | Ok Occluded -> Ok `Occluded
                      | Ok Device_lost -> Ok `Device_lost
                      | Ok (Acquired frame) ->
                          let frame_token = Surface.frame_id frame in
                          Hashtbl.add c.acquired_frames frame_token (surface, frame);
                          Ok (`Acquired { Ogpu_core.Backend.frame_token })
                    in
                    let _acquire () = acquire_with Surface.acquire
                    and acquire_sync () = acquire_with Surface.Private.acquire_scoped in
                    let take f frame =
                      match find_frame frame.Ogpu_core.Backend.frame_token with
                      | None ->
                          error "Ogpu_metal.Backend.surface" Ogpu_core.Error.Invalid_state
                            "frame token is stale"
                      | Some native -> (
                          match f surface native with
                          | Error _ as e -> e
                          | Ok () ->
                              Hashtbl.remove c.acquired_frames frame.frame_token;
                              Ok ())
                    in
                    Ok
                      {

                        Ogpu_core.Backend.configure = (fun x -> Surface.configure surface x);

                        acquire_sync;
                        discard = take Surface.discard;
                        destroy_surface =
                          (fun () ->
                            if frames_of_surface () <> 0 then
                              error "Ogpu_metal.Backend.destroy_surface"
                                Ogpu_core.Error.Invalid_state "surface has outstanding frames"
                            else if Surface.in_flight_presentations surface <> 0 then
                              error "Ogpu_metal.Backend.destroy_surface"
                                Ogpu_core.Error.Invalid_state "surface has presentations in flight"
                            else (
                              Surface.destroy surface;
                              release_adopted ();
                              Ok ()));
                      }))
          in
          let destroy_device () =
            if Hashtbl.length c.active_queues <> 0 then
              error "Ogpu_metal.Backend.destroy_device" Ogpu_core.Error.Invalid_state
                "device has active queues"
            else begin
              let states = ref [] in
              Depth_state_table.iter c.depth_states (fun state _ -> states := state :: !states);
              List.iter
                (fun state ->
                  match Depth_state_table.take c.depth_states state with
                  | Some native -> (
                      match Metal.Depth_stencil.destroy native with
                      | Ok () -> ()
                      | Error error -> record_cleanup (Some error))
                  | None -> ())
                !states;
              let destroyed = if c.device_live then Device.destroy device else Ok () in
              (match destroyed with
              | Ok () ->
                  c.device_live <- false
              | Error _ -> ());
              let cleanup = take_cleanup_error () in
              match (destroyed, cleanup) with
              | (Error _ as failure), _ -> failure
              | Ok (), Some error -> Error error
              | Ok (), None -> Ok ()
            end
          in
          Ok
            {

              Ogpu_core.Backend.device_handle = Device.Private.handle device;
              capabilities = Device.capabilities device;
              create_buffer;
              create_texture;
              create_depth_texture;
              create_library;
              create_accel;
              instance_layout;
              create_sampler;
              create_render_pipeline;
              create_icb;
              create_argument;
              create_queue;
              create_surface;

              destroy_device;
            }
  in
  { Ogpu_core.Backend.create_device }
