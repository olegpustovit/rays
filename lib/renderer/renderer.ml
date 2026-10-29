module Scene_execution = Scene_execution
open Scene_execution
open Renderer_lowering
module Command = Scene_command.Render_ir

type error = Ogpu.Error.t

let pp_error formatter value = Format.pp_print_string formatter (Ogpu.Error.to_string value)
let fail operation kind message = Error (Ogpu.Error.make operation kind message)
let backend _operation value = Error value

let resource operation value =
  Error
    (Ogpu.Error.make operation Ogpu.Error.Invalid_state
       (Format.asprintf "%a" Runtime_resources.pp_error value))

type configuration = {
  logical_width : int;
  logical_height : int;
  drawable_width : int;
  drawable_height : int;
  title : string;
  vsync : bool;
  high_density : bool;
}

let default_configuration =
  {
    logical_width = 640;
    logical_height = 480;
    drawable_width = 640;
    drawable_height = 480;
    title = "Rays";
    vsync = true;
    high_density = true;
  }

type family = Scene_execution.pipeline_family =
  | Scene2
  | Scene2_textured
  | Scene3
  | Scene3_points
  | Scene3_textured
  | Scene3_world
  | Ui
  | Marks

type blend = Ogpu.Pipeline.blend = Replace | Alpha | Add | Multiply | Screen | Subtract
type draw = Scene_execution.sampled_draw

type gpu = {
  gpu_device : Ogpu.Backend.device;
  gpu_queue : Ogpu.Backend.queue;
  gpu_owner : gpu_owner;
  gpu_shared : bool;
  mutable gpu_released : bool;
}

(* A [Marks]-family source: the six attribute buffers of one mark set. A host attribute is
   uploaded into a buffer the source owns; a device attribute is another producer's output,
   bound directly and valid only while that producer's output is current. *)
and marks_source = Host of bytes * int | Device of (unit -> Ogpu.Backend.buffer option) * int

and gpu_marks = {
  marks_id : int;
  marks_gpu : gpu;
  marks_domain : Domain.id;
  mutable marks_closed : bool;
  mutable marks_stamp : int64;
  mutable marks_count : int;
  marks_owned : Ogpu.Backend.buffer option array;
  marks_last : bytes array;
  mutable marks_device : ((unit -> Ogpu.Backend.buffer option) * Ogpu.Backend.buffer) list;
  mutable marks_registration : Scene_execution.Private.gpu_vertices option;
}

and gpu_owner = {
  scene_vertices : Scene_execution.Private.gpu_vertices_owner;
  marks_registry : (int, gpu_marks) Hashtbl.t;
  mutable shared_leases : int;
}

let gpu_marks_capacity = 64
let next_gpu_marks = Atomic.make 1

let fresh_gpu_marks_id () =
  let rec loop () =
    let current = Atomic.get next_gpu_marks in
    if current = max_int then None
    else if Atomic.compare_and_set next_gpu_marks current (current + 1) then Some current
    else loop ()
  in
  loop ()

let create_gpu_owner () =
  {
    scene_vertices = Scene_execution.Private.create_gpu_vertices_owner ();
    marks_registry = Hashtbl.create 16;
    shared_leases = 0;
  }

let valid_gpu_marks value =
  (not value.marks_closed)
  && (not value.marks_gpu.gpu_released)
  && value.marks_domain = Domain.self ()
  && List.for_all
       (fun (source, input) ->
         match source () with Some current -> current == input | None -> false)
       value.marks_device

let gpu_marks_key value = Printf.sprintf "marks:%d:%Ld" value.marks_id value.marks_stamp

let unregister_marks value =
  Option.iter Scene_execution.Private.unregister_gpu_vertices value.marks_registration;
  value.marks_registration <- None

let close_gpu_marks value =
  if value.marks_domain <> Domain.self () then invalid_arg "GPU marks closed on another domain";
  if not value.marks_closed then begin
    value.marks_closed <- true;
    unregister_marks value;
    value.marks_device <- [];
    Hashtbl.remove value.marks_gpu.gpu_owner.marks_registry value.marks_id;
    Array.iteri
      (fun i ->
        Option.iter (fun b ->
            ignore (Ogpu.Backend.destroy_buffer b);
            value.marks_owned.(i) <- None))
      value.marks_owned
  end

module Structural_key (T : sig
  type t
end) =
struct
  type t = T.t

  let equal = ( = )
  let hash = Hashtbl.hash
end

type resource = Renderer_retained_plans.resource =
  | Image of Runtime_resources.Image.t
  | Text of Runtime_resources.Text.t
  | Canvas of Runtime_resources.Canvas.t

type cached_scene2_plan = Renderer_retained_plans.plan
type retained_scene2_segment = Renderer_retained_plans.segment

let prepared_draw ~family ?(blend = Replace) ?texture ?auxiliary ?vertex_attributes ?(samples = 1)
    value =
  { Scene_execution.family; blend; texture; auxiliary; vertex_attributes; samples; draw = value }

type presentation_facts = Runtime.presentation_facts
type submission_state = Open | Closed

type lease_policy = Renderer_leases.policy =
  | Copy_image_snapshots
  | Retain_image_snapshots of Renderer_leases.t

exception Resource_resolver_raised of exn

type snapshot_cache_entry = {
  mutable snapshot_generation : int;
  snapshot_width : int;
  snapshot_height : int;
  mutable snapshot_texture : Scene_execution.sampled_texture;
}

(* key, density *)
module Snapshot_table = Lru.Make (Structural_key (struct
  type t = string * int
end))

let snapshot_cache_capacity = 256
let snapshot_cache_byte_capacity = 64 * 1024 * 1024

(* Full-window snapshots change frequently and already have bounded double
   buffers in [Image].  Keeping entries below 1 MiB admits UI/text assets but
   deliberately leaves a 640x480 RGBA frame (1,228,800 bytes) transient. *)
let snapshot_cache_entry_byte_capacity = Renderer_lowering.snapshot_cache_entry_byte_capacity
let sampled_texture_bytes = Renderer_lowering.sampled_texture_bytes

(* The last bytes one UI batch position lowered to. Unchanged content reuses
   them, so the executor's mesh cache hits by physical identity instead of
   hashing every instance each frame. *)
(* [ui_source] is the batch the vertices were last compared with: a published batch is immutable, so
   the same one again is unchanged without comparing its bytes, and [ui_vertices] may be its bytes. *)
type stats_counters = { mutable logical_draws : int64; mutable logical_passes : int64 }

let new_stats_counters () = { logical_draws = 0L; logical_passes = 0L }

let count_draws counters count =
  counters.logical_draws <- Int64.add counters.logical_draws (Int64.of_int count);
  counters.logical_passes <- Int64.succ counters.logical_passes

type scaled_cache = {
  mutable scaled_input : Scene_execution.sampled_draw list;
  mutable scaled_facts : Runtime.frame_facts option;
  mutable scaled_output : Scene_execution.sampled_draw list;
}

let new_scaled_cache () = { scaled_input = []; scaled_facts = None; scaled_output = [] }

let scale_required (facts : Runtime.frame_facts) =
  facts.logical_width <> facts.drawable_width || facts.logical_height <> facts.drawable_height

let scale_draw (facts : Runtime.frame_facts) (draw : Scene_execution.geometry_draw) =
  let viewport = Runtime.map_logical_rect facts draw.state.viewport
  and scissor = Runtime.map_logical_rect facts draw.state.scissor in
  if viewport = draw.state.viewport && scissor = draw.state.scissor then draw
  else { draw with state = { draw.state with viewport; scissor } }

let scale_draws facts draws =
  if not (scale_required facts) then draws else List.map (scale_draw facts) draws

let scale_sampled_resources facts draws =
  if not (scale_required facts) then draws
  else
    List.map
      (fun entry ->
        let draw = scale_draw facts entry.Scene_execution.draw in
        if draw == entry.draw then entry else { entry with draw })
      draws

let scale_sampled_cached cache (facts : Runtime.frame_facts) draws =
  if not (scale_required facts) then draws
  else if
    draws == cache.scaled_input
    && match cache.scaled_facts with Some cached -> cached == facts | None -> false
  then cache.scaled_output
  else
    let scaled = scale_sampled_resources facts draws in
    cache.scaled_input <- draws;
    cache.scaled_facts <- Some facts;
    cache.scaled_output <- scaled;
    scaled

type stats = {
  logical_draws : int64;
  logical_passes : int64;
  pipeline_cache_entries : int;
  uploaded_bytes : int64;
  gpu_duration_seconds : float;
  retained_plan_hits : int64;
  retained_plan_misses : int64;
  sun_shadow_passes : int64;
}

let zero_stats =
  {
    logical_draws = 0L;
    logical_passes = 0L;
    pipeline_cache_entries = 0;
    uploaded_bytes = 0L;
    gpu_duration_seconds = 0.;
    retained_plan_hits = 0L;
    retained_plan_misses = 0L;
    sun_shadow_passes = 0L;
  }

type t = {
  runtime : Runtime.t;
  shared_device_lease : bool;
  owns_device_lease : bool;
  gpu_owner : gpu_owner;
  renderer : Scene_execution.t;
  mutable renderer_facts : Runtime.frame_facts;
  counters : stats_counters;
  scaled : scaled_cache;
  assets : Runtime_resources.Assets.t;
  mutable dead : bool;
  snapshots : snapshot_cache_entry Snapshot_table.t;
  lowering : Renderer_lowering.t;
  retained_plans : Renderer_retained_plans.t;
  mutable retained_scene2_segment_hits : int64;
  mutable retained_scene2_segment_misses : int64;
  mutable submissions : submission list;
  mutable last_step_draws : draw list;
  mutable last_step_prepared : Scene_execution.sampled_draw list;
  retained_views : (Scene_execution.prepared_scene3, retained_view) Lru.Identity_list.t;
  retain_after : int ref;
      (* the sights of a layer before it gets a target: [retain_soon], or [retain_live] after a
         target was dropped before it paid for itself (a layer that changes every few frames) *)
}

(* A 3D layer that kept the same prepared identity on consecutive frames: its draws render once
   into an offscreen target the size of the window and later frames composite that texture, so
   a frame that only changes the UI never shades the view again.  The identity is the key
   (prepared layers are immutable), the window's drawable size is checked on every use, and the
   list is capped, so nothing here can outlive or disagree with the scene it stands for. *)
and retained_view = {
  view_layer : Scene_execution.prepared_scene3;
  view_drawable : int * int;
  mutable view_target : retained_target option;
  mutable view_failed : bool;
  mutable view_seen : int; (* sights before its target, then the frames the target served *)
  view_retain_after : int ref; (* its owner's [retain_after] *)
}

and retained_target = {
  target_exec : t;
  target_canvas : Runtime_resources.Canvas.t;
  target_ir : Scene_command.Render_ir.t;
}

and submission = {
  owner : t;
  mutable submission_state : submission_state;
  image_leases : Renderer_leases.t;
  mutable ui_layers : int;
}

and batch = { batch_owner : submission; batch_draws : draw list }

let retained_view_capacity = 4

(* A target costs an offscreen execution (its pipelines, its uploads) and pays back by later frames
   compositing it. A live layer, a new one every few frames (a time-driven SOP), never pays: once a
   target is dropped having served fewer than [retain_live] frames, layers wait [retain_live]
   sights for one, until a target serves that many. ponytail: two fixed thresholds, a measured
   break-even if they misjudge. *)
let retain_soon = 2
let retain_live = 8
let release_retained_view = ref (fun (_ : retained_view) -> ())

(* ponytail: one active native window; pass an explicit renderer token if sketches
   start owning multiple windows concurrently. *)
let active_window : t option ref = ref None

(* Device leases: the active window's device when one exists, otherwise one
   lazily created headless device shared by every lease and released with the
   last one. Offscreen executions (Canvas) and GPU film producers both lease. *)
let headless_gpu : (Ogpu.Backend.device * gpu_owner * int ref) option ref = ref None

let release_headless () =
  match !headless_gpu with
  | Some (device, _, count) ->
      decr count;
      if !count <= 0 then (
        headless_gpu := None;
        ignore (Ogpu.Backend.destroy_device device))
  | None -> ()

let acquire_device operation =
  let shared =
    match !active_window with
    | Some ({ runtime; dead = false; _ } as value) when Runtime.is_window runtime ->
        Some (Ok (Scene_execution.device value.renderer, value.gpu_owner))
    | Some _ | None -> None
  in
  match shared with
  | Some (Ok (device, owner)) ->
      owner.shared_leases <- owner.shared_leases + 1;
      Ok (device, true, owner)
  | Some (Error e) -> backend operation e
  | None -> (
      match !headless_gpu with
      | Some (device, owner, count) ->
          incr count;
          Ok (device, false, owner)
      | None -> (
          let driver = Ogpu.Impl.create_driver () in
          match Ogpu.Backend.create_device driver with
          | Error e -> backend operation e
          | Ok device ->
              let owner = create_gpu_owner () in
              headless_gpu := Some (device, owner, ref 1);
              Ok (device, false, owner)))

let release_device shared owner =
  if shared then owner.shared_leases <- owner.shared_leases - 1 else release_headless ()

let valid_configuration operation (configuration : configuration) =
  let positive x = x > 0 in
  if
    not
      (List.for_all positive
         [
           configuration.logical_width;
           configuration.logical_height;
           configuration.drawable_width;
           configuration.drawable_height;
         ])
  then fail operation Ogpu.Error.Invalid_argument "dimensions must be positive"
  else Ok ()

let finish_create ?(shared_device_lease = false) ?(owns_device_lease = false) gpu_owner runtime
    renderer =
  {
    runtime;
    renderer;
    gpu_owner;
    renderer_facts = Runtime.frame_facts runtime;
    counters = new_stats_counters ();
    scaled = new_scaled_cache ();
    shared_device_lease;
    owns_device_lease;
    assets = Runtime_resources.Assets.create ();
    dead = false;
    snapshots =
      Snapshot_table.create snapshot_cache_capacity ~byte_capacity:snapshot_cache_byte_capacity;
    lowering = Renderer_lowering.create ();
    retained_plans = Renderer_retained_plans.create ();
    retained_scene2_segment_hits = 0L;
    retained_scene2_segment_misses = 0L;
    submissions = [];
    last_step_draws = [];
    last_step_prepared = [];
    retained_views =
      Lru.Identity_list.create ~equal:( == )
        ~release:(fun _ view -> !release_retained_view view)
        retained_view_capacity;
    retain_after = ref retain_soon;
  }

let create (configuration : configuration) =
  let operation = "Renderer.create" in
  match valid_configuration operation configuration with
  | Error _ as error -> error
  | Ok () -> (
      match
        Runtime.create ~vsync:configuration.vsync ~hidden:false
          ~high_density:configuration.high_density ~title:configuration.title
          ~width:configuration.logical_width ~height:configuration.logical_height ()
      with
      | Error e -> backend operation e
      | Ok runtime -> (
          match Runtime.set_resizable runtime true with
          | Error e ->
              ignore (Runtime.destroy runtime);
              backend operation e
          | Ok () -> (
              match Runtime.surface_configuration runtime with
              | Error error ->
                  ignore (Runtime.destroy runtime);
                  backend operation error
              | Ok surface -> (
                  let driver = Ogpu.Impl.create_driver () in
                  let gpu_owner = create_gpu_owner () in
                  match
                    Renderer_pipeline.create_renderer ~backend:Ogpu.Impl.backend
                      ~gpu_vertices:gpu_owner.scene_vertices ~offscreen:false ~driver
                      ~configuration:surface ()
                  with
                  | Error error ->
                      ignore (Runtime.destroy runtime);
                      backend operation error
                  | Ok renderer ->
                      let value = finish_create gpu_owner runtime renderer in
                      active_window := Some value;
                      Ok value))))

let create_offscreen (configuration : configuration) =
  let operation = "Renderer.create_offscreen" in
  match valid_configuration operation configuration with
  | Error _ as error -> error
  | Ok () -> (
      match acquire_device operation with
      | Error _ as error -> error
      | Ok (device, lease_shared, gpu_owner) -> (
          match
            Runtime.create_offscreen ~logical_width:configuration.logical_width
              ~logical_height:configuration.logical_height ~width:configuration.drawable_width
              ~height:configuration.drawable_height ()
          with
          | Error error ->
              release_device lease_shared gpu_owner;
              backend operation error
          | Ok runtime -> (
              match Runtime.surface_configuration runtime with
              | Error error ->
                  ignore (Runtime.destroy runtime);
                  release_device lease_shared gpu_owner;
                  backend operation error
              | Ok surface -> (
                  let driver = Ogpu.Impl.create_driver () in
                  match
                    Renderer_pipeline.create_renderer ~backend:Ogpu.Impl.backend ~device
                      ~gpu_vertices:gpu_owner.scene_vertices ~offscreen:true ~driver
                      ~configuration:surface ()
                  with
                  | Error error ->
                      ignore (Runtime.destroy runtime);
                      release_device lease_shared gpu_owner;
                      backend operation error
                  | Ok renderer ->
                      Ok
                        (finish_create ~shared_device_lease:lease_shared ~owns_device_lease:true
                           gpu_owner runtime renderer)))))

let ensure operation value =
  if value.dead then fail operation Ogpu.Error.Stale_handle "renderer is destroyed" else Ok ()

let sync_runtime_facts operation value =
  match Runtime.sync_facts value.runtime with
  | Error error -> backend operation error
  | Ok () -> (
      let facts = Runtime.frame_facts value.runtime in
      if facts = value.renderer_facts then Ok ()
      else
        match Runtime.surface_configuration value.runtime with
        | Error error -> backend operation error
        | Ok configuration -> (
            match Scene_execution.resize value.renderer configuration with
            | Error error -> backend operation error
            | Ok () ->
                value.renderer_facts <- facts;
                Ok ()))

let close_submission submission =
  if submission.submission_state = Open then begin
    submission.submission_state <- Closed;
    Renderer_leases.release submission.image_leases;
    submission.owner.submissions <- List.filter (( != ) submission) submission.owner.submissions
  end

let begin_submission value =
  match ensure "Renderer.Native.begin_submission" value with
  | Error _ as e -> e
  | Ok () ->
      let submission =
        {
          owner = value;
          submission_state = Open;
          image_leases = Renderer_leases.create ();
          ui_layers = 0;
        }
      in
      value.submissions <- submission :: value.submissions;
      Ok submission

let ensure_submission operation submission =
  if submission.submission_state = Closed then
    fail operation Ogpu.Error.Stale_handle "submission is already closed"
  else ensure operation submission.owner

let stats value =
  match ensure "Renderer.stats" value with
  | Error _ as e -> e
  | Ok () ->
      let renderer = value.renderer in
      let timing = Ogpu.Backend.gpu_timing (Scene_execution.queue renderer)
      and retained = Scene_execution.retained_stats renderer in
      Ok
        {
          logical_draws = value.counters.logical_draws;
          logical_passes = value.counters.logical_passes;
          pipeline_cache_entries = Scene_execution.pipeline_count renderer;
          uploaded_bytes = Scene_execution.upload_bytes renderer;
          gpu_duration_seconds = timing.gpu_seconds;
          retained_plan_hits = retained.plan_hits;
          retained_plan_misses = retained.plan_misses;
          sun_shadow_passes = Scene_execution.sun_shadow_passes renderer;
        }

let presentation_facts value = Runtime.presentation_facts value.runtime

let snapshot value ~lease_policy ~density source =
  let operation = "Renderer.lower_scene2" in
  if density <= 0 then fail operation Ogpu.Error.Invalid_argument "density must be positive"
  else
    let find key generation =
      match Snapshot_table.find value.snapshots (key, density) with
      | entry when entry.snapshot_generation = generation ->
          Some (entry.snapshot_width, entry.snapshot_height, entry.snapshot_texture)
      | _ -> None
      | exception Not_found -> None
    in
    let store key generation width height texture bytes =
      if bytes <= snapshot_cache_entry_byte_capacity then
        Snapshot_table.add value.snapshots ~bytes (key, density)
          {
            snapshot_generation = generation;
            snapshot_width = width;
            snapshot_height = height;
            snapshot_texture = texture;
          }
      else Snapshot_table.remove value.snapshots (key, density)
    in
    let texture_key key generation =
      key ^ ":" ^ string_of_int density ^ ":" ^ string_of_int generation
    in
    let finish ?(copy = true) key generation width height pixels =
      match find key generation with
      | Some cached -> Ok cached
      | None -> (
          let find_key () =
            match Snapshot_table.find value.snapshots (key, density) with
            | entry when entry.snapshot_width = width && entry.snapshot_height = height ->
                Some entry
            | _ -> None
            | exception Not_found -> None
          in
          match find_key () with
          | Some entry ->
              entry.snapshot_generation <- generation;
              let bytes = if copy then Bytes.copy pixels else pixels in
              (* The execution key carries the generation: same-key bytes are
               never re-hashed below, and a new generation recycles the
               same-shape texture with one upload. *)
              let texture =
                {
                  entry.snapshot_texture with
                  key = texture_key key generation;
                  levels = [| { (entry.snapshot_texture.levels.(0)) with bytes } |];
                }
              in
              entry.snapshot_texture <- texture;
              Ok (width, height, texture)
          | None ->
              let sampler : Ogpu.Types.sampler_descriptor =
                {
                  label = Some "scene-image";
                  min_filter = Linear;
                  mag_filter = Linear;
                  mip_filter = No_mip;
                  address_u = Clamp_to_edge;
                  address_v = Clamp_to_edge;
                  lod_min = 0.;
                  lod_max = 0.;
                  max_anisotropy = 1;
                }
              in
              let bytes = if copy then Bytes.copy pixels else pixels in
              let texture : Scene_execution.sampled_texture =
                {
                  key = texture_key key generation;
                  levels = [| { width; height; bytes } |];
                  sampler;
                  gpu = None;
                }
              in
              store key generation width height texture (Bytes.length bytes);
              Ok (width, height, texture))
    in
    match source with
    | Image image -> (
        if Runtime_resources.Image.destroyed image then
          fail operation Ogpu.Error.Stale_handle "image snapshot is destroyed"
        else
          let key = "image:" ^ string_of_int (Runtime_resources.Image.identity image)
          and generation = Runtime_resources.Image.generation image in
          match Runtime_resources.Image.gpu_snapshot image with
          | Error e -> resource operation e
          | Ok (Some (width, height, generation, gpu)) ->
              let sampler : Ogpu.Types.sampler_descriptor =
                {
                  label = Some "scene-image";
                  min_filter = Linear;
                  mag_filter = Linear;
                  mip_filter = No_mip;
                  address_u = Clamp_to_edge;
                  address_v = Clamp_to_edge;
                  lod_min = 0.;
                  lod_max = 0.;
                  max_anisotropy = 1;
                }
              in
              Ok
                ( width,
                  height,
                  {
                    Scene_execution.key =
                      key ^ ":" ^ string_of_int density ^ ":" ^ string_of_int generation;
                    levels = [| { width; height; bytes = Bytes.empty } |];
                    sampler;
                    gpu = Some gpu;
                  } )
          | Ok None -> (
              match find key generation with
              | Some cached -> Ok cached
              | None -> (
                  match Runtime_resources.Image.borrow_snapshot image with
                  | Ok (width, height, borrowed_generation, pixels, lease) ->
                      let sampler : Ogpu.Types.sampler_descriptor =
                        {
                          label = Some "scene-image";
                          min_filter = Linear;
                          mag_filter = Linear;
                          mip_filter = No_mip;
                          address_u = Clamp_to_edge;
                          address_v = Clamp_to_edge;
                          lod_min = 0.;
                          lod_max = 0.;
                          max_anisotropy = 1;
                        }
                      in
                      let cacheable = Bytes.length pixels <= snapshot_cache_entry_byte_capacity in
                      let bytes =
                        if cacheable then
                          Fun.protect
                            ~finally:(fun () -> Runtime_resources.Image.release_snapshot lease)
                            (fun () -> Bytes.copy pixels)
                        else
                          match lease_policy with
                          | Retain_image_snapshots leases ->
                              Renderer_leases.retain leases lease;
                              pixels
                          | Copy_image_snapshots ->
                              Fun.protect
                                ~finally:(fun () -> Runtime_resources.Image.release_snapshot lease)
                                (fun () -> Bytes.copy pixels)
                      in
                      (* The generation is part of the GPU texture key: a replaced image
             (same identity, new pixels) then misses the executor's static
             image fast path and re-uploads into its same-shape texture. *)
                      let texture =
                        {
                          Scene_execution.key =
                            key ^ ":" ^ string_of_int density ^ ":"
                            ^ string_of_int borrowed_generation;
                          levels = [| { width; height; bytes } |];
                          sampler;
                          gpu = None;
                        }
                      in
                      if cacheable then
                        store key borrowed_generation width height texture (Bytes.length bytes);
                      Ok (width, height, texture)
                  | Error e -> resource operation e)))
    | Text text -> (
        (* Text snapshots are immutable and uniquely identified, like images:
         a hit touches no pixels, a miss copies them once. *)
        let key = "text:" ^ string_of_int (Runtime_resources.Text.identity text)
        and generation = Runtime_resources.Text.generation text in
        match if Runtime_resources.Text.destroyed text then None else find key generation with
        | Some cached -> Ok cached
        | None -> (
            match (Runtime_resources.Text.size text, Runtime_resources.Text.borrow_pixels text) with
            | Ok (width, height), Ok pixels -> finish key generation width height pixels
            | Error e, _ | _, Error e -> resource operation e))
    | Canvas canvas -> (
        let key = "canvas:" ^ string_of_int (Runtime_resources.Canvas.identity canvas) in
        (* A canvas rendered on this window's device is sampled in place. *)
        let gpu =
          match Runtime_resources.Canvas.gpu_snapshot canvas with
          | Some (width, height, generation, texture) when Runtime.is_window value.runtime -> (
              let device_id, _ = Ogpu.Backend.texture_driver_token texture in
              match Ok (Scene_execution.device value.renderer) with
              | Ok device when Ogpu.Handle.device_id (Ogpu.Backend.device_handle device) = device_id
                ->
                  Some (width, height, generation, texture)
              | _ -> None)
          | _ -> None
        in
        match gpu with
        | Some (width, height, generation, texture) ->
            let sampler : Ogpu.Types.sampler_descriptor =
              {
                label = Some "scene-image";
                min_filter = Linear;
                mag_filter = Linear;
                mip_filter = No_mip;
                address_u = Clamp_to_edge;
                address_v = Clamp_to_edge;
                lod_min = 0.;
                lod_max = 0.;
                max_anisotropy = 1;
              }
            in
            Ok
              ( width,
                height,
                {
                  Scene_execution.key =
                    key ^ ":" ^ string_of_int density ^ ":" ^ string_of_int generation;
                  levels = [| { width; height; bytes = Bytes.empty } |];
                  sampler;
                  gpu = Some texture;
                } )
        | None -> (
            match Runtime_resources.Canvas.snapshot canvas with
            | Ok (width, height, generation, pixels) ->
                finish ~copy:false key generation width height pixels
            | Error e -> resource operation e))

let lower_scene2_uncached value ~lease_policy ~density ~resource:resolve ir =
  match ensure "Renderer.lower_scene2" value with
  | Error _ as error -> error
  | Ok () ->
      let checkpoint = Renderer_leases.checkpoint lease_policy in
      let rollback () = Renderer_leases.rollback lease_policy checkpoint in
      let resolve_gpu shapes =
        match Scene_command.Ui_batch.marks shapes with
        | None -> Ok None
        | Some token -> (
            let identity = Scene_command.Marks.gpu_identity token in
            match Hashtbl.find_opt value.gpu_owner.marks_registry identity with
            | Some source
              when valid_gpu_marks source
                   && source.marks_stamp = Scene_command.Marks.gpu_stamp token
                   && source.marks_count = Scene_command.Marks.gpu_count token ->
                if source.marks_gpu.gpu_device == Scene_execution.device value.renderer then
                  Ok (Some (gpu_marks_key source))
                else Error "GPU shapes belong to another device"
            | _ -> Error "GPU shapes are stale or closed")
      in
      let facts = Runtime.presentation_facts value.runtime |> Result.get_ok in
      Renderer_lowering.lower_scene2 value.lowering ~facts ~density ~resource:resolve
        ~snapshot:(fun ~density source -> snapshot value ~lease_policy ~density source)
        ~resolve_gpu ~rollback
        (Scene_command.Render_ir.commands_readonly ir)

(* A plan holding a GPU shape token is lowered again every time: the token's
   source can go stale, and only lowering checks that. *)
let has_gpu_shapes commands =
  Array.exists
    (function
      | Scene_command.Render_ir.Shapes shapes -> Scene_command.Ui_batch.marks shapes <> None
      | _ -> false)
    commands

let lower_scene2_with_policy value ~lease_policy ~density ~resource:resolve ir =
  if value.dead then lower_scene2_uncached value ~lease_policy ~density ~resource:resolve ir
  else
    let commands = Scene_command.Render_ir.commands_readonly ir in
    let ir_id = Scene_command.Render_ir.identity ir in
    let facts = presentation_facts value |> Result.get_ok in
    let extent =
      (facts.logical_width, facts.logical_height, facts.drawable_width, facts.drawable_height)
    in
    let same_context (plan : cached_scene2_plan) =
      plan.plan_density = density && plan.plan_extent = extent
    in
    let slow () =
      let fingerprint = ref (Hashtbl.hash commands) and command_count = Array.length commands in
      let mix value = fingerprint := Rays_math.Hash.mix !fingerprint value in
      for index = 0 to command_count - 1 do
        match Array.unsafe_get commands index with
        | Scene_command.Render_ir.Push_transform transform ->
            mix (Int64.to_int (Int64.bits_of_float transform.xx));
            mix (Int64.to_int (Int64.bits_of_float transform.xy));
            mix (Int64.to_int (Int64.bits_of_float transform.yx));
            mix (Int64.to_int (Int64.bits_of_float transform.yy));
            mix (Int64.to_int (Int64.bits_of_float transform.tx));
            mix (Int64.to_int (Int64.bits_of_float transform.ty))
        | Geometry geometry ->
            mix (Hashtbl.hash geometry.vertices);
            mix (Hashtbl.hash geometry.indices);
            mix (Int32.to_int geometry.color)
        | _ -> ()
      done;
      let fingerprint = !fingerprint in
      let cacheable, resources = Renderer_retained_plans.resource_stamps ~resolve commands in
      let exact (plan : cached_scene2_plan) =
        plan.plan_command_count = command_count
        && same_context plan
        && Renderer_retained_plans.same_resources plan.plan_resources resources
        && Scene_command.Render_ir.commands_readonly plan.plan_ir = commands
      in
      let rebind (plan : cached_scene2_plan) =
        Renderer_retained_plans.rebind value.retained_plans plan ~ir_id ~ir
      in
      let hit =
        match Renderer_retained_plans.find value.retained_plans fingerprint with
        | plan when exact plan -> Some plan
        | _ -> None
        | exception Not_found -> None
      in
      if cacheable then
        match hit with
        | Some (plan : cached_scene2_plan) ->
            rebind plan;
            Ok plan.plan_draws
        | None -> (
            match lower_scene2_uncached value ~lease_policy ~density ~resource:resolve ir with
            | Error _ as error -> error
            | Ok draws as result ->
                if
                  List.exists
                    (fun draw ->
                      match draw.texture with
                      | Some texture ->
                          sampled_texture_bytes texture > snapshot_cache_entry_byte_capacity
                      | None -> false)
                    draws
                  || has_gpu_shapes commands
                then result
                else
                  let plan : cached_scene2_plan =
                    {
                      plan_ir_id = ir_id;
                      plan_command_count = command_count;
                      plan_source_bytes = Renderer_retained_plans.plan_source_bytes commands;
                      plan_density = density;
                      plan_extent = extent;
                      plan_resources = resources;
                      plan_ir = ir;
                      plan_draws = draws;
                    }
                  in
                  Renderer_retained_plans.add value.retained_plans fingerprint plan;
                  result)
      else lower_scene2_uncached value ~lease_policy ~density ~resource:resolve ir
    in
    match Renderer_retained_plans.find_ir value.retained_plans ir_id with
    | Some (plan : cached_scene2_plan) when same_context plan && plan.plan_resources = [] ->
        Ok plan.plan_draws
    | Some (plan : cached_scene2_plan) when same_context plan ->
        let cacheable, resources = Renderer_retained_plans.resource_stamps ~resolve commands in
        if cacheable && Renderer_retained_plans.same_resources plan.plan_resources resources then
          Ok plan.plan_draws
        else slow ()
    | _ -> slow ()

let lower_scene2 value ~density ~resource ir =
  lower_scene2_with_policy value ~lease_policy:Copy_image_snapshots ~density ~resource ir

let replay_step ?clear ~identity ~version value =
  match ensure "Renderer.Native.replay" value with
  | Error _ as error -> error
  | Ok () -> (
      match sync_runtime_facts "Renderer.Native.replay" value with
      | Error _ as error -> error
      | Ok () -> (
          match
            Scene_execution.replay_prepared_sampled_resources ?clear ~identity ~version
              value.renderer
          with
          | Error error -> backend "Renderer.Native.replay" error
          | Ok None -> Ok None
          | Ok (Some (_presented, draw_count)) ->
              count_draws value.counters draw_count;
              Ok (Some ())))

let step_core ?after_prepare ?clear ?identity ?version value draws =
  let after_prepare = Option.value after_prepare ~default:Fun.id in
  match ensure "Renderer.step" value with
  | Error _ as e -> e
  | Ok () -> (
      match sync_runtime_facts "Renderer.step" value with
      | Error _ as error ->
          after_prepare ();
          error
      | Ok () -> (
          match presentation_facts value with
          | Error _ as error ->
              after_prepare ();
              error
          | Ok f -> (
              let renderer = value.renderer in
              let replayed =
                match (identity, version) with
                | Some identity, Some version ->
                    Scene_execution.replay_prepared_sampled_resources ?clear ~identity ~version
                      renderer
                | _ -> Ok None
              in
              let rendered =
                match replayed with
                | Error error ->
                    after_prepare ();
                    Error error
                | Ok (Some (presented, draw_count)) ->
                    after_prepare ();
                    count_draws value.counters draw_count;
                    Ok presented
                | Ok None ->
                    let rec same_draws left right =
                      match (left, right) with
                      | [], [] -> true
                      | x :: xs, y :: ys -> x == y && same_draws xs ys
                      | _ -> false
                    in
                    let draws =
                      if same_draws draws value.last_step_draws then value.last_step_prepared
                      else
                        let prepared =
                          List.map
                            (fun x ->
                              let draw = x.draw in
                              let state = draw.Scene_execution.state in
                              let viewport =
                                match state.viewport with
                                | _, _, w, h when w < 0 || h < 0 ->
                                    (0, 0, f.logical_width, f.logical_height)
                                | x -> x
                              in
                              let scissor =
                                match state.scissor with
                                | _, _, w, h when w < 0 || h < 0 ->
                                    (0, 0, f.logical_width, f.logical_height)
                                | x -> x
                              in
                              let draw =
                                if viewport = state.viewport && scissor = state.scissor then draw
                                else
                                  {
                                    draw with
                                    Scene_execution.state = { state with viewport; scissor };
                                  }
                              in
                              {
                                Scene_execution.family = x.family;
                                blend = x.blend;
                                texture = x.texture;
                                auxiliary = x.auxiliary;
                                vertex_attributes = x.vertex_attributes;
                                samples = x.samples;
                                draw;
                              })
                            draws
                        in
                        value.last_step_draws <- draws;
                        value.last_step_prepared <- prepared;
                        prepared
                    in
                    let scaled = scale_sampled_cached value.scaled value.renderer_facts draws in
                    let result =
                      match (identity, version) with
                      | None, None -> Scene_execution.submit ~after_prepare ?clear renderer scaled
                      | Some identity, Some version ->
                          Scene_execution.submit ~after_prepare ?clear ~prepared:(identity, version)
                            renderer scaled
                      | _ ->
                          after_prepare ();
                          Error
                            (Ogpu.Error.make "Renderer.step" Ogpu.Error.Invalid_argument
                               "prepared identity and version must be supplied together")
                    in
                    Result.map
                      (fun presented ->
                        count_draws value.counters (List.length scaled);
                        presented)
                      result
              in
              match rendered with
              | Error e -> backend "Renderer.step" e
              | Ok presented -> Ok presented)))

let step ?clear value draws = Result.map (fun _ -> ()) (step_core ?clear value draws)
let draw ?clear value draws = step_core ?clear value draws

let lower_scene2_submission submission ~density ~resource ir =
  match ensure_submission "Renderer.Native.lower_scene2" submission with
  | Error _ as e ->
      close_submission submission;
      e
  | Ok () -> (
      let protected_resource id =
        try resource id with
        | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
        | exn -> raise (Resource_resolver_raised exn)
      in
      try
        match
          lower_scene2_with_policy submission.owner
            ~lease_policy:(Retain_image_snapshots submission.image_leases) ~density
            ~resource:protected_resource ir
        with
        | Ok draws -> Ok { batch_owner = submission; batch_draws = draws }
        | Error _ as error ->
            close_submission submission;
            error
      with
      | Resource_resolver_raised exn ->
          close_submission submission;
          fail "Renderer.Native.lower_scene2" Ogpu.Error.Invalid_state
            ("resource resolver raised: " ^ Printexc.to_string exn)
      | exn ->
          close_submission submission;
          raise exn)

let retained_scene2_segment_bytes draws =
  List.fold_left
    (fun total draw ->
      let value = draw.draw in
      total
      + Bytes.length value.Scene_execution.mesh.vertices
      + Bytes.length value.mesh.indices
      + Option.fold ~none:0 ~some:Bytes.length value.state.transform_uniforms
      + Option.fold ~none:0 ~some:sampled_texture_bytes draw.texture
      + Option.fold ~none:0
          ~some:(fun auxiliary ->
            Bytes.length auxiliary.Scene_execution.buffer + sampled_texture_bytes auxiliary.texture)
          draw.auxiliary)
    0 draws

let lower_scene2_segment submission ~identity ~version ~cacheable ~density ~resource ir =
  let operation = "Renderer.Native.lower_scene2_segment" in
  if identity <= 0L || version < 0L then begin
    close_submission submission;
    fail operation Ogpu.Error.Invalid_argument "segment identity/version is invalid"
  end
  else
    match ensure_submission operation submission with
    | Error _ as error ->
        close_submission submission;
        error
    | Ok () -> (
        let owner = submission.owner in
        match presentation_facts owner with
        | Error _ as error ->
            close_submission submission;
            error
        | Ok facts -> (
            let find () =
              match Renderer_retained_plans.find_segment owner.retained_plans identity with
              | (entry : retained_scene2_segment)
                when entry.segment_version = version && entry.segment_density = density
                     && entry.segment_width = facts.logical_width
                     && entry.segment_height = facts.logical_height ->
                  Some entry.segment_draws
              | _ -> None
              | exception Not_found -> None
            in
            match if cacheable then find () else None with
            | Some draws ->
                owner.retained_scene2_segment_hits <- Int64.succ owner.retained_scene2_segment_hits;
                Ok { batch_owner = submission; batch_draws = draws }
            | None -> (
                match lower_scene2_submission submission ~density ~resource ir with
                | Error _ as error -> error
                | Ok batch ->
                    if cacheable then
                      owner.retained_scene2_segment_misses <-
                        Int64.succ owner.retained_scene2_segment_misses;
                    if cacheable then begin
                      let entry : retained_scene2_segment =
                        {
                          segment_version = version;
                          segment_density = density;
                          segment_width = facts.logical_width;
                          segment_height = facts.logical_height;
                          segment_draws = batch.batch_draws;
                        }
                      in
                      Renderer_retained_plans.add_segment owner.retained_plans identity
                        ~bytes:(retained_scene2_segment_bytes batch.batch_draws)
                        entry
                    end;
                    Ok batch)))

let lower_ui submission ~density ~resource ui =
  let operation = "Renderer.Native.lower_ui" in
  match ensure_submission operation submission with
  | Error _ as error ->
      close_submission submission;
      error
  | Ok () -> (
      match presentation_facts submission.owner with
      | Error _ as error ->
          close_submission submission;
          error
      | Ok facts -> (
          let width = float facts.logical_width and height = float facts.logical_height in
          let framebuffer = (0, 0, facts.logical_width, facts.logical_height) in
          let instances = Scene_command.Ui_batch.instances ui in
          let texture id =
            if id <= 0 then Ok ui_white_texture
            else
              match resource id with
              | None -> fail operation Ogpu.Error.Invalid_state "UI texture resource id is unbound"
              | Some source -> (
                  match
                    snapshot submission.owner
                      ~lease_policy:(Retain_image_snapshots submission.image_leases) ~density source
                  with
                  | Error _ as error -> error
                  | Ok (_, _, texture) -> Ok { texture with Scene_execution.sampler = ui_sampler })
          in
          let layer = submission.ui_layers in
          submission.ui_layers <- layer + 1;
          let lowering = submission.owner.lowering in
          let draw index (batch : Scene_command.Ui_batch.batch) =
            Result.map
              (fun texture ->
                let affine =
                  Renderer_lowering.write_ui_affine lowering
                    {
                      Scene_command.Render_ir.xx = 2. /. width;
                      xy = 0.;
                      yx = 0.;
                      yy = -2. /. height;
                      tx = -1.;
                      ty = 1.;
                    }
                in
                let first = batch.first * Scene_command.Ui_batch.instance_bytes
                and length = batch.count * Scene_command.Ui_batch.instance_bytes in
                let slot_key = (layer * 65536) + index in
                let slot = Renderer_lowering.find_ui_slot lowering slot_key in
                if
                  Bytes.length slot.ui_vertices <> length
                  || not (same_bytes_range instances first slot.ui_vertices)
                then slot.ui_vertices <- Bytes.sub instances first length;
                if Bytes.length slot.ui_indices <> batch.count * 24 then
                  slot.ui_indices <- Renderer_lowering.ui_index_bytes lowering batch.count;
                if not (Bytes.equal slot.ui_affine affine) then slot.ui_affine <- Bytes.copy affine;
                let scissor =
                  match batch.clip with
                  | None -> framebuffer
                  | Some clip ->
                      let x = max 0 (int_of_float (Float.floor clip.x))
                      and y = max 0 (int_of_float (Float.floor clip.y)) in
                      let right =
                        min facts.logical_width (int_of_float (Float.ceil (clip.x +. clip.width)))
                      and bottom =
                        min facts.logical_height (int_of_float (Float.ceil (clip.y +. clip.height)))
                      in
                      (x, y, max 0 (right - x), max 0 (bottom - y))
                in
                {
                  Scene_execution.family = Ui;
                  blend = Alpha;
                  texture = Some texture;
                  auxiliary = None;
                  vertex_attributes = None;
                  samples = 1;
                  draw =
                    {
                      Scene_execution.mesh =
                        {
                          key = Host ("ui:" ^ string_of_int slot_key);
                          vertices = slot.ui_vertices;
                          vertex_count = 4 * batch.count;
                          indices = slot.ui_indices;
                          index_count = 6 * batch.count;
                          primitive = Ogpu.Render_pass.Triangle_list;
                        };
                      state =
                        {
                          (default_state framebuffer scissor) with
                          transform_uniforms = Some slot.ui_affine;
                        };
                    };
                })
              (texture batch.texture)
          in
          let batches = Scene_command.Ui_batch.batches ui in
          let rec build index reversed =
            if index = Array.length batches then Ok (List.rev reversed)
            else
              let batch = batches.(index) in
              let _, _, scissor_width, scissor_height =
                match batch.clip with
                | None -> framebuffer
                | Some clip -> (0, 0, int_of_float clip.width, int_of_float clip.height)
              in
              if batch.count = 0 || scissor_width <= 0 || scissor_height <= 0 then
                build (index + 1) reversed
              else
                match draw index batch with
                | Error _ as error -> error
                | Ok value -> build (index + 1) (value :: reversed)
          in
          match build 0 [] with
          | Error _ as error ->
              close_submission submission;
              error
          | Ok draws -> Ok { batch_owner = submission; batch_draws = draws }))

let adopt_draws submission draws =
  match ensure_submission "Renderer.Native.adopt_draws" submission with
  | Error _ as error ->
      close_submission submission;
      error
  | Ok () -> Ok { batch_owner = submission; batch_draws = draws }

let step_submission ?clear ?identity ?version submission batches =
  match ensure_submission "Renderer.Native.step" submission with
  | Error _ as e ->
      close_submission submission;
      e
  | Ok () ->
      Fun.protect
        ~finally:(fun () -> close_submission submission)
        (fun () ->
          if List.exists (fun batch -> batch.batch_owner != submission) batches then
            fail "Renderer.Native.step" Ogpu.Error.Invalid_argument
              "draw batch belongs to another submission"
          else
            let draws =
              match batches with
              | [ batch ] -> batch.batch_draws
              | _ ->
                  List.rev
                    (List.fold_left
                       (fun reversed batch -> List.rev_append batch.batch_draws reversed)
                       [] batches)
            in
            Result.map
              (fun _ -> ())
              (step_core
                 ~after_prepare:(fun () -> close_submission submission)
                 ?clear ?identity ?version submission.owner draws))

let capture value =
  match ensure "Renderer.capture" value with
  | Error _ as e -> e
  | Ok () -> (
      match presentation_facts value with
      | Error _ as error -> error
      | Ok facts -> (
          match
            Scene_execution.read_pixels value.renderer ~bytes_per_row:(facts.drawable_width * 4)
          with
          | Ok x -> Ok x
          | Error e -> backend "Renderer.capture" e))

let read_pixels value ~bytes_per_row =
  match ensure "Renderer.read_pixels" value with
  | Error _ as error -> error
  | Ok () -> Scene_execution.read_pixels value.renderer ~bytes_per_row

let offscreen_target value =
  let operation = "Renderer.offscreen_target" in
  match ensure operation value with
  | Error _ as e -> e
  | Ok () ->
      if Runtime.is_window value.runtime then
        fail operation Ogpu.Error.Unsupported "operation requires an offscreen execution"
      else Ok (Scene_execution.target value.renderer)

let rec destroy value =
  if value.dead then Ok ()
  else
    let () = release_views value in
    if Runtime.is_window value.runtime && value.gpu_owner.shared_leases > 0 then
      fail "Renderer.destroy" Ogpu.Error.Invalid_argument
        (string_of_int value.gpu_owner.shared_leases
        ^ " GPU lease(s) still share this window's device; release them first")
    else (
      List.iter close_submission value.submissions;
      let assets = Runtime_resources.Assets.destroy value.assets in
      Snapshot_table.clear value.snapshots;
      Renderer_retained_plans.clear value.retained_plans;
      value.last_step_draws <- [];
      value.last_step_prepared <- [];
      Renderer_lowering.clear value.lowering;
      value.dead <- true;
      (match !active_window with
      | Some current when current == value -> active_window := None
      | _ -> ());
      let core = Scene_execution.destroy value.renderer in
      let shell = Runtime.destroy value.runtime in
      if value.owns_device_lease then release_device value.shared_device_lease value.gpu_owner;
      match (assets, core, shell) with
      | Error error, _, _ -> resource "Renderer.destroy" error
      | Ok (), Error error, _ -> backend "Renderer.destroy" error
      | Ok (), Ok (), Error error -> backend "Renderer.destroy" error
      | Ok (), Ok (), Ok () -> Ok ())

and release_view view =
  match view.view_target with
  | Some target ->
      view.view_retain_after := if view.view_seen < retain_live then retain_live else retain_soon;
      view.view_target <- None;
      ignore (Runtime_resources.Canvas.destroy target.target_canvas);
      ignore (destroy target.target_exec)
  | None -> ()

and release_views value = Lru.Identity_list.clear value.retained_views

let () = release_retained_view := release_view

(* The first sight of a layer draws it directly; the second renders it offscreen. [None] when the
   target cannot be made: the view then stays direct for as long as the layer does. *)
let make_retained_target (f : presentation_facts) draws =
  let dw = f.drawable_width and dh = f.drawable_height in
  match
    create_offscreen
      {
        logical_width = f.logical_width;
        logical_height = f.logical_height;
        drawable_width = dw;
        drawable_height = dh;
        title = "Rays retained view";
        vsync = false;
        high_density = true;
      }
  with
  | Error _ -> None
  | Ok exec -> (
      let fail_with canvas =
        Option.iter (fun c -> ignore (Runtime_resources.Canvas.destroy c)) canvas;
        ignore (destroy exec);
        None
      in
      match
        ( step ~clear:(0., 0., 0., 0.) exec draws,
          offscreen_target exec,
          Runtime_resources.Canvas.create ~width:dw ~height:dh )
      with
      | Ok (), Ok texture, Ok canvas -> (
          match
            ( Runtime_resources.Canvas.publish_gpu canvas texture,
              Scene_command.Render_ir.create
                [|
                  Scene_command.Render_ir.Image
                    {
                      resource_id = 1;
                      source = { x = 0.; y = 0.; width = float dw; height = float dh };
                      destination =
                        {
                          x = 0.;
                          y = 0.;
                          width = float f.logical_width;
                          height = float f.logical_height;
                        };
                    };
                |] )
          with
          | Ok (), Ok ir -> Some { target_exec = exec; target_canvas = canvas; target_ir = ir }
          | _ -> fail_with (Some canvas))
      | _, _, Ok canvas -> fail_with (Some canvas)
      | _ -> fail_with None)

let adopt_retained_view submission ~density ~layer draws =
  let value = submission.owner in
  let direct () = adopt_draws submission draws in
  if not (Runtime.is_window value.runtime) then direct ()
  else
    match presentation_facts value with
    | Error _ -> direct ()
    | Ok f -> (
        let drawable = (f.drawable_width, f.drawable_height) in
        let current =
          match Lru.Identity_list.peek value.retained_views layer with
          | Some view when view.view_drawable = drawable -> Some view
          | Some view ->
              Lru.Identity_list.remove value.retained_views layer;
              None
          | None -> None
        in
        match current with
        | None ->
            Lru.Identity_list.add value.retained_views layer
              {
                view_layer = layer;
                view_drawable = drawable;
                view_target = None;
                view_failed = false;
                view_seen = 1;
                view_retain_after = value.retain_after;
              };
            direct ()
        | Some view -> (
            view.view_seen <- view.view_seen + 1;
            (match view.view_target with
            | None when (not view.view_failed) && view.view_seen >= !(value.retain_after) -> (
                match make_retained_target f draws with
                | Some target ->
                    view.view_target <- Some target;
                    (* from here [view_seen] counts the frames the target serves *)
                    view.view_seen <- 0
                | None -> view.view_failed <- true)
            | _ -> ());
            match view.view_target with
            | None -> direct ()
            | Some target ->
                lower_scene2_submission submission ~density
                  ~resource:(function 1 -> Some (Canvas target.target_canvas) | _ -> None)
                  target.target_ir))

(* GPU film leases own their own queue, so their frame pacing never couples
   with presentation. *)
let acquire_gpu () =
  let operation = "Renderer.acquire_gpu" in
  match acquire_device operation with
  | Error _ as e -> e
  | Ok (device, gpu_shared, gpu_owner) -> (
      match Ogpu.Backend.create_queue device with
      | Error e ->
          release_device gpu_shared gpu_owner;
          backend operation e
      | Ok queue ->
          Ok { gpu_device = device; gpu_queue = queue; gpu_owner; gpu_shared; gpu_released = false }
      )

let gpu_device gpu = gpu.gpu_device
let gpu_queue gpu = gpu.gpu_queue
let gpu_shared gpu = gpu.gpu_shared

let release_gpu gpu =
  if not gpu.gpu_released then begin
    List.iter close_gpu_marks
      (Hashtbl.fold
         (fun _ value all -> if value.marks_gpu == gpu then value :: all else all)
         gpu.gpu_owner.marks_registry []);
    gpu.gpu_released <- true;
    ignore (Ogpu.Backend.destroy_queue gpu.gpu_queue);
    release_device gpu.gpu_shared gpu.gpu_owner
  end

module Native = struct
  let runtime value = value.runtime
  let gpu_vertices_owner value = value.gpu_owner.scene_vertices

  type nonrec scaled_cache = scaled_cache

  let new_scaled_cache = new_scaled_cache
  let scale_draws = scale_draws
  let scale_sampled_resources = scale_sampled_resources
  let scale_sampled_cached = scale_sampled_cached

  type nonrec gpu_marks = gpu_marks

  type nonrec marks_source = marks_source =
    | Host of bytes * int
    | Device of (unit -> Ogpu.Backend.buffer option) * int

  let create_offscreen_with_gpu gpu (configuration : configuration) =
    let operation = "Renderer.Native.create_offscreen_with_gpu" in
    if gpu.gpu_released then fail operation Ogpu.Error.Stale_handle "GPU lease is released"
    else
      match valid_configuration operation configuration with
      | Error _ as error -> error
      | Ok () -> (
          match
            Runtime.create_offscreen ~logical_width:configuration.logical_width
              ~logical_height:configuration.logical_height ~width:configuration.drawable_width
              ~height:configuration.drawable_height ()
          with
          | Error error -> backend operation error
          | Ok runtime -> (
              match Runtime.surface_configuration runtime with
              | Error error ->
                  ignore (Runtime.destroy runtime);
                  backend operation error
              | Ok surface -> (
                  let driver = Ogpu.Impl.create_driver () in
                  match
                    Renderer_pipeline.create_renderer ~backend:Ogpu.Impl.backend
                      ~device:gpu.gpu_device ~gpu_vertices:gpu.gpu_owner.scene_vertices
                      ~offscreen:true ~driver ~configuration:surface ()
                  with
                  | Error error ->
                      ignore (Runtime.destroy runtime);
                      backend operation error
                  | Ok renderer -> Ok (finish_create gpu.gpu_owner runtime renderer))))

  let create_gpu_marks gpu =
    let operation = "Renderer.create_gpu_marks" in
    if gpu.gpu_released then fail operation Ogpu.Error.Stale_handle "GPU lease is released"
    else if Hashtbl.length gpu.gpu_owner.marks_registry >= gpu_marks_capacity then
      fail operation Ogpu.Error.Capacity "GPU marks capacity reached"
    else
      match fresh_gpu_marks_id () with
      | None -> fail operation Ogpu.Error.Capacity "GPU marks identity capacity reached"
      | Some identity -> begin
          let value =
            {
              marks_id = identity;
              marks_gpu = gpu;
              marks_domain = Domain.self ();
              marks_closed = false;
              marks_stamp = 0L;
              marks_count = 0;
              marks_owned = Array.make 6 None;
              marks_last = Array.make 6 Bytes.empty;
              marks_device = [];
              marks_registration = None;
            }
          in
          Hashtbl.add gpu.gpu_owner.marks_registry value.marks_id value;
          Ok value
        end

  let gpu_marks ?indirect value ~kind ~count attributes =
    let operation = "Renderer.gpu_marks" in
    if value.marks_closed || value.marks_gpu.gpu_released then
      fail operation Ogpu.Error.Stale_handle "GPU marks or lease is closed"
    else if value.marks_domain <> Domain.self () then
      fail operation Ogpu.Error.Invalid_argument "GPU marks used on another domain"
    else if count <= 0 || count > 1_000_000 || Array.length attributes <> 5 then
      fail operation Ogpu.Error.Invalid_argument
        "a mark set has one to 1,000,000 instances and five attributes"
    else begin
      let changed = ref (value.marks_registration = None || value.marks_count <> count) in
      let device = ref [] in
      let stale = ref false in
      let upload index bytes =
        let owned = value.marks_owned.(index) in
        let size = Int64.of_int (max 16 (Bytes.length bytes)) in
        let buffer =
          match owned with
          | Some b when Ogpu.Backend.buffer_size b >= size -> Ok b
          | _ ->
              let capacity = Int64.of_int (max 16 (2 * Bytes.length bytes)) in
              Result.map
                (fun b ->
                  Option.iter (fun old -> ignore (Ogpu.Backend.destroy_buffer old)) owned;
                  value.marks_owned.(index) <- Some b;
                  value.marks_last.(index) <- Bytes.empty;
                  changed := true;
                  b)
                (Ogpu.Backend.create_buffer value.marks_gpu.gpu_device
                   {
                     label = Some "Marks attribute";
                     size = capacity;
                     usage = [ Vertex; Storage; Copy_dst ];
                   })
        in
        Result.bind buffer (fun b ->
            if value.marks_last.(index) == bytes || Bytes.equal value.marks_last.(index) bytes then
              Ok b
            else
              Result.map
                (fun () ->
                  value.marks_last.(index) <- bytes;
                  changed := true;
                  b)
                (Ogpu.Backend.write_buffer b ~offset:0L bytes))
      in
      let strides = Array.make 5 0 in
      let rec bind index =
        if index = 5 then Ok []
        else begin
          let buffer =
            match attributes.(index) with
            | Host (bytes, stride) ->
                strides.(index) <- stride;
                upload index bytes
            | Device (source, stride) -> (
                strides.(index) <- stride;
                match source () with
                | Some b ->
                    device := (source, b) :: !device;
                    Ok b
                | None ->
                    stale := true;
                    fail operation Ogpu.Error.Stale_handle "GPU display output is stale or closed")
          in
          Result.bind buffer (fun b -> Result.map (fun rest -> b :: rest) (bind (index + 1)))
        end
      in
      (* the device's index count, read by the draw, joins the inputs whose identity the token keeps *)
      let indirect =
        match indirect with
        | None -> Ok None
        | Some (source, offset) -> (
            match source () with
            | Some b ->
                device := (source, b) :: !device;
                Ok (Some (b, offset))
            | None ->
                stale := true;
                fail operation Ogpu.Error.Stale_handle "GPU display count is stale or closed")
      in
      Result.bind indirect (fun indirect ->
          Result.bind (bind 0) (fun buffers ->
              let params = Bytes.create 32 in
              (* A device colour is a region's float r, g, b (mode 1); a host one is packed words. *)
              let mode index = match attributes.(index) with Device _ -> 1 | Host _ -> 0 in
              Array.iteri
                (fun i word -> Bytes.set_int32_le params (i * 4) (Int32.of_int word))
                [|
                  kind;
                  strides.(0);
                  strides.(1);
                  strides.(2);
                  strides.(3);
                  strides.(4);
                  mode 2;
                  mode 3;
                |];
              Result.bind (upload 5 params) (fun params_buffer ->
                  let device = List.rev !device in
                  if
                    (not !changed) && valid_gpu_marks value
                    && List.length device = List.length value.marks_device
                    && List.for_all2 (fun (_, a) (_, b) -> a == b) device value.marks_device
                  then
                    Ok
                      (Scene_command.Marks.gpu_token ~identity:value.marks_id ~count
                         ~stamp:value.marks_stamp)
                  else begin
                    unregister_marks value;
                    value.marks_stamp <- Int64.succ value.marks_stamp;
                    value.marks_count <- count;
                    value.marks_device <- device;
                    match
                      Scene_execution.Private.register_gpu_vertices
                        value.marks_gpu.gpu_owner.scene_vertices
                        ~extra:(List.tl buffers @ [ params_buffer ])
                        ?indirect ~key:(gpu_marks_key value) ~device:value.marks_gpu.gpu_device
                        ~buffer:(List.hd buffers) ~vertex_count:(4 * count)
                        ~valid:(fun () -> valid_gpu_marks value)
                        ()
                    with
                    | Error e ->
                        value.marks_device <- [];
                        backend operation e
                    | Ok registration ->
                        value.marks_registration <- Some registration;
                        Ok
                          (Scene_command.Marks.gpu_token ~identity:value.marks_id ~count
                             ~stamp:value.marks_stamp)
                  end)))
    end

  let close_gpu_marks = close_gpu_marks

  let encoded_draws_for_test (batch : batch) =
    Scene_execution.Private.coalesced_draws_for_test
      (List.map
         (fun x ->
           {
             Scene_execution.family = x.family;
             blend = x.blend;
             texture = x.texture;
             auxiliary = x.auxiliary;
             vertex_attributes = x.vertex_attributes;
             samples = x.samples;
             draw = x.draw;
           })
         batch.batch_draws)

  let snapshot_count_for_test value = Snapshot_table.length value.snapshots
  let window_handle value = Runtime.window_handle value.runtime

  type nonrec submission = submission
  type nonrec batch = batch

  let begin_submission = begin_submission
  let lower_scene2 = lower_scene2_submission
  let lower_scene2_segment = lower_scene2_segment
  let lower_ui = lower_ui
  let adopt_draws = adopt_draws
  let adopt_retained_view = adopt_retained_view
  let step = step_submission
  let replay = replay_step
  let cancel = close_submission

  let retained_scene2_segment_stats value =
    ( Renderer_retained_plans.segment_count value.retained_plans,
      value.retained_scene2_segment_hits,
      value.retained_scene2_segment_misses )
end

let create_gpu_marks = Native.create_gpu_marks
let gpu_marks = Native.gpu_marks
let next_gpu_positions = Atomic.make 0

let gpu_positions gpu key =
  if gpu.gpu_released then None
  else
    match Scene_execution.Private.find_gpu_vertices gpu.gpu_owner.scene_vertices key with
    | Some (positions, (([] | [ _ ]) as extra), count) ->
        Some (positions, List.nth_opt extra 0, count)
    | Some _ | None -> None

let register_gpu_positions gpu ~positions ?normals ~count ~valid () =
  let operation = "Renderer.register_gpu_positions" in
  if gpu.gpu_released then fail operation Ogpu.Error.Stale_handle "GPU lease is released"
  else
    let key = Printf.sprintf "positions:%d" (Atomic.fetch_and_add next_gpu_positions 1) in
    match
      Scene_execution.Private.register_gpu_vertices gpu.gpu_owner.scene_vertices
        ~extra:(Option.to_list normals) ~stride:12 ~key ~device:gpu.gpu_device ~buffer:positions
        ~vertex_count:count
        ~valid:(fun () -> (not gpu.gpu_released) && valid ())
        ()
    with
    | Error e -> backend operation e
    | Ok registration ->
        Ok (key, fun () -> Scene_execution.Private.unregister_gpu_vertices registration)
