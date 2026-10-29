(* GPU vertices are borrowed, never uploaded or destroyed by the renderer.
   Registration owns only a bounded name and the producer's lifetime check. *)
type gpu_vertices = {
  gpu_owner : gpu_vertices_owner;
  gpu_key : string;
  gpu_device : Ogpu.Backend.device;
  gpu_buffer : Ogpu.Backend.buffer;
  gpu_extra : Ogpu.Backend.buffer list;
  gpu_vertex_count : int;
  gpu_domain : Domain.id;
  gpu_indirect : (Ogpu.Backend.buffer * int64) option;
      (* device-written index count: an indirect draw *)
  gpu_valid : unit -> bool;
  mutable gpu_active : bool;
}

and gpu_vertices_owner = { gpu_vertex_registry : (string, gpu_vertices) Hashtbl.t }

let create_gpu_vertices_owner () = { gpu_vertex_registry = Hashtbl.create 16 }
let gpu_vertex_capacity = 128

(* The ids that identify a source's buffers in signatures and fingerprints. *)
let gpu_identity source =
  ( source.gpu_key,
    Ogpu.Backend.buffer_id source.gpu_buffer,
    List.map Ogpu.Backend.buffer_id
      (source.gpu_extra @ Option.to_list (Option.map fst source.gpu_indirect)) )

let gpu_vertices_valid value =
  value.gpu_active && value.gpu_domain = Domain.self () && value.gpu_valid ()

let register_gpu_vertices owner ?(extra = []) ?(stride = 16) ?indirect ~key ~device ~buffer
    ~vertex_count ~valid () =
  let operation = "Scene_execution.register_gpu_vertices" in
  if
    key = "" || vertex_count <= 0 || vertex_count > 4_000_000
    || (extra = [] && Ogpu.Backend.buffer_size buffer < Int64.of_int (vertex_count * stride))
    || List.exists (fun b -> Ogpu.Backend.buffer_size b <= 0L) (buffer :: extra)
    || (match indirect with
      | Some (b, offset) -> offset < 0L || Int64.add offset 20L > Ogpu.Backend.buffer_size b
      | None -> false)
    || Hashtbl.mem owner.gpu_vertex_registry key
  then
    Error
      (Ogpu.Error.make operation Invalid_argument
         "invalid or duplicate borrowed vertex registration")
  else if Hashtbl.length owner.gpu_vertex_registry >= gpu_vertex_capacity then
    Error (Ogpu.Error.make operation Capacity "borrowed vertex registration capacity reached")
  else
    let value =
      {
        gpu_owner = owner;
        gpu_key = key;
        gpu_device = device;
        gpu_buffer = buffer;
        gpu_extra = extra;
        gpu_vertex_count = vertex_count;
        gpu_indirect = indirect;
        gpu_domain = Domain.self ();
        gpu_valid = valid;
        gpu_active = true;
      }
    in
    Hashtbl.add owner.gpu_vertex_registry key value;
    Ok value

let unregister_gpu_vertices value =
  if value.gpu_domain <> Domain.self () then invalid_arg "GPU vertices closed on another domain";
  if value.gpu_active then (
    value.gpu_active <- false;
    Hashtbl.remove value.gpu_owner.gpu_vertex_registry value.gpu_key)

(* A borrowed GPU vertex stream is named apart from every host-owned mesh. *)
type mesh_key = Host of string | Gpu_vertices of string

let mesh_key_string = function Host k -> k | Gpu_vertices k -> "gpu:vertices:" ^ k

type mesh = {
  key : mesh_key;
  vertices : bytes;
  vertex_count : int;
  indices : bytes;
  index_count : int;
  primitive : Ogpu.Render_pass.primitive;
}

type state = {
  viewport : int * int * int * int;
  scissor : int * int * int * int;
  cull : Ogpu.Render_pass.cull;
  depth_compare : Ogpu.Render_pass.comparison;
  depth_write : bool;
  depth_load : Ogpu.Render_pass.load;
  depth_clear : float;
  transform_uniforms : bytes option;
}

type geometry_draw = { mesh : mesh; state : state }

type pipeline_family =
  | Scene2
  | Scene2_textured
  | Scene3
  | Scene3_points
  | Scene3_textured
  | Scene3_world
  | Ui
  | Marks

type texture_level = { width : int; height : int; bytes : bytes }

type sampled_texture = {
  key : string;
  levels : texture_level array;
  sampler : Ogpu.Types.sampler_descriptor;
  gpu : Ogpu.Backend.texture option;
}

type auxiliary_resource = {
  key : string;
  buffer : bytes;
  texture : sampled_texture;
  environment : sampled_texture option;
  sun_shadow : string option;
}

type sampled_draw = {
  family : pipeline_family;
  blend : Ogpu.Pipeline.blend;
  texture : sampled_texture option;
  auxiliary : auxiliary_resource option;
  vertex_attributes : (string * bytes) option;
  samples : int;
  draw : geometry_draw;
}

type scene3_entry = sampled_draw
type prepared_scene3 = { entries : scene3_entry array }

type cached = {
  mutable key : string;
  mutable payload_hash : string;
  mutable trusted_source : mesh option;
  buffer : Ogpu.Backend.buffer;
  mutable index_offset : int64;
  mutable uniform_offset : int64 option;
  mutable uniform_copy : bytes;
  mutable vertex_count : int;
  mutable index_count : int;
  mutable primitive : Ogpu.Render_pass.primitive;
  bytes : int;
}

type uniform_slice = {
  uniform_buffer : Ogpu.Backend.buffer;
  uniform_offset : int64;
  (* The frame's staging bytes and this slice's window into them, kept for
     next frame's unchanged-set comparison without a per-draw copy. *)
  uniform_staging : bytes;
  uniform_staging_offset : int;
  uniform_length : int;
}

type uniform_page = { page_buffer : Ogpu.Backend.buffer; page_capacity : int }

type cached_auxiliary = {
  auxiliary_copy : bytes;
  auxiliary_buffer : Ogpu.Backend.buffer;
  (* The bytes this entry was uploaded from: a physically equal source is a
     hit without comparing (World blocks can carry a whole shadow map). *)
  mutable auxiliary_source : bytes;
}

type cached_texture = {
  mutable texture_key : string;
  mutable texture_hash : string;
  texture_shape : string;
  texture : Ogpu.Backend.texture;
  texture_bytes : int;
  mutable texture_used : int;
  mutable texture_levels : texture_level array;
}

type texture_upload_scratch = {
  scratch_buffer : Ogpu.Backend.buffer;
  scratch_bytes : bytes;
  scratch_size : int;
}

type prepared_run = {
  prepared_version : int64;
  prepared_draws : sampled_draw list;
  prepared_bytes : int;
}

type automatic_signature = {
  signature_family : pipeline_family;
  signature_blend : Ogpu.Pipeline.blend;
  signature_samples : int;
  signature_state : state;
  signature_texture : (int64 * Ogpu.Types.sampler_descriptor) option;
  signature_auxiliary : (int64 * int64 * Ogpu.Types.sampler_descriptor) option;
  signature_environment : (int64 * Ogpu.Types.sampler_descriptor) option;
  signature_buffer : int64;
  signature_index_offset : int64;
  signature_gpu_vertices : (string * int64 * int64 list) option;
  signature_vertex_attributes : int64 option;
  signature_uniform_buffer : int64 option;
  signature_uniform_offset : int64 option;
  signature_vertex_count : int;
  signature_index_count : int;
}

type plan_draw = {
  plan_draw : Ogpu.Backend.batch_draw;
  plan_textures : (Ogpu.Backend.shader_stage * int * Ogpu.Backend.texture) list;
  plan_samplers : (Ogpu.Backend.shader_stage * int * Ogpu.Backend.sampler) list;
  (* Textures reached only through an argument buffer: Metal needs them made
     resident with [use_resources] before any draw of the pass. *)
  plan_resident : Ogpu.Backend.texture list;
  (* A borrowed shape whose index count the device wrote: drawn indirect, never batched or recorded *)
  plan_indirect : (Ogpu.Backend.buffer * int64) option;
}

(* One render pass of a frame: its attachment state and draws. A retained
   batch replays an indirect command buffer instead of re-encoding. *)
type batch_plan = {
  batch_family : pipeline_family;
  batch_samples : int;
  batch_state : state;
  batch_first : bool;
  batch_draws : plan_draw array;
  (* [batch_draws] as the backend batch, built once with the plan. *)
  batch_driver_draws : Ogpu.Backend.batch_draw array;
  batch_resources : [ `Buffer of Ogpu.Backend.buffer | `Texture of Ogpu.Backend.texture ] list;
  mutable batch_icb : Ogpu.Backend.icb option;
}

type replay_plan = {
  replay_clear : float * float * float * float;
  replay_payloads : automatic_signature list;
  replay_batches : batch_plan list;
  replay_gpu_vertices : gpu_vertices list;
}

type prepared_submission = {
  submission_identity : string;
  submission_version : int64;
  submission_draw_count : int;
  submission_plan : replay_plan;
}

type automatic_candidate = {
  candidate_clear : float * float * float * float;
  candidate_length : int;
  candidate_fingerprint : int;
}

type prepared_slot = {
  mutable slot_family : pipeline_family;
  mutable slot_blend : Ogpu.Pipeline.blend;
  mutable slot_samples : int;
  mutable slot_state : state option;
  mutable slot_texture : (sampled_texture * cached_texture) option;
  mutable slot_auxiliary : (auxiliary_resource * cached_auxiliary * cached_texture) option;
  mutable slot_environment : (sampled_texture * cached_texture) option;
  mutable slot_mesh : cached option;
  mutable slot_uniform : uniform_slice option;
  mutable slot_gpu_vertices : gpu_vertices option;
  mutable slot_vertex_attributes : cached_auxiliary option;
}

type prepared_scratch = {
  mutable scratch_slots : prepared_slot array;
  mutable scratch_length : int;
}

type pipeline_variant = {
  family : pipeline_family;
  blend : Ogpu.Pipeline.blend;
  samples : int;
  pipeline : Ogpu.Backend.pipeline;
  mutable argument : Ogpu.Backend.argument option;
}

type icb_stats = {
  mutable icb_builds : int64;
  mutable icb_hits : int64;
  mutable icb_misses : int64;
  mutable icb_evictions : int64;
  mutable icb_executions : int64;
  mutable icb_failures : int64;
  mutable icb_last_failure : string option;
}

type argument_pool = {
  argument_buffer : Ogpu.Backend.buffer;
  argument_stride : int;
  argument_capacity : int;
  slices : (int64 * Ogpu.Types.sampler_descriptor, int64) Hashtbl.t;
  mutable argument_next : int;
}

module String_table = Lru.Make (String)

type retired = {
  mutable retired_buffers : Ogpu.Backend.buffer list;
  mutable retired_textures : Ogpu.Backend.texture list;
}

let cache_byte_capacity = 256 * 1024 * 1024
let mesh_cache_entry_capacity = 256
let texture_cache_entry_capacity = 256
let texture_cache_byte_capacity = 256 * 1024 * 1024

let mesh_table retired =
  String_table.create mesh_cache_entry_capacity ~byte_capacity:cache_byte_capacity
    ~release:(fun _ (item : cached) ->
      retired.retired_buffers <- item.buffer :: retired.retired_buffers)

let texture_table retired =
  String_table.create texture_cache_entry_capacity ~byte_capacity:texture_cache_byte_capacity
    ~release:(fun _ item -> retired.retired_textures <- item.texture :: retired.retired_textures)

let auxiliary_table retired =
  String_table.create 64 ~byte_capacity:cache_byte_capacity ~release:(fun _ item ->
      retired.retired_buffers <- item.auxiliary_buffer :: retired.retired_buffers)

let drain_retired retired destroy_buffer destroy_texture =
  let buffers = retired.retired_buffers and textures = retired.retired_textures in
  retired.retired_buffers <- [];
  retired.retired_textures <- [];
  List.iter destroy_buffer buffers;
  List.iter destroy_texture textures

(* The World sun shadow map: packed 24-bit depth in RGBA8 plus the depth
   attachment its pass tests against, reused across frames (capacity 1). *)
type sun_map = {
  sun_pipeline : Ogpu.Backend.pipeline;
  sun_color : Ogpu.Backend.texture;
  sun_depth : Ogpu.Backend.texture;
  mutable sun_key : string;
}

type t = {
  device : Ogpu.Backend.device;
  gpu_vertices : gpu_vertices_owner;
  queue : Ogpu.Backend.queue;
  surface : Ogpu.Backend.surface option;
  mutable target : Ogpu.Backend.texture;
  mutable configuration : Ogpu.Surface.configuration;
  mutable attachments : Scene_attachment_pool.t;
  pipelines : pipeline_variant list;
  pipeline_lookup : pipeline_variant option array;
  cache : cached String_table.t;
  uniform_pages : uniform_page option array;
  mutable next_uniform_page : int;
  mutable previous_uniforms : uniform_slice option array;
  prepared_cache : prepared_run String_table.t;
  mutable prepared_submission : prepared_submission option;
  mutable automatic_submission : replay_plan option;
  mutable automatic_candidate : automatic_candidate option;
  prepared_scratch : prepared_scratch;
  auxiliary_cache : cached_auxiliary String_table.t;
  texture_cache : cached_texture String_table.t;
  (* Evicted GPU resources wait here until the frame that evicted them has
     been submitted. *)
  retired : retired;
  mutable frame : int;
  mutable texture_upload_scratch : texture_upload_scratch option;
  samplers : (Ogpu.Types.sampler_descriptor, Ogpu.Backend.sampler) Hashtbl.t;
  mutable arguments : argument_pool option;
  icb_stats : icb_stats;
  mutable uploaded : int64;
  sun_factory : (Ogpu.Backend.device -> (Ogpu.Backend.pipeline, Ogpu.Error.t) result) option;
  mutable sun : sun_map option;
  mutable sun_passes : int64;
  mutable in_flight : int64 option;
  mutable dead : bool;
  (* A borrowed device (shared with a window) is never destroyed here. *)
  owns_device : bool;
}

let drop_plan value = function
  | None -> ()
  | Some plan ->
      List.iter
        (fun batch ->
          Option.iter
            (fun icb ->
              value.icb_stats.icb_evictions <- Int64.succ value.icb_stats.icb_evictions;
              match Ogpu.Backend.destroy_icb icb with
              | Ok () -> ()
              | Error error ->
                  value.icb_stats.icb_failures <- Int64.succ value.icb_stats.icb_failures;
                  value.icb_stats.icb_last_failure <- Some ("destroy: " ^ Ogpu.Error.to_string error))
            batch.batch_icb;
          batch.batch_icb <- None)
        plan.replay_batches

let drop_plans value =
  drop_plan value value.automatic_submission;
  value.automatic_submission <- None;
  drop_plan value (Option.map (fun s -> s.submission_plan) value.prepared_submission);
  value.prepared_submission <- None;
  value.automatic_candidate <- None

let device value = value.device
let queue value = value.queue
let target value = value.target
let error op kind text = Error (Ogpu.Error.make op kind text)

let resolve_gpu_vertices value (entry : sampled_draw) =
  let mesh = entry.draw.mesh in
  match mesh.key with
  | Host _ -> Ok None
  | Gpu_vertices name -> (
      match Hashtbl.find_opt value.gpu_vertices.gpu_vertex_registry name with
      | Some source when gpu_vertices_valid source ->
          if source.gpu_device != value.device then
            error "Renderer.submit" Ogpu.Error.Cross_device "GPU vertices belong to another device"
          else if
            match (entry.family, source.gpu_extra) with
            | (Scene3 | Scene3_points), ([] | [ _ ]) ->
                (* a device's packed float3 positions (and normals) *)
                Bytes.length mesh.vertices <> 0 || mesh.vertex_count <> source.gpu_vertex_count
            | Ui, [] | Marks, [ _; _; _; _; _ ] ->
                Bytes.length mesh.vertices <> 0
                || mesh.vertex_count <> source.gpu_vertex_count
                || mesh.vertex_count mod 4 <> 0
                || mesh.index_count <> mesh.vertex_count / 4 * 6
            | _ -> true
          then error "Renderer.submit" Ogpu.Error.Invalid_argument "GPU shape layout is malformed"
          else Ok (Some source)
      | _ -> error "Renderer.submit" Ogpu.Error.Stale_handle "GPU vertices are stale or closed")

(* Scene3 draws carry the 12-byte attribute record a vertex; 2D, UI and Marks draws carry none. *)
let valid_vertex_attributes (entry : sampled_draw) =
  match (entry.family, entry.vertex_attributes) with
  | (Scene2 | Scene2_textured | Ui | Marks), None -> true
  | (Scene2 | Scene2_textured | Ui | Marks), Some _
  | (Scene3 | Scene3_points | Scene3_textured | Scene3_world), None ->
      false
  | _, Some (key, bytes) ->
      key <> "" && entry.draw.mesh.vertex_count >= 0
      && entry.draw.mesh.vertex_count <= Sys.max_string_length / 24
      && (match entry.draw.mesh.key with
        | Gpu_vertices _ -> Bytes.length entry.draw.mesh.vertices = 0
        | Host _ -> Bytes.length entry.draw.mesh.vertices = entry.draw.mesh.vertex_count * 24)
      && Bytes.length bytes = entry.draw.mesh.vertex_count * 12

let valid_index_length (mesh : mesh) =
  mesh.index_count >= 0
  && mesh.index_count <= Sys.max_string_length / 4
  && Bytes.length mesh.indices = mesh.index_count * 4

let prepare_scene3 ~clear ~clear_depth entries =
  let finite = Float.is_finite in
  let r, g, b, a = clear in
  let valid_extent (_, _, width, height) = width > 0 && height > 0 in
  let valid_family (entry : scene3_entry) =
    match (entry.family, entry.texture, entry.auxiliary) with
    | Scene3, None, None | Scene3_points, None, None -> true
    | Scene3_textured, Some _, None -> true
    | Scene3_world, Some _, Some auxiliary -> auxiliary.environment <> None
    | _ -> false
  in
  let valid_entry (entry : scene3_entry) =
    entry.samples > 0
    && List.mem entry.samples [ 1; 4; 9; 16 ]
    && valid_family entry
    && mesh_key_string entry.draw.mesh.key <> ""
    && entry.draw.mesh.vertex_count >= 0 && entry.draw.mesh.index_count >= 0
    && entry.draw.mesh.primitive = Ogpu.Render_pass.Point_list = (entry.family = Scene3_points)
    && (match entry.draw.mesh.primitive with
      | Ogpu.Render_pass.Line_list -> entry.draw.mesh.index_count mod 2 = 0
      | Triangle_list -> entry.draw.mesh.index_count mod 3 = 0
      | Point_list | Triangle_strip -> true)
    && valid_index_length entry.draw.mesh
    && valid_vertex_attributes entry
    && valid_extent entry.draw.state.viewport
    && valid_extent entry.draw.state.scissor
    && finite entry.draw.state.depth_clear
  in
  if (not (List.for_all finite [ r; g; b; a; clear_depth ])) || clear_depth < 0. || clear_depth > 1.
  then error "Scene_execution.prepare_scene3" Ogpu.Error.Invalid_argument "clear state is malformed"
  else if not (Array.for_all valid_entry entries) then
    error "Scene_execution.prepare_scene3" Ogpu.Error.Invalid_argument
      "draw family, resources, samples, mesh, or extent is malformed"
  else Ok { entries = Array.copy entries }

let set_u32_le bytes offset value =
  let open Int32 in
  Bytes.set bytes offset (Char.chr (to_int (logand value 0xffl)));
  Bytes.set bytes (offset + 1) (Char.chr (to_int (logand (shift_right_logical value 8) 0xffl)));
  Bytes.set bytes (offset + 2) (Char.chr (to_int (logand (shift_right_logical value 16) 0xffl)));
  Bytes.set bytes (offset + 3) (Char.chr (to_int (shift_right_logical value 24)))

let texture_descriptor configuration : Ogpu.Types.texture_descriptor =
  {
    label = Some "scene-execution-target";
    width = configuration.Ogpu.Surface.physical_width;
    height = configuration.physical_height;
    depth = 1;
    mip_levels = 1;
    sample_count = 1;
    format = Rgba8_unorm;
    usage = [ Texture_binding; Render_attachment; Texture_copy_src ];
  }

let blends = [ Ogpu.Pipeline.Replace; Alpha; Add; Multiply; Screen; Subtract ]

let families =
  [ Scene2; Scene2_textured; Scene3; Scene3_points; Scene3_textured; Scene3_world; Ui; Marks ]

let blend_count = List.length blends
let pipeline_variants_per_sample = List.length families * blend_count

let pipeline_slot family blend samples =
  let family =
    match family with
    | Scene2 -> 0
    | Scene2_textured -> 1
    | Scene3 -> 2
    | Scene3_points -> 3
    | Scene3_textured -> 4
    | Ui -> 5
    | Scene3_world -> 6
    | Marks -> 7
  in
  let blend =
    match blend with
    | Ogpu.Pipeline.Replace -> 0
    | Alpha -> 1
    | Add -> 2
    | Multiply -> 3
    | Screen -> 4
    | Subtract -> 5
  in
  let samples = match samples with 1 -> 0 | 4 -> 1 | 9 -> 2 | 16 -> 3 | _ -> -1 in
  if samples < 0 then -1 else (((family * blend_count) + blend) * 4) + samples

let find_pipeline value family blend samples =
  let slot = pipeline_slot family blend samples in
  if slot < 0 then None else value.pipeline_lookup.(slot)

let sample_counts device =
  List.filter
    (fun samples -> samples <= (Ogpu.Backend.capabilities device).Ogpu.Caps.limits.max_sample_count)
    [ 1; 4; 9; 16 ]

let allocate_target device configuration =
  Ogpu.Backend.create_texture device (texture_descriptor configuration)

let create ?device ?gpu_vertices ?sun_depth:sun_factory ~offscreen driver configuration make =
  let owns_device = Option.is_none device in
  match
    match device with Some device -> Ok device | None -> Ogpu.Backend.create_device driver
  with
  | Error _ as e -> e
  | Ok device -> (
      let cleanup () = if owns_device then ignore (Ogpu.Backend.destroy_device device) in
      match Ogpu.Backend.create_queue device with
      | Error e ->
          cleanup ();
          Error e
      | Ok queue -> (
          let surface =
            if offscreen then Ok None
            else Result.map Option.some (Ogpu.Backend.create_surface device configuration)
          in
          match surface with
          | Error e ->
              ignore (Ogpu.Backend.destroy_queue queue);
              cleanup ();
              Error e
          | Ok surface -> (
              let samples = sample_counts device in
              let requested =
                List.concat_map
                  (fun family ->
                    List.concat_map
                      (fun blend -> List.map (fun samples -> (family, blend, samples)) samples)
                      blends)
                  families
              in
              let rec variants made = function
                | [] -> Ok (List.rev made)
                | (family, blend, samples) :: rest -> (
                    match make device family blend samples with
                    | Error e ->
                        List.iter (fun x -> ignore (Ogpu.Backend.destroy_pipeline x.pipeline)) made;
                        Error e
                    | Ok pipeline ->
                        variants
                          ({ family; blend; samples; pipeline; argument = None } :: made)
                          rest)
              in
              let destroy_surface () =
                Option.iter (fun surface -> ignore (Ogpu.Backend.destroy_surface surface)) surface
              in
              match variants [] requested with
              | Error e ->
                  destroy_surface ();
                  ignore (Ogpu.Backend.destroy_queue queue);
                  cleanup ();
                  Error e
              | Ok pipelines -> (
                  match allocate_target device configuration with
                  | Ok target ->
                      let attachments =
                        Scene_attachment_pool.create ~device ~configuration ~sample_counts:samples
                      in
                      let pipeline_lookup = Array.make (pipeline_variants_per_sample * 4) None in
                      List.iter
                        (fun variant ->
                          let slot = pipeline_slot variant.family variant.blend variant.samples in
                          if slot >= 0 then pipeline_lookup.(slot) <- Some variant)
                        pipelines;
                      let retired = { retired_buffers = []; retired_textures = [] } in
                      Ok
                        {
                          device;
                          gpu_vertices =
                            (match gpu_vertices with
                            | Some owner -> owner
                            | None -> create_gpu_vertices_owner ());
                          owns_device;
                          queue;
                          surface;
                          target;
                          configuration;
                          attachments;
                          pipelines;
                          pipeline_lookup;
                          cache = mesh_table retired;
                          retired;
                          frame = 0;
                          uniform_pages = Array.make 3 None;
                          next_uniform_page = 0;
                          previous_uniforms = [||];
                          prepared_cache = String_table.create 64 ~byte_capacity:cache_byte_capacity;
                          prepared_submission = None;
                          automatic_submission = None;
                          automatic_candidate = None;
                          prepared_scratch = { scratch_slots = [||]; scratch_length = 0 };
                          auxiliary_cache = auxiliary_table retired;
                          texture_cache = texture_table retired;
                          texture_upload_scratch = None;
                          samplers = Hashtbl.create 8;
                          arguments = None;
                          icb_stats =
                            {
                              icb_builds = 0L;
                              icb_hits = 0L;
                              icb_misses = 0L;
                              icb_evictions = 0L;
                              icb_executions = 0L;
                              icb_failures = 0L;
                              icb_last_failure = None;
                            };
                          uploaded = 0L;
                          sun_factory;
                          sun = None;
                          sun_passes = 0L;
                          in_flight = None;
                          dead = false;
                        }
                  | Error e ->
                      List.iter
                        (fun x -> ignore (Ogpu.Backend.destroy_pipeline x.pipeline))
                        pipelines;
                      destroy_surface ();
                      ignore (Ogpu.Backend.destroy_queue queue);
                      cleanup ();
                      Error e))))

let source_for_trust (mesh : mesh) bytes =
  (* Keep extra CPU retention within the already bounded GPU payload budget,
     even when an indexed mesh contains a large unused vertex plane. *)
  if
    Bytes.length mesh.indices <= bytes
    && Bytes.length mesh.vertices <= bytes - Bytes.length mesh.indices
  then Some mesh
  else None

let same_bytes (mesh : mesh) = function
  | Some (source : mesh) ->
      source.vertices == mesh.vertices && source.indices == mesh.indices
      && source.vertex_count = mesh.vertex_count
      && source.index_count = mesh.index_count
  | None -> false

let content_hash (mesh : mesh) uniform_bytes =
  String.concat ":"
    [
      Digest.to_hex (Digest.bytes mesh.vertices);
      Digest.to_hex (Digest.bytes mesh.indices);
      Digest.to_hex (Digest.bytes uniform_bytes);
    ]

(* One entry per key. A hit needs no hashing: vertex-stable keys carry their
   identity, other draws keep the exact source bytes they were uploaded from
   and compare them physically. Content hashing happens once per upload and
   only decides a same-key miss for fresh but equal bytes. *)
let prepare value ~reserved ~uniforms ?(vertex_stable = false) ~nonindexed ~canonical_plain
    (mesh : mesh) =
  let uniform_bytes = Option.value uniforms ~default:Bytes.empty in
  let valid_uniforms =
    Option.fold ~none:true
      ~some:(fun bytes ->
        if Bytes.length bytes = 48 then (
          let valid = ref true in
          for index = 0 to 5 do
            if not (Float.is_finite (Int64.float_of_bits (Bytes.get_int64_le bytes (index * 8))))
            then valid := false
          done;
          !valid)
        else
          (Bytes.length bytes = 24 || Bytes.length bytes = 208 || Bytes.length bytes = 5456)
          &&
          let valid = ref true in
          for index = 0 to (Bytes.length bytes / 4) - 1 do
            if not (Float.is_finite (Int32.float_of_bits (Bytes.get_int32_le bytes (index * 4))))
            then valid := false
          done;
          let lights =
            if Bytes.length bytes = 5456 then
              Int32.float_of_bits (Bytes.get_int32_le bytes (73 * 4))
            else 0.
          in
          !valid && lights >= 0. && lights <= 64. && Float.is_integer lights)
      uniforms
  in
  let key =
    mesh_key_string mesh.key
    ^ if canonical_plain then ":canonical-scene2" else if nonindexed then ":nonindexed" else ""
  in
  let upload existing =
    let vertex_stride =
      if mesh.vertex_count = 0 then 0 else Bytes.length mesh.vertices / mesh.vertex_count
    in
    let index_at index = Int32.to_int (Bytes.get_int32_le mesh.indices (index * 4)) in
    let valid_indices = ref (valid_index_length mesh) in
    if nonindexed && vertex_stride > 0 && !valid_indices then
      for index = 0 to mesh.index_count - 1 do
        let source = index_at index in
        if source < 0 || source >= mesh.vertex_count then valid_indices := false
      done;
    let vertices, indices, vertex_count, index_count =
      if not nonindexed then (mesh.vertices, mesh.indices, mesh.vertex_count, mesh.index_count)
      else if
        vertex_stride <= 0
        || Bytes.length mesh.vertices <> mesh.vertex_count * vertex_stride
        || not !valid_indices
      then (Bytes.empty, Bytes.empty, 0, 0)
      else if canonical_plain then (
        let expanded = Bytes.make (mesh.index_count * 68) '\000' in
        for index = 0 to mesh.index_count - 1 do
          let source = index_at index and target = index * 68 in
          if vertex_stride >= 24 then begin
            Bytes.set_int64_le expanded target
              (Bytes.get_int64_le mesh.vertices (source * vertex_stride));
            Bytes.set_int64_le expanded (target + 8)
              (Bytes.get_int64_le mesh.vertices ((source * vertex_stride) + 8))
          end
          else begin
            Bytes.set_int64_le expanded target
              (Int64.bits_of_float
                 (Int32.float_of_bits (Bytes.get_int32_le mesh.vertices (source * vertex_stride))));
            Bytes.set_int64_le expanded (target + 8)
              (Int64.bits_of_float
                 (Int32.float_of_bits
                    (Bytes.get_int32_le mesh.vertices ((source * vertex_stride) + 4))))
          end;
          Bytes.set_int32_le expanded (target + 48)
            (if vertex_stride >= 24 then
               Bytes.get_int32_le mesh.vertices ((source * vertex_stride) + 16)
             else Bytes.get_int32_le mesh.vertices ((source * vertex_stride) + 8));
          Bytes.set_int64_le expanded (target + 52) (Int64.bits_of_float 0.5);
          Bytes.set_int64_le expanded (target + 60) (Int64.bits_of_float 0.5)
        done;
        (expanded, Bytes.empty, mesh.index_count, 0))
      else
        let expanded = Bytes.create (mesh.index_count * vertex_stride) in
        for index = 0 to mesh.index_count - 1 do
          let source = index_at index in
          Bytes.blit mesh.vertices (source * vertex_stride) expanded (index * vertex_stride)
            vertex_stride
        done;
        (expanded, Bytes.empty, mesh.index_count, 0)
    in
    let total = Bytes.length vertices + Bytes.length indices + Bytes.length uniform_bytes in
    if
      mesh_key_string mesh.key = ""
      || vertex_count <= 0
      || ((not nonindexed) && index_count <= 0)
      || total = 0 || not valid_uniforms
    then
      error "Scene_execution.prepare" Ogpu.Error.Invalid_argument
        "mesh payload or transform uniforms are malformed"
    else
      let payload_hash = if vertex_stable then "" else content_hash mesh uniform_bytes in
      let uniform_offset =
        if uniforms = None then None
        else Some (Int64.of_int (Bytes.length vertices + Bytes.length indices))
      in
      let uniform_copy =
        if Bytes.length uniform_bytes = 0 then Bytes.empty else Bytes.copy uniform_bytes
      in
      let rewrite (item : cached) =
        let packed = Bytes.concat Bytes.empty [ vertices; indices; uniform_bytes ] in
        match Ogpu.Backend.write_buffer item.buffer ~offset:0L packed with
        | Error _ as e -> e
        | Ok () ->
            let stolen = item.key <> key in
            if stolen then ignore (String_table.take value.cache item.key);
            item.key <- key;
            item.payload_hash <- payload_hash;
            item.trusted_source <- source_for_trust mesh item.bytes;
            item.index_offset <- Int64.of_int (Bytes.length vertices);
            item.uniform_offset <- uniform_offset;
            item.uniform_copy <- uniform_copy;
            item.vertex_count <- vertex_count;
            item.index_count <- index_count;
            item.primitive <- mesh.primitive;
            if stolen then String_table.add value.cache ~bytes:item.bytes key item;
            value.uploaded <- Int64.add value.uploaded (Int64.of_int total);
            Ok item
      in
      let reusable =
        match existing with
        | Some (item : cached) when item.bytes = total && not (reserved item) -> Some item
        | _ when String_table.length value.cache >= mesh_cache_entry_capacity ->
            String_table.find_first value.cache (fun _ (x : cached) ->
                x.bytes = total && not (reserved x))
        | _ -> None
      in
      match reusable with
      | Some item -> rewrite item
      | None -> (
          let descriptor : Ogpu.Types.buffer_descriptor =
            {
              label = Some ("scene-mesh-" ^ mesh_key_string mesh.key);
              size = Int64.of_int total;
              usage = [ Vertex; Index; Storage; Copy_dst ];
            }
          in
          match Ogpu.Backend.create_buffer value.device descriptor with
          | Error _ as e -> e
          | Ok buffer -> (
              let offset = Int64.of_int (Bytes.length vertices) in
              (* Indices and uniforms are written apart: joining them first copied every index. *)
              let write offset bytes =
                match Ogpu.Backend.write_buffer buffer ~offset bytes with
                | Error e ->
                    ignore (Ogpu.Backend.destroy_buffer buffer);
                    Error e
                | Ok () -> Ok ()
              in
              match write 0L vertices with
              | Error e -> Error e
              | Ok () -> (
                  match write offset indices with
                  | Error e -> Error e
                  | Ok () -> (
                      match
                        if Bytes.length uniform_bytes = 0 then Ok ()
                        else
                          write
                            (Int64.add offset (Int64.of_int (Bytes.length indices)))
                            uniform_bytes
                      with
                      | Error e -> Error e
                      | Ok () ->
                          let item =
                            {
                              key;
                              payload_hash;
                              trusted_source = source_for_trust mesh total;
                              buffer;
                              index_offset = offset;
                              uniform_offset;
                              uniform_copy;
                              vertex_count;
                              index_count;
                              primitive = mesh.primitive;
                              bytes = total;
                            }
                          in
                          String_table.add value.cache ~bytes:total key item;
                          value.uploaded <- Int64.add value.uploaded (Int64.of_int total);
                          Ok item))))
  in
  match String_table.find value.cache key with
  | exception Not_found -> upload None
  | item when item.primitive <> mesh.primitive -> upload (Some item)
  | item when vertex_stable -> (
      if Bytes.length uniform_bytes = 0 || Bytes.equal uniform_bytes item.uniform_copy then Ok item
      else
        match item.uniform_offset with
        | None -> Ok item
        | Some offset -> (
            match Ogpu.Backend.write_buffer item.buffer ~offset uniform_bytes with
            | Error _ as error -> error
            | Ok () ->
                Bytes.blit uniform_bytes 0 item.uniform_copy 0 (Bytes.length uniform_bytes);
                value.uploaded <-
                  Int64.add value.uploaded (Int64.of_int (Bytes.length uniform_bytes));
                Ok item))
  | item when same_bytes mesh item.trusted_source && Bytes.equal uniform_bytes item.uniform_copy ->
      Ok item
  | item ->
      if item.payload_hash <> "" && item.payload_hash = content_hash mesh uniform_bytes then begin
        item.trusted_source <- source_for_trust mesh item.bytes;
        Ok item
      end
      else upload (Some item)

let uniform_page_byte_capacity = 256 * 1024 * 1024
let align256 value = (value + 255) land lnot 255

let scene2_identity_affine =
  let bytes = Bytes.make 24 '\000' in
  Bytes.set_int32_le bytes 0 (Int32.bits_of_float 1.);
  Bytes.set_int32_le bytes 16 (Int32.bits_of_float 1.);
  bytes

let scene2_native_affine bytes =
  if Bytes.length bytes = 24 then bytes
  else
    let native = Bytes.make 24 '\000' in
    for index = 0 to 5 do
      Bytes.set_int32_le native (index * 4)
        (Int32.bits_of_float (Int64.float_of_bits (Bytes.get_int64_le bytes (index * 8))))
    done;
    native

(* ponytail: a changed transform repacks the frame; use dirty ranges only if
   sparse-change profiles justify tracking them beside P3-2 scene identities. *)
let prepare_uniforms value ~defer sources =
  let previous = value.previous_uniforms in
  let same =
    Array.length sources = Array.length previous
    && Array.for_all2
         (fun source retained ->
           match (source, retained) with
           | None, None -> true
           | Some bytes, Some slice ->
               let length = Bytes.length bytes in
               length = slice.uniform_length
               &&
               let index = ref 0 in
               while
                 !index < length
                 && Bytes.unsafe_get bytes !index
                    = Bytes.unsafe_get slice.uniform_staging (slice.uniform_staging_offset + !index)
               do
                 incr index
               done;
               !index = length
           | _ -> false)
         sources previous
  in
  (* Scene3 blocks (5456 bytes plus 192 per instance) bypass [prepare]'s
     payload check, so reject non-finite values and light counts outside
     0..64 here, before anything is written; unchanged frames skip this. *)
  let malformed_scene3 () =
    Array.exists
      (function
        | Some bytes when Bytes.length bytes >= 5456 ->
            let finite = ref true and index = ref 0 and count = Bytes.length bytes / 4 in
            while !finite && !index < count do
              if not (Float.is_finite (Int32.float_of_bits (Bytes.get_int32_le bytes (!index * 4))))
              then finite := false;
              incr index
            done;
            let lights = Int32.float_of_bits (Bytes.get_int32_le bytes (73 * 4)) in
            not (!finite && lights >= 0. && lights <= 64. && Float.is_integer lights)
        | _ -> false)
      sources
  in
  if same then Ok previous
  else if malformed_scene3 () then
    error "Scene_execution.prepare_uniforms" Ogpu.Error.Invalid_argument
      "Scene3 transform uniforms are malformed"
  else begin
    drop_plans value;
    value.previous_uniforms <- [||];
    let needed = ref 0 and valid = ref true in
    Array.iter
      (function
        | None -> ()
        | Some bytes ->
            let length = Bytes.length bytes in
            if length > uniform_page_byte_capacity then valid := false
            else
              let aligned = align256 length in
              if !needed > uniform_page_byte_capacity - aligned then valid := false
              else needed := !needed + aligned)
      sources;
    if not !valid then
      error "Scene_execution.prepare_uniforms" Ogpu.Error.Capacity
        "one submission exceeds the bounded transform-uniform bytes"
    else if !needed = 0 then Ok (Array.make (Array.length sources) None)
    else
      let page_index = value.next_uniform_page in
      let page =
        match value.uniform_pages.(page_index) with
        | Some page when page.page_capacity >= !needed -> Ok page
        | old ->
            let rec capacity current =
              if current >= !needed then current
              else capacity (min uniform_page_byte_capacity (current * 2))
            in
            let capacity = capacity 4096 in
            let descriptor : Ogpu.Types.buffer_descriptor =
              {
                label = Some "scene-transform-ring";
                size = Int64.of_int capacity;
                usage = [ Storage; Copy_dst ];
              }
            in
            Result.map
              (fun buffer ->
                Option.iter
                  (fun page ->
                    defer (fun () -> ignore (Ogpu.Backend.destroy_buffer page.page_buffer)))
                  old;
                let page = { page_buffer = buffer; page_capacity = capacity } in
                value.uniform_pages.(page_index) <- Some page;
                page)
              (Ogpu.Backend.create_buffer value.device descriptor)
      in
      match page with
      | Error _ as e -> e
      | Ok page -> (
          value.next_uniform_page <- (page_index + 1) mod Array.length value.uniform_pages;
          (* Pack every slice into one staging block and upload it once; the
           page is a bump allocator, not a list of per-draw buffers. *)
          let prepared = Array.make (Array.length sources) None
          and cursor = ref 0
          and staging = Bytes.create !needed
          and payload = ref 0 in
          Array.iteri
            (fun index -> function
              | None -> ()
              | Some bytes ->
                  let length = Bytes.length bytes in
                  Bytes.blit bytes 0 staging !cursor length;
                  prepared.(index) <-
                    Some
                      {
                        uniform_buffer = page.page_buffer;
                        uniform_offset = Int64.of_int !cursor;
                        uniform_staging = staging;
                        uniform_staging_offset = !cursor;
                        uniform_length = length;
                      };
                  cursor := !cursor + align256 length;
                  payload := !payload + length)
            sources;
          match Ogpu.Backend.write_buffer page.page_buffer ~offset:0L staging with
          | Error _ as e -> e
          | Ok () ->
              value.uploaded <- Int64.add value.uploaded (Int64.of_int !payload);
              Ok prepared)
  end

(* The texel format follows from level 0's byte count: 4, 8 or 16 bytes per
   texel are Rgba8_unorm, Rgba16_float and Rgba32_float. *)
let texel_bytes (source : sampled_texture) =
  let level = source.levels.(0) in
  if level.width <= 0 || level.height <= 0 then 0
  else Bytes.length level.bytes / (level.width * level.height)

let texel_format = function 8 -> Ogpu.Types.Rgba16_float | 16 -> Rgba32_float | _ -> Rgba8_unorm

let valid_texture (source : sampled_texture) =
  source.key <> ""
  && Array.length source.levels > 0
  && (match Ogpu.Validation.validate_sampler source.sampler with Error _ -> false | Ok () -> true)
  &&
  match source.gpu with
  | Some texture ->
      let descriptor = Ogpu.Backend.texture_descriptor texture in
      (not (Ogpu.Backend.texture_destroyed texture))
      && Array.length source.levels = 1
      && source.levels.(0).width = descriptor.width
      && source.levels.(0).height = descriptor.height
      && List.mem Ogpu.Types.Texture_binding descriptor.usage
      && Bytes.length source.levels.(0).bytes = 0
  | None ->
      let texel = texel_bytes source in
      List.mem texel [ 4; 8; 16 ]
      && Array.mapi
           (fun index (level : texture_level) ->
             level.width = max 1 (source.levels.(0).width lsr index)
             && level.height = max 1 (source.levels.(0).height lsr index)
             && Bytes.length level.bytes = level.width * level.height * texel)
           source.levels
         |> Array.for_all Fun.id

let scene2_white_texture : sampled_texture =
  {
    key = "scene2:canonical-white";
    levels = [| { width = 1; height = 1; bytes = Bytes.of_string "\255\255\255\255" } |];
    gpu = None;
    sampler =
      {
        label = Some "scene2-canonical-white";
        min_filter = Nearest;
        mag_filter = Nearest;
        mip_filter = No_mip;
        address_u = Clamp_to_edge;
        address_v = Clamp_to_edge;
        lod_min = 0.;
        lod_max = 0.;
        max_anisotropy = 1;
      };
  }

let texture_upload_scratch value total =
  match value.texture_upload_scratch with
  | Some scratch when scratch.scratch_size = total -> Ok scratch
  | previous -> (
      let descriptor : Ogpu.Types.buffer_descriptor =
        { label = Some "scene-texture-staging"; size = Int64.of_int total; usage = [ Copy_src ] }
      in
      match Ogpu.Backend.create_buffer value.device descriptor with
      | Error _ as error -> error
      | Ok scratch_buffer ->
          let scratch =
            { scratch_buffer; scratch_bytes = Bytes.make total '\000'; scratch_size = total }
          in
          value.texture_upload_scratch <- Some scratch;
          Option.iter (fun old -> ignore (Ogpu.Backend.destroy_buffer old.scratch_buffer)) previous;
          Ok scratch)

let image_or_canvas_key key =
  String.starts_with ~prefix:"canvas:" key
  || String.starts_with ~prefix:"image:" key
  || String.starts_with ~prefix:"texture:" key
  || String.starts_with ~prefix:"text:" key
  || String.starts_with ~prefix:"world:" key

let prepare_texture value ~defer (source : sampled_texture) =
  match source.gpu with
  | Some texture ->
      let device_id, _ = Ogpu.Backend.texture_driver_token texture in
      if not (valid_texture source) then
        error "Scene_execution.prepare_texture" Ogpu.Error.Invalid_argument
          "GPU image is malformed or destroyed"
      else if device_id <> Ogpu.Handle.device_id (Ogpu.Backend.device_handle value.device) then
        error "Scene_execution.prepare_texture" Ogpu.Error.Cross_device
          "GPU image belongs to another renderer"
      else
        Ok
          {
            texture_key = source.key;
            texture_hash = "";
            texture_shape = "";
            texture;
            texture_bytes = 0;
            texture_used = value.frame;
            texture_levels = [||];
          }
  | None -> (
      let levels_hash () =
        Digest.to_hex
          (Digest.string
             (Array.to_list source.levels
             |> List.map (fun (level : texture_level) ->
                 Printf.sprintf "%dx%d:%s" level.width level.height
                   (Digest.to_hex (Digest.string (Bytes.unsafe_to_string level.bytes))))
             |> String.concat "|"))
      in
      (* Image, canvas, texture and text keys carry identity and generation, so their
     bytes are never hashed; other keys are content-addressed. *)
      let upload ~hash ~reusable =
        if not (valid_texture source) then
          error "Scene_execution.prepare_texture" Ogpu.Error.Invalid_argument
            "texture or sampler is malformed"
        else
          let texel = texel_bytes source in
          let shape =
            (if texel = 4 then "" else string_of_int texel ^ "B:")
            ^ (Array.to_list source.levels
              |> List.map (fun (level : texture_level) ->
                  Printf.sprintf "%dx%d" level.width level.height)
              |> String.concat "/")
          in
          (* Canvas and managed-image identities are unique and lower to one
       authoritative generation per staged frame, so their same-shape storage
       can be updated safely between completed submissions.
       A replacement image of the same shape may steal an unused cached
       texture; two same-shape images in one submission keep distinct
       textures because an in-use slot cannot be stolen. *)
          let reusable =
            match reusable with
            | Some _ as hit -> hit
            | None when not (image_or_canvas_key source.key) -> None
            | None ->
                String_table.find_first value.texture_cache (fun _ item ->
                    item.texture_shape = shape
                    && image_or_canvas_key item.texture_key
                    && item.texture_used <> value.frame)
          in
          let descriptor : Ogpu.Types.texture_descriptor =
            {
              label = Some ("scene-texture-" ^ source.key);
              width = source.levels.(0).width;
              height = source.levels.(0).height;
              depth = 1;
              mip_levels = Array.length source.levels;
              sample_count = 1;
              format = texel_format texel;
              usage = [ Texture_binding; Texture_copy_dst ];
            }
          in
          let rows =
            Array.map (fun (level : texture_level) -> align256 (level.width * texel)) source.levels
          in
          let offsets = Array.make (Array.length source.levels) 0 in
          for index = 1 to Array.length offsets - 1 do
            offsets.(index) <-
              offsets.(index - 1) + (rows.(index - 1) * source.levels.(index - 1).height)
          done;
          let total =
            offsets.(Array.length offsets - 1)
            + (rows.(Array.length rows - 1) * source.levels.(Array.length rows - 1).height)
          in
          let cacheable = total <= texture_cache_byte_capacity in
          let texture =
            match reusable with
            | Some item when cacheable -> Ok (item.texture, false)
            | _ ->
                Result.map
                  (fun texture -> (texture, true))
                  (Ogpu.Backend.create_texture value.device descriptor)
          in
          match texture with
          | Error _ as error -> error
          | Ok (texture, created) -> (
              match texture_upload_scratch value total with
              | Error error ->
                  if created then ignore (Ogpu.Backend.destroy_texture texture);
                  Error error
              | Ok scratch -> (
                  let staging = scratch.scratch_buffer and packed = scratch.scratch_bytes in
                  Array.iteri
                    (fun level_index (level : texture_level) ->
                      for row = 0 to level.height - 1 do
                        Bytes.blit level.bytes
                          (row * level.width * texel)
                          packed
                          (offsets.(level_index) + (row * rows.(level_index)))
                          (level.width * texel)
                      done)
                    source.levels;
                  match Ogpu.Backend.write_buffer staging ~offset:0L packed with
                  | Error e ->
                      if created then ignore (Ogpu.Backend.destroy_texture texture);
                      Error e
                  | Ok () -> (
                      let failure = ref None in
                      let finish result =
                        match result with
                        | Error e ->
                            if created then ignore (Ogpu.Backend.destroy_texture texture);
                            Error e
                        | Ok () -> (
                            value.uploaded <- Int64.add value.uploaded (Int64.of_int total);
                            match reusable with
                            | Some item when item.texture == texture ->
                                if item.texture_key <> source.key then begin
                                  ignore (String_table.take value.texture_cache item.texture_key);
                                  item.texture_key <- source.key;
                                  String_table.add value.texture_cache ~bytes:item.texture_bytes
                                    source.key item
                                end;
                                item.texture_hash <- hash;
                                item.texture_levels <- source.levels;
                                item.texture_used <- value.frame;
                                Ok item
                            | _ ->
                                let item =
                                  {
                                    texture_key = source.key;
                                    texture_hash = hash;
                                    texture_shape = shape;
                                    texture;
                                    texture_bytes = total;
                                    texture_used = value.frame;
                                    texture_levels = source.levels;
                                  }
                                in
                                if not cacheable then begin
                                  defer (fun () -> ignore (Ogpu.Backend.destroy_texture texture));
                                  Ok item
                                end
                                else begin
                                  String_table.add value.texture_cache ~bytes:total source.key item;
                                  Ok item
                                end)
                      in
                      match !failure with
                      | Some e -> finish (Error e)
                      | None ->
                          let uploaded =
                            let ( let* ) = Result.bind in
                            let* commands = Ogpu.Backend.begin_commands value.queue in
                            let encoded =
                              let* blit = Ogpu.Backend.blit_encoder commands in
                              let* () =
                                Array.fold_left
                                  (fun result index ->
                                    let* () = result in
                                    let level = source.levels.(index) in
                                    Ogpu.Backend.buffer_to_texture blit ~src:staging
                                      ~offset:(Int64.of_int offsets.(index))
                                      ~bytes_per_row:(Int64.of_int rows.(index))
                                      ~bytes_per_image:(Int64.of_int (rows.(index) * level.height))
                                      ~dst:texture ~mip:index
                                      ~extent:
                                        { width = level.width; height = level.height; depth = 1 }
                                      ())
                                  (Ok ())
                                  (Array.init (Array.length source.levels) Fun.id)
                              in
                              let* () = Ogpu.Backend.end_blit blit in
                              Ogpu.Backend.commit commands
                            in
                            match encoded with
                            | Error e ->
                                ignore (Ogpu.Backend.abandon commands);
                                Error e
                            | Ok receipt -> Ogpu.Backend.complete_through value.queue receipt.epoch
                          in
                          finish uploaded)))
      in
      match String_table.find value.texture_cache source.key with
      | item
        when String.starts_with ~prefix:"image:" source.key
             || String.starts_with ~prefix:"texture:" source.key
             || String.starts_with ~prefix:"shadow:" source.key
             || String.starts_with ~prefix:"text:" source.key
             || String.starts_with ~prefix:"world:" source.key ->
          item.texture_used <- value.frame;
          Ok item
      | item when String.starts_with ~prefix:"canvas:" source.key ->
          upload ~hash:"" ~reusable:(Some item)
      (* The immutable levels this slot was uploaded from: no hashing. *)
      | item when item.texture_levels == source.levels ->
          item.texture_used <- value.frame;
          Ok item
      | item ->
          let hash = levels_hash () in
          if item.texture_hash = hash then (
            item.texture_levels <- source.levels;
            item.texture_used <- value.frame;
            Ok item)
          else upload ~hash ~reusable:None
      | exception Not_found ->
          upload
            ~hash:(if image_or_canvas_key source.key then "" else levels_hash ())
            ~reusable:None)

let prepare_storage value ~key ~bytes =
  let upload () =
    let descriptor : Ogpu.Types.buffer_descriptor =
      {
        label = Some ("scene-auxiliary-" ^ key);
        size = Int64.of_int (Bytes.length bytes);
        usage = [ Storage; Copy_dst ];
      }
    in
    match Ogpu.Backend.create_buffer value.device descriptor with
    | Error _ as e -> e
    | Ok buffer -> (
        match Ogpu.Backend.write_buffer buffer ~offset:0L bytes with
        | Error e ->
            ignore (Ogpu.Backend.destroy_buffer buffer);
            Error e
        | Ok () ->
            let item =
              {
                auxiliary_copy = Bytes.copy bytes;
                auxiliary_buffer = buffer;
                auxiliary_source = bytes;
              }
            in
            String_table.add value.auxiliary_cache ~bytes:(Bytes.length bytes) key item;
            value.uploaded <- Int64.add value.uploaded (Int64.of_int (Bytes.length bytes));
            Ok item)
  in
  match String_table.find value.auxiliary_cache key with
  | item when item.auxiliary_source == bytes -> Ok item
  | item when Bytes.equal item.auxiliary_copy bytes ->
      item.auxiliary_source <- bytes;
      Ok item
  | _ -> upload ()
  | exception Not_found -> upload ()

let prepare_auxiliary value (source : auxiliary_resource) =
  prepare_storage value ~key:source.key ~bytes:source.buffer

let u32_le bytes offset =
  Int32.logor
    (Int32.of_int (Char.code (Bytes.get bytes offset)))
    (Int32.logor
       (Int32.shift_left (Int32.of_int (Char.code (Bytes.get bytes (offset + 1)))) 8)
       (Int32.logor
          (Int32.shift_left (Int32.of_int (Char.code (Bytes.get bytes (offset + 2)))) 16)
          (Int32.shift_left (Int32.of_int (Char.code (Bytes.get bytes (offset + 3)))) 24)))

let mesh_identity (mesh : mesh) =
  mesh_key_string mesh.key ^ ":"
  ^ (match mesh.primitive with
    | Point_list -> "P"
    | Line_list -> "L"
    | Triangle_list -> "T"
    | Triangle_strip -> "S")
  ^ ":"
  ^ Digest.to_hex (Digest.bytes mesh.vertices)
  ^ ":"
  ^ Digest.to_hex (Digest.bytes mesh.indices)

let combine_meshes meshes =
  let operation = "Scene_execution.combine_meshes" in
  let valid_indices (mesh : mesh) =
    let valid = ref true in
    for index = 0 to mesh.index_count - 1 do
      let value = Int32.to_int (u32_le mesh.indices (index * 4)) in
      if value < 0 || value >= mesh.vertex_count then valid := false
    done;
    !valid
  in
  let rec plan vertex_bytes vertex_count index_count identities = function
    | [] -> Ok (vertex_bytes, vertex_count, index_count, List.rev identities)
    | (mesh : mesh) :: rest ->
        if
          mesh.vertex_count <= 0
          || Bytes.length mesh.vertices mod mesh.vertex_count <> 0
          || (not (valid_index_length mesh))
          || not (valid_indices mesh)
        then error operation Ogpu.Error.Invalid_argument "packed mesh cardinality is inconsistent"
        else if
          vertex_bytes > Sys.max_string_length - Bytes.length mesh.vertices
          || vertex_count > Int32.to_int Int32.max_int - mesh.vertex_count
          || index_count > (Sys.max_string_length / 4) - mesh.index_count
        then
          error operation Ogpu.Error.Capacity "combined mesh exceeds packed representation limits"
        else
          plan
            (vertex_bytes + Bytes.length mesh.vertices)
            (vertex_count + mesh.vertex_count)
            (index_count + mesh.index_count)
            (mesh_identity mesh :: identities)
            rest
  in
  match plan 0 0 0 [] meshes with
  | Error _ as result -> result
  | Ok (vertex_bytes, vertex_count, index_count, identities) ->
      let vertices = Bytes.create vertex_bytes in
      let indices = Bytes.create (index_count * 4) in
      let rec copy vertex_offset vertex_base index_offset = function
        | [] -> ()
        | (mesh : mesh) :: rest ->
            Bytes.blit mesh.vertices 0 vertices vertex_offset (Bytes.length mesh.vertices);
            for index = 0 to mesh.index_count - 1 do
              let source = Int32.to_int (u32_le mesh.indices (index * 4)) in
              set_u32_le indices (index_offset + (index * 4)) (Int32.of_int (source + vertex_base))
            done;
            copy
              (vertex_offset + Bytes.length mesh.vertices)
              (vertex_base + mesh.vertex_count)
              (index_offset + (mesh.index_count * 4))
              rest
      in
      copy 0 0 0 meshes;
      let identity = Digest.to_hex (Digest.string (String.concat "|" identities)) in
      Ok
        {
          key = Host ("batch:" ^ identity);
          vertices;
          vertex_count;
          indices;
          index_count;
          primitive = (List.hd meshes).primitive;
        }

let same_optional_resource a b =
  match (a, b) with None, None -> true | Some a, Some b -> a == b | _ -> false

let coalesce_draws draws =
  let stride (mesh : mesh) =
    if mesh.vertex_count = 0 then 0 else Bytes.length mesh.vertices / mesh.vertex_count
  in
  let compatible (first : sampled_draw) (next : sampled_draw) =
    (match (first.draw.mesh.key, next.draw.mesh.key) with Host _, Host _ -> true | _ -> false)
    && first.family = next.family && first.blend = next.blend && first.samples = next.samples
    && first.draw.state = next.draw.state
    && first.draw.mesh.primitive = next.draw.mesh.primitive
    && stride first.draw.mesh = stride next.draw.mesh
    && same_optional_resource first.texture next.texture
    && same_optional_resource first.auxiliary next.auxiliary
    && Option.is_some first.vertex_attributes = Option.is_some next.vertex_attributes
  in
  let rec take first packed_bytes entries = function
    | next :: rest when compatible first next ->
        let draw = next.draw in
        let bytes =
          Bytes.length draw.mesh.vertices + Bytes.length draw.mesh.indices
          + Option.fold ~none:0 ~some:(fun (_, bytes) -> Bytes.length bytes) next.vertex_attributes
        in
        if packed_bytes <= cache_byte_capacity - bytes then
          take first (packed_bytes + bytes) (next :: entries) rest
        else (List.rev entries, next :: rest)
    | rest -> (List.rev entries, rest)
  in
  let rec loop result = function
    | [] -> Ok (List.rev result)
    | (first : sampled_draw) :: rest -> (
        let draw = first.draw in
        let first_bytes =
          Bytes.length draw.mesh.vertices + Bytes.length draw.mesh.indices
          + Option.fold ~none:0 ~some:(fun (_, bytes) -> Bytes.length bytes) first.vertex_attributes
        in
        if first_bytes > cache_byte_capacity then
          error "Scene_execution.coalesce" Ogpu.Error.Capacity
            "one mesh exceeds the batch byte capacity"
        else
          let entries, rest = take first first_bytes [ first ] rest in
          match entries with
          | [ _ ] -> loop (first :: result) rest
          (* Small runs retain their independent stable identities. Coalescing a
           changing member into a shared buffer otherwise reuploads every
           unchanged neighbour. Large runs still coalesce to bound caches and
           draw payloads. *)
          | _ when List.length entries <= 64 -> loop (List.rev_append entries result) rest
          | _ -> (
              let meshes = List.map (fun entry -> entry.draw.mesh) entries in
              match combine_meshes meshes with
              | Error _ as result -> result
              | Ok mesh ->
                  let vertex_attributes =
                    Option.map
                      (fun _ ->
                        let attributes =
                          List.map (fun entry -> Option.get entry.vertex_attributes) entries
                        in
                        let key =
                          "batch:attributes:"
                          ^ Digest.to_hex
                              (Digest.string (String.concat "|" (List.map fst attributes)))
                        in
                        (key, Bytes.concat Bytes.empty (List.map snd attributes)))
                      first.vertex_attributes
                  in
                  loop ({ first with vertex_attributes; draw = { draw with mesh } } :: result) rest)
        )
  in
  loop [] draws

let prepared_bytes draws =
  List.fold_left
    (fun total entry ->
      total
      + Bytes.length entry.draw.mesh.vertices
      + Bytes.length entry.draw.mesh.indices
      + Option.fold ~none:0 ~some:(fun (_, bytes) -> Bytes.length bytes) entry.vertex_attributes)
    0 draws

let resolve_prepared value prepared draws =
  match prepared with
  | Some (identity, _version) when identity = "" ->
      error "Scene_execution.prepare_run" Ogpu.Error.Invalid_argument "prepared identity is empty"
  | Some (identity, version) -> (
      let rebuild () =
        Result.map
          (fun prepared_draws ->
            let item =
              {
                prepared_version = version;
                prepared_draws;
                prepared_bytes = prepared_bytes prepared_draws;
              }
            in
            String_table.add value.prepared_cache ~bytes:item.prepared_bytes identity item;
            prepared_draws)
          (coalesce_draws draws)
      in
      match String_table.find value.prepared_cache identity with
      | item when item.prepared_version = version -> Ok item.prepared_draws
      | _ -> rebuild ()
      | exception Not_found -> rebuild ())
  | None -> coalesce_draws draws

let intersect_extent ~bound_w ~bound_h (x, y, w, h) =
  let x0 = max 0 x and y0 = max 0 y in
  let x1 = min bound_w (x + w) and y1 = min bound_h (y + h) in
  if x1 <= x0 || y1 <= y0 then None else Some (x0, y0, x1 - x0, y1 - y0)

let automatic_signature
    ( family,
      blend,
      samples,
      state,
      texture,
      auxiliary,
      environment,
      item,
      uniform,
      attributes,
      gpu_vertices ) =
  (* Submission signatures own affine bytes.  Scene values are immutable by
     contract, but callers may reuse their input buffer after [render] returns;
     retaining it here would turn later mutation into a false cache hit. *)
  (* Completed ring slices may be replayed when the whole uniform set is
     unchanged. A changed set gets a different page and offsets. *)
  let state =
    {
      state with
      transform_uniforms =
        (match uniform with
        | Some _ -> None
        | None -> Option.map Bytes.copy state.transform_uniforms);
    }
  in
  let texture =
    Option.map
      (fun (source, cached) -> (Ogpu.Backend.texture_id cached.texture, source.sampler))
      texture
  in
  let auxiliary =
    Option.map
      (fun ((source : auxiliary_resource), buffer, texture) ->
        ( Ogpu.Backend.buffer_id buffer.auxiliary_buffer,
          Ogpu.Backend.texture_id texture.texture,
          source.texture.sampler ))
      auxiliary
  in
  {
    signature_family = family;
    signature_blend = blend;
    signature_samples = samples;
    signature_state = state;
    signature_texture = texture;
    signature_auxiliary = auxiliary;
    signature_environment =
      Option.map
        (fun ((source : sampled_texture), cached) ->
          (Ogpu.Backend.texture_id cached.texture, source.sampler))
        environment;
    signature_buffer = Ogpu.Backend.buffer_id item.buffer;
    signature_gpu_vertices = Option.map gpu_identity gpu_vertices;
    signature_vertex_attributes =
      Option.map (fun item -> Ogpu.Backend.buffer_id item.auxiliary_buffer) attributes;
    signature_index_offset = item.index_offset;
    signature_uniform_buffer =
      Option.map (fun item -> Ogpu.Backend.buffer_id item.uniform_buffer) uniform;
    signature_uniform_offset = Option.map (fun item -> item.uniform_offset) uniform;
    signature_vertex_count = item.vertex_count;
    signature_index_count = item.index_count;
  }

let prepared_scratch_capacity = 65_536

let make_prepared_slot () =
  {
    slot_family = Scene2;
    slot_blend = Ogpu.Pipeline.Replace;
    slot_samples = 1;
    slot_state = None;
    slot_texture = None;
    slot_auxiliary = None;
    slot_environment = None;
    slot_mesh = None;
    slot_uniform = None;
    slot_vertex_attributes = None;
    slot_gpu_vertices = None;
  }

let ensure_prepared_scratch scratch needed =
  if needed > prepared_scratch_capacity then
    error "Renderer.submit" Ogpu.Error.Capacity
      "one submission exceeds the bounded prepared-draw capacity"
  else if needed <= Array.length scratch.scratch_slots then Ok ()
  else
    let rec capacity current =
      if current >= needed then current
      else capacity (min prepared_scratch_capacity (max 16 (current * 2)))
    in
    let old = scratch.scratch_slots in
    scratch.scratch_slots <-
      Array.init
        (capacity (Array.length old))
        (fun index ->
          if index < Array.length old then Array.unsafe_get old index else make_prepared_slot ());
    Ok ()

let clear_prepared_scratch scratch =
  for index = 0 to scratch.scratch_length - 1 do
    let slot = Array.unsafe_get scratch.scratch_slots index in
    slot.slot_state <- None;
    slot.slot_texture <- None;
    slot.slot_auxiliary <- None;
    slot.slot_environment <- None;
    slot.slot_gpu_vertices <- None;
    slot.slot_mesh <- None;
    slot.slot_uniform <- None;
    slot.slot_vertex_attributes <- None
  done;
  scratch.scratch_length <- 0

let scratch_mem_mesh scratch item =
  let index = ref 0 and found = ref false in
  while (not !found) && !index < scratch.scratch_length do
    let slot = Array.unsafe_get scratch.scratch_slots !index in
    (found := match slot.slot_mesh with Some candidate -> candidate == item | None -> false);
    incr index
  done;
  !found

let same_automatic_slot signature slot =
  match (slot.slot_state, slot.slot_mesh) with
  | Some state, Some item ->
      let same_state left right =
        left.viewport = right.viewport && left.scissor = right.scissor && left.cull = right.cull
        && left.depth_compare = right.depth_compare
        && left.depth_write = right.depth_write
        && left.depth_load = right.depth_load
        && left.depth_clear = right.depth_clear
        &&
        match slot.slot_uniform with
        | Some _ -> true
        | None -> left.transform_uniforms = right.transform_uniforms
      in
      let same_texture =
        match (signature.signature_texture, slot.slot_texture) with
        | None, None -> true
        | Some (id, sampler), Some (source, cached) ->
            id = Ogpu.Backend.texture_id cached.texture && sampler = source.sampler
        | _ -> false
      in
      let same_auxiliary =
        match (signature.signature_auxiliary, slot.slot_auxiliary) with
        | None, None -> true
        | Some (buffer_id, texture_id, sampler), Some (source, buffer, texture) ->
            buffer_id = Ogpu.Backend.buffer_id buffer.auxiliary_buffer
            && texture_id = Ogpu.Backend.texture_id texture.texture
            && sampler = source.texture.sampler
        | _ -> false
      in
      signature.signature_family = slot.slot_family
      && signature.signature_blend = slot.slot_blend
      && signature.signature_samples = slot.slot_samples
      && signature.signature_environment
         = Option.map
             (fun ((source : sampled_texture), cached) ->
               (Ogpu.Backend.texture_id cached.texture, source.sampler))
             slot.slot_environment
      && same_state signature.signature_state state
      && same_texture && same_auxiliary
      && signature.signature_buffer = Ogpu.Backend.buffer_id item.buffer
      && signature.signature_gpu_vertices = Option.map gpu_identity slot.slot_gpu_vertices
      && signature.signature_vertex_attributes
         = Option.map
             (fun item -> Ogpu.Backend.buffer_id item.auxiliary_buffer)
             slot.slot_vertex_attributes
      && signature.signature_index_offset = item.index_offset
      && signature.signature_uniform_buffer
         = Option.map (fun item -> Ogpu.Backend.buffer_id item.uniform_buffer) slot.slot_uniform
      && signature.signature_uniform_offset
         = Option.map (fun item -> item.uniform_offset) slot.slot_uniform
      && signature.signature_vertex_count = item.vertex_count
      && signature.signature_index_count = item.index_count
  | _ -> false

let same_scratch_payloads payloads scratch =
  let rec loop index = function
    | [] -> index = scratch.scratch_length
    | (signature, _, _) :: rest when index < scratch.scratch_length ->
        same_automatic_slot signature (Array.unsafe_get scratch.scratch_slots index)
        && loop (index + 1) rest
    | _ -> false
  in
  loop 0 payloads

let automatic_candidate_fingerprint scratch =
  let fingerprint = ref 0x41c64e6d in
  for index = 0 to scratch.scratch_length - 1 do
    let slot = Array.unsafe_get scratch.scratch_slots index in
    let state = match slot.slot_state with Some state -> state | None -> assert false
    and mesh = match slot.slot_mesh with Some mesh -> mesh | None -> assert false in
    let texture =
      Option.map
        (fun (source, cached) -> (Ogpu.Backend.texture_id cached.texture, source.sampler))
        slot.slot_texture
    and auxiliary =
      Option.map
        (fun ((source : auxiliary_resource), buffer, texture) ->
          ( Ogpu.Backend.buffer_id buffer.auxiliary_buffer,
            Ogpu.Backend.texture_id texture.texture,
            source.texture.sampler ))
        slot.slot_auxiliary
    in
    (* This compact value is only an admission hint.  Replay still requires
       [same_scratch_payloads], so a hash collision cannot submit stale work. *)
    fingerprint :=
      Hashtbl.seeded_hash !fingerprint
        ( slot.slot_family,
          slot.slot_blend,
          slot.slot_samples,
          state,
          texture,
          auxiliary,
          Option.map
            (fun ((source : sampled_texture), cached) ->
              (Ogpu.Backend.texture_id cached.texture, source.sampler))
            slot.slot_environment,
          Ogpu.Backend.buffer_id mesh.buffer,
          mesh.index_offset,
          Option.map gpu_identity slot.slot_gpu_vertices,
          Option.map
            (fun item -> Ogpu.Backend.buffer_id item.auxiliary_buffer)
            slot.slot_vertex_attributes,
          Option.map
            (fun uniform -> (Ogpu.Backend.buffer_id uniform.uniform_buffer, uniform.uniform_offset))
            slot.slot_uniform,
          mesh.vertex_count,
          mesh.index_count )
  done;
  !fingerprint

(* Attachments and encoder state for one render pass over [target]. *)
let render_target value family samples (state : state) load clear =
  let keys =
    (if samples = 1 then [] else [ (Scene_attachment_pool.Color, samples) ])
    @ match family with Scene2 | Scene2_textured | Ui | Marks -> [] | _ -> [ (Depth, samples) ]
  in
  match Scene_attachment_pool.acquire_many value.attachments keys with
  | Error _ as e -> e
  | Ok pooled ->
      let attachments = ref pooled in
      let take () =
        match !attachments with
        | texture :: rest ->
            attachments := rest;
            texture
        | [] -> assert false
      in
      let color = if samples = 1 then value.target else take () in
      let resolve = if samples = 1 then None else Some value.target in
      let depth =
        match family with
        | Scene2 | Scene2_textured | Ui | Marks -> None
        | _ ->
            Some
              {
                Ogpu.Backend.depth_texture = take ();
                depth_load = state.depth_load;
                depth_store = Store;
                depth_clear = state.depth_clear;
              }
      in
      Ok
        ( {
            Ogpu.Backend.colors =
              [
                {
                  texture = color;
                  resolve;
                  load;
                  store = (if samples = 1 then Store else Resolve);
                  clear;
                };
              ];
            depth;
          },
          pooled )

let has_depth = function Scene2 | Scene2_textured | Ui | Marks -> false | _ -> true

let sampler_for value descriptor =
  match Hashtbl.find_opt value.samplers descriptor with
  | Some sampler -> Ok sampler
  | None -> (
      (* ponytail: 64 distinct sampler states per renderer; a scene using more
       clears the table and drops replay plans that borrowed them. *)
      if Hashtbl.length value.samplers >= 64 then begin
        Hashtbl.iter (fun _ sampler -> ignore (Ogpu.Backend.destroy_sampler sampler)) value.samplers;
        Hashtbl.reset value.samplers;
        drop_plans value
      end;
      match Ogpu.Backend.create_sampler value.device descriptor with
      | Error _ as e -> e
      | Ok sampler ->
          Hashtbl.add value.samplers descriptor sampler;
          Ok sampler)

let argument_page_bytes = 256 * 1024

let argument_encoder variant =
  match variant.argument with
  | Some argument -> Ok argument
  | None -> (
      match Ogpu.Backend.create_argument variant.pipeline Fragment ~index:2 with
      | Error _ as e -> e
      | Ok argument ->
          variant.argument <- Some argument;
          Ok argument)

(* Canonical Scene2 binds its texture and sampler through one argument slice
   per (texture, sampler) pair inside a shared page, so indirect commands can
   reference textures. *)
let argument_slice value variant (texture : Ogpu.Backend.texture) sampler_descriptor =
  match argument_encoder variant with
  | Error _ as e -> e
  | Ok argument -> (
      let pool =
        match value.arguments with
        | Some pool -> Ok pool
        | None -> (
            let length = Ogpu.Backend.argument_length argument
            and alignment = max 1 (Ogpu.Backend.argument_alignment argument) in
            let stride = max 16 ((length + alignment - 1) / alignment * alignment) in
            let descriptor : Ogpu.Types.buffer_descriptor =
              {
                label = Some "scene-argument-page";
                size = Int64.of_int argument_page_bytes;
                usage = [ Storage ];
              }
            in
            match Ogpu.Backend.create_buffer value.device descriptor with
            | Error _ as e -> e
            | Ok argument_buffer ->
                let pool =
                  {
                    argument_buffer;
                    argument_stride = stride;
                    argument_capacity = argument_page_bytes / stride;
                    slices = Hashtbl.create 64;
                    argument_next = 0;
                  }
                in
                value.arguments <- Some pool;
                Ok pool)
      in
      match pool with
      | Error _ as e -> e
      | Ok pool -> (
          if Ogpu.Backend.argument_length argument > pool.argument_stride then
            error "Scene_execution.argument_slice" Ogpu.Error.Capacity
              "argument layout exceeds the shared slice stride"
          else
            let key = (Ogpu.Backend.texture_id texture, sampler_descriptor) in
            match Hashtbl.find_opt pool.slices key with
            | Some offset -> Ok (pool.argument_buffer, offset)
            | None -> (
                if pool.argument_next >= pool.argument_capacity then begin
                  Hashtbl.reset pool.slices;
                  pool.argument_next <- 0;
                  drop_plans value
                end;
                let offset = Int64.of_int (pool.argument_next * pool.argument_stride) in
                match sampler_for value sampler_descriptor with
                | Error _ as e -> e
                | Ok sampler -> (
                    match
                      Ogpu.Backend.argument_texture argument pool.argument_buffer ~offset ~slot:0
                        texture
                    with
                    | Error _ as e -> e
                    | Ok () -> (
                        match
                          Ogpu.Backend.argument_sampler argument pool.argument_buffer ~offset
                            ~slot:1 sampler
                        with
                        | Error _ as e -> e
                        | Ok () ->
                            pool.argument_next <- pool.argument_next + 1;
                            Hashtbl.add pool.slices key offset;
                            Ok (pool.argument_buffer, offset))))))

type acquired = Offscreen | Presented of Ogpu.Backend.frame

let discard = function Offscreen -> () | Presented frame -> ignore (Ogpu.Backend.discard frame)

(* One presented frame stays in flight: the CPU builds the next frame while
   the GPU draws this one. Waiting here, before any preparation, keeps every
   resource the previous frame used alive until it completes, so retiring
   and destroying after the next submission stays safe. *)
let settle value =
  match value.in_flight with
  | None -> Ok ()
  | Some epoch ->
      value.in_flight <- None;
      Ogpu.Backend.complete_through value.queue epoch

let acquire value =
  match settle value with
  | Error _ as error -> error
  | Ok () -> (
      match value.surface with
      | None -> Ok (`Acquired Offscreen)
      | Some surface -> (
          match Ogpu.Backend.acquire_sync surface with
          | Error _ as error -> error
          | Ok (`Timeout | `Occluded) -> Ok `Skipped
          | Ok `Device_lost -> error "Renderer.submit" Ogpu.Error.Device_lost "device lost"
          | Ok (`Acquired frame) -> Ok (`Acquired (Presented frame))))

let plain_draws draws =
  Array.for_all
    (fun draw -> draw.plan_textures = [] && draw.plan_samplers = [] && draw.plan_indirect = None)
    draws

let encode_batch value commands ~clear (batch : batch_plan) =
  let ( let* ) = Result.bind in
  let* target, _ =
    render_target value batch.batch_family batch.batch_samples batch.batch_state
      (if batch.batch_first then Ogpu.Render_pass.Clear else Load)
      clear
  in
  let* encoder = Ogpu.Backend.render_encoder commands target in
  let bound_w = value.configuration.Ogpu.Surface.physical_width
  and bound_h = value.configuration.physical_height in
  (* A viewport or scissor with no pixel on the target draws nothing.  The
     viewport is passed whole: it maps clip space, so a view that hangs over
     the target edge keeps its proportions and only the scissor is clipped. *)
  let visible, (x, y, width, height), (sx, sy, sw, sh) =
    match
      ( intersect_extent ~bound_w ~bound_h batch.batch_state.viewport,
        intersect_extent ~bound_w ~bound_h batch.batch_state.scissor )
    with
    | Some _, Some scissor -> (true, batch.batch_state.viewport, scissor)
    | _ -> (false, (0, 0, max 1 bound_w, max 1 bound_h), (0, 0, max 1 bound_w, max 1 bound_h))
  in
  let* () = Ogpu.Backend.set_viewport encoder { x; y; width; height } in
  let* () = Ogpu.Backend.set_scissor encoder { x = sx; y = sy; width = sw; height = sh } in
  let* () = Ogpu.Backend.set_cull encoder batch.batch_state.cull in
  (* 3D meshes wind counter-clockwise: a fact of the family, never of a bound slot *)
  let* () =
    if has_depth batch.batch_family then Ogpu.Backend.set_winding encoder Counter_clockwise
    else Ok ()
  in
  let* () =
    if has_depth batch.batch_family then
      Ogpu.Backend.set_depth_state encoder
        (Some
           {
             depth_compare = batch.batch_state.depth_compare;
             depth_write = batch.batch_state.depth_write;
           })
    else Ok ()
  in
  let* () =
    if Array.length batch.batch_draws = 0 || not visible then Ok ()
    else
      let* () =
        if batch.batch_resources = [] then Ok ()
        else Ogpu.Backend.use_resources encoder batch.batch_resources
      in
      match batch.batch_icb with
      | Some icb ->
          let* () =
            Ogpu.Backend.set_render_pipeline encoder batch.batch_draws.(0).plan_draw.pipeline
          in
          value.icb_stats.icb_executions <- Int64.succ value.icb_stats.icb_executions;
          Ogpu.Backend.execute_icb encoder icb ~location:0 ~length:(Array.length batch.batch_draws)
      | None ->
          if plain_draws batch.batch_draws then
            Ogpu.Backend.draw_batch encoder batch.batch_driver_draws
          else
            Array.fold_left
              (fun result draw ->
                let* () = result in
                let bd = draw.plan_draw in
                let* () = Ogpu.Backend.set_render_pipeline encoder bd.pipeline in
                let* () =
                  Array.fold_left
                    (fun result (stage, index, buffer, offset) ->
                      let* () = result in
                      Ogpu.Backend.set_stage_buffer encoder stage ~index ~offset buffer)
                    (Ok ()) bd.buffers
                in
                let* () =
                  List.fold_left
                    (fun result (stage, index, texture) ->
                      let* () = result in
                      Ogpu.Backend.set_stage_texture encoder stage ~index texture)
                    (Ok ()) draw.plan_textures
                in
                let* () =
                  List.fold_left
                    (fun result (stage, index, sampler) ->
                      let* () = result in
                      Ogpu.Backend.set_stage_sampler encoder stage ~index sampler)
                    (Ok ()) draw.plan_samplers
                in
                match (bd.index, draw.plan_indirect) with
                | Some (index_type, buffer, offset, _), Some (arguments, arguments_offset) ->
                    Ogpu.Backend.draw_indexed_indirect encoder ~primitive:bd.primitive ~index_type
                      buffer ~offset arguments ~arguments_offset
                | None, _ ->
                    Ogpu.Backend.draw encoder ~primitive:bd.primitive ~first:bd.vertex_start
                      ~count:bd.vertex_count ~instances:bd.instances ()
                | Some (index_type, buffer, offset, count), None ->
                    Ogpu.Backend.draw_indexed encoder ~primitive:bd.primitive ~index_type buffer
                      ~offset ~count ~instances:bd.instances ())
              (Ok ()) batch.batch_draws
  in
  Ogpu.Backend.end_render encoder

let build_icb value (batch : batch_plan) =
  let draws = batch.batch_draws in
  if
    Array.length draws = 0
    || (not (plain_draws draws))
    || Option.is_some batch.batch_icb
    || not
         (Array.for_all (fun draw -> Ogpu.Backend.pipeline_indirect draw.plan_draw.pipeline) draws)
  then Ok ()
  else
    let ( let* ) = Result.bind in
    let* icb = Ogpu.Backend.create_icb value.device ~max_commands:(Array.length draws) in
    let encoded =
      Array.fold_left
        (fun result (index, draw) ->
          let* () = result in
          let bd = draw.plan_draw in
          let* () = Ogpu.Backend.icb_set_pipeline icb ~index bd.pipeline in
          let* () =
            Array.fold_left
              (fun result (stage, slot, buffer, offset) ->
                let* () = result in
                Ogpu.Backend.icb_set_buffer icb ~index stage ~slot ~offset buffer)
              (Ok ()) bd.buffers
          in
          match bd.index with
          | None ->
              Ogpu.Backend.icb_draw icb ~index ~primitive:bd.primitive ~first:bd.vertex_start
                ~count:bd.vertex_count ~instances:bd.instances ()
          | Some (index_type, buffer, offset, count) ->
              Ogpu.Backend.icb_draw_indexed icb ~index ~primitive:bd.primitive ~index_type buffer
                ~offset ~count ~instances:bd.instances ())
        (Ok ())
        (Array.mapi (fun index draw -> (index, draw)) draws)
    in
    match encoded with
    | Error _ as e ->
        ignore (Ogpu.Backend.destroy_icb icb);
        e
    | Ok () ->
        value.icb_stats.icb_builds <- Int64.succ value.icb_stats.icb_builds;
        batch.batch_icb <- Some icb;
        Ok ()

let submit_frame value frame commands =
  let committed =
    match frame with
    | Offscreen -> Ogpu.Backend.commit commands
    | Presented surface_frame ->
        Ogpu.Backend.commit_present commands ~source:value.target surface_frame
  in
  match committed with
  | Error _ as e ->
      ignore (Ogpu.Backend.abandon commands);
      discard frame;
      e
  | Ok receipt -> (
      match frame with
      | Presented _ ->
          value.in_flight <- Some receipt.epoch;
          Ok ()
      | Offscreen -> Ogpu.Backend.complete_through value.queue receipt.epoch)

(* Encodes [batches] into one command buffer and completes it. *)
let encode_frame value frame ~clear batches =
  match Ogpu.Backend.begin_commands value.queue with
  | Error _ as e ->
      discard frame;
      e
  | Ok commands -> (
      let encoded =
        List.fold_left
          (fun result batch ->
            Result.bind result (fun () -> encode_batch value commands ~clear batch))
          (Ok ()) batches
      in
      match encoded with
      | Error _ as e ->
          ignore (Ogpu.Backend.abandon commands);
          discard frame;
          e
      | Ok () -> submit_frame value frame commands)

(* World sun shadows: a draw whose World block asks for [sun_shadow = Some key]
   samples the renderer's sun map, rendered here from every depth-writing
   Replace or Alpha World triangle draw carrying the same key (the block's words 40..55 hold the sun
   view-projection) and re-rendered only when the key changes.
   ponytail: one 2048^2 map per renderer (32 MB with its depth attachment);
   a second World in the same frame with another key samples the first's
   map; a translucent depth-writing draw casts an opaque shadow. Cascades,
   per-view maps and alpha-tested casters lift these ceilings. *)
let sun_map_size = 2048

let sun_key slot =
  match slot.slot_auxiliary with
  | Some ((source : auxiliary_resource), _, _) -> source.sun_shadow
  | None -> None

let render_sun value scratch =
  let ( let* ) = Result.bind in
  let request = ref None in
  for index = scratch.scratch_length - 1 downto 0 do
    match sun_key (Array.unsafe_get scratch.scratch_slots index) with
    | Some key -> request := Some key
    | None -> ()
  done;
  match (!request, value.sun) with
  | None, _ -> Ok ()
  | Some key, Some sun when sun.sun_key = key -> Ok ()
  | Some key, _ -> (
      let* sun =
        match (value.sun, value.sun_factory) with
        | Some sun, _ -> Ok sun
        | None, None ->
            error "Renderer.submit" Ogpu.Error.Unsupported "renderer has no sun shadow pipeline"
        | None, Some factory -> (
            let descriptor label usage : Ogpu.Types.texture_descriptor =
              {
                label = Some label;
                width = sun_map_size;
                height = sun_map_size;
                depth = 1;
                mip_levels = 1;
                sample_count = 1;
                format = Rgba8_unorm;
                usage;
              }
            in
            let* pipeline = factory value.device in
            match
              Ogpu.Backend.create_texture value.device
                (descriptor "scene-sun-shadow" [ Texture_binding; Render_attachment ])
            with
            | Error _ as e ->
                ignore (Ogpu.Backend.destroy_pipeline pipeline);
                e
            | Ok color -> (
                match
                  Ogpu.Backend.create_depth_texture value.device
                    (descriptor "scene-sun-depth" [ Render_attachment ])
                with
                | Error _ as e ->
                    ignore (Ogpu.Backend.destroy_pipeline pipeline);
                    ignore (Ogpu.Backend.destroy_texture color);
                    e
                | Ok depth ->
                    let sun =
                      {
                        sun_pipeline = pipeline;
                        sun_color = color;
                        sun_depth = depth;
                        sun_key = "";
                      }
                    in
                    value.sun <- Some sun;
                    Ok sun))
      in
      (* Retained plans may have sampled the previous map. *)
      drop_plans value;
      sun.sun_key <- "";
      let* commands = Ogpu.Backend.begin_commands value.queue in
      let encoded =
        let* encoder =
          Ogpu.Backend.render_encoder commands
            {
              colors =
                [
                  {
                    texture = sun.sun_color;
                    resolve = None;
                    load = Clear;
                    store = Store;
                    clear = (1., 1., 1., 1.);
                  };
                ];
              depth =
                Some
                  {
                    depth_texture = sun.sun_depth;
                    depth_load = Clear;
                    depth_store = Discard;
                    depth_clear = 1.;
                  };
            }
        in
        let* () = Ogpu.Backend.set_render_pipeline encoder sun.sun_pipeline in
        let* () =
          Ogpu.Backend.set_viewport encoder
            { x = 0; y = 0; width = sun_map_size; height = sun_map_size }
        in
        let* () = Ogpu.Backend.set_cull encoder Cull_none in
        let* () =
          Ogpu.Backend.set_depth_state encoder (Some { depth_compare = Less; depth_write = true })
        in
        let rec draw index =
          if index = scratch.scratch_length then Ok ()
          else
            let slot = Array.unsafe_get scratch.scratch_slots index in
            match (slot.slot_auxiliary, slot.slot_mesh, slot.slot_state) with
            | Some (_, world, _), Some item, Some state
              when sun_key slot = Some key
                   && slot.slot_family = Scene3_world
                   && (slot.slot_blend = Ogpu.Pipeline.Replace || slot.slot_blend = Alpha)
                   && state.depth_write && item.primitive = Triangle_list -> (
                let bytes = Option.fold ~none:0 ~some:Bytes.length state.transform_uniforms in
                let uniforms =
                  match (slot.slot_uniform, item.uniform_offset) with
                  | Some uniform, _ ->
                      Some
                        ( uniform.uniform_buffer,
                          uniform.uniform_offset,
                          if bytes > 5456 then Int64.add uniform.uniform_offset 5456L
                          else uniform.uniform_offset )
                  | None, Some offset -> Some (item.buffer, offset, offset)
                  | None, None -> None
                in
                match uniforms with
                | None -> draw (index + 1)
                | Some (buffer, offset, instances_offset) ->
                    (* scene.rays sun-vertex: records as floats 0 (slot 1 unused), the World block 2, surface 3, instances 4 *)
                    let* () = Ogpu.Backend.set_stage_buffer encoder Vertex ~index:0 item.buffer in
                    let* () =
                      Ogpu.Backend.set_stage_buffer encoder Vertex ~index:2 world.auxiliary_buffer
                    in
                    let* () =
                      Ogpu.Backend.set_stage_buffer encoder Vertex ~index:3 ~offset buffer
                    in
                    let* () =
                      Ogpu.Backend.set_stage_buffer encoder Vertex ~index:4 ~offset:instances_offset
                        buffer
                    in
                    let* () =
                      Ogpu.Backend.draw_indexed encoder ~primitive:Triangle_list ~index_type:Uint32
                        item.buffer ~offset:item.index_offset ~count:(Int64.of_int item.index_count)
                        ~instances:(if bytes > 5456 then (bytes - 5456) / 192 else 1)
                        ()
                    in
                    draw (index + 1))
            | _ -> draw (index + 1)
        in
        let* () = draw 0 in
        Ogpu.Backend.end_render encoder
      in
      match encoded with
      | Error _ as e ->
          ignore (Ogpu.Backend.abandon commands);
          e
      | Ok () ->
          let* receipt = Ogpu.Backend.commit commands in
          let* () = Ogpu.Backend.complete_through value.queue receipt.epoch in
          sun.sun_key <- key;
          value.sun_passes <- Int64.succ value.sun_passes;
          Ok ())

let sun_shadow_passes value = value.sun_passes

let replay_plan value plan =
  if not (List.for_all gpu_vertices_valid plan.replay_gpu_vertices) then
    error "Scene_execution.replay" Ogpu.Error.Stale_handle "GPU vertices are stale or closed"
  else
    match acquire value with
    | Error _ as error -> error
    | Ok `Skipped -> Ok false
    | Ok (`Acquired frame) ->
        Result.map
          (fun () -> true)
          (encode_frame value frame ~clear:plan.replay_clear plan.replay_batches)

let finish_prepared_frame value frame ~clear ~prepared_key ~draws ~scratch ~uniform_slices
    ~after_prepare finish =
  let candidate_fingerprint = automatic_candidate_fingerprint scratch in
  after_prepare ();
  match value.automatic_submission with
  | Some plan
    when plan.replay_clear = clear
         && List.for_all gpu_vertices_valid plan.replay_gpu_vertices
         && same_scratch_payloads (List.map (fun s -> (s, (), ())) plan.replay_payloads) scratch ->
      value.automatic_candidate <- None;
      value.icb_stats.icb_hits <- Int64.succ value.icb_stats.icb_hits;
      clear_prepared_scratch scratch;
      (* A prepared key claims the identical automatic plan, so its replay works. *)
      (match prepared_key with
      | Some (identity, version) ->
          value.automatic_submission <- None;
          value.prepared_submission <-
            Some
              {
                submission_identity = identity;
                submission_version = version;
                submission_draw_count = List.length draws;
                submission_plan = plan;
              }
      | None -> ());
      finish (Result.map (fun () -> true) (encode_frame value frame ~clear plan.replay_batches))
  | _ -> (
      value.icb_stats.icb_misses <- Int64.succ value.icb_stats.icb_misses;
      let slot_state slot = match slot.slot_state with Some state -> state | None -> assert false
      and slot_mesh slot = match slot.slot_mesh with Some mesh -> mesh | None -> assert false in
      let attachment_class = function
        | Scene2 | Scene2_textured -> 0
        (* Ui and Marks draws bind directly; argument-buffer passes cannot mix them. *)
        | Ui | Marks -> 3
        | Scene3 | Scene3_points | Scene3_textured | Scene3_world -> 1
      in
      let same_pass_state class_ (a : state) (b : state) =
        a.viewport = b.viewport && a.scissor = b.scissor && a.cull = b.cull
        && (class_ = 0 || class_ = 3
           || a.depth_compare = b.depth_compare && a.depth_write = b.depth_write
              && a.depth_load = b.depth_load && a.depth_clear = b.depth_clear)
      in
      let take class_ samples state start =
        let index = ref start in
        while
          !index < scratch.scratch_length
          && !index - start < 65_535
          &&
          let slot = Array.unsafe_get scratch.scratch_slots !index in
          attachment_class slot.slot_family = class_
          && slot.slot_samples = samples
          && same_pass_state class_ state (slot_state slot)
        do
          incr index
        done;
        !index
      in
      let plan_entry slot =
        let ( let* ) = Result.bind in
        let family = slot.slot_family
        and blend = slot.slot_blend
        and samples = slot.slot_samples
        and item = slot_mesh slot in
        let* () =
          match slot.slot_gpu_vertices with
          | Some source when not (gpu_vertices_valid source) ->
              error "Renderer.submit" Ogpu.Error.Stale_handle "GPU vertices are stale or closed"
          | _ -> Ok ()
        in
        let scene2 = family = Scene2 || family = Scene2_textured in
        let pipeline_family = if family = Scene2 then Scene2_textured else family in
        let variant = Option.get (find_pipeline value pipeline_family blend samples) in
        let* buffers, textures, samplers, resident =
          match slot.slot_texture with
          | None -> Ok ([], [], [], [])
          | Some (source, cached) ->
              if scene2 then
                let* page, offset = argument_slice value variant cached.texture source.sampler in
                Ok ([ (Ogpu.Backend.Fragment, 2, page, offset) ], [], [], [ cached.texture ])
              else
                let* sampler = sampler_for value source.sampler in
                Ok
                  ( [],
                    [ (Ogpu.Backend.Fragment, 1, cached.texture) ],
                    [ (Ogpu.Backend.Fragment, 2, sampler) ],
                    [] )
        in
        let* buffers, textures, samplers =
          match slot.slot_auxiliary with
          | None ->
              let vertices =
                match slot.slot_gpu_vertices with
                | None -> item.buffer
                | Some source -> source.gpu_buffer
              in
              let extra =
                match slot.slot_gpu_vertices with
                | None -> []
                | Some source -> (
                    List.mapi (fun i b -> (Ogpu.Backend.Vertex, i + 1, b, 0L)) source.gpu_extra
                    (* marks.rays reads the colour columns as words and as floats: bound twice *)
                    @
                    match (family, source.gpu_extra) with
                    | Marks, [ _; fill; stroke; _; _ ] ->
                        [ (Ogpu.Backend.Vertex, 7, fill, 0L); (Ogpu.Backend.Vertex, 8, stroke, 0L) ]
                    | (Scene3 | Scene3_points), [] ->
                        [ (Ogpu.Backend.Vertex, 1, source.gpu_buffer, 0L) ]
                    | _ -> [])
              in
              Ok ((Ogpu.Backend.Vertex, 0, vertices, 0L) :: (extra @ buffers), textures, samplers)
          | Some (source, buffer, texture) ->
              let* sampler = sampler_for value source.texture.sampler in
              Ok
                ( (Ogpu.Backend.Vertex, 0, item.buffer, 0L)
                  :: (Ogpu.Backend.Fragment, 3, buffer.auxiliary_buffer, 0L)
                  :: buffers,
                  (Ogpu.Backend.Fragment, 4, texture.texture) :: textures,
                  (Ogpu.Backend.Fragment, 5, sampler) :: samplers )
        in
        (* World draws bind the sun map at 8 (the camera map stands in when
         there is none; the block says whether to read it) and the camera map
         at 9/10. *)
        let* textures, samplers =
          match (family, slot.slot_environment) with
          | Scene3_world, Some (source, cached) ->
              let* sampler = sampler_for value source.sampler in
              let sun =
                match (value.sun, sun_key slot) with
                | Some sun, Some _ -> sun.sun_color
                | _ -> cached.texture
              in
              Ok
                ( (Ogpu.Backend.Fragment, 8, sun)
                  :: (Ogpu.Backend.Fragment, 9, cached.texture)
                  :: textures,
                  (Ogpu.Backend.Fragment, 10, sampler) :: samplers )
          | _ -> Ok (textures, samplers)
        in
        let buffers =
          if scene2 || family = Ui || family = Marks then buffers
          else
            ( Ogpu.Backend.Vertex,
              11,
              (match slot.slot_vertex_attributes with
              | Some attributes -> attributes.auxiliary_buffer
              | None -> item.buffer),
              0L )
            :: buffers
        in
        let buffers =
          match (slot.slot_uniform, item.uniform_offset) with
          | Some uniform, _ ->
              let binding stage = (stage, 6, uniform.uniform_buffer, uniform.uniform_offset) in
              binding Ogpu.Backend.Vertex
              ::
              (if family = Scene2 || family = Scene2_textured || family = Ui || family = Marks then
                 buffers
               else
                 ( Ogpu.Backend.Vertex,
                   7,
                   uniform.uniform_buffer,
                   match (slot_state slot).transform_uniforms with
                   | Some bytes when Bytes.length bytes > 5456 ->
                       Int64.add uniform.uniform_offset 5456L
                   | _ -> uniform.uniform_offset )
                 :: binding Ogpu.Backend.Fragment :: buffers)
          | None, Some offset ->
              (Ogpu.Backend.Vertex, 6, item.buffer, offset)
              ::
              (if family = Scene2 || family = Scene2_textured || family = Ui || family = Marks then
                 []
               else [ (Ogpu.Backend.Vertex, 7, item.buffer, offset) ])
              @ ((Ogpu.Backend.Fragment, 6, item.buffer, offset) :: buffers)
          | None, None -> buffers
        in
        (* The generated pipelines bind in their Lisp declaration order (shaders/*.rays): ui.rays its records
         as floats 0 and words 1, the affine 2, the atlas 3 (sampler 4); scene.rays its records 0 and 1,
         surface 2, instances 3, attributes 4 and 5, the texture 6 (sampler 7), and for World its block 6,
         image 7 (8), specular map 9 (10), camera map 11 (12) and sun map 13; Scene2 its records 0, the
         affine 1 and the texture's argument buffer 2. *)
        let rebind slots stage slot =
          match List.assoc_opt (stage, slot) slots with Some s -> s | None -> [ slot ]
        in
        let regenerated buffers_to textures_to samplers_to =
          ( List.concat_map
              (fun (stage, slot, b, o) ->
                List.map (fun s -> (stage, s, b, o)) (rebind buffers_to stage slot))
              buffers,
            List.map
              (fun (stage, slot, t) ->
                (stage, Option.value (List.assoc_opt slot textures_to) ~default:slot, t))
              textures,
            List.map
              (fun (stage, slot, s) ->
                (stage, Option.value (List.assoc_opt slot samplers_to) ~default:slot, s))
              samplers )
        in
        let scene3 =
          [
            ((Ogpu.Backend.Vertex, 0), [ 0; 1 ]);
            ((Vertex, 6), [ 2 ]);
            ((Fragment, 6), [ 2 ]);
            ((Vertex, 7), [ 3 ]);
            ((Vertex, 11), [ 4; 5 ]);
          ]
        in
        let buffers, textures, samplers =
          match family with
          | Ui ->
              regenerated
                [ ((Ogpu.Backend.Vertex, 0), [ 0; 1 ]); ((Vertex, 6), [ 2 ]) ]
                [ (1, 3) ]
                [ (2, 4) ]
          | Scene2 | Scene2_textured -> regenerated [ ((Ogpu.Backend.Vertex, 6), [ 1 ]) ] [] []
          | (Scene3 | Scene3_points) when Option.is_some slot.slot_gpu_vertices ->
              (* a device's positions at 0 alone, its normals (or the positions again) at 1 *)
              regenerated
                (((Ogpu.Backend.Vertex, 0), [ 0 ]) :: List.tl scene3)
                [ (1, 6) ]
                [ (2, 7) ]
          | Scene3 | Scene3_points | Scene3_textured -> regenerated scene3 [ (1, 6) ] [ (2, 7) ]
          | Scene3_world ->
              regenerated
                (((Ogpu.Backend.Fragment, 3), [ 6 ]) :: scene3)
                [ (1, 7); (4, 9); (8, 13); (9, 11) ]
                [ (2, 8); (5, 10); (10, 12) ]
          | Marks -> (buffers, textures, samplers)
        in
        let index =
          if scene2 then None
          else
            Some
              ( Ogpu.Render_pass.Uint32,
                item.buffer,
                item.index_offset,
                Int64.of_int item.index_count )
        in
        let instances =
          match (family, (slot_state slot).transform_uniforms) with
          | (Scene3 | Scene3_points | Scene3_textured | Scene3_world), Some bytes
            when Bytes.length bytes > 5456 && (Bytes.length bytes - 5456) mod 192 = 0 ->
              (Bytes.length bytes - 5456) / 192
          | _ -> 1
        in
        Ok
          {
            plan_draw =
              {
                Ogpu.Backend.pipeline = variant.pipeline;
                buffers = Array.of_list buffers;
                primitive = item.primitive;
                index;
                vertex_start = 0;
                vertex_count = item.vertex_count;
                instances;
              };
            plan_textures = textures;
            plan_samplers = samplers;
            plan_resident = resident;
            plan_indirect = Option.bind slot.slot_gpu_vertices (fun source -> source.gpu_indirect);
          }
      in
      let batch_resources draws =
        let seen_buffers = Hashtbl.create 32 and seen_textures = Hashtbl.create 16 in
        let resources = ref [] in
        let buffer b =
          let id = Ogpu.Backend.buffer_id b in
          if not (Hashtbl.mem seen_buffers id) then (
            Hashtbl.add seen_buffers id ();
            resources := `Buffer b :: !resources)
        and texture t =
          let id = Ogpu.Backend.texture_id t in
          if not (Hashtbl.mem seen_textures id) then (
            Hashtbl.add seen_textures id ();
            resources := `Texture t :: !resources)
        in
        Array.iter
          (fun draw ->
            Array.iter (fun (_, _, b, _) -> buffer b) draw.plan_draw.buffers;
            Option.iter (fun (_, b, _, _) -> buffer b) draw.plan_draw.index;
            Option.iter (fun (b, _) -> buffer b) draw.plan_indirect;
            List.iter (fun (_, _, t) -> texture t) draw.plan_textures;
            List.iter texture draw.plan_resident)
          draws;
        (match value.arguments with Some pool -> buffer pool.argument_buffer | None -> ());
        List.rev !resources
      in
      let payloads first last =
        let rec loop index reversed =
          if index = last then Ok (List.rev reversed)
          else
            let slot = Array.unsafe_get scratch.scratch_slots index in
            match plan_entry slot with
            | Error _ as e -> e
            | Ok draw ->
                let signature =
                  automatic_signature
                    ( slot.slot_family,
                      slot.slot_blend,
                      slot.slot_samples,
                      slot_state slot,
                      slot.slot_texture,
                      slot.slot_auxiliary,
                      slot.slot_environment,
                      slot_mesh slot,
                      slot.slot_uniform,
                      slot.slot_vertex_attributes,
                      slot.slot_gpu_vertices )
                in
                loop (index + 1) ((signature, draw) :: reversed)
        in
        loop first []
      in
      let rec batches first index reversed =
        if index = scratch.scratch_length then
          if not first then Ok (List.rev reversed)
          else
            let width = value.configuration.Ogpu.Surface.physical_width
            and height = value.configuration.physical_height in
            let state =
              {
                viewport = (0, 0, width, height);
                scissor = (0, 0, width, height);
                cull = Ogpu.Render_pass.Cull_none;
                depth_compare = Ogpu.Render_pass.Always;
                depth_write = false;
                depth_load = Ogpu.Render_pass.Load;
                depth_clear = 1.;
                transform_uniforms = None;
              }
            in
            Ok
              [
                ( {
                    batch_family = Scene2;
                    batch_samples = 1;
                    batch_state = state;
                    batch_first = true;
                    batch_draws = [||];
                    batch_driver_draws = [||];
                    batch_resources = [];
                    batch_icb = None;
                  },
                  [] );
              ]
        else
          let slot = Array.unsafe_get scratch.scratch_slots index in
          let family = slot.slot_family
          and samples = slot.slot_samples
          and state = slot_state slot in
          let class_ = attachment_class family in
          let next = take class_ samples state (index + 1) in
          match payloads index next with
          | Error _ as e -> e
          | Ok entries ->
              let draws = Array.of_list (List.map snd entries) in
              let batch =
                {
                  batch_family = family;
                  batch_samples = samples;
                  batch_state = state;
                  batch_first = first;
                  batch_draws = draws;
                  batch_driver_draws = Array.map (fun draw -> draw.plan_draw) draws;
                  batch_resources = batch_resources draws;
                  batch_icb = None;
                }
              in
              batches false next ((batch, List.map fst entries) :: reversed)
      in
      match batches true 0 [] with
      | Error _ as result ->
          discard frame;
          finish result
      | Ok planned -> (
          let plan_batches, payload_lists = List.split planned in
          let payloads = List.concat payload_lists in
          let gpu_vertices = ref [] in
          for index = 0 to scratch.scratch_length - 1 do
            Option.iter
              (fun source -> gpu_vertices := source :: !gpu_vertices)
              scratch.scratch_slots.(index).slot_gpu_vertices
          done;
          clear_prepared_scratch scratch;
          match encode_frame value frame ~clear plan_batches with
          | Error _ as result -> finish result
          | Ok () ->
              value.previous_uniforms <- !uniform_slices;
              let plan =
                {
                  replay_clear = clear;
                  replay_payloads = payloads;
                  replay_batches = plan_batches;
                  replay_gpu_vertices = !gpu_vertices;
                }
              in
              let admit =
                match value.automatic_candidate with
                | Some candidate ->
                    candidate.candidate_clear = clear
                    && candidate.candidate_length = List.length payloads
                    && candidate.candidate_fingerprint = candidate_fingerprint
                | None -> false
              in
              let retained = prepared_key <> None || admit in
              if retained then
                List.iter
                  (fun batch ->
                    match build_icb value batch with
                    | Ok () -> ()
                    | Error error ->
                        value.icb_stats.icb_failures <- Int64.succ value.icb_stats.icb_failures;
                        value.icb_stats.icb_last_failure <- Some (Ogpu.Error.to_string error))
                  plan_batches;
              (match prepared_key with
              | Some (identity, version) ->
                  value.prepared_submission <-
                    Some
                      {
                        submission_identity = identity;
                        submission_version = version;
                        submission_draw_count = List.length draws;
                        submission_plan = plan;
                      }
              | None -> ());
              if admit then (
                drop_plan value value.automatic_submission;
                value.automatic_submission <- Some plan;
                value.automatic_candidate <- None)
              else (
                drop_plan value value.automatic_submission;
                value.automatic_submission <- None;
                value.automatic_candidate <-
                  Some
                    {
                      candidate_clear = clear;
                      candidate_length = List.length payloads;
                      candidate_fingerprint;
                    });
              finish (Ok true)))

let submit ?prepared ?(after_prepare = Fun.id) ?(clear = (0., 0., 0., 0.)) value draws =
  if value.dead then (
    after_prepare ();
    error "Renderer.submit" Ogpu.Error.Stale_handle "renderer is destroyed")
  else if List.length draws > prepared_scratch_capacity then (
    after_prepare ();
    error "Renderer.submit" Ogpu.Error.Capacity
      "one submission exceeds the bounded prepared-draw capacity")
  else
    match (prepared, value.prepared_submission) with
    | Some (identity, version), Some cached
      when cached.submission_identity = identity
           && cached.submission_version = version
           && cached.submission_plan.replay_clear = clear ->
        after_prepare ();
        replay_plan value cached.submission_plan
    | _ -> (
        drop_plan value (Option.map (fun s -> s.submission_plan) value.prepared_submission);
        value.prepared_submission <- None;
        let prepared_key = prepared in
        match resolve_prepared value prepared draws with
        | Error _ as result ->
            after_prepare ();
            result
        | Ok draws -> (
            (* An empty mesh (a zero-length line, a zero-radius circle) draws nothing:
     skip it instead of failing the whole frame. *)
            let draws =
              List.filter
                (fun (entry : sampled_draw) ->
                  entry.draw.mesh.vertex_count > 0 && entry.draw.mesh.index_count > 0)
                draws
            in
            let valid_mesh (mesh : mesh) =
              mesh_key_string mesh.key <> ""
              && mesh.vertex_count > 0 && mesh.index_count > 0 && valid_index_length mesh
              && Bytes.length mesh.vertices + Bytes.length mesh.indices > 0
            in
            let supported =
              List.for_all
                (fun (entry : sampled_draw) ->
                  Option.is_some (find_pipeline value entry.family entry.blend entry.samples))
                draws
            in
            let valid =
              List.for_all
                (fun (entry : sampled_draw) ->
                  List.mem entry.samples [ 1; 4; 9; 16 ]
                  && valid_mesh entry.draw.mesh
                  && Option.fold ~none:true ~some:valid_texture entry.texture
                  && valid_vertex_attributes entry
                  && Option.fold ~none:true
                       ~some:(fun (source : auxiliary_resource) ->
                         source.key <> ""
                         && Bytes.length source.buffer > 0
                         && valid_texture source.texture)
                       entry.auxiliary)
                draws
            in
            if not supported then (
              after_prepare ();
              error "Renderer.submit" Ogpu.Error.Unsupported
                "pipeline family/blend variant is unavailable")
            else if not valid then (
              after_prepare ();
              error "Renderer.submit" Ogpu.Error.Invalid_argument "draw resource preflight failed")
            else
              let deferred = ref [] in
              let defer release = deferred := release :: !deferred in
              let finish result =
                (* Evicted and oversize resources are valid through this synchronous
       submission, but not through another replay. Retained plans borrow
       cache resources; drop both replay paths before releasing any of them,
       including on preparation/submission failure. *)
                if
                  !deferred <> []
                  || value.retired.retired_buffers <> []
                  || value.retired.retired_textures <> []
                then drop_plans value;
                List.iter (fun release -> release ()) !deferred;
                drain_retired value.retired
                  (fun b -> ignore (Ogpu.Backend.destroy_buffer b))
                  (fun t -> ignore (Ogpu.Backend.destroy_texture t));
                result
              in
              let scratch = value.prepared_scratch in
              clear_prepared_scratch scratch;
              value.frame <- value.frame + 1;
              match ensure_prepared_scratch scratch (List.length draws) with
              | Error _ as result ->
                  after_prepare ();
                  finish result
              | Ok () ->
                  Fun.protect
                    ~finally:(fun () -> clear_prepared_scratch scratch)
                    (fun () ->
                      let modes =
                        Array.of_list
                          (List.map
                             (fun (entry : sampled_draw) ->
                               let family = entry.family and draw = entry.draw in
                               let scene2 = family = Scene2 || family = Scene2_textured in
                               let affine =
                                 (family = Ui || family = Marks)
                                 && Option.fold ~none:false
                                      ~some:(fun bytes ->
                                        Bytes.length bytes = 24 || Bytes.length bytes = 48)
                                      draw.state.transform_uniforms
                               in
                               let scene3_transform =
                                 (not scene2)
                                 && Option.fold ~none:false
                                      ~some:(fun bytes ->
                                        Bytes.length bytes >= 5456
                                        && (Bytes.length bytes - 5456) mod 192 = 0)
                                      draw.state.transform_uniforms
                               in
                               (* Scene2 always binds an affine through the argument-buffer pipeline. *)
                               let source =
                                 if scene2 then
                                   Some
                                     (match draw.state.transform_uniforms with
                                     | None -> scene2_identity_affine
                                     | Some bytes -> scene2_native_affine bytes)
                                 else if affine || scene3_transform then
                                   draw.state.transform_uniforms
                                 else None
                               in
                               (scene2, affine, scene3_transform, source))
                             draws)
                      in
                      let sources = Array.map (fun (_, _, _, source) -> source) modes in
                      let uniform_slices = ref [||] in
                      let append ?environment family blend samples state texture auxiliary mesh
                          uniform attributes gpu_vertices =
                        let slot = Array.unsafe_get scratch.scratch_slots scratch.scratch_length in
                        slot.slot_family <- family;
                        slot.slot_blend <- blend;
                        slot.slot_samples <- samples;
                        slot.slot_state <- Some state;
                        slot.slot_texture <- texture;
                        slot.slot_auxiliary <- auxiliary;
                        slot.slot_environment <- environment;
                        slot.slot_mesh <- Some mesh;
                        slot.slot_uniform <- uniform;
                        slot.slot_vertex_attributes <- attributes;
                        slot.slot_gpu_vertices <- gpu_vertices;
                        scratch.scratch_length <- scratch.scratch_length + 1
                      in
                      let rec prepare_all = function
                        | [] -> Ok ()
                        | (entry : sampled_draw) :: rest -> (
                            let {
                              family;
                              blend;
                              texture;
                              auxiliary;
                              vertex_attributes;
                              samples;
                              draw;
                            } =
                              entry
                            in
                            let scene2, affine, scene3_transform, _ =
                              modes.(scratch.scratch_length)
                            in
                            match resolve_gpu_vertices value entry with
                            | Error _ as result -> result
                            | Ok gpu_vertices -> (
                                let cpu_mesh =
                                  match gpu_vertices with
                                  | None -> draw.mesh
                                  | Some _ ->
                                      {
                                        draw.mesh with
                                        key =
                                          Host
                                            ("gpu:indices:"
                                            ^ string_of_int draw.mesh.vertex_count
                                            ^
                                            match family with
                                            | Scene3 | Scene3_points -> ":scene3"
                                            | _ -> "");
                                      }
                                in
                                match
                                  prepare value ~reserved:(scratch_mem_mesh scratch)
                                    ~uniforms:
                                      (if scene2 || affine || scene3_transform then None
                                       else draw.state.transform_uniforms)
                                    ~vertex_stable:scene3_transform ~nonindexed:scene2
                                    ~canonical_plain:(family = Scene2) cpu_mesh
                                with
                                | Error _ as result -> result
                                | Ok mesh -> (
                                    let attributes =
                                      match vertex_attributes with
                                      | None -> Ok None
                                      | Some (key, bytes) ->
                                          Result.map Option.some (prepare_storage value ~key ~bytes)
                                    in
                                    match attributes with
                                    | Error _ as result -> result
                                    | Ok attributes -> (
                                        let uniform = !uniform_slices.(scratch.scratch_length) in
                                        let texture =
                                          match (family, texture) with
                                          | Scene2, None -> Some scene2_white_texture
                                          | _ -> texture
                                        in
                                        match texture with
                                        | Some source -> (
                                            match prepare_texture value ~defer source with
                                            | Error _ as result -> result
                                            | Ok texture ->
                                                prepare_aux family blend auxiliary samples draw mesh
                                                  uniform attributes gpu_vertices
                                                  (Some (source, texture))
                                                  rest)
                                        | None ->
                                            prepare_aux family blend auxiliary samples draw mesh
                                              uniform attributes gpu_vertices None rest))))
                      and prepare_aux family blend auxiliary samples draw mesh uniform attributes
                          gpu_vertices texture rest =
                        match auxiliary with
                        | None ->
                            append family blend samples draw.state texture None mesh uniform
                              attributes gpu_vertices;
                            prepare_all rest
                        | Some source -> (
                            match prepare_auxiliary value source with
                            | Error _ as result -> result
                            | Ok buffer -> (
                                match prepare_texture value ~defer source.texture with
                                | Error _ as result -> result
                                | Ok texture2 -> (
                                    let environment =
                                      match source.environment with
                                      | None -> Ok None
                                      | Some environment ->
                                          Result.map
                                            (fun cached -> Some (environment, cached))
                                            (prepare_texture value ~defer environment)
                                    in
                                    match environment with
                                    | Error _ as result -> result
                                    | Ok environment ->
                                        append ?environment family blend samples draw.state texture
                                          (Some (source, buffer, texture2))
                                          mesh uniform attributes gpu_vertices;
                                        prepare_all rest)))
                      in
                      match acquire value with
                      | Error _ as result ->
                          after_prepare ();
                          finish result
                      | Ok `Skipped ->
                          after_prepare ();
                          finish (Ok false)
                      | Ok (`Acquired frame) -> (
                          match prepare_uniforms value ~defer sources with
                          | Error _ as result ->
                              after_prepare ();
                              discard frame;
                              finish result
                          | Ok uniforms -> (
                              uniform_slices := uniforms;
                              match prepare_all draws with
                              | Error _ as result ->
                                  after_prepare ();
                                  discard frame;
                                  finish result
                              | Ok () -> (
                                  match render_sun value scratch with
                                  | Error _ as result ->
                                      after_prepare ();
                                      discard frame;
                                      finish result
                                  | Ok () ->
                                      finish_prepared_frame value frame ~clear ~prepared_key ~draws
                                        ~scratch ~uniform_slices ~after_prepare finish))))))

let replay_prepared_sampled_resources ?(clear = (0., 0., 0., 0.)) ~identity ~version value =
  if value.dead then
    error "Scene_execution.replay_prepared_sampled_resources" Ogpu.Error.Stale_handle
      "renderer is destroyed"
  else
    match value.prepared_submission with
    | Some cached
      when cached.submission_identity = identity
           && cached.submission_version = version
           && cached.submission_plan.replay_clear = clear ->
        Result.map
          (fun presented -> Some (presented, cached.submission_draw_count))
          (replay_plan value cached.submission_plan)
    | _ -> Ok None

let resize value configuration =
  ignore (settle value);
  drop_plans value;
  let samples =
    List.sort_uniq Int.compare (List.map (fun variant -> variant.samples) value.pipelines)
  in
  match allocate_target value.device configuration with
  | Error _ as e -> e
  | Ok target -> (
      let attachments =
        Scene_attachment_pool.create ~device:value.device ~configuration ~sample_counts:samples
      in
      let rec restore = function
        | [] -> Ok ()
        | (kind, samples, _) :: rest -> (
            match Scene_attachment_pool.acquire attachments kind ~samples with
            | Error _ as e -> e
            | Ok _ -> restore rest)
      in
      match restore (Scene_attachment_pool.allocated value.attachments) with
      | Error e ->
          ignore (Ogpu.Backend.destroy_texture target);
          Scene_attachment_pool.destroy attachments;
          Error e
      | Ok () -> (
          let configured =
            match value.surface with
            | None -> Ok ()
            | Some surface -> Ogpu.Backend.configure surface configuration
          in
          match configured with
          | Error e ->
              ignore (Ogpu.Backend.destroy_texture target);
              Scene_attachment_pool.destroy attachments;
              Error e
          | Ok () ->
              let old = value.target and old_attachments = value.attachments in
              value.target <- target;
              value.configuration <- configuration;
              value.attachments <- attachments;
              Scene_attachment_pool.destroy old_attachments;
              Ogpu.Backend.destroy_texture old))

let upload_bytes value = value.uploaded

module Private = struct
  type nonrec gpu_vertices = gpu_vertices
  type nonrec gpu_vertices_owner = gpu_vertices_owner

  let create_gpu_vertices_owner = create_gpu_vertices_owner
  let gpu_vertices_owner value = value.gpu_vertices
  let register_gpu_vertices = register_gpu_vertices
  let unregister_gpu_vertices = unregister_gpu_vertices
  let gpu_vertex_count_for_test owner = Hashtbl.length owner.gpu_vertex_registry
  let cache_count_for_report value = String_table.length value.cache

  let coalesced_draws_for_test draws =
    Result.fold ~ok:List.length ~error:(fun _ -> -1) (coalesce_draws draws)
end

type retained_stats = { plan_hits : int64; plan_misses : int64 }

let retained_stats value =
  { plan_hits = value.icb_stats.icb_hits; plan_misses = value.icb_stats.icb_misses }

let pipeline_count value = List.length value.pipelines

let read_pixels value ~bytes_per_row =
  Result.bind (settle value) (fun () -> Ogpu.Backend.read_texture value.target ~bytes_per_row)

(* Every owned handle is released; the first failure is reported after the
   remaining releases and device teardown have run. *)
let destroy value =
  if value.dead then Ok ()
  else (
    ignore (settle value);
    value.dead <- true;
    let failure = ref None in
    let record = function
      | Ok () -> ()
      | Error error -> if !failure = None then failure := Some error
    in
    let named name = function
      | Ok () -> ()
      | Error (error : Ogpu.Error.t) ->
          record (Error { error with message = error.message ^ " (" ^ name ^ ")" })
    in
    String_table.clear value.prepared_cache;
    drop_plans value;
    clear_prepared_scratch value.prepared_scratch;
    value.prepared_scratch.scratch_slots <- [||];
    List.iter
      (fun variant ->
        Option.iter
          (fun argument -> record (Ogpu.Backend.destroy_argument argument))
          variant.argument;
        variant.argument <- None)
      value.pipelines;
    String_table.clear value.cache;
    value.previous_uniforms <- [||];
    Array.iteri
      (fun index page ->
        Option.iter
          (fun page -> named "uniform page" (Ogpu.Backend.destroy_buffer page.page_buffer))
          page;
        value.uniform_pages.(index) <- None)
      value.uniform_pages;
    String_table.clear value.auxiliary_cache;
    String_table.clear value.texture_cache;
    drain_retired value.retired
      (fun b -> named "mesh or auxiliary" (Ogpu.Backend.destroy_buffer b))
      (fun t -> record (Ogpu.Backend.destroy_texture t));
    Option.iter
      (fun scratch -> record (Ogpu.Backend.destroy_buffer scratch.scratch_buffer))
      value.texture_upload_scratch;
    value.texture_upload_scratch <- None;
    Option.iter
      (fun pool -> named "argument page" (Ogpu.Backend.destroy_buffer pool.argument_buffer))
      value.arguments;
    value.arguments <- None;
    Hashtbl.iter (fun _ sampler -> record (Ogpu.Backend.destroy_sampler sampler)) value.samplers;
    Hashtbl.reset value.samplers;
    Option.iter
      (fun sun ->
        record (Ogpu.Backend.destroy_texture sun.sun_color);
        record (Ogpu.Backend.destroy_texture sun.sun_depth);
        record (Ogpu.Backend.destroy_pipeline sun.sun_pipeline))
      value.sun;
    value.sun <- None;
    Scene_attachment_pool.destroy value.attachments;
    record (Ogpu.Backend.destroy_texture value.target);
    List.iter
      (fun variant -> record (Ogpu.Backend.destroy_pipeline variant.pipeline))
      value.pipelines;
    Option.iter (fun surface -> record (Ogpu.Backend.destroy_surface surface)) value.surface;
    record (Ogpu.Backend.destroy_queue value.queue);
    if value.owns_device then record (Ogpu.Backend.destroy_device value.device);
    match !failure with
    | Some (error : Ogpu.Error.t) ->
        Error
          {
            error with
            message =
              (error.message
              ^
              match value.icb_stats.icb_last_failure with
              | Some text -> " [plans: " ^ text ^ "]"
              | None -> "");
          }
    | None -> Ok ())
