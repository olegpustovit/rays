type frame_state = Live | Presented | Discarded | Stale

type t = {
  device : Device.t;
  layer : Metal.Metal_layer.t;
  portable : Ogpu_core.Surface.t;
  presentation : Presentation.t;
  readable_drawables_for_test : bool;
  mutable frames : frame list;
  mutable in_flight_presentations : int;
  mutable destroy_pending : bool;
  mutable dead : bool;
}

and frame = {
  surface : t;
  portable : Ogpu_core.Surface.frame;
  drawable : Metal.Drawable.t;
  texture : Metal.Texture.t;
  generation : int64;
  mutable state : frame_state;
}

type pending_payload = { mutable owner : t; mutable frame : frame; mutable source : Texture.t }

type pending_presentation = {
  mutable payload : pending_payload option;
  mutable active : bool;
  mutable epoch : int64;
  encode : Metal.Command_buffer.t -> (unit, Ogpu_core.Error.t) result;
}

type acquire_result = Acquired of frame | Timeout | Occluded | Device_lost

let error op kind message = Error (Ogpu_core.Error.make op kind message)

let metal_config ~readable_drawables_for_test (value : Ogpu_core.Surface.configuration) =
  let format =
    match value.format with
    | Bgra8_unorm -> Ok Metal.Texture.Bgra8_unorm
    | Rgba8_unorm ->
        error "Ogpu_metal.Surface.configure" Ogpu_core.Error.Invalid_argument
          "CAMetalLayer does not expose RGBA8 drawable format"
  in
  Result.map
    (fun format ->
      {
        Metal.Metal_layer.width = value.physical_width;
        height = value.physical_height;
        format;
        framebuffer_only = not readable_drawables_for_test;
        maximum_drawables = max 2 value.max_acquired;
        allows_timeout = true;
        display_sync = value.present_mode = Fifo;
        presents_with_transaction = false;
      })
    format

let create_common ~readable_drawables_for_test device ~layer config =
  let op = "Ogpu_metal.Surface.create" in
  if Device.destroyed device then error op Ogpu_core.Error.Stale_handle "device is destroyed"
  else if not (Metal.Device.same (Device.Private.metal device) (Metal.Metal_layer.device layer))
  then error op Ogpu_core.Error.Cross_device "layer belongs to another device"
  else
    match Ogpu_core.Surface.create (Device.Private.handle device) config with
    | Error _ as e -> e
    | Ok portable -> (
        match metal_config ~readable_drawables_for_test config with
        | Error _ as e ->
            Ogpu_core.Surface.destroy portable;
            e
        | Ok native -> (
            match Presentation.create device with
            | Error _ as e ->
                Ogpu_core.Surface.destroy portable;
                e
            | Ok presentation -> (
                match Metal.Metal_layer.configure layer native with
                | Error e ->
                    Presentation.destroy presentation;
                    Ogpu_core.Surface.destroy portable;
                    Error (Device.of_metal_error ~operation:op e)
                | Ok () ->
                    Device.Private.attach_resource device;
                    Ok
                      {
                        device;
                        layer;
                        portable;
                        presentation;
                        readable_drawables_for_test;
                        frames = [];
                        in_flight_presentations = 0;
                        destroy_pending = false;
                        dead = false;
                      })))

let create device ~layer config =
  create_common ~readable_drawables_for_test:false device ~layer config

let destroyed value = value.dead || value.destroy_pending
let generation value = Ogpu_core.Surface.generation value.portable
let in_flight_presentations value = value.in_flight_presentations
let frame_id (value : frame) = Ogpu_core.Surface.frame_id value.portable

let release frame =
  ignore (Metal.Texture.destroy frame.texture);
  ignore (Metal.Drawable.destroy frame.drawable)

let stale_frames value =
  List.iter
    (fun frame ->
      if frame.state = Live then (
        frame.state <- Stale;
        release frame))
    value.frames;
  value.frames <- []

let teardown value =
  if not value.dead then (
    stale_frames value;
    Presentation.destroy value.presentation;
    Ogpu_core.Surface.destroy value.portable;
    value.destroy_pending <- false;
    value.dead <- true;
    Device.Private.detach_resource value.device)

let preflight_portable value config =
  match Ogpu_core.Surface.create (Device.Private.handle value.device) config with
  | Error _ as e -> e
  | Ok probe ->
      Ogpu_core.Surface.destroy probe;
      Ok ()

let configure value config =
  let op = "Ogpu_metal.Surface.configure" in
  if destroyed value then error op Ogpu_core.Error.Stale_handle "surface is destroyed"
  else if value.in_flight_presentations <> 0 then
    error op Ogpu_core.Error.Invalid_state "surface has presentations in flight"
  else
    match metal_config ~readable_drawables_for_test:value.readable_drawables_for_test config with
    | Error _ as e -> e
    | Ok native -> (
        match preflight_portable value config with
        | Error _ as e -> e
        | Ok () -> (
            match Metal.Metal_layer.configure value.layer native with
            | Error e -> Error (Device.of_metal_error ~operation:op e)
            | Ok () ->
                stale_frames value;
                Ogpu_core.Surface.configure value.portable config))

let acquire_common ~scoped value =
  let op = "Ogpu_metal.Surface.acquire" in
  if destroyed value then error op Ogpu_core.Error.Stale_handle "surface is destroyed"
  else
    match Ogpu_core.Surface.acquire value.portable with
    | Error _ as e -> e
    | Ok Timeout -> Ok Timeout
    | Ok Occluded -> Ok Occluded
    | Ok Device_lost -> Ok Device_lost
    | Ok (Acquired portable) -> (
        match
          if scoped then Metal.Drawable.acquire_scoped value.layer
          else Metal.Drawable.acquire value.layer
        with
        | Error e ->
            ignore (Ogpu_core.Surface.discard value.portable portable);
            Error (Device.of_metal_error ~operation:op e)
        | Ok (Error Metal.Drawable.Timeout_or_unavailable) ->
            ignore (Ogpu_core.Surface.discard value.portable portable);
            Ok Timeout
        | Ok (Ok drawable) -> (
            match
              if scoped then Metal.Drawable.texture_scoped drawable
              else Metal.Drawable.texture drawable
            with
            | Error e ->
                ignore (Metal.Drawable.destroy drawable);
                ignore (Ogpu_core.Surface.discard value.portable portable);
                Error (Device.of_metal_error ~operation:op e)
            | Ok texture ->
                let frame =
                  {
                    surface = value;
                    portable;
                    drawable;
                    texture;
                    generation = generation value;
                    state = Live;
                  }
                in
                value.frames <- frame :: value.frames;
                Ok (Acquired frame)))

let acquire = acquire_common ~scoped:false

let validate op value frame =
  if destroyed value then error op Ogpu_core.Error.Stale_handle "surface is destroyed"
  else if frame.surface != value then
    error op Ogpu_core.Error.Cross_device "frame belongs to another surface"
  else if frame.generation <> generation value || frame.state = Stale then
    error op Ogpu_core.Error.Stale_handle "frame generation is stale"
  else
    match frame.state with
    | Live -> Ok ()
    | Presented | Discarded -> error op Ogpu_core.Error.Invalid_state "frame was already consumed"
    | Stale -> error op Ogpu_core.Error.Stale_handle "frame generation is stale"

let remove value frame =
  value.frames <- List.filter (fun candidate -> candidate != frame) value.frames

let encode_present pending commands =
  match (pending.active, pending.payload) with
  | true, Some { owner = value; frame; source } ->
      Presentation.encode value.presentation commands ~present:frame.drawable
        ~source:(Texture.Private.metal source) ~target:frame.texture ()
  | _ ->
      error "Ogpu_metal.Surface.encode_present" Ogpu_core.Error.Invalid_state
        "presentation slot is not prepared"

let create_pending_presentation () =
  let rec pending =
    {
      payload = None;
      active = false;
      epoch = 0L;
      encode = (fun commands -> encode_present pending commands);
    }
  in
  pending

let pending_presentation_available value = not value.active

let prepare_present pending value frame ~source =
  let op = "Ogpu_metal.Surface.prepare_present" in
  if not (pending_presentation_available pending) then
    error op Ogpu_core.Error.Capacity "bounded presentation slot is already in use"
  else
    match validate op value frame with
    | Error _ as e -> e
    | Ok () -> (
        match (Texture.descriptor value.device source, Texture.format value.device source) with
        | (Error _ as e), _ -> e
        | _, (Error _ as e) -> e
        | Ok descriptor, Ok format -> (
            if format <> Texture.Rgba8_unorm then
              error op Ogpu_core.Error.Invalid_argument "presentation source must use RGBA8 format"
            else if
              descriptor.sample_count <> 1
              || not (List.mem Ogpu_core.Types.Texture_binding descriptor.usage)
            then
              error op Ogpu_core.Error.Invalid_argument
                "presentation source must be single-sample and texture-bindable"
            else
              let target = Metal.Texture.descriptor frame.texture in
              if descriptor.width <> target.width || descriptor.height <> target.height then
                error op Ogpu_core.Error.Invalid_argument
                  "presentation source and drawable extents differ"
              else
                match Texture.Private.retain_submission source with
                | Error _ as e -> e
                | Ok () ->
                    (match pending.payload with
                    | None -> pending.payload <- Some { owner = value; frame; source }
                    | Some payload ->
                        payload.owner <- value;
                        payload.frame <- frame;
                        payload.source <- source);
                    pending.active <- true;
                    pending.epoch <- 0L;
                    Ok ()))

let presentation_encoder pending = pending.encode

let clear_pending pending =
  pending.active <- false;
  pending.epoch <- 0L

let rollback_present pending =
  match (pending.active, pending.payload) with
  | true, Some { source; _ } when pending.epoch = 0L ->
      Texture.Private.release_submission source;
      clear_pending pending
  | _ -> ()

let commit_present pending ~epoch =
  match (pending.active, pending.payload) with
  | true, Some { owner = value; frame; _ } when pending.epoch = 0L ->
      ignore (Ogpu_core.Surface.present value.portable frame.portable);
      frame.state <- Presented;
      remove value frame;
      value.in_flight_presentations <- value.in_flight_presentations + 1;
      pending.epoch <- epoch
  | _ -> ()

let complete_present pending =
  match (pending.active, pending.payload) with
  | true, Some { owner = value; frame; source } when pending.epoch <> 0L ->
      release frame;
      Texture.Private.release_submission source;
      value.in_flight_presentations <- value.in_flight_presentations - 1;
      clear_pending pending;
      if value.in_flight_presentations = 0 && value.destroy_pending then teardown value
  | _ -> ()

let complete_presentations_through pending epoch =
  Array.iter
    (fun slot -> if slot.epoch <> 0L && slot.epoch <= epoch then complete_present slot)
    pending

let clear_pending_presentations pending =
  Array.iter (fun slot -> if not slot.active then slot.payload <- None) pending

let discard value frame =
  let op = "Ogpu_metal.Surface.discard" in
  match validate op value frame with
  | Error _ as e -> e
  | Ok () -> (
      match Ogpu_core.Surface.discard value.portable frame.portable with
      | Error _ as e -> e
      | Ok () ->
          frame.state <- Discarded;
          remove value frame;
          release frame;
          Ok ())

let destroy value =
  if not (destroyed value) then
    if value.in_flight_presentations = 0 then teardown value
    else (
      stale_frames value;
      value.destroy_pending <- true)

module Private = struct
  let acquire_scoped = acquire_common ~scoped:true

  type nonrec pending_presentation = pending_presentation

  let create_pending_presentation = create_pending_presentation
  let pending_presentation_available = pending_presentation_available
  let prepare_present = prepare_present
  let presentation_encoder = presentation_encoder
  let rollback_present = rollback_present
  let commit_present = commit_present
  let complete_presentations_through = complete_presentations_through
  let clear_pending_presentations = clear_pending_presentations
end
