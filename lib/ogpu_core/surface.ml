type format = Rgba8_unorm | Bgra8_unorm
type present_mode = Fifo | Immediate

type configuration = {
  logical_width : int;
  logical_height : int;
  physical_width : int;
  physical_height : int;
  format : format;
  present_mode : present_mode;
  max_acquired : int;
  layer : Native_layer_token.t option;
}

type frame_state = Live | Presented | Discarded | Stale
type frame = { id : int64; surface_id : int64; generation : int64; mutable state : frame_state }
type availability = Available | Force_timeout | Force_occluded | Force_device_lost

type t = {
  id : int64;
  device : Handle.device;
  mutable config : configuration;
  mutable generation : int64;
  mutable next_frame : int64;
  mutable frames : frame list;
  availability : availability;
  mutable dead : bool;
}

type acquire_result = Acquired of frame | Timeout | Occluded | Device_lost

let next_surface = Atomic.make 1
let invalid operation message = Error (Error.make operation Error.Invalid_argument message)
let state_error operation message = Error (Error.make operation Error.Invalid_state message)

let valid c =
  c.logical_width > 0 && c.logical_height > 0 && c.physical_width > 0 && c.physical_height > 0
  && c.max_acquired >= 1 && c.max_acquired <= 3

let create device config =
  if not (valid config) then invalid "Ogpu.Surface.create" "surface dimensions/capacity are invalid"
  else
    Ok
      {
        id = Int64.of_int (Atomic.fetch_and_add next_surface 1);
        device;
        config;
        generation = 1L;
        next_frame = 1L;
        frames = [];
        availability = Available;
        dead = false;
      }

let invalidate value =
  List.iter (fun frame -> if frame.state = Live then frame.state <- Stale) value.frames;
  value.frames <- [];
  value.generation <- Int64.succ value.generation

let configure value config =
  if value.dead then state_error "Ogpu.Surface.configure" "surface is destroyed"
  else if not (valid config) then
    invalid "Ogpu.Surface.configure" "surface dimensions/capacity are invalid"
  else (
    invalidate value;
    value.config <- config;
    Ok ())

let outstanding value = List.length value.frames

let acquire value =
  if value.dead then state_error "Ogpu.Surface.acquire" "surface is destroyed"
  else if Handle.device_destroyed value.device then Ok Device_lost
  else
    match value.availability with
    | Force_timeout -> Ok Timeout
    | Force_occluded -> Ok Occluded
    | Force_device_lost -> Ok Device_lost
    | Available when outstanding value >= value.config.max_acquired ->
        Error (Error.make "Ogpu.Surface.acquire" Error.Capacity "acquired-frame capacity reached")
    | Available ->
        let frame =
          {
            id = value.next_frame;
            surface_id = value.id;
            generation = value.generation;
            state = Live;
          }
        in
        value.next_frame <- Int64.succ value.next_frame;
        value.frames <- value.frames @ [ frame ];
        Ok (Acquired frame)

let finish operation final (value : t) (frame : frame) =
  if value.dead then state_error operation "surface is destroyed"
  else if frame.surface_id <> value.id then
    Error (Error.make operation Error.Cross_device "frame belongs to another surface")
  else if frame.generation <> value.generation || frame.state = Stale then
    Error (Error.make operation Error.Stale_handle "frame generation is stale")
  else if frame.state <> Live then state_error operation "frame has already been consumed"
  else (
    frame.state <- final;
    value.frames <- List.filter (fun (candidate : frame) -> candidate.id <> frame.id) value.frames;
    Ok ())

let present value frame = finish "Ogpu.Surface.present" Presented value frame
let discard value frame = finish "Ogpu.Surface.discard" Discarded value frame
let generation value = value.generation
let frame_id (value : frame) = value.id

let destroy value =
  if not value.dead then (
    value.dead <- true;
    invalidate value)
