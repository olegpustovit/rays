type error_kind =
  | Mixer_error
  | Wrong_domain
  | Destroyed
  | Parent_has_dependents
  | Invalid_argument
  | Incompatible_version
  | Not_initialized
  | Handles_still_open

type error = { operation : string; kind : error_kind; message : string }

let pp_error formatter error = Format.fprintf formatter "%s: %s" error.operation error.message
let error operation kind message = Error { operation; kind; message }

external raw_version : unit -> int = "caml_sdl3_mixer_version"
external compiled_version_number : unit -> int = "caml_sdl3_mixer_compiled_version"
external raw_init : unit -> (unit, string) result = "caml_sdl3_mixer_init"
external raw_quit : unit -> unit = "caml_sdl3_mixer_quit"
external raw_create_device : unit -> (nativeint, string) result = "caml_sdl3_mixer_create_device"

external raw_create_memory : int -> int -> (nativeint, string) result
  = "caml_sdl3_mixer_create_memory"

external raw_destroy_mixer : nativeint -> unit = "caml_sdl3_mixer_destroy_mixer"
external raw_mixer_format : nativeint -> (int * int, string) result = "caml_sdl3_mixer_format"
external raw_generate : nativeint -> bytes -> (int, string) result = "caml_sdl3_mixer_generate"

external raw_load_bytes : nativeint -> bytes -> (nativeint, string) result
  = "caml_sdl3_mixer_load_bytes"

external raw_destroy_audio : nativeint -> unit = "caml_sdl3_mixer_destroy_audio"
external raw_create_track : nativeint -> (nativeint, string) result = "caml_sdl3_mixer_create_track"
external raw_destroy_track : nativeint -> unit = "caml_sdl3_mixer_destroy_track"

external raw_set_track_audio : nativeint -> nativeint -> (unit, string) result
  = "caml_sdl3_mixer_set_track_audio"

external raw_set_track_gain : nativeint -> float -> (unit, string) result
  = "caml_sdl3_mixer_set_track_gain"

external raw_play_track : nativeint -> int -> int -> (unit, string) result
  = "caml_sdl3_mixer_play_track"

external raw_stop_track : nativeint -> int -> (unit, string) result = "caml_sdl3_mixer_stop_track"
external raw_track_playing : nativeint -> bool = "caml_sdl3_mixer_track_playing"

let linked_version () : Sdl3.version =
  let number = raw_version () in
  { major = number / 1_000_000; minor = number / 1_000 mod 1_000; patch = number mod 1_000 }

(* The headers' own version macro, not a checked-in constant. *)
let compiled_version : Sdl3.version =
  let number = compiled_version_number () in
  { major = number / 1_000_000; minor = number / 1_000 mod 1_000; patch = number mod 1_000 }

let check_version ?(release = true) () =
  match
    Sdl3.validate_version ~library:"SDL3_mixer" ~compiled:compiled_version ~release
      ~linked:(linked_version ()) ()
  with
  | Ok () -> Ok ()
  | Error source -> error "SDL3_mixer.check_version" Incompatible_version source.message

type mixer_mode = Device | Memory

type mixer_handle = {
  raw : nativeint;
  mode : mixer_mode;
  channels : int;
  tracks : int Atomic.t;
  audios : int Atomic.t;
  mutable destroyed : bool;
}

type audio_handle = { raw : nativeint; mixer : mixer_handle; mutable destroyed : bool }
type track_handle = { raw : nativeint; mixer : mixer_handle; mutable destroyed : bool }

let live_mixers = Atomic.make 0
let live_audios = Atomic.make 0
let live_tracks = Atomic.make 0

module Release_queue = struct
  let mutex = Mutex.create ()
  let mixers = Queue.create ()
  let audios = Queue.create ()
  let tracks = Queue.create ()

  let enqueue queue raw =
    Mutex.lock mutex;
    Queue.add raw queue;
    (* unbounded: never leak a finalized object *)
    Mutex.unlock mutex

  let mixer raw = enqueue mixers raw
  let audio raw = enqueue audios raw
  let track raw = enqueue tracks raw

  let drain () =
    Mutex.lock mutex;
    let pending_tracks = Queue.create () in
    let pending_audios = Queue.create () in
    let pending_mixers = Queue.create () in
    Queue.transfer tracks pending_tracks;
    Queue.transfer audios pending_audios;
    Queue.transfer mixers pending_mixers;
    Mutex.unlock mutex;
    Queue.iter raw_destroy_track pending_tracks;
    Queue.iter raw_destroy_audio pending_audios;
    Queue.iter raw_destroy_mixer pending_mixers
end

let require_main operation =
  if (not (Sdl3.Thread.is_initial_domain ())) || not (Sdl3.Thread.is_sdl_main_thread ()) then
    error operation Wrong_domain
      "SDL3_mixer operation must run on the initial OCaml domain and SDL main thread"
  else Ok ()

let on_main operation callback =
  match require_main operation with
  | Error _ as failure -> failure
  | Ok () ->
      Release_queue.drain ();
      callback ()

let mixer_result operation = function
  | Ok value -> Ok value
  | Error message -> error operation Mixer_error message

let init_count = ref 0

module Init = struct
  let init () =
    on_main "SDL3_mixer.Init.init" (fun () ->
        match check_version () with
        | Error _ as failure -> failure
        | Ok () -> (
            match mixer_result "SDL3_mixer.Init.init" (raw_init ()) with
            | Error _ as failure -> failure
            | Ok () ->
                incr init_count;
                Ok ()))

  let initialized () = on_main "SDL3_mixer.Init.initialized" (fun () -> Ok (!init_count > 0))

  let quit () =
    on_main "SDL3_mixer.Init.quit" (fun () ->
        let handles = Atomic.get live_mixers + Atomic.get live_audios + Atomic.get live_tracks in
        if handles <> 0 then
          error "SDL3_mixer.Init.quit" Handles_still_open
            (Printf.sprintf "%d mixer/audio/track handle(s) are still open" handles)
        else if !init_count = 0 then Ok ()
        else begin
          decr init_count;
          raw_quit ();
          Ok ()
        end)
end

let require_initialized operation =
  match Init.initialized () with
  | Error _ as failure -> failure
  | Ok true -> Ok ()
  | Ok false -> error operation Not_initialized "SDL3_mixer is not initialized"

module Mixer = struct
  type t = mixer_handle
  type mode = mixer_mode = Device | Memory
  type format = { sample_rate : int; channels : int }
  type generated = { mixed_bytes : int; pcm_f32 : bytes }

  let owned raw mode _sample_rate channels =
    Atomic.incr live_mixers;
    let value : mixer_handle =
      { raw; mode; channels; tracks = Atomic.make 0; audios = Atomic.make 0; destroyed = false }
    in
    Gc.finalise
      (fun (value : mixer_handle) ->
        if not value.destroyed then begin
          value.destroyed <- true;
          Atomic.decr live_mixers;
          Release_queue.mixer value.raw
        end)
      value;
    value

  let live operation (value : t) callback =
    on_main operation (fun () ->
        if value.destroyed then error operation Destroyed "mixer is destroyed"
        else
          match require_initialized operation with
          | Error _ as failure -> failure
          | Ok () -> callback value.raw)

  let finish_create operation mode = function
    | Error message -> error operation Mixer_error message
    | Ok raw -> (
        match raw_mixer_format raw with
        | Ok (sample_rate, channels) -> Ok (owned raw mode sample_rate channels)
        | Error message ->
            raw_destroy_mixer raw;
            error operation Mixer_error message)

  let create_device () =
    on_main "SDL3_mixer.Mixer.create_device" (fun () ->
        match require_initialized "SDL3_mixer.Mixer.create_device" with
        | Error _ as failure -> failure
        | Ok () -> finish_create "SDL3_mixer.Mixer.create_device" Device (raw_create_device ()))

  let create_memory ~sample_rate ~channels =
    let operation = "SDL3_mixer.Mixer.create_memory" in
    if sample_rate < 8_000 || sample_rate > 384_000 then
      error operation Invalid_argument "sample rate must be in 8000..384000"
    else if channels <= 0 || channels > 8 then
      error operation Invalid_argument "channel count must be in 1..8"
    else
      on_main operation (fun () ->
          match require_initialized operation with
          | Error _ as failure -> failure
          | Ok () -> finish_create operation Memory (raw_create_memory sample_rate channels))

  let generate (value : t) ~frames =
    let operation = "SDL3_mixer.Mixer.generate" in
    if value.mode <> Memory then
      error operation Invalid_argument "audio can only be generated by a memory mixer"
    else if frames <= 0 then error operation Invalid_argument "frame count must be positive"
    else if value.channels > max_int / 4 || frames > max_int / (value.channels * 4) then
      error operation Invalid_argument "generated PCM byte count overflows"
    else
      live operation value (fun raw ->
          let pcm_f32 = Bytes.create (frames * value.channels * 4) in
          match mixer_result operation (raw_generate raw pcm_f32) with
          | Error _ as failure -> failure
          | Ok mixed_bytes -> Ok { mixed_bytes; pcm_f32 })

  let destroy (value : t) =
    on_main "SDL3_mixer.Mixer.destroy" (fun () ->
        if value.destroyed then Ok ()
        else
          let tracks = Atomic.get value.tracks and audios = Atomic.get value.audios in
          if tracks <> 0 || audios <> 0 then
            error "SDL3_mixer.Mixer.destroy" Parent_has_dependents
              (Printf.sprintf "mixer still owns %d track(s) and %d audio handle(s)" tracks audios)
          else begin
            value.destroyed <- true;
            Atomic.decr live_mixers;
            raw_destroy_mixer value.raw;
            Ok ()
          end)
end

module Audio = struct
  type t = audio_handle

  let owned mixer raw =
    Atomic.incr live_audios;
    Atomic.incr mixer.audios;
    let value : audio_handle = { raw; mixer; destroyed = false } in
    Gc.finalise
      (fun (value : audio_handle) ->
        if not value.destroyed then begin
          value.destroyed <- true;
          Atomic.decr live_audios;
          Atomic.decr value.mixer.audios;
          Release_queue.audio value.raw
        end)
      value;
    value

  let with_mixer operation mixer callback =
    Mixer.live operation mixer (fun raw ->
        match callback raw with
        | Ok audio -> Ok (owned mixer audio)
        | Error message -> error operation Mixer_error message)

  let load_bytes mixer bytes =
    let operation = "SDL3_mixer.Audio.load_bytes" in
    if Bytes.length bytes = 0 then
      error operation Invalid_argument "encoded audio buffer must be non-empty"
    else with_mixer operation mixer (fun raw -> raw_load_bytes raw bytes)

  let destroy (value : t) =
    on_main "SDL3_mixer.Audio.destroy" (fun () ->
        if value.destroyed then Ok ()
        else begin
          value.destroyed <- true;
          Atomic.decr live_audios;
          Atomic.decr value.mixer.audios;
          raw_destroy_audio value.raw;
          Ok ()
        end)
end

module Track = struct
  type t = track_handle
  type status = Stopped | Playing | Paused

  let owned mixer raw =
    Atomic.incr live_tracks;
    Atomic.incr mixer.tracks;
    let value : track_handle = { raw; mixer; destroyed = false } in
    Gc.finalise
      (fun (value : track_handle) ->
        if not value.destroyed then begin
          value.destroyed <- true;
          Atomic.decr live_tracks;
          Atomic.decr value.mixer.tracks;
          Release_queue.track value.raw
        end)
      value;
    value

  let live operation (value : t) callback =
    on_main operation (fun () ->
        if value.destroyed then error operation Destroyed "track is destroyed"
        else if value.mixer.destroyed then error operation Destroyed "track's mixer is destroyed"
        else
          match require_initialized operation with
          | Error _ as failure -> failure
          | Ok () -> callback value.raw)

  let mutate operation value call =
    live operation value (fun raw -> mixer_result operation (call raw))

  let create mixer =
    Mixer.live "SDL3_mixer.Track.create" mixer (fun raw ->
        match raw_create_track raw with
        | Ok track -> Ok (owned mixer track)
        | Error message -> error "SDL3_mixer.Track.create" Mixer_error message)

  let set_audio (value : t) (audio : Audio.t) =
    let operation = "SDL3_mixer.Track.set_audio" in
    if audio.destroyed then error operation Destroyed "audio handle is destroyed"
    else if value.mixer != audio.mixer then
      error operation Invalid_argument "track and audio belong to different mixers"
    else mutate operation value (fun raw -> raw_set_track_audio raw audio.raw)

  let set_gain (value : t) gain =
    let operation = "SDL3_mixer.Track.set_gain" in
    if (not (Float.is_finite gain)) || gain < 0. then
      error operation Invalid_argument "track gain must be finite and non-negative"
    else mutate operation value (fun raw -> raw_set_track_gain raw gain)

  let valid_loops loops = loops >= -1

  let play (value : t) ?(loops = 0) ?(fade_in_ms = 0) () =
    let operation = "SDL3_mixer.Track.play" in
    if not (valid_loops loops) then
      error operation Invalid_argument "loop count must be -1 or non-negative"
    else if fade_in_ms < 0 then
      error operation Invalid_argument "fade duration must be non-negative"
    else mutate operation value (fun raw -> raw_play_track raw loops fade_in_ms)

  let stop (value : t) ?(fade_out_ms = 0) () =
    let operation = "SDL3_mixer.Track.stop" in
    if fade_out_ms < 0 then error operation Invalid_argument "fade duration must be non-negative"
    else mutate operation value (fun raw -> raw_stop_track raw fade_out_ms)

  let playing (value : t) =
    live "SDL3_mixer.Track.playing" value (fun raw -> Ok (raw_track_playing raw))

  let destroy (value : t) =
    on_main "SDL3_mixer.Track.destroy" (fun () ->
        if value.destroyed then Ok ()
        else begin
          value.destroyed <- true;
          Atomic.decr live_tracks;
          Atomic.decr value.mixer.tracks;
          raw_destroy_track value.raw;
          Ok ()
        end)
end

module Channels = struct
  type channel = int
  type slot = { track : Track.t; mutable volume : float }
  type t = { mixer : Mixer.t; slots : slot array; mutable destroyed : bool }

  let operation name = "SDL3_mixer.Channels." ^ name
  let valid_volume value = Float.is_finite value && value >= 0.

  let live name value callback =
    let op = operation name in
    if value.destroyed then error op Destroyed "channel pool is destroyed"
    else if value.mixer.destroyed then error op Destroyed "channel pool mixer is destroyed"
    else on_main op callback

  let slot name value channel callback =
    live name value (fun () ->
        if channel < 0 || channel >= Array.length value.slots then
          error (operation name) Invalid_argument "channel index is out of range"
        else callback value.slots.(channel))

  let create mixer ~count =
    let op = operation "create" in
    if count <= 0 || count > 4096 then error op Invalid_argument "channel count must be in 1..4096"
    else
      Mixer.live op mixer (fun _ ->
          let made = ref [] in
          let rec build remaining =
            if remaining = 0 then
              Ok { mixer; slots = Array.of_list (List.rev !made); destroyed = false }
            else
              match Track.create mixer with
              | Error _ as failure ->
                  List.iter (fun slot -> ignore (Track.destroy slot.track)) !made;
                  failure
              | Ok track ->
                  made := { track; volume = 1. } :: !made;
                  build (remaining - 1)
          in
          build count)

  let allocate value =
    live "allocate" value (fun () ->
        let rec find index =
          if index = Array.length value.slots then
            error (operation "allocate") Mixer_error "no free channel"
          else
            match Track.playing value.slots.(index).track with
            | Ok false -> Ok index
            | Ok true -> find (index + 1)
            | Error _ as failure -> failure
        in
        find 0)

  let play value ?channel ?(loops = 0) ?(fade_in_ms = 0) audio =
    let selected = match channel with Some channel -> Ok channel | None -> allocate value in
    match selected with
    | Error _ as failure -> failure
    | Ok channel ->
        slot "play" value channel (fun selected ->
            match Track.set_audio selected.track audio with
            | Error _ as failure -> failure
            | Ok () -> (
                match Track.set_gain selected.track selected.volume with
                | Error _ as failure -> failure
                | Ok () ->
                    Result.map (fun () -> channel) (Track.play selected.track ~loops ~fade_in_ms ())
                ))

  let set_volume value channel volume =
    if not (valid_volume volume) then
      error (operation "set_volume") Invalid_argument
        "channel volume must be finite and non-negative"
    else
      slot "set_volume" value channel (fun selected ->
          match Track.set_gain selected.track volume with
          | Error _ as failure -> failure
          | Ok () ->
              selected.volume <- volume;
              Ok ())

  let destroy value =
    on_main (operation "destroy") (fun () ->
        if value.destroyed then Ok ()
        else begin
          let failure = ref None in
          Array.iter
            (fun selected ->
              match Track.destroy selected.track with
              | Ok () -> ()
              | Error error -> if !failure = None then failure := Some error)
            value.slots;
          match !failure with
          | Some error -> Error error
          | None ->
              value.destroyed <- true;
              Ok ()
        end)
end

module Music = struct
  type t = Track.t
  type status = Track.status = Stopped | Playing | Paused

  let create = Track.create
  let set_audio = Track.set_audio
  let play = Track.play
  let stop = Track.stop
  let destroy = Track.destroy
end
