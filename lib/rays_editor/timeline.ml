open Rays

type mode = Playing | Paused | Stopped
type t = { mode : mode; time : float; frame : int64 }
type change = Advanced | Paused_now | Resumed | Stopped_now | Reset_now | Seeked

let create () = { mode = Playing; time = 0.; frame = 0L }
let mode value = value.mode
let time value = value.time
let frame value = value.frame
let reset _ = ({ mode = Playing; time = 0.; frame = 0L }, [ Reset_now ])
let stop _ = ({ mode = Stopped; time = 0.; frame = 0L }, [ Stopped_now ])

let toggle_pause value =
  match value.mode with
  | Playing -> ({ value with mode = Paused }, [ Paused_now ])
  | Paused | Stopped -> ({ value with mode = Playing }, [ Resumed ])

(* Time follows the mean step observed so far (1/60 s before any), since the
   runtime clock may vary. Seeking pauses so playback does not fight a scrub. *)
let seek value ~frame =
  let frame = Int64.max 0L frame in
  let step = if value.frame > 0L then value.time /. Int64.to_float value.frame else 1. /. 60. in
  ( {
      mode = (if value.mode = Stopped then Stopped else Paused);
      frame;
      time = Int64.to_float frame *. step;
    },
    [ Seeked ] )

let update value (runtime_frame : Frame.t) =
  match value.mode with
  | Playing when Float.is_finite runtime_frame.input.dt && runtime_frame.input.dt > 0. ->
      ( { value with time = value.time +. runtime_frame.input.dt; frame = Int64.succ value.frame },
        [ Advanced ] )
  | Playing | Paused | Stopped -> (value, [])

let changed_context =
  List.exists (function
    | Advanced | Stopped_now | Reset_now | Seeked -> true
    | Paused_now | Resumed -> false)
