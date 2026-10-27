(* Per-sketch realtime clock and frame pacing. *)

type t = {
  origin : float;
  mutable last_frame_time : float;
  mutable delta_time : float;
  mutable target_fps : int option;
  mutable vsync_enabled : bool;
}

let create ~target_fps ~vsync_enabled = {
  origin=Unix.gettimeofday ();
  last_frame_time=0.;
  delta_time=0.;
  target_fps=(if target_fps > 0 then Some target_fps else None);
  vsync_enabled;
}

let now state = Unix.gettimeofday () -. state.origin
let delta state = state.delta_time
let update state =
  let current_time = now state in
  state.delta_time <- Float.min (current_time -. state.last_frame_time) 0.1;
  state.last_frame_time <- current_time

let limit_frame_rate state = match state.target_fps with
  | None -> ()
  | Some _ when state.vsync_enabled -> ()
  | Some fps ->
      let delay = 1. /. float_of_int fps -. state.delta_time in
      if delay > 0. then Unix.sleepf delay
