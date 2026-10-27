# Time and frame pacing

Time reaches user code only through `Frame.t`: `time` (seconds since the sketch
started), `dt` (seconds since the previous frame), `fps` (`1 /. dt`), and
`count`. There is no global clock, time scale, scheduler, or easing module;
animation state, timers, and pauses live in the immutable model and advance by
`dt`.

## Clocks

`Sketch.config.clock` chooses the source:

- `Realtime` (default) measures wall time. `dt` is clamped to 0.1 s so a stall
  (debugger, minimized window) does not produce one huge step.
- `Fixed dt` advances `time` by exactly `dt` per frame and disables vsync.
  `Sketch.export` uses it, so exported sequences are artifact-deterministic.

## Frame rate

`Sketch.config.fps` requests a cap. With vsync (the `Realtime` default)
presentation already paces frames; without vsync the loop sleeps out the rest
of the frame. The clock and pacing are private to `Sketch`.

## Example

```ocaml
let update model (frame : Frame.t) =
  if model.paused then model
  else { model with x = model.x +. model.vx *. frame.input.dt }
```
