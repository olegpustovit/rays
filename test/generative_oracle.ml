(* Test-only oracle: examples/generative/main.ml as it stood at 3baf0095, up to its
   application entry point. The program was deleted when the example became a Lisp
   workspace; test_2d_ports compares the port's scene with this one. Do not edit it
   to follow the port. *)
open Rays

let update time (frame : Frame.t) =
  if Frame.has_event
      (function
        | Input.Key_pressed Input.Escape
        | Input.Key_pressed (Input.KeyChar 'q') -> true
        | _ -> false)
      frame
  then Sketch.quit ();
  time +. frame.input.dt

let view time (frame : Frame.t) =
  let circles =
    List.init 501 (fun index ->
      let y = index in
      let phase = float_of_int index *. 4. in
      let radius =
        50. +. (40. *. sin (time +. (phase *. 0.0037)))
        |> int_of_float
      in
      let x =
        250. +. (200. *. sin (time +. (phase *. 0.01)))
        |> int_of_float
      in
      let color =
        Rays_math.Color.rgb
          (int_of_float (127. +. (127. *. sin (phase *. 0.01))))
          (int_of_float (127. +. (127. *. sin (phase *. 0.011))))
          (int_of_float (127. +. (127. *. sin (phase *. 0.012))))
      in
      Scene.circle ~at:(x, y) ~radius ~fill:color ())
  in
  let center_x, center_y = (fst frame.input.size) / 2, (snd frame.input.size) / 2 in
  Scene.([
    clear Rays_math.Color.black;
    translate center_x center_y [
      rotate (Float.pi /. 2.) [
        translate (-center_x) (-center_y) [
          scale 2. 2. circles;
        ];
      ];
    ];
  ])
