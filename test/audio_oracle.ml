(* Test-only oracle: examples/audio/main.ml as it stood at 3baf0095, up to its
   application entry point. The program was deleted when the example became a Lisp
   workspace; test_2d_ports compares the port's scene with this one. Do not edit it
   to follow the port. *)
open Rays

type model = {
  audio : Audio.t;
  notes : Audio.Sample.t array;
  active : int option;
}

let frequencies =
  [|261.63; 293.66; 329.63; 349.23; 392.; 440.; 493.88; 523.25|]

let make_note audio frequency =
  match Audio.Sample.synth audio ~waveform:Audio.Sample.Sine
      ~frequency ~duration:0.25 () with
  | Ok sample -> sample
  | Error message -> failwith message

let init _frame =
  let audio = match Audio.init () with Ok audio -> audio | Error message -> failwith message in
  let notes = Array.map (make_note audio) frequencies in
  {
    audio;
    notes;
    active = None;
  }

let pressed_note (frame : Frame.t) =
  List.find_map
    (function
      | Input.Key_pressed (Input.KeyChar key) when key >= '1' && key <= '8' ->
          Some (Char.code key - Char.code '1')
      | Input.Pointer_pressed (Input.LeftButton, (x, y))
        when y >= 150. && y < 310. && x >= 40. && x < 600. ->
          Some (min 7 (int_of_float ((x -. 40.) /. 70.)))
      | _ -> None)
    frame.input.events

let update model (frame : Frame.t) =
  let active = pressed_note frame in
  Option.iter (fun index ->
    ignore (Audio.Sample.play model.audio model.notes.(index))) active;
  { model with active }

let view model _frame =
  let keys =
    Array.to_list
      (Array.mapi (fun index _ ->
        let fill =
          if model.active = Some index then Rays_math.Color.hex_exn "#22d3ee"
          else Rays_math.Color.hex_exn "#f8fafc"
        in
        Scene.rect ~at:(40 + (index * 70), 150) ~w:64 ~h:160
          ~fill ~stroke:(Rays_math.Color.hex_exn "#0f172a") ())
        model.notes)
  in
  Scene.clear (Rays_math.Color.hex_exn "#111827")
  :: Scene.text ~at:(40, 60) "Press 1–8 or click a key"
  :: keys

let stop model =
  Array.iter Audio.Sample.destroy model.notes;
  ignore (Audio.shutdown model.audio)
