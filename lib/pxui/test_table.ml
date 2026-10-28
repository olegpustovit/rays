open Rays
module Ui = Pxui.Ui

let frame count events =
  let frame = Test_ui.frame ~scale:1. ~time:(float count /. 60.) events in
  { frame with input = { frame.input with size = (360, 240) } }

let run () =
  let ui = Ui.create ?font_path:(Sys.getenv_opt "RAYS_UI_FONT") () in
  Fun.protect
    ~finally:(fun () -> Ui.destroy ui)
    (fun () ->
      let cells = ref 0 and first = ref max_int in
      let build ui =
        Ui.table ui ~at:(0., 0.) ~w:300. ~h:168. ~headers:[| "index"; "value" |] ~rows:1_000_000
          ~cell:(fun row column ->
            incr cells;
            first := min !first row;
            string_of_int (row + column))
          "table"
      in
      ignore (Ui.frame ui (frame 0 []) build);
      cells := 0;
      ignore (Ui.frame ui (frame 1 []) build);
      assert (!cells > 0 && !cells <= 20);
      let clicked, _ =
        Ui.frame ui
          (frame 2
             [
               Input.Pointer_pressed (Input.LeftButton, (30., 36.));
               Input.Pointer_released (Input.LeftButton, (30., 36.));
             ])
          build
      in
      assert (clicked = Some 0);
      cells := 0;
      first := max_int;
      ignore
        (Ui.frame ui
           (frame 3 [ Input.Pointer_moved (30., 100.); Input.Scrolled (0., -10000.) ])
           build);
      assert (!cells >= 12 && !cells <= 20 && !first = 10000);
      cells := 0;
      ignore (Ui.frame ui (frame 4 []) build);
      if not (!cells >= 12 && !cells <= 20 && !first = 10000) then
        failwith (Printf.sprintf "scrolled table cells=%d first=%d" !cells !first);
      let clicked, _ =
        Ui.frame ui
          (frame 5
             [
               Input.Pointer_pressed (Input.LeftButton, (30., 36.));
               Input.Pointer_released (Input.LeftButton, (30., 36.));
             ])
          build
      in
      assert (clicked = Some 10000);
      for count = 6 to 15 do
        cells := 0;
        first := max_int;
        ignore
          (Ui.frame ui
             (frame count [ Input.Pointer_moved (30., 100.); Input.Scrolled (0., -3.) ])
             build);
        assert (!cells >= 12 && !cells <= 20 && !first = 10000 + ((count - 5) * 3))
      done;
      cells := 0;
      first := max_int;
      ignore
        (Ui.frame ui
           (frame 16
              [
                Input.Trackpad_scrolled { delta = (0., -72.); phase = Moved; time = 16. /. 60. };
                Input.Scrolled (0., -3.);
              ])
           build);
      assert (!cells >= 12 && !cells <= 20 && !first = 10033);
      ignore
        (Ui.frame ui
           (frame 17
              [ Input.Trackpad_scrolled { delta = (0., -72.); phase = Moved; time = 17. /. 60. } ])
           build);
      ignore
        (Ui.frame ui
           (frame 18
              [ Input.Trackpad_scrolled { delta = (0., 0.); phase = Lifted; time = 18. /. 60. } ])
           build);
      cells := 0;
      first := max_int;
      ignore (Ui.frame ui (frame 19 []) build);
      assert (!cells >= 12 && !cells <= 20 && !first > 10036);
      print_endline "table: million-row virtualization and row click passed")
