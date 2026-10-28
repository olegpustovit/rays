open Rays

let frame ?(keys = []) ?(time = 1.) ?(dt = 0.) events : Frame.t =
  {
    input =
      {
        (Input.at_time time) with
        dt;
        size = (320, 240);
        frame = 0;
        tick = 0;
        pointer = (20., 20.);
        keys;
        events;
      };
    pixel_scale = (1., 1.);
    fps = 60.;
    mouse_delta = (0., 0.);
  }

let snapshot value =
  let camera = Easy_camera.camera value in
  ( Camera.position camera,
    Camera.target camera,
    Camera.up camera,
    Camera.projection camera,
    Easy_camera.distance value )

let drag button =
  [
    Input.Pointer_pressed (button, (20., 20.));
    Pointer_moved (35., 27.);
    Pointer_released (button, (35., 27.));
  ]

let run () =
  let initial = Easy_camera.create ~inertia:false () in
  List.iter
    (fun button ->
      let plain = Easy_camera.update initial (frame (drag button)) in
      List.iter
        (fun key ->
          let mixed =
            [
              Input.Key_pressed key;
              Pointer_pressed (button, (20., 20.));
              Key_released key;
              Pointer_moved (35., 27.);
              Pointer_released (button, (35., 27.));
            ]
          in
          assert (snapshot (Easy_camera.update initial (frame ~keys:[ key ] mixed)) = snapshot plain))
        [ Input.Shift; Ctrl; Alt; Meta ];
      if button = Input.LeftButton then begin
        assert (Easy_camera.target plain = Easy_camera.target initial);
        assert (
          Camera.position (Easy_camera.camera plain) <> Camera.position (Easy_camera.camera initial))
      end
      else begin
        assert (Easy_camera.target plain <> Easy_camera.target initial);
        assert (Easy_camera.distance plain = Easy_camera.distance initial)
      end)
    [ Input.LeftButton; MiddleButton; RightButton ];
  assert (snapshot (Easy_camera.update initial (frame (drag Input.MouseX1))) = snapshot initial);
  let bounded = Easy_camera.with_control_area (Some (10, 10, 30, 30)) initial in
  assert (snapshot (Easy_camera.update bounded (frame (drag Input.LeftButton))) <> snapshot bounded);
  assert (
    snapshot
      (Easy_camera.update bounded
         (frame
            [
              Input.Pointer_pressed (Input.LeftButton, (40., 20.));
              Pointer_moved (35., 27.);
              Pointer_released (Input.LeftButton, (35., 27.));
            ]))
    = snapshot bounded);
  let moved = Easy_camera.update initial (frame (drag Input.LeftButton)) in
  assert (
    snapshot
      (Easy_camera.update initial { (frame (drag Input.LeftButton)) with pixel_scale = (2., 2.) })
    = snapshot moved);
  assert (
    snapshot
      (Easy_camera.update bounded
         (frame
            [
              Input.Pointer_pressed (Input.LeftButton, (20., 20.));
              Pointer_moved (90., 70.);
              Pointer_released (Input.LeftButton, (90., 70.));
            ]))
    = snapshot
        (Easy_camera.update initial
           (frame
              [
                Input.Pointer_pressed (Input.LeftButton, (20., 20.));
                Pointer_moved (90., 70.);
                Pointer_released (Input.LeftButton, (90., 70.));
              ])));
  let reset =
    Easy_camera.update moved
      (frame ~time:1.2 [ Input.Pointer_pressed (Input.LeftButton, (20., 20.)) ])
  in
  assert (snapshot reset = snapshot initial);
  List.iter
    (fun stop ->
      let active =
        Easy_camera.update initial
          (frame
             [
               Input.Pointer_pressed (Input.LeftButton, (20., 20.)); Pointer_moved (35., 27.); stop;
             ])
      in
      assert (
        snapshot (Easy_camera.update active (frame [ Input.Pointer_moved (70., 80.) ]))
        = snapshot active))
    [ Input.Pointer_cancelled Input.LeftButton; Focus_lost ];
  let wheel = Easy_camera.update initial (frame [ Input.Scrolled (0., 1.) ]) in
  assert (Easy_camera.distance wheel < Easy_camera.distance initial);
  let walk, _ = Easy_camera.fly ~speed:2. initial (frame ~dt:1. ~keys:[ Input.KeyChar 'w' ] []) in
  let fast, speed =
    Easy_camera.fly ~speed:2. initial
      (frame ~dt:1. ~keys:[ Input.KeyChar 'w'; Shift ] [ Input.Scrolled (0., 1.) ])
  in
  let eye = Camera.position (Easy_camera.camera initial) in
  let delta value = Rays_math.Vec3.sub (Camera.position (Easy_camera.camera value)) eye in
  assert (Rays_math.Vec3.nearly_equal (delta fast) (Rays_math.Vec3.scale (delta walk) 4.) ~eps:1e-12);
  assert (abs_float (speed -. 2.4) < 1e-12);
  print_endline
    "Easy camera: fixed buttons, modifiers, logical bounds, reset, cancellation, fly keys"

let dump () =
  let value = ref (Easy_camera.create ()) in
  let states =
    Array.init 128 (fun i ->
        let button = [| Input.LeftButton; MiddleButton; RightButton |].(i mod 3) in
        let key = [| Input.Shift; Ctrl; Alt; Meta |].(i mod 4) in
        let events =
          match i mod 4 with
          | 0 -> Input.Key_pressed key :: drag button
          | 1 -> [ Input.Key_released key; Pointer_moved (90., 50.) ]
          | 2 -> [ Input.Scrolled (1., 0.5) ]
          | _ -> []
        in
        value :=
          Easy_camera.update !value
            (frame ~time:(float i *. 0.5) ~dt:(1. /. 60.) ~keys:[ key ] events);
        snapshot !value)
  in
  Marshal.to_channel stdout states []

let bench () =
  let measure name count frames =
    let sample () =
      let value = ref (Easy_camera.create ~inertia:false ()) in
      Gc.full_major ();
      let allocated = Gc.allocated_bytes () and started = Unix.gettimeofday () in
      for i = 0 to count - 1 do
        value := Easy_camera.update !value frames.(i land 511)
      done;
      let seconds = Unix.gettimeofday () -. started
      and bytes = Gc.allocated_bytes () -. allocated in
      assert (Float.is_finite (Easy_camera.distance !value));
      (seconds, bytes)
    in
    ignore (sample ());
    let samples = Array.init 3 (fun _ -> sample ()) in
    let times = Array.map fst samples and bytes = Array.map snd samples in
    Array.sort Float.compare times;
    Array.sort Float.compare bytes;
    Printf.printf "%s,%d,%.9f,%.0f\n%!" name count times.(1) bytes.(1)
  in
  let inputs active =
    Array.init 512 (fun i ->
        let key = if i land 1 = 0 then Input.Shift else Input.Ctrl in
        frame ~keys:[ key ]
          ~time:(float i *. 0.5)
          (if active then
             Input.Key_pressed key
             :: drag [| Input.LeftButton; MiddleButton; RightButton |].(i mod 3)
           else []))
  in
  print_endline "name,frames,median_s,allocated_bytes";
  measure "easy_camera_idle_keys" 500000 (inputs false);
  measure "easy_camera_drag_keys" 100000 (inputs true)
