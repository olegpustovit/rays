open Rays

(* A 3D layer that keeps its prepared identity is rendered offscreen once and composited
   afterwards (Renderer retained views).  The picture must not change when that
   starts, and a changed camera must never show the old picture. *)
let run () =
  let config = { Sketch.default_config with width = 96; height = 64; title = "retained view";
                 fps = None; clock = Sketch.Fixed (1. /. 60.) } in
  let camera distance = Camera.perspective ~at:(Rays_math.Vec3.create 0. 0. distance) ~target:Rays_math.Vec3.zero () in
  let far_camera = camera 6. and near_camera = camera 2.5 in
  let sphere = Scene.D3.mesh ~material:(Material.matte Rays_math.Color.white)
      (Test_meshes.sphere ~radius:1. ()) in
  let scene3 = Scene.D3.create [ sphere ] in
  let view (_ : unit) (frame : Frame.t) =
    let camera = if frame.input.frame > 8 then near_camera else far_camera in
    (* the corner square changes every frame: only the UI layer differs between frames *)
    Scene.[ clear Rays_math.Color.black; view3d ~camera scene3;
            rect ~at:(0, 0) ~w:6 ~h:6 ~fill:(Rays_math.Color.rgb (frame.input.frame * 20 mod 256) 0 0) () ] in
  (* an 8x8 grid of samples of the presented frame, away from the corner square *)
  let path = Filename.concat (Filename.temp_dir "rays-retained-view" "") "frame.png" in
  let grid () =
    (match Canvas.save_screen_png path with Ok () -> () | Error message -> failwith message);
    let image = Image.load_exn path in
    Fun.protect ~finally:(fun () -> ignore(Image.destroy image)) (fun () ->
      let w, h = Image.Private.dimensions image in
      let pixels = Result.get_ok (Image.Private.pixels image) in
      List.init 64 (fun i ->
        let x = (i mod 8 + 1) * w / 10 + w / 20 and y = ((i / 8) + 1) * h / 10 + h / 20 in
        Bytes.sub pixels (((y * w) + x) * 4) 4)) in
  let seen = ref [] in
  let after_present () (frame : Frame.t) = seen := (frame.input.frame, grid ()) :: !seen in
  Result.get_ok(Sketch.run_state ~config ~max_frames:12 ~init:(fun _ -> ()) ~update:(fun () _ -> ()) ~view
    ~after_present ());
  Sys.remove path;
  let at n = List.assoc n !seen in
  if List.for_all (( = ) (List.hd (at 2))) (at 2) then
    failwith "retained view: the sphere is not in the picture";
  let same a b = at a = at b in
  if not (same 2 3 && same 3 5 && same 5 8) then
    failwith "retained view: the picture changed when the layer started being retained";
  if same 8 9 then failwith "retained view: a camera change showed the previous picture";
  if not (same 9 10 && same 10 12) then
    failwith "retained view: the picture changed when the new camera's layer was retained";
  print_endline "retained view: direct, retained and camera-change frames agree"
