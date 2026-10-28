open Rays

let green = Rays_math.Color.rgb 0 255 0
let blue = Rays_math.Color.rgb 0 0 255
let require condition message = if not condition then failwith message
let render_canvas ?density canvas scene = Result.get_ok (Canvas.render ?density canvas scene)
let native_stats canvas = Result.get_ok (Canvas.Private.native_stats canvas)

let channel canvas ~x ~y =
  match Result.get_ok (Canvas.pixel canvas ~x ~y) with
  | Some color -> Rays_math.Color.to_tuple color
  | None -> failwith "Canvas native pixel is out of bounds"

let live_handles = Ogpu.Impl.live_handles

let snapshot canvas =
  let image = Result.get_ok (Canvas.to_image canvas) in
  Fun.protect
    ~finally:(fun () -> ignore (Image.destroy image))
    (fun () -> Result.get_ok (Image.pixels image))

let region_has_pixel_other_than canvas ~x0 ~y0 ~x1 ~y1 expected =
  let found = ref false in
  for y = y0 to y1 do
    for x = x0 to x1 do
      if channel canvas ~x ~y <> expected then found := true
    done
  done;
  !found

let camera_depth () =
  let baseline = live_handles () in
  let canvas = Canvas.create_exn ~width:32 ~height:32 in
  Fun.protect
    ~finally:(fun () -> ignore (Canvas.destroy canvas))
    (fun () ->
      let mesh = Mesh.plane ~width:1. ~height:1. () in
      let cameras =
        [
          Camera.orthographic ~height:2. ~at:(Rays_math.Vec3.create 0. 0. 2.)
            ~target:Rays_math.Vec3.zero ();
          Camera.perspective ~at:(Rays_math.Vec3.create 0. 0. 0.15) ~target:Rays_math.Vec3.zero ();
        ]
      in
      List.iter
        (fun camera ->
          let far =
            Scene.D3.translate
              (Rays_math.Vec3.create 0. 0. (-0.02))
              [ Scene.D3.mesh ~material:(Material.unlit Rays_math.Color.red) ~cull:Cull_none mesh ]
          in
          let near = Scene.D3.mesh ~material:(Material.unlit green) ~cull:Cull_none mesh in
          let scene =
            Scene.[ clear Rays_math.Color.black; view3d ~camera (Scene.D3.create [ near; far ]) ]
          in
          render_canvas canvas scene;
          require
            (channel canvas ~x:16 ~y:16 = (0, 255, 0, 255))
            "camera native near/far clipping or depth order";
          let first = snapshot canvas in
          render_canvas canvas scene;
          require (snapshot canvas = first) "camera depth retained replay pixels";
          let transforms =
            [|
              Rays_math.Mat4.identity;
              Rays_math.Mat4.translation (Rays_math.Vec3.create 0. 0. (-0.02));
            |]
          in
          let instances =
            Scene.D3.instances_array ~material:(Material.unlit blue) ~cull:Cull_none mesh transforms
          in
          let scene =
            Scene.[ clear Rays_math.Color.black; view3d ~camera (Scene.D3.create [ instances ]) ]
          in
          render_canvas canvas scene;
          require (channel canvas ~x:16 ~y:16 = (0, 0, 255, 255)) "camera native instance clipping";
          let first = snapshot canvas in
          render_canvas canvas scene;
          require (snapshot canvas = first) "camera instance retained replay pixels")
        cameras);
  require (live_handles () = baseline) "camera native live-handle delta";
  print_endline
    "camera native depth: default orthographic, close perspective, depth order, instances and \
     retained replay pass"

let completed_source () =
  let baseline = live_handles () in
  let canvas = Canvas.create_exn ~width:65 ~height:3
  and destination = Canvas.create_exn ~width:65 ~height:3 in
  let images = ref [] in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun image -> ignore (Image.destroy image)) !images;
      ignore (Canvas.destroy canvas);
      ignore (Canvas.destroy destination))
    (fun () ->
      require (Result.is_error (Canvas.gpu_source canvas)) "fresh Canvas allowed a GPU borrow";
      let render color = render_canvas canvas Scene.[ clear color ] in
      let borrow () =
        let width, height, source = Result.get_ok (Canvas.gpu_source canvas) in
        require ((width, height) = (65, 3)) "Canvas borrow extent";
        let resource = Result.get_ok (Runtime_resources.Image.of_gpu ~width ~height ~source) in
        let image = Result.get_ok (Image.of_resource resource) in
        images := image :: !images;
        (source, image)
      in
      let expires source image =
        require (Option.is_none (source ())) "old Canvas source remains current";
        match Runtime_resources.Image.gpu_snapshot (Image.resource image) with
        | Error { kind = Ogpu.Error.Destroyed; _ } -> ()
        | _ -> failwith "expired Canvas image did not report Destroyed"
      in
      render Rays_math.Color.red;
      let source, image = borrow () in
      let generation = Runtime_resources.Image.generation (Image.resource image) in
      let published = image in
      let scene = Scene.[ clear Rays_math.Color.black; image published ~at:(0, 0) () ] in
      render_canvas destination scene;
      require
        (channel destination ~x:64 ~y:2 = (255, 0, 0, 255))
        "borrowed Canvas native consumer pixel";
      require (Canvas.pixel_stats canvas = (0, 0)) "Canvas borrowing captured or read source pixels";
      let wrong_context () =
        require (Option.is_none (source ())) "Canvas callback accepted wrong execution context";
        require
          (Result.is_error (Canvas.gpu_source canvas))
          "Canvas borrow accepted wrong execution context";
        require
          (Result.is_error (Canvas.destroy canvas))
          "Canvas destroy accepted wrong execution context";
        require
          (Result.is_error (Image.destroy image))
          "Image destroy accepted wrong execution context";
        let rejected = Result.is_error (Canvas.render canvas Scene.[ clear green ]) in
        require rejected "Canvas render accepted wrong execution context"
      in
      Domain.join (Domain.spawn wrong_context);
      let thread_result = ref (Ok ()) in
      let thread =
        Thread.create
          (fun () ->
            try
              require (Domain.is_main_domain ()) "thread fixture did not run on the initial domain";
              wrong_context ()
            with exn -> thread_result := Error exn)
          ()
      in
      Thread.join thread;
      (match !thread_result with Ok () -> () | Error exn -> raise exn);
      require (Option.is_some (source ())) "rejected wrong-context operations invalidated Canvas";
      render blue;
      expires source image;
      require
        (Runtime_resources.Image.generation (Image.resource image) = generation)
        "source expiry mutated published Image generation";
      let replay_failed = Result.is_error (Canvas.render destination scene) in
      require replay_failed "retained Scene replay accepted an expired Canvas source";
      let source, image = borrow () in
      let failed =
        Result.is_error
          (Canvas.render canvas Scene.[ rect ~at:(0, 0) ~w:1 ~h:1 (); clear Rays_math.Color.red ])
      in
      require failed "Canvas late Clear fixture succeeded";
      expires source image;
      require
        (Result.is_error (Canvas.gpu_source canvas))
        "failed render left a completed Canvas source";
      require (Canvas.pixel_stats canvas = (0, 0)) "failed Canvas render read or captured pixels";
      render green;
      let source, image = borrow () in
      let failed = Result.is_error (Canvas.render ~density:0 canvas Scene.[ clear blue ]) in
      require failed "Canvas accepted zero density";
      expires source image;
      require (Result.is_error (Canvas.gpu_source canvas)) "invalid density left a completed source";
      render green;
      let source, image = borrow () in
      ignore (Result.get_ok (Canvas.invalidate canvas));
      expires source image;
      render green;
      let captured = Result.get_ok (Canvas.to_image canvas) in
      images := captured :: !images;
      let _, _, _, bytes, lease =
        Result.get_ok (Runtime_resources.Image.borrow_snapshot (Image.resource captured))
      in
      Fun.protect
        ~finally:(fun () -> Runtime_resources.Image.release_snapshot lease)
        (fun () ->
          let saved = Bytes.copy bytes in
          require
            (Bytes.sub saved 0 4 = Bytes.of_string "\x00\xff\x00\xff")
            "Canvas CPU snapshot pixel";
          require (Canvas.pixel_stats canvas = (1, 1)) "Canvas capture actual readback count";
          render blue;
          let source, image = borrow () in
          ignore (Canvas.destroy canvas);
          expires source image;
          require
            (Canvas.pixel_stats canvas = (1, 1))
            "Canvas destruction read discarded GPU pixels";
          require
            (bytes = saved && Result.get_ok (Image.pixels captured) = saved)
            "Canvas updates or destruction changed retained CPU bytes";
          ignore (Canvas.destroy canvas)));
  require (live_handles () = baseline) "Canvas borrowed-source live-handle delta";
  print_endline
    "Canvas completed source: native consumer, domain/thread guards, attempt/CPU/destroy expiry, \
     retained CPU lease, no teardown readback, zero delta"

let rotated_image () =
  let source = Canvas.create_exn ~width:20 ~height:10
  and canvas = Canvas.create_exn ~width:60 ~height:60 in
  Fun.protect
    ~finally:(fun () ->
      ignore (Canvas.destroy source);
      ignore (Canvas.destroy canvas))
    (fun () ->
      render_canvas source
        Scene.[ clear Rays_math.Color.red; rect ~at:(10, 0) ~w:10 ~h:10 ~fill:green () ];
      let picture = Result.get_ok (Canvas.to_image source) in
      Fun.protect
        ~finally:(fun () -> ignore (Image.destroy picture))
        (fun () ->
          (* 20x10 at (10,20): centre (20,25). A quarter turn about it maps the red left half to
         x 15..25, y 25..35 and the green right half to x 15..25, y 15..25. *)
          render_canvas canvas
            Scene.
              [ clear Rays_math.Color.black; image picture ~at:(10, 20) ~angle:(Float.pi /. 2.) () ];
          require
            (channel canvas ~x:20 ~y:30 = (255, 0, 0, 255))
            "rotated image: red half not below the pivot";
          require
            (channel canvas ~x:20 ~y:20 = (0, 255, 0, 255))
            "rotated image: green half not above the pivot";
          require
            (channel canvas ~x:12 ~y:25 = (0, 0, 0, 255))
            "rotated image left its unrotated footprint";
          require
            (channel canvas ~x:28 ~y:25 = (0, 0, 0, 255))
            "rotated image left its unrotated footprint"));
  print_endline "rotated image: a quarter turn pivots about the destination centre"

let run () =
  completed_source ();
  rotated_image ();
  camera_depth ();
  let baseline = live_handles () in
  let canvas = Canvas.create_exn ~width:64 ~height:64 in
  let rendered = ref 0 in
  let render scene =
    render_canvas canvas scene;
    incr rendered
  in
  try
    render Scene.[ clear (Rays_math.Color.rgba 7 11 13 17) ];
    require (channel canvas ~x:0 ~y:0 = (7, 11, 13, 17)) "Canvas native clear-only pixel";
    render
      Scene.
        [ clear blue; rect ~at:(2, 2) ~w:4 ~h:4 ~fill:green (); text ~at:(12, 2) ~size:12 "steady" ];
    require (channel canvas ~x:0 ~y:0 = (0, 0, 255, 255)) "Canvas native drawn background";
    require (channel canvas ~x:3 ~y:3 = (0, 255, 0, 255)) "Canvas native drawn foreground";
    require
      (region_has_pixel_other_than canvas ~x0:12 ~y0:2 ~x1:55 ~y1:18 (0, 0, 255, 255))
      "Canvas native automatic text was not visible";
    let _, _, references = Font.Raster.automatic_counts () in
    require (references = 0) "Canvas retained automatic text through capture";
    Gc.full_major ();
    let gc_before = Gc.quick_stat () in
    for frame = 3 to 600 do
      let clear_color = if frame land 1 = 0 then Rays_math.Color.red else blue in
      render
        Scene.
          [
            clear clear_color;
            rect ~at:(2, 2) ~w:4 ~h:4 ~fill:green ();
            text ~at:(12, 2) ~size:12 "steady";
          ];
      if frame = 60 || frame = 600 then begin
        require
          (channel canvas ~x:0 ~y:0 = (255, 0, 0, 255))
          "Canvas native repeated-frame background";
        require
          (channel canvas ~x:3 ~y:3 = (0, 255, 0, 255))
          "Canvas native repeated-frame foreground"
      end
    done;
    let gc_after = Gc.quick_stat () in
    let promoted_per_frame =
      (gc_after.promoted_words -. gc_before.promoted_words) *. float (Sys.word_size / 8) /. 598.
    in
    require (promoted_per_frame < 4_096.) "Canvas native transient Scene promotion regression";
    let borrowed =
      Image.create ~width:2 ~height:2 ~color:(Rays_math.Color.rgba 37 83 149 211) ()
      |> Result.get_ok
    in
    let stage_failed =
      Result.is_error
        (Canvas.render canvas
           Scene.
             [
               image borrowed ~at:(3, 3) ();
               text ~at:(12, 2) ~size:12 "stage failure";
               clear Rays_math.Color.red;
             ])
    in
    require stage_failed "Canvas accepted a late native Clear";
    let _, _, references = Font.Raster.automatic_counts () in
    require (references = 0) "stage failure retained automatic text";
    require
      (Result.is_ok (Image.pixels borrowed))
      "stage failure shortened explicit Image ownership";
    let font = Result.get_ok (Font.system ~size:12 ()) in
    let camera =
      Camera.orthographic ~height:2. ~at:(Rays_math.Vec3.create 0. 0. 2.)
        ~target:Rays_math.Vec3.zero ()
    in
    let mesh =
      Result.get_ok
        (Mesh.View.create_owned ~indices:[| 0; 1; 2 |]
           [|
             Rays_math.Vec3.create (-0.8) (-0.8) 0.;
             Rays_math.Vec3.create 0.8 (-0.8) 0.;
             Rays_math.Vec3.create 0. 0.8 0.;
           |])
    in
    let scene3 =
      Scene.D3.create [ Scene.D3.mesh ~material:(Material.unlit Rays_math.Color.red) mesh ]
    in
    let reusable =
      Scene.
        [
          clear (Rays_math.Color.rgba 9 17 31 255);
          image borrowed ~at:(3, 3) ();
          text ~at:(12, 2) ~size:12 "reusable";
          view3d ~viewport:(32, 32, 32, 32) ~camera scene3;
          rect ~at:(46, 46) ~w:4 ~h:4 ~fill:green ();
        ]
    in
    render reusable;
    let first = snapshot canvas in
    let _, _, references = Font.Raster.automatic_counts () in
    require (references = 0) "reusable Scene retained automatic text after lowering";
    require
      (Result.is_ok (Image.pixels borrowed))
      "Scene release shortened explicit Image ownership";
    require
      (region_has_pixel_other_than canvas ~x0:12 ~y0:2 ~x1:55 ~y1:30 (9, 17, 31, 255))
      "released mixed Scene did not render text";
    require
      (region_has_pixel_other_than canvas ~x0:32 ~y0:32 ~x1:63 ~y1:63 (9, 17, 31, 255))
      "released mixed Scene did not render View3d";
    require
      (channel canvas ~x:47 ~y:47 = (0, 255, 0, 255))
      "Scene2 overlay did not preserve mixed-layer order";
    render reusable;
    let second = snapshot canvas in
    require (first = second) "released reusable Scene changed exact native pixels";
    (* Replacing an image's pixels in place must reach the next render. *)
    let before_replacement = channel canvas ~x:3 ~y:3 in
    (match
       Image.upload_rgba ~into:borrowed ~width:2 ~height:2
         ~rgba:(Bytes.of_string "\x11\x22\x33\xff\x11\x22\x33\xff\x11\x22\x33\xff\x11\x22\x33\xff")
         ()
     with
    | Ok _ -> ()
    | Error message -> failwith message);
    (* A fresh Scene value, as a sketch's view builds each frame, sees the
       replacement; the released [reusable] value keeps its snapshot. *)
    render Scene.[ clear (Rays_math.Color.rgba 9 17 31 255); image borrowed ~at:(3, 3) () ];
    require
      (channel canvas ~x:3 ~y:3 <> before_replacement
      && channel canvas ~x:3 ~y:3 = (0x11, 0x22, 0x33, 0xff))
      "replaced image pixels did not reach the render";
    ignore (Image.destroy borrowed);
    Font.destroy font;
    require (!rendered = 603) "Canvas native frame count";
    let stats = native_stats canvas in
    require (stats.logical_passes = 603L) "Canvas native submission accounting";
    require (stats.logical_draws >= 599L) "Canvas native draw accounting";
    require (stats.uploaded_bytes > 0L) "Canvas native upload accounting";
    let image = Result.get_ok (Canvas.to_image canvas) in
    Fun.protect
      ~finally:(fun () -> ignore (Image.destroy image))
      (fun () ->
        require (Result.get_ok (Image.get_size image) = (64, 64)) "Canvas native to_image extent";
        let pixels = Result.get_ok (Image.pixels image) in
        require
          (Bytes.sub pixels 0 4 = Bytes.of_string "\009\017\031\255")
          "Canvas native to_image exact pixel");
    ignore (Canvas.destroy canvas);
    require (native_stats canvas = Renderer.zero_stats) "Canvas native cache teardown delta";
    ignore (Canvas.destroy canvas);
    require (live_handles () = baseline) "Canvas native Metal live-handle delta";
    Printf.printf
      "Canvas native: exact released-scene reuse, %.1f promoted B/frame, frames1/2/60/600, bounded \
       cache, zero delta\n"
      promoted_per_frame
  with exn ->
    ignore (Canvas.destroy canvas);
    raise exn
