open Rays

let require condition message = if not condition then failwith message

let serialize scene =
  let ir = Result.get_ok (Scene.Layer.to_ir scene) in
  Scene_command.Render_ir.serialize ir

let first_geometry scene =
  let ir = Result.get_ok (Scene.Layer.to_ir scene) in
  Array.find_map
    (function Scene_command.Render_ir.Geometry geometry -> Some geometry | _ -> None)
    (Scene_command.Render_ir.commands_readonly ir)
  |> Option.get

let require_geometry_reuse label make =
  let first = first_geometry [ make () ] and second = first_geometry [ make () ] in
  require
    (first.vertices == second.vertices && first.indices == second.indices)
    (label ^ " rebuilt tessellated geometry arrays")

let measure calls make =
  for _ = 1 to 20 do
    ignore (Sys.opaque_identity (make ()))
  done;
  Gc.full_major ();
  let gc_before = Gc.quick_stat () and allocated_before = Gc.allocated_bytes () in
  for _ = 1 to calls do
    ignore (Sys.opaque_identity (make ()))
  done;
  let gc_after = Gc.quick_stat () and allocated_after = Gc.allocated_bytes () in
  let bytes_per_word = float (Sys.word_size / 8) in
  ( (allocated_after -. allocated_before) /. float calls,
    (gc_after.promoted_words -. gc_before.promoted_words) *. bytes_per_word /. float calls )

(* a window presents only a picture that is not the same as the last one *)
let same_picture () =
  let picture label =
    Scene.[ clear Rays_math.Color.black; translate 4 8 [ text label ~at:(1, 2) ] ]
  in
  require
    (Scene.Layer.same (picture "a") (picture "a"))
    "an equal picture rebuilt counts as changed";
  require
    (not (Scene.Layer.same (picture "a") (picture "b")))
    "a changed label counts as the same picture"

let run () =
  Scene.Layer.with_cache_context (Scene.Layer.create_cache_context ()) (fun () ->
      same_picture ();
      let make x y =
        Scene.
          [
            rounded_rect ~at:(x, y) ~w:84 ~h:31 ~radius:7
              ~fill:(Rays_math.Color.rgba 12 34 56 220)
              ~stroke:(Rays_math.Color.rgb 90 80 70) ();
          ]
      in
      let baseline = serialize (make 17 23) in
      require (baseline = serialize (make 17 23)) "cached rounded geometry changed IR";
      require (baseline <> serialize (make 18 23)) "translation was omitted from cache key lowering";
      for index = 0 to 299 do
        ignore
          (serialize
             Scene.
               [
                 rounded_rect
                   ~at:(index land 7, index land 3)
                   ~w:(20 + index) ~h:31 ~radius:7 ~fill:Rays_math.Color.red ();
               ])
      done;
      require (baseline = serialize (make 17 23)) "bounded eviction changed rounded geometry";
      for _ = 1 to 20 do
        ignore (make 17 23)
      done;
      Gc.full_major ();
      let before = Gc.allocated_bytes () in
      for index = 1 to 1_000 do
        ignore (make index (index land 31))
      done;
      let per_call = (Gc.allocated_bytes () -. before) /. 1_000. in
      require (per_call < 512.) "rounded geometry cache allocation regression";
      let primitive () =
        Scene.[ circle ~at:(44, 37) ~radius:19 ~fill:(Rays_math.Color.rgba 11 22 33 210) () ]
      in
      let first = first_geometry (primitive ()) and second = first_geometry (primitive ()) in
      require
        (first.vertices == second.vertices && first.indices == second.indices)
        "structurally identical primitive rebuilt geometry arrays";
      let primitive_baseline = serialize (primitive ()) in
      for index = 0 to 299 do
        ignore
          (serialize
             Scene.
               [
                 rect ~at:(index, 0) ~w:(index + 1) ~h:3
                   ~fill:(Rays_math.Color.rgba (index land 255) 20 30 255)
                   ();
               ])
      done;
      require
        (primitive_baseline = serialize (primitive ()))
        "bounded primitive cache eviction changed geometry";
      let ellipse_node () =
        Scene.circle ~at:(44, 37) ~radius:19 ~fill:(Rays_math.Color.rgba 11 22 33 210) ()
      in
      let ellipse_first = ellipse_node () and ellipse_second = ellipse_node () in
      require (ellipse_first == ellipse_second)
        "stable ellipse did not reuse its immutable scene node";
      for index = 0 to 299 do
        ignore
          (Scene.circle
             ~at:(index, index land 15)
             ~radius:(index + 2) ~fill:(Rays_math.Color.rgb 0 0 255) ())
      done;
      require
        (primitive_baseline = serialize (primitive ()))
        "bounded ellipse cache eviction changed geometry";
      ignore (ellipse_node ());
      Gc.full_major ();
      let ellipse_before = Gc.allocated_bytes () in
      for _ = 1 to 1_000 do
        ignore (ellipse_node ())
      done;
      let ellipse_per_hit = (Gc.allocated_bytes () -. ellipse_before) /. 1_000. in
      require (ellipse_per_hit < 512.) "stable ellipse cache allocation regression";
      ignore (first_geometry (primitive ()));
      Gc.full_major ();
      let primitive_before = Gc.allocated_bytes () in
      for _ = 1 to 1_000 do
        ignore (first_geometry (primitive ()))
      done;
      let primitive_per_stage = (Gc.allocated_bytes () -. primitive_before) /. 1_000. in
      require (primitive_per_stage < 4_000.) "stable primitive staging allocation regression";
      let polygon () =
        Scene.polygon
          [ (11, 7); (83, 13); (72, 61); (19, 55) ]
          ~fill:(Rays_math.Color.rgba 17 41 89 213)
          ~stroke:(Rays_math.Color.rgb 220 170 90) ()
      in
      require_geometry_reuse "polygon" polygon;
      require_geometry_reuse "rect" (fun () ->
          Scene.rect ~at:(13, 17) ~w:71 ~h:29
            ~fill:(Rays_math.Color.rgba 27 51 93 207)
            ~stroke:(Rays_math.Color.rgb 201 151 71) ());
      require_geometry_reuse "line" (fun () ->
          Scene.line ~from_:(7, 9) ~to_:(93, 67) ~width:5
            ~color:(Rays_math.Color.rgba 61 103 149 219)
            ());
      let native_paths () =
        Scene.
          [
            rect ~at:(8, 12) ~w:63 ~h:37
              ~fill:(Rays_math.Color.rgba 19 47 83 229)
              ~stroke:(Rays_math.Color.rgb 191 157 113) ();
            polygon
              [ (17, 71); (41, 19); (89, 27); (101, 78); (53, 91) ]
              ~fill:(Rays_math.Color.rgba 13 79 127 197)
              ();
            line ~from_:(3, 97) ~to_:(117, 43) ~width:7
              ~color:(Rays_math.Color.rgba 149 83 31 221)
              ();
          ]
      in
      let native_baseline = serialize (native_paths ()) in
      require
        (native_baseline = serialize (native_paths ()))
        "cached native path geometry changed exact serialized IR";
      require
        (serialize
           Scene.
             [
               line ~from_:(3, 97) ~to_:(117, 43) ~width:6
                 ~color:(Rays_math.Color.rgba 149 83 31 221)
                 ();
             ]
        <> serialize
             Scene.
               [
                 line ~from_:(3, 97) ~to_:(117, 43) ~width:7
                   ~color:(Rays_math.Color.rgba 149 83 31 221)
                   ();
               ])
        "native path cache omitted stroke width from its semantic key";
      require
        (serialize
           Scene.[ polygon [ (4, 5); (31, 7); (29, 37) ] ~fill:(Rays_math.Color.rgb 20 40 60) () ]
        <> serialize
             Scene.[ polygon [ (4, 5); (31, 7); (29, 37) ] ~fill:(Rays_math.Color.rgb 21 40 60) () ]
        )
        "native path cache omitted packed fill from its semantic key";
      let evicted_before =
        first_geometry
          Scene.
            [
              polygon
                [ (-71, -53); (-19, -47); (-23, -11) ]
                ~fill:(Rays_math.Color.rgba 23 59 101 227)
                ();
            ]
      in
      for index = 0 to 299 do
        ignore
          (Scene.polygon
             [ (index, 0); (index + 17, 3); (index + 11, 29) ]
             ~fill:(Rays_math.Color.rgba (index land 255) 37 73 211)
             ())
      done;
      let evicted_after =
        first_geometry
          Scene.
            [
              polygon
                [ (-71, -53); (-19, -47); (-23, -11) ]
                ~fill:(Rays_math.Color.rgba 23 59 101 227)
                ();
            ]
      in
      require
        (evicted_before.vertices != evicted_after.vertices
        && evicted_before.indices != evicted_after.indices)
        "native path cache did not deterministically evict its oldest geometry";
      require
        (native_baseline = serialize (native_paths ()))
        "native path cache eviction changed exact serialized IR";
      let allocated_per_scene, promoted_per_scene = measure 4_000 native_paths in
      require (allocated_per_scene < 12_000.) "stable native path cache allocation regression";
      require (promoted_per_scene < 256.) "stable native path cache promotion regression";
      Printf.printf
        "rounded/path caches deterministic/bounded: rounded %.0f B/hit, paths %.0f allocated + \
         %.1f promoted B/scene\n"
        per_call allocated_per_scene promoted_per_scene)
