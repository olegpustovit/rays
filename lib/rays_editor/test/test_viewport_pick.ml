(* W6: viewport provenance.  The triangle to primitive map, the tags of nested
   merges through the editor's cook path, a CPU ray pick on a known seed and a
   known petal, the highlight (a per-corner colour, no recook), and the bench
   (`dune exec lib/rays_editor/test/test_main.exe -- bench_viewport_pick`). *)
open Flow_sop
module Session = Sop.Session
module Cook = Rays_editor.Private.Cook
module Pick = Rays_editor.Private.Pick
module Int_map = Network.Int_map
module Geometry = Rdk.Geometry

let fail message = failwith ("test_viewport_pick: " ^ message)
let check condition message = if not condition then fail message
open Test_text

let lower name =
  match Flow.Syntax.parse (case name) with
  | Error d -> fail (Diagnostic.to_string d)
  | Ok forms -> (match Lower.workspace ~extra:Editor_document.Kinds.descriptors ~factories:Sop.Nodes.factories forms with
      | Ok lowered -> lowered | Error d -> fail (Diagnostic.to_string d))


(* the cooked piece of a lowered fixture *)
let cooked ?(lit = Pick.Set.empty) ?cook lowered ~prepared =
  let cook = match cook with
    | Some cook -> cook
    | None ->
        let cook = Result.get_ok (Cook.create ~await:true
          ~prepare:(fun _ output -> incr prepared; Ok (Result.get_ok (Sop.Payload.geometry output.Session.payload)))
          ~seed:42L ~grain:97 ~domains:1 ~max_entries:512
          ~max_payload_bytes:(256 * 1024 * 1024) ()) in
        Cook.set_volatile cook (Lower.is_volatile lowered); cook in
  let timeline = Rays_editor.Timeline.create () in
  let update = Cook.update ~live:false ~lit
      cook ~settings:Rays_editor.Settings.none
      ~objects:(Lower.objects lowered) ~edit_error:None
      ~effects:Param.no_effects ~timeline_changes:[] ~timeline
      ~frame:(let frame = Test_editor_input.frame (0., 0.) [] 0 in
        { frame with input = { frame.input with dt = 0. } })
      ~frame_request:None in
  check (update.edit_error = None) "cook error";
  update.cook

let primitives_of tags tag =
  List.filter (fun p -> tags.(p) = tag) (List.init (Array.length tags) Fun.id)

(* a point just above the highest primitive of [tag] *)
let above geometry tag =
  let tags = Option.get (Pick.tags geometry) in
  let topology = Geometry.topology geometry and points = Geometry.positions geometry in
  let centre primitive =
    let first, last = Rdk.Topology.primitive_vertex_range topology primitive in
    let sum = ref (0., 0., 0.) in
    for v = first to last - 1 do
      let x, y, z = Rdk.Packed.Float3.get points (Rdk.Topology.point_of_vertex topology v) in
      let a, b, c = !sum in sum := (a +. x, b +. y, c +. z)
    done;
    let n = float (last - first) and a, b, c = !sum in a /. n, b /. n, c /. n in
  let best = List.fold_left (fun best p ->
    let (_, y, _) as c = centre p in
    match best with Some (_, (_, by, _)) when by >= y -> best | _ -> Some (p, c))
    None (primitives_of tags tag) in
  let x, y, z = snd (Option.get best) in
  Rays_math.Vec3.create x (y +. 1e-3) z

let present geometry tag = Array.exists (( = ) tag) (Option.get (Pick.tags geometry))

let down = Rays_math.Vec3.create 0. (-1.) 0.

let pick_run () =
  (* --- sunflower: one merge, tag = the seed --- *)
  let sunflower = lower "sunflower" in
  let prepared = ref 0 in
  let cook = cooked sunflower ~prepared in
  let piece = List.hd (Cook.pieces cook) in
  let tags = Option.get (Pick.tags (Result.get_ok (Sop.Payload.geometry piece.output.payload))) in
  let of_iteration lowered i = Int_map.fold (fun tag (o : Lower.origin) found ->
    if o.iter = [ i ] then tag :: found else found) lowered.Lower.provenance [] in
  let tag83 = List.hd (of_iteration sunflower 83) in
  let origin = Int_map.find tag83 sunflower.provenance in
  check (origin.iter = [ 83 ] && origin.input = 83) "the provenance table names iteration 83";
  check (Array.length tags = Geometry.primitive_count (Result.get_ok (Sop.Payload.geometry piece.output.payload))
    && Int_map.cardinal sunflower.provenance = 240) "a tag per primitive, 240 origins";
  (match Cook.pick piece ~origin:(above (Result.get_ok (Sop.Payload.geometry piece.output.payload)) tag83) ~direction:down with
   | Some (_, tag) -> check (tag = tag83) "the ray finds seed 83"
   | None -> fail "the ray missed seed 83");
  check (Cook.pick piece ~origin:(Rays_math.Vec3.create 0. 9. 9.) ~direction:down = None) "a miss";
  (* instanced pieces: the prototype is picked at every instance, hits compare in world units *)
  let module M = Rays_math.Mat4 in
  let module V = Rays_math.Vec3 in
  let over = above (Result.get_ok (Sop.Payload.geometry piece.output.payload)) tag83 in
  let instanced transforms = { piece with output = { piece.output with instances = Some transforms } } in
  let hit p origin = Cook.pick p ~origin ~direction:down in
  let d1 = match hit piece over with Some (d, _) -> d | None -> fail "no plain hit" in
  let shifted = V.create (over.V.x +. 100.) over.y over.z in
  (match hit (instanced [| M.identity; M.translation (V.create 100. 0. 0.) |]) shifted with
   | Some (d, tag) -> check (tag = tag83 && Float.abs (d -. d1) < 1e-9) "the second instance was not picked"
   | None -> fail "an instanced piece was not picked");
  check (hit (instanced [| M.translation (V.create 100. 0. 0.) |]) over = None) "the prototype at the origin was picked without an instance there";
  let big = V.create (2. *. over.V.x) (2. *. over.y) (2. *. over.z) in
  (match hit (instanced [| M.scaling (V.create 2. 2. 2.) |]) big with
   | Some (d, tag) -> check (tag = tag83 && Float.abs (d -. 2. *. d1) < 1e-7) "a scaled instance's distance is not in world units"
   | None -> fail "a scaled instance was not picked");
  (match hit (instanced [| M.translation (V.create 0. (-5.) 0.); M.identity |]) over with
   | Some (d, _) -> check (Float.abs (d -. d1) < 1e-9) "the nearest instance wins"
   | None -> fail "the nearest instance was not picked");
  (* --- the highlight: prepared again from the kept output, never recooked --- *)
  let misses = (Cook.stats cook).misses and before = !prepared in
  let lit = Pick.Set.singleton tag83 in
  let cook = cooked ~lit ~cook sunflower ~prepared in
  check (!prepared = before + 1) "a new highlight prepares the piece once";
  check ((Cook.stats cook).misses = misses) "the highlight does not recook";
  let lit_piece = List.hd (Cook.pieces cook) in
  let colours = Geometry.find_attribute ~owner:Rdk.Attribute.Vertex "Cd" lit_piece.prepared in
  (match Option.map Rdk.Attribute.View.storage colours with
   | Some (Rdk.Attribute.Float4 c) ->
       let c = Rdk.Packed.Float4.View.view c and topology = Geometry.topology lit_piece.prepared in
       let corner primitive = fst (Rdk.Topology.primitive_vertex_range topology primitive) in
       let seed = List.hd (primitives_of tags tag83)
       and other = List.hd (primitives_of tags (List.hd (of_iteration sunflower 0))) in
       check (Float.abs (c.x.(corner other) -. 0.3) < 1e-9) "the rest is dimmed";
       check (c.x.(corner seed) > c.x.(corner other)) "the probed iteration is tinted"
   | _ -> fail "no vertex colour");
  let cook = cooked ~lit ~cook sunflower ~prepared in
  check (!prepared = before + 1) "an unchanged highlight prepares nothing";
  let cook = cooked ~cook sunflower ~prepared in
  check (!prepared = before + 2) "deselecting prepares once";
  check ((List.hd (Cook.pieces cook)).prepared == (Result.get_ok (Sop.Payload.geometry piece.output.payload))) "deselect restores the geometry";
  Cook.close cook;
  (* --- bloom: petals sit inside a merge of merges; the tag is the innermost --- *)
  let bloom = lower "bloom" in
  let cook = cooked bloom ~prepared in
  let piece = List.hd (Cook.pieces cook) in
  let petal = Int_map.filter (fun _ (o : Lower.origin) -> o.iter = [ 4 ]) bloom.provenance in
  check (Int_map.cardinal petal = 1 || Int_map.cardinal petal = 2) "petal 4 of the first flower";
  let tag, o = List.hd (List.filter (fun (t, (o : Lower.origin)) ->
    present (Result.get_ok (Sop.Payload.geometry piece.output.payload)) t && o.iter = [ 4 ]) (Int_map.bindings bloom.provenance)) in
  (match Cook.pick piece ~origin:(above (Result.get_ok (Sop.Payload.geometry piece.output.payload)) tag) ~direction:down with
   | Some (_, hit) ->
       let hit = Int_map.find hit bloom.provenance in
       check (hit.iter = [ 4 ] && hit.site = o.site) "a petal picks its iteration through two merges"
   | None -> fail "the ray missed petal 4");
  let heart = List.find (fun (t, (o : Lower.origin)) -> o.iter = [] && o.input = 1
    && present (Result.get_ok (Sop.Payload.geometry piece.output.payload)) t) (Int_map.bindings bloom.provenance) in
  check ((snd heart).site <> o.site) "the heart is another site";
  Cook.close cook;
  print_endline "viewport pick tests passed"

(* The editor picks the same way: a click in the viewport is a ray through its camera, and the
   node that made the clicked box is selected in the graph pane. *)
let flat text clicks =
  let open Rays in
  let module E3 = Rays_editor.Editor in
  let workspace = Ws_fixture.of_text text in
  (* the cook is awaited, not waited for: the editor blocks on the job each frame submits *)
  let env = ref (E3.create ~ui_text_size:11 ~await:true ~workspace ~camera:(Easy_camera.create ~inertia:false ())
      ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
        |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun _ _ -> Scene.D3.empty) () |> Result.get_ok) in
  let count = ref 0 in
  let step ?(buttons = []) mouse events =
    incr count; env := E3.update !env (Test_editor_input.frame ~buttons mouse events !count) in
  let selected () =
    let directory = Filename.temp_dir "rays-pick2" "" in
    Fun.protect ~finally:(fun () ->
      Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
      Unix.rmdir directory) (fun () ->
      E3.crash_dump !env directory;
      let text = In_channel.with_open_bin (Filename.concat directory "editor.txt") In_channel.input_all in
      List.find_map (fun line -> if String.starts_with ~prefix:"scope selected: " line
        then Some (String.sub line 16 (String.length line - 16)) else None)
        (String.split_on_char '\n' text) |> Option.get) in
  for _ = 1 to 6 do step (450., 300.) [] done;
  let panes = E3.panes !env (Test_editor_input.frame (0., 0.) [] 0) in
  (* the scene opens as a list: a click on the object's row selects it and [i] enters its graph *)
  let gx, gy, _, _ = panes.graph in
  let row = float (gx + 60), float (gy + 24 + 12) in
  step row [];
  step row [ Input.Pointer_pressed (Input.LeftButton, row); Input.Pointer_released (Input.LeftButton, row) ];
  step row [ Input.Key_pressed (Input.KeyChar 'i') ];
  count := !count + 30;  (* past the double-click interval *)
  for _ = 1 to 4 do step (450., 300.) [] done;
  let at (x, y) =
    match Camera.world_to_screen ~viewport:panes.view (E3.view_camera !env) (Rays_math.Vec3.create x y 0.) with
    | Some p -> p.Rays_math.Vec3.x, p.y | None -> failwith "the point is behind the camera" in
  let click world =
    let p = at world in
    step p [ Input.Pointer_moved p ];
    step ~buttons:[ Input.LeftButton ] p [ Input.Pointer_pressed (Input.LeftButton, p) ];
    step p [ Input.Pointer_released (Input.LeftButton, p) ]; step p [] in
  List.iter (fun (world, expected, message) ->
    click world;
    if selected () <> expected then failwith (message ^ ": " ^ selected ())) clicks;
  E3.close !env

let run_2d () =
  flat {|(workspace flat
    (graph g :context sop
      (let* [left (sop/transform (sop/box) :translate [-2 0 0])
             right (sop/transform (sop/box) :translate [2 0 0])]
        (sop/merge left right))))|}
    [ (2., 0.), "g/right", "a click on the right box selected";
      (-2., 0.), "g/left", "a click on the left box selected";
      (0., 0.), "-", "a click on nothing kept the selection" ];
  (* a pick inside a collapsed loop selects the loop, which shows what is inside *)
  let loop = {|(workspace looped
    (graph g :context sop
      (let* [boxes (for [i (range 2)] (sop/transform (sop/box) :translate [(- (* i 4) 2) 0 0]))
             all (sop/merge boxes)]
        all)))%s|} in
  flat (Printf.sprintf (Scanf.format_from_string loop "%s") "")
    [ (-2., 0.), "g/boxes/@result", "a click inside an open loop selects the body's node" ];
  flat (Printf.sprintf (Scanf.format_from_string loop "%s") "\n(layout (node [\"g\" \"boxes\"] :collapsed true))")
    [ (-2., 0.), "g/boxes", "a click inside a collapsed loop did not select the loop" ]

let run () = pick_run (); run_2d ()
