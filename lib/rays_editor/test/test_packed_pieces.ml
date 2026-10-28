open Sop
open Test_support

let string_ok = function Ok value -> value | Error message -> fail message

let geometry () =
  let positions = Rdk.Packed.Float3.View.of_owned_exn
      ~x:[|-2.5; -1.5; -2.; 1.5; 2.5; 2.|]
      ~y:[|-0.5; -0.5; 0.5; -0.5; -0.5; 0.5|]
      ~z:(Array.make 6 0.) in
  let topology = Rdk.Topology.polygons_owned ~point_count:6
      ~vertex_points:[|0; 1; 2; 3; 4; 5|]
      ~primitive_offsets:[|0; 3; 6|] |> string_ok in
  let geometry = Rdk.Geometry.create ~positions ~topology () |> string_ok in
  let normals = Rdk.Attribute.create_owned ~name:"N"
      ~owner:Rdk.Attribute.Vertex
      (Rdk.Attribute.Float3 (Rdk.Packed.Float3.View.of_owned_exn
        ~x:(Array.make 6 0.) ~y:(Array.make 6 1.) ~z:(Array.make 6 0.)))
      |> string_ok in
  let geometry = Rdk.Geometry.with_attribute normals geometry |> string_ok in
  let pieces = Rdk.Attribute.create_owned ~name:"class"
      ~owner:Rdk.Attribute.Primitive (Rdk.Attribute.Int [|0; 1|]) |> string_ok in
  Rdk.Geometry.with_attribute pieces geometry |> string_ok

let positions mesh = (Rays.Mesh.View.packed_view mesh).vertices

let frame mouse_buttons = {
  Rays.Frame.input = { (Input.at_time 0.) with frame = 0; tick = 0; size = 100, 100;
    dt = 1. /. 60.; buttons = mouse_buttons };
  pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0.;
}

let timeline_frame ?(dt = 0.25) ?(events = []) ?(buttons = []) () =
  let frame = frame buttons in
  { frame with Rays.Frame.input = { frame.input with dt; events } }

let test_timeline_and_schedule () =
  let timeline = Rays_editor.Timeline.create () in
  let timeline, changes = Rays_editor.Timeline.update timeline
      (timeline_frame ()) in
  if Rays_editor.Timeline.time timeline <> 0.25
     || Rays_editor.Timeline.frame timeline <> 1L
     || not (Rays_editor.Timeline.changed_context changes)
  then fail "sketch timeline did not advance deterministically";
  let timeline, _ = Rays_editor.Timeline.toggle_pause timeline in
  let timeline, _ = Rays_editor.Timeline.update timeline (timeline_frame ()) in
  if Rays_editor.Timeline.mode timeline <> Rays_editor.Timeline.Paused
     || Rays_editor.Timeline.time timeline <> 0.25
  then fail "sketch timeline pause did not freeze time";
  let timeline, _ = Rays_editor.Timeline.stop timeline in
  if Rays_editor.Timeline.mode timeline <> Rays_editor.Timeline.Stopped
     || Rays_editor.Timeline.time timeline <> 0.
     || Rays_editor.Timeline.frame timeline <> 0L
  then fail "sketch timeline stop did not rewind";
  let timeline, reset_changes = Rays_editor.Timeline.reset timeline in
  let timeline, _ = Rays_editor.Timeline.update timeline (timeline_frame ()) in
  if Rays_editor.Timeline.mode timeline <> Rays_editor.Timeline.Playing
     || Rays_editor.Timeline.time timeline <> 0.25
     || not (Rays_editor.Timeline.changed_context reset_changes)
  then fail "sketch timeline reset did not restart playback";
  (* Seek jumps at the mean observed step (0.25 s), pauses, and changes the
     cook context; keys never drive the clock directly. *)
  let module T = Rays_editor.Timeline in
  let sought, seek_changes = T.seek timeline ~frame:8L in
  if T.frame sought <> 8L || T.time sought <> 2. || T.mode sought <> T.Paused
     || not (T.changed_context seek_changes)
  then fail "sketch timeline seek did not jump, pause, and change the context";
  if T.frame (fst (T.seek timeline ~frame:(-3L))) <> 0L then
    fail "sketch timeline seek did not clamp at frame 0";
  let quiet, _ = T.update (T.create ())
      (timeline_frame ~events:[Input.Key_pressed (Input.KeyChar 'p')] ()) in
  if T.mode quiet <> T.Playing then fail "timeline reacted to a key press";

  let static_graph = Lisp_sop.node "(sop/box)" in
  let dynamic_graph = Custom.plain ~operation:"timeline_test"
      ~dependencies:(Context.Dependencies.one Context.Dependencies.Time)
      [static_graph] (fun ~context:_ inputs -> Ok inputs.(0)) in
  let schedule, fire = Rays_editor__.Schedule.step
      Rays_editor__.Schedule.initial ~graphs:[static_graph]
      ~effects:Param.no_effects ~context_changed:false ~force:false
      ~busy:false
      ~frame:(timeline_frame ()) in
  if not fire then fail "cook scheduler skipped the initial graph";
  let schedule, fire = Rays_editor__.Schedule.step schedule
      ~graphs:[static_graph] ~effects:Param.no_effects ~context_changed:true
      ~force:false ~busy:false ~frame:(timeline_frame ()) in
  if fire then fail "static graph recooked for an unrelated clock change";
  let schedule, fire = Rays_editor__.Schedule.step schedule
      ~graphs:[dynamic_graph] ~effects:Param.no_effects ~context_changed:true
      ~force:false ~busy:false
      ~frame:(timeline_frame ~buttons:[Input.LeftButton] ()) in
  if fire then fail "dynamic graph cooked while a parameter drag was held";
  let _, fire = Rays_editor__.Schedule.step ~live:true schedule
      ~graphs:[dynamic_graph] ~effects:Param.no_effects ~context_changed:true
      ~force:false ~busy:false
      ~frame:(timeline_frame ~buttons:[Input.LeftButton] ()) in
  if not fire then fail "live cooking did not cook during a held drag";
  let _, fire = Rays_editor__.Schedule.step ~live:true schedule
      ~graphs:[dynamic_graph] ~effects:Param.no_effects ~context_changed:true
      ~force:false ~busy:true
      ~frame:(timeline_frame ~buttons:[Input.LeftButton] ()) in
  if fire then fail "live cooking cancelled a running cook during a drag";
  let _, fire = Rays_editor__.Schedule.step schedule
      ~graphs:[dynamic_graph] ~effects:Param.no_effects ~context_changed:false
      ~force:false ~busy:false ~frame:(timeline_frame ()) in
  if not fire then fail "cook scheduler lost the latest held dynamic request"

(* The piece offsets of the two-triangle fixture through an Exploded View
   node with the given parameters, as the editor's terminal mesh builds them. *)
let exploded pieces parameters =
  let node = Lisp_sop.node ~with_:["src", Lisp_sop.snapshot (geometry ())]
      (Printf.sprintf {|(sop/exploded_view (sop/ext_src) :piece_attribute "class" %s)|} parameters) in
  let explosion = Rdk_rays.Packed_pieces.explosion
      ~operation:(Node.operation node) ~fields:(Node.parameter_fields node) in
  if explosion = None then
    fail "an Exploded View node carried no explosion";
  Rdk_rays.Packed_pieces.mesh_for_explosion explosion pieces

let run () =
  test_timeline_and_schedule ();
  let pieces = Rdk_rays.Packed_pieces.of_geometry
      ~piece_attribute:"class" (geometry ()) |> string_ok in
  if Rdk_rays.Packed_pieces.piece_count pieces <> 2 then
    fail "Packed_pieces did not preserve two connected pieces";
  let plain = Lisp_sop.node ~with_:["src", Lisp_sop.snapshot (geometry ())] "(sop/ext_src)" in
  if Rdk_rays.Packed_pieces.explosion ~operation:(Node.operation plain)
      ~fields:(Node.parameter_fields plain) <> None then
    fail "a node that is not an Exploded View carried an explosion";
  let base = exploded pieces ":amount 0.0" |> positions
  and moved = exploded pieces ":amount 1.0" |> positions in
  if positions (Rdk_rays.Packed_pieces.mesh_for_explosion None pieces) <> base then
    fail "a zero explosion moved the pieces";
  let base_normals = (exploded pieces ":amount 0.0"
      |> Rays.Mesh.View.packed_view).normals |> Option.get in
  if not (Array.for_all (( = ) 0.) base_normals.x
      && Array.for_all (( = ) 1.) base_normals.y
      && Array.for_all (( = ) 0.) base_normals.z) then
    fail "Packed_pieces replaced authored vertex normals with flat faces";
  if Array.length base.x <> 6 then fail "Packed_pieces changed the vertex count";
  for vertex = 0 to Array.length base.x - 1 do
    let expected = if base.x.(vertex) < 0. then -2. else 2. in
    if abs_float ((moved.x.(vertex) -. base.x.(vertex)) -. expected) > 1e-12
    then fail "Packed_pieces explosion was not a rigid center translation";
    if moved.y.(vertex) <> base.y.(vertex)
        || moved.z.(vertex) <> base.z.(vertex) then
      fail "Packed_pieces explosion changed an orthogonal coordinate"
  done;
  (* The per-axis scale multiplies the offset: half as far along X. *)
  let half = exploded pieces ":amount 1.0 :scale [0.5 1.0 1.0]" |> positions in
  Array.iteri (fun vertex x ->
    let expected = if x < 0. then -1. else 1. in
    if abs_float ((half.x.(vertex) -. x) -. expected) > 1e-12 then
      fail "Packed_pieces ignored the explosion's axis scale") base.x;
  let noisy seed = exploded pieces
      (Printf.sprintf ":amount 0.7 :noise_amount 0.4 :noise_frequency 1.3 :noise_seed %d" seed)
    |> positions in
  let noisy_a = noisy 19 and noisy_b = noisy 19 in
  if noisy_a.x <> noisy_b.x || noisy_a.y <> noisy_b.y || noisy_a.z <> noisy_b.z
  then fail "Packed_pieces noise is not deterministic";
  let quiet = exploded pieces ":amount 0.7" |> positions in
  if noisy_a.x = quiet.x && noisy_a.y = quiet.y && noisy_a.z = quiet.z then
    fail "Packed_pieces noise moved nothing";
  let source = Lisp_sop.node {|(sop/box :connectivity "Quads" :normals "None" :size [2.0 2.0 2.0] :consolidate_points true)|} in
  let cutter = Lisp_sop.node {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 1 :rows 1 :size 3.0)|} in
  let fractured = Lisp_sop.node ~with_:["source", (source); "cutter", (cutter)] {|(sop/boolean_fracture (sop/ext_source) (sop/ext_cutter))|} in
  let session = Session.create ~max_entries:16
      ~max_payload_bytes:(16 * 1024 * 1024) |> string_ok in
  let context = Context.create ~seed:0L () |> string_ok in
  let output = match Session.cook session ~context fractured with
    | Ok output -> output
    | Error error -> fail (Diagnostic.error_to_string error) in
  Session.close session;
  let pieces = Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive
      "piece" (Result.get_ok (Sop.Payload.geometry output.payload)) |> Option.get |> Rdk.Attribute.storage in
  (match pieces with
   | Rdk.Attribute.Int values ->
       let maximum = Array.fold_left Int.max (-1) values in
       if maximum <> 1 then
         fail "one cutting plane did not produce exactly two Boolean cells";
       let counts = Array.make 2 0 in
       Array.iter (fun piece -> counts.(piece) <- counts.(piece) + 1) values;
       if counts.(0) <= 1 || counts.(1) <= 1 then
         fail "Boolean fracture assigned individual polygons as pieces";
       let packed = Rdk_rays.Packed_pieces.of_geometry
           ~piece_attribute:"piece" (Result.get_ok (Sop.Payload.geometry output.payload)) |> string_ok in
       if Rdk_rays.Packed_pieces.piece_count packed <> 2 then
         fail "Boolean fracture piece attribute did not pack two rigid shards"
   | _ -> fail "Boolean fracture piece attribute is not integer-valued");
  print_endline "sketch support tests passed"
