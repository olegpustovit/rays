(* Neighbouring mark sets with one family, blend, clip and transform become one draw, painter order
   kept: a run of more than 64 CPU sets is one draw (shorter runs keep their own, by design: a
   changing member must not re-upload its neighbours), and a set drawn by the Marks family never
   merges with a Ui_batch set but keeps its place in the order. Pixels equal one big batch. *)
module B = Ogpu.Backend
module Sb = Scene_command.Ui_batch.Builder
module R = Scene_command.Render_ir

let get = function Ok value -> value | Error error ->
  failwith (Format.asprintf "%a" Renderer.pp_error error)

let width = 64 and height = 40
let per_set = 3
let word s = Test_marks_native.rgba (40 + s * 37) (200 - s * 23) (s * 71) 255
let centre s j = float (8 + (s * 13 + j * 9) mod 48), float (8 + (s * 7 + j * 5) mod 24)
let radius = 5.

let cpu_set s =
  let b = Sb.create () in
  for j = 0 to per_set - 1 do
    let x, y = centre s j in
    Sb.circle b ~x ~y ~radius ~fill:(word s) ~stroke:0l ~stroke_width:1. ()
  done;
  Sb.publish b

let all_sets sets =
  let b = Sb.create () in
  List.iter (fun s -> for j = 0 to per_set - 1 do
    let x, y = centre s j in
    Sb.circle b ~x ~y ~radius ~fill:(word s) ~stroke:0l ~stroke_width:1. ()
  done) sets;
  Sb.publish b

let floats a = Test_marks_native.floats a
let marks_token gpu s =
  let sink = get (Renderer.Private.create_gpu_marks gpu) in
  let position = floats (Array.init (3 * per_set) (fun k ->
    let x, y = centre s (k / 3) in match k mod 3 with 0 -> x | 1 -> y | _ -> 0.)) in
  let token = get (Renderer.Private.gpu_marks sink ~kind:4 ~count:per_set
    [| Host (position, 3); Host (floats [| radius |], 0); Host (Test_marks_native.words [| word s |], 0);
       Host (Test_marks_native.words [| 0l |], 0); Host (floats [| 1. |], 0) |]) in
  sink, token

let run () =
  let live_handles = Ogpu.Impl.live_handles in
  let baseline = live_handles () in
  let execution = get (Renderer.create_offscreen {Renderer.
    logical_width = width; logical_height = height; drawable_width = width; drawable_height = height;
    title = "batch-merge"; vsync = false; high_density = true}) in
  let gpu = get (Renderer.acquire_gpu ()) in
  Fun.protect ~finally:(fun () -> Renderer.release_gpu gpu;
      ignore (get (Renderer.destroy execution))) (fun () ->
    let clear = (0.125, 0.25, 0.375, 1.) in
    let capture commands =
      let ir = Result.get_ok (R.create (Array.of_list (R.Set_blend Alpha :: commands))) in
      let encoded = ref 0 in
      let pixels = Test_ui_pipeline.capture ~clear execution (fun submission ->
        let batch = Renderer.Private.lower_scene2 submission ~density:1 ~resource:(fun _ -> None) ir in
        Result.iter (fun batch -> encoded := Renderer.Private.encoded_draws_for_test batch) batch; batch) in
      pixels, !encoded in
    let check name sets shapes expected_draws =
      let reference, one = capture [R.Shapes (all_sets sets)] in
      let pixels, count = capture shapes in
      let painted = ref 0 in
      Bytes.iteri (fun i v -> if i mod 4 <> 3 && v <> '\000' then incr painted) reference;
      assert (!painted > 200);
      if pixels <> reference then failwith (name ^ ": pixels differ from one batch");
      if one <> 1 || count <> expected_draws then
        failwith (Printf.sprintf "%s: %d draws for the sets, %d for the one batch; expected %d and 1" name count one expected_draws) in
    let range n = List.init n Fun.id in
    check "3 CPU sets" (range 3) (List.map (fun s -> R.Shapes (cpu_set s)) (range 3)) 3;
    check "100 CPU sets" (range 100) (List.map (fun s -> R.Shapes (cpu_set s)) (range 100)) 1;
    (* Alternate Ui_batch and Marks sets: three families runs never merge, order is kept. *)
    let sources = List.map (fun s -> if s mod 2 = 0 then None else Some (marks_token gpu s)) (range 6) in
    Fun.protect ~finally:(fun () -> List.iter (Option.iter (fun (sink, _) ->
        Renderer.Private.close_gpu_marks sink)) sources) (fun () ->
      let shapes = List.mapi (fun s -> function
        | None -> R.Shapes (cpu_set s)
        | Some (_, token) -> R.Shapes (Scene_command.Ui_batch.Private.of_marks token)) sources in
      check "interleaved Ui_batch and Marks sets" (range 6) shapes 6);
    (* A clip between two sets keeps them apart; the same clip around both keeps them together. *)
    let clip = R.Push_clip {x = 4.; y = 4.; width = 50.; height = 30.} in
    let apart = [R.Shapes (cpu_set 0); clip; R.Shapes (cpu_set 1); R.Pop_clip; R.Shapes (cpu_set 2)] in
    let reference, _ = capture [R.Shapes (cpu_set 0); clip; R.Shapes (cpu_set 1); R.Pop_clip; R.Shapes (cpu_set 2)] in
    let pixels, count = capture apart in
    assert (pixels = reference && count = 3));
  assert (Scene_execution.Private.gpu_vertex_count_for_test
    (Renderer.Private.gpu_vertices_owner execution) = 0);
  if live_handles () <> baseline then failwith "batch merge handle delta";
  print_endline "Batch merging: more than 64 neighbouring CPU sets are one draw, Marks sets keep their place, a clip separates, pixels equal one batch"
