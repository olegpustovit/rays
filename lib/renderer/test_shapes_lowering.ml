(* A published Ui_batch reaches the GPU without another copy on the way: lowering draws from
   the batch's own bytes, the index table is written once without a closure per quad and shared by
   a batch that needs all of it, and the upload writes indices and uniforms apart instead of
   joining them. What is left per instance is the 24 bytes of the index table, once. Before, the
   first frame of a batch allocated about 170 bytes an instance here. *)
module Sb = Scene_command.Ui_batch.Builder
module R = Scene_command.Render_ir

let get = function Ok value -> value | Error error ->
  failwith (Format.asprintf "%a" Renderer.pp_error error)

let width = 64 and height = 40
let count = 100_000

let run () =
  let execution = get (Renderer.create_offscreen {Renderer.
    logical_width = width; logical_height = height; drawable_width = width; drawable_height = height;
    title = "shapes-lowering"; vsync = false; high_density = true}) in
  Fun.protect ~finally:(fun () -> ignore (get (Renderer.destroy execution))) (fun () ->
    let positions = Array.init (2 * count) (fun k ->
      if k mod 2 = 0 then float (k / 2 mod width) +. 0.5 else float (k / 2 / width mod height) +. 0.5) in
    let builder = Sb.create ~capacity:count () in
    Sb.circles builder ~count ~position:(positions, 2) ~radius:([|0.6|], 0)
      ~fill:(Test_marks_native.rgba 118 186 202 255) ~stroke:0l ~stroke_width:([|1.|], 0);
    let ir = match R.create_owned [|R.Set_blend Alpha; R.Shapes (Sb.publish builder)|] with
      | Ok ir -> ir | Error _ -> failwith "the batch is not a scene" in
    let frame () = Test_ui_pipeline.capture ~clear:(0., 0., 0., 1.) execution (fun submission ->
      Renderer.Native.lower_scene2 submission ~density:1 ~resource:(fun _ -> None) ir) in
    let before = Gc.allocated_bytes () in
    let pixels = frame () in
    let first = (Gc.allocated_bytes () -. before) /. float count in
    let before = Gc.allocated_bytes () in
    let again = frame () in
    let second = (Gc.allocated_bytes () -. before) /. float count in
    let painted = ref 0 in
    Bytes.iteri (fun i v -> if i mod 4 = 1 && v <> '\000' then incr painted) pixels;
    assert (!painted = width * height && Bytes.equal pixels again);
    if first > 32. then
      failwith (Printf.sprintf "the first frame of a %d-instance batch allocated %.1f bytes an instance" count first);
    if second > 1. then
      failwith (Printf.sprintf "an unchanged %d-instance batch allocated %.1f bytes an instance again" count second);
    Printf.printf "Shapes lowering: a %d-instance batch allocates %.1f bytes an instance on its first frame, %.2f after\n"
      count first second)
