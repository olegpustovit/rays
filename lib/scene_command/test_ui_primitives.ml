(* A published batch is validated once: a scene that holds the same batch again (a moved transform
   over unchanged marks) must not scan its instances, which allocated about 230 bytes per instance. *)
open Scene_command

let () =
  let fractional = Shape2.polygon
      [0.25, 0.5; 3.25, 0.5; 3.25, 2.75; 0.25, 2.75]
      ~fill:(Some 0xffffffffl) ~stroke:None in
  assert (Array.exists ((=) 0.25) fractional.(0).vertices);
  print_endline "Shape2 preserves subpixel vertices"

let () =
  let n = 20_000 in
  let builder = Ui_batch.Builder.create ~capacity:n () in
  for i = 0 to n - 1 do
    Ui_batch.Builder.circle builder ~x:(float i) ~y:1. ~radius:0.5 ()
  done;
  let batch = Ui_batch.Builder.publish builder in
  let scene dx = [| Render_ir.Push_transform {xx = 1.; xy = 0.; yx = 0.; yy = 1.; tx = dx; ty = 0.};
    Render_ir.Shapes batch; Render_ir.Pop_transform |] in
  let create dx = match Render_ir.create_owned (scene dx) with
    | Ok _ -> () | Error _ -> assert false in
  let first_before = Gc.allocated_bytes () in
  create 0.;
  let first_scene = Gc.allocated_bytes () -. first_before in
  if first_scene > 4096. then
    failwith (Printf.sprintf "first scene validation allocated %.0f bytes for a published batch" first_scene);
  let before = Gc.allocated_bytes () in
  for i = 1 to 10 do create (float i) done;
  let per_scene = (Gc.allocated_bytes () -. before) /. 10. in
  assert (per_scene < 4096.);
  (* a tampered copy is still refused: the mark belongs to the batch that passed, not to its bytes *)
  let good = Ui_batch.Builder.publish builder in
  assert (Ui_batch.valid_shapes good);
  let reject_copy name offset value =
    let bad = Ui_batch.copy good in
    Bytes.set_int32_le (Ui_batch.instances bad) offset value;
    if Ui_batch.valid_shapes bad then failwith (name ^ " packed copy accepted")
  in
  reject_copy "nonfinite coordinate" 0 0x7fc00000l;
  reject_copy "invalid shape tag" 40 1l;
  reject_copy "negative width" 48 (Int32.bits_of_float (-1.));
  let rect_builder = Ui_batch.Builder.create () in
  Ui_batch.Builder.shape_rect rect_builder ~x:1. ~y:1. ~width:2. ~height:2.
    ~fill:0xffffffffl ~stroke:0l ();
  let rect = Ui_batch.Builder.publish rect_builder in
  let bad_rect = Ui_batch.copy rect in
  Bytes.set_int32_le (Ui_batch.instances bad_rect) 8 (Int32.bits_of_float 0.);
  assert (not (Ui_batch.valid_shapes bad_rect));
  print_endline "Ui_batch validation is remembered per published batch"

(* A builder filled to its capacity publishes its own storage (no second copy of every instance),
   and a published batch never changes: the next append moves the builder to new storage. A
   builder with room left publishes a copy, as before. *)
let () =
  let n = 20_000 in
  let fill builder count = for i = 0 to count - 1 do
      Ui_batch.Builder.circle builder ~x:(float i) ~y:1. ~radius:0.5 () done in
  let full = Ui_batch.Builder.create ~capacity:n () in
  fill full n;
  let before = Gc.allocated_bytes () in
  let batch = Ui_batch.Builder.publish full in
  let allocated = Gc.allocated_bytes () -. before in
  if allocated > 1024. then failwith (Printf.sprintf "publishing a full builder allocated %.0f bytes" allocated);
  let snapshot = Bytes.copy (Ui_batch.instances batch) in
  fill full 3;
  assert (Ui_batch.count batch = n && Bytes.equal snapshot (Ui_batch.instances batch));
  let again = Ui_batch.Builder.publish full in
  assert (Ui_batch.count again = n + 3);
  assert (Bytes.equal snapshot (Bytes.sub (Ui_batch.instances again) 0 (n * Ui_batch.instance_bytes)));
  let roomy = Ui_batch.Builder.create ~capacity:(n + 1) () in
  fill roomy n;
  let partial = Ui_batch.Builder.publish roomy in
  assert (Bytes.equal snapshot (Ui_batch.instances partial));
  fill roomy 1;
  assert (Ui_batch.count partial = n && Bytes.equal snapshot (Ui_batch.instances partial));
  print_endline "Ui_batch: a full builder publishes its storage; published batches do not change"

(* The whole-set writers give the bytes and the failure of one call an instance, for constants
   (stride 0) and packed arrays of two or three floats, and allocate nothing per instance. *)
let () =
  let module B = Ui_batch.Builder in
  let n = 257 in
  let v3 = Array.init (3 * n) (fun k -> float (k mod 97) *. 1.37 -. 20.) in
  let v2 = Array.init (2 * n) (fun k -> float (k mod 89) *. 0.83 +. 0.25) in
  let s1 = Array.init n (fun k -> float (k mod 7) *. 0.45) in
  let at (data, stride) i k = data.(stride * i + k) in
  (* the refusal, if any, and every instance appended before it *)
  let outcome build =
    let b = B.create () in
    let refused = match build b with () -> None | exception Invalid_argument message -> Some message in
    refused, Bytes.to_string (Ui_batch.instances (B.publish b)) in
  let same name whole each = if outcome whole <> outcome each then failwith (name ^ ": whole set and one by one differ") in
  let positions = [v3, 3; v2, 2; [|4.5; -2.25|], 0] and scalars = [s1, 1; [|1.75|], 0] in
  List.iter (fun position -> List.iter (fun radius -> List.iter (fun stroke_width ->
    List.iter (fun (fill, stroke) ->
      same "circles" (fun b -> B.circles b ~count:n ~position ~radius ~fill ~stroke ~stroke_width)
        (fun b -> for i = 0 to n - 1 do
            B.circle b ~x:(at position i 0) ~y:(at position i 1) ~radius:(at radius i 0) ~fill ~stroke
              ~stroke_width:(at stroke_width i 0) () done))
      [0x76bacaffl, 0l; 0l, 0xffffffffl; 0x10203040l, 0x0000ff80l]) scalars) scalars;
    List.iter (fun size -> List.iter (fun (fill, stroke) ->
      same "rects" (fun b -> B.rects b ~count:n ~position ~size ~fill ~stroke)
        (fun b -> for i = 0 to n - 1 do
            B.shape_rect b ~x:(at position i 0) ~y:(at position i 1) ~width:(at size i 0) ~height:(at size i 1)
              ~fill ~stroke () done))
      [0x76bacaffl, 0l; 0l, 0xffffffffl]) [v2, 2; (Array.map Float.abs v3), 3; [|3.5; 0.|], 0];
    List.iter (fun to_ -> List.iter (fun width ->
      same "lines" (fun b -> B.lines b ~count:n ~from:position ~to_ ~color:0xff8040ffl ~width)
        (fun b -> for i = 0 to n - 1 do
            B.line b ~x0:(at position i 0) ~y0:(at position i 1) ~x1:(at to_ i 0) ~y1:(at to_ i 1)
              ~color:0xff8040ffl ~width:(at width i 0) done)) scalars) positions) positions;
  (* the same refusal at the same instance *)
  let spoiled data k value = let copy = Array.copy data in copy.(k) <- value; copy in
  let refusal name expected whole each =
    same name whole each;
    match fst (outcome whole) with
    | Some message when message = expected -> ()
    | Some message -> failwith (name ^ ": refused with " ^ message)
    | None -> failwith (name ^ ": accepted") in
  List.iter (fun (name, expected, position, radius, stroke_width) ->
    refusal name expected
      (fun b -> B.circles b ~count:n ~position ~radius ~fill:1l ~stroke:1l ~stroke_width)
      (fun b -> for i = 0 to n - 1 do
          B.circle b ~x:(at position i 0) ~y:(at position i 1) ~radius:(at radius i 0) ~fill:1l ~stroke:1l
            ~stroke_width:(at stroke_width i 0) () done))
    [ "a negative radius", "Ui_batch radius", (v3, 3), (spoiled s1 200 (-1.), 1), ([|1.|], 0);
      "a nonfinite radius", "Ui_batch radius", (v3, 3), (spoiled s1 3 Float.nan, 1), ([|1.|], 0);
      "a finite coordinate past binary32", "Ui_batch: invalid coordinates",
        (spoiled v3 300 3.5e38, 3), (s1, 1), ([|1.|], 0);
      "a negative stroke width", "Ui_batch: invalid coordinates", (v3, 3), (s1, 1), (spoiled s1 100 (-2.), 1);
      "a short array", "index out of bounds", (Array.sub v3 0 30, 3), (s1, 1), ([|1.|], 0) ];
  let sizes = spoiled v2 41 (-0.5) in
  refusal "a negative extent" "Ui_batch extent"
    (fun b -> B.rects b ~count:n ~position:(v3, 3) ~size:(sizes, 2) ~fill:1l ~stroke:0l)
    (fun b -> for i = 0 to n - 1 do
        B.shape_rect b ~x:v3.(3 * i) ~y:v3.(3 * i + 1) ~width:sizes.(2 * i) ~height:sizes.(2 * i + 1)
          ~fill:1l ~stroke:0l () done);
  let count = 50_000 in
  let big = Array.init (3 * count) (fun k -> float (k mod 1000)) in
  let b = B.create ~capacity:count () in
  let before = Gc.allocated_bytes () in
  B.circles b ~count ~position:(big, 3) ~radius:([|0.6|], 0) ~fill:0x76bacaffl ~stroke:0l ~stroke_width:([|1.|], 0);
  B.rects (B.create ~capacity:1 ()) ~count:0 ~position:(big, 3) ~size:([|1.; 1.|], 0) ~fill:0l ~stroke:0l;
  let allocated = Gc.allocated_bytes () -. before in
  if allocated > 4096. then
    failwith (Printf.sprintf "writing %d circles from arrays allocated %.0f bytes" count allocated);
  print_endline "Ui_batch: whole-set writers equal one call an instance, refusals too, and allocate nothing an instance"
