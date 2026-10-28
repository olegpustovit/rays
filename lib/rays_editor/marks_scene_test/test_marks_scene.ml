(* Marks_scene against the scenes the per-kind drawing arms built: the SHA-256
   of each draw graph's serialized command stream is the checked-in digest those arms produced. *)
open Rays
module E = Flow.Eval
module M = Scene_command.Marks
module Buffers = Flow_ir.Core_exec.Buffers
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let checked body =
  let forms = ok (Flow.Syntax.parse ("(workspace w (graph g :context draw " ^ body ^ "))")) in
  match Flow.Workspace.check {Flow.Catalog.version=1;kinds=[]} forms with
  | Some ws,[] -> ws | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let serialize scene = Scene_command.Render_ir.serialize (Result.get_ok (Scene.Private.to_ir scene))
let array3 = "(map (fn [i] [(* i 7.5) (+ 3 (* (* i t) 9)) 0]) (array/range 5))"
let pictures = [
  "(draw/background \"#14161c\")";
  "(draw/merge (draw/background \"#102030\") (draw/rect [1 2 0] [30 40 0]) (draw/rect [1.5 2 0] [3 4 0] :stroke \"#fff8\") (draw/rect [0 0 0] [5 5 0] :fill \"#f00\" :stroke \"#00f\"))";
  "(draw/circle [10.5 20 0] 4)";
  "(draw/circle [10 20 0] (+ 4 (* 2 (sin t))) :fill \"#5aaaf080\" :stroke \"#000\")";
  "(draw/line [0 0 0] [5 5.5 0])";
  "(draw/line [0 0 0] [5 5 0] :color \"#0f0\" :width 3)";
  "(draw/point [3.9 -4.9 0] :color \"#abc\")";
  "(draw/translate [1.9 -2.5 0] (draw/merge (draw/point [0 0 0]) (draw/rotate (* t 40) (draw/scale 2 (draw/point [1 1 0])))))";
  "(draw/rotate (* t 4) (draw/translate [10 10 0] (draw/scale [2 3 0] (draw/rect [0 0 0] [4 4 0]))))";
  "(draw/scale [2 3 0] (draw/background \"#000\"))";
  "(draw/circles " ^ array3 ^ ")";
  "(let* [p " ^ array3 ^ " r (map (fn [i] (* i 0.5)) (array/range 5))] (draw/circles p :radius r :stroke \"#fff\"))";
  "(let* [p " ^ array3 ^ " c (map (fn [i] [(/ i 5) 0.5 0.25]) (array/range 5))] (draw/circles p :radius 3 :fill c))";
  "(let* [p " ^ array3 ^ "] (draw/rects p [10 10 0] :fill \"#f00\"))";
  "(let* [p " ^ array3 ^ " c (map (fn [i] [(/ i 5) 0 0]) (array/range 5)) s (map (fn [i] [i 1 0]) (array/range 5))] (draw/rects p s :fill c :stroke \"#123\"))";
  "(let* [p " ^ array3 ^ "] (draw/lines p (map (fn [q] [q.y q.x 0]) p) :width 2))";
  "(let* [p " ^ array3 ^ " c (map (fn [i] [(/ i 5) 0 1]) (array/range 5))] (draw/lines p p :color c))";
  "(let* [p " ^ array3 ^ "] (draw/merge (draw/points p :color \"#1e1e1e\") (draw/points p)))";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 0))] (draw/merge (draw/circles p) (draw/rects p [1 1 0]) (draw/points p) (draw/lines p p)))";
  "(draw/rounded_rect [4 5 0] [60 40 0] :radius 8 :fill \"#123\" :stroke \"#fff\")";
  "(draw/rounded_rect [4 5 0] [60 40 0])";
  "(draw/translate [3 4 0] (draw/rounded_rect [4 5 0] [-60 40 0] :radius 80 :stroke \"#f0f\"))";
  "(draw/polygon (map (fn [i] [i (* i i) 0]) (array/range 4)) :stroke \"#fff\")";
  "(draw/polygon (map (fn [i] [i (* i i) 0]) (array/range 4)))";
  "(draw/polyline (map (fn [i] [i (* i i) 0]) (array/range 4)) :color \"#f0f\")";
  "(draw/merge (draw/text [1 2 0] \"\") (draw/circle [5 5 0] 2 :fill \"#fff\"))";
]
let size = 200, 100
let live = {(Input.at_time 0.25) with size}
let digests = [
  "f4ff989c22afb8f52ab260b54d17bb7891e07ab75f29b2de12f487f832f3d92a";
  "38167b07c39a01579645ef0d43fd6ec8c1b524edc07e49436d9754eecbb11d2c";
  "66494d0f3ffb1159cdf6fb629fbd8105e690fa96d3f4956f4edf8b52e7eba59f";
  "c3ffa1398d4a79818e0afa089e1131a048b20881dbf0858482fa7f0c28bfcffd";
  "23a418fc491c5d3a95aa3029d2113bf002423a4a78e65dfc8a71c79818b8c89b";
  "3b93ccef6bb7daa17148022b6a0fdac1b409ce7c2ad6a370c4797151e146a5d1";
  "988dd6e539be91ba2f51ced75a01ac4deeaa2480a60aaa3a9c3039e06cf369eb";
  "1113c42cc2148b7eff360ec11fbdf7cd1735930475d0ccef8429d6a75d0118a5";
  "68ecc6c3903328f5dad628fb39ab2fdcbbd03b55ce2caac17cfd4f3b52693483";
  "71f685a283353a1f6081e6fa92ec9ad61674563f3ef8cf8d9e8309814bbef0f6";
  "14a723a3cffd08ceec72997d010416750af9e001bf15283d13d495d083f2faa2";
  "c0bcc14d631dc745aa86c73d3fff98fce242b956f3e8924e187d20d0267dcff6";
  "d175863740935e5233b0a46f97e47a31a8233830684c736b444fe95c1b666401";
  "5f1de8f73c12b33352bb627731d0c3cd347c07c818278f41d81fefd93bdbba37";
  "34a94c9916d7321808a516723e5d5b0d51ddaedbf6671ee96293b6e58b9fcd4a";
  "e560c7f427e139fcaf098f3993ff6ba3b3ac536391b2b8e504f56da2d52b4654";
  "53eee607276c67a0280cb9fcbb59e5ebc81e0f6502a82077c0c2bdc7d7323c8b";
  "4622e6a3ff25f8cc3c389f231513adc2a69e0bf11f4db96d6b072e8db860beb8";
  "6b3831072362e57f0f8b90919a3f922911b46d54d87b1eaed1d45b82d3033a33";
  "27bae8d4612c790aac0527df20a81bc4f0f3d21d22f51e0e03834b948f5f89a9";
  "447a8178eb85cb3be6245c4d0ba5e559ef876fb7495596970eaa09f8b9b5fadb";
  "5de2d001b43fb50d2243d6b231c156f71c35b3bfe2f673e6dc357293d6c1748a";
  "db2db290db2f7d80a744b3625af71f1706e44b1d4d565f3d6373693427a6dbb8";
  "b967bc0170e57ab1d5239e87988c193dc731db361c815ff8edfd089fe9526c53";
  "382cc67de39df737faadf6e1e2d9927794967f6bfbe995cc1ec36e7d6401f90f";
  "bc9a96cc00caf757afb46c0b94ab6e514f8676ad396541d50171b303b82e9c73";
]
let new_scene body =
  let evaluation = ok (E.static (checked body)) in
  let program, root = Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results)) in
  let buffers = Buffers.create () in
  let marks = ok (Flow_ir.Core_exec.draw ?root (Flow_ir.Core_exec.prepare program) ~live ~size ~buffers) in
  ok (Flow_native.Marks_scene.scene ~buffers marks)
let () =
  let point = new_scene "(draw/point [3.25 -4.75 0] :color \"#abc\")" in
  let point_geometry = Scene_command.Shape2.polygon
      [3.25, -4.75; 4.25, -4.75; 4.25, -3.75; 3.25, -3.75]
      ~fill:(Some 0xaabbccffl) ~stroke:None
    |> Array.to_list |> List.map Scene.Private.geometry in
  assert (Bytes.equal (serialize point) (serialize point_geometry));
  let points = new_scene
      "(draw/points (map (fn [i] [(+ 1.25 (* i 2.25)) (+ -2.75 (* i 3.5)) 0]) (array/range 2)) :color \"#def\")" in
  let batch = Scene_command.Ui_batch.Builder.(let b = create ~capacity:2 () in
    shape_rect b ~x:1.25 ~y:(-2.75) ~width:1. ~height:1. ~fill:0xddeeffffl ~stroke:0l ();
    shape_rect b ~x:3.5 ~y:0.75 ~width:1. ~height:1. ~fill:0xddeeffffl ~stroke:0l ();
    publish b) in
  assert (Bytes.equal (serialize points) (serialize [Scene.Private.shapes batch]))
let () =
  assert (List.length digests = List.length pictures);
  List.iteri (fun i body ->
    let digest = Editor_document.Lowering.sha256 (Bytes.to_string (serialize (new_scene body))) in
    if digest <> List.nth digests i then failwith (Printf.sprintf "picture %d differs: %s (expected %s, got %s)" i body (List.nth digests i) digest))
    pictures;
  (* Hand-built marks against the Scene API. *)
  let none = Buffers.create () in
  let circle = M.Circle {position = Constant (1.5, 2.); radius = Constant 3.; fill = Constant 0xff0000ffl;
    stroke = Constant 0l; stroke_width = Constant 1.} in
  let angle = 10. in
  let marks = [|
    M.Mark_set {count = 1; kind = circle};
    Push_transform {xx = cos angle; xy = sin angle; yx = -. sin angle; yy = cos angle; tx = 0.; ty = 0.};
    Mark_set {count = 0; kind = Rect {position = Constant (0., 0.); size = Constant (1., 1.);
      fill = Constant 0l; stroke = Constant 0l; stroke_width = Constant 1.}};
    Pop_transform |] in
  let batch = Scene_command.Ui_batch.Builder.(let b = create () in
    circle b ~x:1.5 ~y:2. ~radius:3. ~fill:0xff0000ffl ~stroke:0l (); publish b) in
  let empty = Scene_command.Ui_batch.Builder.(publish (create ())) in
  let expected = [Scene.Private.shapes batch; Scene.rotate angle [Scene.Private.shapes empty]] in
  assert (Bytes.equal (serialize expected) (serialize (ok (Flow_native.Marks_scene.scene ~buffers:none marks))));
  (* A mark the contract rejects is an error, and so is an image without a resolver. *)
  assert (Result.is_error (Flow_native.Marks_scene.scene ~buffers:none [|M.Pop_transform|]));
  (match Flow_native.Marks_scene.scene ~buffers:none [|M.Image {resource = 1; x = 0; y = 0; scale = 1.; angle = 0.}|] with
   | Error d -> assert (d.code = "E_IMAGE") | Ok _ -> assert false);
  Printf.printf "Marks_scene: %d draw graphs serialize to the digests of the removed arms; hand-built marks match\n"
    (List.length pictures)

(* A static batch under a moving translate is kept: a frame allocates the same at 10,000 and 100,000
   instances, and the kept batch draws what a fresh drawing draws. *)
let () =
  let size = (800, 600) in
  let prepare n =
    let source = Printf.sprintf "(workspace w (graph g :context draw (draw/translate [(* 10 (sin t)) 0 0] (draw/rects (map (fn [i] [(* (mod i 800) 1.0) (/ i 1250) 0]) (array/range %d)) [3 3 0] :fill \"#5aaaf0\"))))" n in
    let evaluation = ok (E.static (match Flow.Workspace.check {Flow.Catalog.version=0;kinds=[]} (ok (Flow.Syntax.parse source)) with
      | Some ws,[] -> ws | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds)))) in
    ok (Flow_native.Drawing.prepare ~states:evaluation.states evaluation.plan (List.assoc "g" evaluation.results)) in
  let frame prepared state i = ok (Flow_native.Drawing.render_prepared ~state prepared
    ~live:{(Input.at_time (float i /. 60.)) with frame = i; tick = i; size} ~size) in
  let allocated n =
    let prepared = prepare n and state = E.create_state () in
    ignore (frame prepared state 0); ignore (frame prepared state 1);
    let before = Gc.allocated_bytes () in
    ignore (frame prepared state 2);
    Gc.allocated_bytes () -. before in
  let small = allocated 10_000 and large = allocated 100_000 in
  if large > small +. 100_000. then
    failwith (Printf.sprintf "a moving translate over a static batch allocates with the count: %.0f bytes at 10,000, %.0f at 100,000" small large);
  let kept = prepare 2_000 and state = E.create_state () in
  ignore (frame kept state 0);
  assert (Bytes.equal (serialize (frame kept state 3)) (serialize (frame (prepare 2_000) (E.create_state ()) 3)));
  Printf.printf "Retention: a moving translate over a static batch allocates %.0f bytes at 10,000 instances and %.0f at 100,000\n" small large

(* A mark set with a device attribute is registered with the GPU host, and a nonfinite or over-large host
   coordinate in it is refused before it is (the GPU route's counterpart of Ui_batch.Builder). *)
let () =
  let device = {Flow_ir.Gpu.identity = 1; count = 2; width = 1; output = 0; stamp = 1L; gpu_seconds = None} in
  let registered = ref 0 in
  let gpu ~slot:_ ~kind:_ ~count:_ _ =
    incr registered; Ok (Scene_command.Marks.gpu_token ~identity:1 ~count:2 ~stamp:1L) in
  let scene x =
    let evaluation = ok (E.static (checked (Printf.sprintf
      "(draw/circles (map (fn [i] [(+ %s i) 0 0]) (array/range 2)) :radius (map (fn [i] 1.0) (array/range 2)))" x))) in
    let program, _ = Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results)) in
    let buffers = Buffers.create () in
    let marks = ok (Flow_ir.Core_exec.draw (Flow_ir.Core_exec.prepare program) ~live:(Input.at_time 0.)
      ~size:(64, 64) ~buffers) in
    (* The radius array stands for a GPU region's output. *)
    let marks = Array.map (function
      | M.Mark_set {count; kind = Circle ({radius = Buffer b; _} as c)} ->
          M.Mark_set {count; kind = Circle {c with radius = Device b}}
      | entry -> entry) marks in
    Flow_native.Marks_scene.scene ~buffers ~gpu ~device:(fun _ -> device) marks in
  ignore (ok (scene "1.0"));
  assert (!registered = 1);
  List.iter (fun bad -> match scene bad with
    | Error d -> assert (d.code = "E_DRAW_RANGE")
    | Ok _ -> failwith "an over-large host coordinate reached the GPU route") ["3.5e38"; "1e300"];
  assert (!registered = 1);
  print_endline "Marks_scene: a device mark set registers once; over-large host floats are E_DRAW_RANGE"

(* A set with constant colours is written by the builder from the arrays; one with a colour array
   goes mark by mark. Both give the same batch and the same refusal: here every colour array
   holds the constant's colour, so the mark-by-mark path is the oracle. *)
let () =
  let result body =
    let evaluation = ok (E.static (checked body)) in
    let program, root = Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results)) in
    let buffers = Buffers.create () in
    match Flow_ir.Core_exec.draw ?root (Flow_ir.Core_exec.prepare program) ~live ~size ~buffers with
    | Error d -> Error (d.code, d.message)
    | Ok marks -> (match Flow_native.Marks_scene.scene ~buffers marks with
        | Ok scene -> Ok (serialize scene) | Error d -> Error (d.code, d.message)) in
  let n = 9 in
  let range = Printf.sprintf "(array/range %d)" n in
  let colours = Printf.sprintf "(map (fn [i] [1 0.5 0.25]) %s)" range and colour = "\"#ff8040\"" in
  let p3 = Printf.sprintf "(map (fn [i] [(* i 7.3) (- 3.1 (* i 0.7)) 5]) %s)" range in
  let q3 = Printf.sprintf "(map (fn [i] [(+ 2.2 (* i 1.9)) (* (* i i) 0.3) 0]) %s)" range in
  let floats bad = Printf.sprintf "(map (fn [i] (if (= i 6) %s (+ 0.4 (* i 0.3)))) %s)" bad range in
  let sizes bad = Printf.sprintf "(map (fn [i] [(if (= i 4) %s (+ 1.1 i)) 2.3 0]) %s)" bad range in
  let far bad = Printf.sprintf "(map (fn [i] [(if (= i 2) %s (* i 7.3)) 1.7 0]) %s)" bad range in
  (* [make fill] is one drawing with its colours spelled [fill]: the constant, then the array. *)
  let cases = [
    "circles, constant radius", (fun c -> Printf.sprintf "(draw/circles %s :radius 2.5 :fill %s)" p3 c);
    "circles, radius array", (fun c -> Printf.sprintf "(draw/circles %s :radius %s :fill %s)" p3 (floats "1.6") c);
    "circles, stroke", (fun c -> Printf.sprintf "(draw/circles %s :radius %s :stroke %s)" p3 (floats "1.6") c);
    "circles, a negative radius", (fun c -> Printf.sprintf "(draw/circles %s :radius %s :fill %s)" p3 (floats "-0.5") c);
    "circles, a coordinate past binary32", (fun c -> Printf.sprintf "(draw/circles %s :radius 1 :fill %s)" (far "1e300") c);
    "rects, constant size", (fun c -> Printf.sprintf "(draw/rects %s [4.5 3.25 0] :fill %s)" p3 c);
    "rects, size array", (fun c -> Printf.sprintf "(draw/rects %s %s :stroke %s)" p3 (sizes "0.75") c);
    "rects, a negative size", (fun c -> Printf.sprintf "(draw/rects %s %s :fill %s)" p3 (sizes "-1") c);
    "rects, a corner past binary32", (fun c -> Printf.sprintf "(draw/rects %s [1 1 0] :fill %s)" (far "3.5e38") c);
    "lines", (fun c -> Printf.sprintf "(draw/lines %s %s :color %s :width 1.5)" p3 q3 c);
    "lines, an end past binary32", (fun c -> Printf.sprintf "(draw/lines %s %s :color %s)" p3 (far "-1e300") c);
  ] in
  let refused = ref 0 in
  List.iter (fun (name, make) ->
    let whole = result (make colour) and each = result (make colours) in
    (match whole, each with
     | Ok a, Ok b -> if not (Bytes.equal a b) then failwith (name ^ ": the whole-set builder wrote other bytes")
     | Error a, Error b ->
         incr refused;
         if a <> b then failwith (Printf.sprintf "%s: refused with %s %s, mark by mark with %s %s" name (fst a) (snd a) (fst b) (snd b));
         (* the builder's own refusal, so the set did reach it *)
         if fst a <> "E_DRAW_RANGE" || not (String.starts_with ~prefix:"Ui_batch" (snd a)) then
           failwith (Printf.sprintf "%s: refused with %s %s" name (fst a) (snd a))
     | Ok _, Error (code, _) -> failwith (name ^ ": accepted a set that mark by mark is " ^ code)
     | Error (code, _), Ok _ -> failwith (name ^ ": refused (" ^ code ^ ") a set that mark by mark is drawn"))) cases;
  assert (!refused = 5);
  Printf.printf "Marks_scene: %d constant-colour sets equal their mark-by-mark batches, %d of them refusals\n"
    (List.length cases) !refused

(* A static set of circles costs its 64-byte instances once: the builder reads the arrays (no
   tuple or boxed float per mark) and the batch takes the builder's storage (no second copy).
   Before, a mark cost about 330 bytes here. *)
let () =
  let n = 100_000 in
  let source = Printf.sprintf "(draw/circles (map (fn [i] [(* i 0.001) (* i 0.002) 0]) (array/range %d)) :radius 0.6 :fill \"#76baca\")" n in
  let evaluation = ok (Flow_ir.static (checked source)) in
  let program, root = Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results)) in
  let buffers = Buffers.create () in
  let marks = ok (Flow_ir.Core_exec.draw ?root (Flow_ir.Core_exec.prepare program) ~live ~size ~buffers) in
  let before = Gc.allocated_bytes () in
  let scene = ok (Flow_native.Marks_scene.scene ~buffers marks) in
  let per_mark = (Gc.allocated_bytes () -. before) /. float n in
  (match Rays.Scene.Private.commands scene with
   | [| Scene_command.Render_ir.Shapes batch |] -> assert (Scene_command.Ui_batch.count batch = n)
   | _ -> failwith "a circle set is one Shapes command");
  if per_mark > 72. then failwith (Printf.sprintf "a static circle set allocates %.0f bytes a mark, over its 64-byte instance" per_mark);
  Printf.printf "Marks_scene: a static set of %d circles allocates %.1f bytes a mark\n" n per_mark

(* The cache is bounded: a frame with more mark sets than it holds keeps the most recent. *)
let () =
  let sets = Array.init 3000 (fun i ->
    M.Mark_set {count = 1; kind = Point {position = Constant (float i, 0.); fill = 0l}}) in
  let cache = Flow_native.Marks_scene.cache () in
  ignore (ok (Flow_native.Marks_scene.scene ~buffers:(Buffers.create ()) ~cache sets));
  let size = Flow_native.Marks_scene.cache_size cache in
  if size > 1024 then failwith (Printf.sprintf "the node cache grew to %d entries" size);
  Printf.printf "Marks_scene: 3000 mark sets leave %d cached nodes\n" size
