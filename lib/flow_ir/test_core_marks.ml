module E = Flow.Eval
module M = Scene_command.Marks
module Buffers = Flow_ir.Core_exec.Buffers
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let checked body =
  let forms = ok (Flow.Syntax.parse ("(workspace w (graph g :context draw " ^ body ^ "))")) in
  match Flow.Workspace.check {Flow.Catalog.version=1;kinds=[]} forms with
  | Some ws,[] -> ws | _,ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let pictures = [
  "(draw/background \"#14161c\")";
  "(draw/merge (draw/rect [1 2 0] [30 40 0]) (draw/rect [1 2 0] [3 4 0] :stroke \"#fff\") (draw/rect [0 0 0] [5 5 0] :fill \"#f00\" :stroke \"#00f\"))";
  "(draw/circle [10.5 20 0] 4)";
  "(draw/circle [10 20 0] (+ 4 (* 2 (sin t))) :fill \"#5aaaf080\")";
  "(draw/line [0 0 0] [5 5 0])";
  "(draw/line [0 0 0] [5 5 0] :color \"#0f0\" :width 3)";
  "(draw/point [3.9 -4.9 0] :color \"#abc\")";
  "(draw/translate [1.9 -2.5 0] (draw/merge (draw/point [0 0 0]) (draw/rotate 0.5 (draw/scale 2 (draw/point [1 1 0])))))";
  "(draw/scale [2 3 0] (draw/background \"#000\"))";
  "(let* [p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 3))] (draw/circles p))";
  "(let* [p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 3)) r (map (fn [i] (* i 0.5)) (array/range 3))] (draw/circles p :radius r :stroke \"#fff\"))";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 3))] (draw/rects p [10 10 0] :fill \"#f00\"))";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 3)) c (map (fn [i] [(/ i 4) 0 0]) (array/range 3)) s (map (fn [i] [i 1 0]) (array/range 3))] (draw/rects p s :fill c))";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 3))] (draw/lines p p :width 2))";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 3))] (draw/points p :color \"#1e1e1e\"))";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 0))] (draw/merge (draw/circles p) (draw/points p)))";
  "(draw/text [12.7 12 0] \"hi\" :color \"#abc\" :size 20)";
  "(draw/text [12 12 0] \"\")";
  "(draw/rounded_rect [4 5 0] [60 40 0] :radius 8 :fill \"#123\")";
  "(draw/rounded_rect [4 5 0] [60 40 0])";
  "(draw/polygon (map (fn [i] [i (* i i) 0]) (array/range 4)) :stroke \"#fff\")";
  "(draw/polyline (map (fn [i] [i (* i i) 0]) (array/range 4)) :color \"#f0f\")";
]
let errors = [
  "(let* [p (map (fn [i] [i 0 0]) (array/range 3)) r (map (fn [i] 1.0) (array/range 2))] (draw/circles p :radius r))", "E_DRAW_LENGTH";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 3)) c (map (fn [i] [0.5 0 0]) (array/range 2))] (draw/rects p [1 1 0] :fill c))", "E_DRAW_LENGTH";
  "(let* [p (map (fn [i] [i 0 0]) (array/range 3)) q (map (fn [i] [i 0 0]) (array/range 2))] (draw/lines p q))", "E_DRAW_LENGTH";
  "(draw/rect [0 0 0] [-1 1 0])", "E_DRAW_RANGE";
  "(draw/point [1e40 0 0])", "E_DRAW_RANGE";
  "(draw/image (image/noise))", "E_IMAGE";
]
let float x = Printf.sprintf "%h" x
let dump () =
  let buffer = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buffer in
  let out f = Format.fprintf fmt f in
  List.iteri (fun i body ->
    out "picture %d: %s\n" i body;
    let evaluation = ok (E.static (checked body)) in
    let program, root = Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results)) in
    let prepared = Flow_ir.Core_exec.prepare program in
    let buffers = Buffers.create () in
    let live = {(Input.at_time 0.25) with size = (200, 100)} in
    let marks = ok (Flow_ir.Core_exec.draw ?root prepared ~live ~size:(200, 100) ~buffers) in
    let attr show = function
      | M.Constant v -> "const " ^ show v
      | Buffer {id; count; width} ->
          Printf.sprintf "buffer %d count=%d width=%d [%s]" id count width
            (String.concat " " (Array.to_list (Array.map float (Buffers.find buffers id))))
      | Device _ -> failwith "no device attribute without a device host" in
    let pair (x, y) = float x ^ "," ^ float y and ink c = Printf.sprintf "%08lx" c and one = float in
    Array.iter (fun entry -> match entry with
      | M.Mark_set {count; kind} ->
          (match kind with
           | Rect {position; size; fill; stroke; stroke_width} ->
               out "  rect %d position=%s size=%s fill=%s stroke=%s width=%s\n" count (attr pair position)
                 (attr pair size) (attr ink fill) (attr ink stroke) (attr one stroke_width)
           | Circle {position; radius; fill; stroke; stroke_width} ->
               out "  circle %d position=%s radius=%s fill=%s stroke=%s width=%s\n" count (attr pair position)
                 (attr one radius) (attr ink fill) (attr ink stroke) (attr one stroke_width)
           | Line {position; end_; stroke; stroke_width} ->
               out "  line %d from=%s to=%s color=%s width=%s\n" count (attr pair position) (attr pair end_)
                 (attr ink stroke) (attr one stroke_width)
           | Point {position; fill} -> out "  point %d position=%s fill=%s\n" count (attr pair position) (ink fill))
      | Geometry {vertices; indices; color} ->
          out "  geometry %s vertices=[%s] indices=[%s]\n" (ink color)
            (String.concat " " (Array.to_list (Array.map float vertices)))
            (String.concat " " (Array.to_list (Array.map string_of_int indices)))
      | Image _ -> out "  image\n"
      | Text {x; y; size; color; text} -> out "  text %d,%d size=%d %s %S\n" x y size (ink color) text
      | Push_transform {xx; xy; yx; yy; tx; ty} ->
          out "  push %s %s %s %s %s %s\n" (float xx) (float xy) (float yx) (float yy) (float tx) (float ty)
      | Pop_transform -> out "  pop\n"
      | Push_clip _ -> out "  push-clip\n" | Pop_clip -> out "  pop-clip\n") marks) pictures;
  List.iter (fun (body, code) ->
    let evaluation = ok (E.static (checked body)) in
    let program, root = Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results)) in
    match Flow_ir.Core_exec.draw ?root (Flow_ir.Core_exec.prepare program) ~live:(Input.at_time 0.)
            ~size:(10, 10) ~buffers:(Buffers.create ()) with
    | Error d -> assert (d.code = code); out "error %s: %s\n" d.code d.message
    | Ok _ -> failwith ("expected " ^ code ^ " for " ^ body)) errors;
  Format.pp_print_flush fmt ();
  Buffer.contents buffer
(* A host that takes every region it is offered (here it runs them on the CPU and hands back a
   token). A region read only as attributes of mark sets is offered, a colour like a position, and
   becomes a device buffer of its width; one the frame program also reads is not offered. *)
let device_marks body =
  let evaluation = ok (E.static (checked body)) in
  let program, root = Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results)) in
  let live = Input.at_time 0.25 in
  let offered = ref 0 in
  let device packed =
    let values=ok (Flow_ir.Packed.force_all packed ~live) |> Array.of_list in
    incr offered;
    Ok (Some (Array.map (fun value -> let count, width = match value with
      | E.Float_array xs -> Array.length xs, 1 | Vec2_array xs -> Array.length xs / 2, 2
      | Vec3_array xs -> Array.length xs / 3, 3 | _ -> failwith "a region is a packed array" in
      !offered, count, width) values)) in
  let marks = ok (Flow_ir.Core_exec.draw ?root ~device (Flow_ir.Core_exec.prepare program) ~live
    ~size:(200, 100) ~buffers:(Buffers.create ())) in
  !offered, marks
let devices () =
  let p = "p (map (fn [i] [(* i 10) (* i t) 0]) (array/range 3))"
  and c = "c (map (fn [i] [(/ i 4) t 0]) (array/range 3))" in
  let device : type a. int -> a M.attr -> bool = fun width -> function
    | M.Device {count = 3; width = w; _} -> w = width | _ -> false in
  (match device_marks (Printf.sprintf "(let* [%s %s] (draw/rects p [10 10 0] :fill c :stroke c))" p c) with
   | 1, [|M.Mark_set {count = 3; kind = Rect {position; size = Constant _; fill; stroke; _}}|] ->
       assert (device 3 position && device 3 fill && device 3 stroke)
   | _ -> failwith "rect positions, fill and stroke are device buffers");
  (match device_marks (Printf.sprintf "(let* [%s %s r (map (fn [i] (+ i t)) (array/range 3))] (draw/circles p :radius r :fill c))" p c) with
   | 1, [|M.Mark_set {kind = Circle {position; radius; fill; stroke = Constant 0l; _}; _}|] ->
       assert (device 3 position && device 1 radius && device 3 fill)
   | _ -> failwith "circle positions, radii and fill are device buffers");
  (match device_marks (Printf.sprintf "(let* [%s %s] (draw/lines p p :color c))" p c) with
   | 1, [|M.Mark_set {kind = Line {position; end_; stroke; _}; _}|] ->
       assert (device 3 position && device 3 end_ && device 3 stroke)
   | _ -> failwith "line ends and colour are device buffers");
  (* The frame reads the colours too (their count), so they stay a host buffer. *)
  (match device_marks (Printf.sprintf
      "(let* [%s %s] (draw/merge (draw/rects p [10 10 0] :fill c) (draw/text [0 0 0] (str (array/count c)))))" p c) with
   | 1, [|M.Mark_set {kind = Rect {position; fill = Buffer {count = 3; width = 3; _}; _}; _}; M.Text _|] ->
       assert (device 3 position)
   | _ -> failwith "a colour array the frame reads is a host buffer");
  (* A device colour of another length than the positions is the host buffer's error. *)
  let evaluation = ok (E.static (checked (Printf.sprintf
    "(let* [%s c (map (fn [i] [(/ i 4) t 0]) (array/range 2))] (draw/rects p [10 10 0] :fill c))" p))) in
  let program, root = Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results)) in
  let live = Input.at_time 0.25 in
  let device packed = match ok (Flow_ir.Packed.force packed ~live) with
    | E.Vec3_array xs -> Ok (Some [|(1, Array.length xs / 3, 3)|]) | _ -> Ok None in
  (match Flow_ir.Core_exec.draw ?root ~device (Flow_ir.Core_exec.prepare program) ~live ~size:(200, 100)
           ~buffers:(Buffers.create ()) with
   | Error d -> assert (d.code = "E_DRAW_LENGTH")
   | Ok _ -> failwith "a device colour of the wrong length is refused")
(* A fold whose body maps over the previous level with gathers, integer arithmetic and noise (the
   shape of recursive_rectangles' split-level): a drawing runs its nested maps as packed kernels,
   and the points it draws equal the reference walker's at one, four and eight domains, past the
   4,096 elements where a kernel splits across domains. *)
let nested_maps () =
  (* each level keeps the rows a filter of a range picks, then maps them *)
  let boxes = "(fold [b (map (fn [i] [i (* i 2) 0]) (array/range 600))] [d (range 4)] \
    (let* [kept (filter (fn [i] (>= (int/mod (int/mul (int i) 3) 7) 1)) (array/range (array/count b)))] \
      (map (fn [i] (let* [p (array/nth b (int/div (int (array/nth kept (int/div (int i) 2))) 1)) k (int/mod (int/mul (int i) 7) 5)] \
        [(+ p.x (sin (* (* p.y 0.01) (+ d t)))) (+ p.y k) 0])) (array/range (* (array/count kept) 2)))))" in
  let live = {(Input.at_time 0.25) with size = (200, 100)} in
  (* the walker's points, summed in order as x + 1000 y: the same sum of the drawn buffer must match it *)
  let expected = match E.Private.force_reference (List.assoc "g" (ok (E.static (match
      Flow.Workspace.check {Flow.Catalog.version=1;kinds=[]} (ok (Flow.Syntax.parse
        ("(workspace w (graph g :context value (array/sum (map (fn [v] (+ v.x (* v.y 1000))) " ^ boxes ^ "))))"))) with
       | Some ws, [] -> ws
       | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))))).results) ~live with
    | Ok (E.Float f) -> f
    | Ok _ -> failwith "nested maps: the reference sum is not a float"
    | Error d -> failwith (Diagnostic.to_string d) in
  let evaluation = ok (E.static (checked ("(let* [p " ^ boxes ^ "] (draw/points p))"))) in
  let program, root = Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results)) in
  let text = Format.asprintf "%a" Flow_ir.Core.pp program in
  let rec regions i = match String.index_from_opt text i '[' with
    | Some j when j >= 6 && String.sub text (j - 6) 6 = "region" -> 1 + regions (j + 1)
    | Some j -> regions (j + 1) | None -> 0 in
  if regions 0 < 2 then failwith "nested maps: the fold's maps are not packed regions";
  List.iter (fun domains -> Parallel.run ~domains (fun () ->
    let buffers = Buffers.create () in
    match ok (Flow_ir.Core_exec.draw ?root (Flow_ir.Core_exec.prepare program) ~live ~size:(200, 100) ~buffers) with
    | [| M.Mark_set {kind = Point {position = Buffer {id; width; _}; _}; count} |] ->
        let xs = Buffers.find buffers id in
        let sum = ref 0. in
        for k = 0 to count - 1 do sum := !sum +. (xs.(k * width) +. xs.(k * width + 1) *. 1000.) done;
        assert (count > 4096);
        if Int64.bits_of_float !sum <> Int64.bits_of_float expected then
          failwith (Printf.sprintf "nested maps: points differ from the walker at %d domains (%h vs %h)" domains !sum expected)
    | _ -> failwith "nested maps: one point set expected")) [ 1; 4; 8 ]

(* A drawing's loops keep their last results by their inputs: the same inputs at another time give
   the fresh program's points, and changed inputs are never answered from the cache. [k] is drawn
   once before the fold, so its frame read is a statement of its own, outside the loop. *)
let remembered_loops () =
  let body = "(let* [k (int (/ t 2)) p (fold [b (map (fn [i] [i 0 0]) (array/range 3))] [d (range 3)] \
    (map (fn [i] (let* [q (array/nth b i)] [(+ q.x (* k 10)) (+ q.y d) 0])) (array/range 3)))] \
    (draw/merge (draw/point [k 0 0]) (draw/points p)))" in
  let evaluation = ok (E.static (checked body)) in
  let program, root = Result.get_ok (Flow_ir.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results)) in
  let remembered = Flow_ir.Core_exec.prepare ~remember_loops:true program in
  let points prepared t =
    let buffers = Buffers.create () and live = {(Input.at_time t) with size = (200, 100)} in
    match ok (Flow_ir.Core_exec.draw ?root prepared ~live ~size:(200, 100) ~buffers) with
    | [| _; M.Mark_set {kind = Point {position = Buffer {id; _}; _}; _} |] -> Array.copy (Buffers.find buffers id)
    | _ -> failwith "remembered loops: a point and a point set expected" in
  List.iter (fun t ->
    if points remembered t <> points (Flow_ir.Core_exec.prepare program) t then
      failwith (Printf.sprintf "remembered loops: a cached frame at t=%g differs from a fresh one" t))
    [ 0.25; 0.5; 3.0; 0.25; 5.0 ]

let grouping () =
  let module C=Flow_ir.Core in
  let lower body =
    let e=ok(E.static(checked body))in
    Result.get_ok(Flow_ir.Core_draw.lower e.plan(List.assoc "g" e.results))in
  let body="(let* [a (+ t 2) b (+ t 4) \
    p (map (fn [i] [(+ i a) i 0]) (array/range 4)) \
    r (map (fn [i] (+ i b)) (array/range 4))] (draw/circles p :radius r))"in
  let program,root=lower body in
  let groups ?(approx=Flow.Workspace.Paths.empty) ?(legal=fun _->true) program =
    snd(Flow_ir.Core_draw.device_groups ~approx program ~legal
      ~width:(fun k->Some(Array.length program.C.regions.(k).outputs.(0))))in
  assert(List.map(fun(g:Flow_ir.Core_draw.group)->List.length g.members)(groups program)=[2]);
  let first=List.hd(groups program)in
  let _,site,_=Flow_ir.Packed.site program.regions.(List.hd first.members)in
  assert(List.length(groups ~approx:(Flow.Workspace.Paths.singleton site) program)=2);
  assert(List.length(groups ~legal:(fun _->false) program)=2);
  let count=ref 0 in
  let barrier=List.concat_map(fun(s:C.statement)->match s.node with
    |Invoke _->incr count;if !count=2 then [{s with node=Draw Pop};s]else[s]|_->[s])program.body in
  assert(List.length(groups {program with body=barrier})=2);
  let static,_=lower "(let* [p (map (fn [i] [(+ i t) i 0]) (array/range 4)) \
    r (map (fn [i] (+ i 4)) (array/range 4))] (draw/circles p :radius r))"in
  (* The static radius was already staged; it stays outside the live group. *)
  assert(List.map(fun(g:Flow_ir.Core_draw.group)->List.length g.members)(groups static)=[1]);
  let different,_=lower "(let* [p (map (fn [i] [(+ i t) i 0]) (array/range 4)) \
    r (map (fn [i] (+ i t)) (array/range 3))] (draw/circles p :radius r))"in
  assert(List.length(groups different)=2);
  let live=Input.at_time 0.25 and offers=ref []in
  let prepared=Flow_ir.Core_exec.prepare ~remember_loops:true program in
  let draw ?device ()=Flow_ir.Core_exec.draw ?root ?device prepared ~live
    ~size:(200,100)~buffers:(Buffers.create())in
  let expected=ok(draw())in
  List.iter(fun refuse->
    offers:=[];
    let device packed=
      let outputs=List.length(ok(Flow_ir.Packed.force_all packed ~live))in
      offers:=outputs:: !offers;
      if refuse && outputs>1 then Error(Diagnostic.error ~code:"E_GPU" "injected fused refusal")else Ok None in
    assert(ok(draw ~device())=expected);
    assert(List.rev !offers=[2;1;1]))[false;true];
  let device packed=match ok(Flow_ir.Packed.force_all packed ~live)with
    |[E.Vec3_array positions;Float_array radii]->
        assert(positions=[|2.25;0.;0.;3.25;1.;0.;4.25;2.;0.;5.25;3.;0.|]);
        assert(radii=[|4.25;5.25;6.25;7.25|]);
        Ok(Some[|(0,4,3);(1,4,1)|])
    |_->failwith "two fused outputs in authored order"in
  ignore(ok(draw ~device()));
  let bad,root=lower "(let* [p (map (fn [i] [(pow (+ i t) 1000) i 0]) (array/range 4)) \
    r (map (fn [i] (pow (+ i t) 1001)) (array/range 4))] (draw/circles p :radius r))"in
  let prepared=Flow_ir.Core_exec.prepare bad in
  let fail ?device()=Flow_ir.Core_exec.draw ?root ?device prepared ~live:(Input.at_time 0.)
    ~size:(200,100)~buffers:(Buffers.create())in
  let cpu=fail()in
  assert(Result.is_error cpu);
  assert(fail ~device:(fun _->Ok None)()=cpu)

let device_inputs () =
  let module G=Flow_ir.Gpu in
  let seen=ref [] and serial=ref 0 in
  let backend:G.backend={cost=(fun ~fast:_ _ ~count:_->Some 0.);prepare=(fun ~fast:_ packed->
    let width=Array.length(Flow_ir.Packed.Private.view packed).outputs.(0)in
    Ok{run=(fun inputs->
      seen:=inputs.arrays:: !seen;incr serial;
      Ok{identity=1;output=0;count=inputs.count;width;stamp=Int64.of_int !serial;gpu_seconds=None});
      readback=(fun _->assert false)})}in
  let gpu={G.backend=Some backend;fast=true;policy=G.Qualification}in
  let display=Flow_ir.Executor.region_display ~gpu ()in
  let prepared count=
    let body=Printf.sprintf "(draw/circles (map (fn [i] [(+ i t) i 0]) (array/range %s)) \
      :radius (map (fn [i] (+ i t)) (array/range %s)))"count count in
    let e=ok(E.static(checked body))in
    let program,root=Result.get_ok(Flow_ir.Core_draw.lower e.plan(List.assoc "g" e.results))in
    Flow_ir.Core_exec.prepare ~fast:true program,root in
  let draw (program,root) time=
    let live=Input.at_time time in
    let device packed=Result.map(Option.map(fun(v:G.value)->
      Array.map(fun output->v.identity,v.count,Array.length output)
        (Flow_ir.Packed.Private.view packed).outputs))(display packed ~live)in
    ignore(ok(Flow_ir.Core_exec.draw ?root ~device program ~live
      ~size:(200,100)~buffers:(Buffers.create())))in
      let fixed=prepared "1024"in
      draw fixed 0.25;draw fixed 0.5;
      (match !seen with
       |second::first::[]->
           assert(Array.length first>0 && Array.length second>0);
           assert(Array.for_all(fun values->values==first.(0))first);
           assert(Array.for_all(fun values->values==first.(0))second)
       |_->failwith "one fused dispatch each frame");
      seen:=[];
      let dynamic=prepared "(+ 1024 (int t))"in
      draw dynamic 1.;draw dynamic 2.;
      let counts=List.map(fun arrays->Array.length arrays.(0))!seen in
      assert(List.sort_uniq Int.compare counts=[1025;1026]);
      assert(List.hd counts=1026 && List.hd(List.rev counts)=1025)

let () =
  let actual = dump () in
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--print" then print_string actual
  else begin
    remembered_loops ();
    nested_maps ();
    devices ();
    grouping ();
    device_inputs ();
    if actual <> In_channel.with_open_bin "core_marks.expected" In_channel.input_all
    then failwith ("Core marks changed:\n" ^ actual)
  end
