(* Flow_native.Drawing.render_prepared through the core path: draw/circles with per-instance radius and fill,
   draw/rects with per-instance size and fill (static and animated positions), and a static batch
   under a changing draw/translate. Wall time and allocated bytes per frame (median of the frames run). *)
module E = Flow.Eval

let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let catalog = { Flow.Check.version = 0; kinds = [] }
let size = (800, 600)

let check source = match Flow.Workspace.check catalog (ok (Flow.Syntax.parse source)) with
  | Some ws, ds when not (List.exists (fun (d : Diagnostic.t) -> d.severity = Diagnostic.Error_severity) ds) -> ws
  | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))

let points n animated =
  Printf.sprintf "(map (fn [i] [(* (mod i 800) 1.0) (+ (/ i 1250) %s) 0]) (array/range %d))"
    (if animated then "(* 5 (sin t))" else "0") n

let colours n = Printf.sprintf "(map (fn [i] [(/ (mod i 256) 255) 0.5 0.8]) (array/range %d))" n

let picture kind n animated =
  let p = points n animated in
  match kind with
  | "circles" -> Printf.sprintf "(draw/circles %s :radius (map (fn [i] (+ 1 (mod i 4))) (array/range %d)) :fill %s)" p n (colours n)
  | "rects" -> Printf.sprintf "(draw/rects %s (map (fn [i] [(+ 2 (mod i 3)) 3 0]) (array/range %d)) :fill %s)" p n (colours n)
  | _ -> Printf.sprintf "(draw/translate [(* 10 (sin t)) 0 0] (draw/rects %s [3 3 0] :fill \"#5aaaf0\"))" p

let median a = let a = Array.copy a in Array.sort Float.compare a; a.(Array.length a / 2)

let measure kind n animated =
  let source = Printf.sprintf "(workspace w (graph g :context draw %s))" (picture kind n animated) in
  let evaluation = ok (E.static (check source)) in
  let prepared = ok (Flow_native.Drawing.prepare ~states:evaluation.states evaluation.plan
    (List.assoc "g" evaluation.results)) in
  let frames = if n >= 1_000_000 then 3 else 8 in
  let state = E.create_state () in
  let frame i = ok (Flow_native.Drawing.render_prepared ~state prepared
    ~live:{(Input.at_time (float i /. 60.)) with frame = i; tick = i; size} ~size) in
  ignore (frame 0);
  let samples = Array.init frames (fun i ->
    Gc.full_major ();
    let bytes = Gc.allocated_bytes () and start = Unix.gettimeofday () in
    ignore (frame (i + 1));
    Unix.gettimeofday () -. start, Gc.allocated_bytes () -. bytes) in
  median (Array.map fst samples), median (Array.map snd samples)

let () =
  Printf.printf "%-10s %-9s %9s | %12s %14s\n" "kind" "mode" "n" "ms" "bytes";
  let row kind animated n =
    let t, b = measure kind n animated in
    Printf.printf "%-10s %-9s %9d | %12.3f %14.0f\n%!" kind (if animated then "animated" else if kind = "translate" then "moving" else "static") n (t *. 1e3) b in
  List.iter (fun n ->
    List.iter (fun kind -> List.iter (fun animated -> row kind animated n) [false; true]) ["circles"; "rects"];
    row "translate" false n) [100_000; 1_000_000]
