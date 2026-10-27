(* P6.4a: the frame of a program whose mark-set attributes the export placed on the GPU. [Js.frame ~gpu] prints the
   [Invoke] of a GPU region as [await rt.gpuArray(k)]; without it the output is the CPU form, unchanged.
   Regenerate the golden with [test_js_gpu.exe --write-goldens] from lib/flow_web. *)
module E = Flow.Eval
module I = Flow_ir

let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let writing = Array.to_list Sys.argv = [Sys.argv.(0); "--write-goldens"]
let has text part = try ignore (Str.search_forward (Str.regexp_string part) text 0); true with Not_found -> false

let source = {|(workspace w (graph g :context draw
  (let* [positions (map (fn [i] [(+ i t) (* i 2) 0]) (array/range 2048))
         radii (map (fn [i] (+ 1 (* 0.001 (+ i t)))) (array/range 2048))]
    (draw/circles positions :radius radii))))|}

let () =
  let ops = Core_fixtures.offset :: [] in
  let ws = match Flow.Workspace.check ~ops {Flow.Check.version = 1; kinds = []} (ok (Flow.Syntax.parse source)) with
    | Some ws, [] -> ws | _ -> failwith "does not check" in
  let evaluation = ok (E.static ws) in
  let program = match I.Core_draw.lower evaluation.plan (List.assoc "g" evaluation.results) with
    | Ok (p, _) -> p | Error n -> failwith n in
  assert (Array.length program.regions = 2);
  let width region = Some (Array.length program.regions.(region).outputs.(0)) in
  let eligible, batches = I.Core_draw.device_regions program ~width in
  assert (eligible = [|true; true|] && batches = [[0; 1]]);
  let eligible, batches = I.Core_draw.device_regions program
      ~width:(fun region -> if region = 0 then None else width region) in
  assert (eligible = [|false; true|] && batches = [[1]]);
  let cpu = ok (Flow_web.Js.frame program) in
  assert (has cpu "await rt.invoke(0, [])" && has cpu "await rt.invoke(1, [])" && not (has cpu "gpuArray"));
  assert (cpu = ok (Flow_web.Js.frame ~gpu:(fun _ -> false) program));
  let gpu = ok (Flow_web.Js.frame ~gpu:(fun _ -> true) program) in
  assert (has gpu "await rt.gpuArray(0)" && has gpu "await rt.gpuArray(1)" && not (has gpu "rt.invoke"));
  (* One GPU region and one CPU region: only region 1 changes. *)
  let mixed = ok (Flow_web.Js.frame ~gpu:(fun k -> k = 1) program) in
  assert (has mixed "await rt.invoke(0, [])" && has mixed "await rt.gpuArray(1)");
  let path = "goldens/frame_gpu_circles.js" in
  if writing then Out_channel.with_open_bin path (fun c -> output_string c gpu)
  else if In_channel.with_open_bin path In_channel.input_all <> gpu then failwith ("golden changed: " ^ path);
  print_endline "JS frame: GPU attribute regions print rt.gpuArray, CPU programs are unchanged"
