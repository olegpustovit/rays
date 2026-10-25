(* G.1: a ported node's large position kernel is placed on the GPU by measured cost, read back once at
   the SOP input sink, and lies within 32-bit tolerance of the CPU body. A mock device rounds the CPU
   result to float32, so the comparison is the tolerance the real device is held to. *)
open Rdk
open Rdk_test_support
module L = Flow_sop.Lower
module Edit = Sop.Edit_graph
module Session = Sop.Session
let flow_ok = function Ok x -> x | Error d -> failwith (Flow.Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 100
let text = Printf.sprintf "(workspace g (graph g :context sop (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns %d :rows %d :size 3.0 :rotation 0.3)))" side side
let positions backend =
  let forms = Flow.Syntax.parse text |> flow_ok in
  let lowered = Option.fold backend ~none:(fun f -> f ()) ~some:(fun b f -> Flow_ir.Gpu.with_backend b f)
    (fun () -> L.workspace ~factories:Sop_catalog.Editor.factories forms |> flow_ok) in
  let graph = List.hd lowered.graphs in
  let node = Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok in
  let session = Session.create ~max_entries:4 ~max_payload_bytes:(1 lsl 30) |> get_string_ok in
  Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
    match Session.cook session ~context:(Sop.Context.create ~domains:4 ~time:0. () |> get_string_ok) node with
    | Ok output -> Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))
    | Error d -> failwith (Sop.Diagnostic.error_to_string d))
let () =
  let calls = ref 0 in
  let mock ~cost : Flow_ir.Gpu.backend = {cost = (fun _ ~count:_ -> cost);
    prepare = (fun packed -> assert (Domain.is_main_domain());
      let result = ref None in
      Ok {run = (fun inputs -> assert (Domain.is_main_domain()); incr calls;
            result := Some (Flow_ir.Packed.force packed ~live:(Frame_input.at_time 0.) |> flow_ok);
            Ok {identity = 1; count = inputs.count; width = 1; stamp = Int64.of_int !calls; gpu_seconds = None});
          readback = (fun _ -> match !result with
            | Some (Flow.Eval.Float_array d) ->
                Ok (Flow.Eval.Float_array (Array.map (fun x -> Int32.float_of_bits (Int32.bits_of_float x)) d))
            | _ -> assert false)})} in
  let cpu = positions None in
  let count = side * side in
  (* the CPU branch: an unmeasured or slower device keeps the bit-exact walker value *)
  List.iter (fun cost ->
    calls := 0;
    let again = positions (Some (mock ~cost)) in
    assert (!calls = 0);
    for i = 0 to count - 1 do assert (Packed.Float3.get again i = Packed.Float3.get cpu i) done)
    [None; Some 1.0];
  (* the GPU branch: three position planes dispatched, read back, within float32 tolerance *)
  calls := 0;
  let gpu = positions (Some (mock ~cost:(Some 0.))) in
  assert (!calls = 3);
  for i = 0 to count - 1 do
    let (x, y, z) = Packed.Float3.get cpu i and (gx, gy, gz) = Packed.Float3.get gpu i in
    let near a b = Float.abs (a -. b) <= 1e-4 *. (1. +. Float.abs a) in
    assert (near x gx && near y gy && near z gz);
  done;
  Printf.printf "Grid GPU placement: %d points, 3 planes dispatched when cheaper, none when unmeasured or slower, within float32 tolerance\n" count

(* G.2: the fused four-deformer chain reads cooked attributes, so its kernel runs inside a cook on a worker
   domain. The device is reached through the service: the worker queues the request, the initial domain
   pumps it, and the cook continues with the read-back planes. *)
let () =
  let stages = [0.3, 0.7, 0; -0.2, 1.3, 3; 0.15, 2.1, 5; 0.05, 0.4, 7] in
  let text = Printf.sprintf "(workspace fused (graph g :context sop (let* [g (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns 2000 :rows 2) %s] d4)))"
    (String.concat " " (List.mapi (fun i (amplitude, frequency, seed) ->
      Printf.sprintf "d%d (sop/noise_displace %s :amplitude %g :frequency %g :seed %d)" (i + 1)
        (if i = 0 then "g" else Printf.sprintf "d%d" i) amplitude frequency seed) stages)) in
  let lowered = L.workspace ~factories:Sop_catalog.Editor.factories (Flow.Syntax.parse text |> flow_ok) |> flow_ok in
  let graph = List.hd lowered.graphs in
  let node = Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok in
  let cook () =
    let session = Session.create ~max_entries:4 ~max_payload_bytes:(1 lsl 28) |> get_string_ok in
    Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
      match Session.cook session ~context:(Sop.Context.create ~domains:4 ~time:0. () |> get_string_ok) node with
      | Ok output -> Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))
      | Error d -> failwith (Sop.Diagnostic.error_to_string d)) in
  let cpu = cook () in
  let count = 4000 in
  let flat = Array.make (count * 3) 0. in
  for i = 0 to count - 1 do
    let (x, y, z) = Packed.Float3.get cpu i in flat.(3*i) <- x; flat.(3*i+1) <- y; flat.(3*i+2) <- z
  done;
  let runs = ref 0 and reads = ref 0 in
  let device ~cost : Flow_ir.Gpu.backend = {cost = (fun _ ~count:_ -> cost);
    prepare = (fun _ -> assert (Domain.is_main_domain());
      Ok {run = (fun inputs -> assert (Domain.is_main_domain()); assert (inputs.count = count); incr runs;
            Ok {identity = 1; count; width = 3; stamp = Int64.of_int !runs; gpu_seconds = None});
          readback = (fun _ -> assert (Domain.is_main_domain()); incr reads;
            Ok (Flow.Eval.Vec3_array (Array.map (fun x -> Int32.float_of_bits (Int32.bits_of_float x)) flat)))})} in
  let on_worker () =
    let finished = Atomic.make false in
    let domain = Domain.spawn (fun () -> let result = cook () in Atomic.set finished true; result) in
    while not (Atomic.get finished) do Flow_ir.Gpu.pump (); Domain.cpu_relax () done;
    Domain.join domain in
  Fun.protect ~finally:(fun () -> Flow_ir.Gpu.set_service None) (fun () ->
    Flow_ir.Gpu.set_service (Some (device ~cost:(Some 1.0)));
    let slower = on_worker () in
    assert (!runs = 0);
    for i = 0 to count - 1 do assert (Packed.Float3.get slower i = Packed.Float3.get cpu i) done;
    Flow_ir.Gpu.set_service (Some (device ~cost:(Some 0.)));
    let placed = on_worker () in
    assert (!runs = 1 && !reads = 1);
    for i = 0 to count - 1 do
      let (x, y, z) = Packed.Float3.get cpu i and (gx, gy, gz) = Packed.Float3.get placed i in
      let near a b = Float.abs (a -. b) <= 1e-4 *. (1. +. Float.abs a) in
      assert (near x gx && near y gy && near z gz)
    done);
  print_endline "Staged cook: the fused four-deformer chain dispatches once through the service from a worker domain, CPU when slower"
