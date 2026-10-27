(* A worker cook reaches the workspace-owned packed-attribute GPU proxy only while the initial
   domain pumps it. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let graph_ok = function Ok value -> value | Error message -> failwith message
let count = 2048
let text = Printf.sprintf
  "(workspace kernel (graph g :context sop (let* [g (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns %d :rows 2) amp 0.8 freq 0.16 result (sop/with_attr g :P (exact (map (fn [i] [(+ i t) 0 0]) (array/range %d))))] result)))"
  (count / 2) count

let () =
  let lowered = Flow_sop.Lower.workspace ~factories:Sop_catalog.Editor.factories
      (Flow.Syntax.parse text |> ok) |> ok in
  let cpu_graph = List.hd lowered.graphs in
  assert (not (Flow_sop.Network.Int_map.is_empty cpu_graph.network.frame_nodes));
  let cook ?gpu (graph : Flow_sop.Lower.graph) =
    let resolved = Flow_sop.Value_lane.resolve ?gpu (Flow_sop.Value_lane.create ()) ~time:0. graph.network |> ok in
    let node = Sop.Edit_graph.compile_node resolved.geometry ~node_id:(Option.get graph.root) |> graph_ok in
    let session = Sop.Session.create ~max_entries:4 ~max_payload_bytes:(1 lsl 28) |> graph_ok in
    Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
      match Sop.Session.cook session ~context:(Sop.Context.create ~domains:4 ~time:0. () |> graph_ok) node with
      | Ok output -> Rdk.Geometry.positions (Sop.Payload.geometry output.payload |> ok)
      | Error d -> failwith (Diagnostic.error_to_string d)) in
  let cpu = cook cpu_graph in
  let flat = Array.make (count * 3) 0. in
  for i = 0 to count - 1 do
    let x, y, z = Rdk.Packed.Float3.get cpu i in
    flat.(3*i) <- x; flat.(3*i+1) <- y; flat.(3*i+2) <- z
  done;
  let costs = ref 0 and prepares = ref 0 and runs = ref 0 and reads = ref 0 in
  let backend ~cost : Flow_ir.Gpu.backend = {
    cost = (fun ~fast:_ _ ~count:_ -> incr costs; cost);
    prepare = (fun ~fast:_ _ -> assert (Domain.is_main_domain()); incr prepares;
      Ok {run = (fun inputs -> assert (Domain.is_main_domain()); assert (inputs.count = count); incr runs;
            Ok {identity = 1; count; width = 3; output = 0; stamp = Int64.of_int !runs; gpu_seconds = None});
          readback = (fun _ ->
            assert (Domain.is_main_domain()); incr reads;
            let rounded = Array.map (fun x -> Int32.float_of_bits (Int32.bits_of_float x)) flat in
            Ok (Flow.Eval.Vec3_array rounded))})} in
  let owner = Flow_native.Workspace_gpu.create () in
  Fun.protect ~finally:(fun () -> Flow_native.Workspace_gpu.close owner) (fun () ->
    let on_worker ?(scope = true) (graph : Flow_sop.Lower.graph) () =
      let work gpu =
      let finished = Atomic.make false in
      let service_seen = Atomic.make false in
      let domain = Domain.spawn (fun () ->
        Fun.protect ~finally:(fun () -> Atomic.set finished true; Parallel.release_current_domain_pools ())
          (fun () ->
            Atomic.set service_seen (Option.is_some gpu.Flow_ir.Gpu.backend);
            cook ~gpu graph)) in
      while not (Atomic.get finished) do Flow_native.Workspace_gpu.pump owner; Domain.cpu_relax () done;
      let result = Domain.join domain in
      assert (Atomic.get service_seen);
      result in
      if scope then Flow_native.Workspace_gpu.with_backend owner work
      else work (Flow_native.Workspace_gpu.config owner) in
    Flow_native.Workspace_gpu.Private.install_test_backend owner (backend ~cost:(Some 1.0));
    let gpu=Flow_native.Workspace_gpu.config owner in
    let gpu_lowered=Flow_sop.Lower.workspace ~gpu ~factories:Sop_catalog.Editor.factories
        (Flow.Syntax.parse text |> ok) |> ok in
    let graph=List.hd gpu_lowered.graphs in
    let slower = on_worker graph () in
    assert (!runs = 0);
    for i = 0 to count - 1 do assert (Rdk.Packed.Float3.get slower i = Rdk.Packed.Float3.get cpu i) done;
    Flow_native.Workspace_gpu.Private.install_test_backend owner (backend ~cost:(Some 0.));
    Flow_native.Workspace_gpu.with_backend owner (fun _ -> ());
    (* The lowered graph captures its immutable backend config. Re-lower after replacing the
       test backend instead of expecting an old graph to observe the replacement. *)
    let cheap_gpu = Flow_native.Workspace_gpu.config owner in
    let cheap_lowered = Flow_sop.Lower.workspace ~gpu:cheap_gpu ~factories:Sop_catalog.Editor.factories
        (Flow.Syntax.parse text |> ok) |> ok in
    let cheap_graph = List.hd cheap_lowered.graphs in
    let placed = on_worker ~scope:false cheap_graph () in
    if !runs <> 1 || !reads <> 1 then failwith (Printf.sprintf
      "expected one worker dispatch/readback, got cost/prepare/run/readback %d/%d/%d/%d"
      !costs !prepares !runs !reads);
    for i = 0 to count - 1 do
      let x, y, z = Rdk.Packed.Float3.get cpu i and gx, gy, gz = Rdk.Packed.Float3.get placed i in
      let near a b = Float.abs (a -. b) <= 1e-4 *. (1. +. Float.abs a) in
      assert (near x gx && near y gy && near z gz)
    done);
  let outer = Flow_native.Workspace_gpu.create () in
  Flow_native.Workspace_gpu.Private.install_test_backend outer (backend ~cost:(Some 0.));
  let nested = Flow_native.Workspace_gpu.create () in
  Flow_native.Workspace_gpu.Private.install_test_backend nested (backend ~cost:(Some 0.));
  Flow_native.Workspace_gpu.with_backend outer (fun first ->
    Flow_native.Workspace_gpu.with_backend nested (fun second ->
      assert (Option.get first.backend != Option.get second.backend));
    assert (Option.get first.backend == Option.get (Flow_native.Workspace_gpu.config outer).backend));
  Flow_native.Workspace_gpu.close nested;
  Flow_native.Workspace_gpu.close outer;
  let closing = Flow_native.Workspace_gpu.create () in
  Flow_native.Workspace_gpu.Private.install_test_backend closing (backend ~cost:(Some 0.));
  Flow_native.Workspace_gpu.with_backend closing (fun _ -> ());
  let entered = Atomic.make false in
  let waiting = Domain.spawn (fun () ->
    Atomic.set entered true;
    try Flow_native.Workspace_gpu.Private.on_owner_for_test closing (fun () -> ()); None
    with Diagnostic.Error d -> Some d) in
  while not (Atomic.get entered) do Domain.cpu_relax () done;
  Flow_native.Workspace_gpu.close closing;
  let cancelled = Domain.join waiting in
  assert (Option.fold ~none:false ~some:(fun d -> d.Diagnostic.code = "E_GPU") cancelled);
  print_endline "Workspace GPU service: worker cook is pumped by its owner, selects CPU when slower, and reads back when cheaper"
