(* G.1: a ported node's large position kernel is placed on the GPU by measured cost, read back once at
   the SOP input sink, and lies within 32-bit tolerance of the CPU body. A mock device rounds the CPU
   result to float32, so the comparison is the tolerance the real device is held to. *)
open Rdk
open Rdk_test_support
module L = Flow_sop.Lower
module Edit = Sop.Edit_graph
module Session = Sop.Session

let flow_ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 100

let text =
  Printf.sprintf
    "(workspace g (graph g :context sop (sop/grid :counts \"Point counts\" :connectivity \
     \"Points\" :columns %d :rows %d :size 3.0 :rotation 0.3)))"
    side side

let positions backend =
  let forms = Flow.Syntax.parse text |> flow_ok in
  let gpu =
    Option.fold backend ~none:Flow_ir.Gpu.default ~some:(fun backend ->
        { Flow_ir.Gpu.default with backend = Some backend })
  in
  let lowered = L.workspace ~gpu forms |> flow_ok in
  let graph = List.hd lowered.graphs in
  let node =
    Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok
  in
  let session = Session.create ~max_entries:4 ~max_payload_bytes:(1 lsl 30) |> get_string_ok in
  Fun.protect
    ~finally:(fun () -> Session.close session)
    (fun () ->
      match
        Session.cook session
          ~context:(Sop.Context.create ~domains:4 ~time:0. () |> get_string_ok)
          node
      with
      | Ok output -> Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))
      | Error d -> failwith (Diagnostic.error_to_string d))

let () =
  let calls = ref 0 and mock_time = ref 0. in
  let mock ~cost : Flow_ir.Gpu.backend =
    {
      residency = (fun ~fast:_ -> None);
      cost = (fun ?inputs:_ ~fast:_ _ ~count:_ -> cost);
      prepare =
        (fun ~fast:_ packed ->
          assert (Domain.is_main_domain ());
          let result = ref None in
          let rec kernel : Flow_ir.Gpu.kernel =
            {
              run =
                (fun inputs ->
                  assert (Domain.is_main_domain ());
                  incr calls;
                  result := Some (Flow_ir.Packed.force packed ~live:(Input.at_time !mock_time) |> flow_ok);
                  Ok
                    {
                      identity = 1;
                      count = inputs.count;
                      width = 1;
                      output = 0;
                      stamp = Int64.of_int !calls;
                      gpu_seconds = None;
                    });
              readback =
                (fun _ ->
                  match !result with
                  | Some (Flow.Eval.Float_array d) ->
                      Ok
                        (Flow.Eval.Float_array
                           (Array.map (fun x -> Int32.float_of_bits (Int32.bits_of_float x)) d))
                  | Some (Flow.Eval.Vec3_array d) ->
                      Ok
                        (Flow.Eval.Vec3_array
                           (Array.map (fun x -> Int32.float_of_bits (Int32.bits_of_float x)) d))
                  | _ -> assert false);
              planes = (fun v -> Flow_ir.Gpu.planes_of kernel.readback v);
            }
          in
          Ok kernel);
    }
  in
  if (if Array.length Sys.argv > 1 then Sys.argv.(1) = "1"
      else Sys.getenv_opt "RAYS_SLOW" = Some "1") then begin
    let cpu = positions None in
    let count = side * side in
    (* the CPU branch: an unmeasured or slower device keeps the bit-exact walker value *)
    List.iter
      (fun cost ->
        calls := 0;
        let again = positions (Some (mock ~cost)) in
        assert (!calls = 0);
        for i = 0 to count - 1 do
          assert (Packed.Float3.get again i = Packed.Float3.get cpu i)
        done)
      [ None; Some 1.0 ];
    (* the GPU branch: three position planes dispatched, read back, within float32 tolerance *)
    calls := 0;
    let gpu = positions (Some (mock ~cost:(Some 0.))) in
    assert (!calls = 3);
    for i = 0 to count - 1 do
      let x, y, z = Packed.Float3.get cpu i and gx, gy, gz = Packed.Float3.get gpu i in
      let near a b = Float.abs (a -. b) <= 1e-4 *. (1. +. Float.abs a) in
      assert (near x gx && near y gy && near z gz)
    done;
    Printf.printf
      "Grid GPU placement: %d points, 3 planes dispatched when cheaper, none when unmeasured or \
       slower, within float32 tolerance\n"
      count
  end;
  let planes = ref 0 in
  let ordinary = mock ~cost:(Some 0.) in
  let backend =
    {
      ordinary with
      prepare =
        (fun ~fast packed ->
          Result.map
            (fun (kernel : Flow_ir.Gpu.kernel) ->
              {
                kernel with
                planes =
                  (fun output ->
                    incr planes;
                    kernel.planes output);
              })
            (ordinary.prepare ~fast packed));
    }
  in
  let gpu = Flow_ir.Gpu.{ default with backend = Some backend; fast = true } in
  let authored =
    "(workspace readers (graph g :context sop\n\
     (sop/with_attr (sop/mesh_planes :points {:x (array/float 32) :y (array/float 32) :z (array/float 32)} :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)) :P\n\
       (exact (map (fn [i] [(+ i t) 0 0]) (array/range 32))))))"
  in
  let ticks = ref 0 in
  let captured ?(points = false) capture time =
    mock_time := time;
    planes := 0;
    let authored =
      if points then
        "(workspace readers (graph g :context sop\n\
         (sop/with_attr (sop/points :points 32) :P\n\
           (exact (map (fn [i] [(+ i t) 0 0]) (array/range 32))))))"
      else authored
    in
    let lowered =
      (* a clock that advances: the write's first cooks measure its CPU route *)
      L.workspace ~gpu ~clock:(fun () -> incr ticks; float !ticks)
        (Flow.Syntax.parse authored |> flow_ok)
      |> flow_ok
    in
    let profile = lowered.profile in
    let graph = List.hd lowered.graphs in
    let resolved =
      Flow_sop.Value_lane.resolve ~gpu (Flow_sop.Value_lane.create ()) ~time graph.network
      |> flow_ok
    in
    let root =
      Edit.compile_node resolved.geometry ~node_id:(Option.get graph.root) |> get_string_ok
    in
    let root = if capture then Flow_sop.Attribute_kernel.materialized_source root else root in
    let session = Session.create ~max_entries:0 ~max_payload_bytes:0 |> get_string_ok in
    Fun.protect
      ~finally:(fun () -> Session.close session)
      (fun () ->
        let cook () =
          Session.cook session ~context:(Sop.Context.create ~domains:1 ~time () |> get_string_ok) root
          |> flow_ok
        in
        if capture then ignore (cook (), cook ());
        planes := 0;
        let output = cook () in
        let geometry = Sop.Payload.geometry output.payload |> flow_ok in
        if points then
          List.iter
            (fun (name, expected) ->
              let attribute = Option.get (Geometry.find_attribute ~owner:Point name geometry) in
              match Attribute.storage attribute with
              | Int values -> assert (values = Array.init 32 expected)
              | _ -> assert false)
            [ ("sourcepoint", fun _ -> -1); ("sourceindex", Fun.id) ];
        let points = Geometry.positions geometry in
        assert (!planes = if capture then 1 else 0);
        if capture then
          assert (
            List.exists
              (fun (p : Flow_ir.placement) ->
                p.cpu_consumers = Some 1 && p.reason = Device_measured_cheaper)
              (Flow_ir.Profile.placements profile));
        assert (Packed.Float3.get points 31 = (31. +. time, 0., 0.)))
  in
  captured false 0.;
  captured true 0.;
  captured true 1.;
  captured ~points:true false 0.;
  captured ~points:true true 0.;
  captured ~points:true true 1.;
  print_endline
    "Measured SOP consumer plumbing: opaque root declines resident record; capture supplies one \
     CPU reader"
