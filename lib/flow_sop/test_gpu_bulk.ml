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
  let calls = ref 0 in
  let mock ~cost : Flow_ir.Gpu.backend =
    {
      cost = (fun ~fast:_ _ ~count:_ -> cost);
      prepare =
        (fun ~fast:_ packed ->
          assert (Domain.is_main_domain ());
          let result = ref None in
          Ok
            {
              run =
                (fun inputs ->
                  assert (Domain.is_main_domain ());
                  incr calls;
                  result := Some (Flow_ir.Packed.force packed ~live:(Input.at_time 0.) |> flow_ok);
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
                  | _ -> assert false);
            });
    }
  in
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
