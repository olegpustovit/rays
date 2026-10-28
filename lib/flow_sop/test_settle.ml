(* A fused write placed on a mock device leaves its dispatch in flight: the session settles it at a
   cook's end, a failed status recooks that write on the CPU (the result is the CPU cook's), and a
   result the session drops (evicted by a later cook) releases its device outputs at the end of
   the first cook whose root, and the previous cook's, do not hold them. *)
let ok = function Ok v -> v | Error d -> failwith (Diagnostic.to_string d)

let text y =
  Printf.sprintf
    "(workspace w (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \
     \"Points\" :columns 8 :rows 8) (sop/attribute_noise :name \"noise\" :seed 7) (sop/transform \
     :translate [0.0 %g 0.0]))))"
    y

let settles = ref 0
and releases = ref 0
and fail = ref false

(* every output reads as 42s: a device result is recognisable, a CPU one is not *)
let backend : Flow_ir.Gpu.backend =
  {
    cost = (fun ~fast:_ _ ~count:_ -> Some 0.);
    prepare =
      (fun ~fast:_ _ ->
        Ok
          {
            run =
              (fun inputs ->
                Ok
                  {
                    identity = 1;
                    count = inputs.count;
                    width = 1;
                    output = 0;
                    stamp = 0L;
                    gpu_seconds = None;
                  });
            readback = (fun _ -> Error (Diagnostic.error ~code:"E_GPU" "no readback"));
            planes =
              (fun (v : Flow_ir.Gpu.value) ->
                Ok
                  {
                    count = v.count;
                    width = v.width;
                    materialise = (fun () -> Array.init v.width (fun _ -> Array.make v.count 42.));
                    resident = None;
                    settle =
                      (fun () ->
                        incr settles;
                        if !fail then Error (Diagnostic.error ~code:"E_KERNEL" "mock status")
                        else Ok ());
                    release = (fun ~held:_ -> incr releases);
                  });
          });
  }

let gpu = { Flow_ir.Gpu.backend = Some backend; fast = true; policy = Qualification }

let node ?gpu y =
  let lowered = Flow_sop.Lower.workspace ?gpu (Flow.Syntax.parse (text y) |> ok) |> ok in
  let graph = List.hd lowered.graphs in
  Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)
  |> Result.get_ok

let cook session node =
  match
    Sop.Session.cook session ~context:(Sop.Context.create ~time:0. () |> Result.get_ok) node
  with
  | Ok output -> Result.get_ok (Sop.Payload.geometry output.payload)
  | Error d -> failwith (Diagnostic.error_to_string d)

let positions g =
  Array.init (Rdk.Geometry.point_count g) (Rdk.Packed.Float3.get (Rdk.Geometry.positions g))

let () =
  let session () =
    Sop.Session.create ~max_entries:2 ~max_payload_bytes:(1 lsl 24) |> Result.get_ok
  in
  let cpu = positions (cook (session ()) (node 1.0)) in
  (* placed and settled: the device's values *)
  let s = session () in
  let placed = cook s (node ~gpu 1.0) in
  assert (!settles = 1 && Rdk.Packed.Float3.resident (Rdk.Geometry.positions placed));
  assert (Array.for_all (fun p -> p = (42., 42., 42.)) (positions placed));
  (* a toggled edit evicts the other write: its outputs are released once neither the cook's root
     nor the previous cook's holds them *)
  ignore (cook s (node ~gpu 2.0));
  assert (!releases = 0);
  ignore (cook s (node ~gpu 1.0));
  assert (!releases > 0);
  (* a failed status: the write recooks on the CPU, settled once, never released twice *)
  fail := true;
  settles := 0;
  let recooked = cook (session ()) (node ~gpu 1.0) in
  assert (!settles = 1);
  assert (not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions recooked)));
  assert (positions recooked = cpu);
  print_endline
    "Settle: in-flight device writes settle at a cook's end, recook on the CPU on failure"
