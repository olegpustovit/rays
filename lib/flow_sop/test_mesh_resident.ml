module H = Flow_gpu.Host
module G = Rdk.Geometry

let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let string_ok = function Ok x -> x | Error e -> failwith e

let source ~host case =
  let domain = if case = "overflow" then "selected" else "(array/range count :type int)" in
  let vertex = match case with
    | "vertex" -> "(if (int/eq i 1025) 1026 i)"
    | "negative" -> "(if (int/eq i 1025) -1 i)"
    | _ -> "i" in
  let offset = match case with
    | "start" -> "(if (int/eq i 0) 1 (int/mul i 3))"
    | "terminal" -> "(if (int/eq i n) (int/sub count 1) (int/mul i 3))"
    | "order" -> "(if (int/eq i 1) 9 (int/mul i 3))"
    | "minimum" -> "(if (int/eq i 1) 2 (int/mul i 3))"
    | _ -> "(int/mul i 3)" in
  let vertices = if case = "points" then
      "(array/range (int/mod (frame/index) 342) :type int)"
    else Printf.sprintf
      "(map (fn [(i : int)] (int/add %s (int/mod (frame/index) 1))) %s)" vertex domain in
  let kinds = match case with
    | "points" -> "(array/int (int/mod (frame/index) 342) 0)"
    | "length" -> "(array/int (int/sub n 1) 0)"
    | "kind" -> "(map (fn [(i : int)] (if (int/eq i 0) 3 0)) (array/range n :type int))"
    | _ -> "(array/int n 0)" in
  let offsets = if case = "points" then
      "(array/range (int/add (int/mod (frame/index) 342) 1) :type int)"
    else if case = "missing" then
      "(filter (fn [(i : int)] (int/lt i (int/mod (frame/index) 342))) (array/range (int/add n 1) :type int))"
    else Printf.sprintf
      "(map (fn [(i : int)] (int/add %s (int/mod (frame/index) 1))) (array/range (int/add n 1) :type int))" offset in
  let body = Printf.sprintf {|(let* [n (frame/index)
           count (int/mul n 3)
           %s
           points (map (fn [(i : int)] [(+ (float i) t) 0. 0.]) %s)
           vertices %s
           offsets %s
           kinds %s
           mesh (sop/mesh :points (exact points) :vertex_points (exact vertices)
                :primitive_offsets (exact offsets) :kinds (exact kinds))]
      %s)|}
    (if case = "overflow" then
       "selected (filter (fn [(i : int)] (int/lt i count)) (array/range 8196 :type int))" else "")
    domain vertices offsets kinds
    (if case = "cpu" then
       "(sop/clean mesh :remove_degenerate false :remove_nan_points false :remove_unused_points false :reverse_winding true)"
     else if case = "twice" then
       "(sop/mesh :points (if (int/eq (sop/count mesh :points) count) (exact points) (error \"E_TEST\" \"first mesh count\")) :vertex_points (exact vertices) :primitive_offsets (exact offsets) :kinds (exact kinds))"
     else "mesh") in
  let body = if host then Printf.sprintf
      "(if (int/gt (sop/count (sop/mesh :points (array/vec3 1) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)) :points) 0) %s (sop/mesh :points (array/vec3 0) :vertex_points (array/int 0) :primitive_offsets (array/int 1) :kinds (array/int 0)))" body
    else body in
  "(workspace resident_mesh (graph g :context sop " ^ body ^ "))"

let () =
  Printexc.record_backtrace true;
  Printexc.register_printer (function Diagnostic.Error d -> Some (Diagnostic.to_string d) | _ -> None);
  let cases = [| "vertex"; "negative"; "missing"; "start"; "terminal"; "order"; "minimum"; "length"; "kind"; "dynamic"; "cpu"; "cancel"; "host"; "host_twice"; "host_cpu"; "host_cancel"; "host_vertex"; "host_points"; "host_negative"; "host_dynamic"; "overflow"; "host_overflow" |] in
  for attempt = 1 to 20 + Array.length cases do
  let test = if attempt <= 20 then "valid" else cases.(attempt - 21) in
  let host_call, case = match test with
    | "host" -> true, "valid" | "host_twice" -> true, "twice"
    | "host_cpu" -> true, "cpu" | "host_cancel" -> true, "cancel"
    | "host_negative" -> true, "negative" | "host_dynamic" -> true, "dynamic"
    | "host_overflow" -> true, "overflow"
    | "host_vertex" -> true, "vertex" | "host_points" -> true, "points" | case -> false, case in
  let bad = not (List.mem case [ "valid"; "dynamic"; "cpu"; "cancel"; "twice"; "points"; "overflow" ]) in
  let baseline = Ogpu.Impl.live_handles () in
  let gpu = match Renderer.acquire_gpu () with
    | Ok gpu -> gpu | Error e -> failwith (Format.asprintf "%a" Renderer.pp_error e) in
  Fun.protect ~finally:(fun () -> Renderer.release_gpu gpu) (fun () ->
      let host = H.create ~clock:(fun () -> 0.) gpu in
      Fun.protect ~finally:(fun () -> H.close host) (fun () ->
          let cancel = Rdk.Cancel.create () in
          let device_closes = ref 0 in
          let backend = H.backend host in
          if attempt = 1 then begin
            let scope = Option.get (backend.residency ~fast:true) in
            Fun.protect ~finally:scope.executor.close (fun () ->
                let offset = Option.get (scope.executor.fill (Flow.Ty.Array Flow.Ty.Int)
                    ~length:1 (Flow.Eval.Int 1)) in
                let result = Flow_sop.Topology_kernel_impl.mesh_resident scope
                    [ "points", Flow.Eval.Vec3_array [||];
                      "vertex_points", Flow.Eval.Int_array [||];
                      "primitive_offsets", offset; "kinds", Flow.Eval.Int_array [||] ] in
                (match result with
                | Some (Error d) ->
                    assert (d.code = "E_TOPOLOGY"
                      && d.message = "Topology: first primitive offset must be zero")
                | _ -> failwith "malformed resident empty offset was accepted or drained");
                assert ((H.stats host).payload_readback_bytes = 0
                  && (H.stats host).validation_readback_bytes = 36))
          end;
          let backend = { backend with residency = (fun ~fast -> Option.map
                  (fun (scope : Flow_ir.Gpu.residency) ->
                    let dispatched = ref false in
                    { scope with executor = { scope.executor with
                        invoke = (fun packed args ~locals ~live ->
                          let value = scope.executor.invoke packed args ~locals ~live in
                          if value <> None then dispatched := true;
                          (match value with
                          | Some (Flow.Eval.Float_array a | Vec2_array a | Vec3_array a | Vec4_array a) ->
                              Array.fill a 0 (Array.length a) nan
                          | _ -> ());
                          value);
                        settle = (fun () -> scope.executor.settle ();
                          if case = "cancel" && !dispatched then Rdk.Cancel.cancel cancel);
                        count = (fun value ->
                          let count = scope.executor.count value in
                          if case = "cancel" && !dispatched && count <> None then Rdk.Cancel.cancel cancel;
                          count);
                        close = (fun () -> if !dispatched then incr device_closes; scope.executor.close ()) } })
                  (backend.residency ~fast)) } in
          let config = Flow_ir.Gpu.{ backend = Some backend; fast = true; policy = Qualification; readback_per_float = None } in
          let lowered = ok (Flow_sop.Lower.workspace ~defer:true ~gpu:config (ok (Flow.Syntax.parse (source ~host:host_call case)))) in
          let graph = List.hd lowered.graphs in
          let resolved = ok (Flow_sop.Value_lane.resolve ~gpu:config (Flow_sop.Value_lane.create ())
                               ~time:0. graph.network) in
          let node = string_ok (Sop.Edit_graph.compile_node resolved.geometry ~node_id:(Option.get graph.root)) in
          if host_call then assert (Sop.Node.operation node = "flow.geometry_select");
          let session = string_ok (Sop.Session.create ~max_entries:4 ~max_payload_bytes:(1 lsl 24)) in
          Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
              let held = ref [] in
              let replays = ref 0 in
              let read (geometry, count) =
                let t = Rdk.Topology.View.view (G.topology geometry) in
                let primitives = if case = "points" then 0 else count in
                assert (t.vertex_points = Array.init (3 * primitives)
                    (if case = "cpu" then (fun i -> 3 * (i / 3) + 2 - i mod 3) else Fun.id));
                assert (t.primitive_offsets = Array.init (primitives + 1) (fun i -> 3 * i));
                assert (t.primitive_kinds = Bytes.make primitives '\000');
                for i = 0 to 3 * count - 1 do
                  assert (Rdk.Packed.Float3.get (G.positions geometry) i = (float i, 0., 0.))
                done in
              List.iter (fun count ->
              Sop.Session.clear session;
              let before = H.stats host in
              let closed = !device_closes in
              let context = string_ok (Sop.Context.create ~cancel ~domains:1 ~time:0. ~frame:(Int64.of_int count) ()) in
              let result = Sop.Session.cook session ~context node in
              let result = if case <> "cancel" then result else begin
                  assert (Rdk.Cancel.is_cancelled cancel);
                  (match result with
                  | Error d -> assert (d.code = "E_CANCELLED" || d.code = "cancelled")
                  | Ok _ -> failwith "Cancelled resident cook published geometry");
                  assert (Sop.Session.cached session ~context node = None);
                  assert ((H.stats host).payload_readback_bytes = 0);
                  assert ((H.stats host).validation_readback_bytes = 0);
                  Sop.Session.cook session
                    ~context:(string_ok (Sop.Context.create ~domains:1 ~time:0.
                        ~frame:(Int64.of_int count) ())) node
                end in
              if bad then begin
                (match result with
                | Error d ->
                    let expected = if case = "kind" then "E_TOPOLOGY_KIND" else "E_TOPOLOGY" in
                    let message = match case with
                      | "vertex" | "negative" -> "Topology: vertex references an invalid point"
                      | "missing" -> "Topology: primitive offsets must contain the initial zero"
                      | "start" -> "Topology: first primitive offset must be zero"
                      | "terminal" -> "Topology: final primitive offset must equal vertex count"
                      | "order" -> "Topology: primitive offsets are not ordered"
                      | "minimum" -> "Topology: primitive has too few vertices for its kind"
                      | "length" -> "Topology: primitive kind count must match primitive count"
                      | "kind" -> "Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline)."
                      | _ -> assert false in
                    if d.code <> expected || d.message <> message then
                      failwith (case ^ ": " ^ Diagnostic.to_string d)
                | Ok _ -> failwith "Malformed GPU mesh was published");
                assert (Sop.Session.cached session ~context node = None);
                let stats = H.stats host in
                let payload, validation = match case with
                  | "negative" -> 0, 0 | _ -> 0, 36 in
                if stats.payload_readback_bytes <> payload || stats.validation_readback_bytes <> validation then
                  failwith (Printf.sprintf "%s: payload=%d validation=%d" case
                      stats.payload_readback_bytes stats.validation_readback_bytes)
              end
              else begin
              let output = match result with
                | Ok output -> output | Error d -> failwith (Printf.sprintf "attempt=%d primitives=%d: %s"
                    attempt count (Diagnostic.error_to_string d)) in
              let geometry = ok (Sop.Payload.geometry output.payload) in
              replays := !replays + max 0 (!device_closes - closed - 1);
              let stats = H.stats host in
              let payload = stats.payload_readback_bytes - before.payload_readback_bytes in
              if payload <> (if case = "cpu" then 19156 else 0) then
                failwith (Printf.sprintf "%s: publication payload=%d validation=%d" case payload
                    (stats.validation_readback_bytes - before.validation_readback_bytes));
              let validation = stats.validation_readback_bytes - before.validation_readback_bytes in
              let expected = if count = 0 then 0 else if case = "twice" then 72 else 36 in
              if validation <> expected && not (count = 0 && validation = 36) then failwith (Printf.sprintf "%s count=%d validation=%d expected=%d"
                  test count validation expected);
              assert (G.point_count geometry = 3 * count
                  && G.primitive_count geometry = if case = "points" then 0 else count);
              if count > 0 && case <> "cpu" then begin
                assert (Rdk.Topology.resident (G.topology geometry));
                assert (Rdk.Packed.Float3.resident (G.positions geometry));
                let vertices, offsets, kinds = Option.get (Flow_sop.Topology_kernel.resident geometry) in
                List.iter (fun (stream : Flow_ir.Gpu.integers) ->
                  if stream.count > 0 then begin
                    let _, n, valid, unpin = Option.get (Flow_gpu.Run.pin_integers stream) in
                    assert (n = stream.count && valid ());
                    unpin ();
                    assert (not (valid ()))
                  end) [ vertices; offsets; kinds ];
                assert ((H.stats host).payload_readback_bytes = stats.payload_readback_bytes)
              end;
              Gc.full_major ();
              held := (geometry, count) :: !held
              end) (if List.mem case [ "dynamic"; "overflow" ] then [ 0; 342; 683; 1025; 0; 342 ] else [ 342 ]);
              if case = "overflow" then assert (!replays > 0);
              Parallel.run ~domains:4 (fun () -> Parallel.for_ ~chunk_size:1 ~start:0 ~finish:7
                  (fun _ -> List.iter read !held));
              let bytes = (H.stats host).payload_readback_bytes in
              List.iter read !held;
              assert ((H.stats host).payload_readback_bytes = bytes);
              if case = "overflow" then Printf.printf "Resident counted mesh (%s): %d capacity replays, zero publication payload, old/new CPU arrays exact\n%!"
                  (if host_call then "host" else "direct") !replays)));
  assert (Ogpu.Impl.live_handles () = baseline)
  done;
  print_endline "Resident mesh: validated deferred publication, concurrent CPU reads and bounded ownership"
