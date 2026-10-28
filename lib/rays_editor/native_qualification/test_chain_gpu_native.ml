(* On the real device: grid 1000 x 1000 Points (GRID_SIDE; 512 in @runtest-native, the smallest
   grid the cost model places) > attribute_noise > transform, the fused write of noise,
   P and N, cooked on the CPU and with the workspace GPU in fast precision (Executor.record_on_device).
   The device cook takes at most two dispatches, a recook uploads nothing and reads nothing back
   (its attributes stay in device buffers), a CPU read of P materialises exactly that one output,
   its noise and P agree with the CPU within float32 tolerance and N exactly (a translation keeps
   it). Two consecutive fused writes run on the device with nothing uploaded or read back on a
   recook (the second binds the first's resident P), and reading the second's P materialises one
   output. Once the geometries are collected no buffer is held. CPU and GPU cook times are
   printed, median of 5. One session toggles an edit 20 times (two lowerings): what the session
   drops goes back to the device at once (held buffers stay bounded and no buffer is created once
   warm, with no collection), nothing is uploaded or read back, and the median recook is printed. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000

let text =
  Printf.sprintf
    "(workspace chain (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \
     \"Points\" :columns %d :rows %d) (sop/attribute_noise :name \"noise\" :seed 7) (sop/transform \
     :translate [0.0 1.0 0.0]))))"
    side side

(* Two fused writes, the second over the first ([a] is read twice, so they do not fold into one):
   the second binds the first's resident P where the device holds it. *)
let consecutive =
  Printf.sprintf
    "(workspace chain (graph g :context sop (let* [a (-> (sop/grid :counts \"Point counts\" \
     :connectivity \"Points\" :columns %d :rows %d) (sop/attribute_noise :name \"noise\" :seed 7) \
     (sop/transform :translate [0.0 1.0 0.0])) b (-> a (sop/attribute_noise :name \"n2\" :seed 3) \
     (sop/transform :translate [1.0 0.0 0.0]))] (sop/merge b (sop/attribute_noise a :name \
     \"n2\")))))"
    side side

(* the root, or with [~after] the first write of N after the first write of [after]: the last
   write of the chain that starts there, which the lowering fuses *)
let node ?gpu ?(text = text) ?after () =
  let lowered = Flow_sop.Lower.workspace ?gpu (Flow.Syntax.parse text |> ok) |> ok in
  let graph = List.hd lowered.graphs in
  let node_id =
    match after with
    | None -> Option.get graph.root
    | Some name ->
        let write name (n : Flow.Eval.node) =
          n.kind = "sop/with_attr" && List.assoc_opt "attribute" n.args = Some (Text name)
        in
        let first = Array.find_opt (write name) lowered.plan.nodes |> Option.get in
        let last =
          Array.find_opt
            (fun (n : Flow.Eval.node) -> n.id > first.id && write "N" n)
            lowered.plan.nodes
          |> Option.get
        in
        Flow_sop.Network.Int_map.find last.id lowered.compiled
  in
  Sop.Edit_graph.compile_node graph.network.geometry ~node_id |> Result.get_ok

let cook_in session node =
  let start = Unix.gettimeofday () in
  match
    Sop.Session.cook session ~context:(Sop.Context.create ~time:0. () |> Result.get_ok) node
  with
  | Ok output -> (Result.get_ok (Sop.Payload.geometry output.payload), Unix.gettimeofday () -. start)
  | Error d -> failwith (Diagnostic.error_to_string d)

let cook node =
  let session = Sop.Session.create ~max_entries:4 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      let start = Unix.gettimeofday () in
      match
        Sop.Session.cook session ~context:(Sop.Context.create ~time:0. () |> Result.get_ok) node
      with
      | Ok output ->
          (Result.get_ok (Sop.Payload.geometry output.payload), Unix.gettimeofday () -. start)
      | Error d -> failwith (Diagnostic.error_to_string d))

let median f =
  let runs = List.init 5 (fun _ -> f ()) in
  (List.nth (List.sort compare (List.map snd runs)) 2, fst (List.hd runs))

let noise geometry =
  match Rdk.Geometry.find_attribute ~owner:Point "noise" geometry with
  | Some attribute -> (
      match Rdk.Attribute.View.storage attribute with
      | Rdk.Attribute.Float data -> data
      | _ -> failwith "noise is not float storage")
  | None -> failwith "no noise attribute"

let normals geometry =
  match Rdk.Geometry.find_attribute ~owner:Point "N" geometry with
  | Some attribute -> (
      match Rdk.Attribute.View.storage attribute with
      | Rdk.Attribute.Float3 data -> data
      | _ -> failwith "N is not vec3 storage")
  | None -> failwith "no N attribute"

let () =
  let count = side * side in
  let unplaced = node () in
  let cpu_seconds, cpu = median (fun () -> cook unplaced) in
  let gpu = Flow_native.Workspace_gpu.create () in
  Fun.protect
    ~finally:(fun () -> Flow_native.Workspace_gpu.close gpu)
    (fun () ->
      Flow_native.Workspace_gpu.with_backend ~fast:true gpu (fun config ->
          let dispatches = ref 0 in
          let backend = Option.get config.Flow_ir.Gpu.backend in
          let counted : Flow_ir.Gpu.config =
            {
              config with
              backend =
                Some
                  {
                    backend with
                    prepare =
                      (fun ~fast packed ->
                        Result.map
                          (fun (kernel : Flow_ir.Gpu.kernel) ->
                            {
                              kernel with
                              run =
                                (fun inputs ->
                                  incr dispatches;
                                  kernel.run inputs);
                            })
                          (backend.prepare ~fast packed));
                  };
            }
          in
          let placed = node ~gpu:counted () in
          (* the first cook compiles the kernel and uploads every source *)
          let _, first_seconds = cook placed in
          dispatches := 0;
          let stats () =
            let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
            host
          in
          let before = stats () in
          let device, _ = cook placed in
          let per_cook = !dispatches in
          let after = stats () in
          (* a recook over unchanged geometry finds its inputs resident: the same arrays *)
          if after.input_uploads <> before.input_uploads then
            failwith
              (Printf.sprintf "the recook uploaded %d inputs"
                 (after.input_uploads - before.input_uploads));
          (* and keeps its outputs on the device: nothing is read back until a CPU reader asks *)
          if
            after.readback_bytes <> before.readback_bytes
            || after.materialisations <> before.materialisations
          then failwith "the recook read its outputs back";
          if not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions device)) then
            failwith "the recook's P is not device-resident";
          let gpu_seconds, _ = median (fun () -> cook placed) in
          let steady = stats () in
          ignore (Rdk.Packed.Float3.get (Rdk.Geometry.positions device) 0);
          if (stats ()).materialisations <> steady.materialisations + 1 then
            failwith "reading P did not materialise exactly one output";
          ignore (Rdk.Packed.Float3.get (Rdk.Geometry.positions device) 1);
          if (stats ()).materialisations <> steady.materialisations + 1 then
            failwith "P materialised twice";
          if per_cook < 1 || per_cook > 2 then
            failwith (Printf.sprintf "the chain took %d dispatches, not 1 or 2" per_cook);
          let worst = ref 0. in
          let close a b = worst := Float.max !worst (Float.abs (a -. b)) in
          let p = Rdk.Geometry.positions cpu and q = Rdk.Geometry.positions device in
          let n = noise cpu and m = noise device in
          for i = 0 to count - 1 do
            let x, y, z = Rdk.Packed.Float3.get p i and gx, gy, gz = Rdk.Packed.Float3.get q i in
            close x gx;
            close y gy;
            close z gz;
            close n.(i) m.(i)
          done;
          (* float32 rounding of positions within [-0.5, 1.5] and of noise in [0, 1] *)
          if !worst > 1e-5 then failwith (Printf.sprintf "GPU differs from CPU by %.3e" !worst);
          let normals_cpu = normals cpu and normals_gpu = normals device in
          for i = 0 to count - 1 do
            if Rdk.Packed.Float3.get normals_cpu i <> Rdk.Packed.Float3.get normals_gpu i then
              failwith "GPU N differs from CPU"
          done;
          Printf.printf
            "grid > attribute_noise > transform, %d points: CPU cook %.3f s, GPU cook %.3f s \
             (first %.3f s), %d dispatch(es), largest difference %.2e\n\
             %!"
            count cpu_seconds gpu_seconds first_seconds per_cook !worst;
          (* GPU to GPU: the second write reads the first's P where it is, a CPU reader of the
             result reads back its P alone *)
          let second = node ~gpu:counted ~text:consecutive ~after:"n2" () in
          let reference, _ = cook (node ~text:consecutive ~after:"n2" ()) in
          ignore (cook second);
          dispatches := 0;
          let before = stats () in
          let chained, _ = cook second in
          let after = stats () in
          if !dispatches <> 2 then
            failwith (Printf.sprintf "two consecutive writes took %d dispatches" !dispatches);
          if
            after.input_uploads <> before.input_uploads
            || after.readback_bytes <> before.readback_bytes
            || after.materialisations <> before.materialisations
          then
            failwith
              (Printf.sprintf "consecutive writes moved data: %d uploads, %d bytes, %d read"
                 (after.input_uploads - before.input_uploads)
                 (after.readback_bytes - before.readback_bytes)
                 (after.materialisations - before.materialisations));
          if not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions chained)) then
            failwith "the second write's P is not device-resident";
          let chained_seconds, _ = median (fun () -> cook second) in
          let steady = stats () in
          let p = Rdk.Geometry.positions reference and q = Rdk.Geometry.positions chained in
          let worst = ref 0. in
          for i = 0 to count - 1 do
            let x, y, z = Rdk.Packed.Float3.get p i and gx, gy, gz = Rdk.Packed.Float3.get q i in
            worst :=
              Float.max !worst
                (Float.max
                   (Float.abs (x -. gx))
                   (Float.max (Float.abs (y -. gy)) (Float.abs (z -. gz))))
          done;
          if (stats ()).materialisations <> steady.materialisations + 1 then
            failwith "reading the second write's P did not materialise exactly one output";
          if !worst > 1e-5 then
            failwith (Printf.sprintf "chained GPU P differs from CPU by %.3e" !worst);
          Printf.printf
            "two consecutive fused writes: GPU recook %.3f s, largest difference %.2e\n%!"
            chained_seconds !worst;
          (* one session toggling an edit (two lowerings, translations 1 and 2): the grid hits,
             each cook evicts the other write, whose buffers come back without a collection *)
          let session =
            Sop.Session.create ~max_entries:2 ~max_payload_bytes:(1 lsl 30) |> Result.get_ok
          in
          Fun.protect
            ~finally:(fun () -> Sop.Session.close session)
            (fun () ->
              let edit y =
                node ~gpu:counted
                  ~text:
                    (Printf.sprintf
                       "(workspace chain (graph g :context sop (-> (sop/grid :counts \"Point \
                        counts\" :connectivity \"Points\" :columns %d :rows %d) \
                        (sop/attribute_noise :name \"noise\" :seed 7) (sop/transform :translate \
                        [0.0 %g 0.0]))))"
                       side side y)
                  ()
              in
              let a = edit 1.0 and b = edit 2.0 in
              let toggles = List.init 32 (fun i -> if i mod 2 = 0 then a else b) in
              (* warm: the pool fills until retired results are read back to bound it *)
              List.iter (fun n -> ignore (cook_in session n)) (List.take 16 toggles);
              Gc.full_major ();
              let before = stats () in
              dispatches := 0;
              let cooked =
                List.map
                  (fun n ->
                    let geometry, seconds = cook_in session n in
                    if not (Rdk.Packed.Float3.resident (Rdk.Geometry.positions geometry)) then
                      failwith "the edited chain's P is not device-resident";
                    (geometry, seconds))
                  (List.drop 16 toggles)
              in
              let after = stats () in
              if !dispatches <> 16 then
                failwith (Printf.sprintf "16 edited recooks took %d dispatches" !dispatches);
              (* the results are all kept (read below), so new buffers are made only until 12 are
                 retired; then each cook reads the oldest back and reuses its buffer (48 without the
                 pool) *)
              if after.buffer_creations - before.buffer_creations > 12 then
                failwith
                  (Printf.sprintf "edited recooks created %d buffers"
                     (after.buffer_creations - before.buffer_creations));
              if
                after.readback_bytes <> before.readback_bytes
                || after.input_uploads <> before.input_uploads
              then failwith "edited recooks moved data";
              (* bounded with no collection: 12 retired, a cook's 3, two roots' and two entries' *)
              if after.held_buffers > 30 then
                failwith (Printf.sprintf "edited recooks hold %d buffers" after.held_buffers);
              (* a result handed out 15 cooks ago, retired and possibly read back to bound the
                 pool, reads every attribute as the latest cook of the same lowering does *)
              let stale = fst (List.hd cooked) and latest = fst (List.nth cooked 14) in
              let read g =
                ( Array.init (Rdk.Geometry.point_count g)
                    (Rdk.Packed.Float3.get (Rdk.Geometry.positions g)),
                  List.map
                    (fun at ->
                      ( Rdk.Attribute.name at,
                        match Rdk.Attribute.View.storage at with
                        | Rdk.Attribute.Float v -> Array.map (fun x -> (x, 0., 0.)) v
                        | Float3 v ->
                            Array.init (Rdk.Packed.Float3.length v) (Rdk.Packed.Float3.get v)
                        | _ -> failwith "unexpected storage" ))
                    (Rdk.Geometry.attributes g) )
              in
              if read stale <> read latest then failwith "a stale result reads other values";
              let sorted = List.sort compare (List.map snd cooked) in
              Printf.printf "edited chain, one session: GPU recook median %.4f s (min %.4f)\n%!"
                (List.nth sorted 8) (List.hd sorted);
              (* every cook a new lowering (a new runner, as an edit makes): output buffers come
                 from the host's pool and stay bounded (a new runner makes its inputs' and at most a few) *)
              let fresh y =
                let creations = (stats ()).buffer_creations in
                ignore (cook_in session (edit y));
                (stats ()).buffer_creations - creations
              in
              let first = fresh 3.0 in
              let later = List.map fresh [ 4.0; 5.0; 6.0 ] in
              Printf.printf "new runners per edit: %d buffers created (the first %d), %d held\n%!"
                (List.nth later 2) first (stats ()).held_buffers;
              if List.exists (fun n -> n > 6) later || (stats ()).held_buffers > 30 then
                failwith
                  (Printf.sprintf "new runners created %s buffers, the first %d"
                     (String.concat "," (List.map string_of_int later))
                     first)));
      (* the geometries are gone: every buffer they held is back with its runner *)
      Gc.full_major ();
      let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
      if host.held_buffers <> 0 then
        failwith (Printf.sprintf "%d buffers outlive their geometry" host.held_buffers))
