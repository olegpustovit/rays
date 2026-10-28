(* On the real device: grid 1000 x 1000 Points (GRID_SIDE; 512 in @runtest-native, the smallest
   grid the cost model places) > attribute_noise > transform, the fused write of noise,
   P and N, cooked on the CPU and with the workspace GPU in fast precision (Executor.record_on_device).
   The device cook takes at most two dispatches, a recook uploads nothing, its noise and P agree with the CPU within float32
   tolerance and N exactly (a translation keeps it); CPU and GPU cook times are printed, median of 5. *)
let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let side = try int_of_string (Sys.getenv "GRID_SIDE") with Not_found -> 1000

let text =
  Printf.sprintf
    "(workspace chain (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \
     \"Points\" :columns %d :rows %d) (sop/attribute_noise :name \"noise\" :seed 7) (sop/transform \
     :translate [0.0 1.0 0.0]))))"
    side side

let node ?gpu () =
  let lowered = Flow_sop.Lower.workspace ?gpu (Flow.Syntax.parse text |> ok) |> ok in
  let graph = List.hd lowered.graphs in
  Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)
  |> Result.get_ok

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
          let uploads () =
            let host, _, _, _, _ = Flow_native.Workspace_gpu.image_stats gpu in
            host.input_uploads
          in
          let before = uploads () in
          let device, _ = cook placed in
          let per_cook = !dispatches in
          (* a recook over unchanged geometry finds its inputs resident: the same arrays *)
          if uploads () <> before then
            failwith (Printf.sprintf "the recook uploaded %d inputs" (uploads () - before));
          let gpu_seconds, _ = median (fun () -> cook placed) in
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
            count cpu_seconds gpu_seconds first_seconds per_cook !worst))
