(* Frozen-workspace residency/parity and CPU/device cook-cost probe.
   Optional native presentation; no source rewrite. *)
module E = Flow.Eval
module Gpu = Flow_native.Workspace_gpu
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let string_ok = function Ok x -> x | Error s -> failwith s

let () =
  Printexc.record_backtrace true;
  Printexc.register_printer (function Diagnostic.Error d -> Some (Diagnostic.to_string d) | _ -> None);
  let started = Unix.gettimeofday () and allocated = Gc.allocated_bytes () in
  let publication_only = ref false and cpu = ref false and present = ref false and resolution = ref None
  and domains = ref 1 and repeat = ref 1
  and file = ref "sketches/isogrid.rays" in
  Arg.parse
    [ ("--publication-only", Arg.Set publication_only, "skip the explicit CPU parity read");
      ("--cpu", Arg.Set cpu, "measure the strict CPU route instead of residency");
      ("--present", Arg.Set present, "render each cook through a synchronized 1280x900 native canvas");
      ("--domains", Arg.Set_int domains, "CPU domains for both routes");
      ("--repeat", Arg.Set_int repeat, "uncached cooks on one host, including cold and warm");
      ("--resolution", Arg.Int (fun n -> resolution := Some n), "in-memory volume resolution") ]
    (fun path -> file := path) "bench_isogrid_resident [FILE] [--resolution N] [--publication-only] [--cpu] [--domains N] [--repeat N] [--present]";
  if !domains < 1 || !repeat < 1 then invalid_arg "domains and repeat must be positive";
  let file = !file and publication_only = !publication_only in
  let text = In_channel.with_open_bin file In_channel.input_all in
  assert (Editor_document.Lowering.sha256 text =
          "75686b00542de41aba26b5ff4cc63f88805924f4a625e8ad44c033d66664eeda");
  let workspace = match Rays_editor.Workspace.load text with
    | Ok workspace -> workspace
    | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds)) in
  let workspace = match !resolution with
    | None -> workspace
    | Some resolution ->
        let value = List.hd (ok (Flow.Syntax.parse (string_of_int resolution))) in
        ok (Editor_document.Workspace_doc.edit (ok (Rays_editor.workspace_catalog ())) workspace
          (Flow_graph.Flow_edit.Set_input_default { form = "volume"; input = "resolution"; value }))
  in
  let owner = Gpu.create () in
  let canvas = if !present then Some (Rays.Canvas.create_exn ~width:1280 ~height:900) else None in
  Gpu.qualification owner;
  Fun.protect ~finally:(fun () ->
      Option.iter (fun canvas -> string_ok (Rays.Canvas.destroy canvas)) canvas;
      Gpu.close owner) (fun () ->
      Gpu.with_backend ~fast:true owner (fun base_config ->
          let stats () = let host, _, _, _, _ = Gpu.image_stats owner in host in
          let names = [| "invoke"; "fill"; "range"; "compact"; "coerce"; "count";
            "finish"; "settle"; "close"; "retain_uint32"; "retain_float"; "validate_topology" |] in
          let calls = Array.make (Array.length names) 0
          and seconds = Array.make (Array.length names) 0. in
          let time i run =
            calls.(i) <- calls.(i) + 1;
            let start = Unix.gettimeofday () in
            Fun.protect run ~finally:(fun () ->
              seconds.(i) <- seconds.(i) +. Unix.gettimeofday () -. start)
          in
          let backend = Option.get base_config.backend in
          let backend = { backend with Flow_ir.Gpu.residency = (fun ~fast ->
            Option.map (fun (scope : Flow_ir.Gpu.residency) ->
              let r = scope.executor in
              { Flow_ir.Gpu.executor = {
                  invoke = (fun p v ~locals ~live -> time 0 (fun () -> r.invoke p v ~locals ~live));
                  fill = (fun ty ~length v -> time 1 (fun () -> r.fill ty ~length v));
                  range = (fun ty v ~div ~length -> time 2 (fun () -> r.range ty v ~div ~length));
                  compact = (fun v -> time 3 (fun () -> r.compact v));
                  coerce = (fun ty v -> time 4 (fun () -> r.coerce ty v));
                  count = (fun v -> time 5 (fun () -> r.count v));
                  finish = (fun v -> time 6 (fun () -> r.finish v));
                  capacity = r.capacity;
                  settle = (fun () -> time 7 r.settle);
                  close = (fun () -> time 8 r.close);
                };
                retain_uint32 = (fun v -> time 9 (fun () -> scope.retain_uint32 v));
                retain_float = (fun v -> time 10 (fun () -> scope.retain_float v));
                validate_topology = (fun ~point_count ~vertices ~offsets ~kinds ~max_polygon_size ->
                  time 11 (fun () -> scope.validate_topology ~point_count ~vertices ~offsets ~kinds ~max_polygon_size));
              }) (backend.residency ~fast)) } in
          let config = if !cpu then { Flow_ir.Gpu.default with fast = true }
            else Flow_ir.Gpu.{ backend = Some backend; fast = true; policy = Qualification } in
          let lowered = ok (Flow_sop.Lower.of_checked ~gpu:config ~defer:true
                              ~inputs:workspace.inputs workspace.checked) in
          let graph = List.find (fun (g : Flow_sop.Lower.graph) -> g.default && g.name = "sculpture") lowered.graphs in
          let node = Result.get_ok (Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)) in
          let session = Result.get_ok (Sop.Session.create
              ~max_entries:(if !repeat = 1 then 16 else 0) ~max_payload_bytes:(1 lsl 28)) in
          Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () ->
              let cook iteration =
                let began = Unix.gettimeofday () and allocated_cook = Gc.allocated_bytes () in
                let output = ok (Sop.Session.cook session
                    ~context:(Result.get_ok (Sop.Context.create ~domains:!domains ())) node) in
                let cook_seconds = Unix.gettimeofday () -. began
                and cook_allocated = Gc.allocated_bytes () -. allocated_cook in
                let geometry = ok (Sop.Payload.geometry output.payload) in
                Option.iter (fun canvas ->
                  let before = stats ()
                  and renderer_before = string_ok (Rays.Canvas.Private.native_stats canvas) in
                  let surface = string_ok (Rdk_rays.Surface.of_geometry geometry) in
                  let scene = Rdk_rays.Surface.scene3 ~device:(Gpu.display owner) surface in
                  let mesh = (List.hd (Rays.Scene.D3.Walk.drawings scene)).mesh in
                  assert (Option.is_some (Rays.Mesh.View.device mesh) = not !cpu);
                  if not !cpu then begin
                    assert (Rays.Mesh.View.device_color_width mesh = 3);
                    assert (Rays.Mesh.View.device_index_count mesh =
                      Some (Rdk.Geometry.primitive_count geometry * 6))
                  end;
                  let camera = Rays.Camera.perspective ~fov_y:(36. *. Float.pi /. 180.)
                      ~at:(Rays_math.Vec3.create 10. 8. 10.) ~target:Rays_math.Vec3.zero () in
                  string_ok (Rays.Canvas.render canvas
                      Rays.Scene.[ clear Rays_math.Color.black; view3d ~camera scene ]);
                  ignore (string_ok (Rays.Canvas.pixel canvas ~x:0 ~y:0));
                  let presented = Unix.gettimeofday () -. began in
                  let presented_allocated = Gc.allocated_bytes () -. allocated_cook in
                  let after = stats ()
                  and renderer_after = string_ok (Rays.Canvas.Private.native_stats canvas) in
                  let uploaded = Int64.sub renderer_after.uploaded_bytes renderer_before.uploaded_bytes in
                  assert (after.payload_readback_bytes = before.payload_readback_bytes);
                  if not !cpu then assert (uploaded < 65536L);
                  Printf.printf "chain_present route=%s iteration=%d cook_to_pixel_seconds=%.6f main_allocated_bytes=%.0f renderer_upload_bytes=%Ld payload_delta=%d validation_delta=%d bounds_delta=%d\n%!"
                    (if !cpu then "cpu" else "qualification_resident") iteration presented presented_allocated uploaded
                    (after.payload_readback_bytes - before.payload_readback_bytes)
                    (after.validation_readback_bytes - before.validation_readback_bytes)
                    (after.bounds_readback_bytes - before.bounds_readback_bytes)) canvas;
                Printf.printf "chain_route=%s iteration=%d domains=%d cook_seconds=%.6f cook_allocated_bytes=%.0f\n%!"
                  (if !cpu then "cpu" else "qualification_resident") iteration !domains
                  cook_seconds cook_allocated;
                geometry
              in
              let geometry = ref (cook 0) in
              for iteration = 1 to !repeat - 1 do
                (* This probe relinquishes each prior result before measuring the
                   next; immutable overlapping generations have their own tests. *)
                Gpu.frame ~keep:[] owner;
                Gpu.frame owner;
                Gpu.frame owner;
                List.iter (fun (_, release) -> release ~held:false) (Rdk.Geometry.releasers !geometry);
                geometry := cook iteration
              done;
              Option.iter (fun canvas ->
                assert (Array.exists (fun (c : Rays_math.Color.t) -> c.r + c.g + c.b > 0)
                  (string_ok (Rays.Canvas.pixels canvas)))) canvas;
              let geometry = !geometry in
              let stats = stats () in
              assert (stats.payload_readback_bytes = 0
                && stats.validation_readback_bytes = (if !cpu then 0 else
                  (36 + if !present then 4 else 0) * !repeat));
              assert (Rdk.Topology.resident (Rdk.Geometry.topology geometry) = not !cpu);
              assert (Rdk.Packed.Float3.resident (Rdk.Geometry.positions geometry) = not !cpu);
              let resident name = Rdk.Attribute.resident (Option.get (Rdk.Geometry.find_attribute
                  ~owner:Rdk.Attribute.Point name geometry)) in
              assert (resident "N" = not !cpu && resident "Cd" = not !cpu);
              Printf.printf "chain_totals iterations=%d upload_bytes=%d\n%!" !repeat stats.input_uploaded_bytes;
              Array.iteri (fun i name -> Printf.printf "chain_callback=%s calls=%d seconds=%.6f\n%!"
                name calls.(i) seconds.(i)) names;
              assert ((Array.fold_left ( + ) 0 calls > 0) = not !cpu);
              Printf.printf "points=%d primitives=%d topology_resident=%b positions_resident=%b N_resident=%b Cd_resident=%b payload=%d validation=%d status=%d count=%d bounds=%d\n%!"
                (Rdk.Geometry.point_count geometry) (Rdk.Geometry.primitive_count geometry)
                (Rdk.Topology.resident (Rdk.Geometry.topology geometry))
                (Rdk.Packed.Float3.resident (Rdk.Geometry.positions geometry))
                (resident "N") (resident "Cd")
                stats.payload_readback_bytes stats.validation_readback_bytes stats.status_readback_bytes
                stats.count_readback_bytes stats.bounds_readback_bytes;
              Printf.printf "publication_seconds=%.6f main_allocated_bytes=%.0f cook_domains=%d iterations=%d\n%!"
                (Unix.gettimeofday () -. started) (Gc.allocated_bytes () -. allocated) !domains !repeat;
              if not publication_only then begin
              let nodes = Array.to_list lowered.evaluated.plan.nodes in
              let mesh = List.find (fun (n : E.node) -> n.kind = "sop/mesh") nodes in
              let attribute name = List.find (fun (n : E.node) ->
                  n.kind = "sop/with_attr" && List.assoc_opt "attribute" n.args = Some (E.Text name)) nodes
                  |> fun n -> List.assoc "values" n.args in
              let expected = match ok (E.Host.force_reference
                    (E.Record (mesh.args @ [ "N", attribute "N"; "Cd", attribute "Cd" ]))
                    ~live:(Input.at_time 0.)) with E.Record fields -> fields | _ -> assert false in
              let verify () =
                let vector name actual =
                  let values = match List.assoc name expected with E.Vec3_array a -> a | _ -> assert false in
                  let p = Rdk.Packed.Float3.View.view actual in
                  assert (Array.length values = Array.length p.x * 3);
                  Array.iteri (fun c plane -> Array.iteri (fun i value ->
                      assert (Float.abs (value -. values.(3 * i + c)) <= 1e-6)) plane) [| p.x; p.y; p.z |]
                in
                vector "points" (Rdk.Geometry.positions geometry);
                List.iter (fun name -> match Rdk.Attribute.storage (Option.get
                    (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point name geometry)) with
                    | Rdk.Attribute.Float3 p -> vector name p | _ -> assert false) [ "N"; "Cd" ];
                let topology = Rdk.Topology.View.view (Rdk.Geometry.topology geometry) in
                List.iter (fun (name, actual) ->
                    match List.assoc name expected with E.Int_array a -> assert (a = actual) | _ -> assert false)
                  [ "vertex_points", topology.vertex_points; "primitive_offsets", topology.primitive_offsets;
                    "kinds", Array.init (Bytes.length topology.primitive_kinds) (fun i -> Char.code (Bytes.get topology.primitive_kinds i)) ]
              in
              verify ();
              let current_stats () = let host, _, _, _, _ = Gpu.image_stats owner in host in
              let bytes = (current_stats ()).payload_readback_bytes in
              assert (bytes = if !cpu then 0 else
                    36 * Rdk.Geometry.point_count geometry
                    + 4 * (Rdk.Geometry.vertex_count geometry + 2 * Rdk.Geometry.primitive_count geometry + 1));
              verify ();
              assert ((current_stats ()).payload_readback_bytes = bytes);
              Printf.printf "CPU parity: topology exact; P/N/Cd <=1e-6; first-read payload=%d; repeated-read delta=0\n%!" bytes
              end)))
