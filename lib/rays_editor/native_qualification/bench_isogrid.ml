(* The frozen sketch, checked without a window. Goldens come from Eval.static's walker;
   the benchmark uses Editor.edit/update/scene and waits for an offscreen presented pixel.
   --write-goldens SIDE, --check-goldens FILE, --bench SIDE [--single], or no args for
   the complete 24 -> 64 -> 24 editor sequence. No source file is rewritten. *)
module E = Flow.Eval

(* These are the public Editor entry points, with the owner's private model
   visible so the retention benchmark can count its caches without a new API. *)
module Editor = struct
  include Rays_editor__.Loop_model

  let update = Rays_editor__.Editor_loop.update
  let scene = Rays_editor__.Editor_loop.scene
  let close = Rays_editor__.Editor_loop.close
  let prepare = Rays_editor__.Cook.prepare
  let draw = Rays_editor__.Cook.draw
  let gpu_stats value = Flow_native.Workspace_gpu.image_stats value.host.gpu
end

module F = Flow_graph.Flow_edit
module Doc = Editor_document.Workspace_doc

let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let string_ok = function Ok value -> value | Error message -> failwith message
let read file = In_channel.with_open_bin file In_channel.input_all
let source = "sketches/isogrid.rays"
let frozen = "75686b00542de41aba26b5ff4cc63f88805924f4a625e8ad44c033d66664eeda"
let parse text = List.hd (ok (Flow.Syntax.parse text))

module Scheduler_slots = struct
  (* Read-only qualification probe for the installed Domainslib/Saturn layout.
     Saturn leaves a two-word [ref ()] in each used task-ring slot. These empty
     cells fill lazily across edits, even though the task was cleared. Count
     exactly those cells; a payload, a changed layout or a capacity increase
     must still fail. No cache is cleared and no heap tolerance is applied. *)
  let field value index =
    if Obj.is_int value || Obj.tag value <> 0 || Obj.size value <= index then
      failwith "scheduler qualification layout changed";
    Obj.field value index

  let create () = Parallel.qualification_pools ()

  let counts table =
    let capacity = ref 0 and empty_words = ref 0 in
    let rec bucket entry =
      if Obj.is_block entry then begin
        let pooled = field entry 1 in
        let data = field (field (field pooled 0) 0) 0 in
        let channels = field (field data 1) 0 in
        for index = 0 to Obj.size channels - 1 do
          let slots = field (field channels index) 3 in
          capacity := !capacity + Obj.size slots;
          for index = 0 to Obj.size slots - 1 do
            let cell = Obj.field slots index in
            if Obj.is_block cell then begin
              if Obj.tag cell <> 0 || Obj.size cell <> 1 || field cell 0 != Obj.repr () then
                failwith "scheduler still retains a task";
              empty_words := !empty_words + 2
            end
            else if cell != Obj.repr () then failwith "scheduler slot layout changed"
          done
        done;
        bucket (field entry 2)
      end
    in
    let buckets = field table 1 in
    for index = 0 to Obj.size buckets - 1 do
      bucket (Obj.field buckets index)
    done;
    (!capacity, !empty_words)
end

module Retainers = struct
  type cell = { site : string; words : int Atomic.t }
  type t = { profile : Gc.Memprof.t; cells : (string, cell) Hashtbl.t; lock : Mutex.t }

  let start () =
    let cells = Hashtbl.create 128 and lock = Mutex.create () in
    let allocate (allocation : Gc.Memprof.allocation) =
      let site =
        Option.value ~default:[||] (Printexc.backtrace_slots allocation.callstack)
        |> Array.to_list
        |> List.filter_map Printexc.Slot.name
        |> List.take 6 |> String.concat " <- "
      in
      let cell =
        Mutex.protect lock (fun () ->
            match Hashtbl.find_opt cells site with
            | Some cell -> cell
            | None ->
                let cell = { site; words = Atomic.make 0 } in
                Hashtbl.add cells site cell;
                cell)
      in
      ignore (Atomic.fetch_and_add cell.words allocation.n_samples);
      Some (cell, allocation.n_samples)
    in
    let deallocate (cell, words) = ignore (Atomic.fetch_and_add cell.words (-words)) in
    let profile =
      Gc.Memprof.start ~sampling_rate:1e-4 ~callstack_size:16
        {
          alloc_minor = allocate;
          alloc_major = allocate;
          promote = (fun value -> Some value);
          dealloc_minor = deallocate;
          dealloc_major = deallocate;
        }
    in
    { profile; cells; lock }

  let snapshot value =
    (* Allocating under the metadata lock can invoke this profiler recursively. *)
    let capacity = Hashtbl.length value.cells + 1024 in
    let sites = Array.make capacity "" and words = Array.make capacity 0 in
    let length = ref 0 in
    let copy site cell =
      if !length < capacity then begin
        sites.(!length) <- site;
        words.(!length) <- Atomic.get cell.words;
        incr length
      end
    in
    Mutex.lock value.lock;
    Hashtbl.iter copy value.cells;
    Mutex.unlock value.lock;
    List.init !length (fun i -> (sites.(i), words.(i)))

  let report value before =
    snapshot value
    |> List.map (fun (site, words) ->
        (words - Option.value ~default:0 (List.assoc_opt site before), site))
    |> List.sort (fun (a, _) (b, _) -> Int.compare b a)
    |> List.take 20
    |> List.iter (fun (words, site) -> Printf.printf "retainer estimated_words=%d %s\n%!" (words * 10_000) site)

  let stop value =
    Gc.Memprof.stop ();
    Gc.Memprof.discard value.profile
end

let recipe value =
  F.Set_arg
    {
      node = [ "volume"; "recipe-switch" ];
      key = Whole;
      sub = [];
      value = parse (string_of_int value);
    }

let input name value =
  F.Set_input_default { form = "volume"; input = name; value = parse (string_of_int value) }

let load text =
  match Rays_editor.Workspace.load text with
  | Ok workspace -> workspace
  | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))

let variant base side preset seed =
  let catalog = ok (Rays_editor.workspace_catalog ()) in
  List.fold_left
    (fun doc edit -> ok (Doc.edit catalog doc edit))
    base
    [ input "resolution" side; recipe preset; input "seed" seed ]

(* A canonical byte stream: named fields, lengths, binary64 bits and signed int64s,
   all little endian. Both the walker plan and the cooked RDK geometry use this order. *)
let digest points vertices offsets kinds normals colours =
  let buffer = Buffer.create 1024 in
  let field name length =
    Buffer.add_string buffer name;
    Buffer.add_int64_le buffer (Int64.of_int length)
  in
  let floats name data =
    field name (Array.length data);
    Array.iter (fun f -> Buffer.add_int64_le buffer (Int64.bits_of_float f)) data
  and ints name data =
    field name (Array.length data);
    Array.iter (fun n -> Buffer.add_int64_le buffer (Int64.of_int n)) data
  in
  floats "P" points;
  ints "vertex_points" vertices;
  ints "primitive_offsets" offsets;
  ints "kinds" kinds;
  floats "N" normals;
  floats "Cd" colours;
  Editor_document.Lowering.sha256 (Buffer.contents buffer)

let plan_digest ?(compiled = false) ?(fast_colours = false) (evaluated : E.t) =
  let nodes = Array.to_list evaluated.plan.nodes in
  let mesh = List.find (fun (node : E.node) -> node.kind = "sop/mesh") nodes in
  let attr name =
    let node =
      List.find
        (fun (node : E.node) ->
          node.kind = "sop/with_attr" && List.assoc_opt "attribute" node.args = Some (E.Text name))
        nodes
    in
    List.assoc "values" node.args
  in
  let data = E.Record (mesh.args @ [ ("N", attr "N"); ("Cd", attr "Cd") ]) in
  let fields =
    match
      ok
        (if compiled then
           Result.bind (Flow_ir.Executor.compile ~strict:true data) (fun program ->
               Flow_ir.Executor.force ~strict:true program ~live:(Input.at_time 0.))
         else E.force data ~live:(Input.at_time 0.))
    with
    | E.Record fields -> fields
    | _ -> assert false
  in
  let value name = List.assoc name fields in
  let ints name = match value name with E.Int_array a -> a | _ -> failwith name in
  let floats = function E.Vec3_array a -> a | _ -> failwith "expected vec3 array" in
  let colours = floats (value "Cd") in
  let colours =
    if fast_colours then Array.map (fun f -> Int32.float_of_bits (Int32.bits_of_float f)) colours
    else colours
  in
  digest
    (floats (value "points"))
    (ints "vertex_points") (ints "primitive_offsets") (ints "kinds") (floats (value "N")) colours

let interleave (planes : Rdk.Packed.Float3.View.view) =
  Array.init
    (3 * Array.length planes.x)
    (fun i -> (match i mod 3 with 0 -> planes.x | 1 -> planes.y | _ -> planes.z).(i / 3))

(* A first read gives a resident stream's buffer back (Flow_gpu.Run.materialise), so hashing through
   the host would send every later visit of the mesh down the host route, and the flip gates would
   measure that instead of the displayed route. A pin reads the bytes the display binds and leaves
   them there; a stream that is not resident or not settled is read on the host. *)
let held pin width decode =
  Option.map
    (fun (buffer, count, _, unpin) ->
      let floats = Option.get (Ogpu.Backend.mapped_view buffer) in
      Result.get_ok (Ogpu.Backend.invalidate_mapped buffer ~length:(count * width * 4));
      let values = Array.init (count * width) (fun i -> decode (Bigarray.Array1.get floats i)) in
      unpin ();
      values)
    pin

let geometry_digest geometry =
  let vec3 name host =
    match
      Option.bind (Flow_sop.Attribute_kernel.resident_vec3 geometry name) (fun placeholder ->
          held (Flow_gpu.Run.pin placeholder) 3 Fun.id)
    with
    | Some values -> values
    | None -> host ()
  in
  let attribute name =
    vec3 name (fun () ->
        match Rdk.Geometry.find_attribute ~owner:Point name geometry with
        | Some attr -> (
            match Rdk.Attribute.View.storage attr with
            | Float3 data -> interleave (Rdk.Packed.Float3.View.view data)
            | _ -> failwith (name ^ " is not float3"))
        | None -> failwith ("missing " ^ name))
  in
  let words holder =
    held (Flow_gpu.Run.pin_integers holder) 1 (fun word ->
        Int32.to_int (Int32.bits_of_float word) land 0xffffffff)
  in
  let vertices, offsets, kinds =
    match
      Option.map
        (fun (v, o, k) -> (words v, words o, words k))
        (Flow_sop.Topology_kernel.resident geometry)
    with
    | Some (Some v, Some o, Some k) -> (v, o, k)
    | _ ->
        let topology = Rdk.Topology.View.view (Rdk.Geometry.topology geometry) in
        ( topology.vertex_points,
          topology.primitive_offsets,
          Array.init (Bytes.length topology.primitive_kinds) (fun i ->
              Char.code (Bytes.get topology.primitive_kinds i)) )
  in
  digest
    (vec3 "P" (fun () -> interleave (Rdk.Packed.Float3.View.view (Rdk.Geometry.positions geometry))))
    vertices offsets kinds (attribute "N") (attribute "Cd")

let print_maps profile =
  List.iter
    (fun (map : Flow_ir.Profile.static_map) ->
      if map.count > 4096 then
        Printf.printf
          "map %s n=%d tier=%s head=%d tail=%d wall=%.6f head_s=%.6f kernel_s=%.6f tail_s=%.6f \
           refusal=%S\n\
           %!"
          (String.concat "/" map.site) map.count
          (if map.deferred then "deferred"
           else
             match map.tier with Flow_ir.Gpu -> "gpu" | Cpu_kernel -> "cpu_kernel" | _ -> "interp")
          map.head_elements map.tail_elements map.seconds map.head_seconds map.kernel_seconds
          map.tail_seconds
          (String.concat "; " (List.map Diagnostic.to_string map.refusal)))
    (Flow_ir.Profile.static_maps profile)

let golden_line side preset seed hash = Printf.sprintf "%d %d %d %s" side preset seed hash

let write_goldens base side =
  for preset = 0 to 7 do
    let workspace = variant base side preset 17 in
    let evaluated = ok (E.static workspace.checked) in
    Printf.printf "%s\n%!" (golden_line side preset 17 (plan_digest evaluated))
  done;
  let workspace = variant base side 7 18 in
  Printf.printf "%s\n%!" (golden_line side 7 18 (plan_digest (ok (E.static workspace.checked))))

let check_goldens ?(defer = false) ?(shard = 0) ?(shards = 1) base file =
  if shards <= 0 || shard < 0 || shard >= shards then
    invalid_arg "shard must be i/n with 0 <= i < n";
  In_channel.with_open_text file (fun channel ->
      In_channel.input_lines channel
      |> List.filteri (fun index _ -> index mod shards = shard)
      |> List.iter (fun line ->
          Scanf.sscanf line "%d %d %d %s" (fun side preset seed expected ->
              let workspace = variant base side preset seed in
              let profile = Flow_ir.Profile.create ~clock:(fun () -> 0.) in
              let lowered =
                ok
                  (Flow_ir.Profile.sample profile (fun () ->
                       Flow_sop.Lower.of_checked ~defer ~inputs:workspace.inputs workspace.checked))
              in
              Flow_ir.Profile.static_maps profile
              |> List.iter (fun (map : Flow_ir.Profile.static_map) ->
                  if map.count > 4096 && map.tail_elements > 0 then
                    failwith ("side-scaled map walked: " ^ String.concat "/" map.site));
              let actual = plan_digest ~compiled:defer lowered.evaluated in
              if actual <> expected then failwith ("isogrid geometry differs: " ^ line);
              let graph =
                List.find
                  (fun (g : Flow_sop.Lower.graph) -> g.name = "sculpture" && g.default)
                  lowered.graphs
              in
              let node =
                string_ok
                  (Sop.Edit_graph.compile_node graph.network.geometry
                     ~node_id:(Option.get graph.root))
              in
              let session =
                Sop.Session.create ~max_entries:16 ~max_payload_bytes:(1 lsl 28) |> Result.get_ok
              in
              Fun.protect
                ~finally:(fun () -> Sop.Session.close session)
                (fun () ->
                  let output =
                    ok
                      (Sop.Session.cook session
                         ~context:(Sop.Context.create ~time:0. () |> Result.get_ok)
                         node)
                  in
                  let geometry = Sop.Payload.geometry output.payload |> Result.get_ok in
                  if geometry_digest geometry <> expected then
                    failwith ("isogrid cooked geometry differs: " ^ line)))));
  print_endline "isogrid geometry goldens passed"

let frame count : Rays.Frame.t =
  {
    input =
      {
        (Input.at_time (float count /. 60.)) with
        frame = count;
        tick = count;
        dt = 1. /. 60.;
        size = (1280, 900);
        pointer = (-100., -100.);
      };
    pixel_scale = (1., 1.);
    fps = 60.;
    mouse_delta = (0., 0.);
  }

(* --no-device: the same editor sequence with an owner whose backend declines every device region
   and resident scope, so every route is the CPU one. A bench option, never a product switch. *)
let no_device = ref false

let no_device_owner () =
  if not !no_device then None
  else begin
    let owner = Flow_native.Workspace_gpu.create () in
    let backend =
      Option.get (Flow_native.Workspace_gpu.config ~fast:true owner).Flow_ir.Gpu.backend
    in
    Flow_native.Workspace_gpu.Private.install_test_backend owner
      { backend with
        residency = (fun ~fast:_ -> None);
        cost = (fun ?inputs:_ ~fast:_ _ ~count:_ -> None) };
    Some owner
  end

let benchmark ?domains ?(warmup = 8) ?(cycles = false) ?(roots = false) ?(switch_only = false) ?(flips = 0) ?(empty = false)
    ?(present = true) ?(retainers = false) base side single =
  let handles = Ogpu.Impl.live_handles () in
  let retainers = if retainers then Some (Retainers.start ()) else None in
  let canvas = Rays.Canvas.create_exn ~width:1280 ~height:900 in
  let editor = ref None and count = ref 0 in
  let presets = Filename.temp_dir "isogrid-bench" "" in
  let prepared_seconds = ref 0. in
  let prepare_calls = ref 0 in
  let prepare settings output =
    incr prepare_calls;
    let started = Unix.gettimeofday () in
    Fun.protect
      (fun () -> Editor.prepare settings output)
      ~finally:(fun () -> prepared_seconds := !prepared_seconds +. Unix.gettimeofday () -. started)
  in
  let workspace = variant base side 1 17 in
  let workspace =
    if not empty then workspace
    else
      ok
        (Doc.edit
           (ok (Rays_editor.workspace_catalog ()))
           workspace
           (F.Set_arg
              {
                node = [ "volume"; "voxels" ];
                key = Whole;
                sub = [];
                value = parse (Printf.sprintf "(array/int %d 0)" (side * side * side));
              }))
  in
  let text = Doc.to_text workspace in
  let current_side = ref side and current_preset = ref 1 and current_seed = ref 17 in
  let phase_names =
    [|
      "present";
      "prepare";
      "evaluate";
      "check";
      "lower";
      "print";
      "parse";
      "cook";
      "layout";
      "project";
      "reduce";
    |]
  in
  (* Fixed storage is allocated before either retained-heap snapshot. *)
  let samples = Array.init flips (fun _ -> Array.make (Array.length phase_names) 0.) in
  let allocated_total = Array.make 1 0. in
  (* Keeping a freshly allocated baseline tuple would itself count as growth. *)
  let retained_words = Array.make 3 0 in
  let cache_snapshots = Array.init 3 (fun _ -> Array.make 17 0) in
  let scheduler = if flips = 0 then None else Some (Scheduler_slots.create ()) in
  let began = Unix.gettimeofday () in
  let facts count =
    let value = frame count in
    if flips = 0 then value
    else
      (* These are interactive edits, taking about 0.1 s each. Recovery and
         other timed services must see that time, rather than two fictitious
         60-Hz frames per edit (which left their first use after warmup). *)
      { value with input = { value.input with t = Unix.gettimeofday () -. began } }
  in
  let sample_index = ref (-1) in
  let expected () =
    if empty then digest [||] [||] [| 0 |] [||] [||] [||]
    else
      let prefix = Printf.sprintf "%d %d %d " !current_side !current_preset !current_seed in
      read (Printf.sprintf "tools/isogrid_%d.expected" !current_side)
      |> String.split_on_char '\n'
      |> List.find (String.starts_with ~prefix)
      |> fun line ->
      String.sub line (String.length prefix) (String.length line - String.length prefix)
  in
  Fun.protect
    ~finally:(fun () ->
      Option.iter Editor.close !editor;
      ignore (Rays.Canvas.destroy canvas);
      Option.iter Retainers.stop retainers;
      if Sys.readdir presets = [||] then Unix.rmdir presets)
    (fun () ->
      let after_hash = ref (Flow_gpu.Host.zero, 0L) in
      let measured name make =
        let profile = Flow_ir.Profile.create ~clock:Unix.gettimeofday in
        let renderer_before = (string_ok (Rays.Canvas.Private.native_stats canvas)).uploaded_bytes in
        let before_alloc = Gc.allocated_bytes () and before_prepare = !prepared_seconds in
        let result, phases =
          Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
              Flow_ir.Profile.sample profile (fun () ->
                  let value = make () in
                  editor := Some value;
                  let value = ref value in
                  for _ = 1 to 2 do
                    let facts = facts !count in
                    let facts =
                      if flips = 0 then facts
                      else
                        { facts with input = { facts.input with buttons = [ Input.LeftButton ] } }
                    in
                    value := Editor.update !value facts;
                    incr count
                  done;
                  editor := Some !value;
                  let scene = Editor.scene !value (facts !count) in
                  if present then begin
                    Rays.Canvas.render canvas scene |> string_ok;
                    ignore (Rays.Canvas.pixel canvas ~x:0 ~y:0);
                    value := Editor.observe_presented !value
                  end
                  else Rays.Scene.Layer.release scene;
                  !value))
        in
        let allocated = Gc.allocated_bytes () -. before_alloc in
        if !sample_index >= 0 then begin
          allocated_total.(0) <- allocated_total.(0) +. allocated;
          Array.iteri
            (fun column name ->
              samples.(!sample_index).(column) <-
                (match name with
                | "present" -> phases.total
                | "prepare" -> !prepared_seconds -. before_prepare
                | name -> Flow.Phase_timer.seconds phases name))
            phase_names
        end;
        (* Hashing reads the held bytes where they are, after measuring time to presentation. *)
        let host, _, _, _, _ = Editor.gpu_stats result in
        assert (Flow_native.Workspace_gpu.held_displays result.host.gpu <= 128);
        let geometry = Option.get (Editor.prepared result) in
        let hash = geometry_digest geometry in
        let colour_storage =
          if hash = expected () then "binary64"
          else begin
            (* Warm production may measure/place Bulk_approx for the constant axis palette.
               Prove the CPU arrays still match the walker golden, then allow exactly one
               float32 conversion of Cd. P/topology/N remain bit-exact; no tolerance is widened. *)
            let cpu = ok (Flow_ir.static (Editor.workspace result).checked) in
            if plan_digest cpu <> expected () || plan_digest ~fast_colours:true cpu <> hash then
              failwith ("stale or incorrect geometry after " ^ name);
            "float32"
          end
        in
        let materialised, _, _, _, _ = Editor.gpu_stats result in
        let previous, previous_uploads = !after_hash in
        let renderer_after = (string_ok (Rays.Canvas.Private.native_stats canvas)).uploaded_bytes in
        Printf.printf
          "production %s handles=%d payload_bytes=%d dispatches=%d flow_input_bytes=%d renderer_upload_bytes=%Ld\n%!"
          name (Ogpu.Impl.live_handles ()) (host.payload_readback_bytes - previous.payload_readback_bytes)
          (host.status_reads - previous.status_reads)
          (host.input_uploaded_bytes - previous.input_uploaded_bytes)
          (Int64.sub renderer_after renderer_before);
        ignore previous_uploads;
        after_hash := (materialised, renderer_after);
        Printf.printf
          "%s present_s=%.6f alloc_bytes=%.0f prepare_s=%.6f heap_words=%d uploads=%d \
           upload_bytes=%d readback_bytes=%d inspect_readback_bytes=%d pipelines=%d held=%d \
           digest=%s verified_digest=%s Cd=%s phases=[%s]\n\
           %!"
          name phases.total allocated
          (!prepared_seconds -. before_prepare)
          (Gc.quick_stat ()).heap_words host.input_uploads host.input_uploaded_bytes
          host.readback_bytes
          (materialised.readback_bytes - host.readback_bytes)
          host.pipeline_compilations host.held_buffers hash (expected ()) colour_storage
          (Flow.Phase_timer.summary phases);
        (let host_rate, resident_rate =
           Option.fold ~none:(infinity, infinity)
             ~some:Flow_sop.Attribute_kernel.Display_sink.rates
             (Flow_native.Workspace_gpu.config result.host.gpu).backend
         in
         let floats =
           (Rdk.Geometry.point_count geometry * 9) + (Rdk.Geometry.primitive_count geometry * 6)
         in
         Printf.printf "display_sink stage=%s points=%d floats=%d host_model_s=%.6f resident_model_s=%.6f\n%!"
           name (Rdk.Geometry.point_count geometry) floats (host_rate *. float floats)
           (resident_rate *. float floats));
        print_maps profile;
        Printf.printf
          "transfer_totals stage=%s payload=%d status=%d validation=%d count=%d bounds=%d\n%!"
          name host.payload_readback_bytes host.status_readback_bytes
          host.validation_readback_bytes host.count_readback_bytes host.bounds_readback_bytes;
        List.iter
          (fun (p : Flow_ir.placement) ->
            Printf.printf
              "placement stage=%s owner=%s count=%s reason=%s cpu_s=%s device_s=%s readers=%s\n%!"
              name (String.concat "/" (fst p.owner))
              (Option.fold ~none:"unknown" ~some:string_of_int p.count)
              (Flow_ir.Cost.reason_name p.reason)
              (Option.fold ~none:"unknown" ~some:string_of_float p.cpu_seconds)
              (Option.fold ~none:"unknown" ~some:string_of_float p.device_seconds)
              (Option.fold ~none:"unknown" ~some:string_of_int p.cpu_consumers))
          (Flow_ir.Profile.placements profile
          @ Flow_ir.Profile.placements result.core.doc.workspace.lowered.profile);
        result
      in
      let current =
        ref
          (measured "cold" (fun () ->
               Editor.create ~platform:Input.Apple ?domains ?gpu_owner:(no_device_owner ()) ~workspace:(load text)
                 ~await:true ~presets ~prepare ~draw:Editor.draw ()
               |> string_ok))
      in
      let change name op =
        current := measured name (fun () -> Editor.edit !current op |> string_ok)
      in
      let retention snapshot name =
        Gc.full_major ();
        Gc.full_major ();
        let gc = Gc.stat () in
        let value = !current in
        let core = value.core in
        let stats = Rays_editor__.Cook.stats core.cook in
        let host, _, _, _, _ = Editor.gpu_stats value in
        let registrations = Flow_native.Workspace_gpu.held_displays value.host.gpu in
        let native = Rays.Canvas.Private.native_stats canvas |> string_ok in
        let history = Editor_core.History.depth core.history in
        let compiled = Editor_document.Document.Int_map.cardinal core.cook.compiled in
        let lanes = Editor_document.Document.Int_map.cardinal core.cook.value_lanes in
        let drawn = Editor_document.Document.Int_map.cardinal value.drawn in
        let converted = Rays_editor__.Renderer.Mesh_cache.length value.extra.renderer.meshes in
        let ids = Flow_sop.Lower.Path_map.cardinal core.doc.workspace.lowered.compiled_ids in
        let preparations = Rays_editor__.Cook.Prepared_cache.length core.cook.prepared_cache in
        let preparation_bytes = Rays_editor__.Cook.Prepared_cache.bytes core.cook.prepared_cache in
        let scheduler_capacity, scheduler_empty_words =
          Scheduler_slots.counts (Option.get scheduler)
        in
        (* Timers sometimes share the current frame's boxed float. Count their
           actual distinct blocks, so merely sampling FPS/recovery on different
           frames cannot look like retained workload data. *)
        let timers =
          [
            Obj.repr value.state_checked;
            Obj.repr core.status_fps_at;
            Obj.repr (Pxui.Ui.input core.ui).input.t;
          ]
        in
        let timer_words = Obj.reachable_words (Obj.repr timers) - 9 in
        let workload_words = gc.live_words - scheduler_empty_words - timer_words in
        retained_words.(snapshot) <- workload_words;
        Array.blit
          [|
            history;
            stats.retained_entries;
            stats.retained_payload_bytes;
            compiled;
            lanes;
            drawn;
            converted;
            ids;
            host.pipeline_compilations;
            host.held_buffers;
            preparations;
            preparation_bytes;
            native.pipeline_cache_entries;
            scheduler_capacity;
            registrations;
            host.buffer_creations;
            Ogpu.Impl.live_handles ();
          |]
          0 cache_snapshots.(snapshot) 0 17;
        if roots then begin
          let rec fields depth path o =
            if Obj.is_block o && Obj.tag o < Obj.no_scan_tag && depth > 0 then
              for i = 0 to Obj.size o - 1 do
                let f = Obj.field o i in
                let w = Obj.reachable_words f in
                if w > 200_000 then begin
                  Printf.printf "root %s canvas%s.%d words=%d\n%!" name path i w;
                  fields (depth - 1) (path ^ "." ^ string_of_int i) f
                end
              done
          in
          fields 5 "" (Obj.repr canvas);
          (* the largest child at each level below the canvas's biggest cache *)
          let rec chain depth o =
            if Obj.is_block o && Obj.tag o < Obj.no_scan_tag && depth > 0 then begin
              let best = ref (-1) and best_words = ref 0 in
              for i = 0 to Obj.size o - 1 do
                let w = Obj.reachable_words (Obj.field o i) in
                if w > !best_words then (best := i; best_words := w)
              done;
              if !best >= 0 then begin
                Printf.printf "chain %s depth=%d field=%d tag=%d size=%d words=%d\n%!" name depth !best
                  (Obj.tag o) (Obj.size o) !best_words;
                chain (depth - 1) (Obj.field o !best)
              end
            end
          in
          chain 24 (Obj.field (Obj.repr canvas) 3);
          List.iter
            (fun (owner, root) ->
              Printf.printf "root %s %s words=%d\n%!" name owner (Obj.reachable_words root))
            [
              ("model", Obj.repr value);
              ("core", Obj.repr core);
              ("doc", Obj.repr core.doc);
              ("history", Obj.repr core.history);
              ("panes", Obj.repr core.panes);
              ("ui", Obj.repr core.ui);
              ("text", Obj.repr core.text);
              ("cook", Obj.repr core.cook);
              ("compiled", Obj.repr core.cook.compiled);
              ("preparations", Obj.repr core.cook.prepared_cache);
              ("drawn", Obj.repr value.drawn);
              ("extra", Obj.repr value.extra);
              ("rendered", Obj.repr value.rendered);
              ("baked", Obj.repr value.baked);
              ("opened", Obj.repr value.opened);
              ("saved_doc", Obj.repr value.saved_doc);
              ("rows", Obj.repr core.rows);
              ("host", Obj.repr value.host);
              ("canvas", Obj.repr canvas);
            ]
        end;
        Printf.printf
          "retention %s live_words=%d workload_words=%d scheduler_empty_words=%d \
           scheduler_capacity=%d timer_words=%d heap_words=%d history=%d entries=%d \
           payload_bytes=%d compiled=%d lanes=%d drawn=%d converted=%d ids=%d pipelines=%d held=%d \
           preparations=%d preparation_bytes=%d prepare_calls=%d \
           renderer_pipelines=%d registrations=%d flow_buffers_created=%d live_handles=%d\n\
           %!"
          name gc.live_words workload_words scheduler_empty_words scheduler_capacity timer_words
          gc.heap_words history stats.retained_entries stats.retained_payload_bytes compiled lanes
          drawn converted ids host.pipeline_compilations host.held_buffers preparations
          preparation_bytes !prepare_calls
          native.pipeline_cache_entries registrations host.buffer_creations (Ogpu.Impl.live_handles ())
      in
      if flips > 0 then begin
        (* One closed eight-recipe cycle returns to the initial recipe 1.
           Both snapshots then follow a return, rather than comparing recipe
           7's first-created scene wrapper with a later equivalent wrapper.
           Every recipe runs once per cycle, with no extra warmup cycle. *)
        let[@inline never] run_flips warm length =
          for i = 0 to length - 1 do
            let preset = (i + 2) mod 8 in
            if not warm then sample_index := i;
            current_preset := preset;
            let name =
              if warm then Printf.sprintf "warm_%d" preset
              else Printf.sprintf "flip_%d_recipe_%d" i preset
            in
            change name (recipe preset);
            if ((not warm) && Option.is_some retainers || cycles) && i mod 8 = 7 then
              retention 2
                (Printf.sprintf "%s_%d" (if warm then "warm_cycle" else "cycle") ((i + 1) / 8))
          done
        in
        run_flips true warmup;
        retention 0 "warm";
        let allocation_before = Option.map Retainers.snapshot retainers in
        run_flips false flips;
        retention 1 "final";
        Printf.printf "allocation flips=%d mean_bytes=%.0f\n%!" flips
          (allocated_total.(0) /. float flips);
        Option.iter (fun value -> Retainers.report value (Option.get allocation_before)) retainers;
        let means =
          Array.mapi
            (fun column name ->
              let values = Array.map (fun row -> row.(column)) samples in
              let mean = Array.fold_left ( +. ) 0. values /. float flips in
              Array.sort Float.compare values;
              Printf.printf "latency %s mean_s=%.6f p50_s=%.6f p95_s=%.6f max_s=%.6f\n%!" name mean
                values.(flips / 2)
                values.(min (flips - 1) (flips * 95 / 100))
                values.(flips - 1);
              (name, mean))
            phase_names
        in
        Array.sort (fun (_, a) (_, b) -> Float.compare b a) means;
        Array.iter (fun (name, mean) -> Printf.printf "phase_rank %s %.6f\n%!" name mean) means;
        if flips mod 8 = 0 then begin
          (* past 24³ the recipes outgrow the session and the buffer pool, so flips recook and
             creations are turnover: live_handles and retained words are what bound the buffers *)
          let turnover = 15 in
          if side <> 24 then begin
            Printf.printf "buffer_creations warm=%d final=%d\n%!" cache_snapshots.(0).(turnover)
              cache_snapshots.(1).(turnover);
            cache_snapshots.(1).(turnover) <- cache_snapshots.(0).(turnover)
          end;
          if cache_snapshots.(0) <> cache_snapshots.(1) then begin
            let names =
              [| "history"; "session_entries"; "session_payload_bytes"; "compiled"; "lanes"; "drawn";
                 "converted"; "ids"; "pipelines"; "held_buffers"; "preparations";
                 "preparation_bytes"; "renderer_pipelines"; "scheduler_capacity"; "registrations";
                 "buffer_creations"; "live_handles" |]
            in
            Array.iteri
              (fun i name ->
                if cache_snapshots.(0).(i) <> cache_snapshots.(1).(i) then
                  Printf.printf "snapshot_differs %s warm=%d final=%d\n%!" name
                    cache_snapshots.(0).(i) cache_snapshots.(1).(i))
              names;
            failwith "selector caches/history grew after warmup"
          end;
          if retained_words.(1) > retained_words.(0) then failwith "retained heap grew after warmup"
        end;
        let budget = if side = 24 then 0.150 else 2. in
        if Array.exists (fun row -> row.(2) > budget) samples then
          failwith "warm evaluate performance gate failed"
      end
      else if not single then begin
        List.iter
          (fun preset ->
            current_preset := preset;
            change (Printf.sprintf "recipe_%d" preset) (recipe preset))
          (if switch_only then [ 3; 1 ] else [ 3; 1; 0; 2; 4; 5; 6; 7 ]);
        if not switch_only then begin
          current_seed := 18;
          change "seed_18" (input "seed" 18);
          current_side := 64;
          change "side_64" (input "resolution" 64);
          current_side := 24;
          change "side_24" (input "resolution" 24)
        end
      end);
  assert (Ogpu.Impl.live_handles () = handles);
  Printf.printf "workspace_close handles=%d baseline=%d\n%!" (Ogpu.Impl.live_handles ()) handles

let cli base =
  let file = Filename.temp_file "isogrid-cli-" ".rays" in
  Fun.protect
    ~finally:(fun () -> Sys.remove file)
    (fun () ->
      let failures = ref [] in
      List.iter
        (fun side ->
          for preset = 0 to 7 do
            Out_channel.with_open_bin file (fun output ->
                output_string output (Doc.to_text (variant base side preset 17)));
            let began = Unix.gettimeofday () in
            let executable = "_build/default/bin/rays.exe" in
            let pid =
              Unix.create_process executable [| executable; "check"; file |] Unix.stdin Unix.stdout
                Unix.stderr
            in
            let _, status = Unix.waitpid [] pid in
            let seconds = Unix.gettimeofday () -. began in
            let budget = if side = 24 then 1.2 else if preset = 1 then 3. else 5. in
            Printf.printf "cli side=%d recipe=%d seconds=%.6f budget=%.1f\n%!" side preset seconds
              budget;
            if status <> Unix.WEXITED 0 || seconds > budget then
              failures := (side, preset, seconds) :: !failures
          done)
        [ 24; 64 ];
      if !failures <> [] then failwith "cold CLI performance gate failed")

(* --ab SIDE FLIPS ROUNDS: adjacent processes alternate production and --no-device on the same
   unforced editor sequence (the flip warmup and gate included); the medians of their means follow. *)
let ab side flips rounds =
  let exe = Sys.executable_name in
  let run extra =
    let file = Filename.temp_file "isogrid-ab" ".log" in
    let out = Unix.openfile file [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
    let pid =
      Unix.create_process exe
        (Array.of_list ([ exe; "--flips"; string_of_int side; string_of_int flips ] @ extra))
        Unix.stdin out out
    in
    ignore (Unix.waitpid [] pid);
    Unix.close out;
    let lines = String.split_on_char '\n' (read file) in
    Sys.remove file;
    let mean prefix =
      List.find_map
        (fun line ->
          if String.starts_with ~prefix line then
            Scanf.sscanf_opt line "%_s %_s mean_s=%f" Fun.id
          else None)
        lines
      |> Option.get
    in
    (mean "latency present", mean "latency cook")
  in
  let load () =
    let channel = Unix.open_process_in "uptime" in
    let line = try input_line channel with End_of_file -> "" in
    ignore (Unix.close_process_in channel);
    line
  in
  let production = ref [] and cpu = ref [] in
  for round = 1 to rounds do
    let first_cpu = round mod 2 = 0 in
    let one cpu_run =
      let present, cook = run (if cpu_run then [ "--no-device" ] else []) in
      Printf.printf "ab round=%d route=%s present_mean_s=%.6f cook_mean_s=%.6f load: %s\n%!" round
        (if cpu_run then "cpu" else "production") present cook (load ());
      if cpu_run then cpu := present :: !cpu else production := present :: !production
    in
    one first_cpu;
    one (not first_cpu)
  done;
  let median l =
    let a = Array.of_list (List.sort Float.compare l) in
    a.(Array.length a / 2)
  in
  Printf.printf "ab side=%d flips=%d rounds=%d median_present_s cpu=%.6f production=%.6f ratio=%.3f\n%!"
    side flips rounds (median !cpu) (median !production) (median !production /. median !cpu)

let () =
  if Editor_document.Lowering.sha256 (read source) <> frozen then failwith "isogrid sketch changed";
  let base = load (read source) in
  let argv = List.filter (( <> ) "--no-device") (Array.to_list Sys.argv) in
  no_device := List.length argv <> Array.length Sys.argv;
  match argv with
  | [ _; "--ab"; side; flips; rounds ] ->
      ab (int_of_string side) (int_of_string flips) (int_of_string rounds)
  | [ _; "--source"; side ] -> print_string (Doc.to_text (variant base (int_of_string side) 1 17))
  | [ _; "--write-goldens"; side ] -> write_goldens base (int_of_string side)
  | [ _; "--check-goldens"; file ] -> check_goldens base file
  | [ _; "--check-deferred"; file ] -> check_goldens ~defer:true base file
  | [ _; (("--check-goldens" | "--check-deferred") as mode); file; "--shard"; value ] ->
      Scanf.sscanf value "%d/%d" (fun shard shards ->
          check_goldens ~defer:(mode = "--check-deferred") ~shard ~shards base file)
  | [ _; "--bench"; side; "--single" ] -> benchmark base (int_of_string side) true
  | [ _; "--bench"; side ] -> benchmark base (int_of_string side) false
  | [ _; "--flips"; side; count ] ->
      benchmark ~flips:(int_of_string count) base (int_of_string side) false
  | [ _; "--plateau"; side; warmup; count ] ->
      benchmark ~cycles:true ~warmup:(int_of_string warmup) ~flips:(int_of_string count) base
        (int_of_string side) false
  | [ _; "--flips-no-canvas"; side; count ] ->
      benchmark ~present:false ~flips:(int_of_string count) base (int_of_string side) false
  | [ _; "--retainers"; side; count ] ->
      benchmark ~retainers:true ~flips:(int_of_string count) base (int_of_string side) false
  | [ _; "--roots"; side; count ] ->
      benchmark ~roots:true ~flips:(int_of_string count) base (int_of_string side) false
  | [ _; "--roots-no-canvas"; side; count ] ->
      benchmark ~roots:true ~present:false ~flips:(int_of_string count) base (int_of_string side)
        false
  | [ _; "--retainers"; side; count; "--single-domain" ] ->
      Parallel.run ~domains:1 (fun () ->
          benchmark ~domains:1 ~retainers:true ~flips:(int_of_string count) base
            (int_of_string side) false)
  | [ _; "--empty"; side ] -> benchmark ~empty:true base (int_of_string side) true
  | [ _; "--cli" ] -> cli base
  | [ _; "--switch"; side ] -> benchmark ~switch_only:true base (int_of_string side) false
  | [ _ ] -> benchmark base 24 false
  | _ ->
      failwith
        "usage: bench_isogrid [--source SIDE | --write-goldens SIDE | --check-goldens FILE | \
         --bench SIDE [--single] | --switch SIDE | --flips SIDE COUNT | --plateau SIDE WARMUP COUNT | --empty \
         SIDE | --cli]"
