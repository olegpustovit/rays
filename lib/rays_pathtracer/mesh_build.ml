open Mesh_data
module B = Ogpu.Backend
let ( let* ) = Result.bind
let gpu result = Result.map_error Ogpu.Error.to_string result
let put_f32 bytes offset value = Bytes.set_int32_le bytes offset (Int32.bits_of_float value)
let put_u32 bytes offset value = Bytes.set_int32_le bytes offset (Int32.of_int value)

type shape = { instanced : bool; motion : bool; spheres : bool; curves : bool; scene : bool }

(* The kernel's views of a mesh (pathtrace.rays): float material ids (per triangle, then per strand segment),
   four vec3 rows per instance (first triangle, material or -1, then the normal-matrix columns; seven with
   motion, the shutter end's columns last), scene/trace's instance table (16 floats: first triangle and the
   world-to-object rows; 32 with motion: first triangle, start and end object-to-world rows), the analytic
   spheres as (centre, radius) and the strand segments as (end a, radius), (end b, radius). *)
type tables = {
  ids : B.buffer; inst : B.buffer; trace_instances : B.buffer option; spheres : B.buffer; strands : B.buffer;
  id_count : int; instance_count : int; sphere_count : int; sphere_base : int; strand_count : int; triangle_ids : int;
  classes : bool * bool * bool  (* some material is metallic, emissive, round: the kernel folds what none is *) }

type gpu_mesh = {
  positions : B.buffer;
  normals : B.buffer;
  materials : B.buffer;
  curves : (B.accel * B.buffer list) option;  (* strands as native curves, with their control data *)
  mutable structure : B.accel;
  mutable primitive : B.accel option;
  instance_buffer : B.buffer option;
  motion_buffer : B.buffer option;
  prototype_key : (bytes * bytes * bytes * bytes * bytes) option;
  scene_blas : B.accel list;
  tables : tables;  (* owned even when the prototype is borrowed *)
  mutable owns_prototype : bool;
  shape : shape;
}

(* A replacement build runs in stages: the primitive structure builds and
   writes its compacted size; then it is compacted into a right-sized
   structure and the instance structure (if any) builds over the compacted
   one; then the uncompacted structure is released. *)
type stage =
  | Building of B.receipt
  | Compacting of B.receipt
  | Ready

type mesh_build = {
  gpu : gpu_mesh;
  scratch : B.buffer;
  mutable extra_scratch : B.buffer option;
  mutable size_buffer : B.buffer option;
  mutable uncompacted : B.accel option;
  mutable stage : stage;
  instance_count : int;
}

let buffer ?(memory = Ogpu.Types.Shared) ?(usage = [ Ogpu.Types.Storage ]) device ~label size =
  gpu
    (B.create_buffer ~memory device
       { label = Some label; size = Int64.of_int (max 4 size); usage })

let shared device ~label bytes =
  let* value = buffer device ~label (Bytes.length bytes) in
  match gpu (B.write_buffer value ~offset:0L bytes) with
  | Ok () -> Ok value
  | Error _ as failure ->
      ignore (B.destroy_buffer value);
      failure

let table_buffers t = t.ids :: t.inst :: t.spheres :: t.strands :: Option.to_list t.trace_instances

let destroy_gpu gpu =
  ignore (B.destroy_accel gpu.structure);
  List.iter (fun b -> ignore (B.destroy_buffer b)) (table_buffers gpu.tables);
  Option.iter (fun motion -> ignore (B.destroy_buffer motion)) gpu.motion_buffer;
  Option.iter (fun instances -> ignore (B.destroy_buffer instances)) gpu.instance_buffer;
  if gpu.owns_prototype then begin
    Option.iter (fun primitive -> ignore (B.destroy_accel primitive)) gpu.primitive;
    List.iter
      (fun value -> ignore (B.destroy_buffer value))
      [ gpu.positions; gpu.normals; gpu.materials ];
    List.iter (fun blas -> ignore (B.destroy_accel blas)) gpu.scene_blas;
    Option.iter (fun (curves, buffers) ->
      ignore (B.destroy_accel curves); List.iter (fun value -> ignore (B.destroy_buffer value)) buffers) gpu.curves
  end

(* Encodes the instance structure build over [build.gpu.primitive]. *)
let encode_instances device (build : mesh_build) command =
  let structures = match build.gpu.primitive with Some primitive -> [ primitive ] | None -> build.gpu.scene_blas in
  match (build.gpu.instance_buffer, structures) with
  | Some instances, _ :: _ ->
      let kind = if build.gpu.motion_buffer = None then B.User_id_instances else B.Motion_instances in
      let* structure =
        gpu
          (B.create_accel device
             (B.Tlas_of
                { instances; offset = 0L; instance_count = build.instance_count; kind
                ; structures
                ; motion_transforms =
                    Option.map (fun buffer -> (buffer, 0L, 2 * build.instance_count)) build.gpu.motion_buffer }))
      in
      let needed = (B.accel_sizes structure).build_scratch_size in
      let* scratch =
        if needed <= B.buffer_size build.scratch then Ok build.scratch
        else
          let* extra = buffer ~memory:Ogpu.Types.Device_local device ~label:"pathtracer-instance-scratch" (Int64.to_int needed) in
          build.extra_scratch <- Some extra;
          Ok extra
      in
      let encoded =
        let* encoder = gpu (B.accel_encoder command) in
        let* () = gpu (B.build_accel encoder structure ~scratch ()) in
        gpu (B.end_accel encoder)
      in
      (match encoded with
       | Ok () -> build.gpu.structure <- structure; Ok ()
       | Error _ as failure -> ignore (B.destroy_accel structure); failure)
  | _ -> Ok ()

let submit_instances device queue (build : mesh_build) =
  let* command = gpu (B.begin_commands queue) in
  match (let* () = encode_instances device build command in gpu (B.commit command)) with
  | Ok receipt -> build.stage <- Compacting receipt; Ok ()
  | Error _ as failure -> ignore (B.abandon command); failure

let mesh_shape mesh =
  { instanced = mesh.instances <> None
  ; motion = (match mesh.instances with Some { motion_transforms = Some _; _ } -> true | _ -> false)
  ; spheres = mesh.extras.sphere_count > 0
  ; curves = mesh.extras.strand_segments > 0
  ; scene = mesh.scene <> None }

let floats values =
  let bytes = Bytes.create (4 * max 1 (Array.length values)) in
  Bytes.fill bytes 0 4 '\000';
  Array.iteri (fun i v -> put_f32 bytes (4 * i) v) values;
  bytes

let upload_tables ~material_data device mesh =
  let some k = let found = ref false in
    for m = 0 to (Bytes.length material_data / 48) - 1 do
      List.iter (fun c -> if Int32.float_of_bits (Bytes.get_int32_le material_data (48 * m + 4 * c)) <> 0. then found := true) k
    done; !found in
  let f32 bytes offset = Int32.float_of_bits (Bytes.get_int32_le bytes offset) in
  let records, shader = match mesh.instances with Some i -> i.records, i.shader_bytes | None -> [||], Bytes.empty in
  let keyframes = match mesh.instances with Some { motion_transforms = Some k; _ } -> Some k | _ -> None in
  let n = Array.length records and stride = if keyframes = None then 84 else 168 in
  let first i = match mesh.scene with Some l -> Int32.to_float (Bytes.get_int32_le l.table (8 * i)) | None -> 0. in
  let material i = match mesh.scene with
    | Some l -> Int32.to_float (Bytes.get_int32_le l.table (8 * i + 4))
    | None -> let user = records.(i).B.user_id in if user = 0 then -1. else float user in
  let column ?(at = 0) i k = Array.init 3 (fun r -> f32 shader (stride * i + at + 4 * (3 * k + r))) in
  let inst = Array.concat (List.concat (List.init n (fun i ->
    [ [| first i; material i; 0. |]; column i 4; column i 5; column i 6 ]
    @ if keyframes = None then [] else [ column ~at:84 i 4; column ~at:84 i 5; column ~at:84 i 6 ]))) in
  (* world-to-object row r is normal-matrix column r; its translation is minus that row dotted with t *)
  let trace = Array.concat (List.init n (fun i -> match keyframes with
    | Some k -> Array.concat [ [| first i |]; k.(2 * i); k.(2 * i + 1); Array.make 7 0. ]
    | None ->
        let t = column i 3 in
        Array.concat ([| first i |] :: List.init 3 (fun r -> let w = column i (4 + r) in
          [| w.(0); w.(1); w.(2); -.((w.(0) *. t.(0)) +. (w.(1) *. t.(1)) +. (w.(2) *. t.(2))) |]) @ [ [| 0.; 0.; 0. |] ]))) in
  let e = mesh.extras in
  let spheres = Array.init (4 * e.sphere_count) (fun j -> f32 e.sphere_bytes (4 * j)) in
  let strands = Array.init (8 * e.strand_segments) (fun j ->
    let first = Int32.to_int (Bytes.get_int32_le e.strand_indices (4 * (j / 8))) + (j mod 8) / 4 in
    if j mod 4 = 3 then f32 e.strand_radii (4 * first) else f32 e.strand_points (12 * first + 4 * (j mod 4))) in
  let id k bytes = Array.init (Bytes.length bytes / 4) (fun i -> Int32.to_float (Bytes.get_int32_le bytes (4 * i))) |> k in
  let triangle_ids = Bytes.length mesh.mesh_ids / 4 in
  let ids = Array.append (id Fun.id mesh.mesh_ids) (id Fun.id e.strand_ids) in
  let owned = ref [] in
  let upload label values = let* b = shared device ~label (floats values) in owned := b :: !owned; Ok b in
  let made =
    let* ids_buffer = upload "pathtracer-lisp-ids" ids in
    let* inst_buffer = upload "pathtracer-lisp-instances" (if n = 0 then Array.make 12 0. else inst) in
    let* spheres_buffer = upload "pathtracer-lisp-spheres" (if spheres = [||] then Array.make 4 0. else spheres) in
    let* strands_buffer = upload "pathtracer-lisp-strands" (if strands = [||] then Array.make 8 0. else strands) in
    let* trace_instances = if n = 0 then Ok None else Result.map Option.some (upload "pathtracer-lisp-trace" trace) in
    Ok { ids = ids_buffer; inst = inst_buffer; trace_instances; spheres = spheres_buffer; strands = strands_buffer
       ; id_count = Array.length ids; instance_count = max 1 n; sphere_count = e.sphere_count
       ; sphere_base = e.sphere_material; strand_count = e.strand_segments; triangle_ids
       ; classes = (some [ 7 ], some [ 4; 5; 6 ], some [ 8 ]) } in
  (match made with Ok _ -> () | Error _ -> List.iter (fun b -> ignore (B.destroy_buffer b)) !owned);
  made

(* Stage one of a replacement build: upload the geometry, build the primitive
   structure, and write its compacted size, all without waiting. A reused
   prototype skips straight to the instance build. *)
let upload_mesh_async ?reuse ~material_data device queue mesh =
  let buffers = ref []
  and structures = ref []
  and scratch = ref None
  and commands = ref None in
  let owned bytes ~label =
    let* value = shared device ~label bytes in
    buffers := value :: !buffers;
    Ok value
  in
  let accel descriptor =
    let* value = gpu (B.create_accel device descriptor) in
    structures := value :: !structures;
    Ok value
  in
  let* mesh =
    if mesh_shape mesh |> fun s -> s.motion && not (Ogpu.Caps.has (B.capabilities device) Ray_tracing_motion)
    then Mesh_data.shutter_midpoint mesh else Ok mesh in
  let shape = mesh_shape mesh in
  let extras = mesh.extras in
  let capabilities = B.capabilities device in
  (* without native curves scene/trace tests each strand segment as a capsule *)
  let native_curves = shape.curves && Ogpu.Caps.has capabilities Ray_tracing_curves in
  let reused =
    match (reuse, mesh.instances) with
    | Some gpu, Some _ when mesh.scene = None -> (
        match (gpu.prototype_key, gpu.primitive) with
        | Some (positions, normals, ids, materials, extra_key), Some primitive
          when Bytes.equal positions mesh.mesh_positions
               && Bytes.equal normals mesh.mesh_normals
               && Bytes.equal ids mesh.mesh_ids
               && Bytes.equal materials material_data
               && Bytes.equal extra_key (Bytes.cat extras.sphere_bytes extras.strand_points) ->
            Some (gpu, primitive)
        | _ -> None)
    | _ -> None
  in
  let result =
    let* positions, normals, materials, curves, primitive, scene_blas, build_primitive =
      match reused with
      | Some (gpu, primitive) -> Ok (gpu.positions, gpu.normals, gpu.materials, gpu.curves, primitive, [], false)
      | None ->
          let* positions = owned mesh.mesh_positions ~label:"pathtracer-positions" in
          let* normals = owned mesh.mesh_normals ~label:"pathtracer-normals" in
          let* materials = owned material_data ~label:"pathtracer-materials" in
          let* curves =
            if not native_curves then Ok None
            else
              let* points = owned extras.strand_points ~label:"pathtracer-strand-points" in
              let* radii = owned extras.strand_radii ~label:"pathtracer-strand-radii" in
              let* indices = owned extras.strand_indices ~label:"pathtracer-strand-indices" in
              let* curves = accel (B.Blas { geometries = [ B.Curves
                { control_points = [ { buffer = points; offset = 0L } ]; control_stride = 12
                ; control_point_count = extras.strand_point_count
                ; radii = [ { buffer = radii; offset = 0L } ]; radius_stride = 4; indices; index_offset = 0L
                ; segment_count = extras.strand_segments; control_points_per_segment = 2
                ; curve_type = Round_curve; basis = Linear_basis; caps = Sphere_caps } ] }) in
              Ok (Some (curves, [ points; radii; indices ]))
          in
          let triangles (Triangle_part (first, vertex_count)) =
            B.Triangles
              { vertices = positions; offset = Int64.of_int (first * 12); length = Int64.of_int (vertex_count * 12)
              ; vertex_stride = 12; vertex_count }
          in
          let parts = match mesh.scene with
            | Some layout -> Array.to_list layout.parts
            | None -> [ Triangle_part (0, Bytes.length mesh.mesh_positions / 12) ] in
          let rec build_all acc = function
            | [] -> Ok (List.rev acc)
            | part :: rest ->
                let* blas = accel (B.Blas { geometries = [ triangles part ] }) in
                build_all (blas :: acc) rest
          in
          let* blas = build_all [] parts in
          let primitive = List.hd blas in
          Ok (positions, normals, materials, curves, primitive, (if mesh.scene = None then [] else blas), true)
    in
    let* instance_buffer, motion_buffer =
      match mesh.instances with
      | None -> Ok (None, None)
      | Some instances ->
          let* motion_buffer =
            match instances.motion_transforms with
            | None -> Ok None
            | Some keyframes ->
                let* packed = gpu (B.pack_transforms keyframes) in
                let* buffer = owned packed ~label:"pathtracer-motion" in
                Ok (Some buffer)
          in
          let* packed =
            match instances.motion_transforms with
            | None -> gpu (B.pack_instance_records device instances.records)
            | Some _ ->
                gpu
                  (B.pack_motion_instances device
                     (Array.mapi
                        (fun index record ->
                          { B.record; transforms_start = 2 * index; transforms_count = 2; start_time = 0.
                          ; end_time = 1.; start_border = Clamp; end_border = Clamp })
                        instances.records))
          in
          let* instance_buffer = owned packed ~label:"pathtracer-instances" in
          Ok (Some instance_buffer, motion_buffer)
    in
    let* tables = upload_tables ~material_data device mesh in
    buffers := table_buffers tables @ !buffers;
    (* Scene BLASes build side by side, each in its own 256-byte-aligned
       slice of one scratch buffer. *)
    let scratch_offsets =
      List.rev
        (snd
           (List.fold_left
              (fun (offset, acc) blas ->
                let size = Int64.to_int (B.accel_sizes blas).build_scratch_size in
                (offset + ((size + 255) / 256 * 256), offset :: acc))
              (0, []) scene_blas))
    in
    let* work =
      buffer ~memory:Ogpu.Types.Device_local device ~label:"pathtracer-scratch"
        (if scene_blas <> [] then
           List.fold_left (fun total blas -> total + ((Int64.to_int (B.accel_sizes blas).build_scratch_size + 255) / 256 * 256))
             1024 scene_blas
         else max 1024 (Int64.to_int (Int64.mul 2L (B.accel_sizes primitive).build_scratch_size)))
    in
    scratch := Some work;
    let gpu_mesh =
      { positions; normals; materials; curves
      ; structure = primitive; primitive = None; instance_buffer; motion_buffer
      ; prototype_key =
          (match mesh.instances with
           | Some _ when mesh.scene = None ->
               Some (mesh.mesh_positions, mesh.mesh_normals, mesh.mesh_ids, material_data,
                     Bytes.cat extras.sphere_bytes extras.strand_points)
           | _ -> None)
      ; scene_blas; tables
      ; owns_prototype = reused = None; shape }
    in
    let instance_count = match mesh.instances with None -> 0 | Some i -> Array.length i.records in
    if scene_blas <> [] then begin
      (* ponytail: scene BLASes are not compacted (one stage instead of two);
         compaction would save roughly half their memory. *)
      let* command = gpu (B.begin_commands queue) in
      commands := Some command;
      let build = { gpu = gpu_mesh; scratch = work; extra_scratch = None; size_buffer = None
                  ; uncompacted = None; stage = Ready; instance_count } in
      let* encoder = gpu (B.accel_encoder command) in
      let* () =
        List.fold_left2
          (fun acc blas offset ->
            let* () = acc in
            gpu (B.build_accel encoder blas ~scratch:work ~scratch_offset:(Int64.of_int offset) ()))
          (Ok ()) scene_blas scratch_offsets
      in
      let* () = gpu (B.end_accel encoder) in
      let* () = encode_instances device build command in
      structures := build.gpu.structure :: !structures;
      Option.iter (fun extra -> buffers := extra :: !buffers) build.extra_scratch;
      let* receipt = gpu (B.commit command) in
      build.stage <- Compacting receipt;
      Ok build
    end
    else if build_primitive then begin
      let* size_buffer = buffer device ~label:"pathtracer-compacted-size" ~usage:[ Ogpu.Types.Storage; Copy_src ] 8 in
      buffers := size_buffer :: !buffers;
      let* command = gpu (B.begin_commands queue) in
      commands := Some command;
      (* the curve structure builds beside the triangles, in its own scratch *)
      let* curve_scratch = match curves with
        | None -> Ok None
        | Some (c, _) ->
            let* s = buffer ~memory:Ogpu.Types.Device_local device ~label:"pathtracer-curve-scratch"
                (Int64.to_int (B.accel_sizes c).build_scratch_size) in
            Ok (Some s) in
      let* receipt =
        let* encoder = gpu (B.accel_encoder command) in
        let* () = match curves, curve_scratch with
          | Some (c, _), Some scratch -> gpu (B.build_accel encoder c ~scratch ())
          | _ -> Ok () in
        let* () = gpu (B.build_accel encoder primitive ~scratch:work ()) in
        let* () = gpu (B.write_compacted_size encoder primitive ~dst:size_buffer ()) in
        let* () = gpu (B.end_accel encoder) in
        gpu (B.commit command)
      in
      Ok { gpu = gpu_mesh; scratch = work; extra_scratch = curve_scratch; size_buffer = Some size_buffer
         ; uncompacted = None; stage = Building receipt; instance_count }
    end
    else
      (* A reused compacted prototype only needs its instance structure. *)
      let build = { gpu = { gpu_mesh with primitive = Some primitive }; scratch = work; extra_scratch = None
                  ; size_buffer = None; uncompacted = None; stage = Ready; instance_count } in
      let* () = submit_instances device queue build in
      Ok build
  in
  match result with
  | Ok _ as ready -> ready
  | Error _ as failure ->
      Option.iter (fun command -> ignore (B.abandon command)) !commands;
      Option.iter (fun work -> ignore (B.destroy_buffer work)) !scratch;
      List.iter (fun built -> ignore (B.destroy_accel built)) !structures;
      List.iter (fun value -> ignore (B.destroy_buffer value)) !buffers;
      failure

(* Polling a later epoch retires earlier ones, so a wait on an already
   completed receipt is a no-op rather than an invalid-epoch error. *)
let wait queue (receipt : B.receipt) =
  if Ogpu.Backend.completed_epoch queue >= receipt.epoch then Ok ()
  else gpu (B.complete_through queue receipt.epoch)

(* Stage two: build the instance structure over the compacted primitive. *)
let advance_build device queue (build : mesh_build) =
  match build.stage with
  | Ready -> Ok ()
  | Building _ -> (
      let uncompacted = build.gpu.structure in
      let* size_bytes =
        match build.size_buffer with
        | Some size_buffer -> gpu (B.read_buffer size_buffer ~offset:0L ~length:8)
        | None -> Error "path tracer build lost its compacted size"
      in
      let size = Bytes.get_int64_le size_bytes 0 in
      let* compacted = gpu (B.create_accel device (B.Sized { size; template = uncompacted })) in
      build.uncompacted <- Some uncompacted;
      build.gpu.structure <- compacted;
      if build.instance_count > 0 then build.gpu.primitive <- Some compacted;
      let* command = gpu (B.begin_commands queue) in
      let encoded =
        let* encoder = gpu (B.accel_encoder command) in
        let* () = gpu (B.compact_accel encoder ~src:uncompacted ~dst:compacted) in
        let* () = gpu (B.end_accel encoder) in
        let* () = encode_instances device build command in
        gpu (B.commit command)
      in
      match encoded with
      | Ok receipt ->
          build.stage <- Compacting receipt;
          Ok ()
      | Error _ as failure ->
          ignore (B.abandon command);
          failure)
  | Compacting _ ->
      Option.iter (fun structure -> ignore (B.destroy_accel structure)) build.uncompacted;
      build.uncompacted <- None;
      Option.iter (fun size_buffer -> ignore (B.destroy_buffer size_buffer)) build.size_buffer;
      build.size_buffer <- None;
      build.stage <- Ready;
      Ok ()

let finish_build device queue (build : mesh_build) =
  let rec finish () =
    match build.stage with
    | Ready -> Ok ()
    | Building receipt | Compacting receipt ->
        let* () = wait queue receipt in
        let* () = advance_build device queue build in
        finish ()
  in
  let finished = finish () in
  ignore (B.destroy_buffer build.scratch);
  Option.iter (fun extra -> ignore (B.destroy_buffer extra)) build.extra_scratch;
  match finished with
  | Ok () -> Ok build.gpu
  | Error _ as failure ->
      Option.iter (fun structure -> ignore (B.destroy_accel structure)) build.uncompacted;
      Option.iter (fun size_buffer -> ignore (B.destroy_buffer size_buffer)) build.size_buffer;
      destroy_gpu build.gpu;
      failure

let discard_build queue (build : mesh_build) =
  (match build.stage with
   | Ready -> ()
   | Building receipt | Compacting receipt -> ignore (wait queue receipt));
  ignore (B.destroy_buffer build.scratch);
  Option.iter (fun extra -> ignore (B.destroy_buffer extra)) build.extra_scratch;
  Option.iter (fun structure -> ignore (B.destroy_accel structure)) build.uncompacted;
  Option.iter (fun size_buffer -> ignore (B.destroy_buffer size_buffer)) build.size_buffer;
  destroy_gpu build.gpu
