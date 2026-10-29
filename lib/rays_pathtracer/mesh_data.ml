module B = Ogpu.Backend

let ( let* ) = Result.bind
let put_u32 bytes offset value = Bytes.set_int32_le bytes offset (Int32.of_int value)
let put_f32 bytes offset value = Bytes.set_int32_le bytes offset (Int32.bits_of_float value)
let add_f32 builder value = Stdlib.Buffer.add_int32_le builder (Int32.bits_of_float value)

let add_vec3 builder (x, y, z) =
  add_f32 builder x;
  add_f32 builder y;
  add_f32 builder z

type instances = {
  records : B.instance_record array;
  shader_bytes : bytes;
  motion_transforms : float array array option;
  matrices : Rays_math.Mat4.t array;
  motion_end : Rays_math.Mat4.t array option;
}

(* A scene is a TLAS over several BLASes, one per part and one geometry kind
   each (Metal cannot mix kinds in one BLAS). A placement is one TLAS
   instance: its world matrix, its part, and its material (-1: the per-triangle
   ids). The shader table holds, per instance, the part's first triangle in
   the shared vertex buffer and that material. A [scene_mesh] layout also
   keeps, for [move], each object's placement range ([objects.(o)] to
   [objects.(o + 1)]; empty for [mesh]) and each placement's matrix relative
   to its object. *)
type part = Triangle_part of int * int (* first vertex, vertex count *)

type layout = {
  parts : part array;
  placements : (Rays_math.Mat4.t * int * int) array;
  table : bytes;
  objects : int array;
  placed : Rays_math.Mat4.t array; (* each object's world matrix *)
  locals : Rays_math.Mat4.t array;
}

(* Prepared analytic primitives: spheres as (centre, radius) float4s whose materials start at
   [sphere_material], strands as linear round curves (control points, radii, each segment's first point and
   material). *)
type extras = {
  sphere_bytes : bytes;
  sphere_material : int;
  sphere_count : int;
  strand_points : bytes;
  strand_radii : bytes;
  strand_indices : bytes;
  strand_ids : bytes;
  strand_point_count : int;
  strand_segments : int;
}

(* A device mesh's triangles ({!Rays.Mesh.View.create_device}): the registration [key] names its
   packed float3 positions and normals on the device, [corners] its topology (three vertex indices
   per triangle, kept by identity), [holder] the mesh that keeps them alive. The host arrays of the
   mesh it is part of stay empty: the tracer gathers the corners on the device. *)
type device = { key : string; vertex_count : int; corners : int array; holder : Rays.Mesh.t }

type 'material t = {
  device : device option;
  mesh_positions : bytes;
  mesh_normals : bytes;
  mesh_ids : bytes;
  triangles : int;
  mesh_materials : 'material list;
  instances : instances option;
  extras : extras;
  scene : layout option;
}

let flat_mesh ~extras ~materials source_meshes =
  let flatten objects =
    let positions = Stdlib.Buffer.create 4096
    and normals = Stdlib.Buffer.create 4096
    and ids = Stdlib.Buffer.create 1024
    and count = ref 0 in
    let* () =
      List.fold_left
        (fun acc (source_mesh, material_index) ->
          let* () = acc in
          let view = Rays.Mesh.View.packed_view source_mesh in
          if view.mode <> Rays.Mesh.Triangles then Error "path tracer meshes must be triangle-ready"
          else
            let* normals_view =
              match view.normals with
              | Some normals -> Ok normals
              | None -> Error "path tracer meshes need prepared vertex normals"
            in
            if Array.length view.indices mod 3 <> 0 then
              Error "path tracer mesh indices must contain complete triangles"
            else
              let positions_view = view.vertices in
              let component (plane : Rays.Mesh.View.vec3_view) index =
                (plane.x.(index), plane.y.(index), plane.z.(index))
              in
              let id_bytes = Bytes.create 4 in
              put_u32 id_bytes 0 material_index;
              for triangle = 0 to (Array.length view.indices / 3) - 1 do
                for corner = 0 to 2 do
                  let vertex = view.indices.((triangle * 3) + corner) in
                  add_vec3 positions (component positions_view vertex);
                  add_vec3 normals (component normals_view vertex)
                done;
                Stdlib.Buffer.add_bytes ids id_bytes;
                incr count
              done;
              Ok ())
        (Ok ()) objects
    in
    if !count = 0 && extras.sphere_count = 0 && extras.strand_segments = 0 then
      Error "path tracer scene has no triangles"
    else
      Ok
        ( Stdlib.Buffer.to_bytes positions,
          Stdlib.Buffer.to_bytes normals,
          Stdlib.Buffer.to_bytes ids,
          !count )
  in
  let* mesh_positions, mesh_normals, mesh_ids, triangles =
    flatten (List.mapi (fun index mesh -> (mesh, index)) source_meshes)
  in
  Ok
    {
      device = None;
      mesh_positions;
      mesh_normals;
      mesh_ids;
      triangles;
      mesh_materials = materials;
      instances = None;
      extras;
      scene = None;
    }

(* Column-major 3x4 transform plus its 3x3 normal matrix (cofactors / det):
   seven packed float3 columns, 84 bytes. *)
let transform_columns (matrix : Rays_math.Mat4.t) =
  let values =
    Array.init 16 (fun index -> Rays_math.Mat4.get matrix ~row:(index / 4) ~column:(index mod 4))
  in
  let a = values.(0)
  and b = values.(1)
  and c = values.(2)
  and d = values.(4)
  and e = values.(5)
  and f = values.(6)
  and g = values.(8)
  and h = values.(9)
  and i = values.(10) in
  let cofactors =
    [|
      (e *. i) -. (f *. h);
      (c *. h) -. (b *. i);
      (b *. f) -. (c *. e);
      (f *. g) -. (d *. i);
      (a *. i) -. (c *. g);
      (c *. d) -. (a *. f);
      (d *. h) -. (e *. g);
      (b *. g) -. (a *. h);
      (a *. e) -. (b *. d);
    |]
  in
  let determinant = (a *. cofactors.(0)) +. (b *. cofactors.(3)) +. (c *. cofactors.(6)) in
  let finite value = Float.is_finite value && Float.abs value <= 3.402823466e38 in
  if
    (not (Array.for_all finite values))
    || Float.abs values.(12) > 1e-6
    || Float.abs values.(13) > 1e-6
    || Float.abs values.(14) > 1e-6
    || Float.abs (values.(15) -. 1.) > 1e-6
    || (not (Float.is_finite determinant))
    || determinant = 0.
  then Error "path tracer instance transform must be finite, affine, and nonsingular"
  else
    let normal = Array.map (fun value -> value /. determinant) cofactors in
    if not (Array.for_all finite normal) then
      Error "path tracer instance normal transform exceeds float32 range"
    else Ok (Array.sub values 0 12, normal)

let put_transform_record bytes base (transform, normal) =
  for column = 0 to 3 do
    for row = 0 to 2 do
      put_f32 bytes (base + (((column * 3) + row) * 4)) transform.((row * 4) + column)
    done
  done;
  for column = 0 to 2 do
    for row = 0 to 2 do
      put_f32 bytes (base + 48 + (((column * 3) + row) * 4)) normal.((column * 3) + row)
    done
  done

(* Packs placements into TLAS records (structure index = part), shader
   transform records, and the per-instance (first triangle, material) table. *)
let layout_mesh ?(objects = [||]) ?(placed = [||]) ?(locals = [||]) base parts placements =
  let count = Array.length placements in
  let records =
    Array.make count
      { B.instance = { transform = [||]; mask = 0; structure_index = 0 }; user_id = 0 }
  and shader_bytes = Bytes.create (count * 84)
  and table = Bytes.create (count * 8) in
  let rec pack index =
    if index = count then Ok ()
    else
      let matrix, part, material = placements.(index) in
      let* columns = transform_columns matrix in
      records.(index) <-
        {
          B.instance = { transform = fst columns; mask = 0xFFFF_FFFF; structure_index = part };
          user_id = 0;
        };
      put_transform_record shader_bytes (index * 84) columns;
      put_u32 table (index * 8) (match parts.(part) with Triangle_part (first, _) -> first / 3);
      put_u32 table ((index * 8) + 4) material;
      pack (index + 1)
  in
  let* () = pack 0 in
  Ok
    {
      base with
      instances =
        Some
          {
            records;
            shader_bytes;
            motion_transforms = None;
            motion_end = None;
            matrices = Array.map (fun (m, _, _) -> m) placements;
          };
      scene = Some { parts; placements; table; objects; placed; locals };
    }

(* Spheres and strands are traced beside the triangle structure (scene/trace), which then needs a triangle: a
   degenerate one, never hit, stands in when there is none. *)
let mesh ~extras ~materials source_meshes =
  let* flat = flat_mesh ~extras ~materials source_meshes in
  if flat.triangles = 0 then
    Ok
      {
        flat with
        mesh_positions = Bytes.make 36 '\000';
        mesh_normals = Bytes.make 36 '\000';
        mesh_ids = Bytes.make 4 '\000';
      }
  else Ok flat

let pack_instances ~materials ~motion transforms =
  let count = Array.length transforms in
  let stride = if motion = None then 84 else 168 in
  let records =
    Array.make count
      { B.instance = { transform = [||]; mask = 0; structure_index = 0 }; user_id = 0 }
  and shader_bytes = Bytes.create (count * stride)
  and motion_transforms = Array.make (2 * count) [||] in
  let rec pack instance =
    if instance = count then Ok ()
    else
      let* start = transform_columns transforms.(instance) in
      let* finish =
        match motion with None -> Ok start | Some ends -> transform_columns ends.(instance)
      in
      records.(instance) <-
        {
          B.instance = { transform = fst start; mask = 0xFFFF_FFFF; structure_index = 0 };
          user_id = (if materials = 0 then 0 else 1 + instance);
        };
      put_transform_record shader_bytes (instance * stride) start;
      if motion <> None then put_transform_record shader_bytes ((instance * stride) + 84) finish;
      motion_transforms.(2 * instance) <- fst start;
      motion_transforms.((2 * instance) + 1) <- fst finish;
      pack (instance + 1)
  in
  let* () = pack 0 in
  Ok
    {
      records;
      shader_bytes;
      matrices = transforms;
      motion_end = motion;
      motion_transforms = (if motion = None then None else Some motion_transforms);
    }

let mesh_instanced ~(prototype : 'material t) ?(materials = [||]) ?motion transforms =
  if
    prototype.scene <> None || prototype.instances <> None || Option.is_some prototype.device
    || prototype.extras.sphere_count <> 0
    || prototype.extras.strand_segments <> 0
  then Error "path tracer instancing needs one prepared triangle mesh"
  else
    let count = Array.length transforms in
    if count = 0 then Error "path tracer instancing needs at least one transform"
    else if prototype.triangles > max_int / count then
      Error "path tracer instanced triangle count overflows"
    else if count > max_int / 168 then Error "path tracer transform storage overflows"
    else if Array.length materials <> 0 && Array.length materials <> count then
      Error "path tracer instance materials must match the transform count"
    else if match motion with Some ends -> Array.length ends <> count | None -> false then
      Error "path tracer motion transforms must match the transform count"
    else
      let* instances = pack_instances ~materials:(Array.length materials) ~motion transforms in
      Ok
        {
          prototype with
          triangles = prototype.triangles * count;
          mesh_materials = prototype.mesh_materials @ Array.to_list materials;
          instances = Some instances;
        }

(* Without [Ray_tracing_motion] a motion mesh is built at the shutter midpoint:
   the mean of its start and end transforms, as an ordinary instanced mesh. *)
let shutter_midpoint (m : 'material t) =
  match m.instances with
  | Some ({ motion_end = Some ends; _ } as instances) ->
      let mid a b =
        let m r c =
          (Rays_math.Mat4.get a ~row:r ~column:c +. Rays_math.Mat4.get b ~row:r ~column:c) /. 2.
        in
        let row r = (m r 0, m r 1, m r 2, m r 3) in
        Rays_math.Mat4.of_rows (row 0) (row 1) (row 2) (row 3)
      in
      let used = Array.exists (fun (r : B.instance_record) -> r.user_id <> 0) instances.records in
      let* packed =
        pack_instances
          ~materials:(if used then 1 else 0)
          ~motion:None
          (Array.mapi (fun i a -> mid a ends.(i)) instances.matrices)
      in
      Ok { m with instances = Some packed }
  | _ -> Ok m

let scene_mesh (objects : (Rays_math.Mat4.t * 'material t) list) =
  (* Each input as its parts and placements in its own index spaces. *)
  let input (matrix, (m : 'material t)) =
    let whole = [| Triangle_part (0, Bytes.length m.mesh_positions / 12) |] in
    match (m.scene, m.instances) with
    | _ when Option.is_some m.device -> Error "path tracer scene_mesh objects are host meshes"
    | _ when m.extras.sphere_count > 0 || m.extras.strand_segments > 0 ->
        (* ponytail: spheres and strands would need per-instance bases in the
           intersection function; scene objects are triangle meshes. *)
        Error "path tracer scene_mesh objects cannot carry spheres or strands"
    | _, Some { motion_transforms = Some _; _ } ->
        Error "path tracer scene_mesh objects cannot carry motion"
    | Some layout, _ -> Ok (matrix, m, layout.parts, layout.placements)
    | None, None -> Ok (matrix, m, whole, [| (Rays_math.Mat4.identity, 0, -1) |])
    | None, Some instances ->
        Ok
          ( matrix,
            m,
            whole,
            Array.mapi
              (fun i record -> (instances.matrices.(i), 0, record.B.user_id))
              instances.records )
  in
  let rec inputs acc = function
    | [] -> Ok (List.rev acc)
    | object_ :: rest ->
        let* value = input object_ in
        inputs (value :: acc) rest
  in
  let* inputs = inputs [] objects in
  if inputs = [] then Error "path tracer scene needs at least one object"
  else
    let positions = Stdlib.Buffer.create 4096
    and normals = Stdlib.Buffer.create 4096
    and ids = Stdlib.Buffer.create 1024 in
    let parts = ref []
    and placements = ref []
    and materials = ref []
    and locals = ref []
    and objects = ref [ 0 ]
    and placed = List.map (fun (matrix, _, _, _) -> matrix) inputs in
    let vertex_base = ref 0 and part_base = ref 0 and material_base = ref 0 and triangles = ref 0 in
    List.iter
      (fun (matrix, (m : 'material t), input_parts, input_placements) ->
        Stdlib.Buffer.add_bytes positions m.mesh_positions;
        Stdlib.Buffer.add_bytes normals m.mesh_normals;
        for i = 0 to (Bytes.length m.mesh_ids / 4) - 1 do
          Stdlib.Buffer.add_int32_le ids
            (Int32.add (Bytes.get_int32_le m.mesh_ids (4 * i)) (Int32.of_int !material_base))
        done;
        Array.iter
          (fun (Triangle_part (first, count)) ->
            parts := Triangle_part (first + !vertex_base, count) :: !parts)
          input_parts;
        Array.iter
          (fun (instance, part, material) ->
            locals := instance :: !locals;
            placements :=
              ( Rays_math.Mat4.mul matrix instance,
                part + !part_base,
                if material < 0 then -1 else material + !material_base )
              :: !placements)
          input_placements;
        objects := (List.hd !objects + Array.length input_placements) :: !objects;
        materials := List.rev_append m.mesh_materials !materials;
        vertex_base := !vertex_base + (Bytes.length m.mesh_positions / 12);
        part_base := !part_base + Array.length input_parts;
        material_base := !material_base + List.length m.mesh_materials;
        triangles := !triangles + m.triangles)
      inputs;
    let extras =
      {
        sphere_bytes = Bytes.empty;
        sphere_material = 0;
        sphere_count = 0;
        strand_points = Bytes.empty;
        strand_radii = Bytes.empty;
        strand_indices = Bytes.empty;
        strand_ids = Bytes.empty;
        strand_point_count = 0;
        strand_segments = 0;
      }
    in
    layout_mesh
      ~objects:(Array.of_list (List.rev !objects))
      ~placed:(Array.of_list placed)
      ~locals:(Array.of_list (List.rev !locals))
      {
        device = None;
        mesh_positions = Stdlib.Buffer.to_bytes positions;
        mesh_normals = Stdlib.Buffer.to_bytes normals;
        mesh_ids = Stdlib.Buffer.to_bytes ids;
        triangles = !triangles;
        mesh_materials = List.rev !materials;
        instances = None;
        extras;
        scene = None;
      }
      (Array.of_list (List.rev !parts))
      (Array.of_list (List.rev !placements))

(* The same scene with object [o]'s placements at [matrices.(o) * local]. *)
let moved_mesh (m : 'material t) matrices =
  match (m.scene, m.instances) with
  | Some layout, Some instances when Array.length layout.objects > 1 ->
      let matrices = Array.of_list matrices and objects = layout.objects in
      if Array.length matrices <> Array.length objects - 1 then
        Error "path tracer move needs one matrix per scene_mesh object"
      else
        (* Only objects whose matrix changed repack their placements. *)
        let placements = Array.copy layout.placements
        and records = Array.copy instances.records
        and shader_bytes = Bytes.copy instances.shader_bytes
        and world = Array.copy instances.matrices in
        let rec place o i =
          if o = Array.length matrices then Ok ()
          else if
            i = objects.(o + 1)
            || Rays_math.Mat4.nearly_equal matrices.(o) layout.placed.(o) ~eps:0.
          then place (o + 1) objects.(o + 1)
          else
            let _, part, material = placements.(i) in
            let matrix = Rays_math.Mat4.mul matrices.(o) layout.locals.(i) in
            let* columns = transform_columns matrix in
            placements.(i) <- (matrix, part, material);
            world.(i) <- matrix;
            records.(i) <-
              {
                (records.(i)) with
                instance = { records.(i).instance with transform = fst columns };
              };
            put_transform_record shader_bytes (i * 84) columns;
            place o (i + 1)
        in
        let* () = place 0 0 in
        Ok
          {
            m with
            instances = Some { instances with records; shader_bytes; matrices = world };
            scene = Some { layout with placements; placed = matrices };
          }
  | _ -> Error "path tracer move needs a scene_mesh scene"
