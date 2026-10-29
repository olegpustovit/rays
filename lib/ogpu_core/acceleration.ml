type buffer_range = { buffer : unit Handle.t; buffer_size : int64; offset : int64; length : int64 }

type geometry =
  | Triangles of { vertices : buffer_range; vertex_stride : int; vertex_count : int }
  | Motion_triangles of { keyframes : buffer_range list; vertex_stride : int; vertex_count : int }
  | Bounding_boxes of { boxes : buffer_range list; stride : int; count : int }
  | Curves of {
      control_points : buffer_range list;
      control_stride : int;
      control_point_count : int;
      radii : buffer_range list;
      radius_stride : int;
      indices : buffer_range;
      segment_count : int;
      control_points_per_segment : int;
    }

type instance_kind = User_id_instances | Motion_instances

type descriptor =
  | Blas of { geometries : geometry array; motion_keyframes : int option }
  | Tlas of {
      instances : buffer_range;
      instance_stride : int;
      instance_count : int;
      instance_kind : instance_kind;
      structures : unit Handle.t list;
    }
  | Sized of { size : int64; template : unit Handle.t }

type state = Empty | Built | Compacted
type t = { handle : unit Handle.t; descriptor : descriptor; refit : bool; mutable state : state }

let invalid op text = Error (Error.make op Error.Invalid_argument text)

let validate_range device range =
  let op = "Ogpu.Acceleration.validate_range" in
  match Handle.validate_for ~operation:op device range.buffer with
  | Error _ as e -> e
  | Ok ()
    when range.buffer_size < 0L || range.offset < 0L || range.length <= 0L
         || Int64.rem range.offset 4L <> 0L ->
      invalid op "buffer range is invalid or unaligned"
  | Ok () when range.offset > Int64.sub range.buffer_size range.length ->
      invalid op "buffer range exceeds its buffer"
  | Ok () -> Ok ()

let validate_ranges device ranges =
  List.fold_left
    (fun result range -> Result.bind result (fun () -> validate_range device range))
    (Ok ()) ranges

(* Every keyframed range must have exactly the descriptor's keyframe count
   (one for a static structure). *)
let keyframes_ok expected ranges = List.length ranges = Option.value expected ~default:1

let validate_geometry device ~motion = function
  | Triangles { vertices; vertex_stride; vertex_count } ->
      if
        vertex_stride < 12
        || vertex_stride mod 4 <> 0
        || vertex_count < 3
        || vertex_count mod 3 <> 0
      then invalid "Ogpu.Acceleration.Triangles" "triangle cardinality/stride is invalid"
      else if motion <> None then
        invalid "Ogpu.Acceleration.Triangles" "static triangles cannot join a motion structure"
      else validate_range device vertices
  | Motion_triangles { keyframes; vertex_stride; vertex_count } ->
      if
        vertex_stride < 12
        || vertex_stride mod 4 <> 0
        || vertex_count < 3
        || vertex_count mod 3 <> 0
      then invalid "Ogpu.Acceleration.Motion_triangles" "triangle cardinality/stride is invalid"
      else if motion = None || not (keyframes_ok motion keyframes) then
        invalid "Ogpu.Acceleration.Motion_triangles"
          "keyframe count must match the structure's motion keyframes"
      else validate_ranges device keyframes
  | Bounding_boxes { boxes; stride; count } ->
      if stride < 24 || stride mod 4 <> 0 || count <= 0 then
        invalid "Ogpu.Acceleration.Bounding_boxes" "box cardinality/stride is invalid"
      else if not (keyframes_ok motion boxes) then
        invalid "Ogpu.Acceleration.Bounding_boxes"
          "keyframe count must match the structure's motion keyframes"
      else validate_ranges device boxes
  | Curves
      {
        control_points;
        control_stride;
        control_point_count;
        radii;
        radius_stride;
        indices;
        segment_count;
        control_points_per_segment;
      } ->
      if
        control_stride < 12 || radius_stride < 4 || control_point_count < 2 || segment_count <= 0
        || control_points_per_segment < 2 || control_points_per_segment > 4
      then invalid "Ogpu.Acceleration.Curves" "curve strides, counts, or segment shape are invalid"
      else if
        (not (keyframes_ok motion control_points))
        || List.length radii <> List.length control_points
      then
        invalid "Ogpu.Acceleration.Curves"
          "keyframe count must match the structure's motion keyframes"
      else
        Result.bind
          (validate_ranges device (control_points @ radii))
          (fun () -> validate_range device indices)

let geometry_kind = function
  | Triangles _ | Motion_triangles _ -> 0
  | Bounding_boxes _ -> 1
  | Curves _ -> 2

let validate_descriptor device = function
  | Blas { geometries; _ } when Array.length geometries = 0 ->
      invalid "Ogpu.Acceleration.create" "BLAS geometry array is empty"
  | Blas { motion_keyframes = Some k; _ } when k < 2 ->
      invalid "Ogpu.Acceleration.create" "motion requires at least two keyframes"
  (* Metal sizes a primitive structure by its first geometry's kind and sends
     that kind's selectors to every geometry, so one BLAS holds one kind. *)
  | Blas { geometries; _ }
    when Array.exists (fun g -> geometry_kind g <> geometry_kind geometries.(0)) geometries ->
      invalid "Ogpu.Acceleration.create"
        "BLAS geometries must all be one kind (triangles, bounding boxes, or curves)"
  | Blas { geometries; motion_keyframes; _ } ->
      Array.fold_left
        (fun result geometry ->
          Result.bind result (fun () -> validate_geometry device ~motion:motion_keyframes geometry))
        (Ok ()) geometries
  | Tlas { instances; instance_stride; instance_count; instance_kind; structures; _ } ->
      let minimum_stride =
        match instance_kind with User_id_instances -> 68 | Motion_instances -> 44
      in
      if instance_count <= 0 || instance_stride < minimum_stride || instance_stride mod 4 <> 0 then
        invalid "Ogpu.Acceleration.create" "instance cardinality/stride is invalid"
      else if structures = [] then
        invalid "Ogpu.Acceleration.create" "TLAS references no structures"
      else
        Result.bind (validate_range device instances) (fun () ->
            List.fold_left
              (fun result handle ->
                Result.bind result (fun () ->
                    Handle.validate_for ~operation:"Ogpu.Acceleration.create" device handle))
              (Ok ()) structures)
  | Sized { size; template } ->
      if size <= 0L then invalid "Ogpu.Acceleration.create" "structure size must be positive"
      else Handle.validate_for ~operation:"Ogpu.Acceleration.create" device template

let create device ~ray_tracing ?(refit = false) descriptor =
  if not ray_tracing then
    Error (Error.make "Ogpu.Acceleration.create" Error.Unsupported "ray tracing is unsupported")
  else if refit && match descriptor with Blas _ -> false | Tlas _ | Sized _ -> true then
    invalid "Ogpu.Acceleration.create" "only a primitive structure is refittable"
  else
    Result.bind (validate_descriptor device descriptor) (fun () ->
        Ok { handle = Handle.create ~device; descriptor; refit; state = Empty })

let check op device value = Handle.validate_for ~operation:op device value.handle

let build device value =
  let op = "Ogpu.Acceleration.build" in
  Result.bind (check op device value) (fun () ->
      match (value.descriptor, value.state) with
      | Sized _, _ ->
          Error
            (Error.make op Error.Invalid_state "a sized structure is filled by copy or compaction")
      | _, Empty ->
          value.state <- Built;
          Ok ()
      | _, (Built | Compacted) ->
          Error (Error.make op Error.Invalid_state "structure has already been built"))

(* A refit keeps the structure's shape: the same geometries in kind, count and stride (and motion
   keyframes); only their buffers and contents change. *)
let same_shape a b =
  match (a, b) with
  | Blas a, Blas b ->
      a.motion_keyframes = b.motion_keyframes
      && Array.length a.geometries = Array.length b.geometries
      && Array.for_all2
           (fun x y ->
             match (x, y) with
             | Triangles x, Triangles y ->
                 x.vertex_stride = y.vertex_stride && x.vertex_count = y.vertex_count
             | Motion_triangles x, Motion_triangles y ->
                 x.vertex_stride = y.vertex_stride && x.vertex_count = y.vertex_count
             | Bounding_boxes x, Bounding_boxes y -> x.stride = y.stride && x.count = y.count
             | Curves x, Curves y ->
                 x.control_stride = y.control_stride
                 && x.control_point_count = y.control_point_count
                 && x.radius_stride = y.radius_stride && x.segment_count = y.segment_count
                 && x.control_points_per_segment = y.control_points_per_segment
             | _ -> false)
           a.geometries b.geometries
  | _ -> false

let refit device value descriptor =
  let op = "Ogpu.Acceleration.refit" in
  Result.bind (check op device value) (fun () ->
      if not value.refit then
        Error (Error.make op Error.Invalid_state "structure was not created refittable")
      else if value.state <> Built then
        Error (Error.make op Error.Invalid_state "refit requires a built, uncompacted structure")
      else
        Result.bind (validate_descriptor device descriptor) (fun () ->
            if same_shape value.descriptor descriptor then Ok ()
            else invalid op "refit geometry differs in kind, count or stride from the build"))

let compact_into device ~source ~destination =
  let op = "Ogpu.Acceleration.compact" in
  Result.bind (check op device source) (fun () ->
      Result.bind (check op device destination) (fun () ->
          match (source.state, destination.state) with
          | Built, Empty ->
              destination.state <- Compacted;
              Ok ()
          | (Empty | Compacted), _ ->
              Error
                (Error.make op Error.Invalid_state "compaction requires an uncompacted built source")
          | _, (Built | Compacted) ->
              Error (Error.make op Error.Invalid_state "compaction destination is already filled")))

let compacted_size device value =
  let op = "Ogpu.Acceleration.compacted_size" in
  Result.bind (check op device value) (fun () ->
      match value.state with
      | Built -> Ok ()
      | Empty | Compacted ->
          Error
            (Error.make op Error.Invalid_state
               "compacted size requires an uncompacted built structure"))

let destroy value = Handle.destroy value.handle
let handle value = value.handle
let built value = match value.state with Built | Compacted -> true | Empty -> false
let descriptor value = value.descriptor

(* Instance records packed to a driver-reported layout: [size; transform;
   options; mask; structure_index; user_id; transforms_start;
   transforms_count; start_border; end_border; start_time; end_time], with -1
   where the record kind lacks the field. *)
type instance_layout = int array

let put_transform bytes base transform =
  for column = 0 to 3 do
    for row = 0 to 2 do
      Bytes.set_int32_le bytes
        (base + (((column * 3) + row) * 4))
        (Int32.bits_of_float transform.((row * 4) + column))
    done
  done

let put32 bytes offset value =
  if offset >= 0 then Bytes.set_int32_le bytes offset (Int32.of_int value)

let put_float bytes offset value =
  if offset >= 0 then Bytes.set_int32_le bytes offset (Int32.bits_of_float value)

let pack_records (layout : instance_layout) count fill =
  let size = layout.(0) in
  let bytes = Bytes.make (count * size) '\000' in
  for i = 0 to count - 1 do
    fill bytes (i * size)
  done;
  bytes

let pack_instances layout instances =
  pack_records layout (Array.length instances) (fun bytes base ->
      let transform, mask, structure_index, user_id = instances.(base / layout.(0)) in
      if layout.(1) >= 0 then put_transform bytes (base + layout.(1)) transform;
      put32 bytes (base + layout.(3)) mask;
      put32 bytes (base + layout.(4)) structure_index;
      if layout.(5) >= 0 then put32 bytes (base + layout.(5)) user_id)

let pack_motion_instances layout instances =
  pack_records layout (Array.length instances) (fun bytes base ->
      let ( (mask, structure_index, user_id),
            (transforms_start, transforms_count),
            (start_border, end_border),
            (start_time, end_time) ) =
        instances.(base / layout.(0))
      in
      put32 bytes (base + layout.(3)) mask;
      put32 bytes (base + layout.(4)) structure_index;
      put32 bytes (base + layout.(5)) user_id;
      put32 bytes (base + layout.(6)) transforms_start;
      put32 bytes (base + layout.(7)) transforms_count;
      put32 bytes (base + layout.(8)) start_border;
      put32 bytes (base + layout.(9)) end_border;
      put_float bytes (base + layout.(10)) start_time;
      put_float bytes (base + layout.(11)) end_time)

(* Packed 4x3 column-major transforms, 48 bytes each, for motion keyframes. *)
let pack_transforms transforms =
  let bytes = Bytes.make (Array.length transforms * 48) '\000' in
  Array.iteri (fun i transform -> put_transform bytes (i * 48) transform) transforms;
  bytes
