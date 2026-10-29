type buffer_range = { buffer : unit Handle.t; buffer_size : int64; offset : int64; length : int64 }

(** Keyframed ranges hold one entry for a static structure and exactly the structure's motion
    keyframe count otherwise. A BLAS holds one geometry kind (static and motion triangles count as
    one); mixing triangles, bounding boxes, and curves is [Invalid_argument]. *)
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

type t

val create :
  Handle.device -> ray_tracing:bool -> ?refit:bool -> descriptor -> (t, Error.t) result
(** [refit] (a [Blas] only) allows {!refit} after the build. *)
val build : Handle.device -> t -> (unit, Error.t) result
val refit : Handle.device -> t -> descriptor -> (unit, Error.t) result
(** An in-place update of a built refittable structure from [descriptor]: the same geometries in
    kind, count and stride, over new buffers or contents. *)

val compact_into : Handle.device -> source:t -> destination:t -> (unit, Error.t) result
val compacted_size : Handle.device -> t -> (unit, Error.t) result
val destroy : t -> unit
val handle : t -> unit Handle.t
val built : t -> bool
val descriptor : t -> descriptor

type instance_layout = int array
(** Byte layout of one instance record:
    [size; transform; options; mask; structure_index; user_id; transforms_start; transforms_count;
     start_border; end_border; start_time; end_time], -1 where absent. *)

val pack_instances : instance_layout -> (float array * int * int * int) array -> bytes

val pack_motion_instances :
  instance_layout ->
  ((int * int * int) * (int * int) * (int * int) * (float * float)) array ->
  bytes

val pack_transforms : float array array -> bytes
