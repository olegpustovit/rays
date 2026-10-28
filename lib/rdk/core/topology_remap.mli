(** Shared exact-ancestry remapping for topology-changing RDK kernels. *)

val select :
  ?cancel:Cancel.t -> grain:int -> ?missing:'a -> int array -> 'a array -> 'a array
(** [missing] is the value of a row whose source index is [-1]. *)

val select_float :
  ?cancel:Cancel.t -> grain:int -> ?missing:float -> int array -> float array -> float array

val attribute :
  ?cancel:Cancel.t -> grain:int -> ?defaults:bool -> int array -> Attribute.t -> Attribute.t
(** With [defaults], a source index of [-1] is a created element: zero, [""], empty ragged row. *)

val drop_point_vertex_normals : (Attribute.owner * string) list

val mapped_attribute :
  ?cancel:Cancel.t -> grain:int -> point_map:int array ->
  vertex_map:int array -> primitive_map:int array ->
  drop:(Attribute.owner * string) list -> Attribute.t ->
  (Attribute.t option, string) result
(** Remap by owner and omit attributes named in [drop]. *)

val group :
  ?cancel:Cancel.t -> grain:int -> int array -> Group.t -> Group.t

val mapped_group :
  grain:int -> point_map:int array ->
  vertex_map:int array -> primitive_map:int array -> Group.t -> Group.t

val attributes :
  ?cancel:Cancel.t -> grain:int -> point_map:int array ->
  vertex_map:int array -> primitive_map:int array -> Geometry.t ->
  Attribute.t list

val groups :
  ?cancel:Cancel.t -> grain:int -> point_map:int array -> vertex_map:int array ->
  primitive_map:int array -> Geometry.t -> Group.t list

val edge_groups :
  ?cancel:Cancel.t -> source_topology:Topology.t ->
  target_topology:Topology.t -> point_map:int array -> Edge_group.t list ->
  (Edge_group.t list, string) result

val preserving_points :
  ?cancel:Cancel.t -> grain:int -> topology:Topology.t ->
  vertex_map:int array -> primitive_map:int array -> Geometry.t ->
  (Geometry.t, string) result
(** Install [topology] while sharing positions, point/detail attributes, and
    point groups. Vertex/primitive payload follows exact source ancestry;
    native edge groups follow unchanged point endpoints. *)

val split_point_edge_groups :
  ?cancel:Cancel.t -> grain:int -> source_index:Topology_index.t ->
  target_topology:Topology.t -> Edge_group.t list ->
  (Edge_group.t list, string) result
(** Replicate topology-affine membership when a topology edit changes only
    corner-to-point indices and preserves corner order/cardinality. Every
    target edge records its unique source edge through corresponding source
    corners. The implementation retains a lightweight target endpoint table
    and one integer ancestry plane, not a complete target reverse CSR index. *)

(* Exact OR ancestry per target unique edge. Inputs remain borrowed immutable
   arrays; outputs own their bitsets. CSR shape, index bounds and topology/group
   affinity are validated before copying. Empty rows are unselected. *)
val edge_groups_from_ancestry :
  ?cancel:Cancel.t -> grain:int -> source_index:Topology_index.t ->
  target_topology:Topology.t -> target_index:Topology_index.t ->
  offsets:int array -> ancestors:int array -> Edge_group.t list ->
  (Edge_group.t list, string) result

val blend_attribute :
  ?cancel:Cancel.t -> grain:int -> int array -> int array -> float array -> Attribute.t -> Attribute.t
(** [blend_attribute ~grain left right weight attribute]: row [i] mixes source rows [left.(i)] and
    [right.(i)] at [weight.(i)] (all in bounds). Float and vector storage: [a + (b - a) * t], or
    [a * (1 - t) + b * t] when the delta is not finite; a [Float3] named ["N"] is renormalised when
    its length exceeds [1e-20]. Int, text and ragged rows take [left] when [t < 0.5], else [right].
    Bytes equal [Curve_join.interpolate_attribute]'s. Raises [Invalid_argument] on a non-positive grain. *)

val blend_group :
  ?cancel:Cancel.t -> grain:int -> int array -> int array -> float array -> Group.t -> Group.t
(** Membership of the nearer source row ([left] when [t < 0.5]); ordered groups follow {!group}. *)

val blend_positions :
  ?cancel:Cancel.t -> grain:int -> int array -> int array -> float array ->
  Packed.Float3.t -> Packed.Float3.t
(** The point positions mixed like a [Float] attribute (no renormalisation). *)
