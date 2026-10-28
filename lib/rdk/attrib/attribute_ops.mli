(** Attribute ownership and reduction operations over packed topology. *)

type method_ =
  | First
  | Last
  | Average
  | Minimum
  | Maximum
  | Mode
  | Median
  | Sum
  | Sum_squares
  | Root_mean_square
  | Array_all
  | Unique_values

val promote_pattern :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?method_:method_ ->
  ?delete_source:bool ->
  ?piece_attribute:string ->
  ?into_pattern:string ->
  ?index_pattern:string ->
  source:Attribute.owner ->
  destination:Attribute.owner ->
  pattern:string ->
  Geometry.t ->
  (Geometry.t, Error.t) result
(** Promote an ordinary attribute between point, vertex, primitive, and detail
    owners. Incidence reductions use stable source order and disjoint
    destination ranges, so results are byte-identical across domain counts.

    Float scalar/tuple storage supports every scalar reduction, component by
    component.
    Integer storage supports [First], [Last], [Minimum], [Maximum], [Mode], and
    [Median] without overflow-prone implicit coercion. Text/index storage
    follows Houdini's string policy: [Average] uses upper [Median], [Sum]
    concatenates incidence-order strings without separators, the other numeric
    reductions use [First], and explicit [First]/[Last]/[Mode]/[Median] retain
    their named behavior. Mode ties choose the smallest value and an even-sized
    median chooses the upper middle value.

    [Array_all] writes every scalar integer or float match in incidence order
    to packed CSR rows. [Unique_values] writes duplicate-free values in
    ascending [Int.compare]/[Float.compare] order; floating signed zeros and
    NaNs therefore use OCaml's total-comparison equivalence. Wider floating
    tuples, text, and existing arrays return a structured unsupported-storage
    error until RDK has tuple-width and text-array representations.

    [piece_attribute] names an integer or text partition attribute owned by
    [destination]. Destination elements with equal partition values receive
    one reduction over the union of their corresponding source elements.
    Sources are deduplicated and ordered by element index before reduction, so
    first/last and all tie behavior are deterministic. Same-owner promotion is
    useful with this option. Empty incidences produce zero (or empty text).
    Canonical point position [P] is intentionally not an ordinary attribute
    and must be changed by a geometry operator.

    [index_pattern] writes the contributing source element number as a
    destination-owned integer attribute for [First], [Last], [Minimum],
    [Maximum], or [Mode]. Ties select the first stable incidence; piece
    relations are deduplicated and source-index ordered. Empty incidences write
    [-1]. Scalar sources produce an ordinary integer attribute. Float2/3/4
    sources reduce independently per component and produce a fixed-width
    packed integer CSR row containing one source index per component; this
    preserves divergent extrema/modes without inventing an ambiguous scalar.

    Ordinary reductions are O(destination elements + traversed incidence *
    tuple width). Median uses expected linear-time introspective selection with
    an O(incidences * log(max incidence)) worst-case fallback. Mode uses
    allocation-free range sorting after one packed incidence materialization;
    large dense integer ranges instead use O(incidences + value range)
    counting. Piece promotion radix-orders encoded
    piece/source pairs in O(incidences * machine-word digits), then reduces
    unique sources. Auxiliary storage is O(points + incidences + destination
    elements), including primitive-to-point reverse incidence when needed.
    [Array_all] is O(incidences) time/storage. [Unique_values] is
    O(incidences * log(max incidence)) time and O(incidences + destinations)
    auxiliary storage, plus its exact packed result.

    Promote every source-owner attribute selected by a compiled
    {!Attribute_pattern} in stable source attribute order. Blank selects all;
    no match is an identity. The topology incidence and optional piece
    partition plan are built once and reused for every selected payload.
    [into_pattern] enables aligned multi-term glob-capture renaming following
    {!Attribute_pattern.compile_rewrite_set}: every positive source term has
    one replacement term, exclusions consume no replacement, and the last
    matching positive rule wins. Output names are preflighted and duplicate
    rewrites fail atomically. Without it, destination names equal source names.
    [index_pattern] applies the same aligned capture rules to create one
    distinct source-index attribute per selected value. Complexity is the
    shared plan cost plus the documented
    per-attribute cost of {!promote}. *)

type rename_conflict =
  | Attribute_rename_skip
  | Attribute_rename_error
  | Attribute_rename_overwrite

type rename_rule = {
  rename_attribute_owner : Attribute.owner option;
  rename_attribute_pattern : string;
  rename_attribute_replacement : string;
  rename_attribute_conflict : rename_conflict;
}

val delete :
  ?cancel:Cancel.t ->
  ?reference:Geometry.t ->
  ?delete_non_selected:bool ->
  ?point_pattern:string ->
  ?vertex_pattern:string ->
  ?primitive_pattern:string ->
  ?detail_pattern:string ->
  Geometry.t ->
  (Geometry.t, Error.t) result
(** Delete ordinary attribute metadata selected independently for each owner.
    Patterns use {!Attribute_pattern}; omitted and blank patterns select
    nothing. [delete_non_selected] reverses the final selection and therefore
    acts as a keep-pattern mode. When [reference] is present, its attribute
    names are implicit leading inclusions for their respective owners, so
    later exclusions such as [^name] can retain selected reference names.
    Canonical point position [P] is never ordinary metadata and is preserved.

    The operation is atomic, O(attributes + reference attributes) for a fixed
    number of pattern terms, uses O(attributes + reference attributes)
    auxiliary metadata, rebuilds the attribute table once, and structurally
    shares every surviving packed payload. A no-op preserves geometry identity.
    Cancellation never publishes a partial rewrite. *)

val rename :
  ?cancel:Cancel.t ->
  rules:rename_rule list ->
  Geometry.t ->
  (Geometry.t, Error.t) result
(** Apply ordered wildcard-capture rename rules to ordinary attributes.
    Owner-less rules cover point, vertex, primitive, and detail attributes;
    earlier outputs may match later rules. Destination conflicts can skip,
    fail atomically, or overwrite. Point name [P] remains reserved.

    For [A] attributes and [R] rules, time is O(A * R) pattern matching and
    auxiliary metadata is O(A). The geometry attribute table is rebuilt at
    most once and renamed attributes share their original packed payloads.
    Metadata-scale work deliberately stays sequential because domain dispatch
    costs more than name matching. *)

type swap_method =
  | Attribute_swap
  | Attribute_move
  | Attribute_copy

type swap_rule = {
  swap_attribute_owner : Attribute.owner;
  swap_attribute_source : string;
  swap_attribute_destination : string;
  swap_attribute_method : swap_method;
}

val swap :
  ?cancel:Cancel.t ->
  rules:swap_rule list ->
  Geometry.t ->
  (Geometry.t, Error.t) result
(** Apply ordered owner-specific copy, move, or swap pairs. Source and
    destination use matching full-name capture globs. Copy preserves the
    source; move removes it after replacing the destination; swap exchanges
    packed payloads. When one side of a swap is absent, the existing side is
    copied to it. Canonical point position [P] participates as packed float3;
    moving [P] is intentionally a copy, while moving another float3 into [P]
    removes the source.

    Rules are atomic and ordered, so later rules see earlier results. Payloads
    are structurally shared, metadata is rebuilt once, and a no-op preserves
    geometry identity. For [A] live attributes and [R] rules, expected time is
    O(A * R), auxiliary metadata is O(A + generated attributes), and payload
    storage is O(1). Metadata-scale work stays sequential. *)

type transfer_mode =
  | Nearest
  | Inverse_distance of { neighbors : int; power : float }
  | Kernel of {
      neighbors : int;
      radius : float;
      kernel : transfer_kernel;
    }

and transfer_kernel = Links | RenderMan | Hart
(** Spatial source-combination policy. [Kernel] uses the exact compact-support
    formulas published by SideFX for its corresponding metaball models,
    normalizes nonzero rows, and falls back to the stable nearest sample when
    [radius=0] or every candidate has zero support. [neighbors] is the maximum
    sample count. *)

type unmatched = Keep_target | Default_value
type transfer_falloff = Linear | Smoothstep | Uniform of float
(* Influence across a transfer blend band. [Uniform bias] applies one fixed
    influence in the band; the bias must be finite and in the closed unit
    interval. *)
type surface_falloff = transfer_falloff
type surface_vertex_selection = Surface_index.vertex_selection =
  | All_triangle_vertices
  | Any_triangle_vertex

type copy_match =
  | Cyclic
  | By_values of { source_attribute : string; target_attribute : string }
  | To_element of { target_attribute : string }

type copy_rule = {
  copy_owner : Attribute.owner;
  copy_pattern : string;
  copy_into : string option;
}

type interpolate_attribute = {
  interpolate_owner : Attribute.owner;
  interpolate_source : string;
  interpolate_target : string;
}

type interpolate_driver =
  | Primitive_uvw of {
      primitive_attribute : string;
      uvw_attribute : string;
    }
  | Point_weights of {
      numbers_attribute : string;
      weights_attribute : string;
    }
  | Vertex_weights of {
      numbers_attribute : string;
      weights_attribute : string;
    }
  | Primitive_weights of {
      numbers_attribute : string;
      weights_attribute : string;
    }
(** Typed destination-driver mode. Weighted modes consume matching CSR integer
    and floating array attributes without per-element list allocation. *)

type interpolate_computed = {
  computed_owner : Attribute.owner;
  computed_numbers_attribute : string;
  computed_weights_attribute : string;
}
(** Optional point- or vertex-number/weight arrays computed from primitive UVW
    coordinates. The resulting pair can drive a later weighted interpolation
    with equivalent coefficients. *)

type enumeration_storage = Integer | Text of { prefix : string }
type enumeration_mode = Enumerate_piece_elements | Enumerate_pieces
type blur_method = Uniform | Edge_length
type blur_mode = Laplacian of float | Custom_steps of { odd : float; even : float }
type numeric_value =
  | Scalar of float
  | Vec2 of Rays_math.Vec2.t
  | Vec3 of Rays_math.Vec3.t
  | Vec4 of float * float * float * float
type random_operation =
  | Random_set
  | Random_add
  | Random_minimum
  | Random_maximum
  | Random_multiply
type noise_kind = Noise_float | Noise_vector | Noise_quaternion
type noise_location =
  | Noise_position
  | Noise_element_number
  | Noise_attribute of string
type noise_range =
  | Noise_positive
  | Noise_zero_centered
  | Noise_min_max of numeric_value * numeric_value
type noise_operation =
  | Noise_set_initial
  | Noise_set
  | Noise_add
  | Noise_subtract
  | Noise_multiply
  | Noise_minimum
  | Noise_maximum
type random_selection =
  | Random_points of Group.t
  | Random_vertices of Group.t
  | Random_primitives of Group.t
  | Random_edges of Edge_group.t
type random_distribution =
  | Random_constant of numeric_value
  | Random_two_values of {
      a : numeric_value;
      b : numeric_value;
      probability_b : float;
    }
  | Random_uniform of { min : numeric_value; max : numeric_value }
  | Random_uniform_discrete of {
      min : numeric_value;
      max : numeric_value;
      step : numeric_value;
    }
  | Random_normal of { middle : numeric_value; scale : numeric_value }
  | Random_exponential of { median : numeric_value }
  | Random_log_normal of { median : numeric_value; stddev : numeric_value }
  | Random_cauchy of { median : numeric_value; scale : numeric_value }
  | Random_direction of {
      direction : numeric_value;
      cone_angle : float;
    }
  | Random_inside_sphere of { dimensions : int }
  | Random_inside_sphere_cone of {
      direction : numeric_value;
      cone_angle : float;
    }
  | Random_custom_ramp of {
      ramp : (float * float) list;
      fit_min : numeric_value;
      fit_max : numeric_value;
    }
  | Random_custom_discrete of (numeric_value * float) list
  | Random_custom_discrete_text of (string * float) list
(** [Random_direction] samples uniform unit vectors in two or three dimensions
    and unit orientation quaternions in four. [cone_angle] is radians; for a
    quaternion it denotes rotation angle, so its corresponding 4D spherical
    cap has half that angle. [Random_inside_sphere] is uniform by volume in
    two through four dimensions; [Random_inside_sphere_cone] restricts that
    volume around a non-zero direction. [direction_bias] may be greater than
    -1 for either directional mode: zero is uniform, positive values favor the
    axis, and negative values favor the cone boundary. [Random_custom_ramp]
    interprets its strictly
    ordered endpoint-complete ramp as an inverse CDF before fitting each
    component. [Random_custom_discrete] selects a complete scalar/tuple value
    from finite non-negative unnormalized weights;
    [Random_custom_discrete_text] applies the same stable weighted selection to
    immutable strings. Multidimensional [Random_cauchy] is the rotationally
    symmetric multivariate distribution and requires equal scale components. *)
type remap_input =
  | Remap_explicit of { min : numeric_value; max : numeric_value }
  | Remap_auto
type remap_policy = Remap_clamp | Remap_cycle | Remap_extrapolate

val enumerate :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?selection:Group.t ->
  ?start:int ->
  ?step:int ->
  ?storage:enumeration_storage ->
  ?piece_attribute:string ->
  ?mode:enumeration_mode ->
  owner:Attribute.owner ->
  name:string ->
  Geometry.t ->
  (Geometry.t, Error.t) result
(** Assign stable sequential values to selected point, vertex, or primitive
    elements in increasing element order. Unselected existing same-kind values
    are preserved; a newly created attribute uses zero/empty defaults outside
    the selection. Integer output is allocation-tight. Text output necessarily
    allocates one formatted string per selected element.

    A same-owner integer or text [piece_attribute] enables SideFX-compatible
    piece modes. [Enumerate_piece_elements] restarts the sequence at zero for
    each distinct piece; [Enumerate_pieces] assigns one dense number to each
    piece. Piece identity follows first selected element occurrence, and local
    order follows increasing element number.

    Work is expected O(elements) with O(chunks + output + pieces) storage.
    Restricted global selection uses a parallel count, stable sequential
    chunk-prefix scan, and parallel disjoint fill. Piece lookup/rank planning
    is stable sequential work with a specialized integer table or string table;
    output materialization remains parallel. Results are exact for every
    domain count. *)

val blur_points :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?selection:Group.t ->
  ?iterations:int ->
  ?method_:blur_method ->
  ?mode:blur_mode ->
  ?weight_attribute:string ->
  ?alpha_attribute:string ->
  ?pin_borders:bool ->
  ?original_blend:float ->
  ?blurred_blend:float ->
  pattern:string ->
  Geometry.t ->
  (Geometry.t, Error.t) result
(** Blur canonical [P] and matching point-owned floating scalar/tuple
    attributes over topology connectivity. Blank [pattern] is an identity.
    [Uniform] weights every unique neighboring point equally; [Edge_length]
    uses inverse original edge length, with coincident neighbors taking
    exclusive precedence. [Laplacian] uses one step size and [Custom_steps]
    alternates odd/even sizes. Optional scalar point weight and alpha
    attributes control receiver displacement and neighbor influence;
    [pin_borders] fixes points incident to one-sided polygon/curve edges.
    Final output is [original_blend * original + blurred_blend * blurred].

    The cached {!Topology_index} is built once. Work is O(iterations *
    components * (points + edges)); auxiliary/output storage is O(edges +
    components * points). Each point sums neighbors in stable edge order and
    writes one disjoint slot, so one- and multi-domain results are exact. *)

val interpolate :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?selection:Group.t ->
  ?driver:interpolate_driver ->
  ?compute_weights:interpolate_computed ->
  ?point_pattern:string ->
  ?vertex_pattern:string ->
  ?primitive_pattern:string ->
  ?detail_pattern:string ->
  ?match_groups:bool ->
  ?pre_scale:float ->
  ?normalize_weights:bool ->
  ?threshold:float ->
  ?blend:float ->
  ?unmatched:unmatched ->
  target_owner:Attribute.owner ->
  attributes:interpolate_attribute list ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result
(** Interpolate source attributes onto destination elements. The default
    driver uses destination integer [source_primitive] and float3 [source_uvw]
    attributes; [driver] also accepts corresponding CSR number/weight rows for
    source points, vertices, or primitives. [driver] is mutually exclusive
    with the two legacy driver-name arguments. Polygons use Houdini's
    triangle, bilinear-quad, or polygon-fan parameterization; open and closed
    curves interpolate over their ordered segments. Numeric point/vertex
    fields are weighted, primitive/detail fields are constant, and integer or
    text fields choose the greatest-weight corner with stable local ties.
    Integer-array and float-array fields are opaque variable-length values:
    the whole CSR row at that same greatest coefficient is copied rather than
    performing element-wise arithmetic across unequal row lengths. Destination
    blend selects the new row inclusively at one half; keep/default misses
    preserve/clear the row.
    Point-weight mode accepts point/detail sources, vertex-weight mode accepts
    every owner, and primitive-weight mode accepts primitive/detail sources.
    Weighted row layouts must match exactly; source numbers and finite weights
    are validated before parallel writes. [normalize_weights] divides a row
    whose signed sum has magnitude greater than [1e-20] by that signed sum;
    negative totals therefore normalize to positive unit total while retaining
    each coefficient's algebraic contribution. A zero-sum row has zero new
    influence and preserves existing numeric, discrete, and group values. The
    absolute pre-normalization sum ramps existing/new influence linearly to
    full at [threshold]. In primitive mode, [compute_weights] atomically emits
    equivalent point- or vertex-number and weight CSR rows while preserving
    paired existing rows outside the selection.

    The four optional owner patterns expand stable source metadata into
    same-name outputs after the explicit [attributes] list; point patterns may
    include canonical [P]. With [match_groups], the point, vertex, and
    primitive patterns also select same-name source groups. Group membership
    is interpolated as a weighted 0/1 field, blended with existing destination
    membership, and thresholded inclusively at one half after destination
    blending. Duplicate destinations are
    rejected atomically.
    Standard float3 [N] is normalized before blending. Invalid primitive
    numbers follow [unmatched]; [selection] restricts destination writes.

    For [d] selected destinations, [a] attributes, [c] source corners per
    referenced primitive, and [r] copied array values, work is O(d*c*a + r),
    output storage is O(d*a + r), and auxiliary metadata is O(d) per array
    field plus O(a). All attributes share one topology traversal;
    destination payload planes are disjoint and byte-identical across domain
    counts. Output positions and ordinary metadata commit atomically. *)

val transfer_points :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?names:string list ->
  ?pattern:string ->
  ?mode:transfer_mode ->
  ?max_distance:float ->
  ?blend_width:float ->
  ?falloff:transfer_falloff ->
  ?unmatched:unmatched ->
  ?source_points:Group.t ->
  ?target_points:Group.t ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result
(** Transfer point attributes from a spatial source onto target points.
    [Nearest] copies from the closest source point. [Inverse_distance] and
    [Kernel] blend numeric scalar/tuple values from up to [neighbors] nearest
    points; integer and text values use the closest point. Kernel [radius]
    controls compact support independently of [max_distance]. Exact-position
    inverse-distance matches always copy the lowest-index exact source and
    cannot divide by zero; kernel modes blend equal-position samples and use a
    stable nearest fallback when their total support is zero.

    [names] selects exact names; [pattern] selects Houdini-style globs and is
    mutually exclusive with [names]. Both absent defaults to every point
    attribute in source order. Canonical [P] is excluded. [source_points] and
    [target_points] restrict indexed source
    points and written target points through matching packed groups. When no
    source lies within [max_distance], [Keep_target]
    retains an existing same-kind target value and otherwise uses the storage
    default; [Default_value] always uses the storage default.

    [max_distance] is the full-influence threshold. A positive [blend_width]
    extends the search radius and blends numeric values through [falloff];
    discrete values switch at half influence. [Uniform bias] applies its fixed
    weight only inside the blend band. Zero blend width retains the direct-copy
    hot path and allocates no influence plane.

    Equal-distance ties use source index, and each target is evaluated in a
    disjoint output slot, making results byte-identical across domain counts.
    Index construction is expected O(s log s); querying is expected
    O(t log s + t * k), with O(s + t * k + output) auxiliary storage where
    [k] is one for [Nearest] or [neighbors] for weighted transfer. Weighted
    modes allocate one normalized O(t*k) coefficient plane shared by every
    numeric payload component. Degenerate
    search distributions can visit all source points per target. *)

val transfer_primitives :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?names:string list ->
  ?pattern:string ->
  ?mode:transfer_mode ->
  ?max_distance:float ->
  ?blend_width:float ->
  ?falloff:transfer_falloff ->
  ?unmatched:unmatched ->
  ?source_primitives:Group.t ->
  ?target_primitives:Group.t ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result
(** Transfer primitive attributes by arithmetic primitive-barycenter
    proximity. Numeric and discrete storage, groups, miss policy, stable ties,
    determinism, and inverse-distance behavior match {!transfer_points}.
    Barycenter preparation is O(total source and target corners); index/query
    complexity and storage are otherwise the same with primitive counts in
    place of point counts. *)

val transfer_detail :
  ?names:string list ->
  ?pattern:string ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result
(** Structurally share selected source detail attributes onto the target.
    [names] selects exact names; [pattern] selects {!Attribute_pattern} globs
    and is mutually exclusive with [names]. Both absent defaults to every
    detail attribute in source order. Existing target attributes must have
    matching storage kinds. Work is expected O(source + target attributes),
    payload storage is shared without copying, and all selected metadata is
    committed in one immutable table rebuild. *)

type surface_attribute = {
  source_owner : Attribute.owner;
  source_name : string;
  target_name : string;
}

val transfer_surface :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?max_distance:float ->
  ?blend_width:float ->
  ?falloff:surface_falloff ->
  ?unmatched:unmatched ->
  ?target_owner:Attribute.owner ->
  ?distance_attribute:string ->
  ?source_primitives:Group.t ->
  ?source_vertices:Group.t ->
  ?source_vertex_selection:surface_vertex_selection ->
  ?target_elements:Group.t ->
  attributes:surface_attribute list ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result
(** Transfer selected source point, vertex, and primitive attributes to target
    point, vertex, or primitive elements at the closest position on a polygon
    surface. Target vertices query their referenced point positions; target
    primitives query their arithmetic point barycenters. Numeric point/vertex
    storage uses exact barycentric weights; primitive storage is constant over
    its triangle. Integer/text storage selects the largest barycentric corner
    with stable local-corner ties. Standard float3 [N] is normalized after
    interpolation. [distance_attribute] optionally writes closest Euclidean
    distance with [target_owner]. [source_primitives] restricts complete source
    polygons. [source_vertices] further retains triangulated regions with all
    selected source corners by default, or any selected corner with
    [Any_triangle_vertex]. Both groups may be combined. [target_elements] must
    match the destination owner and restricts writes.

    [max_distance] is a full-influence threshold. A positive [blend_width]
    extends the query radius and blends numeric values back to their existing
    target/default values using [Linear], [Smoothstep], or fixed
    [Uniform bias] falloff. Discrete integer/text values switch at half
    influence. A blend width requires a finite [max_distance].
    Zero blend width uses direct writes and allocates no influence plane.

    Source geometry must contain finite, non-degenerate simple polygons.
    [unmatched] and [max_distance] follow {!transfer_points}. Equal-distance
    triangles select the lower source primitive and then stable internal
    triangle. Ear clipping is O(sum(c²)) for polygon corner counts [c]; BVH
    construction is O(s log s) time/O(s) storage over emitted triangles.
    Expected query time is O(t log s), with O(t + output) query-plan storage.
    Payload planes fill stable disjoint ranges in parallel. *)

val transfer_vertices :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?names:string list ->
  ?pattern:string ->
  ?max_distance:float ->
  ?blend_width:float ->
  ?falloff:transfer_falloff ->
  ?unmatched:unmatched ->
  ?source_primitives:Group.t ->
  ?source_vertices:Group.t ->
  ?source_vertex_selection:surface_vertex_selection ->
  ?target_vertices:Group.t ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result
(** Transfer source vertex attributes onto target vertices by closest-polygon
    lookup and barycentric interpolation. Source primitive and partial vertex
    surface restrictions compose as described by {!transfer_surface};
    destination restriction is vertex owned. Complexity and deterministic tie
    behavior follow {!transfer_surface}. *)

val transfer_all :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?point_pattern:string ->
  ?vertex_pattern:string ->
  ?primitive_pattern:string ->
  ?detail_pattern:string ->
  ?mode:transfer_mode ->
  ?max_distance:float ->
  ?blend_width:float ->
  ?falloff:transfer_falloff ->
  ?unmatched:unmatched ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, Error.t) result
(** Transfer any requested point, primitive, vertex, and detail attribute tabs
    in one operation. An omitted owner pattern skips that owner. Each spatial
    owner constructs exactly one shared query plan for all attributes matching
    its pattern; owner kernels run in stable point, primitive, vertex order and
    use parallel disjoint fills internally, avoiding nested-pool
    oversubscription. Detail payloads are structurally shared last. Output is
    byte-identical to the corresponding sequence of owner-specific calls. *)

type image_channel = Red | Green | Blue | Alpha | Luminance
