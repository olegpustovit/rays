(** Shared allocation-free triangle/triangle narrow phase.

    The module is private to RDK. It is the single floating-point decision and
    event-generation boundary used by Boolean diagnostics and intersection
    point construction. *)

type surface

val surface : Surface_index.t -> Geometry.t -> surface

val max_events : int
val event_stride : int

val events_into :
  scratch:float array ->
  point_info:int array ->
  events:float array ->
  self:bool ->
  tolerance:float ->
  include_coplanar:bool ->
  surface -> int -> surface -> int -> int
(** Write at most [max_events] unique pair-local events to [events] and return
    the count. Each event stores world position, source barycentric weights,
    and collision barycentric weights in [event_stride] consecutive floats.
    Ordinary shared-point/shared-edge contact is suppressed in self mode;
    duplicate triangles and overlap beyond a shared edge remain visible. The
    three supplied scratch arrays are worker-local and must have lengths at
    least 26, 10, and [max_events * event_stride]. *)

val events_points_into :
  scratch:float array ->
  point_info:int array ->
  events:float array ->
  self:bool ->
  tolerance:float ->
  include_coplanar:bool ->
  left_positions:Packed.Float3.View.view ->
  left_a:int -> left_b:int -> left_c:int ->
  right_positions:Packed.Float3.View.view ->
  right_a:int -> right_b:int -> right_c:int ->
  int
(** The same decision/event kernel over explicit packed point indices. This is
    used by mixed triangle/curve indexes so they do not need to construct a
    redundant triangle-only BVH. *)
