(** Allocation-free polygon-curve narrow phase used by Intersection Analysis. *)

val segment_segment_events_into :
  scratch:float array ->
  events:float array ->
  self:bool ->
  tolerance:float ->
  left_positions:Packed.Float3.View.view ->
  left_a:int ->
  left_b:int ->
  left_u0:float ->
  left_u1:float ->
  right_positions:Packed.Float3.View.view ->
  right_a:int ->
  right_b:int ->
  right_u0:float ->
  right_u1:float ->
  int

val segment_triangle_events_into :
  scratch:float array ->
  events:float array ->
  self:bool ->
  tolerance:float ->
  include_coplanar:bool ->
  segment_first:bool ->
  segment_positions:Packed.Float3.View.view ->
  segment_a:int ->
  segment_b:int ->
  segment_u0:float ->
  segment_u1:float ->
  triangle_positions:Packed.Float3.View.view ->
  triangle_a:int ->
  triangle_b:int ->
  triangle_c:int ->
  int
(** Event records use [Triangle_intersection.event_stride]: world position, then left and right
    primitive UVW triplets. A curve UVW is [(u,0,0)] and a triangle UVW is barycentric. *)
