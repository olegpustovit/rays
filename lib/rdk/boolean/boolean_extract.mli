(** View typed expression extraction from classified Weiler cells. *)

type expression =
  | Left
  | Right
  | Not of expression
  | And of expression * expression
  | Or of expression * expression
  | Xor of expression * expression

val union : expression
val intersection : expression
val difference : expression
val reverse_difference : expression
val xor : expression

type ancestry

val geometry : ancestry -> Geometry.t
val primitive_side : ancestry -> int -> Boolean_constraints.side
val primitive_face : ancestry -> int -> int
val primitive_refined_triangle : ancestry -> int -> int
val primitive_winding : ancestry -> int -> int

val piece_ids :
  weiler:Boolean_weiler.t ->
  ancestry:ancestry ->
  source:(int -> int) ->
  primitive_count:int ->
  int array
(** Dense stable component IDs for materialized output primitives. *)

val build_with_ancestry :
  ?cancel:Cancel.t ->
  ?require_closed:bool ->
  ?defer_rounded_slivers:bool ->
  ?corner_payload:bool ->
  expression:expression ->
  Boolean_complex.t ->
  Boolean_weiler.t ->
  Boolean_cells.t ->
  (ancestry, Error.t) result

module View : sig
  type barycentric_cache

  type ancestry_view = {
    corner_complex_vertices : int array;
    primitive_sides : bytes;
    primitive_triangles : int array;
    primitive_source_points : int array;
    primitive_source_vertices : int array;
    barycentric_a : float array;
    barycentric_b : float array;
    barycentric_c : float array;
    edge_source_offsets : int array;
    edge_source_sides : bytes;
    edge_source_edges : int array;
  }

  val ancestry_view : ancestry -> ancestry_view
  val complex : ancestry -> Boolean_complex.t

  (* Output point for a selected exact-complex vertex, or [-1]. Distinct exact
     vertices that round to bit-identical binary64 positions may map to the
     same output point after certified coalescing. *)
  val complex_output_point : ancestry -> int -> int
  val primitive_complex_facet : ancestry -> int -> int
  val left_geometry : ancestry -> Geometry.t
  val right_geometry : ancestry -> Geometry.t
  val validate_positions : x:float array -> y:float array -> z:float array -> (unit, Error.t) result

  val validate_materialized :
    ?allow_degenerate:bool ->
    x:float array ->
    y:float array ->
    z:float array ->
    vertex_points:int array ->
    unit ->
    (unit, Error.t) result

  val coalesce_positions :
    complex_vertices:int array ->
    x:float array ->
    y:float array ->
    z:float array ->
    float array * float array * float array * int array * int array

  val validate_closed_topology : Topology.t -> (unit, Error.t) result
  val barycentric_cache : capacity:int -> barycentric_cache

  (* Materialize the exact facets selected by packed orientation bytes:
      zero omits, one keeps canonical orientation, and two reverses it. The
      requested side supplies deterministic payload ancestry. *)
  val build_selected_with_ancestry :
    ?cancel:Cancel.t ->
    ?defer_rounded_slivers:bool ->
    ?barycentric_cache:barycentric_cache ->
    ?corner_payload:bool ->
    selection:bytes ->
    side:Boolean_constraints.side ->
    Boolean_complex.t ->
    Boolean_weiler.t ->
    Boolean_cells.t ->
    (ancestry, Error.t) result

  val reverse_ancestry : ?cancel:Cancel.t -> ancestry -> (ancestry, Error.t) result

  (* Concatenate products from one exact complex while sharing equal exact
      complex vertices and preserving every packed ancestry plane. *)
  val concatenate_ancestries : ?cancel:Cancel.t -> ancestry array -> (ancestry, Error.t) result
end
