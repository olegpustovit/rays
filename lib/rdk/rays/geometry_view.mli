(** What an editor reads of a cooked geometry: summaries, bounds, the sheet's columns, a wire mesh,
    picking and the probed-iteration tint.  Pure over the geometry; no UI. *)

type t = Rdk_core.Geometry.t
type surface = Rdk_spatial.Surface_index.t

val error_string : Rdk_core.Error.t -> string

val points_of_nothing : t
(** A geometry of no points. *)

val point_count : t -> int
val bounds : t -> (Rays_math.Vec3.t * Rays_math.Vec3.t) option

type summary = { points : int; prims : int; groups : string list; attributes : (string * string * string * int) list }

val summary : t -> summary
(** Point and primitive counts, group names and (owner, name, kind, length) of each attribute. *)

val triangles : t -> int
(** The triangles a scene draws: a polygon of n corners fans into n - 2 (a count, not a walk). *)

val wire_mesh : t -> (Rays.Mesh.t, string) result
(** Every polygon edge once, as a line mesh. *)

val count : int -> t -> int
(** Rows of the sheet for an owner: 0 point, 1 vertex, 2 primitive, 3 detail. *)

val columns : int -> t -> (string * (int -> string)) array
(** The sheet's columns for an owner (see {!count}): a header and the cell of a row. *)

val tags : attribute:string -> t -> int array option
(** The primitive-owned integer provenance tags named [attribute]. *)

val surface : t -> surface option
(** A ray index of the geometry; [None] when it cannot be picked (degenerate polygons). *)

val cast : attribute:string -> surface option -> t -> origin:Rays_math.Vec3.t -> direction:Rays_math.Vec3.t ->
  (float * int) option
(** The nearest hit as (distance, tag); a geometry without tags never picks. *)

val cast_material : surface option -> t -> origin:Rays_math.Vec3.t -> direction:Rays_math.Vec3.t ->
  (float * string option) option
(** The nearest hit as (distance, [shop_materialpath] of that primitive). *)

val tint : attribute:string -> accent:float * float * float -> lit:(int -> bool) -> t -> t
(** The primitives whose tag satisfies [lit] take the accent over their colour, the rest dim. *)
