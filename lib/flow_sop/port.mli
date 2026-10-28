type t = { node : int; path : string }

val compare : t -> t -> int

module Map : Map.S with type key = t

type literal = Scalar of Param.value | Vector of float * float * float

type parameter = private {
  path : string;
  fields : Param.field_view list;
  ty : Flow.Port_type.t option;
}

val parameters : ?infer:bool -> Param.field_view list -> (parameter list, Diagnostic.t) result
(** Group vec3 metadata, checking the same grouping invariants as the PPX. [infer=true] uses native
    scene-schema suffix inference from [Param.group_vec3]. Underlying component field names are not
    public port paths. *)

val find_parameter : parameter list -> string -> (parameter, Diagnostic.t) result
val literal : parameter -> literal
val literal_changes : parameter -> literal -> ((string * Param.value) list, Diagnostic.t) result

val normalize :
  parameter ->
  Flow.Port_type.value ->
  (Flow.Port_type.value * (string * Param.value) list, Diagnostic.t) result
(** Coerce and use [Param.normalize_value] for hard bounds, matching writes through [Param.apply].
    The returned value is the effective live readout. *)

val color_of_text : string -> (float * float * float) option
(** ["#rgb"], ["#rrggbb"] or ["#rrggbbaa"] (alpha ignored) as a vec3. *)
