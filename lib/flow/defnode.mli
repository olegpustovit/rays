type validation = { predicate : Syntax.t; code : string; message : string }
(** Geometry-independent declarations of Lisp-backed catalog kinds. *)

type t = {
  kind : Catalog.kind;
  slots : Catalog.slot list;
  keyword_inputs : string list;
  fields : Param.field_view list;
  label : string;
  operation : string;
  version : int;  (** Nonnegative runtime version, default 1. *)
  category : string list;
  description : string option;
  inputs : (string * Ty.t * Syntax.t option) list;
  body : Syntax.t;
  validations : validation list;
  normalizers : (string * Syntax.t) list;
  form : Syntax.t;
}

val valid_name : string -> bool
val parse : Syntax.t -> (t, Diagnostic.t) result
