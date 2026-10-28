(** Concatenate geometries with matching attribute schemas. Group schemas are
    unioned in first-seen order by default; a missing group contributes no
    members. Pass [~pad_groups:false] to require exact group schemas. Detail
    attributes still require an explicit merge policy.

    [~source_attribute:name] also writes a primitive int attribute [name]
    holding, per primitive, [source_base] (default 0) plus the index of the
    input it came from.  An input that already has [name] keeps its values, so
    a merge of merges keeps the innermost tag (give every merge its own
    [source_base] range and a tag names one input of one merge). *)
type run_params = {
  cancel : Cancel.t option;
  grain : int option;
  source_attribute : string option;
  source_base : int option;
}

val run : run_params -> Geometry.t list -> (Geometry.t, Error.t) result

val provenance_attribute : string
(** ["__flow_src"]: the primitive int attribute the editor's merge lowering writes. *)

type run_tagged_params = {
  cancel : Cancel.t option;
  grain : int option;
  tag_base : int;
  source_attribute : string option;
  source_base : int;
}

val run_tagged : run_tagged_params -> Geometry.t list -> (Geometry.t, Error.t) result
(** [run] plus the provenance attribute {!provenance_attribute} numbered from [tag_base] (negative:
    none) beside an authored [source_attribute]/[source_base]. Without an authored attribute it is
    [run ~source_attribute:provenance_attribute ~source_base:tag_base]. *)
