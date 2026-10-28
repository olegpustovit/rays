val apply :
  ?cancel:Cancel.t ->
  grain:int ->
  attribute_rules:Fuse_rules.attribute_rule list ->
  group_rules:Fuse_rules.group_rule list ->
  destinations:int array ->
  source:Geometry.t ->
  target:Geometry.t ->
  unit ->
  (Geometry.t, string) result
(** Copy rule-selected target point payload to mapped query points. Each query
    has one stable target, so every supported heuristic reduces to that target
    value; concatenate policies retain their required scalar-to-array shape. *)
