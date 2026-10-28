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
  (Geometry.t, string) result

val rename :
  ?cancel:Cancel.t ->
  rules:rename_rule list ->
  Geometry.t ->
  (Geometry.t, string) result

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
  (Geometry.t, string) result
