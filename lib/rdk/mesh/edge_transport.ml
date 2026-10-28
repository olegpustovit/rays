
type roots = Transport_first_point | Transport_last_point
  | Transport_root_group of Group.t
type operation = Transport | Transport_from_root | Transport_total
  | Transport_maximum | Transport_minimum
type root_value = Transport_root_zero | Transport_root_hold
type split = Transport_copy | Transport_split
type normalization = Transport_no_normalization
  | Transport_normalize_components | Transport_normalize_global
type direction = Transport_forward | Transport_backward
type merge = Transport_merge_add | Transport_merge_maximum
  | Transport_merge_minimum

exception Transport_error of string

