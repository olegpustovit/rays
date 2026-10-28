(* Private editor environment interface. *)
open Loop_model

val open_import :
  'a t -> ('b, 'c) Core_model.update -> 'a t * ('b, 'c) Core_model.update
val open_dialog :
  'a t ->
  ('b, 'c) Core_model.update ->
  Input.event list -> 'a t * ('b, 'c) Core_model.update
val reload_source :
  'a t ->
  ('b, 'c) Core_model.update ->
  now:float -> 'a t * ('b, 'c) Core_model.update
val save_source :
  'a Core_model.t ->
  Source_file.t option ->
  Flow.Syntax.t -> 'a Core_model.t * Source_file.t option
val autosave : ?force:bool -> 'a t -> now:float -> 'a t
