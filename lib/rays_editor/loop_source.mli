(* Private editor environment interface. *)
open Loop_model

val open_import : t -> 'c Editor_model.update -> t * 'c Editor_model.update
val open_dialog : t -> 'c Editor_model.update -> Input.event list -> t * 'c Editor_model.update
val reload_source : t -> 'c Editor_model.update -> now:float -> t * 'c Editor_model.update

val save_source :
  Editor_model.t -> Source_file.t option -> Flow.Syntax.t -> Editor_model.t * Source_file.t option

val autosave : ?force:bool -> t -> now:float -> t
