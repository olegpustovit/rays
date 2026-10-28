(* Private graph-pane interface; only the pane's shared operations. *)
open Scope_state

val parents_removed : path list -> path list
val wired : P.row -> bool
val unwire : t -> P.node -> P.row -> E.op list
val unwire_at : t -> (path * E.arg_key) list -> E.op list
val scope_of_path : 'a list -> 'a list
val frame_list : t -> path -> fr list
val input_of : t -> path -> P.input option
val conditional_target : P.node -> path * int
val action_changes : t -> command -> change list
val step_hints : t -> pressed:Input.key list -> clicked:bool -> t * change list
val run_command : t -> command -> t * change list
val bindings : ('a, command) Editor_core.Command.t list
