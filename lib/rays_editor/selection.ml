(* The rows selected in the list of the open level (scene objects, World layers, the nodes of
   an object's network): view state, never in history.  The first id is the one the inspector
   and the handles follow. *)
type t = { ids : int list; primary : int option }

let empty = { ids = []; primary = None }
let selected t = t.primary
let selected_nodes t = t.ids
let select id = { ids = [ id ]; primary = Some id }

let select_nodes ids =
  match ids with
  | [] -> empty
  | first :: _ -> { ids = List.sort_uniq Int.compare ids; primary = Some first }

let clear = empty
