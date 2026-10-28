type 'a t = { in_use : string list; at : int list option; panes : (string list * 'a) list }
(* [in_use] is always a key of [panes]; [[]] is the pane of no panel *)

let make pane = { in_use = []; at = None; panes = [ [], pane ] }
let active t = List.assoc t.in_use t.panes
let key t = if t.in_use = [] then None else Some t.in_use
let leaf t = t.at
let set_leaf at t = { t with at }
let set k pane t = { t with panes = (k, pane) :: List.remove_assoc k t.panes }
let update f t = set t.in_use (f (active t)) t
let others t = List.remove_assoc t.in_use t.panes
let activate k ~create t =
  let t = if List.mem_assoc k t.panes then t else set k (create ()) t in
  { t with in_use = k }
let adopt k t = { in_use = k; at = t.at; panes = (k, active t) :: List.remove_assoc t.in_use t.panes }
let with_others panes t =
  { t with panes = (t.in_use, active t) :: List.filter (fun (k, _) -> k <> t.in_use) panes }
let retain keep t = { t with panes = List.filter (fun (k, _) -> k = t.in_use || keep k) t.panes }
