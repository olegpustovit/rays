type hit = { t : float; prim : int; u : float; v : float; nx : float; ny : float; nz : float; hit : bool }
let miss = { t = 0.; prim = -1; u = 0.; v = 0.; nx = 0.; ny = 0.; nz = 0.; hit = false }
(* ponytail: one process-wide id registry; replace with a typed scene value carried by the evaluation
   when two hosts in one process need overlapping ids. *)
module M = Map.Make (Int)
let scenes : (float array -> hit) M.t Atomic.t = Atomic.make M.empty
let rec update f = let old = Atomic.get scenes in if not (Atomic.compare_and_set scenes old (f old)) then update f
let register id closure = update (M.add id closure)
let unregister id = update (M.remove id)
let query id args = match M.find_opt id (Atomic.get scenes) with
  | Some closure -> closure args
  | None -> Value.fail "E_TRACE" (Printf.sprintf "scene/trace: no scene is registered with id %d." id)
