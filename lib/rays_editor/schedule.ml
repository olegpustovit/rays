open Rays
open Sop

(* Effect- and dependency-aware cook scheduler. It fires initially, after a
   committed cook parameter change, after [force], and whenever the sketch
   clock changed and any reachable node declares [Time] or [Frame]. While a
   primary-pointer edit is held it cooks only when [live] and the worker is
   idle; otherwise only the latest desired cook is retained until release. *)
type t = {
  initialized : bool;
  dirty : bool;
  graphs : Node.t list;
  dependencies : Context.Dependencies.t;
  input : Input.t option; (* the host input the last step saw *)
}

let initial =
  {
    initialized = false;
    dirty = false;
    graphs = [];
    dependencies = Context.Dependencies.static;
    input = None;
  }

let step ?(live = false) ?input value ~graphs ~effects ~context_changed ~force ~busy ~frame =
  let dirty = value.dirty || effects.Param.cook || force in
  let dependencies =
    if List.equal ( == ) graphs value.graphs then value.dependencies
    else
      List.fold_left
        (fun union graph -> Context.Dependencies.union union (Node.graph_dependencies graph))
        Context.Dependencies.static graphs
  in
  let dynamic =
    (* a graph that reads the host input recooks when that input differs: a stopped clock and a
       still pointer leave every idle frame the same *)
    Context.Dependencies.mem Context.Dependencies.Input dependencies
    && not
         (match (input, value.input) with
         | Some now, Some before -> Input.equal now before
         | _ -> false)
    || context_changed
       && (Context.Dependencies.mem Context.Dependencies.Time dependencies
          || Context.Dependencies.mem Context.Dependencies.Frame dependencies)
  in
  let held = Frame.mouse_down Input.LeftButton frame in
  let desired = (not value.initialized) || dirty || dynamic in
  let urgent = (not value.initialized) || effects.Param.cook || force in
  (* Live: while a drag holds the pointer, cook whenever the worker is idle,
     so each finished cook shows and the next picks up the latest value. *)
  let fire = if held then live && desired && not busy else desired && ((not busy) || urgent) in
  ( {
      initialized = value.initialized || fire;
      dirty = desired && not fire;
      graphs;
      dependencies;
      input;
    },
    fire )
