(** Canvas layout keyed by {!type:Flow.Workspace.path}: the fields of the workspace's active layout.
    Scope consumes positions, detail levels with their pins, collapsed zones and frames. A path is
    the lexical identity of a binding, so a key survives every text edit that keeps the path;
    {!Flow_graph.Flow_edit.remap} rewrites keys in the same transaction as a rename or a hoist.

    Persisted as one [(layout ...)] s-expression after the workspace:
    {[
      (layout
        (editor "studio")
        (panel ["studio" "network"] :collapsed false :window [500 80 620 450])
        (node ["g" "ring"] :at [120 48] :level "full" :pinned true :collapsed true :rows {:radius false})
        (frame ["g"] "Legs" :at [0 0] :size [200 100]))
    ]} *)

type path = Flow.Workspace.path

module Path_map : Map.S with type key = path
module String_map : Map.S with type key = string

type frame = { title : string; at : float * float; size : float * float }
(** A titled rectangle on the canvas of the scope the frame is keyed by. *)

type t = {
  editor : string option;
      (** The selected editor graph (a named shell layout); first by default. *)
  panels : Editor_core.Panels.state Path_map.t;
      (** Saved panel disclosure and floating window bounds, keyed by editor graph and binding. *)
  at : (float * float) Path_map.t;
  pinned : bool Path_map.t;  (** opened explicitly: the node ignores the zoom caps *)
  level : Flow_graph.Projection.level Path_map.t;
      (** the detail level of a node (absent: a card), by path like [at] *)
  rows : bool String_map.t Path_map.t;
  collapsed : bool Path_map.t;
  frames : frame list Path_map.t;
}

val empty : t
val is_empty : t -> bool

val remap : (path -> path option) -> t -> t
(** Rewrite every key; [None] drops it. *)

val to_syntax : t -> Flow.Syntax.t
(** The [(layout ...)] form. *)

val of_syntax : Flow.Syntax.t -> (t, string) result
