(** The graph pane of a workspace document and its node menu. The pane presents one
    {!Flow_graph.Projection.scope}; it returns typed requests and never edits. *)

module Scope = Scope_pane
(** The workspace pane: zones, rails, iteration selectors, chips and typed sockets. *)

module Node_menu = Node_menu
(** The categorised menu of the kinds a graph can add. *)
