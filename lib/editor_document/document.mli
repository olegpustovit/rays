(** The editor document: the scene, the SOP networks, the shell tree and the workspace they are
    lowered from.  One immutable value is what {!Editor_core.History} snapshots. *)

module Int_map : Map.S with type key = int

type network = {
  context : Flow.Context.t;
  graph : Flow_sop.Network.t;
  displayed : int option;
}
val of_geometry :
  context:Flow.Context.t -> Sop.Edit_graph.t -> int option -> network
type home =
    Bound_at of Flow.Workspace.path
  | Inline_in of home * Flow_graph.Flow_edit.arg_key
  | Copy of { loop : home; rel : string list; index : int; }
  | Looped
type homes = {
  objects : (int * home) list;
  world : home option;
  layers : (int * home) list;
  settings : home option;
  root : home option;
  world_graph : string option;
}
val no_homes : homes
type origin =
    Bound of string
  | Loop of home * Flow_graph.Flow_edit.arg_key
  | Inline of home * Flow_graph.Flow_edit.arg_key
type preview_source = {
  editor_graph : string;
  panel : origin option;
  scene_ref : Flow.Syntax.t option;
  instance : Flow.Eval.value;
}
type view_world = { node : Kind_graph.node; layers : Kind_graph.t; }
type view_root = {
  params : Objects.Root.parameters;
  camera : Rays.Camera.t option;
}
type switch = { layouts : Editor_core.Panels.t list; active : int; }
type start = {
  focus : Editor_core.Panels.path option;
  looking : string list;
  graph_views : (Editor_core.Panels.path * string) list;
  tabs : (Editor_core.Panels.path * string) list;
}
type shell = {
  tree : Editor_core.Panels.t;
  origins : (Editor_core.Panels.path * origin) list;
  named : (Editor_core.Panels.path * string) list;
  repeated : string list;
  follows : (Editor_core.Panels.path * Editor_core.Panels.path) list;
  start : start;
  views : (string * int list) list;
  canvases : (string * Flow.Eval.value) list;
  preview_sources : (string * preview_source) list;
  switch : switch option;
}
type workspace = {
  ws : Workspace_doc.t;
  lowered : Flow_sop.Lower.t;
  fast : bool lazy_t;
  catalog : (Flow.Catalog.catalog, Diagnostic.t) result lazy_t;
}
type t = {
  scene : Kind_graph.t;  (** the objects; an object's input is its parent *)
  networks : network Int_map.t;  (** by geometry object id *)
  layers : Kind_graph.t Int_map.t;  (** by World id: its layers, each on the one below *)
  active_camera : int option;
  root : Objects.Root.parameters;
  settings : Settings.t;
  shell : shell option;
  view_worlds : (string * view_world option) list;
  view_roots : (string * view_root) list;
  homes : homes;
  scene_drives :
    (string * (Flow_sop.Port.parameter * Flow.Eval.value) list) Int_map.t;
  scene_textures : Flow.Eval.value Int_map.t;
  workspace : workspace;
}
type level = Scene | Inside of int
val network : t -> int -> network option
(** A geometry object's SOP network. *)

val layers : t -> int -> Kind_graph.t option
(** The World's layers. *)

val with_layers : t -> int -> Kind_graph.t -> t
val kinds : t -> level -> Kind_graph.t option
(** The objects of the scene, or the layers inside the World; none inside a geometry object. *)

val with_kinds : t -> level -> Kind_graph.t -> t
val resolve_level : t -> level -> level
val validate : t -> (unit, string) result
val prune : t -> t
val object_graph : t -> int -> string option
val scene_graph : t -> Kind_graph.t
val object_network : t -> int -> (Flow_sop.Network.t * int option) option
val dump : t -> string
val syntax_of : Flow.Syntax.t list -> home -> Flow.Syntax.t option
val describe : Flow.Syntax.t list -> home -> string
val template : home -> home
val loop_of : home -> home option
val copies : t -> home -> int
