(* Private graph-pane state and geometry queries. *)
module P = Flow_graph.Projection
module E = Flow_graph.Flow_edit
module S = Flow.Syntax
module Ty = Flow.Ty
module Ui = Pxui.Ui
type path = E.path
module Path_set : Set.S with type elt = path
type change =
    Syntax_edit of E.op
  | Layout_active of { graph : string; index : int; }
  | Probe_set of { zone : path; index : int; }
  | Zone_collapsed of { zone : path; collapsed : bool; }
  | Selected of path list
  | Moved of (path * float * float) list
  | Level_set of (path * P.level option * bool) list
  | Copy_requested of path list
  | Paste_requested
  | Menu_requested of float * float
  | Open_import of string
  | Macro_requested of path list
  | Defn_requested of path list
  | Frames_set of { scope : path;
      frames : (string * (float * float) * (float * float)) list;
    }
  | Display_set of path
  | Activated of path
  | Drop_over of { path : path; kind : string; value : string; }
  | Dropped of { path : path; kind : string; value : string; }
  | Notice of string
type direction = Left | Down | Up | Right
type command =
    Delete
  | Fold_into
  | Unfold
  | Unfold_all  (** [⌥⇧U]: every call written in a row of the selected card becomes its own card *)
  | Hoist
  | Bypass
  | Wrap_repeat
  | Wrap_iterate
  | Wrap_if
  | Wrap_map  (** [⇧M]: the selection becomes the body of a [map] over a [range] *)
  | Wrap_filter  (** [⇧L]: the same as the test of a [filter] *)
  | Inline_fn  (** [⌥⇧I]: a call of a [defn] becomes its body *)
  | Add_arm
  | Delete_arm
  | Make_fn
  | Make_macro
  | Make_defn
  | Collapse
  | Probe_step of int
  | Frame_all
  | Tidy  (** [⇧T]: every card back to the computed layout (the dragged ones stay) *)
  | Toggle_tags  (** [⇧Y]: long wires read as name tags at their use site, or all of them drawn *)
  | Walk of direction
  | Edit_name
  | Item_up
  | Item_down
  | Make_frame
  | Duplicate
  | Display
  | Copy
  | Cut
  | Paste
  | Frame_selection
  | Open_level
  | Point_level
  | Open_all
  | Point_all
  | Show_hints
type stats = {
  nodes : int;
  zones : int;
  rows : int;
  drawn_items : int;
  drawn_zones : int;
}
type wiring = {
  src : string;
  ty : Ty.t option;
  iter : bool;
  from : float * float;
  lift : (path * E.arg_key) list;  (** the inputs an Alt-drag lifted it from *)
}
type drag =
    Moving of { paths : path list; dx : float; dy : float; moved : bool; }
  | Wiring of wiring
  | Marquee of { base : Path_set.t; }
  | Sizing of { scope : path; index : int; dw : float; dh : float; }
  | Carrying of { scope : path; index : int; paths : path list; dx : float;
      dy : float;
    }
type editing =
    Name of path
  | Default of path
  | Title of path * int
  | Output of path
type fr = string * (float * float) * (float * float)
type hint_target = {
  label : string;
  node : path;
  key : E.arg_key option;
  pos : float * float;
}
type hinting = {
  src : string;
  src_ty : Ty.t;
  typed : string;
  targets : hint_target list;
  stage : path option;
  armed : bool;
}
type t = {
  x : int;
  y : int;
  width : int;
  height : int;
  theme : Pxui.theme;
  guide : bool;
  key : string;
  scope : P.scope option;
  imported : path -> string option;
  at : path -> (float * float) option;
  level_at : path -> (P.level * bool) option;
  pin_at : path -> string -> bool option;
  collapsed : path -> bool;
  lens : (path * int) list;
  probe : path -> int;
  records : Flow_graph.Probe.t option;
  chains : (path, path list) Hashtbl.t;
  folds : (path * int, path) Hashtbl.t;
  counts : (path, int) Hashtbl.t;
  frames : path -> (string * (float * float) * (float * float)) list;
  display : path option;
  framed : bool;
  layout : P.layout;
  geo : Scope_wires.geo;
  geo_fs : int;
  pan_x : float;
  pan_y : float;
  zoom : float;
  selected : Path_set.t;
  selected_wire : (path * E.arg_key * S.t option) option;
  panning_grab : bool;
  hovered_row : (path * E.arg_key) option;
  highlighted : Scope_wires.wire list;
  drag : drag option;
  editing : editing option;
  context : ((float * float) * path) option;
  stats : stats;
  switches : (path * int) list;
  carry_lit : (path * string) list;
  carry_hot : (path * bool) option;
  failed : (path * string) list;
  hinting : hinting option;
  back : (path * P.level) list;
  measure : int -> string -> float;
  tags : bool;
  chain : (Path_set.t * Scope_wires.wire array * (Path_set.t * Path_set.t)) option ref;
}
val chains_of : t -> (Path_set.t * Path_set.t) option
val wire_lit : Path_set.t * Path_set.t -> Scope_wires.wire -> bool
val tagged : t -> Scope_wires.wire -> bool
val node_lit : Path_set.t * Path_set.t -> path -> bool
val font_of : float -> int
val regeo : t -> P.scope -> P.layout -> shift:(path -> float * float) -> t
val no_stats : stats
val create : ?width:int -> ?height:int -> unit -> t
val with_bounds : x:int -> y:int -> width:int -> height:int -> t -> t
val suspend : t -> t
val with_guide : bool -> t -> t
val with_theme : Pxui.theme -> t -> t
val with_failed : (path * string) list -> t -> t
val with_carry :
  lit:(path * string) list -> hot:(path * bool) option -> t -> t
val selected : t -> path list
val editing : t -> bool
val select : path list -> t -> t
val clear_selection : t -> t
val stats : t -> stats
val zoom : t -> float
val scope_point : t -> scope:path -> float * float -> (float * float) option
val count_scope : P.scope -> int * int * int
val frame_all : t -> t
val count_of : t -> path -> int
val refresh : t -> t
val no_shift : 'a -> float * float
val lens_of : t -> path -> int option
val lay :
  ?sticky:bool -> t ->
  P.scope ->
  at:(path -> (float * float) option) -> collapsed:(path -> bool) -> P.layout
val fold_sources : P.scope -> (path * int, string list) Hashtbl.t
val switch_nodes : P.scope -> (path * int) list
val with_scope :
  ?at:(path -> (float * float) option) ->
  ?level:(path -> (P.level * bool) option) ->
  ?pin:(path -> string -> bool option) ->
  ?collapsed:(path -> bool) ->
  ?probe:(path -> int) ->
  ?frames:(path -> (string * (float * float) * (float * float)) list) ->
  ?imported:(path -> string option) ->
  ?display:path ->
  ?layouts:string list -> key:string -> P.scope -> t -> t
val relayout : ?sticky:bool -> t -> t

val tidy : t -> t
(** every card back to the computed layout; [with_scope] otherwise keeps each card where it was *)

val with_display : path option -> t -> t
val with_arguments : (path * E.arg_key * S.t) list -> t -> t
val with_records : Flow_graph.Probe.t -> t -> t
val node_of : t -> path -> P.node option
val collapsed_zone : t -> path -> bool
val item_at : t -> path -> (P.placed * float * float) option
val box_at : t -> path -> (float * float * float * float) option
val rel_at : t -> path -> (float * float) option
val selected_nodes : t -> P.node list
