(** The shell as data: a tree of panels. The editor graph evaluates to one; [Pxui_shell.Layout]
    gives it geometry. Pure and presentation-free, so the document library can lower into it. *)

type panel =
  | View of string
  | Canvas of string
  | Graph
  | List
  | Lisp
  | Inspector
  | Spreadsheet
  | Outline
  | Timeline
      (** [View key] identifies a viewport by its unique named binding; inline, looped and repeated
          panels use a tree path. The default layout uses [main]. [Graph] shows the graph/list/text
          views of the open network, [List] and [Lisp] force one of them, [Outline] lists the
          workspace's graphs. *)

type axis = [ `H | `V ]
(** [`H] puts [a] left of [b], [`V] puts [a] above [b] *)

type size = [ `Ratio of float | `First of int | `Second of int ]
(** How a split divides its extent: [a]'s share of it, or one side fixed in logical points with the
    other side taking the rest. *)

type t =
  | Leaf of panel
  | Split of { axis : axis; size : size; a : t; b : t }
  | Tile of t list  (** a grid of equal cells *)
  | Float of t  (** an overlay on its parent's rectangle, never a window *)

type path = int list
(** Child indices from the root: [a]/[b] are 0/1, a tile's cells and a float's content are numbered
    from 0. *)

type state = { collapsed : bool; window : (int * int * int * int) option }
(** A panel's saved disclosure and optional floating window bounds in logical points. *)

val default_state : state

val default : t
(** The three columns: view 45%, graph 35%, inspector 20%. *)

val main : panel
(** [View "main"], the viewport of the default layout. *)

val name : panel -> string
(** ["VIEW"], ["GRAPH"], ... *)

val leaves : t -> (path * panel) list
(** Every leaf in tree order, floats last. *)

val at : path -> t -> t option
(** The subtree at a path of {!leaves}; [None] when the tree has no such place. *)

val clamp_size : size -> size
(** A ratio within 0.1-0.9, a fixed side of at least one point. *)

val set_size : path -> size -> t -> t
(** The split at [path] gets this size ({!clamp_size}); anything else is unchanged. *)

val valid : t -> (unit, string) result
(** Ratios in 0.1-0.9, fixed sides of one point or more, tiles of 1-16 cells, no empty tree. *)

val to_string : t -> string
(** A one-line shape such as [(h view (h graph inspector))], for messages and tests. *)

val label : t -> string
(** A layout's name read from its tree: leaves are [View], [Graph], [List], [Lisp], [Inspector],
    [Outline], [Timeline]; side by side is [a | b], stacked [a / b] (a row inside a stack is
    bracketed), same-axis nesting flattens, a tile is a row, neighbours of one name collapse to
    [View ×4], and floating windows come last after [+]. *)

val summary : t -> string
(** The short name the status strip and the outline's layout rows use: one name per column group,
    [Outline | View | Inspector] (a stack or a tile reads as its first panel, floating windows are
    left out). *)

val labels : t list -> string list
(** {!label} of each layout; layouts that read the same add their largest panel and its share
    ([View | Graph · Graph 70%]), and any that still match get a number. *)
