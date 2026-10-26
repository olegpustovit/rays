(** The host bar of the workspace: the graph panel's toolbar (Add, Repeat, Iterate, function, macro, defn).  Buttons only
    report a click; [Core] maps it to the one command or edit it means. *)

type tool = Add | Repeat | Iterate | Fn | Macro | Defn

val tool_rect : Pxui.Ui.t -> header:int * int * int * int -> from:float -> tool -> (float * float * float * float) option
(** Where a toolbar button sits ([from] points after the header's left edge, see
    {!Pxui_shell.Chrome.tools_start}); none when the header is too narrow for it. *)

val rule : Pxui.Ui.t -> key:string -> header:int * int * int * int -> from:float -> unit
(** The 1 x 12 rule that parts a header's title from the tools that start at [from]. *)

val graph_tools : Pxui.Ui.t -> header:int * int * int * int -> from:float -> enabled:(tool -> bool) -> tool option
(** The toolbar in a panel header, [from] points after the header's left edge; the tool
    clicked this frame. *)

val tree_text : name:string -> scene:string -> Editor_core.Panels.t ->
  string * (Editor_core.Panels.path -> string option)
(** The editor graph [name] of a panel tree (a document without one is given its host layout
    before its first panel edit): every leaf and split is a binding, and the function names the
    binding of a leaf by its path. *)
