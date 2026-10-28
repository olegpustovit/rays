(** The node menu (leader [/ a], the sheet's [01]): a search field over the kinds from the start,
    the likeliest first (what takes an input after the selected node, then the rest), typed search
    over the whole catalog by name, key or category. Pure state plus one PXUI popup; the host
    decides what a picked key adds. *)

type entry = {
  key : string;
  label : string;
  category : string list;
  arity : int;
  context : string;  (** the graph the kind is for: ["sop"], ["value"], ["scene"] ... *)
  output : Flow.Ty.t;  (** what the node makes: the colour of its square in the rows *)
  off : string option;
      (** why the kind cannot be placed in this graph ([Some "not in sop"]), if it cannot: such a
          kind is listed after the others when searching, whole row in ink-3, and cannot be picked
      *)
}

type t

val of_ops : ?extra:Flow.Op.t list -> Flow.Context.t -> entry list
(** Entries for the context's immutable operator declarations. *)

val create : ?after:string -> x:int -> y:int -> entry list -> t
(** A menu open at a screen point (clamped inside the bounds it is drawn in); [after] is the node
    the new one goes after, which the title says. Entries with an empty key, label or category path,
    or a repeated key, are dropped. *)

val port_color : Pxui.theme -> Flow.Ty.t -> Rays_math.Color.t
(** The colour of a type's port: the square of a kind in the menu, and the ports, type squares and
    wires of the graph pane (a list is its elements' colour). *)

val color : Pxui.theme -> Flow.Ty.color -> Rays_math.Color.t
(** Resolve a declaration's color role through the current kit palette. *)

val position : t -> int * int
(** The screen point the menu was opened at (where the host places what it adds). *)

val update : t -> Pxui.Ui.t -> bounds:int * int * int * int -> t option * string option
(** Build the popup inside [Pxui.Ui.frame]: the menu while it stays open ([None] after Escape, a
    press outside or a pick) and the key of the entry picked this frame. *)

module Private : sig
  val keys : t -> query:string -> string list
  (** The entry keys the query lists, in order (tests). *)
end
