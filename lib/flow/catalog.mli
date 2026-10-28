(** Context-independent checking of the catalog. The catalog is plain
    data so the editor and the checker share one description. *)
type parameter = {
  name : string;
  label : string;
  ty : Port_type.t option;  (** [None] is a literal-only text/choice field. *)
  fields : (string * Param.kind_view * Param.value) list;
  folder : string list;  (** the folder of its (first) field *)
  primary : bool;  (** a primary field: the card shows it unset ([Flow_graph.Exposure]) *)
  unit : string option;  (** the unit suffix of a scalar field *)
}
type slot = { name : string; required : bool; rest : bool; ty : Ty.t option }
(** [rest]: the last slot repeats; extras are [name_2], [name_3], ... *)

type kernel_elements = Points | Primitives | Irregular
type kernel_facts = {
  elementwise : kernel_elements;
  reads : string list;
  writes : string list;
  preserves_topology : bool;
  exact : bool;
}
(** Domain-neutral kernel metadata at a catalog kind's default parameters.
    Runtime component dependencies come from the instantiated node. *)

val resolve_component_names : (string -> string option) -> string list -> string list
(** A [:facts] component name is literal, [$param] (the trimmed text of that parameter, dropped when
    blank), [$param|fallback] (the fallback when blank) or [$param~prefix] (what follows [prefix] in
    the parameter, dropped when it does not start with it). [lookup] gives a parameter's text. *)

type kind = {
  qualified : string;
  context : Context.t;
  slots : slot list;
  parameters : parameter list;
  outputs : (string * Port_type.t) list;
  facts : kernel_facts option;
}

type catalog = { version : int; kinds : kind list }

val parameters_of_fields : Param.field_view list -> (parameter list, string) result
(** Inspector fields grouped into the catalog's scalar and vec3 inputs. *)

type term = { node : term_node; ty : Port_type.t option }
and term_node =
  | Literal of Param.value
  | Nil
  | Vector of term list
  | Reference of string * string  (** binding, output *)

val catalog_of_manifest : string -> (catalog * string, Diagnostic.t) result
(** Read the generated catalog snapshot for compile-time checking, returning
    its descriptors and digest. *)

(** {2 Reuse by the workspace checker}
    [Flow.Workspace] calls catalog kinds through the same resolution and
    parameter validation as the sketch language. *)

val resolve_kind : catalog -> Context.t -> string -> (kind, string * string) result
(** Resolve a short or qualified kind name for a graph of the given context;
    [Error (code, message)] carries the diagnostic. Built-in [value/*] kinds
    are included. *)

val validate_parameter :
  (Diagnostic.severity -> string -> string -> unit) -> parameter -> term -> unit
(** Type, integer-literal, choice and range checks of one keyword argument;
    the callback receives severity, code and message. *)

val suggestion : string -> string list -> string
(** [" Did you mean x?"] for a close candidate, else [""]. *)

val short : string -> string
(** Unqualified kind name. *)
