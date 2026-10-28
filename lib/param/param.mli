(** Typed, renderer-independent parameter templates for procedural operators.

    Templates describe immutable record fields. Soft bounds define UI drag
    ranges; optional hard bounds are enforced for every write. Current values
    remain in the user's immutable record and never live in global state. *)

type impact = Cook | View | Export

type effects = {
  cook : bool;
  view : bool;
  export : bool;
}

val no_effects : effects
val add_impact : impact -> effects -> effects
val union_effects : effects -> effects -> effects
val has_effects : effects -> bool

type int_range = {
  soft_min : int;
  soft_max : int;
  hard_min : int option;
  hard_max : int option;
}

type float_range = {
  soft_min : float;
  soft_max : float;
  hard_min : float option;
  hard_max : float option;
}

type 'a choice = {
  options : (string * 'a) array;
  equal : 'a -> 'a -> bool;
}

type 'a encoding = {
  encode : 'a -> string;
  decode : string -> ('a, string) result;
  equal : 'a -> 'a -> bool;
}

type _ kind =
  | Toggle : bool kind
  | Integer : int_range -> int kind
  | Floating : float_range -> float kind
  | Text : string kind
  | Choice : 'a choice -> 'a kind
  | Index_choice : string array -> int kind
  | Encoded : 'a encoding -> 'a kind

type 'record field = Field : {
  name : string;
  label : string;
  description : string option;
  folder : string list;
  impact : impact;
  primary : bool;
  unit : string option;  (** suffix drawn after the value, e.g. ["mm"] *)
  vec3 : (string * int) option;
  kind : 'value kind;
  default : 'value;
  get : 'record -> 'value;
  set : 'value -> 'record -> 'record;
} -> 'record field

type 'record schema

type value =
  | Bool_value of bool
  | Int_value of int
  | Float_value of float
  | Text_value of string
  | Choice_value of string

(** Renderer-independent, type-erased metadata for one concrete parameter
    instance. Ordinary choices use public labels; index choices use [Int_value]. *)
type kind_view =
  | Toggle_view
  | Integer_view of int_range
  | Floating_view of float_range
  | Text_view
  | Choice_view of string array

type field_view = {
  name : string;
  label : string;
  description : string option;
  folder : string list;
  impact : impact;
  primary : bool;
  unit : string option;  (** suffix drawn after the value, e.g. ["mm"] *)
  vec3 : (string * int) option;  (** group name and x/y/z component index (0/1/2) *)
  kind : kind_view;
  default : value;
  current : value;
}

val group_vec3 : ?infer:bool -> field_view list -> (field_view list list, string) result
(** Scalars remain single fields. A vector is three contiguous float fields
    in component order and one folder; group names cannot overlap fields or
    other groups. [infer=true] first marks ungrouped consecutive [_x/_y/_z] or
    [_r/_g/_b] fields used by native scene schemas; a conflicting stem gets
    [_color] appended. The field order and remaining metadata are retained. *)

val integer :
  ?hard_min:int -> ?hard_max:int -> min:int -> max:int -> unit -> int kind

val floating :
  ?hard_min:float -> ?hard_max:float ->
  min:float -> max:float -> unit -> float kind

val choice : equal:('a -> 'a -> bool) -> (string * 'a) list -> 'a kind

val index_choice : string list -> int kind
(** Labelled integer indices. Labels affect presentation; values and cache keys
    retain the integer index. Empty or duplicate labels are rejected. *)

val encoded :
  equal:('a -> 'a -> bool) ->
  encode:('a -> string) ->
  decode:(string -> ('a, string) result) ->
  'a kind
(** Expose a structured immutable value through a deterministic textual
    representation. This is the typed fallback for rule lists and nested
    values until a renderer adapter provides dedicated multiparm rows. The
    encoded form is also the cache-key representation. *)

val field :
  name:string ->
  ?label:string ->
  ?description:string ->
  ?folder:string list ->
  ?impact:impact ->
  ?primary:bool ->
  ?unit:string ->
  ?vec3:(string * int) ->
  kind:'value kind ->
  default:'value ->
  get:('record -> 'value) ->
  set:('value -> 'record -> 'record) ->
  unit ->
  'record field

val schema :
  name:string -> default:'record -> 'record field list -> 'record schema

val default : 'record schema -> 'record
val fields : 'record schema -> 'record field list

val normalize_value : 'a kind -> 'a -> ('a, string) result
(** The same finite-value and hard-bound normalization used by [apply].
    Soft bounds do not clamp values. *)

(** Materialize type-erased inspector metadata for the supplied immutable
    record. This is the narrow boundary consumed by UI adapters. *)
val view : 'record schema -> 'record -> field_view list

(** Deterministic, unambiguous cache-key encoding of one concrete record.
    Custom SOP builders use this so PPX-exposed cook parameters and session
    cache identity cannot drift apart. *)
val key : 'record schema -> 'record -> string

(** [key] restricted to [Cook]-impact fields. [Node.parameterize] folds it into
    the session cache identity, so every cook-affecting schema field changes a
    parameterized node's cache key and view/export edits never invalidate it. *)
val cook_key : 'record schema -> 'record -> string

val float_text : ?decimal:bool -> float -> string
(** The fewest significant decimal digits that round-trip finite floats,
    including signed zero. Compact output chooses the shorter plain/exponent
    spelling, preferring plain on ties. [decimal=true] writes workspace float literals:
    a decimal point and no exponent. Non-finite classes print [NaN],
    [Infinity] or [-Infinity]. *)

(** The [Cook] fields as readable [name=value] pairs joined by [;]: a bool as
    [true]/[false], an int as itself, a float as the shortest decimal that reads back exactly, a text or
    encoded value verbatim, a choice as its label lowercased with every other
    character [_]. [Node.parameters] of a parameterized node is this text. *)
val cook_text : 'record schema -> 'record -> string

(** Apply a batch in order and accumulate its cook/view/export effects. *)
val apply_all :
  'record schema ->
  'record ->
  (string * value) list ->
  ('record * effects, string) result

(** Validate and normalize every field in a record, reusing the record when
    normalization leaves its values (including float bits) unchanged. *)
val normalize : 'record schema -> 'record -> ('record, string) result

(** Escaped tab-separated cells and newline-separated rows. Empty text is an
    empty table; backslashes, tabs and newlines within cells are escaped. *)
module String_table : sig
  val encode : string list list -> string
  val decode : string -> (string list list, string) result
end

(** The one model of a number field (a card's row, the inspector's control, a vector's cell): how
    a drag changes its text.  The kind is the parameter's, never the literal's spelling, so a float
    written [1] scrubs to [1.5]. *)
module Number : sig
  type kind = Int | Float

  val valid : kind -> string -> bool
  (** Text a field of this kind commits: an integer, or a finite float. *)

  val fraction : float * float -> string -> float option
  (** Where the value sits in a soft range, 0 to 1, for the field's position line. *)

  val show : float -> string
  (** A float as a field displays it: ten significant digits, never an exponent.  What is written
      is {!float_text}. *)

  val scrub : kind -> ?range:float * float -> string -> float -> bool -> string
  (** [scrub kind ?range origin dx fine]: the text after a drag of [dx] points from the text it
      began at.  An integer moves by one every 6 points; a float by 0.05 a point, or by a
      two-hundredth of its soft range, a tenth of that when [fine] (Shift).  The result is rounded
      to the step's digits (the origin's when it wrote more) and printed by {!float_text}, so
      [1234567.89] stays exact and no exponent appears.  With a [range] the drag stops at its ends
      (typing goes past them; a value already outside is not pulled in).  A drag shorter than one
      step returns [origin] itself: a click writes nothing. *)
end
