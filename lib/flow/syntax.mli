(** The authored tree of the workspace language (`.rays` text): what [Lisp.print] prints and what
    [Macro] and [Workspace] read. It keeps everything a person wrote: comments (notes), [^:flags]
    (meta), the spelling of numbers, and stable form ids. It also reads the generated catalog
    manifest ({!Catalog.catalog_of_manifest}). *)

type id = int
(** Unique per form within one parse ([parse] numbers forms 0, 1, ... in reading order); [renumber]
    gives another tree a disjoint range. *)

val library_base : id
(** The ids of the embedded library's forms start here, fixed by their place in the library; an
    authored form's id is below it and may restart across imports and reparses. *)

type quote_kind = Plain | Quasi | Unquote | Splice  (** ['] [`] [~] [~@] *)

type t = {
  id : id;
  node : node;
  span : Diagnostic.span;  (** byte span in the parsed source; [0,0] when built by [make] *)
  notes : string list;  (** comment lines written before the form, without [;] *)
  meta : string list;  (** [^:bypass] flags, in written order *)
  tail : string list;  (** comment lines before the closing bracket of a container *)
}

and node =
  | Sym of string
  | Kw of string  (** without the colon *)
  | Num of string  (** as written, so [2.0] stays a float *)
  | Str of string  (** decoded *)
  | List of t list
  | Vec of t list  (** any length: a 3-vector is vec3 by type, not by syntax *)
  | Map of t list  (** [{}]: the children alternate key and value *)
  | Quote of quote_kind * t

module Text_lexer : sig
  type word = Symbol of string | Keyword of string | Number of string

  type kind =
    | Open of char
    | Close of char
    | Prefix of char
    | Meta of string
    | Comment of string
    | Word of word
    | String of string
    | End

  type issue = { start : int; finish : int; code : string; message : string }
  type token = { kind : kind; start : int; stop : int; issue : issue option }

  val separator : char -> bool
  val number : string -> bool
  val lex : string -> token array
end

val number : string -> bool
(** Whether a word is a number literal: [-?digits(.digits)?] with an optional exponent ([1e-14],
    [2.5E+6]), or [NaN], [Infinity], [-Infinity]. *)

val number_value : string -> float option
(** Decode a finite numeric value or an explicit non-finite literal. Numeric overflow and other
    non-finite spellings are refused. *)

val separator : char -> bool
(** Whether a byte ends a word. *)

val parse : string -> (t list, Diagnostic.t) result
(** Read every top-level form. Errors carry code, span and position ([E_UNCLOSED], [E_UNEXPECTED],
    [E_DEPTH]). A comment becomes a note on the next form (register N1); comments before a closing
    bracket become that container's [tail]. *)

val make : ?notes:string list -> ?meta:string list -> node -> t
(** A form with id 0 and an empty span, for macro expansion; [renumber] it. *)

val equal : t -> t -> bool
(** Equal authored content, including comments and metadata, ignoring form IDs and byte spans. *)

val head : t -> string option
(** The symbol a list starts with: the name of a call or a special form. *)

val children : t -> t list
(** Direct sub-forms in reading order. *)

val attribute_args : string -> t list -> t list
(** Normalize cooked attribute/count selectors and supported attribute, group and array type
    enumeration spellings to text, retaining source identity and notes. *)

val renumber : int -> t -> t * int
(** [renumber first form] copies [form] numbering every form from [first] in reading order (spans,
    notes and meta kept); returns the next free id. *)

val pairs : 'a list -> ('a * 'a) list
(** Consecutive pairs of a flat list; a trailing odd element is dropped. *)
