(** Compiled Houdini-style attribute-name selection patterns. *)

type t
type rewrite
type rewrite_set

val compile : string -> (t, string) result
(** Compile a whitespace-separated sequence of full-name glob terms. Blank and [*] select all names.
    A leading [^] excludes a term; when the first term is excluded, selection starts from all names,
    otherwise it starts empty. Later matching terms override earlier ones. Globs support [*], [?],
    byte classes/ranges such as [[a-z]], negated classes such as [[!0-9]], and backslash escaping.
    Malformed escapes/classes return [Error]. *)

val matches : t -> string -> bool
(** Test one name without allocation. Matching is against the complete name. Time is O(pattern bytes
    \+ name bytes) for ordinary globs; repeated-star backtracking has O(pattern bytes * name bytes)
    worst-case time. *)

val apply : selected:bool -> t -> string -> bool
(** Apply the compiled terms to an existing selection bit. This is useful when implicit names, such
    as attributes supplied by reference geometry, precede the textual pattern. Unlike {!matches},
    this does not infer the initial bit from whether the first term is an exclusion. *)

val compile_rewrite : pattern:string -> replacement:string -> (rewrite, string) result
(** Compile one full-name glob and replacement. Each [*], run of [?], or run of character classes in
    the source captures its matched bytes. Replacement wildcards substitute captures, pairing
    distinctive wildcard shapes first and remaining captures in stable order; this permits [*_???]
    to become [old_???_*]. Source and replacement must each be one non-exclusion term with equal
    wildcard-group counts. Backslash escapes make metacharacters and whitespace literal. *)

val rewrite : rewrite -> string -> string option
(** Return the rewritten name, or [None] when the complete source glob does not match. *)

val compile_rewrite_set : pattern:string -> replacement:string -> (rewrite_set, string) result
(** Compile aligned whitespace-separated rename terms. Exclusion terms in the source selection do
    not consume replacements; every positive source term has one non-exclusion replacement term.
    When several source terms match, the last matching rename wins. *)

val rewrite_set : rewrite_set -> string -> string option
(** Apply an aligned rename set to one complete name. *)
