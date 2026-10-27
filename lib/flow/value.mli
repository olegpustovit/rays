(** Pure workspace values, parameterized by the evaluator's functions and residuals. *)
type ('f, 'r) t =
  | Int of int | Float of float | Bool of bool | Text of string
  | Vec2 of float * float
  | Vec3 of float * float * float
  | Vec4 of float * float * float * float
  | List of ('f, 'r) t array
  | Int_array of int array
  | Text_array of string array
  | Float_array of float array
  | Vec2_array of float array  (** interleaved xy coordinates *)
  | Vec3_array of float array  (** interleaved xyz coordinates *)
  | Vec4_array of float array  (** interleaved xyzw coordinates *)
  | Record of (string * ('f, 'r) t) list
  | Deferred of Ty.t * int | No_geo
  | Struct of string * Ty.t * (string * ('f, 'r) t) list
  | Source of string * Ty.t * (string * ('f, 'r) t) list
      (** A typed host source; its fields describe resolution rather than the result. *)
  | Fn of 'f | Residual of 'r

val raise_diagnostic : Diagnostic.t -> 'a
val fail : string -> string -> 'a
val failf : string -> ('a, unit, string, 'b) format4 -> 'a
val hash : float list -> float
val to_int : string -> float -> int
val fin : string -> float -> float
val num : ('f, 'r) t -> float
val truthy : ('f, 'r) t -> bool
val int_of : ('f, 'r) t -> int
val ty_of : ('f, 'r) t -> Ty.t
val elem_ty : ('f, 'r) t array -> Ty.t
val coerce_to : Ty.t -> ('f, 'r) t -> ('f, 'r) t
val show_with : (('f, 'r) t -> ('f, 'r) t) -> ('f, 'r) t -> string
val key_of : residual:('r -> int) -> ('f, 'r) t -> string
val comps : ('f, 'r) t -> float * float * float
val is_vec : ('f, 'r) t -> bool
val arith : string -> (float -> float -> float) -> ('f, 'r) t -> ('f, 'r) t -> ('f, 'r) t
val list_arg : ('f, 'r) t -> ('f, 'r) t array
val array_length : ('f, 'r) t -> int
val array_get : ('f, 'r) t -> int -> ('f, 'r) t
val array_put : float array -> int -> int -> ('f, 'r) t -> unit
val int_array_put : int array -> int -> ('f, 'r) t -> unit
val array_init : Ty.t -> int -> (int -> ('f, 'r) t) -> ('f, 'r) t
val validate : ('f, 'r) t -> unit

module Private : sig
  (** Evaluate the protected thunk once. A reached typed failure is rethrown
      through its pure mapper, preserving provenance; cancellation bypasses it. *)
  val with_error : protected:(unit -> ('f,'r) t) -> map:(('f,'r) t -> ('f,'r) t) -> ('f,'r) t

  (** Full 0/1 validation followed by first, or last in the first failing range.
      The optional check is invoked at entry and coarse row boundaries. *)
  val error_row_index : ?grain:int -> ?check:(unit -> unit) -> ('f,'r) t -> int option

  (** The selected index is passed only to an always-throwing diagnostic mapper.
      An all-valid mask returns nil without evaluating select or map. *)
  val diagnose_rows : select:(('f,'r) t -> int -> int) -> map:(('f,'r) t -> ('f,'r) t) -> ('f,'r) t -> ('f,'r) t
end
