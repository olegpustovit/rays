type t = Geometry | Float | Int | Bool | Vec3 | Image | Fn of Ty.fn_signature

type value =
  | Float_value of float
  | Int_value of int
  | Bool_value of bool
  | Vec3_value of float * float * float

val name : t -> string
val of_field_kind : Param.kind_view -> t option
val can_connect : source:t -> target:t -> bool

val coerce : target:t -> value -> (value, Diagnostic.t) result
(** Coercion at a driven port. Float-to-int rounds, saturates the machine int range, then the caller
    applies field hard bounds through [Param.apply]. Non-finite float-to-int values are errors.
    Vector-to-scalar is rejected. *)
