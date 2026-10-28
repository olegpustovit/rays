(** The operation names shared by the packed CPU/GPU emitter and the checker's advisory eligibility
    set. Adding a name requires both execution tiers to implement its instruction semantics. *)
type binary =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Min
  | Max
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | And
  | Or
  | Atan2
  | Hypot
  | Ldexp

type unary = Sin | Cos | Sqrt | Abs | Not | Tan | Asin | Acos | Exp | Log | Sign | Rsqrt
type integer = Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ishl | Ishr
type comparison = Ilt | Ile | Igt | Ige | Ieq

type conversion =
  | To_float
  | Truncate
  | Floor
  | Ceil
  | Round
  | Coerce_int
  | Integer_result of string

val register_limit : int
val supported_noise_octaves : int -> bool (* 1 to 64, the native attribute noise range *)

val supported_gpu_noise_octaves :
  int -> bool (* 1 to 32: Metal and WGSL refuse more as E_GPU_FORM *)

val finite_float32 : float -> bool
(** Limits shared by packed compilation and GPU form qualification. *)

val scalar_width : Ty.t -> int option
(** Register widths for Float/Int/Bool and Vec2/3/4 expressions. *)

val array_width : Ty.t -> int option
(** Source widths for Array Int/Float/Vec2/3/4; Bool arrays are refused. *)

val binary : string -> binary option
val unary : string -> unary option
val integer : string -> integer option
val comparison : string -> comparison option
val integer_name : integer -> string
val comparison_name : comparison -> string
val integer_apply : integer -> int -> int -> int
val comparison_apply : comparison -> int -> int -> bool
val rand_at : int -> int -> float
val noise_names : string list
val names : string list
val supports : string -> bool
