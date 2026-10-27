type binary = Add | Sub | Mul | Div | Mod | Pow | Min | Max | Lt | Le | Gt | Ge | Eq | And | Or | Atan2 | Hypot
type unary = Sin | Cos | Sqrt | Abs | Not | Tan | Asin | Acos | Exp | Log | Sign
type integer = Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ishl | Ishr
type comparison = Ilt | Ile | Igt | Ige | Ieq
type conversion = To_float | Truncate | Floor | Ceil | Round | Coerce_int | Integer_result of string

let register_limit = 1024
let supported_noise_octaves n = n >= 1 && n <= 64
let supported_gpu_noise_octaves n = n >= 1 && n <= 32
let finite_float32 x = Float.is_finite (Int32.float_of_bits (Int32.bits_of_float x))

let scalar_width = function
  | Ty.Float | Int | Bool -> Some 1 | Vec2 -> Some 2 | Vec3 -> Some 3 | Vec4 -> Some 4
  | _ -> None
let array_width = function
  | Ty.Array (Ty.Int | Float | Vec2 | Vec3 | Vec4 as ty) -> scalar_width ty
  | _ -> None

let binaries = ["+", Add; "-", Sub; "*", Mul; "/", Div; "mod", Mod; "pow", Pow;
  "min", Min; "max", Max; "<", Lt; "<=", Le; ">", Gt; ">=", Ge; "=", Eq;
  "and", And; "or", Or; "atan2", Atan2; "hypot", Hypot]
let unaries = ["sin", Sin; "cos", Cos; "sqrt", Sqrt; "abs", Abs; "not", Not;
  "tan", Tan; "asin", Asin; "acos", Acos; "exp", Exp; "log", Log; "sign", Sign]
let noise_names = ["noise3"]
let derived_names = ["length";"dot";"cross";"normalize";"fma"]
let binary name = List.assoc_opt name binaries
let unary name = List.assoc_opt name unaries
let integers = ["int/add", Iadd; "int/sub", Isub; "int/mul", Imul; "int/div", Idiv;
  "int/mod", Imod; "int/and", Iand; "int/or", Ior; "int/xor", Ixor;
  "int/shl", Ishl; "int/shr", Ishr]
let comparisons = ["int/lt", Ilt; "int/le", Ile; "int/gt", Igt; "int/ge", Ige; "int/eq", Ieq]
let integer name = List.assoc_opt name integers
let comparison name = List.assoc_opt name comparisons
let integer_name operation = fst (List.find (fun (_,op)->op=operation) integers)
let comparison_name operation = fst (List.find (fun (_,op)->op=operation) comparisons)
let integer_apply operation a b =
  (match operation with
   | Ishl | Ishr when b < 0 || b > 62 ->
       Value.fail "E_RANGE" "Integer shift count must be within [0, 62]."
   | _ -> ());
  try match operation with
    | Iadd -> a+b | Isub -> a-b | Imul -> a*b | Idiv -> a/b | Imod -> a mod b
    | Iand -> a land b | Ior -> a lor b | Ixor -> a lxor b
    | Ishl -> a lsl b | Ishr -> a lsr b
  with Division_by_zero -> Value.fail "E_RANGE" "Integer division by zero."
let comparison_apply operation (a : int) (b : int) = match operation with
  | Ilt -> a<b | Ile -> a<=b | Igt -> a>b | Ige -> a>=b | Ieq -> a=b
let rand_at seed index = Rays_math.Rand.float_at_int seed ~index
let names = List.map fst binaries @ List.map fst unaries @ noise_names @ derived_names
  @ List.map fst integers @ List.map fst comparisons @ ["rand/at"]
let supports name = List.mem name names
