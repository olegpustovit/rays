type t = Geometry | Float | Int | Bool | Vec3 | Image | Fn of Ty.fn_signature

type value =
  | Float_value of float
  | Int_value of int
  | Bool_value of bool
  | Vec3_value of float * float * float

let name = function
  | Geometry -> "Geometry"
  | Float -> "Float"
  | Int -> "Int"
  | Bool -> "Bool"
  | Vec3 -> "Vec3"
  | Image -> "Image"
  | Fn signature -> Ty.to_string (Ty.Fn (Some signature))

let of_field_kind = function
  | Param.Floating_view _ -> Some Float
  | Integer_view _ -> Some Int
  | Toggle_view -> Some Bool
  | Text_view | Choice_view _ -> None

let value_type = function
  | Float_value _ -> Float
  | Int_value _ -> Int
  | Bool_value _ -> Bool
  | Vec3_value _ -> Vec3

let can_connect ~source ~target =
  match (source, target) with
  | Geometry, Geometry | Vec3, Vec3 | Image, Image -> true
  | Fn source, Fn target -> source.params = target.params && Ty.fits source.result target.result
  | (Image | Fn _), _ | _, (Image | Fn _) -> false
  | Geometry, _ | _, Geometry | Vec3, _ -> false
  | (Float | Int | Bool), (Float | Int | Bool | Vec3) -> true

let coerce ~target value =
  let mismatch () =
    Error
      (Diagnostic.error ~code:"E_TYPE"
         (Printf.sprintf "Cannot drive %s with %s" (name target) (name (value_type value))))
  in
  if not (can_connect ~source:(value_type value) ~target) then mismatch ()
  else if value_type value = target then Ok value
  else
    match value with
    | Vec3_value _ -> mismatch ()
    | Float_value _ | Int_value _ | Bool_value _ -> (
        let number =
          match value with
          | Float_value x -> x
          | Int_value x -> float_of_int x
          | Bool_value x -> if x then 1. else 0.
          | Vec3_value _ -> assert false
        in
        match target with
        | Float -> Ok (Float_value number)
        | Bool -> Ok (Bool_value (number <> 0.))
        | Vec3 -> Ok (Vec3_value (number, number, number))
        | Int ->
            if not (Float.is_finite number) then
              Error (Diagnostic.error ~code:"E_TYPE" "A non-finite value cannot drive Int")
            else
              let rounded = Float.floor (number +. 0.5) in
              (* as [Eval.round]: a tie rounds up *)
              let integer =
                if rounded >= float_of_int max_int then max_int
                else if rounded <= float_of_int min_int then min_int
                else int_of_float rounded
              in
              Ok (Int_value integer)
        | Geometry | Image | Fn _ -> mismatch ())
