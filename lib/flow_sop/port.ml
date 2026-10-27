type t = { node : int; path : string }
let compare a b = let node = Int.compare a.node b.node in
  if node <> 0 then node else String.compare a.path b.path
module Map = Map.Make (struct type nonrec t = t let compare = compare end)

type literal = Scalar of Param.value | Vector of float * float * float
type parameter = { path : string; fields : Param.field_view list; ty : Flow.Port_type.t option }
let error code message = Error (Diagnostic.error ~code message)
let parameters ?infer fields =
  Result.map (List.map (function
    | [] -> assert false
    | (field : Param.field_view) :: _ as fields ->
        let path, ty = match field.vec3 with
          | Some (name, _) -> name, Some Flow.Port_type.Vec3
          | None -> field.name, (match field.kind, field.current with
              | Param.Choice_view _, Param.Int_value _ -> Some Flow.Port_type.Int
              | _ -> Flow.Port_type.of_field_kind field.kind) in
        {path; fields; ty}))
    (Result.map_error (Diagnostic.error ~code:"E_VEC3") (Param.group_vec3 ?infer fields))

let components parameter = match parameter.ty, parameter.fields with
  | Some Flow.Port_type.Vec3, [x;y;z] -> List.map2 (fun axis field ->
      {path = parameter.path ^ "." ^ axis; fields = [field]; ty = Some Flow.Port_type.Float}) ["x";"y";"z"] [x;y;z]
  | _ -> []
let find_parameter parameters path =
  match List.find_opt (fun parameter -> parameter.path = path) parameters with
  | Some parameter -> Ok parameter
  | None ->
      let rec find = function
        | [] -> error "E_PORT" ("No parameter port " ^ path)
        | parameter :: rest -> match List.find_opt (fun parameter -> parameter.path = path) (components parameter) with
            | Some parameter -> Ok parameter | None -> find rest in
      find parameters
let number (field : Param.field_view) = match field.current with Param.Float_value value -> value | _ -> assert false
let literal parameter = match parameter.fields with
  | [field] -> Scalar field.current
  | [x;y;z] -> Vector (number x, number y, number z)
  | _ -> assert false
let literal_changes parameter value = match parameter.fields, value with
  | [field], Scalar value -> Ok [field.name, value]
  | [x;y;z], Vector (a,b,c) -> Ok [x.name, Param.Float_value a; y.name, Param.Float_value b; z.name, Param.Float_value c]
  | _ -> error "E_TYPE" ("Literal does not match port " ^ parameter.path)

let normalize_float (field : Param.field_view) number = match field.kind with
  | Param.Floating_view range -> Result.map_error
      (fun message -> Diagnostic.error ~code:"E_TYPE" (field.name ^ ": " ^ message))
      (Param.normalize_value (Param.Floating range) number)
  | _ -> error "E_TYPE" ("Not a float field: " ^ field.name)
let normalize parameter value =
  match parameter.ty with
  | None -> error "E_TYPE" ("Port " ^ parameter.path ^ " is literal-only")
  | Some target -> Result.bind (Flow.Port_type.coerce ~target value) (fun value ->
      match parameter.fields, value with
      | [field], Flow.Port_type.Float_value value -> Result.map (fun value ->
          Flow.Port_type.Float_value value, [field.name, Param.Float_value value]) (normalize_float field value)
      | [field], Flow.Port_type.Int_value value ->
          (match field.kind with
           | Param.Integer_view range -> Result.map (fun value ->
               Flow.Port_type.Int_value value, [field.name, Param.Int_value value])
               (Result.map_error (Diagnostic.error ~code:"E_TYPE") (Param.normalize_value (Param.Integer range) value))
           | Param.Choice_view labels when (match field.current with Param.Int_value _ -> true | _ -> false) ->
               Result.map (fun value ->
                 Flow.Port_type.Int_value value, [field.name, Param.Int_value value])
                 (Result.map_error (Diagnostic.error ~code:"E_TYPE")
                   (Param.normalize_value (Param.Index_choice labels) value))
           | _ -> error "E_TYPE" ("Not an integer field: " ^ field.name))
      | [field], Flow.Port_type.Bool_value value -> Ok (Flow.Port_type.Bool_value value, [field.name, Param.Bool_value value])
      | [x;y;z], Flow.Port_type.Vec3_value (a,b,c) ->
          Result.bind (normalize_float x a) (fun a -> Result.bind (normalize_float y b) (fun b ->
            Result.map (fun c -> Flow.Port_type.Vec3_value (a,b,c),
              [x.name, Param.Float_value a; y.name, Param.Float_value b; z.name, Param.Float_value c]) (normalize_float z c)))
      | _ -> error "E_TYPE" ("Value does not match port " ^ parameter.path))

let color_of_text text =
  let digit c = match c with
    | '0' .. '9' -> Some (Char.code c - 48) | 'a' .. 'f' -> Some (Char.code c - 87)
    | 'A' .. 'F' -> Some (Char.code c - 55) | _ -> None in
  let n = String.length text in
  let channel i = match n with
    | 4 -> Option.map (fun d -> float (17 * d) /. 255.) (digit text.[i])
    | 7 | 9 -> (match digit text.[2 * i - 1], digit text.[2 * i] with
        | Some a, Some b -> Some (float (16 * a + b) /. 255.) | _ -> None)
    | _ -> None in
  if n = 0 || text.[0] <> '#' then None
  else match channel 1, channel 2, channel 3 with
    | Some r, Some g, Some b -> Some (r, g, b) | _ -> None
