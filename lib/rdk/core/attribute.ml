type owner = Point | Vertex | Primitive | Detail
type storage =
  | Float of float array
  | Int of int array
  | Int_array of Packed.Int_array.t
  | Float_array of Packed.Float_array.t
  | Float2 of Packed.Float2.t
  | Float3 of Packed.Float3.t
  | Float4 of Packed.Float4.t
  | Text of string array
type t = {
  name : string;
  owner : owner;
  storage : storage;
  data_id : int;
  storage_id : int;
}

type _ kind =
  | K_float : float array kind

  | K_float2 : Packed.Float2.t kind
  | K_float3 : Packed.Float3.t kind
  | K_float4 : Packed.Float4.t kind

type 'a key = { key_name : string; key_owner : owner; key_kind : 'a kind }

let float = K_float
let float2 = K_float2
let float3 = K_float3
let float4 = K_float4
let key ~name ~owner key_kind = { key_name = name; key_owner = owner; key_kind }

let storage_of_kind : type a. a kind -> a -> storage = fun kind value ->
  match kind with
  | K_float -> Float value

  | K_float2 -> Float2 value
  | K_float3 -> Float3 value
  | K_float4 -> Float4 value

let storage_length = function
  | Float values -> Array.length values
  | Int values -> Array.length values
  | Int_array values -> Packed.Int_array.length values
  | Float_array values -> Packed.Float_array.length values
  | Float2 values -> Packed.Float2.length values
  | Float3 values -> Packed.Float3.length values
  | Float4 values -> Packed.Float4.length values
  | Text values -> Array.length values

let create_owned ~name ~owner storage =
  if String.trim name = "" then Error "Attribute.create_owned: empty name"
  else Ok { name; owner; storage; data_id = Data_id.fresh ();
            storage_id = Data_id.fresh () }
let create_key_owned key value =
  create_owned ~name:key.key_name ~owner:key.key_owner
    (storage_of_kind key.key_kind value)

let get : type a. a key -> t -> a option = fun key attribute ->
  if key.key_owner <> attribute.owner || not (String.equal key.key_name attribute.name)
  then None
  else match key.key_kind, attribute.storage with
    | K_float, Float values -> Some (Array.copy values)

    | K_float2, Float2 values -> Some values
    | K_float3, Float3 values -> Some values
    | K_float4, Float4 values -> Some values

    | _ -> None

let normal ~owner = key ~name:"N" ~owner float3
let color ~owner = key ~name:"Cd" ~owner float4
let tex_coord ~owner = key ~name:"uv" ~owner float2
let name value = value.name
let owner value = value.owner
let storage value = match value.storage with
  | Float values -> Float (Array.copy values)
  | Int values -> Int (Array.copy values)
  | Text values -> Text (Array.copy values)
  | Int_array _ | Float_array _ | Float2 _ | Float3 _ | Float4 _ as storage ->
      storage
let length value = storage_length value.storage
let data_id value = value.data_id
let storage_id value = value.storage_id
let with_name name value =
  if String.trim name = "" then Error "Attribute.with_name: empty name"
  else if String.equal name value.name then Ok value
  else Ok { value with name; data_id = Data_id.fresh () }
let payload_bytes value = match value.storage with
  | Float values -> Array.length values * 8
  | Int values -> Array.length values * (Sys.word_size / 8)
  | Int_array values -> Packed.Int_array.payload_bytes values
  | Float_array values -> Packed.Float_array.payload_bytes values
  | Float2 values -> Packed.Float2.payload_bytes values
  | Float3 values -> Packed.Float3.payload_bytes values
  | Float4 values -> Packed.Float4.payload_bytes values
  | Text values ->
      Array.fold_left (fun total text -> total + String.length text) 0 values
      + (Array.length values * (Sys.word_size / 8))
let kind_name value = match value.storage with
  | Float _ -> "float" | Int _ -> "int" | Float2 _ -> "float2"
  | Int_array _ -> "int_array" | Float_array _ -> "float_array"
  | Float3 _ -> "float3" | Float4 _ -> "float4" | Text _ -> "text"

module Private = struct
  let storage value = value.storage
end

let float_planes = function
  | Float values -> Some [| values |]
  | Float2 v -> let v = Packed.Float2.Private.view v in Some [| v.x; v.y |]
  | Float3 v -> let v = Packed.Float3.Private.view v in Some [| v.x; v.y; v.z |]
  | Float4 v -> let v = Packed.Float4.Private.view v in Some [| v.x; v.y; v.z; v.w |]
  | Int _ | Int_array _ | Float_array _ | Text _ -> None

let of_float_planes ~like planes = match like with
  | Float _ -> Float planes.(0)
  | Float2 _ ->
      Float2 (Result.get_ok (Packed.Float2.of_owned ~x:planes.(0) ~y:planes.(1)))
  | Float3 _ ->
      Float3 (Packed.Float3.Private.of_owned_exn ~x:planes.(0) ~y:planes.(1)
        ~z:planes.(2))
  | Float4 _ ->
      Float4 (Result.get_ok (Packed.Float4.of_owned ~x:planes.(0) ~y:planes.(1)
        ~z:planes.(2) ~w:planes.(3)))
  | Int _ | Int_array _ | Float_array _ | Text _ ->
      invalid_arg "Attribute.of_float_planes: not a float storage"
