type stage = Vertex | Fragment | Compute
type binding_kind = Uniform_buffer | Storage_buffer | Sampled_texture | Storage_texture | Sampler
  | Acceleration_structure
type entry_point = { name : string; stage : stage }
type binding =
  { group : int; binding : int; kind : binding_kind; visibility : stage list }
type constant_value = Bool of bool | Int32 of int32 | Uint32 of int32 | Float32 of float
type descriptor =
  { backend : string
  ; label : string option
  ; bytes : bytes
  ; entry_points : entry_point list
  ; bindings : binding list
  }
type t =
  { backend : string
  ; label : string option
  ; artifact : bytes
  ; entries : entry_point list
  ; layout : binding list
  }

let error message = Error (Error.make "Ogpu.Shader.create" Error.Invalid_argument message)
let valid_text value = value <> "" && not (String.contains value '\000')

let validate_entries entries =
  let rec loop seen = function
    | [] -> Ok ()
    | entry :: _ when not (valid_text entry.name) -> error "entry-point name is empty or contains NUL"
    | entry :: _ when List.mem entry.name seen -> error "duplicate entry point"
    | entry :: rest -> loop (entry.name :: seen) rest
  in
  loop [] entries

(* Buffers (including inline constants and acceleration structures), textures
   and samplers occupy separate index spaces, as in Metal's argument tables. *)
let index_space = function
  | Uniform_buffer | Storage_buffer | Acceleration_structure -> 0
  | Sampled_texture | Storage_texture -> 1
  | Sampler -> 2

let validate_bindings bindings =
  let rec loop seen = function
    | [] -> Ok ()
    | value :: _ when value.group < 0 || value.binding < 0 -> error "binding indices must be nonnegative"
    | value :: _ when value.visibility = [] -> error "binding visibility is empty"
    | value :: _ when List.sort_uniq compare value.visibility <> value.visibility ->
        error "binding visibility must be sorted and unique"
    | value :: _ when List.mem (value.group, value.binding, index_space value.kind) seen ->
        error "duplicate reflected binding"
    | value :: rest -> loop ((value.group, value.binding, index_space value.kind) :: seen) rest
  in
  loop [] bindings

let create (descriptor : descriptor) =
  if descriptor.backend <> "metal" && descriptor.backend <> "mock" then
    error "unknown shader backend"
  else if Option.fold ~none:false ~some:(fun value -> not (valid_text value)) descriptor.label then
    error "shader label is empty or contains NUL"
  else if Bytes.length descriptor.bytes = 0 then error "shader artifact is empty"
  else
    match validate_entries descriptor.entry_points with
    | Error _ as result -> result
    | Ok () ->
        match validate_bindings descriptor.bindings with
        | Error _ as result -> result
        | Ok () ->
            let descriptor = { descriptor with bytes = Bytes.copy descriptor.bytes } in
            Ok { backend = descriptor.backend; label = descriptor.label
               ; artifact = descriptor.bytes; entries = descriptor.entry_points
               ; layout = descriptor.bindings }

let validate_constants constants =
  if List.exists (fun (name, _) -> not (valid_text name)) constants
     || List.length (List.sort_uniq compare (List.map fst constants)) <> List.length constants then
    error "function constant names must be nonempty and unique"
  else Ok ()

let of_source = create

let backend value = value.backend
let label value = value.label
let bytes value = Bytes.copy value.artifact
let entry_points value = value.entries
let bindings value = value.layout
