let write_text = Runtime_resources.write_text

let read_text ~filename =
  try Ok (In_channel.with_open_bin filename In_channel.input_all)
  with Sys_error message -> Error message

(* ---- the forms stored beside the documents: all s-expressions ---- *)
module S = Flow.Syntax

let make = S.make
let kw name = make (S.Kw name)
let number x = make (S.Num (Flow.Lisp.float x))
let vec xs = make (S.Vec (List.map number xs))
let map pairs = make (S.Map (List.concat_map (fun (k, v) -> [ kw k; v ]) pairs))
let print form = fst (Flow.Lisp.print [ form ])
let num_of (f : S.t) = match f.node with
  | S.Num n -> (match float_of_string_opt n with Some v when Float.is_finite v -> Some v | _ -> None) | _ -> None
let vec_of (f : S.t) = match f.node with
  | S.Vec xs -> let values = List.filter_map num_of xs in
      if List.length values = List.length xs then Some values else None
  | _ -> None
let field (f : S.t) name = match f.node with
  | S.Map items ->
      let rec go = function
        | { S.node = S.Kw k; _ } :: v :: _ when k = name -> Some v
        | _ :: _ :: rest -> go rest | _ -> None in
      go items
  | _ -> None

(* The environment's view state in a preset: the viewport camera. *)
module Viewport = struct
  open Rays

  let encode easy ~look_through =
    let camera = Easy_camera.camera easy in
    let vector (v : Rays_math.Vec3.t) = vec [ v.x; v.y; v.z ] in
    map [ "eye", vector (Camera.position camera); "target", vector (Camera.target camera);
          "fov", number (Easy_camera.fov_y easy);
          "look_through", make (S.Sym (string_of_bool look_through)) ]

  let decode easy form =
    let vector name = match Option.bind (field form name) vec_of with
      | Some [ x; y; z ] -> Some (Rays_math.Vec3.create x y z) | _ -> None in
    let easy = match vector "eye", vector "target" with
      | Some eye, Some target -> Easy_camera.of_view ~eye ~target easy
      | _ -> easy in
    let easy = match Option.bind (field form "fov") num_of with
      | Some fov when fov > 0. && fov < Float.pi -> Easy_camera.with_fov_y fov easy
      | _ -> easy in
    easy, (match field form "look_through" with Some { S.node = S.Sym "true"; _ } -> true | _ -> false)

end

(* A sketch's or the editor's saved settings: one [(settings :sketch "name" :key value ...)] form. *)
module Settings = struct
  type value =
    | Bool of bool | Float of float | Int of int
    | Text of string | Choice of string | Pair of float * float
  type t = (string * value) list

  let encode = function
    | Bool v -> make (S.Sym (string_of_bool v))
    | Float v -> number v
    | Int v -> make (S.Num (string_of_int v))
    | Text v -> make (S.Str v)
    | Choice v -> make (S.List [ make (S.Sym "choice"); make (S.Str v) ])
    | Pair (x, y) -> vec [ x; y ]

  let decode (f : S.t) = match f.node with
    | S.Sym "true" -> Ok (Bool true)
    | S.Sym "false" -> Ok (Bool false)
    | S.Num n when int_of_string_opt n <> None -> Ok (Int (int_of_string n))
    | S.Num _ -> (match num_of f with Some v -> Ok (Float v) | None -> Error "invalid settings number")
    | S.Str v -> Ok (Text v)
    | S.List [ { S.node = S.Sym "choice"; _ }; { S.node = S.Str v; _ } ] -> Ok (Choice v)
    | S.Vec _ -> (match vec_of f with Some [ x; y ] -> Ok (Pair (x, y)) | _ -> Error "invalid settings pair")
    | _ -> Error "invalid settings value"

  let save ~sketch filename values =
    if List.exists (function
      | _, Float x -> not (Float.is_finite x)
      | _, Pair (x, y) -> not (Float.is_finite x && Float.is_finite y)
      | _ -> false) values then Error "non-finite settings value"
    else
      let form = make (S.List (make (S.Sym "settings") :: kw "sketch" :: make (S.Str sketch)
        :: List.concat_map (fun (name, value) -> [ kw name; encode value ]) values)) in
      write_text ~filename (print form)

  let load ~sketch filename =
    let ( let* ) = Result.bind in
    let* text = read_text ~filename in
    let* forms = Result.map_error Diagnostic.to_string (S.parse text) in
    match forms with
    | [ { S.node = S.List ({ S.node = S.Sym "settings"; _ } :: items); _ } ] ->
        let rec pairs acc = function
          | [] -> Ok (List.rev acc)
          | { S.node = S.Kw "sketch"; _ } :: { S.node = S.Str name; _ } :: rest when name = sketch ->
              pairs acc rest
          | { S.node = S.Kw "sketch"; _ } :: _ :: _ -> Error "settings belong to another sketch"
          | { S.node = S.Kw k; _ } :: v :: rest ->
              let* value = decode v in
              pairs ((k, value) :: acc) rest
          | _ -> Error "invalid settings entry" in
        pairs [] items
    | _ -> Error "a settings file is one (settings ...) form"

  let find settings name extract = Option.bind (List.assoc_opt name settings) extract
  let bool settings name = find settings name (function Bool v -> Some v | _ -> None)
  let float settings name = find settings name (function Float v -> Some v | _ -> None)
  let int settings name = find settings name (function Int v -> Some v | _ -> None)
end
