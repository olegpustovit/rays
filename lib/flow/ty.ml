type t =
  | Named of string
  | Float
  | Int
  | Bool
  | Vec2
  | Vec3
  | Vec4
  | Text
  | Color
  | List of t
  | Array of t
  | Record of (string * t) list
  | Fn of fn_signature option
  | Any

and fn_signature = { params : t list; result : t }

type color =
  [ `Geometry | `Float | `Int | `Bool | `Vec3 | `Text | `Fn | `Record | `Output | `Compound ]

type nominal = { name : string; shape : bool; color : color; default : Syntax.t option }

let geometry = Named "geometry"
and image = Named "image"
and drawing = Named "drawing"
and scene = Named "scene"
and world = Named "world"
and settings = Named "settings"
and panel = Named "panel"
and editor = Named "editor"
and material = Named "material"

let is_geometry ty = ty = geometry
let is_cooked ty = ty = geometry || ty = image
let nominal name ?(shape = true) ?(color = `Output) ?default () = { name; shape; color; default }

let descriptors =
  [
    nominal "geometry" ~color:`Geometry ~default:(Syntax.make (Sym "nil")) ();
    nominal "image"
      ~default:
        (Syntax.make
           (List
              [
                Syntax.make (Sym "image/noise");
                Syntax.make (Kw "width");
                Syntax.make (Num "1");
                Syntax.make (Kw "height");
                Syntax.make (Num "1");
              ]))
      ();
    nominal "drawing" ~default:(Syntax.make (List [ Syntax.make (Sym "draw/merge") ])) ();
    nominal "scene" ();
    nominal "world" ();
    nominal "settings" ~shape:false ();
    nominal "panel" ();
    nominal "editor" ();
    nominal "material" ~shape:false ();
    nominal "effect" ();
    nominal "sample" ~shape:false ();
  ]

let descriptor name = List.find_opt (fun entry -> entry.name = name) descriptors

let structural =
  [
    ("float", Float);
    ("int", Int);
    ("bool", Bool);
    ("vec2", Vec2);
    ("vec3", Vec3);
    ("vec4", Vec4);
    ("text", Text);
    ("color", Color);
    ("fn", Fn None);
    ("any", Any);
  ]

let names () = structural @ List.map (fun entry -> (entry.name, Named entry.name)) descriptors

let shape = function
  | Named name -> Option.fold ~none:true ~some:(fun entry -> entry.shape) (descriptor name)
  | _ -> false

let color = function
  | Named name -> Option.fold ~none:`Output ~some:(fun entry -> entry.color) (descriptor name)
  | _ -> `Output

let default = function
  | Named name -> Option.bind (descriptor name) (fun entry -> entry.default)
  | _ -> None

let rec to_string = function
  | Named name -> name
  | List e -> "list:" ^ to_string e
  | Array e -> "array:" ^ to_string e
  | Record fs ->
      "rec{" ^ String.concat "," (List.map (fun (n, t) -> n ^ ":" ^ to_string t) fs) ^ "}"
  | Fn (Some signature) ->
      "fn("
      ^ String.concat "," (List.map to_string signature.params)
      ^ ")->" ^ to_string signature.result
  | t -> fst (List.find (fun (_, u) -> u = t) (names ()))

let of_string s =
  let n = String.length s in
  let starts i p = i + String.length p <= n && String.sub s i (String.length p) = p in
  let ( let* ) = Option.bind in
  (* each parser returns the type and the next index *)
  let rec ty i =
    if starts i "list:" then
      let* e, j = ty (i + 5) in
      Some (List e, j)
    else if starts i "array:" then
      let* e, j = ty (i + 6) in
      if List.mem e [ Int; Float; Vec2; Vec3; Vec4; Text; Any ] then Some (Array e, j) else None
    else if starts i "rec{" then fields (i + 4) []
    else if starts i "fn(" then parameters (i + 3) []
    else
      let j = ref i in
      while !j < n && s.[!j] <> ',' && s.[!j] <> '}' && s.[!j] <> ')' do
        incr j
      done;
      let* t = List.assoc_opt (String.sub s i (!j - i)) (names ()) in
      Some (t, !j)
  and fields i acc =
    if i < n && s.[i] = '}' then Some (Record (List.rev acc), i + 1)
    else
      let* c = String.index_from_opt s i ':' in
      let* t, j = ty (c + 1) in
      let field = (String.sub s i (c - i), t) in
      fields (if j < n && s.[j] = ',' then j + 1 else j) (field :: acc)
  and parameters i acc =
    if starts i ")->" then
      let* result, j = ty (i + 3) in
      Some (Fn (Some { params = List.rev acc; result }), j)
    else
      let* parameter, j = ty i in
      if j < n && s.[j] = ',' && not (starts (j + 1) ")->") then
        parameters (j + 1) (parameter :: acc)
      else if starts j ")->" then parameters j (parameter :: acc)
      else None
  in
  match ty 0 with Some (t, j) when j = n -> Some t | _ -> None

let field_name = function
  | "" -> false
  | name ->
      (match name.[0] with 'a' .. 'z' -> true | _ -> false)
      && String.for_all (function 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true | _ -> false) name

let rec of_syntax (x : Syntax.t) =
  match x.node with
  | Sym name -> (
      match
        List.find_map (fun (n, t) -> if String.equal n name then Some t else None) (names ())
      with
      | Some (Any | Color) | None -> None
      | some -> some)
  | Map items ->
      let rec go seen = function
        | [] -> Some []
        | { Syntax.node = Kw name; _ } :: value :: rest
          when field_name name && not (List.mem name seen) ->
            Option.bind (of_syntax value) (fun t ->
                Option.map (fun fs -> (name, t) :: fs) (go (name :: seen) rest))
        | _ -> None
      in
      Option.map (fun fs -> Record fs) (go [] items)
  | List [ { node = Sym "list"; _ }; inner ] -> Option.map (fun t -> List t) (of_syntax inner)
  | List [ { node = Sym "fn"; _ }; { node = Vec params; _ }; result ] ->
      let rec types = function
        | [] -> Some []
        | p :: ps -> Option.bind (of_syntax p) (fun p -> Option.map (fun ps -> p :: ps) (types ps))
      in
      Option.bind (types params) (fun params ->
          Option.map (fun result -> Fn (Some { params; result })) (of_syntax result))
  | List [ { node = Sym "array"; _ }; inner ] -> (
      match of_syntax inner with
      | Some ((Int | Float | Vec2 | Vec3 | Vec4 | Text) as t) -> Some (Array t)
      | _ -> None)
  | _ -> None

let elem = function List e | Array e -> Some e | _ -> None

let rec has_fn = function
  | Fn _ -> true
  | List e | Array e -> has_fn e
  | Record fs -> List.exists (fun (_, t) -> has_fn t) fs
  | _ -> false

let num = function Int | Float -> true | _ -> false

let rec fits have want =
  have = Any || want = Any || have = want
  || (num have && (num want || want = Bool))
  || (have = Bool && num want)
  || (want = Color && (have = Text || have = Vec3 || have = List Float || have = List Int))
  || ((match want with Vec2 | Vec3 | Vec4 -> true | _ -> false) && num have)
  ||
  match (have, want) with
  | List h, List w -> fits h w
  | Array h, Array w -> h = w || h = Any || w = Any
  | Record hs, Record ws ->
      List.for_all
        (fun (n, w) ->
          match List.find_map (fun (m, h) -> if String.equal m n then Some h else None) hs with
          | Some h -> fits h w
          | None -> false)
        ws
  | Fn _, Fn None -> true
  | Fn (Some h), Fn (Some w) -> h.params = w.params && fits h.result w.result
  | _ -> false

let rec coerce have want =
  if want = Any || have = Any || have = want then have
  else
    match (have, want) with
    | List h, List w -> List (coerce h w)
    | Array h, Array w -> Array (if w = Any then h else w)
    | Record hs, Record ws ->
        Record
          (List.map
             (fun (n, h) ->
               match List.assoc_opt n ws with Some w -> (n, coerce h w) | None -> (n, h))
             hs)
    | _
      when (num have || have = Bool)
           && (num want || want = Bool || (List.mem want [ Vec2; Vec3; Vec4 ] && have <> Bool)) ->
        want
    | _ -> have

let rec join a b =
  if a = b then Some a
  else if a = Any then Some b
  else if b = Any then Some a
  else
    match (a, b) with
    | (Int | Float), (Int | Float) -> Some Float
    | List x, List y -> Option.map (fun t -> List t) (join x y)
    | Array x, Array y -> Option.map (fun t -> Array t) (join x y)
    | Record xs, Record ys
      when List.length xs = List.length ys && List.for_all (fun (n, _) -> List.mem_assoc n ys) xs ->
        let fields = List.map (fun (n, x) -> (n, join x (List.assoc n ys))) xs in
        if List.for_all (fun (_, t) -> t <> None) fields then
          Some (Record (List.map (fun (n, t) -> (n, Option.get t)) fields))
        else None
    | _ -> None

let unify a b =
  match join a b with
  | Some _ as joined -> joined
  | None -> if fits a b && fits b a then Some a else None

let rec numeric_record = function
  | Record fields ->
      fields <> []
      && List.for_all
           (fun (_, t) ->
             match t with
             | Float | Int | Bool | Vec2 | Vec3 | Vec4 -> true
             | Record _ -> numeric_record t
             | _ -> false)
           fields
  | _ -> false
