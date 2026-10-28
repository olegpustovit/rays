module S = Flow.Syntax

type path = Flow.Workspace.path

module Path_map = Map.Make (struct
  type t = path

  let compare = compare
end)

module String_map = Map.Make (String)

type frame = { title : string; at : float * float; size : float * float }

type t = {
  editor : string option;
  panels : Editor_core.Panels.state Path_map.t;
  at : (float * float) Path_map.t;
  pinned : bool Path_map.t;
  level : Flow_graph.Projection.level Path_map.t;
      (* a node's detail level, when it is not the default card *)
  rows : bool String_map.t Path_map.t;
  collapsed : bool Path_map.t;
  frames : frame list Path_map.t;
}

let empty =
  {
    editor = None;
    panels = Path_map.empty;
    at = Path_map.empty;
    pinned = Path_map.empty;
    level = Path_map.empty;
    rows = Path_map.empty;
    collapsed = Path_map.empty;
    frames = Path_map.empty;
  }

let is_empty t = t = empty

let remap f t =
  let keys m =
    Path_map.fold
      (fun k v acc -> match f k with Some k -> Path_map.add k v acc | None -> acc)
      m Path_map.empty
  in
  {
    editor =
      Option.bind t.editor (fun name ->
          match f [ name ] with Some [ name ] -> Some name | _ -> None);
    panels = keys t.panels;
    at = keys t.at;
    pinned = keys t.pinned;
    level = keys t.level;
    rows = keys t.rows;
    collapsed = keys t.collapsed;
    frames = keys t.frames;
  }

(* ---- s-expression ---- *)

let mk node = S.make node
let str s = mk (S.Str s)
let kw s = mk (S.Kw s)
let sym s = mk (S.Sym s)
let vec l = mk (S.Vec l)
let bool b = sym (if b then "true" else "false")

let number x =
  (* positions are whole points far more often than not: keep them short *)
  mk
    (S.Num
       (if Float.is_integer x && Float.abs x < 1e15 then Printf.sprintf "%.0f" x
        else Flow.Lisp.float x))

let pair (x, y) = vec [ number x; number y ]
let path_form p = vec (List.map str p)

let to_syntax t =
  let nodes =
    Path_map.empty |> Path_map.fold (fun p v -> Path_map.add p [ (kw "at", pair v) ]) t.at
    |> fun m ->
    Path_map.fold
      (fun p v m ->
        Path_map.add p
          (Option.value ~default:[] (Path_map.find_opt p m)
          @ [ (kw "level", str (Flow_graph.Projection.level_name v)) ])
          m)
      t.level m
    |> fun m ->
    Path_map.fold
      (fun p v m ->
        Path_map.add p
          (Option.value ~default:[] (Path_map.find_opt p m) @ [ (kw "pinned", bool v) ])
          m)
      t.pinned m
    |> fun m ->
    Path_map.fold
      (fun p v m ->
        Path_map.add p
          (Option.value ~default:[] (Path_map.find_opt p m) @ [ (kw "collapsed", bool v) ])
          m)
      t.collapsed m
    |> fun m ->
    Path_map.fold
      (fun p rows m ->
        Path_map.add p
          (Option.value ~default:[] (Path_map.find_opt p m)
          @ [
              ( kw "rows",
                mk
                  (S.Map
                     (List.concat_map (fun (k, v) -> [ kw k; bool v ]) (String_map.bindings rows)))
              );
            ])
          m)
      t.rows m
  in
  let node_forms =
    List.map
      (fun (p, fields) ->
        mk (S.List (sym "node" :: path_form p :: List.concat_map (fun (k, v) -> [ k; v ]) fields)))
      (Path_map.bindings nodes)
  in
  let frames =
    List.concat_map
      (fun (p, fs) ->
        List.map
          (fun f ->
            mk
              (S.List
                 [
                   sym "frame"; path_form p; str f.title; kw "at"; pair f.at; kw "size"; pair f.size;
                 ]))
          fs)
      (Path_map.bindings t.frames)
  in
  let editor =
    Option.to_list (Option.map (fun name -> mk (S.List [ sym "editor"; str name ])) t.editor)
  in
  let panels =
    List.map
      (fun (path, (state : Editor_core.Panels.state)) ->
        mk
          (S.List
             ([ sym "panel"; path_form path; kw "collapsed"; bool state.collapsed ]
             @
             match state.window with
             | None -> []
             | Some (x, y, w, h) ->
                 [ kw "window"; vec (List.map (fun n -> number (float n)) [ x; y; w; h ]) ])))
      (Path_map.bindings t.panels)
  in
  mk (S.List ((sym "layout" :: editor) @ panels @ node_forms @ frames))

let ( let* ) = Result.bind
let fail fmt = Printf.ksprintf (fun m -> Error ("layout: " ^ m)) fmt

let read_number (f : S.t) =
  match f.node with
  | S.Num s -> (
      match float_of_string_opt s with
      | Some x when Float.is_finite x -> Ok x
      | _ -> fail "bad number %s" s)
  | _ -> fail "expected a number"

let read_pair (f : S.t) =
  match f.node with
  | S.Vec [ x; y ] ->
      let* x = read_number x in
      let* y = read_number y in
      Ok (x, y)
  | _ -> fail "expected [x y]"

let read_bool (f : S.t) =
  match f.node with
  | S.Sym "true" -> Ok true
  | S.Sym "false" -> Ok false
  | _ -> fail "expected true or false"

let read_str (f : S.t) = match f.node with S.Str s -> Ok s | _ -> fail "expected text"

let read_path (f : S.t) =
  match f.node with
  | S.Vec l ->
      List.fold_right
        (fun x acc ->
          let* acc = acc in
          let* s = read_str x in
          Ok (s :: acc))
        l (Ok [])
  | _ -> fail "expected a path vector"

let rec all f = function
  | [] -> Ok []
  | x :: r ->
      let* y = f x in
      let* r = all f r in
      Ok (y :: r)

let of_syntax (form : S.t) =
  match form.node with
  | S.List ({ S.node = S.Sym "layout"; _ } :: items) ->
      List.fold_left
        (fun acc (item : S.t) ->
          let* t = acc in
          match item.node with
          | S.List ({ S.node = S.Sym "panel"; _ } :: p :: fields) ->
              let* p = read_path p in
              let rec go (state : Editor_core.Panels.state) = function
                | [] -> Ok { t with panels = Path_map.add p state t.panels }
                | { S.node = S.Kw "collapsed"; _ } :: v :: rest ->
                    let* collapsed = read_bool v in
                    go { state with collapsed } rest
                | { S.node = S.Kw "window"; _ } :: { S.node = S.Vec values; _ } :: rest -> (
                    let* values = all read_number values in
                    match values with
                    | [ x; y; w; h ]
                      when w >= 120. && h >= 80.
                           && List.for_all (fun n -> abs_float n <= 1e6) values ->
                        go
                          {
                            state with
                            window =
                              Some (int_of_float x, int_of_float y, int_of_float w, int_of_float h);
                          }
                          rest
                    | _ -> fail "window needs [x y width height], width >= 120 and height >= 80")
                | _ -> fail "bad panel entry"
              in
              go Editor_core.Panels.default_state fields
          | S.List [ { S.node = S.Sym "editor"; _ }; name ] ->
              let* name = read_str name in
              if t.editor <> None then fail "duplicate editor entry"
              else Ok { t with editor = Some name }
          | S.List ({ S.node = S.Sym "node"; _ } :: p :: fields) ->
              let* p = read_path p in
              let rec go t = function
                | [] -> Ok t
                | { S.node = S.Kw "at"; _ } :: v :: r ->
                    let* v = read_pair v in
                    go { t with at = Path_map.add p v t.at } r
                | { S.node = S.Kw "level"; _ } :: v :: r -> (
                    let* name = read_str v in
                    match Flow_graph.Projection.level_of_name name with
                    | Some l -> go { t with level = Path_map.add p l t.level } r
                    | None -> fail "unknown level %s" name)
                | { S.node = S.Kw "pinned"; _ } :: v :: r ->
                    let* v = read_bool v in
                    go { t with pinned = Path_map.add p v t.pinned } r
                | { S.node = S.Kw "collapsed"; _ } :: v :: r ->
                    let* v = read_bool v in
                    go { t with collapsed = Path_map.add p v t.collapsed } r
                | { S.node = S.Kw "rows"; _ } :: { S.node = S.Map kvs; _ } :: r ->
                    let rec rows m = function
                      | { S.node = S.Kw k; _ } :: v :: r ->
                          let* v = read_bool v in
                          rows (String_map.add k v m) r
                      | [] -> Ok m
                      | _ -> fail "bad rows"
                    in
                    let* m = rows String_map.empty kvs in
                    go { t with rows = Path_map.add p m t.rows } r
                | _ -> fail "bad node entry"
              in
              go t fields
          (* entries of features that are gone: an older file still opens, and saves without them *)
          | S.List ({ S.node = S.Sym ("display" | "bend" | "wireless"); _ } :: _) -> Ok t
          | S.List
              [
                { S.node = S.Sym "frame"; _ };
                p;
                title;
                { S.node = S.Kw "at"; _ };
                at;
                { S.node = S.Kw "size"; _ };
                size;
              ] ->
              let* p = read_path p in
              let* title = read_str title in
              let* at = read_pair at in
              let* size = read_pair size in
              let existing = Option.value ~default:[] (Path_map.find_opt p t.frames) in
              Ok { t with frames = Path_map.add p (existing @ [ { title; at; size } ]) t.frames }
          | _ -> fail "unknown entry %s" (Flow.Lisp.flat item))
        (Ok empty) items
  | _ -> fail "expected (layout ...)"
