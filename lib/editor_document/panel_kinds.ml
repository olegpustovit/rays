(* The editor's panel kinds as the forms a layout gesture writes.  [Flow_graph.Flow_edit]
   rewrites the editor graph's layout and is given these forms: it knows no panel kind. *)
module S = Flow.Syntax

let sym name = S.make (S.Sym name)
let call head args = S.make (S.List (sym head :: args))

(* the first graph of the workspace [source] whose [:context] is [context] *)
let graph_of (source : S.t list) context =
  let rec has = function
    | { S.node = S.Kw "context"; _ } :: { S.node = S.Sym c; _ } :: _ -> c = context
    | _ :: rest -> has rest
    | [] -> false in
  match source with
  | { S.node = S.List ({ S.node = S.Sym "workspace"; _ } :: _ :: items); _ } :: _ ->
      List.find_map (fun (item : S.t) -> match item.node with
        | S.List ({ S.node = S.Sym "graph"; _ } :: { S.node = S.Sym n; _ } :: rest) when has rest -> Some n
        | _ -> None) items
  | _ -> None

let expr source kind = match kind with
  | "outline" | "graph" | "list" | "lisp" | "inspector" | "spreadsheet" | "timeline" -> Ok (call ("ui/" ^ kind) [])
  | "viewport" ->
      Option.to_result ~none:"A viewport needs a scene graph."
        (Option.map (fun n -> call "ui/viewport" [ call "ref" [ sym n ] ]) (graph_of source "scene"))
  | "canvas" ->
      Option.to_result ~none:"A canvas needs a draw graph."
        (Option.map (fun n -> call "ui/canvas" [ call "ref" [ sym n ] ]) (graph_of source "draw"))
  | _ when String.starts_with ~prefix:"graph:" kind ->
      Ok (call "ui/graph" [ S.make (S.Str (String.sub kind 6 (String.length kind - 6))) ])
  | _ -> Error (Printf.sprintf "Unknown panel type %s." kind)

let beside = function "lisp" -> "graph" | _ -> "lisp"
