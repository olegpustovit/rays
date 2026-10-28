(* The scene's objects and the World's layers as plain data: each an instance of a kind (a typed
   schema and its descriptor), with its values and at most one input, an object's parent or the
   layer below.  No SOP node, no cook: lowering builds them from the text, the editor reads them. *)

module Kind = struct
  type t = { key : string; label : string; category : string list; defaults : Settings.t }

  let make ~key ~label ~category schema =
    { key; label; category; defaults = Settings.make schema (Param.default schema) }

  let fields kind = Settings.fields kind.defaults
end

type node = { id : int; kind : string; label : string; values : Settings.t; input : int option }

(* in the order they were lowered; ponytail: a list, scenes hold tens of objects, a map when they
   hold thousands *)
type t = node list

let empty = []

(* ids of their own: an object's id names it in the scene and its network in [Document.networks],
   never a node of a geometry network *)
let next_id = Atomic.make 1

let instance ?id ~label (kind : Kind.t) =
  { id = (match id with Some id -> id | None -> Atomic.fetch_and_add next_id 1);
    kind = kind.key; label; values = kind.defaults; input = None }

let fields node = Settings.fields node.values
let field node name = List.find_map (fun (f : Param.field_view) ->
  if f.name = name then Some f.current else None) (fields node)

let find graph id = List.find_opt (fun node -> node.id = id) graph
let ids kind graph = List.filter_map (fun node -> if node.kind = kind then Some node.id else None) graph
let add graph node = graph @ [ node ]
let replace graph node = List.map (fun n -> if n.id = node.id then node else n) graph
let update graph id f = List.map (fun n -> if n.id = id then f n else n) graph

let remove ids graph = List.filter_map (fun n ->
  if List.mem n.id ids then None
  else Some (match n.input with Some i when List.mem i ids -> { n with input = None } | _ -> n)) graph

let apply node changes =
  Result.map (fun (values, effects) -> { node with values }, effects) (Settings.apply node.values changes)

let set_fields graph id changes = match find graph id with
  | None -> Error (Printf.sprintf "no object #%d" id)
  | Some node -> Result.map (fun (node, effects) -> replace graph node, effects) (apply node changes)

(* the chain from the node no other consumes, down the inputs: a World's layers, top first *)
let stack graph =
  let consumed = List.filter_map (fun n -> n.input) graph in
  let rec down acc node =
    let acc = node :: acc in
    match Option.bind node.input (find graph) with
    | Some below when not (List.memq below acc) -> down acc below
    | _ -> acc in
  match List.find_opt (fun n -> not (List.mem n.id consumed)) graph with
  | Some top -> List.rev (down [] top)
  | None -> []

let same a b =
  List.equal (fun a b -> a.id = b.id && a.kind = b.kind && a.label = b.label && a.input = b.input
    && (a.values == b.values || List.map (fun (f : Param.field_view) -> f.name, f.current) (fields a)
                                = List.map (fun (f : Param.field_view) -> f.name, f.current) (fields b))) a b
