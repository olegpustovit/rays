(* Carry: what putting a payload at a place means.

   A payload is a Flow value held as text (kind "material" or "sop", value "(ref cobalt)").
   A place is a target when the edit that writes the value there passes the checker, so there
   is no list of what a widget accepts: [put] runs the edit (the one the inspector or the
   graph already writes) and the document's own check, and its result is the preview, the
   refusal's reason and the write.  See specification/flow.md, "Carry". *)
open Sop
open Editor_document

module W = Flow.Workspace
module S = Flow.Syntax
module E = Flow_graph.Flow_edit
module P = Flow_graph.Projection

type payload = Pxui.Ui.payload = { kind : string; value : string }

type place =
  | Node of W.path  (** a node of a graph pane, or of a text tile *)
  | Graph of string  (** the body of a graph: its empty canvas *)
  | Object of int  (** a geometry object of the scene *)
  | Surface of { object_ : int; material : string option }
      (** the surface of a geometry object a viewport shows under the pointer, with the
          material graph its primitive reads *)
  | Viewport of string
      (** a viewport panel under the pointer, before the host's pick says which surface *)
  | Text of { target : text_target; text : string; byte : int }
      (** a byte of the text pane's shown text, where [(ref name)] would be inserted *)

and text_target =
  | Whole  (** the Document tab: the whole workspace text *)
  | Graph_text of string  (** the Graph tab of this graph *)
  | Selection_text of W.path  (** the Selection tab: the closure of this binding *)
  | Drafted  (** the tab holds an unapplied draft, whose bytes are not the document's *)

(* what a put plans: ops over a document, or the whole text rewritten *)
type edit =
  | Edits of Document.t * E.op list * string
  | Rewrite of string * string

let ( let* ) = Result.bind

let name_of (expr : S.t) = match expr.node with
  | S.List [ { node = S.Sym "ref"; _ }; { node = S.Sym name; _ } ] -> Some name
  | _ -> None

(* the payload as an expression, and the graph it names *)
let parse payload =
  match S.parse payload.value with
  | Ok [ ({ node = S.Sym name; _ } as expr) ] when payload.kind = "camera" -> Ok (expr, name)
  | Ok [ expr ] when payload.kind <> "camera" -> (match name_of expr with
      | Some name -> Ok (expr, name)
      | None -> Error "A carried value is a graph reference")
  | Ok [ _ ] -> Error "A carried camera is the name of its object"
  | _ -> Error "A carried value is one form"

let find_graph (doc : Document.t) name =
  List.find_opt (fun (g : W.graph) -> g.name = name) (doc.workspace.ws).checked.graphs

(* the lowered SOP graph a geometry object instantiates, by its network *)
let graph_of_object (doc : Document.t) id =
  let lowered = doc.workspace.lowered in
  Option.bind (Document.Int_map.find_opt id doc.networks) (fun (n : Document.network) ->
    List.find_map (fun (g : Flow_sop.Lower.graph) ->
      if g.network == n.graph || (match g.root, Edit_graph.root n.graph.geometry with
        | Some a, Some b -> a = b | _ -> false)
      then Some g.name else None) lowered.graphs)

let geometry_objects (doc : Document.t) =
  let scene = Document.scene_graph doc in
  List.filter_map (fun id ->
    Option.map (fun node -> id, Node.label node) (Edit_graph.find scene ~node_id:id))
    (Objects.ids "geometry" scene)


let readers doc graph =
  List.length (List.filter (fun (id, _) -> graph_of_object doc id = Some graph) (geometry_objects doc))

let shared doc graph =
  match readers doc graph with n when n > 1 -> Printf.sprintf " · %d objects read it" n | _ -> ""

let scope_of ~catalog (doc : Document.t) graph =
  try Some (P.of_graph catalog (doc.workspace.ws).checked graph) with Invalid_argument _ -> None

(* a [sop/material] that has no group: it paints everything that reaches it *)
let whole_material (n : P.node) =
  n.head = "sop/material"
  && not (List.exists (fun (r : P.row) -> r.key = E.Kw "group" && r.expr <> None) n.rows)

let reads_material (n : P.node) graph =
  n.head = "sop/material"
  && List.exists (fun (r : P.row) -> r.key = E.Kw "material"
      && Option.bind r.expr name_of = Some graph) n.rows

(* A material put on a SOP graph as a whole: its result node when that is a material node, else
   a new one after it that becomes the result. *)
let material_on_graph ~catalog doc graph expr material =
  match find_graph doc graph with
  | None -> Error ("There is no graph " ^ graph)
  | Some g when g.context <> Flow.Context.sop -> Error (Printf.sprintf "A %s graph has no surface" (W.context_name g.context))
  | Some _ ->
      (match scope_of ~catalog doc graph with
       | None -> Error ("There is no graph " ^ graph)
       | Some scope ->
           (match scope.result with
            | Link name ->
                (match P.find scope [ graph; name ] with
                 | Some n when whole_material n ->
                     Ok ([ E.Set_arg { node = n.path; key = E.Kw "material"; sub = []; value = expr } ],
                         Printf.sprintf ":material (ref %s) in graph %s%s" material graph (shared doc graph))
                 | _ ->
                     let fresh = E.fresh_name (doc.workspace.ws).source ~root:graph "material" in
                     Ok ([ E.Add_node { scope = [ graph ]; name = fresh;
                             expr = S.make (S.List [ S.make (S.Sym "sop/material"); S.make (S.Sym name);
                                                     S.make (S.Kw "material"); expr ]) };
                           E.Connect { node = [ graph; "@result" ]; key = E.Whole; src = fresh; iter = false } ],
                         Printf.sprintf "a sop/material node and :material (ref %s) in graph %s%s" material graph
                           (shared doc graph)))
            | Node _ | Literal _ -> Error ("The result of " ^ graph ^ " is not a node a material can follow")))

(* [text] with [value] inserted at [byte], spaced from what it touches *)
let insert_at text byte value =
  let before = String.sub text 0 byte and after = String.sub text byte (String.length text - byte) in
  let opens c = String.contains " \n\t([{" c and closes c = String.contains " \n\t)]}" c in
  let lead = if before = "" || opens before.[String.length before - 1] then "" else " "
  and trail = if after = "" || closes after.[0] then "" else " " in
  before ^ lead ^ value ^ trail ^ after

(* ---- viewport panels: a scene graph or a camera put on one ---- *)

(* the viewport panels of the active layout, in tree order: key and tree path *)
let viewports (doc : Document.t) =
  match doc.shell with
  | None -> []
  | Some shell ->
      List.filter_map (function
        | path, Editor_core.Panels.View key -> Some (key, path) | _ -> None)
        (Editor_core.Panels.leaves shell.tree)

(* A scene graph on a viewport panel re-points the panel's [(ui/viewport (ref ...))]; a camera
   makes the [:camera] of the root of the scene the panel shows (a root is written first for a
   part).  Either way the panel is bound first when it is written in place. *)
let on_viewport ~factories ~catalog (doc : Document.t) payload expr name key =
  let ( let* ) = Result.bind in
  let* path = match List.assoc_opt key (viewports doc) with
    | Some path -> Ok path
    | None -> Error "That viewport is not part of the editor graph's layout" in
  let* bound, node = Editor_document.Document_edit.panel_node ~factories doc path in
  let source = (bound.workspace.ws).source in
  let shown = Option.bind (E.arg_text source node (E.Pos 0)) name_of in
  if payload.kind = "scene" then
    (match find_graph doc name with
     | Some { context; _ } when context = Flow.Context.scene && shown = Some name -> Error ("That viewport already shows " ^ name)
     | Some { context; _ } when context = Flow.Context.scene ->
         Ok (bound, [ E.Set_arg { node; key = E.Pos 0; sub = []; value = expr } ],
             Printf.sprintf "(ui/viewport (ref %s))" name)
     | Some g -> Error (Printf.sprintf "A %s graph is not a scene" (W.context_name g.context))
     | None -> Error ("There is no graph " ^ name))
  else
    let* scene = match shown with Some s -> Ok s | None -> Error "That viewport shows no scene graph" in
    let* scope = match scope_of ~catalog bound scene with
      | Some scope -> Ok scope | None -> Error ("There is no scene graph " ^ scene) in
    let on_root (n : P.node) =
      Ok (bound, [ E.Set_arg { node = n.path; key = E.Kw "camera"; sub = []; value = expr } ],
          Printf.sprintf ":camera %s on %s in graph %s" name (if n.synthetic then "its result" else n.name) scene) in
    (match scope.result with
     | Node path ->
         (match P.find scope path with
          | Some n when n.head = "scene/root" -> on_root n
          | _ -> Error (Printf.sprintf "%s returns a scene written in place: name it first, then put the camera" scene))
     | Link result ->
         (match P.find scope [ scene; result ] with
          | Some n when n.head = "scene/root" -> on_root n
          | _ ->
              let fresh = E.fresh_name source ~root:scene "root" in
              Ok (bound,
                  [ E.Add_node { scope = [ scene ]; name = fresh;
                      expr = S.make (S.List [ S.make (S.Sym "scene/root"); S.make (S.Sym result);
                                              S.make (S.Kw "camera"); expr ]) };
                    E.Connect { node = [ scene; "@result" ]; key = E.Whole; src = fresh; iter = false } ],
                  Printf.sprintf "a scene/root with :camera %s in graph %s" name scene))
     | Literal _ -> Error ("The result of " ^ scene ^ " is not a scene a root can follow"))

(* The edit of a payload at a place: the document it applies to (an object's home is bound
   first), the ops of one gesture, and the words of the text that say what they write. *)
let plan ~factories ~catalog (doc : Document.t) payload place =
  let* expr, name = parse payload in
  let material = payload.kind = "material" in
  let ok ?(on = doc) ops what = Ok (Edits (on, ops, what)) in
  let with_doc (ops, what) = ok ops what in
  let geometry_in_scene existing =
    ok (Editor_document.Scene_sync.add_geometry doc ~existing:(Some existing))
      (Printf.sprintf "a scene/geometry (ref %s) binding and a merge input" existing) in
  let object_graph id = match graph_of_object doc id with
    | Some graph -> Ok graph | None -> Error "That object has no SOP graph" in
  let label id = Option.value ~default:"object" (List.assoc_opt id (geometry_objects doc)) in
  let repoint id =
    let* current = object_graph id in
    if current = name then Error (Printf.sprintf "%s already shows %s" (label id) name)
    else match List.assoc_opt id doc.homes.objects with
      | None -> Error "That object is not in the text"
      | Some home ->
          let* bound, path = Editor_document.Scene_sync.bind_home ~factories doc home in
          ok ~on:bound [ E.Set_arg { node = path; key = E.Pos 0; sub = []; value = expr } ]
            (Printf.sprintf "%s (scene/geometry (ref %s))" (label id) name) in
  match place with
  | Viewport key when payload.kind = "scene" || payload.kind = "camera" ->
      Result.map (fun (on, ops, what) -> Edits (on, ops, what))
        (on_viewport ~factories ~catalog doc payload expr name key)
  | _ when payload.kind = "scene" || payload.kind = "camera" ->
      Error (Printf.sprintf "A %s goes on a viewport panel" payload.kind)
  | Viewport _ -> Error "Move over a surface"
  | Text { target; text; byte } ->
      let byte = max 0 (min byte (String.length text)) in
      let* () = if payload.kind = "camera" then Error "A camera is put on a viewport panel" else Ok () in
      let line = 1 + String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 (String.sub text 0 byte) in
      let what = Printf.sprintf "%s at line %d of the text" payload.value line in
      let inserted = insert_at text byte payload.value in
      let source = (doc.workspace.ws).source in
      let message (d : Diagnostic.t) = d.message in
      (match target with
       | Drafted -> Error "The text has an unapplied draft: apply or discard it first"
       | Whole -> Ok (Rewrite (inserted, what))
       | Graph_text graph ->
           let* op = Result.map_error message (Text_pane.graph_op source ~graph inserted) in
           ok [ op ] what
       | Selection_text path ->
           let* op = Result.map_error message
             (Text_pane.graph_op source ~graph:(List.hd path) ~selection:path inserted) in
           ok [ op ] what)
  | Graph graph ->
      (match find_graph doc graph with
       | Some g when g.context = Flow.Context.scene && not material -> geometry_in_scene name
       | Some g when g.context = Flow.Context.sop && material ->
           Result.bind (material_on_graph ~catalog doc graph expr name) with_doc
       | Some g -> Error (Printf.sprintf "A %s graph takes no %s" (W.context_name g.context) payload.kind)
       | None -> Error ("There is no graph " ^ graph))
  | Surface { object_; material = surface } when material ->
      let* graph = object_graph object_ in
      let on_node = Option.bind surface (fun reading ->
        Option.bind (scope_of ~catalog doc graph) (fun scope ->
          List.find_opt (fun n -> reads_material n reading) scope.nodes)) in
      (match on_node with
       | Some n ->
           ok [ E.Set_arg { node = n.path; key = E.Kw "material"; sub = []; value = expr } ]
             (Printf.sprintf ":material (ref %s) on %s in graph %s%s" name n.name graph (shared doc graph))
       | None -> Result.bind (material_on_graph ~catalog doc graph expr name) with_doc)
  | Object id when material ->
      let* graph = object_graph id in
      Result.bind (material_on_graph ~catalog doc graph expr name) with_doc
  | Surface { object_ = id; _ } | Object id -> repoint id
  | Node [] -> Error "Nothing is there"
  | Node (root :: _ as path) ->
      let node = Option.bind (scope_of ~catalog doc root) (fun scope -> P.find scope path) in
      let head = Option.fold ~none:"" ~some:(fun (n : P.node) -> n.head) node in
      let here = String.concat "/" path in
      (* a scene/geometry node: the SOP graph of the object it makes *)
      let shown = Option.bind (Option.bind node (fun (n : P.node) ->
        List.find_map (fun (r : P.row) -> if r.key = E.Pos 0 then r.expr else None) n.rows)) name_of in
      let assign key what = ok [ E.Set_arg { node = path; key; sub = []; value = expr } ] what in
      if material then
        (match head, shown with
         | "scene/geometry", Some graph ->
             Result.bind (material_on_graph ~catalog doc graph expr name) with_doc
         | _ -> assign (E.Kw "material") (Printf.sprintf ":material (ref %s) on %s" name here))
      else
        (match head, shown with
         | "scene/merge", _ -> geometry_in_scene name
         | "scene/geometry", Some current when current = name -> Error (here ^ " already shows " ^ name)
         | "scene/geometry", _ ->
             assign (E.Pos 0) (Printf.sprintf "%s (scene/geometry (ref %s))" here name)
         | _ -> assign (E.Pos 0) (Printf.sprintf "(ref %s) as the first input of %s" name here))

(* The put: the document with the payload at the place and what was written, or the checker's
   reason. *)
let put ~factories ~catalog doc payload place =
  let* edit = plan ~factories ~catalog doc payload place in
  match edit with
  | Edits (on, ops, what) ->
      let* edited = Editor_document.Document_edit.syntax_batch ~factories on ops in
      Ok (edited, what)
  | Rewrite (text, what) ->
      let* edited = Result.map_error (function
        | (d : Diagnostic.t) :: _ -> d.message | [] -> "The text does not check")
        (Editor_document.Document_edit.text_edit ~factories doc text) in
      Ok (edited, what)

(* The places a payload can be put, for the key route: the open graph, the scene, then each
   geometry object, those whose put passes the checker, with the words of their label. *)
let targets ~factories ~catalog ?(limit = 9) (doc : Document.t) payload ~graph =
  let candidates =
    if payload.kind = "scene" || payload.kind = "camera" then
      List.mapi (fun i (key, _) -> Viewport key, Printf.sprintf "viewport %d" (i + 1)) (viewports doc)
    else
    (match Option.bind graph (find_graph doc) with
     | Some { context; name; _ } when context = Flow.Context.sop && payload.kind = "material" -> [ Graph name, "this graph" ]
     | Some { context; name; _ } when context = Flow.Context.scene && payload.kind = "sop" -> [ Graph name, "the scene" ]
     | _ -> [])
    @ (match find_graph doc "scene" with
       | Some { context; _ } when context = Flow.Context.scene && payload.kind = "sop" && graph <> Some "scene" ->
           [ Graph "scene", "the scene" ]
       | _ -> [])
    @ List.map (fun (id, label) -> Object id, label) (geometry_objects doc) in
  let rec take n = function
    | [] -> []
    | _ when n = 0 -> []
    | (place, label) :: rest ->
        (match put ~factories ~catalog doc payload place with
         | Ok _ -> (place, label) :: take (n - 1) rest
         | Error _ -> take n rest) in
  take limit candidates
