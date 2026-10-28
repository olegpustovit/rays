(* Edits of the scene and the World, written to the workspace text.

   The text is the truth.  What the inspector, the handles, the scene list (reparent, rename,
   delete, hide), the World keys and the panels ask of an object the text has is written to its
   text ([set_fields], [write]: [Flow_edit] ops on the graph that declares it, an inline call
   unfolded into a binding first) and the text lowered again; the derived document is never edited
   in between.  Objects that only the host made (its camera and lights, a scene of one geometry
   object per [sop] graph, a World given by [?world]) have no text: those are edited as derived
   objects, and [reconcile] writes their kind as a graph on the first explicit edit, once.  It
   also writes the fields of a camera that follows the viewport. *)
open Sop
module S = Flow.Syntax
module F = Flow_graph.Flow_edit
module Edit = Edit_graph

exception Stop of string
let stop fmt = Printf.ksprintf (fun message -> raise (Stop message)) fmt

let mk node = S.make node
let sym name = mk (S.Sym name)

(* ---- values as text ---- *)

let number x = mk (S.Num (Flow.Lisp.float x))

let scalar : Param.value -> S.t = function
  | Float_value x -> number x
  | Int_value i -> mk (S.Num (string_of_int i))
  | Bool_value b -> sym (string_of_bool b)
  | Text_value t | Choice_value t -> mk (S.Str t)

let value_syntax (fields : Param.field_view list) = match fields with
  | [ field ] -> scalar field.current
  | fields -> mk (S.Vec (List.map (fun (field : Param.field_view) -> scalar field.current) fields))

let current fields name = List.find_map (fun (f : Param.field_view) ->
  if f.name = name then Some f.current else None) fields

(* the keywords a node's fields say, with [keep] deciding which fields are written *)
let ports node = match Flow_sop.Port.parameters ~infer:true (Node.parameter_fields node) with
  | Ok ports -> ports | Error _ -> []

let differing ~before node =
  let before = Node.parameter_fields before in
  List.filter_map (fun (p : Flow_sop.Port.parameter) ->
    if List.exists (fun (f : Param.field_view) -> current before f.name <> Some f.current) p.fields
    then Some (p.path, Some (value_syntax p.fields)) else None) (ports node)

(* a node's argument as text *)
let current_syntax node key =
  List.find_map (fun (p : Flow_sop.Port.parameter) ->
    if p.path = key then Some (value_syntax p.fields) else None) (ports node)

let non_default node =
  List.filter_map (fun (p : Flow_sop.Port.parameter) ->
    if List.exists (fun (f : Param.field_view) -> f.current <> f.default) p.fields
    then Some (p.path, value_syntax p.fields) else None) (ports node)

(* ---- applying ops to the text ---- *)

type state = { catalog : Flow.Check.catalog; mutable workspace : Workspace_doc.t;
               mutable unfolded : (Document.home * F.path) list;  (* homes already made bindings *)
               mutable adopted : bool  (* the host's objects are already written *) }

let apply st op = match Workspace_doc.edit st.catalog st.workspace op with
  | Ok workspace -> st.workspace <- workspace
  | Error (d : Diagnostic.t) -> stop "%s" d.message

(* The binding of a home; an inline call is unfolded into one first. *)
let rec bind st : Document.home -> F.path = function
  | Bound_at path -> path
  | Looped -> stop "Made by a macro; edit the text."
  | Copy { loop; rel; _ } -> bind st loop @ rel
  | Inline_in (parent, key) as home ->
      (match List.assoc_opt (Document.template home) st.unfolded with
       | Some path -> path
       | None ->
           let node = bind st parent in
           apply st (F.Unfold { node; key; sub = [] });
           (match F.arg_text st.workspace.source node key with
            | Some { S.node = S.Sym name; _ } ->
                let path = List.rev (name :: List.tl (List.rev node)) in
                st.unfolded <- (Document.template home, path) :: st.unfolded; path
            | _ -> stop "The call could not be named."))

(* An edit of a copy is an edit of the loop's template.  A literal argument takes it (every copy
   changes); so does a component of a vector whose other components the loop computes.  An argument
   or component the loop computes from its variable has no value of its own: writing a number would
   flatten every copy, so the edit is refused and says where the expression is.  [before] gives
   the copy's value before the edit, to find the components that changed. *)
let literal (e : S.t) = match e.node with
  | S.Num _ | S.Str _ | S.Sym ("true" | "false") -> true
  | S.Vec l -> List.for_all (fun (c : S.t) -> match c.node with S.Num _ -> true | _ -> false) l
  | _ -> false

let set ?(before = fun _ -> None) st home edits =
  if edits <> [] then begin
    let node = bind st home in
    let looped = Document.loop_of home <> None in
    List.iter (fun (key, value) ->
      let write ?(sub = []) value = apply st (F.Set_arg { node; key = F.Kw key; sub; value }) in
      match value, (if looped then F.arg_text st.workspace.source node (F.Kw key) else None) with
      | None, _ -> apply st (F.Disconnect { node; key = F.Kw key; fallback = None })
      | Some value, (None | Some _ as written) ->
          let computed e = stop "%s is computed by the loop: type =(expression) in its row (now %s)."
            key (Flow.Lisp.flat e) in
          (match written, value.S.node, Option.map (fun (b : S.t) -> b.node) (before key) with
           | None, _, _ -> write value
           | Some e, _, _ when literal e -> write value
           | Some { S.node = S.Vec template; _ }, S.Vec now, Some (S.Vec was)
             when List.length template = List.length now && List.length now = List.length was ->
               List.iteri (fun i (c : S.t) ->
                 if c.node <> (List.nth was i).node then begin
                   if literal (List.nth template i) then write ~sub:[ i ] c
                   else computed (List.nth template i)
                 end) now
           | Some e, _, _ -> computed e)) edits
  end

(* ---- loops: deleting copies ---- *)

(* The iteration tuple of a loop-made home: the running index of each enclosing loop, outermost
   first (what [Flow.Eval] calls the iteration, and what a [:skip] lists). *)
let rec tuple = function
  | Document.Copy { loop; index; _ } -> tuple loop @ [ index ]
  | Inline_in (home, _) -> tuple home
  | Bound_at _ | Looped -> []

(* The loops a home is a copy of, innermost first, each with the tuple of its iteration. *)
let rec levels = function
  | Document.Copy { loop; _ } as home -> (Document.template loop, tuple home) :: levels loop
  | Inline_in (home, _) -> levels home
  | Bound_at _ | Looped -> []

(* The [scene/merge] that holds a copy's object, and the position of the object among its
   arguments: a call written in it, or a binding of the loop body it names. *)
let holder source : Document.home -> (Document.home * int) option = function
  | Inline_in (parent, F.Pos p) -> Some (parent, p)
  | Copy { loop; rel = [ name ]; index } ->
      let positional args =
        let rec go = function
          | { S.node = S.Kw _; _ } :: _ :: rest -> go rest
          | x :: rest -> x :: go rest
          | [] -> [] in
        go args in
      let in_merge (e : S.t) = match e.node with
        | S.List ({ node = S.Sym "scene/merge"; _ } :: args) ->
            List.find_index (fun (a : S.t) -> a.node = S.Sym name) (positional args)
        | _ -> None in
      let rec last = function [] -> None | [ x ] -> Some x | _ :: r -> last r in
      Option.bind (Document.syntax_of source loop) (fun (e : S.t) -> match e.node with
        | S.List (_ :: rest) ->
            Option.bind (last rest) (fun (body : S.t) ->
              let bindings, result = match body.node with
                | S.List [ { S.node = S.Sym "let*"; _ }; { S.node = S.Vec bs; _ }; res ] ->
                    let rec pairs = function
                      | { S.node = S.Sym n; _ } :: v :: r -> (n, v) :: pairs r | _ :: _ :: r -> pairs r | _ -> [] in
                    pairs bs, res
                | _ -> [], body in
              let homes = List.map (fun (n, v) -> n, v) bindings @ [ "@result", result ] in
              List.find_map (fun (n, v) ->
                Option.map (fun p -> Document.Copy { loop; rel = [ n ]; index }, p) (in_merge v)) homes)
        | _ -> None)
  | _ -> None

(* The deleted objects of loops, [(id, home)], are left out of the text by skipping them: the
   iteration that made all of an iteration's objects (the outermost such one) is listed in the
   [:skip] of its loop; an object that shares its iteration with others that stay is left out
   of the [scene/merge] that holds it, at this iteration's tuple and its position.  Nothing else
   changes: the other copies keep their iteration tuples, so their ids, cache keys and
   provenance stay. *)
let delete_loops st (before : Document.t) gone =
  let source = (before.workspace.ws).source in
  let deleted id = List.mem_assoc id gone in
  let made (key, t) = List.filter (fun (_, h) ->
    List.exists (fun (k, u) -> k = key && u = t) (levels h)) before.homes.objects in
  let skips = ref [] in
  let add home entry =
    let path = bind st home in
    let known = Option.value ~default:[] (List.assoc_opt path !skips) in
    if not (List.mem entry known) then skips := (path, known @ [ entry ]) :: List.remove_assoc path !skips in
  List.iter (fun (_, home) ->
    let outward = List.rev (levels home) in
    match List.find_opt (fun level -> List.for_all (fun (i, _) -> deleted i) (made level)) outward with
    | Some (key, t) -> add key t
    | None ->
        (match holder source home, levels home with
         | Some (merge, p), (_, t) :: _ -> add (Document.template merge) (t @ [ p ])
         | _ ->
             stop "An object made by %s cannot be deleted alone here; edit the text." (Document.describe source home)))
    gone;
  List.iter (fun (path, entries) ->
    let old = match F.arg_text st.workspace.source path (F.Kw "skip") with
      | Some e -> Option.value ~default:[] (Flow.Workspace.skip_tuples e) | None -> [] in
    let all = List.sort_uniq compare (old @ entries) in
    apply st (F.Set_arg { node = path; key = F.Kw "skip"; sub = []; value = F.skip_value all })) !skips

(* ---- the scene ---- *)

let label graph id = Option.map Node.label (Edit.find graph ~node_id:id)

let object_edits ~before ~after id nb na =
  let parent graph = Option.bind (Objects.parent graph id) (label graph) in
  let name = if Node.label nb <> Node.label na then [ "name", Some (mk (S.Str (Node.label na))) ] else [] in
  let under = if parent before = parent after then []
    else [ "parent", Option.map (fun p -> mk (S.Str p)) (parent after) ] in
  name @ under @ differing ~before:nb na

let homed (doc : Document.t) id = match List.assoc_opt id doc.homes.objects with
  | Some Document.Looped -> Contexts.graph_of (doc.workspace.ws) Flow.Context.scene <> None
  | Some _ -> true
  | None -> false

let unique graph id =
  match Option.bind (label graph id) (fun l ->
      List.find_opt (fun (i : Edit.node_info) -> i.id <> id && i.label = l && i.operation <> "world")
        (Edit.inspect graph)) with
  | Some _ -> stop "Two objects are named %S. Rename one of them first." (Option.value ~default:"" (label graph id))
  | None -> ()

(* the graphs a deleted object leaves behind, when no other [(ref g)] reads them *)
let rec refs name (e : S.t) = match e.node with
  | S.List ({ node = S.Sym "ref"; _ } :: { node = S.Sym n; _ } :: _) when n = name -> 1
  | S.List ({ node = S.Sym "ui/graph"; _ } :: { node = S.Str n; _ } :: _) when n = name -> 1  (* a panel shows it *)
  | _ -> List.fold_left (fun n c -> n + refs name c) 0 (S.children e)

(* the graph that makes a scene object: the [(ref g)] its call holds, for a geometry's SOP graph or
   a World's layer graph *)
let source_graph_of (doc : Document.t) id =
  Option.bind (List.assoc_opt id doc.homes.objects) (fun home ->
    Option.bind (Document.syntax_of (doc.workspace.ws).source home) (fun (e : S.t) ->
      match e.node with
      | S.List ({ node = S.Sym ("scene/geometry" | "scene/world"); _ } :: args) ->
          List.find_map (fun (a : S.t) -> match a.node with
            | S.List [ { node = S.Sym "ref"; _ }; { node = S.Sym g; _ } ] -> Some g | _ -> None) args
      | _ -> None))

(* an object a viewport over another scene instance draws is derived: its text is that reference *)
let refuse_preview (doc : Document.t) id =
  match Option.bind doc.shell (fun shell -> List.find_map (fun (key, ids) ->
    if List.mem id ids then Some key else None) shell.views) with
  | Some key -> stop "Preview [%s] is derived; edit its viewport scene reference or source graph." key
  | None -> ()

(* The deleted objects [(id, home)] leave the text: an inline call leaves its merge (the last one
   first, so positions hold), then a binding goes whole. *)
let delete_objects st (before : Document.t) gone =
  let looped, rest = List.partition (fun (_, h) -> Document.loop_of h <> None) gone in
  delete_loops st before looped;
  let bound, inline = List.partition (function Document.Bound_at _ -> true | _ -> false) (List.map snd rest) in
  let position = function Document.Inline_in (_, F.Pos i) -> i | _ -> -1 in
  List.iter (function
    | Document.Inline_in (parent, (F.Pos _ as key)) ->
        apply st (F.Disconnect { node = bind st parent; key; fallback = None })
    | home -> stop "Made by %s; edit the text." (Document.describe (before.workspace.ws).source home))
    (List.sort (fun x y -> compare (position y) (position x)) inline);
  List.iter (function
    | Document.Bound_at path -> apply st (F.Delete_nodes { nodes = [ path ] })
    | _ -> ()) bound;
  (* an object's graph goes with it unless another object or panel still references it *)
  let orphans = List.sort_uniq compare (List.filter_map (fun (id, _) -> source_graph_of before id) gone) in
  List.iter (fun name ->
    if List.for_all (fun e -> refs name e = 0) st.workspace.source
    && List.exists (fun (g : Flow.Workspace.graph) -> g.name = name) st.workspace.checked.graphs
    then apply st (F.Remove_graph { name })) orphans

(* The render camera in the text: the root's [:camera] when the scene has a root, else [:active]
   on the camera.  [alive] says whether the camera it replaces is still an object. *)
let set_camera st (before : Document.t) ~alive camera =
  let home id = List.assoc_opt id before.homes.objects in
  match before.homes.root with
  | Some root ->
      (* the root names the active camera; an old [:active] on the camera it replaces goes *)
      let path id = Option.map (bind st) (home id) in
      (match Option.bind before.active_camera (fun id -> if alive id then path id else None) with
       | Some p when F.arg_text st.workspace.source p (F.Kw "active") <> None ->
           apply st (F.Disconnect { node = p; key = F.Kw "active"; fallback = None })
       | _ -> ());
      (match Option.bind camera path with
       | Some p -> set st root [ "camera", Some (sym (List.nth p (List.length p - 1))) ]
       | None -> set st root [ "camera", None ])
  | None ->
      (match Option.bind before.active_camera home with
       | Some home when Option.fold ~none:false ~some:alive before.active_camera -> set st home [ "active", None ]
       | _ -> ());
      Option.iter (fun home -> set st home [ "active", Some (sym "true") ]) (Option.bind camera home)

(* The objects of the text as the scene [a] has them: a changed one's name, parent and fields are
   written. *)
let object_changes st (before : Document.t) a =
  let b = Document.scene_graph before in
  let refuse_preview = refuse_preview before in
  List.iter (fun (id, home) -> if homed before id then
    match Edit.find b ~node_id:id, Edit.find a ~node_id:id with
    | Some nb, Some na ->
        let edits = object_edits ~before:b ~after:a id nb na in
        if edits <> [] then refuse_preview id;
        if List.mem_assoc "parent" edits then Option.iter (unique a) (Objects.parent a id);
        (* a name is what [:parent] reads: one two objects share would move the other's children *)
        if List.mem_assoc "name" edits then unique a id;
        set ~before:(current_syntax nb) st home edits
    | _ -> ()) before.homes.objects

(* ---- the World ---- *)

let world_id (doc : Document.t) = List.nth_opt (Objects.ids "world" (Document.scene_graph doc)) 0

(* layer ids from the top down *)
let order (network : Document.network) =
  let graph = network.graph.geometry in
  let infos = Edit.inspect graph in
  let consumed = List.concat_map (fun (i : Edit.node_info) ->
    List.filter_map Fun.id (Array.to_list i.inputs)) infos in
  let rec down id acc =
    let acc = id :: acc in
    match Edit.inputs graph ~node_id:id with
    | Some inputs when Array.length inputs > 0 && inputs.(0) <> None
        && not (List.mem (Option.get inputs.(0)) acc) -> down (Option.get inputs.(0)) acc
    | _ -> acc in
  match List.filter (fun (i : Edit.node_info) -> not (List.mem i.id consumed)) infos with
  | top :: _ -> List.rev (down top.id [])
  | [] -> []

(* ---- objects the text does not have ---- *)

(* the names a workspace's graphs, functions and macros already own *)
let owned (workspace : Workspace_doc.t) =
  let c = workspace.checked in
  List.map (fun (g : Flow.Workspace.graph) -> g.name) (c.graphs @ c.defs)
  @ List.filter_map (fun (m : S.t) -> match m.node with
      | S.List [ _; { S.node = S.Sym n; _ }; _; _ ] | S.List ({ S.node = S.Sym _; _ } :: { S.node = S.Sym n; _ } :: _) -> Some n
      | _ -> None) c.macros

(* [(kind slots... :name "label" :field value ...)] for a derived node; only what differs from the
   kind's defaults is written. *)
let call_of ~kind ~label ~default_label ?(slots = []) ?(extra = []) node =
  let name = if label = default_label then [] else [ mk (S.Kw "name"); mk (S.Str label) ] in
  mk (S.List (sym kind :: slots @ name @ extra
    @ List.concat_map (fun (key, value) -> [ mk (S.Kw key); value ]) (non_default node)))

let graph_form name context body =
  mk (S.List [ sym "graph"; sym name; mk (S.Kw "context"); sym context; body ])

let let_star bindings result =
  mk (S.List [ sym "let*"; mk (S.Vec (List.concat_map (fun (n, e) -> [ sym n; e ]) bindings)); result ])

let default_label node = String.lowercase_ascii (Node.operation node)

let object_binding (doc : Document.t) used id (info : Edit.node_info) =
  let graph = Document.scene_graph doc in
  let parent = Objects.parent graph id in
  let slots = if info.operation = "geometry" then
      (match Document.object_graph doc id with
       | Some g -> [ mk (S.List [ sym "ref"; sym g ]) ]
       | None -> stop "%s has no graph of its own to place." info.label)
    else [] in
  let parent = match Option.bind parent (label graph) with
    | Some p -> [ mk (S.Kw "parent"); mk (S.Str p) ] | None -> [] in
  let active = if doc.active_camera = Some id && info.operation = "camera"
    then [ mk (S.Kw "active"); sym "true" ] else [] in
  let default_label = match info.operation, slots with
    | "geometry", [ { S.node = S.List [ _; { S.node = S.Sym g; _ } ]; _ } ] -> g
    | _ -> default_label info.node in
  F.fresh_among used info.label,
  call_of ~kind:("scene/" ^ info.operation) ~label:info.label ~default_label ~slots
    ~extra:(parent @ active) info.node

(* the text of every object no binding holds, into the scene graph (made when there is none,
   even with nothing to write: the graph is what says "no camera, no lights").  The scene graph
   is authoritative, so all of the host's objects are written together. *)
let adopt_objects st (doc : Document.t) =
  if not st.adopted then begin
  st.adopted <- true;
  let graph = Document.scene_graph doc in
  let unhomed = List.filter (fun (i : Edit.node_info) ->
    i.operation <> "world" && not (homed doc i.id)) (Edit.inspect graph) in
  if unhomed = [] && Contexts.graph_of st.workspace Flow.Context.scene = None then
    apply st (F.Set_graph { name = "scene"; form = graph_form "scene" "scene" (mk (S.List [ sym "scene/merge" ])) });
  if unhomed <> [] then begin
    let used = ref (owned st.workspace) in
    let bindings = List.map (fun (i : Edit.node_info) -> object_binding doc used i.id i) unhomed in
    match Contexts.graph_of st.workspace Flow.Context.scene with
    | None ->
        let names = List.map (fun (n, _) -> sym n) bindings in
        let body = let_star bindings (mk (S.List (sym "scene/merge" :: names))) in
        apply st (F.Set_graph { name = "scene"; form = graph_form "scene" "scene" body })
    | Some g ->
        (* into a graph the text has: each name is free of what the graph binds by then *)
        List.iter2 (fun (_, expr) (i : Edit.node_info) ->
          apply st (F.Add_node { scope = [ g.name ]; expr;
                                 name = F.fresh_name st.workspace.source ~root:g.name i.label })) bindings unhomed
  end
  end

let none_world = mk (S.List [ sym "world/none" ])

(* the layers of a World as a world graph named [name]: bound bottom first, the top layer the
   result; [(world/none)] without layers *)
let write_stack st g bottom_first name =
  let used = ref (owned st.workspace) in
  let bindings = List.map (fun id ->
    let info = List.find (fun (i : Edit.node_info) -> i.id = id) (Edit.inspect g) in
    id, F.fresh_among used info.label, info) bottom_first in
  let layers = List.mapi (fun i (_, name, (info : Edit.node_info)) ->
    let below = if i = 0 then [] else [ sym (let _, n, _ = List.nth bindings (i - 1) in n) ] in
    name, call_of ~kind:("world/" ^ info.operation) ~label:info.label
      ~default_label:(Edit.factory_label (List.assoc ("world/" ^ info.operation) Kinds.world_kinds))
      ~slots:below info.node) bindings in
  let body = match List.rev layers with
    | [] -> none_world
    | (top, _) :: _ -> let_star layers (sym top) in
  apply st (F.Set_graph { name; form = graph_form name "world" body })

(* The layers of the World [wid] the text has, with [removed] gone and the others stacked as [stack]
   says (top down): a deleted or moved layer is one edit of its binding, so the graph's inputs,
   expressions, comments and names stay.  [graph] has the layers as they are to be written when the
   stack has to be written whole. *)
let stack_layers st (before : Document.t) wid ~graph ~removed ~stack =
  let member = List.mem_assoc wid before.homes.objects in
  let known = List.map fst before.homes.layers in
  let was = List.filter (fun id -> List.mem id stack) (order (Document.Int_map.find wid before.networks)) in
  let reordered = was <> stack in
  (* a World member whose top layer is its graph's result written in place has no name to
     delete or rewire by *)
  let unnamed = member && List.exists (fun (_, home) -> match home with
    | Document.Bound_at path -> List.nth_opt (List.rev path) 0 = Some "@result" | _ -> false)
    before.homes.layers in
  if (removed <> [] || reordered) && unnamed then
    (match before.homes.world_graph with
     | Some name -> write_stack st graph (List.rev stack) name
     | None -> stop "The World's graph is not named; edit the text.")
  else if removed <> [] || reordered then begin
    (* every layer is a binding of its own before the stack is rewired *)
    let path id = bind st (List.assoc id before.homes.layers) in
    let paths = List.map (fun id -> id, path id) known in
    List.iter (fun id -> apply st (F.Delete_nodes { nodes = [ List.assoc id paths ] })) removed;
    if reordered then begin
      let bottom_first = List.rev stack in
      let name id = List.nth (List.assoc id paths) (List.length (List.assoc id paths) - 1) in
      ignore (List.fold_left (fun below id ->
        (match below with
         | Some b -> apply st (F.Connect { node = List.assoc id paths; key = F.Pos 0; src = name b; iter = false })
         | None -> apply st (F.Disconnect { node = List.assoc id paths; key = F.Pos 0; fallback = None }));
        Some id) None bottom_first);
      match List.rev bottom_first, before.homes.world with
      | top :: _, _ when member ->
          (* a world graph's result is its top layer *)
          (match before.homes.world_graph with
           | Some graph -> apply st (F.Connect { node = [ graph; "@result" ]; key = F.Whole; src = name top; iter = false })
           | None -> stop "The World's graph is not named; edit the text.")
      | top :: _, Some home ->
          apply st (F.Connect { node = bind st home; key = F.Pos 0; src = name top; iter = false })
      | _ -> ()
    end
  end

(* ---- the root ---- *)

(* the render settings that changed, as keywords *)
let root_edits ~before ~after =
  let was = Objects.Root.fields before in
  List.filter_map (fun (f : Param.field_view) ->
    if current was f.name <> Some f.current then Some (f.name, Some (scalar f.current)) else None)
    (Objects.Root.fields after)

(* a scene with no [scene/root] writes one over its result on the first edit of a render setting
   (only what differs from the defaults), the scene graph made first when there is none *)
let adopt_root st (doc : Document.t) root =
  if Contexts.graph_of st.workspace Flow.Context.scene = None then adopt_objects st doc;
  let scene = (Option.get (Contexts.graph_of st.workspace Flow.Context.scene)).name in
  let changed = root_edits ~before:Objects.Root.default ~after:root in
  let expr = mk (S.List (sym "scene/root" :: List.concat_map (fun (key, value) ->
    [ mk (S.Kw key); Option.get value ]) changed)) in
  apply st (F.Add_node { scope = [ scene ]; name = F.fresh_name st.workspace.source ~root:scene "root"; expr })

let root st (before : Document.t) now =
  if before.root != now && before.root <> now then
    match before.homes.root with
    | Some home -> set st home (root_edits ~before:before.root ~after:now)
    | None -> adopt_root st before now

(* ---- settings ---- *)

let settings st (before : Document.t) now =
  match before.homes.settings with
  | Some home when before.settings != now ->
      let changed = List.filter_map (fun (f : Param.field_view) ->
        if current (Settings.fields before.settings) f.name <> Some f.current
        then Some (f.name, Some (scalar f.current)) else None) (Settings.fields now) in
      set st home changed
  | None when before.settings != now ->
      st.workspace <- { st.workspace with settings = now }
  | _ -> ()

let unhomed_changes (before : Document.t) (after : Document.t) =
  let b = Document.scene_graph before and a = Document.scene_graph after in
  List.exists (fun (i : Edit.node_info) ->
    i.operation <> "world" && not (homed before i.id)
    && (match Edit.find b ~node_id:i.id with
        | Some nb -> object_edits ~before:b ~after:a i.id nb i.node <> []
                     || Objects.parent b i.id <> Objects.parent a i.id
        | None -> false)) (Edit.inspect a)
  || List.exists (fun (i : Edit.node_info) ->
       i.operation <> "world" && not (homed before i.id) && Edit.find a ~node_id:i.id = None) (Edit.inspect b)

(* ---- one object's fields, text first ---- *)

(* Where the text of a scene object (at the scene level) or of a World layer (inside the World) is,
   when it has one an edit can be written to; none for an object only the host made. *)
let field_home (doc : Document.t) level node = match level with
  | Document.Scene ->
      (match List.assoc_opt node doc.homes.objects with
       | Some home when homed doc node -> Some home
       | Some _ -> None
       | None -> if world_id doc = Some node then doc.homes.world else None)
  | Inside id when world_id doc = Some id -> List.assoc_opt node doc.homes.layers
  | Inside _ -> None

let in_text doc level node = field_home doc level node <> None

(* ---- derived edits, text first ---- *)

(* The status line for an edit of a loop's copies (they are one template: every copy changes),
   given the home the edit was written to. *)
let template_note (doc : Document.t) home =
  Option.map (fun loop -> Printf.sprintf "Edited the loop template (%s); %d copies change."
                (Document.describe (doc.workspace.ws).source loop) (Document.copies doc home)) (Document.loop_of home)

let first notes = List.find_map Fun.id notes

(* What a list, a key or a panel asks of the derived scene or World, written to the text with no
   edit of the derived document in between.  Ids are scene objects at the scene level and World
   layers inside the World. *)
type edit =
  | Fields of (int * (string * Param.value) list) list  (* several objects' fields: a stroke of flags *)
  | Rename of int * string
  | Delete of int list
  | Restack of int list  (* the World's layers, bottom first *)
  | Layers of Document.network * (string * Param.value) list  (* a preset: the layers and the World's fields *)
  | Camera of int option  (* the render camera *)
  | Root of Objects.Root.parameters  (* the render settings *)
  | Settings of Settings.t
  | Reparent of int list * int option  (* objects under a parent (none: the root), their places kept *)
  | Outdent of int list  (* each out to its parent's parent *)

let graph_at (doc : Document.t) = function
  | Document.Scene -> Some (Document.scene_graph doc)
  | Inside id -> Option.map (fun (n : Document.network) -> n.graph.geometry)
      (Document.Int_map.find_opt id doc.networks)

(* every object the edit names has text of its own; else the caller edits the derived document and
   {!reconcile} adopts it *)
let writes (doc : Document.t) level = function
  | Fields edits -> List.for_all (fun (node, _) -> in_text doc level node) edits
  | Rename (node, _) -> in_text doc level node
  | Delete ids | Restack ids -> List.for_all (in_text doc level) ids
  | Camera _ -> true  (* a camera the text does not have is the document's alone *)
  | Root _ | Settings _ -> true  (* the first edit of a render setting writes the root *)
  | Reparent (ids, _) | Outdent ids -> level = Document.Scene && List.for_all (in_text doc level) ids
  | Layers _ -> (match level with
    | Inside wid -> world_id doc = Some wid && doc.homes.world <> None
    | Scene -> false)

let node_at doc level id = match Option.bind (graph_at doc level) (fun g -> Edit.find g ~node_id:id) with
  | Some node -> node | None -> stop "That object is gone."

let write_edit st (doc : Document.t) level = function
  | Fields edits ->
      List.map (fun (node, values) ->
        let was = node_at doc level node in
        match field_home doc level node, Node.apply_parameters was values with
        | _, Error message -> stop "%s" message
        | None, _ -> stop "That object is not in the text."
        | Some home, Ok (now, _) ->
            if level = Document.Scene then refuse_preview doc node;
            set ~before:(current_syntax was) st home (differing ~before:was now);
            template_note doc home) edits |> first
  | Rename (id, name) when Node.label (node_at doc level id) = name -> None
  | Rename (id, name) ->
      let value = Some (mk (S.Str name)) in
      if level = Document.Scene && List.mem_assoc id doc.homes.objects then begin
        (* a name is what [:parent] reads: the children's follow, and one two objects share would
           move the other's children *)
        let graph = Document.scene_graph doc in
        if List.exists (fun (i : Edit.node_info) -> i.id <> id && i.label = name && i.operation <> "world")
            (Edit.inspect graph)
        then stop "Two objects are named %S. Rename one of them first." name;
        List.iter (fun (o, home) ->
          let key = if o = id then Some "name" else if Objects.parent graph o = Some id then Some "parent" else None in
          match key with
          | Some key when homed doc o -> refuse_preview doc o; set st home [ key, value ]
          | _ -> ()) doc.homes.objects
      end
      else Option.iter (fun home -> set st home [ "name", value ]) (field_home doc level id);
      Option.bind (field_home doc level id) (template_note doc)
  | Delete ids when level = Document.Scene ->
      let graph = Document.scene_graph doc in
      let gone = List.rev (List.filter (fun (id, _) -> List.mem id ids && homed doc id) doc.homes.objects) in
      List.iter (fun (o, home) ->
        if List.mem_assoc o gone then refuse_preview doc o
        else if homed doc o && Option.fold ~none:false ~some:(fun p -> List.mem p ids) (Objects.parent graph o)
        then begin refuse_preview doc o; set st home [ "parent", None ] end) doc.homes.objects;
      delete_objects st doc gone;
      (match doc.homes.root, doc.active_camera with
       | Some root, Some camera when List.mem camera ids -> set st root [ "camera", None ]
       | _ -> ());
      List.find_map (fun (_, home) -> Option.map (fun loop ->
        Printf.sprintf "Removed a copy from the loop (%s)." (Document.describe (doc.workspace.ws).source loop))
        (Document.loop_of home)) (List.rev gone)
  | Layers (network, values) ->
      (match level with
       | Inside wid ->
           let was = node_at doc Document.Scene wid in
           let now = match Node.apply_parameters was values with
             | Ok (now, _) -> now | Error message -> stop "%s" message in
           let edits = differing ~before:was now in
           if edits <> [] then refuse_preview doc wid;
           Option.iter (fun home -> set ~before:(current_syntax was) st home edits) (field_home doc Document.Scene wid);
           (match doc.homes.world_graph with
             | Some name -> write_stack st network.graph.geometry (List.rev (order network)) name
             | None -> stop "The World's graph is not named; edit the text.")
       | Scene -> ());
      None
  | Camera camera ->
      if camera <> doc.active_camera then set_camera st doc ~alive:(fun _ -> true) camera;
      None
  | Reparent (ids, _) | Outdent ids as edit ->
      (* the scene rewired aside, for the transform that keeps each object where it is: only the
         text is written *)
      let graph = Document.scene_graph doc in
      let moved = match edit with
        | Reparent (ids, parent) -> Objects.reparent graph ids parent
        | _ -> Objects.outdent graph ids in
      object_changes st doc moved;
      first (List.map (fun id -> Option.bind (List.assoc_opt id doc.homes.objects) (template_note doc)) ids)
  | Root now -> root st doc now; None
  | Settings now -> settings st doc now; None
  | Restack bottom_first ->
      (match level, world_id doc with
       | Inside wid, Some w when w = wid ->
           stack_layers st doc wid ~graph:(Document.Int_map.find wid doc.networks).graph.geometry ~removed:[]
             ~stack:(List.rev bottom_first)
       | _ -> ());
      None
  | Delete ids ->
      (match level, world_id doc with
       | Inside wid, Some w when w = wid ->
           let network = Document.Int_map.find wid doc.networks in
           let removed = List.filter (fun id -> List.mem id ids) (List.map fst doc.homes.layers) in
           stack_layers st doc wid ~graph:network.graph.geometry ~removed
             ~stack:(List.filter (fun id -> not (List.mem id ids)) (order network))
       | _ -> ());
      None

(* [None] when {!writes} says the edit is not the text's to take. *)
let write ~factories (doc : Document.t) level edit =
  if not (writes doc level edit) then Ok None else
  let ( let* ) = Result.bind in
  let* catalog = Result.map_error Diagnostic.to_string
      (Lazy.force doc.workspace.catalog) in
  let st = { catalog; workspace = doc.workspace.ws; unfolded = []; adopted = false } in
  try
    let note = write_edit st doc level edit in
    (* what the text does not say is the document's; what it says is lowered over this *)
    let doc = match edit with
      | Camera active_camera -> { doc with active_camera }
      | Root root -> { doc with root }
      | Settings settings -> { doc with settings }
      | Delete ids ->
          (* the lowering finds an object of the text by its home, else by kind and label: the
             deleted ones are not there to be taken for the copies of a loop that stay *)
          let without (graph : Flow_sop.Network.t) = match Flow_sop.Network.remove_nodes ids graph with
            | Ok graph -> Result.value ~default:graph
                (Flow_sop.Network.with_geometry (Edit.remove_nodes ids graph.geometry) graph)
            | Error _ -> graph in
          (match Document.network doc level with
           | Some network -> Document.prune (Document.with_network doc level { network with graph = without network.graph })
           | None -> doc)
      | Fields _ | Rename _ | Restack _ | Layers _ | Reparent _ | Outdent _ -> doc in
    if st.workspace == doc.workspace.ws then Ok (Some (doc, note))
    else if st.workspace.source == (doc.workspace.ws).source then
      Ok (Some ({ doc with workspace = { doc.workspace with ws = st.workspace } }, note))
    else Result.map (fun doc -> Some (doc, note)) (Contexts.of_workspace ~factories ~previous:doc st.workspace
                                                   |> Result.map_error Diagnostic.to_string)
  with Stop message -> Error message

(* Field values of one object (an inspector row, a viewport handle) written to its text and lowered
   again: the node only says what the values are once its ranges have had their say, and which
   effects they have.  [None]: the object has no text (the caller edits the derived object and
   {!reconcile} adopts it). *)
let set_fields ~factories (doc : Document.t) level ~node values =
  match field_home doc level node, Option.bind (graph_at doc level) (fun g -> Edit.find g ~node_id:node) with
  | Some home, Some was ->
      Result.bind (Node.apply_parameters was values) (fun (_, effects) ->
        Result.map (Option.map (fun (doc, _) -> doc, effects, home))
          (write ~factories doc level (Fields [ node, values ])))
  | _ -> Ok None

(* [after] is [before] with objects the host made edited (or a camera moved that follows the
   viewport); the result is [after] with the edit written to its text and lowered again.  [adopt]: an
   explicit edit of an object the text lacks writes it (a follow-the-viewport camera move is
   not one, so it stays the host's). *)
let run ~factories ~adopt (before : Document.t) (after : Document.t) =
  if before.scene == after.scene && before.networks == after.networks then Ok after
  else
    let ( let* ) = Result.bind in
    let* catalog = Result.map_error Diagnostic.to_string
        (Lazy.force before.workspace.catalog) in
    let st = { catalog; workspace = before.workspace.ws; unfolded = []; adopted = false } in
    try
      object_changes st before (Document.scene_graph after);
      if adopt && unhomed_changes before after then adopt_objects st after;
      if st.workspace == before.workspace.ws then Ok after
      else Contexts.of_workspace ~factories ~previous:after st.workspace
           |> Result.map_error Diagnostic.to_string
    with Stop message -> Error message

let reconcile ~factories ?(adopt = true) before after = run ~factories ~adopt before after


(* The scene graph of a document that has none: the objects the host made are written, so that
   an object can be added to it. *)
let adopt ~factories (doc : Document.t) =
  let ( let* ) = Result.bind in
  let* catalog = Result.map_error Diagnostic.to_string
      (Lazy.force doc.workspace.catalog) in
  let st = { catalog; workspace = doc.workspace.ws; unfolded = []; adopted = false } in
  try
    adopt_objects st doc;
    Contexts.of_workspace ~factories ~previous:doc st.workspace |> Result.map_error Diagnostic.to_string
  with Stop message -> Error message

(* The binding of a home, unfolding what is written in place: the document with that edit and
   the binding's path (panels of the editor graph are edited through it). *)
let bind_home ~factories (doc : Document.t) home =
  let ( let* ) = Result.bind in
  let* catalog = Result.map_error Diagnostic.to_string
      (Lazy.force doc.workspace.catalog) in
  let st = { catalog; workspace = doc.workspace.ws; unfolded = []; adopted = false } in
  try
    let path = bind st home in
    if st.workspace == doc.workspace.ws then Ok (doc, path)
    else
      Result.map (fun doc -> doc, path)
        (Contexts.of_workspace ~factories ~previous:doc st.workspace
         |> Result.map_error Diagnostic.to_string)
  with Stop message -> Error message

(* ---- composition: gestures on the scene's merge ---- *)

let scene_name (workspace : Workspace_doc.t) =
  match Contexts.graph_of workspace Flow.Context.scene with Some g -> g.name | None -> "scene"

(* A new geometry object: its own SOP graph (a box), a [scene/geometry] binding and one more merge
   input; [~existing] reuses a SOP graph (two objects share it and it cooks once).  The ops make
   one gesture: apply them together. *)
let add_geometry (doc : Document.t) ~existing =
  let workspace = doc.workspace.ws in
  let used = ref (owned workspace) in
  let graph_ops, graph = match existing with
    | Some g -> [], g
    | None ->
        let g = F.fresh_among used "shape" in
        [ F.Set_graph { name = g; form = graph_form g "sop" (mk (S.List [ sym "sop/box" ])) } ], g in
  let labels = List.map (fun (i : Edit.node_info) -> i.label) (Edit.inspect (Document.scene_graph doc)) in
  let name = if not (List.mem graph labels) then []
    else
      let rec free n = let l = Printf.sprintf "%s %d" graph n in if List.mem l labels then free (n + 1) else l in
      [ mk (S.Kw "name"); mk (S.Str (free 2)) ] in
  let expr = mk (S.List (sym "scene/geometry" :: mk (S.List [ sym "ref"; sym graph ]) :: name)) in
  let scene = scene_name workspace in
  graph_ops @ [ F.Add_node { scope = [ scene ]; name = F.fresh_name workspace.source ~root:scene "object"; expr } ]

(* A new World: a world graph (a sky and a sun) and a [scene/world] member; a scene has one. *)
let add_world (doc : Document.t) =
  if Objects.ids "world" (Document.scene_graph doc) <> [] then Error "The scene already has a World."
  else
    let workspace = doc.workspace.ws in
    let used = ref (owned workspace) in
    let g = F.fresh_among used "sky" in
    let body = mk (S.List [ sym "world/sun"; mk (S.List [ sym "world/sky" ]) ]) in
    Ok [ F.Set_graph { name = g; form = graph_form g "world" body };
         F.Add_node { scope = [ scene_name workspace ];
                      name = F.fresh_name workspace.source ~root:(scene_name workspace) "world";
                      expr = mk (S.List [ sym "scene/world"; mk (S.List [ sym "ref"; sym g ]) ]) } ]
