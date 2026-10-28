(* Everything the user edits and saves, as one immutable value: the one
   thing [Editor_core.History] snapshots.  Selection, hover, and the open
   level are view state and stay outside.  The document is a workspace (its
   text is the authored truth); the scene, the networks and the shell are what
   lowering derives from it. *)
open Sop
module Int_map = Map.Make (Int)

(* A geometry object's SOP network with its display node. *)
type network = {
  context : Flow.Context.t;
  graph : Flow_sop.Network.t;
  displayed : int option;  (* None when no node has a geometry output *)
}

let of_geometry ~context graph displayed =
  { context; graph = Flow_sop.Network.of_geometry graph; displayed }

(* Where the text of a scene object, a World layer or the settings lives: a binding of its
   graph, an argument of another home (an inline call, written in place), a copy of a loop's
   template, or nowhere an edit can reach.  Edits to the derived objects are written back
   through it.  The copies of a loop are instances of ONE template: [Copy]
   names the loop (its binding, or the argument holding it), the template's path inside the
   loop body ([["@result"]] or a binding of the body) and which copy this is; an edit of a
   copy is an edit of the template, so every copy changes. *)
type home =
  | Bound_at of Flow.Workspace.path
  | Inline_in of home * Flow_graph.Flow_edit.arg_key
  | Copy of { loop : home; rel : string list; index : int }
  | Looped

type homes = {
  objects : (int * home) list;  (* scene object id *)
  world : home option;  (* the [scene/world] call *)
  layers : (int * home) list;  (* World layer id *)
  settings : home option;  (* the [settings/config] call *)
  root : home option;  (* the [scene/root] call; none: the scene is a part with a default root *)
  world_graph : string option;  (* the world graph the [scene/world] call references *)
}

let no_homes = { objects = []; world = None; layers = []; settings = None; root = None; world_graph = None }

(* Where a panel of the shell tree came from in the editor graph. *)
type origin =
  | Bound of string  (* its binding *)
  | Loop of home * Flow_graph.Flow_edit.arg_key
      (* the tile call a loop makes panels for, and the loop's argument in it *)
  | Inline of home * Flow_graph.Flow_edit.arg_key  (* written in place: an argument of this call *)

type preview_source = {
  editor_graph : string;
  panel : origin option;
  scene_ref : Flow.Syntax.t option;
  instance : Flow.Eval.value;
}

(* The World a viewport over another scene instance shows: the object holding its fields and its
   layers (they are not in the scene) *)
type view_world = { node : Kind_graph.node; layers : Kind_graph.t }

(* How a viewport over another scene instance renders: the settings of its [scene/root] and the
   view of the camera object that root's [:camera] slot names (none: the root names no camera) *)
type view_root = { params : Objects.Root.parameters; camera : Rays.Camera.t option }

type switch = { layouts : Editor_core.Panels.t list; active : int }

(* What the panels' keywords say the editor opens with (flow.md 11.11): read once, not kept in step *)
type start = {
  focus : Editor_core.Panels.path option;  (* the first leaf with [:focus true] *)
  looking : string list;  (* the viewports with [:look_through true], by key *)
  graph_views : (Editor_core.Panels.path * string) list;  (* each graph leaf's [:view] *)
  tabs : (Editor_core.Panels.path * string) list;  (* each lisp leaf's [:tab] *)
}

(* The lowered editor graph: the shell tree, the origin of each named or
   looped panel (any other panel is inline), the graph each [(ui/graph "name")] leaf shows
   and, for each viewport over a scene other than the default one, the scene objects
   of that instance (they live in [scene], and no other viewport draws them). *)
type shell = {
  tree : Editor_core.Panels.t;
  origins : (Editor_core.Panels.path * origin) list;
  named : (Editor_core.Panels.path * string) list;
  repeated : string list;  (* the bindings several leaves are made from: those leaves are keyed by place *)
  follows : (Editor_core.Panels.path * Editor_core.Panels.path) list;
  (* an inspector, list or lisp leaf with [:of]: the graph leaf it shows *)
  start : start;
  views : (string * int list) list;
  canvases : (string * Flow.Eval.value) list;
  preview_sources : (string * preview_source) list;
  switch : switch option;  (* the first [ui/switch]: every layout it holds, and the active one *)
}

(* The authored document, its lowering and the window's precision, read once with the lowering. *)
type workspace = { ws : Workspace_doc.t; lowered : Flow_sop.Lower.t; fast : bool Lazy.t;
                   catalog : (Flow.Catalog.catalog, Diagnostic.t) result Lazy.t (* the factories' kinds, built when an edit first asks *) }

type t = {
  scene : Kind_graph.t;  (* objects: the input is the parent *)
  networks : network Int_map.t;  (* by geometry object id: its SOP network *)
  layers : Kind_graph.t Int_map.t;  (* by World id: its layers, each on the one below *)
  active_camera : int option;  (* a camera object *)
  root : Objects.Root.parameters;  (* how the scene renders: the root's settings, else the defaults *)
  settings : Settings.t;
  shell : shell option;  (* the [editor] graph of the workspace, if it has one *)
  view_worlds : (string * view_world option) list;
  (* a viewport over a scene instance that names another World than the document's: its World
     (none: that instance has none); a viewport not listed shows the document's *)
  view_roots : (string * view_root) list;
  (* a viewport over a scene instance with a [scene/root] of its own: how it renders (the render
     size, samples and renderer) and through which camera; a viewport not listed (one over a part,
     or over the document's own scene) renders as [root] and [active_camera] say *)
  homes : homes;  (* where each derived object is written in the text *)
  scene_drives : (string * (Flow_sop.Port.parameter * Flow.Eval.value) list) Int_map.t;
  scene_textures : Flow.Eval.value Int_map.t;
  (* Residual light fields, resolved for composition; never written into history. *)
  workspace : workspace;
  (* the authored document and its lowering: [scene] and [networks] are that lowering, one
     geometry object per [sop] graph; the lowering (compiled ids, volatile set) is history
     state so undo restores both *)
}

(* What the list shows: the scene's objects, or inside an object its SOP network or the World's
   layers. *)
type level = Scene | Inside of int

let network value id = Int_map.find_opt id value.networks
let layers value id = Int_map.find_opt id value.layers
let with_layers value id layers = { value with layers = Int_map.add id layers value.layers }

(* the objects or layers of a level; none inside a geometry object *)
let kinds value = function
  | Scene -> Some value.scene
  | Inside id -> layers value id

let with_kinds value level graph = match level with
  | Scene -> { value with scene = graph }
  | Inside id -> with_layers value id graph

(* Resolve navigation after load, undo, or object removal. The scene is
   always a valid level, including an empty scene. *)
let resolve_level value preferred =
  match preferred with
  | Inside id when (Int_map.mem id value.networks || Int_map.mem id value.layers)
      && Kind_graph.find value.scene id <> None -> Inside id
  | Inside _ | Scene -> Scene

(* The saved/loaded document boundary. Disconnected SOPs are editable and
   valid; compiling their display is a separate cook-time check. *)
let validate value =
  let ( let* ) = Result.bind in
  let finite_fields fields = List.for_all (fun (field : Param.field_view) ->
    match field.current with Float_value x -> Float.is_finite x | _ -> true) fields in
  let validate_network name network =
    let* () = Result.map_error Diagnostic.to_string (Flow_sop.Network.validate network.graph) in
    let nodes = Edit_graph.inspect network.graph.geometry in
    let exists id = Edit_graph.find network.graph.geometry ~node_id:id <> None in
    if (match network.displayed with Some id -> not (exists id) | None -> false)
    then Error (name ^ " has an invalid display node")
    else if not (List.for_all (fun (info : Edit_graph.node_info) ->
      Array.for_all (Option.fold ~none:true ~some:exists) info.inputs
      && finite_fields (Node.parameter_fields info.node)) nodes)
    then Error (name ^ " has invalid inputs or nonfinite parameters")
    else Ok () in
  let validate_kinds name graph =
    let exists id = Kind_graph.find graph id <> None in
    if List.for_all (fun (node : Kind_graph.node) ->
      Option.fold ~none:true ~some:exists node.input && finite_fields (Kind_graph.fields node)) graph
    then Ok () else Error (name ^ " has invalid inputs or nonfinite parameters") in
  let* () = validate_kinds "scene" value.scene in
  let* () = List.fold_left (fun state (node : Kind_graph.node) ->
    let* () = state in
    match node.kind, Int_map.mem node.id value.networks, Int_map.mem node.id value.layers with
    | "geometry", true, false | "world", false, true | ("camera" | "light"), false, false -> Ok ()
    | ("geometry" | "world"), _, _ ->
        Error (Printf.sprintf "object %S (#%d) has no network" node.label node.id)
    | ("camera" | "light"), _, _ ->
        Error (Printf.sprintf "object #%d cannot own a network" node.id)
    | _ -> Error (Printf.sprintf "invalid scene object %S" node.kind))
      (Ok ()) value.scene in
  let owner id kind = match Kind_graph.find value.scene id with
    | None -> Error (Printf.sprintf "network has missing owner #%d" id)
    | Some node when node.kind <> kind -> Error (Printf.sprintf "network #%d has the wrong context" id)
    | Some _ -> Ok () in
  let* () = Int_map.fold (fun id network state ->
    let* () = state in
    let* () = owner id "geometry" in
    if network.context <> Flow.Context.sop then Error (Printf.sprintf "network #%d has the wrong context" id)
    else validate_network (Printf.sprintf "network of object #%d" id) network)
      value.networks (Ok ()) in
  let* () = Int_map.fold (fun id layers state ->
    let* () = state in
    let* () = owner id "world" in
    validate_kinds (Printf.sprintf "layers of object #%d" id) layers) value.layers (Ok ()) in
  if not (finite_fields (Settings.fields value.settings))
  then Error "settings contain nonfinite parameters"
  else match value.active_camera with
    | None -> Ok ()
    | Some id -> (match Kind_graph.find value.scene id with
      | Some node when node.kind = "camera" -> Ok ()
      | _ -> Error (Printf.sprintf "active camera #%d is not a camera object" id))

(* Networks follow their objects: a deleted object's network goes. *)
let prune value =
  let alive id _ = Kind_graph.find value.scene id <> None in
  let networks = Int_map.filter alive value.networks and layers = Int_map.filter alive value.layers in
  let active_camera = Option.bind value.active_camera (fun id ->
    if alive id () then Some id else None) in
  if Int_map.cardinal networks = Int_map.cardinal value.networks
      && Int_map.cardinal layers = Int_map.cardinal value.layers
      && active_camera = value.active_camera then value
  else { value with networks; layers; active_camera }

(* The lowered sop graph a geometry object's network is.  An unchanged network is kept physically
   across lowerings while the lowering is rebuilt, so identity alone is not enough: the compiled
   root still names the graph. *)
let object_graph value id =
  let lowered = value.workspace.lowered in
  Option.bind (Int_map.find_opt id value.networks) (fun (n : network) ->
    List.find_map (fun (g : Flow_sop.Lower.graph) ->
      if g.network == n.graph || (match g.root, Edit_graph.root n.graph.geometry with
        | Some a, Some b -> a = b | _ -> false)
      then Some g.name else None) lowered.graphs)

(* Read-only views for tests and tools. *)
let scene_graph value = value.scene
let object_network value id = Option.map (fun (network : network) ->
    network.graph, network.displayed) (Int_map.find_opt id value.networks)

(* A deterministic text of everything the editor keeps in a document, for
   crash reports and for tests that compare two documents.  Not loadable:
   the workspace text is the saved form ([Preset]). *)
let dump value =
  let b = Buffer.create 4096 in
  let line fmt = Printf.ksprintf (fun s -> Buffer.add_string b s; Buffer.add_char b '\n') fmt in
  let pv : Param.value -> string = function
    | Bool_value x -> string_of_bool x | Int_value x -> string_of_int x
    | Float_value x -> Printf.sprintf "%h" x
    | Text_value x | Choice_value x -> Printf.sprintf "%S" x in
  let fields fs = String.concat " " (List.map (fun (f : Param.field_view) ->
    f.name ^ "=" ^ pv f.current) fs) in
  let graph (n : Flow_sop.Network.t) =
    List.iter (fun (info : Edit_graph.node_info) ->
      line "  node %d %s %S slots=%s inputs=%s%s %s" info.id
        (Option.value ~default:info.operation (Edit_graph.node_factory_key n.geometry ~node_id:info.id))
        info.label
        (String.concat "," (Option.value ~default:[] (Edit_graph.node_slot_names n.geometry ~node_id:info.id)))
        (String.concat "," (Array.to_list (Array.map (function Some i -> string_of_int i | None -> "_") info.inputs)))
        (match Edit_graph.root n.geometry with Some r when r = info.id -> " root" | _ -> "")
        (fields (Node.parameter_fields info.node))) (Edit_graph.inspect n.geometry);
    Flow_sop.Port.Map.iter (fun (p : Flow_sop.Port.t) _ -> line "  drive %d.%s live" p.node p.path)
      n.drives in
  let net name (n : network) =
    line "%s %s display=%s" name (Flow.Context.name n.context)
      (match n.displayed with Some d -> string_of_int d | None -> "-");
    graph n.graph in
  let kinds name context graph =
    line "%s %s" name context;
    List.iter (fun (node : Kind_graph.node) ->
      line "  node %d %s %S input=%s %s" node.id node.kind node.label
        (match node.input with Some i -> string_of_int i | None -> "_") (fields (Kind_graph.fields node))) graph in
  kinds "scene" "scene" value.scene;
  Int_map.iter (fun id n -> net (Printf.sprintf "object %d" id) n) value.networks;
  Int_map.iter (fun id l -> kinds (Printf.sprintf "object %d" id) "world" l) value.layers;
  List.iter (fun (key, world) -> line "view %s world %s" key (match world with
    | Some (w : view_world) -> Printf.sprintf "%S" w.node.label | None -> "-")) value.view_worlds;
  List.iter (fun (key, (r : view_root)) -> line "view %s root %s%s" key (fields (Objects.Root.fields r.params))
    (match r.camera with
      | Some c -> let p = Rays.Camera.position c in Printf.sprintf " camera %g %g %g" p.x p.y p.z
      | None -> "")) value.view_roots;
  line "camera %s" (match value.active_camera with Some c -> string_of_int c | None -> "-");
  line "root %s" (fields (Objects.Root.fields value.root));
  line "settings %s" (fields (Settings.fields value.settings));
  Buffer.contents b

(* ---- naming a home for a message: structural, read from the workspace source ---- *)

module S = Flow.Syntax

(* the form a home is written as, when the source can be walked to it: a loop's body is its last
   child, and a copy's [rel] walks the body's [let*] bindings to the template *)
let rec syntax_of (source : S.t list) = function
  | Bound_at path -> Flow_graph.Flow_edit.arg_text source path Flow_graph.Flow_edit.Whole
  | Inline_in (parent, key) ->
      Option.bind (syntax_of source parent) (fun e -> Flow_graph.Flow_edit.arg_of e key)
  | Copy { loop; rel; _ } ->
      let rec last = function [] -> None | [ x ] -> Some x | _ :: r -> last r in
      let rec walk (e : S.t) = function
        | [] -> Some e
        | step :: rest ->
            (match e.node with
             | S.List (({ node = S.Sym "let*"; _ }) :: { node = S.Vec bs; _ } :: body) ->
                 if step = "@result" then Option.bind (last body) (fun b -> walk b rest)
                 else
                   let rec find = function
                     | { S.node = S.Sym n; _ } :: v :: _ when n = step -> Some v
                     | _ :: _ :: r -> find r | _ -> None in
                   Option.bind (find bs) (fun v -> walk v rest)
             | _ -> if step = "@result" then walk e rest else None) in
      Option.bind (syntax_of source loop) (fun e ->
        match e.node with
        | S.List (_ :: rest) -> Option.bind (last rest) (fun body -> walk body rel)
        | _ -> None)
  | Looped -> None

(* the nearest named binding around a home: the graph, or the let* binding *)
let rec enclosing = function
  | Bound_at path -> List.find_opt (fun n -> n <> "" && n.[0] <> '@') (List.rev path)
  | Copy { loop; _ } -> enclosing loop
  | Inline_in (parent, _) -> enclosing parent
  | Looped -> None

let truncate n s = if String.length s <= n then s else String.sub s 0 (n - 3) ^ "..."

(* The head of a form for a message: [for [j (range 2)]], [scene/light]. *)
let head_text (e : S.t) = match e.node with
  | S.List ({ node = S.Sym h; _ } :: { node = S.Vec _ as v; span; id; _ } :: _) ->
      h ^ " " ^ Flow.Lisp.flat { e with node = v; span; id }
  | S.List ({ node = S.Sym h; _ } :: _) -> h
  | _ -> Flow.Lisp.flat e

(* The line of a form in the printed text of the workspace, when it is part of it. *)
let line_of source (e : S.t) =
  let text, spans = Flow.Lisp.print source in
  match List.assoc_opt e.id spans with
  | Some { Diagnostic.start; _ } when start <= String.length text ->
      let n = ref 1 in
      String.iteri (fun i c -> if i < start && c = '\n' then incr n) text;
      Some !n
  | _ -> None

(* What a loop or an expression is called in a message: its binding if it has one, else its head
   form, with its line and the binding around it: [`for [j (range 2)]` (line 12, in "lights")].
   A copy is named by the loop that makes it.  [source] is the workspace's forms. *)
let rec describe source = function
  | Bound_at path when path <> [] -> List.nth path (List.length path - 1)
  | Copy { loop; _ } -> describe source loop
  | home ->
      (match syntax_of source home with
       | None -> (match enclosing home with Some n -> Printf.sprintf "a form in %S" n | None -> "a form")
       | Some e ->
           let where = (match line_of source e with Some l -> [ Printf.sprintf "line %d" l ] | None -> [])
             @ (match enclosing home with Some n -> [ Printf.sprintf "in %S" n ] | None -> []) in
           Printf.sprintf "`%s`%s" (truncate 40 (head_text e))
             (if where = [] then "" else " (" ^ String.concat ", " where ^ ")"))

(* a home with its copy indices erased: the copies of one loop share it *)
let rec template = function
  | Copy { loop; rel; _ } -> Copy { loop = template loop; rel; index = 0 }
  | Inline_in (home, key) -> Inline_in (template home, key)
  | home -> home

(* the loop that holds a home's template, erased, and how many of the document's objects are
   copies from it *)
let rec loop_of = function
  | Copy { loop; _ } -> Some (template loop)
  | Inline_in (home, _) -> loop_of home
  | Bound_at _ | Looped -> None

let copies doc home = match loop_of home with
  | None -> 0
  | key -> List.length (List.filter (fun (_, h) -> loop_of h = key) doc.homes.objects)
