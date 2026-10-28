(* The scene, world and settings kinds: one Lisp kind per object schema ([scene/light]), per World
   layer ([world/sky]) and the workspace settings record ([settings/config]), generated from their
   schemas, with the window the settings graph asks of the host. *)
module Edit = Sop.Edit_graph

let name_field : Param.field_view = {
  name = "name"; label = "Name"; description = None; folder = []; impact = Param.Cook;
  primary = false; unit = None; vec3 = None; kind = Param.Text_view; default = Param.Text_value "";
  current = Param.Text_value "" }

(* [:parent "label"] places an object under another by name (a camera has none); [:active true]
   makes a camera the render camera.  Neither is a field of the node: the lowering reads them. *)
let parent_field = { name_field with name = "parent"; label = "Parent" }
let active_field = { name_field with name = "active"; label = "Active camera";
  kind = Param.Toggle_view; default = Param.Bool_value false; current = Param.Bool_value false }

(* The World and the root are scene kinds too: [scene/world] is a merge member that references a
   world graph, [scene/root] ends the scene graph. *)
let scene_kinds = List.map (fun (k : Kind_graph.Kind.t) -> "scene/" ^ k.key, k)
    (Objects.kinds @ [ Layers.Settings.kind; Objects.Root.kind ])
let world_kinds = List.map (fun (k : Kind_graph.Kind.t) -> "world/" ^ k.key, k) Layers.kinds

(* Workspace settings: what the host needs to open the window. *)
type window = { title : string; width : int; height : int; fps : int; seed : int; fast : bool }

let window_schema =
  let integer min max hard_min = Param.integer ~hard_min ~min ~max () in
  let int_field name label kind default get set =
    Param.field ~name ~label:(label ^ " (on restart)") ~kind ~default ~get ~set () in
  Param.schema ~name:"workspace" ~default:{ title = "Rays"; width = 1280; height = 800;
                                            fps = 60; seed = 1; fast = true } [
    Param.field ~name:"title" ~label:"Title (on restart)" ~kind:Param.Text ~default:"Rays"
      ~get:(fun w -> w.title) ~set:(fun title w -> { w with title }) ();
    int_field "width" "Width" (integer 320 3840 64) 1280 (fun w -> w.width)
      (fun width w -> { w with width });
    int_field "height" "Height" (integer 240 2160 64) 800 (fun w -> w.height)
      (fun height w -> { w with height });
    int_field "fps" "Frames per second" (Param.integer ~hard_min:1 ~hard_max:240 ~min:1 ~max:120 ())
      60 (fun w -> w.fps) (fun fps w -> { w with fps });
    int_field "seed" "Seed" (Param.integer ~hard_min:0 ~min:0 ~max:9999 ()) 1
      (fun w -> w.seed) (fun seed w -> { w with seed });
    (* fast: what only reaches the screen may run on the GPU in float32 and int32 *)
    Param.field ~name:"precision" ~label:"Precision (live)" ~kind:(Param.choice ~equal:Bool.equal [ "fast", true; "exact", false ])
      ~default:true ~get:(fun w -> w.fast) ~set:(fun fast w -> { w with fast }) () ]

let window_fields = Param.view window_schema (Param.default window_schema)

(* slot of each kind: a geometry object takes its geometry, a World layer the layer below *)
let kind_slots qualified = match qualified with
  | "scene/geometry" -> [ "geometry", Edit.Required; "texture", Edit.Optional ]
  | "scene/world" -> [ "world", Edit.Required ]
  | "scene/root" -> [ "scene", Edit.Required; "camera", Edit.Optional ]
  | q when String.starts_with ~prefix:"world/" q -> [ "below", Edit.Optional ]
  | _ -> []

let extra_fields = function
  | "scene/camera" -> [ active_field ]
  | "scene/geometry" | "scene/light" -> [ parent_field ]
  | _ -> []

let descriptors : Flow_sop.Catalog.descriptor list =
  List.map (fun (qualified, (kind : Kind_graph.Kind.t)) ->
    { Flow_sop.Catalog.qualified; key = kind.key; operation = kind.key; label = kind.label;
      category = kind.category; slots = kind_slots qualified;
      slot_types = (if qualified="scene/geometry" then ["geometry";"image"] else []);
      keyword_inputs = [];
      fields = name_field :: extra_fields qualified @
        List.concat (Result.get_ok (Param.group_vec3 ~infer:true (Kind_graph.Kind.fields kind))) })
    (scene_kinds @ world_kinds)
  @ [ { Flow_sop.Catalog.qualified = "settings/config"; key = "config"; operation = "config";
        label = "Settings"; category = [ "Workspace" ]; slots = []; slot_types = [];
        keyword_inputs = [];
        fields = window_fields } ]

(* A function of the factories alone (0.18 ms and 0.6 MB to build): a document keeps its own
   ([Document.workspace.catalog]), so edits do not rebuild it. *)
let catalog ~version factories = Flow_sop.Catalog.of_factories ~version ~extra:descriptors factories

let ports qualified =
  match List.find_opt (fun (d : Flow_sop.Catalog.descriptor) -> d.qualified = qualified) descriptors with
  | Some d -> Result.get_ok (Flow_sop.Port.parameters d.fields)  (* valid by construction *)
  | None -> []
