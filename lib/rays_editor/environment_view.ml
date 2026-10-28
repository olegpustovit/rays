open Rays
open Editor_document
module Image = Rays.Image
open World_map

let set_ui_cursor ui visible =
  let shape = match if visible then Pxui.Ui.cursor ui else None with
    | Some shape -> (shape :> [`Default|`Horizontal_resize|`Vertical_resize|`Text])
    | None -> `Default in
  Sketch.set_cursor shape

type ('rendered, 'camera) hidden_scene_cache = {
  width : int;
  height : int;
  rendered : 'rendered option;
  camera : 'camera;
  background : Rays_math.Color.t;
  view_visible : bool;
  scene : Scene.t;
}

type canvas_picture = {size : int * int; dynamic : bool; scene : Scene.t;
  plan : Flow.Eval.plan;
  prepared : Flow_native.Drawing.prepared option;
  preview : (Flow.Ty.t * int) option  (* the node shown instead of the pane's own picture *)}

(* A World bake for this frame: the preview size while a gesture or the
   day cycle is live, the final size when idle. *)
let bake_world ?view ~previous core ~live =
  Option.map (fun world ->
    let width, height = if live then 512, 256 else 2048, 1024 in
    match previous with
    (* The same World keeps the same bake value, so renderers see no change. *)
    | Some (last, baked) when last = world && (baked : World.baked).camera.width = width ->
        world, baked
    | Some _ | None -> world, World.bake_cached ~width ~height world)
    (Core_host.world ?view core ~time:(Timeline.time (Core_list.timeline core)))

(* the viewports over another scene instance: those that draw objects of their own and those that
   show another World *)
let view_keys (core : _ Core_model.t) =
  let own = match core.Core_model.doc.Document.shell with Some shell -> List.map fst shell.views | None -> [] in
  own @ List.filter (fun key -> not (List.mem key own)) (List.map fst core.Core_model.doc.Document.view_worlds)

let save_status ~save ~filename pending rendered =
  match pending, rendered with
  | Some request, Some _ ->
      Some (match save request with
        | Ok () -> "Saved " ^ filename request
        | Error message -> "Render failed: " ^ message)
  | _ -> None

let world ~paint_view ~key ~camera ~rendered ~view_visible viewport =
  match rendered with
  | Some image when view_visible -> paint_view ~key viewport camera image
  | Some _ | None -> []

(* Each viewport shows its own scene instance's image, else the primary one. *)
let worlds ~paint_view ~camera ~rendered ~views ~view_visible bodies =
  List.concat_map (fun (key, bounds) ->
    world ~paint_view ~key ~camera:(camera key) ~view_visible
      ~rendered:(match List.assoc_opt key views with Some image -> Some image | None -> rendered)
      bounds) bodies

let hidden_only ~ui_visible core =
  not ui_visible && core.Core_model.leader = Leader.Idle

(* The fully hidden scene, reused while its inputs are physically unchanged
   so the renderer sees the same [Scene.t]. *)
let hidden_entry ~background ~rendered ~camera ~paint_view ~cache core
    (frame : Frame.t) =
  let view_visible = Core_shell.view_visible core in
  match cache with
  | Some cached when cached.width = (fst frame.input.size)
      && cached.height = (snd frame.input.size) && cached.rendered == rendered
      && cached.camera == camera && cached.background = background
      && cached.view_visible = view_visible -> cached
  | _ ->
      let scene = Scene.clear background ::
        world ~paint_view ~key:"@hidden" ~camera ~rendered ~view_visible
          (0, 0, (fst frame.input.size), (snd frame.input.size)) in
      { width = (fst frame.input.size); height = (snd frame.input.size);
        rendered; camera; background; view_visible; scene }

let compose_view ?map ~canvases ~ui_visible ~background ~rendered ~focused ~views ~camera ~camera_of ~paint_view
    ~guides ~cache core (frame : Frame.t) =
  let view_visible = Core_shell.view_visible core in
  let paint_world = match map with
    | Some image when view_visible && core.Core_model.map_view -> (fun viewport ->
        let x, y, w, _ = map_rect viewport in
        [Scene.image image ~at:(x, y)
           ~scale:(float w /. float (max 1 (fst(Image.dimensions image)))) ()])
    | _ -> world ~paint_view ~key:(match core.Core_model.focus with View key -> key | _ -> "main")
        ~camera ~rendered:focused ~view_visible in
  (* each viewport is drawn from its own orbit *)
  if not ui_visible && (match core.Core_model.focus with Canvas _ -> true | _ -> false) then
    let key = match core.Core_model.focus with Canvas key -> key | _ -> assert false in
    Scene.clear background :: Option.fold ~none:[] ~some:(fun picture -> picture.scene) (List.assoc_opt key canvases)
    @ Core_host.machinery core ~all_ui_visible:false
  else if hidden_only ~ui_visible core then
    (hidden_entry ~background ~rendered:focused ~camera ~paint_view ~cache core frame).scene
  else if not ui_visible then
    Scene.clear background :: (world ~paint_view ~key:"@hidden" ~camera ~rendered:focused ~view_visible
      (0, 0, (fst frame.input.size), (snd frame.input.size)))
    @ Core_host.machinery core ~all_ui_visible:false
  else
    let viewport = (Core_shell.panes core frame).view in
    let guides = if view_visible then guides viewport else [] in
    (* every viewport shows its own scene instance's image; the map replaces the focused one *)
    let floating = List.filter_map (fun (leaf : Pxui_shell.Layout.leaf) ->
      match leaf.panel with
      | View key when leaf.floating ->
          Some (key, leaf.path, leaf.body)
      | _ -> None) (Core_shell.geometry core core.Core_model.workspace frame).leaves in
    let painted, windows = List.fold_left (fun (painted, windows) (key, bounds) ->
      let image = if bounds = viewport && core.Core_model.map_view then paint_world viewport
        else worlds ~paint_view ~camera:camera_of ~rendered ~views ~view_visible [key, bounds] in
      match List.find_opt (fun (k, _, _) -> k = key) floating with
      | None -> image :: painted, windows
      | Some (_, path, (x, y, w, h)) ->
          let root = List.find_map (fun (root, (panel, p)) ->
            if panel = Pxui_shell.Layout.View key && p = Some path then Some root else None) core.Core_model.pane_keys in
          (* inside the window's line-2 frame, which Chrome paints on the body's edge *)
          let image = Scene.rect ~at:(x + 1, y + 1) ~w:(w - 2) ~h:(h - 2) ~fill:Rays_math.Color.white () :: image in
          painted, (root, image) :: windows) ([], []) (Core_shell.view_bodies core frame) in
    let canvas_painted, canvas_windows = List.fold_left (fun (painted, windows) (leaf : Pxui_shell.Layout.leaf) -> match leaf.panel with
      | Canvas key ->
          let x, y, w, h = leaf.body in
          let scene = Option.fold ~none:[] ~some:(fun picture -> picture.scene) (List.assoc_opt key canvases) in
          let root = List.find_map (fun (root, (panel, p)) ->
            if panel = Pxui_shell.Layout.Canvas key && p = Some leaf.path then Some root else None) core.Core_model.pane_keys in
          let picture = [Scene.clip ~at:(x, y) ~w ~h [Scene.translate x y scene]] in
          if leaf.floating then painted, (root, picture) :: windows
          else picture :: painted, windows
      | _ -> painted, windows) ([], []) (Core_shell.geometry core core.Core_model.workspace frame).leaves in
    let under key = Option.value ~default:[] (List.assoc_opt (Some key) (canvas_windows @ windows)) in
    Scene.clear background :: List.concat (List.rev (canvas_painted @ painted))
    @ guides
    @ Core_host.machinery ~under core ~all_ui_visible:true
