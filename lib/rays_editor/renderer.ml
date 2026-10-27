(* The one renderer choice every sketch shows in its settings block, so the
   editor shell reads the same across sketches. *)
open Rays

type t = Path_traced | Raster | Wireframe

let kind = Editor_core.Param.choice ~equal:( = )
    [ "Path traced", Path_traced; "Raster", Raster; "Wireframe", Wireframe ]

let field ~default ~get ~set =
  Editor_core.Param.field ~name:"renderer" ~label:"Renderer" ~kind ~default ~get ~set ()

let of_env name = match Sys.getenv_opt name with
  | Some "path" -> Some Path_traced
  | Some "raster" -> Some Raster
  | Some "wireframe" -> Some Wireframe
  | Some _ | None -> None

let wire_color background =
  let lightness = 299 * background.Color.r + 587 * background.g
    + 114 * background.b in
  if lightness >= 128000 then Color.hex_exn "#285f77"
  else Color.hex_exn "#bed7e1"

(* Every polygon edge once, as a line mesh. *)
let wire_mesh geometry =
  let topology = Rdk.Geometry.topology geometry in
  let edges = Rdk.Topology_index.create topology in
  let points = Rdk.Geometry.positions geometry in
  let vertices = Array.init (Rdk.Packed.Float3.length points) (fun index ->
    let x, y, z = Rdk.Packed.Float3.get points index in Vec3.create x y z) in
  let indices = Array.make (Rdk.Topology_index.edge_count edges * 2) 0 in
  for edge = 0 to Rdk.Topology_index.edge_count edges - 1 do
    let a, b = Rdk.Topology_index.edge_points edges edge in
    indices.(edge * 2) <- a; indices.(edge * 2 + 1) <- b
  done;
  Mesh.Private.create_owned ~mode:Mesh.Lines ~indices vertices

let label = function Path_traced -> "Path traced" | Raster -> "Raster" | Wireframe -> "Wireframe"
let of_label = function "Path traced" -> Some Path_traced | "Raster" -> Some Raster
  | "Wireframe" -> Some Wireframe | _ -> None

module P = Rays_pathtracer
type cached_mesh = { source : Mesh.t; material : Material.t; geometry : Rdk.Geometry.t;
                     traced : P.mesh }
(* how one viewport renders: the root of its scene instance, else the document's.  The sample cap
   is not part of it: a cap is read each frame, so raising one continues the accumulation while any
   other change discards the tracer *)
type setting = { mode : t; bounces : int; round_samples : int }

(* A viewport as one frame asks to draw it: [film] is the tracer's film in drawable pixels, [step]
   the divisor of the root's resolution it came from, [cap] the root's samples per pixel. *)
type view = { key : string; film : int * int; step : int; camera : Camera.t; scene : Scene3.t;
              setting : setting; cap : int }

(* One tracer, shared by every viewport that asks for the same picture.  [scene], [camera] and
   [film] are what the tracer was last given: a slot differing from the frame's request is stale. *)
type slot = { keys : string list; film : int * int; step : int; scene : Scene3.t; camera : Camera.t option;
              wire : Scene3.t option; tracer : P.t option; setting : setting; cap : int;
              failure : string option  (* why the last render of the slot failed, until one succeeds *) }
(* [mode] is the document's; [raster_only]: every viewport draws as raster, so there are no slots;
   [turn]: the viewport whose slot last took a turn of the sample budget *)
type state = { mode : t; custom : bool; slots : slot list; meshes : cached_mesh list;
               error : string option; raster_only : bool; turn : string option }
let empty = { mode = Raster; custom = false; slots = []; meshes = []; error = None; raster_only = true;
              turn = None }
let close state = List.iter (fun slot -> Option.iter P.destroy slot.tracer) state.slots

(* The film of a traced viewport: the root's resolution divided by 1, 2, 4 or 8, the largest that
   does not exceed the gate (in drawable pixels, the root's aspect fitted in the pane), the eighth
   when even that is too big.  A pane resize changes the film only by crossing a step.
   A viewport that is not looking [through] the render camera shows its own free view, not the
   render: its film is its rectangle, pixel for pixel, so the picture has the pane's aspect and
   lies where the raster one does. *)
let film_steps = [ 1; 2; 4; 8 ]
let film ~through ~resolution:(width, height) ~gate:(gate_width, gate_height) =
  if not through then (max 1 gate_width, max 1 gate_height), 1 else
  let step = Option.value ~default:8 (List.find_opt (fun step ->
    width / step <= gate_width && height / step <= gate_height) film_steps) in
  (max 1 (width / step), max 1 (height / step)), step

let linear (color : Color.t) = P.Linear_color.rgb
  (float color.r /. 255.) (float color.g /. 255.) (float color.b /. 255.)
let traced_material (material : Material.t) =
  P.material ~roughness:(sqrt (2. /. (max 0. material.shininess +. 2.))) ~round:material.round
    ~emission:(linear material.emissive) (linear material.diffuse)

let wire_scene scene =
  let nodes = ref [] in
  Scene3.Private.iter_batches (fun (drawing : Scene3.Private.drawing) transforms ->
    let material = Material.unlit (wire_color (Scene3.Private.ambient scene)) in
    let drawing_node = match transforms with
      | None -> Scene3.mesh ~material ~mode:Scene3.Wireframe ~cull:Scene3.Cull_none drawing.mesh
      | Some transforms -> Scene3.instances_array ~material ~mode:Scene3.Wireframe
          ~cull:Scene3.Cull_none drawing.mesh transforms in
    nodes := Scene3.transform drawing.transform [drawing_node] :: !nodes) scene;
  Scene3.create ~samples:1 (List.rev !nodes)

(* The cache keeps at most 64 converted source meshes. Placement remains in the
   tracer's instance structure, so an orbit does not convert or upload geometry. *)
let traced_scene meshes scene =
  let ( let* ) = Result.bind in
  let cache = ref meshes and result = ref (Ok []) in
  Scene3.Private.iter_batches (fun (drawing : Scene3.Private.drawing) transforms ->
    result := let* placed = !result in
      let* () = if drawing.texture = None then Ok ()
        else Error "Path tracing textured meshes is not supported." in
      let* cached = match List.find_opt (fun cached -> cached.source == drawing.mesh
          && cached.material = drawing.material) !cache with
        | Some cached -> Ok cached
        | None ->
            let* geometry = Result.map_error Rdk.Error.to_string
                (Rdk_rays.Rays_mesh.of_mesh drawing.mesh) in
            let* traced = P.mesh [geometry, traced_material drawing.material] in
            let cached = {source = drawing.mesh; material = drawing.material; geometry; traced} in
            cache := cached :: List.filteri (fun i _ -> i < 63) !cache;
            Ok cached in
      let* mesh = match transforms with
        | None -> Ok cached.traced
        | Some transforms -> P.mesh_instanced
            ~prototype:(cached.geometry, traced_material cached.material) transforms in
      Ok ((drawing.transform, mesh) :: placed)) scene;
  let* placed = !result in
  let* mesh = P.scene_mesh (List.rev placed) in
  Ok (!cache, mesh)

(* Viewports asking for the same traced picture share one tracer: the same scene, camera, film
   and setting. *)
let shares (a : view) (b : view) =
  a.setting.mode = Path_traced && a.setting = b.setting && a.scene == b.scene && a.film = b.film
  && a.camera = b.camera

(* The groups of viewports that share a tracer, the focused one first so it keeps the slot it had. *)
let groups ~focus views =
  let ordered = match List.find_opt (fun (v : view) -> v.key = focus) views with
    | Some first -> first :: List.filter (fun v -> v != first) views
    | None -> views in
  List.fold_left (fun groups (v : view) ->
    if List.exists (fun group -> shares (List.hd group) v) groups
    then List.map (fun group -> if shares (List.hd group) v then group @ [v] else group) groups
    else groups @ [ [v] ]) [] ordered

type plan = { view : view; names : string list; limit : int; previous : slot option; wants : bool;
              focused : bool }

let no_batches scene =
  let found = ref false in
  Scene3.Private.iter_batches (fun _ _ -> found := true) scene; not !found

(* Of the viewports that want a turn, the one after [last] in [order], cyclically: each takes its
   turn before any takes a second one. *)
let next_turn ~order ~last wanting =
  let count = List.length order in
  let position key = Option.value ~default:(-1) (List.find_index (( = ) key) order) in
  let after = Option.fold ~none:(-1) ~some:position last in
  let distance key = (position key - after - 1 + count) mod count in
  List.fold_left (fun best key -> match best with
    | Some b when distance b <= distance key -> best
    | Some _ | None -> Some key) None wanting

(* [focus]: the viewport whose slot renders every frame.  The others share the sample budget: of
   the slots that want the GPU (their picture differs from the frame's, or they are short of their
   cap) one takes a turn per frame, in rotation.  A slot at its cap costs nothing. *)
let update state ~mode ~custom ~focus views =
  if custom || List.for_all (fun (v : view) -> v.setting.mode = Raster) views then begin
    close state; { empty with mode; custom }
  end else if List.length views > 16 then begin
    close state; { empty with mode; raster_only = false;
                   error = Some "At most 16 viewports can render together." }
  end else
    let focus = match focus with
      | Some key when List.exists (fun (v : view) -> v.key = key) views -> key
      | Some _ | None -> (List.hd views).key in
    let claimed = ref [] in
    (* what each group had: a slot of the same setting that served one of its viewports, taken by
       one group only *)
    let plans = List.map (fun group ->
      let view = List.hd group in
      let keys = List.map (fun (v : view) -> v.key) group in
      let previous = List.find_opt (fun (slot : slot) -> not (List.memq slot !claimed)
          && slot.setting = view.setting && List.exists (fun key -> List.mem key slot.keys) keys) state.slots in
      Option.iter (fun slot -> claimed := slot :: !claimed) previous;
      let cap = List.fold_left (fun cap (v : view) -> max cap v.cap) 1 group in
      let wants = view.setting.mode = Path_traced && (match previous with
        | Some { tracer = Some tracer; scene; camera; film; _ } ->
            scene != view.scene || camera <> Some view.camera || film <> view.film
            || P.samples tracer < cap
        | Some { tracer = None; _ } | None -> true) in
      { view; names = keys; limit = cap; previous; wants; focused = List.mem focus keys }) (groups ~focus views) in
    let chosen = Option.bind (next_turn ~order:(List.map (fun (v : view) -> v.key) views) ~last:state.turn
        (List.filter_map (fun plan -> if plan.wants && not plan.focused then Some plan.view.key else None) plans))
      (fun key -> List.find_opt (fun plan -> plan.view.key = key) plans) in
    let meshes = ref state.meshes in
    let settle plan =
      let ({ view; names = keys; limit = cap; previous; _ } : plan) = plan in
      let ({ film; step; scene; camera; setting; _ } : view) = view in
      let blank = {keys; film; step; scene = Scene3.empty; camera = None; wire = None; tracer = None;
                   setting; cap; failure = None} in
      match setting.mode with
      | Raster -> {blank with scene}
      | Wireframe ->
          {blank with scene; wire = Some (match previous with
            | Some {wire = Some wire; scene = last; _} when last == scene -> wire
            | Some _ | None -> wire_scene scene)}
      | Path_traced when not plan.wants
                         || not (plan.focused || Option.fold ~none:false ~some:(( == ) plan) chosen) ->
          (* at its cap with nothing changed (the focused one too), or not this frame's turn: the
             picture it had stays *)
          (match previous with Some slot -> {slot with keys; cap} | None -> blank)
      | Path_traced ->
          let prior = Option.value ~default:blank previous in
          if (prior.scene != scene || prior.tracer = None) && no_batches scene then {blank with scene}
          else
            let ( let* ) = Result.bind in
            let width, height = film in
            let created = ref None in
            let render () =
              let* cache, mesh = if prior.scene == scene && prior.tracer <> None
                then Ok (!meshes, None) else
                  Result.map (fun (cache, mesh) -> cache, Some mesh) (traced_scene !meshes scene) in
              meshes := cache;
              let* tracer = match prior.tracer with
                | Some tracer -> Ok tracer
                | None ->
                    let first = List.hd cache in
                    let* tracer = P.create ~bounces:setting.bounces ~round_samples:setting.round_samples
                      ~width ~height
                      {P.objects = [first.geometry, traced_material first.material]; spheres = []; strands = [];
                       environment = {sky = linear (Color.rgb 130 145 170);
                         ground = linear (Color.rgb 80 80 80); panels = []}; lights = []} in
                    created := Some tracer; Ok tracer in
              let submitted = match mesh with None -> Ok () | Some mesh -> P.queue_mesh tracer mesh in
              let* () = submitted in
              let* () = P.resize tracer ~width ~height in
              let* () = if prior.tracer = None || Scene3.Private.lights prior.scene <> Scene3.Private.lights scene
                then P.set_lights tracer (List.map P.light_of (Scene3.Private.lights scene)) else Ok () in
              let* () = P.set_world tracer (Scene3.Private.world scene) in
              let* () = P.render tracer camera in
              Ok {blank with scene; camera = Some camera; tracer = Some tracer} in
            match render () with
            | Ok slot -> slot
            | Error message -> Option.iter P.destroy !created;
                let stale = prior.tracer <> None in
                let failure = Some (Printf.sprintf "Renderer [%s]: %s (%s)" view.key message
                  (if stale then "stale output retained" else "no output")) in
                if stale then {prior with keys; cap; failure} else {blank with scene; failure} in
    let slots = List.map settle plans in
    List.iter (fun previous -> Option.iter (fun tracer ->
      if not (List.exists (fun slot -> Option.fold ~none:false ~some:(fun t -> t == tracer) slot.tracer) slots)
      then P.destroy tracer) previous.tracer) state.slots;
    let failures = List.filter_map (fun slot -> slot.failure) slots in
    {mode; custom; slots; meshes = !meshes; raster_only = false;
     turn = (match chosen with Some plan -> Some plan.view.key | None -> state.turn);
     error = if failures = [] then None else Some (String.concat "; " failures)}

let slot_of state key = List.find_opt (fun slot -> List.mem key slot.keys) state.slots

(* What a traced viewport's slot holds: the film in pixels, the step of the root's resolution it
   is at, the samples per pixel accumulated and the cap they run to, and how many viewports it serves. *)
type info = { size : int * int; step : int; samples : int; cap : int; viewports : int }

let info state ~key = if state.custom || state.raster_only then None else
  match slot_of state key with
  | Some {tracer = Some tracer; step; cap; keys; _} ->
      Some {size = P.size tracer; step; samples = P.samples tracer; cap; viewports = List.length keys}
  | Some _ | None -> None

let paint state ~key bounds camera scene =
  if state.custom || state.raster_only then [Scene.view3d ~viewport:bounds ~camera scene]
  else match slot_of state key with
    | Some {setting = {mode = Raster; _}; _} -> [Scene.view3d ~viewport:bounds ~camera scene]
    | Some {wire = Some wire; _} -> [Scene.view3d ~viewport:bounds ~camera wire]
    | Some {tracer = Some tracer; _} -> let x, y, w, _ = bounds in
        (* the film is scaled to its pane's gate, so a resize inside a step never re-renders *)
        let sw, _ = P.size tracer in
        [Scene.image (P.image tracer) ~at:(x, y) ~scale:(float w /. float (max 1 sw)) ()]
    | _ -> []
