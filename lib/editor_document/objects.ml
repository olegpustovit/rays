open Rays
open Sop

(* Scene objects are nodes of the scene network: input 0 is the parent and
   parameters hold the transform and each kind's settings, so the graph, the
   list, the inspector, handles, copy/paste, and undo work on them unchanged.
   A geometry object owns a SOP network and the World a layer stack (both in
   [Document.networks]); the scene network itself is never cooked. *)

let empty = Rdk.Line_geometry.points [||]

let marker operation ~label _ parent =
  Custom.plain ~label ~operation (Option.to_list parent) (fun ~context:_ _ -> Ok empty)

module Geometry = struct
  type parameters = {
    translate_x : float [@sop.default 0.] [@sop.label "Translate X"]
      [@sop.folder "Transform"] [@sop.min (-20.)] [@sop.max 20.];
    translate_y : float [@sop.default 0.] [@sop.label "Translate Y"]
      [@sop.folder "Transform"] [@sop.min (-20.)] [@sop.max 20.];
    translate_z : float [@sop.default 0.] [@sop.label "Translate Z"]
      [@sop.folder "Transform"] [@sop.min (-20.)] [@sop.max 20.];
    rotate_x : float [@sop.default 0.] [@sop.label "Rotate X (degrees)"]
      [@sop.folder "Transform"] [@sop.min (-180.)] [@sop.max 180.];
    rotate_y : float [@sop.default 0.] [@sop.label "Rotate Y (degrees)"]
      [@sop.folder "Transform"] [@sop.min (-180.)] [@sop.max 180.];
    rotate_z : float [@sop.default 0.] [@sop.label "Rotate Z (degrees)"]
      [@sop.folder "Transform"] [@sop.min (-180.)] [@sop.max 180.];
    scale_x : float [@sop.default 1.] [@sop.label "Scale X"]
      [@sop.folder "Transform"] [@sop.min 0.] [@sop.max 5.];
    scale_y : float [@sop.default 1.] [@sop.label "Scale Y"]
      [@sop.folder "Transform"] [@sop.min 0.] [@sop.max 5.];
    scale_z : float [@sop.default 1.] [@sop.label "Scale Z"]
      [@sop.folder "Transform"] [@sop.min 0.] [@sop.max 5.];
    visible : bool [@sop.default true] [@sop.label "Visible"];
    render : bool [@sop.default true] [@sop.label "Renderable"];
  } [@@sop.node_key "geometry"] [@@sop.node_label "Geometry"]
    [@@sop.node_category "Object"] [@@sop.node_inputs 1]
    [@@sop.node_optional "0"] [@@deriving sop_params, sop_node]

  let build = parameters_build (marker "geometry")
  let factory = parameters_factory build
end

(* A camera looks from its eye at its target; it has no parent. *)
module Camera = struct
  type parameters = {
    fov : float [@sop.default 60.] [@sop.label "FOV (degrees)"] [@sop.unit "deg"]
      [@sop.folder "Lens"] [@sop.min 5.] [@sop.max 150.] [@sop.hard_min 1.] [@sop.hard_max 179.];
    near : float [@sop.default 0.1] [@sop.label "Near clip"]
      [@sop.folder "Lens"] [@sop.min 0.01] [@sop.max 10.] [@sop.hard_min 0.0001];
    far : float [@sop.default 1000.] [@sop.label "Far clip"]
      [@sop.folder "Lens"] [@sop.min 10.] [@sop.max 10000.] [@sop.hard_min 0.001];
    aperture : float [@sop.default 0.] [@sop.label "Aperture (lens radius)"]
      [@sop.folder "Lens"] [@sop.min 0.] [@sop.max 2.] [@sop.hard_min 0.];
    focus_distance : float [@sop.default 0.] [@sop.label "Focus distance (0: target)"]
      [@sop.folder "Lens"] [@sop.min 0.] [@sop.max 100.] [@sop.hard_min 0.];
    eye_x : float [@sop.default 0.] [@sop.label "Eye X"] [@sop.folder "Transform"]
      [@sop.min (-100.)] [@sop.max 100.];
    eye_y : float [@sop.default 0.] [@sop.label "Eye Y"] [@sop.folder "Transform"]
      [@sop.min (-100.)] [@sop.max 100.];
    eye_z : float [@sop.default 7.] [@sop.label "Eye Z"] [@sop.folder "Transform"]
      [@sop.min (-100.)] [@sop.max 100.];
    target_x : float [@sop.default 0.] [@sop.label "Target X"] [@sop.folder "Transform"]
      [@sop.min (-100.)] [@sop.max 100.];
    target_y : float [@sop.default 0.] [@sop.label "Target Y"] [@sop.folder "Transform"]
      [@sop.min (-100.)] [@sop.max 100.];
    target_z : float [@sop.default 0.] [@sop.label "Target Z"] [@sop.folder "Transform"]
      [@sop.min (-100.)] [@sop.max 100.];
    up_x : float [@sop.default 0.] [@sop.label "Up X"] [@sop.folder "Transform"]
      [@sop.min (-1.)] [@sop.max 1.];
    up_y : float [@sop.default 1.] [@sop.label "Up Y"] [@sop.folder "Transform"]
      [@sop.min (-1.)] [@sop.max 1.];
    up_z : float [@sop.default 0.] [@sop.label "Up Z"] [@sop.folder "Transform"]
      [@sop.min (-1.)] [@sop.max 1.];
    follow_viewport : bool [@sop.default false] [@sop.label "Follow viewport"] [@sop.folder "Transform"];
  } [@@sop.node_key "camera"] [@@sop.node_label "Camera"]
    [@@sop.node_category "Object"] [@@sop.node_inputs 0]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label _parameters ->
    Custom.plain ~label ~operation:"camera" [] (fun ~context:_ _ -> Ok empty))

  let factory = parameters_factory build

  let of_node node =
    if Node.operation node <> "camera" then None else
    let values = List.map (fun (field : Param.field_view) ->
      field.name, field.current) (Node.parameter_fields node) in
    match Param.apply_all parameters_schema parameters_default values with
    | Error _ -> None
    | Ok (p, _) ->
        let at = Rays_math.Vec3.create p.eye_x p.eye_y p.eye_z
        and target = Rays_math.Vec3.create p.target_x p.target_y p.target_z
        and up = Rays_math.Vec3.create p.up_x p.up_y p.up_z in
        let lens = { Rays.Camera.aperture = p.aperture;
                     focus_distance = if p.focus_distance > 0. then Some p.focus_distance else None } in
        (match Rays.Camera.perspective ~fov_y:(p.fov *. Float.pi /. 180.)
            ~near:p.near ~far:p.far ~at ~target ()
              |> Rays.Camera.with_up up |> Rays.Camera.with_lens lens with
         | camera -> Some (camera, p.follow_viewport)
         | exception Invalid_argument _ -> None)

  let lens_values (lens : Rays.Camera.lens) =
    ["aperture", Param.Float_value lens.aperture;
     "focus_distance", Param.Float_value (Option.value ~default:0. lens.focus_distance)]

  let to_values ~(eye : Rays_math.Vec3.t) ~(target : Rays_math.Vec3.t) ~fov_y =
    let float name value = name, Param.Float_value value in
    [float "eye_x" eye.x; float "eye_y" eye.y; float "eye_z" eye.z;
     float "target_x" target.x; float "target_y" target.y;
     float "target_z" target.z; float "fov" (fov_y *. 180. /. Float.pi)]
end

(* How the scene is rendered: the one [scene/root] of a scene graph (a scene graph without one is a
   part and gets the defaults).  Not a node of the scene network; the document keeps its values
   ([Document.root]).  The active camera is the root's [:camera] slot, not a field. *)
module Root = struct
  type renderer = Raster | Wireframe | Path_traced

  let renderer_parameter = Param.choice ~equal:( = )
      ["Raster", Raster; "Wireframe", Wireframe; "Path traced", Path_traced]

  type parameters = {
    renderer : renderer [@sop.default Raster] [@sop.label "Renderer"] [@sop.kind renderer_parameter];
    width : int [@sop.default 1920] [@sop.label "Width (pixels)"] [@sop.folder "Size"]
      [@sop.min 64] [@sop.max 7680] [@sop.hard_min 1] [@sop.hard_max 16384];
    height : int [@sop.default 1080] [@sop.label "Height (pixels)"] [@sop.folder "Size"]
      [@sop.min 64] [@sop.max 4320] [@sop.hard_min 1] [@sop.hard_max 16384];
    max_spp : int [@sop.default 256] [@sop.label "Max samples per pixel"] [@sop.folder "Samples"]
      [@sop.min 1] [@sop.max 4096] [@sop.hard_min 1];
    bounces : int [@sop.default 4] [@sop.label "Bounces"] [@sop.folder "Samples"]
      [@sop.min 1] [@sop.max 32] [@sop.hard_min 1] [@sop.hard_max 64];
    round_samples : int [@sop.default 4] [@sop.label "Round-corner samples"] [@sop.folder "Samples"]
      [@sop.min 0] [@sop.max 16] [@sop.hard_min 0] [@sop.hard_max 64];
  } [@@sop.node_key "root"] [@@sop.node_label "Root"]
    [@@sop.node_category "Render"] [@@sop.node_inputs 0]
    [@@deriving sop_params, sop_node]

  let build = parameters_build (fun ~label _parameters ->
    Custom.plain ~label ~operation:"root" [] (fun ~context:_ _ -> Ok empty))
  let factory = parameters_factory build

  let default = parameters_default

  (* The render resolution and samples a sketch exports at. *)
  type render = { width : int; height : int; max_spp : int }

  let render (p : parameters) =
    { width = max 1 p.width; height = max 1 p.height; max_spp = max 1 p.max_spp }

  let fields (p : parameters) = Param.view parameters_schema p
end

module Light = struct
  type shape = Point | Spot | Area | Directional

  let shape_parameter = Param.choice ~equal:( = )
      ["Area", Area; "Point", Point; "Spot", Spot; "Directional", Directional]

  type parameters = {
    shape : shape [@sop.default Area] [@sop.label "Type"] [@sop.kind shape_parameter];
    translate_x : float [@sop.default 3.] [@sop.label "Translate X"]
      [@sop.folder "Transform"] [@sop.min (-20.)] [@sop.max 20.];
    translate_y : float [@sop.default 5.] [@sop.label "Translate Y"]
      [@sop.folder "Transform"] [@sop.min (-20.)] [@sop.max 20.];
    translate_z : float [@sop.default 4.] [@sop.label "Translate Z"]
      [@sop.folder "Transform"] [@sop.min (-20.)] [@sop.max 20.];
    target_x : float [@sop.default 0.] [@sop.label "Target X"]
      [@sop.folder "Aim"] [@sop.min (-20.)] [@sop.max 20.];
    target_y : float [@sop.default 0.] [@sop.label "Target Y"]
      [@sop.folder "Aim"] [@sop.min (-20.)] [@sop.max 20.];
    target_z : float [@sop.default 0.] [@sop.label "Target Z"]
      [@sop.folder "Aim"] [@sop.min (-20.)] [@sop.max 20.];
    color_r : float [@sop.default 1.] [@sop.label "Red"] [@sop.folder "Color"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.];
    color_g : float [@sop.default 1.] [@sop.label "Green"] [@sop.folder "Color"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.];
    color_b : float [@sop.default 1.] [@sop.label "Blue"] [@sop.folder "Color"]
      [@sop.min 0.] [@sop.max 1.] [@sop.hard_min 0.];
    intensity : float [@sop.default 40.] [@sop.label "Intensity"]
      [@sop.min 0.] [@sop.max 2000.] [@sop.hard_min 0.];
    width : float [@sop.default 2.] [@sop.label "Width"] [@sop.folder "Area"]
      [@sop.min 0.01] [@sop.max 20.] [@sop.hard_min 0.001];
    height : float [@sop.default 2.] [@sop.label "Height"] [@sop.folder "Area"]
      [@sop.min 0.01] [@sop.max 20.] [@sop.hard_min 0.001];
    cone : float [@sop.default 30.] [@sop.label "Cone (degrees)"] [@sop.folder "Spot"]
      [@sop.min 1.] [@sop.max 89.] [@sop.hard_min 0.1] [@sop.hard_max 89.9];
    visible : bool [@sop.default true] [@sop.label "Visible"];
    render : bool [@sop.default true] [@sop.label "Renderable"];
  } [@@sop.node_key "light"] [@@sop.node_label "Light"]
    [@@sop.node_category "Object"] [@@sop.node_inputs 1]
    [@@sop.node_optional "0"] [@@deriving sop_params, sop_node]

  let build = parameters_build (marker "light")
  let factory = parameters_factory build
end

let read schema default node =
  match Param.apply_all schema default
      (List.map (fun (field : Param.field_view) -> field.name, field.current)
        (Node.parameter_fields node)) with
  | Ok (values, _) -> Some values
  | Error _ -> None

let geometry node = if Node.operation node = "geometry"
  then read Geometry.parameters_schema Geometry.parameters_default node else None
let light node = if Node.operation node = "light"
  then read Light.parameters_schema Light.parameters_default node else None

let radians degrees = degrees *. Float.pi /. 180.

(* Translate, then rotate Z·Y·X (degrees), then scale; lights and cameras
   carry positions instead of a rotation, so only a translation. *)
let local node = match geometry node, light node with
  | Some p, _ ->
      Rays_math.Mat4.mul (Rays_math.Mat4.translation (Rays_math.Vec3.create p.translate_x p.translate_y p.translate_z))
        (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z (radians p.rotate_z))
          (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y (radians p.rotate_y))
            (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_x (radians p.rotate_x))
              (Rays_math.Mat4.scaling (Rays_math.Vec3.create p.scale_x p.scale_y p.scale_z)))))
  | None, _ -> Rays_math.Mat4.identity

let parent scene id = match Edit_graph.inputs scene ~node_id:id with
  | Some inputs when Array.length inputs > 0 -> inputs.(0)
  | Some _ | None -> None

(* The parent chain's transforms, excluding the object's own; a cycle is
   impossible because the scene network is a DAG. *)
let rec parent_world scene id = match parent scene id with
  | None -> Rays_math.Mat4.identity
  | Some parent_id ->
      (match Edit_graph.find scene ~node_id:parent_id with
       | Some node -> Rays_math.Mat4.mul (parent_world scene parent_id) (local node)
       | None -> Rays_math.Mat4.identity)

let world scene id = match Edit_graph.find scene ~node_id:id with
  | Some node -> Rays_math.Mat4.mul (parent_world scene id) (local node)
  | None -> Rays_math.Mat4.identity

let visible node = match geometry node, light node with
  | Some p, _ -> p.visible
  | None, Some p -> p.visible
  | None, None -> true

let flag name node = List.exists (fun (field : Param.field_view) ->
    field.name = name && field.current = Param.Bool_value true)
    (Node.parameter_fields node)

let has_flag name node = List.exists (fun (field : Param.field_view) ->
    field.name = name) (Node.parameter_fields node)

(* A light object as a renderer light, its positions in world space. *)
let to_light scene id =
  Option.bind (Edit_graph.find scene ~node_id:id) (fun node ->
    Option.map (fun (p : Light.parameters) ->
      let space = parent_world scene id in
      let at = Rays_math.Mat4.transform_point space
          (Rays_math.Vec3.create p.translate_x p.translate_y p.translate_z)
      and target = Rays_math.Mat4.transform_point space
          (Rays_math.Vec3.create p.target_x p.target_y p.target_z) in
      let direction = let d = Rays_math.Vec3.sub target at in
        if Rays_math.Vec3.length d < 1e-6 then Rays_math.Vec3.create 0. (-1.) 0. else Rays_math.Vec3.normalize d in
      let channel value = int_of_float (Float.round (255. *. Float.min 1. value)) in
      let diffuse = Rays_math.Color.rgb (channel p.color_r) (channel p.color_g) (channel p.color_b) in
      let intensity = p.intensity in
      (* Physical 1/d^2 falloff, so raster and the path tracer agree. *)
      let attenuation = Rays.Light.attenuation ~constant:0. ~quadratic:1. () in
      match p.shape with
      | Light.Point -> Rays.Light.point ~diffuse ~intensity ~attenuation ~at ()
      | Spot -> Rays.Light.spot ~diffuse ~intensity ~attenuation ~at ~direction
          ~cutoff:(radians p.cone) ~concentration:8. ()
      | Area -> Rays.Light.area ~diffuse ~intensity ~attenuation ~at ~direction
          ~width:p.width ~height:p.height ~samples:1 ()
          (* ponytail: one raster sample per area light keeps the viewport
             fast (2x2 cost ~3 ms at Retina); the path tracer integrates
             the true rectangle. *)
      | Directional -> Rays.Light.directional ~diffuse ~intensity ~direction ())
      (light node))

let ids operation scene = List.filter_map (fun (info : Edit_graph.node_info) ->
    if info.operation = operation then Some info.id else None) (Edit_graph.inspect scene)

let lights ?(render = false) scene = List.filter_map (fun id ->
    match Edit_graph.find scene ~node_id:id with
    | Some node when visible node && (not render || flag "render" node) -> to_light scene id
    | Some _ | None -> None) (ids "light" scene)

(* Parameters a light object gets from a renderer light. *)
let light_values (light : Rays.Light.t) =
  let float name value = name, Param.Float_value value in
  let r, g, b, _ = Rays_math.Color.to_tuple light.diffuse in
  let at, target, shape, extra = match light.kind with
    | Directional { direction } ->
        Rays_math.Vec3.scale direction (-8.), Rays_math.Vec3.zero, "Directional", []
    | Point { position; _ } -> position, Rays_math.Vec3.zero, "Point", []
    | Spot { position; direction; cutoff; _ } ->
        position, Rays_math.Vec3.add position direction, "Spot",
        [float "cone" (cutoff *. 180. /. Float.pi)]
    | Area { position; direction; width; height; _ } ->
        position, Rays_math.Vec3.add position direction, "Area",
        [float "width" width; float "height" height] in
  ["shape", Param.Choice_value shape;
   float "translate_x" at.x; float "translate_y" at.y; float "translate_z" at.z;
   float "target_x" target.x; float "target_y" target.y; float "target_z" target.z;
   float "color_r" (float_of_int r /. 255.); float "color_g" (float_of_int g /. 255.);
   float "color_b" (float_of_int b /. 255.); float "intensity" light.intensity] @ extra

let catalog = [Geometry.factory; Light.factory; Camera.factory]

(* Object parameters for a local matrix T·Rz·Ry·Rx·S (shear is dropped). *)
let decompose matrix =
  let get row column = Rays_math.Mat4.get matrix ~row ~column in
  let scale column = let length = Rays_math.Vec3.length (Rays_math.Vec3.create (get 0 column)
      (get 1 column) (get 2 column)) in if length < 1e-9 then 1. else length in
  let sx = scale 0 and sy = scale 1 and sz = scale 2 in
  let r row column = get row column /. [| sx; sy; sz |].(column) in
  let degrees angle = angle *. 180. /. Float.pi in
  let float name value = name, Param.Float_value value in
  [float "translate_x" (get 0 3); float "translate_y" (get 1 3);
   float "translate_z" (get 2 3);
   float "rotate_x" (degrees (Float.atan2 (r 2 1) (r 2 2)));
   float "rotate_y" (degrees (Float.asin (Float.max (-1.) (Float.min 1. (-. r 2 0)))));
   float "rotate_z" (degrees (Float.atan2 (r 1 0) (r 0 0)));
   float "scale_x" sx; float "scale_y" sy; float "scale_z" sz]

(* Parameters that keep [id] where it was in the world after its parent
   changed; [before] is the old parent chain's transform. *)
let keep_world scene id ~before =
  match Edit_graph.find scene ~node_id:id,
      Rays_math.Mat4.inverse (parent_world scene id) with
  | Some node, Some inverse ->
      let change = Rays_math.Mat4.mul inverse before in
      (match geometry node, light node with
       | Some _, _ -> decompose (Rays_math.Mat4.mul change (local node))
       | None, Some p ->
           let point x y z = Rays_math.Mat4.transform_point change (Rays_math.Vec3.create x y z) in
           let at = point p.translate_x p.translate_y p.translate_z
           and target = point p.target_x p.target_y p.target_z in
           let float name value = name, Param.Float_value value in
           [float "translate_x" at.x; float "translate_y" at.y; float "translate_z" at.z;
            float "target_x" target.x; float "target_y" target.y;
            float "target_z" target.z]
       | None, None -> [])
  | _ -> []

(* [scene] with [ids] rewired under [under] (None: the scene root), each keeping its world
   placement. *)
let reparent scene ids under =
  List.fold_left (fun scene id ->
    let before = parent_world scene id in
    let rewired = match under with
      | Some above when above <> id -> Edit_graph.connect ~source:above ~consumer:id ~input_index:0 scene
      | Some _ -> Ok scene
      | None when parent scene id <> None ->
          Edit_graph.disconnect ~consumer:id ~input_index:0 scene
      | None -> Ok scene in
    match rewired with
    | Error _ -> scene
    | Ok scene ->
        (match keep_world scene id ~before with
         | [] -> scene
         | values -> (match Edit_graph.apply_parameters scene ~node_id:id values with
           | Ok (scene, _) -> scene | Error _ -> scene))) scene ids

(* each of [ids] moved out to its parent's parent *)
let outdent scene ids =
  List.fold_left (fun scene id -> match parent scene id with
    | Some above -> reparent scene [ id ] (parent scene above)
    | None -> scene) scene ids
