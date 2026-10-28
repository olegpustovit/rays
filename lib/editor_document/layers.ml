open Rays

(* The World: a scene object holding its global settings, entered like a geometry object to edit
   its layer stack.  Layers are plain instances ([Kind_graph]) whose input is the layer below;
   [to_world] reads the chain into a [World.t].  Angles are degrees in the inspector and radians
   in [World]. *)
module G = Kind_graph

let emit_parameter = Param.choice ~equal:( = ) [ ("Dome", World.Dome); ("Light", World.Light) ]

module Settings = struct
  type background = Environment | Color | Transparent

  let background_parameter =
    Param.choice ~equal:( = )
      [ ("Environment", Environment); ("Color", Color); ("Transparent", Transparent) ]

  type parameters = {
    background : background;
        [@sop.default Environment] [@sop.label "Background"] [@sop.kind background_parameter]
    background_r : float;
        [@sop.default 0.05]
        [@sop.label "Red"]
        [@sop.folder "Background color"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    background_g : float;
        [@sop.default 0.05]
        [@sop.label "Green"]
        [@sop.folder "Background color"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    background_b : float;
        [@sop.default 0.06]
        [@sop.label "Blue"]
        [@sop.folder "Background color"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    rotation : float;
        [@sop.default 0.] [@sop.label "Rotation (degrees)"] [@sop.min -180.] [@sop.max 180.]
    exposure : float; [@sop.default 0.] [@sop.label "Exposure (stops)"] [@sop.min -6.] [@sop.max 6.]
    time_of_day : float;
        [@sop.default 15.5]
        [@sop.label "Time of day (hours)"]
        [@sop.folder "Sun"]
        [@sop.min 0.]
        [@sop.max 24.]
        [@sop.hard_min 0.]
        [@sop.hard_max 24.]
    day_cycle : float;
        [@sop.default 0.]
        [@sop.label "Day cycle (hours per second)"]
        [@sop.folder "Sun"]
        [@sop.min 0.]
        [@sop.max 4.]
        [@sop.hard_min 0.]
    latitude : float;
        [@sop.default 45.]
        [@sop.label "Latitude (degrees)"]
        [@sop.folder "Sun"]
        [@sop.min -90.]
        [@sop.max 90.]
        [@sop.hard_min -90.]
        [@sop.hard_max 90.]
    day_of_year : int;
        [@sop.default 172]
        [@sop.label "Day of year"]
        [@sop.folder "Sun"]
        [@sop.min 1]
        [@sop.max 365]
    sun_linked : bool; [@sop.default true] [@sop.label "Sun follows time"] [@sop.folder "Sun"]
    sun_azimuth : float;
        [@sop.default 30.]
        [@sop.label "Sun azimuth (degrees)"]
        [@sop.folder "Sun"]
        [@sop.min -180.]
        [@sop.max 180.]
    sun_elevation : float;
        [@sop.default 35.]
        [@sop.label "Sun elevation (degrees)"]
        [@sop.folder "Sun"]
        [@sop.min -10.]
        [@sop.max 90.]
  }
  [@@deriving sop_params]

  let kind =
    Kind_graph.Kind.make ~key:"world" ~label:"World" ~category:[ "Object" ] parameters_schema
end

module Gradient = struct
  type parameters = {
    zenith_r : float;
        [@sop.default 0.35]
        [@sop.label "Red"]
        [@sop.folder "Zenith"]
        [@sop.min 0.]
        [@sop.max 4.]
        [@sop.hard_min 0.]
    zenith_g : float;
        [@sop.default 0.45]
        [@sop.label "Green"]
        [@sop.folder "Zenith"]
        [@sop.min 0.]
        [@sop.max 4.]
        [@sop.hard_min 0.]
    zenith_b : float;
        [@sop.default 0.6]
        [@sop.label "Blue"]
        [@sop.folder "Zenith"]
        [@sop.min 0.]
        [@sop.max 4.]
        [@sop.hard_min 0.]
    horizon_r : float;
        [@sop.default 0.6]
        [@sop.label "Red"]
        [@sop.folder "Horizon"]
        [@sop.min 0.]
        [@sop.max 4.]
        [@sop.hard_min 0.]
    horizon_g : float;
        [@sop.default 0.6]
        [@sop.label "Green"]
        [@sop.folder "Horizon"]
        [@sop.min 0.]
        [@sop.max 4.]
        [@sop.hard_min 0.]
    horizon_b : float;
        [@sop.default 0.62]
        [@sop.label "Blue"]
        [@sop.folder "Horizon"]
        [@sop.min 0.]
        [@sop.max 4.]
        [@sop.hard_min 0.]
    nadir_r : float;
        [@sop.default 0.12]
        [@sop.label "Red"]
        [@sop.folder "Nadir"]
        [@sop.min 0.]
        [@sop.max 4.]
        [@sop.hard_min 0.]
    nadir_g : float;
        [@sop.default 0.11]
        [@sop.label "Green"]
        [@sop.folder "Nadir"]
        [@sop.min 0.]
        [@sop.max 4.]
        [@sop.hard_min 0.]
    nadir_b : float;
        [@sop.default 0.1]
        [@sop.label "Blue"]
        [@sop.folder "Nadir"]
        [@sop.min 0.]
        [@sop.max 4.]
        [@sop.hard_min 0.]
    sharpness : float;
        [@sop.default 2.]
        [@sop.label "Sharpness"]
        [@sop.min 0.1]
        [@sop.max 10.]
        [@sop.hard_min 0.01]
    visible : bool; [@sop.default true] [@sop.label "Visible"]
  }
  [@@deriving sop_params]

  let kind =
    Kind_graph.Kind.make ~key:"gradient" ~label:"Gradient" ~category:[ "Layer" ] parameters_schema
end

module Sky = struct
  type parameters = {
    turbidity : float;
        [@sop.default 2.] [@sop.label "Turbidity"] [@sop.min 1.] [@sop.max 10.] [@sop.hard_min 0.]
    intensity : float;
        [@sop.default 1.] [@sop.label "Intensity"] [@sop.min 0.] [@sop.max 10.] [@sop.hard_min 0.]
    visible : bool; [@sop.default true] [@sop.label "Visible"]
  }
  [@@deriving sop_params]

  let kind = Kind_graph.Kind.make ~key:"sky" ~label:"Sky" ~category:[ "Layer" ] parameters_schema
end

module Sun = struct
  type parameters = {
    angular_radius : float;
        [@sop.default 0.27]
        [@sop.label "Radius (degrees)"]
        [@sop.min 0.05]
        [@sop.max 5.]
        [@sop.hard_min 0.01]
    intensity : float;
        [@sop.default 2000.]
        [@sop.label "Intensity"]
        [@sop.min 0.]
        [@sop.max 50000.]
        [@sop.hard_min 0.]
    tint_r : float;
        [@sop.default 1.]
        [@sop.label "Red"]
        [@sop.folder "Tint"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    tint_g : float;
        [@sop.default 0.95]
        [@sop.label "Green"]
        [@sop.folder "Tint"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    tint_b : float;
        [@sop.default 0.88]
        [@sop.label "Blue"]
        [@sop.folder "Tint"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    visible : bool; [@sop.default true] [@sop.label "Visible"]
  }
  [@@deriving sop_params]

  let kind = Kind_graph.Kind.make ~key:"sun" ~label:"Sun" ~category:[ "Layer" ] parameters_schema
end

module Shape = struct
  let shape_parameter =
    Param.choice ~equal:( = )
      [
        ("Rect", World.Rect); ("Strip", World.Strip); ("Circle", World.Circle); ("Ring", World.Ring);
      ]

  type parameters = {
    shape : World.shape; [@sop.default World.Rect] [@sop.label "Shape"] [@sop.kind shape_parameter]
    azimuth : float;
        [@sop.default 30.] [@sop.label "Azimuth (degrees)"] [@sop.min -180.] [@sop.max 180.]
    elevation : float;
        [@sop.default 35.] [@sop.label "Elevation (degrees)"] [@sop.min -90.] [@sop.max 90.]
    width : float;
        [@sop.default 30.]
        [@sop.label "Width (degrees)"]
        [@sop.min 0.5]
        [@sop.max 360.]
        [@sop.hard_min 0.01]
    height : float;
        [@sop.default 20.]
        [@sop.label "Height (degrees)"]
        [@sop.min 0.5]
        [@sop.max 180.]
        [@sop.hard_min 0.01]
    softness : float;
        [@sop.default 2.]
        [@sop.label "Softness (degrees)"]
        [@sop.min 0.]
        [@sop.max 30.]
        [@sop.hard_min 0.]
    color_r : float;
        [@sop.default 1.]
        [@sop.label "Red"]
        [@sop.folder "Color"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    color_g : float;
        [@sop.default 1.]
        [@sop.label "Green"]
        [@sop.folder "Color"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    color_b : float;
        [@sop.default 1.]
        [@sop.label "Blue"]
        [@sop.folder "Color"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    intensity : float;
        [@sop.default 8.] [@sop.label "Intensity"] [@sop.min 0.] [@sop.max 100.] [@sop.hard_min 0.]
    emit : World.emit; [@sop.default World.Dome] [@sop.label "Emit as"] [@sop.kind emit_parameter]
    visible : bool; [@sop.default true] [@sop.label "Visible"]
  }
  [@@deriving sop_params]

  let kind =
    Kind_graph.Kind.make ~key:"shape" ~label:"Shape" ~category:[ "Layer" ] parameters_schema
end

module Scatter = struct
  type parameters = {
    seed : int; [@sop.default 1] [@sop.label "Seed"] [@sop.min 0] [@sop.max 9999]
    count : int; [@sop.default 24] [@sop.label "Count"] [@sop.min 1] [@sop.max 400]
    elevation_min : float;
        [@sop.default 5.]
        [@sop.label "Elevation min (degrees)"]
        [@sop.folder "Band"]
        [@sop.min -90.]
        [@sop.max 90.]
    elevation_max : float;
        [@sop.default 60.]
        [@sop.label "Elevation max (degrees)"]
        [@sop.folder "Band"]
        [@sop.min -90.]
        [@sop.max 90.]
    size_min : float;
        [@sop.default 2.]
        [@sop.label "Size min (degrees)"]
        [@sop.folder "Band"]
        [@sop.min 0.1]
        [@sop.max 45.]
        [@sop.hard_min 0.01]
    size_max : float;
        [@sop.default 9.]
        [@sop.label "Size max (degrees)"]
        [@sop.folder "Band"]
        [@sop.min 0.1]
        [@sop.max 45.]
        [@sop.hard_min 0.01]
    first_r : float;
        [@sop.default 1.]
        [@sop.label "Red"]
        [@sop.folder "Palette A"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    first_g : float;
        [@sop.default 0.55]
        [@sop.label "Green"]
        [@sop.folder "Palette A"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    first_b : float;
        [@sop.default 0.35]
        [@sop.label "Blue"]
        [@sop.folder "Palette A"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    second_r : float;
        [@sop.default 0.4]
        [@sop.label "Red"]
        [@sop.folder "Palette B"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    second_g : float;
        [@sop.default 0.6]
        [@sop.label "Green"]
        [@sop.folder "Palette B"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    second_b : float;
        [@sop.default 1.]
        [@sop.label "Blue"]
        [@sop.folder "Palette B"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    intensity : float;
        [@sop.default 4.] [@sop.label "Intensity"] [@sop.min 0.] [@sop.max 100.] [@sop.hard_min 0.]
    mirror : bool; [@sop.default false] [@sop.label "Floor mirror"]
    emit : World.emit; [@sop.default World.Dome] [@sop.label "Emit as"] [@sop.kind emit_parameter]
    visible : bool; [@sop.default true] [@sop.label "Visible"]
  }
  [@@deriving sop_params]

  let kind =
    Kind_graph.Kind.make ~key:"scatter" ~label:"Scatter" ~category:[ "Layer" ] parameters_schema
end

module Room = struct
  type parameters = {
    width : float;
        [@sop.default 12.] [@sop.label "Width"] [@sop.min 1.] [@sop.max 100.] [@sop.hard_min 0.1]
    depth : float;
        [@sop.default 16.] [@sop.label "Depth"] [@sop.min 1.] [@sop.max 100.] [@sop.hard_min 0.1]
    height : float;
        [@sop.default 5.] [@sop.label "Height"] [@sop.min 1.] [@sop.max 40.] [@sop.hard_min 0.1]
    rows : int; [@sop.default 8] [@sop.label "Panel rows"] [@sop.min 1] [@sop.max 32]
    columns : int; [@sop.default 6] [@sop.label "Panel columns"] [@sop.min 1] [@sop.max 32]
    panel : float;
        [@sop.default 0.6]
        [@sop.label "Panel fraction"]
        [@sop.min 0.05]
        [@sop.max 1.]
        [@sop.hard_min 0.]
        [@sop.hard_max 1.]
    intensity : float;
        [@sop.default 6.] [@sop.label "Intensity"] [@sop.min 0.] [@sop.max 100.] [@sop.hard_min 0.]
    wall_r : float;
        [@sop.default 0.8]
        [@sop.label "Red"]
        [@sop.folder "Walls"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    wall_g : float;
        [@sop.default 0.8]
        [@sop.label "Green"]
        [@sop.folder "Walls"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    wall_b : float;
        [@sop.default 0.8]
        [@sop.label "Blue"]
        [@sop.folder "Walls"]
        [@sop.min 0.]
        [@sop.max 1.]
        [@sop.hard_min 0.]
    cove : float;
        [@sop.default 1.5] [@sop.label "Cove glow"] [@sop.min 0.] [@sop.max 20.] [@sop.hard_min 0.]
    emit : World.emit;
        [@sop.default World.Dome] [@sop.label "Panels emit as"] [@sop.kind emit_parameter]
    visible : bool; [@sop.default true] [@sop.label "Visible"]
  }
  [@@deriving sop_params]

  let kind = Kind_graph.Kind.make ~key:"room" ~label:"Room" ~category:[ "Layer" ] parameters_schema
end

let kinds = [ Gradient.kind; Sky.kind; Sun.kind; Shape.kind; Scatter.kind; Room.kind ]

(* ---- nodes <-> World ---- *)

let radians degrees = degrees *. Float.pi /. 180.
let degrees radians = radians *. 180. /. Float.pi
let read schema default node = Objects.read schema default node

let layer (node : G.node) : World.entry option =
  let entry layer visible = Some { World.name = node.label; layer; visible } in
  match node.kind with
  | "gradient" ->
      Option.bind (read Gradient.parameters_schema Gradient.parameters_default node) (fun p ->
          entry
            (World.Gradient
               {
                 zenith = World.rgb p.zenith_r p.zenith_g p.zenith_b;
                 horizon = World.rgb p.horizon_r p.horizon_g p.horizon_b;
                 nadir = World.rgb p.nadir_r p.nadir_g p.nadir_b;
                 sharpness = p.sharpness;
               })
            p.visible)
  | "sky" ->
      Option.bind (read Sky.parameters_schema Sky.parameters_default node) (fun p ->
          entry (World.Sky { turbidity = p.turbidity; intensity = p.intensity }) p.visible)
  | "sun" ->
      Option.bind (read Sun.parameters_schema Sun.parameters_default node) (fun p ->
          entry
            (World.Sun
               {
                 angular_radius = radians p.angular_radius;
                 intensity = p.intensity;
                 tint = World.rgb p.tint_r p.tint_g p.tint_b;
               })
            p.visible)
  | "shape" ->
      Option.bind (read Shape.parameters_schema Shape.parameters_default node) (fun p ->
          entry
            (World.Shape
               {
                 shape = p.shape;
                 azimuth = radians p.azimuth;
                 elevation = radians p.elevation;
                 width = radians p.width;
                 height = radians p.height;
                 softness = radians p.softness;
                 color = World.rgb p.color_r p.color_g p.color_b;
                 intensity = p.intensity;
                 emit = p.emit;
               })
            p.visible)
  | "scatter" ->
      Option.bind (read Scatter.parameters_schema Scatter.parameters_default node) (fun p ->
          entry
            (World.Scatter
               {
                 seed = p.seed;
                 count = p.count;
                 elevation_min = radians p.elevation_min;
                 elevation_max = radians p.elevation_max;
                 size_min = radians p.size_min;
                 size_max = radians p.size_max;
                 palette =
                   [
                     World.rgb p.first_r p.first_g p.first_b;
                     World.rgb p.second_r p.second_g p.second_b;
                   ];
                 intensity = p.intensity;
                 mirror = p.mirror;
                 emit = p.emit;
               })
            p.visible)
  | "room" ->
      Option.bind (read Room.parameters_schema Room.parameters_default node) (fun p ->
          entry
            (World.Room
               {
                 width = p.width;
                 depth = p.depth;
                 height = p.height;
                 rows = p.rows;
                 columns = p.columns;
                 panel = p.panel;
                 intensity = p.intensity;
                 wall = World.rgb p.wall_r p.wall_g p.wall_b;
                 cove = p.cove;
                 emit = p.emit;
               })
            p.visible)
  | _ -> None

(* The World a world object and its layers describe;
   [time] (seconds) advances the time of day by the node's day cycle. *)
let to_world ?(time = 0.) node layers =
  Option.map
    (fun (p : Settings.parameters) ->
      let hours = Float.rem (p.time_of_day +. (time *. p.day_cycle)) 24. in
      {
        World.layers = List.filter_map layer (List.rev (G.stack layers));
        background =
          (match p.background with
          | Environment -> World.Environment
          | Color -> World.Color (World.rgb p.background_r p.background_g p.background_b)
          | Transparent -> World.Transparent);
        rotation = radians p.rotation;
        exposure = p.exposure;
        time_of_day = (if hours < 0. then hours +. 24. else hours);
        latitude = radians p.latitude;
        day_of_year = p.day_of_year;
        sun =
          (if p.sun_linked then `Linked else `Manual (radians p.sun_azimuth, radians p.sun_elevation));
      })
    (read Settings.parameters_schema Settings.parameters_default node)

(* Nodes (bottom first) and world-node values for a [World.t], e.g. a
   preset: the inverse of [to_world]. *)
let of_world (world : World.t) =
  let float name value = (name, Param.Float_value value) in
  let rgb prefix (c : World.rgb) =
    [ float (prefix ^ "_r") c.r; float (prefix ^ "_g") c.g; float (prefix ^ "_b") c.b ]
  in
  let choice name value = (name, Param.Choice_value value) in
  let emit = function World.Dome -> "Dome" | Light -> "Light" in
  let layer (entry : World.entry) =
    let key, values =
      match entry.layer with
      | World.Gradient g ->
          ( Gradient.kind,
            rgb "zenith" g.zenith @ rgb "horizon" g.horizon @ rgb "nadir" g.nadir
            @ [ float "sharpness" g.sharpness ] )
      | Sky s -> (Sky.kind, [ float "turbidity" s.turbidity; float "intensity" s.intensity ])
      | Sun s ->
          ( Sun.kind,
            [ float "angular_radius" (degrees s.angular_radius); float "intensity" s.intensity ]
            @ rgb "tint" s.tint )
      | Shape s ->
          ( Shape.kind,
            [
              choice "shape"
                (match s.shape with
                | Rect -> "Rect"
                | Strip -> "Strip"
                | Circle -> "Circle"
                | Ring -> "Ring");
              float "azimuth" (degrees s.azimuth);
              float "elevation" (degrees s.elevation);
              float "width" (degrees s.width);
              float "height" (degrees s.height);
              float "softness" (degrees s.softness);
              float "intensity" s.intensity;
              choice "emit" (emit s.emit);
            ]
            @ rgb "color" s.color )
      | Scatter s ->
          ( Scatter.kind,
            [
              ("seed", Param.Int_value s.seed);
              ("count", Param.Int_value s.count);
              float "elevation_min" (degrees s.elevation_min);
              float "elevation_max" (degrees s.elevation_max);
              float "size_min" (degrees s.size_min);
              float "size_max" (degrees s.size_max);
              float "intensity" s.intensity;
              ("mirror", Param.Bool_value s.mirror);
              choice "emit" (emit s.emit);
            ]
            @
            match s.palette with
            | first :: second :: _ -> rgb "first" first @ rgb "second" second
            | [ only ] -> rgb "first" only @ rgb "second" only
            | [] -> [] )
      | Room r ->
          ( Room.kind,
            [
              float "width" r.width;
              float "depth" r.depth;
              float "height" r.height;
              ("rows", Param.Int_value r.rows);
              ("columns", Param.Int_value r.columns);
              float "panel" r.panel;
              float "intensity" r.intensity;
              float "cove" r.cove;
              choice "emit" (emit r.emit);
            ]
            @ rgb "wall" r.wall )
    in
    (key, entry.name, ("visible", Param.Bool_value entry.visible) :: values)
  in
  let settings =
    [
      choice "background"
        (match world.background with
        | Environment -> "Environment"
        | Color _ -> "Color"
        | Transparent -> "Transparent");
      float "rotation" (degrees world.rotation);
      float "exposure" world.exposure;
      float "time_of_day" world.time_of_day;
      float "latitude" (degrees world.latitude);
      ("day_of_year", Param.Int_value world.day_of_year);
      ("sun_linked", Param.Bool_value (world.sun = `Linked));
    ]
    @ (match world.background with
      | Color c -> rgb "background" c
      | Environment | Transparent -> [])
    @
    match world.sun with
    | `Manual (azimuth, elevation) ->
        [ float "sun_azimuth" (degrees azimuth); float "sun_elevation" (degrees elevation) ]
    | `Linked -> []
  in
  (List.map layer world.layers, settings)

(* The layers of [world], each on the one below it. *)
let graph_of_world world =
  let layers, _ = of_world world in
  let ( let* ) = Result.bind in
  let* graph, _ =
    List.fold_left
      (fun state (kind, name, values) ->
        let* graph, below = state in
        let* node, _ = G.apply { (G.instance ~label:name kind) with input = below } values in
        Ok (G.add graph node, Some node.G.id))
      (Ok (G.empty, None))
      layers
  in
  Ok graph
