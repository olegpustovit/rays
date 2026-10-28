open Rays
open Editor_document
open Sop

(* The World's camera map as an sRGB image: the path tracer's exposure,
   ACES fit, and gamma, so the map reads like the renders. *)
let map_rgba (baked : World.baked) =
  let map = baked.camera in
  let step = max 1 (map.width / 1024) in
  let width = map.width / step and height = map.height / step in
  let rgba = Bytes.create (width * height * 4) in
  let exposure = 2. ** baked.exposure in
  let channel value =
    let c = Rays_math.Color.aces (value *. exposure) in
    Char.chr (int_of_float ((Float.pow (Float.max 0. (Float.min 1. c)) (1. /. 2.2) *. 255.) +. 0.5))
  in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let source = 3 * ((y * step * map.width) + (x * step)) and target = 4 * ((y * width) + x) in
      for c = 0 to 2 do
        Bytes.set rgba (target + c) (channel (Float.Array.get map.pixels (source + c)))
      done;
      Bytes.set rgba (target + 3) '\255'
    done
  done;
  (width, height, rgba)

(* The map fitted at 2:1 in the view pane, and the texel under a point. *)
let map_rect (x, y, width, height) =
  let w = min width (2 * height) in
  let h = w / 2 in
  (x + ((width - w) / 2), y + ((height - h) / 2), w, h)

let map_uv viewport (px, py) =
  let x, y, w, h = map_rect viewport in
  let u = (px -. float x) /. float (max 1 w) and v = (py -. float y) /. float (max 1 h) in
  if u < 0. || u > 1. || v < 0. || v > 1. then None else Some (u, v)

type world_operation = Rotate_world of int | Move_layer of int * int | Move_sun of int

type world_drag = {
  operation : world_operation;
  point : float * float;
  area : Pxui_shell.Layout.bounds;
}

let wrap_degrees value = Float.rem (Float.rem (value +. 180.) 360. +. 360.) 360. -. 180.

let world_operation core area point ~shift =
  match Editor_host.world_id core with
  | Some world when core.Editor_model.map_view && map_uv area point <> None -> (
      match Editor_list.selected_node core with
      | Some node
        when List.exists
               (fun (field : Param.field_view) -> field.name = "azimuth")
               node.Editor_model.fields ->
          Some (Move_layer (world, node.id))
      | Some node when node.kind = "sun" -> Some (Move_sun world)
      | _ -> None)
  | Some world when (not core.Editor_model.map_view) && shift -> Some (Rotate_world world)
  | _ -> None

let move_world core drag point =
  if point = drag.point then core
  else
    match drag.operation with
    | Rotate_world world -> (
        match
          Option.bind
            (Kind_graph.find (Editor_inspect.scene core) world)
            (fun node ->
              match Kind_graph.field node "rotation" with
              | Some (Param.Float_value value) -> Some value
              | _ -> None)
        with
        | Some rotation ->
            let rotation = wrap_degrees (rotation +. (0.5 *. (fst point -. fst drag.point))) in
            Editor_host.edit_node core Document.Scene world ~label:"Rotate World"
              [ ("rotation", Param.Float_value rotation) ]
        | None -> core)
    | Move_layer (world, _) | Move_sun world -> (
        let x, y, w, h = map_rect drag.area in
        let u = Float.max 0. (Float.min 1. ((fst point -. float x) /. float (max 1 w)))
        and v = Float.max 0. (Float.min 1. ((snd point -. float y) /. float (max 1 h))) in
        let direction = World.direction_of_uv u v in
        let rotation =
          match Editor_host.world core ~time:0. with
          | Some world -> world.World.rotation
          | None -> 0.
        in
        let degrees radians = radians *. 180. /. Float.pi in
        let azimuth =
          wrap_degrees
            (degrees (Float.atan2 direction.Rays_math.Vec3.x (-.direction.z) -. rotation))
        and elevation = degrees (Float.asin direction.y) in
        match drag.operation with
        | Move_layer (_, id) ->
            Editor_host.edit_node core (Document.Inside world) id ~label:"Move layer"
              [ ("azimuth", Param.Float_value azimuth); ("elevation", Param.Float_value elevation) ]
        | Move_sun _ ->
            Editor_host.edit_node core Document.Scene world ~label:"Move sun"
              [
                ("sun_linked", Param.Bool_value false);
                ("sun_azimuth", Param.Float_value azimuth);
                ("sun_elevation", Param.Float_value (Float.max (-10.) elevation));
              ]
        | Rotate_world _ -> assert false)
