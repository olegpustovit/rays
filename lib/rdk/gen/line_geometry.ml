open Rdk_core

let points values =
  let count = Array.length values in
  let builder = Packed.Float3.Builder.create count in
  Array.iteri (fun index (x, y, z) ->
    Packed.Float3.Builder.set builder index x y z) values;
  let positions = Packed.Float3.Builder.freeze builder in
  Geometry.create ~positions ~topology:(Topology.empty ~point_count:count) ()
  |> Support.get_ok

let polyline ?(closed = false) values =
  Error.guard ~operation:"polyline" ~code:"invalid_parameter" @@ fun () ->
  let count = Array.length values in
  let minimum = if closed then 3 else 2 in
  if count < minimum then Error (Printf.sprintf
      "Rdk.Line_geometry.polyline: %s polylines require at least %d points"
      (if closed then "closed" else "open") minimum)
  else if Array.exists (fun (x, y, z) -> not (Float.is_finite x && Float.is_finite y && Float.is_finite z)) values
  then Error "Rdk.Line_geometry.polyline: positions must be finite"
  else
    let geometry = points values in
    let topology = Topology.Builder.create ~point_count:count
        ~vertex_capacity:count ~primitive_capacity:1 () in
    let indices = Array.init count Fun.id in
    if closed then Topology.Builder.add_closed_polyline topology indices
    else Topology.Builder.add_open_polyline topology indices;
    Geometry.create ~positions:(Geometry.positions geometry)
      ~topology:(Topology.Builder.freeze topology) ()
