open Rdk_core
open Rays_math

type kind = Line_curve | Line_points

let points values =
  let count = Array.length values in
  let builder = Packed.Float3.Builder.create count in
  Array.iteri (fun index (x, y, z) ->
    Packed.Float3.Builder.set builder index x y z) values;
  let positions = Packed.Float3.Builder.freeze builder in
  Geometry.create ~positions ~topology:(Topology.empty ~point_count:count) ()
  |> Support.get_ok

let line ?cancel ?(grain = 16_384) ?(kind = Line_curve) ?(points = 2)
    ~origin ~direction ~length () =
  Error.guard ~operation:"line" ~code:"invalid_parameter" @@ fun () ->
  let minimum = match kind with Line_curve -> 2 | Line_points -> 1 in
  if grain <= 0 then Error "Rdk.Line_geometry.line: grain must be positive"
  else if points < minimum then Error (Printf.sprintf
      "Rdk.Line_geometry.line: %s output requires at least %d points"
      (match kind with Line_curve -> "curve" | Line_points -> "point") minimum)
  else if not (Float.is_finite origin.Rays_math.Vec3.x && Float.is_finite origin.y && Float.is_finite origin.z
      && Float.is_finite direction.Rays_math.Vec3.x && Float.is_finite direction.y && Float.is_finite direction.z
      && Float.is_finite length && length >= 0.) then
    Error "Rdk.Line_geometry.line: origin/direction must be finite and length finite and non-negative"
  else
    let scale = max (abs_float direction.x)
        (max (abs_float direction.y) (abs_float direction.z)) in
    if scale = 0. then Error "Rdk.Line_geometry.line: direction must be non-zero"
    else
      let sx = direction.x /. scale and sy = direction.y /. scale
      and sz = direction.z /. scale in
      let magnitude = sqrt ((sx *. sx) +. (sy *. sy) +. (sz *. sz)) in
      let dx = (sx /. magnitude) *. length
      and dy = (sy /. magnitude) *. length
      and dz = (sz /. magnitude) *. length in
      let end_x = origin.x +. dx and end_y = origin.y +. dy
      and end_z = origin.z +. dz in
      if not (Float.is_finite end_x && Float.is_finite end_y && Float.is_finite end_z) then
        Error "Rdk.Line_geometry.line: endpoint is not finite"
      else begin
        let x = Array.make points 0. and y = Array.make points 0.
        and z = Array.make points 0.
        and vertex_points = match kind with
          | Line_curve -> Some (Array.make points 0)
          | Line_points -> None in
        Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(points - 1)
          (fun point ->
            if point land 16_383 = 0 then Cancel.check_opt cancel;
            let t = if points = 1 then 0.
              else float_of_int point /. float_of_int (points - 1) in
            x.(point) <- origin.x +. (dx *. t);
            y.(point) <- origin.y +. (dy *. t);
            z.(point) <- origin.z +. (dz *. t);
            match vertex_points with
            | None -> () | Some values -> values.(point) <- point);
        x.(0) <- origin.x; y.(0) <- origin.y; z.(0) <- origin.z;
        if points > 1 then begin
          x.(points - 1) <- end_x;
          y.(points - 1) <- end_y;
          z.(points - 1) <- end_z
        end;
        let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
        let topology = match kind with
          | Line_points -> Topology.empty ~point_count:points
          | Line_curve -> Topology.Private.create_validated_owned
              ~point_count:points ~vertex_points:(Option.get vertex_points)
              ~primitive_offsets:[|0; points|]
              ~primitive_kinds:(Bytes.make 1 '\001') in
        Geometry.create ~positions ~topology ()
      end

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
