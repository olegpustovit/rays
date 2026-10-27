open Rays_math

type deform_selection = Deform.selection =
  | Selected_points of Group.t
  | Selected_vertices of Group.t
  | Selected_primitives of Group.t
  | Selected_edges of Edge_group.t

let transform = Transform_ops.transform
let uv_sphere = Uv_sphere.run

let finite_vec3 value = Float.is_finite value.Rays_math.Vec3.x && Float.is_finite value.y && Float.is_finite value.z

type bound_shape =
  | Bound_box of { divisions : int * int * int }
  | Bound_sphere of { segments : int; rings : int; minimum_radius : float }

type bound_face = {
  u_divisions : int;
  v_divisions : int;
  origin_x : float; origin_y : float; origin_z : float;
  u_x : float; u_y : float; u_z : float;
  v_x : float; v_y : float; v_z : float;
  normal_x : float; normal_y : float; normal_z : float;
}

let selected_bounds ?cancel ~grain ~operation selection geometry =
  let topology = Geometry.topology geometry in
  match Deform.validate_selection topology selection with
  | Error message -> Error ("Rdk.Bound." ^ operation ^ ": " ^ message)
  | Ok () ->
      let point_count = Geometry.point_count geometry in
      let needs_index = Deform.selection_needs_index selection in
      let index = if needs_index then
          Some (Topology_index.create ?cancel topology) else None in
      let parallel_work = point_count >= 2_000_000
        || needs_index
           && Topology.vertex_count topology >= max 0 (2_000_000 - point_count) in
      let range_grain = if parallel_work then grain else max 1 point_count in
      let range_count = if point_count = 0 then 0
        else (point_count + range_grain - 1) / range_grain in
      let min_x = Array.make range_count Float.infinity
      and min_y = Array.make range_count Float.infinity
      and min_z = Array.make range_count Float.infinity
      and max_x = Array.make range_count Float.neg_infinity
      and max_y = Array.make range_count Float.neg_infinity
      and max_z = Array.make range_count Float.neg_infinity
      and found = Array.make range_count false
      and errors = Array.make range_count (-1) in
      let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
      if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
          ~finish:(range_count - 1) (fun range ->
        let first = range * range_grain
        and last = min point_count ((range + 1) * range_grain) in
        for point = first to last - 1 do
          if point land 4095 = 0 then Cancel.check_opt cancel;
          if Deform.point_selected selection index point then begin
            let x = positions.x.(point) and y = positions.y.(point)
            and z = positions.z.(point) in
            if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
              errors.(range) <- if errors.(range) < 0 then point
                else errors.(range)
            else begin
              found.(range) <- true;
              min_x.(range) <- Float.min min_x.(range) x;
              min_y.(range) <- Float.min min_y.(range) y;
              min_z.(range) <- Float.min min_z.(range) z;
              max_x.(range) <- Float.max max_x.(range) x;
              max_y.(range) <- Float.max max_y.(range) y;
              max_z.(range) <- Float.max max_z.(range) z
            end
          end
        done);
      let invalid = Array.fold_left (fun first point ->
          if point < 0 then first else if first < 0 then point
          else min first point) (-1) errors in
      if invalid >= 0 then Error (Printf.sprintf
          "Rdk.Bound.%s: selected point %d has a non-finite position"
          operation invalid)
      else begin
        let xmin = ref Float.infinity and ymin = ref Float.infinity
        and zmin = ref Float.infinity and xmax = ref Float.neg_infinity
        and ymax = ref Float.neg_infinity and zmax = ref Float.neg_infinity
        and any = ref false in
        for range = 0 to range_count - 1 do
          if found.(range) then begin
            any := true;
            xmin := Float.min !xmin min_x.(range);
            ymin := Float.min !ymin min_y.(range);
            zmin := Float.min !zmin min_z.(range);
            xmax := Float.max !xmax max_x.(range);
            ymax := Float.max !ymax max_y.(range);
            zmax := Float.max !zmax max_z.(range)
          end
        done;
        if not !any then Error
            ("Rdk.Bound." ^ operation ^ ": selection contains no points")
        else
          let sx = !xmax -. !xmin and sy = !ymax -. !ymin
          and sz = !zmax -. !zmin in
          let cx = !xmin +. (sx *. 0.5) and cy = !ymin +. (sy *. 0.5)
          and cz = !zmin +. (sz *. 0.5) in
          if not (Float.is_finite sx && Float.is_finite sy && Float.is_finite sz && Float.is_finite cx && Float.is_finite cy
              && Float.is_finite cz) then Error
              ("Rdk.Bound." ^ operation ^ ": selected bounds overflow")
          else Ok Analysis.{
            min = Rays_math.Vec3.create !xmin !ymin !zmin;
            max = Rays_math.Vec3.create !xmax !ymax !zmax;
            center = Rays_math.Vec3.create cx cy cz;
            size = Rays_math.Vec3.create sx sy sz;
          }
      end

let divided_box ?cancel ~minimum ~maximum ~divisions () =
  let dx, dy, dz = divisions in
  let sx = maximum.Rays_math.Vec3.x -. minimum.Rays_math.Vec3.x
  and sy = maximum.y -. minimum.y and sz = maximum.z -. minimum.z in
  let faces = [|
    { u_divisions = dy; v_divisions = dz;
      origin_x = maximum.x; origin_y = minimum.y; origin_z = minimum.z;
      u_x = 0.; u_y = sy; u_z = 0.; v_x = 0.; v_y = 0.; v_z = sz;
      normal_x = 1.; normal_y = 0.; normal_z = 0. };
    { u_divisions = dz; v_divisions = dy;
      origin_x = minimum.x; origin_y = minimum.y; origin_z = minimum.z;
      u_x = 0.; u_y = 0.; u_z = sz; v_x = 0.; v_y = sy; v_z = 0.;
      normal_x = -1.; normal_y = 0.; normal_z = 0. };
    { u_divisions = dz; v_divisions = dx;
      origin_x = minimum.x; origin_y = maximum.y; origin_z = minimum.z;
      u_x = 0.; u_y = 0.; u_z = sz; v_x = sx; v_y = 0.; v_z = 0.;
      normal_x = 0.; normal_y = 1.; normal_z = 0. };
    { u_divisions = dx; v_divisions = dz;
      origin_x = minimum.x; origin_y = minimum.y; origin_z = minimum.z;
      u_x = sx; u_y = 0.; u_z = 0.; v_x = 0.; v_y = 0.; v_z = sz;
      normal_x = 0.; normal_y = -1.; normal_z = 0. };
    { u_divisions = dx; v_divisions = dy;
      origin_x = minimum.x; origin_y = minimum.y; origin_z = maximum.z;
      u_x = sx; u_y = 0.; u_z = 0.; v_x = 0.; v_y = sy; v_z = 0.;
      normal_x = 0.; normal_y = 0.; normal_z = 1. };
    { u_divisions = dy; v_divisions = dx;
      origin_x = minimum.x; origin_y = minimum.y; origin_z = minimum.z;
      u_x = 0.; u_y = sy; u_z = 0.; v_x = sx; v_y = 0.; v_z = 0.;
      normal_x = 0.; normal_y = 0.; normal_z = -1. };
  |] in
  let point_offsets = Array.make 7 0 and primitive_offsets_by_face = Array.make 7 0 in
  let overflow = ref false in
  let point_limit = Sys.max_array_length
  and primitive_limit = (Sys.max_array_length - 1) / 3 in
  for face = 0 to 5 do
    let value = faces.(face) in
    let points = if value.u_divisions >= point_limit
        || value.v_divisions >= point_limit
        || value.u_divisions + 1 > point_limit / (value.v_divisions + 1)
      then None else Some ((value.u_divisions + 1) * (value.v_divisions + 1))
    and primitives = if value.u_divisions > primitive_limit / 2
        || value.v_divisions > primitive_limit / (2 * value.u_divisions)
      then None else Some (2 * value.u_divisions * value.v_divisions) in
    match points, primitives with
    | Some points, Some primitives
      when points <= point_limit - point_offsets.(face)
        && primitives <= primitive_limit - primitive_offsets_by_face.(face) ->
      point_offsets.(face + 1) <- point_offsets.(face) + points;
      primitive_offsets_by_face.(face + 1) <-
        primitive_offsets_by_face.(face) + primitives
    | _ -> overflow := true
  done;
  if !overflow then Error "Rdk.Bound.bound: divided box output is too large"
  else begin
    let point_count = point_offsets.(6)
    and primitive_count = primitive_offsets_by_face.(6) in
    let px = Array.make point_count 0. and py = Array.make point_count 0.
    and pz = Array.make point_count 0. and nx = Array.make point_count 0.
    and ny = Array.make point_count 0. and nz = Array.make point_count 0.
    and vertex_points = Array.make (primitive_count * 3) 0 in
    let fill_face face_index =
      Cancel.check_opt cancel;
      let face = faces.(face_index) and first_point = point_offsets.(face_index)
      and first_primitive = primitive_offsets_by_face.(face_index) in
      let width = face.v_divisions + 1 in
      for u = 0 to face.u_divisions do
        if u land 255 = 0 then Cancel.check_opt cancel;
        let fu = float_of_int u /. float_of_int face.u_divisions in
        for v = 0 to face.v_divisions do
          let fv = float_of_int v /. float_of_int face.v_divisions in
          let point = first_point + (u * width) + v in
          px.(point) <- face.origin_x +. (fu *. face.u_x) +. (fv *. face.v_x);
          py.(point) <- face.origin_y +. (fu *. face.u_y) +. (fv *. face.v_y);
          pz.(point) <- face.origin_z +. (fu *. face.u_z) +. (fv *. face.v_z);
          nx.(point) <- face.normal_x; ny.(point) <- face.normal_y;
          nz.(point) <- face.normal_z
        done
      done;
      for u = 0 to face.u_divisions - 1 do
        if u land 255 = 0 then Cancel.check_opt cancel;
        for v = 0 to face.v_divisions - 1 do
          let a = first_point + (u * width) + v in
          let b = a + width and d = a + 1 and c = a + width + 1 in
          let primitive = first_primitive
              + (2 * ((u * face.v_divisions) + v)) in
          let at = primitive * 3 in
          vertex_points.(at) <- a; vertex_points.(at + 1) <- b;
          vertex_points.(at + 2) <- c;
          vertex_points.(at + 3) <- a; vertex_points.(at + 4) <- c;
          vertex_points.(at + 5) <- d
        done
      done in
    if point_count + primitive_count < 500_000 then
      for face = 0 to 5 do fill_face face done
    else Parallel.for_ ~chunk_size:1 ~start:0 ~finish:5 fill_face;
    let primitive_offsets = Array.make (primitive_count + 1) 0
    and primitive_kinds = Bytes.make primitive_count '\000' in
    let fill_offset primitive = primitive_offsets.(primitive) <- primitive * 3 in
    if primitive_count < 500_000 then
      for primitive = 0 to primitive_count do fill_offset primitive done
    else Parallel.for_ ~chunk_size:16_384 ~start:0 ~finish:primitive_count
        fill_offset;
    let topology = Topology.Private.create_validated_owned ~point_count
        ~vertex_points ~primitive_offsets ~primitive_kinds in
    let positions = Packed.Float3.Private.of_owned_exn ~x:px ~y:py ~z:pz in
    let normals = Packed.Float3.Private.of_owned_exn ~x:nx ~y:ny ~z:nz in
    let normal = Attribute.create_key_owned (Attribute.normal ~owner:Attribute.Point)
        normals |> Support.get_ok in
    Geometry.create ~positions ~topology ~attributes:[normal] ()
  end

let bound ?cancel ?(grain = 16_384) ?selection
    ?(shape = Bound_box { divisions = 1, 1, 1 })
    ?(lower_padding = Rays_math.Vec3.zero) ?(upper_padding = Rays_math.Vec3.zero) ?bounds_group
    ?center_attribute ?radii_attribute geometry =
  Error.guard ~operation:"bound" ~code:"invalid_geometry" @@ fun () ->
  if grain <= 0 then invalid_arg "Rdk.Bound.bound: grain must be positive";
  let valid_padding value = finite_vec3 value && value.Rays_math.Vec3.x >= 0.
      && value.y >= 0. && value.z >= 0. in
  let output_names = List.filter_map Fun.id [center_attribute; radii_attribute] in
  if not (valid_padding lower_padding && valid_padding upper_padding) then
    Error "Rdk.Bound.bound: lower and upper padding must be finite and non-negative"
  else if List.exists (fun name -> String.trim name = "" || String.equal name "P")
      output_names then Error "Rdk.Bound.bound: output attribute names must be non-empty and cannot be P"
  else if List.length output_names <> List.length (List.sort_uniq String.compare output_names)
  then Error "Rdk.Bound.bound: output attribute names must be distinct"
  else if match bounds_group with Some name -> String.trim name = "" | None -> false
  then Error "Rdk.Bound.bound: bounds group name must not be empty"
  else
    let shape_valid = match shape with
      | Bound_box { divisions = dx, dy, dz } -> dx > 0 && dy > 0 && dz > 0
      | Bound_sphere { segments; rings; minimum_radius } ->
          segments >= 3 && rings >= 2 && Float.is_finite minimum_radius
          && minimum_radius >= 0. in
    if not shape_valid then Error
        "Rdk.Bound.bound: box divisions must be positive; sphere segments/rings/minimum radius are invalid"
    else Result.bind
        (selected_bounds ?cancel ~grain ~operation:"bound" selection geometry)
      (fun source_bounds ->
        let create, center, radii = match shape with
          | Bound_box { divisions } ->
              let minimum = Rays_math.Vec3.create
                  (source_bounds.min.x -. lower_padding.x)
                  (source_bounds.min.y -. lower_padding.y)
                  (source_bounds.min.z -. lower_padding.z)
              and maximum = Rays_math.Vec3.create
                  (source_bounds.max.x +. upper_padding.x)
                  (source_bounds.max.y +. upper_padding.y)
                  (source_bounds.max.z +. upper_padding.z) in
              let size = Rays_math.Vec3.sub maximum minimum in
              let center = Rays_math.Vec3.add minimum (Rays_math.Vec3.scale size 0.5)
              and radii = Rays_math.Vec3.scale size 0.5 in
              (fun () -> if not (finite_vec3 size && finite_vec3 center)
                    || size.x <= 0. || size.y <= 0. || size.z <= 0. then
                    Error "Rdk.Bound.bound: box output must have finite positive extent on every axis"
                  else divided_box ?cancel ~minimum ~maximum ~divisions ()),
              center, radii
          | Bound_sphere { segments; rings; minimum_radius } ->
              let base_radius = Float.hypot (source_bounds.size.x *. 0.5)
                  (Float.hypot (source_bounds.size.y *. 0.5)
                    (source_bounds.size.z *. 0.5)) in
              let center = Rays_math.Vec3.create
                  (source_bounds.center.x
                    +. ((upper_padding.x -. lower_padding.x) *. 0.5))
                  (source_bounds.center.y
                    +. ((upper_padding.y -. lower_padding.y) *. 0.5))
                  (source_bounds.center.z
                    +. ((upper_padding.z -. lower_padding.z) *. 0.5)) in
              let radius lower upper = Float.max minimum_radius
                  (base_radius +. ((lower +. upper) *. 0.5)) in
              let radii = Rays_math.Vec3.create
                  (radius lower_padding.x upper_padding.x)
                  (radius lower_padding.y upper_padding.y)
                  (radius lower_padding.z upper_padding.z) in
              (fun () -> if not (Float.is_finite base_radius && finite_vec3 center
                    && finite_vec3 radii) || radii.x <= 0. || radii.y <= 0.
                    || radii.z <= 0. then
                    Error "Rdk.Bound.bound: sphere output radii must be finite and positive"
                  else Result.map (transform ~grain
                      (Rays_math.Mat4.mul (Rays_math.Mat4.translation center) (Rays_math.Mat4.scaling radii)))
                      (Error.unguard (uv_sphere ?cancel ~grain ~segments ~rings ~radius:1. ()))),
              center, radii in
        Result.bind (create ()) (fun output ->
          let detail_float3 name value output =
            let values = Packed.Float3.Private.of_owned_exn ~x:[|value.Rays_math.Vec3.x|]
                ~y:[|value.y|] ~z:[|value.z|] in
            let attribute = Attribute.create_owned ~name ~owner:Attribute.Detail
                (Attribute.Float3 values) |> Support.get_ok in
            Geometry.with_attribute attribute output |> Support.get_ok in
          let output = match center_attribute with
            | None -> output | Some name -> detail_float3 name center output in
          let output = match radii_attribute with
            | None -> output | Some name -> detail_float3 name radii output in
          let output = match bounds_group with
            | None -> output
            | Some name ->
                let group = Group.init ~grain ~owner:Group.Primitive ~name
                    (Geometry.primitive_count output) (fun _ -> true) in
                Geometry.with_group group output |> Support.get_ok in
          Ok output))

let run = bound

module Private = struct
  let selected_bounds = selected_bounds
end
