
type deform_selection = Deform.selection =
  | Selected_points of Group.t
  | Selected_vertices of Group.t
  | Selected_primitives of Group.t
  | Selected_edges of Edge_group.t

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
      let positions = Packed.Float3.View.view (Geometry.positions geometry) in
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

module View = struct
  let selected_bounds = selected_bounds
end
