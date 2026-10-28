type metric = Euclidean | Componentwise

type clusters = {
  count : int;
  of_point : int array;
  offsets : int array;
  members : int array;
  representatives : int array;
}

type t = Identity | Clusters of clusters

let of_links ?cancel ~operation destinations =
  try
    let count = Array.length destinations in
    let parent = Array.init count Fun.id in
    let rec root point =
      let parent_point = parent.(point) in
      if parent_point = point then point
      else begin
        let result = root parent_point in
        parent.(point) <- result;
        result
      end in
    let union left right =
      let left = root left and right = root right in
      if left <> right then
        if left < right then parent.(right) <- left else parent.(left) <- right in
    for point = 0 to count - 1 do
      Cancel.poll cancel point 4095;
      let destination = destinations.(point) in
      if destination < -1 || destination >= count then invalid_arg
          (Printf.sprintf "%s target point %d at query %d is out of range"
            operation destination point);
      if destination >= 0 && destination <> point then union point destination
    done;
    let changed = ref false in
    for point = 0 to count - 1 do
      let representative = root point in
      parent.(point) <- representative;
      if representative <> point then changed := true
    done;
    if not !changed then Ok Identity
    else begin
      let representative_to_cluster = Array.make count (-1)
      and cluster_count = ref 0 in
      for point = 0 to count - 1 do
        let representative = parent.(point) in
        if representative_to_cluster.(representative) < 0 then begin
          representative_to_cluster.(representative) <- !cluster_count;
          incr cluster_count
        end;
        parent.(point) <- representative_to_cluster.(representative)
      done;
      let sizes = Array.make !cluster_count 0 in
      for point = 0 to count - 1 do
        sizes.(parent.(point)) <- sizes.(parent.(point)) + 1
      done;
      let offsets = Array.make (!cluster_count + 1) 0 in
      for cluster = 0 to !cluster_count - 1 do
        offsets.(cluster + 1) <- offsets.(cluster) + sizes.(cluster)
      done;
      let members = Array.make count 0 and cursors = Array.copy offsets in
      for point = 0 to count - 1 do
        let cluster = parent.(point) and slot = cursors.(parent.(point)) in
        members.(slot) <- point;
        cursors.(cluster) <- slot + 1
      done;
      let representatives = Array.init !cluster_count (fun cluster ->
          members.(offsets.(cluster))) in
      Ok (Clusters { count = !cluster_count; of_point = parent; offsets;
        members; representatives })
    end
  with Invalid_argument message -> Error message


let create ?cancel ?selection ?(metric = Euclidean) ?(inclusive = true)
    ~operation ~tolerance ~compatible geometry =
  try
    let count = Geometry.point_count geometry in
    (match selection with
     | Some group when Group.owner group <> Group.Point
         || Group.length group <> count ->
         invalid_arg (Printf.sprintf
           "%s selection must be a matching point group" operation)
     | _ -> ());
    if not (Float.is_finite tolerance) || tolerance < 0. then
      invalid_arg (Printf.sprintf
        "%s tolerance must be finite and non-negative" operation);
    Cancel.check_opt cancel;
    if count = 0 then Ok Identity
    else if count > Sys.max_array_length / 2 then
      Error (Printf.sprintf "%s point count exceeds spatial-table limits"
        operation)
    else begin
      let positions = Packed.Float3.View.view (Geometry.positions geometry) in
      let min_x = ref infinity and min_y = ref infinity
      and min_z = ref infinity and max_x = ref neg_infinity
      and max_y = ref neg_infinity and max_z = ref neg_infinity
      and non_finite = ref (-1) in
      for point = 0 to count - 1 do
        Cancel.poll cancel point 4095;
        let x = positions.x.(point) and y = positions.y.(point)
        and z = positions.z.(point) in
        if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then non_finite := point
        else begin
          if x < !min_x then min_x := x;
          if y < !min_y then min_y := y;
          if z < !min_z then min_z := z;
          if x > !max_x then max_x := x;
          if y > !max_y then max_y := y;
          if z > !max_z then max_z := z
        end
      done;
      if !non_finite >= 0 then Error (Printf.sprintf
          "%s point %d has a non-finite position" operation !non_finite)
      else begin
        let exact = tolerance = 0. in
        let scaled_delta left right =
          let delta = right -. left in
          if Float.is_finite delta then delta /. tolerance
          else (right /. tolerance) -. (left /. tolerance) in
        let largest_cell = if exact then 0. else
          let x = scaled_delta !min_x !max_x
          and y = scaled_delta !min_y !max_y
          and z = scaled_delta !min_z !max_z in
          if x >= y then if x >= z then x else z else if y >= z then y else z in
        if not (Float.is_finite largest_cell)
            || largest_cell > float_of_int (max_int / 4) then
          Error (Printf.sprintf
            "%s tolerance is too small for the geometry extent" operation)
        else begin
          let grid = Point_grid.create ~cells:count ~points:count in
          let { Point_grid.cell_x; cell_y; cell_z; cell_head; point_next; _ } = grid in
          let find_cell = Point_grid.find grid in
          let representative = Array.init count Fun.id in
          let neighbor_delta = if exact then 0 else 1 in
          let[@inline always] cell_coord value minimum =
            if exact then Point_grid.float_cell value
            else int_of_float (floor (scaled_delta minimum value)) in
          for point = 0 to count - 1 do
            Cancel.poll cancel point 4095;
            if match selection with None -> true
                | Some group -> Group.mem point group then begin
              let x = positions.x.(point) and y = positions.y.(point)
              and z = positions.z.(point) in
              let cx = cell_coord x !min_x and cy = cell_coord y !min_y
              and cz = cell_coord z !min_z in
              let nearest = ref (-1) in
              for dx = -neighbor_delta to neighbor_delta do
                for dy = -neighbor_delta to neighbor_delta do
                  for dz = -neighbor_delta to neighbor_delta do
                    let cell = find_cell (cx + dx) (cy + dy) (cz + dz) in
                    if cell >= 0 then begin
                      let candidate = ref cell_head.(cell) in
                      while !candidate >= 0 do
                        let other = !candidate in
                        let px = x -. positions.x.(other)
                        and py = y -. positions.y.(other)
                        and pz = z -. positions.z.(other) in
                        let position_matches = if exact then
                            px = 0. && py = 0. && pz = 0.
                          else match metric with
                          | Euclidean ->
                              let scale = max (abs_float px)
                                  (max (abs_float py) (abs_float pz)) in
                              if scale = 0. then true
                              else if scale > tolerance then false
                              else
                                let x = px /. scale and y = py /. scale
                                and z = pz /. scale in
                                let ratio = scale /. tolerance in
                                let normalized = ratio *. ratio
                                    *. ((x *. x) +. (y *. y) +. (z *. z)) in
                                if inclusive then normalized <= 1.
                                else normalized < 1.
                          | Componentwise ->
                              if inclusive then abs_float px <= tolerance
                                && abs_float py <= tolerance
                                && abs_float pz <= tolerance
                              else abs_float px < tolerance
                                && abs_float py < tolerance
                                && abs_float pz < tolerance in
                        if position_matches && compatible point other then
                          if !nearest < 0 || other < !nearest then
                            nearest := other;
                        candidate := point_next.(other)
                      done
                    end
                  done
                done
              done;
              if !nearest >= 0 then
                representative.(point) <- !nearest
              else begin
                Point_grid.insert grid cx cy cz point
              end
            end
          done;
          let of_point = representative
          and representative_to_cluster = cell_x
          and cluster_count = ref 0 in
          Array.fill representative_to_cluster 0 count (-1);
          for point = 0 to count - 1 do
            let root = representative.(point) in
            if representative_to_cluster.(root) < 0 then begin
              representative_to_cluster.(root) <- !cluster_count;
              incr cluster_count
            end;
            of_point.(point) <- representative_to_cluster.(root)
          done;
          if !cluster_count = count then Ok Identity
          else begin
            let sizes = cell_y in
            Array.fill sizes 0 !cluster_count 0;
            for point = 0 to count - 1 do
              let cluster = of_point.(point) in
              sizes.(cluster) <- sizes.(cluster) + 1
            done;
            let offsets = Array.make (!cluster_count + 1) 0 in
            for cluster = 0 to !cluster_count - 1 do
              offsets.(cluster + 1) <- offsets.(cluster) + sizes.(cluster)
            done;
            let members = Array.make count 0 and cursors = cell_z in
            Array.blit offsets 0 cursors 0 (!cluster_count + 1);
            for point = 0 to count - 1 do
              let cluster = of_point.(point) and output = cursors.(of_point.(point)) in
              members.(output) <- point;
              cursors.(cluster) <- output + 1
            done;
            let representatives = Array.init !cluster_count (fun cluster ->
                members.(offsets.(cluster))) in
            Ok (Clusters { count = !cluster_count; of_point; offsets; members;
              representatives })
          end
        end
      end
    end
  with Invalid_argument message -> Error message
