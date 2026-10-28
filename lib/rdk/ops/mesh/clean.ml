open Rays_math

let[@inline] clean_length dx dy dz =
  let scale = Float.max (abs_float dx) (Float.max (abs_float dy) (abs_float dz)) in
  if scale = 0. then 0.
  else if not (Float.is_finite scale) then infinity
  else
    let x = dx /. scale and y = dy /. scale and z = dz /. scale in
    scale *. sqrt ((x *. x) +. (y *. y) +. (z *. z))

let[@inline] clean_point_distance positions a b =
  let scale =
    Float.max
      (Float.max (abs_float positions.Packed.Float3.View.x.(a)) (abs_float positions.x.(b)))
      (Float.max
         (Float.max (abs_float positions.y.(a)) (abs_float positions.y.(b)))
         (Float.max (abs_float positions.z.(a)) (abs_float positions.z.(b))))
  in
  if scale = 0. then 0.
  else if not (Float.is_finite scale) then infinity
  else
    clean_length
      ((positions.x.(b) /. scale) -. (positions.x.(a) /. scale))
      ((positions.y.(b) /. scale) -. (positions.y.(a) /. scale))
      ((positions.z.(b) /. scale) -. (positions.z.(a) /. scale))
    *. scale

let[@inline] clean_triangle_is_degenerate ~epsilon positions a b c =
  let ax = positions.Packed.Float3.View.x.(a)
  and ay = positions.y.(a)
  and az = positions.z.(a)
  and bx = positions.x.(b)
  and by = positions.y.(b)
  and bz = positions.z.(b)
  and cx = positions.x.(c)
  and cy = positions.y.(c)
  and cz = positions.z.(c) in
  if
    not
      (Float.is_finite ax && Float.is_finite ay && Float.is_finite az && Float.is_finite bx
     && Float.is_finite by && Float.is_finite bz && Float.is_finite cx && Float.is_finite cy
     && Float.is_finite cz)
  then true
  else
    let ux = bx -. ax
    and uy = by -. ay
    and uz = bz -. az
    and vx = cx -. ax
    and vy = cy -. ay
    and vz = cz -. az in
    let nx = (uy *. vz) -. (uz *. vy)
    and ny = (uz *. vx) -. (ux *. vz)
    and nz = (ux *. vy) -. (uy *. vx) in
    if Float.is_finite nx && Float.is_finite ny && Float.is_finite nz then
      0.5 *. clean_length nx ny nz <= epsilon *. epsilon
    else
      let scale =
        Float.max
          (Float.max
             (Float.max (abs_float ax) (abs_float ay))
             (Float.max (abs_float az) (abs_float bx)))
          (Float.max
             (Float.max (abs_float by) (abs_float bz))
             (Float.max (abs_float cx) (Float.max (abs_float cy) (abs_float cz))))
      in
      if scale = 0. then true
      else
        let ax = ax /. scale and ay = ay /. scale and az = az /. scale in
        let ux = (bx /. scale) -. ax
        and uy = (by /. scale) -. ay
        and uz = (bz /. scale) -. az
        and vx = (cx /. scale) -. ax
        and vy = (cy /. scale) -. ay
        and vz = (cz /. scale) -. az in
        let nx = (uy *. vz) -. (uz *. vy)
        and ny = (uz *. vx) -. (ux *. vz)
        and nz = (ux *. vy) -. (uy *. vx)
        and ratio = epsilon /. scale in
        0.5 *. clean_length nx ny nz <= ratio *. ratio

type delete_degenerate_params = {
  cancel : Cancel.t option;
  grain : int;
  primitives : Group.t option;
  epsilon : float;
}

let delete_degenerate ({ cancel; grain; primitives; epsilon } : delete_degenerate_params) geometry =
  let primitive_count = Geometry.primitive_count geometry in
  if
    match primitives with
    | Some group -> Group.owner group <> Group.Primitive || Group.length group <> primitive_count
    | None -> false
  then Error "Facet selection must be a matching primitive group"
  else if primitive_count = 0 then Ok geometry
  else
    let positions = Packed.Float3.View.view (Geometry.positions geometry)
    and topology_value = Geometry.topology geometry
    and flags = Bytes.make primitive_count '\000' in
    let topology = Topology.View.view topology_value in
    if Topology.all_triangles topology_value then
      Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(primitive_count - 1) (fun primitive ->
          Cancel.poll cancel primitive 4_095;
          let first = topology.primitive_offsets.(primitive) in
          if
            (match primitives with None -> true | Some group -> Group.mem primitive group)
            && clean_triangle_is_degenerate ~epsilon positions topology.vertex_points.(first)
                 topology.vertex_points.(first + 1)
                 topology.vertex_points.(first + 2)
          then Bytes.unsafe_set flags primitive '\001')
    else begin
      let scratch_a = Array.make primitive_count 0.
      and scratch_b = Array.make primitive_count 0.
      and scratch_c = Array.make primitive_count 0.
      and scratch_d = Array.make primitive_count 0. in
      Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(primitive_count - 1) (fun primitive ->
          Cancel.poll cancel primitive 4_095;
          let first = topology.primitive_offsets.(primitive)
          and last = topology.primitive_offsets.(primitive + 1)
          and kind = Bytes.unsafe_get topology.primitive_kinds primitive in
          if match primitives with Some group -> not (Group.mem primitive group) | None -> false
          then ()
          else if kind = '\000' then begin
            let origin = topology.vertex_points.(first) in
            let ox = positions.x.(origin)
            and oy = positions.y.(origin)
            and oz = positions.z.(origin) in
            for vertex = first to last - 1 do
              let next = if vertex + 1 < last then vertex + 1 else first in
              let a = topology.vertex_points.(vertex) and b = topology.vertex_points.(next) in
              let ax = positions.x.(a) -. ox
              and ay = positions.y.(a) -. oy
              and az = positions.z.(a) -. oz
              and bx = positions.x.(b) -. ox
              and by = positions.y.(b) -. oy
              and bz = positions.z.(b) -. oz in
              if
                not
                  (Float.is_finite positions.x.(a)
                  && Float.is_finite positions.y.(a)
                  && Float.is_finite positions.z.(a))
              then Bytes.unsafe_set flags primitive '\001'
              else if Bytes.unsafe_get flags primitive <> '\001' then begin
                let nx = scratch_b.(primitive) +. ((ay *. bz) -. (az *. by))
                and ny = scratch_c.(primitive) +. ((az *. bx) -. (ax *. bz))
                and nz = scratch_d.(primitive) +. ((ax *. by) -. (ay *. bx)) in
                scratch_b.(primitive) <- nx;
                scratch_c.(primitive) <- ny;
                scratch_d.(primitive) <- nz;
                if not (Float.is_finite nx && Float.is_finite ny && Float.is_finite nz) then
                  Bytes.unsafe_set flags primitive '\002'
              end
            done;
            if Bytes.unsafe_get flags primitive = '\000' then begin
              let area =
                0.5
                *. clean_length scratch_b.(primitive) scratch_c.(primitive) scratch_d.(primitive)
              in
              if area <= epsilon *. epsilon then Bytes.unsafe_set flags primitive '\001'
            end
            else if Bytes.unsafe_get flags primitive = '\002' then begin
              Bytes.unsafe_set flags primitive '\000';
              scratch_b.(primitive) <- 0.;
              scratch_c.(primitive) <- 0.;
              scratch_d.(primitive) <- 0.;
              for vertex = first to last - 1 do
                let point = topology.vertex_points.(vertex) in
                scratch_a.(primitive) <-
                  Float.max scratch_a.(primitive) (abs_float positions.x.(point));
                scratch_a.(primitive) <-
                  Float.max scratch_a.(primitive) (abs_float positions.y.(point));
                scratch_a.(primitive) <-
                  Float.max scratch_a.(primitive) (abs_float positions.z.(point))
              done;
              let scale = scratch_a.(primitive) in
              let ox = positions.x.(origin) /. scale
              and oy = positions.y.(origin) /. scale
              and oz = positions.z.(origin) /. scale in
              for vertex = first to last - 1 do
                let next = if vertex + 1 < last then vertex + 1 else first in
                let a = topology.vertex_points.(vertex) and b = topology.vertex_points.(next) in
                let ax = (positions.x.(a) /. scale) -. ox
                and ay = (positions.y.(a) /. scale) -. oy
                and az = (positions.z.(a) /. scale) -. oz
                and bx = (positions.x.(b) /. scale) -. ox
                and by = (positions.y.(b) /. scale) -. oy
                and bz = (positions.z.(b) /. scale) -. oz in
                scratch_b.(primitive) <- scratch_b.(primitive) +. ((ay *. bz) -. (az *. by));
                scratch_c.(primitive) <- scratch_c.(primitive) +. ((az *. bx) -. (ax *. bz));
                scratch_d.(primitive) <- scratch_d.(primitive) +. ((ax *. by) -. (ay *. bx))
              done;
              let area =
                0.5
                *. clean_length scratch_b.(primitive) scratch_c.(primitive) scratch_d.(primitive)
              and ratio = epsilon /. scale in
              if area <= ratio *. ratio then Bytes.unsafe_set flags primitive '\001'
            end
          end
          else begin
            let size = last - first in
            let edges = size - 1 + if kind = '\001' then 0 else 1 in
            for edge = 0 to edges - 1 do
              let a = topology.vertex_points.(first + (edge mod size))
              and b = topology.vertex_points.(first + ((edge + 1) mod size)) in
              scratch_a.(primitive) <- scratch_a.(primitive) +. clean_point_distance positions a b
            done;
            if (not (Float.is_finite scratch_a.(primitive))) || scratch_a.(primitive) <= epsilon
            then Bytes.unsafe_set flags primitive '\001'
          end)
    end;
    let removed = ref false in
    Bytes.iter (fun flag -> if flag <> '\000' then removed := true) flags;
    if not !removed then Ok geometry
    else
      let selection =
        Group.init ~grain ~owner:Group.Primitive ~name:"__clean_degenerate" primitive_count
          (fun primitive -> Bytes.get flags primitive <> '\000')
      in
      Ok
        (Error.get
           (Deletion.delete
              { cancel; grain = Some grain; selected = true; compact_points = false }
              selection geometry))

let delete_nan_points ?cancel ~grain geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let point_count = Geometry.point_count geometry in
  if point_count = 0 then Ok geometry
  else
    let flags = Bytes.make point_count '\000' in
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(point_count - 1) (fun point ->
        Cancel.poll cancel point 16_383;
        if
          positions.x.(point) <> positions.x.(point)
          || positions.y.(point) <> positions.y.(point)
          || positions.z.(point) <> positions.z.(point)
        then Bytes.set flags point '\001');
    let removed = ref false in
    Bytes.iter (fun flag -> if flag <> '\000' then removed := true) flags;
    if not !removed then Ok geometry
    else
      let selection =
        Group.init ~grain ~owner:Group.Point ~name:"__clean_nan_points" point_count (fun point ->
            Bytes.get flags point <> '\000')
      in
      Ok
        (Error.get
           (Deletion.delete
              { cancel; grain = Some grain; selected = true; compact_points = false }
              selection geometry))

let[@inline] clean_cycle_point topology first size start direction offset =
  let local = (start + (direction * offset)) mod size in
  topology.Topology.View.vertex_points.(first + if local < 0 then local + size else local)

let clean_minimal_rotation_into topology first size direction primitive work_left work_right
    work_offset =
  work_left.(primitive) <- 0;
  work_right.(primitive) <- 1;
  work_offset.(primitive) <- 0;
  while
    work_left.(primitive) < size && work_right.(primitive) < size && work_offset.(primitive) < size
  do
    let offset = work_offset.(primitive) in
    let a = clean_cycle_point topology first size work_left.(primitive) direction offset
    and b = clean_cycle_point topology first size work_right.(primitive) direction offset in
    if a = b then work_offset.(primitive) <- offset + 1
    else begin
      if a > b then begin
        work_left.(primitive) <- work_left.(primitive) + offset + 1;
        if work_left.(primitive) = work_right.(primitive) then
          work_left.(primitive) <- work_left.(primitive) + 1
      end
      else begin
        work_right.(primitive) <- work_right.(primitive) + offset + 1;
        if work_right.(primitive) = work_left.(primitive) then
          work_right.(primitive) <- work_right.(primitive) + 1
      end;
      work_offset.(primitive) <- 0
    end
  done;
  min work_left.(primitive) work_right.(primitive) mod size

let delete_overlaps_general ?cancel ~grain ~delete_pairs geometry =
  let topology = Topology.View.view (Geometry.topology geometry) in
  let primitive_count = Geometry.primitive_count geometry in
  let sizes = Array.make primitive_count 0
  and starts = Array.make primitive_count 0
  and directions = Array.make primitive_count 1
  and hashes = Array.make primitive_count 0
  and reverse_starts = Array.make primitive_count 0
  and work_left = Array.make primitive_count 0
  and work_right = Array.make primitive_count 0
  and work_offset = Array.make primitive_count 0
  and comparisons = Array.make primitive_count 0 in
  let polygon_count = ref 0 in
  for primitive = 0 to primitive_count - 1 do
    if Bytes.get topology.primitive_kinds primitive = '\000' then incr polygon_count
  done;
  if !polygon_count = 0 then Ok geometry
  else if !polygon_count > Sys.max_array_length / 4 then
    Error "Rdk_mesh.Clean.clean: overlap table exceeds array limits"
  else begin
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(primitive_count - 1) (fun primitive ->
        Cancel.poll cancel primitive 4_095;
        if Bytes.get topology.primitive_kinds primitive = '\000' then begin
          let first = topology.primitive_offsets.(primitive)
          and size =
            topology.primitive_offsets.(primitive + 1) - topology.primitive_offsets.(primitive)
          in
          starts.(primitive) <-
            clean_minimal_rotation_into topology first size 1 primitive work_left work_right
              work_offset;
          reverse_starts.(primitive) <-
            clean_minimal_rotation_into topology first size (-1) primitive work_left work_right
              work_offset;
          work_offset.(primitive) <- 0;
          comparisons.(primitive) <- 0;
          while work_offset.(primitive) < size && comparisons.(primitive) = 0 do
            let offset = work_offset.(primitive) in
            comparisons.(primitive) <-
              Int.compare
                (clean_cycle_point topology first size starts.(primitive) 1 offset)
                (clean_cycle_point topology first size reverse_starts.(primitive) (-1) offset);
            work_offset.(primitive) <- offset + 1
          done;
          if comparisons.(primitive) > 0 then begin
            starts.(primitive) <- reverse_starts.(primitive);
            directions.(primitive) <- -1
          end;
          hashes.(primitive) <- 17 lxor size;
          for offset = 0 to size - 1 do
            hashes.(primitive) <-
              Hash.mix hashes.(primitive)
                (clean_cycle_point topology first size starts.(primitive) directions.(primitive)
                   offset)
              land max_int
          done;
          sizes.(primitive) <- size
        end);
    let capacity = ref 8 in
    while !capacity < !polygon_count * 2 do
      capacity := !capacity lsl 1
    done;
    let table = Array.make !capacity (-1)
    and mask = !capacity - 1
    and flags = Bytes.make primitive_count '\000' in
    let point primitive offset =
      let first = topology.primitive_offsets.(primitive) and size = sizes.(primitive) in
      let local = (starts.(primitive) + (directions.(primitive) * offset)) mod size in
      topology.vertex_points.(first + if local < 0 then local + size else local)
    in
    let equal_offset = ref 0 and equal_same = ref true in
    let equal left right =
      if sizes.(left) <> sizes.(right) then false
      else begin
        equal_offset := 0;
        equal_same := true;
        while !equal_same && !equal_offset < sizes.(left) do
          equal_same := point left !equal_offset = point right !equal_offset;
          incr equal_offset
        done;
        !equal_same
      end
    in
    let slot = ref 0 and found = ref (-1) and searching = ref true in
    for primitive = 0 to primitive_count - 1 do
      Cancel.poll cancel primitive 4_095;
      if sizes.(primitive) > 0 then begin
        slot := hashes.(primitive) land mask;
        found := -1;
        searching := true;
        while !searching do
          let candidate = table.(!slot) in
          if candidate < 0 then searching := false
          else if hashes.(candidate) = hashes.(primitive) && equal candidate primitive then begin
            found := candidate;
            searching := false
          end
          else slot := (!slot + 1) land mask
        done;
        if !found < 0 then table.(!slot) <- primitive
        else begin
          Bytes.set flags primitive '\001';
          if delete_pairs then Bytes.set flags !found '\001'
        end
      end
    done;
    let removed = ref false in
    Bytes.iter (fun flag -> if flag <> '\000' then removed := true) flags;
    if not !removed then Ok geometry
    else
      let selection =
        Group.init ~grain ~owner:Group.Primitive ~name:"__clean_overlaps" primitive_count
          (fun primitive -> Bytes.get flags primitive <> '\000')
      in
      Ok
        (Error.get
           (Deletion.delete
              { cancel; grain = Some grain; selected = true; compact_points = false }
              selection geometry))
  end

let delete_triangle_overlaps ?cancel ~grain ~delete_pairs geometry =
  let topology = Topology.View.view (Geometry.topology geometry) in
  let primitive_count = Geometry.primitive_count geometry in
  if primitive_count = 0 then Ok geometry
  else if primitive_count > Sys.max_array_length / 4 then
    Error "Rdk_mesh.Clean.clean: overlap table exceeds array limits"
  else begin
    let first_points = Array.make primitive_count 0
    and second_points = Array.make primitive_count 0
    and third_points = Array.make primitive_count 0
    and hashes = Array.make primitive_count 0 in
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(primitive_count - 1) (fun primitive ->
        Cancel.poll cancel primitive 4_095;
        let first = topology.primitive_offsets.(primitive) in
        let p0 = topology.vertex_points.(first)
        and p1 = topology.vertex_points.(first + 1)
        and p2 = topology.vertex_points.(first + 2) in
        let low, high = if p0 <= p1 then (p0, p1) else (p1, p0) in
        let a, b, c =
          if p2 <= low then (p2, low, high)
          else if p2 >= high then (low, high, p2)
          else (low, p2, high)
        in
        first_points.(primitive) <- a;
        second_points.(primitive) <- b;
        third_points.(primitive) <- c;
        hashes.(primitive) <- Hash.mix (Hash.mix (Hash.mix (17 lxor 3) a) b) c land max_int);
    let capacity = ref 8 in
    while !capacity < primitive_count * 2 do
      capacity := !capacity lsl 1
    done;
    let table = Array.make !capacity (-1)
    and mask = !capacity - 1
    and flags = Bytes.make primitive_count '\000' in
    let slot = ref 0 and found = ref (-1) and searching = ref true in
    for primitive = 0 to primitive_count - 1 do
      Cancel.poll cancel primitive 4_095;
      slot := hashes.(primitive) land mask;
      found := -1;
      searching := true;
      while !searching do
        let candidate = table.(!slot) in
        if candidate < 0 then searching := false
        else if
          hashes.(candidate) = hashes.(primitive)
          && first_points.(candidate) = first_points.(primitive)
          && second_points.(candidate) = second_points.(primitive)
          && third_points.(candidate) = third_points.(primitive)
        then begin
          found := candidate;
          searching := false
        end
        else slot := (!slot + 1) land mask
      done;
      if !found < 0 then table.(!slot) <- primitive
      else begin
        Bytes.unsafe_set flags primitive '\001';
        if delete_pairs then Bytes.unsafe_set flags !found '\001'
      end
    done;
    let removed = ref false in
    Bytes.iter (fun flag -> if flag <> '\000' then removed := true) flags;
    if not !removed then Ok geometry
    else
      let selection =
        Group.init ~grain ~owner:Group.Primitive ~name:"__clean_overlaps" primitive_count
          (fun primitive -> Bytes.get flags primitive <> '\000')
      in
      Ok
        (Error.get
           (Deletion.delete
              { cancel; grain = Some grain; selected = true; compact_points = false }
              selection geometry))
  end

let delete_overlaps ?cancel ~grain ~delete_pairs geometry =
  if Topology.all_triangles (Geometry.topology geometry) then
    delete_triangle_overlaps ?cancel ~grain ~delete_pairs geometry
  else delete_overlaps_general ?cancel ~grain ~delete_pairs geometry

type overlap_policy = Keep_first_overlap | Delete_overlap_pairs

type params = {
  cancel : Cancel.t option;
  grain : int;
  epsilon : float;
  remove_degenerate : bool;
  consolidate_distance : float option;
  overlaps : overlap_policy option;
  reverse_winding : bool;
  remove_nan_points : bool;
  remove_unused_points : bool;
  delete_unused_groups : bool;
  point_attributes : string option;
  vertex_attributes : string option;
  primitive_attributes : string option;
  detail_attributes : string option;
  point_groups : string option;
  vertex_groups : string option;
  primitive_groups : string option;
  edge_groups : string option;
}

let run
    ({
       cancel;
       grain;
       epsilon;
       remove_degenerate;
       consolidate_distance;
       overlaps;
       reverse_winding;
       remove_nan_points;
       remove_unused_points;
       delete_unused_groups;
       point_attributes;
       vertex_attributes;
       primitive_attributes;
       detail_attributes;
       point_groups;
       vertex_groups;
       primitive_groups;
       edge_groups;
     } :
      params) geometry =
  Error.guard ~operation:"clean" ~code:"invalid_geometry" @@ fun () ->
  let overlaps = Option.map (fun policy -> policy = Delete_overlap_pairs) overlaps in
  let consolidate tolerance geometry =
    Ok (Error.get (Fuse_grid.weld { cancel; grain; selection = None; tolerance } geometry))
  and compact geometry = Ok (Error.get (Compact_points.run ?cancel ~grain geometry)) in
  if grain <= 0 then invalid_arg "Rdk_mesh.Clean.clean: grain must be positive";
  if (not (Float.is_finite epsilon)) || epsilon < 0. then
    Error "Rdk_mesh.Clean.clean: epsilon must be finite and non-negative"
  else if
    match consolidate_distance with
    | Some value -> (not (Float.is_finite value)) || value < 0.
    | None -> false
  then Error "Rdk_mesh.Clean.clean: consolidate distance must be finite and non-negative"
  else begin
    let validate_pattern = function
      | None -> Ok ()
      | Some pattern when String.trim pattern = "" -> Ok ()
      | Some pattern -> Result.map (fun _ -> ()) (Attribute_pattern.compile pattern)
    in
    let patterns =
      [
        point_attributes;
        vertex_attributes;
        primitive_attributes;
        detail_attributes;
        point_groups;
        vertex_groups;
        primitive_groups;
        edge_groups;
      ]
    in
    let validation =
      List.fold_left
        (fun result pattern -> Result.bind result (fun () -> validate_pattern pattern))
        (Ok ()) patterns
    in
    Result.bind validation (fun () ->
        let result =
          if remove_nan_points then delete_nan_points ?cancel ~grain geometry else Ok geometry
        in
        let result =
          Result.bind result (fun geometry ->
              match consolidate_distance with
              | None -> Ok geometry
              | Some tolerance -> consolidate tolerance geometry)
        in
        let result =
          Result.bind result (fun geometry ->
              if remove_degenerate then
                delete_degenerate { cancel; grain; primitives = None; epsilon } geometry
              else Ok geometry)
        in
        let result =
          Result.bind result (fun geometry ->
              match overlaps with
              | None -> Ok geometry
              | Some delete_pairs -> delete_overlaps ?cancel ~grain ~delete_pairs geometry)
        in
        let result =
          Result.bind result (fun geometry ->
              if reverse_winding then Ok (Error.get (Reverse_faces.run ?cancel geometry))
              else Ok geometry)
        in
        let result =
          Result.bind result (fun geometry ->
              if remove_unused_points then compact geometry else Ok geometry)
        in
        let result =
          Result.bind result (fun geometry ->
              Attribute_lifecycle.delete
                {
                  cancel;
                  reference = None;
                  delete_non_selected = false;
                  point_pattern = point_attributes;
                  vertex_pattern = vertex_attributes;
                  primitive_pattern = primitive_attributes;
                  detail_pattern = detail_attributes;
                }
                geometry)
        in
        Result.bind result (fun geometry ->
            let add owner pattern rules =
              match pattern with
              | None -> rules
              | Some pattern when String.trim pattern = "" -> rules
              | Some delete_pattern ->
                  ({ delete_owner = Some owner; delete_pattern } : Group_ops.delete_rule) :: rules
            in
            let rules =
              []
              |> add Group_ops.Group_points point_groups
              |> add Group_ops.Group_vertices vertex_groups
              |> add Group_ops.Group_primitives primitive_groups
              |> add Group_ops.Group_edges edge_groups
              |> List.rev
            in
            if rules = [] && not delete_unused_groups then Ok geometry
            else
              Ok
                (Error.get
                   (Group_ops.delete { rules; delete_unused = delete_unused_groups } geometry))))
  end
