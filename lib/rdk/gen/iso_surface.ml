open Rdk_core

type sample = float array

module Field = struct
  type t =

    | Custom of (sample -> float)

  let custom field = Custom field
end

type evaluator =  Dense of Field.t

let sample_scratch = Domain.DLS.new_key (fun () -> Array.make 3 0.)

let finite_vec3 point =
  Float.is_finite point.Rays_math.Vec3.x
  && Float.is_finite point.y
  && Float.is_finite point.z

let cell_triangle_counts =
  let tetrahedron_counts = [|0;1;1;2;1;2;2;1;1;2;2;1;2;1;1;0|] in
  Array.init 256 (fun mask ->
    let bit corner = (mask lsr corner) land 1 in
    let count a b c d =
      tetrahedron_counts.(bit a lor (bit b lsl 1)
        lor (bit c lsl 2) lor (bit d lsl 3)) in
    count 0 1 2 6 + count 0 2 3 6 + count 0 3 7 6
    + count 0 7 4 6 + count 0 4 5 6 + count 0 5 1 6)

let extract_with evaluator ?cancel ?(grain = 16_384) ?(smooth = true)
    ~resolution:(x_cells, y_cells, z_cells) ~min ~max ~iso () =
  if x_cells <= 0 || y_cells <= 0 || z_cells <= 0 then
    invalid_arg "Iso3.extract: resolution counts must be positive";
  if grain <= 0 then invalid_arg "Iso3.extract: grain must be positive";
  if x_cells >= Sys.max_array_length || y_cells >= Sys.max_array_length
     || z_cells >= Sys.max_array_length then
    invalid_arg "Iso3.extract: resolution is too large";
  if not (finite_vec3 min && finite_vec3 max && Float.is_finite iso) then
    invalid_arg "Iso3.extract: bounds and isovalue must be finite";
  let extent = Rays_math.Vec3.sub max min in
  if extent.x <= 0. || extent.y <= 0. || extent.z <= 0. then
    invalid_arg
      "Iso3.extract: every maximum bound must exceed its minimum";
  let x_points = x_cells + 1 and y_points = y_cells + 1 in
  let x_step = extent.x /. float_of_int x_cells
  and y_step = extent.y /. float_of_int y_cells
  and z_step = extent.z /. float_of_int z_cells in
  let safe_product left right =
    if left > max_int / right then None else Some (left * right) in
  match safe_product x_points y_points, safe_product x_cells y_cells with
  | None, _ | _, None -> Error "Iso3.extract: resolution is too large"
  | Some plane_stride, Some slab_cell_count ->
  (match evaluator with

   | _ -> ());
  let exception Non_finite_field in
  let slabs_per_chunk = match evaluator with

    | _ -> z_cells in
  let chunk_count = 1 + (z_cells - 1) / slabs_per_chunk in
  let split_slabs = chunk_count >= 2 in
  let iter_slabs body =
    if split_slabs then Parallel.for_ ~chunk_size:1
      ~start:0 ~finish:(chunk_count - 1) (fun chunk ->
        let first = chunk * slabs_per_chunk in
        body first (first + Int.min slabs_per_chunk (z_cells - first)))
    else body 0 z_cells in
  let iter_plane body =
    if split_slabs || plane_stride / grain < 2 then
      for flat = 0 to plane_stride - 1 do body flat done
    else
      Parallel.for_ ~chunk_size:grain
        ~start:0 ~finish:(plane_stride - 1) body in
  let sample_values_into values z =
    Cancel.check_opt cancel;
    let pz = Float.fma (float_of_int z) z_step min.z in
    (match evaluator with

     | Dense (Custom field) ->
         iter_plane (fun flat ->
           let x = flat mod x_points and y = flat / x_points in
           let point = Domain.DLS.get sample_scratch in
           point.(0) <- Float.fma (float_of_int x) x_step min.x;
           point.(1) <- Float.fma (float_of_int y) y_step min.y;
           point.(2) <- pz;
           values.(flat) <- field point));
    let flat = ref 0 in
    while !flat < plane_stride do
      if not (Float.is_finite values.(!flat)) then raise Non_finite_field;
      incr flat
    done
  in
  let fill_xy_gradients values gx gy =
    iter_plane (fun flat ->
      let x = flat mod x_points in
      gx.(flat) <-
      if x = 0 then (values.(flat + 1) -. values.(flat)) /. x_step
      else if x = x_cells then
        (values.(flat) -. values.(flat - 1)) /. x_step
      else (values.(flat + 1) -. values.(flat - 1)) /. (2. *. x_step));
    iter_plane (fun flat ->
      let y = flat / x_points in
      gy.(flat) <-
      if y = 0 then
        (values.(flat + x_points) -. values.(flat)) /. y_step
      else if y = y_cells then
        (values.(flat) -. values.(flat - x_points)) /. y_step
      else
        (values.(flat + x_points) -. values.(flat - x_points))
        /. (2. *. y_step))
  in
  let create_plane z =
    let values = Array.make plane_stride 0.
    and gx = Array.make plane_stride 0.
    and gy = Array.make plane_stride 0. in
    sample_values_into values z;
    fill_xy_gradients values gx gy;
    values, gx, gy
  in
  let refill_plane (values, gx, gy) z =
    sample_values_into values z;
    fill_xy_gradients values gx gy
  in
  let count_slab lower upper counts =
    Cancel.check_opt cancel;
    let count_cell cell =
        let x = cell mod x_cells and y = cell / x_cells in
        let i0 = (y * x_points) + x in
        let i1 = i0 + 1 and i3 = i0 + x_points
        and i2 = i0 + x_points + 1 in
        let b0 = if lower.(i0) >= iso then 1 else 0
        and b1 = if lower.(i1) >= iso then 1 else 0
        and b2 = if lower.(i2) >= iso then 1 else 0
        and b3 = if lower.(i3) >= iso then 1 else 0
        and b4 = if upper.(i0) >= iso then 1 else 0
        and b5 = if upper.(i1) >= iso then 1 else 0
        and b6 = if upper.(i2) >= iso then 1 else 0
        and b7 = if upper.(i3) >= iso then 1 else 0 in
        counts.(cell) <-
          cell_triangle_counts.(b0 lor (b1 lsl 1) lor (b2 lsl 2)
            lor (b3 lsl 3) lor (b4 lsl 4) lor (b5 lsl 5)
            lor (b6 lsl 6) lor (b7 lsl 7))
    in
    if split_slabs || slab_cell_count / grain < 2 then
      for cell = 0 to slab_cell_count - 1 do count_cell cell done
    else
      Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(slab_cell_count - 1) count_cell
  in
  try
    let slab_triangles = Array.make z_cells 0 in
    let count_range first last =
    let counts = Array.make slab_cell_count 0 in
    let lower = ref (Array.make plane_stride 0.)
    and upper = ref (Array.make plane_stride 0.) in
    sample_values_into !lower first;
    for z = first to last - 1 do
      Cancel.check_opt cancel;
      sample_values_into !upper (z + 1);
      count_slab !lower !upper counts;
      let total = ref 0 in
      Array.iter (fun count -> total := !total + count) counts;
      slab_triangles.(z) <- !total;
      let previous = !lower in
      lower := !upper;
      upper := previous
    done in
    iter_slabs count_range;
    let slab_offsets = Array.make (z_cells + 1) 0 in
    for z = 0 to z_cells - 1 do
      if slab_offsets.(z) > max_int - slab_triangles.(z) then
        invalid_arg "Iso3.extract: output is too large";
      slab_offsets.(z + 1) <- slab_offsets.(z) + slab_triangles.(z)
    done;
    let total_triangles = slab_offsets.(z_cells) in
      if total_triangles = 0 then
        Error "Iso3.extract: the requested isosurface is empty"
      else if total_triangles > Sys.max_array_length / 3 then
        Error "Iso3.extract: output is too large"
      else
        let packed_vec3 count : Packed.Float3.View.view = {
          x = Array.make count 0.;
          y = Array.make count 0.;
          z = Array.make count 0.;
        } in
        let output_vertex_count = total_triangles * 3 in
        let vertices = packed_vec3 output_vertex_count
        and normals = packed_vec3 output_vertex_count in
        let store (target : Packed.Float3.View.view) index value =
          target.x.(index) <- value.Rays_math.Vec3.x;
          target.y.(index) <- value.y;
          target.z.(index) <- value.z
        in
        let[@inline] swap (values : float array) left right =
          let value = values.(left) in
          values.(left) <- values.(right);
          values.(right) <- value
        in
        let[@inline] swap_slots (target : Packed.Float3.View.view) left right =
          swap target.x left right;
          swap target.y left right;
          swap target.z left right
        in
        let fill_z_gradient gradient z values previous next =
          iter_plane (fun flat ->
            gradient.(flat) <-
            if z = 0 then
              let next, _, _ = Option.get next in
              (next.(flat) -. values.(flat)) /. z_step
            else if z = z_cells then
              let previous, _, _ = Option.get previous in
              (values.(flat) -. previous.(flat)) /. z_step
            else
              let previous, _, _ = Option.get previous
              and next, _, _ = Option.get next in
              (next.(flat) -. previous.(flat)) /. (2. *. z_step))
        in
        let emit_range first last =
        let counts = Array.make slab_cell_count 0 in
        let lower = ref (create_plane first) in
        let upper = ref (create_plane (first + 1)) in
        let next = ref
            (if first + 2 <= z_cells then
               Some (create_plane (first + 2)) else None) in
        let lower_z = ref (Array.make plane_stride 0.)
        and upper_z_buffer = ref (Array.make plane_stride 0.) in
        (* A chunk seam retains the global central derivative, using its halo. *)
        let previous = if first > 0 then Some (create_plane (first - 1)) else None in
        fill_z_gradient !lower_z first
          (let values, _, _ = !lower in values) previous (Some !upper);
        let local_offsets = Array.make (slab_cell_count + 1) 0 in
        let corner_x = [|0;1;1;0;0;1;1;0|]
        and corner_y = [|0;0;1;1;0;0;1;1|] in
        for z = first to last - 1 do
          Cancel.check_opt cancel;
          let lower_values, lower_gx, lower_gy = !lower
          and upper_values, upper_gx, upper_gy = !upper in
          fill_z_gradient !upper_z_buffer (z + 1) upper_values
            (Some !lower) !next;
          let upper_z = !upper_z_buffer in
          count_slab lower_values upper_values counts;
          local_offsets.(0) <- 0;
          for cell = 0 to slab_cell_count - 1 do
            local_offsets.(cell + 1) <- local_offsets.(cell) + counts.(cell)
          done;
          assert (local_offsets.(slab_cell_count) = slab_triangles.(z));
          let corner_index base = function
            | 0 | 4 -> base
            | 1 | 5 -> base + 1
            | 2 | 6 -> base + x_points + 1
            | 3 | 7 -> base + x_points
            | _ -> assert false in
          let edge cell_x cell_y base left right output =
            let left_index = corner_index base left
            and right_index = corner_index base right in
            let left_value = if left < 4 then lower_values.(left_index)
              else upper_values.(left_index)
            and right_value = if right < 4 then lower_values.(right_index)
              else upper_values.(right_index) in
            let denominator = right_value -. left_value in
            let amount = if abs_float denominator <= 1e-15 then 0.5
              else Float.max 0.
                  (Float.min 1. ((iso -. left_value) /. denominator)) in
            let lx = min.x +. float_of_int (cell_x + corner_x.(left)) *. x_step
            and ly = min.y +. float_of_int (cell_y + corner_y.(left)) *. y_step
            and lz = min.z +. float_of_int (z + if left < 4 then 0 else 1) *. z_step
            and rx = min.x +. float_of_int (cell_x + corner_x.(right)) *. x_step
            and ry = min.y +. float_of_int (cell_y + corner_y.(right)) *. y_step
            and rz = min.z +. float_of_int (z + if right < 4 then 0 else 1) *. z_step in
            vertices.x.(output) <- lx +. (rx -. lx) *. amount;
            vertices.y.(output) <- ly +. (ry -. ly) *. amount;
            vertices.z.(output) <- lz +. (rz -. lz) *. amount;
            let lgx = if left < 4 then lower_gx.(left_index)
              else upper_gx.(left_index)
            and lgy = if left < 4 then lower_gy.(left_index)
              else upper_gy.(left_index)
            and lgz = if left < 4 then (!lower_z).(left_index)
              else upper_z.(left_index)
            and rgx = if right < 4 then lower_gx.(right_index)
              else upper_gx.(right_index)
            and rgy = if right < 4 then lower_gy.(right_index)
              else upper_gy.(right_index)
            and rgz = if right < 4 then (!lower_z).(right_index)
              else upper_z.(right_index) in
            let nx = -. (lgx +. (rgx -. lgx) *. amount)
            and ny = -. (lgy +. (rgy -. lgy) *. amount)
            and nz = -. (lgz +. (rgz -. lgz) *. amount) in
            let length = sqrt (nx *. nx +. ny *. ny +. nz *. nz) in
            if length <= 1e-18 then begin
              normals.x.(output) <- 0.;
              normals.y.(output) <- 0.;
              normals.z.(output) <- 0.
            end else begin
              normals.x.(output) <- nx /. length;
              normals.y.(output) <- ny /. length;
              normals.z.(output) <- nz /. length
            end in
          let fill_cell cell =
            if counts.(cell) <> 0 then begin
            let cell_x = cell mod x_cells and cell_y = cell / x_cells in
            let i0 = (cell_y * x_points) + cell_x in
            let output = ref
                ((slab_offsets.(z) + local_offsets.(cell)) * 3) in
            let emit e0a e0b e1a e1b e2a e2b =
              let first = !output in
              let second = first + 1 and third = first + 2 in
              edge cell_x cell_y i0 e0a e0b first;
              edge cell_x cell_y i0 e1a e1b second;
              edge cell_x cell_y i0 e2a e2b third;
              let abx = vertices.x.(second)-.vertices.x.(first)
              and aby = vertices.y.(second)-.vertices.y.(first)
              and abz = vertices.z.(second)-.vertices.z.(first)
              and acx = vertices.x.(third)-.vertices.x.(first)
              and acy = vertices.y.(third)-.vertices.y.(first)
              and acz = vertices.z.(third)-.vertices.z.(first) in
              let gx = aby*.acz-.abz*.acy and gy=abz*.acx-.abx*.acz
              and gz=abx*.acy-.aby*.acx in
              let outward_x=normals.x.(first)+.normals.x.(second)+.normals.x.(third)
              and outward_y=normals.y.(first)+.normals.y.(second)+.normals.y.(third)
              and outward_z=normals.z.(first)+.normals.z.(second)+.normals.z.(third) in
              let flip = gx*.outward_x +. gy*.outward_y +. gz*.outward_z < 0. in
              if flip then begin
                swap_slots vertices second third;
                swap_slots normals second third
              end;
              if not smooth then begin
                let length = sqrt (gx*.gx +. gy*.gy +. gz*.gz) in
                let sign = if flip then -1. else 1. in
                let normal = if length <= 1e-18 then Rays_math.Vec3.zero else
                    Rays_math.Vec3.create (sign*.gx/.length) (sign*.gy/.length) (sign*.gz/.length) in
                store normals !output normal; store normals (!output + 1) normal;
                store normals (!output + 2) normal
              end;
              output := !output + 3 in
            let emit_one i a b c = emit i a i b i c in
            let emit_two i j a b =
              emit i a i b j b;
              emit i a j b j a in
            let tetra code a b c d =
              match code with
              | 0 | 15 -> ()
              | 1 | 14 -> emit_one a b c d
              | 2 | 13 -> emit_one b a c d
              | 4 | 11 -> emit_one c a b d
              | 8 | 7 -> emit_one d a b c
              | 3 | 12 -> emit_two a b c d
              | 5 | 10 -> emit_two a c b d
              | 9 | 6 -> emit_two a d b c
              | _ -> assert false in
            let l0 = lower_values.(i0)
            and l1 = lower_values.(i0 + 1)
            and l2 = lower_values.(i0 + x_points + 1)
            and l3 = lower_values.(i0 + x_points)
            and u0 = upper_values.(i0)
            and u1 = upper_values.(i0 + 1)
            and u2 = upper_values.(i0 + x_points + 1)
            and u3 = upper_values.(i0 + x_points) in
            let b0 = if l0 >= iso then 1 else 0
            and b1 = if l1 >= iso then 1 else 0
            and b2 = if l2 >= iso then 1 else 0
            and b3 = if l3 >= iso then 1 else 0
            and b4 = if u0 >= iso then 1 else 0
            and b5 = if u1 >= iso then 1 else 0
            and b6 = if u2 >= iso then 1 else 0
            and b7 = if u3 >= iso then 1 else 0 in
            tetra (b0 lor (b1 lsl 1) lor (b2 lsl 2) lor (b6 lsl 3)) 0 1 2 6;
            tetra (b0 lor (b2 lsl 1) lor (b3 lsl 2) lor (b6 lsl 3)) 0 2 3 6;
            tetra (b0 lor (b3 lsl 1) lor (b7 lsl 2) lor (b6 lsl 3)) 0 3 7 6;
            tetra (b0 lor (b7 lsl 1) lor (b4 lsl 2) lor (b6 lsl 3)) 0 7 4 6;
            tetra (b0 lor (b4 lsl 1) lor (b5 lsl 2) lor (b6 lsl 3)) 0 4 5 6;
            tetra (b0 lor (b5 lsl 1) lor (b1 lsl 2) lor (b6 lsl 3)) 0 5 1 6;
            assert (!output =
              (slab_offsets.(z) + local_offsets.(cell + 1)) * 3)
            end
          in
          if split_slabs || slab_cell_count / grain < 2 then
            for cell = 0 to slab_cell_count - 1 do fill_cell cell done
          else
            Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(slab_cell_count - 1) fill_cell;
          if z < last - 1 then begin
            let spare_plane = !lower in
            lower := !upper;
            upper := Option.get !next;
            let spare_z = !lower_z in
            lower_z := !upper_z_buffer;
            upper_z_buffer := spare_z;
            next :=
              if z + 3 <= z_cells then begin
                refill_plane spare_plane (z + 3);
                Some spare_plane
              end else None
          end
        done in
        iter_slabs emit_range;
        let positions = Packed.Float3.View.of_owned_exn
            ~x:vertices.x ~y:vertices.y ~z:vertices.z in
        let normals = Packed.Float3.View.of_owned_exn
            ~x:normals.x ~y:normals.y ~z:normals.z in
        Ok (positions, normals)
  with Non_finite_field ->
    Error "Iso3.extract: field returned a non-finite sample"

let geometry_of_packed (positions, normals) =
  let point_count = Packed.Float3.length positions in
  let triangle_count = point_count / 3 in
  let vertex_points = Array.init point_count Fun.id in
  let primitive_offsets = Array.init (triangle_count + 1)
      (fun index -> index * 3) in
  (* The extractor emits exactly three uniquely numbered points per triangle. *)
  let topology = Topology.View.create_validated_owned ~point_count
      ~vertex_points ~primitive_offsets
      ~primitive_kinds:(Bytes.make triangle_count '\000') in
  Result.bind
    (Attribute.create_key_owned (Attribute.normal ~owner:Attribute.Point)
       normals)
    (fun normal -> Geometry.create ~positions ~topology ~attributes:[normal] ())

let protect ?cancel evaluator finish ?grain ?smooth ~resolution ~min ~max ~iso () =
  Error.guard ~operation:"iso_surface" ~code:"invalid_parameter" (fun () ->
    try Result.bind
      (extract_with evaluator ?cancel ?grain ?smooth ~resolution ~min ~max ~iso ())
      finish
    with Invalid_argument message -> Error message)

let extract_dense ~resolution ~min ~max ~iso ~field () =
  protect (Dense field) geometry_of_packed
    ~resolution ~min ~max ~iso ()

module View = struct

end
