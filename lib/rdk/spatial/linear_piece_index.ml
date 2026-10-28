
type kind = Triangle | Segment

type t = {
  positions : Packed.Float3.View.view;
  topology : Topology.View.view;
  kinds : bytes;
  primitives : int array;
  locals : int array;
  vertex_a : int array;
  vertex_b : int array;
  vertex_c : int array;
  u0 : float array;
  u1 : float array;
  piece_min_x : float array;
  piece_min_y : float array;
  piece_min_z : float array;
  piece_max_x : float array;
  piece_max_y : float array;
  piece_max_z : float array;
  order : int array;
  min_x : float array;
  min_y : float array;
  min_z : float array;
  max_x : float array;
  max_y : float array;
  max_z : float array;
  left : int array;
  right : int array;
  first : int array;
  count : int array;
}

let fail code message = Error.fail_with ~operation:"linear_piece_index" ~code message

let create ?cancel ?(grain = 16_384) ?primitives geometry =
  Error.run ~operation:"linear_piece_index" ~code:"invalid_parameter"
    ~cancelled:"linear-piece index construction was cancelled" @@ fun () ->
  if grain <= 0 then invalid_arg "Linear_piece_index: grain must be positive";
  Cancel.check_opt cancel;
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry) in
  let primitive_count = Bytes.length topology.primitive_kinds in
  let selected = match primitives with
    | None -> Array.init primitive_count Fun.id
    | Some group ->
        if Group.owner group <> Group.Primitive then
          fail "invalid_group" "selection must be a primitive group";
        if Group.length group <> primitive_count then
          fail "invalid_group" "primitive group length does not match geometry";
        let output = Array.make (Group.cardinality group) 0 and next = ref 0 in
        Group.iter (fun primitive -> output.(!next) <- primitive; incr next) group;
        output in
  let selected_count = Array.length selected in
  let piece_offsets = Array.make (selected_count + 1) 0 in
  for slot = 0 to selected_count - 1 do
    if slot land 4095 = 0 then Cancel.check_opt cancel;
    let primitive = selected.(slot) in
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    let size = last - first in
    let pieces = match
        Char.code (Bytes.unsafe_get topology.primitive_kinds primitive) with
      | 0 ->
          if size <> 3 then fail "invalid_surface" (Printf.sprintf
            "primitive %d is a polygon with %d corners, not a triangle"
            primitive size);
          1
      | 1 -> size - 1
      | 2 -> size
      | _ -> fail "invalid_surface" (Printf.sprintf
          "primitive %d has an invalid kind" primitive) in
    if pieces > Sys.max_array_length - piece_offsets.(slot) then
      fail "invalid_surface" "piece cardinality exceeds array limits";
    piece_offsets.(slot + 1) <- piece_offsets.(slot) + pieces
  done;
  let piece_count = piece_offsets.(selected_count) in
  let kinds = Bytes.make piece_count '\000'
  and primitive_of_piece = Array.make piece_count 0
  and locals = Array.make piece_count 0
  and vertex_a = Array.make piece_count 0
  and vertex_b = Array.make piece_count 0
  and vertex_c = Array.make piece_count (-1)
  and u0 = Array.make piece_count 0.
  and u1 = Array.make piece_count 0. in
  let average = if selected_count = 0 then 1
    else max 1 (Support.ceiling_div piece_count selected_count) in
  let chunk = max 1 (grain / average) in
  let ranges = Support.ceiling_div selected_count chunk in
  if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1)
      (fun range ->
    let first_selected = range * chunk
    and last_selected = min selected_count ((range + 1) * chunk) in
    for slot = first_selected to last_selected - 1 do
      if slot land 4095 = 0 then Cancel.check_opt cancel;
      let primitive = selected.(slot) and output = piece_offsets.(slot) in
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1) in
      let size = last - first
      and code = Char.code (Bytes.unsafe_get topology.primitive_kinds primitive) in
      if code = 0 then begin
        primitive_of_piece.(output) <- primitive;
        vertex_a.(output) <- first;
        vertex_b.(output) <- first + 1;
        vertex_c.(output) <- first + 2
      end else begin
        let segment_count = if code = 1 then size - 1 else size in
        for local = 0 to segment_count - 1 do
          let piece = output + local in
          Bytes.unsafe_set kinds piece '\001';
          primitive_of_piece.(piece) <- primitive;
          locals.(piece) <- local;
          vertex_a.(piece) <- first + local;
          vertex_b.(piece) <- if local + 1 < size then first + local + 1 else first;
          u0.(piece) <- float_of_int local /. float_of_int segment_count;
          u1.(piece) <- float_of_int (local + 1) /. float_of_int segment_count
        done
      end
    done);
  let centroid_x = Array.make piece_count 0.
  and centroid_y = Array.make piece_count 0.
  and centroid_z = Array.make piece_count 0.
  and piece_min_x = Array.make piece_count 0.
  and piece_min_y = Array.make piece_count 0.
  and piece_min_z = Array.make piece_count 0.
  and piece_max_x = Array.make piece_count 0.
  and piece_max_y = Array.make piece_count 0.
  and piece_max_z = Array.make piece_count 0. in
  let piece_ranges = Support.ceiling_div piece_count grain in
  let errors = Array.make piece_ranges None in
  if piece_ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(piece_ranges - 1) (fun range ->
    let first_piece = range * grain
    and last_piece = min piece_count ((range + 1) * grain) in
    for piece = first_piece to last_piece - 1 do
      if piece land 4095 = 0 then Cancel.check_opt cancel;
      if errors.(range) = None then begin
        let primitive = primitive_of_piece.(piece) in
        let pa = topology.vertex_points.(vertex_a.(piece))
        and pb = topology.vertex_points.(vertex_b.(piece)) in
        let ax = positions.x.(pa) and ay = positions.y.(pa)
        and az = positions.z.(pa) and bx = positions.x.(pb)
        and by = positions.y.(pb) and bz = positions.z.(pb) in
        if not (Float.is_finite ax && Float.is_finite ay && Float.is_finite az && Float.is_finite bx && Float.is_finite by
            && Float.is_finite bz) then errors.(range) <- Some (Printf.sprintf
              "primitive %d has a non-finite position" primitive)
        else if Bytes.unsafe_get kinds piece = '\001' then begin
          if ax = bx && ay = by && az = bz then
            errors.(range) <- Some (Printf.sprintf
              "primitive %d segment %d is degenerate" primitive locals.(piece))
          else begin
            centroid_x.(piece) <- (ax *. 0.5) +. (bx *. 0.5);
            centroid_y.(piece) <- (ay *. 0.5) +. (by *. 0.5);
            centroid_z.(piece) <- (az *. 0.5) +. (bz *. 0.5);
            piece_min_x.(piece) <- Float.min ax bx;
            piece_min_y.(piece) <- Float.min ay by;
            piece_min_z.(piece) <- Float.min az bz;
            piece_max_x.(piece) <- Float.max ax bx;
            piece_max_y.(piece) <- Float.max ay by;
            piece_max_z.(piece) <- Float.max az bz
          end
        end else begin
          let pc = topology.vertex_points.(vertex_c.(piece)) in
          let cx = positions.x.(pc) and cy = positions.y.(pc)
          and cz = positions.z.(pc) in
          if not (Float.is_finite cx && Float.is_finite cy && Float.is_finite cz) then
            errors.(range) <- Some (Printf.sprintf
              "primitive %d has a non-finite position" primitive)
          else begin
            let abx = bx -. ax and aby = by -. ay and abz = bz -. az
            and acx = cx -. ax and acy = cy -. ay and acz = cz -. az in
            let nx = (aby *. acz) -. (abz *. acy)
            and ny = (abz *. acx) -. (abx *. acz)
            and nz = (abx *. acy) -. (aby *. acx) in
            let area2 = (nx *. nx) +. (ny *. ny) +. (nz *. nz) in
            if not (Float.is_finite area2) || area2 <= 1e-30 then
              errors.(range) <- Some (Printf.sprintf
                "primitive %d is degenerate" primitive)
            else begin
              centroid_x.(piece) <- (ax +. bx +. cx) /. 3.;
              centroid_y.(piece) <- (ay +. by +. cy) /. 3.;
              centroid_z.(piece) <- (az +. bz +. cz) /. 3.;
              piece_min_x.(piece) <- Float.min ax (Float.min bx cx);
              piece_min_y.(piece) <- Float.min ay (Float.min by cy);
              piece_min_z.(piece) <- Float.min az (Float.min bz cz);
              piece_max_x.(piece) <- Float.max ax (Float.max bx cx);
              piece_max_y.(piece) <- Float.max ay (Float.max by cy);
              piece_max_z.(piece) <- Float.max az (Float.max bz cz)
            end
          end
        end
      end
    done);
  let failure = ref None in
  Array.iter (fun value -> match !failure, value with
    | None, Some message -> failure := Some message
    | None, None | Some _, _ -> ()) errors;
  Option.iter (fun message -> fail "invalid_surface" message) !failure;
  let bounds = Bounds3_index.create ?cancel ~grain ~centroid_x ~centroid_y
      ~centroid_z ~item_min_x:piece_min_x ~item_min_y:piece_min_y
      ~item_min_z:piece_min_z ~item_max_x:piece_max_x ~item_max_y:piece_max_y
      ~item_max_z:piece_max_z () in
  { positions; topology; kinds; primitives = primitive_of_piece; locals;
    vertex_a; vertex_b; vertex_c; u0; u1; piece_min_x; piece_min_y; piece_min_z;
    piece_max_x; piece_max_y; piece_max_z;
    order = bounds.order; min_x = bounds.min_x; min_y = bounds.min_y;
    min_z = bounds.min_z; max_x = bounds.max_x; max_y = bounds.max_y;
    max_z = bounds.max_z; left = bounds.left; right = bounds.right;
    first = bounds.first; count = bounds.count }

let piece_count value = Array.length value.primitives

let[@inline always] bounds_overlap tolerance left right_index node left_piece =
  left.piece_min_x.(left_piece) <= right_index.max_x.(node) +. tolerance
  && left.piece_max_x.(left_piece) >= right_index.min_x.(node) -. tolerance
  && left.piece_min_y.(left_piece) <= right_index.max_y.(node) +. tolerance
  && left.piece_max_y.(left_piece) >= right_index.min_y.(node) -. tolerance
  && left.piece_min_z.(left_piece) <= right_index.max_z.(node) +. tolerance
  && left.piece_max_z.(left_piece) >= right_index.min_z.(node) -. tolerance

let[@inline always] piece_bounds_overlap tolerance left left_piece right
    right_piece =
  left.piece_min_x.(left_piece) <= right.piece_max_x.(right_piece) +. tolerance
  && left.piece_max_x.(left_piece) >= right.piece_min_x.(right_piece) -. tolerance
  && left.piece_min_y.(left_piece) <= right.piece_max_y.(right_piece) +. tolerance
  && left.piece_max_y.(left_piece) >= right.piece_min_y.(right_piece) -. tolerance
  && left.piece_min_z.(left_piece) <= right.piece_max_z.(right_piece) +. tolerance
  && left.piece_max_z.(left_piece) >= right.piece_min_z.(right_piece) -. tolerance

let pairs_of ?cancel ~self ~grain ~tolerance left_index right_index =
  if grain <= 0 then invalid_arg
      "Linear_piece_index.overlapping_pairs: grain must be positive";
  if not (Float.is_finite tolerance) || tolerance < 0. then invalid_arg
      "Linear_piece_index.overlapping_pairs: tolerance must be finite and non-negative";
  let left_count = piece_count left_index in
  if left_count = 0 || piece_count right_index = 0 then [||], [||]
  else begin
    let chunk = max 1 grain and ranges = Support.ceiling_div left_count (max 1 grain) in
    let counts = Array.make left_count 0 in
    let scan stack left_piece output_left output_right output_at =
      let top = ref 1 and found = ref 0 in
      stack.(0) <- 0;
      while !top > 0 do
        decr top;
        let node = stack.(!top) in
        if bounds_overlap tolerance left_index right_index node left_piece then
          if right_index.count.(node) > 0 then begin
            let last = right_index.first.(node) + right_index.count.(node) in
            for slot = right_index.first.(node) to last - 1 do
              let right_piece = right_index.order.(slot) in
              if (not self || right_piece > left_piece)
                  && piece_bounds_overlap tolerance left_index left_piece
                       right_index right_piece then begin
                (match output_left, output_right with
                 | Some left, Some right ->
                     left.(output_at + !found) <- left_piece;
                     right.(output_at + !found) <- right_piece
                 | None, None -> ()
                 | _ -> assert false);
                incr found
              end
            done
          end else begin
            stack.(!top) <- right_index.right.(node);
            stack.(!top + 1) <- right_index.left.(node);
            top := !top + 2
          end
      done;
      !found in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
      let stack = Array.make 65 0 in
      let first = range * chunk and last = min left_count ((range + 1) * chunk) in
      for piece = first to last - 1 do
        if piece land 4095 = 0 then Cancel.check_opt cancel;
        counts.(piece) <- scan stack piece None None 0
      done);
    let offsets = Array.make (left_count + 1) 0 in
    for piece = 0 to left_count - 1 do
      if counts.(piece) > Sys.max_array_length - offsets.(piece) then
        invalid_arg "Linear piece candidate cardinality exceeds array limits";
      offsets.(piece + 1) <- offsets.(piece) + counts.(piece)
    done;
    let total = offsets.(left_count) in
    let left_output = Array.make total 0 and right_output = Array.make total 0 in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
      let stack = Array.make 65 0 in
      let first = range * chunk and last = min left_count ((range + 1) * chunk) in
      for piece = first to last - 1 do
        if piece land 4095 = 0 then Cancel.check_opt cancel;
        let written = scan stack piece (Some left_output) (Some right_output)
            offsets.(piece) in
        if written <> counts.(piece) then invalid_arg
            "Linear piece candidate count/fill drift"
      done);
    left_output, right_output
  end

let self_pair_of ?cancel ~grain ~tolerance index predicate =
  if grain <= 0 then invalid_arg
      "Linear_piece_index.find_overlapping_self_pair: grain must be positive";
  if not (Float.is_finite tolerance) || tolerance < 0. then invalid_arg
      "Linear_piece_index.find_overlapping_self_pair: tolerance must be finite and non-negative";
  let pieces = piece_count index in
  if pieces = 0 then None
  else begin
    let ranges = Support.ceiling_div pieces grain in
    let found_left = Array.make ranges (-1) and found_right = Array.make ranges (-1) in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
      let stack = Array.make 65 0
      and last_piece = min pieces ((range + 1) * grain)
      and left_piece = ref (range * grain) in
      while found_left.(range) < 0 && !left_piece < last_piece do
        if !left_piece land 4095 = 0 then Cancel.check_opt cancel;
        let top = ref 1 and best_right = ref (-1) in
        stack.(0) <- 0;
        while !top > 0 do
          decr top;
          let node = stack.(!top) in
          if node land 4095 = 0 then Cancel.check_opt cancel;
          if bounds_overlap tolerance index index node !left_piece then
            if index.count.(node) > 0 then begin
              let last = index.first.(node) + index.count.(node) in
              for slot = index.first.(node) to last - 1 do
                let right_piece = index.order.(slot) in
                if right_piece > !left_piece
                    && (right_piece < !best_right || !best_right < 0)
                    && piece_bounds_overlap tolerance index !left_piece
                         index right_piece
                    && predicate !left_piece right_piece then
                  best_right := right_piece
              done
            end else begin
              stack.(!top) <- index.right.(node);
              stack.(!top + 1) <- index.left.(node);
              top := !top + 2
            end
        done;
        if !best_right >= 0 then begin
          found_left.(range) <- !left_piece;
          found_right.(range) <- !best_right
        end;
        incr left_piece
      done);
    let range = ref 0 in
    while !range < ranges && found_left.(!range) < 0 do incr range done;
    if !range = ranges then None
    else Some (found_left.(!range), found_right.(!range))
  end

module View = struct
  let[@inline always] kind value piece =
    if Bytes.unsafe_get value.kinds piece = '\000' then Triangle else Segment
  let[@inline always] primitive value piece = value.primitives.(piece)
  let[@inline always] local value piece = value.locals.(piece)
  let[@inline always] vertex value piece corner = match corner with
    | 0 -> value.vertex_a.(piece)
    | 1 -> value.vertex_b.(piece)
    | 2 when Bytes.unsafe_get value.kinds piece = '\000' -> value.vertex_c.(piece)
    | _ -> invalid_arg "Linear_piece_index.vertex: invalid local corner"
  let[@inline always] point value piece corner =
    value.topology.vertex_points.(vertex value piece corner)
  let[@inline always] curve_u value piece endpoint = match endpoint with
    | 0 -> value.u0.(piece) | 1 -> value.u1.(piece)
    | _ -> invalid_arg "Linear_piece_index.curve_u: invalid endpoint"
  let positions value = value.positions
  let overlapping_pairs ?cancel ~grain ~tolerance left right =
    pairs_of ?cancel ~self:false ~grain ~tolerance left right
  let overlapping_self_pairs ?cancel ~grain ~tolerance value =
    pairs_of ?cancel ~self:true ~grain ~tolerance value value
  let find_overlapping_self_pair ?cancel ~grain ~tolerance value predicate =
    self_pair_of ?cancel ~grain ~tolerance value predicate
end
