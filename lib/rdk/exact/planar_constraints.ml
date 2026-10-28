
type t = {
  source_x : float array;
  source : Implicit_point.source;
  explicit_cache : Implicit_point.t option array;
  split_points : Implicit_point.t array;
  x : float array;
  y : float array;
  inserted : int array;
  constraints : int array;
  constraint_winding : int array;
  split_first : int array;
  split_second : int array;
  split_parameter : float array;
}

let source_point_count value = Array.length value.source_x
let point_count value = Array.length value.x
let split_point_count value = Array.length value.split_points

let check_split value split =
  if split < 0 || split >= split_point_count value then
    invalid_arg "Planar_constraints: split point is out of range"

let split_source_first value split = check_split value split; value.split_first.(split)
let split_source_second value split = check_split value split; value.split_second.(split)
let split_source_parameter value split =
  check_split value split; value.split_parameter.(split)

let exact_point value point =
  let source_count = source_point_count value in
  if point < 0 || point >= point_count value then
    invalid_arg "Planar_constraints: point is out of range";
  if point >= source_count then value.split_points.(point - source_count)
  else match value.explicit_cache.(point) with
    | Some result -> result
    | None ->
        let result = match Implicit_point.explicit value.source point with
          | Ok result -> result | Error _ ->
              invalid_arg "Planar constraints lost an explicit source point" in
        value.explicit_cache.(point) <- Some result;
        result

module Private = struct
  type view = {
    x : float array;
    y : float array;
    insert_points : int array;
    constraint_points : int array;
    constraint_winding : int array;
  }

  let view (value:t) = { x=value.x; y=value.y; insert_points=value.inserted;
    constraint_points=value.constraints;
    constraint_winding=value.constraint_winding }

  let point = exact_point

  let orient2d value a b c =
    Implicit_point.orient2d_xy
      (exact_point value a) (exact_point value b) (exact_point value c)

  let incircle value a b c d =
    Implicit_point.incircle_xy
      (exact_point value a) (exact_point value b)
      (exact_point value c) (exact_point value d)
end

type int_buffer = { mutable values : int array; mutable length : int }
let buffer capacity = { values = Array.make (max 16 capacity) 0; length = 0 }
let add buffer value =
  if buffer.length = Array.length buffer.values then begin
    if buffer.length > Sys.max_array_length / 2 then
      invalid_arg "Planar constraint event cardinality exceeds array limits";
    let values = Array.make (buffer.length * 2) 0 in
    Array.blit buffer.values 0 values 0 buffer.length; buffer.values <- values
  end;
  buffer.values.(buffer.length) <- value; buffer.length <- buffer.length + 1

let opposite left right = match left,right with
  | Predicates.Negative,Predicates.Positive
  | Predicates.Positive,Predicates.Negative -> true
  | _ -> false

let stable_parameter first second point =
  let denominator = second -. first and numerator = point -. first in
  if Float.is_finite denominator && Float.is_finite numerator then
    numerator /. denominator
  else begin
    let scale = max (abs_float first)
        (max (abs_float second) (abs_float point)) in
    if scale = 0. then 0.
    else ((point /. scale) -. (first /. scale))
      /. ((second /. scale) -. (first /. scale))
  end

let build_bvh ?cancel min_x min_y max_x max_y =
  Bounds2_index.create ?cancel ~min_x ~min_y ~max_x ~max_y ()

let candidate_pairs = Bounds2_index.candidate_pairs
let build ?cancel ?(grain = 16_384) ~split_crossings ~x ~y ~segment_points
    ?(embedded_points = [||]) ?segment_winding () =
  try
    Cancel.check_opt cancel;
    if grain <= 0 then invalid_arg "Planar constraint grain must be positive";
    let source_count = Array.length x in
    if source_count = 0 || Array.length y <> source_count then
      invalid_arg "Planar constraints require equal non-empty coordinate planes";
    Array.iteri (fun point value -> if not (Float.is_finite value
        && Float.is_finite y.(point)) then invalid_arg (Printf.sprintf
          "Planar constraint point %d is non-finite" point)) x;
    if Array.length segment_points mod 2 <> 0 then
      invalid_arg "Planar constraint endpoint array must contain pairs";
    let segment_count = Array.length segment_points / 2 in
    let segment_winding = match segment_winding with
      | None -> Array.make segment_count 0
      | Some values when Array.length values = segment_count -> values
      | Some _ -> invalid_arg
          "Planar constraint winding cardinality must match segment count" in
    let first = Array.make segment_count 0 and second = Array.make segment_count 0
    and min_x = Array.make segment_count 0. and min_y = Array.make segment_count 0.
    and max_x = Array.make segment_count 0. and max_y = Array.make segment_count 0. in
    for segment = 0 to segment_count - 1 do
      let a = segment_points.(segment * 2)
      and b = segment_points.((segment * 2) + 1) in
      if a < 0 || a >= source_count || b < 0 || b >= source_count then
        invalid_arg "Planar constraint endpoint is out of range";
      if a = b || x.(a) = x.(b) && y.(a) = y.(b) then
        invalid_arg "Planar constraint segment is degenerate";
      first.(segment) <- a; second.(segment) <- b;
      min_x.(segment) <- min x.(a) x.(b); min_y.(segment) <- min y.(a) y.(b);
      max_x.(segment) <- max x.(a) x.(b); max_y.(segment) <- max y.(a) y.(b)
    done;
    let source_z = Array.make source_count 0. in
    let source = match Implicit_point.source ~x ~y ~z:source_z with
      | Ok source -> source | Error _ -> invalid_arg "Planar exact source failed" in
    let explicit_cache = Array.make source_count None in
    let explicit point = match explicit_cache.(point) with
      | Some value -> value
      | None -> let value = match Implicit_point.explicit source point with
          | Ok value -> value | Error _ -> invalid_arg "Planar explicit point failed" in
        explicit_cache.(point) <- Some value; value in
    let initial_events =
      if segment_count > (Sys.max_array_length - 16) / 3 then 16
      else max 16 (segment_count * 3) in
    let split_segments = buffer initial_events
    and split_points = buffer initial_events in
    let add_split segment point = add split_segments segment; add split_points point in
    for segment = 0 to segment_count - 1 do
      add_split segment first.(segment); add_split segment second.(segment)
    done;
    let crossing_points = ref (Array.make 16 (explicit 0))
    and crossing_first = ref (Array.make 16 0)
    and crossing_second = ref (Array.make 16 0)
    and crossing_count = ref 0 in
    let add_crossing left right point =
      if !crossing_count = Array.length !crossing_points then begin
        let capacity = !crossing_count * 2 in
        let points = Array.make capacity (explicit 0)
        and firsts = Array.make capacity 0 and seconds = Array.make capacity 0 in
        Array.blit !crossing_points 0 points 0 !crossing_count;
        Array.blit !crossing_first 0 firsts 0 !crossing_count;
        Array.blit !crossing_second 0 seconds 0 !crossing_count;
        crossing_points := points; crossing_first := firsts; crossing_second := seconds
      end;
      (!crossing_points).(!crossing_count) <- point;
      (!crossing_first).(!crossing_count) <- left;
      (!crossing_second).(!crossing_count) <- right;
      incr crossing_count in
    let on_segment segment point =
      x.(point) >= min_x.(segment) && x.(point) <= max_x.(segment)
      && y.(point) >= min_y.(segment) && y.(point) <= max_y.(segment) in
    let classified_pairs = ref 0 in
    let classify left right =
      incr classified_pairs;
      if !classified_pairs land 4095 = 0 then Cancel.check_opt cancel;
      let a = first.(left) and b = second.(left)
      and c = first.(right) and d = second.(right) in
      let ab_c = Predicates.orient2d_packed ~x ~y a b c
      and ab_d = Predicates.orient2d_packed ~x ~y a b d
      and cd_a = Predicates.orient2d_packed ~x ~y c d a
      and cd_b = Predicates.orient2d_packed ~x ~y c d b in
      if opposite ab_c ab_d && opposite cd_a cd_b then begin
        if not split_crossings then
          invalid_arg "Planar constraints cross and crossing splitting is disabled";
        let point = match Implicit_point.line_line source
            ~projection:Implicit_point.XY ~first_start:a ~first_end:b
            ~second_start:c ~second_end:d with
          | Ok point -> point | Error _ ->
              invalid_arg "Planar proper crossing construction failed" in
        add_crossing left right point
      end else begin
        if ab_c = Predicates.Zero && on_segment left c then add_split left c;
        if ab_d = Predicates.Zero && on_segment left d then add_split left d;
        if cd_a = Predicates.Zero && on_segment right a then add_split right a;
        if cd_b = Predicates.Zero && on_segment right b then add_split right b
      end in
    let segment_tree = build_bvh ?cancel min_x min_y max_x max_y in
    candidate_pairs ?cancel segment_tree classify;
    let embedded_count = Array.length embedded_points in
    if embedded_count > 0 then begin
      let embedded_seen = Bytes.make source_count '\000' in
      Array.iter (fun point ->
        if point < 0 || point >= source_count then
          invalid_arg "Planar embedded point is out of range";
        if Bytes.unsafe_get embedded_seen point <> '\000' then
          invalid_arg "Planar embedded point is duplicated";
        Bytes.unsafe_set embedded_seen point '\001') embedded_points;
      let query point visit =
        Bounds2_index.query ?cancel segment_tree
          ~min_x:x.(point) ~min_y:y.(point)
          ~max_x:x.(point) ~max_y:y.(point) (fun segment ->
            if point <> first.(segment) && point <> second.(segment)
                && on_segment segment point
                && Predicates.orient2d_packed ~x ~y
                  first.(segment) second.(segment) point = Predicates.Zero then
              visit segment) in
      let counts = Array.make embedded_count 0 in
      let count index =
        if index land 4095 = 0 then Cancel.check_opt cancel;
        let value = ref 0 in
        query embedded_points.(index) (fun _ ->
          if !value = max_int then
            invalid_arg "Planar embedded incidence cardinality exceeds integer range";
          incr value);
        counts.(index) <- !value in
      if embedded_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(embedded_count - 1) count
      else for index = 0 to embedded_count - 1 do count index done;
      if embedded_count = Sys.max_array_length then
        invalid_arg "Planar embedded point cardinality exceeds array limits";
      let offsets = Array.make (embedded_count + 1) 0 in
      for index = 0 to embedded_count - 1 do
        if offsets.(index) > Sys.max_array_length - counts.(index) then
          invalid_arg "Planar embedded incidence cardinality exceeds array limits";
        offsets.(index + 1) <- offsets.(index) + counts.(index)
      done;
      let incidences = Array.make offsets.(embedded_count) 0 in
      let fill index =
        if index land 4095 = 0 then Cancel.check_opt cancel;
        let at = ref offsets.(index) in
        query embedded_points.(index) (fun segment ->
          incidences.(!at) <- segment; incr at);
        if !at <> offsets.(index + 1) then
          invalid_arg "Planar embedded incidence count changed between passes" in
      if embedded_count > grain then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(embedded_count - 1) fill
      else for index = 0 to embedded_count - 1 do fill index done;
      for index = 0 to embedded_count - 1 do
        let point = embedded_points.(index) in
        for incidence = offsets.(index) to offsets.(index + 1) - 1 do
          add_split incidences.(incidence) point
        done
      done
    end;
    let crossing_point_id = Array.make !crossing_count (-1)
    and unique_points = ref (Array.make 16 (explicit 0))
    and unique_first = ref (Array.make 16 0)
    and unique_second = ref (Array.make 16 0)
    and unique_count = ref 0 in
    if !crossing_count > 0 then begin
      let original_seen = Bytes.make source_count '\000'
      and original_count = ref 0 in
      let include_original point =
        if Bytes.unsafe_get original_seen point = '\000' then begin
          Bytes.unsafe_set original_seen point '\001'; incr original_count
        end in
      for segment = 0 to segment_count - 1 do
        let a = first.(segment) and b = second.(segment) in
        include_original a; include_original b
      done;
      Array.iter include_original embedded_points;
      if !crossing_count > Sys.max_array_length - !original_count then
        invalid_arg "Planar constraint intersection cardinality exceeds array limits";
      let entries = !original_count + !crossing_count in
      let entry_points = Array.make entries (explicit 0)
      and entry_original = Array.make entries (-1)
      and entry_crossing = Array.make entries (-1) in
      let at = ref 0 in
      for point = 0 to source_count - 1 do
        if Bytes.unsafe_get original_seen point <> '\000' then begin
          entry_points.(!at) <- explicit point;
          entry_original.(!at) <- point;
          incr at
        end
      done;
      for crossing = 0 to !crossing_count - 1 do
        entry_points.(!at) <- (!crossing_points).(crossing);
        entry_crossing.(!at) <- crossing;
        incr at
      done;
      let order = Array.init entries Fun.id in
      Array.sort (fun left right ->
        let compared = Implicit_point.compare_x
            entry_points.(left) entry_points.(right) in
        if compared <> 0 then compared else
        let compared = Implicit_point.compare_y
            entry_points.(left) entry_points.(right) in
        if compared <> 0 then compared else Int.compare left right) order;
      let position = ref 0 in
      while !position < entries do
        let last = ref (!position + 1) and representative = ref (-1) in
        while !last < entries
            && Implicit_point.equal entry_points.(order.(!position))
              entry_points.(order.(!last)) do incr last done;
        for slot = !position to !last - 1 do
          let original = entry_original.(order.(slot)) in
          if original >= 0
              && (!representative < 0 || original < !representative) then
            representative := original
        done;
        let point_id = if !representative >= 0 then !representative else begin
          if !unique_count = Array.length !unique_points then begin
            let capacity = !unique_count * 2 in
            let points = Array.make capacity (explicit 0)
            and firsts = Array.make capacity 0
            and seconds = Array.make capacity 0 in
            Array.blit !unique_points 0 points 0 !unique_count;
            Array.blit !unique_first 0 firsts 0 !unique_count;
            Array.blit !unique_second 0 seconds 0 !unique_count;
            unique_points := points;
            unique_first := firsts;
            unique_second := seconds
          end;
          let crossing = entry_crossing.(order.(!position)) in
          if crossing < 0 then
            invalid_arg "Planar crossing group has no construction";
          (!unique_points).(!unique_count) <- entry_points.(order.(!position));
          (!unique_first).(!unique_count) <- (!crossing_first).(crossing);
          (!unique_second).(!unique_count) <- (!crossing_second).(crossing);
          let result = source_count + !unique_count in
          incr unique_count;
          result
        end in
        for slot = !position to !last - 1 do
          let crossing = entry_crossing.(order.(slot)) in
          if crossing >= 0 then crossing_point_id.(crossing) <- point_id
        done;
        position := !last
      done
    end;
    for crossing = 0 to !crossing_count - 1 do
      let point = crossing_point_id.(crossing) in
      add_split (!crossing_first).(crossing) point;
      add_split (!crossing_second).(crossing) point
    done;
    let total_points = source_count + !unique_count in
    let split_objects = Array.sub !unique_points 0 !unique_count in
    let output_x = Array.make total_points 0. and output_y = Array.make total_points 0. in
    Array.blit x 0 output_x 0 source_count; Array.blit y 0 output_y 0 source_count;
    let split_first_source = Array.make !unique_count 0
    and split_second_source = Array.make !unique_count 0
    and split_parameter = Array.make !unique_count 0. in
    for split = 0 to !unique_count - 1 do
      let px,py,_ = Implicit_point.approximate split_objects.(split) in
      output_x.(source_count + split) <- px; output_y.(source_count + split) <- py;
      let segment = (!unique_first).(split) in
      let a = first.(segment) and b = second.(segment) in
      split_first_source.(split) <- a; split_second_source.(split) <- b;
      let dx = x.(b) -. x.(a) and dy = y.(b) -. y.(a) in
      let parameter = if abs_float dx >= abs_float dy then
          stable_parameter x.(a) x.(b) px
        else stable_parameter y.(a) y.(b) py in
      split_parameter.(split) <- max 0. (min 1. parameter)
    done;
    let event_count = split_segments.length in
    let atomic_capacity =
      if event_count > (Sys.max_array_length - 16) / 2 then 16
      else max 16 (event_count * 2) in
    let atomic = buffer atomic_capacity
    and atomic_winding = buffer (max 16 (atomic_capacity / 2)) in
    if event_count = Array.length segment_points then
      for segment = 0 to segment_count - 1 do
        add atomic first.(segment); add atomic second.(segment);
        add atomic_winding segment_winding.(segment)
      done
    else begin
      let event_order = Array.init event_count Fun.id in
      let object_of_point point = if point < source_count then explicit point
        else split_objects.(point - source_count) in
      let same_point left right =
        if left < source_count && right < source_count then
          x.(left) = x.(right) && y.(left) = y.(right)
        else Implicit_point.equal (object_of_point left) (object_of_point right) in
      Array.sort (fun left right ->
        let ls = split_segments.values.(left)
        and rs = split_segments.values.(right) in
        if ls <> rs then Int.compare ls rs else begin
          let segment = ls and lp = split_points.values.(left)
          and rp = split_points.values.(right) in
          let a = first.(segment) and b = second.(segment) in
          let by_x = abs_float (x.(b) -. x.(a))
              >= abs_float (y.(b) -. y.(a)) in
          let direction = if by_x then Float.compare x.(a) x.(b)
            else Float.compare y.(a) y.(b) in
          let compared = if lp < source_count && rp < source_count then
              if by_x then Float.compare x.(lp) x.(rp)
              else Float.compare y.(lp) y.(rp)
            else if by_x then
              Implicit_point.compare_x (object_of_point lp) (object_of_point rp)
            else Implicit_point.compare_y
                (object_of_point lp) (object_of_point rp) in
          let compared = if direction <= 0 then compared else -compared in
          if compared <> 0 then compared else Int.compare lp rp
        end) event_order;
      let event = ref 0 in
      while !event < event_count do
        let segment = split_segments.values.(event_order.(!event))
        and points = buffer 8 in
        while !event < event_count
            && split_segments.values.(event_order.(!event)) = segment do
          let point = split_points.values.(event_order.(!event)) in
          if points.length = 0
              || not (same_point points.values.(points.length - 1) point) then
            add points point;
          incr event
        done;
        for local = 0 to points.length - 2 do
          if points.values.(local) <> points.values.(local + 1) then begin
            add atomic points.values.(local);
            add atomic points.values.(local + 1);
            add atomic_winding segment_winding.(segment)
          end
        done
      done
    end;
    let atomic_count = atomic.length / 2 in
    let atomic_order = Array.init atomic_count Fun.id in
    let atomic_first segment =
      let a = atomic.values.(segment * 2)
      and b = atomic.values.((segment * 2) + 1) in
      min a b
    and atomic_second segment =
      let a = atomic.values.(segment * 2)
      and b = atomic.values.((segment * 2) + 1) in
      max a b in
    Array.sort (fun left right ->
      let left_a = atomic_first left and left_b = atomic_second left
      and right_a = atomic_first right and right_b = atomic_second right in
      let compared = Int.compare left_a right_a in
      if compared <> 0 then compared else Int.compare left_b right_b)
      atomic_order;
    let unique_constraints = buffer atomic.length
    and unique_winding = buffer (max 16 atomic_count)
    and previous_a = ref (-1) and previous_b = ref (-1) in
    Array.iter (fun segment ->
      let a = atomic_first segment and b = atomic_second segment in
      let winding = atomic_winding.values.(segment) in
      let winding = if atomic.values.(segment * 2) = a then winding
        else if winding = min_int then
          invalid_arg "Planar constraint winding exceeds integer range"
        else -winding in
      if a <> !previous_a || b <> !previous_b then begin
        add unique_constraints a; add unique_constraints b;
        add unique_winding winding;
        previous_a := a; previous_b := b
      end else begin
        let previous = unique_winding.values.(unique_winding.length - 1) in
        if (winding > 0 && previous > max_int - winding)
            || (winding < 0 && previous < min_int - winding) then
          invalid_arg "Planar constraint winding exceeds integer range";
        unique_winding.values.(unique_winding.length - 1) <- previous + winding
      end) atomic_order;
    let inserted = Array.init !unique_count (fun split -> source_count + split) in
    Ok { source_x=x; source; explicit_cache;
      split_points=split_objects; x=output_x; y=output_y; inserted;
      constraints=Array.sub unique_constraints.values 0 unique_constraints.length;
      constraint_winding=Array.sub unique_winding.values 0 unique_winding.length;
      split_first=split_first_source; split_second=split_second_source;
      split_parameter }
  with
  | Cancel.Cancelled -> Error "Planar constraint arrangement was cancelled"
  | Invalid_argument message -> Error message
