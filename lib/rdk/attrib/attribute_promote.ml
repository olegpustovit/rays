
type method_ =
  | First
  | Last
  | Average
  | Minimum
  | Maximum
  | Mode
  | Median
  | Sum
  | Sum_squares
  | Root_mean_square
  | Array_all
  | Unique_values

type relation = Attribute_relation.t =
  | Identity of int
  | Constant_zero of int
  | Direct of int array
  | All of { source_count : int }
  | Csr_identity of { offsets : int array }
  | Csr_map of { offsets : int array; indices : int array }

module String_key = Hashtbl.Make (struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

let owner_count geometry = function
  | Attribute.Point -> Geometry.point_count geometry
  | Attribute.Vertex -> Geometry.vertex_count geometry
  | Attribute.Primitive -> Geometry.primitive_count geometry
  | Attribute.Detail -> 1

let sort_int_range ?cancel values first last =
  let swap left right =
    let value = values.(left) in
    values.(left) <- values.(right);
    values.(right) <- value
  in
  let sift root finish =
    let root = ref root and active = ref true in
    while !active do
      let child = ((!root - first) * 2) + 1 + first in
      if child >= finish then active := false
      else begin
        let child = if child + 1 < finish
            && values.(child) < values.(child + 1) then child + 1 else child in
        if values.(!root) < values.(child) then begin
          swap !root child;
          root := child
        end else active := false
      end
    done
  in
  let length = last - first in
  if length < 16 then
    for at = first + 1 to last - 1 do
      if at land 4095 = 0 then Cancel.check_opt cancel;
      let value = values.(at) and slot = ref at in
      while !slot > first && values.(!slot - 1) > value do
        values.(!slot) <- values.(!slot - 1);
        decr slot
      done;
      values.(!slot) <- value
    done
  else begin
    for root = first + (length / 2) - 1 downto first do
      if root land 4095 = 0 then Cancel.check_opt cancel;
      sift root last
    done;
    for finish = last - 1 downto first + 1 do
      if finish land 4095 = 0 then Cancel.check_opt cancel;
      swap first finish;
      sift first finish
    done
  end

let sort_float_range ?cancel values first last =
  let swap left right =
    let value = values.(left) in
    values.(left) <- values.(right);
    values.(right) <- value
  in
  let sift root finish =
    let root = ref root and active = ref true in
    while !active do
      let child = ((!root - first) * 2) + 1 + first in
      if child >= finish then active := false
      else begin
        let child = if child + 1 < finish
            && Float.compare values.(child) values.(child + 1) < 0
          then child + 1 else child in
        if Float.compare values.(!root) values.(child) < 0 then begin
          swap !root child;
          root := child
        end else active := false
      end
    done
  in
  let length = last - first in
  if length < 16 then
    for at = first + 1 to last - 1 do
      if at land 4095 = 0 then Cancel.check_opt cancel;
      let value = values.(at) and slot = ref at in
      while !slot > first && Float.compare values.(!slot - 1) value > 0 do
        values.(!slot) <- values.(!slot - 1);
        decr slot
      done;
      values.(!slot) <- value
    done
  else begin
    for root = first + (length / 2) - 1 downto first do
      if root land 4095 = 0 then Cancel.check_opt cancel;
      sift root last
    done;
    for finish = last - 1 downto first + 1 do
      if finish land 4095 = 0 then Cancel.check_opt cancel;
      swap first finish;
      sift first finish
    done
  end

let sort_text_range ?cancel values first last =
  let swap left right =
    let value = values.(left) in
    values.(left) <- values.(right);
    values.(right) <- value
  in
  let sift root finish =
    let root = ref root and active = ref true in
    while !active do
      let child = ((!root - first) * 2) + 1 + first in
      if child >= finish then active := false
      else begin
        let child = if child + 1 < finish
            && String.compare values.(child) values.(child + 1) < 0
          then child + 1 else child in
        if String.compare values.(!root) values.(child) < 0 then begin
          swap !root child;
          root := child
        end else active := false
      end
    done
  in
  let length = last - first in
  if length < 16 then
    for at = first + 1 to last - 1 do
      if at land 4095 = 0 then Cancel.check_opt cancel;
      let value = values.(at) and slot = ref at in
      while !slot > first && String.compare values.(!slot - 1) value > 0 do
        values.(!slot) <- values.(!slot - 1);
        decr slot
      done;
      values.(!slot) <- value
    done
  else begin
    for root = first + (length / 2) - 1 downto first do
      if root land 4095 = 0 then Cancel.check_opt cancel;
      sift root last
    done;
    for finish = last - 1 downto first + 1 do
      if finish land 4095 = 0 then Cancel.check_opt cancel;
      swap first finish;
      sift first finish
    done
  end

let select_range ?cancel ~compare ~sort values first last nth =
  let swap left right =
    let value = values.(left) in
    values.(left) <- values.(right);
    values.(right) <- value
  in
  let median_of_three left middle right =
    let a = values.(left) and b = values.(middle) and c = values.(right) in
    if compare a b <= 0 then
      if compare b c <= 0 then b else if compare a c <= 0 then c else a
    else if compare a c <= 0 then a else if compare b c <= 0 then c else b
  in
  let rec logarithm value result =
    if value <= 1 then result else logarithm (value lsr 1) (result + 1)
  in
  let left = ref first and right = ref last
  and budget = ref (2 * logarithm (last - first) 0)
  and selected = ref false in
  while not !selected && !right - !left > 16 && !budget > 0 do
    Cancel.check_opt cancel;
    decr budget;
    let pivot = median_of_three !left (!left + ((!right - !left) / 2))
        (!right - 1) in
    let lower = ref !left and scan = ref !left and upper = ref !right in
    while !scan < !upper do
      if !scan land 16_383 = 0 then Cancel.check_opt cancel;
      let order = compare values.(!scan) pivot in
      if order < 0 then begin
        swap !lower !scan;
        incr lower;
        incr scan
      end else if order > 0 then begin
        decr upper;
        swap !scan !upper
      end else incr scan
    done;
    if nth < !lower then right := !lower
    else if nth >= !upper then left := !upper
    else selected := true
  done;
  if not !selected then sort values !left !right;
  values.(nth)

let mode_sorted ?cancel compare values first last =
  let best = ref first and best_count = ref 1 and run = ref first in
  for slot = first + 1 to last do
    if slot land 16_383 = 0 then Cancel.check_opt cancel;
    if slot = last || compare values.(slot) values.(!run) <> 0 then begin
      let count = slot - !run in
      if count > !best_count then begin
        best := !run;
        best_count := count
      end;
      run := slot
    end
  done;
  values.(!best)

let mode_int_range ?cancel values first last =
  let length = last - first in
  if length < 4_096 then begin
    sort_int_range ?cancel values first last;
    mode_sorted ?cancel Int.compare values first last
  end else begin
    let minimum = ref values.(first) and maximum = ref values.(first) in
    for slot = first + 1 to last - 1 do
      if slot land 16_383 = 0 then Cancel.check_opt cancel;
      minimum := Int.min !minimum values.(slot);
      maximum := Int.max !maximum values.(slot)
    done;
    let span = Int64.(add (sub (of_int !maximum) (of_int !minimum)) 1L) in
    let dense_limit = if length > Sys.max_array_length / 2
        then Sys.max_array_length else length * 2 in
    if span > 0L && span <= Int64.of_int dense_limit
    then begin
      let counts = Array.make (Int64.to_int span) 0 in
      for slot = first to last - 1 do
        let bucket = values.(slot) - !minimum in
        counts.(bucket) <- counts.(bucket) + 1
      done;
      let best = ref 0 in
      for bucket = 1 to Array.length counts - 1 do
        if counts.(bucket) > counts.(!best) then best := bucket
      done;
      !minimum + !best
    end else begin
      sort_int_range ?cancel values first last;
      mode_sorted ?cancel Int.compare values first last
    end
  end

let reduction_chunk_size ~grain relation =
  let destinations = Attribute_relation.count relation in
  if destinations = 0 then grain
  else
    let incidences = Attribute_relation.incidence_count relation in
    let average = max 1 ((incidences + destinations - 1) / destinations) in
    max 1 (grain / average)

let fill_incidence_plane ?cancel ~grain ~default relation source =
  let count = Attribute_relation.count relation in
  let values = Array.make (Attribute_relation.incidence_count relation) default in
  if count > 0 then Parallel.for_
      ~chunk_size:(reduction_chunk_size ~grain relation)
      ~start:0 ~finish:(count - 1)
      (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let first = Attribute_relation.flat_first relation destination
        and last = Attribute_relation.flat_last relation destination in
        for slot = first to last - 1 do
          if slot land 16_383 = 0 then Cancel.check_opt cancel;
          values.(slot) <- source.(Attribute_relation.source_flat relation destination slot)
        done);
  values

let radix_sort_nonnegative ?cancel values =
  let length = Array.length values in
  if length < 2 then ()
  else if length < 4_096 then Array.sort Int.compare values
  else begin
    let maximum = Array.fold_left Int.max 0 values in
    let rec pass_count value count =
      if value = 0 then max 1 count else pass_count (value lsr 16) (count + 1)
    in
    let passes = pass_count maximum 0 in
    let scratch = Array.make length 0 and counts = Array.make 65_536 0 in
    let source = ref values and target = ref scratch in
    for pass = 0 to passes - 1 do
      Cancel.check_opt cancel;
      Array.fill counts 0 (Array.length counts) 0;
      let shift = pass * 16 in
      for at = 0 to length - 1 do
        if at land 16_383 = 0 then Cancel.check_opt cancel;
        let bucket = ((!source).(at) lsr shift) land 0xffff in
        counts.(bucket) <- counts.(bucket) + 1
      done;
      let offset = ref 0 in
      for bucket = 0 to Array.length counts - 1 do
        let size = counts.(bucket) in
        counts.(bucket) <- !offset;
        offset := !offset + size
      done;
      for at = 0 to length - 1 do
        let value = (!source).(at) in
        let bucket = (value lsr shift) land 0xffff in
        (!target).(counts.(bucket)) <- value;
        counts.(bucket) <- counts.(bucket) + 1
      done;
      let previous = !source in
      source := !target;
      target := previous
    done;
    if !source != values then Array.blit !source 0 values 0 length
  end

let primitive_to_point ?cancel geometry index =
  let index_view = Topology_index.Private.view index in
  let point_count = Geometry.point_count geometry in
  let offsets = Array.make (point_count + 1) 0 in
  for point = 0 to point_count - 1 do
    if point land 4095 = 0 then Cancel.check_opt cancel;
    let first = index_view.point_offsets.(point)
    and last = index_view.point_offsets.(point + 1)
    and previous = ref (-1) in
    for slot = first to last - 1 do
      let primitive = index_view.primitive_of_vertex.(index_view.point_vertices.(slot)) in
      if primitive <> !previous then begin
        offsets.(point + 1) <- offsets.(point + 1) + 1;
        previous := primitive
      end
    done
  done;
  for point = 0 to point_count - 1 do
    offsets.(point + 1) <- offsets.(point + 1) + offsets.(point)
  done;
  let indices = Array.make offsets.(point_count) 0 in
  for point = 0 to point_count - 1 do
    let output = ref offsets.(point) and previous = ref (-1) in
    for slot = index_view.point_offsets.(point)
        to index_view.point_offsets.(point + 1) - 1 do
      let primitive = index_view.primitive_of_vertex.(index_view.point_vertices.(slot)) in
      if primitive <> !previous then begin
        indices.(!output) <- primitive;
        incr output;
        previous := primitive
      end
    done
  done;
  Csr_map { offsets; indices }

let make_relation ?cancel ~source ~destination geometry =
  let topology = Geometry.topology geometry
  and topology_view = Topology.Private.view (Geometry.topology geometry) in
  if source = destination then Identity (owner_count geometry destination)
  else match source, destination with
  | Attribute.Detail, destination -> Constant_zero (owner_count geometry destination)
  | source, Attribute.Detail -> All { source_count = owner_count geometry source }
  | Attribute.Point, Attribute.Vertex -> Direct topology_view.vertex_points
  | Attribute.Primitive, Attribute.Vertex ->
      let index = Topology_index.create ?cancel topology in
      Direct (Topology_index.Private.view index).primitive_of_vertex
  | Attribute.Vertex, Attribute.Point ->
      let index = Topology_index.create ?cancel topology in
      let view = Topology_index.Private.view index in
      Csr_map { offsets = view.point_offsets; indices = view.point_vertices }
  | Attribute.Vertex, Attribute.Primitive ->
      Csr_identity { offsets = topology_view.primitive_offsets }
  | Attribute.Point, Attribute.Primitive ->
      Csr_map { offsets = topology_view.primitive_offsets;
                indices = topology_view.vertex_points }
  | Attribute.Primitive, Attribute.Point ->
      let index = Topology_index.create ?cancel topology in
      primitive_to_point ?cancel geometry index
  | (Attribute.Point, Attribute.Point)
  | (Attribute.Vertex, Attribute.Vertex)
  | (Attribute.Primitive, Attribute.Primitive) ->
      Identity (owner_count geometry destination)

let table_capacity count =
  if count > Sys.max_array_length / 2 then
    invalid_arg "Rdk.Attribute_ops.promote: piece table is too large";
  let target = max 16 (count * 2) in
  let rec grow value =
    if value >= target then value
    else if value > Sys.max_array_length / 2 then
      invalid_arg "Rdk.Attribute_ops.promote: piece table is too large"
    else grow (value * 2)
  in
  grow 16

let int_piece_ids values =
  let count = Array.length values in
  if count = 0 then [||], 0
  else begin
    let minimum = ref values.(0) and maximum = ref values.(0) in
    for at = 1 to count - 1 do
      minimum := Int.min !minimum values.(at);
      maximum := Int.max !maximum values.(at)
    done;
    let span = Int64.(add (sub (of_int !maximum) (of_int !minimum)) 1L) in
    let dense_limit = min Sys.max_array_length
        (if count > Sys.max_array_length / 2 then Sys.max_array_length
         else max 16 (count * 2)) in
    let output = Array.make count 0 and next = ref 0 in
    if span > 0L && span <= Int64.of_int dense_limit then begin
      let ids = Array.make (Int64.to_int span) (-1) in
      for at = 0 to count - 1 do
        let slot = values.(at) - !minimum in
        if ids.(slot) < 0 then begin
          ids.(slot) <- !next;
          incr next
        end;
        output.(at) <- ids.(slot)
      done
    end else begin
      let capacity = table_capacity count in
      let keys = Array.make capacity 0 and ids = Array.make capacity (-1) in
      let mask = capacity - 1 in
      for at = 0 to count - 1 do
        let key = values.(at) in
        let hash = ((key lxor (key lsr 16)) * 0x45d9f3b) land max_int in
        let slot = ref (hash land mask) in
        while ids.(!slot) >= 0 && keys.(!slot) <> key do
          slot := (!slot + 1) land mask
        done;
        if ids.(!slot) < 0 then begin
          keys.(!slot) <- key;
          ids.(!slot) <- !next;
          incr next
        end;
        output.(at) <- ids.(!slot)
      done
    end;
    output, !next
  end

let text_piece_ids values =
  let count = Array.length values in
  let table = String_key.create (max 16 (min count 4_096)) and next = ref 0 in
  let output = Array.make count 0 in
  for at = 0 to count - 1 do
    let key = values.(at) in
    let id = match String_key.find_opt table key with
      | Some id -> id
      | None ->
          let id = !next in
          incr next;
          String_key.add table key id;
          id
    in
    output.(at) <- id
  done;
  output, !next

let make_piece_relation ?cancel ~source_count ~piece_attribute base_relation =
  let piece_of_destination, piece_count =
    match Attribute.Private.storage piece_attribute with
    | Attribute.Int values -> int_piece_ids values
    | Attribute.Text values -> text_piece_ids values
    | Attribute.Float _ | Attribute.Float2 _ | Attribute.Float3 _
    | Attribute.Float4 _ | Attribute.Int_array _ | Attribute.Float_array _ ->
        raise (Invalid_argument
          "Rdk.Attribute_ops.promote: piece attribute must use integer or text storage")
  in
  match base_relation with
  | Identity count ->
      let offsets = Array.make (piece_count + 1) 0 in
      for destination = 0 to count - 1 do
        if destination land 16_383 = 0 then Cancel.check_opt cancel;
        let piece = piece_of_destination.(destination) in
        offsets.(piece + 1) <- offsets.(piece + 1) + 1
      done;
      for piece = 0 to piece_count - 1 do
        offsets.(piece + 1) <- offsets.(piece + 1) + offsets.(piece)
      done;
      let cursors = Array.copy offsets and indices = Array.make count 0 in
      for destination = 0 to count - 1 do
        let piece = piece_of_destination.(destination) in
        indices.(cursors.(piece)) <- destination;
        cursors.(piece) <- cursors.(piece) + 1
      done;
      Csr_map { offsets; indices }, piece_of_destination
  | Constant_zero _ | Direct _ | All _ | Csr_identity _ | Csr_map _ ->
      if source_count > 0 && piece_count > max_int / source_count then
        invalid_arg "Rdk.Attribute_ops.promote: piece/source key space is too large";
      let pair_count = Attribute_relation.incidence_count base_relation in
      let pairs = Array.make pair_count 0 in
      for destination = 0 to Attribute_relation.count base_relation - 1 do
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let piece = piece_of_destination.(destination) in
        for slot = Attribute_relation.flat_first base_relation destination
            to Attribute_relation.flat_last base_relation destination - 1 do
          let source = Attribute_relation.source_flat base_relation destination slot in
          pairs.(slot) <- (piece * source_count) + source
        done
      done;
      radix_sort_nonnegative ?cancel pairs;
      let unique_count = ref 0 and previous = ref (-1) in
      let offsets = Array.make (piece_count + 1) 0 in
      for at = 0 to pair_count - 1 do
        let key = pairs.(at) in
        if key <> !previous then begin
          incr unique_count;
          offsets.((key / source_count) + 1) <-
            offsets.((key / source_count) + 1) + 1;
          previous := key
        end
      done;
      for piece = 0 to piece_count - 1 do
        offsets.(piece + 1) <- offsets.(piece + 1) + offsets.(piece)
      done;
      let indices = Array.make !unique_count 0 in
      let output = ref 0 in
      previous := -1;
      for at = 0 to pair_count - 1 do
        let key = pairs.(at) in
        if key <> !previous then begin
          indices.(!output) <- key mod source_count;
          incr output;
          previous := key
        end
      done;
      Csr_map { offsets; indices }, piece_of_destination

let reduce_float ?cancel method_ relation source destination =
  let first = Attribute_relation.first relation destination
  and last = Attribute_relation.last relation destination in
  if first = last then 0.
  else match method_ with
  | First -> source.(Attribute_relation.source relation destination first)
  | Last -> source.(Attribute_relation.source relation destination (last - 1))
  | Minimum | Maximum ->
      let result = ref source.(Attribute_relation.source relation destination first) in
      for slot = first + 1 to last - 1 do
        if slot land 16_383 = 0 then Cancel.check_opt cancel;
        let value = source.(Attribute_relation.source relation destination slot) in
        result := (if method_ = Minimum then Float.min !result value
                   else Float.max !result value)
      done;
      !result
  | Mode | Median | Array_all | Unique_values -> assert false
  | Average | Sum | Sum_squares | Root_mean_square ->
      let result = ref 0. in
      for slot = first to last - 1 do
        if slot land 16_383 = 0 then Cancel.check_opt cancel;
        let value = source.(Attribute_relation.source relation destination slot) in
        result := !result +. (if method_ = Sum_squares || method_ = Root_mean_square
                              then value *. value else value)
      done;
      (match method_ with
      | Average -> !result /. float_of_int (last - first)
      | Root_mean_square -> sqrt (!result /. float_of_int (last - first))
      | Sum | Sum_squares -> !result
      | First | Last | Minimum | Maximum | Mode | Median
      | Array_all | Unique_values -> assert false)

let reduce_int ?cancel method_ relation source destination =
  let first = Attribute_relation.first relation destination
  and last = Attribute_relation.last relation destination in
  if first = last then 0
  else match method_ with
  | First -> source.(Attribute_relation.source relation destination first)
  | Last -> source.(Attribute_relation.source relation destination (last - 1))
  | Minimum | Maximum ->
      let result = ref source.(Attribute_relation.source relation destination first) in
      for slot = first + 1 to last - 1 do
        if slot land 16_383 = 0 then Cancel.check_opt cancel;
        let value = source.(Attribute_relation.source relation destination slot) in
        result := (if method_ = Minimum then Int.min !result value
                   else Int.max !result value)
      done;
      !result
  | Average | Sum | Sum_squares | Root_mean_square | Mode | Median
  | Array_all | Unique_values -> assert false

let reduce_text method_ relation source destination =
  let first = Attribute_relation.first relation destination
  and last = Attribute_relation.last relation destination in
  if first = last then ""
  else match method_ with
  | First -> source.(Attribute_relation.source relation destination first)
  | Last -> source.(Attribute_relation.source relation destination (last - 1))
  | Average | Minimum | Maximum | Mode | Median | Sum | Sum_squares
  | Root_mean_square | Array_all | Unique_values -> assert false

let sorted_float_plane ?cancel ~grain method_ relation source =
  let count = Attribute_relation.count relation and output = Array.make (Attribute_relation.count relation) 0. in
  let values = fill_incidence_plane ?cancel ~grain ~default:0. relation source in
  if count > 0 then Parallel.for_
      ~chunk_size:(reduction_chunk_size ~grain relation)
      ~start:0 ~finish:(count - 1)
      (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let first = Attribute_relation.flat_first relation destination
        and last = Attribute_relation.flat_last relation destination in
        if first < last then begin
          output.(destination) <- match method_ with
            | Median -> select_range ?cancel ~compare:Float.compare
                ~sort:(sort_float_range ?cancel) values first last
                (first + ((last - first) / 2))
            | Mode ->
                sort_float_range ?cancel values first last;
                mode_sorted ?cancel Float.compare values first last
            | First | Last | Average | Minimum | Maximum | Sum | Sum_squares
            | Root_mean_square | Array_all | Unique_values -> assert false
        end);
  output

let sorted_int_plane ?cancel ~grain method_ relation source =
  let count = Attribute_relation.count relation and output = Array.make (Attribute_relation.count relation) 0 in
  let values = fill_incidence_plane ?cancel ~grain ~default:0 relation source in
  if count > 0 then Parallel.for_
      ~chunk_size:(reduction_chunk_size ~grain relation)
      ~start:0 ~finish:(count - 1)
      (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let first = Attribute_relation.flat_first relation destination
        and last = Attribute_relation.flat_last relation destination in
        if first < last then begin
          output.(destination) <- match method_ with
            | Median -> select_range ?cancel ~compare:Int.compare
                ~sort:(sort_int_range ?cancel) values first last
                (first + ((last - first) / 2))
            | Mode -> mode_int_range ?cancel values first last
            | First | Last | Average | Minimum | Maximum | Sum | Sum_squares
            | Root_mean_square | Array_all | Unique_values -> assert false
        end);
  output

let sorted_text_plane ?cancel ~grain method_ relation source =
  let count = Attribute_relation.count relation and output = Array.make (Attribute_relation.count relation) "" in
  let values = fill_incidence_plane ?cancel ~grain ~default:"" relation source in
  if count > 0 then Parallel.for_
      ~chunk_size:(reduction_chunk_size ~grain relation)
      ~start:0 ~finish:(count - 1)
      (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let first = Attribute_relation.flat_first relation destination
        and last = Attribute_relation.flat_last relation destination in
        if first < last then begin
          output.(destination) <- match method_ with
            | Median -> select_range ?cancel ~compare:String.compare
                ~sort:(sort_text_range ?cancel) values first last
                (first + ((last - first) / 2))
            | Mode ->
                sort_text_range ?cancel values first last;
                mode_sorted ?cancel String.compare values first last
            | First | Last | Average | Minimum | Maximum | Sum | Sum_squares
            | Root_mean_square | Array_all | Unique_values -> assert false
        end);
  output

let float_plane ?cancel ~grain method_ relation source =
  match method_ with
  | Mode | Median -> sorted_float_plane ?cancel ~grain method_ relation source
  | First | Last | Average | Minimum | Maximum | Sum | Sum_squares
  | Root_mean_square ->
      let count = Attribute_relation.count relation
      and output = Array.make (Attribute_relation.count relation) 0. in
      if count > 0 then Parallel.for_
          ~chunk_size:(reduction_chunk_size ~grain relation)
          ~start:0 ~finish:(count - 1)
          (fun destination ->
            if destination land 4095 = 0 then Cancel.check_opt cancel;
            output.(destination) <- reduce_float ?cancel method_ relation source
                destination);
      output
  | Array_all | Unique_values -> assert false

let int_plane ?cancel ~grain method_ relation source =
  match method_ with
  | Mode | Median -> sorted_int_plane ?cancel ~grain method_ relation source
  | First | Last | Minimum | Maximum ->
      let count = Attribute_relation.count relation and output = Array.make (Attribute_relation.count relation) 0 in
      if count > 0 then Parallel.for_
          ~chunk_size:(reduction_chunk_size ~grain relation)
          ~start:0 ~finish:(count - 1)
          (fun destination ->
            if destination land 4095 = 0 then Cancel.check_opt cancel;
            output.(destination) <- reduce_int ?cancel method_ relation source
                destination);
      output
  | Average | Sum | Sum_squares | Root_mean_square
  | Array_all | Unique_values -> assert false

let text_plane ?cancel ~grain method_ relation source =
  match method_ with
  | Mode | Median | Average -> sorted_text_plane ?cancel ~grain
      (if method_ = Average then Median else method_) relation source
  | Sum ->
      let count = Attribute_relation.count relation in
      let output = Array.make count "" in
      if count > 0 then Parallel.for_
          ~chunk_size:(reduction_chunk_size ~grain relation)
          ~start:0 ~finish:(count - 1) (fun destination ->
            if destination land 4095 = 0 then Cancel.check_opt cancel;
            let first = Attribute_relation.first relation destination
            and last = Attribute_relation.last relation destination
            and length = ref 0 in
            for slot = first to last - 1 do
              if slot land 16_383 = 0 then Cancel.check_opt cancel;
              let source_index = Attribute_relation.source relation destination slot in
              let source_length = String.length source.(source_index) in
              if source_length > Sys.max_string_length - !length then
                invalid_arg
                  "Rdk.Attribute_ops.promote: concatenated text exceeds string limits";
              length := !length + source_length
            done;
            if !length > 0 then begin
              let bytes = Bytes.create !length and at = ref 0 in
              for slot = first to last - 1 do
                let value = source.(Attribute_relation.source relation destination slot) in
                let length = String.length value in
                Bytes.blit_string value 0 bytes !at length;
                at := !at + length
              done;
              output.(destination) <- Bytes.unsafe_to_string bytes
            end);
      output
  | First | Last | Minimum | Maximum | Sum_squares | Root_mean_square ->
      let count = Attribute_relation.count relation
      and output = Array.make (Attribute_relation.count relation) "" in
      if count > 0 then Parallel.for_
          ~chunk_size:(reduction_chunk_size ~grain relation)
          ~start:0 ~finish:(count - 1)
          (fun destination ->
            if destination land 4095 = 0 then Cancel.check_opt cancel;
            output.(destination) <- reduce_text
                (if method_ = Last then Last else First) relation source destination);
      output
  | Array_all | Unique_values ->
      assert false

let indexed_method = function
  | First | Last | Minimum | Maximum | Mode -> true
  | Average | Median | Sum | Sum_squares | Root_mean_square
  | Array_all | Unique_values -> false

let indexed_float_plane ?cancel ~grain method_ relation source =
  match method_ with
  | Mode ->
      let output = sorted_float_plane ?cancel ~grain Mode relation source
      and indices = Array.make (Attribute_relation.count relation) (-1) in
      let count = Attribute_relation.count relation in
      if count > 0 then Parallel.for_
          ~chunk_size:(reduction_chunk_size ~grain relation)
          ~start:0 ~finish:(count - 1)
          (fun destination ->
            if destination land 4095 = 0 then Cancel.check_opt cancel;
            let first = Attribute_relation.first relation destination
            and last = Attribute_relation.last relation destination
            and found = ref (-1) in
            let slot = ref first in
            while !slot < last && !found < 0 do
              if !slot land 16_383 = 0 then Cancel.check_opt cancel;
              let source_index = Attribute_relation.source relation destination !slot in
              if Float.compare source.(source_index) output.(destination) = 0
              then found := source_index;
              incr slot
            done;
            indices.(destination) <- !found);
      output, indices
  | First | Last | Minimum | Maximum ->
      let count = Attribute_relation.count relation
      and output = Array.make (Attribute_relation.count relation) 0.
      and indices = Array.make (Attribute_relation.count relation) (-1) in
      if count > 0 then Parallel.for_
          ~chunk_size:(reduction_chunk_size ~grain relation)
          ~start:0 ~finish:(count - 1)
          (fun destination ->
            if destination land 4095 = 0 then Cancel.check_opt cancel;
            let first = Attribute_relation.first relation destination
            and last = Attribute_relation.last relation destination in
            if first < last then begin
              let slot = if method_ = Last then last - 1 else first in
              let source_index = ref (Attribute_relation.source relation destination slot) in
              let value = ref source.(!source_index) in
              if method_ = Minimum || method_ = Maximum then
                for slot = first + 1 to last - 1 do
                  if slot land 16_383 = 0 then Cancel.check_opt cancel;
                  let candidate_index = Attribute_relation.source relation destination slot in
                  let candidate = source.(candidate_index) in
                  let reduced = if method_ = Minimum
                      then Float.min !value candidate
                      else Float.max !value candidate in
                  if Int64.bits_of_float reduced
                      <> Int64.bits_of_float !value then
                    source_index := candidate_index;
                  value := reduced
                done;
              output.(destination) <- !value;
              indices.(destination) <- !source_index
            end);
      output, indices
  | Average | Median | Sum | Sum_squares | Root_mean_square
  | Array_all | Unique_values -> assert false

let indexed_int_plane ?cancel ~grain method_ relation source =
  match method_ with
  | Mode ->
      let output = sorted_int_plane ?cancel ~grain Mode relation source
      and indices = Array.make (Attribute_relation.count relation) (-1) in
      let count = Attribute_relation.count relation in
      if count > 0 then Parallel.for_
          ~chunk_size:(reduction_chunk_size ~grain relation)
          ~start:0 ~finish:(count - 1)
          (fun destination ->
            if destination land 4095 = 0 then Cancel.check_opt cancel;
            let first = Attribute_relation.first relation destination
            and last = Attribute_relation.last relation destination
            and found = ref (-1) and slot = ref 0 in
            slot := first;
            while !slot < last && !found < 0 do
              if !slot land 16_383 = 0 then Cancel.check_opt cancel;
              let source_index = Attribute_relation.source relation destination !slot in
              if source.(source_index) = output.(destination)
              then found := source_index;
              incr slot
            done;
            indices.(destination) <- !found);
      output, indices
  | First | Last | Minimum | Maximum ->
      let count = Attribute_relation.count relation
      and output = Array.make (Attribute_relation.count relation) 0
      and indices = Array.make (Attribute_relation.count relation) (-1) in
      if count > 0 then Parallel.for_
          ~chunk_size:(reduction_chunk_size ~grain relation)
          ~start:0 ~finish:(count - 1)
          (fun destination ->
            if destination land 4095 = 0 then Cancel.check_opt cancel;
            let first = Attribute_relation.first relation destination
            and last = Attribute_relation.last relation destination in
            if first < last then begin
              let slot = if method_ = Last then last - 1 else first in
              let source_index = ref (Attribute_relation.source relation destination slot) in
              let value = ref source.(!source_index) in
              if method_ = Minimum || method_ = Maximum then
                for slot = first + 1 to last - 1 do
                  if slot land 16_383 = 0 then Cancel.check_opt cancel;
                  let candidate_index = Attribute_relation.source relation destination slot in
                  let candidate = source.(candidate_index) in
                  if (method_ = Minimum && candidate < !value)
                      || (method_ = Maximum && candidate > !value) then begin
                    value := candidate;
                    source_index := candidate_index
                  end
                done;
              output.(destination) <- !value;
              indices.(destination) <- !source_index
            end);
      output, indices
  | Average | Median | Sum | Sum_squares | Root_mean_square
  | Array_all | Unique_values -> assert false

let indexed_text_plane ?cancel ~grain method_ relation source =
  match method_ with
  | Mode ->
      let output = sorted_text_plane ?cancel ~grain Mode relation source
      and indices = Array.make (Attribute_relation.count relation) (-1) in
      let count = Attribute_relation.count relation in
      if count > 0 then Parallel.for_
          ~chunk_size:(reduction_chunk_size ~grain relation)
          ~start:0 ~finish:(count - 1)
          (fun destination ->
            if destination land 4095 = 0 then Cancel.check_opt cancel;
            let first = Attribute_relation.first relation destination
            and last = Attribute_relation.last relation destination
            and found = ref (-1) and slot = ref 0 in
            slot := first;
            while !slot < last && !found < 0 do
              if !slot land 16_383 = 0 then Cancel.check_opt cancel;
              let source_index = Attribute_relation.source relation destination !slot in
              if String.equal source.(source_index) output.(destination)
              then found := source_index;
              incr slot
            done;
            indices.(destination) <- !found);
      output, indices
  | First | Last | Minimum | Maximum ->
      let count = Attribute_relation.count relation
      and output = Array.make (Attribute_relation.count relation) ""
      and indices = Array.make (Attribute_relation.count relation) (-1) in
      if count > 0 then Parallel.for_
          ~chunk_size:(reduction_chunk_size ~grain relation)
          ~start:0 ~finish:(count - 1)
          (fun destination ->
            if destination land 4095 = 0 then Cancel.check_opt cancel;
            let first = Attribute_relation.first relation destination
            and last = Attribute_relation.last relation destination in
            if first < last then begin
              let slot = if method_ = Last then last - 1 else first in
              let source_index = Attribute_relation.source relation destination slot in
              output.(destination) <- source.(source_index);
              indices.(destination) <- source_index
            end);
      output, indices
  | Average | Median | Sum | Sum_squares
  | Root_mean_square | Array_all | Unique_values -> assert false

let fixed_int_tuple_array ?cancel ~grain components =
  let width = Array.length components in
  if width = 0 then invalid_arg
      "Rdk.Attribute_ops.promote: integer tuple width must be positive";
  let count = Array.length components.(0) in
  Array.iter (fun component ->
    if Array.length component <> count then invalid_arg
        "Rdk.Attribute_ops.promote: integer tuple component lengths differ")
    components;
  if count > Sys.max_array_length / width then invalid_arg
      "Rdk.Attribute_ops.promote: integer tuple index output is too large";
  let offsets = Array.init (count + 1) (fun destination -> destination * width)
  and values = Array.make (count * width) (-1) in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
      (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let first = destination * width in
        for component = 0 to width - 1 do
          values.(first + component) <- components.(component).(destination)
        done);
  Packed.Int_array.Private.create_validated_owned ~offsets ~values

let map_plane ?cancel ~grain mapping values default =
  let count = Array.length mapping and output = Array.make (Array.length mapping) default in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
      (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        output.(destination) <- values.(mapping.(destination)));
  output

let relation_offsets relation =
  let count = Attribute_relation.count relation in
  Array.init (count + 1) (fun destination ->
    if destination = count then Attribute_relation.incidence_count relation
    else Attribute_relation.flat_first relation destination)

let int_array_all ?cancel ~grain relation source =
  let offsets = relation_offsets relation
  and values = fill_incidence_plane ?cancel ~grain ~default:0 relation source in
  Packed.Int_array.Private.create_validated_owned ~offsets ~values

let float_array_all ?cancel ~grain relation source =
  let offsets = relation_offsets relation
  and values = fill_incidence_plane ?cancel ~grain ~default:0. relation source in
  Packed.Float_array.Private.create_validated_owned ~offsets ~values

let int_array_unique ?cancel ~grain relation source =
  let count = Attribute_relation.count relation
  and sorted = fill_incidence_plane ?cancel ~grain ~default:0 relation source in
  let counts = Array.make count 0 in
  if count > 0 then Parallel.for_
      ~chunk_size:(reduction_chunk_size ~grain relation)
      ~start:0 ~finish:(count - 1) (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let first = Attribute_relation.flat_first relation destination
        and last = Attribute_relation.flat_last relation destination in
        sort_int_range ?cancel sorted first last;
        if first < last then begin
          let unique = ref 1 in
          for slot = first + 1 to last - 1 do
            if slot land 16_383 = 0 then Cancel.check_opt cancel;
            if sorted.(slot) <> sorted.(slot - 1) then incr unique
          done;
          counts.(destination) <- !unique
        end);
  let offsets = Array.make (count + 1) 0 in
  for destination = 0 to count - 1 do
    offsets.(destination + 1) <- offsets.(destination) + counts.(destination)
  done;
  let values = Array.make offsets.(count) 0 in
  if count > 0 then Parallel.for_
      ~chunk_size:(reduction_chunk_size ~grain relation)
      ~start:0 ~finish:(count - 1) (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let first = Attribute_relation.flat_first relation destination
        and last = Attribute_relation.flat_last relation destination in
        if first < last then begin
          let output = ref offsets.(destination) in
          values.(!output) <- sorted.(first);
          incr output;
          for slot = first + 1 to last - 1 do
            if slot land 16_383 = 0 then Cancel.check_opt cancel;
            if sorted.(slot) <> sorted.(slot - 1) then begin
              values.(!output) <- sorted.(slot);
              incr output
            end
          done
        end);
  Packed.Int_array.Private.create_validated_owned ~offsets ~values

let float_array_unique ?cancel ~grain relation source =
  let count = Attribute_relation.count relation
  and sorted = fill_incidence_plane ?cancel ~grain ~default:0. relation source in
  let counts = Array.make count 0 in
  if count > 0 then Parallel.for_
      ~chunk_size:(reduction_chunk_size ~grain relation)
      ~start:0 ~finish:(count - 1) (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let first = Attribute_relation.flat_first relation destination
        and last = Attribute_relation.flat_last relation destination in
        sort_float_range ?cancel sorted first last;
        if first < last then begin
          let unique = ref 1 in
          for slot = first + 1 to last - 1 do
            if slot land 16_383 = 0 then Cancel.check_opt cancel;
            if Float.compare sorted.(slot) sorted.(slot - 1) <> 0 then incr unique
          done;
          counts.(destination) <- !unique
        end);
  let offsets = Array.make (count + 1) 0 in
  for destination = 0 to count - 1 do
    offsets.(destination + 1) <- offsets.(destination) + counts.(destination)
  done;
  let values = Array.make offsets.(count) 0. in
  if count > 0 then Parallel.for_
      ~chunk_size:(reduction_chunk_size ~grain relation)
      ~start:0 ~finish:(count - 1) (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let first = Attribute_relation.flat_first relation destination
        and last = Attribute_relation.flat_last relation destination in
        if first < last then begin
          let output = ref offsets.(destination) in
          values.(!output) <- sorted.(first);
          incr output;
          for slot = first + 1 to last - 1 do
            if slot land 16_383 = 0 then Cancel.check_opt cancel;
            if Float.compare sorted.(slot) sorted.(slot - 1) <> 0 then begin
              values.(!output) <- sorted.(slot);
              incr output
            end
          done
        end);
  Packed.Float_array.Private.create_validated_owned ~offsets ~values

let remap_int_array ?cancel ~grain mapping values =
  let source = Packed.Int_array.Private.view values
  and count = Array.length mapping in
  let offsets = Array.make (count + 1) 0 in
  for destination = 0 to count - 1 do
    if destination land 4095 = 0 then Cancel.check_opt cancel;
    let source_row = mapping.(destination) in
    let length = source.offsets.(source_row + 1) - source.offsets.(source_row) in
    if length > Sys.max_array_length - offsets.(destination) then
      invalid_arg "Rdk.Attribute_ops.promote: remapped integer array exceeds array limits";
    offsets.(destination + 1) <- offsets.(destination) + length
  done;
  let output = Array.make offsets.(count) 0 in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
      (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let source_row = mapping.(destination) in
        let first = source.offsets.(source_row)
        and length = source.offsets.(source_row + 1) - source.offsets.(source_row) in
        Array.blit source.values first output offsets.(destination) length);
  Packed.Int_array.Private.create_validated_owned ~offsets ~values:output

let remap_float_array ?cancel ~grain mapping values =
  let source = Packed.Float_array.Private.view values
  and count = Array.length mapping in
  let offsets = Array.make (count + 1) 0 in
  for destination = 0 to count - 1 do
    if destination land 4095 = 0 then Cancel.check_opt cancel;
    let source_row = mapping.(destination) in
    let length = source.offsets.(source_row + 1) - source.offsets.(source_row) in
    if length > Sys.max_array_length - offsets.(destination) then
      invalid_arg "Rdk.Attribute_ops.promote: remapped float array exceeds array limits";
    offsets.(destination + 1) <- offsets.(destination) + length
  done;
  let output = Array.make offsets.(count) 0. in
  if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
      (fun destination ->
        if destination land 4095 = 0 then Cancel.check_opt cancel;
        let source_row = mapping.(destination) in
        let first = source.offsets.(source_row)
        and length = source.offsets.(source_row + 1) - source.offsets.(source_row) in
        Array.blit source.values first output offsets.(destination) length);
  Packed.Float_array.Private.create_validated_owned ~offsets ~values:output

type promotion_plan = {
  relation : relation;
  mapping : int array option;
  destination_count : int;
}

let make_promotion_plan ?cancel ?piece_attribute ~source ~destination geometry =
  let relation = make_relation ?cancel ~source ~destination geometry
  and destination_count = owner_count geometry destination in
  match piece_attribute with
  | None -> Ok { relation; mapping = None; destination_count }
  | Some piece_name ->
      (match Geometry.find_attribute ~owner:destination piece_name geometry with
       | None -> Error (Printf.sprintf
           "Rdk.Attribute_ops.promote: missing %s-owned piece attribute %s"
           (match destination with Attribute.Point -> "point"
             | Attribute.Vertex -> "vertex"
             | Attribute.Primitive -> "primitive"
             | Attribute.Detail -> "detail") piece_name)
       | Some piece ->
           (match Attribute.Private.storage piece with
            | Attribute.Int _ | Attribute.Text _ ->
                let relation, mapping = make_piece_relation ?cancel
                    ~source_count:(owner_count geometry source)
                    ~piece_attribute:piece relation in
                Ok { relation; mapping = Some mapping; destination_count }
            | Attribute.Float _ | Attribute.Float2 _ | Attribute.Float3 _
            | Attribute.Float4 _ | Attribute.Int_array _
            | Attribute.Float_array _ -> Error
                "Rdk.Attribute_ops.promote: piece attribute must use integer or text storage"))

let validate_index_request ~method_ ~destination ~into ~piece_attribute
    ~index_attribute attribute = match index_attribute with
  | None -> Ok ()
  | Some name when String.trim name = "" -> Error
      "Rdk.Attribute_ops.promote: index attribute name must not be empty"
  | Some name when destination = Attribute.Point && String.equal name "P" -> Error
      "Rdk.Attribute_ops.promote: canonical P cannot store a source index"
  | Some name when String.equal name into -> Error
      "Rdk.Attribute_ops.promote: value and index attributes must have different names"
  | Some name when (match piece_attribute with
      | Some piece -> String.equal name piece | None -> false) -> Error
      "Rdk.Attribute_ops.promote: index attribute must not replace the piece attribute"
  | Some _ when not (indexed_method method_) -> Error
      "Rdk.Attribute_ops.promote: source index requires first, last, minimum, maximum, or mode"
  | Some _ ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float _ | Attribute.Int _ | Attribute.Text _
       | Attribute.Float2 _ | Attribute.Float3 _ | Attribute.Float4 _ -> Ok ()
       | Attribute.Int_array _ | Attribute.Float_array _ -> Error
           "Rdk.Attribute_ops.promote: array source indices are not defined")

let promote_bound ?cancel ~grain ~method_ ~delete_source ~source ~destination
    ~name ~into ?index_attribute ~attribute plan geometry =
  let relation = plan.relation and mapping = plan.mapping in
  assert (match mapping with None -> Attribute_relation.count relation = plan.destination_count
    | Some values -> Array.length values = plan.destination_count);
  let unsupported message = Error (Printf.sprintf
      "Rdk.Attribute_ops.promote: %s storage does not support %s"
      message (match method_ with First -> "first" | Last -> "last"
        | Average -> "average" | Minimum -> "minimum" | Maximum -> "maximum"
        | Mode -> "mode" | Median -> "median" | Sum -> "sum"
        | Sum_squares -> "sum_squares"
        | Root_mean_square -> "root_mean_square"
        | Array_all -> "array_all" | Unique_values -> "unique_values")) in
  let map_float values = match mapping with None -> values
    | Some mapping -> map_plane ?cancel ~grain mapping values 0.
  and map_int values = match mapping with None -> values
    | Some mapping -> map_plane ?cancel ~grain mapping values 0
  and map_text values = match mapping with None -> values
    | Some mapping -> map_plane ?cancel ~grain mapping values ""
  and map_int_array values = match mapping with None -> values
    | Some mapping -> remap_int_array ?cancel ~grain mapping values
  and map_float_array values = match mapping with None -> values
    | Some mapping -> remap_float_array ?cancel ~grain mapping values in
  let storage_result = match Attribute.Private.storage attribute with
    | Attribute.Float values ->
        (match method_, index_attribute with
         | Array_all, None -> Ok (Attribute.Float_array
             (float_array_all ?cancel ~grain relation values |> map_float_array), None)
         | Unique_values, None -> Ok (Attribute.Float_array
             (float_array_unique ?cancel ~grain relation values
              |> map_float_array), None)
         | (First | Last | Average | Minimum | Maximum | Mode | Median | Sum
           | Sum_squares | Root_mean_square), None -> Ok (Attribute.Float
             (float_plane ?cancel ~grain method_ relation values |> map_float), None)
         | (Array_all | Unique_values), Some _ -> assert false
         | _, Some _ ->
             let values, indices = indexed_float_plane ?cancel ~grain method_
                 relation values in
             Ok (Attribute.Float (map_float values),
               Some (Attribute.Int (map_int indices))))
    | Attribute.Int values ->
        (match method_ with
         | Array_all -> Ok (Attribute.Int_array
             (int_array_all ?cancel ~grain relation values |> map_int_array), None)
         | Unique_values -> Ok (Attribute.Int_array
             (int_array_unique ?cancel ~grain relation values |> map_int_array), None)
         | First | Last | Minimum | Maximum | Mode | Median ->
             (match index_attribute with
              | None -> Ok (Attribute.Int
                  (int_plane ?cancel ~grain method_ relation values |> map_int), None)
              | Some _ ->
                  let values, indices = indexed_int_plane ?cancel ~grain method_
                      relation values in
                  Ok (Attribute.Int (map_int values),
                    Some (Attribute.Int (map_int indices))))
         | Average | Sum | Sum_squares | Root_mean_square -> unsupported "integer")
    | Attribute.Text values ->
        (match method_ with
         | First | Last | Average | Minimum | Maximum | Mode | Median | Sum
         | Sum_squares | Root_mean_square ->
             (match index_attribute with
              | None -> Ok (Attribute.Text
                  (text_plane ?cancel ~grain method_ relation values |> map_text), None)
              | Some _ ->
                  let values, indices = indexed_text_plane ?cancel ~grain method_
                      relation values in
                  Ok (Attribute.Text (map_text values),
                    Some (Attribute.Int (map_int indices))))
         | Array_all | Unique_values -> unsupported "text")
    | Attribute.Float2 values ->
        (match method_ with
         | Array_all | Unique_values -> unsupported "float2"
         | First | Last | Average | Minimum | Maximum | Mode | Median | Sum
         | Sum_squares | Root_mean_square ->
             let view = Packed.Float2.Private.view values in
             (match index_attribute with
              | None ->
                  let x = float_plane ?cancel ~grain method_ relation view.x
                      |> map_float
                  and y = float_plane ?cancel ~grain method_ relation view.y
                      |> map_float in
                  Ok (Attribute.Float2 (Packed.Float2.of_owned ~x ~y
                    |> Result.get_ok), None)
              | Some _ ->
                  let x, ix = indexed_float_plane ?cancel ~grain method_
                      relation view.x
                  and y, iy = indexed_float_plane ?cancel ~grain method_
                      relation view.y in
                  Ok (Attribute.Float2 (Packed.Float2.of_owned
                      ~x:(map_float x) ~y:(map_float y) |> Result.get_ok),
                    Some (Attribute.Int_array (fixed_int_tuple_array ?cancel
                      ~grain [|map_int ix; map_int iy|])))))
    | Attribute.Float3 values ->
        (match method_ with
         | Array_all | Unique_values -> unsupported "float3"
         | First | Last | Average | Minimum | Maximum | Mode | Median | Sum
         | Sum_squares | Root_mean_square ->
             let view = Packed.Float3.Private.view values in
             (match index_attribute with
              | None ->
                  let x = float_plane ?cancel ~grain method_ relation view.x
                      |> map_float
                  and y = float_plane ?cancel ~grain method_ relation view.y
                      |> map_float
                  and z = float_plane ?cancel ~grain method_ relation view.z
                      |> map_float in
                  Ok (Attribute.Float3
                    (Packed.Float3.Private.of_owned_exn ~x ~y ~z), None)
              | Some _ ->
                  let x, ix = indexed_float_plane ?cancel ~grain method_
                      relation view.x
                  and y, iy = indexed_float_plane ?cancel ~grain method_
                      relation view.y
                  and z, iz = indexed_float_plane ?cancel ~grain method_
                      relation view.z in
                  Ok (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
                      ~x:(map_float x) ~y:(map_float y) ~z:(map_float z)),
                    Some (Attribute.Int_array (fixed_int_tuple_array ?cancel
                      ~grain [|map_int ix; map_int iy; map_int iz|])))))
    | Attribute.Float4 values ->
        (match method_ with
         | Array_all | Unique_values -> unsupported "float4"
         | First | Last | Average | Minimum | Maximum | Mode | Median | Sum
         | Sum_squares | Root_mean_square ->
             let view = Packed.Float4.Private.view values in
             (match index_attribute with
              | None ->
                  let x = float_plane ?cancel ~grain method_ relation view.x
                      |> map_float
                  and y = float_plane ?cancel ~grain method_ relation view.y
                      |> map_float
                  and z = float_plane ?cancel ~grain method_ relation view.z
                      |> map_float
                  and w = float_plane ?cancel ~grain method_ relation view.w
                      |> map_float in
                  Ok (Attribute.Float4 (Packed.Float4.of_owned ~x ~y ~z ~w
                    |> Result.get_ok), None)
              | Some _ ->
                  let x, ix = indexed_float_plane ?cancel ~grain method_
                      relation view.x
                  and y, iy = indexed_float_plane ?cancel ~grain method_
                      relation view.y
                  and z, iz = indexed_float_plane ?cancel ~grain method_
                      relation view.z
                  and w, iw = indexed_float_plane ?cancel ~grain method_
                      relation view.w in
                  Ok (Attribute.Float4 (Packed.Float4.of_owned
                      ~x:(map_float x) ~y:(map_float y) ~z:(map_float z)
                      ~w:(map_float w) |> Result.get_ok),
                    Some (Attribute.Int_array (fixed_int_tuple_array ?cancel
                      ~grain [|map_int ix; map_int iy; map_int iz; map_int iw|])))))
    | Attribute.Int_array _ -> unsupported "integer array"
    | Attribute.Float_array _ -> unsupported "float array" in
  Result.bind storage_result (fun (storage, indices) ->
    Result.bind (Attribute.create_owned ~name:into ~owner:destination storage)
      (fun promoted ->
        let geometry = if delete_source then
            Geometry.without_attribute ~owner:source name geometry
          else geometry in
        Result.bind (Geometry.with_attribute promoted geometry) (fun geometry ->
          match index_attribute, indices with
          | None, None -> Ok geometry
          | Some name, Some storage ->
              Result.bind (Attribute.create_owned ~name ~owner:destination
                  storage) (fun indices ->
                Geometry.with_attribute indices geometry)
          | None, Some _ | Some _, None -> assert false)))

let promote_pattern ?cancel ?(grain = 16_384) ?(method_ = Average)
    ?(delete_source = true) ?piece_attribute ?into_pattern ?index_pattern
    ~source ~destination ~pattern geometry =
  Error.guard ~cancelled:"attribute promotion was cancelled" ~invalid_argument:"invalid_parameter" ~operation:"attribute_promote"
    ~code:"invalid_attribute" @@ fun () ->
  if grain <= 0 then invalid_arg
      "Rdk.Attribute_ops.promote_pattern: grain must be positive";
  if (match piece_attribute with Some value -> String.trim value = "" | None -> false)
  then Error "Rdk.Attribute_ops.promote_pattern: piece attribute name must not be empty"
  else if Option.is_some index_pattern && not (indexed_method method_) then Error
      "Rdk.Attribute_ops.promote_pattern: source index requires first, last, minimum, maximum, or mode"
  else Result.bind (Attribute_pattern.compile pattern) (fun compiled ->
    Result.bind (match into_pattern with
      | None -> Ok None
      | Some replacement -> Result.map Option.some
          (Attribute_pattern.compile_rewrite_set ~pattern ~replacement))
      (fun rewrite -> Result.bind (match index_pattern with
        | None -> Ok None
        | Some replacement -> Result.map Option.some
            (Attribute_pattern.compile_rewrite_set ~pattern ~replacement))
      (fun index_rewrite ->
    let rec select result = function
      | [] -> Result.map (fun values -> Array.of_list (List.rev values)) result
      | attribute :: rest when Attribute.owner attribute <> source ->
          select result rest
      | attribute :: rest ->
          let name = Attribute.name attribute in
          if not (Attribute_pattern.matches compiled name) then select result rest
          else Result.bind result (fun values ->
            let apply label rule fallback = match rule with
              | None -> Ok fallback
              | Some rule ->
                  (match Attribute_pattern.rewrite_set rule name with
                   | Some value -> Ok value
                   | None -> Error (Printf.sprintf
                       "Rdk.Attribute_ops.promote_pattern: %s rewrite did not match %s"
                       label name)) in
            Result.bind (apply "destination" rewrite name) (fun into ->
              Result.bind (match index_rewrite with
                | None -> Ok None
                | Some _ -> Result.map Option.some
                    (apply "index" index_rewrite name)) (fun index_attribute ->
                select (Ok ((attribute, into, index_attribute) :: values)) rest))) in
    Result.bind (select (Ok []) (Geometry.attributes geometry)) (fun selected ->
    let validation =
      let destinations = Hashtbl.create (Array.length selected * 2) in
      let reserve name =
        if Hashtbl.mem destinations name then Error (Printf.sprintf
            "Rdk.Attribute_ops.promote_pattern: multiple outputs use name %s" name)
        else begin Hashtbl.add destinations name (); Ok () end in
      Array.fold_left (fun result (attribute, into, index_attribute) ->
        Result.bind result (fun () ->
          if String.trim into = "" then Error
              "Rdk.Attribute_ops.promote_pattern: rewritten name must not be empty"
          else if destination = Attribute.Point && String.equal into "P" then Error
              "Rdk.Attribute_ops.promote_pattern: canonical P is not an ordinary attribute"
          else Result.bind (reserve into) (fun () ->
            Result.bind (validate_index_request ~method_ ~destination ~into
                ~piece_attribute ~index_attribute attribute) (fun () ->
              match index_attribute with None -> Ok () | Some name -> reserve name))))
        (Ok ()) selected in
    Result.bind validation (fun () ->
    let selected = if source = destination && Option.is_none piece_attribute
      then Array.of_list (List.filter (fun (attribute, into, _) ->
        not (String.equal (Attribute.name attribute) into))
        (Array.to_list selected))
      else selected in
    if Array.length selected = 0
    then Ok geometry
    else Result.bind (make_promotion_plan ?cancel ?piece_attribute
        ~source ~destination geometry) (fun plan ->
      let geometry = if delete_source then
          Array.fold_left (fun geometry (attribute, _, _) ->
            Geometry.without_attribute ~owner:source (Attribute.name attribute)
              geometry) geometry selected
        else geometry in
      let rec promote geometry index =
        if index = Array.length selected then Ok geometry
        else
          let attribute, into, index_attribute = selected.(index) in
          let name = Attribute.name attribute in
          Result.bind (promote_bound ?cancel ~grain ~method_
              ~delete_source:false ~source ~destination ~name ~into ~attribute
              ?index_attribute plan geometry)
            (fun geometry -> promote geometry (index + 1)) in
      promote geometry 0))))))
