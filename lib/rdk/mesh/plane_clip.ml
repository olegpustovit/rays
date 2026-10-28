
type keep = Above | Below | All

exception Clip_error of string

let finite value = Float.is_finite value
let get_ok = function Ok value -> value | Error message -> raise (Clip_error message)
let clamp01 value = if value < 0. then 0. else if value > 1. then 1. else value

type builder = {
  mutable tokens : int array;
  mutable corner_a : int array;
  mutable corner_b : int array;
  mutable corner_t : float array;
  mutable corner_count : int;
  mutable offsets : int array;
  mutable kinds : bytes;
  mutable primitive_source : int array;
  mutable primitive_side : int array;
  mutable primitive_clipped : bool array;
  mutable primitive_cap : bool array;
  mutable primitive_count : int;
}

let grown_capacity current required =
  let rec grow value =
    if value >= required then value
    else if value >= Sys.max_array_length / 2 then
      if required <= Sys.max_array_length then required
      else raise (Clip_error "Rdk.Plane_clip.clip: output exceeds OCaml array limits")
    else grow (max 8 (value * 2)) in
  grow current

let grow_int source capacity default =
  let output = Array.make capacity default in
  Array.blit source 0 output 0 (Array.length source);
  output

let grow_float source capacity =
  let output = Array.make capacity 0. in
  Array.blit source 0 output 0 (Array.length source);
  output

let grow_bool source capacity =
  let output = Array.make capacity false in
  Array.blit source 0 output 0 (Array.length source);
  output

let grow_bytes source capacity =
  let output = Bytes.make capacity '\000' in
  Bytes.blit source 0 output 0 (Bytes.length source);
  output

let trim_int source length =
  if Array.length source = length then source else Array.sub source 0 length

let trim_float source length =
  if Array.length source = length then source else Array.sub source 0 length

let trim_bool source length =
  if Array.length source = length then source else Array.sub source 0 length

let trim_bytes source length =
  if Bytes.length source = length then source else Bytes.sub source 0 length

let create_builder ~corner_capacity ~primitive_capacity =
  let corner_capacity = max 8 corner_capacity
  and primitive_capacity = max 8 primitive_capacity in
  { tokens = Array.make corner_capacity 0;
    corner_a = Array.make corner_capacity (-1);
    corner_b = Array.make corner_capacity (-1);
    corner_t = Array.make corner_capacity 0.;
    corner_count = 0;
    offsets = Array.make (primitive_capacity + 1) 0;
    kinds = Bytes.make primitive_capacity '\000';
    primitive_source = Array.make primitive_capacity (-1);
    primitive_side = Array.make primitive_capacity 0;
    primitive_clipped = Array.make primitive_capacity false;
    primitive_cap = Array.make primitive_capacity false;
    primitive_count = 0 }

let ensure_corners value additional =
  if additional > Sys.max_array_length - value.corner_count then
    raise (Clip_error "Rdk.Plane_clip.clip: corner count exceeds OCaml array limits");
  let required = value.corner_count + additional in
  if required > Array.length value.tokens then begin
    let capacity = grown_capacity (Array.length value.tokens) required in
    value.tokens <- grow_int value.tokens capacity 0;
    value.corner_a <- grow_int value.corner_a capacity (-1);
    value.corner_b <- grow_int value.corner_b capacity (-1);
    value.corner_t <- grow_float value.corner_t capacity
  end

let ensure_primitives value additional =
  if additional > Sys.max_array_length - value.primitive_count then
    raise (Clip_error "Rdk.Plane_clip.clip: primitive count exceeds OCaml array limits");
  let required = value.primitive_count + additional in
  if required > Bytes.length value.kinds then begin
    let capacity = grown_capacity (Bytes.length value.kinds) required in
    value.offsets <- grow_int value.offsets (capacity + 1) 0;
    value.kinds <- grow_bytes value.kinds capacity;
    value.primitive_source <- grow_int value.primitive_source capacity (-1);
    value.primitive_side <- grow_int value.primitive_side capacity 0;
    value.primitive_clipped <- grow_bool value.primitive_clipped capacity;
    value.primitive_cap <- grow_bool value.primitive_cap capacity
  end

let add_corner value ~token ~a ~b ~t =
  ensure_corners value 1;
  let index = value.corner_count in
  value.tokens.(index) <- token;
  value.corner_a.(index) <- a;
  value.corner_b.(index) <- b;
  value.corner_t.(index) <- t;
  value.corner_count <- index + 1

let add_corner_unique value start ~token ~a ~b ~t =
  if value.corner_count = start || value.tokens.(value.corner_count - 1) <> token
  then add_corner value ~token ~a ~b ~t

let drop_closing_duplicate value start =
  if value.corner_count - start > 1
     && value.tokens.(start) = value.tokens.(value.corner_count - 1)
  then value.corner_count <- value.corner_count - 1

let finish_primitive value ~start ~kind ~source ~side ~clipped ~cap ~minimum =
  let count = value.corner_count - start in
  if count < minimum then begin value.corner_count <- start; false end
  else begin
    ensure_primitives value 1;
    let primitive = value.primitive_count in
    value.offsets.(primitive) <- start;
    value.offsets.(primitive + 1) <- value.corner_count;
    Bytes.set value.kinds primitive kind;
    value.primitive_source.(primitive) <- source;
    value.primitive_side.(primitive) <- side;
    value.primitive_clipped.(primitive) <- clipped;
    value.primitive_cap.(primitive) <- cap;
    value.primitive_count <- primitive + 1;
    true
  end

type segments = {
  mutable segment_a : int array;
  mutable segment_b : int array;
  mutable segment_side : int array;
  mutable segment_count : int;
}

let create_segments capacity =
  let capacity = max 8 capacity in
  { segment_a = Array.make capacity 0; segment_b = Array.make capacity 0;
    segment_side = Array.make capacity 0; segment_count = 0 }

let add_segment value ~a ~b ~side =
  if a <> b then begin
    if value.segment_count = Array.length value.segment_a then begin
      let capacity = grown_capacity (Array.length value.segment_a)
          (value.segment_count + 1) in
      value.segment_a <- grow_int value.segment_a capacity 0;
      value.segment_b <- grow_int value.segment_b capacity 0;
      value.segment_side <- grow_int value.segment_side capacity 0
    end;
    let index = value.segment_count in
    value.segment_a.(index) <- a;
    value.segment_b.(index) <- b;
    value.segment_side.(index) <- side;
    value.segment_count <- index + 1
  end

let clip ?cancel ?(grain = 16_384) ?(keep = Above)
    ?(snapping_tolerance = 1e-9) ?(fill = false)
    ?(split_connectivity = false) ?(clip_attribute = "P") ?(distance = 0.)
    ?selection ?(replace_existing_groups = true) ?clipped_edge_group ?cap_group
    ?clipped_group ?above_group ?below_group ~origin ~normal geometry =
  Error.guard ~operation:"clip" ~code:"invalid_geometry" @@ fun () ->
  try
    if grain <= 0 then invalid_arg "Rdk.Plane_clip.clip: grain must be positive";
    Cancel.check_opt cancel;
    let ox = origin.Rays_math.Vec3.x and oy = origin.y and oz = origin.z
    and supplied_nx = normal.Rays_math.Vec3.x and supplied_ny = normal.y
    and supplied_nz = normal.z in
    if not (finite ox && finite oy && finite oz && finite supplied_nx
        && finite supplied_ny && finite supplied_nz) then
      raise (Clip_error "Rdk.Plane_clip.clip: plane origin and normal must be finite");
    if not (finite snapping_tolerance) || snapping_tolerance < 0. then
      raise (Clip_error
        "Rdk.Plane_clip.clip: snapping tolerance must be finite and non-negative");
    if String.trim clip_attribute = "" then
      raise (Clip_error "Rdk.Plane_clip.clip: clip attribute must not be empty");
    if not (finite distance) then
      raise (Clip_error "Rdk.Plane_clip.clip: plane distance must be finite");
    if split_connectivity && keep <> All then
      raise (Clip_error
        "Rdk.Plane_clip.clip: split connectivity requires keep=All");
    let output_names = [|cap_group; clipped_group; above_group; below_group|] in
    let seen_names = Hashtbl.create 4 in
    Array.iter (function
      | None -> ()
      | Some name when String.trim name = "" -> raise (Clip_error
          "Rdk.Plane_clip.clip: output group names must not be empty")
      | Some name when Hashtbl.mem seen_names name -> raise (Clip_error
          "Rdk.Plane_clip.clip: output group names must be distinct")
      | Some name -> Hashtbl.add seen_names name ()) output_names;
    (match clipped_edge_group with
     | Some name when String.trim name = "" -> raise (Clip_error
         "Rdk.Plane_clip.clip: clipped edge group name must not be empty")
     | None | Some _ -> ());
    let normal_scale = Float.max (abs_float supplied_nx)
        (Float.max (abs_float supplied_ny) (abs_float supplied_nz)) in
    if normal_scale = 0. then
      raise (Clip_error "Rdk.Plane_clip.clip: plane normal must be non-zero");
    let scaled_nx = supplied_nx /. normal_scale
    and scaled_ny = supplied_ny /. normal_scale
    and scaled_nz = supplied_nz /. normal_scale in
    let normal_length = sqrt ((scaled_nx *. scaled_nx)
        +. (scaled_ny *. scaled_ny) +. (scaled_nz *. scaled_nz)) in
    let nx = scaled_nx /. normal_length and ny = scaled_ny /. normal_length
    and nz = scaled_nz /. normal_length in
    let point_count = Geometry.point_count geometry in
    let positions = Packed.Float3.View.view (Geometry.positions geometry) in
    let clip_x, clip_y, clip_z =
      if String.equal clip_attribute "P" then
        positions.x, Some positions.y, Some positions.z
      else match Geometry.find_attribute ~owner:Attribute.Point clip_attribute geometry with
      | None -> raise (Clip_error (Printf.sprintf
          "Rdk.Plane_clip.clip: point clip attribute %S does not exist" clip_attribute))
      | Some attribute ->
          match Attribute.View.storage attribute with
          | Attribute.Float values ->
              values, None, None
          | Attribute.Int values ->
              Array.map float_of_int values, None, None
          | Attribute.Float2 values ->
              let values = Packed.Float2.View.view values in
              values.x, Some values.y, None
          | Attribute.Float3 values ->
              let values = Packed.Float3.View.view values in
              values.x, Some values.y, Some values.z
          | Attribute.Float4 values ->
              let values = Packed.Float4.View.view values in
              values.x, Some values.y, Some values.z
          | Attribute.Int_array _ | Attribute.Float_array _ | Attribute.Text _ ->
              raise (Clip_error (Printf.sprintf
                "Rdk.Plane_clip.clip: point clip attribute %S must be a scalar or fixed-width numeric tuple"
                clip_attribute)) in
    let raw_distance = Array.make point_count 0.
    and classified_distance = Array.make point_count 0. in
    if point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(point_count - 1) (fun point ->
          if point land 4095 = 0 then Cancel.check_opt cancel;
          let x = positions.x.(point) and y = positions.y.(point)
          and z = positions.z.(point) in
          if not (finite x && finite y && finite z) then
            raise (Clip_error "Rdk.Plane_clip.clip: point positions must be finite");
          let cx = clip_x.(point)
          and cy = match clip_y with None -> 0. | Some values -> values.(point)
          and cz = match clip_z with None -> 0. | Some values -> values.(point) in
          if not (finite cx && finite cy && finite cz) then
            raise (Clip_error (Printf.sprintf
              "Rdk.Plane_clip.clip: point clip attribute %S must contain finite values"
              clip_attribute));
          let value = ((cx -. ox) *. nx) +. ((cy -. oy) *. ny)
              +. ((cz -. oz) *. nz) -. distance in
          if not (finite value) then raise (Clip_error
              "Rdk.Plane_clip.clip: signed plane distance overflowed");
          raw_distance.(point) <- value;
          classified_distance.(point) <- if abs_float value <= snapping_tolerance then 0.
            else value);
    let topology = Geometry.topology geometry in
    let topology_view = Topology.View.view topology in
    let topology_index = Topology_index.create ?cancel topology in
    let index_view = Topology_index.View.view topology_index in
    let primitive_selection = match selection with
      | None -> None
      | Some selection -> Some (Element_selection.promote ?cancel ~grain
          ~name:"__clip_selection" ~destination:Group.Primitive selection
          topology |> get_ok) in
    let primitive_selected primitive = match primitive_selection with
      | None -> true | Some group -> Group.mem primitive group in
    let affected_points, shared_points = match primitive_selection with
      | None -> None, None
      | Some _ ->
          let affected = Bytes.make point_count '\000'
          and shared = Bytes.make point_count '\000' in
          if point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
              ~finish:(point_count - 1) (fun point ->
                if point land 4095 = 0 then Cancel.check_opt cancel;
                let has_selected = ref false and has_unselected = ref false
                and at = ref index_view.point_offsets.(point) in
                let last = index_view.point_offsets.(point + 1) in
                while !at < last && not (!has_selected && !has_unselected) do
                  let vertex = index_view.point_vertices.(!at) in
                  if primitive_selected index_view.primitive_of_vertex.(vertex)
                  then has_selected := true else has_unselected := true;
                  incr at
                done;
                if !has_selected then Bytes.set affected point '\001';
                if !has_selected && !has_unselected then
                  Bytes.set shared point '\001');
          (match selection with
           | Some (Element_selection.Selected_points group) ->
               for point = 0 to point_count - 1 do
                 if Group.mem point group then Bytes.set affected point '\001'
               done
           | None | Some (Element_selection.Selected_vertices _
               | Element_selection.Selected_primitives _
               | Element_selection.Selected_edges _) -> ());
          Some affected, Some shared in
    let point_affected point = match affected_points with
      | None -> true | Some values -> Bytes.get values point <> '\000' in
    let shared_token_of_point, shared_point_of_token = match shared_points with
      | None -> [||], [||]
      | Some shared ->
          let count = ref 0 in
          for point = 0 to point_count - 1 do
            if Bytes.get shared point <> '\000' then incr count
          done;
          if !count = 0 then [||], [||]
          else begin
            let mapping = Array.make point_count (-1)
            and points = Array.make !count 0 and at = ref 0 in
            for point = 0 to point_count - 1 do
              if Bytes.get shared point <> '\000' then begin
                mapping.(point) <- !at; points.(!at) <- point; incr at
              end
            done;
            mapping, points
          end in
    let edge_count = Topology_index.edge_count topology_index in
    if point_count > Sys.max_array_length - edge_count then
      raise (Clip_error "Rdk.Plane_clip.clip: token count exceeds OCaml array limits");
    let base_token_count = point_count + edge_count in
    let split = keep = All && split_connectivity in
    if split && base_token_count > Sys.max_array_length / 2 then
      raise (Clip_error "Rdk.Plane_clip.clip: split token count exceeds OCaml array limits");
    let selected_token_count = if split then base_token_count * 2
      else base_token_count in
    let passthrough_offset = selected_token_count in
    let shared_token_count = Array.length shared_point_of_token in
    let has_isolation_tokens = shared_token_count > 0 in
    if has_isolation_tokens
       && selected_token_count > Sys.max_array_length - shared_token_count
    then raise (Clip_error "Rdk.Plane_clip.clip: selection-isolation token count exceeds OCaml array limits");
    let geometry_token_count = selected_token_count
        + shared_token_count in
    let token side base = if split then (side * base_token_count) + base else base in
    let is_passthrough_token value = has_isolation_tokens
        && value >= passthrough_offset in
    let passthrough_point value =
      shared_point_of_token.(value - passthrough_offset) in
    let base_of_token value = value mod base_token_count in
    let point_on_plane point = classified_distance.(point) = 0. in
    let token_on_plane value =
      if is_passthrough_token value then false
      else let base = base_of_token value in
        base >= point_count || (point_affected base && point_on_plane base) in
    let edge_token side vertex =
      let edge = index_view.edge_of_vertex.(vertex) in
      if edge < 0 then raise (Clip_error "Rdk.Plane_clip.clip: missing topology edge")
      else token side (point_count + edge) in
    let source_token side point = token side point in
    let passthrough_token point =
      if Array.length shared_token_of_point = 0 then source_token 0 point
      else let shared = shared_token_of_point.(point) in
        if shared < 0 then source_token 0 point else passthrough_offset + shared in
    let position_of_selected_token value =
      let base = base_of_token value in
      if base < point_count then begin
        let projection = if point_affected base
            && classified_distance.(base) = 0. then raw_distance.(base) else 0. in
        positions.x.(base) -. (projection *. nx),
        positions.y.(base) -. (projection *. ny),
        positions.z.(base) -. (projection *. nz)
      end else begin
        let edge = base - point_count in
        let a = index_view.edge_a.(edge) and b = index_view.edge_b.(edge) in
        let denominator = raw_distance.(b) -. raw_distance.(a) in
        if denominator = 0. || not (finite denominator) then
          raise (Clip_error "Rdk.Plane_clip.clip: unstable shared-edge intersection");
        let t = clamp01 (-.raw_distance.(a) /. denominator) in
        positions.x.(a) +. ((positions.x.(b) -. positions.x.(a)) *. t),
        positions.y.(a) +. ((positions.y.(b) -. positions.y.(a)) *. t),
        positions.z.(a) +. ((positions.z.(b) -. positions.z.(a)) *. t)
      end in
    let side_inside side value = if side = 0 then value >= 0. else value <= 0. in
    let referenced = Array.make point_count false in
    Array.iter (fun point -> referenced.(point) <- true)
      topology_view.vertex_points;
    let standalone_used = Array.make geometry_token_count false in
    for point = 0 to point_count - 1 do
      if not referenced.(point) && not (point_affected point) then
        standalone_used.(source_token 0 point) <- true
      else if not referenced.(point) then
        match keep with
        | Above ->
            if classified_distance.(point) >= 0. then
              standalone_used.(source_token 0 point) <- true
        | Below ->
            if classified_distance.(point) <= 0. then
              standalone_used.(source_token 1 point) <- true
        | All ->
            let side = if classified_distance.(point) < 0. then 1 else 0 in
            standalone_used.(source_token side point) <- true
    done;
    (* Polygon-only, unfilled clipping has independent primitive plans. Count
       them in parallel, prefix once in stable source/side order, and then fill
       exact disjoint slices. Degenerate repeated-corner polygons retain the
       general builder path so its duplicate suppression semantics stay exact. *)
    let fast_plan =
      if fill || not (Bytes.for_all (fun kind -> kind = '\000')
          topology_view.primitive_kinds)
      then None
      else begin
        let primitive_count = Geometry.primitive_count geometry in
        let slots = if keep = All then 2 else 1 in
        if primitive_count > Sys.max_array_length / slots then
          raise (Clip_error "Rdk.Plane_clip.clip: primitive plan exceeds OCaml array limits");
        let candidate_count = primitive_count * slots in
        let candidate_corners = Array.make candidate_count 0 in
        if candidate_count > 0 then Parallel.for_
            ~chunk_size:(max 1 (grain / 8)) ~start:0
            ~finish:(candidate_count - 1) (fun candidate ->
              if candidate land 4095 = 0 then Cancel.check_opt cancel;
              let primitive = candidate / slots in
              let side = match keep with
                | Above -> 0 | Below -> 1 | All -> candidate mod 2 in
              let first = topology_view.primitive_offsets.(primitive)
              and last = topology_view.primitive_offsets.(primitive + 1) in
              let selected = primitive_selected primitive in
              if not selected then begin
                if slots = 1 || candidate mod slots = 0 then
                  candidate_corners.(candidate) <- last - first
              end else begin
                let arity = last - first in
                let repeated = ref (arity > 64) in
                if not !repeated then
                  for left = first to last - 2 do
                    let point = topology_view.vertex_points.(left) in
                    for right = left + 1 to last - 1 do
                      if topology_view.vertex_points.(right) = point then
                        repeated := true
                    done
                  done;
                let enabled = if keep <> All || side = 0 then true
                  else begin
                    let found = ref false in
                    for vertex = first to last - 1 do
                      if classified_distance.(topology_view.vertex_points.(vertex)) < 0.
                      then found := true
                    done;
                    !found
                  end in
                if !repeated then candidate_corners.(candidate) <- -1
                else if enabled then begin
                  if arity <= 3 then begin
                    let count = ref 0 and previous = ref (last - 1) in
                    for current = first to last - 1 do
                      let previous_point = topology_view.vertex_points.(!previous)
                      and current_point = topology_view.vertex_points.(current) in
                      let previous_inside = side_inside side
                          classified_distance.(previous_point)
                      and current_inside = side_inside side
                          classified_distance.(current_point) in
                      if current_inside then
                        count := !count + if previous_inside then 1 else 2
                      else if previous_inside then incr count;
                      previous := current
                    done;
                    candidate_corners.(candidate) <-
                      if !count >= 3 then !count else 0
                  end else begin
                    let count = ref 0 and inside_runs = ref 0
                    and previous = ref (last - 1) in
                    for current = first to last - 1 do
                      let previous_point = topology_view.vertex_points.(!previous)
                      and current_point = topology_view.vertex_points.(current) in
                      let previous_inside = side_inside side
                          classified_distance.(previous_point)
                      and current_inside = side_inside side
                          classified_distance.(current_point) in
                      if current_inside && not previous_inside then incr inside_runs;
                      if current_inside then
                        count := !count + if previous_inside then 1 else 2
                      else if previous_inside then incr count;
                      previous := current
                    done;
                    candidate_corners.(candidate) <-
                      if !inside_runs > 1 then -1
                      else if !count >= 3 then !count else 0
                  end
                end
              end);
        if Array.exists (fun count -> count < 0) candidate_corners then None
        else begin
          let output_count = ref 0 and total_corners = ref 0 in
          Array.iter (fun count -> if count > 0 then begin
              if count > Sys.max_array_length - !total_corners then
                raise (Clip_error "Rdk.Plane_clip.clip: corner plan exceeds OCaml array limits");
              incr output_count; total_corners := !total_corners + count
            end) candidate_corners;
          let offsets = Array.make (!output_count + 1) 0 in
          let output = ref 0 and corner = ref 0 in
          for candidate = 0 to candidate_count - 1 do
            let count = candidate_corners.(candidate) in
            if count > 0 then begin
              offsets.(!output) <- !corner;
              candidate_corners.(candidate) <- !output + 1;
              corner := !corner + count;
              incr output
            end
          done;
          offsets.(!output_count) <- !total_corners;
          Some (slots, candidate_corners, offsets)
        end
      end in
    let corner_capacity, primitive_capacity = match fast_plan with
      | Some (_, _, offsets) -> offsets.(Array.length offsets - 1),
          Array.length offsets - 1
      | None -> Geometry.vertex_count geometry, Geometry.primitive_count geometry in
    let builder = create_builder ~corner_capacity:(max 8 corner_capacity)
        ~primitive_capacity:(max 8 primitive_capacity) in
    (match fast_plan with
     | None -> ()
     | Some (_, _, offsets) ->
         Array.blit offsets 0 builder.offsets 0 (Array.length offsets);
         builder.corner_count <- offsets.(Array.length offsets - 1);
         builder.primitive_count <- Array.length offsets - 1);
    let segments = create_segments (max 8 (Geometry.primitive_count geometry)) in
    let process_passthrough primitive =
      let first = topology_view.primitive_offsets.(primitive)
      and last = topology_view.primitive_offsets.(primitive + 1) in
      let start = builder.corner_count in
      for vertex = first to last - 1 do
        let point = topology_view.vertex_points.(vertex) in
        add_corner builder ~token:(passthrough_token point)
          ~a:vertex ~b:vertex ~t:0.
      done;
      let kind = Bytes.get topology_view.primitive_kinds primitive in
      let minimum = if kind = '\001' then 2 else 3 in
      ignore (finish_primitive builder ~start ~kind ~source:primitive ~side:(-1)
        ~clipped:false ~cap:false ~minimum) in
    let add_source_corner side start vertex =
      let point = topology_view.vertex_points.(vertex) in
      add_corner_unique builder start ~token:(source_token side point)
        ~a:vertex ~b:vertex ~t:0. in
    let add_intersection_corner side start previous_vertex current_vertex =
      let previous_point = topology_view.vertex_points.(previous_vertex)
      and current_point = topology_view.vertex_points.(current_vertex) in
      let previous_distance = classified_distance.(previous_point)
      and current_distance = classified_distance.(current_point) in
      if previous_distance = 0. then
        add_corner_unique builder start ~token:(source_token side previous_point)
          ~a:previous_vertex ~b:previous_vertex ~t:0.
      else if current_distance = 0. then
        add_corner_unique builder start ~token:(source_token side current_point)
          ~a:current_vertex ~b:current_vertex ~t:0.
      else begin
        let denominator = raw_distance.(current_point)
            -. raw_distance.(previous_point) in
        if denominator = 0. || not (finite denominator) then
          raise (Clip_error "Rdk.Plane_clip.clip: unstable plane intersection");
        let t = clamp01 (-.raw_distance.(previous_point) /. denominator) in
        add_corner_unique builder start ~token:(edge_token side previous_vertex)
          ~a:previous_vertex ~b:current_vertex ~t
      end in
    let collect_plane_segment start side crossed =
      if crossed && fill then begin
        let finish = builder.corner_count in
        for corner = start to finish - 1 do
          let next = if corner + 1 = finish then start else corner + 1 in
          let a = builder.tokens.(corner) and b = builder.tokens.(next) in
          if a <> b && token_on_plane a && token_on_plane b then
            add_segment segments ~a ~b ~side
        done
      end in
    let process_polygon primitive side =
      let first = topology_view.primitive_offsets.(primitive)
      and last = topology_view.primitive_offsets.(primitive + 1) in
      let has_positive = ref false and has_negative = ref false
      and inside_runs = ref 0 and previous = ref (last - 1) in
      for vertex = first to last - 1 do
        let value = classified_distance.(topology_view.vertex_points.(vertex)) in
        if value > 0. then has_positive := true;
        if value < 0. then has_negative := true;
        let previous_point = topology_view.vertex_points.(!previous) in
        if side_inside side value
           && not (side_inside side classified_distance.(previous_point))
        then incr inside_runs;
        previous := vertex
      done;
      let crossed = !has_positive && !has_negative in
      let finish_fragment start =
        drop_closing_duplicate builder start;
        if builder.corner_count - start >= 3 then begin
          collect_plane_segment start side crossed;
          ignore (finish_primitive builder ~start ~kind:'\000' ~source:primitive
            ~side ~clipped:crossed ~cap:false ~minimum:3)
        end else builder.corner_count <- start in
      if !inside_runs <= 1 then begin
        let start = builder.corner_count and previous = ref (last - 1) in
        for current = first to last - 1 do
          let previous_point = topology_view.vertex_points.(!previous)
          and current_point = topology_view.vertex_points.(current) in
          let previous_inside = side_inside side classified_distance.(previous_point)
          and current_inside = side_inside side classified_distance.(current_point) in
          if current_inside then begin
            if not previous_inside then
              add_intersection_corner side start !previous current;
            add_source_corner side start current
          end else if previous_inside then
            add_intersection_corner side start !previous current;
          previous := current
        done;
        finish_fragment start
      end else begin
        (* A half-plane can disconnect a concave simple polygon, but multiple
           accepted boundary runs do not necessarily mean multiple output
           components. Pair the ordered line intersections through the
           polygon interior, then stitch accepted source-boundary chains into
           one or more cycles. *)
        let count = last - first and outside_local = ref 0 in
        while !outside_local < count
            && side_inside side classified_distance.(topology_view.vertex_points.(
              first + !outside_local)) do
          incr outside_local
        done;
        if !outside_local = count then
          raise (Clip_error "Rdk.Plane_clip.clip: disconnected-fragment plan disagreement");
        let scratch_start = builder.corner_count
        and chain_starts = Array.make count 0
        and chain_finishes = Array.make count 0
        and chain_count = ref 0
        and path_start = ref builder.corner_count in
        let finish_chain () =
          drop_closing_duplicate builder !path_start;
          if builder.corner_count - !path_start >= 3 then begin
            chain_starts.(!chain_count) <- !path_start - scratch_start;
            chain_finishes.(!chain_count) <- builder.corner_count - scratch_start;
            incr chain_count
          end else builder.corner_count <- !path_start;
          path_start := builder.corner_count in
        for edge = 0 to count - 1 do
          let a = first + ((!outside_local + edge) mod count)
          and b = first + ((!outside_local + edge + 1) mod count) in
          let pa = topology_view.vertex_points.(a)
          and pb = topology_view.vertex_points.(b) in
          let a_inside = side_inside side classified_distance.(pa)
          and b_inside = side_inside side classified_distance.(pb) in
          if a_inside && b_inside then begin
            if builder.corner_count = !path_start then
              add_source_corner side !path_start a;
            add_source_corner side !path_start b
          end else if a_inside then begin
            if builder.corner_count = !path_start then
              add_source_corner side !path_start a;
            add_intersection_corner side !path_start a b;
            finish_chain ()
          end else if b_inside then begin
            if builder.corner_count > !path_start then finish_chain ();
            add_intersection_corner side !path_start a b;
            add_source_corner side !path_start b
          end else if builder.corner_count > !path_start then finish_chain ()
        done;
        if builder.corner_count > !path_start then finish_chain ();
        if !chain_count > 0 then begin
          let scratch_count = builder.corner_count - scratch_start in
          let scratch_tokens = Array.sub builder.tokens scratch_start scratch_count
          and scratch_a = Array.sub builder.corner_a scratch_start scratch_count
          and scratch_b = Array.sub builder.corner_b scratch_start scratch_count
          and scratch_t = Array.sub builder.corner_t scratch_start scratch_count in
          builder.corner_count <- scratch_start;
          let endpoint_count = !chain_count * 2 in
          let endpoint_x = Array.make endpoint_count 0.
          and endpoint_y = Array.make endpoint_count 0.
          and endpoint_z = Array.make endpoint_count 0.
          and endpoint_chain = Array.make endpoint_count 0
          and endpoint_start = Array.make endpoint_count false
          and endpoint_token = Array.make endpoint_count 0 in
          for chain = 0 to !chain_count - 1 do
            let start_index = chain_starts.(chain)
            and end_index = chain_finishes.(chain) - 1 in
            let write endpoint index is_start =
              let token = scratch_tokens.(index) in
              let x, y, z = position_of_selected_token token in
              endpoint_x.(endpoint) <- x;
              endpoint_y.(endpoint) <- y;
              endpoint_z.(endpoint) <- z;
              endpoint_chain.(endpoint) <- chain;
              endpoint_start.(endpoint) <- is_start;
              endpoint_token.(endpoint) <- token in
            write (chain * 2) start_index true;
            write ((chain * 2) + 1) end_index false
          done;
          let span values =
            let minimum = ref values.(0) and maximum = ref values.(0) in
            for endpoint = 1 to endpoint_count - 1 do
              minimum := Float.min !minimum values.(endpoint);
              maximum := Float.max !maximum values.(endpoint)
            done;
            !maximum -. !minimum in
          let x_span = span endpoint_x and y_span = span endpoint_y
          and z_span = span endpoint_z in
          let coordinate = if x_span >= y_span && x_span >= z_span then endpoint_x
            else if y_span >= z_span then endpoint_y else endpoint_z in
          if Float.max x_span (Float.max y_span z_span) = 0. then
            raise (Clip_error
              "Rdk.Plane_clip.clip: coincident concave clipping intersections");
          let order = Array.init endpoint_count Fun.id in
          Array.sort (fun left right ->
            let compared = Float.compare coordinate.(left) coordinate.(right) in
            if compared <> 0 then compared
            else let compared = Int.compare endpoint_token.(left)
                endpoint_token.(right) in
              if compared <> 0 then compared
              else Bool.compare endpoint_start.(left) endpoint_start.(right)) order;
          let successor = Array.make !chain_count (-1) in
          for pair = 0 to !chain_count - 1 do
            let left = order.(pair * 2) and right = order.((pair * 2) + 1) in
            if endpoint_start.(left) = endpoint_start.(right) then
              raise (Clip_error
                "Rdk.Plane_clip.clip: ambiguous concave clipping intersection order");
            let end_chain, start_chain = if endpoint_start.(left)
              then endpoint_chain.(right), endpoint_chain.(left)
              else endpoint_chain.(left), endpoint_chain.(right) in
            if successor.(end_chain) >= 0 then
              raise (Clip_error
                "Rdk.Plane_clip.clip: duplicate concave clipping successor");
            successor.(end_chain) <- start_chain
          done;
          let visited = Array.make !chain_count false in
          for first_chain = 0 to !chain_count - 1 do
            if not visited.(first_chain) then begin
              let component_start = builder.corner_count
              and current = ref first_chain in
              while not visited.(!current) do
                visited.(!current) <- true;
                for corner = chain_starts.(!current)
                    to chain_finishes.(!current) - 1 do
                  add_corner_unique builder component_start
                    ~token:scratch_tokens.(corner) ~a:scratch_a.(corner)
                    ~b:scratch_b.(corner) ~t:scratch_t.(corner)
                done;
                current := successor.(!current);
                if !current < 0 then raise (Clip_error
                    "Rdk.Plane_clip.clip: incomplete concave clipping cycle")
              done;
              if !current <> first_chain then raise (Clip_error
                  "Rdk.Plane_clip.clip: intersecting concave clipping cycles");
              finish_fragment component_start
            end
          done
        end
      end
    in
    let process_open_curve primitive side first last =
      let path_start = ref builder.corner_count in
      let finish_path () =
        ignore (finish_primitive builder ~start:!path_start ~kind:'\001'
          ~source:primitive ~side ~clipped:true ~cap:false ~minimum:2);
        path_start := builder.corner_count in
      for vertex = first to last - 2 do
        let next = vertex + 1 in
        let point = topology_view.vertex_points.(vertex)
        and next_point = topology_view.vertex_points.(next) in
        let inside = side_inside side classified_distance.(point)
        and next_inside = side_inside side classified_distance.(next_point) in
        if inside && next_inside then begin
          if builder.corner_count = !path_start then add_source_corner side !path_start vertex;
          add_source_corner side !path_start next
        end else if inside then begin
          if builder.corner_count = !path_start then add_source_corner side !path_start vertex;
          add_intersection_corner side !path_start vertex next;
          finish_path ()
        end else if next_inside then begin
          if builder.corner_count > !path_start then finish_path ();
          add_intersection_corner side !path_start vertex next;
          add_source_corner side !path_start next
        end else if builder.corner_count > !path_start then finish_path ()
      done;
      if builder.corner_count > !path_start then finish_path ()
    in
    let process_curve primitive side =
      let first = topology_view.primitive_offsets.(primitive)
      and last = topology_view.primitive_offsets.(primitive + 1) in
      let kind = Bytes.get topology_view.primitive_kinds primitive in
      let has_outside = ref false in
      for vertex = first to last - 1 do
        let point = topology_view.vertex_points.(vertex) in
        if not (side_inside side classified_distance.(point)) then has_outside := true
      done;
      if kind = '\001' && not !has_outside then begin
        let start = builder.corner_count in
        for vertex = first to last - 1 do add_source_corner side start vertex done;
        ignore (finish_primitive builder ~start ~kind:'\001' ~source:primitive
          ~side ~clipped:false ~cap:false ~minimum:2)
      end else if kind = '\001' then process_open_curve primitive side first last
      else if not !has_outside then begin
        let start = builder.corner_count in
        for vertex = first to last - 1 do add_source_corner side start vertex done;
        ignore (finish_primitive builder ~start ~kind:'\002' ~source:primitive
          ~side ~clipped:false ~cap:false ~minimum:3)
      end else begin
        (* Rotate a clipped closed curve to an outside vertex, then process its
           cyclic edges as one open scan so wraparound fragments join. *)
        let count = last - first and outside_local = ref 0 in
        while !outside_local < count
            && side_inside side classified_distance.(topology_view.vertex_points.(
              first + !outside_local)) do incr outside_local done;
        let path_start = ref builder.corner_count in
        let finish_path () =
          ignore (finish_primitive builder ~start:!path_start ~kind:'\001'
            ~source:primitive ~side ~clipped:true ~cap:false ~minimum:2);
          path_start := builder.corner_count in
        for edge = 0 to count - 1 do
          let a = first + ((!outside_local + edge) mod count)
          and b = first + ((!outside_local + edge + 1) mod count) in
          let pa = topology_view.vertex_points.(a)
          and pb = topology_view.vertex_points.(b) in
          let a_inside = side_inside side classified_distance.(pa)
          and b_inside = side_inside side classified_distance.(pb) in
          if a_inside && b_inside then begin
            if builder.corner_count = !path_start then add_source_corner side !path_start a;
            add_source_corner side !path_start b
          end else if a_inside then begin
            if builder.corner_count = !path_start then add_source_corner side !path_start a;
            add_intersection_corner side !path_start a b;
            finish_path ()
          end else if b_inside then begin
            if builder.corner_count > !path_start then finish_path ();
            add_intersection_corner side !path_start a b;
            add_source_corner side !path_start b
          end else if builder.corner_count > !path_start then finish_path ()
        done;
        if builder.corner_count > !path_start then finish_path ()
      end
    in
    (match fast_plan with
     | Some (slots, candidate_output, _) ->
         let candidate_count = Array.length candidate_output in
         if candidate_count > 0 then Parallel.for_
             ~chunk_size:(max 1 (grain / 8)) ~start:0
             ~finish:(candidate_count - 1) (fun candidate ->
               let encoded = candidate_output.(candidate) in
               if encoded > 0 then begin
                 if candidate land 4095 = 0 then Cancel.check_opt cancel;
                 let output = encoded - 1 and primitive = candidate / slots in
                 let first = topology_view.primitive_offsets.(primitive)
                 and last = topology_view.primitive_offsets.(primitive + 1) in
                 if not (primitive_selected primitive) then begin
                   let at = ref builder.offsets.(output) in
                   for vertex = first to last - 1 do
                     let point = topology_view.vertex_points.(vertex)
                     and corner = !at in
                     builder.tokens.(corner) <- passthrough_token point;
                     builder.corner_a.(corner) <- vertex;
                     builder.corner_b.(corner) <- vertex;
                     builder.corner_t.(corner) <- 0.;
                     incr at
                   done;
                   if !at <> builder.offsets.(output + 1) then
                     raise (Clip_error
                       "Rdk.Plane_clip.clip: passthrough plan/fill disagreement");
                   Bytes.set builder.kinds output '\000';
                   builder.primitive_source.(output) <- primitive;
                   builder.primitive_side.(output) <- -1;
                   builder.primitive_clipped.(output) <- false;
                   builder.primitive_cap.(output) <- false
                 end else begin
                 let side = match keep with
                   | Above -> 0 | Below -> 1 | All -> candidate mod 2 in
                 let at = ref builder.offsets.(output)
                 and has_positive = ref false and has_negative = ref false in
                 let write_source vertex =
                   let point = topology_view.vertex_points.(vertex)
                   and corner = !at in
                   builder.tokens.(corner) <- source_token side point;
                   builder.corner_a.(corner) <- vertex;
                   builder.corner_b.(corner) <- vertex;
                   builder.corner_t.(corner) <- 0.;
                   incr at in
                 let write_intersection previous_vertex current_vertex =
                   let previous_point = topology_view.vertex_points.(previous_vertex)
                   and current_point = topology_view.vertex_points.(current_vertex)
                   and corner = !at in
                   let previous_distance = classified_distance.(previous_point)
                   and current_distance = classified_distance.(current_point) in
                   if previous_distance = 0. then begin
                     builder.tokens.(corner) <- source_token side previous_point;
                     builder.corner_a.(corner) <- previous_vertex;
                     builder.corner_b.(corner) <- previous_vertex
                   end else if current_distance = 0. then begin
                     builder.tokens.(corner) <- source_token side current_point;
                     builder.corner_a.(corner) <- current_vertex;
                     builder.corner_b.(corner) <- current_vertex
                   end else begin
                     let denominator = raw_distance.(current_point)
                         -. raw_distance.(previous_point) in
                     if denominator = 0. || not (finite denominator) then
                       raise (Clip_error
                         "Rdk.Plane_clip.clip: unstable plane intersection");
                     builder.tokens.(corner) <- edge_token side previous_vertex;
                     builder.corner_a.(corner) <- previous_vertex;
                     builder.corner_b.(corner) <- current_vertex;
                     builder.corner_t.(corner) <- clamp01
                         (-.raw_distance.(previous_point) /. denominator)
                   end;
                   incr at in
                 let previous = ref (last - 1) in
                 for current = first to last - 1 do
                   let previous_point = topology_view.vertex_points.(!previous)
                   and current_point = topology_view.vertex_points.(current) in
                   let value = classified_distance.(current_point) in
                   if value > 0. then has_positive := true;
                   if value < 0. then has_negative := true;
                   let previous_inside = side_inside side
                       classified_distance.(previous_point)
                   and current_inside = side_inside side value in
                   if current_inside then begin
                     if not previous_inside then
                       write_intersection !previous current;
                     write_source current
                   end else if previous_inside then
                     write_intersection !previous current;
                   previous := current
                 done;
                 if !at <> builder.offsets.(output + 1) then
                   raise (Clip_error "Rdk.Plane_clip.clip: polygon plan/fill disagreement");
                 let crossed = !has_positive && !has_negative in
                 Bytes.set builder.kinds output '\000';
                 builder.primitive_source.(output) <- primitive;
                 builder.primitive_side.(output) <- side;
                 builder.primitive_clipped.(output) <- crossed;
                 builder.primitive_cap.(output) <- false
                 end
               end)
     | None ->
         for primitive = 0 to Geometry.primitive_count geometry - 1 do
           if primitive land 1023 = 0 then Cancel.check_opt cancel;
           if not (primitive_selected primitive) then process_passthrough primitive
           else begin
             let kind = Bytes.get topology_view.primitive_kinds primitive in
             let process side = if kind = '\000' then process_polygon primitive side
               else process_curve primitive side in
             match keep with
             | Above -> process 0
             | Below -> process 1
             | All ->
                 process 0;
                 let first = topology_view.primitive_offsets.(primitive)
                 and last = topology_view.primitive_offsets.(primitive + 1) in
                 let has_negative = ref false in
                 for vertex = first to last - 1 do
                   if classified_distance.(topology_view.vertex_points.(vertex)) < 0.
                   then has_negative := true
                 done;
                 if !has_negative then process 1
           end
         done);
    let position_of_geometry_token value =
      if is_passthrough_token value then begin
        let point = passthrough_point value in
        positions.x.(point), positions.y.(point), positions.z.(point)
      end else position_of_selected_token value in
    let trace_caps side =
      let side_segment_count = ref 0 in
      for segment = 0 to segments.segment_count - 1 do
        if segment land 4095 = 0 then Cancel.check_opt cancel;
        if segments.segment_side.(segment) = side then incr side_segment_count
      done;
      let outgoing = Array.make geometry_token_count (-1)
      and incoming = Array.make geometry_token_count (-1) in
      for segment = 0 to segments.segment_count - 1 do
        if segment land 4095 = 0 then Cancel.check_opt cancel;
        if segments.segment_side.(segment) = side then begin
          let a = segments.segment_a.(segment) and b = segments.segment_b.(segment) in
          if outgoing.(a) >= 0 || incoming.(b) >= 0 then
            raise (Clip_error
              "Rdk.Plane_clip.clip: cap boundary is non-manifold or inconsistently wound");
          outgoing.(a) <- b;
          incoming.(b) <- a
        end
      done;
      let visited = Array.make geometry_token_count false and loops = ref []
      and scratch = Array.make (max 1 !side_segment_count) 0 in
      for start = 0 to geometry_token_count - 1 do
        if start land 4095 = 0 then Cancel.check_opt cancel;
        if (outgoing.(start) >= 0) <> (incoming.(start) >= 0) then
          raise (Clip_error "Rdk.Plane_clip.clip: cap boundary is open or inconsistently wound")
        else if outgoing.(start) >= 0 && not visited.(start) then begin
          let count = ref 0 and current = ref start in
          while !current <> start || !count = 0 do
            if !current < 0 || (!count > 0 && visited.(!current)) then
              raise (Clip_error "Rdk.Plane_clip.clip: cap boundary does not form simple loops");
            if !count >= Array.length scratch then
              raise (Clip_error "Rdk.Plane_clip.clip: cap boundary traversal overflow");
            scratch.(!count) <- !current;
            incr count;
            visited.(!current) <- true;
            current := outgoing.(!current)
          done;
          if !count < 3 then raise (Clip_error
              "Rdk.Plane_clip.clip: cap boundary contains fewer than three points");
          loops := Array.sub scratch 0 !count :: !loops
        end
      done;
      let loops = Array.of_list (List.rev !loops) in
      let dominant =
        let ax = abs_float nx and ay = abs_float ny and az = abs_float nz in
        if ax >= ay && ax >= az then 0 else if ay >= az then 1 else 2 in
      let cap_anchor_x, cap_anchor_y, cap_anchor_z =
        position_of_geometry_token loops.(0).(0) in
      let cap_coordinate_scale = ref 0. in
      Array.iter (Array.iter (fun token ->
          let x, y, z = position_of_geometry_token token in
          cap_coordinate_scale := Float.max !cap_coordinate_scale
              (abs_float (x -. cap_anchor_x));
          cap_coordinate_scale := Float.max !cap_coordinate_scale
              (abs_float (y -. cap_anchor_y));
          cap_coordinate_scale := Float.max !cap_coordinate_scale
              (abs_float (z -. cap_anchor_z)))) loops;
      if !cap_coordinate_scale = 0. then raise (Clip_error
          "Rdk.Plane_clip.clip: cap contours have zero coordinate extent");
      let project (x, y, z) = if dominant = 0 then
          (y -. cap_anchor_y) /. !cap_coordinate_scale,
          (z -. cap_anchor_z) /. !cap_coordinate_scale
        else if dominant = 1 then
          (x -. cap_anchor_x) /. !cap_coordinate_scale,
          (z -. cap_anchor_z) /. !cap_coordinate_scale
        else (x -. cap_anchor_x) /. !cap_coordinate_scale,
          (y -. cap_anchor_y) /. !cap_coordinate_scale in
      let boundary_token_count = Array.fold_left (fun count loop ->
          count + Array.length loop) 0 loops in
      let projected_u = Array.make boundary_token_count 0.
      and projected_v = Array.make boundary_token_count 0.
      and projected_at = ref 0 in
      Array.iter (Array.iter (fun token ->
          let u, v = project (position_of_geometry_token token) in
          incoming.(token) <- !projected_at;
          projected_u.(!projected_at) <- u;
          projected_v.(!projected_at) <- v;
          incr projected_at)) loops;
      let[@inline] projected_x token = projected_u.(incoming.(token))
      and[@inline] projected_y token = projected_v.(incoming.(token)) in
      let loop_bounds loop =
        let x0 = projected_x loop.(0) and y0 = projected_y loop.(0) in
        let min_x = ref x0 and max_x = ref x0 and min_y = ref y0 and max_y = ref y0 in
        for index = 1 to Array.length loop - 1 do
          let x = projected_x loop.(index) and y = projected_y loop.(index) in
          min_x := Float.min !min_x x; max_x := Float.max !max_x x;
          min_y := Float.min !min_y y; max_y := Float.max !max_y y
        done;
        !min_x, !min_y, !max_x, !max_y in
      let bounds = Array.map loop_bounds loops in
      let point_inside loop x y =
        let inside = ref false and previous = ref (Array.length loop - 1) in
        for current = 0 to Array.length loop - 1 do
          let ax = projected_x loop.(!previous)
          and ay = projected_y loop.(!previous)
          and bx = projected_x loop.(current)
          and by = projected_y loop.(current) in
          if ((ay > y) <> (by > y))
             && x < ((bx -. ax) *. (y -. ay) /. (by -. ay)) +. ax then
            inside := not !inside;
          previous := current
        done;
        !inside in
      let loop_count = Array.length loops in
      let projected_cross a b c =
        let ax = projected_x a and ay = projected_y a
        and bx = projected_x b and by = projected_y b
        and cx = projected_x c and cy = projected_y c in
        ((bx -. ax) *. (cy -. ay)) -. ((by -. ay) *. (cx -. ax)) in
      let projected_on_segment point a b =
        let px = projected_x point and py = projected_y point
        and ax = projected_x a and ay = projected_y a
        and bx = projected_x b and by = projected_y b in
        abs_float (projected_cross a b point) <= 1e-12
        && px >= Float.min ax bx -. 1e-12 && px <= Float.max ax bx +. 1e-12
        && py >= Float.min ay by -. 1e-12 && py <= Float.max ay by +. 1e-12 in
      let projected_segments_intersect a b c d =
        let ab_c = projected_cross a b c and ab_d = projected_cross a b d
        and cd_a = projected_cross c d a and cd_b = projected_cross c d b in
        (((ab_c > 1e-12 && ab_d < -.1e-12)
          || (ab_c < -.1e-12 && ab_d > 1e-12))
         && ((cd_a > 1e-12 && cd_b < -.1e-12)
           || (cd_a < -.1e-12 && cd_b > 1e-12)))
        || (abs_float ab_c <= 1e-12 && projected_on_segment c a b)
        || (abs_float ab_d <= 1e-12 && projected_on_segment d a b)
        || (abs_float cd_a <= 1e-12 && projected_on_segment a c d)
        || (abs_float cd_b <= 1e-12 && projected_on_segment b c d) in
      let edge_count = boundary_token_count in
      let edge_a = Array.make edge_count 0 and edge_b = Array.make edge_count 0
      and edge_loop = Array.make edge_count 0
      and edge_local = Array.make edge_count 0
      and edge_min_x = Array.make edge_count 0.
      and edge_max_x = Array.make edge_count 0.
      and edge_min_y = Array.make edge_count 0.
      and edge_max_y = Array.make edge_count 0. and edge_at = ref 0 in
      Array.iteri (fun loop_index loop ->
        for local = 0 to Array.length loop - 1 do
          let edge = !edge_at and a = loop.(local)
          and b = loop.((local + 1) mod Array.length loop) in
          edge_a.(edge) <- a; edge_b.(edge) <- b;
          edge_loop.(edge) <- loop_index; edge_local.(edge) <- local;
          edge_min_x.(edge) <- Float.min (projected_x a) (projected_x b);
          edge_max_x.(edge) <- Float.max (projected_x a) (projected_x b);
          edge_min_y.(edge) <- Float.min (projected_y a) (projected_y b);
          edge_max_y.(edge) <- Float.max (projected_y a) (projected_y b);
          incr edge_at
        done) loops;
      let edge_order = Array.init edge_count Fun.id in
      Array.sort (fun left right ->
        let compared = Float.compare edge_min_x.(left) edge_min_x.(right) in
        if compared <> 0 then compared
        else let compared = Float.compare edge_min_y.(left) edge_min_y.(right) in
          if compared <> 0 then compared else Int.compare left right) edge_order;
      let active = Array.make edge_count 0 and active_count = ref 0 in
      for ordered = 0 to edge_count - 1 do
        if ordered land 4095 = 0 then Cancel.check_opt cancel;
        let edge = edge_order.(ordered) and retained = ref 0 in
        for slot = 0 to !active_count - 1 do
          let candidate = active.(slot) in
          if edge_max_x.(candidate) >= edge_min_x.(edge) -. 1e-12 then begin
            active.(!retained) <- candidate;
            incr retained
          end
        done;
        active_count := !retained;
        for slot = 0 to !active_count - 1 do
          let candidate = active.(slot) in
          if edge_min_y.(candidate) <= edge_max_y.(edge) +. 1e-12
             && edge_min_y.(edge) <= edge_max_y.(candidate) +. 1e-12
          then begin
            let same_loop = edge_loop.(candidate) = edge_loop.(edge) in
            let adjacent = if not same_loop then false
              else let left = edge_local.(candidate) and right = edge_local.(edge)
                and count = Array.length loops.(edge_loop.(edge)) in
                abs (left - right) = 1
                || (min left right = 0 && max left right = count - 1) in
            if not adjacent && projected_segments_intersect
                edge_a.(candidate) edge_b.(candidate) edge_a.(edge) edge_b.(edge)
            then raise (Clip_error (if same_loop then
                "Rdk.Plane_clip.clip: cap contour is geometrically self-intersecting"
              else "Rdk.Plane_clip.clip: cap contours intersect or touch"))
          end
        done;
        active.(!active_count) <- edge;
        incr active_count
      done;
      let loop_orientation = Array.mapi (fun loop_index loop ->
          if loop_index land 255 = 0 then Cancel.check_opt cancel;
          let anchor_x, anchor_y, anchor_z =
            position_of_geometry_token loop.(0) in
          let coordinate_scale = ref 0. in
          Array.iter (fun token ->
            let x, y, z = position_of_geometry_token token in
            coordinate_scale := Float.max !coordinate_scale
                (abs_float (x -. anchor_x));
            coordinate_scale := Float.max !coordinate_scale
                (abs_float (y -. anchor_y));
            coordinate_scale := Float.max !coordinate_scale
                (abs_float (z -. anchor_z))) loop;
          if !coordinate_scale = 0. then raise (Clip_error
              "Rdk.Plane_clip.clip: cap contour has zero coordinate extent");
          let orientation = ref 0. in
          for index = 0 to Array.length loop - 1 do
            let next = (index + 1) mod Array.length loop in
            let x0, y0, z0 = position_of_geometry_token loop.(index)
            and x1, y1, z1 = position_of_geometry_token loop.(next) in
            let x0 = (x0 -. anchor_x) /. !coordinate_scale
            and y0 = (y0 -. anchor_y) /. !coordinate_scale
            and z0 = (z0 -. anchor_z) /. !coordinate_scale
            and x1 = (x1 -. anchor_x) /. !coordinate_scale
            and y1 = (y1 -. anchor_y) /. !coordinate_scale
            and z1 = (z1 -. anchor_z) /. !coordinate_scale in
            orientation := !orientation
                +. (((y0 -. y1) *. (z0 +. z1)) *. nx)
                +. (((z0 -. z1) *. (x0 +. x1)) *. ny)
                +. (((x0 -. x1) *. (y0 +. y1)) *. nz)
          done;
          if not (finite !orientation) || abs_float !orientation <= 1e-14 then
            raise (Clip_error
              "Rdk.Plane_clip.clip: cap contour has degenerate projected area");
          !orientation) loops in
      let parent = Array.make loop_count (-1) in
      for child = 0 to loop_count - 1 do
        if child land 255 = 0 then Cancel.check_opt cancel;
        let x = projected_x loops.(child).(0)
        and y = projected_y loops.(child).(0) in
        for candidate = 0 to loop_count - 1 do
          if candidate <> child
             && (loop_orientation.(candidate) > 0.)
                <> (loop_orientation.(child) > 0.)
          then begin
            let x0, y0, x1, y1 = bounds.(candidate) in
            if x >= x0 && x <= x1 && y >= y0 && y <= y1
               && point_inside loops.(candidate) x y
            then begin
              let current = parent.(child) in
              if current < 0 then parent.(child) <- candidate
              else begin
                let candidate_x = projected_x loops.(candidate).(0)
                and candidate_y = projected_y loops.(candidate).(0) in
                if point_inside loops.(current) candidate_x candidate_y then
                  parent.(child) <- candidate
              end
            end
          end
        done
      done;
      let depth = Array.make loop_count 0 in
      for loop = 0 to loop_count - 1 do
        if loop land 255 = 0 then Cancel.check_opt cancel;
        let current = ref parent.(loop) and value = ref 0 in
        while !current >= 0 do
          incr value;
          if !value > loop_count then raise (Clip_error
              "Rdk.Plane_clip.clip: cyclic cap contour containment");
          current := parent.(!current)
        done;
        depth.(loop) <- !value
      done;
      let child_lists = Array.make loop_count [] in
      for child = loop_count - 1 downto 0 do
        let container = parent.(child) in
        if container >= 0 then
          child_lists.(container) <- loops.(child) :: child_lists.(container)
      done;
      let child_holes = Array.map Array.of_list child_lists
      and triangulated = Array.make loop_count None
      and nested_components = ref 0 in
      for outer = 0 to loop_count - 1 do
        if depth.(outer) land 1 = 0 && Array.length child_holes.(outer) > 0 then
          incr nested_components
      done;
      let triangulate_outer outer =
        if depth.(outer) land 1 = 0 && Array.length child_holes.(outer) > 0 then
          triangulated.(outer) <- Some (Contour_triangulation.triangulate ?cancel
              ~point:(fun token -> projected_x token, projected_y token)
              ~outer:loops.(outer) ~holes:child_holes.(outer) ()) in
      if !nested_components >= 2 && boundary_token_count >= 64 then
        Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(loop_count - 1)
          triangulate_outer
      else
        for outer = 0 to loop_count - 1 do triangulate_outer outer done;
      let want_positive = side = 1 in
      let emit_loop loop_index =
        let loop = loops.(loop_index) in
        let reverse = (loop_orientation.(loop_index) > 0.) <> want_positive in
        let start = builder.corner_count in
        for local = 0 to Array.length loop - 1 do
          let source_index = if reverse then Array.length loop - local - 1 else local in
          let boundary_token = loop.(source_index) in
          add_corner builder ~token:boundary_token ~a:(-1) ~b:(-1) ~t:0.
        done;
        ignore (finish_primitive builder ~start ~kind:'\000' ~source:(-1)
          ~side ~clipped:true ~cap:true ~minimum:3) in
      let emit_triangle a b c =
        let ax, ay, az = position_of_geometry_token a
        and bx, by, bz = position_of_geometry_token b
        and cx, cy, cz = position_of_geometry_token c in
        let abx = (bx -. ax) /. !cap_coordinate_scale
        and aby = (by -. ay) /. !cap_coordinate_scale
        and abz = (bz -. az) /. !cap_coordinate_scale
        and acx = (cx -. ax) /. !cap_coordinate_scale
        and acy = (cy -. ay) /. !cap_coordinate_scale
        and acz = (cz -. az) /. !cap_coordinate_scale in
        let orientation = ((aby *. acz) -. (abz *. acy)) *. nx
            +. ((abz *. acx) -. (abx *. acz)) *. ny
            +. ((abx *. acy) -. (aby *. acx)) *. nz in
        if not (finite orientation) || orientation = 0. then raise (Clip_error
            "Rdk.Plane_clip.clip: cap contour triangulation emitted a degenerate triangle");
        let b, c = if (orientation > 0.) = want_positive then b, c else c, b in
        let start = builder.corner_count in
        add_corner builder ~token:a ~a:(-1) ~b:(-1) ~t:0.;
        add_corner builder ~token:b ~a:(-1) ~b:(-1) ~t:0.;
        add_corner builder ~token:c ~a:(-1) ~b:(-1) ~t:0.;
        ignore (finish_primitive builder ~start ~kind:'\000' ~source:(-1)
          ~side ~clipped:true ~cap:true ~minimum:3) in
      for outer = 0 to loop_count - 1 do
        if outer land 255 = 0 then Cancel.check_opt cancel;
        if depth.(outer) land 1 = 0 then begin
          if Array.length child_holes.(outer) = 0 then emit_loop outer
          else begin
              let triangles = match triangulated.(outer) with
                | Some (Ok triangles) -> triangles
                | Some (Error message) -> raise (Clip_error
                    ("Rdk.Plane_clip.clip: cap contours: " ^ message))
                | None -> raise (Clip_error
                    "Rdk.Plane_clip.clip: missing cap contour triangulation plan") in
              for triangle = 0 to (Array.length triangles / 3) - 1 do
                let at = triangle * 3 in
                emit_triangle triangles.(at) triangles.(at + 1)
                  triangles.(at + 2)
              done
          end
        end
      done
    in
    if fill && segments.segment_count > 0 then begin
      (match keep with Above -> trace_caps 0 | Below -> trace_caps 1
       | All -> trace_caps 0; trace_caps 1)
    end;
    let total_tokens = geometry_token_count in
    let used = Array.make total_tokens false in
    Array.blit standalone_used 0 used 0 geometry_token_count;
    for corner = 0 to builder.corner_count - 1 do used.(builder.tokens.(corner)) <- true done;
    let token_to_point = Array.make total_tokens (-1) and output_point_count = ref 0 in
    for value = 0 to total_tokens - 1 do
      if used.(value) then begin
        token_to_point.(value) <- !output_point_count;
        incr output_point_count
      end
    done;
    let point_token = Array.make !output_point_count 0 and at = ref 0 in
    for value = 0 to total_tokens - 1 do
      if used.(value) then begin point_token.(!at) <- value; incr at end
    done;
    let point_source_a = Array.make !output_point_count 0
    and point_source_b = Array.make !output_point_count 0
    and point_source_t = Array.make !output_point_count 0.
    and px = Array.make !output_point_count 0.
    and py = Array.make !output_point_count 0.
    and pz = Array.make !output_point_count 0. in
    if !output_point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(!output_point_count - 1) (fun output ->
          if output land 4095 = 0 then Cancel.check_opt cancel;
          let value = point_token.(output) in
          if is_passthrough_token value then begin
            let point = passthrough_point value in
            point_source_a.(output) <- point; point_source_b.(output) <- point;
            px.(output) <- positions.x.(point);
            py.(output) <- positions.y.(point);
            pz.(output) <- positions.z.(point)
          end else let base = base_of_token value in
          if base < point_count then begin
            point_source_a.(output) <- base; point_source_b.(output) <- base;
            let projection = if point_affected base
                && classified_distance.(base) = 0. then raw_distance.(base) else 0. in
            px.(output) <- positions.x.(base) -. (projection *. nx);
            py.(output) <- positions.y.(base) -. (projection *. ny);
            pz.(output) <- positions.z.(base) -. (projection *. nz)
          end else begin
            let edge = base - point_count in
            let a = index_view.edge_a.(edge) and b = index_view.edge_b.(edge) in
            let t = clamp01 (-.raw_distance.(a) /.
                (raw_distance.(b) -. raw_distance.(a))) in
            point_source_a.(output) <- a; point_source_b.(output) <- b;
            point_source_t.(output) <- t;
            px.(output) <- positions.x.(a) +. ((positions.x.(b) -. positions.x.(a)) *. t);
            py.(output) <- positions.y.(a) +. ((positions.y.(b) -. positions.y.(a)) *. t);
            pz.(output) <- positions.z.(a) +. ((positions.z.(b) -. positions.z.(a)) *. t)
          end);
    let output_vertex_points = Array.make builder.corner_count 0 in
    if builder.corner_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(builder.corner_count - 1) (fun corner ->
          output_vertex_points.(corner) <- token_to_point.(builder.tokens.(corner)));
    let output_offsets = trim_int builder.offsets (builder.primitive_count + 1)
    and output_kinds = trim_bytes builder.kinds builder.primitive_count
    and primitive_source = trim_int builder.primitive_source builder.primitive_count
    and primitive_side = trim_int builder.primitive_side builder.primitive_count
    and primitive_clipped = trim_bool builder.primitive_clipped builder.primitive_count
    and primitive_cap = trim_bool builder.primitive_cap builder.primitive_count
    and corner_a = trim_int builder.corner_a builder.corner_count
    and corner_b = trim_int builder.corner_b builder.corner_count
    and corner_t = trim_float builder.corner_t builder.corner_count in
    let corner_cap_side = Array.make builder.corner_count (-1) in
    for primitive = 0 to builder.primitive_count - 1 do
      if primitive_cap.(primitive) then
        for corner = output_offsets.(primitive) to output_offsets.(primitive + 1) - 1 do
          corner_cap_side.(corner) <- primitive_side.(primitive)
        done
    done;
    let output_topology = Topology.View.create_validated_owned
        ~point_count:!output_point_count ~vertex_points:output_vertex_points
        ~primitive_offsets:output_offsets ~primitive_kinds:output_kinds in
    let interpolate_float source mapping_a mapping_b mapping_t =
      let count = Array.length mapping_a and output = Array.make (Array.length mapping_a) 0. in
      if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1)
          (fun index ->
            if index land 4095 = 0 then Cancel.check_opt cancel;
            let a = mapping_a.(index) and b = mapping_b.(index) in
            if a >= 0 then output.(index) <- source.(a)
                +. ((source.(b) -. source.(a)) *. mapping_t.(index)));
      output in
    let remap_attribute attribute =
      let owner = Attribute.owner attribute in
      let mapping = match owner with
        | Attribute.Point -> Some (point_source_a, point_source_b, point_source_t)
        | Attribute.Vertex -> Some (corner_a, corner_b, corner_t)
        | Attribute.Primitive -> None
        | Attribute.Detail -> None in
      match owner, mapping with
      | Attribute.Detail, _ -> Ok attribute
      | Attribute.Primitive, _ ->
          let nearest_float source =
            let output = Array.make builder.primitive_count 0. in
            if builder.primitive_count > 0 then Parallel.for_ ~chunk_size:grain
                ~start:0 ~finish:(builder.primitive_count - 1) (fun index ->
                  if index land 4095 = 0 then Cancel.check_opt cancel;
                  let input = primitive_source.(index) in
                  if input >= 0 then output.(index) <- source.(input));
            output
          and nearest_int source =
            let output = Array.make builder.primitive_count 0 in
            if builder.primitive_count > 0 then Parallel.for_ ~chunk_size:grain
                ~start:0 ~finish:(builder.primitive_count - 1) (fun index ->
                  if index land 4095 = 0 then Cancel.check_opt cancel;
                  let input = primitive_source.(index) in
                  if input >= 0 then output.(index) <- source.(input));
            output
          and nearest_text source =
            let output = Array.make builder.primitive_count "" in
            if builder.primitive_count > 0 then Parallel.for_ ~chunk_size:grain
                ~start:0 ~finish:(builder.primitive_count - 1) (fun index ->
                  if index land 4095 = 0 then Cancel.check_opt cancel;
                  let input = primitive_source.(index) in
                  if input >= 0 then output.(index) <- source.(input));
            output in
          let storage = match Attribute.View.storage attribute with
            | Attribute.Float values -> Attribute.Float (nearest_float values)
            | Attribute.Int values -> Attribute.Int (nearest_int values)
            | Attribute.Int_array values -> Attribute.Int_array
                (Ragged_ops.remap_int ?cancel ~grain primitive_source values)
            | Attribute.Float_array values -> Attribute.Float_array
                (Ragged_ops.remap_float ?cancel ~grain primitive_source values)
            | Attribute.Text values -> Attribute.Text (nearest_text values)
            | Attribute.Float2 values ->
                let values = Packed.Float2.View.view values in
                Attribute.Float2 (Packed.Float2.of_owned
                  ~x:(nearest_float values.x) ~y:(nearest_float values.y) |> get_ok)
            | Attribute.Float3 values ->
                let values = Packed.Float3.View.view values in
                Attribute.Float3 (Packed.Float3.View.of_owned_exn
                  ~x:(nearest_float values.x) ~y:(nearest_float values.y)
                  ~z:(nearest_float values.z))
            | Attribute.Float4 values ->
                let values = Packed.Float4.View.view values in
                Attribute.Float4 (Packed.Float4.of_owned
                  ~x:(nearest_float values.x) ~y:(nearest_float values.y)
                  ~z:(nearest_float values.z) ~w:(nearest_float values.w) |> get_ok) in
          Attribute.create_owned ~name:(Attribute.name attribute) ~owner storage
      | (Attribute.Point | Attribute.Vertex), Some (mapping_a, mapping_b, mapping_t) ->
          let count = Array.length mapping_a in
          let nearest_index index =
            let a = mapping_a.(index) and b = mapping_b.(index) in
            if a < 0 then -1 else if mapping_t.(index) < 0.5 then a
            else if mapping_t.(index) > 0.5 then b else min a b in
          let nearest_mapping = Array.init count nearest_index in
          let is_normal = String.equal (Attribute.name attribute) "N" in
          let float_plane source = interpolate_float source
              mapping_a mapping_b mapping_t in
          let storage = match Attribute.View.storage attribute with
            | Attribute.Float values -> Attribute.Float (float_plane values)
            | Attribute.Int values ->
                let output = Array.make count 0 in
                if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                    ~finish:(count - 1) (fun index ->
                      if index land 4095 = 0 then Cancel.check_opt cancel;
                      let source = nearest_index index in
                      if source >= 0 then output.(index) <- values.(source));
                Attribute.Int output
            | Attribute.Int_array values -> Attribute.Int_array
                (Ragged_ops.remap_int ?cancel ~grain nearest_mapping values)
            | Attribute.Float_array values -> Attribute.Float_array
                (Ragged_ops.remap_float ?cancel ~grain nearest_mapping values)
            | Attribute.Text values ->
                let output = Array.make count "" in
                if count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
                    ~finish:(count - 1) (fun index ->
                      if index land 4095 = 0 then Cancel.check_opt cancel;
                      let source = nearest_index index in
                      if source >= 0 then output.(index) <- values.(source));
                Attribute.Text output
            | Attribute.Float2 values ->
                let values = Packed.Float2.View.view values in
                Attribute.Float2 (Packed.Float2.of_owned ~x:(float_plane values.x)
                  ~y:(float_plane values.y) |> get_ok)
            | Attribute.Float3 values ->
                let values = Packed.Float3.View.view values in
                let x = float_plane values.x and y = float_plane values.y
                and z = float_plane values.z in
                if is_normal && count > 0 then Parallel.for_ ~chunk_size:grain
                    ~start:0 ~finish:(count - 1) (fun index ->
                      let cap_side = if owner = Attribute.Point then -1
                        else corner_cap_side.(index) in
                      if cap_side >= 0 then begin
                        let direction = if cap_side = 0 then -1. else 1. in
                        x.(index) <- direction *. nx; y.(index) <- direction *. ny;
                        z.(index) <- direction *. nz
                      end else begin
                        let length = sqrt ((x.(index) *. x.(index))
                          +. (y.(index) *. y.(index)) +. (z.(index) *. z.(index))) in
                        if length > 1e-20 then begin x.(index) <- x.(index) /. length;
                          y.(index) <- y.(index) /. length; z.(index) <- z.(index) /. length end
                      end);
                Attribute.Float3 (Packed.Float3.View.of_owned_exn ~x ~y ~z)
            | Attribute.Float4 values ->
                let values = Packed.Float4.View.view values in
                Attribute.Float4 (Packed.Float4.of_owned ~x:(float_plane values.x)
                  ~y:(float_plane values.y) ~z:(float_plane values.z)
                  ~w:(float_plane values.w) |> get_ok) in
          Attribute.create_owned ~name:(Attribute.name attribute) ~owner storage
      | _ -> assert false
    in
    let rec remap_attributes result = function
      | [] -> Ok (List.rev result)
      | attribute :: rest -> Result.bind (remap_attribute attribute)
          (fun attribute -> remap_attributes (attribute :: result) rest) in
    Result.bind (remap_attributes [] (Geometry.attributes geometry)) (fun attributes ->
      let has_polygon = ref false and has_cap = ref false in
      for primitive = 0 to builder.primitive_count - 1 do
        if Bytes.get output_kinds primitive = '\000' then has_polygon := true;
        if primitive_cap.(primitive) then has_cap := true
      done;
      let source_vertex_normal = match Geometry.find_attribute
          ~owner:Attribute.Vertex "N" geometry with
        | Some attribute ->
            (match Attribute.View.storage attribute with
             | Attribute.Float3 _ -> true | _ -> false)
        | None -> false in
      let output_point_normal = match List.find_opt (fun attribute ->
          Attribute.owner attribute = Attribute.Point
          && String.equal (Attribute.name attribute) "N") attributes with
        | Some attribute ->
            (match Attribute.View.storage attribute with
             | Attribute.Float3 values -> Some values | _ -> None)
        | None -> None in
      let attributes =
        if source_vertex_normal || not !has_polygon then attributes
        else match output_point_normal with
        | Some point_normals when !has_cap ->
            let point_normals = Packed.Float3.View.view point_normals in
            let vx = Array.make builder.corner_count 0.
            and vy = Array.make builder.corner_count 0.
            and vz = Array.make builder.corner_count 0. in
            if builder.corner_count > 0 then Parallel.for_ ~chunk_size:grain
                ~start:0 ~finish:(builder.corner_count - 1) (fun corner ->
                  let cap_side = corner_cap_side.(corner) in
                  if cap_side >= 0 then begin
                    let direction = if cap_side = 0 then -1. else 1. in
                    vx.(corner) <- direction *. nx;
                    vy.(corner) <- direction *. ny;
                    vz.(corner) <- direction *. nz
                  end else begin
                    let point = output_vertex_points.(corner) in
                    vx.(corner) <- point_normals.x.(point);
                    vy.(corner) <- point_normals.y.(point);
                    vz.(corner) <- point_normals.z.(point)
                  end);
            let normal = Attribute.create_key_owned
                (Attribute.normal ~owner:Attribute.Vertex)
                (Packed.Float3.View.of_owned_exn ~x:vx ~y:vy ~z:vz) |> get_ok in
            attributes @ [normal]
        | Some _ -> attributes
        | None ->
          let vx = Array.make builder.corner_count 0.
          and vy = Array.make builder.corner_count 0.
          and vz = Array.make builder.corner_count 0. in
          if builder.primitive_count > 0 then Parallel.for_
              ~chunk_size:(max 1 (grain / 8)) ~start:0
              ~finish:(builder.primitive_count - 1) (fun primitive ->
                if primitive land 1023 = 0 then Cancel.check_opt cancel;
                if Bytes.get output_kinds primitive = '\000' then begin
                  let first = output_offsets.(primitive)
                  and last = output_offsets.(primitive + 1) in
                  let sx = ref 0. and sy = ref 0. and sz = ref 0. in
                  for corner = first to last - 1 do
                    let next = if corner + 1 = last then first else corner + 1 in
                    let a = output_vertex_points.(corner)
                    and b = output_vertex_points.(next) in
                    sx := !sx +. ((py.(a) -. py.(b)) *. (pz.(a) +. pz.(b)));
                    sy := !sy +. ((pz.(a) -. pz.(b)) *. (px.(a) +. px.(b)));
                    sz := !sz +. ((px.(a) -. px.(b)) *. (py.(a) +. py.(b)))
                  done;
                  let length = sqrt ((!sx *. !sx) +. (!sy *. !sy) +. (!sz *. !sz)) in
                  if length > 1e-20 then
                    for corner = first to last - 1 do
                      vx.(corner) <- !sx /. length;
                      vy.(corner) <- !sy /. length;
                      vz.(corner) <- !sz /. length
                    done
                end);
          let normal = Attribute.create_key_owned
              (Attribute.normal ~owner:Attribute.Vertex)
              (Packed.Float3.View.of_owned_exn ~x:vx ~y:vy ~z:vz) |> get_ok in
          attributes @ [normal]
      in
      let remap_group group =
        let name = Group.name group in
        let target, source_of_target = match Group.owner group with
          | Group.Point ->
              Group.init ~grain ~owner:Group.Point ~name !output_point_count
                (fun output ->
                  if output land 4095 = 0 then Cancel.check_opt cancel;
                  let a = point_source_a.(output) and b = point_source_b.(output) in
                  Group.mem a group || Group.mem b group),
              (fun () -> Array.init !output_point_count (fun output ->
                let a = point_source_a.(output) and b = point_source_b.(output) in
                if a = b then a else -1))
          | Group.Vertex ->
              Group.init ~grain ~owner:Group.Vertex ~name builder.corner_count
                (fun output ->
                  if output land 4095 = 0 then Cancel.check_opt cancel;
                  let a = corner_a.(output) and b = corner_b.(output) in
                  a >= 0 && (Group.mem a group || Group.mem b group)),
              (fun () -> Array.init builder.corner_count (fun output ->
                let a = corner_a.(output) and b = corner_b.(output) in
                if a >= 0 && a = b then a else -1))
          | Group.Primitive ->
              Group.init ~grain ~owner:Group.Primitive ~name
                builder.primitive_count (fun output ->
                  if output land 4095 = 0 then Cancel.check_opt cancel;
                  let source = primitive_source.(output) in
                  source >= 0 && Group.mem source group),
              (fun () -> primitive_source) in
        if not (Group.is_ordered group) then target
        else Group.View.remap_order ~source:group
            ~source_of_target:(source_of_target ()) target in
      let groups = List.map remap_group (Geometry.groups geometry) in
      let source_edge_groups = Geometry.edge_groups geometry in
      let target_index =
        if source_edge_groups = [] && clipped_edge_group = None then None
        else Some (Topology_index.create ?cancel output_topology) in
      let edge_groups = match source_edge_groups, target_index with
        | [], _ -> []
        | source_groups, Some target_index ->
            let target_view = Topology_index.View.view target_index in
            let source_edge_of_output_edge edge =
              let a = target_view.edge_a.(edge) and b = target_view.edge_b.(edge) in
              let aa = point_source_a.(a) and ab = point_source_b.(a)
              and ba = point_source_a.(b) and bb = point_source_b.(b) in
              let candidate =
                if aa <> ab then Topology_index.find_edge topology_index ~a:aa ~b:ab
                else if ba <> bb then Topology_index.find_edge topology_index ~a:ba ~b:bb
                else Topology_index.find_edge topology_index ~a:aa ~b:ba in
              match candidate with
              | None -> None
              | Some source_edge ->
                  let ea, eb = Topology_index.edge_points topology_index source_edge in
                  let on_edge x y = (x = ea || x = eb) && (y = ea || y = eb) in
                  if on_edge aa ab && on_edge ba bb then Some source_edge else None in
            List.map (fun source_group ->
              Edge_group.init ~grain ~topology:output_topology ~index:target_index
                ~name:(Edge_group.name source_group) (fun edge ->
                  if edge land 16_383 = 0 then Cancel.check_opt cancel;
                  match source_edge_of_output_edge edge with
                  | None -> false
                  | Some source_edge -> Edge_group.mem source_edge source_group))
              source_groups
        | _ -> assert false in
      Result.bind (Geometry.create
        ~positions:(Packed.Float3.View.of_owned_exn ~x:px ~y:py ~z:pz)
        ~topology:output_topology ~attributes ~groups ~edge_groups ()) (fun output ->
          let install_clipped_edges output = match clipped_edge_group, target_index with
            | None, _ -> Ok output
            | Some name, Some target_index ->
                let target_view = Topology_index.View.view target_index in
                let generated = Edge_group.init ~grain ~topology:output_topology
                    ~index:target_index ~name (fun edge ->
                      if edge land 16_383 = 0 then Cancel.check_opt cancel;
                      let a = target_view.edge_a.(edge)
                      and b = target_view.edge_b.(edge) in
                      token_on_plane point_token.(a)
                      && token_on_plane point_token.(b)) in
                let group = if replace_existing_groups then generated
                  else match Geometry.find_edge_group name output with
                    | None -> generated
                    | Some existing -> Edge_group.union existing generated |> get_ok in
                Geometry.with_edge_group group output
            | Some _, None -> assert false in
          let install name predicate output = match name with
            | None -> Ok output
            | Some name ->
                let generated = Group.init ~grain ~owner:Group.Primitive ~name
                    builder.primitive_count predicate in
                let group = if replace_existing_groups then generated
                  else match Geometry.find_group ~owner:Group.Primitive name output with
                    | None -> generated
                    | Some existing -> Group.union existing generated |> get_ok in
                Geometry.with_group group output in
          Result.bind (install_clipped_edges output) (fun output ->
          Result.bind (install cap_group (fun p -> primitive_cap.(p)) output)
            (fun output -> Result.bind
              (install clipped_group
                 (fun p -> primitive_clipped.(p) && not primitive_cap.(p)) output)
              (fun output -> Result.bind
                (install above_group
                   (fun p -> primitive_side.(p) = 0 && not primitive_cap.(p)) output)
                (fun output -> install below_group
                  (fun p -> primitive_side.(p) = 1 && not primitive_cap.(p)) output))))))
  with Clip_error message -> Error message

type selection = Deform.selection =
  | Selected_points of Group.t
  | Selected_vertices of Group.t
  | Selected_primitives of Group.t
  | Selected_edges of Edge_group.t
