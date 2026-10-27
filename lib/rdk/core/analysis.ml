open Rays_math

type bounds = {
  min : Rays_math.Vec3.t;
  max : Rays_math.Vec3.t;
  center : Rays_math.Vec3.t;
  size : Rays_math.Vec3.t;
}

let bounds ?cancel geometry =
  Cancel.check_opt cancel;
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let count = Array.length positions.x in
  if count = 0 then None
  else begin
    let min_x = ref positions.x.(0) and min_y = ref positions.y.(0)
    and min_z = ref positions.z.(0) and max_x = ref positions.x.(0)
    and max_y = ref positions.y.(0) and max_z = ref positions.z.(0) in
    for index = 1 to count - 1 do
      if index land 16_383 = 0 then Cancel.check_opt cancel;
      let x = positions.x.(index) and y = positions.y.(index)
      and z = positions.z.(index) in
      if x < !min_x then min_x := x; if x > !max_x then max_x := x;
      if y < !min_y then min_y := y; if y > !max_y then max_y := y;
      if z < !min_z then min_z := z; if z > !max_z then max_z := z
    done;
    let min = Rays_math.Vec3.create !min_x !min_y !min_z
    and max = Rays_math.Vec3.create !max_x !max_y !max_z in
    Some {
      min; max;
      center = Rays_math.Vec3.create ((!min_x +. !max_x) *. 0.5)
          ((!min_y +. !max_y) *. 0.5) ((!min_z +. !max_z) *. 0.5);
      size = Rays_math.Vec3.create (!max_x -. !min_x) (!max_y -. !min_y)
          (!max_z -. !min_z);
    }
  end

type measure = Perimeter | Area | Signed_volume
type accumulation = Per_element | Throughout
type connectivity_owner = Connectivity_points | Connectivity_primitives
type connectivity_attribute = Connectivity_integer | Connectivity_text of string

let measure_label = function
  | Perimeter -> "perimeter"
  | Area -> "area"
  | Signed_volume -> "volume"

type primitive_selection = All_primitives of int | Selected_primitives of int array

let selected_primitives ?primitives geometry =
  let count = Geometry.primitive_count geometry in
  match primitives with
  | None -> Ok (All_primitives count)
  | Some group when Group.owner group <> Group.Primitive -> Error
      "Rdk.Analysis.measure: selection must be primitive-owned"
  | Some group when Group.length group <> count -> Error
      "Rdk.Analysis.measure: primitive selection length does not match geometry"
  | Some group ->
      let selected = Array.make (Group.cardinality group) 0 and next = ref 0 in
      Group.iter (fun primitive -> selected.(!next) <- primitive; incr next) group;
      Ok (Selected_primitives selected)

let selection_count = function
  | All_primitives count -> count
  | Selected_primitives selected -> Array.length selected

let selected_primitive selection index = match selection with
  | All_primitives _ -> index
  | Selected_primitives selected -> selected.(index)

let iter_selected selection f = match selection with
  | All_primitives count -> for primitive = 0 to count - 1 do f primitive done
  | Selected_primitives selected -> Array.iter f selected

let measurement_anchor ?cancel measure geometry = match measure with
  | Perimeter | Area -> 0., 0., 0.
  | Signed_volume ->
      (match bounds ?cancel geometry with
       | None -> 0., 0., 0.
       | Some bounds -> bounds.center.x, bounds.center.y, bounds.center.z)

let triangle_measure measure anchor positions topology a_vertex b_vertex c_vertex =
  let a = topology.Topology.Private.vertex_points.(a_vertex)
  and b = topology.vertex_points.(b_vertex)
  and c = topology.vertex_points.(c_vertex) in
  let ax = positions.Packed.Float3.Private.x.(a)
  and ay = positions.y.(a) and az = positions.z.(a)
  and bx = positions.x.(b) and by = positions.y.(b) and bz = positions.z.(b)
  and cx = positions.x.(c) and cy = positions.y.(c) and cz = positions.z.(c) in
  if not (Float.is_finite ax && Float.is_finite ay && Float.is_finite az
      && Float.is_finite bx && Float.is_finite by && Float.is_finite bz
      && Float.is_finite cx && Float.is_finite cy && Float.is_finite cz)
  then Float.nan
  else match measure with
    | Perimeter -> assert false
    | Area ->
        let abx = bx -. ax and aby = by -. ay and abz = bz -. az
        and acx = cx -. ax and acy = cy -. ay and acz = cz -. az in
        let nx = (aby *. acz) -. (abz *. acy)
        and ny = (abz *. acx) -. (abx *. acz)
        and nz = (abx *. acy) -. (aby *. acx) in
        let value = 0.5 *. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
        value
    | Signed_volume ->
        let ox, oy, oz = anchor in
        let ax = ax -. ox and ay = ay -. oy and az = az -. oz
        and bx = bx -. ox and by = by -. oy and bz = bz -. oz
        and cx = cx -. ox and cy = cy -. oy and cz = cz -. oz in
        let cross_x = (by *. cz) -. (bz *. cy)
        and cross_y = (bz *. cx) -. (bx *. cz)
        and cross_z = (bx *. cy) -. (by *. cx) in
        let value = ((ax *. cross_x) +. (ay *. cross_y) +. (az *. cross_z)) /. 6. in
        value

let perimeter_measure positions topology primitive =
  let first = topology.Topology.Private.primitive_offsets.(primitive)
  and last = topology.primitive_offsets.(primitive + 1) in
  let size = last - first in
  let closed = Bytes.get topology.primitive_kinds primitive <> '\001' in
  let edges = if closed then size else max 0 (size - 1) in
  let total = ref 0. and valid = ref true in
  for local = 0 to edges - 1 do
    if !valid then begin
      let next = if local + 1 = size then 0 else local + 1 in
      let a = topology.vertex_points.(first + local)
      and b = topology.vertex_points.(first + next) in
      let ax = positions.Packed.Float3.Private.x.(a)
      and ay = positions.y.(a) and az = positions.z.(a)
      and bx = positions.x.(b) and by = positions.y.(b) and bz = positions.z.(b) in
      let dx = bx -. ax and dy = by -. ay and dz = bz -. az in
      let length = sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz)) in
      if not (Float.is_finite length) then valid := false
      else total := !total +. length
    end
  done;
  if !valid && Float.is_finite !total then !total else Float.nan

let primitive_measure_selected ?cancel ~grain selected measure geometry =
  if grain <= 0 then invalid_arg "Rdk.Analysis.measure: grain must be positive";
    let positions = Packed.Float3.Private.view (Geometry.positions geometry)
    and topology = Topology.Private.view (Geometry.topology geometry) in
    let primitive_count = Geometry.primitive_count geometry
    and selected_count = selection_count selected in
    let values = Array.make primitive_count 0. in
    let selected_vertices = ref 0 in
    iter_selected selected (fun primitive ->
      selected_vertices := !selected_vertices
        + topology.primitive_offsets.(primitive + 1)
        - topology.primitive_offsets.(primitive));
    let average_vertices = if selected_count = 0 then 1 else
        max 1 ((!selected_vertices + selected_count - 1) / selected_count) in
    let primitive_chunk = max 1 (grain / average_vertices) in
    let range_count = if selected_count = 0 then 0 else
        (selected_count + primitive_chunk - 1) / primitive_chunk in
    let errors = Array.make range_count None
    and anchor = measurement_anchor ?cancel measure geometry in
    if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
        ~finish:(range_count - 1) (fun range ->
      let first_selected = range * primitive_chunk
      and last_selected = min selected_count ((range + 1) * primitive_chunk) in
      let scratch = Polygon_triangulation.create_scratch () in
      for selected_index = first_selected to last_selected - 1 do
        if selected_index land 4095 = 0 then Cancel.check_opt cancel;
        if errors.(range) = None then begin
          let primitive = selected_primitive selected selected_index in
          match measure with
          | Perimeter ->
              let value = perimeter_measure positions topology primitive in
              if Float.is_finite value then values.(primitive) <- value
              else errors.(range) <- Some (Printf.sprintf
                  "primitive %d produced a non-finite perimeter" primitive)
          | Area | Signed_volume ->
              if Bytes.get topology.primitive_kinds primitive <> '\000' then
                errors.(range) <- Some (Printf.sprintf
                    "primitive %d is a curve, not a polygon" primitive)
              else begin
                let first = topology.primitive_offsets.(primitive)
                and last = topology.primitive_offsets.(primitive + 1) in
                if last - first = 3 then
                  let value = triangle_measure measure anchor positions topology
                      first (first + 1) (first + 2) in
                  if Float.is_finite value then values.(primitive) <- value
                  else errors.(range) <- Some (Printf.sprintf
                      "primitive %d produced a non-finite %s" primitive
                      (measure_label measure))
                else begin
                  let value = ref 0. and failure = ref None in
                  let emit _ a b c = if !failure = None then
                    let triangle = triangle_measure measure anchor positions
                        topology a b c in
                    if Float.is_finite triangle then value := !value +. triangle
                    else failure := Some "non-finite triangle measurement" in
                  match Polygon_triangulation.primitive ?cancel ~positions ~topology
                      ~scratch primitive ~emit with
                  | Error message -> errors.(range) <- Some message
                  | Ok () ->
                      (match !failure with
                       | Some message -> errors.(range) <- Some (Printf.sprintf
                           "primitive %d: %s" primitive message)
                       | None when Float.is_finite !value ->
                           values.(primitive) <- !value
                       | None -> errors.(range) <- Some (Printf.sprintf
                           "primitive %d produced a non-finite measurement" primitive))
                end
              end
        end
      done);
    let failure = ref None in
    Array.iter (fun error -> match !failure, error with
      | None, Some message -> failure := Some message
      | None, None | Some _, _ -> ()) errors;
    match !failure with None -> Ok values | Some message -> Error message

let primitive_measure_raw ?cancel ?(grain = 16_384) ?primitives measure geometry =
  Result.bind (selected_primitives ?primitives geometry) (fun selected ->
    primitive_measure_selected ?cancel ~grain selected measure geometry)

let primitive_measure ?cancel ?grain ?primitives measure geometry =
  try primitive_measure_raw ?cancel ?grain ?primitives measure geometry with
  | Cancel.Cancelled -> Error "Rdk.Analysis.measure: cancelled"
  | Invalid_argument message -> Error message

let total values =
  let result = ref 0. in
  for primitive = 0 to Array.length values - 1 do
    result := !result +. values.(primitive)
  done;
  !result

let surface_area ?cancel ?grain ?primitives geometry =
  Result.map total (primitive_measure ?cancel ?grain ?primitives Area geometry)

let perimeter ?cancel ?grain ?primitives geometry =
  Result.map total
    (primitive_measure ?cancel ?grain ?primitives Perimeter geometry)

let signed_volume ?cancel ?grain ?primitives geometry =
  Result.map total
    (primitive_measure ?cancel ?grain ?primitives Signed_volume geometry)

type disjoint_set = { parent : int array; rank : bytes }

let disjoint_set count allowed =
  { parent = Array.init count (fun element -> if allowed element then element else -1);
    rank = Bytes.make count '\000' }

let root set element =
  let cursor = ref element in
  while set.parent.(!cursor) <> !cursor do cursor := set.parent.(!cursor) done;
  let result = !cursor in
  let cursor = ref element in
  while set.parent.(!cursor) <> !cursor do
    let next = set.parent.(!cursor) in
    set.parent.(!cursor) <- result;
    cursor := next
  done;
  result

let union set left right =
  if left >= 0 && right >= 0 && set.parent.(left) >= 0 && set.parent.(right) >= 0
  then begin
    let left = root set left and right = root set right in
    if left <> right then begin
      let left_rank = Char.code (Bytes.unsafe_get set.rank left)
      and right_rank = Char.code (Bytes.unsafe_get set.rank right) in
      if left_rank < right_rank then set.parent.(left) <- right
      else if left_rank > right_rank then set.parent.(right) <- left
      else begin
        set.parent.(right) <- left;
        Bytes.unsafe_set set.rank left (Char.chr (left_rank + 1))
      end
    end
  end

let component_classes set =
  let count = Array.length set.parent in
  (* [parent] and [rank] are locally owned after the final union. Reuse rank
     byte 255 as an assigned-class marker and parent as the output plane.
     Union-by-rank cannot reach 255 for an OCaml-sized array. Marking every
     visited path node makes later traversals stop at an already classified
     ancestor, so stable classes need one pass and no owner-sized class map. *)
  let next_class = ref 0 in
  for element = 0 to count - 1 do
    if set.parent.(element) >= 0 then begin
      let representative = ref element in
      while Char.code (Bytes.unsafe_get set.rank !representative) <> 255
          && set.parent.(!representative) <> !representative do
        representative := set.parent.(!representative)
      done;
      let class_id =
        if Char.code (Bytes.unsafe_get set.rank !representative) = 255 then
          set.parent.(!representative)
        else begin
          let class_id = !next_class in
          incr next_class;
          set.parent.(!representative) <- class_id;
          Bytes.unsafe_set set.rank !representative '\255';
          class_id
        end in
      let cursor = ref element in
      while Char.code (Bytes.unsafe_get set.rank !cursor) <> 255 do
        let next = set.parent.(!cursor) in
        set.parent.(!cursor) <- class_id;
        Bytes.unsafe_set set.rank !cursor '\255';
        cursor := next
      done
    end
  done;
  set.parent, !next_class

let require_connectivity_group label owner count = function
  | None -> Ok ()
  | Some group when Group.owner group <> owner -> Error (Printf.sprintf
      "Connectivity: %s must be %s-owned" label
      (match owner with Group.Point -> "point" | Group.Vertex -> "vertex"
        | Group.Primitive -> "primitive"))
  | Some group when Group.length group <> count -> Error (Printf.sprintf
      "Connectivity: %s length does not match geometry" label)
  | Some _ -> Ok ()

type uv_view = Uv2 of Packed.Float2.Private.view | Uv3 of Packed.Float3.Private.view

let uv_view name geometry =
  match Geometry.find_attribute ~owner:Attribute.Vertex name geometry with
  | None -> Error (Printf.sprintf
      "Connectivity: missing vertex UV attribute %S" name)
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float2 values -> Ok (Uv2 (Packed.Float2.Private.view values))
       | Attribute.Float3 values -> Ok (Uv3 (Packed.Float3.Private.view values))
       | Attribute.Float _ | Attribute.Int _ | Attribute.Int_array _
       | Attribute.Float_array _ | Attribute.Float4 _ | Attribute.Text _ ->
           Error (Printf.sprintf
           "Connectivity: vertex UV attribute %S must be float2 or float3" name))

let uv_vertex_equal view left right = match view with
  | Uv2 values -> values.x.(left) = values.x.(right)
      && values.y.(left) = values.y.(right)
  | Uv3 values -> values.x.(left) = values.x.(right)
      && values.y.(left) = values.y.(right)
      && values.z.(left) = values.z.(right)

let edge_uv_equal topology reverse view left right =
  let left_next = reverse.Topology_index.Private.next_vertex.(left)
  and right_next = reverse.next_vertex.(right) in
  if left_next < 0 || right_next < 0 then false
  else
    let left_point = topology.Topology.Private.vertex_points.(left)
    and right_point = topology.vertex_points.(right) in
    if left_point = right_point then
      uv_vertex_equal view left right
      && uv_vertex_equal view left_next right_next
    else
      uv_vertex_equal view left right_next
      && uv_vertex_equal view left_next right

let classify_connectivity_raw ?cancel ?(grain = 16_384) ?primitives ?points
    ?seams ?uv_attribute owner geometry =
  let point_count = Geometry.point_count geometry
  and primitive_count = Geometry.primitive_count geometry in
  let topology_value = Geometry.topology geometry in
  let validation =
    if grain <= 0 then Error "Connectivity: grain must be positive"
    else if owner = Connectivity_primitives && points <> None then
      Error "Connectivity: point include group is available only in point mode"
    else if owner = Connectivity_points && uv_attribute <> None then
      Error "Connectivity: UV connectivity is available only in primitive mode"
    else if seams <> None && uv_attribute <> None then
      Error "Connectivity: seam and UV connectivity modes are mutually exclusive"
    else Result.bind
        (require_connectivity_group "primitive include group" Group.Primitive
           primitive_count primitives)
        (fun () -> require_connectivity_group "point include group" Group.Point
          point_count points)
  in
  Result.bind validation (fun () ->
    match seams with
    | Some group when Edge_group.topology_data_id group
        <> Topology.data_id topology_value ->
        Error "Connectivity: seam group belongs to a different topology"
    | None | Some _ ->
        Cancel.check_opt cancel;
        let topology = Topology.Private.view topology_value in
        let primitive_allowed primitive = match primitives with
          | None -> true | Some group -> Group.mem primitive group in
        let point_allowed point = match points with
          | None -> true | Some group -> Group.mem point group in
        match owner with
        | Connectivity_points ->
            let set = disjoint_set point_count point_allowed in
            let index_value = Topology_index.create ?cancel topology_value in
            let reverse = Topology_index.Private.view index_value in
            let edge_validation = match seams with
              | Some group when Edge_group.length group
                  <> Topology_index.edge_count index_value ->
                  Error "Connectivity: seam group length does not match topology"
              | None | Some _ -> Ok () in
            Result.map (fun () ->
              for primitive = 0 to primitive_count - 1 do
                if primitive land 16_383 = 0 then Cancel.check_opt cancel;
                if primitive_allowed primitive then begin
                  let first = topology.primitive_offsets.(primitive)
                  and last = topology.primitive_offsets.(primitive + 1) in
                  for vertex = first to last - 1 do
                    let edge = reverse.edge_of_vertex.(vertex) in
                    let is_seam = edge >= 0 && match seams with
                      | None -> false | Some group -> Edge_group.mem edge group in
                    if edge >= 0 && not is_seam then
                      union set reverse.edge_a.(edge) reverse.edge_b.(edge)
                  done
                end
              done;
              component_classes set) edge_validation
        | Connectivity_primitives ->
            let set = disjoint_set primitive_count primitive_allowed in
            match seams, uv_attribute with
            | None, None ->
                let first_primitive = Array.make point_count (-1) in
                for primitive = 0 to primitive_count - 1 do
                  if primitive land 16_383 = 0 then Cancel.check_opt cancel;
                  if primitive_allowed primitive then begin
                    let first = topology.primitive_offsets.(primitive)
                    and last = topology.primitive_offsets.(primitive + 1) in
                    for vertex = first to last - 1 do
                      let point = topology.vertex_points.(vertex) in
                      let previous = first_primitive.(point) in
                      if previous < 0 then first_primitive.(point) <- primitive
                      else union set primitive previous
                    done
                  end
                done;
                Ok (component_classes set)
            | seam_group, uv_name ->
                let index = Topology_index.create ?cancel topology_value in
                let reverse = Topology_index.Private.view index in
                let edge_validation = match seam_group with
                  | Some group when Edge_group.length group
                      <> Topology_index.edge_count index ->
                      Error "Connectivity: seam group length does not match topology"
                  | None | Some _ -> Ok () in
                Result.bind edge_validation (fun () ->
                  let uv_result = match uv_name with
                    | None -> Ok None
                    | Some name -> Result.map Option.some (uv_view name geometry) in
                  Result.map (fun uv ->
                    for edge = 0 to Topology_index.edge_count index - 1 do
                      if edge land 16_383 = 0 then Cancel.check_opt cancel;
                      let is_seam = match seam_group with
                        | None -> false | Some group -> Edge_group.mem edge group in
                      if not is_seam then begin
                        let first = reverse.edge_offsets.(edge)
                        and last = reverse.edge_offsets.(edge + 1) in
                        for left_slot = first to last - 2 do
                          let left_vertex = reverse.edge_vertices.(left_slot) in
                          let left = reverse.primitive_of_vertex.(left_vertex) in
                          if primitive_allowed left then
                            for right_slot = left_slot + 1 to last - 1 do
                              let right_vertex = reverse.edge_vertices.(right_slot) in
                              let right = reverse.primitive_of_vertex.(right_vertex) in
                              let uv_matches = match uv with
                                | None -> true
                                | Some uv -> edge_uv_equal topology reverse uv
                                    left_vertex right_vertex in
                              if primitive_allowed right && uv_matches then
                                union set left right
                            done
                        done
                      end
                    done;
                    component_classes set) uv_result))

let classify_connectivity ?cancel ?grain ?primitives ?points ?seams
    ?uv_attribute owner geometry =
  try match grain with
  | Some grain when grain <= 0 -> Error (Error.make ~operation:"connectivity"
      ~code:"invalid_parameter" "grain must be positive")
  | None | Some _ -> Result.map_error
        (Error.of_string ~operation:"connectivity" ~code:"invalid_connectivity")
        (classify_connectivity_raw ?cancel ?grain ?primitives ?points ?seams
          ?uv_attribute owner geometry)
  with
  | Cancel.Cancelled -> Error (Error.make ~operation:"connectivity"
      ~code:"cancelled" "connectivity classification was cancelled")
  | Invalid_argument message -> Error (Error.make ~operation:"connectivity"
      ~code:"invalid_parameter" message)

let with_measure_raw ?cancel ?grain ?primitives ?(accumulation = Per_element)
    ?name ?total_name measure geometry =
  let name = Option.value ~default:(measure_label measure) name in
  Result.bind (selected_primitives ?primitives geometry) (fun selected ->
    let grain = Option.value ~default:16_384 grain in
    Result.bind (primitive_measure_selected ?cancel ~grain selected measure geometry)
      (fun measured ->
        let measured_total = total measured in
        let output = match Geometry.find_attribute ~owner:Attribute.Primitive
            name geometry with
          | None when accumulation = Per_element -> Ok measured
          | None -> Ok (Array.make (Geometry.primitive_count geometry) 0.)
          | Some attribute ->
              (match Attribute.Private.storage attribute with
               | Attribute.Float values -> Ok (Array.copy values)
               | Attribute.Int _ | Attribute.Int_array _ | Attribute.Float_array _
               | Attribute.Float2 _ | Attribute.Float3 _
               | Attribute.Float4 _ | Attribute.Text _ -> Error (Printf.sprintf
                   "Rdk.Analysis.measure: existing primitive attribute %s is not float"
                   name)) in
        Result.bind output (fun output ->
          iter_selected selected (fun primitive -> output.(primitive) <- match accumulation with
            | Per_element -> measured.(primitive)
            | Throughout -> measured_total);
          Result.bind (Attribute.create_owned ~name ~owner:Attribute.Primitive
              (Attribute.Float output)) (fun attribute ->
            Result.bind (Geometry.with_attribute attribute geometry) (fun geometry ->
              match total_name with
              | None -> Ok geometry
              | Some name ->
                  Result.bind (Attribute.create_owned ~name ~owner:Attribute.Detail
                      (Attribute.Float [|measured_total|])) (fun attribute ->
                    Geometry.with_attribute attribute geometry))))))

let with_measure ?cancel ?grain ?primitives ?accumulation ?name ?total_name
    measure geometry =
  try Result.map_error
      (Error.of_string ~operation:"measure" ~code:"invalid_measure")
      (with_measure_raw ?cancel ?grain ?primitives ?accumulation ?name
         ?total_name measure geometry)
  with
  | Cancel.Cancelled -> Error (Error.make ~operation:"measure" ~code:"cancelled"
      "measurement was cancelled")
  | Invalid_argument message -> Error (Error.make ~operation:"measure"
      ~code:"invalid_parameter" message)

let with_connectivity ?cancel ?(grain = 16_384) ?primitives ?points ?seams
    ?uv_attribute ?(owner = Connectivity_primitives) ?(name = "class")
    ?(attribute = Connectivity_integer) geometry =
  try
    if String.trim name = "" then Error (Error.make ~operation:"connectivity"
        ~code:"invalid_parameter" "attribute name must not be empty")
    else Result.bind (classify_connectivity ?cancel ~grain ?primitives ?points
        ?seams ?uv_attribute owner geometry) (fun (classes, class_count) ->
      let attribute_owner = match owner with
        | Connectivity_points -> Attribute.Point
        | Connectivity_primitives -> Attribute.Primitive in
      let storage = match attribute with
        | Connectivity_integer -> Attribute.Int classes
        | Connectivity_text prefix ->
            let labels = Array.init class_count (fun class_id ->
              prefix ^ string_of_int class_id) in
            let values = Array.make (Array.length classes) "" in
            if Array.length values > 0 then
              Parallel.for_ ~chunk_size:grain ~start:0
                ~finish:(Array.length values - 1) (fun element ->
                  if element land 16_383 = 0 then Cancel.check_opt cancel;
                  let class_id = classes.(element) in
                  if class_id >= 0 then values.(element) <- labels.(class_id));
            Attribute.Text values in
      Result.bind (Attribute.create_owned ~name ~owner:attribute_owner storage
        |> Result.map_error (Error.of_string ~operation:"connectivity"
          ~code:"invalid_attribute")) (fun attribute ->
        Geometry.with_attribute attribute geometry
        |> Result.map_error (Error.of_string ~operation:"connectivity"
          ~code:"invalid_geometry")))
  with
  | Cancel.Cancelled -> Error (Error.make ~operation:"connectivity"
      ~code:"cancelled" "connectivity attribute creation was cancelled")
  | Invalid_argument message -> Error (Error.make ~operation:"connectivity"
      ~code:"invalid_parameter" message)
