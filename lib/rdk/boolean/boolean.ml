type operation = Boolean_solid.operation =
  | Union | Intersection | Difference | Reverse_difference | Xor | Shatter
type treatment = Boolean_solid.treatment = Solid | Surface
type point_conflict = Boolean_payload.point_conflict = Reject | Promote_to_vertex
type seam_points = Shared_seam_points | Split_seam_points
type detriangulation = Boolean_materialization.detriangulation =
  | Triangles | All_polygons | Unchanged_polygons
type seam_output = Seam_curves | Coincident_patches

let error code message = Error (Error.make ~operation:"boolean" ~code message)

let bind result next = match result with
  | Ok value -> next value
  | Error _ as failure -> failure

let has_corner_payload geometry =
  Array.exists (fun attribute -> match Attribute.owner attribute with
    | Attribute.Point | Attribute.Vertex -> true
    | Attribute.Primitive | Attribute.Detail -> false)
    (Geometry.View.attributes geometry)
  || List.exists (fun group -> match Group.owner group with
    | Group.Point | Group.Vertex -> true
    | Group.Primitive -> false) (Geometry.groups geometry)
  || Geometry.edge_groups geometry <> []

let run ?cancel ?(grain = 16_384) ?(operation = Union)
    ?(left_treatment = Solid) ?(right_treatment = Solid)
    ?(resolve_left_self_intersections = false)
    ?(resolve_right_self_intersections = false)
    ?(point_conflict = Promote_to_vertex) ?(point_tolerance = 0.)
    ?(tiny_seam_threshold = 0.) ?(cleanup_max_batches = 8)
    ?(strict_cleanup = true)
    ?(seam_points = Shared_seam_points) ?(detriangulation = Triangles)
    ?(assume_flat = false) ?require_closed
    ?piece_attribute
    ?(left_piece_group = Some "boolean_left")
    ?(overlap_piece_group = Some "boolean_overlap")
    ?(right_piece_group = Some "boolean_right") ~right left =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else if not (Float.is_finite point_tolerance) || point_tolerance < 0. then
    error "invalid_parameter"
      "point_tolerance must be finite and non-negative"
  else if not (Float.is_finite tiny_seam_threshold)
      || tiny_seam_threshold < 0. then
    error "invalid_parameter"
      "tiny_seam_threshold must be finite and non-negative"
  else if cleanup_max_batches <= 0 then
    error "invalid_parameter" "cleanup_max_batches must be positive"
  else begin
    if (match piece_attribute with
        | Some name -> String.trim name = ""
        | None -> false) then
      error "invalid_parameter" "piece attribute name must not be empty"
    else let group_names = List.filter_map Fun.id
        [left_piece_group; overlap_piece_group; right_piece_group] in
    if operation = Shatter
        && List.exists (fun name -> String.trim name = "") group_names then
      error "invalid_parameter" "shatter group names must not be empty"
    else let ordered = List.sort String.compare group_names in
    let rec duplicate = function
      | first :: (second :: _ as rest) ->
          String.equal first second || duplicate rest
      | [] | [_] -> false in
    if operation = Shatter && duplicate ordered then
      error "invalid_parameter" "shatter group names must be distinct"
    else begin
    let left_kind = left_treatment
    and right_kind = right_treatment
    and corner_payload = has_corner_payload left || has_corner_payload right in
    let require_closed = match require_closed with
      | Some value -> value
      | None -> left_kind = Boolean_solid.Solid
          && right_kind = Boolean_solid.Solid in
    let allow_opposite_duplicates =
      (operation = Difference && left_kind = Boolean_solid.Solid
         && right_kind = Boolean_solid.Surface)
      || (operation = Reverse_difference && left_kind = Boolean_solid.Surface
          && right_kind = Boolean_solid.Solid)
      || (operation = Xor && left_kind <> right_kind)
      || operation = Shatter in
    if operation = Shatter && (left_kind <> Boolean_solid.Solid
        || right_kind <> Boolean_solid.Solid) then
      error "invalid_parameter" "shatter currently requires two solid operands"
    else bind (Boolean_solid.prepare ?cancel ~grain ~left ~right
        ~left_treatment:left_kind ~right_treatment:right_kind
        ~resolve_left_self_intersections ~resolve_right_self_intersections ())
      (fun prepared ->
        let extracted = if operation = Shatter then
            bind (Boolean_solid.shatter_with_ancestry ?cancel
                ~require_closed:false ~defer_rounded_slivers:true
                ~corner_payload prepared) (fun pieces ->
              bind (Boolean_extract.View.concatenate_ancestries ?cancel pieces)
                (fun ancestry ->
                  let counts = Array.map (fun piece -> Geometry.primitive_count
                      (Boolean_extract.geometry piece)) pieces in
                  Ok (ancestry, Some counts)))
          else bind (Boolean_solid.extract_product_with_ancestry ?cancel
              ~require_closed:false ~defer_rounded_slivers:true
              ~corner_payload
              ~operation prepared)
              (fun ancestry -> Ok (ancestry, None)) in
        bind extracted (fun (ancestry, shatter_counts) ->
            bind (Boolean_payload.copy_primitives ?cancel ~grain ancestry)
              (fun primitive_payload ->
                bind (if corner_payload then
                    Boolean_payload.copy_points_and_vertices ?cancel ~grain
                      ~point_conflict ~point_tolerance ancestry primitive_payload
                  else Ok primitive_payload)
                  (fun payload ->
                    let finalize source geometry =
                      let add_piece_attribute geometry = match piece_attribute with
                        | None -> Ok geometry
                        | Some name ->
                            let values = Boolean_extract.piece_ids
                                ~weiler:(Boolean_solid.View.weiler prepared)
                                ~ancestry ~source
                                ~primitive_count:(Geometry.primitive_count geometry) in
                            bind (Attribute.create_owned ~name
                                ~owner:Attribute.Primitive (Attribute.Int values)
                              |> Result.map_error (fun message ->
                                Error.make ~operation:"boolean"
                                  ~code:"invalid_output" message))
                              (fun attribute ->
                                Geometry.with_attribute attribute geometry
                                |> Result.map_error (fun message ->
                                  Error.make ~operation:"boolean"
                                    ~code:"invalid_output" message)) in
                      bind (add_piece_attribute geometry) (fun geometry ->
                      match shatter_counts with
                      | None -> Ok geometry
                      | Some counts ->
                          let first_overlap = counts.(0)
                          and first_right = counts.(0) + counts.(1) in
                          let add name predicate geometry = match name with
                            | None -> Ok geometry
                            | Some name ->
                                let group = Group.init ~grain
                                    ~owner:Group.Primitive ~name
                                    (Geometry.primitive_count geometry)
                                    (fun primitive -> predicate (source primitive)) in
                                Geometry.with_group group geometry
                                |> Result.map_error (fun message ->
                                  Error.make ~operation:"boolean"
                                    ~code:"invalid_output" message) in
                          bind (add left_piece_group
                              (fun value -> value < first_overlap) geometry)
                            (fun geometry ->
                              bind (add overlap_piece_group
                                  (fun value -> value >= first_overlap
                                    && value < first_right) geometry)
                                (fun geometry -> add right_piece_group
                                  (fun value -> value >= first_right) geometry))) in
                    let finish_cleanup cleanup =
                      let detriangulated = match detriangulation with
                        | Triangles -> Ok cleanup
                        | Unchanged_polygons | All_polygons ->
                            Boolean_materialization.detriangulate ?cancel
                              ~grain ~assume_flat
                              ~mode:detriangulation
                              ancestry cleanup in
                      bind detriangulated (fun cleanup ->
                        let split = match seam_points with
                          | Shared_seam_points -> Ok cleanup
                          | Split_seam_points ->
                              Boolean_materialization.split_seam_points
                                ?cancel ~grain cleanup in
                        bind split (fun cleanup ->
                          finalize
                            (Boolean_materialization.cleanup_primitive_source cleanup)
                            (Boolean_materialization.cleanup_geometry cleanup))) in
                    let cleanup () =
                      bind (Boolean_solid.seams ?cancel ~grain ~materialize:false
                          prepared) (fun seams ->
                        bind (Boolean_materialization.collapse_tiny_seams
                            ?cancel ~grain ~threshold:tiny_seam_threshold
                            ~require_closed ~allow_opposite_duplicates
                            ~max_batches:cleanup_max_batches
                            ~strict:strict_cleanup ancestry seams payload)
                          finish_cleanup) in
                    if tiny_seam_threshold = 0.
                        && detriangulation = Triangles
                        && seam_points = Shared_seam_points then
                      bind (Boolean_materialization.verify_unchanged_extraction
                          ?cancel ~grain ~require_closed ~allow_opposite_duplicates
                          ancestry payload) (fun accepted ->
                        if accepted then finalize Fun.id payload else cleanup ())
                    else cleanup ()))))
    end
  end

let seam ?cancel ?(grain = 16_384) ?(output = Seam_curves)
    ?(left_treatment = Solid) ?(right_treatment = Solid)
    ?(resolve_left_self_intersections = false)
    ?(resolve_right_self_intersections = false)
    ?(left_self_group = Some "boolean_left_self_seam")
    ?(between_group = Some "boolean_seam")
    ?(right_self_group = Some "boolean_right_self_seam")
    ?(coincident_group = Some "boolean_coincident") ~right left =
  if grain <= 0 then error "invalid_parameter" "grain must be positive"
  else
    let names = match output with
      | Seam_curves -> [left_self_group; between_group; right_self_group]
      | Coincident_patches -> [coincident_group] in
    let names = List.filter_map Fun.id names in
    if List.exists (fun name -> String.trim name = "") names then
      error "invalid_parameter" "Boolean seam group names must not be empty"
    else
      let names = List.sort String.compare names in
      let rec duplicate = function
        | first :: (second :: _ as rest) ->
            String.equal first second || duplicate rest
        | [] | [_] -> false in
      if duplicate names then
        error "invalid_parameter" "Boolean seam group names must be distinct"
      else
        bind (Boolean_solid.prepare ?cancel ~grain ~left ~right
            ~left_treatment ~right_treatment
            ~resolve_left_self_intersections
            ~resolve_right_self_intersections ()) (fun prepared ->
          bind (Boolean_solid.seams ?cancel ~grain prepared) (fun seams ->
            let geometry = match output with
              | Seam_curves -> Boolean_seam.curves seams
              | Coincident_patches -> Boolean_seam.coincident seams in
            let add_group name predicate geometry = match name with
              | None -> Ok geometry
              | Some name ->
                  let group = Group.init ~grain ~owner:Group.Primitive ~name
                      (Geometry.primitive_count geometry) predicate in
                  Geometry.with_group group geometry
                  |> Result.map_error (fun message -> Error.make
                    ~operation:"boolean_seam" ~code:"invalid_output" message) in
            match output with
            | Coincident_patches ->
                add_group coincident_group (fun _ -> true) geometry
            | Seam_curves ->
                let kind curve = Boolean_seam.curve_kind seams curve in
                bind (add_group left_self_group
                    (fun curve -> kind curve = Boolean_seam.Left_self)
                    geometry) (fun geometry ->
                  bind (add_group between_group
                      (fun curve -> kind curve = Boolean_seam.Between)
                      geometry) (fun geometry ->
                    add_group right_self_group
                      (fun curve -> kind curve = Boolean_seam.Right_self)
                      geometry))))
