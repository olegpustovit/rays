module E = Flow.Eval
module G = Rdk.Geometry
module Group = Rdk.Group
module Edge = Rdk.Edge_group

type owner = Ordinary of Group.owner | Edge

let error code message = Error (Diagnostic.error ~code message)

let owner = function
  | "point" -> Ok (Ordinary Group.Point)
  | "vertex" -> Ok (Ordinary Group.Vertex)
  | "primitive" -> Ok (Ordinary Group.Primitive)
  | "edge" -> Ok Edge
  | _ -> error "E_GROUP_OWNER" "Group owner is point, vertex, primitive or edge."

let count geometry = function
  | Ordinary Point -> G.point_count geometry
  | Ordinary Vertex -> G.vertex_count geometry
  | Ordinary Primitive -> G.primitive_count geometry
  | Edge -> Rdk.Topology_index.edge_count (Rdk.Topology_index.create (G.topology geometry))

let name name =
  if String.trim name = "" then error "E_GROUP_NAME" "Group name must not be blank." else Ok ()

let missing name = error "E_GROUP_MISSING" ("Missing group " ^ name ^ ".")

let mask group =
  let length = Group.length group and bits = Group.View.bits_view group in
  let values = Array.make length 0 in
  for byte = 0 to Bytes.length bits - 1 do
    let value = Char.code (Bytes.unsafe_get bits byte) and first = byte lsl 3 in
    let count = min 8 (length - first) in
    if value = 255 then Array.fill values first count 1
    else if value <> 0 then
      for bit = 0 to count - 1 do
        values.(first + bit) <- (value lsr bit) land 1
      done
  done;
  E.Int_array values

let read ~owner ~name geometry =
  match owner with
  | Ordinary owner -> (
      match G.find_group ~owner name geometry with
      | None -> missing name
      | Some group -> Ok (mask group))
  | Edge -> (
      match G.find_edge_group name geometry with
      | None -> missing name
      | Some group ->
          Ok
            (E.Int_array
               (Array.init (Edge.length group) (fun index -> if Edge.mem index group then 1 else 0)))
      )

let read_points ~owner ~name geometry =
  let selection =
    match owner with
    | Ordinary owner -> (
        match G.find_group ~owner name geometry with
        | None -> missing name
        | Some group ->
            Ok
              (match owner with
              | Point -> Rdk_core.Element_selection.Selected_points group
              | Vertex -> Selected_vertices group
              | Primitive -> Selected_primitives group))
    | Edge -> (
        match G.find_edge_group name geometry with
        | None -> missing name
        | Some group -> Ok (Rdk_core.Element_selection.Selected_edges group))
  in
  Result.bind selection (fun selection ->
      Result.map mask
        (Result.map_error
           (Diagnostic.error ~code:"E_GROUP_SOURCE")
           (Rdk_core.Element_selection.promote ~grain:4096 ~destination:Group.Point selection
              (G.topology geometry))))

let order ~owner ~name geometry =
  match owner with
  | Edge -> (
      match G.find_edge_group name geometry with
      | None -> missing name
      | Some _ -> Ok (E.Record [ ("ordered", E.Bool false); ("indices", E.Int_array [||]) ]))
  | Ordinary owner -> (
      match G.find_group ~owner name geometry with
      | None -> missing name
      | Some group ->
          Ok
            (E.Record
               [
                 ("ordered", E.Bool (Group.is_ordered group));
                 ("indices", E.Int_array (Option.value ~default:[||] (Group.ordered_elements group)));
               ]))

let names ~owner ~pattern geometry =
  match Rdk.Attribute_pattern.compile pattern with
  | Error message -> error "E_GROUP_PATTERN" message
  | Ok pattern ->
      let names =
        match owner with
        | Ordinary owner ->
            G.groups geometry
            |> List.filter_map (fun group ->
                if Group.owner group = owner then Some (Group.name group) else None)
        | Edge -> List.map Edge.name (G.edge_groups geometry)
      in
      Ok (E.Text_array (Array.of_list (List.filter (Rdk.Attribute_pattern.matches pattern) names)))

let entries ~selected_owner ~pattern ~component geometry =
  Result.bind (Attribute_metadata.group_owner selected_owner) (fun selected ->
      Result.bind
        (Result.map_error
           (Diagnostic.error ~code:"E_GROUP_PATTERN")
           (Rdk.Attribute_pattern.compile pattern))
        (fun pattern ->
          let visit f =
            List.iter
              (fun group ->
                let owner =
                  match Group.owner group with
                  | Group.Point -> Rdk.Group_ops.Group_points
                  | Vertex -> Group_vertices
                  | Primitive -> Group_primitives
                in
                f owner (Group.name group))
              (G.groups geometry);
            List.iter
              (fun group -> f Rdk.Group_ops.Group_edges (Edge.name group))
              (G.edge_groups geometry)
          in
          let matches owner name =
            (selected = None || selected = Some owner) && Rdk.Attribute_pattern.matches pattern name
          in
          let count = ref 0 in
          visit (fun owner name -> if matches owner name then incr count);
          let values = Array.make !count "" and index = ref 0 in
          visit (fun owner name ->
              if matches owner name then begin
                values.(!index) <-
                  (if component = "names" then name
                   else
                     match owner with
                     | Rdk.Group_ops.Group_points -> "point"
                     | Group_vertices -> "vertex"
                     | Group_primitives -> "primitive"
                     | Group_edges -> "edge");
                incr index
              end);
          Ok (E.Text_array values)))

let write ?order ~owner ~name:group_name mask geometry =
  Result.bind (name group_name) (fun () ->
      if Array.length mask <> count geometry owner then
        error "E_GROUP_COUNT" "Group mask must match its owner element count."
      else if Array.exists (fun value -> value <> 0 && value <> 1) mask then
        error "E_GROUP_MASK" "Group mask values must be 0 or 1."
      else
        let map_error result =
          Result.map_error (fun message -> Diagnostic.error ~code:"E_GROUP_ORDER" message) result
        in
        match (owner, order) with
        | Edge, Some _ ->
            error "E_GROUP_OWNER" "Edge groups do not have an explicit traversal order."
        | Edge, None ->
            let topology = G.topology geometry in
            let index = Rdk.Topology_index.create topology in
            map_error
              (G.with_edge_group
                 (Edge.init ~topology ~index ~name:group_name (fun index -> mask.(index) = 1))
                 geometry)
        | Ordinary owner, None ->
            map_error
              (G.with_group
                 (Group.init ~owner ~name:group_name (Array.length mask) (fun index ->
                      mask.(index) = 1))
                 geometry)
        | Ordinary owner, Some order ->
            Result.bind
              (map_error (Group.ordered ~owner ~name:group_name ~length:(Array.length mask) order))
              (fun group ->
                if
                  Group.cardinality group <> Array.fold_left ( + ) 0 mask
                  || not (Array.for_all (fun index -> mask.(index) = 1) order)
                then
                  error "E_GROUP_ORDER"
                    "Explicit order must contain each selected mask index exactly once."
                else map_error (G.with_group group geometry)))

let resolve ~geometry = function
  | E.Source
      ((("text/glob_rewrite" | "text/glob_rewrite_set" | "text/glob_literal") as operation), _, args)
    ->
      let text key =
        match List.assoc_opt key args with
        | Some (E.Text text) -> Ok text
        | _ -> error "E_METADATA_TEXT" "Metadata names and patterns must be text."
      in
      Result.bind (text "text") (fun value ->
          if operation = "text/glob_literal" then
            Result.map (fun value -> E.Text value) (Attribute_metadata.glob_literal value)
          else
            Result.bind (text "pattern") (fun pattern ->
                Result.bind (text "replacement") (fun replacement ->
                    Result.map
                      (fun value -> E.Text value)
                      ((if operation = "text/glob_rewrite_set" then
                          Attribute_metadata.glob_rewrite_set
                        else Attribute_metadata.glob_rewrite)
                         ~pattern ~replacement value))))
  | E.Source ("text/glob_rewrite_set_matches", _, args) ->
      let text key =
        match List.assoc_opt key args with
        | Some (E.Text text) -> Ok text
        | _ -> error "E_METADATA_TEXT" "Metadata names and patterns must be text."
      in
      Result.bind (text "text") (fun value ->
          Result.bind (text "pattern") (fun pattern ->
              Result.bind (text "replacement") (fun replacement ->
                  Result.map
                    (fun matched -> E.Bool matched)
                    (Attribute_metadata.glob_rewrite_set_matches ~pattern ~replacement value))))
  | E.Source ((("text/group_name_valid" | "text/group_name_forced") as operation), _, args) -> (
      match List.assoc_opt "text" args with
      | Some (E.Text value) ->
          Ok
            (if operation = "text/group_name_valid" then
               E.Bool (Rdk.Group_ops.valid_group_name value)
             else E.Text (Rdk.Group_ops.force_valid_group_name value))
      | _ -> error "E_METADATA_TEXT" "Metadata names and patterns must be text.")
  | E.Source ("sop/group_entries", _, args) ->
      let text key default =
        match List.assoc_opt key args with
        | None -> Ok default
        | Some (E.Text text) -> Ok text
        | _ -> error "E_GROUP_TYPE" ("Group " ^ key ^ " must be text.")
      in
      Result.bind (text "component" "") (fun component ->
          if not (List.mem component [ "owners"; "names" ]) then
            error "E_GROUP_TYPE" "Group entry component is owners or names."
          else
            Result.bind (text "owner" "any") (fun selected_owner ->
                Result.bind (text "pattern" "") (fun pattern ->
                    match List.assoc_opt "geometry" args with
                    | Some (E.Deferred (ty, id)) when Flow.Ty.is_geometry ty -> (
                        match geometry id with
                        | None -> error "E_DATA_SOURCE" "Group source has not been cooked."
                        | Some geometry -> entries ~selected_owner ~pattern ~component geometry)
                    | _ -> error "E_DATA_SOURCE" "Group source requires cooked geometry.")))
  | E.Source ((("sop/group" | "sop/group_names" | "sop/group_order") as operation), _, args) ->
      let text key default =
        match List.assoc_opt key args with
        | None -> Ok default
        | Some (E.Text value) -> Ok value
        | _ -> error "E_GROUP_TYPE" ("Group " ^ key ^ " must be text.")
      in
      Result.bind (text "owner" "point") (fun owner_name ->
          Result.bind (owner owner_name) (fun owner ->
              match List.assoc_opt "geometry" args with
              | Some (E.Deferred (ty, id)) when Flow.Ty.is_geometry ty -> (
                  match geometry id with
                  | None -> error "E_DATA_SOURCE" "Group source has not been cooked."
                  | Some geometry ->
                      if operation = "sop/group_names" then
                        Result.bind (text "pattern" "") (fun pattern ->
                            names ~owner ~pattern geometry)
                      else
                        Result.bind (text "name" "") (fun group_name ->
                            Result.bind (name group_name) (fun () ->
                                if operation = "sop/group" then
                                  match List.assoc_opt "as_owner" args with
                                  | None -> read ~owner ~name:group_name geometry
                                  | Some (E.Text "point") ->
                                      read_points ~owner ~name:group_name geometry
                                  | Some (E.Text _) ->
                                      error "E_GROUP_TARGET" "Group promotion target is point."
                                  | Some _ -> error "E_GROUP_TYPE" "Group as_owner must be text."
                                else
                                  Result.bind (order ~owner ~name:group_name geometry) (function
                                    | E.Record fields -> (
                                        match List.assoc_opt "component" args with
                                        | Some (E.Text component) -> (
                                            match List.assoc_opt component fields with
                                            | Some value -> Ok value
                                            | None ->
                                                error "E_GROUP_TYPE"
                                                  "Group order component is ordered or indices.")
                                        | _ ->
                                            error "E_GROUP_TYPE"
                                              "Group order component is ordered or indices.")
                                    | _ -> assert false))))
              | _ -> error "E_DATA_SOURCE" "Group source requires cooked geometry."))
  | E.Source (name, _, _) -> error "E_DATA_SOURCE" ("Unknown group data source " ^ name ^ ".")
  | _ -> error "E_DATA_SOURCE" "Unknown group data source."
