
type owner =
  | Group_points
  | Group_vertices
  | Group_primitives
  | Group_edges

type promote_mode =
  | Include_any
  | Include_all
  | Include_shared_edge

type boundary_attribute = {
  boundary_attribute_owner : Attribute.owner;
  boundary_attribute_pattern : string;
}

type promote_boundary_options = {
  promote_boundary_attributes : boundary_attribute list;
  promote_boundary_tolerance : float;
  promote_include_unshared_edges : bool;
  promote_all_unshared_edges : bool;
  promote_all_boundary_prims : bool;
}

and promote_operation =
  | Promote_elements of promote_mode
  | Promote_boundary of promote_boundary_options

and promotion_rule = {
  promotion_source : owner;
  promotion_destination : owner;
  promotion_pattern : string;
  promotion_new_name : string option;
  promotion_keep_original : bool;
  promotion_output_as_attribute : bool;
  promotion_operation : promote_operation;
}

type boolean_operation =
  | Group_replace
  | Group_union
  | Group_intersection
  | Group_subtract
  | Group_xor

type range_collision = {
  collision_owner : owner;
  collision_pattern : string;
  keep_boundary : bool;
}

type range_connectivity =
  | Range_disconnected of { region : int option }
  | Range_connected of {
      connectivity_attributes : string option;
      connectivity_tolerance : float;
      collision : range_collision option;
      region : int option;
      remove_other_regions : bool;
    }

type rename_conflict =
  | Rename_skip
  | Rename_error
  | Rename_overwrite
  | Rename_union

type rename_rule = {
  rename_owner : owner option;
  rename_pattern : string;
  rename_replacement : string;
  rename_conflict : rename_conflict;
}

type delete_rule = {
  delete_owner : owner option;
  delete_pattern : string;
}

type copy_conflict = Copy_skip | Copy_overwrite | Copy_add_suffix

type transfer_rule = {
  transfer_owner : owner;
  transfer_pattern : string;
  transfer_prefix : string;
}

type bounds =
  | Bounds_box of { minimum : Rays_math.Vec3.t; maximum : Rays_math.Vec3.t }
  | Bounds_sphere of { center : Rays_math.Vec3.t; radius : float }

type selection =
  | Ordinary of Group.t
  | Native_edges of Edge_group.t

let owner_name = function
  | Group_points -> "point"
  | Group_vertices -> "vertex"
  | Group_primitives -> "primitive"
  | Group_edges -> "edge"

let ordinary_owner = function
  | Group_points -> Some Group.Point
  | Group_vertices -> Some Group.Vertex
  | Group_primitives -> Some Group.Primitive
  | Group_edges -> None

let selection_mem selection index = match selection with
  | Ordinary group -> Group.mem index group
  | Native_edges group -> Edge_group.mem index group

let selection_cardinality = function
  | Ordinary group -> Group.cardinality group
  | Native_edges group -> Edge_group.cardinality group

(* Named-group metadata and boolean operations intentionally share one mutable
   table local to a cook. Public geometry and group values remain immutable;
   this prevents a many-rule node from rebuilding and rescanning Geometry.t
   after every metadata edit. *)

type named_entry = {
  owner : owner;
  mutable entry_name : string;
  mutable value : selection;
  mutable alive : bool;
}

type group_store = {
  table : ((owner * string), named_entry) Hashtbl.t;
  initial : named_entry array;
  mutable added_rev : named_entry list;
  mutable dirty : bool;
}

let owner_of_group = function
  | Group.Point -> Group_points
  | Group.Vertex -> Group_vertices
  | Group.Primitive -> Group_primitives

let selection_with_name name = function
  | Ordinary group -> Ordinary (Group.with_name name group)
  | Native_edges group -> Native_edges (Edge_group.with_name name group)

let store_of_geometry geometry =
  let ordinary = Geometry.groups geometry and edges = Geometry.edge_groups geometry in
  let entries_rev = ref [] in
  List.iter (fun group -> entries_rev := {
      owner = owner_of_group (Group.owner group); entry_name = Group.name group;
      value = Ordinary group; alive = true } :: !entries_rev) ordinary;
  List.iter (fun group -> entries_rev := {
      owner = Group_edges; entry_name = Edge_group.name group;
      value = Native_edges group; alive = true } :: !entries_rev) edges;
  let initial = Array.of_list (List.rev !entries_rev) in
  let table = Hashtbl.create (max 16 (Array.length initial * 2)) in
  Array.iter (fun entry ->
    Hashtbl.add table (entry.owner, entry.entry_name) entry) initial;
  { table; initial; added_rev = []; dirty = false }

let store_entries store =
  Array.to_list store.initial @ List.rev store.added_rev

let store_find store owner name =
  match Hashtbl.find_opt store.table (owner, name) with
  | Some entry when entry.alive -> Some entry
  | None | Some _ -> None

let store_remove store entry =
  if entry.alive then begin
    Hashtbl.remove store.table (entry.owner, entry.entry_name);
    entry.alive <- false;
    store.dirty <- true
  end

let store_replace store entry value =
  entry.value <- selection_with_name entry.entry_name value;
  store.dirty <- true

let store_add store owner name value =
  if Hashtbl.mem store.table (owner, name) then
    invalid_arg "Group operation: duplicate local group-store key";
  let entry = { owner; entry_name = name;
    value = selection_with_name name value; alive = true } in
  Hashtbl.add store.table (owner, name) entry;
  store.added_rev <- entry :: store.added_rev;
  store.dirty <- true;
  entry

let store_rename store entry name value =
  Hashtbl.remove store.table (entry.owner, entry.entry_name);
  entry.entry_name <- name;
  entry.value <- selection_with_name name value;
  Hashtbl.replace store.table (entry.owner, name) entry;
  store.dirty <- true

let store_commit store geometry =
  if not store.dirty then Ok geometry
  else begin
    let groups_rev = ref [] and edges_rev = ref [] in
    List.iter (fun entry -> if entry.alive then match entry.value with
      | Ordinary group -> groups_rev := group :: !groups_rev
      | Native_edges group -> edges_rev := group :: !edges_rev)
      (store_entries store);
    Geometry.create ~positions:(Geometry.positions geometry)
      ~topology:(Geometry.topology geometry)
      ~attributes:(Geometry.attributes geometry)
      ~groups:(List.rev !groups_rev) ~edge_groups:(List.rev !edges_rev) ()
  end

let valid_group_name name =
  let length = String.length name in
  let ascii_letter byte =
    (byte >= Char.code 'a' && byte <= Char.code 'z')
    || (byte >= Char.code 'A' && byte <= Char.code 'Z') in
  let ascii_digit byte = byte >= Char.code '0' && byte <= Char.code '9' in
  length > 0 && not (ascii_digit (Char.code (String.unsafe_get name 0)))
  && begin
    let index = ref 0 and valid = ref true in
    while !valid && !index < length do
      let byte = Char.code (String.unsafe_get name !index) in
      valid := ascii_letter byte || ascii_digit byte || byte = Char.code '_';
      incr index
    done;
    !valid
  end

let force_valid_group_name name =
  let length = String.length name in
  let ascii_letter byte =
    (byte >= Char.code 'a' && byte <= Char.code 'z')
    || (byte >= Char.code 'A' && byte <= Char.code 'Z') in
  let ascii_digit byte = byte >= Char.code '0' && byte <= Char.code '9' in
  let prefix_digit = length > 0
      && ascii_digit (Char.code (String.unsafe_get name 0)) in
  let output = Bytes.make (length + if prefix_digit then 1 else 0) '_' in
  let offset = if prefix_digit then 1 else 0 in
  for index = 0 to length - 1 do
    let byte = Char.code (String.unsafe_get name index) in
    if ascii_letter byte || ascii_digit byte || byte = Char.code '_' then
      Bytes.unsafe_set output (offset + index) (Char.chr byte)
  done;
  Bytes.unsafe_to_string output

let selection_boolean operation left right = match left, right with
  | Ordinary left, Ordinary right ->
      let result = match operation with
        | Group_replace -> Ok right
        | Group_union -> Group.union left right
        | Group_intersection -> Group.intersection left right
        | Group_subtract -> Group.difference left right
        | Group_xor -> Group.symmetric_difference left right in
      Result.map (fun group -> Ordinary group) result
  | Native_edges left, Native_edges right ->
      let result = match operation with
        | Group_replace -> Ok right
        | Group_union -> Edge_group.union left right
        | Group_intersection -> Edge_group.intersection left right
        | Group_subtract -> Edge_group.difference left right
        | Group_xor -> Edge_group.symmetric_difference left right in
      Result.map (fun group -> Native_edges group) result
  | Ordinary _, Native_edges _ | Native_edges _, Ordinary _ ->
      Error "Group operation: cannot combine ordinary and native-edge groups"

let owner_selected selected owner = match selected with
  | None -> true | Some expected -> expected = owner

let rename_entry store ~conflict entry new_name value =
  if String.equal entry.entry_name new_name then begin
    store_replace store entry value;
    Ok ()
  end else match store_find store entry.owner new_name with
    | None -> store_rename store entry new_name value; Ok ()
    | Some destination when destination == entry ->
        store_replace store entry value; Ok ()
    | Some destination ->
        (match conflict with
         | Rename_skip -> Ok ()
         | Rename_error -> Error (Printf.sprintf
             "Group Rename: destination %s group %S already exists"
             (owner_name entry.owner) new_name)
         | Rename_overwrite ->
             store_remove store destination;
             store_rename store entry new_name value;
             Ok ()
         | Rename_union ->
             Result.map (fun combined ->
               store_replace store destination combined;
               store_remove store entry)
               (selection_boolean Group_union destination.value value))

let rename ~rules geometry =
  Error.guard ~operation:"group_rename" ~code:"invalid_group" @@ fun () ->
  let store = store_of_geometry geometry in
  let result = List.fold_left (fun result rule -> Result.bind result (fun () ->
    Result.bind (Attribute_pattern.compile_rewrite
      ~pattern:rule.rename_pattern ~replacement:rule.rename_replacement)
      (fun rewrite ->
        let snapshot = store_entries store in
        List.fold_left (fun result entry -> Result.bind result (fun () ->
          if not entry.alive || not (owner_selected rule.rename_owner entry.owner)
          then Ok ()
          else match Attribute_pattern.rewrite rewrite entry.entry_name with
            | None -> Ok ()
            | Some new_name when String.trim new_name = "" ->
                Error "Group Rename: rewrite produced an empty group name"
            | Some new_name -> rename_entry store
                ~conflict:rule.rename_conflict entry new_name entry.value))
          (Ok ()) snapshot))) (Ok ()) rules in
  Result.bind result (fun () -> store_commit store geometry)

type delete_params = {
  rules : delete_rule list;
  delete_unused : bool;
}

let delete ({ rules; delete_unused } : delete_params) geometry =
  Error.guard ~operation:"group_delete" ~code:"invalid_group" @@ fun () ->
  let store = store_of_geometry geometry in
  let compiled = List.fold_left (fun result rule -> Result.bind result
      (fun rules ->
        if String.trim rule.delete_pattern = "" then Ok rules
        else Result.map (fun pattern -> (rule.delete_owner, pattern) :: rules)
            (Attribute_pattern.compile rule.delete_pattern))) (Ok []) rules in
  Result.bind compiled (fun compiled ->
    List.iter (fun entry -> if entry.alive then begin
      let matched = List.exists (fun (owner, pattern) ->
        owner_selected owner entry.owner
        && Attribute_pattern.matches pattern entry.entry_name) compiled in
      let empty = delete_unused && selection_cardinality entry.value = 0 in
      if matched || empty then store_remove store entry
    end) (store_entries store);
    store_commit store geometry)

type element_map =
  | Identity of int
  | Explicit of int array
  | Proximity of { indices : int array; distances_squared : float array }

let mapped_source mapping target = match mapping with
  | Identity source_count -> if target < source_count then target else -1
  | Explicit values -> values.(target)
  | Proximity values -> values.indices.(target)

let target_owner_count ?cancel owner geometry = match owner with
  | Group_points -> Geometry.point_count geometry
  | Group_vertices -> Geometry.vertex_count geometry
  | Group_primitives -> Geometry.primitive_count geometry
  | Group_edges -> Topology_index.edge_count
      (Topology_index.create ?cancel (Geometry.topology geometry))

let copied_selection ?cancel ~grain owner mapping source_group target =
  let count = target_owner_count ?cancel owner target in
  let member element =
    let source = mapped_source mapping element in
    source >= 0 && selection_mem source_group source in
  match ordinary_owner owner with
  | Some group_owner ->
      let target_group = Group.init ~grain ~owner:group_owner
          ~name:"__copied" count (fun element ->
            Cancel.poll cancel element 16_383;
            member element) in
      let target_group = match source_group with
        | Native_edges _ -> target_group
        | Ordinary source_group when not (Group.is_ordered source_group) ->
            target_group
        | Ordinary source_group ->
            begin match mapping with
            | Explicit source_of_target ->
                Group.View.remap_order ~source:source_group ~source_of_target
                  target_group
            | Identity source_count ->
                let source_of_target = Array.init count (fun target ->
                    if target < source_count then target else -1) in
                Group.View.remap_order ~source:source_group ~source_of_target
                  target_group
            | Proximity { indices; distances_squared } ->
                let source_order = Option.get
                    (Group.View.order_view source_group) in
                let rank = Array.make (Group.length source_group) (-1) in
                Array.iteri (fun position element -> rank.(element) <- position)
                  source_order;
                let order = Array.make (Group.cardinality target_group) 0
                and next = ref 0 in
                Group.iter (fun target_element ->
                  order.(!next) <- target_element;
                  incr next) target_group;
                Array.sort (fun left right ->
                  let left_source = indices.(left)
                  and right_source = indices.(right) in
                  let by_source = Int.compare rank.(left_source) rank.(right_source) in
                  if by_source <> 0 then by_source
                  else
                    let by_distance = Float.compare distances_squared.(left)
                        distances_squared.(right) in
                    if by_distance <> 0 then by_distance
                    else Int.compare left right) order;
                Group.View.with_owned_order order target_group
            end in
      Ordinary target_group
  | None ->
      let topology = Geometry.topology target in
      let index = Topology_index.create ?cancel topology in
      Native_edges (Edge_group.init ~grain ~topology ~index ~name:"__copied"
        (fun edge ->
          Cancel.poll cancel edge 16_383;
          member edge))

let unique_suffix store owner base =
  let suffix = ref 2 and candidate = ref (base ^ "2") in
  while store_find store owner !candidate <> None do
    if !suffix = max_int then invalid_arg "Group Copy: suffix space exhausted";
    incr suffix;
    candidate := base ^ string_of_int !suffix
  done;
  !candidate

let point_proximity_map ?cancel ~grain ~maximum_squared source target =
  Result.bind (Spatial_index.create ?cancel ~grain (Geometry.positions source)
      |> Result.map_error Error.to_string) (fun index ->
    let count = Geometry.point_count target in
    let indices = Array.make count (-1)
    and distances = Array.make count Float.infinity
    and counts = Array.make count 0 in
    Spatial_index.View.nearest_k_many_into ?cancel ~grain index
      ~queries:(Geometry.positions target) ~max_distance_squared:maximum_squared
      ~capacity:1 ~indices ~distances_squared:distances ~counts;
    Ok (Proximity { indices; distances_squared = distances }))

let feature_proximity_map ?cancel ~grain ~distance ~source_features
    ~target_features () =
  Result.bind (source_features ()) (fun source_features ->
    let source_index = Proximity_index.create ?cancel ~grain source_features in
    Result.bind (target_features ()) (fun target_features ->
      Result.map (fun (indices, distances_squared) ->
        Proximity { indices; distances_squared })
        (Proximity_index.nearest_entities_with_distances ?cancel ~grain
          ~max_distance:distance source_index target_features)))

let transfer_mapping ?cancel ~grain ~distance ~maximum_squared owner source target =
  match owner with
  | Group_points -> point_proximity_map ?cancel ~grain ~maximum_squared
      source target
  | Group_primitives -> feature_proximity_map ?cancel ~grain ~distance
      ~source_features:(fun () ->
        Proximity_index.primitive_features ?cancel ~grain source)
      ~target_features:(fun () ->
        Proximity_index.primitive_features ?cancel ~grain target) ()
  | Group_edges -> feature_proximity_map ?cancel ~grain ~distance
      ~source_features:(fun () ->
        Proximity_index.edge_features ?cancel ~grain source)
      ~target_features:(fun () ->
        Proximity_index.edge_features ?cancel ~grain target) ()
  | Group_vertices -> Error "Group Transfer: vertex groups are not supported"

type transfer_params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  rules : transfer_rule list;
  conflict : copy_conflict;
  create_empty : bool;
  distance : float;
  source : Rdk_core.Geometry.t;
  target : Rdk_core.Geometry.t;
}

let transfer ({ cancel; grain; rules; conflict; create_empty; distance; source; target } : transfer_params) =
  let grain = Option.value ~default:16_384 grain in
  Error.guard ~operation:"group_transfer" ~code:"invalid_group" @@ fun () ->
  if grain <= 0 then Error "Group Transfer: grain must be positive"
  else if not (Float.is_finite distance) || distance < 0.
      || distance > sqrt max_float then
    Error "Group Transfer: distance threshold must be finite, non-negative, and safely squarable"
  else
    try
      let maximum_squared = distance *. distance in
      let source_store = store_of_geometry source
      and target_store = store_of_geometry target in
      let point_mapping = ref None and primitive_mapping = ref None
      and edge_mapping = ref None in
      let mapping_for owner =
        let slot = match owner with
          | Group_points -> point_mapping
          | Group_primitives -> primitive_mapping
          | Group_edges -> edge_mapping
          | Group_vertices -> invalid_arg
              "Group Transfer: vertex groups are not supported" in
        match !slot with
        | Some result -> result
        | None ->
            let result = transfer_mapping ?cancel ~grain ~distance
                ~maximum_squared owner source target in
            slot := Some result;
            result in
      let transfer_source rule mapping source_entry =
        let proposed = rule.transfer_prefix ^ source_entry.entry_name in
        if String.trim proposed = "" then
          Error "Group Transfer: prefix produced an empty group name"
        else
          let existing = store_find target_store rule.transfer_owner proposed in
          match conflict, existing with
          | Copy_skip, Some _ -> Ok ()
          | _ ->
              let output_name = match conflict, existing with
                | Copy_add_suffix, Some _ ->
                    unique_suffix target_store rule.transfer_owner proposed
                | _ -> proposed in
              let copied = copied_selection ?cancel ~grain rule.transfer_owner
                  mapping source_entry.value target in
              if not create_empty && selection_cardinality copied = 0 then Ok ()
              else begin
                match store_find target_store rule.transfer_owner output_name with
                | Some destination ->
                    store_replace target_store destination copied; Ok ()
                | None ->
                    ignore (store_add target_store rule.transfer_owner output_name
                      copied);
                    Ok ()
              end in
      let transfer_rule rule =
        Cancel.check_opt cancel;
        if rule.transfer_owner = Group_vertices then
          Error "Group Transfer: vertex groups are not supported"
        else
          let pattern_source = if String.trim rule.transfer_pattern = ""
            then "*" else rule.transfer_pattern in
          Result.bind (Attribute_pattern.compile pattern_source) (fun pattern ->
            let sources = List.filter (fun entry -> entry.alive
                && entry.owner = rule.transfer_owner
                && Attribute_pattern.matches pattern entry.entry_name)
                (store_entries source_store) in
            if sources = [] then Ok ()
            else Result.bind (mapping_for rule.transfer_owner) (fun mapping ->
              List.fold_left (fun result source_entry ->
                Result.bind result (fun () ->
                  transfer_source rule mapping source_entry))
                (Ok ()) sources)) in
      let result = List.fold_left (fun result rule ->
        Result.bind result (fun () -> transfer_rule rule)) (Ok ()) rules in
      Result.bind result (fun () -> store_commit target_store target)
    with Invalid_argument message -> Error message
