type rename_conflict = Attribute_rename_skip | Attribute_rename_error | Attribute_rename_overwrite

type rename_rule = {
  rename_attribute_owner : Attribute.owner option;
  rename_attribute_pattern : string;
  rename_attribute_replacement : string;
  rename_attribute_conflict : rename_conflict;
}

let owner_index = function
  | Attribute.Point -> 0
  | Attribute.Vertex -> 1
  | Attribute.Primitive -> 2
  | Attribute.Detail -> 3

let owner_name = function
  | Attribute.Point -> "point"
  | Attribute.Vertex -> "vertex"
  | Attribute.Primitive -> "primitive"
  | Attribute.Detail -> "detail"

let check_cancel cancel index = Cancel.poll cancel index 1023

let compile_optional label = function
  | None -> Ok None
  | Some source when String.trim source = "" -> Ok None
  | Some source ->
      Result.map Option.some (Attribute_pattern.compile source)
      |> Result.map_error (fun message -> label ^ ": " ^ message)

let reference_names = function
  | None -> Array.init 4 (fun _ -> Hashtbl.create 0)
  | Some geometry ->
      let tables = Array.init 4 (fun _ -> Hashtbl.create 16) in
      Array.iter
        (fun attribute ->
          Hashtbl.replace
            tables.(owner_index (Attribute.owner attribute))
            (Attribute.name attribute) ())
        (Geometry.View.attributes geometry);
      tables

type delete_params = {
  cancel : Cancel.t option;
  reference : Geometry.t option;
  delete_non_selected : bool;
  point_pattern : string option;
  vertex_pattern : string option;
  primitive_pattern : string option;
  detail_pattern : string option;
}

let delete
    {
      cancel;
      reference;
      delete_non_selected;
      point_pattern;
      vertex_pattern;
      primitive_pattern;
      detail_pattern;
    } geometry =
  Result.bind (compile_optional "Attribute Delete point pattern" point_pattern) (fun point ->
      Result.bind (compile_optional "Attribute Delete vertex pattern" vertex_pattern) (fun vertex ->
          Result.bind (compile_optional "Attribute Delete primitive pattern" primitive_pattern)
            (fun primitive ->
              Result.bind (compile_optional "Attribute Delete detail pattern" detail_pattern)
                (fun detail ->
                  let patterns = [| point; vertex; primitive; detail |]
                  and reference_names = reference_names reference in
                  let reference_owner_nonempty =
                    Array.map (fun names -> Hashtbl.length names > 0) reference_names
                  in
                  let selected attribute =
                    let owner = owner_index (Attribute.owner attribute)
                    and name = Attribute.name attribute in
                    let from_reference = Hashtbl.mem reference_names.(owner) name in
                    match patterns.(owner) with
                    | None -> from_reference
                    | Some pattern when reference_owner_nonempty.(owner) ->
                        Attribute_pattern.apply ~selected:from_reference pattern name
                    | Some pattern -> Attribute_pattern.matches pattern name
                  in
                  let attributes = Geometry.View.attributes geometry in
                  if Array.length attributes = 0 then Ok geometry
                  else begin
                    let output = Array.make (Array.length attributes) attributes.(0) in
                    let output_count = ref 0 in
                    Array.iteri
                      (fun index attribute ->
                        check_cancel cancel index;
                        let remove =
                          if delete_non_selected then not (selected attribute)
                          else selected attribute
                        in
                        if not remove then begin
                          output.(!output_count) <- attribute;
                          incr output_count
                        end)
                      attributes;
                    if !output_count = Array.length attributes then Ok geometry
                    else
                      let output =
                        if !output_count = Array.length output then output
                        else Array.sub output 0 !output_count
                      in
                      Geometry.View.with_attributes_owned output geometry
                  end))))

type entry = { owner : Attribute.owner; mutable attribute : Attribute.t; mutable alive : bool }

type store = {
  entries : entry array;
  tables : (string, entry) Hashtbl.t array;
  mutable changed : bool;
}

let store_of_geometry geometry =
  let attributes = Geometry.View.attributes geometry in
  let entries =
    Array.map
      (fun attribute -> { owner = Attribute.owner attribute; attribute; alive = true })
      attributes
  in
  let tables = Array.init 4 (fun _ -> Hashtbl.create (max 16 (Array.length entries / 4))) in
  Array.iter
    (fun entry ->
      Hashtbl.add tables.(owner_index entry.owner) (Attribute.name entry.attribute) entry)
    entries;
  { entries; tables; changed = false }

let store_find store owner name = Hashtbl.find_opt store.tables.(owner_index owner) name

let store_remove store entry =
  if entry.alive then begin
    Hashtbl.remove store.tables.(owner_index entry.owner) (Attribute.name entry.attribute);
    entry.alive <- false;
    store.changed <- true
  end

let store_rename store entry name =
  let table = store.tables.(owner_index entry.owner) in
  Hashtbl.remove table (Attribute.name entry.attribute);
  entry.attribute <- Attribute.with_name name entry.attribute |> Result.get_ok;
  Hashtbl.replace table name entry;
  store.changed <- true

let rename_entry store ~conflict entry name =
  let old_name = Attribute.name entry.attribute in
  if String.equal old_name name then Ok ()
  else if String.trim name = "" then
    Error "Attribute Rename: rewrite produced an empty attribute name"
  else if entry.owner = Attribute.Point && String.equal name "P" then
    Error "Attribute Rename: canonical point position P is reserved"
  else
    match store_find store entry.owner name with
    | None ->
        store_rename store entry name;
        Ok ()
    | Some destination when destination == entry -> Ok ()
    | Some destination -> (
        match conflict with
        | Attribute_rename_skip -> Ok ()
        | Attribute_rename_error ->
            Error
              (Printf.sprintf "Attribute Rename: destination %s attribute %S already exists"
                 (owner_name entry.owner) name)
        | Attribute_rename_overwrite ->
            store_remove store destination;
            store_rename store entry name;
            Ok ())

let compile_rule rule =
  Result.map
    (fun rewrite -> (rule, rewrite))
    (Attribute_pattern.compile_rewrite ~pattern:rule.rename_attribute_pattern
       ~replacement:rule.rename_attribute_replacement)
  |> Result.map_error (fun message -> "Attribute Rename: " ^ message)

let rename ?cancel ~rules geometry =
  let compiled =
    List.fold_left
      (fun result rule ->
        Result.bind result (fun compiled ->
            Result.map (fun rule -> rule :: compiled) (compile_rule rule)))
      (Ok []) rules
    |> Result.map List.rev
  in
  Result.bind compiled (fun compiled ->
      let store = store_of_geometry geometry in
      let result =
        List.fold_left
          (fun result (rule, rewrite) ->
            Result.bind result (fun () ->
                let rule_result = ref (Ok ()) in
                Array.iteri
                  (fun index entry ->
                    match !rule_result with
                    | Error _ -> ()
                    | Ok () -> (
                        check_cancel cancel index;
                        if
                          entry.alive
                          &&
                          match rule.rename_attribute_owner with
                          | None -> true
                          | Some owner -> owner = entry.owner
                        then
                          match
                            Attribute_pattern.rewrite rewrite (Attribute.name entry.attribute)
                          with
                          | None -> ()
                          | Some name ->
                              rule_result :=
                                rename_entry store ~conflict:rule.rename_attribute_conflict entry
                                  name))
                  store.entries;
                !rule_result))
          (Ok ()) compiled
      in
      Result.bind result (fun () ->
          if not store.changed then Ok geometry
          else begin
            let count =
              Array.fold_left
                (fun count entry -> if entry.alive then count + 1 else count)
                0 store.entries
            in
            let output = Array.make count store.entries.(0).attribute in
            let next = ref 0 in
            Array.iter
              (fun entry ->
                if entry.alive then begin
                  output.(!next) <- entry.attribute;
                  incr next
                end)
              store.entries;
            Geometry.View.with_attributes_owned output geometry
          end))

type swap_method = Attribute_swap | Attribute_move | Attribute_copy

type swap_rule = {
  swap_attribute_owner : Attribute.owner;
  swap_attribute_source : string;
  swap_attribute_destination : string;
  swap_attribute_method : swap_method;
}

type swap_store = {
  swap_entries : entry Dynarray.t;
  swap_tables : (string, entry) Hashtbl.t array;
  mutable swap_positions : Packed.Float3.t;
  mutable swap_attributes_changed : bool;
  mutable swap_positions_changed : bool;
}

type swap_slot = Swap_position | Swap_attribute of entry

let swap_store_of_geometry geometry =
  let attributes = Geometry.View.attributes geometry in
  let entries =
    Dynarray.init (Array.length attributes) (fun index ->
        let attribute = attributes.(index) in
        { owner = Attribute.owner attribute; attribute; alive = true })
  in
  let tables = Array.init 4 (fun _ -> Hashtbl.create (max 16 (Array.length attributes / 4))) in
  Dynarray.iter
    (fun entry ->
      Hashtbl.add tables.(owner_index entry.owner) (Attribute.name entry.attribute) entry)
    entries;
  {
    swap_entries = entries;
    swap_tables = tables;
    swap_positions = Geometry.positions geometry;
    swap_attributes_changed = false;
    swap_positions_changed = false;
  }

let swap_find store owner name =
  if owner = Attribute.Point && String.equal name "P" then Some Swap_position
  else
    match Hashtbl.find_opt store.swap_tables.(owner_index owner) name with
    | Some entry when entry.alive -> Some (Swap_attribute entry)
    | Some _ | None -> None

let swap_remove store entry =
  if entry.alive then begin
    Hashtbl.remove store.swap_tables.(owner_index entry.owner) (Attribute.name entry.attribute);
    entry.alive <- false;
    store.swap_attributes_changed <- true
  end

let swap_set_position store attribute =
  match Attribute.View.storage attribute with
  | Attribute.Float3 positions ->
      if Packed.Float3.data_id positions <> Packed.Float3.data_id store.swap_positions then begin
        store.swap_positions <- positions;
        store.swap_positions_changed <- true
      end;
      Ok ()
  | Attribute.Float _ | Attribute.Int _ | Attribute.Int_array _ | Attribute.Float_array _
  | Attribute.Float2 _ | Attribute.Float4 _ | Attribute.Text _ ->
      Error
        (Printf.sprintf "Attribute Swap: point position P requires float3 storage, but %S is %s"
           (Attribute.name attribute) (Attribute.kind_name attribute))

let position_attribute store name =
  Attribute.create_owned ~name ~owner:Attribute.Point (Attribute.Float3 store.swap_positions)

let swap_replace_or_append store owner name attribute =
  Result.bind (Attribute.with_name name attribute) (fun replacement ->
      match swap_find store owner name with
      | Some Swap_position -> swap_set_position store replacement
      | Some (Swap_attribute destination) ->
          destination.attribute <- replacement;
          store.swap_attributes_changed <- true;
          Ok ()
      | None ->
          let entry = { owner; attribute = replacement; alive = true } in
          Dynarray.add_last store.swap_entries entry;
          Hashtbl.add store.swap_tables.(owner_index owner) name entry;
          store.swap_attributes_changed <- true;
          Ok ())

let swap_copy_slot store owner source destination =
  if String.equal source destination then Ok ()
  else
    match swap_find store owner source with
    | None -> Ok ()
    | Some Swap_position ->
        Result.bind (position_attribute store destination) (fun attribute ->
            swap_replace_or_append store owner destination attribute)
    | Some (Swap_attribute source) ->
        swap_replace_or_append store owner destination source.attribute

let swap_move_slot store owner source destination =
  if String.equal source destination then Ok ()
  else
    match swap_find store owner source with
    | None -> Ok ()
    | Some Swap_position ->
        (* Canonical P must always exist, so Houdini defines moving it as a
           copy. *)
        swap_copy_slot store owner source destination
    | Some (Swap_attribute source_entry) -> (
        match swap_find store owner destination with
        | Some Swap_position ->
            Result.bind (swap_set_position store source_entry.attribute) (fun () ->
                swap_remove store source_entry;
                Ok ())
        | Some (Swap_attribute destination_entry) ->
            swap_remove store destination_entry;
            Result.bind (Attribute.with_name destination source_entry.attribute) (fun attribute ->
                Hashtbl.remove store.swap_tables.(owner_index owner) source;
                source_entry.attribute <- attribute;
                Hashtbl.replace store.swap_tables.(owner_index owner) destination source_entry;
                store.swap_attributes_changed <- true;
                Ok ())
        | None ->
            Result.bind (Attribute.with_name destination source_entry.attribute) (fun attribute ->
                Hashtbl.remove store.swap_tables.(owner_index owner) source;
                source_entry.attribute <- attribute;
                Hashtbl.add store.swap_tables.(owner_index owner) destination source_entry;
                store.swap_attributes_changed <- true;
                Ok ()))

let swap_two_slots store owner source destination =
  if String.equal source destination then Ok ()
  else
    match (swap_find store owner source, swap_find store owner destination) with
    | None, None -> Ok ()
    | Some _, None -> swap_copy_slot store owner source destination
    | None, Some _ -> swap_copy_slot store owner destination source
    | Some Swap_position, Some Swap_position -> Ok ()
    | Some (Swap_attribute source_entry), Some (Swap_attribute destination_entry) ->
        Result.bind (Attribute.with_name source destination_entry.attribute)
          (fun source_attribute ->
            Result.bind (Attribute.with_name destination source_entry.attribute)
              (fun destination_attribute ->
                source_entry.attribute <- source_attribute;
                destination_entry.attribute <- destination_attribute;
                store.swap_attributes_changed <- true;
                Ok ()))
    | Some Swap_position, Some (Swap_attribute destination_entry) ->
        let old_positions = store.swap_positions in
        Result.bind (swap_set_position store destination_entry.attribute) (fun () ->
            Result.bind
              (Attribute.create_owned ~name:destination ~owner:Attribute.Point
                 (Attribute.Float3 old_positions)) (fun attribute ->
                destination_entry.attribute <- attribute;
                store.swap_attributes_changed <- true;
                Ok ()))
    | Some (Swap_attribute source_entry), Some Swap_position ->
        let old_positions = store.swap_positions in
        Result.bind (swap_set_position store source_entry.attribute) (fun () ->
            Result.bind
              (Attribute.create_owned ~name:source ~owner:Attribute.Point
                 (Attribute.Float3 old_positions)) (fun attribute ->
                source_entry.attribute <- attribute;
                store.swap_attributes_changed <- true;
                Ok ()))

let compile_swap_rule rule =
  Result.bind
    (Attribute_pattern.compile_rewrite ~pattern:rule.swap_attribute_source
       ~replacement:rule.swap_attribute_destination) (fun forward ->
      Result.map
        (fun backward -> (rule, forward, backward))
        (Attribute_pattern.compile_rewrite ~pattern:rule.swap_attribute_destination
           ~replacement:rule.swap_attribute_source))
  |> Result.map_error (fun message -> "Attribute Swap: " ^ message)

let swap ?cancel ~rules geometry =
  let compiled =
    List.fold_left
      (fun result rule ->
        Result.bind result (fun compiled ->
            Result.map (fun rule -> rule :: compiled) (compile_swap_rule rule)))
      (Ok []) rules
    |> Result.map List.rev
  in
  Result.bind compiled (fun compiled ->
      let store = swap_store_of_geometry geometry in
      let apply_pair rule source destination =
        match rule.swap_attribute_method with
        | Attribute_swap -> swap_two_slots store rule.swap_attribute_owner source destination
        | Attribute_move -> swap_move_slot store rule.swap_attribute_owner source destination
        | Attribute_copy -> swap_copy_slot store rule.swap_attribute_owner source destination
      in
      let apply_rule (rule, forward, backward) =
        let matches = Dynarray.create () in
        if rule.swap_attribute_owner = Attribute.Point then begin
          match Attribute_pattern.rewrite forward "P" with
          | Some destination -> Dynarray.add_last matches ("P", destination)
          | None -> ()
        end;
        let limit = Dynarray.length store.swap_entries in
        for index = 0 to limit - 1 do
          check_cancel cancel index;
          let entry = Dynarray.get store.swap_entries index in
          if entry.alive && entry.owner = rule.swap_attribute_owner then
            let source = Attribute.name entry.attribute in
            match Attribute_pattern.rewrite forward source with
            | Some destination -> Dynarray.add_last matches (source, destination)
            | None -> ()
        done;
        if not (Dynarray.is_empty matches) then
          Dynarray.fold_left
            (fun result (source, destination) ->
              Result.bind result (fun () ->
                  Cancel.check_opt cancel;
                  apply_pair rule source destination))
            (Ok ()) matches
        else if rule.swap_attribute_method <> Attribute_swap then Ok ()
        else begin
          (* For an exact or wildcard pair with a missing source, Swap behaves as
         Copy in the other direction. Scan the destination pattern only when
         the forward side matched nothing, avoiding double-processing pairs. *)
          let inverse = Dynarray.create () in
          if rule.swap_attribute_owner = Attribute.Point then begin
            match Attribute_pattern.rewrite backward "P" with
            | Some source -> Dynarray.add_last inverse ("P", source)
            | None -> ()
          end;
          let limit = Dynarray.length store.swap_entries in
          for index = 0 to limit - 1 do
            check_cancel cancel index;
            let entry = Dynarray.get store.swap_entries index in
            if entry.alive && entry.owner = rule.swap_attribute_owner then
              let destination = Attribute.name entry.attribute in
              match Attribute_pattern.rewrite backward destination with
              | Some source -> Dynarray.add_last inverse (destination, source)
              | None -> ()
          done;
          Dynarray.fold_left
            (fun result (destination, source) ->
              Result.bind result (fun () ->
                  Cancel.check_opt cancel;
                  if swap_find store rule.swap_attribute_owner source = None then
                    swap_copy_slot store rule.swap_attribute_owner destination source
                  else Ok ()))
            (Ok ()) inverse
        end
      in
      Result.bind
        (List.fold_left
           (fun result rule -> Result.bind result (fun () -> apply_rule rule))
           (Ok ()) compiled)
        (fun () ->
          if (not store.swap_attributes_changed) && not store.swap_positions_changed then
            Ok geometry
          else
            let attributes =
              Dynarray.to_seq store.swap_entries
              |> Seq.filter_map (fun entry -> if entry.alive then Some entry.attribute else None)
              |> Array.of_seq
            in
            if store.swap_positions_changed then
              Geometry.View.with_positions_and_attributes_owned store.swap_positions attributes
                geometry
            else Geometry.View.with_attributes_owned attributes geometry))
