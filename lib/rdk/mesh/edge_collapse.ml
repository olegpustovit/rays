open Fuse_reduce

let raw ?cancel ?(grain = 16_384) ?edges
    ?connectivity_attribute ?(position = Average_position)
    ?(remove_degenerate_primitives = true)
    ?(recompute_point_normals = true) geometry =
  if grain <= 0 then invalid_arg "Rdk.Edge_collapse.edge_collapse: grain must be positive";
  let topology_value = Geometry.topology geometry in
  let index_value = Topology_index.create ?cancel topology_value in
  let index = Topology_index.View.view index_value in
  let edge_count = Array.length index.edge_a
  and point_count = Geometry.point_count geometry in
  let selected edge = match edges with
    | None -> true
    | Some group -> Edge_group.mem edge group in
  let invalid_selection = match edges with
    | Some group when Edge_group.topology_data_id group
        <> Topology.data_id topology_value ->
        Some "edge selection belongs to a different topology"
    | Some group when Edge_group.length group <> edge_count ->
        Some "edge selection length does not match topology edge count"
    | None | Some _ -> None in
  match invalid_selection with
  | Some message -> Error ("Rdk.Edge_collapse.edge_collapse: " ^ message)
  | None ->
      let connectivity = match connectivity_attribute with
        | None -> Ok None
        | Some name when String.trim name = "" ->
            Error "Rdk.Edge_collapse.edge_collapse: connectivity attribute name must not be empty"
        | Some name ->
            (match Geometry.find_attribute ~owner:Attribute.Point name geometry with
             | None -> Error (Printf.sprintf
                 "Rdk.Edge_collapse.edge_collapse: point connectivity attribute %S is missing"
                 name)
             | Some attribute -> Ok (Some attribute)) in
      Result.bind connectivity (fun connectivity ->
        let same_ragged offsets values equal left right =
          let left_first = offsets.(left) and left_last = offsets.(left + 1)
          and right_first = offsets.(right) and right_last = offsets.(right + 1) in
          let length = left_last - left_first in
          if length <> right_last - right_first then false
          else begin
            let local = ref 0 and same = ref true in
            while !same && !local < length do
              same := equal values.(left_first + !local)
                  values.(right_first + !local);
              incr local
            done;
            !same
          end in
        let same_connectivity = match connectivity with
          | None -> fun _ _ -> true
          | Some attribute ->
              (match Attribute.View.storage attribute with
               | Attribute.Float values -> fun a b -> values.(a) = values.(b)
               | Attribute.Int values -> fun a b -> values.(a) = values.(b)
               | Attribute.Text values -> fun a b -> String.equal values.(a) values.(b)
               | Attribute.Float2 values ->
                   let values = Packed.Float2.View.view values in
                   fun a b -> values.x.(a) = values.x.(b)
                     && values.y.(a) = values.y.(b)
               | Attribute.Float3 values ->
                   let values = Packed.Float3.View.view values in
                   fun a b -> values.x.(a) = values.x.(b)
                     && values.y.(a) = values.y.(b)
                     && values.z.(a) = values.z.(b)
               | Attribute.Float4 values ->
                   let values = Packed.Float4.View.view values in
                   fun a b -> values.x.(a) = values.x.(b)
                     && values.y.(a) = values.y.(b)
                     && values.z.(a) = values.z.(b)
                     && values.w.(a) = values.w.(b)
               | Attribute.Int_array values ->
                   let values = Packed.Int_array.View.view values in
                   fun a b -> same_ragged values.offsets values.values ( = ) a b
               | Attribute.Float_array values ->
                   let values = Packed.Float_array.View.view values in
                   fun a b -> same_ragged values.offsets values.values ( = ) a b) in
        let parent = Array.init point_count Fun.id
        and rank = Bytes.make point_count '\000' in
        let rec find point =
          let ancestor = parent.(point) in
          if ancestor = point then point
          else begin
            let root = find ancestor in
            parent.(point) <- root;
            root
          end in
        let union left right =
          let left_root = find left and right_root = find right in
          if left_root = right_root then false
          else begin
            let left_rank = Char.code (Bytes.unsafe_get rank left_root)
            and right_rank = Char.code (Bytes.unsafe_get rank right_root) in
            if left_rank < right_rank then parent.(left_root) <- right_root
            else if right_rank < left_rank then parent.(right_root) <- left_root
            else begin
              parent.(right_root) <- left_root;
              Bytes.unsafe_set rank left_root (Char.chr (left_rank + 1))
            end;
            true
          end in
        let merged = ref 0 and selected_self_edge = ref false in
        for edge = 0 to edge_count - 1 do
          if edge land 4095 = 0 then Cancel.check_opt cancel;
          if selected edge then begin
            let a = index.edge_a.(edge) and b = index.edge_b.(edge) in
            if a = b then selected_self_edge := true
            else if same_connectivity a b then begin
              if union a b then incr merged
            end
          end
        done;
        if !merged = 0 then
          if !selected_self_edge && remove_degenerate_primitives then
            Fuse_cleanup.apply ?cancel ~grain ~remove_degenerate_primitives:true
              ~remove_unused_points_from_degenerate_primitives:true
              ~remove_all_unused_points:false geometry
          else Ok geometry
        else begin
          let root_to_cluster = Array.make point_count (-1)
          and of_point = Array.make point_count 0
          and cluster_count = ref 0 in
          for point = 0 to point_count - 1 do
            let root = find point in
            if root_to_cluster.(root) < 0 then begin
              root_to_cluster.(root) <- !cluster_count;
              incr cluster_count
            end;
            of_point.(point) <- root_to_cluster.(root)
          done;
          let sizes = Array.make !cluster_count 0 in
          for point = 0 to point_count - 1 do
            sizes.(of_point.(point)) <- sizes.(of_point.(point)) + 1
          done;
          let offsets = Array.make (!cluster_count + 1) 0 in
          for cluster = 0 to !cluster_count - 1 do
            offsets.(cluster + 1) <- offsets.(cluster) + sizes.(cluster)
          done;
          let members = Array.make point_count 0
          and cursors = Array.copy offsets in
          for point = 0 to point_count - 1 do
            let cluster = of_point.(point) in
            let slot = cursors.(cluster) in
            members.(slot) <- point;
            cursors.(cluster) <- slot + 1
          done;
          let representatives = Array.init !cluster_count (fun cluster ->
            members.(offsets.(cluster))) in
          let clusters : Point_clusters.clusters = {
            count = !cluster_count; of_point; offsets; members;
            representatives;
          } in
          let had_point_normals = Geometry.find_attribute
              ~owner:Attribute.Point "N" geometry <> None in
          let source_edge_groups = Geometry.edge_groups geometry in
          let result = Fuse_reduce.apply ?cancel ~grain
              ~position ~attributes:Keep_first
              ~attribute_rules:[] ~group_rules:[] ~compact:true ~rewire:true
              ~remap_edge_groups:false
              clusters geometry
            |> fun reduced -> Result.bind reduced (fun output ->
                 Fuse_cleanup.apply_with_mapping ?cancel ~grain
                   ~remove_degenerate_primitives
                   ~remove_unused_points_from_degenerate_primitives:
                     remove_degenerate_primitives
                   ~remove_all_unused_points:false output)
            |> fun cleaned -> Result.bind cleaned (fun (output, fused_to_output) ->
                 match source_edge_groups with
                 | [] -> Ok output
                 | groups ->
                     let target_topology = Geometry.topology output in
                     let target_index = Topology_index.create ?cancel
                         target_topology in
                     let source_to_output = Array.init point_count (fun point ->
                       let fused = clusters.of_point.(point) in
                       if fused < 0 || fused >= Array.length fused_to_output
                       then -1 else fused_to_output.(fused)) in
                     let rec remap output = function
                       | [] -> Ok output
                       | group :: rest ->
                           Result.bind (Edge_group.remap ?cancel
                             ~source_index:index_value ~target_topology
                             ~target_index ~point_map:source_to_output group)
                             (fun group ->
                               Result.bind (Geometry.with_edge_group group output)
                                 (fun output -> remap output rest)) in
                     remap output groups)
            |> Result.map (fun output ->
                 Geometry.without_attribute ~owner:Attribute.Point "N" output
                 |> Geometry.without_attribute ~owner:Attribute.Vertex "N") in
          if not recompute_point_normals || not had_point_normals then result
          else Result.bind result (fun output ->
            Deform.normals ?cancel ~grain output)
        end)


let run ?cancel ?grain ?edges ?connectivity_attribute ?position
    ?remove_degenerate_primitives ?recompute_point_normals geometry =
  Error.guard ~operation:"edge_collapse" ~code:"invalid_topology" (fun () ->
    raw ?cancel ?grain ?edges ?connectivity_attribute ?position
      ?remove_degenerate_primitives ?recompute_point_normals geometry)
