type t =
  | Selected_points of Group.t
  | Selected_vertices of Group.t
  | Selected_primitives of Group.t
  | Selected_edges of Edge_group.t

let validate ~operation topology = function
  | None -> Ok ()
  | Some (Selected_points group) ->
      if Group.owner group <> Group.Point then
        Error (operation ^ ": point selection must own points")
      else if Group.length group <> Topology.point_count topology then
        Error (operation ^ ": point selection length does not match point count")
      else Ok ()
  | Some (Selected_vertices group) ->
      if Group.owner group <> Group.Vertex then
        Error (operation ^ ": vertex selection must own vertices")
      else if Group.length group <> Topology.vertex_count topology then
        Error (operation ^ ": vertex selection length does not match vertex count")
      else Ok ()
  | Some (Selected_primitives group) ->
      if Group.owner group <> Group.Primitive then
        Error (operation ^ ": primitive selection must own primitives")
      else if Group.length group <> Topology.primitive_count topology then
        Error (operation ^ ": primitive selection length does not match primitive count")
      else Ok ()
  | Some (Selected_edges group) ->
      if Edge_group.topology_data_id group <> Topology.data_id topology then
        Error (operation ^ ": edge selection belongs to different topology")
      else Ok ()

let promote ?cancel ~grain ?(name = "__element_selection") ~destination selection topology_value =
  Result.bind (validate ~operation:"element selection" topology_value (Some selection)) (fun () ->
      match (destination, selection) with
      | Group.Point, Selected_points group
      | Group.Vertex, Selected_vertices group
      | Group.Primitive, Selected_primitives group ->
          Ok (Group.with_name name group)
      | _ -> (
          let topology = Topology.View.view topology_value in
          let needs_point_index =
            match (destination, selection) with
            | Group.Point, (Selected_vertices _ | Selected_primitives _)
            | Group.Vertex, Selected_primitives _ ->
                true
            | (Group.Point | Group.Vertex | Group.Primitive), _ -> false
          in
          let point_index =
            if needs_point_index then
              Some (Point_index.View.view (Point_index.create ?cancel topology_value))
            else None
          in
          let edge_index =
            match selection with
            | Selected_edges _ ->
                Some (Topology_index.View.view (Topology_index.create ?cancel topology_value))
            | Selected_points _ | Selected_vertices _ | Selected_primitives _ -> None
          in
          let point_reverse () = Option.get point_index
          and edge_reverse () = Option.get edge_index in
          let cancelled element = Cancel.poll cancel element 4095 in
          let point_selected point =
            cancelled point;
            match selection with
            | Selected_points group -> Group.mem point group
            | Selected_vertices group ->
                let reverse = point_reverse () in
                let found = ref false and at = ref reverse.point_offsets.(point) in
                let last = reverse.point_offsets.(point + 1) in
                while (not !found) && !at < last do
                  found := Group.mem reverse.point_vertices.(!at) group;
                  incr at
                done;
                !found
            | Selected_primitives group ->
                let reverse = point_reverse () in
                let found = ref false and at = ref reverse.point_offsets.(point) in
                let last = reverse.point_offsets.(point + 1) in
                while (not !found) && !at < last do
                  let vertex = reverse.point_vertices.(!at) in
                  found := Group.mem reverse.primitive_of_vertex.(vertex) group;
                  incr at
                done;
                !found
            | Selected_edges group ->
                let reverse = edge_reverse () in
                let found = ref false and at = ref reverse.point_edge_offsets.(point) in
                let last = reverse.point_edge_offsets.(point + 1) in
                while (not !found) && !at < last do
                  found := Edge_group.mem reverse.point_edges.(!at) group;
                  incr at
                done;
                !found
          in
          let vertex_selected vertex =
            cancelled vertex;
            match selection with
            | Selected_points group -> Group.mem topology.vertex_points.(vertex) group
            | Selected_vertices group -> Group.mem vertex group
            | Selected_primitives group ->
                let reverse = point_reverse () in
                Group.mem reverse.primitive_of_vertex.(vertex) group
            | Selected_edges group ->
                let reverse = edge_reverse () in
                let outgoing = reverse.edge_of_vertex.(vertex)
                and previous = reverse.previous_vertex.(vertex) in
                (outgoing >= 0 && Edge_group.mem outgoing group)
                || previous >= 0
                   && reverse.edge_of_vertex.(previous) >= 0
                   && Edge_group.mem reverse.edge_of_vertex.(previous) group
          in
          let primitive_selected primitive =
            cancelled primitive;
            match selection with
            | Selected_primitives group -> Group.mem primitive group
            | Selected_points group ->
                let found = ref false and vertex = ref topology.primitive_offsets.(primitive) in
                let last = topology.primitive_offsets.(primitive + 1) in
                while (not !found) && !vertex < last do
                  found := Group.mem topology.vertex_points.(!vertex) group;
                  incr vertex
                done;
                !found
            | Selected_vertices group ->
                let found = ref false and vertex = ref topology.primitive_offsets.(primitive) in
                let last = topology.primitive_offsets.(primitive + 1) in
                while (not !found) && !vertex < last do
                  found := Group.mem !vertex group;
                  incr vertex
                done;
                !found
            | Selected_edges group ->
                let reverse = edge_reverse () in
                let found = ref false and vertex = ref topology.primitive_offsets.(primitive) in
                let last = topology.primitive_offsets.(primitive + 1) in
                while (not !found) && !vertex < last do
                  let edge = reverse.edge_of_vertex.(!vertex) in
                  found := edge >= 0 && Edge_group.mem edge group;
                  incr vertex
                done;
                !found
          in
          match destination with
          | Group.Point ->
              Ok (Group.init ~grain ~owner:Group.Point ~name topology.point_count point_selected)
          | Group.Vertex ->
              Ok
                (Group.init ~grain ~owner:Group.Vertex ~name
                   (Array.length topology.vertex_points)
                   vertex_selected)
          | Group.Primitive ->
              Ok
                (Group.init ~grain ~owner:Group.Primitive ~name
                   (Array.length topology.primitive_offsets - 1)
                   primitive_selected)))
