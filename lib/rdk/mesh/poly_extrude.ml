open Rays_math

type divide = Extrude_individual | Extrude_connected_components

let ( let* ) = Result.bind

module Int_builder = struct
  type t = { mutable values : int array; mutable length : int }

  let create capacity =
    { values = Array.make (max 1 capacity) 0; length = 0 }

  let add value item =
    if value.length = Array.length value.values then begin
      if value.length > Sys.max_array_length / 2 then
        invalid_arg "Poly Extrude: association count exceeds array limits";
      let grown = Array.make (min Sys.max_array_length (value.length * 2)) 0 in
      Array.blit value.values 0 grown 0 value.length;
      value.values <- grown
    end;
    value.values.(value.length) <- item;
    let index = value.length in
    value.length <- index + 1;
    index

  let freeze value = Array.sub value.values 0 value.length
end

module Pair_table = struct
  type t = {
    mutable components : int array;
    mutable points : int array;
    mutable values : int array;
    mutable size : int;
  }

  let capacity expected =
    let wanted = max 16 (expected + (expected / 2) + 1) in
    let value = ref 16 in
    while !value < wanted && !value <= Sys.max_array_length / 2 do
      value := !value lsl 1
    done;
    if !value < wanted then
      invalid_arg "Poly Extrude: association table exceeds array limits";
    !value

  let create expected =
    let capacity = capacity expected in
    { components = Array.make capacity (-1);
      points = Array.make capacity 0;
      values = Array.make capacity 0;
      size = 0 }

  let hash component point mask =
    let value = Hash.mix point (component * 8_191) in
    (value lxor (value lsr 16)) land mask

  let slot components points component point =
    let mask = Array.length components - 1 in
    let index = ref (hash component point mask) in
    while components.(!index) >= 0
        && (components.(!index) <> component || points.(!index) <> point) do
      index := (!index + 1) land mask
    done;
    !index

  let insert_raw value component point association =
    let index = slot value.components value.points component point in
    value.components.(index) <- component;
    value.points.(index) <- point;
    value.values.(index) <- association;
    value.size <- value.size + 1

  let grow value =
    let old_components = value.components and old_points = value.points
    and old_values = value.values in
    if Array.length old_components > Sys.max_array_length / 2 then
      invalid_arg "Poly Extrude: association table exceeds array limits";
    let capacity = Array.length old_components * 2 in
    value.components <- Array.make capacity (-1);
    value.points <- Array.make capacity 0;
    value.values <- Array.make capacity 0;
    value.size <- 0;
    Array.iteri (fun index component ->
      if component >= 0 then
        insert_raw value component old_points.(index) old_values.(index))
      old_components

  let find_or_add value ~component ~point create =
    if (value.size + 1) * 3 >= Array.length value.components * 2 then grow value;
    let index = slot value.components value.points component point in
    if value.components.(index) >= 0 then value.values.(index)
    else begin
      let association = create () in
      value.components.(index) <- component;
      value.points.(index) <- point;
      value.values.(index) <- association;
      value.size <- value.size + 1;
      association
    end

  let find value ~component ~point =
    let index = slot value.components value.points component point in
    if value.components.(index) < 0 then
      invalid_arg "Poly Extrude: missing component/point association";
    value.values.(index)
end

module Edge_lookup = Topology_edge_lookup

let validate_selection geometry = function
  | None -> Ok ()
  | Some group when Group.owner group <> Group.Primitive ->
      Error "Rdk_mesh.Poly_extrude.poly_extrude: selection must own primitives"
  | Some group when Group.length group <> Geometry.primitive_count geometry ->
      Error "Rdk_mesh.Poly_extrude.poly_extrude: selection length does not match primitive count"
  | Some _ -> Ok ()

let validate_name label = function
  | None -> Ok ()
  | Some name when String.trim name = "" ->
      Error ("Rdk_mesh.Poly_extrude.poly_extrude: empty " ^ label ^ " group name")
  | Some _ -> Ok ()

let checked_array_length label value =
  if Int64.compare value 0L < 0
      || Int64.compare value (Int64.of_int Sys.max_array_length) > 0 then
    Error ("Rdk_mesh.Poly_extrude.poly_extrude: " ^ label ^
      " cardinality exceeds OCaml array limits")
  else Ok (Int64.to_int value)

let find_root parent value =
  let root = ref value in
  while parent.(!root) <> !root do root := parent.(!root) done;
  let current = ref value in
  while parent.(!current) <> !root do
    let next = parent.(!current) in
    parent.(!current) <- !root;
    current := next
  done;
  !root

let union_roots parent left right =
  let left = find_root parent left and right = find_root parent right in
  if left <> right then
    if left < right then parent.(right) <- left else parent.(left) <- right

let remap_attribute ?cancel ~grain point_map vertex_map primitive_map value =
  Topology_remap.mapped_attribute ?cancel ~grain ~point_map ~vertex_map
    ~primitive_map ~drop:Topology_remap.drop_point_vertex_normals value

let remap_group ~grain point_map vertex_map primitive_map value =
  Topology_remap.mapped_group ~grain ~point_map ~vertex_map ~primitive_map value

let rec merge_group group = function
  | [] -> Ok [group]
  | existing :: rest when Group.owner existing = Group.owner group
      && String.equal (Group.name existing) (Group.name group) ->
      Result.map (fun merged -> merged :: rest) (Group.union existing group)
  | existing :: rest ->
      Result.map (fun rest -> existing :: rest) (merge_group group rest)

let rec merge_edge_group group = function
  | [] -> Ok [group]
  | existing :: rest when String.equal (Edge_group.name existing)
      (Edge_group.name group) ->
      Result.map (fun merged -> merged :: rest) (Edge_group.union existing group)
  | existing :: rest ->
      Result.map (fun rest -> existing :: rest) (merge_edge_group group rest)

let run_default ?cancel ?(grain = 1_024) ~distance geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Poly_extrude.poly_extrude: grain must be positive";
  if not (Float.is_finite distance) then
    Error "Rdk_mesh.Poly_extrude.poly_extrude: distance must be finite"
  else
    let topology = Geometry.topology geometry
    and positions = Packed.Float3.Private.view (Geometry.positions geometry) in
    let source_vertices = Geometry.vertex_count geometry
    and source_primitives = Geometry.primitive_count geometry in
    let curve = ref None in
    for primitive = 0 to source_primitives - 1 do
      if Topology.primitive_kind topology primitive <> Topology.Polygon
         && !curve = None
      then curve := Some primitive
    done;
    match !curve with
    | Some primitive -> Error (Printf.sprintf
        "Rdk_mesh.Poly_extrude.poly_extrude: primitive %d is a curve" primitive)
    | None when source_vertices > max_int / 6
             || source_primitives > (max_int - source_vertices) / 2 ->
        Error "Rdk_mesh.Poly_extrude.poly_extrude: output cardinality exceeds OCaml array limits"
    | None ->
        let output_points = source_vertices * 2
        and output_vertices = source_vertices * 6
        and output_primitives = source_primitives * 2 + source_vertices in
        let px = Array.make output_points 0. and py = Array.make output_points 0.
        and pz = Array.make output_points 0.
        and point_map = Array.make output_points 0
        and vertex_points = Array.make output_vertices 0
        and vertex_map = Array.make output_vertices 0
        and primitive_offsets = Array.make (output_primitives + 1) 0
        and primitive_map = Array.make output_primitives 0
        and primitive_kinds = Bytes.make output_primitives '\000' in
        let failures = Array.make source_primitives None in
        if source_primitives > 0 then Parallel.for_ ~chunk_size:grain ~start:0
            ~finish:(source_primitives - 1) (fun primitive ->
              if primitive land 255 = 0 then Cancel.check_opt cancel;
              let first, last = Topology.primitive_vertex_range topology primitive in
              let count = last - first and nx = ref 0. and ny = ref 0. and nz = ref 0. in
              for local = 0 to count - 1 do
                let next = (local + 1) mod count in
                let a = Topology.point_of_vertex topology (first + local)
                and b = Topology.point_of_vertex topology (first + next) in
                let ax = positions.x.(a) and ay = positions.y.(a) and az = positions.z.(a)
                and bx = positions.x.(b) and by = positions.y.(b) and bz = positions.z.(b) in
                nx := !nx +. ((ay -. by) *. (az +. bz));
                ny := !ny +. ((az -. bz) *. (ax +. bx));
                nz := !nz +. ((ax -. bx) *. (ay +. by))
              done;
              let length = sqrt ((!nx *. !nx) +. (!ny *. !ny) +. (!nz *. !nz)) in
              if length <= 1e-20 || not (Float.is_finite length) then
                failures.(primitive) <- Some (Printf.sprintf
                  "Rdk_mesh.Poly_extrude.poly_extrude: primitive %d is degenerate" primitive)
              else begin
                let dx = distance *. !nx /. length and dy = distance *. !ny /. length
                and dz = distance *. !nz /. length and output_vertex = first * 6
                and output_primitive = (primitive * 2) + first in
                for local = 0 to count - 1 do
                  let vertex = first + local in
                  let point = Topology.point_of_vertex topology vertex
                  and bottom = vertex * 2 in
                  px.(bottom) <- positions.x.(point);
                  py.(bottom) <- positions.y.(point);
                  pz.(bottom) <- positions.z.(point);
                  px.(bottom + 1) <- positions.x.(point) +. dx;
                  py.(bottom + 1) <- positions.y.(point) +. dy;
                  pz.(bottom + 1) <- positions.z.(point) +. dz;
                  point_map.(bottom) <- point; point_map.(bottom + 1) <- point;
                  let reversed_vertex = first + count - local - 1 in
                  vertex_points.(output_vertex + local) <- reversed_vertex * 2;
                  vertex_map.(output_vertex + local) <- reversed_vertex;
                  vertex_points.(output_vertex + count + local) <- (vertex * 2) + 1;
                  vertex_map.(output_vertex + count + local) <- vertex
                done;
                primitive_offsets.(output_primitive) <- output_vertex;
                primitive_offsets.(output_primitive + 1) <- output_vertex + count;
                primitive_map.(output_primitive) <- primitive;
                primitive_map.(output_primitive + 1) <- primitive;
                for local = 0 to count - 1 do
                  let next = (local + 1) mod count and current_vertex = first + local in
                  let next_vertex = first + next
                  and output = output_vertex + (2 * count) + (local * 4)
                  and output_side = output_primitive + 2 + local in
                  vertex_points.(output) <- current_vertex * 2;
                  vertex_points.(output + 1) <- next_vertex * 2;
                  vertex_points.(output + 2) <- (next_vertex * 2) + 1;
                  vertex_points.(output + 3) <- (current_vertex * 2) + 1;
                  vertex_map.(output) <- current_vertex;
                  vertex_map.(output + 1) <- next_vertex;
                  vertex_map.(output + 2) <- next_vertex;
                  vertex_map.(output + 3) <- current_vertex;
                  primitive_offsets.(output_side) <- output;
                  primitive_map.(output_side) <- primitive
                done
              end);
        primitive_offsets.(output_primitives) <- output_vertices;
        match Array.find_opt Option.is_some failures |> Option.join with
        | Some message -> Error message
        | None ->
            let output_positions = Packed.Float3.Private.of_owned_exn
                ~x:px ~y:py ~z:pz in
            let output_topology = Topology.Private.create_validated_owned
                ~point_count:output_points ~vertex_points ~primitive_offsets
                ~primitive_kinds in
            let rec attributes result = function
              | [] -> Ok (List.rev result)
              | attribute :: rest ->
                  Result.bind (remap_attribute ?cancel ~grain point_map vertex_map primitive_map
                    attribute) (function
                    | None -> attributes result rest
                    | Some attribute -> attributes (attribute :: result) rest) in
            Result.bind (attributes [] (Geometry.attributes geometry)) (fun attributes ->
              let groups = List.map
                  (remap_group ~grain point_map vertex_map primitive_map)
                  (Geometry.groups geometry) in
              let edge_groups = match Geometry.edge_groups geometry with
                | [] -> []
                | source_groups ->
                    let source_index = Topology_index.create ?cancel topology
                    and target_index = Topology_index.create ?cancel
                        output_topology in
                    List.map (fun source_group ->
                      let builder = Edge_group.Builder.create
                          ~topology:output_topology ~index:target_index
                          ~name:(Edge_group.name source_group) in
                      Edge_group.iter (fun edge ->
                        let incidence = Topology_index.edge_incidence_count
                            source_index edge in
                        for local = 0 to incidence - 1 do
                          let current = Topology_index.edge_vertex source_index
                              ~edge ~local in
                          let next = Topology_index.next_vertex source_index current in
                          let add a b = match Topology_index.find_edge target_index
                              ~a ~b with
                            | None -> ()
                            | Some target -> Edge_group.Builder.set builder target true in
                          if next >= 0 then begin
                            add (current * 2) (next * 2);
                            add (current * 2 + 1) (next * 2 + 1)
                          end
                        done) source_group;
                      Edge_group.Builder.freeze builder) source_groups in
              Geometry.create ~positions:output_positions ~topology:output_topology
                ~attributes ~groups ~edge_groups ())

let run_general ?cancel ?(grain = 16_384) ?primitives ?split_edges
    ?(divide = Extrude_connected_components) ?(divisions = 1)
    ?(output_front = true) ?(output_back = true) ?(output_side = true)
    ?front_group ?back_group ?side_group ?front_boundary_group
    ?back_boundary_group ~distance geometry =
  if grain <= 0 then invalid_arg "Rdk_mesh.Poly_extrude.poly_extrude: grain must be positive";
  if divisions <= 0 then Error "Rdk_mesh.Poly_extrude.poly_extrude: divisions must be positive"
  else if not (Float.is_finite distance) then
    Error "Rdk_mesh.Poly_extrude.poly_extrude: distance must be finite"
  else
  let* () = validate_selection geometry primitives in
  let* () = validate_name "front" front_group in
  let* () = validate_name "back" back_group in
  let* () = validate_name "side" side_group in
  let* () = validate_name "front boundary" front_boundary_group in
  let* () = validate_name "back boundary" back_boundary_group in
  let topology = Geometry.topology geometry in
  let* () = match split_edges with
    | Some _ when divide = Extrude_individual ->
        Error "Rdk_mesh.Poly_extrude.poly_extrude: split edges require connected-components mode"
    | Some group when Edge_group.topology_data_id group
        <> Topology.data_id topology ->
        Error "Rdk_mesh.Poly_extrude.poly_extrude: split edge group belongs to another topology"
    | None | Some _ -> Ok () in
  let topology_view = Topology.Private.view topology in
  let source_points = topology_view.point_count
  and source_primitives = Bytes.length topology_view.primitive_kinds in
  let selected = Array.init source_primitives (fun primitive ->
      match primitives with None -> true | Some group -> Group.mem primitive group) in
  let selected_count = Array.fold_left
      (fun total value -> if value then total + 1 else total) 0 selected in
  if selected_count = 0 then Ok geometry
  else
  let* face_nx, face_ny, face_nz = Face_normals.compute ?cancel ~grain
      ?primitives ~operation:"Rdk_mesh.Poly_extrude.poly_extrude" geometry in
  let index = Topology_index.create ?cancel topology in
  let index_view = Topology_index.Private.view index in
  let primitive_component = Array.make source_primitives (-1) in
  let* component_count = match divide with
    | Extrude_individual ->
        let next = ref 0 in
        Array.iteri (fun primitive is_selected -> if is_selected then begin
          primitive_component.(primitive) <- !next;
          incr next
        end) selected;
        Ok !next
    | Extrude_connected_components ->
        let parent = Array.init source_primitives (fun primitive ->
          if selected.(primitive) then primitive else -1) in
        let non_manifold = ref (-1) in
        for edge = 0 to Topology_index.edge_count index - 1 do
          if edge land 16_383 = 0 then Cancel.check_opt cancel;
          let first = index_view.edge_offsets.(edge)
          and last = index_view.edge_offsets.(edge + 1) in
          let selected_incidence = ref 0 in
          for at = first to last - 1 do
            let primitive = index_view.primitive_of_vertex.
                (index_view.edge_vertices.(at)) in
            if selected.(primitive) then incr selected_incidence
          done;
          if !selected_incidence > 0 && last - first > 2 then
            non_manifold := edge
          else if !selected_incidence > 1
              && not (match split_edges with
                | None -> false | Some group -> Edge_group.mem edge group) then begin
            let base = ref (-1) in
            for at = first to last - 1 do
              let primitive = index_view.primitive_of_vertex.
                  (index_view.edge_vertices.(at)) in
              if selected.(primitive) then
                if !base < 0 then base := primitive
                else union_roots parent !base primitive
            done
          end
        done;
        if !non_manifold >= 0 then Error (Printf.sprintf
            "Rdk_mesh.Poly_extrude.poly_extrude: connected extrusion touches non-manifold edge %d"
            !non_manifold)
        else begin
          let root_component = Array.make source_primitives (-1)
          and next = ref 0 in
          for primitive = 0 to source_primitives - 1 do
            if selected.(primitive) then begin
              let root = find_root parent primitive in
              if root_component.(root) < 0 then begin
                root_component.(root) <- !next;
                incr next
              end;
              primitive_component.(primitive) <- root_component.(root)
            end
          done;
          Ok !next
        end in
  let selected_vertices = ref 0 in
  for primitive = 0 to source_primitives - 1 do
    if primitive land 4_095 = 0 then Cancel.check_opt cancel;
    if selected.(primitive) then
      selected_vertices := !selected_vertices
        + topology_view.primitive_offsets.(primitive + 1)
        - topology_view.primitive_offsets.(primitive)
  done;
  let boundary_vertices_full = Array.make !selected_vertices 0
  and boundary_count = ref 0 in
  for primitive = 0 to source_primitives - 1 do
    if primitive land 4_095 = 0 then Cancel.check_opt cancel;
    if selected.(primitive) then begin
      let first = topology_view.primitive_offsets.(primitive)
      and last = topology_view.primitive_offsets.(primitive + 1) in
      for vertex = first to last - 1 do
        let boundary = match divide with
          | Extrude_individual -> true
          | Extrude_connected_components ->
              let edge = index_view.edge_of_vertex.(vertex) in
              (match split_edges with
               | Some group when Edge_group.mem edge group -> true
               | _ ->
                   let edge_first = index_view.edge_offsets.(edge)
                   and edge_last = index_view.edge_offsets.(edge + 1) in
                   if edge_last - edge_first = 1 then true
                   else begin
                     let other = ref (-1) in
                     for at = edge_first to edge_last - 1 do
                       let candidate = index_view.primitive_of_vertex.
                           (index_view.edge_vertices.(at)) in
                       if candidate <> primitive then other := candidate
                     done;
                     !other < 0 || not selected.(!other)
                   end) in
        if boundary then begin
          boundary_vertices_full.(!boundary_count) <- vertex;
          incr boundary_count
        end
      done
    end
  done;
  let boundary_vertices = Array.sub boundary_vertices_full 0 !boundary_count in
  let need_new_points = output_front || output_side in
  let association_table, association_sources =
    if not need_new_points then Pair_table.create 0, [||]
    else begin
      let expected = min !selected_vertices
          (max 16 (source_points + component_count)) in
      let table = Pair_table.create expected and sources = Int_builder.create expected in
      for primitive = 0 to source_primitives - 1 do
        if primitive land 4_095 = 0 then Cancel.check_opt cancel;
        if selected.(primitive) then begin
          let component = primitive_component.(primitive)
          and first = topology_view.primitive_offsets.(primitive)
          and last = topology_view.primitive_offsets.(primitive + 1) in
          for vertex = first to last - 1 do
            let point = topology_view.vertex_points.(vertex) in
            ignore (Pair_table.find_or_add table ~component ~point
              (fun () -> Int_builder.add sources point))
          done
        end
      done;
      table, Int_builder.freeze sources
    end in
  let association_count = Array.length association_sources in
  let direction_x = Array.make association_count 0.
  and direction_y = Array.make association_count 0.
  and direction_z = Array.make association_count 0. in
  let* () = if not need_new_points then Ok () else begin
    for primitive = 0 to source_primitives - 1 do
      if primitive land 4_095 = 0 then Cancel.check_opt cancel;
      if selected.(primitive) then begin
        let component = primitive_component.(primitive)
        and first = topology_view.primitive_offsets.(primitive)
        and last = topology_view.primitive_offsets.(primitive + 1) in
        for vertex = first to last - 1 do
          let point = topology_view.vertex_points.(vertex) in
          let association = Pair_table.find association_table ~component ~point in
          direction_x.(association) <- direction_x.(association) +. face_nx.(primitive);
          direction_y.(association) <- direction_y.(association) +. face_ny.(primitive);
          direction_z.(association) <- direction_z.(association) +. face_nz.(primitive)
        done
      end
    done;
    let invalid_direction = Atomic.make false in
    if association_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(association_count - 1) (fun association ->
          if association land 16_383 = 0 then Cancel.check_opt cancel;
          let x = direction_x.(association) and y = direction_y.(association)
          and z = direction_z.(association) in
          let scale = max (abs_float x) (max (abs_float y) (abs_float z)) in
          if scale = 0. || not (Float.is_finite scale) then Atomic.set invalid_direction true
          else begin
            let x = x /. scale and y = y /. scale and z = z /. scale in
            let inverse = 1. /. sqrt (x *. x +. y *. y +. z *. z) in
            direction_x.(association) <- x *. inverse;
            direction_y.(association) <- y *. inverse;
            direction_z.(association) <- z *. inverse
          end);
    if Atomic.get invalid_direction then Error
        "Rdk_mesh.Poly_extrude.poly_extrude: selected point normals cancel to zero"
    else Ok ()
  end in
  let base_index = Array.make source_primitives (-1)
  and front_index = Array.make source_primitives (-1) in
  let base_primitives = ref 0 and base_vertices = ref 0 in
  for primitive = 0 to source_primitives - 1 do
    if primitive land 4_095 = 0 then Cancel.check_opt cancel;
    if not selected.(primitive) || output_back then begin
      base_index.(primitive) <- !base_primitives;
      incr base_primitives;
      base_vertices := !base_vertices
        + topology_view.primitive_offsets.(primitive + 1)
        - topology_view.primitive_offsets.(primitive)
    end
  done;
  let front_primitives = if output_front then selected_count else 0
  and front_vertices = if output_front then !selected_vertices else 0
  and side_primitives_64 = if output_side then
      Int64.mul (Int64.of_int !boundary_count) (Int64.of_int divisions) else 0L
  and side_vertices_64 = if output_side then
      Int64.mul 4L (Int64.mul (Int64.of_int !boundary_count)
        (Int64.of_int divisions)) else 0L in
  let* side_primitives = checked_array_length "primitive" side_primitives_64 in
  let* side_vertices = checked_array_length "vertex" side_vertices_64 in
  let* output_points = checked_array_length "point"
      (Int64.add (Int64.of_int source_points)
        (if need_new_points then Int64.mul (Int64.of_int association_count)
           (Int64.of_int divisions) else 0L)) in
  let* output_primitives = checked_array_length "primitive"
      (Int64.add (Int64.of_int !base_primitives)
        (Int64.add (Int64.of_int front_primitives)
          (Int64.of_int side_primitives))) in
  let* output_vertices = checked_array_length "vertex"
      (Int64.add (Int64.of_int !base_vertices)
        (Int64.add (Int64.of_int front_vertices)
          (Int64.of_int side_vertices))) in
  let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
  let px = Array.make output_points 0. and py = Array.make output_points 0.
  and pz = Array.make output_points 0. and point_map = Array.make output_points 0 in
  Array.blit positions.x 0 px 0 source_points;
  Array.blit positions.y 0 py 0 source_points;
  Array.blit positions.z 0 pz 0 source_points;
  for point = 0 to source_points - 1 do point_map.(point) <- point done;
  let new_point_count = output_points - source_points in
  if new_point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(new_point_count - 1) (fun local ->
        if local land 16_383 = 0 then Cancel.check_opt cancel;
        let association = local / divisions and layer = local mod divisions + 1 in
        let point = association_sources.(association)
        and t = float_of_int layer /. float_of_int divisions in
        let output = source_points + local in
        px.(output) <- positions.x.(point) +. distance *. t
          *. direction_x.(association);
        py.(output) <- positions.y.(point) +. distance *. t
          *. direction_y.(association);
        pz.(output) <- positions.z.(point) +. distance *. t
          *. direction_z.(association);
        point_map.(output) <- point);
  let vertex_points = Array.make output_vertices 0
  and vertex_map = Array.make output_vertices 0
  and primitive_offsets = Array.make (output_primitives + 1) 0
  and primitive_map = Array.make output_primitives 0
  and primitive_kinds = Bytes.make output_primitives '\000'
  and front_membership = Bytes.make output_primitives '\000'
  and back_membership = Bytes.make output_primitives '\000'
  and side_membership = Bytes.make output_primitives '\000' in
  let primitive_cursor = ref 0 and vertex_cursor = ref 0 in
  for primitive = 0 to source_primitives - 1 do
    if primitive land 4_095 = 0 then Cancel.check_opt cancel;
    if base_index.(primitive) >= 0 then begin
      let output = !primitive_cursor in
      base_index.(primitive) <- output;
      primitive_offsets.(output) <- !vertex_cursor;
      primitive_map.(output) <- primitive;
      Bytes.set primitive_kinds output
        (Bytes.get topology_view.primitive_kinds primitive);
      if selected.(primitive) then Bytes.set back_membership output '\001';
      vertex_cursor := !vertex_cursor
        + topology_view.primitive_offsets.(primitive + 1)
        - topology_view.primitive_offsets.(primitive);
      incr primitive_cursor
    end
  done;
  if output_front then
    for primitive = 0 to source_primitives - 1 do
      if primitive land 4_095 = 0 then Cancel.check_opt cancel;
      if selected.(primitive) then begin
        let output = !primitive_cursor in
        front_index.(primitive) <- output;
        primitive_offsets.(output) <- !vertex_cursor;
        primitive_map.(output) <- primitive;
        Bytes.set front_membership output '\001';
        vertex_cursor := !vertex_cursor
          + topology_view.primitive_offsets.(primitive + 1)
          - topology_view.primitive_offsets.(primitive);
        incr primitive_cursor
      end
    done;
  let side_first_primitive = !primitive_cursor
  and side_first_vertex = !vertex_cursor in
  if output_side then
    for boundary = 0 to !boundary_count - 1 do
      for division = 0 to divisions - 1 do
        let output = side_first_primitive + (boundary * divisions) + division in
        primitive_offsets.(output) <- side_first_vertex
          + ((boundary * divisions + division) * 4);
        primitive_map.(output) <- index_view.primitive_of_vertex.
            (boundary_vertices.(boundary));
        Bytes.set side_membership output '\001'
      done
    done;
  primitive_offsets.(output_primitives) <- output_vertices;
  if source_primitives > 0 then Parallel.for_ ~chunk_size:grain ~start:0
      ~finish:(source_primitives - 1) (fun primitive ->
        if primitive land 4_095 = 0 then Cancel.check_opt cancel;
        let first = topology_view.primitive_offsets.(primitive)
        and last = topology_view.primitive_offsets.(primitive + 1) in
        let base = base_index.(primitive) in
        if base >= 0 then begin
          let output = primitive_offsets.(base) in
          for vertex = first to last - 1 do
            let local = vertex - first in
            vertex_points.(output + local) <- topology_view.vertex_points.(vertex);
            vertex_map.(output + local) <- vertex
          done
        end;
        let front = front_index.(primitive) in
        if front >= 0 then begin
          let output = primitive_offsets.(front)
          and component = primitive_component.(primitive) in
          for vertex = first to last - 1 do
            let local = vertex - first and point = topology_view.vertex_points.(vertex) in
            let association = Pair_table.find association_table ~component ~point in
            vertex_points.(output + local) <- source_points
              + (association * divisions) + divisions - 1;
            vertex_map.(output + local) <- vertex
          done
        end);
  let layer_point point association layer =
    if layer = 0 then point
    else source_points + (association * divisions) + layer - 1 in
  if output_side && !boundary_count > 0 then
    Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(!boundary_count - 1)
      (fun boundary ->
        if boundary land 4_095 = 0 then Cancel.check_opt cancel;
        let vertex = boundary_vertices.(boundary)
        and next = index_view.next_vertex.(boundary_vertices.(boundary)) in
        let primitive = index_view.primitive_of_vertex.(vertex) in
        let component = primitive_component.(primitive)
        and point = topology_view.vertex_points.(vertex)
        and next_point = topology_view.vertex_points.(next) in
        let association = Pair_table.find association_table ~component ~point
        and next_association = Pair_table.find association_table
            ~component ~point:next_point in
        for division = 0 to divisions - 1 do
          let output = side_first_vertex + ((boundary * divisions + division) * 4) in
          vertex_points.(output) <- layer_point point association division;
          vertex_points.(output + 1) <-
            layer_point next_point next_association division;
          vertex_points.(output + 2) <-
            layer_point next_point next_association (division + 1);
          vertex_points.(output + 3) <-
            layer_point point association (division + 1);
          vertex_map.(output) <- vertex;
          vertex_map.(output + 1) <- next;
          vertex_map.(output + 2) <- next;
          vertex_map.(output + 3) <- vertex
        done);
  let output_positions = Packed.Float3.Private.of_owned_exn ~x:px ~y:py ~z:pz in
  let output_topology = Topology.Private.create_validated_owned
      ~point_count:output_points ~vertex_points ~primitive_offsets ~primitive_kinds in
  let rec remap_attributes result = function
    | [] -> Ok (List.rev result)
    | attribute :: rest ->
        let* mapped = remap_attribute ?cancel ~grain point_map vertex_map
            primitive_map attribute in
        remap_attributes (match mapped with None -> result | Some value -> value :: result)
          rest in
  let* attributes = remap_attributes [] (Geometry.attributes geometry) in
  let groups = List.map (remap_group ~grain point_map vertex_map primitive_map)
      (Geometry.groups geometry) in
  let add_role name membership groups = match name with
    | None -> Ok groups
    | Some name ->
        merge_group (Group.init ~grain ~owner:Group.Primitive ~name
          output_primitives (fun primitive -> Bytes.get membership primitive <> '\000'))
          groups in
  let* groups = add_role front_group front_membership groups in
  let* groups = add_role back_group back_membership groups in
  let* groups = add_role side_group side_membership groups in
  let source_edge_groups = Geometry.edge_groups geometry in
  let needs_edge_output = source_edge_groups <> []
      || Option.is_some front_boundary_group
      || Option.is_some back_boundary_group in
  let* edge_groups = if not needs_edge_output then Ok [] else begin
    let source_edge_index = index
    and target_edges = Edge_lookup.create ?cancel
        (Topology.Private.view output_topology) in
    let edge_count = Edge_lookup.count target_edges in
    let make_bits () = Bytes.make ((edge_count + 7) / 8) '\000' in
    let add_edge bits a b =
      let edge = Edge_lookup.find target_edges ~a ~b in
      if edge >= 0 then begin
        let byte = edge lsr 3 and mask = 1 lsl (edge land 7) in
        Bytes.set bits byte
          (Char.chr (Char.code (Bytes.get bits byte) lor mask))
      end in
    let freeze name bits = Edge_group.Private.of_owned_bits
        ~topology:output_topology ~edge_count ~name bits in
    let map_edge_group source_group =
      let bits = make_bits () in
      Edge_group.iter (fun edge ->
        if edge land 16_383 = 0 then Cancel.check_opt cancel;
        let a, b = Topology_index.edge_points source_edge_index edge in
        add_edge bits a b;
        if need_new_points then begin
          let first = index_view.edge_offsets.(edge)
          and last = index_view.edge_offsets.(edge + 1) in
          for at = first to last - 1 do
            let primitive = index_view.primitive_of_vertex.
                (index_view.edge_vertices.(at)) in
            if selected.(primitive) then begin
              let component = primitive_component.(primitive) in
              let association_a = Pair_table.find association_table
                  ~component ~point:a
              and association_b = Pair_table.find association_table
                  ~component ~point:b in
              for layer = 1 to divisions do
                add_edge bits
                  (layer_point a association_a layer)
                  (layer_point b association_b layer)
              done
            end
          done
        end) source_group;
      freeze (Edge_group.name source_group) bits in
    let edge_groups = List.map map_edge_group source_edge_groups in
    let add_boundary name layer edge_groups = match name with
      | None -> Ok edge_groups
      | Some name ->
          let bits = make_bits () in
          Array.iteri (fun boundary vertex ->
            if boundary land 4_095 = 0 then Cancel.check_opt cancel;
            let next = index_view.next_vertex.(vertex)
            and primitive = index_view.primitive_of_vertex.(vertex) in
            let component = primitive_component.(primitive)
            and a = topology_view.vertex_points.(vertex)
            and b = topology_view.vertex_points.(next) in
            if layer = 0 then add_edge bits a b
            else begin
              let association_a = Pair_table.find association_table
                  ~component ~point:a
              and association_b = Pair_table.find association_table
                  ~component ~point:b in
              add_edge bits (layer_point a association_a layer)
                (layer_point b association_b layer)
            end) boundary_vertices;
          merge_edge_group (freeze name bits) edge_groups in
    let* edge_groups = add_boundary back_boundary_group 0 edge_groups in
    if need_new_points then
      add_boundary front_boundary_group divisions edge_groups
    else match front_boundary_group with
      | None -> Ok edge_groups
      | Some name -> merge_edge_group
          (freeze name (make_bits ())) edge_groups
  end in
  Geometry.create ~positions:output_positions ~topology:output_topology
    ~attributes ~groups ~edge_groups ()

let run ?cancel ?(grain = 1_024) ?primitives ?split_edges
    ?(divide = Extrude_individual) ?(divisions = 1)
    ?(output_front = true) ?(output_back = true) ?(output_side = true)
    ?front_group ?back_group ?side_group ?front_boundary_group
    ?back_boundary_group ~distance geometry =
  Error.guard ~operation:"poly_extrude" ~code:"invalid_geometry" @@ fun () ->
  if primitives = None && split_edges = None && divide = Extrude_individual
      && divisions = 1 && output_front && output_back && output_side
      && front_group = None && back_group = None && side_group = None
      && front_boundary_group = None && back_boundary_group = None then
    run_default ?cancel ~grain ~distance geometry
  else
    run_general ?cancel ~grain ?primitives ?split_edges ~divide ~divisions
      ~output_front ~output_back ~output_side ?front_group ?back_group
      ?side_group ?front_boundary_group ?back_boundary_group ~distance geometry
