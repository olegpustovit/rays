type operation = Dissolve_selected | Dissolve_non_selected
type bridge_policy = Create_bridged_polygons | Create_disjoint_polygons | Delete_bridge_polygons

let fail message = raise (Error.Fail ("Rdk.Dissolve.dissolve: " ^ message))
let get_ok = function Ok value -> value | Error message -> fail message

module Int_buffer = struct
  type t = { mutable values : int array; mutable length : int }

  let create capacity = { values = Array.make (max 8 capacity) 0; length = 0 }

  let ensure value needed =
    if needed > Array.length value.values then begin
      let capacity = ref (Array.length value.values) in
      while !capacity < needed do
        if !capacity > Sys.max_array_length / 2 then fail "output exceeds array limits";
        capacity := !capacity * 2
      done;
      let next = Array.make !capacity 0 in
      Array.blit value.values 0 next 0 value.length;
      value.values <- next
    end

  let add value item =
    ensure value (value.length + 1);
    value.values.(value.length) <- item;
    value.length <- value.length + 1

  let freeze value = Array.sub value.values 0 value.length
end

module Output = struct
  type t = {
    points : Int_buffer.t;
    vertices : Int_buffer.t;
    offsets : Int_buffer.t;
    primitives : Int_buffer.t;
    kinds : Topology.primitive_kind Dynarray.t;
  }

  let create source_vertices source_primitives =
    let offsets = Int_buffer.create (source_primitives + 1) in
    Int_buffer.add offsets 0;
    {
      points = Int_buffer.create source_vertices;
      vertices = Int_buffer.create source_vertices;
      offsets;
      primitives = Int_buffer.create source_primitives;
      kinds = Dynarray.create ();
    }

  let add value ~source ~kind corners source_points =
    let minimum = match kind with Topology.Open_polyline -> 2 | _ -> 3 in
    if Array.length corners >= minimum then begin
      Array.iter
        (fun vertex ->
          Int_buffer.add value.points source_points.(vertex);
          Int_buffer.add value.vertices vertex)
        corners;
      Dynarray.add_last value.kinds kind;
      Int_buffer.add value.primitives source;
      Int_buffer.add value.offsets value.points.length
    end

  let freeze value =
    ( Int_buffer.freeze value.points,
      Int_buffer.freeze value.vertices,
      Int_buffer.freeze value.offsets,
      Int_buffer.freeze value.primitives,
      Dynarray.to_array value.kinds )
end

let selected bits edge =
  Char.code (Bytes.unsafe_get bits (edge lsr 3)) land (1 lsl (edge land 7)) <> 0

let set_selected bits edge =
  let byte = edge lsr 3 and mask = 1 lsl (edge land 7) in
  Bytes.unsafe_set bits byte (Char.chr (Char.code (Bytes.unsafe_get bits byte) lor mask))

let point_of_vertex topology vertex = topology.Topology.View.vertex_points.(vertex)

let simplify ?cancel ~tolerance ~affected ~closed positions topology corners =
  let count = Array.length corners and minimum = if closed then 3 else 2 in
  if count <= minimum then corners
  else begin
    let keep = Bytes.make count '\001'
    and queued = Bytes.make count '\001'
    and previous = Array.make count 0
    and next = Array.make count 0
    and queue = Array.init count Fun.id in
    for index = 0 to count - 1 do
      previous.(index) <- (if index = 0 then if closed then count - 1 else -1 else index - 1);
      next.(index) <- (if index + 1 = count then if closed then 0 else -1 else index + 1)
    done;
    let head = ref 0
    and tail = ref 0
    and queued_count = ref count
    and active = ref count
    and cosine = cos tolerance in
    let inline index =
      let left = previous.(index) and right = next.(index) in
      if left < 0 || right < 0 then false
      else
        let a = point_of_vertex topology corners.(left)
        and b = point_of_vertex topology corners.(index)
        and c = point_of_vertex topology corners.(right) in
        if not (Bytes.get affected b = '\001') then false
        else
          let ux = positions.Packed.Float3.View.x.(b) -. positions.x.(a)
          and uy = positions.y.(b) -. positions.y.(a)
          and uz = positions.z.(b) -. positions.z.(a)
          and vx = positions.x.(c) -. positions.x.(b)
          and vy = positions.y.(c) -. positions.y.(b)
          and vz = positions.z.(c) -. positions.z.(b) in
          if
            not
              (Float.is_finite ux && Float.is_finite uy && Float.is_finite uz && Float.is_finite vx
             && Float.is_finite vy && Float.is_finite vz)
          then fail "Remove Inline Points encountered non-finite positions";
          let us = max (abs_float ux) (max (abs_float uy) (abs_float uz))
          and vs = max (abs_float vx) (max (abs_float vy) (abs_float vz)) in
          if us = 0. || vs = 0. then true
          else
            let ux = ux /. us
            and uy = uy /. us
            and uz = uz /. us
            and vx = vx /. vs
            and vy = vy /. vs
            and vz = vz /. vs in
            let ul = sqrt ((ux *. ux) +. (uy *. uy) +. (uz *. uz))
            and vl = sqrt ((vx *. vx) +. (vy *. vy) +. (vz *. vz)) in
            let value = ((ux *. vx) +. (uy *. vy) +. (uz *. vz)) /. (ul *. vl) in
            value >= cosine -. (64. *. Float.epsilon)
    in
    while !queued_count > 0 && !active > minimum do
      Cancel.poll cancel !head 4095;
      let index = queue.(!head) in
      head := (!head + 1) mod count;
      decr queued_count;
      Bytes.set queued index '\000';
      if Bytes.get keep index = '\001' && inline index then begin
        let left = previous.(index) and right = next.(index) in
        Bytes.set keep index '\000';
        decr active;
        if left >= 0 then next.(left) <- right;
        if right >= 0 then previous.(right) <- left;
        let enqueue item =
          if item >= 0 && Bytes.get keep item = '\001' && Bytes.get queued item = '\000' then begin
            queue.(!tail) <- item;
            tail := (!tail + 1) mod count;
            incr queued_count;
            Bytes.set queued item '\001'
          end
        in
        enqueue left;
        enqueue right
      end
    done;
    if !active = count then corners
    else begin
      let result = Array.make !active 0 and output = ref 0 in
      for index = 0 to count - 1 do
        if Bytes.get keep index = '\001' then begin
          result.(!output) <- corners.(index);
          incr output
        end
      done;
      result
    end
  end

let rotate value at =
  Array.init (Array.length value) (fun index -> value.((at + index) mod Array.length value))

let find_point topology corners point =
  let result = ref (-1) and index = ref 0 in
  while !result < 0 && !index < Array.length corners do
    if point_of_vertex topology corners.(!index) = point then result := !index;
    incr index
  done;
  !result

let bridge_loops ?cancel topology index selected_edges loops =
  match loops with
  | [] -> fail "bridge component has no boundary"
  | first :: rest ->
      let current = ref first and pending = ref (Array.of_list rest) in
      while Array.length !pending > 0 do
        Cancel.check_opt cancel;
        let found_loop = ref (-1)
        and found_current = ref (-1)
        and found_other = ref (-1)
        and edge = ref 0 in
        while !found_loop < 0 && !edge < Array.length index.Topology_index.View.edge_a do
          if selected selected_edges !edge then begin
            let a = index.edge_a.(!edge) and b = index.edge_b.(!edge) in
            let ca = find_point topology !current a and cb = find_point topology !current b in
            let loop = ref 0 in
            while !found_loop < 0 && !loop < Array.length !pending do
              let oa = find_point topology !pending.(!loop) a
              and ob = find_point topology !pending.(!loop) b in
              if ca >= 0 && ob >= 0 then begin
                found_loop := !loop;
                found_current := ca;
                found_other := ob
              end
              else if cb >= 0 && oa >= 0 then begin
                found_loop := !loop;
                found_current := cb;
                found_other := oa
              end;
              incr loop
            done
          end;
          incr edge
        done;
        if !found_loop < 0 then fail "bridge loops are not connected by a selected source edge";
        let left = rotate !current !found_current
        and right = rotate !pending.(!found_loop) !found_other in
        let merged = Array.make (Array.length left + Array.length right + 2) 0 in
        Array.blit left 0 merged 0 (Array.length left);
        merged.(Array.length left) <- left.(0);
        Array.blit right 0 merged (Array.length left + 1) (Array.length right);
        merged.(Array.length merged - 1) <- right.(0);
        current := merged;
        pending :=
          Array.init
            (Array.length !pending - 1)
            (fun output -> !pending.(if output < !found_loop then output else output + 1))
      done;
      !current

type params = {
  cancel : Rdk_core.Cancel.t option;
  grain : int option;
  edges : Rdk_core.Edge_group.t option;
  operation : operation;
  bridge_policy : bridge_policy;
  remove_inline_points : bool;
  collinearity_tolerance : float;
  remove_unused_points : bool;
  create_boundary_curves : bool;
  recompute_normals : bool;
}

let run
    ({
       cancel;
       grain;
       edges;
       operation;
       bridge_policy;
       remove_inline_points;
       collinearity_tolerance;
       remove_unused_points;
       create_boundary_curves;
       recompute_normals;
     } :
      params) geometry =
  let grain = Option.value ~default:16_384 grain in
  Error.guard ~operation:"dissolve" ~code:"invalid_topology" @@ fun () ->
  try
    if grain <= 0 then fail "grain must be positive";
    if
      (not (Float.is_finite collinearity_tolerance))
      || collinearity_tolerance < 0. || collinearity_tolerance > Float.pi
    then fail "collinearity tolerance must be finite and between zero and pi radians";
    Cancel.check_opt cancel;
    let topology_value = Geometry.topology geometry in
    let topology = Topology.View.view topology_value
    and index_value = Topology_index.create ?cancel topology_value in
    let index = Topology_index.View.view index_value in
    let edge_count = Array.length index.edge_a
    and primitive_count = Bytes.length topology.primitive_kinds
    and vertex_count = Array.length topology.vertex_points in
    (match edges with
    | Some group when Edge_group.topology_data_id group <> Topology.data_id topology_value ->
        fail "edge selection belongs to a different topology"
    | Some group when Edge_group.length group <> edge_count ->
        fail "edge selection length does not match topology"
    | _ -> ());
    let selected_edges = Bytes.make ((edge_count + 7) / 8) '\000'
    and affected_points = Bytes.make topology.point_count '\000'
    and selected_count = ref 0 in
    for edge = 0 to edge_count - 1 do
      Cancel.poll cancel edge 4095;
      let requested =
        match (edges, operation) with
        | None, Dissolve_selected -> false
        | None, Dissolve_non_selected -> true
        | Some group, Dissolve_selected -> Edge_group.mem edge group
        | Some group, Dissolve_non_selected -> not (Edge_group.mem edge group)
      in
      if requested then begin
        let first = index.edge_offsets.(edge)
        and last = index.edge_offsets.(edge + 1)
        and polygon = ref true in
        for slot = first to last - 1 do
          let primitive = index.primitive_of_vertex.(index.edge_vertices.(slot)) in
          if Topology.primitive_kind topology_value primitive <> Topology.Polygon then
            polygon := false
        done;
        if !polygon then begin
          if last - first > 2 then fail (Printf.sprintf "selected edge %d is non-manifold" edge);
          set_selected selected_edges edge;
          incr selected_count;
          Bytes.set affected_points index.edge_a.(edge) '\001';
          Bytes.set affected_points index.edge_b.(edge) '\001'
        end
      end
    done;
    if !selected_count = 0 then Ok geometry
    else begin
      let parent = Array.init primitive_count Fun.id in
      let rec find value =
        let ancestor = parent.(value) in
        if ancestor = value then value
        else begin
          let root = find ancestor in
          parent.(value) <- root;
          root
        end
      in
      let union a b =
        let a = find a and b = find b in
        if a <> b then if a < b then parent.(b) <- a else parent.(a) <- b
      in
      for edge = 0 to edge_count - 1 do
        if selected selected_edges edge then begin
          let first = index.edge_offsets.(edge) and last = index.edge_offsets.(edge + 1) in
          if last - first = 2 then begin
            let left_vertex = index.edge_vertices.(first)
            and right_vertex = index.edge_vertices.(first + 1) in
            let left_next = index.next_vertex.(left_vertex)
            and right_next = index.next_vertex.(right_vertex) in
            if
              point_of_vertex topology left_vertex <> point_of_vertex topology right_next
              || point_of_vertex topology left_next <> point_of_vertex topology right_vertex
            then
              fail
                (Printf.sprintf "selected manifold edge %d has inconsistent polygon winding" edge);
            union index.primitive_of_vertex.(left_vertex) index.primitive_of_vertex.(right_vertex)
          end
        end
      done;
      for primitive = 0 to primitive_count - 1 do
        parent.(primitive) <- find primitive
      done;
      let component_selected = Bytes.make primitive_count '\000'
      and component_boundary = Bytes.make primitive_count '\000' in
      for edge = 0 to edge_count - 1 do
        if selected selected_edges edge then begin
          let first = index.edge_offsets.(edge) and last = index.edge_offsets.(edge + 1) in
          if first < last then begin
            let root = parent.(index.primitive_of_vertex.(index.edge_vertices.(first))) in
            Bytes.set component_selected root '\001';
            if last - first = 1 then Bytes.set component_boundary root '\001'
          end
        end
      done;
      let candidate = Bytes.make vertex_count '\000'
      and successor = Array.make vertex_count (-2)
      and predecessor_count = Bytes.make vertex_count '\000'
      and component_vertices = Array.make primitive_count [] in
      for vertex = 0 to vertex_count - 1 do
        let primitive = index.primitive_of_vertex.(vertex) in
        if Topology.primitive_kind topology_value primitive = Topology.Polygon then begin
          let root = parent.(primitive) and edge = index.edge_of_vertex.(vertex) in
          if
            Bytes.get component_selected root = '\001'
            && edge >= 0
            && not (selected selected_edges edge)
          then begin
            Bytes.set candidate vertex '\001';
            component_vertices.(root) <- vertex :: component_vertices.(root)
          end
        end
      done;
      let next_boundary vertex =
        let current = ref index.next_vertex.(vertex) and steps = ref 0 in
        while !current >= 0 && selected selected_edges index.edge_of_vertex.(!current) do
          incr steps;
          if !steps > vertex_count then fail "selected edges form an invalid cycle";
          let edge = index.edge_of_vertex.(!current) in
          let first = index.edge_offsets.(edge) and last = index.edge_offsets.(edge + 1) in
          if last - first = 1 then current := -1
          else begin
            let opposite = index.opposite_vertex.(!current) in
            if opposite < 0 then fail "selected edge has no manifold opposite";
            current := index.next_vertex.(opposite)
          end
        done;
        !current
      in
      for vertex = 0 to vertex_count - 1 do
        if Bytes.get candidate vertex = '\001' then begin
          let next = next_boundary vertex in
          successor.(vertex) <- next;
          if next >= 0 then begin
            if Bytes.get candidate next <> '\001' then
              fail "dissolved component boundary escaped its component";
            let count = Char.code (Bytes.get predecessor_count next) + 1 in
            if count > 1 then fail "dissolved component has branching boundary topology";
            Bytes.set predecessor_count next (Char.chr count)
          end
        end
      done;
      let dense_selection = !selected_count > edge_count / 4 in
      let initial_vertices = if dense_selection then min vertex_count 4_096 else vertex_count
      and initial_primitives =
        if dense_selection then min primitive_count 4_096 else primitive_count
      in
      let visited = Bytes.make vertex_count '\000'
      and output = Output.create initial_vertices initial_primitives
      and processed = Bytes.make primitive_count '\000'
      and positions = Packed.Float3.View.view (Geometry.positions geometry) in
      let trace start =
        let values = Int_buffer.create 16 and current = ref start and closed = ref false in
        while !current >= 0 && Bytes.get visited !current = '\000' do
          Bytes.set visited !current '\001';
          Int_buffer.add values !current;
          let next = successor.(!current) in
          if next = start then begin
            closed := true;
            current := -1
          end
          else current := next
        done;
        if !current >= 0 then fail "dissolved boundary joins an earlier path";
        (Int_buffer.freeze values, !closed)
      in
      let paths root =
        let vertices = Array.of_list component_vertices.(root) in
        Array.sort Int.compare vertices;
        let result = ref [] in
        Array.iter
          (fun vertex ->
            if Bytes.get visited vertex = '\000' && Bytes.get predecessor_count vertex = '\000' then
              result := trace vertex :: !result)
          vertices;
        Array.iter
          (fun vertex ->
            if Bytes.get visited vertex = '\000' then result := trace vertex :: !result)
          vertices;
        List.rev !result
      in
      for primitive = 0 to primitive_count - 1 do
        Cancel.poll cancel primitive 1023;
        let root = parent.(primitive) in
        if Bytes.get component_selected root = '\000' then begin
          let first = topology.primitive_offsets.(primitive)
          and last = topology.primitive_offsets.(primitive + 1) in
          Output.add output ~source:primitive
            ~kind:(Topology.primitive_kind topology_value primitive)
            (Array.init (last - first) (fun local -> first + local))
            topology.vertex_points
        end
        else if Bytes.get processed root = '\000' then begin
          Bytes.set processed root '\001';
          let paths = paths root in
          if Bytes.get component_boundary root = '\001' then begin
            if create_boundary_curves then
              List.iter
                (fun (corners, closed) ->
                  let corners =
                    if closed then corners
                    else begin
                      let result = Array.make (Array.length corners + 1) 0 in
                      Array.blit corners 0 result 0 (Array.length corners);
                      if Array.length corners > 0 then
                        result.(Array.length corners) <-
                          index.next_vertex.(corners.(Array.length corners - 1));
                      result
                    end
                  in
                  let corners =
                    if remove_inline_points then
                      simplify ?cancel ~tolerance:collinearity_tolerance ~affected:affected_points
                        ~closed positions topology corners
                    else corners
                  in
                  Output.add output ~source:root
                    ~kind:(if closed then Topology.Closed_polyline else Topology.Open_polyline)
                    corners topology.vertex_points)
                paths
          end
          else begin
            let loops =
              List.map
                (fun (corners, closed) ->
                  if not closed then fail "interior dissolve produced an open boundary";
                  corners)
                paths
            in
            let emit corners =
              let corners =
                if remove_inline_points then
                  simplify ?cancel ~tolerance:collinearity_tolerance ~affected:affected_points
                    ~closed:true positions topology corners
                else corners
              in
              Output.add output ~source:root ~kind:Topology.Polygon corners topology.vertex_points
            in
            match (loops, bridge_policy) with
            | [], _ -> ()
            | [ loop ], _ -> emit loop
            | loops, Create_disjoint_polygons -> List.iter emit loops
            | _, Delete_bridge_polygons -> ()
            | loops, Create_bridged_polygons ->
                emit (bridge_loops ?cancel topology index selected_edges loops)
          end
        end
      done;
      let vertex_points, vertex_map, primitive_offsets, primitive_map, primitive_kinds =
        Output.freeze output
      in
      let output_topology =
        Topology.create_owned ~point_count:topology.point_count ~vertex_points ~primitive_offsets
          ~primitive_kinds
        |> get_ok
      in
      let had_point_normals = Geometry.find_attribute ~owner:Attribute.Point "N" geometry <> None
      and had_vertex_normals =
        Geometry.find_attribute ~owner:Attribute.Vertex "N" geometry <> None
      in
      let output =
        Topology_remap.preserving_points ?cancel ~grain ~topology:output_topology ~vertex_map
          ~primitive_map geometry
        |> get_ok
      in
      let output =
        if remove_unused_points then
          Fuse_cleanup.apply ?cancel ~grain ~remove_degenerate_primitives:false
            ~remove_unused_points_from_degenerate_primitives:false ~remove_all_unused_points:true
            output
          |> get_ok
        else output
      in
      if (not recompute_normals) || not (had_point_normals || had_vertex_normals) then Ok output
      else begin
        let output =
          output
          |> Geometry.without_attribute ~owner:Attribute.Point "N"
          |> Geometry.without_attribute ~owner:Attribute.Vertex "N"
        in
        Ok
          (Error.get
             (Normal_ops.run
                {
                  cancel;
                  grain = Some grain;
                  selection = None;
                  primitives = None;
                  owner = (if had_point_normals then Attribute.Point else Attribute.Vertex);
                  weighting = Face_area;
                  cusp_angle = Float.pi;
                  attribute = "N";
                }
                output))
      end
    end
  with
  | Error.Fail message -> Error message
  | Cancel.Cancelled -> raise Cancel.Cancelled
