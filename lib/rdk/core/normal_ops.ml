
type weighting = Vertex_angle | Each_vertex | Face_area

type planes = {
  x : float array;
  y : float array;
  z : float array;
}

type faces = {
  raw : planes;
  inverse_length : float array;
}

let operation = "Rdk.Normal_ops.normals"

let[@inline] max_abs3 x y z =
  let x = abs_float x and y = abs_float y and z = abs_float z in
  let value = if x > y then x else y in
  if value > z then value else z

let validate_primitives topology = function
  | None -> Ok ()
  | Some group when Group.owner group <> Group.Primitive ->
      Error (operation ^ ": contribution selection must own primitives")
  | Some group when Group.length group <> Topology.primitive_count topology ->
      Error (operation ^ ": contribution selection length does not match primitive count")
  | Some _ -> Ok ()

let compute_faces ?cancel ~grain ~need_inverse ?primitives geometry =
  Result.map (fun ((x, y, z), inverse_length) ->
    { raw = { x; y; z }; inverse_length })
    (Face_normals.polygon_area_vector ?cancel ~grain ~need_inverse
      ~first_failure:false ?primitives ~operation geometry)

let[@inline always] corner_angle (positions : Packed.Float3.View.view)
    (topology : Topology.View.view) primitive vertex =
  let first = topology.Topology.View.primitive_offsets.(primitive)
  and last = topology.primitive_offsets.(primitive + 1) in
  let previous = if vertex = first then last - 1 else vertex - 1
  and next = if vertex + 1 = last then first else vertex + 1 in
  let point = topology.vertex_points.(vertex)
  and previous_point = topology.vertex_points.(previous)
  and next_point = topology.vertex_points.(next) in
  let ux = positions.x.(previous_point) -. positions.x.(point)
  and uy = positions.y.(previous_point) -. positions.y.(point)
  and uz = positions.z.(previous_point) -. positions.z.(point)
  and vx = positions.x.(next_point) -. positions.x.(point)
  and vy = positions.y.(next_point) -. positions.y.(point)
  and vz = positions.z.(next_point) -. positions.z.(point) in
  let us = max_abs3 ux uy uz and vs = max_abs3 vx vy vz in
  if us = 0. || vs = 0. then 0.
  else
    let ux = ux /. us and uy = uy /. us and uz = uz /. us
    and vx = vx /. vs and vy = vy /. vs and vz = vz /. vs in
    let cx = (uy *. vz) -. (uz *. vy)
    and cy = (uz *. vx) -. (ux *. vz)
    and cz = (ux *. vy) -. (uy *. vx) in
    atan2 (sqrt ((cx *. cx) +. (cy *. cy) +. (cz *. cz)))
      ((ux *. vx) +. (uy *. vy) +. (uz *. vz))

let accumulate_points ?cancel ~weighting geometry faces =
  let topology = Topology.View.view (Geometry.topology geometry)
  and positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let count = topology.point_count in
  let x = Array.make count 0. and y = Array.make count 0.
  and z = Array.make count 0. in
  let primitive_count = Array.length topology.primitive_offsets - 1 in
  (match weighting with
   | Face_area ->
      for primitive = 0 to primitive_count - 1 do
        Cancel.poll cancel primitive 4095;
        if Bytes.get topology.primitive_kinds primitive = '\000' then begin
          let first = topology.primitive_offsets.(primitive)
          and last = topology.primitive_offsets.(primitive + 1) in
          for vertex = first to last - 1 do
            let point = topology.vertex_points.(vertex) in
            x.(point) <- x.(point) +. faces.raw.x.(primitive);
            y.(point) <- y.(point) +. faces.raw.y.(primitive);
            z.(point) <- z.(point) +. faces.raw.z.(primitive)
          done
        end
      done
   | Each_vertex ->
      for primitive = 0 to primitive_count - 1 do
        Cancel.poll cancel primitive 4095;
        if Bytes.get topology.primitive_kinds primitive = '\000' then begin
          let inverse = faces.inverse_length.(primitive) in
          if inverse <> 0. then begin
            let fx = faces.raw.x.(primitive) *. inverse
            and fy = faces.raw.y.(primitive) *. inverse
            and fz = faces.raw.z.(primitive) *. inverse
            and first = topology.primitive_offsets.(primitive)
            and last = topology.primitive_offsets.(primitive + 1) in
            for vertex = first to last - 1 do
              let point = topology.vertex_points.(vertex) in
              x.(point) <- x.(point) +. fx;
              y.(point) <- y.(point) +. fy;
              z.(point) <- z.(point) +. fz
            done
          end
        end
      done
   | Vertex_angle ->
      for primitive = 0 to primitive_count - 1 do
        Cancel.poll cancel primitive 4095;
        if Bytes.get topology.primitive_kinds primitive = '\000' then begin
          let inverse = faces.inverse_length.(primitive) in
          if inverse <> 0. then begin
        let first = topology.primitive_offsets.(primitive)
        and last = topology.primitive_offsets.(primitive + 1) in
        for vertex = first to last - 1 do
          let point = topology.vertex_points.(vertex) in
          let weight = inverse *. corner_angle positions topology primitive vertex in
          x.(point) <- x.(point) +. (faces.raw.x.(primitive) *. weight);
          y.(point) <- y.(point) +. (faces.raw.y.(primitive) *. weight);
          z.(point) <- z.(point) +. (faces.raw.z.(primitive) *. weight)
        done
      end
        end
      done);
  { x; y; z }

let selection_group ?cancel ~grain topology point_index edge_index selection owner =
  let source = Topology.View.view topology in
  let need_point_reverse = match owner, selection with
    | Attribute.Point, (Element_selection.Selected_vertices _
        | Selected_primitives _)
    | Attribute.Vertex, Selected_primitives _ -> true
    | (Attribute.Point, Selected_points _
      | Attribute.Point, Selected_edges _
      | Attribute.Vertex, (Selected_points _ | Selected_vertices _ | Selected_edges _)
      | Attribute.Primitive, (Selected_points _ | Selected_vertices _
          | Selected_primitives _ | Selected_edges _)
      | Attribute.Detail, _) -> false in
  let point_reverse = if need_point_reverse then
      Some (Point_index.View.view (Lazy.force point_index)) else None in
  let edge_reverse = match selection, owner with
    | Element_selection.Selected_edges _, (Attribute.Point | Vertex | Primitive) ->
        Some (Topology_index.View.view (Lazy.force edge_index))
    | _ -> None in
  let point_reverse () = Option.get point_reverse
  and edge_reverse () = Option.get edge_reverse in
  let cancelled element = Cancel.poll cancel element 4095 in
  let point_selected point =
    cancelled point;
    match selection with
    | Element_selection.Selected_points group -> Group.mem point group
    | Selected_vertices group ->
        let reverse = point_reverse () in
        let found = ref false and at = ref reverse.point_offsets.(point) in
        let last = reverse.point_offsets.(point + 1) in
        while not !found && !at < last do
          found := Group.mem reverse.point_vertices.(!at) group; incr at
        done;
        !found
    | Selected_primitives group ->
        let reverse = point_reverse () in
        let found = ref false and at = ref reverse.point_offsets.(point) in
        let last = reverse.point_offsets.(point + 1) in
        while not !found && !at < last do
          found := Group.mem
              reverse.primitive_of_vertex.(reverse.point_vertices.(!at)) group;
          incr at
        done;
        !found
    | Selected_edges group ->
        let reverse = edge_reverse () in
        let found = ref false and at = ref reverse.point_edge_offsets.(point) in
        let last = reverse.point_edge_offsets.(point + 1) in
        while not !found && !at < last do
          found := Edge_group.mem reverse.point_edges.(!at) group; incr at
        done;
        !found in
  let vertex_selected vertex =
    cancelled vertex;
    match selection with
    | Element_selection.Selected_points group ->
        Group.mem source.vertex_points.(vertex) group
    | Selected_vertices group -> Group.mem vertex group
    | Selected_primitives group ->
        let reverse = point_reverse () in
        Group.mem reverse.primitive_of_vertex.(vertex) group
    | Selected_edges group ->
        let reverse = edge_reverse () in
        let outgoing = reverse.edge_of_vertex.(vertex)
        and previous = reverse.previous_vertex.(vertex) in
        (outgoing >= 0 && Edge_group.mem outgoing group)
        || (previous >= 0 && reverse.edge_of_vertex.(previous) >= 0
            && Edge_group.mem reverse.edge_of_vertex.(previous) group) in
  let primitive_selected primitive =
    cancelled primitive;
    match selection with
    | Element_selection.Selected_primitives group -> Group.mem primitive group
    | Selected_points group ->
        let found = ref false
        and vertex = ref source.primitive_offsets.(primitive) in
        let last = source.primitive_offsets.(primitive + 1) in
        while not !found && !vertex < last do
          found := Group.mem source.vertex_points.(!vertex) group; incr vertex
        done;
        !found
    | Selected_vertices group ->
        let found = ref false
        and vertex = ref source.primitive_offsets.(primitive) in
        let last = source.primitive_offsets.(primitive + 1) in
        while not !found && !vertex < last do
          found := Group.mem !vertex group; incr vertex
        done;
        !found
    | Selected_edges group ->
        let reverse = edge_reverse () in
        let found = ref false
        and vertex = ref source.primitive_offsets.(primitive) in
        let last = source.primitive_offsets.(primitive + 1) in
        while not !found && !vertex < last do
          let edge = reverse.edge_of_vertex.(!vertex) in
          found := edge >= 0 && Edge_group.mem edge group; incr vertex
        done;
        !found in
  match owner with
  | Attribute.Detail -> None
  | Point -> Some (Group.init ~grain ~owner:Group.Point ~name:"normal_selection"
      source.point_count point_selected)
  | Vertex -> Some (Group.init ~grain ~owner:Group.Vertex ~name:"normal_selection"
      (Array.length source.vertex_points) vertex_selected)
  | Primitive -> Some (Group.init ~grain ~owner:Group.Primitive
      ~name:"normal_selection"
      (Array.length source.primitive_offsets - 1) primitive_selected)

let existing_planes ~owner ~attribute geometry =
  match Geometry.find_attribute ~owner attribute geometry with
  | None -> Ok None
  | Some value ->
      (match Attribute.View.storage value with
       | Attribute.Float3 values ->
           let values = Packed.Float3.View.view values in
           Ok (Some { x = values.x; y = values.y; z = values.z })
       | _ -> Error (Printf.sprintf "%s: %s %s must have float3 storage"
           operation (match owner with Point -> "point" | Vertex -> "vertex"
             | Primitive -> "primitive" | Detail -> "detail") attribute))

let initial count = function
  | None -> { x = Array.make count 0.; y = Array.make count 0.;
      z = Array.make count 0. }
  | Some values -> { x = Array.copy values.x; y = Array.copy values.y;
      z = Array.copy values.z }

let[@inline always] store ~keep_original_zero ~reverse ~existing output index vx vy vz =
  if not (Float.is_finite vx && Float.is_finite vy && Float.is_finite vz) then false
  else
    let scale = max_abs3 vx vy vz in
    if scale = 0. then begin
      if not keep_original_zero || existing = None then begin
        output.x.(index) <- 0.; output.y.(index) <- 0.; output.z.(index) <- 0.
      end;
      true
    end else begin
      let sx = vx /. scale and sy = vy /. scale and sz = vz /. scale in
      let inverse = (if reverse then -1. else 1.)
          /. sqrt ((sx *. sx) +. (sy *. sy) +. (sz *. sz)) in
      output.x.(index) <- sx *. inverse;
      output.y.(index) <- sy *. inverse;
      output.z.(index) <- sz *. inverse;
      true
    end

let write_points ?cancel ~grain ~selection ~keep_original_zero ~reverse
    ~existing output sums =
  let count = Array.length output.x in
  let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
  let errors = Array.make ranges (-1) in
  Support.run_indexed_ranges ~grain count (fun ~range ~first ~last ->
    for point = first to last - 1 do
      Cancel.poll cancel point 4095;
      if match selection with None -> true | Some group -> Group.mem point group then begin
        let vx = sums.x.(point) and vy = sums.y.(point) and vz = sums.z.(point) in
        if not (Float.is_finite vx && Float.is_finite vy && Float.is_finite vz)
        then errors.(range) <- point
        else
          let scale = max_abs3 vx vy vz in
          if scale = 0. then begin
            if not keep_original_zero || existing = None then begin
              output.x.(point) <- 0.; output.y.(point) <- 0.; output.z.(point) <- 0.
            end
          end else begin
            let sx = vx /. scale and sy = vy /. scale and sz = vz /. scale in
            let inverse = (if reverse then -1. else 1.)
                /. sqrt ((sx *. sx) +. (sy *. sy) +. (sz *. sz)) in
            output.x.(point) <- sx *. inverse;
            output.y.(point) <- sy *. inverse;
            output.z.(point) <- sz *. inverse
          end
      end
    done);
  match Array.find_opt (fun point -> point >= 0) errors with
  | None -> Ok ()
  | Some point -> Error (Printf.sprintf "%s: non-finite point normal at %d"
      operation point)

let write_smooth_vertices ?cancel ~grain ~selection ~keep_original_zero ~reverse
    ~existing topology output sums =
  let source = Topology.View.view topology in
  let count = Array.length source.vertex_points in
  let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
  let errors = Array.make ranges (-1) in
  Support.run_indexed_ranges ~grain count (fun ~range ~first ~last ->
    for vertex = first to last - 1 do
      Cancel.poll cancel vertex 4095;
      if match selection with None -> true | Some group -> Group.mem vertex group then
        let point = source.vertex_points.(vertex) in
        if not (store ~keep_original_zero ~reverse ~existing output vertex
            sums.x.(point) sums.y.(point) sums.z.(point)) then
          errors.(range) <- vertex
    done);
  match Array.find_opt (fun vertex -> vertex >= 0) errors with
  | None -> Ok ()
  | Some vertex -> Error (Printf.sprintf "%s: non-finite vertex normal at %d"
      operation vertex)

let write_cusped_vertices ?cancel ~grain ~weighting ~cusp_angle ~selection
    ~keep_original_zero ~reverse ~existing geometry index faces output =
  let topology = Topology.View.view (Geometry.topology geometry)
  and positions = Packed.Float3.View.view (Geometry.positions geometry)
  and reverse_topology = Point_index.View.view index in
  let count = Array.length topology.vertex_points in
  let angles = match weighting with
    | Vertex_angle ->
        let values = Array.make count 0. in
        Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(count - 1) (fun vertex ->
          Cancel.poll cancel vertex 4095;
          let primitive = reverse_topology.primitive_of_vertex.(vertex) in
          if Bytes.get topology.primitive_kinds primitive = '\000' then
            values.(vertex) <- corner_angle positions topology primitive vertex);
        values
    | Each_vertex | Face_area -> [||] in
  let cosine = cos cusp_angle in
  let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
  let errors = Array.make ranges (-1) in
  Support.run_indexed_ranges ~grain count (fun ~range ~first ~last ->
    for vertex = first to last - 1 do
      Cancel.poll cancel vertex 4095;
      if match selection with None -> true | Some group -> Group.mem vertex group then begin
        let primitive = reverse_topology.primitive_of_vertex.(vertex) in
        let current_inverse = faces.inverse_length.(primitive) in
        let nx = ref 0. and ny = ref 0. and nz = ref 0. in
        if current_inverse <> 0. then begin
          let current_x = faces.raw.x.(primitive) *. current_inverse
          and current_y = faces.raw.y.(primitive) *. current_inverse
          and current_z = faces.raw.z.(primitive) *. current_inverse
          and point = topology.vertex_points.(vertex) in
          let at = ref reverse_topology.point_offsets.(point)
          and incidence_last = reverse_topology.point_offsets.(point + 1) in
          while !at < incidence_last do
            let source_vertex = reverse_topology.point_vertices.(!at) in
            let source_primitive =
              reverse_topology.primitive_of_vertex.(source_vertex) in
            let inverse = faces.inverse_length.(source_primitive) in
            if inverse <> 0. then begin
              let x = faces.raw.x.(source_primitive) *. inverse
              and y = faces.raw.y.(source_primitive) *. inverse
              and z = faces.raw.z.(source_primitive) *. inverse in
              let dot = (current_x *. x) +. (current_y *. y) +. (current_z *. z) in
              if dot +. 1e-12 >= cosine then begin
                let weight = match weighting with
                  | Each_vertex -> inverse
                  | Face_area -> 1.
                  | Vertex_angle -> inverse *. angles.(source_vertex) in
                nx := !nx +. (faces.raw.x.(source_primitive) *. weight);
                ny := !ny +. (faces.raw.y.(source_primitive) *. weight);
                nz := !nz +. (faces.raw.z.(source_primitive) *. weight)
              end
            end;
            incr at
          done
        end;
        if not (store ~keep_original_zero ~reverse ~existing output vertex
            !nx !ny !nz) then errors.(range) <- vertex
      end
    done);
  match Array.find_opt (fun vertex -> vertex >= 0) errors with
  | None -> Ok ()
  | Some vertex -> Error (Printf.sprintf "%s: non-finite vertex normal at %d"
      operation vertex)

let install ~owner ~attribute values geometry =
  let geometry = List.fold_left (fun geometry owner ->
      Geometry.without_attribute ~owner attribute geometry)
      geometry [Attribute.Point; Vertex; Primitive; Detail] in
  let packed = Packed.Float3.View.of_owned_exn
      ~x:values.x ~y:values.y ~z:values.z in
  Result.bind (Attribute.create_owned ~owner ~name:attribute
      (Attribute.Float3 packed)) (fun value -> Geometry.with_attribute value geometry)

(* The compatibility profile is the dominant deformation/rendering path. It
   deliberately fuses area accumulation and normalization into the final
   point planes: the general owner-aware planner would otherwise retain a
   second point-sized buffer and box cross-function float arguments. *)
let fast_point_area ?cancel ~grain geometry =
  let topology = Topology.View.view (Geometry.topology geometry) in
  match Face_normals.polygon_area_vector ?cancel ~grain ~need_inverse:false
      ~first_failure:true ~operation geometry with
  | Error _ as error -> error
  | Ok ((face_x, face_y, face_z), _) ->
      let point_count = topology.point_count in
      let x = Array.make point_count 0. and y = Array.make point_count 0.
      and z = Array.make point_count 0. in
      let primitive = ref 0 in
      for vertex = 0 to Array.length topology.vertex_points - 1 do
        Cancel.poll cancel vertex 16_383;
        while vertex >= topology.primitive_offsets.(!primitive + 1) do
          incr primitive
        done;
        let point = topology.vertex_points.(vertex) in
        x.(point) <- x.(point) +. face_x.(!primitive);
        y.(point) <- y.(point) +. face_y.(!primitive);
        z.(point) <- z.(point) +. face_z.(!primitive)
      done;
      let point_ranges = if point_count = 0 then 0
        else (point_count + grain - 1) / grain in
      let point_errors = Array.make point_ranges (-1) in
      Support.run_indexed_ranges ~grain point_count (fun ~range ~first ~last ->
        for point = first to last - 1 do
          Cancel.poll cancel point 4095;
          let vx = x.(point) and vy = y.(point) and vz = z.(point) in
          if not (Float.is_finite vx && Float.is_finite vy && Float.is_finite vz)
          then point_errors.(range) <- point
          else begin
            let scale = max_abs3 vx vy vz in
            if scale > 0. then begin
              let sx = vx /. scale and sy = vy /. scale and sz = vz /. scale in
              let inverse = 1. /. sqrt ((sx *. sx) +. (sy *. sy) +. (sz *. sz)) in
              x.(point) <- sx *. inverse;
              y.(point) <- sy *. inverse;
              z.(point) <- sz *. inverse
            end
          end
        done);
      (match Array.find_opt (fun point -> point >= 0) point_errors with
       | Some point -> Error (Printf.sprintf "%s: non-finite point normal at %d"
           operation point)
       | None -> install ~owner:Attribute.Point ~attribute:"N" { x; y; z } geometry)

type params = {
  cancel : Cancel.t option;
  grain : int option;
  selection : Element_selection.t option;
  primitives : Group.t option;
  owner : Attribute.owner;
  weighting : weighting;
  cusp_angle : float;
  attribute : string;
}

let run ({ cancel; grain; selection; primitives; owner; weighting; cusp_angle; attribute } : params) geometry =
  let grain = Option.value ~default:16_384 grain in
  let keep_original_zero = (false) in let reverse = (false) in Error.guard ~operation:"normals" ~code:"invalid_topology" @@ fun () ->
  if grain <= 0 then Error (operation ^ ": grain must be positive")
  else if String.trim attribute = "" then Error
      (operation ^ ": attribute name must not be empty")
  else if not (Float.is_finite cusp_angle) || cusp_angle < 0.
      || cusp_angle > Float.pi then Error
      (operation ^ ": cusp angle must be finite and in [0, pi]")
  else begin
    Cancel.check_opt cancel;
    if selection = None && primitives = None && owner = Attribute.Point
       && weighting = Face_area && cusp_angle = Float.pi
       && not keep_original_zero && not reverse && String.equal attribute "N"
       && (match Geometry.find_attribute ~owner:Attribute.Point "N" geometry with
           | None -> true
           | Some value -> (match Attribute.View.storage value with
               | Attribute.Float3 _ -> true | _ -> false))
    then fast_point_area ?cancel ~grain geometry
    else
    let topology = Geometry.topology geometry in
    Result.bind (Element_selection.validate ~operation topology selection) (fun () ->
    Result.bind (validate_primitives topology primitives) (fun () ->
    Result.bind (existing_planes ~owner ~attribute geometry) (fun existing ->
      let point_index = lazy (Point_index.create ?cancel topology)
      and edge_index = lazy (Topology_index.create ?cancel topology) in
      let authored_selection = Option.bind selection (fun selection ->
          selection_group ?cancel ~grain topology point_index edge_index
            selection owner) in
      let effective_selection = match existing, owner with
        | None, Attribute.Vertex -> authored_selection
        | None, _ -> None
        | Some _, _ -> authored_selection in
      let need_inverse = owner = Attribute.Vertex || owner = Attribute.Primitive
          || weighting <> Face_area in
      Result.bind (compute_faces ?cancel ~grain ~need_inverse ?primitives geometry)
        (fun faces ->
        match owner with
        | Attribute.Point ->
            let sums = accumulate_points ?cancel ~weighting geometry faces in
            let output = if effective_selection = None
                && (not keep_original_zero || existing = None) then sums
              else initial (Geometry.point_count geometry)
                  (if effective_selection <> None || keep_original_zero
                   then existing else None) in
            Result.bind (write_points ?cancel ~grain ~selection:effective_selection
                ~keep_original_zero ~reverse ~existing output sums) (fun () ->
              install ~owner ~attribute output geometry)
        | Attribute.Vertex ->
            let output = initial (Geometry.vertex_count geometry) existing in
            let write_smooth ~selection ~keep_original_zero ~reverse ~existing =
              let sums = accumulate_points ?cancel ~weighting geometry faces in
              write_smooth_vertices ?cancel ~grain ~selection ~keep_original_zero
                ~reverse ~existing topology output sums in
            let write_requested ~selection ~keep_original_zero ~reverse ~existing =
              if cusp_angle >= Float.pi -. 1e-12 then
                write_smooth ~selection ~keep_original_zero ~reverse ~existing
              else write_cusped_vertices ?cancel ~grain ~weighting ~cusp_angle
                  ~selection ~keep_original_zero ~reverse ~existing geometry
                  (Lazy.force point_index) faces output in
            let computed = match existing, effective_selection with
              | None, Some selection ->
                  Result.bind (write_smooth ~selection:None ~keep_original_zero:false
                      ~reverse:false ~existing:None) (fun () ->
                    write_requested ~selection:(Some selection) ~keep_original_zero:false
                      ~reverse ~existing:None)
              | _ -> write_requested ~selection:effective_selection
                  ~keep_original_zero ~reverse ~existing in
            Result.bind computed (fun () -> install ~owner ~attribute output geometry)
        | Attribute.Primitive ->
            let count = Geometry.primitive_count geometry in
            let output = initial count
                (if effective_selection <> None || keep_original_zero
                 then existing else None) in
            let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
            let errors = Array.make ranges (-1) in
            Support.run_indexed_ranges ~grain count (fun ~range ~first ~last ->
              for primitive = first to last - 1 do
                Cancel.poll cancel primitive 4095;
                if match effective_selection with None -> true
                    | Some group -> Group.mem primitive group then
                  if not (store ~keep_original_zero ~reverse ~existing output primitive
                      faces.raw.x.(primitive) faces.raw.y.(primitive)
                      faces.raw.z.(primitive)) then errors.(range) <- primitive
              done);
            (match Array.find_opt (fun value -> value >= 0) errors with
             | Some primitive -> Error (Printf.sprintf
                 "%s: non-finite primitive normal at %d" operation primitive)
             | None -> install ~owner ~attribute output geometry)
        | Attribute.Detail ->
            let sx = ref 0. and sy = ref 0. and sz = ref 0. in
            for primitive = 0 to Geometry.primitive_count geometry - 1 do
              Cancel.poll cancel primitive 4095;
              sx := !sx +. faces.raw.x.(primitive);
              sy := !sy +. faces.raw.y.(primitive);
              sz := !sz +. faces.raw.z.(primitive)
            done;
            let output = initial 1 (if keep_original_zero then existing else None) in
            if store ~keep_original_zero ~reverse ~existing output 0 !sx !sy !sz
            then install ~owner ~attribute output geometry
            else Error (operation ^ ": non-finite detail normal")))))
  end
