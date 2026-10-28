
type selection = Element_selection.t =
  | Selected_points of Group.t
  | Selected_vertices of Group.t
  | Selected_primitives of Group.t
  | Selected_edges of Edge_group.t

type planes = {
  x : float array;
  y : float array;
  z : float array;
}

let finite_vec3 value =
  Float.is_finite value.Rays_math.Vec3.x && Float.is_finite value.y
  && Float.is_finite value.z

let[@inline] max_abs3 x y z =
  let x = abs_float x and y = abs_float y and z = abs_float z in
  let value = if x > y then x else y in
  if value > z then value else z

let validate_selection topology = function
  | None -> Ok ()
  | Some (Selected_points group) ->
      if Group.owner group <> Group.Point then Error
          "Rdk.Deform: point selection must own points"
      else if Group.length group <> Topology.point_count topology then Error
          "Rdk.Deform: point selection length does not match point count"
      else Ok ()
  | Some (Selected_vertices group) ->
      if Group.owner group <> Group.Vertex then Error
          "Rdk.Deform: vertex selection must own vertices"
      else if Group.length group <> Topology.vertex_count topology then Error
          "Rdk.Deform: vertex selection length does not match vertex count"
      else Ok ()
  | Some (Selected_primitives group) ->
      if Group.owner group <> Group.Primitive then Error
          "Rdk.Deform: primitive selection must own primitives"
      else if Group.length group <> Topology.primitive_count topology then Error
          "Rdk.Deform: primitive selection length does not match primitive count"
      else Ok ()
  | Some (Selected_edges group) ->
      if Edge_group.topology_data_id group <> Topology.data_id topology then Error
          "Rdk.Deform: edge selection belongs to different topology"
      else Ok ()

let selection_needs_index = function
  | Some (Selected_vertices _ | Selected_primitives _ | Selected_edges _) -> true
  | None | Some (Selected_points _) -> false

let[@inline] point_selected selection index point = match selection with
  | None -> true
  | Some (Selected_points group) -> Group.mem point group
  | Some (Selected_vertices group) ->
      let index = Option.get index in
      let count = Topology_index.point_incidence_count index point in
      let local = ref 0 and found = ref false in
      while !local < count && not !found do
        found := Group.mem (Topology_index.point_vertex index ~point ~local:!local)
            group;
        incr local
      done;
      !found
  | Some (Selected_primitives group) ->
      let index = Option.get index in
      let count = Topology_index.point_incidence_count index point in
      let local = ref 0 and found = ref false in
      while !local < count && not !found do
        let vertex = Topology_index.point_vertex index ~point ~local:!local in
        found := Group.mem (Topology_index.primitive_of_vertex index vertex) group;
        incr local
      done;
      !found
  | Some (Selected_edges group) ->
      let index = Option.get index in
      let count = Topology_index.point_edge_count index point in
      let local = ref 0 and found = ref false in
      while !local < count && not !found do
        found := Edge_group.mem (Topology_index.point_edge index ~point
            ~local:!local) group;
        incr local
      done;
      !found

let point_float_attribute operation name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Error (Printf.sprintf "%s: missing point float attribute %s"
      operation name)
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float values -> Ok values
       | _ -> Error (Printf.sprintf
           "%s: point attribute %s must have float storage" operation name))

let point_vector_attribute operation name geometry =
  if String.equal name "P" then
    let values = Packed.Float3.Private.view (Geometry.positions geometry) in
    Ok { x = values.x; y = values.y; z = values.z }
  else match Geometry.find_attribute ~owner:Attribute.Point name geometry with
    | None -> Error (Printf.sprintf "%s: missing point float3 attribute %s"
        operation name)
    | Some attribute ->
        (match Attribute.Private.storage attribute with
         | Attribute.Float3 values ->
             let values = Packed.Float3.Private.view values in
             Ok { x = values.x; y = values.y; z = values.z }
         | _ -> Error (Printf.sprintf
             "%s: point attribute %s must have float3 storage" operation name))

let vertex_normals geometry =
  match Geometry.find_attribute ~owner:Attribute.Vertex "N" geometry with
  | None -> Ok None
  | Some attribute ->
      (match Attribute.Private.storage attribute with
       | Attribute.Float3 values ->
           let values = Packed.Float3.Private.view values in
           Ok (Some { x = values.x; y = values.y; z = values.z })
       | _ -> Error "Rdk.Deform: vertex N must have float3 storage")

let face_vectors ?cancel ~grain geometry =
  Result.map (fun ((x, y, z), _) -> { x; y; z })
    (Face_normals.polygon_area_vector ?cancel ~grain ~need_inverse:false
      ~first_failure:true ~operation:"Rdk.Deform" geometry)

let point_vectors_from_vertices ?cancel ~grain topology source =
  let topology_view = Topology.Private.view topology in
  let point_count = topology_view.point_count in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. and incidence = Array.make point_count 0 in
  for vertex = 0 to Array.length topology_view.vertex_points - 1 do
    if vertex land 16_383 = 0 then Cancel.check_opt cancel;
    let point = topology_view.vertex_points.(vertex) in
    x.(point) <- x.(point) +. source.x.(vertex);
    y.(point) <- y.(point) +. source.y.(vertex);
    z.(point) <- z.(point) +. source.z.(vertex);
    incidence.(point) <- incidence.(point) + 1
  done;
  Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(point_count - 1)
    (fun point ->
      if point land 4095 = 0 then Cancel.check_opt cancel;
      let count = incidence.(point) in
      if count > 1 then begin
        let inverse = 1. /. float_of_int count in
        x.(point) <- x.(point) *. inverse;
        y.(point) <- y.(point) *. inverse;
        z.(point) <- z.(point) *. inverse
      end);
  { x; y; z }

let geometric_point_vectors ?cancel ~grain geometry =
  Result.map (fun faces ->
    let topology = Geometry.topology geometry in
    let topology_view = Topology.Private.view topology in
    let point_count = topology_view.point_count in
    let x = Array.make point_count 0. and y = Array.make point_count 0.
    and z = Array.make point_count 0. in
    let primitive = ref 0 in
    for vertex = 0 to Array.length topology_view.vertex_points - 1 do
      if vertex land 16_383 = 0 then Cancel.check_opt cancel;
      while vertex >= topology_view.primitive_offsets.(!primitive + 1) do
        incr primitive
      done;
      let point = topology_view.vertex_points.(vertex) in
      x.(point) <- x.(point) +. faces.x.(!primitive);
      y.(point) <- y.(point) +. faces.y.(!primitive);
      z.(point) <- z.(point) +. faces.z.(!primitive)
    done;
    { x; y; z }) (face_vectors ?cancel ~grain geometry)

let resolve_directions ?cancel ~grain ?direction_attribute geometry =
  match direction_attribute with
  | Some name when String.trim name = "" -> Error
      "Rdk.Deform: direction attribute name must not be empty"
  | Some name -> point_vector_attribute "Rdk.Deform" name geometry
  | None ->
      (match Geometry.find_attribute ~owner:Attribute.Point "N" geometry with
       | Some attribute ->
           (match Attribute.Private.storage attribute with
            | Attribute.Float3 values ->
                let values = Packed.Float3.Private.view values in
                Ok { x = values.x; y = values.y; z = values.z }
            | _ -> Error "Rdk.Deform: point N must have float3 storage")
       | None ->
           Result.bind (vertex_normals geometry) (function
             | Some values -> Ok (point_vectors_from_vertices ?cancel
                 ~grain (Geometry.topology geometry) values)
             | None -> geometric_point_vectors ?cancel ~grain geometry))

let with_point_normals ?cancel ~grain ?primitives geometry =
  let selection = Option.map (fun group -> Selected_primitives group) primitives in
  Ok (Error.get (Normal_ops.run ?cancel ~grain ?selection ?primitives
    ~owner:Attribute.Point ~weighting:Normal_ops.Face_area geometry))

let normals ?cancel ~grain ?primitives geometry =
  if grain <= 0 then Error "Rdk_mesh.Deform.normals: grain must be positive"
  else with_point_normals ?cancel ~grain ?primitives geometry

let prepare ?cancel ~grain ?selection ?direction_attribute ?mask_attribute geometry =
  let topology = Geometry.topology geometry in
  Result.bind (validate_selection topology selection) (fun () ->
    let needs_index = selection_needs_index selection in
    let index = if needs_index then
        Some (Topology_index.create ?cancel topology) else None in
    Result.bind (resolve_directions ?cancel ~grain ?direction_attribute geometry)
      (fun directions ->
      Result.map (fun mask -> index, directions, mask) (match mask_attribute with
        | None -> Ok None
        | Some name when String.trim name = "" -> Error
            "Rdk.Deform: mask attribute name must not be empty"
        | Some name -> Result.map Option.some
            (point_float_attribute "Rdk.Deform" name geometry))))

let validate_noop ?selection ?direction_attribute ?mask_attribute geometry =
  Result.bind (validate_selection (Geometry.topology geometry) selection) (fun () ->
    Result.bind (match direction_attribute with
      | Some name when String.trim name = "" -> Error
          "Rdk.Deform: direction attribute name must not be empty"
      | Some name -> Result.map ignore
          (point_vector_attribute "Rdk.Deform" name geometry)
      | None ->
          (match Geometry.find_attribute ~owner:Attribute.Point "N" geometry with
           | Some attribute ->
               (match Attribute.Private.storage attribute with
                | Attribute.Float3 _ -> Ok ()
                | _ -> Error "Rdk.Deform: point N must have float3 storage")
           | None -> Result.map ignore (vertex_normals geometry))) (fun () ->
      match mask_attribute with
      | None -> Ok ()
      | Some name when String.trim name = "" -> Error
          "Rdk.Deform: mask attribute name must not be empty"
      | Some name -> Result.map ignore
          (point_float_attribute "Rdk.Deform" name geometry)))

let finish_positions ?cancel ~grain ~recompute_normals geometry x y z =
  let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
  Result.bind (Geometry.with_positions positions geometry) (fun geometry ->
    if recompute_normals then with_point_normals ?cancel ~grain geometry
    else Ok (geometry
      |> Geometry.without_attribute ~owner:Attribute.Point "N"
      |> Geometry.without_attribute ~owner:Attribute.Vertex "N"))

let peak ?cancel ?(grain = 16_384) ?selection ?direction_attribute
    ?(normalize_direction = true) ?mask_attribute ~distance
    ?(recompute_normals = false) geometry =
  Error.guard ~operation:"peak" ~code:"invalid_deformation" @@ fun () ->
  if grain <= 0 then Error "Rdk_mesh.Deform.peak: grain must be positive"
  else if not (Float.is_finite distance) then Error
      "Rdk_mesh.Deform.peak: distance must be finite"
  else if distance = 0. then
    Result.bind (validate_noop ?selection ?direction_attribute ?mask_attribute
        geometry) (fun () ->
      if recompute_normals then with_point_normals ?cancel ~grain geometry
      else Ok geometry)
  else Result.bind (prepare ?cancel ~grain ?selection ?direction_attribute
      ?mask_attribute geometry) (fun (index, directions, mask) ->
    let source = Packed.Float3.Private.view (Geometry.positions geometry) in
    let count = Geometry.point_count geometry in
    let x = Array.copy source.x and y = Array.copy source.y
    and z = Array.copy source.z in
    let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
    let errors = Array.make ranges (-1) and changed = Array.make ranges false in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
      let first = range * grain and last = min count ((range + 1) * grain) in
      for point = first to last - 1 do
        if point land 4095 = 0 then Cancel.check_opt cancel;
        if point_selected selection index point then begin
          let vx = directions.x.(point) and vy = directions.y.(point)
          and vz = directions.z.(point)
          and mask = match mask with None -> 1. | Some values -> values.(point) in
          if not (Float.is_finite vx && Float.is_finite vy && Float.is_finite vz
              && Float.is_finite mask) then errors.(range) <- point
          else begin
            let scale = if normalize_direction then max_abs3 vx vy vz
              else 1. in
            let dx = if normalize_direction && scale > 0. then vx /. scale else vx
            and dy = if normalize_direction && scale > 0. then vy /. scale else vy
            and dz = if normalize_direction && scale > 0. then vz /. scale else vz in
            let multiplier = if normalize_direction then
                if scale = 0. then 0. else
                  (distance *. mask) /. sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz))
              else distance *. mask in
            if multiplier <> 0. then begin
              let px = source.x.(point) +. (dx *. multiplier)
              and py = source.y.(point) +. (dy *. multiplier)
              and pz = source.z.(point) +. (dz *. multiplier) in
              if not (Float.is_finite px && Float.is_finite py
                  && Float.is_finite pz) then errors.(range) <- point
              else begin
                x.(point) <- px; y.(point) <- py; z.(point) <- pz;
                if px <> source.x.(point) || py <> source.y.(point)
                    || pz <> source.z.(point)
                then changed.(range) <- true
              end
            end
          end
        end
      done);
    match Array.find_opt (fun value -> value >= 0) errors with
    | Some point -> Error (Printf.sprintf
        "Rdk_mesh.Deform.peak: non-finite direction, mask, or output at point %d" point)
    | None when not (Array.exists Fun.id changed) -> Ok geometry
    | None -> finish_positions ?cancel ~grain ~recompute_normals geometry x y z)

let initial_height_attribute name count geometry = match name with
  | None -> Ok None
  | Some name when String.trim name = "" -> Error
      "Rdk_mesh.Deform.mountain: height attribute name must not be empty"
  | Some name ->
      (match Geometry.find_attribute ~owner:Attribute.Point name geometry with
       | None -> Ok (Some (name, Array.make count 0.))
       | Some attribute ->
           (match Attribute.Private.storage attribute with
            | Attribute.Float values -> Ok (Some (name, Array.copy values))
            | _ -> Error (Printf.sprintf
                "Rdk_mesh.Deform.mountain: height attribute %s must have float storage"
                name)))

let mountain ?cancel ?(grain = 16_384) ?selection ?direction_attribute
    ?(normalize_direction = true) ?mask_attribute ?(seed = 0) ~height
    ?(frequency = Rays_math.Vec3.create 1. 1. 1.) ?(offset = Rays_math.Vec3.zero) ?(octaves = 4)
    ?(lacunarity = 2.) ?(roughness = 0.5) ?height_attribute
    ?(recompute_normals = false) geometry =
  Error.guard ~operation:"mountain" ~code:"invalid_deformation" @@ fun () ->
  if grain <= 0 then Error "Rdk_mesh.Deform.mountain: grain must be positive"
  else if not (Float.is_finite height && finite_vec3 frequency
      && finite_vec3 offset && Float.is_finite lacunarity
      && Float.is_finite roughness) then Error
      "Rdk_mesh.Deform.mountain: numeric parameters must be finite"
  else if octaves <= 0 || octaves > 64 then Error
      "Rdk_mesh.Deform.mountain: octaves must be in [1, 64]"
  else if lacunarity <= 0. then Error
      "Rdk_mesh.Deform.mountain: lacunarity must be positive"
  else if roughness < 0. || roughness > 1. then Error
      "Rdk_mesh.Deform.mountain: roughness must be in [0, 1]"
  else Result.bind (prepare ?cancel ~grain ?selection ?direction_attribute
      ?mask_attribute geometry) (fun (index, directions, mask) ->
    let source = Packed.Float3.Private.view (Geometry.positions geometry) in
    let count = Geometry.point_count geometry in
    Result.bind (initial_height_attribute height_attribute count geometry)
      (fun height_output ->
    let x = Array.copy source.x and y = Array.copy source.y
    and z = Array.copy source.z and noise = Rays_math.Noise.create seed in
    let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
    let errors = Array.make ranges (-1) and changed = Array.make ranges false in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
      let first = range * grain and last = min count ((range + 1) * grain) in
      let noise_scratch = Rays_math.Noise.Private.create_fbm3_scratch () in
      for point = first to last - 1 do
        if point land 4095 = 0 then Cancel.check_opt cancel;
        if point_selected selection index point then begin
          let vx = directions.x.(point) and vy = directions.y.(point)
          and vz = directions.z.(point)
          and mask = match mask with None -> 1. | Some values -> values.(point) in
          let sx = (source.x.(point) +. offset.x) *. frequency.x
          and sy = (source.y.(point) +. offset.y) *. frequency.y
          and sz = (source.z.(point) +. offset.z) *. frequency.z in
          if not (Float.is_finite vx && Float.is_finite vy && Float.is_finite vz
              && Float.is_finite mask && Float.is_finite sx
              && Float.is_finite sy && Float.is_finite sz) then
            errors.(range) <- point
          else begin
            let sample = Rays_math.Noise.Private.fbm3_with_scratch noise_scratch noise
                ~octaves ~lacunarity ~gain:roughness ~x:sx ~y:sy ~z:sz in
            let displacement = height *. ((sample *. 2.) -. 1.) *. mask in
            (match height_output with
             | None -> () | Some (_, values) -> values.(point) <- displacement);
            let scale = if normalize_direction then max_abs3 vx vy vz
              else 1. in
            let dx = if normalize_direction && scale > 0. then vx /. scale else vx
            and dy = if normalize_direction && scale > 0. then vy /. scale else vy
            and dz = if normalize_direction && scale > 0. then vz /. scale else vz in
            let multiplier = if normalize_direction then
                if scale = 0. then 0. else displacement /.
                  sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz))
              else displacement in
            let px = source.x.(point) +. (dx *. multiplier)
            and py = source.y.(point) +. (dy *. multiplier)
            and pz = source.z.(point) +. (dz *. multiplier) in
            if not (Float.is_finite sample && Float.is_finite displacement
                && Float.is_finite px && Float.is_finite py
                && Float.is_finite pz) then errors.(range) <- point
            else begin
              x.(point) <- px; y.(point) <- py; z.(point) <- pz;
              if px <> source.x.(point) || py <> source.y.(point)
                  || pz <> source.z.(point)
              then changed.(range) <- true
            end
          end
        end
      done);
    match Array.find_opt (fun value -> value >= 0) errors with
    | Some point -> Error (Printf.sprintf
        "Rdk_mesh.Deform.mountain: non-finite direction, noise, mask, or output at point %d"
        point)
    | None ->
        let position_changed = Array.exists Fun.id changed in
        let geometry_result = if position_changed then
            finish_positions ?cancel ~grain ~recompute_normals geometry x y z
          else if recompute_normals then with_point_normals ?cancel ~grain geometry
          else Ok geometry in
        Result.bind geometry_result (fun geometry -> match height_output with
          | None -> Ok geometry
          | Some (name, values) ->
              Result.bind (Attribute.create_owned ~name ~owner:Attribute.Point
                  (Attribute.Float values)) (fun attribute ->
                Geometry.with_attribute attribute geometry))))

type noise_displace_mode = Height_2d | Normal_3d
let noise_displace ?cancel ?(grain = 16_384) ?(mode = Height_2d) ~amplitude ~frequency ~seed geometry =
  Error.guard ~operation:"noise_displace" ~code:"invalid_parameter" @@ fun () ->
  if not (Float.is_finite amplitude && Float.is_finite frequency) then
    Error "Rdk_mesh.Deform.noise_displace: amplitude and frequency must be finite"
  else if grain <= 0 then Error "Rdk_mesh.Deform.noise_displace: grain must be positive"
  else
    let () = Cancel.check_opt cancel in
    let direction = match mode with
      | Height_2d -> Ok None
      | Normal_3d -> Result.map Option.some (point_vector_attribute "Rdk_mesh.Deform.noise_displace" "N" geometry) in
    Result.bind direction (fun direction ->
    let noise = Rays_math.Noise.create seed in
    let count = Geometry.point_count geometry in
    let source = Packed.Float3.Private.view (Geometry.positions geometry) in
    let x, y, z = match direction with
      | None -> source.x, Array.copy source.y, source.z
      | Some _ -> Array.copy source.x, Array.copy source.y, Array.copy source.z in
    let errors = Array.make (if count = 0 then 0 else (count - 1) / grain + 1) (-1) in
    if Array.length errors > 0 then Parallel.for_ ~chunk_size:1 ~start:0
        ~finish:(Array.length errors - 1) (fun range ->
          let first = range * grain in
          let last = first + min grain (count - first) in
          Cancel.check_opt cancel;
          match direction with
          | None ->
              Rays_math.Noise.Private.sample2_into noise ~first ~last ~frequency
                ~x:source.x ~y:source.z ~output:y;
              for index = first to last - 1 do
                let value = source.y.(index) +. amplitude *. ((y.(index) *. 2.) -. 1.) in
                if Float.is_finite value then y.(index) <- value
                else if errors.(first / grain) < 0 then errors.(first / grain) <- index
              done
          | Some direction ->
              Rays_math.Noise.Private.sample3_into noise ~first ~last ~frequency
                ~x:source.x ~y:source.y ~z:source.z ~output:z ();
              for index = first to last - 1 do
                let scale = amplitude *. z.(index) in
                let px = source.x.(index) +. direction.x.(index) *. scale
                and py = source.y.(index) +. direction.y.(index) *. scale
                and pz = source.z.(index) +. direction.z.(index) *. scale in
                if Float.is_finite px && Float.is_finite py && Float.is_finite pz then begin
                  x.(index) <- px; y.(index) <- py; z.(index) <- pz
                end else if errors.(first / grain) < 0 then errors.(first / grain) <- index
              done);
    match Array.find_opt (fun index -> index >= 0) errors with
    | Some index -> Error (Printf.sprintf "Rdk_mesh.Deform.noise_displace: non-finite noise, normal or output at point %d" index)
    | None ->
    let positions = Packed.Float3.Private.of_shared_exn ~x ~y ~z in
    let displaced = Geometry.with_positions positions geometry |> Result.get_ok in
    Ok (displaced
        |> Geometry.without_attribute ~owner:Attribute.Point "N"
        |> Geometry.without_attribute ~owner:Attribute.Vertex "N"))
