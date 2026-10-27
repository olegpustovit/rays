open Rays_math

type density = {
  density_owner : Attribute.owner;
  density_attribute : string;
}

let density ~owner density_attribute = { density_owner = owner; density_attribute }

type density_values =
  | Uniform
  | Point_density of float array
  | Vertex_density of float array
  | Primitive_density of float array
  | Detail_density of float

type vec3_source = Point_vec3 of Packed.Float3.t | Vertex_vec3 of Packed.Float3.t
type vec4_source = Point_vec4 of Packed.Float4.t | Vertex_vec4 of Packed.Float4.t

type plan = {
  triangle_count : int;
  direct_triangles : bool;
  triangle_primitives : int array;
  triangle_a : int array;
  triangle_b : int array;
  triangle_c : int array;
}

let selected primitives primitive = match primitives with
  | None -> true
  | Some group -> Group.mem primitive group

let validate_selection geometry = function
  | None -> Ok ()
  | Some group when Group.owner group <> Group.Primitive ->
      Error "Rdk.Scatter.scatter_surface: selection must own primitives"
  | Some group when Group.length group <> Geometry.primitive_count geometry ->
      Error "Rdk.Scatter.scatter_surface: selection length does not match primitive count"
  | Some _ -> Ok ()

let optional_vec3 geometry name =
  let get owner = match Geometry.find_attribute ~owner name geometry with
    | None -> Ok None
    | Some attribute ->
        (match Attribute.Private.storage attribute with
         | Attribute.Float3 values -> Ok (Some values)
         | _ -> Error (Printf.sprintf "Rdk.Scatter.scatter_surface: %s has storage %s"
             name (Attribute.kind_name attribute))) in
  match get Attribute.Vertex with
  | Error _ as error -> error
  | Ok (Some values) -> Ok (Some (Vertex_vec3 values))
  | Ok None -> Result.map (Option.map (fun values -> Point_vec3 values))
      (get Attribute.Point)

let optional_vec4 geometry name =
  let get owner = match Geometry.find_attribute ~owner name geometry with
    | None -> Ok None
    | Some attribute ->
        (match Attribute.Private.storage attribute with
         | Attribute.Float4 values -> Ok (Some values)
         | _ -> Error (Printf.sprintf "Rdk.Scatter.scatter_surface: %s has storage %s"
             name (Attribute.kind_name attribute))) in
  match get Attribute.Vertex with
  | Error _ as error -> error
  | Ok (Some values) -> Ok (Some (Vertex_vec4 values))
  | Ok None -> Result.map (Option.map (fun values -> Point_vec4 values))
      (get Attribute.Point)

let resolve_density geometry = function
  | None -> Ok (Uniform, 1.)
  | Some density when String.trim density.density_attribute = "" ->
      Error "Rdk.Scatter.scatter_surface: density attribute name must not be empty"
  | Some density ->
      match Geometry.find_attribute ~owner:density.density_owner
          density.density_attribute geometry with
      | None -> Error (Printf.sprintf
          "Rdk.Scatter.scatter_surface: missing %s density attribute %S"
          (match density.density_owner with
           | Attribute.Point -> "point" | Attribute.Vertex -> "vertex"
           | Attribute.Primitive -> "primitive" | Attribute.Detail -> "detail")
          density.density_attribute)
      | Some attribute ->
          match Attribute.Private.storage attribute with
          | Attribute.Float values ->
              let maximum = ref 0. and invalid = ref (-1) in
              for index = 0 to Array.length values - 1 do
                let value = values.(index) in
                if not (Float.is_finite value) then invalid := index
                else if value > !maximum then maximum := value
              done;
              if !invalid >= 0 then Error (Printf.sprintf
                  "Rdk.Scatter.scatter_surface: density is non-finite at element %d"
                  !invalid)
              else
                let source = match density.density_owner with
                  | Attribute.Point -> Point_density values
                  | Attribute.Vertex -> Vertex_density values
                  | Attribute.Primitive -> Primitive_density values
                  | Attribute.Detail -> Detail_density values.(0) in
                Ok (source, !maximum)
          | _ -> Error (Printf.sprintf
              "Rdk.Scatter.scatter_surface: density attribute %S must have float storage"
              density.density_attribute)

let[@inline always] triangle_primitive plan triangle =
  if plan.direct_triangles then triangle else plan.triangle_primitives.(triangle)

let[@inline always] triangle_vertex (topology : Topology.Private.view) plan
    triangle local =
  if Array.length plan.triangle_a > 0 then
    if local = 0 then plan.triangle_a.(triangle)
    else if local = 1 then plan.triangle_b.(triangle)
    else plan.triangle_c.(triangle)
  else
    topology.Topology.Private.primitive_offsets.(triangle_primitive plan triangle)
      + local

let create_plan ?cancel ~grain ?primitives geometry =
  let topology_value = Geometry.topology geometry in
  let topology = Topology.Private.view topology_value in
  let primitive_count = Geometry.primitive_count geometry in
  let triangle_count = ref 0 and direct = ref (Option.is_none primitives)
  and has_ngon = ref false and invalid = ref None in
  for primitive = 0 to primitive_count - 1 do
    if primitive land 4095 = 0 then Cancel.check_opt cancel;
    if selected primitives primitive then begin
      let first = topology.primitive_offsets.(primitive)
      and last = topology.primitive_offsets.(primitive + 1) in
      let size = last - first in
      if Bytes.unsafe_get topology.primitive_kinds primitive = '\000' then begin
        if size < 3 then invalid := Some primitive
        else begin
          if size <> 3 then begin direct := false; has_ngon := true end;
          if !triangle_count > Sys.max_array_length - (size - 2) then
            invalid_arg "Rdk.Scatter.scatter_surface: triangle cardinality exceeds array limits";
          triangle_count := !triangle_count + size - 2
        end
      end else direct := false
    end else direct := false
  done;
  match !invalid with
  | Some primitive -> Error (Printf.sprintf
      "Rdk.Scatter.scatter_surface: selected polygon %d has fewer than three corners"
      primitive)
  | None when !direct -> Ok {
      triangle_count = primitive_count; direct_triangles = true;
      triangle_primitives = [||]; triangle_a = [||]; triangle_b = [||];
      triangle_c = [||];
    }
  | None ->
      let offsets = Array.make (primitive_count + 1) 0 in
      for primitive = 0 to primitive_count - 1 do
        let size = topology.primitive_offsets.(primitive + 1)
            - topology.primitive_offsets.(primitive) in
        offsets.(primitive + 1) <- offsets.(primitive)
          + if selected primitives primitive
              && Bytes.unsafe_get topology.primitive_kinds primitive = '\000'
            then size - 2 else 0
      done;
      let triangle_primitives = Array.make !triangle_count 0 in
      let triangle_a = if !has_ngon then Array.make !triangle_count 0 else [||]
      and triangle_b = if !has_ngon then Array.make !triangle_count 0 else [||]
      and triangle_c = if !has_ngon then Array.make !triangle_count 0 else [||] in
      let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
      let chunk_size = max 1 (grain / 3) in
      let ranges = if primitive_count = 0 then 0
        else (primitive_count + chunk_size - 1) / chunk_size in
      let errors = Array.make ranges None in
      if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1)
        (fun range ->
          let scratch = Polygon_triangulation.create_scratch () in
          let first_primitive = range * chunk_size
          and last_primitive = min primitive_count ((range + 1) * chunk_size) in
          let primitive = ref first_primitive in
          while !primitive < last_primitive && Option.is_none errors.(range) do
            if !primitive land 1023 = 0 then Cancel.check_opt cancel;
            if offsets.(!primitive + 1) > offsets.(!primitive) then begin
              let output_first = offsets.(!primitive) in
              let emit ordinal a b c =
                let triangle = output_first + ordinal in
                triangle_primitives.(triangle) <- !primitive;
                if !has_ngon then begin
                  triangle_a.(triangle) <- a; triangle_b.(triangle) <- b;
                  triangle_c.(triangle) <- c
                end in
              match Polygon_triangulation.primitive ?cancel ~positions ~topology
                  ~scratch !primitive ~emit with
              | Ok () -> ()
              | Error message -> errors.(range) <- Some message
            end;
            incr primitive
          done);
      match Array.find_map Fun.id errors with
      | Some message -> Error ("Rdk.Scatter.scatter_surface: " ^ message)
      | None -> Ok {
          triangle_count = !triangle_count; direct_triangles = false;
          triangle_primitives; triangle_a; triangle_b; triangle_c;
        }

let coordinate_scale ?cancel ~grain (topology : Topology.Private.view) plan
    (positions : Packed.Float3.Private.view) =
  let count = if plan.direct_triangles then Array.length positions.x
    else plan.triangle_count in
  let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
  let maxima = Array.make ranges 0. and invalid = Array.make ranges (-1) in
  if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1)
    (fun range ->
      let first = range * grain and last = min count ((range + 1) * grain) in
      let consider point =
        let x = positions.Packed.Float3.Private.x.(point)
        and y = positions.y.(point) and z = positions.z.(point) in
        if not (Float.is_finite x && Float.is_finite y && Float.is_finite z) then
          invalid.(range) <- point
        else begin
          let x = abs_float x and y = abs_float y and z = abs_float z in
          if x > maxima.(range) then maxima.(range) <- x;
          if y > maxima.(range) then maxima.(range) <- y;
          if z > maxima.(range) then maxima.(range) <- z
        end in
      if plan.direct_triangles then
        for point = first to last - 1 do
          if point land 4095 = 0 then Cancel.check_opt cancel;
          if invalid.(range) < 0 then begin
            let x = positions.x.(point) and y = positions.y.(point)
            and z = positions.z.(point) in
            if not (Float.is_finite x && Float.is_finite y
                && Float.is_finite z) then invalid.(range) <- point
            else begin
              let x = abs_float x and y = abs_float y and z = abs_float z in
              if x > maxima.(range) then maxima.(range) <- x;
              if y > maxima.(range) then maxima.(range) <- y;
              if z > maxima.(range) then maxima.(range) <- z
            end
          end
        done
      else
        for triangle = first to last - 1 do
          if triangle land 4095 = 0 then Cancel.check_opt cancel;
          if invalid.(range) < 0 then begin
            let a = triangle_vertex topology plan triangle 0
            and b = triangle_vertex topology plan triangle 1
            and c = triangle_vertex topology plan triangle 2 in
            consider topology.vertex_points.(a);
            if invalid.(range) < 0 then consider topology.vertex_points.(b);
            if invalid.(range) < 0 then consider topology.vertex_points.(c)
          end
        done);
  match Array.find_opt (fun point -> point >= 0) invalid with
  | Some point -> Error (Printf.sprintf
      "Rdk.Scatter.scatter_surface: non-finite selected position at point %d" point)
  | None ->
      let maximum = ref 0. in
      Array.iter (fun value -> if value > !maximum then maximum := value) maxima;
      Ok !maximum

let[@inline always] varying_density = function
  | Point_density _ | Vertex_density _ -> true
  | Uniform | Primitive_density _ | Detail_density _ -> false

let triangle_distribution ?cancel ~grain (topology : Topology.Private.view) plan
    (positions : Packed.Float3.Private.view)
    density density_scale coordinate_scale =
  let count = plan.triangle_count in
  let weights = Array.make count 0. in
  let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
  let errors = Array.make ranges (-1) in
  if ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1)
    (fun range ->
      let first = range * grain and last = min count ((range + 1) * grain) in
      for triangle = first to last - 1 do
        if triangle land 4095 = 0 then Cancel.check_opt cancel;
        let va = triangle_vertex topology plan triangle 0
        and vb = triangle_vertex topology plan triangle 1
        and vc = triangle_vertex topology plan triangle 2 in
        let a = topology.Topology.Private.vertex_points.(va)
        and b = topology.vertex_points.(vb) and c = topology.vertex_points.(vc) in
        let ax = positions.Packed.Float3.Private.x.(a) /. coordinate_scale
        and ay = positions.y.(a) /. coordinate_scale
        and az = positions.z.(a) /. coordinate_scale
        and bx = positions.x.(b) /. coordinate_scale
        and by = positions.y.(b) /. coordinate_scale
        and bz = positions.z.(b) /. coordinate_scale
        and cx = positions.x.(c) /. coordinate_scale
        and cy = positions.y.(c) /. coordinate_scale
        and cz = positions.z.(c) /. coordinate_scale in
        let abx = bx -. ax and aby = by -. ay and abz = bz -. az
        and acx = cx -. ax and acy = cy -. ay and acz = cz -. az in
        let nx = (aby *. acz) -. (abz *. acy)
        and ny = (abz *. acx) -. (abx *. acz)
        and nz = (abx *. acy) -. (aby *. acx) in
        let twice_area = sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
        let primitive = triangle_primitive plan triangle in
        let density_sum = match density with
          | Uniform -> 3.
          | Detail_density value ->
              (3. *. (if value > 0. then value else 0.)) /. density_scale
          | Primitive_density values ->
              let value = values.(primitive) in
              (3. *. (if value > 0. then value else 0.)) /. density_scale
          | Point_density values ->
              let da = values.(a) and db = values.(b) and dc = values.(c) in
              ((if da > 0. then da else 0.) +. (if db > 0. then db else 0.)
                +. (if dc > 0. then dc else 0.)) /. density_scale
          | Vertex_density values ->
              let da = values.(va) and db = values.(vb) and dc = values.(vc) in
              ((if da > 0. then da else 0.) +. (if db > 0. then db else 0.)
                +. (if dc > 0. then dc else 0.)) /. density_scale in
        let weight = twice_area *. density_sum /. 6. in
        if not (Float.is_finite weight) then errors.(range) <- triangle
        else weights.(triangle) <- weight
      done);
  match Array.find_opt (fun triangle -> triangle >= 0) errors with
  | Some triangle -> Error (Printf.sprintf
      "Rdk.Scatter.scatter_surface: non-finite area or density at triangle %d"
      triangle)
  | None ->
      let total = ref 0. and compensation = ref 0. in
      for triangle = 0 to count - 1 do
        let corrected = weights.(triangle) -. !compensation in
        let next = !total +. corrected in
        compensation := (next -. !total) -. corrected;
        total := next
      done;
      if not (Float.is_finite !total) || !total <= 0. then Error
          "Rdk.Scatter.scatter_surface: selected surface has zero weighted area"
      else Ok (weights, !total)

let build_alias weights total =
  let count = Array.length weights in
  let aliases = Array.make count 0 and work = Array.make count 0 in
  let small = ref 0 and large = ref count in
  let scale = float_of_int count /. total in
  for triangle = 0 to count - 1 do
    weights.(triangle) <- weights.(triangle) *. scale;
    if weights.(triangle) < 1. then begin
      work.(!small) <- triangle; incr small
    end else begin
      decr large; work.(!large) <- triangle
    end
  done;
  while !small > 0 && !large < count do
    decr small;
    let below = work.(!small) and above = work.(!large) in
    incr large;
    aliases.(below) <- above;
    weights.(above) <- weights.(above) +. weights.(below) -. 1.;
    if weights.(above) < 1. then begin
      work.(!small) <- above; incr small
    end else begin
      decr large; work.(!large) <- above
    end
  done;
  while !small > 0 do
    decr small; let triangle = work.(!small) in
    weights.(triangle) <- 1.; aliases.(triangle) <- triangle
  done;
  while !large < count do
    let triangle = work.(!large) in incr large;
    weights.(triangle) <- 1.; aliases.(triangle) <- triangle
  done;
  weights, aliases

let[@inline always] random_float_into output slot seed index stream =
  output.(slot) <- Rays_math.Rand.float_at_int seed ~index:((index * 8) + stream)

let unique_attribute_name geometry reserved base =
  let exists name = List.mem name reserved || List.exists (fun attribute ->
    String.equal (Attribute.name attribute) name) (Geometry.attributes geometry) in
  let rec find suffix =
    let name = if suffix = 0 then base else base ^ string_of_int suffix in
    if exists name then find (suffix + 1) else name in
  find 0

let validate_name label = function
  | None -> Ok ()
  | Some name when String.trim name = "" ->
      Error ("Rdk.Scatter.scatter_surface: " ^ label ^ " must not be empty")
  | Some "P" -> Error ("Rdk.Scatter.scatter_surface: " ^ label ^ " cannot be P")
  | Some _ -> Ok ()

let run ?cancel ?(grain = 16_384) ?primitives ?density ?point_pattern
    ?vertex_pattern ?primitive_pattern ?detail_pattern ?(match_groups = false)
    ?source_primitive_attribute ?source_vertex_numbers_attribute
    ?source_vertex_weights_attribute ~count ~seed geometry =
  Error.guard ~operation:"scatter_surface" ~code:"invalid_geometry" @@ fun () ->
  if count < 0 then Error "Rdk.Scatter.scatter_surface: count must be non-negative"
  else if grain <= 0 then Error "Rdk.Scatter.scatter_surface: grain must be positive"
  else if match_groups && point_pattern = None && vertex_pattern = None
      && primitive_pattern = None then Error
      "Rdk.Scatter.scatter_surface: group interpolation requires an attribute/group pattern"
  else Result.bind (validate_selection geometry primitives) (fun () ->
    Result.bind (validate_name "source primitive attribute"
      source_primitive_attribute) (fun () ->
    Result.bind (validate_name "source vertex numbers attribute"
      source_vertex_numbers_attribute) (fun () ->
    Result.bind (validate_name "source vertex weights attribute"
      source_vertex_weights_attribute) (fun () ->
    let paired = match source_vertex_numbers_attribute,
        source_vertex_weights_attribute with
      | None, None | Some _, Some _ -> true
      | None, Some _ | Some _, None -> false in
    if not paired then Error
        "Rdk.Scatter.scatter_surface: source vertex number and weight attributes must be requested together"
    else
      let names = List.filter_map Fun.id [source_primitive_attribute;
        source_vertex_numbers_attribute; source_vertex_weights_attribute] in
      if List.length names <> List.length (List.sort_uniq String.compare names)
      then Error "Rdk.Scatter.scatter_surface: output attribute names must be distinct"
      else Result.bind (resolve_density geometry density)
        (fun (density_values, density_scale) ->
      Result.bind (optional_vec3 geometry "N") (fun normals ->
      Result.bind (optional_vec4 geometry "Cd") (fun colors ->
      Result.bind (create_plan ?cancel ~grain ?primitives geometry) (fun plan ->
      let topology = Topology.Private.view (Geometry.topology geometry)
      and positions = Packed.Float3.Private.view (Geometry.positions geometry) in
      if count > 0 && (plan.triangle_count = 0 || density_scale <= 0.) then Error
          "Rdk.Scatter.scatter_surface: selected surface has zero weighted area"
      else Result.bind (if count = 0 then Ok 1.
        else coordinate_scale ?cancel ~grain topology plan positions)
        (fun coordinate_scale ->
      if count > 0 && coordinate_scale = 0. then Error
          "Rdk.Scatter.scatter_surface: selected surface has zero weighted area"
      else Result.bind (if count = 0 then Ok ([||], 1.)
        else triangle_distribution ?cancel ~grain topology plan positions
          density_values density_scale coordinate_scale) (fun (weights, total) ->
      let probabilities, aliases = if count = 0 then [||], [||]
        else build_alias weights total in
      let random = seed in
      let needs_interpolation = point_pattern <> None || vertex_pattern <> None
        || primitive_pattern <> None || detail_pattern <> None || match_groups in
      let keep_drivers = source_vertex_numbers_attribute <> None in
      let need_drivers = needs_interpolation || keep_drivers in
      if need_drivers && count > Sys.max_array_length / 3 then Error
          "Rdk.Scatter.scatter_surface: provenance cardinality exceeds array limits"
      else
        let reserved = "P" :: "N" :: "Cd" :: "id" :: names in
        let numbers_name = match source_vertex_numbers_attribute with
          | Some name -> name
          | None -> unique_attribute_name geometry reserved
              "__rays_scatter_vertex_numbers" in
        let weights_name = match source_vertex_weights_attribute with
          | Some name -> name
          | None -> unique_attribute_name geometry (numbers_name :: reserved)
              "__rays_scatter_vertex_weights" in
        let px = Array.make count 0. and py = Array.make count 0.
        and pz = Array.make count 0. and nx = Array.make count 0.
        and ny = Array.make count 0. and nz = Array.make count 0.
        and ids = Array.make count 0 in
        let color_planes = Option.map (fun _ ->
          Array.make count 0., Array.make count 0., Array.make count 0.,
            Array.make count 1.) colors in
        let source_primitives = Option.map (fun _ -> Array.make count 0)
            source_primitive_attribute in
        let driver_numbers = if need_drivers then Array.make (count * 3) 0
          else [||]
        and driver_weights = if need_drivers then Array.make (count * 3) 0.
          else [||] in
        let normal_view = Option.map (function
          | Point_vec3 values | Vertex_vec3 values -> Packed.Float3.Private.view values)
            normals
        and color_view = Option.map (function
          | Point_vec4 values | Vertex_vec4 values -> Packed.Float4.Private.view values)
            colors in
        let source_index source vertex point = match source with
          | `Point -> point | `Vertex -> vertex in
        let sample_ranges = if count = 0 then 0 else (count + grain - 1) / grain in
        if sample_ranges > 0 then Parallel.for_ ~chunk_size:1 ~start:0
          ~finish:(sample_ranges - 1) (fun range ->
            let first_index = range * grain
            and last_index = min count ((range + 1) * grain) in
            let barycentric = Array.make 3 0. and random_values = Array.make 6 0.
            and density_values_scratch = Array.make 3 0. in
            for index = first_index to last_index - 1 do
            if index land 4095 = 0 then Cancel.check_opt cancel;
            random_float_into random_values 0 random index 0;
            let scaled = random_values.(0) *. float_of_int plan.triangle_count in
            let column = min (plan.triangle_count - 1) (int_of_float scaled) in
            let triangle = if scaled -. float_of_int column
                < probabilities.(column) then column else aliases.(column) in
            let primitive = triangle_primitive plan triangle in
            let va = triangle_vertex topology plan triangle 0
            and vb = triangle_vertex topology plan triangle 1
            and vc = triangle_vertex topology plan triangle 2 in
            let a = topology.vertex_points.(va) and b = topology.vertex_points.(vb)
            and c = topology.vertex_points.(vc) in
            if not (varying_density density_values) then begin
              random_float_into random_values 1 random index 1;
              random_float_into random_values 2 random index 2;
              let root = sqrt random_values.(1) in
              let third = random_values.(2) in
              barycentric.(0) <- 1. -. root;
              barycentric.(1) <- root *. (1. -. third);
              barycentric.(2) <- root *. third
            end else begin
              (match density_values with
               | Point_density values ->
                   let da = values.(a) and db = values.(b) and dc = values.(c) in
                   density_values_scratch.(0) <-
                     (if da > 0. then da else 0.) /. density_scale;
                   density_values_scratch.(1) <-
                     (if db > 0. then db else 0.) /. density_scale;
                   density_values_scratch.(2) <-
                     (if dc > 0. then dc else 0.) /. density_scale
               | Vertex_density values ->
                   let da = values.(va) and db = values.(vb) and dc = values.(vc) in
                   density_values_scratch.(0) <-
                     (if da > 0. then da else 0.) /. density_scale;
                   density_values_scratch.(1) <-
                     (if db > 0. then db else 0.) /. density_scale;
                   density_values_scratch.(2) <-
                     (if dc > 0. then dc else 0.) /. density_scale
               | Uniform | Primitive_density _ | Detail_density _ -> assert false);
              let da = density_values_scratch.(0)
              and db = density_values_scratch.(1)
              and dc = density_values_scratch.(2) in
              let density_sum = da +. db +. dc in
              for stream = 1 to 5 do
                random_float_into random_values stream random index stream
              done;
              let choice = random_values.(1) *. density_sum in
              let selected = if choice < da then 0
                else if choice < da +. db then 1 else 2 in
              let ga = -. log (1. -. random_values.(2))
              and gb = -. log (1. -. random_values.(3))
              and gc = -. log (1. -. random_values.(4))
              and extra = -. log (1. -. random_values.(5)) in
              let ga = if selected = 0 then ga +. extra else ga
              and gb = if selected = 1 then gb +. extra else gb
              and gc = if selected = 2 then gc +. extra else gc in
              let gamma_sum = ga +. gb +. gc in
              if gamma_sum = 0. then begin barycentric.(0) <- 1.;
                barycentric.(1) <- 0.; barycentric.(2) <- 0. end
              else begin barycentric.(0) <- ga /. gamma_sum;
                barycentric.(1) <- gb /. gamma_sum;
                barycentric.(2) <- gc /. gamma_sum end
            end;
            let wa = barycentric.(0) and wb = barycentric.(1)
            and wc = barycentric.(2) in
            px.(index) <- wa *. positions.x.(a) +. wb *. positions.x.(b)
                +. wc *. positions.x.(c);
            py.(index) <- wa *. positions.y.(a) +. wb *. positions.y.(b)
                +. wc *. positions.y.(c);
            pz.(index) <- wa *. positions.z.(a) +. wb *. positions.z.(b)
                +. wc *. positions.z.(c);
            (match normals, normal_view with
             | Some source, Some view ->
                 let owner = match source with
                   | Point_vec3 _ -> `Point | Vertex_vec3 _ -> `Vertex in
                 let ia = source_index owner va a and ib = source_index owner vb b
                 and ic = source_index owner vc c in
                 let x = wa *. view.x.(ia) +. wb *. view.x.(ib)
                    +. wc *. view.x.(ic)
                 and y = wa *. view.y.(ia) +. wb *. view.y.(ib)
                    +. wc *. view.y.(ic)
                 and z = wa *. view.z.(ia) +. wb *. view.z.(ib)
                    +. wc *. view.z.(ic) in
                 random_values.(0) <- abs_float x;
                 if abs_float y > random_values.(0) then
                   random_values.(0) <- abs_float y;
                 if abs_float z > random_values.(0) then
                   random_values.(0) <- abs_float z;
                 let scale = random_values.(0) in
                 if scale > 0. then begin
                   let x = x /. scale and y = y /. scale and z = z /. scale in
                   let inverse = 1. /. sqrt (x *. x +. y *. y +. z *. z) in
                   nx.(index) <- x *. inverse; ny.(index) <- y *. inverse;
                   nz.(index) <- z *. inverse
                 end
             | _ ->
                 let ax = positions.x.(a) /. coordinate_scale
                 and ay = positions.y.(a) /. coordinate_scale
                 and az = positions.z.(a) /. coordinate_scale
                 and bx = positions.x.(b) /. coordinate_scale
                 and by = positions.y.(b) /. coordinate_scale
                 and bz = positions.z.(b) /. coordinate_scale
                 and cx = positions.x.(c) /. coordinate_scale
                 and cy = positions.y.(c) /. coordinate_scale
                 and cz = positions.z.(c) /. coordinate_scale in
                 let abx = bx -. ax and aby = by -. ay and abz = bz -. az
                 and acx = cx -. ax and acy = cy -. ay and acz = cz -. az in
                 let x = aby *. acz -. abz *. acy
                 and y = abz *. acx -. abx *. acz
                 and z = abx *. acy -. aby *. acx in
                 let inverse = 1. /. sqrt (x *. x +. y *. y +. z *. z) in
                 nx.(index) <- x *. inverse; ny.(index) <- y *. inverse;
                 nz.(index) <- z *. inverse);
            (match colors, color_view, color_planes with
             | Some source, Some view, Some (cr, cg, cb, ca) ->
                 let owner = match source with
                   | Point_vec4 _ -> `Point | Vertex_vec4 _ -> `Vertex in
                 let ia = source_index owner va a and ib = source_index owner vb b
                 and ic = source_index owner vc c in
                 cr.(index) <- wa *. view.x.(ia) +. wb *. view.x.(ib)
                    +. wc *. view.x.(ic);
                 cg.(index) <- wa *. view.y.(ia) +. wb *. view.y.(ib)
                    +. wc *. view.y.(ic);
                 cb.(index) <- wa *. view.z.(ia) +. wb *. view.z.(ib)
                    +. wc *. view.z.(ic);
                 ca.(index) <- wa *. view.w.(ia) +. wb *. view.w.(ib)
                    +. wc *. view.w.(ic)
             | _ -> ());
            (match source_primitives with
             | Some values -> values.(index) <- primitive | None -> ());
            if need_drivers then begin
              let first = index * 3 in
              driver_numbers.(first) <- va; driver_numbers.(first + 1) <- vb;
              driver_numbers.(first + 2) <- vc;
              driver_weights.(first) <- wa; driver_weights.(first + 1) <- wb;
              driver_weights.(first + 2) <- wc
            end;
            ids.(index) <- index
            done);
        let normal_attribute = Attribute.create_key_owned
            (Attribute.normal ~owner:Attribute.Point)
            (Packed.Float3.Private.of_owned_exn ~x:nx ~y:ny ~z:nz)
            |> Result.get_ok
        and id_attribute = Attribute.create_owned ~name:"id" ~owner:Attribute.Point
            (Attribute.Int ids) |> Result.get_ok in
        let attributes = ref [normal_attribute; id_attribute]
        and protected_attributes = ref [id_attribute] in
        Option.iter (fun (cr, cg, cb, ca) ->
          let color = Packed.Float4.of_owned ~x:cr ~y:cg ~z:cb ~w:ca
              |> Result.get_ok in
          let attribute = Attribute.create_key_owned
              (Attribute.color ~owner:Attribute.Point) color |> Result.get_ok in
          attributes := attribute :: !attributes) color_planes;
        (match source_primitive_attribute, source_primitives with
         | Some name, Some values ->
             let attribute = Attribute.create_owned ~name ~owner:Attribute.Point
                 (Attribute.Int values) |> Result.get_ok in
             attributes := attribute :: !attributes;
             protected_attributes := attribute :: !protected_attributes
         | None, None -> ()
         | None, Some _ | Some _, None -> assert false);
        if need_drivers then begin
          let offsets = Array.init (count + 1) (fun index -> index * 3) in
          let numbers = Packed.Int_array.Private.create_validated_owned
              ~offsets ~values:driver_numbers
          and weights = Packed.Float_array.Private.create_validated_owned
              ~offsets:(Array.copy offsets) ~values:driver_weights in
          let numbers_attribute = Attribute.create_owned ~name:numbers_name
              ~owner:Attribute.Point (Attribute.Int_array numbers) |> Result.get_ok
          and weights_attribute = Attribute.create_owned ~name:weights_name
              ~owner:Attribute.Point (Attribute.Float_array weights)
              |> Result.get_ok in
          attributes := numbers_attribute :: weights_attribute :: !attributes;
          if keep_drivers then protected_attributes :=
            numbers_attribute :: weights_attribute :: !protected_attributes
        end;
        Result.bind (Geometry.create
          ~positions:(Packed.Float3.Private.of_owned_exn ~x:px ~y:py ~z:pz)
          ~topology:(Topology.empty ~point_count:count)
          ~attributes:(List.rev !attributes) ()) (fun output ->
        Result.bind (if not needs_interpolation then Ok output
          else Result.map_error Error.to_string
            (Attribute_ops.interpolate ?cancel ~grain
              ~driver:(Attribute_ops.Vertex_weights {
                numbers_attribute = numbers_name; weights_attribute = weights_name })
              ?point_pattern ?vertex_pattern ?primitive_pattern ?detail_pattern
              ~match_groups ~target_owner:Attribute.Point ~attributes:[]
              ~source:geometry ~target:output ())) (fun output ->
        let output = if need_drivers && not keep_drivers then
            output |> Geometry.without_attribute ~owner:Attribute.Point numbers_name
            |> Geometry.without_attribute ~owner:Attribute.Point weights_name
          else output in
        List.fold_left (fun result attribute -> Result.bind result
          (Geometry.with_attribute attribute)) (Ok output) !protected_attributes
        ))))))))))))
