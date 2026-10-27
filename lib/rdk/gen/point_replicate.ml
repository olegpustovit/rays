open Rays_math

type shape =
  | Replicate_point
  | Replicate_box
  | Replicate_sphere
  | Replicate_disk
  | Replicate_line
  | Replicate_custom

type velocity_stretch =
  | Replicate_no_velocity_stretch
  | Replicate_scaled_velocity
  | Replicate_velocity_only

let error message = Error ("Rdk_gen.Point_replicate.point_replicate: " ^ message)
let finite3 value = Float.is_finite value.Rays_math.Vec3.x
  && Float.is_finite value.y && Float.is_finite value.z
let[@inline always] fract value = value -. Float.floor value

let point_int_optional name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Ok None
  | Some attribute -> (match Attribute.Private.storage attribute with
      | Attribute.Int values -> Ok (Some values)
      | _ -> error (Printf.sprintf "point attribute %S must use int storage" name))

let point_float3_optional name geometry =
  match Geometry.find_attribute ~owner:Attribute.Point name geometry with
  | None -> Ok None
  | Some attribute -> (match Attribute.Private.storage attribute with
      | Attribute.Float3 values -> Ok (Some (Packed.Float3.Private.view values))
      | _ -> error (Printf.sprintf "point attribute %S must use float3 storage" name))

let fresh_point_name base geometry =
  let rec loop suffix =
    let name = if suffix = 0 then base else base ^ string_of_int suffix in
    if Geometry.find_attribute ~owner:Attribute.Point name geometry = None
    then name else loop (suffix + 1) in
  loop 0

let radical_inverse base value =
  let inverse = 1. /. float_of_int base in
  let rec loop value factor output =
    if value = 0 then output
    else loop (value / base) (factor *. inverse)
        (output +. (float_of_int (value mod base) *. factor)) in
  loop value inverse 0.

type quasi_data = {
  offset_u : float array;
  offset_v : float array;
  offset_w : float array;
  sequence_v : float array;
  sequence_w : float array;
}

let create_sample_scratch () = Array.make 6 0.

let[@inline always] sample_coordinates_into scratch ~quasi ~seed ~identity ~source
    ~local ~count =
  let key = (identity * 0x1e3779b9) lxor (local * 0x165667b1) in
  match quasi with
  | None ->
    scratch.(0) <- Rays_math.Rand.float_at seed ~index:(key lxor 0x27d4eb2d);
    scratch.(1) <- Rays_math.Rand.float_at seed ~index:(key lxor 0x6c8e9cf5);
    scratch.(2) <- Rays_math.Rand.float_at seed ~index:(key lxor 0x5bd1e995)
  | Some quasi ->
    scratch.(0) <- fract (((float_of_int local +. 0.5)
      /. float_of_int (max 1 count)) +. quasi.offset_u.(source));
    scratch.(1) <- fract (quasi.sequence_v.(local) +. quasi.offset_v.(source));
    scratch.(2) <- fract (quasi.sequence_w.(local) +. quasi.offset_w.(source))

let[@inline always] local_shape_into scratch shape
    (custom_positions : Packed.Float3.Private.view option) =
  match shape with
  | Replicate_point ->
      scratch.(3) <- 0.; scratch.(4) <- 0.; scratch.(5) <- 0.;
      -1
  | Replicate_box ->
      scratch.(3) <- scratch.(0) -. 0.5;
      scratch.(4) <- scratch.(1) -. 0.5;
      scratch.(5) <- scratch.(2) -. 0.5;
      -1
  | Replicate_sphere ->
      let z = (2. *. scratch.(0)) -. 1. in
      let radial = 0.5 *. (scratch.(2) ** (1. /. 3.)) in
      let planar = radial *. sqrt (max 0. (1. -. (z *. z))) in
      let angle = 2. *. Float.pi *. scratch.(1) in
      scratch.(3) <- planar *. cos angle;
      scratch.(4) <- planar *. sin angle;
      scratch.(5) <- radial *. z;
      -1
  | Replicate_disk ->
      let radius = 0.5 *. sqrt scratch.(0)
      and angle = 2. *. Float.pi *. scratch.(1) in
      scratch.(3) <- radius *. cos angle;
      scratch.(4) <- radius *. sin angle;
      scratch.(5) <- 0.;
      -1
  | Replicate_line ->
      scratch.(3) <- 0.; scratch.(4) <- 0.; scratch.(5) <- scratch.(0) -. 0.5;
      -1
  | Replicate_custom ->
      let custom = Option.get custom_positions in
      let count = Array.length custom.x in
      let point = min (count - 1)
          (int_of_float (scratch.(0) *. float_of_int count)) in
      scratch.(3) <- custom.x.(point); scratch.(4) <- custom.y.(point);
      scratch.(5) <- custom.z.(point); point

let make_basis () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;1.;0.;0.|] ~y:[|0.;0.;1.;0.|] ~z:[|0.;0.;0.;1.|] in
  Geometry.create ~positions ~topology:(Topology.empty ~point_count:4) ()
  |> Result.get_ok

let transform_inherited_attributes ?cancel ~grain ~pattern ~prefix ~source_map
    ~counts ~(frame_positions : Packed.Float3.Private.view) generated =
  match pattern with
  | None -> Ok generated
  | Some pattern ->
      let attributes = Geometry.attributes generated in
      let transformable attribute = Attribute.owner attribute = Attribute.Point
        && not (String.equal (Attribute.name attribute) "P")
        && Attribute_pattern.matches pattern (Attribute.name attribute)
        && match Attribute.Private.storage attribute with
          | Attribute.Float3 _ -> true | _ -> false in
      let transforms_normal = List.exists (fun attribute -> transformable attribute
          && String.equal (Attribute.name attribute) "N") attributes in
      let source_count = Array.length frame_positions.x / 4 in
      let singular_normal = ref false and source = ref 0 in
      while transforms_normal && not !singular_normal && !source < source_count do
        let base = !source * 4 in
        let tx = frame_positions.x.(base) and ty = frame_positions.y.(base)
        and tz = frame_positions.z.(base) in
        let a = frame_positions.x.(base+1)-.tx
        and d = frame_positions.y.(base+1)-.ty
        and g = frame_positions.z.(base+1)-.tz
        and b = frame_positions.x.(base+2)-.tx
        and e = frame_positions.y.(base+2)-.ty
        and h = frame_positions.z.(base+2)-.tz
        and c = frame_positions.x.(base+3)-.tx
        and f = frame_positions.y.(base+3)-.ty
        and i = frame_positions.z.(base+3)-.tz in
        let scale = max (abs_float a) (max (abs_float b) (max (abs_float c)
            (max (abs_float d) (max (abs_float e) (max (abs_float f)
              (max (abs_float g) (max (abs_float h) (abs_float i)))))))) in
        if counts.(!source) = 0 then ()
        else if scale = 0. then singular_normal := true
        else begin
          let a=a/.scale and b=b/.scale and c=c/.scale and d=d/.scale
          and e=e/.scale and f=f/.scale and g=g/.scale and h=h/.scale
          and i=i/.scale in
          let determinant = a*.((e*.i)-.(f*.h))
              +. b*.((f*.g)-.(d*.i)) +. c*.((d*.h)-.(e*.g)) in
          if abs_float determinant <= 1e-20 then singular_normal := true
        end;
        incr source
      done;
      let total = Geometry.point_count generated in
      let remap attribute =
        if not (transformable attribute) then Some attribute
        else if !singular_normal && String.equal (Attribute.name attribute) "N"
        then None
        else match Attribute.Private.storage attribute with
        | Attribute.Float3 values ->
            let values = Packed.Float3.Private.view values in
            let x = Array.copy values.x and y = Array.copy values.y
            and z = Array.copy values.z in
            let normal = String.equal (Attribute.name attribute) "N" in
            let range_count = if total = prefix then 0
              else (total - prefix + grain - 1) / grain in
            let failures = Array.make range_count (-1) in
            if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
                ~finish:(range_count - 1) (fun range ->
              let first = prefix + (range * grain)
              and last = min total (prefix + ((range + 1) * grain)) in
              for output = first to last - 1 do
                if output land 4095 = 0 then Cancel.check_opt cancel;
                let source = source_map.(output) in
                let base = source * 4 in
                let tx = frame_positions.x.(base) and ty = frame_positions.y.(base)
                and tz = frame_positions.z.(base) in
                let a = frame_positions.x.(base+1)-.tx
                and d = frame_positions.y.(base+1)-.ty
                and g = frame_positions.z.(base+1)-.tz
                and b = frame_positions.x.(base+2)-.tx
                and e = frame_positions.y.(base+2)-.ty
                and h = frame_positions.z.(base+2)-.tz
                and c = frame_positions.x.(base+3)-.tx
                and f = frame_positions.y.(base+3)-.ty
                and i = frame_positions.z.(base+3)-.tz in
                let vx=values.x.(output) and vy=values.y.(output)
                and vz=values.z.(output) in
                if normal then begin
                    let scale = max (abs_float a) (max (abs_float b)
                        (max (abs_float c) (max (abs_float d)
                          (max (abs_float e) (max (abs_float f)
                            (max (abs_float g) (max (abs_float h)
                              (abs_float i)))))))) in
                    let a=a/.scale and b=b/.scale and c=c/.scale
                    and d=d/.scale and e=e/.scale and f=f/.scale
                    and g=g/.scale and h=h/.scale and i=i/.scale in
                    let c00=(e*.i)-.(f*.h) and c01=(f*.g)-.(d*.i)
                    and c02=(d*.h)-.(e*.g) and c10=(c*.h)-.(b*.i)
                    and c11=(a*.i)-.(c*.g) and c12=(b*.g)-.(a*.h)
                    and c20=(b*.f)-.(c*.e) and c21=(c*.d)-.(a*.f)
                    and c22=(a*.e)-.(b*.d) in
                    let determinant = a*.c00 +. b*.c01 +. c*.c02 in
                    let sign = if determinant < 0. then -1. else 1. in
                    let ox=sign*.((c00*.vx)+.(c01*.vy)+.(c02*.vz))
                    and oy=sign*.((c10*.vx)+.(c11*.vy)+.(c12*.vz))
                    and oz=sign*.((c20*.vx)+.(c21*.vy)+.(c22*.vz)) in
                    let length = sqrt (ox*.ox +. oy*.oy +. oz*.oz) in
                    let inverse_length = if length <= 1e-20 then 0.
                      else 1. /. length in
                    let ox = ox *. inverse_length and oy = oy *. inverse_length
                    and oz = oz *. inverse_length in
                    if Float.is_finite ox && Float.is_finite oy
                        && Float.is_finite oz then begin
                      x.(output)<-ox; y.(output)<-oy; z.(output)<-oz
                    end else if failures.(range) < 0 then failures.(range) <- source
                end else begin
                  let ox=(a*.vx)+.(b*.vy)+.(c*.vz)
                  and oy=(d*.vx)+.(e*.vy)+.(f*.vz)
                  and oz=(g*.vx)+.(h*.vy)+.(i*.vz) in
                  if Float.is_finite ox && Float.is_finite oy
                      && Float.is_finite oz then begin
                    x.(output)<-ox; y.(output)<-oy; z.(output)<-oz
                  end else if failures.(range) < 0 then failures.(range) <- source
                end
              done);
            (match Array.find_opt (fun source -> source >= 0) failures with
             | Some source -> raise (Invalid_argument (Printf.sprintf
                 "transformed attribute %S is non-finite at source point %d"
                 (Attribute.name attribute) source))
             | None -> ());
            let values = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
            Some (Attribute.create_owned ~owner:Attribute.Point
              ~name:(Attribute.name attribute) (Attribute.Float3 values)
              |> Result.get_ok)
        | _ -> assert false in
      try
        let attributes = List.filter_map remap attributes |> Array.of_list in
        Geometry.Private.with_attributes_owned attributes generated
      with Invalid_argument message -> error message

let run ?cancel ?(grain = 16_384) ?points ?(keep_input = false)
    ?(seed = Rays_math.Rand.seed 0) ?(id_attribute = "id") ?generated_group
    ?(copy_point_attributes = "*") ?(keep_source_attributes = false)
    ?(transform_attributes = "P")
    ?(source_point_attribute = "sourcepoint")
    ?(source_index_attribute = "sourceindex")
    ?(shape = Replicate_sphere) ?custom_shape ?(center = Rays_math.Vec3.zero)
    ?(size = Rays_math.Vec3.create 1. 1. 1.) ?(orientation = Rays_math.Vec3.zero)
    ?(uniform_scale = 1.) ?(quasi_stratified = false)
    ?(velocity_stretch = Replicate_no_velocity_stretch) ?(velocity_scale = 1.)
    ?(inherit_velocity = 1.) ?(radial_velocity = 0.)
    ?noise_amplitude ?(noise_frequency = Rays_math.Vec3.create 1. 1. 1.)
    ?(noise_offset = Rays_math.Vec3.zero) ?(noise_roughness = 0.5)
    ?(noise_attenuation = 1.) ?(noise_turbulence = 3) ?(noise_seed = 0)
    ~copy_basis ~points_per_point ?scale_attribute geometry =
  if grain <= 0 then invalid_arg "Rdk_gen.Point_replicate.point_replicate: grain must be positive";
  if String.trim id_attribute = "" then error "id attribute name must not be empty"
  else if not (finite3 center && finite3 size && finite3 orientation
      && Float.is_finite uniform_scale && uniform_scale >= 0.
      && size.Rays_math.Vec3.x >= 0. && size.y >= 0. && size.z >= 0.
      && Float.is_finite velocity_scale && Float.is_finite inherit_velocity
      && Float.is_finite radial_velocity && finite3 noise_frequency
      && finite3 noise_offset
      && (match noise_amplitude with None -> true | Some value -> finite3 value)
      && Float.is_finite noise_roughness && noise_roughness >= 0.
      && noise_roughness <= 1. && Float.is_finite noise_attenuation
      && noise_attenuation > 0. && noise_turbulence > 0) then
    error "center/orientation must be finite; size/scale must be finite and non-negative; velocity controls must be finite"
  else if shape = Replicate_custom && Option.is_none custom_shape then
    error "custom shape requires shape geometry"
  else if shape <> Replicate_custom && Option.is_some custom_shape then
    error "shape geometry is valid only for the custom shape"
  else
  let transform_pattern = if String.trim transform_attributes = "" then Ok None
    else Attribute_pattern.compile transform_attributes |> Result.map Option.some
      |> Result.map_error (fun message ->
        "Rdk_gen.Point_replicate.point_replicate: transform attribute pattern: " ^ message) in
  Result.bind transform_pattern (fun transform_pattern ->
  let custom_positions = Option.map (fun geometry ->
      Packed.Float3.Private.view (Geometry.positions geometry)) custom_shape in
  if shape = Replicate_custom && (match custom_positions with
      | Some values -> Array.length values.x = 0 | None -> true) then
    error "custom shape must contain at least one point"
  else Result.bind (point_int_optional id_attribute geometry) (fun ids ->
  Result.bind (point_float3_optional "v" geometry) (fun source_velocity ->
  Result.bind (if Option.is_none noise_amplitude then Ok None
      else point_float3_optional "rest" geometry) (fun source_rest ->
  let internal_source = if keep_source_attributes then source_point_attribute
    else fresh_point_name "__rdk_replicate_sourcepoint" geometry in
  let internal_index = if keep_source_attributes then source_index_attribute
    else fresh_point_name "__rdk_replicate_sourceindex" geometry in
  let internal_index = if String.equal internal_source internal_index then
      fresh_point_name (internal_index ^ "_") geometry else internal_index in
  Result.bind (Error.unguard @@ Point_generate.run ?cancel ~grain ?points ?count_ids:ids
      ~keep_input ~seed
      ?generated_group ~source_point_attribute:internal_source
      ~source_index_attribute:internal_index ~copy_point_attributes
      ~mode:(Point_generate.Generate_per_point {
        points_per_point; scale_attribute }) geometry) (fun generated ->
  Result.bind (copy_basis (make_basis ()) geometry) (fun frames ->
  let source_count = Geometry.point_count geometry in
  if Geometry.point_count frames mod 4 <> 0
      || Geometry.point_count frames / 4 <> source_count then
    error "internal instancing-frame cardinality mismatch"
  else
  let prefix = if keep_input then source_count else 0 in
  let total = Geometry.point_count generated in
  let generated_count = total - prefix in
  let source_attr = Geometry.find_attribute ~owner:Attribute.Point internal_source
      generated |> Option.get
  and index_attr = Geometry.find_attribute ~owner:Attribute.Point internal_index
      generated |> Option.get in
  let source_map = match Attribute.Private.storage source_attr with
    | Attribute.Int values -> values | _ -> assert false
  and local_map = match Attribute.Private.storage index_attr with
    | Attribute.Int values -> values | _ -> assert false in
  let counts = Array.make source_count 0 in
  for output = prefix to total - 1 do
    let source = source_map.(output) in
    if source >= 0 then counts.(source) <- counts.(source) + 1
  done;
  let quasi = if not quasi_stratified then None else
    let maximum = Array.fold_left max 0 counts in
    let sequence_v = Array.init maximum (radical_inverse 2)
    and sequence_w = Array.init maximum (radical_inverse 3)
    and offset_u = Array.make source_count 0.
    and offset_v = Array.make source_count 0.
    and offset_w = Array.make source_count 0. in
    if source_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
        ~finish:(source_count - 1) (fun source ->
      if source land 4095 = 0 then Cancel.check_opt cancel;
      let identity = match ids with None -> source | Some values -> values.(source) in
      let key = identity * 0x9e3779b in
      offset_u.(source) <- Rays_math.Rand.float_at seed ~index:(key lxor 0x27d4eb2d);
      offset_v.(source) <- Rays_math.Rand.float_at seed ~index:(key lxor 0x6c8e9cf5);
      offset_w.(source) <- Rays_math.Rand.float_at seed ~index:(key lxor 0x5bd1e995));
    Some { offset_u; offset_v; offset_w; sequence_v; sequence_w } in
  let noise = Option.map (fun _ -> Rays_math.Noise.create noise_seed) noise_amplitude in
  let source_positions = Packed.Float3.Private.view (Geometry.positions geometry)
  and frame_positions = Packed.Float3.Private.view (Geometry.positions frames)
  and generated_positions = Packed.Float3.Private.view (Geometry.positions generated) in
  let x = Array.copy generated_positions.x and y = Array.copy generated_positions.y
  and z = Array.copy generated_positions.z in
  let shape_points = if shape = Replicate_custom then Some (Array.make total (-1))
    else None in
  let rx = Rays_math.Mat4.mul (Rays_math.Mat4.rotation_z orientation.z)
      (Rays_math.Mat4.mul (Rays_math.Mat4.rotation_y orientation.y)
        (Rays_math.Mat4.rotation_x orientation.x)) in
  let (l00,l01,l02,_), (l10,l11,l12,_), (l20,l21,l22,_), _ = Rays_math.Mat4.to_rows rx in
  let range_count = if generated_count = 0 then 0
    else (generated_count + grain - 1) / grain in
  let failures = Array.make range_count (-1) in
  if range_count > 0 then Parallel.for_ ~chunk_size:1 ~start:0
      ~finish:(range_count - 1) (fun range ->
    let scratch = create_sample_scratch () in
    let noise_scratch = Rays_math.Noise.Private.create_fbm3_scratch () in
    let first = prefix + (range * grain)
    and last = min total (prefix + ((range + 1) * grain)) in
    for output = first to last - 1 do
      if output land 4095 = 0 then Cancel.check_opt cancel;
      let source = source_map.(output) and local = local_map.(output) in
      let identity = match ids with None -> source | Some values -> values.(source) in
      sample_coordinates_into scratch ~quasi ~seed ~identity ~source ~local
        ~count:counts.(source);
      let shape_point = local_shape_into scratch shape custom_positions in
      (match shape_points with None -> ()
       | Some values -> values.(output) <- shape_point);
      let qx = center.x +. (scratch.(3) *. size.x *. uniform_scale)
      and qy = center.y +. (scratch.(4) *. size.y *. uniform_scale)
      and qz = center.z +. (scratch.(5) *. size.z *. uniform_scale) in
      (match noise, noise_amplitude with
       | Some noise, Some amplitude ->
           let rest_x = match source_rest with None -> source_positions.x.(source)
             | Some values -> values.x.(source) in
           let rest_y = match source_rest with None -> source_positions.y.(source)
             | Some values -> values.y.(source) in
           let rest_z = match source_rest with None -> source_positions.z.(source)
             | Some values -> values.z.(source) in
           let nx = ((rest_x +. qx) *. noise_frequency.x) +. noise_offset.x
           and ny = ((rest_y +. qy) *. noise_frequency.y) +. noise_offset.y
           and nz = ((rest_z +. qz) *. noise_frequency.z) +. noise_offset.z in
           let sx = (2. *. Rays_math.Noise.Private.fbm3_with_scratch noise_scratch noise
               ~octaves:noise_turbulence ~lacunarity:2. ~gain:noise_roughness
               ~x:nx ~y:ny ~z:nz) -. 1.
           and sy = (2. *. Rays_math.Noise.Private.fbm3_with_scratch noise_scratch noise
               ~octaves:noise_turbulence ~lacunarity:2. ~gain:noise_roughness
               ~x:(nz +. 31.416) ~y:nx ~z:ny) -. 1.
           and sz = (2. *. Rays_math.Noise.Private.fbm3_with_scratch noise_scratch noise
               ~octaves:noise_turbulence ~lacunarity:2. ~gain:noise_roughness
               ~x:ny ~y:(nz +. 73.205) ~z:nx) -. 1. in
           let shaped_x = Float.copy_sign
               (abs_float sx ** noise_attenuation) sx
           and shaped_y = Float.copy_sign
               (abs_float sy ** noise_attenuation) sy
           and shaped_z = Float.copy_sign
               (abs_float sz ** noise_attenuation) sz in
           scratch.(3) <- qx +. (shaped_x *. amplitude.x);
           scratch.(4) <- qy +. (shaped_y *. amplitude.y);
           scratch.(5) <- qz +. (shaped_z *. amplitude.z)
       | None, None ->
           scratch.(3) <- qx; scratch.(4) <- qy; scratch.(5) <- qz
       | Some _, None | None, Some _ -> assert false);
      let qx = scratch.(3) and qy = scratch.(4) and qz = scratch.(5) in
      let rotated_x = l00*.qx +. l01*.qy +. l02*.qz
      and rotated_y = l10*.qx +. l11*.qy +. l12*.qz
      and rotated_z = l20*.qx +. l21*.qy +. l22*.qz in
      let base = source * 4 in
      let tx = frame_positions.x.(base) and ty = frame_positions.y.(base)
      and tz = frame_positions.z.(base) in
      let ax = frame_positions.x.(base + 1) -. tx
      and ay = frame_positions.y.(base + 1) -. ty
      and az = frame_positions.z.(base + 1) -. tz
      and bx = frame_positions.x.(base + 2) -. tx
      and by = frame_positions.y.(base + 2) -. ty
      and bz = frame_positions.z.(base + 2) -. tz
      and cx = frame_positions.x.(base + 3) -. tx
      and cy = frame_positions.y.(base + 3) -. ty
      and cz = frame_positions.z.(base + 3) -. tz in
      let ai = if velocity_stretch <> Replicate_velocity_only then 1. else
          let length = sqrt (ax*.ax +. ay*.ay +. az*.az) in
          if length <= 1e-20 then 0. else 1. /. length in
      let bi = if velocity_stretch <> Replicate_velocity_only then 1. else
          let length = sqrt (bx*.bx +. by*.by +. bz*.bz) in
          if length <= 1e-20 then 0. else 1. /. length in
      let ci = if velocity_stretch <> Replicate_velocity_only then 1. else
          let length = sqrt (cx*.cx +. cy*.cy +. cz*.cz) in
          if length <= 1e-20 then 0. else 1. /. length in
      let stretch = match velocity_stretch, source_velocity with
        | Replicate_no_velocity_stretch, _ | _, None -> 1.
        | (Replicate_scaled_velocity | Replicate_velocity_only), Some velocity ->
            let vx = velocity.x.(source) and vy = velocity.y.(source)
            and vz = velocity.z.(source) in
            1. +. (sqrt (vx*.vx +. vy*.vy +. vz*.vz) *. velocity_scale) in
      let rotated_z = rotated_z *. stretch in
      let ox = tx +. ax*.ai*.rotated_x +. bx*.bi*.rotated_y
          +. cx*.ci*.rotated_z
      and oy = ty +. ay*.ai*.rotated_x +. by*.bi*.rotated_y
          +. cy*.ci*.rotated_z
      and oz = tz +. az*.ai*.rotated_x +. bz*.bi*.rotated_y
          +. cz*.ci*.rotated_z in
      if Float.is_finite ox && Float.is_finite oy && Float.is_finite oz then begin
        x.(output) <- ox; y.(output) <- oy; z.(output) <- oz
      end else if failures.(range) < 0 then failures.(range) <- source
    done);
  match Array.find_opt (fun source -> source >= 0) failures with
  | Some source -> error (Printf.sprintf "source point %d produced a non-finite position" source)
  | None ->
      let positions = Packed.Float3.Private.of_owned_exn ~x ~y ~z in
      Result.bind (Geometry.with_positions positions generated) (fun generated ->
      let needs_velocity = Option.is_some source_velocity || radial_velocity <> 0. in
      let generated = if not needs_velocity then generated else
        let vx = Array.make total 0. and vy = Array.make total 0.
        and vz = Array.make total 0. in
        (match source_velocity with
         | None -> ()
         | Some values when keep_input ->
             Array.blit values.x 0 vx 0 prefix; Array.blit values.y 0 vy 0 prefix;
             Array.blit values.z 0 vz 0 prefix
         | Some _ -> ());
        if generated_count > 0 then Parallel.for_ ~chunk_size:grain ~start:prefix
            ~finish:(total - 1) (fun output ->
          if output land 4095 = 0 then Cancel.check_opt cancel;
          let source = source_map.(output) in
          let inherited_x = match source_velocity with None -> 0.
            | Some values -> values.x.(source) *. inherit_velocity in
          let inherited_y = match source_velocity with None -> 0.
            | Some values -> values.y.(source) *. inherit_velocity in
          let inherited_z = match source_velocity with None -> 0.
            | Some values -> values.z.(source) *. inherit_velocity in
          vx.(output) <- inherited_x +. radial_velocity *.
              (x.(output) -. source_positions.x.(source));
          vy.(output) <- inherited_y +. radial_velocity *.
              (y.(output) -. source_positions.y.(source));
          vz.(output) <- inherited_z +. radial_velocity *.
              (z.(output) -. source_positions.z.(source)));
        let value = Packed.Float3.Private.of_owned_exn ~x:vx ~y:vy ~z:vz in
        let attribute = Attribute.create_owned ~owner:Attribute.Point ~name:"v"
            (Attribute.Float3 value) |> Result.get_ok in
        Geometry.with_attribute attribute generated |> Result.get_ok in
      let generated = match shape_points with
        | None -> generated
        | Some values ->
            let attribute = Attribute.create_owned ~owner:Attribute.Point
                ~name:"shapeptnum" (Attribute.Int values) |> Result.get_ok in
            Geometry.with_attribute attribute generated |> Result.get_ok in
      Result.bind (transform_inherited_attributes ?cancel ~grain
          ~pattern:transform_pattern
          ~prefix ~source_map ~counts ~frame_positions generated) (fun generated ->
      let generated = if keep_source_attributes then generated else
          generated |> Geometry.without_attribute ~owner:Attribute.Point internal_source
          |> Geometry.without_attribute ~owner:Attribute.Point internal_index in
      Ok generated))
  ))))))
