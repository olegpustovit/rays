open Rays_math

exception Circle_error of string
let fail message = raise (Circle_error message)

let[@inline always] maximum_abs left right =
  let left = abs_float left and right = abs_float right in
  if left > right then left else right

let write_unit ~canonical output_x output_y output_z at x y z =
  let scale = maximum_abs x (maximum_abs y z) in
  if scale = 0. || not (Float.is_finite scale) then false
  else
    let x = x /. scale and y = y /. scale and z = z /. scale in
    let length = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
    if length = 0. || not (Float.is_finite length) then false
    else begin
      let x = x /. length and y = y /. length and z = z /. length in
      let negative = if canonical then begin
          let ax = abs_float x and ay = abs_float y and az = abs_float z in
          if ax >= ay && ax >= az then x < 0.
          else if ay >= az then y < 0. else z < 0.
        end else false in
      output_x.(at) <- if negative then -.x else x;
      output_y.(at) <- if negative then -.y else y;
      output_z.(at) <- if negative then -.z else z;
      true
    end

let atomic_min target candidate =
  let rec update current =
    if candidate >= current then ()
    else if Atomic.compare_and_set target current candidate then ()
    else update (Atomic.get target)
  in
  update (Atomic.get target)

let write_eigenvector output_x output_y output_z at xx xy xz yy yz zz off
    minimum middle maximum =
  if not (Float.is_finite middle && Float.is_finite maximum) || maximum <= 0.
      || middle <= (512. *. Float.epsilon *. maximum) then false
  else if off = 0. then begin
    if xx <= yy && xx <= zz then begin
      output_x.(at) <- 1.; output_y.(at) <- 0.; output_z.(at) <- 0.
    end else if yy <= zz then begin
      output_x.(at) <- 0.; output_y.(at) <- 1.; output_z.(at) <- 0.
    end else begin
      output_x.(at) <- 0.; output_y.(at) <- 0.; output_z.(at) <- 1.
    end;
    true
  end else begin
      let a = xx -. minimum and d = yy -. minimum and f = zz -. minimum in
      let x01 = (xy *. yz) -. (xz *. d)
      and y01 = (xz *. xy) -. (a *. yz)
      and z01 = (a *. d) -. (xy *. xy)
      and x02 = (xy *. f) -. (xz *. yz)
      and y02 = (xz *. xz) -. (a *. f)
      and z02 = (a *. yz) -. (xy *. xz)
      and x12 = (d *. f) -. (yz *. yz)
      and y12 = (yz *. xz) -. (xy *. f)
      and z12 = (xy *. yz) -. (d *. xz) in
      let n01 = (x01 *. x01) +. (y01 *. y01) +. (z01 *. z01)
      and n02 = (x02 *. x02) +. (y02 *. y02) +. (z02 *. z02)
      and n12 = (x12 *. x12) +. (y12 *. y12) +. (z12 *. z12) in
      if n01 >= n02 && n01 >= n12 then
        write_unit ~canonical:true output_x output_y output_z at x01 y01 z01
      else if n02 >= n12 then
        write_unit ~canonical:true output_x output_y output_z at x02 y02 z02
      else write_unit ~canonical:true output_x output_y output_z at x12 y12 z12
  end

let write_smallest_normal output_x output_y output_z at xx xy xz yy yz zz =
  let off = (xy *. xy) +. (xz *. xz) +. (yz *. yz) in
  if off = 0. then begin
    let minimum = Float.min xx (Float.min yy zz)
    and maximum = Float.max xx (Float.max yy zz) in
    write_eigenvector output_x output_y output_z at xx xy xz yy yz zz off
      minimum ((xx +. yy +. zz) -. minimum -. maximum) maximum
  end else begin
    let q = (xx +. yy +. zz) /. 3. in
    let ax = xx -. q and ay = yy -. q and az = zz -. q in
    let p2 = (ax *. ax) +. (ay *. ay) +. (az *. az) +. (2. *. off) in
    let p = sqrt (p2 /. 6.) in
    if p = 0. || not (Float.is_finite p) then
      write_eigenvector output_x output_y output_z at xx xy xz yy yz zz off
        q q q
    else begin
      let bxx = ax /. p and bxy = xy /. p and bxz = xz /. p
      and byy = ay /. p and byz = yz /. p and bzz = az /. p in
      let determinant =
        (bxx *. ((byy *. bzz) -. (byz *. byz)))
        -. (bxy *. ((bxy *. bzz) -. (byz *. bxz)))
        +. (bxz *. ((bxy *. byz) -. (byy *. bxz))) in
      let r = Float.max (-1.) (Float.min 1. (determinant /. 2.)) in
      let phi = acos r /. 3. in
      let maximum = q +. (2. *. p *. cos phi) in
      let minimum = q +. (2. *. p *. cos
          (phi +. (2. *. Float.pi /. 3.))) in
      write_eigenvector output_x output_y output_z at xx xy xz yy yz zz off
        minimum ((3. *. q) -. minimum -. maximum) maximum
    end
  end

let write_plane_basis axis_u_x axis_u_y axis_u_z axis_v_x axis_v_y axis_v_z
    at nx ny nz =
  let ax = abs_float nx and ay = abs_float ny and az = abs_float nz in
  let ux = if ax <= ay && ax <= az then 0.
      else if ay <= az then -.nz else ny
  and uy = if ax <= ay && ax <= az then nz
      else if ay <= az then 0. else -.nx
  and uz = if ax <= ay && ax <= az then -.ny
      else if ay <= az then nx else 0. in
  if not (write_unit ~canonical:false axis_u_x axis_u_y axis_u_z at ux uy uz)
  then false
  else begin
    let ux = axis_u_x.(at) and uy = axis_u_y.(at) and uz = axis_u_z.(at) in
    axis_v_x.(at) <- (ny *. uz) -. (nz *. uy);
    axis_v_y.(at) <- (nz *. ux) -. (nx *. uz);
    axis_v_z.(at) <- (nx *. uy) -. (ny *. ux);
    true
  end

let run ?cancel ?(grain = 16_384) ?edges ?radius
    ?(scale = Rays_math.Vec3.create 1. 1. 1.) ?output_group geometry =
  Error.guard ~operation:"circle_from_edges" ~code:"invalid_circle" @@ fun () ->
  try
    if grain <= 0 then fail "Circle from Edges grain must be positive";
    (match radius with
     | Some value when not (Float.is_finite value) || value <= 0. ->
         fail "Circle from Edges radius must be finite and positive"
     | None | Some _ -> ());
    if not (Float.is_finite scale.Rays_math.Vec3.x && Float.is_finite scale.y && Float.is_finite scale.z) then
      fail "Circle from Edges scale must be finite";
    Option.iter (fun name -> if String.trim name = "" then
      fail "Circle from Edges output edge group name must not be empty")
      output_group;
    Cancel.check_opt cancel;
    let topology = Geometry.topology geometry in
    let index = Topology_index.create ?cancel topology in
    let view = Topology_index.Private.view index in
    let edge_count = Array.length view.edge_a
    and point_count = Geometry.point_count geometry in
    (match edges with
     | Some selection when Edge_group.topology_data_id selection
         <> Topology.data_id topology ->
         fail "Circle from Edges selection belongs to a different topology"
     | Some selection when Edge_group.length selection <> edge_count ->
         fail "Circle from Edges selection length does not match topology"
     | None | Some _ -> ());
    let selected edge = match edges with
      | Some selection -> Edge_group.mem edge selection
      | None -> view.edge_offsets.(edge + 1) - view.edge_offsets.(edge) = 1 in
    let install_group geometry = match output_group with
      | None -> Ok geometry
      | Some name ->
          let group = Edge_group.init ~grain ~topology ~index ~name selected in
          Geometry.with_edge_group group geometry in
    let selected_count = match edges with
      | Some selection -> Edge_group.cardinality selection
      | None ->
          let count = ref 0 in
          for edge = 0 to edge_count - 1 do if selected edge then incr count done;
          !count in
    if selected_count = 0 then install_group geometry
    else begin
      let positions = Packed.Float3.Private.view (Geometry.positions geometry) in
      let parent = Array.init point_count Fun.id
      and rank = Bytes.make point_count '\000'
      and touched = Bytes.make point_count '\000'
      and degree = Array.make point_count 0 in
      let root point =
        let representative = ref point in
        while parent.(!representative) <> !representative do
          representative := parent.(!representative)
        done;
        let representative = !representative and current = ref point in
        while parent.(!current) <> representative do
          let next = parent.(!current) in
          parent.(!current) <- representative;
          current := next
        done;
        representative in
      let union left right =
        let left = root left and right = root right in
        if left <> right then begin
          let lr = Char.code (Bytes.get rank left)
          and rr = Char.code (Bytes.get rank right) in
          if lr < rr then parent.(left) <- right
          else if rr < lr then parent.(right) <- left
          else begin
            let representative, child = if left < right then left,right else right,left in
            parent.(child) <- representative;
            Bytes.set rank representative (Char.chr (lr + 1))
          end
        end in
      let invalid_edge = ref (-1) in
      for edge = 0 to edge_count - 1 do
        if edge land 4095 = 0 then Cancel.check_opt cancel;
        if selected edge then begin
          let a = view.edge_a.(edge) and b = view.edge_b.(edge) in
          if !invalid_edge < 0 && (a = b
              || not (Float.is_finite positions.x.(a) && Float.is_finite positions.y.(a)
                && Float.is_finite positions.z.(a) && Float.is_finite positions.x.(b)
                && Float.is_finite positions.y.(b) && Float.is_finite positions.z.(b))) then
            invalid_edge := edge;
          Bytes.set touched a '\001'; Bytes.set touched b '\001';
          degree.(a) <- degree.(a) + 1; degree.(b) <- degree.(b) + 1;
          union a b
        end
      done;
      if !invalid_edge >= 0 then fail (Printf.sprintf
          "Circle from Edges selected edge %d has an invalid endpoint"
          !invalid_edge);
      let root_component = Array.make point_count (-1)
      and point_component = Array.make point_count (-1)
      and component_count = ref 0 in
      for point = 0 to point_count - 1 do
        if point land 4095 = 0 then Cancel.check_opt cancel;
        if Bytes.get touched point <> '\000' then begin
          let representative = root point in
          let component = if root_component.(representative) >= 0 then
              root_component.(representative)
            else begin
              let value = !component_count in
              incr component_count;
              root_component.(representative) <- value;
              value
            end in
          point_component.(point) <- component
        end
      done;
      let component_count = !component_count in
      let counts = Array.make component_count 0
      and endpoints = Array.make component_count 0
      and invalid_degree = ref (-1) in
      for point = 0 to point_count - 1 do
        let component = point_component.(point) in
        if component >= 0 then begin
          counts.(component) <- counts.(component) + 1;
          if degree.(point) = 1 then endpoints.(component) <- endpoints.(component) + 1
          else if degree.(point) <> 2 && !invalid_degree < 0 then
            invalid_degree := point
        end
      done;
      if !invalid_degree >= 0 then fail (Printf.sprintf
          "Circle from Edges selected component branches at point %d"
          !invalid_degree);
      for component = 0 to component_count - 1 do
        if counts.(component) < 3 then fail (Printf.sprintf
            "Circle from Edges component %d has fewer than three points"
            component);
        if endpoints.(component) <> 0 && endpoints.(component) <> 2 then
          fail (Printf.sprintf
            "Circle from Edges component %d is not a simple path or loop"
            component)
      done;
      let offsets = Array.make (component_count + 1) 0 in
      for component = 0 to component_count - 1 do
        offsets.(component + 1) <- offsets.(component) + counts.(component)
      done;
      let members = Array.make offsets.(component_count) 0
      and next = Array.copy offsets in
      for point = 0 to point_count - 1 do
        let component = point_component.(point) in
        if component >= 0 then begin
          members.(next.(component)) <- point;
          next.(component) <- next.(component) + 1
        end
      done;
      let center_x = Array.make component_count 0.
      and center_y = Array.make component_count 0.
      and center_z = Array.make component_count 0.
      and axis_u_x = Array.make component_count 0.
      and axis_u_y = Array.make component_count 0.
      and axis_u_z = Array.make component_count 0.
      and axis_v_x = Array.make component_count 0.
      and axis_v_y = Array.make component_count 0.
      and axis_v_z = Array.make component_count 0.
      and fitted_radius = Array.make component_count 0.
      and coordinate_scale = Array.make component_count 1.
      and errors = Array.make component_count 0 in
      if component_count > 0 then Parallel.for_
          ~chunk_size:(max 1 (grain / 16)) ~start:0
          ~finish:(component_count - 1) (fun component ->
        Cancel.check_opt cancel;
        let first = offsets.(component) and last = offsets.(component + 1) in
        let coordinate = ref 0. in
        for at = first to last - 1 do
          let point = members.(at) in
          coordinate := maximum_abs !coordinate positions.x.(point);
          coordinate := maximum_abs !coordinate positions.y.(point);
          coordinate := maximum_abs !coordinate positions.z.(point)
        done;
        let coordinate = if !coordinate = 0. then 1. else !coordinate in
        coordinate_scale.(component) <- coordinate;
        let sx = ref 0. and sy = ref 0. and sz = ref 0. in
        for at = first to last - 1 do
          let point = members.(at) in
          sx := !sx +. (positions.x.(point) /. coordinate);
          sy := !sy +. (positions.y.(point) /. coordinate);
          sz := !sz +. (positions.z.(point) /. coordinate)
        done;
        let inverse = 1. /. Float.of_int (last - first) in
        let mx = !sx *. inverse and my = !sy *. inverse
        and mz = !sz *. inverse in
        let xx = ref 0. and xy = ref 0. and xz = ref 0.
        and yy = ref 0. and yz = ref 0. and zz = ref 0. in
        for at = first to last - 1 do
          let point = members.(at) in
          let dx = (positions.x.(point) /. coordinate) -. mx
          and dy = (positions.y.(point) /. coordinate) -. my
          and dz = (positions.z.(point) /. coordinate) -. mz in
          xx := !xx +. (dx *. dx); xy := !xy +. (dx *. dy);
          xz := !xz +. (dx *. dz); yy := !yy +. (dy *. dy);
          yz := !yz +. (dy *. dz); zz := !zz +. (dz *. dz)
        done;
        let stable_normal = write_smallest_normal axis_u_x axis_u_y axis_u_z
            component !xx !xy !xz !yy !yz !zz in
        if not stable_normal then errors.(component) <- 1
        else begin
          let nx = axis_u_x.(component) and ny = axis_u_y.(component)
          and nz = axis_u_z.(component) in
          if not (write_plane_basis axis_u_x axis_u_y axis_u_z
              axis_v_x axis_v_y axis_v_z component nx ny nz) then
            errors.(component) <- 1
          else begin
            let ux = axis_u_x.(component) and uy = axis_u_y.(component)
            and uz = axis_u_z.(component) and vx = axis_v_x.(component)
            and vy = axis_v_y.(component) and vz = axis_v_z.(component) in
            let uu = ref 0. and uv = ref 0. and vv = ref 0.
            and ur2 = ref 0. and vr2 = ref 0. in
            for at = first to last - 1 do
              let point = members.(at) in
              let dx = (positions.x.(point) /. coordinate) -. mx
              and dy = (positions.y.(point) /. coordinate) -. my
              and dz = (positions.z.(point) /. coordinate) -. mz in
              let u = (dx *. ux) +. (dy *. uy) +. (dz *. uz)
              and v = (dx *. vx) +. (dy *. vy) +. (dz *. vz) in
              let r2 = (u *. u) +. (v *. v) in
              uu := !uu +. (u *. u); uv := !uv +. (u *. v);
              vv := !vv +. (v *. v); ur2 := !ur2 +. (u *. r2);
              vr2 := !vr2 +. (v *. r2)
            done;
            let determinant = (!uu *. !vv) -. (!uv *. !uv) in
            let product = abs_float (!uu *. !vv) in
            if determinant <= 0. || determinant <=
                (512. *. Float.epsilon *. product) then
              errors.(component) <- 1
            else begin
              let rhs_u = -. !ur2 and rhs_v = -. !vr2 in
              let d = ((rhs_u *. !vv) -. (!uv *. rhs_v)) /. determinant
              and e = ((!uu *. rhs_v) -. (!uv *. rhs_u)) /. determinant in
              let cu = -.d *. 0.5 and cv = -.e *. 0.5 in
              let cx = mx +. (cu *. ux) +. (cv *. vx)
              and cy = my +. (cu *. uy) +. (cv *. vy)
              and cz = mz +. (cu *. uz) +. (cv *. vz) in
              let sum = ref 0. and correction = ref 0. in
              for at = first to last - 1 do
                let point = members.(at) in
                let dx = (positions.x.(point) /. coordinate) -. cx
                and dy = (positions.y.(point) /. coordinate) -. cy
                and dz = (positions.z.(point) /. coordinate) -. cz in
                let u = (dx *. ux) +. (dy *. uy) +. (dz *. uz)
                and v = (dx *. vx) +. (dy *. vy) +. (dz *. vz) in
                let value = sqrt ((u *. u) +. (v *. v)) in
                let adjusted = value -. !correction in
                let next = !sum +. adjusted in
                correction := (next -. !sum) -. adjusted;
                sum := next
              done;
              let fitted = match radius with
                | None -> !sum *. inverse
                | Some value -> value /. coordinate in
              if not (Float.is_finite cx && Float.is_finite cy && Float.is_finite cz && Float.is_finite fitted)
                  || fitted <= 0. then errors.(component) <- 2
              else begin
                center_x.(component) <- cx; center_y.(component) <- cy;
                center_z.(component) <- cz;
                fitted_radius.(component) <- fitted
              end
            end
          end
        end);
      for component = 0 to component_count - 1 do
        if errors.(component) <> 0 then fail (Printf.sprintf
            "Circle from Edges component %d %s" component
            (if errors.(component) = 1 then "is collinear or has no stable fit"
             else "has an unrepresentable radius or center"))
      done;
      let x = Array.copy positions.x and y = Array.copy positions.y
      and z = Array.copy positions.z and first_bad = Atomic.make max_int in
      if point_count > 0 then Parallel.for_ ~chunk_size:grain ~start:0
          ~finish:(point_count - 1) (fun point ->
        if point land 4095 = 0 then Cancel.check_opt cancel;
        let component = point_component.(point) in
        if component >= 0 then begin
          let coordinate = coordinate_scale.(component) in
          let dx = (positions.x.(point) /. coordinate) -. center_x.(component)
          and dy = (positions.y.(point) /. coordinate) -. center_y.(component)
          and dz = (positions.z.(point) /. coordinate) -. center_z.(component) in
          let du = (dx *. axis_u_x.(component))
              +. (dy *. axis_u_y.(component)) +. (dz *. axis_u_z.(component))
          and dv = (dx *. axis_v_x.(component))
              +. (dy *. axis_v_y.(component)) +. (dz *. axis_v_z.(component)) in
          let radial = sqrt ((du *. du) +. (dv *. dv)) in
          if radial = 0. || not (Float.is_finite radial) then
            atomic_min first_bad point
          else begin
            let amount = fitted_radius.(component) /. radial in
            let qx = ((du *. axis_u_x.(component))
                +. (dv *. axis_v_x.(component))) *. amount
            and qy = ((du *. axis_u_y.(component))
                +. (dv *. axis_v_y.(component))) *. amount
            and qz = ((du *. axis_u_z.(component))
                +. (dv *. axis_v_z.(component))) *. amount in
            let px = coordinate *. (center_x.(component) +. (scale.x *. qx))
            and py = coordinate *. (center_y.(component) +. (scale.y *. qy))
            and pz = coordinate *. (center_z.(component) +. (scale.z *. qz)) in
            if Float.is_finite px && Float.is_finite py && Float.is_finite pz then begin
              x.(point) <- px; y.(point) <- py; z.(point) <- pz
            end else atomic_min first_bad point
          end
        end);
      if Atomic.get first_bad <> max_int then fail (Printf.sprintf
          "Circle from Edges produced an invalid position at point %d"
          (Atomic.get first_bad));
      let output = Geometry.with_positions
          (Packed.Float3.Private.of_owned_exn ~x ~y ~z) geometry |> function
        | Ok value -> value | Error message -> fail message in
      let output = output
          |> Geometry.without_attribute ~owner:Attribute.Point "N"
          |> Geometry.without_attribute ~owner:Attribute.Vertex "N" in
      install_group output
    end
  with Circle_error message | Invalid_argument message -> Error message
