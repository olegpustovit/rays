
let selected selection primitive = match selection with
  | None -> true
  | Some group -> Group.mem primitive group

let polygon_area_vector ?cancel ~grain ~need_inverse ~first_failure ?primitives
    ~operation geometry =
  let topology = Geometry.topology geometry in
  let source = Topology.View.view topology
  and positions = Packed.Float3.View.view (Geometry.positions geometry) in
  let count = Topology.primitive_count topology in
  let x = Array.make count 0. and y = Array.make count 0.
  and z = Array.make count 0. in
  let inverse_length = if need_inverse then Array.make count 0. else [||] in
  let ranges = if count = 0 then 0 else (count + grain - 1) / grain in
  let errors = Array.make ranges (-1) in
  Parallel.for_ ~chunk_size:1 ~start:0 ~finish:(ranges - 1) (fun range ->
    let first_primitive = range * grain
    and last_primitive = min count ((range + 1) * grain) in
    for primitive = first_primitive to last_primitive - 1 do
      Cancel.poll cancel primitive 4095;
      if (match primitives with None -> true | Some group -> Group.mem primitive group)
         && Bytes.get source.primitive_kinds primitive = '\000' then begin
        let first = source.primitive_offsets.(primitive)
        and last = source.primitive_offsets.(primitive + 1) in
        let anchor = source.vertex_points.(first) in
        let ax = positions.x.(anchor) and ay = positions.y.(anchor)
        and az = positions.z.(anchor) in
        for vertex = first + 1 to last - 2 do
          let b = source.vertex_points.(vertex)
          and c = source.vertex_points.(vertex + 1) in
          let ux = positions.x.(b) -. ax and uy = positions.y.(b) -. ay
          and uz = positions.z.(b) -. az and vx = positions.x.(c) -. ax
          and vy = positions.y.(c) -. ay and vz = positions.z.(c) -. az in
          x.(primitive) <- x.(primitive) +. ((uy *. vz) -. (uz *. vy));
          y.(primitive) <- y.(primitive) +. ((uz *. vx) -. (ux *. vz));
          z.(primitive) <- z.(primitive) +. ((ux *. vy) -. (uy *. vx))
        done;
        if not (Float.is_finite x.(primitive) && Float.is_finite y.(primitive)
            && Float.is_finite z.(primitive)) then begin
          if not first_failure || errors.(range) < 0 then
            errors.(range) <- primitive
        end else if need_inverse then begin
          let nx = x.(primitive) and ny = y.(primitive) and nz = z.(primitive) in
          let ax = abs_float nx and ay = abs_float ny
          and az = abs_float nz in
          let scale = let value = if ax > ay then ax else ay in
            if value > az then value else az in
          if scale > 0. then begin
            let sx = nx /. scale and sy = ny /. scale
            and sz = nz /. scale in
            inverse_length.(primitive) <- (1. /. scale)
              /. sqrt ((sx *. sx) +. (sy *. sy) +. (sz *. sz))
          end
        end
      end
    done);
  match Array.find_opt (fun primitive -> primitive >= 0) errors with
  | Some primitive -> Error (Printf.sprintf
      "%s: non-finite geometric normal at primitive %d" operation primitive)
  | None -> Ok ((x, y, z), inverse_length)

let compute ?cancel ?(grain = 16_384) ?primitives ~operation geometry =
  if grain <= 0 then Error (operation ^ ": grain must be positive")
  else
    let topology = Geometry.topology geometry in
    let primitive_count = Topology.primitive_count topology in
    match primitives with
    | Some group when Group.owner group <> Group.Primitive ->
        Error (operation ^ ": selection must own primitives")
    | Some group when Group.length group <> primitive_count ->
        Error (operation ^ ": selection length does not match primitive count")
    | _ ->
        let topology_view = Topology.View.view topology
        and positions = Packed.Float3.View.view (Geometry.positions geometry) in
        let nx = Array.make primitive_count 0.
        and ny = Array.make primitive_count 0.
        and nz = Array.make primitive_count 0. in
        let invalid = Atomic.make false in
        if primitive_count > 0 then
          Parallel.for_ ~chunk_size:grain ~start:0 ~finish:(primitive_count - 1)
            (fun primitive ->
              Cancel.poll cancel primitive 16_383;
              if selected primitives primitive then begin
                if Topology.primitive_kind topology primitive <> Topology.Polygon then
                  Atomic.set invalid true
                else begin
                  let first = topology_view.primitive_offsets.(primitive)
                  and last = topology_view.primitive_offsets.(primitive + 1) in
                  let first_point = topology_view.vertex_points.(first) in
                  let ax = positions.x.(first_point)
                  and ay = positions.y.(first_point)
                  and az = positions.z.(first_point) in
                  for vertex = first + 1 to last - 2 do
                    Cancel.poll cancel vertex 16_383;
                    let b = topology_view.vertex_points.(vertex)
                    and c = topology_view.vertex_points.(vertex + 1) in
                    let ux = positions.x.(b) -. ax
                    and uy = positions.y.(b) -. ay
                    and uz = positions.z.(b) -. az
                    and vx = positions.x.(c) -. ax
                    and vy = positions.y.(c) -. ay
                    and vz = positions.z.(c) -. az in
                    nx.(primitive) <- nx.(primitive) +. uy *. vz -. uz *. vy;
                    ny.(primitive) <- ny.(primitive) +. uz *. vx -. ux *. vz;
                    nz.(primitive) <- nz.(primitive) +. ux *. vy -. uy *. vx
                  done;
                  let x = nx.(primitive) and y = ny.(primitive)
                  and z = nz.(primitive) in
                  let scale = abs_float x in
                  let scale = if abs_float y > scale then abs_float y else scale in
                  let scale = if abs_float z > scale then abs_float z else scale in
                  if scale = 0. || not (Float.is_finite x && Float.is_finite y
                      && Float.is_finite z) then Atomic.set invalid true
                  else begin
                    let x = x /. scale and y = y /. scale and z = z /. scale in
                    let inverse = 1. /. sqrt (x *. x +. y *. y +. z *. z) in
                    nx.(primitive) <- x *. inverse;
                    ny.(primitive) <- y *. inverse;
                    nz.(primitive) <- z *. inverse
                  end
                end
              end);
        if Atomic.get invalid then Error (operation
            ^ ": selected primitives must be finite non-degenerate polygons")
        else Ok (nx, ny, nz)
