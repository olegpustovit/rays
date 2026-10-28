let point (x, y) = { Path.x; y }

let geometry_of_mesh color (mesh : Path.mesh) =
  let vertices = Array.make (Array.length mesh.vertices * 2) 0. in
  Array.iteri
    (fun index (point : Path.point) ->
      Array.unsafe_set vertices (index * 2) point.x;
      Array.unsafe_set vertices ((index * 2) + 1) point.y)
    mesh.vertices;
  { Render_ir.vertices; indices = mesh.indices; color }

let path_error operation = function
  | Ok mesh -> mesh
  | Error Path.Empty_path -> { Path.vertices = [||]; indices = [||] }
  | Error _ -> invalid_arg operation

let fill_path color path =
  geometry_of_mesh color
    (path_error "Shape2 fill" (Path.tessellate ~tolerance:0.25 ~fill_rule:Path.Non_zero path))

let stroke_path ?(width = 1.) color path =
  geometry_of_mesh color
    (path_error "Shape2 stroke"
       (Path.stroke ~tolerance:0.25 ~width ~cap:Path.Butt ~join:Path.Miter ~miter_limit:4. path))

let styled ?fill ?stroke path =
  match (fill, stroke) with
  | None, None -> [||]
  | Some fill, None -> [| fill_path fill path |]
  | None, Some stroke -> [| stroke_path stroke path |]
  | Some fill, Some stroke -> [| fill_path fill path; stroke_path stroke path |]

let path_of_points ~closed points =
  match points with
  | [] -> Path.of_commands [||]
  | first :: rest ->
      let commands =
        (Path.Move_to (point first) :: List.map (fun value -> Path.Line_to (point value)) rest)
        @ if closed then [ Path.Close ] else []
      in
      Path.of_commands (Array.of_list commands)

let line ~from_ ~to_ ~width ~color =
  stroke_path ~width:(max 1. width) color (path_of_points ~closed:false [ from_; to_ ])

let polygon points ~fill ~stroke =
  let fill = match (fill, stroke) with None, None -> Some 0xffffffffl | _ -> fill in
  styled ?fill ?stroke (path_of_points ~closed:true points)

let polyline points ~color = stroke_path color (path_of_points ~closed:false points)
let path ~fill ~stroke path = styled ?fill ?stroke path

let rounded_rect ~width ~height ~radius ~fill ~stroke =
  let fill = match (fill, stroke) with None, None -> Some 0xffffffffl | _ -> fill in
  let radius = max 0. (min radius (min (abs_float width) (abs_float height) /. 2.)) in
  let r = radius in
  let k = 0.5522847498307936 *. r in
  let p x y = { Path.x; y } in
  let path =
    Path.of_commands
      [|
        Path.Move_to (p r 0.);
        Line_to (p (width -. r) 0.);
        Cubic_to (p (width -. r +. k) 0., p width (r -. k), p width r);
        Line_to (p width (height -. r));
        Cubic_to (p width (height -. r +. k), p (width -. r +. k) height, p (width -. r) height);
        Line_to (p r height);
        Cubic_to (p (r -. k) height, p 0. (height -. r +. k), p 0. (height -. r));
        Line_to (p 0. r);
        Cubic_to (p 0. (r -. k), p (r -. k) 0., p r 0.);
        Close;
      |]
  in
  styled ?fill ?stroke path

let ellipse_points (cx, cy) rx ry =
  List.init 32 (fun index ->
      let angle = 2. *. Float.pi *. float index /. 32. in
      (cx +. (rx *. cos angle), cy +. (ry *. sin angle)))

let ellipse ~center ~rx ~ry ~fill ~stroke = polygon (ellipse_points center rx ry) ~fill ~stroke
