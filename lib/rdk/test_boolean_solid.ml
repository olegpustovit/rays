open Rdk
open Rdk_test_support

module Solid = Rdk_boolean.Boolean_solid
module Extract = Rdk_boolean.Boolean_extract
module Seam = Rdk_boolean.Boolean_seam
module Materialization = Rdk_boolean.Boolean_materialization
module Complex = Rdk_boolean.Boolean_complex


let geometry points triangles =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.polygons_owned ~point_count:count
      ~vertex_points:triangles
      ~primitive_offsets:(Array.init ((Array.length triangles / 3) + 1)
        (fun primitive -> primitive * 3)) |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let polygon_geometry points vertex_points primitive_offsets =
  let count = Array.length points in
  let x = Array.init count (fun point -> let x, _, _ = points.(point) in x)
  and y = Array.init count (fun point -> let _, y, _ = points.(point) in y)
  and z = Array.init count (fun point -> let _, _, z = points.(point) in z) in
  let topology = Topology.polygons_owned ~point_count:count ~vertex_points
      ~primitive_offsets |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let tetra ~origin:(ox, oy, oz) size = geometry
    [|ox,oy,oz; ox+.size,oy,oz; ox,oy+.size,oz; ox,oy,oz+.size|]
    [|0;2;1; 0;1;3; 1;2;3; 2;0;3|]

let cube ~origin:(ox, oy, oz) size = geometry
    [|ox,oy,oz; ox+.size,oy,oz; ox+.size,oy+.size,oz; ox,oy+.size,oz;
      ox,oy,oz+.size; ox+.size,oy,oz+.size;
      ox+.size,oy+.size,oz+.size; ox,oy+.size,oz+.size|]
    [|0;3;2; 0;2;1; 4;5;6; 4;6;7; 0;1;5; 0;5;4;
      3;7;6; 3;6;2; 0;4;7; 0;7;3; 1;2;6; 1;6;5|]

let cube_quads ~origin:(ox, oy, oz) size = polygon_geometry
    [|ox,oy,oz; ox+.size,oy,oz; ox+.size,oy+.size,oz; ox,oy+.size,oz;
      ox,oy,oz+.size; ox+.size,oy,oz+.size;
      ox+.size,oy+.size,oz+.size; ox,oy+.size,oz+.size|]
    [|0;3;2;1; 4;5;6;7; 0;1;5;4; 3;7;6;2; 0;4;7;3; 1;2;6;5|]
    [|0;4;8;12;16;20;24|]

let torus ~major_segments ~minor_segments ~center_x ~major ~minor =
  let point_count = major_segments * minor_segments in
  let points = Array.make point_count (0., 0., 0.) in
  let point u v = (u mod major_segments) * minor_segments
      + (v mod minor_segments) in
  for u = 0 to major_segments - 1 do
    let alpha = 2. *. Float.pi *. float_of_int u /. float_of_int major_segments in
    for v = 0 to minor_segments - 1 do
      let beta = 2. *. Float.pi *. float_of_int v /. float_of_int minor_segments in
      let radial = major +. (minor *. cos beta) in
      (* The stress cutter rotates the ordinary XZ torus by pi/2 around X. *)
      let source_y = minor *. sin beta and source_z = radial *. sin alpha in
      let cosine = cos (Float.pi *. 0.5) and sine = sin (Float.pi *. 0.5) in
      points.(point u v) <-
        center_x +. (radial *. cos alpha),
        (cosine *. source_y) -. (sine *. source_z),
        (sine *. source_y) +. (cosine *. source_z)
    done
  done;
  let triangles = Array.make (major_segments * minor_segments * 6) 0
  and cursor = ref 0 in
  let triangle a b c =
    triangles.(!cursor) <- a; triangles.(!cursor + 1) <- b;
    triangles.(!cursor + 2) <- c; cursor := !cursor + 3 in
  for u = 0 to major_segments - 1 do
    for v = 0 to minor_segments - 1 do
      let a = point u v and b = point u (v + 1)
      and c = point (u + 1) (v + 1) and d = point (u + 1) v in
      triangle a b c; triangle a c d
    done
  done;
  geometry points triangles

let merge_geometry geometries =
  let point_count = Array.fold_left
      (fun count geometry -> count + Geometry.point_count geometry) 0 geometries
  and vertex_count = Array.fold_left
      (fun count geometry -> count + Geometry.vertex_count geometry) 0 geometries
  and primitive_count = Array.fold_left
      (fun count geometry -> count + Geometry.primitive_count geometry) 0 geometries in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. and vertices = Array.make vertex_count 0
  and offsets = Array.make (primitive_count + 1) 0 in
  let point_base = ref 0 and vertex_base = ref 0 and primitive_base = ref 0 in
  Array.iter (fun source ->
    let positions = Packed.Float3.View.view (Geometry.positions source)
    and topology = Topology.View.view (Geometry.topology source) in
    Array.blit positions.x 0 x !point_base (Array.length positions.x);
    Array.blit positions.y 0 y !point_base (Array.length positions.y);
    Array.blit positions.z 0 z !point_base (Array.length positions.z);
    Array.iteri (fun vertex point ->
      vertices.(!vertex_base + vertex) <- !point_base + point)
      topology.vertex_points;
    for primitive = 0 to Geometry.primitive_count source - 1 do
      offsets.(!primitive_base + primitive) <-
        !vertex_base + topology.primitive_offsets.(primitive)
    done;
    point_base := !point_base + Geometry.point_count source;
    vertex_base := !vertex_base + Geometry.vertex_count source;
    primitive_base := !primitive_base + Geometry.primitive_count source) geometries;
  offsets.(primitive_count) <- vertex_count;
  let topology = Topology.polygons_owned ~point_count ~vertex_points:vertices
      ~primitive_offsets:offsets |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let signature geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry) in
  Array.copy positions.x, Array.copy positions.y, Array.copy positions.z,
  Array.copy topology.vertex_points, Array.copy topology.primitive_offsets

let signed_volume geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry)
  and volume = ref 0. in
  for triangle = 0 to Geometry.primitive_count geometry - 1 do
    let offset = topology.primitive_offsets.(triangle) in
    let a = topology.vertex_points.(offset)
    and b = topology.vertex_points.(offset + 1)
    and c = topology.vertex_points.(offset + 2) in
    volume := !volume +.
      (positions.x.(a) *. ((positions.y.(b) *. positions.z.(c))
                           -. (positions.z.(b) *. positions.y.(c)))
       +. positions.y.(a) *. ((positions.z.(b) *. positions.x.(c))
                              -. (positions.x.(b) *. positions.z.(c)))
       +. positions.z.(a) *. ((positions.x.(b) *. positions.y.(c))
                              -. (positions.y.(b) *. positions.x.(c)))) /. 6.
  done;
  !volume

let surface_area geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry)
  and area = ref 0. in
  for triangle = 0 to Geometry.primitive_count geometry - 1 do
    let offset = topology.primitive_offsets.(triangle) in
    let a = topology.vertex_points.(offset)
    and b = topology.vertex_points.(offset + 1)
    and c = topology.vertex_points.(offset + 2) in
    let abx = positions.x.(b) -. positions.x.(a)
    and aby = positions.y.(b) -. positions.y.(a)
    and abz = positions.z.(b) -. positions.z.(a)
    and acx = positions.x.(c) -. positions.x.(a)
    and acy = positions.y.(c) -. positions.y.(a)
    and acz = positions.z.(c) -. positions.z.(a) in
    let nx = (aby *. acz) -. (abz *. acy)
    and ny = (abz *. acx) -. (abx *. acz)
    and nz = (abx *. acy) -. (aby *. acx) in
    area := !area +. (sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) *. 0.5)
  done;
  !area

let test_degenerate_rejection () =
  let degenerate = geometry
      [|0.,0.,0.; 0.5,0.,0.; 1.,0.,0.|] [|0;1;2|]
  and solid = tetra ~origin:(0.,0.,0.) 1. in
  match Solid.prepare ~grain:1 ~left:degenerate ~right:solid () with
  | Error error when Error.code error = "degenerate_triangle" -> ()
  | Error error when Error.code error = "invalid_surface" -> ()
  | Error error -> failf "unexpected degenerate error: %s" (Error.to_string error)
  | Ok _ -> failf "degenerate source triangle entered the solid Boolean pipeline"

let test_cancellation () =
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Solid.prepare ~cancel ~grain:1
      ~left:(tetra ~origin:(0.,0.,0.) 1.)
      ~right:(tetra ~origin:(0.2,0.2,0.2) 1.) () with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> failf "unexpected cancellation error: %s" (Error.to_string error)
  | Ok _ -> failf "cancelled Boolean preparation completed"

let tiny_edge_signature group =
  Array.init (Edge_group.length group) (fun edge -> Edge_group.mem edge group)

let run () =
  test_degenerate_rejection ();
  test_cancellation ()
