open Rays
open Rdk
open Rdk_test_support

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> get_string

let geometry_owned ?(attributes = []) ?(groups = []) ?(edge_groups = [])
    ~kinds points vertex_points primitive_offsets =
  let points = Array.of_list points and point_count = List.length points in
  let x = Array.map (fun (x, _, _) -> x) points
  and y = Array.map (fun (_, y, _) -> y) points
  and z = Array.map (fun (_, _, z) -> z) points in
  let topology = Topology.create_owned ~point_count ~vertex_points
      ~primitive_offsets ~primitive_kinds:kinds |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology ~attributes ~groups ~edge_groups () |> get_string

let edge_group_of_pairs topology name pairs =
  let index = Topology_index.create topology in
  let selected = Array.map (fun (a, b) ->
      let edge = Topology_index.find_edge_index index ~a ~b in
      if edge < 0 then fail "test edge is absent";
      edge) pairs in
  Edge_group.init ~grain:1 ~topology ~index ~name (fun edge ->
    Array.mem edge selected)

let closed_loops count vertices_per_loop =
  let point_count = count * vertices_per_loop in
  let x = Array.make point_count 0. and y = Array.make point_count 0.
  and z = Array.make point_count 0. in
  for component = 0 to count - 1 do
    let cx = float_of_int (component mod 250) *. 4.5
    and cy = float_of_int (component / 250) *. 4.5 in
    for local = 0 to vertices_per_loop - 1 do
      let point = (component * vertices_per_loop) + local in
      let angle = 2. *. Float.pi *. float_of_int local
          /. float_of_int vertices_per_loop in
      let radius = 1. +. (0.18 *. sin (3. *. angle)) in
      x.(point) <- cx +. (radius *. cos angle);
      y.(point) <- cy +. (radius *. sin angle);
      z.(point) <- 0.08 *. cos (2. *. angle)
    done
  done;
  let topology = Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (count + 1)
        (fun primitive -> primitive * vertices_per_loop))
      ~primitive_kinds:(Array.make count Topology.Closed_polyline)
      |> get_string in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology () |> get_string

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && Attribute.name left = Attribute.name right
  && Attribute.storage left = Attribute.storage right

let equal_group left right =
  Group.owner left = Group.owner right && Group.name left = Group.name right
  && Group.length left = Group.length right
  && Group.ordered_elements left = Group.ordered_elements right
  && Array.for_all Fun.id (Array.init (Group.length left) (fun element ->
    Group.mem element left = Group.mem element right))

let equal_edge_group left right =
  Edge_group.name left = Edge_group.name right
  && Edge_group.length left = Edge_group.length right
  && Array.for_all Fun.id (Array.init (Edge_group.length left) (fun edge ->
    Edge_group.mem edge left = Edge_group.mem edge right))

let equal_geometry left right =
  let lp = positions left and rp = positions right
  and lt = Topology.View.view (Geometry.topology left)
  and rt = Topology.View.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && lt.primitive_kinds = rt.primitive_kinds
  && List.equal equal_attribute (Geometry.attributes left)
       (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group (Geometry.edge_groups left)
       (Geometry.edge_groups right)

let expect_code code work message = match work () with
  | Error error when Error.code error = code -> ()
  | Error error -> fail (Printf.sprintf "%s: expected %s, received %s"
      message code (Error.to_string error))
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let circumcenter_xy geometry a b c =
  let p = positions geometry in
  let ax = p.x.(a) and ay = p.y.(a)
  and bx = p.x.(b) and by = p.y.(b)
  and cx = p.x.(c) and cy = p.y.(c) in
  let d = 2. *. ((ax *. (by -. cy)) +. (bx *. (cy -. ay))
      +. (cx *. (ay -. by))) in
  if abs_float d < 1e-15 then fail "test circumcenter is degenerate";
  let aa = (ax *. ax) +. (ay *. ay)
  and bb = (bx *. bx) +. (by *. by)
  and cc = (cx *. cx) +. (cy *. cy) in
  ((aa *. (by -. cy)) +. (bb *. (cy -. ay)) +. (cc *. (ay -. by))) /. d,
  ((aa *. (cx -. bx)) +. (bb *. (ax -. cx)) +. (cc *. (bx -. ax))) /. d

let run () =
  print_endline "circle from edges tests passed"
