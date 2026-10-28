open Rays
open Rdk
open Rdk_test_support

let get = function Ok value -> value | Error error -> fail (Error.to_string error)

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (name ^ " has non-integer storage"))
  | None -> fail ("missing " ^ name)

let detail_int_array name geometry =
  match Geometry.find_attribute ~owner:Attribute.Detail name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int_array values -> Packed.Int_array.get values 0
       | _ -> fail (name ^ " has non-integer-array storage"))
  | None -> fail ("missing " ^ name)

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let quad_grid () =
  Plane_generators.grid ~connectivity:Plane_generators.Grid_quads ~columns:2 ~rows:2 ~size:2. () |> get

let verify_points_by_primitive geometry colors =
  let topology = Geometry.topology geometry in
  for primitive = 0 to Topology.primitive_count topology - 1 do
    let first, last = Topology.primitive_vertex_range topology primitive in
    for left = first to last - 1 do
      for right = left + 1 to last - 1 do
        let a = Topology.point_of_vertex topology left
        and b = Topology.point_of_vertex topology right in
        if colors.(a) >= 0 && colors.(b) >= 0 then
          check (colors.(a) <> colors.(b))
            "Graph Color gave points in one primitive the same color"
      done
    done
  done

let verify_primitives_by_point geometry colors =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  for point = 0 to Topology.point_count topology - 1 do
    let incidence = Topology_index.point_incidence_count index point in
    for left = 0 to incidence - 1 do
      let lv = Topology_index.point_vertex index ~point ~local:left in
      let a = Topology_index.primitive_of_vertex index lv in
      for right = left + 1 to incidence - 1 do
        let rv = Topology_index.point_vertex index ~point ~local:right in
        let b = Topology_index.primitive_of_vertex index rv in
        if a <> b && colors.(a) >= 0 && colors.(b) >= 0 then
          check (colors.(a) <> colors.(b))
            "Graph Color gave point-adjacent primitives the same color"
      done
    done
  done

let verify_primitives_by_edge geometry colors =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  for edge = 0 to Topology_index.edge_count index - 1 do
    let incidence = Topology_index.edge_incidence_count index edge in
    for left = 0 to incidence - 1 do
      let lv = Topology_index.edge_vertex index ~edge ~local:left in
      let a = Topology_index.primitive_of_vertex index lv in
      for right = left + 1 to incidence - 1 do
        let rv = Topology_index.edge_vertex index ~edge ~local:right in
        let b = Topology_index.primitive_of_vertex index rv in
        if a <> b && colors.(a) >= 0 && colors.(b) >= 0 then
          check (colors.(a) <> colors.(b))
            "Graph Color gave edge-adjacent primitives the same color"
      done
    done
  done

let test_connectivities () =
  let source = quad_grid () in
  let by_point = Graph_color.run ~grain:1
      ~connectivity:Graph_color.Graph_primitives_by_point source |> get in
  let point_colors = int_attribute Attribute.Primitive "color" by_point in
  verify_primitives_by_point by_point point_colors;
  check (Array.to_list point_colors = [0;1;2;3])
    "shared-center quads did not receive stable greedy colors";

  let by_edge = Graph_color.run ~grain:1
      ~connectivity:Graph_color.Graph_primitives_by_edge source |> get in
  let edge_colors = int_attribute Attribute.Primitive "color" by_edge in
  verify_primitives_by_edge by_edge edge_colors;
  check (Array.to_list edge_colors = [0;1;1;0])
    "edge graph did not reuse checkerboard colors";

  let points = Graph_color.run ~grain:1 ~color_attribute:"point_color"
      ~connectivity:Graph_color.Graph_points_by_primitive source |> get in
  verify_points_by_primitive points
    (int_attribute Attribute.Point "point_color" points)

let test_selection_promotion () =
  let source = quad_grid () in
  let primitives = Group.init ~grain:1 ~owner:Group.Primitive ~name:"diagonal" 4
      (fun primitive -> primitive = 0 || primitive = 3) in
  let edge_graph = Graph_color.run ~grain:1
      ~selection:(Transform_ops.Selected_primitives primitives)
      ~connectivity:Graph_color.Graph_primitives_by_edge source |> get in
  check (Array.to_list (int_attribute Attribute.Primitive "color" edge_graph)
      = [0;-1;-1;0])
    "Graph Color selected induced edge graph";
  let point_graph = Graph_color.run ~grain:1
      ~selection:(Transform_ops.Selected_primitives primitives)
      ~connectivity:Graph_color.Graph_primitives_by_point source |> get in
  check (Array.to_list (int_attribute Attribute.Primitive "color" point_graph)
      = [0;-1;-1;1])
    "Graph Color selected induced point graph";

  let center = Group.init ~grain:1 ~owner:Group.Point ~name:"center" 9
      (fun point -> point = 4) in
  let promoted = Graph_color.run ~grain:1
      ~selection:(Transform_ops.Selected_points center)
      ~connectivity:Graph_color.Graph_primitives_by_point source |> get in
  check (Array.to_list (int_attribute Attribute.Primitive "color" promoted)
      = [0;1;2;3])
    "Graph Color did not promote a point selection to incident primitives"

let test_sort_and_worksets () =
  let source = quad_grid ()
      |> with_attribute Attribute.Primitive "id"
           (Attribute.Int [|0;1;2;3|]) in
  let selected = Group.init ~grain:1 ~owner:Group.Primitive ~name:"middle" 4
      (fun primitive -> primitive = 1 || primitive = 2) in
  let output = Graph_color.run ~grain:1
      ~selection:(Transform_ops.Selected_primitives selected)
      ~connectivity:Graph_color.Graph_primitives_by_point ~sort_output:true
      ~worksets:{Graph_color.begin_attribute="work_begin";length_attribute="work_length"}
      source |> get in
  check (Array.to_list (int_attribute Attribute.Primitive "id" output)
      = [0;3;1;2])
    "Graph Color did not stably sort unselected/color blocks";
  check (Array.to_list (int_attribute Attribute.Primitive "color" output)
      = [-1;-1;0;1])
    "Graph Color did not remap its color attribute through Sort";
  check (Array.to_list (detail_int_array "work_begin" output) = [2;3]
      && Array.to_list (detail_int_array "work_length" output) = [1;1])
    "Graph Color worksets do not describe sorted color blocks";
  verify_primitives_by_point output
    (int_attribute Attribute.Primitive "color" output)

let expect_code code operation message = match operation () with
  | Error error when Error.code error = code -> ()
  | Error error -> fail (message ^ ": " ^ Error.to_string error)
  | Ok _ -> fail (message ^ ": unexpectedly accepted")

let test_validation () =
  let source = quad_grid () in
  expect_code "invalid_graph" (fun () -> Graph_color.run ~grain:0 source)
    "zero grain";
  expect_code "invalid_graph" (fun () -> Graph_color.run ~color_attribute:"P" source)
    "reserved color attribute";
  expect_code "invalid_graph" (fun () -> Graph_color.run
      ~worksets:{Graph_color.begin_attribute="begin";length_attribute="length"} source)
    "worksets without sorting";
  expect_code "invalid_graph" (fun () -> Graph_color.run ~sort_output:true
      ~worksets:{Graph_color.begin_attribute="same";length_attribute="same"} source)
    "duplicate workset names";
  let wrong = source |> with_attribute Attribute.Primitive "color"
      (Attribute.Float (Array.make 4 0.)) in
  expect_code "invalid_graph" (fun () -> Graph_color.run wrong)
    "wrong output storage";
  let foreign = Group.init ~grain:1 ~owner:Group.Primitive ~name:"foreign" 1
      (fun _ -> true) in
  expect_code "invalid_graph" (fun () -> Graph_color.run
      ~selection:(Transform_ops.Selected_primitives foreign) source)
    "foreign selection cardinality";
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect_code "cancelled" (fun () -> Graph_color.run ~cancel:cancelled source)
    "cancellation";
  let empty = Line_geometry.points [||] in
  let empty = Graph_color.run ~connectivity:Graph_color.Graph_points_by_primitive
      ~sort_output:true
      ~worksets:{Graph_color.begin_attribute="begin";length_attribute="length"}
      empty |> get in
  check (int_attribute Attribute.Point "color" empty = [||]
      && detail_int_array "begin" empty = [||]
      && detail_int_array "length" empty = [||])
    "empty Graph Color contract"

let disconnected_triangles count =
  let point_count = count * 3 in
  let x = Array.init point_count (fun point -> float_of_int (point / 3))
  and y = Array.init point_count (fun point ->
      match point mod 3 with 0 -> 0. | 1 -> 1. | _ -> 0.)
  and z = Array.make point_count 0. in
  let topology = Topology.create_owned ~point_count
      ~vertex_points:(Array.init point_count Fun.id)
      ~primitive_offsets:(Array.init (count + 1) (fun primitive -> primitive * 3))
      ~primitive_kinds:(Array.make count Topology.Polygon) |> get_string in
  Geometry.create
    ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z) ~topology ()
  |> get_string

let test_parallel_exact () =
  let source = disconnected_triangles 40_000 in
  let run domains = Parallel.run ~domains (fun () ->
      Graph_color.run ~grain:257
        ~connectivity:Graph_color.Graph_points_by_primitive source |> get) in
  let one = run 1 and four = run 4 in
  let one_colors = int_attribute Attribute.Point "color" one
  and four_colors = int_attribute Attribute.Point "color" four in
  check (one_colors = four_colors)
    "Graph Color differs across one and four domains";
  check (Array.length one_colors = 120_000
      && Array.for_all Fun.id (Array.init 120_000 (fun point ->
           one_colors.(point) = point mod 3)))
    "Graph Color scale cardinality/order";
  verify_points_by_primitive one one_colors;
  let run_sorted domains = Parallel.run ~domains (fun () ->
      Graph_color.run ~grain:257 ~connectivity:Graph_color.Graph_points_by_primitive
        ~sort_output:true
        ~worksets:{Graph_color.begin_attribute="begin";length_attribute="length"}
        source |> get) in
  let sorted_one = run_sorted 1 and sorted_four = run_sorted 4 in
  let signature geometry =
    let p = Packed.Float3.View.view (Geometry.positions geometry)
    and t = Topology.View.view (Geometry.topology geometry) in
    Array.copy p.x, Array.copy p.y, Array.copy p.z,
    Array.copy t.vertex_points, Array.copy t.primitive_offsets,
    Bytes.copy t.primitive_kinds,
    Array.copy (int_attribute Attribute.Point "color" geometry),
    detail_int_array "begin" geometry, detail_int_array "length" geometry in
  check (signature sorted_one = signature sorted_four)
    "sorted Graph Color/worksets differ across one and four domains";
  check (detail_int_array "begin" sorted_one = [|0;40_000;80_000|]
      && detail_int_array "length" sorted_one = [|40_000;40_000;40_000|])
    "sorted Graph Color scale worksets"

let run () =
  test_connectivities ();
  test_selection_promotion ();
  test_sort_and_worksets ();
  test_validation ();
  test_parallel_exact ();
  print_endline "graph color tests passed"
