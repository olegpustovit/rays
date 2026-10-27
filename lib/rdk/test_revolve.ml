open Rays
open Rdk
open Rdk_test_support

let topology geometry = Topology.Private.view (Geometry.topology geometry)

let attribute_storage geometry owner name =
  match Geometry.find_attribute ~owner name geometry with
  | None -> fail ("missing attribute " ^ name)
  | Some attribute -> Attribute.Private.storage attribute

let equal_geometry left right =
  let lp = positions left and rp = positions right
  and lt = topology left and rt = topology right in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal (fun left right ->
       Attribute.owner left = Attribute.owner right
       && String.equal (Attribute.name left) (Attribute.name right)
       && equal_storage left right)
       (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group
       (Geometry.edge_groups left) (Geometry.edge_groups right)

let with_attribute owner name storage geometry =
  let attribute = Attribute.create_owned ~owner ~name storage |> get_string in
  Geometry.with_attribute attribute geometry |> get_string

let with_group group geometry = Geometry.with_group group geometry |> get_string

let base_profile () =
  Line_geometry.polyline [|(1., -1., 0.); (1., 1., 0.)|] |> get_ok

let revolve ?(divisions = 4) ?revolve_type ?connectivity ?start_angle
    ?end_angle ?reverse_cross_sections ?caps ?cap_group ?uv_attribute geometry =
  Sweep_modeling.revolve ~grain:1 ?revolve_type ?connectivity ?start_angle ?end_angle
    ?reverse_cross_sections ?caps ?cap_group ?uv_attribute ~divisions
    ~origin:Rays_math.Vec3.zero ~axis:Rays_math.Vec3.unit_y geometry |> get_ok

let check_full_surface () =
  let result = revolve (base_profile ()) in
  let point = positions result and topo = topology result in
  check (Geometry.point_count result = 8 && Geometry.vertex_count result = 16
      && Geometry.primitive_count result = 4)
    "Revolve quad cardinality";
  check (topo.vertex_points =
      [|0;1;5;4; 1;2;6;5; 2;3;7;6; 3;0;4;7|])
    "Revolve quad topology/order";
  check (near point.x.(0) 1. && near point.z.(0) 0.
      && near point.x.(1) 0. && near point.z.(1) (-1.)
      && near point.x.(2) (-1.) && near point.z.(2) 0.)
    "Revolve right-handed axis rotation";
  match attribute_storage result Attribute.Vertex "uv" with
  | Attribute.Float2 uv ->
      let uv = Packed.Float2.Private.view uv in
      check (near uv.x.(0) 0. && near uv.y.(0) 0.
          && near uv.x.(2) 1. && near uv.y.(2) 0.25)
        "Revolve seam-safe UV coordinates"
  | _ -> fail "Revolve UV storage"

let check_connectivity_and_arcs () =
  let source = base_profile () in
  let make connectivity = revolve ~connectivity source in
  let points = make Plane_generators.Grid_points
  and rows = make Plane_generators.Grid_rows
  and columns = make Plane_generators.Grid_columns
  and both = make Plane_generators.Grid_rows_and_columns
  and triangles = make Plane_generators.Grid_triangles
  and reverse = make Plane_generators.Grid_reverse_triangles
  and alternating = make Plane_generators.Grid_alternating_triangles in
  check (Geometry.point_count points = 8 && Geometry.vertex_count points = 0)
    "Revolve point connectivity";
  check (Geometry.primitive_count rows = 2 && Geometry.vertex_count rows = 8
      && Topology.primitive_kind (Geometry.topology rows) 0
          = Topology.Closed_polyline)
    "Revolve row connectivity";
  check (Geometry.primitive_count columns = 4
      && Geometry.vertex_count columns = 8)
    "Revolve column connectivity";
  check (Geometry.primitive_count both = 6 && Geometry.vertex_count both = 16)
    "Revolve combined curve connectivity";
  List.iter (fun geometry -> check (Geometry.primitive_count geometry = 8
      && Geometry.vertex_count geometry = 24) "Revolve triangle cardinality")
    [triangles; reverse; alternating];
  check ((topology triangles).vertex_points <> (topology reverse).vertex_points
      && (topology triangles).vertex_points <>
         (topology alternating).vertex_points)
    "Revolve triangle split variants collapsed";
  let arc = revolve ~divisions:2 ~revolve_type:Sweep_modeling.Revolve_open_arc
      ~start_angle:0. ~end_angle:Float.pi source in
  check (Geometry.point_count arc = 6 && Geometry.primitive_count arc = 2
      && Geometry.vertex_count arc = 8)
    "open-arc Revolve cardinality";
  let point = positions arc in
  check (near point.x.(2) (-1.) && near point.z.(2) 0.)
    "open-arc Revolve endpoint"

let check_poles_caps_and_reverse () =
  let pole_profile = Line_geometry.polyline
      [|(0., -1., 0.); (1., 0., 0.); (0., 1., 0.)|] |> get_ok in
  let pole = revolve ~caps:true ~cap_group:"caps" pole_profile in
  check (Geometry.point_count pole = 6 && Geometry.primitive_count pole = 8
      && Geometry.vertex_count pole = 24)
    "Revolve compact pole/cardinality";
  for primitive = 0 to Geometry.primitive_count pole - 1 do
    check (Topology.primitive_size (Geometry.topology pole) primitive = 3)
      "Revolve pole emitted a non-triangle cell"
  done;
  let caps = match Geometry.find_group ~owner:Group.Primitive "caps" pole with
    | Some group -> group | None -> fail "missing Revolve cap group" in
  check (Group.cardinality caps = 0) "axis endpoints emitted degenerate caps";
  let capped = revolve ~caps:true ~cap_group:"caps" (base_profile ()) in
  check (Geometry.primitive_count capped = 6
      && Geometry.vertex_count capped = 24)
    "Revolve cap cardinality";
  let caps = match Geometry.find_group ~owner:Group.Primitive "caps" capped with
    | Some group -> group | None -> fail "missing Revolve cap group" in
  check (Group.cardinality caps = 2 && Group.mem 4 caps && Group.mem 5 caps)
    "Revolve cap membership";
  let reversed = revolve ~reverse_cross_sections:true (base_profile ()) in
  check (Array.sub (topology reversed).vertex_points 0 4 = [|4;5;1;0|])
    "Revolve reverse-cross-section topology"

let check_payload () =
  let source = base_profile ()
      |> with_attribute Attribute.Point "id" (Attribute.Int [|10;20|])
      |> with_attribute Attribute.Vertex "vtag" (Attribute.Int [|100;200|])
      |> with_attribute Attribute.Primitive "piece"
           (Attribute.Text [|"body"|])
      |> with_attribute Attribute.Detail "answer" (Attribute.Int [|42|])
      |> with_attribute Attribute.Point "N"
           (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
             ~x:[|1.;1.|] ~y:[|0.;0.|] ~z:[|0.;0.|]))
      |> with_group (Group.init ~owner:Group.Point ~name:"first" 2
           (fun point -> point = 0))
      |> with_group (Group.init ~owner:Group.Vertex ~name:"first_corner" 2
           (fun vertex -> vertex = 0))
      |> with_group (Group.init ~owner:Group.Primitive ~name:"profile" 1
           (fun _ -> true))
      |> fun geometry -> Group_mesh.group_edges ~grain:1 ~name:"profile_edges" geometry
           |> get_ok in
  let result = revolve ~caps:true ~cap_group:"caps" source in
  (match attribute_storage result Attribute.Point "id" with
   | Attribute.Int values ->
       check (values = [|10;10;10;10;20;20;20;20|])
         "Revolve point attribute ancestry"
   | _ -> fail "Revolve point attribute storage");
  (match attribute_storage result Attribute.Vertex "vtag" with
   | Attribute.Int values ->
       check (Array.sub values 0 4 = [|100;100;200;200|])
         "Revolve vertex attribute ancestry"
   | _ -> fail "Revolve vertex attribute storage");
  (match attribute_storage result Attribute.Primitive "piece" with
   | Attribute.Text values ->
       check (values = Array.make 6 "body") "Revolve primitive ancestry"
   | _ -> fail "Revolve primitive attribute storage");
  check (Geometry.find_attribute ~owner:Attribute.Point "N" result = None)
    "Revolve retained stale point normals";
  let first = match Geometry.find_group ~owner:Group.Point "first" result with
    | Some group -> group | None -> fail "missing remapped point group" in
  check (Group.cardinality first = 4) "Revolve point-group ancestry";
  let edge_group = match Geometry.find_edge_group "profile_edges" result with
    | Some group -> group | None -> fail "missing remapped edge group" in
  check (Edge_group.cardinality edge_group = 4)
    "Revolve native-edge ancestry"

let check_selection_validation_and_parallel () =
  let source = Mesh_merge.run [base_profile ();
      Line_geometry.polyline [|(2., -1., 0.); (2., 1., 0.)|] |> get_ok] |> get_ok in
  let selected = Group.init ~owner:Group.Primitive ~name:"selected" 2
      (fun primitive -> primitive = 1) in
  let result = Sweep_modeling.revolve ~grain:1 ~primitives:selected ~divisions:4
      ~origin:Rays_math.Vec3.zero ~axis:Rays_math.Vec3.unit_y source |> get_ok in
  check (Geometry.point_count result = 8 && Geometry.primitive_count result = 4
      && near (positions result).x.(0) 2.)
    "Revolve primitive restriction";
  let count = 10_001 in
  let dense = Array.init count (fun point ->
      let y = (float_of_int point /. float_of_int (count - 1)) *. 8. -. 4. in
      1.2 +. (0.2 *. sin (y *. 3.)), y, 0.) |> Line_geometry.polyline |> get_ok
      |> fun geometry -> Group_mesh.group_edges ~grain:257 ~name:"profile_edges" geometry
           |> get_ok in
  let run domains = Parallel.run ~domains (fun () ->
      Sweep_modeling.revolve ~grain:257 ~connectivity:Plane_generators.Grid_alternating_triangles
        ~caps:true ~cap_group:"caps" ~divisions:64 ~origin:Rays_math.Vec3.zero
        ~axis:Rays_math.Vec3.unit_y dense |> get_ok) in
  let one = run 1 and many = run 4 in
  check (equal_geometry one many)
    "one-domain and four-domain Revolve geometry differ";
  check (Geometry.point_count one = count * 64
      && Geometry.primitive_count one = ((count - 1) * 64 * 2) + 2)
    "dense Revolve cardinality";
  let expect_invalid = function
    | Error error when Error.code error = "invalid_geometry" -> ()
    | Error error -> fail ("unexpected Revolve error " ^ Error.to_string error)
    | Ok _ -> fail "expected invalid Revolve input" in
  expect_invalid (Sweep_modeling.revolve ~divisions:2 ~origin:Rays_math.Vec3.zero
    ~axis:Rays_math.Vec3.unit_y (base_profile ()));
  expect_invalid (Sweep_modeling.revolve ~divisions:4 ~origin:Rays_math.Vec3.zero
    ~axis:Rays_math.Vec3.zero (base_profile ()));
  expect_invalid (Sweep_modeling.revolve ~revolve_type:Sweep_modeling.Revolve_open_arc
    ~start_angle:1. ~end_angle:1. ~divisions:4 ~origin:Rays_math.Vec3.zero
    ~axis:Rays_math.Vec3.unit_y (base_profile ()));
  expect_invalid (Sweep_modeling.revolve ~revolve_type:Sweep_modeling.Revolve_open_arc
    ~divisions:max_int ~origin:Rays_math.Vec3.zero ~axis:Rays_math.Vec3.unit_y (base_profile ()));
  expect_invalid (Sweep_modeling.revolve ~revolve_type:Sweep_modeling.Revolve_open_arc ~caps:true
    ~divisions:4 ~origin:Rays_math.Vec3.zero ~axis:Rays_math.Vec3.unit_y (base_profile ()));
  let polygon = Plane_generators.grid ~columns:1 ~rows:1 ~size:1. () |> get_ok in
  expect_invalid (Sweep_modeling.revolve ~divisions:4 ~origin:Rays_math.Vec3.zero
    ~axis:Rays_math.Vec3.unit_y polygon);
  let repeated = Line_geometry.polyline [|(1.,0.,0.); (1.,0.,0.)|] |> get_ok in
  expect_invalid (Sweep_modeling.revolve ~divisions:4 ~origin:Rays_math.Vec3.zero
    ~axis:Rays_math.Vec3.unit_y repeated);
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Sweep_modeling.revolve ~cancel:cancelled ~divisions:4 ~origin:Rays_math.Vec3.zero
      ~axis:Rays_math.Vec3.unit_y (base_profile ()) with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "Revolve ignored cancellation")

let run () =
  check_full_surface ();
  check_connectivity_and_arcs ();
  check_poles_caps_and_reverse ();
  check_payload ();
  check_selection_validation_and_parallel ();
  print_endline "Revolve tests passed"
