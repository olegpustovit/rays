open Rays
open Rdk
open Rdk_test_support

let contains text needle =
  let text_length = String.length text and needle_length = String.length needle in
  let rec search offset =
    offset + needle_length <= text_length
    && (String.sub text offset needle_length = needle || search (offset + 1)) in
  needle_length = 0 || search 0

let equal_topology left right =
  let left = Topology.View.view (Geometry.topology left)
  and right = Topology.View.view (Geometry.topology right) in
  left.point_count = right.point_count
  && left.vertex_points = right.vertex_points
  && left.primitive_offsets = right.primitive_offsets
  && Bytes.equal left.primitive_kinds right.primitive_kinds

let attribute owner name storage =
  Attribute.create_owned ~owner ~name storage |> get_string

let with_detail name storage geometry =
  Geometry.with_attribute (attribute Attribute.Detail name storage) geometry
  |> get_string

let without_details names geometry =
  List.fold_left (fun geometry name ->
    Geometry.without_attribute ~owner:Attribute.Detail name geometry)
    geometry names

let group owner name geometry =
  match Geometry.find_group ~owner name geometry with
  | Some group -> group
  | None -> fail ("missing group " ^ name)

let int_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail ("wrong integer storage for " ^ name))
  | None -> fail ("missing integer attribute " ^ name)

let float_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.storage attribute with
       | Attribute.Float values -> values
       | _ -> fail ("wrong float storage for " ^ name))
  | None -> fail ("missing float attribute " ^ name)

let float3_values owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.storage attribute with
       | Attribute.Float3 values -> Packed.Float3.View.view values
       | _ -> fail ("wrong float3 storage for " ^ name))
  | None -> fail ("missing float3 attribute " ^ name)

let quad ?vertex_weights ?primitive_weights ?corner_weights () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 2.; 2.; 0.|] ~y:[|0.; 0.; 2.; 2.|] ~z:[|0.; 0.; 0.; 0.|] in
  let topology = Topology.polygons_owned ~point_count:4
      ~vertex_points:[|0;1;2;3|] ~primitive_offsets:[|0;4|] |> get_string in
  let attributes = List.filter_map Fun.id [
    Option.map (fun values -> attribute Attribute.Vertex "creaseweight"
      (Attribute.Float values)) vertex_weights;
    Option.map (fun values -> attribute Attribute.Primitive "creaseweight"
      (Attribute.Float values)) primitive_weights;
    Option.map (fun values -> attribute Attribute.Point "cornerweight"
      (Attribute.Float values)) corner_weights;
  ] in
  Geometry.create ~positions ~topology ~attributes () |> get_string

let crease_paths ?(moved = false) ?vertex_weights ?primitive_weights () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:(if moved then [|91.; -7.; 42.; 13.|] else [|0.; 2.; 2.; 0.|])
      ~y:(if moved then [|-4.; 81.; 17.; -23.|] else [|0.; 0.; 2.; 2.|])
      ~z:(if moved then [|8.; 9.; 10.; 11.|] else [|0.; 0.; 0.; 0.|]) in
  let topology = Topology.create_owned ~point_count:4
      ~vertex_points:[|0;1; 1;2|] ~primitive_offsets:[|0;2;4|]
      ~primitive_kinds:[|Topology.Open_polyline; Topology.Open_polyline|]
      |> get_string in
  let selected = Group.ordered ~owner:Group.Primitive ~name:"crease_pick"
      ~length:2 [|1|] |> get_string in
  let attributes = List.filter_map Fun.id [
    Option.map (fun values -> attribute Attribute.Vertex "creaseweight"
      (Attribute.Float values)) vertex_weights;
    Option.map (fun values -> attribute Attribute.Primitive "creaseweight"
      (Attribute.Float values)) primitive_weights;
  ] in
  Geometry.create ~positions ~topology ~attributes ~groups:[selected] ()
  |> get_string

let exact_crease_input ?(moved = false) ?vertex_weights ?primitive_weights () =
  let geometry = quad ?vertex_weights ?primitive_weights () in
  if not moved then geometry
  else
    Geometry.with_positions
      (Packed.Float3.View.of_owned_exn
        ~x:[|100.; -20.; 70.; 11.|] ~y:[|2.; 88.; -9.; 31.|]
        ~z:[|14.; 15.; 16.; 17.|]) geometry |> get_string

let edge_weight geometry a b =
  match Geometry.find_attribute ~owner:Attribute.Vertex "creaseweight" geometry with
  | None -> 0.
  | Some attribute ->
      let values = match Attribute.storage attribute with
        | Attribute.Float values -> values
        | _ -> fail "creaseweight storage changed" in
      let index = Topology_index.create (Geometry.topology geometry) in
      let view = Topology_index.View.view index in
      let edge = Topology_index.find_edge index ~a ~b |> Option.get in
      let result = ref 0. in
      for at = view.edge_offsets.(edge) to view.edge_offsets.(edge + 1) - 1 do
        result := max !result values.(view.edge_vertices.(at))
      done;
      !result

let child_edges geometry ~source_point_count ~source_edge a b =
  let midpoint = source_point_count + source_edge in
  let index = Topology_index.create (Geometry.topology geometry) in
  Topology_index.find_edge index ~a ~b:midpoint |> Option.get,
  Topology_index.find_edge index ~a:midpoint ~b |> Option.get

let test_selected_loop_validation_scope () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;0.; 3.;4.;4.;3.|]
      ~y:[|0.;0.;1.; 0.;0.;1.;1.|]
      ~z:(Array.make 7 0.) in
  let topology = Topology.polygons_owned ~point_count:7
      ~vertex_points:[|0;1;2; 3;4;5;6|]
      ~primitive_offsets:[|0;3;7|] |> get_string in
  let geometry = Geometry.create ~positions ~topology () |> get_string in
  let triangle = Group.init ~owner:Group.Primitive ~name:"triangle" 2
      (fun primitive -> primitive = 0) in
  let output = Subdivide_front.subdivide ~scheme:Subdivide.Loop ~primitives:triangle geometry
      |> get_rdk in
  check (Geometry.primitive_count output = 5
      && Topology.primitive_size (Geometry.topology output) 0 = 4)
    "local Loop validated or changed an unselected quad"

let test_disconnected_selected_fans () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;2.; 0.;1.;2.; 0.;1.;2.|]
      ~y:[|0.;0.;0.; 1.;1.;1.; 2.;2.;2.|]
      ~z:(Array.make 9 0.) in
  let topology = Topology.polygons_owned ~point_count:9
      ~vertex_points:[|0;3;4;1; 1;4;5;2; 3;6;7;4; 4;7;8;5|]
      ~primitive_offsets:[|0;4;8;12;16|] |> get_string in
  let point_id = attribute Attribute.Point "point_id"
      (Attribute.Int (Array.init 9 (fun point -> 100 + point))) in
  let geometry = Geometry.create ~positions ~topology ~attributes:[point_id] ()
      |> get_string in
  let diagonal = Group.ordered ~owner:Group.Primitive ~name:"diagonal"
      ~length:4 [|0;3|] |> get_string in
  let run domains = Parallel.run ~domains (fun () ->
      Subdivide_front.subdivide ~grain:1 ~primitives:diagonal geometry |> get_rdk) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four)
    "disconnected selected-fan splitting differs across domains";
  check (Geometry.point_count one = 25 && Geometry.vertex_count one = 40
      && Geometry.primitive_count one = 10)
    "disconnected selected-fan splitting cardinality";
  let ids = int_values Attribute.Point "point_id" one in
  let center_copies = Array.fold_left
      (fun count value -> if value = 104 then count + 1 else count) 0
      (Array.sub ids 0 15) in
  check (center_copies = 3)
    "disconnected selected-fan splitting lost point ancestry"

let test_chaikin_creasing () =
  let weights = [|(1, 4.); (3, 2.); (5, 8.); (7, 0.5)|] in
  let make ?(connectivity = Plane_generators.Grid_quads) () =
    let geometry = Plane_generators.grid ~connectivity ~columns:2 ~rows:2 ~size:2. ()
        |> get_rdk in
    let index = Topology_index.create (Geometry.topology geometry)
        |> Topology_index.View.view in
    let creaseweight = Array.init (Geometry.vertex_count geometry) (fun vertex ->
      let edge = index.edge_of_vertex.(vertex) in
      if edge < 0 then 0.
      else
        let a = index.edge_a.(edge) and b = index.edge_b.(edge) in
        let neighbor = if a = 4 then Some b else if b = 4 then Some a else None in
        match neighbor with
        | None -> 0.
        | Some neighbor ->
            (match Array.find_opt (fun (point, _) -> point = neighbor) weights with
             | None -> 0. | Some (_, weight) -> weight)) in
    Geometry.with_attribute
      (attribute Attribute.Vertex "creaseweight" (Attribute.Float creaseweight))
      geometry |> get_string in
  let input = make () in
  let source_index = Topology_index.create (Geometry.topology input) in
  let edge neighbor = Topology_index.find_edge source_index ~a:4 ~b:neighbor
      |> Option.get in
  let uniform = Subdivide_front.subdivide ~grain:1
      ~creasing_method:Subdivide.Subdivide_creasing_uniform
      ~resulting_crease_group:"remaining" input |> get_rdk
  and chaikin = Subdivide_front.subdivide ~grain:1
      ~creasing_method:Subdivide.Subdivide_creasing_chaikin
      ~resulting_crease_group:"remaining" input |> get_rdk in
  check (equal_geometry uniform
      (Subdivide_front.subdivide ~grain:1 ~resulting_crease_group:"remaining" input |> get_rdk))
    "uniform creasing is not the compatibility default";
  let midpoint neighbor = Geometry.point_count input + edge neighbor in
  let close left right = Float.abs (left -. right) <= 1e-12 in
  check (close (edge_weight uniform 4 (midpoint 1)) 3.
      && close (edge_weight uniform (midpoint 1) 1) 3.)
    "uniform creasing did not decrement both child edges equally";
  check (close (edge_weight chaikin 4 (midpoint 1)) 2.875
      && close (edge_weight chaikin (midpoint 1) 1) 3.
      && close (edge_weight chaikin 4 (midpoint 3)) (37. /. 24.)
      && close (edge_weight chaikin (midpoint 3) 3) 1.
      && close (edge_weight chaikin 4 (midpoint 5)) (133. /. 24.)
      && close (edge_weight chaikin (midpoint 5) 5) 7.
      && close (edge_weight chaikin 4 (midpoint 7)) (13. /. 24.)
      && close (edge_weight chaikin (midpoint 7) 7) 0.)
    "Chaikin child sharpness did not use the exact endpoint neighborhood mean";
  let uniform_group = Geometry.find_edge_group "remaining" uniform |> Option.get
  and chaikin_group = Geometry.find_edge_group "remaining" chaikin |> Option.get in
  check (Edge_group.cardinality uniform_group = 6
      && Edge_group.cardinality chaikin_group = 7)
    "resulting crease group did not classify endpoint-dependent Chaikin children";
  let chaikin_index = Topology_index.create (Geometry.topology chaikin) in
  let lifted = Topology_index.find_edge chaikin_index ~a:4 ~b:(midpoint 7)
      |> Option.get
  and decayed = Topology_index.find_edge chaikin_index ~a:(midpoint 7) ~b:7
      |> Option.get in
  check (Edge_group.mem lifted chaikin_group
      && not (Edge_group.mem decayed chaikin_group))
    "Chaikin resulting group lost its asymmetric child-edge membership";

  let mask_base = Plane_generators.grid ~connectivity:Plane_generators.Grid_quads
      ~columns:3 ~rows:2 ~size:3. () |> get_rdk in
  let mask_positions = Packed.Float3.View.view
      (Geometry.positions mask_base) in
  let mask_positions = Packed.Float3.View.of_owned_exn
      ~x:(Array.copy mask_positions.x) ~y:(Array.copy mask_positions.y)
      ~z:(Array.init (Geometry.point_count mask_base) (fun point ->
        float_of_int ((point * point + (3 * point)) mod 11) *. 0.17)) in
  let mask_base = Geometry.with_positions mask_positions mask_base |> get_string in
  let mask_index_value = Topology_index.create (Geometry.topology mask_base) in
  let mask_index = Topology_index.View.view mask_index_value in
  let target_edge = Topology_index.find_edge mask_index_value ~a:5 ~b:6
      |> Option.get
  and high_left = Topology_index.find_edge mask_index_value ~a:1 ~b:5
      |> Option.get
  and high_right = Topology_index.find_edge mask_index_value ~a:6 ~b:10
      |> Option.get in
  let mask_weights = Array.init (Geometry.vertex_count mask_base) (fun vertex ->
    let edge = mask_index.edge_of_vertex.(vertex) in
    if edge = target_edge then 0.5
    else if edge = high_left || edge = high_right then 8.
    else 0.) in
  let mask_input = Geometry.with_attribute
      (attribute Attribute.Vertex "creaseweight" (Attribute.Float mask_weights))
      mask_base |> get_string in
  let mask_topology = Topology.View.view (Geometry.topology mask_input) in
  let mask_fvar = attribute Attribute.Vertex "fvar"
      (Attribute.Float (Array.map (fun point ->
        (Packed.Float3.View.view (Geometry.positions mask_input)).z.(point))
        mask_topology.vertex_points)) in
  let mask_input = Geometry.with_attribute mask_fvar mask_input |> get_string in
  let mask_uniform = Subdivide_front.subdivide ~grain:1
      ~creasing_method:Subdivide.Subdivide_creasing_uniform
      ~face_varying_interpolation:Subdivide.Subdivide_fvar_none mask_input |> get_rdk
  and mask_chaikin = Subdivide_front.subdivide ~grain:1
      ~creasing_method:Subdivide.Subdivide_creasing_chaikin
      ~face_varying_interpolation:Subdivide.Subdivide_fvar_none mask_input |> get_rdk in
  let source_positions = Packed.Float3.View.view (Geometry.positions mask_input)
  and uniform_positions = Packed.Float3.View.view
      (Geometry.positions mask_uniform)
  and chaikin_positions = Packed.Float3.View.view
      (Geometry.positions mask_chaikin) in
  let target_point = Geometry.point_count mask_input + target_edge in
  let midpoint_z = (source_positions.z.(5) +. source_positions.z.(6)) *. 0.5 in
  check (close chaikin_positions.z.(target_point) midpoint_z
      && not (close uniform_positions.z.(target_point) midpoint_z))
    "Chaikin did not keep a sub-unit edge fully creased when both children survive";
  let fvar_at geometry point =
    let topology = Topology.View.view (Geometry.topology geometry) in
    let values = float_values Attribute.Vertex "fvar" geometry in
    let result = ref None in
    Array.iteri (fun vertex vertex_point ->
      if vertex_point = point && !result = None then result := Some values.(vertex))
      topology.vertex_points;
    Option.get !result in
  check (close (fvar_at mask_chaikin target_point) midpoint_z
      && not (close (fvar_at mask_uniform target_point) midpoint_z))
    "Chaikin edge rules did not refine continuous face-varying data";
  let crease_vertex_z = (source_positions.z.(5) *. 0.75)
      +. ((source_positions.z.(1) +. source_positions.z.(6)) *. 0.125) in
  check (close chaikin_positions.z.(5) crease_vertex_z
      && not (close uniform_positions.z.(5) crease_vertex_z))
    "Chaikin vertex rule did not follow its surviving child crease neighborhood";

  let exact domains = Parallel.run ~domains (fun () ->
    Subdivide_front.subdivide ~grain:1 ~iterations:2
      ~creasing_method:Subdivide.Subdivide_creasing_chaikin
      ~resulting_crease_group:"remaining" (make ()) |> get_rdk) in
  let exact_one = exact 1 and exact_four = exact 4 in
  check (equal_geometry exact_one exact_four)
    "recursive Chaikin creasing differs across one and four domains";
  check (not (equal_geometry exact_one
      (Subdivide_front.subdivide ~grain:1 ~iterations:2
        ~creasing_method:Subdivide.Subdivide_creasing_uniform
        ~resulting_crease_group:"remaining" (make ()) |> get_rdk)))
    "recursive Chaikin creasing collapsed to uniform decay";

  let clean = Plane_generators.grid ~connectivity:Plane_generators.Grid_quads
      ~columns:2 ~rows:2 ~size:2. () |> get_rdk in
  let second_input = Subdivide_front.subdivide ~grain:1 ~creases:input
      ~creasing_method:Subdivide.Subdivide_creasing_chaikin
      ~resulting_crease_group:"remaining" clean |> get_rdk in
  check (equal_geometry chaikin second_input)
    "second-input Chaikin creases differ from source-attribute creases";

  let selection = Group.ordered ~owner:Group.Primitive ~name:"refine"
      ~length:(Geometry.primitive_count input) [|0;1|] |> get_string in
  let local_input = Geometry.with_group selection input |> get_string in
  let local domains = Parallel.run ~domains (fun () ->
    Subdivide_front.subdivide ~grain:1 ~primitives:selection
      ~creasing_method:Subdivide.Subdivide_creasing_chaikin local_input |> get_rdk) in
  check (equal_geometry (local 1) (local 4))
    "local Chaikin creasing differs across one and four domains";

  let loop domains = Parallel.run ~domains (fun () ->
    Subdivide_front.subdivide ~grain:1 ~scheme:Subdivide.Loop
      ~creasing_method:Subdivide.Subdivide_creasing_chaikin
      (make ~connectivity:Plane_generators.Grid_triangles ()) |> get_rdk) in
  check (equal_geometry (loop 1) (loop 4))
    "Loop Chaikin creasing differs across one and four domains";

  let corner_only = quad ~corner_weights:[|2.;0.;0.;0.|] () in
  let corner_uniform = Subdivide_front.subdivide
      ~creasing_method:Subdivide.Subdivide_creasing_uniform corner_only |> get_rdk
  and corner_chaikin = Subdivide_front.subdivide
      ~creasing_method:Subdivide.Subdivide_creasing_chaikin corner_only |> get_rdk in
  check (equal_geometry corner_uniform corner_chaikin)
    "Chaikin edge policy changed uniform corner sharpness decay"

let test_point_boundary_interpolation () =
  let input = quad () |> Geometry.with_attribute
      (attribute Attribute.Point "sample" (Attribute.Float [|0.;2.;4.;6.|]))
      |> get_string in
  let edge_only = Subdivide_front.subdivide
      ~boundary_interpolation:Subdivide.Subdivide_boundary_edge_only input |> get_rdk
  and edge_and_corner = Subdivide_front.subdivide
      ~boundary_interpolation:Subdivide.Subdivide_boundary_edge_and_corner input
      |> get_rdk in
  let edge_positions = Packed.Float3.View.view
      (Geometry.positions edge_only)
  and corner_positions = Packed.Float3.View.view
      (Geometry.positions edge_and_corner) in
  check (edge_positions.x.(0) = 0.25 && edge_positions.y.(0) = 0.25)
    "Edge Only did not apply the smooth boundary-curve stencil";
  check (corner_positions.x.(0) = 0. && corner_positions.y.(0) = 0.)
    "Edge and Corner did not pin a one-face boundary vertex";
  check ((float_values Attribute.Point "sample" edge_only).(0) = 1.)
    "Edge Only did not interpolate an ordinary point attribute";
  check ((float_values Attribute.Point "sample" edge_and_corner).(0) = 0.)
    "Edge and Corner did not pin an ordinary point attribute";

  let none = Subdivide_front.subdivide
      ~boundary_interpolation:Subdivide.Subdivide_boundary_none input |> get_rdk in
  check (Geometry.point_count none = 9 && Geometry.vertex_count none = 0
      && Geometry.primitive_count none = 0)
    "None did not turn an open single-face boundary into a hole";
  check (match Geometry.find_group ~owner:Group.Primitive
      "subdivision_hole" none with
    | Some holes -> Group.length holes = 0 && Group.cardinality holes = 0
    | None -> false)
    "None did not preserve canonical empty output-hole membership";
  let retained = Subdivide_front.subdivide ~remove_holes:false
      ~boundary_interpolation:Subdivide.Subdivide_boundary_none input |> get_rdk in
  check (Geometry.primitive_count retained = 4
      && Group.cardinality
           (group Group.Primitive "subdivision_hole" retained) = 4)
    "None with Remove Holes disabled did not retain propagated boundary holes";

  let grid () = Plane_generators.grid ~connectivity:Plane_generators.Grid_quads
      ~columns:3 ~rows:3 ~size:3. () |> get_rdk in
  let run policy domains = Parallel.run ~domains (fun () ->
    Subdivide_front.subdivide ~grain:1 ~boundary_interpolation:policy (grid ()) |> get_rdk) in
  List.iter (fun policy ->
    let one = run policy 1 and four = run policy 4 in
    check (equal_geometry one four)
      "point-boundary interpolation differs between one and four domains")
    [Subdivide.Subdivide_boundary_none; Subdivide.Subdivide_boundary_edge_only;
     Subdivide.Subdivide_boundary_edge_and_corner];
  let center_only = run Subdivide.Subdivide_boundary_none 1 in
  check (Geometry.primitive_count center_only = 4)
    "None did not retain exactly the descendants of the interior grid face";
  let recursive = Subdivide_front.subdivide ~iterations:2
      ~boundary_interpolation:Subdivide.Subdivide_boundary_none (grid ()) |> get_rdk in
  check (Geometry.primitive_count recursive = 16)
    "recursive None removed boundary holes before the final level";
  let with_center_hole = grid () |> Geometry.with_group
      (Group.ordered ~owner:Group.Primitive ~name:"subdivision_hole"
        ~length:9 [|4|] |> get_string) |> get_string in
  let all_holes = Subdivide_front.subdivide
      ~boundary_interpolation:Subdivide.Subdivide_boundary_none with_center_hole
      |> get_rdk in
  check (Geometry.primitive_count all_holes = 0)
    "None did not union automatic boundary holes with explicit holes";
  let local_source = grid () |> Geometry.with_group
      (Group.ordered ~owner:Group.Primitive ~name:"center"
        ~length:9 [|4|] |> get_string) |> get_string in
  let local = Subdivide_front.subdivide ~grain:1
      ~primitives:(group Group.Primitive "center" local_source)
      ~boundary_interpolation:Subdivide.Subdivide_boundary_none local_source
      |> get_rdk in
  check (Geometry.primitive_count local = 4)
    "local None did not include its automatic boundary-hole partition";

  let strip = Plane_generators.grid ~connectivity:Plane_generators.Grid_quads
      ~columns:2 ~rows:1 ~size:2. () |> get_rdk in
  let strip_edge = Subdivide_front.subdivide
      ~boundary_interpolation:Subdivide.Subdivide_boundary_edge_only strip |> get_rdk
  and strip_corners = Subdivide_front.subdivide
      ~boundary_interpolation:Subdivide.Subdivide_boundary_edge_and_corner strip
      |> get_rdk in
  let edge_positions = Packed.Float3.View.view
      (Geometry.positions strip_edge)
  and corner_positions = Packed.Float3.View.view
      (Geometry.positions strip_corners) in
  check (edge_positions.x.(1) = corner_positions.x.(1)
      && edge_positions.y.(1) = corner_positions.y.(1)
      && edge_positions.z.(1) = corner_positions.z.(1))
    "Edge and Corner pinned a two-face boundary vertex";

  let triangle_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;2.;0.|] ~y:[|0.;0.;2.|] ~z:[|0.;0.;0.|] in
  let triangle_topology = Topology.polygons_owned ~point_count:3
      ~vertex_points:[|0;1;2|] ~primitive_offsets:[|0;3|] |> get_string in
  let triangle = Geometry.create ~positions:triangle_positions
      ~topology:triangle_topology () |> get_string in
  let loop_edge = Subdivide_front.subdivide ~scheme:Subdivide.Loop
      ~boundary_interpolation:Subdivide.Subdivide_boundary_edge_only triangle |> get_rdk
  and loop_corners = Subdivide_front.subdivide ~scheme:Subdivide.Loop
      ~boundary_interpolation:Subdivide.Subdivide_boundary_edge_and_corner triangle
      |> get_rdk
  and loop_none = Subdivide_front.subdivide ~scheme:Subdivide.Loop
      ~boundary_interpolation:Subdivide.Subdivide_boundary_none triangle |> get_rdk in
  let loop_edge_positions = Packed.Float3.View.view
      (Geometry.positions loop_edge)
  and loop_corner_positions = Packed.Float3.View.view
      (Geometry.positions loop_corners) in
  check (loop_edge_positions.x.(0) = 0.25
      && loop_edge_positions.y.(0) = 0.25
      && loop_corner_positions.x.(0) = 0.
      && loop_corner_positions.y.(0) = 0.)
    "Loop point-boundary policies did not use their documented stencils";
  check (Geometry.point_count loop_none = 6
      && Geometry.primitive_count loop_none = 0)
    "Loop None did not classify the open triangle as a hole";

  let closed = Box_generator.box ~connectivity:Box_generator.Box_quads ~consolidate_points:true
      ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_rdk in
  let closed_edge = Subdivide_front.subdivide
      ~boundary_interpolation:Subdivide.Subdivide_boundary_edge_only closed |> get_rdk
  and closed_corner = Subdivide_front.subdivide
      ~boundary_interpolation:Subdivide.Subdivide_boundary_edge_and_corner closed
      |> get_rdk
  and closed_none = Subdivide_front.subdivide
      ~boundary_interpolation:Subdivide.Subdivide_boundary_none closed |> get_rdk in
  check (equal_geometry closed_edge closed_corner
      && equal_geometry closed_edge closed_none)
    "point-boundary policy changed a closed manifold";
  let bilinear_none = Subdivide_front.subdivide ~scheme:Subdivide.Bilinear
      ~boundary_interpolation:Subdivide.Subdivide_boundary_none input |> get_rdk
  and bilinear_edge = Subdivide_front.subdivide ~scheme:Subdivide.Bilinear
      ~boundary_interpolation:Subdivide.Subdivide_boundary_edge_only input |> get_rdk in
  check (equal_geometry bilinear_none bilinear_edge)
    "point-boundary interpolation changed bilinear subdivision";

  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Subdivide_front.subdivide ~cancel
      ~boundary_interpolation:Subdivide.Subdivide_boundary_none (grid ()) with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "cancelled boundary-hole classification published geometry")

let test_triangle_subdivision_policy () =
  let make vertex_points primitive_offsets values =
    let positions = Packed.Float3.View.of_owned_exn
        ~x:(Array.copy values) ~y:(Array.make (Array.length values) 0.)
        ~z:(Array.make (Array.length values) 0.) in
    let topology = Topology.polygons_owned ~point_count:(Array.length values)
        ~vertex_points ~primitive_offsets |> get_string in
    let topology_view = Topology.View.view topology in
    let point_value = attribute Attribute.Point "value"
        (Attribute.Float (Array.copy values))
    and fvar = attribute Attribute.Vertex "fvar"
        (Attribute.Float (Array.map (fun point -> values.(point))
          topology_view.vertex_points)) in
    Geometry.create ~positions ~topology ~attributes:[point_value; fvar] ()
    |> get_string in
  let edge_point geometry ~a ~b =
    let index = Topology_index.create (Geometry.topology geometry) in
    Geometry.point_count geometry
      + Option.get (Topology_index.find_edge index ~a ~b) in
  let close left right = Float.abs (left -. right) <= 1e-12 in
  let triangle_values = [|2.;6.;10.;14.|] in
  let triangles = make [|0;1;2; 1;0;3|] [|0;3;6|] triangle_values in
  let shared = edge_point triangles ~a:0 ~b:1 in
  let standard = Subdivide_front.subdivide ~grain:1
      ~face_varying_interpolation:Subdivide.Subdivide_fvar_none
      ~triangle_policy:Subdivide.Subdivide_triangles_catmull_clark triangles |> get_rdk
  and smooth = Subdivide_front.subdivide ~grain:1
      ~face_varying_interpolation:Subdivide.Subdivide_fvar_none
      ~triangle_policy:Subdivide.Subdivide_triangles_smooth triangles |> get_rdk in
  let face0 = (2. +. 6. +. 10.) /. 3.
  and face1 = (6. +. 2. +. 14.) /. 3. in
  let expected_standard = 0.25 *. (2. +. 6. +. face0 +. face1)
  and expected_smooth = (0.03 *. (2. +. 6.)) +. (0.470 *. (face0 +. face1)) in
  let standard_x = (Packed.Float3.View.view
      (Geometry.positions standard)).x.(shared)
  and smooth_x = (Packed.Float3.View.view
      (Geometry.positions smooth)).x.(shared) in
  check (close standard_x expected_standard && close smooth_x expected_smooth)
    "Smooth Triangles did not apply Pixar's two-triangle edge mask";
  check (close (float_values Attribute.Point "value" standard).(shared)
      expected_standard
      && close (float_values Attribute.Point "value" smooth).(shared)
        expected_smooth)
    "Smooth Triangles did not reuse the geometry stencil for point attributes";
  let fvar_at_shared geometry =
    let topology = Topology.View.view (Geometry.topology geometry) in
    let values = float_values Attribute.Vertex "fvar" geometry in
    let result = ref None in
    Array.iteri (fun vertex point ->
      if point = shared && !result = None then result := Some values.(vertex))
      topology.vertex_points;
    Option.get !result in
  check (close (fvar_at_shared standard) expected_standard
      && close (fvar_at_shared smooth) expected_smooth)
    "Smooth Triangles did not apply to smoothly refined face-varying data";
  check (equal_geometry standard
      (Subdivide_front.subdivide ~grain:1
        ~face_varying_interpolation:Subdivide.Subdivide_fvar_none triangles |> get_rdk))
    "Catmull-Clark triangle policy is not the compatibility default";

  let mixed_values = [|2.;6.;10.;14.;18.|] in
  let mixed = make [|0;1;2; 1;0;3;4|] [|0;3;7|] mixed_values in
  let mixed_shared = edge_point mixed ~a:0 ~b:1 in
  let mixed_smooth = Subdivide_front.subdivide ~grain:1
      ~triangle_policy:Subdivide.Subdivide_triangles_smooth mixed |> get_rdk in
  let triangle_face = (2. +. 6. +. 10.) /. 3.
  and quad_face = (6. +. 2. +. 14. +. 18.) /. 4. in
  let expected_mixed = (0.14 *. (2. +. 6.))
      +. (0.36 *. (triangle_face +. quad_face)) in
  check (close (Packed.Float3.View.view
      (Geometry.positions mixed_smooth)).x.(mixed_shared) expected_mixed)
    "Smooth Triangles did not interpolate the mixed triangle/quad edge mask";

  let source_index = Topology_index.create (Geometry.topology triangles)
      |> Topology_index.View.view in
  let source_edge = shared - Geometry.point_count triangles in
  let creaseweights = Array.init (Geometry.vertex_count triangles) (fun vertex ->
    if source_index.edge_of_vertex.(vertex) = source_edge then 1. else 0.) in
  let creased = Geometry.with_attribute
      (attribute Attribute.Vertex "creaseweight" (Attribute.Float creaseweights))
      triangles |> get_string in
  let creased_standard = Subdivide_front.subdivide ~grain:1
      ~triangle_policy:Subdivide.Subdivide_triangles_catmull_clark creased |> get_rdk
  and creased_smooth = Subdivide_front.subdivide ~grain:1
      ~triangle_policy:Subdivide.Subdivide_triangles_smooth creased |> get_rdk in
  check (equal_geometry creased_standard creased_smooth
      && close (Packed.Float3.View.view
        (Geometry.positions creased_smooth)).x.(shared) 4.)
    "fully sharp edge did not take precedence over Smooth Triangles";

  List.iter (fun scheme ->
    let ordinary = Subdivide_front.subdivide ~grain:1 ~scheme
        ~triangle_policy:Subdivide.Subdivide_triangles_catmull_clark triangles |> get_rdk
    and alternate = Subdivide_front.subdivide ~grain:1 ~scheme
        ~triangle_policy:Subdivide.Subdivide_triangles_smooth triangles |> get_rdk in
    check (equal_geometry ordinary alternate)
      "triangle policy changed a non-Catmull-Clark scheme")
    [Subdivide.Loop; Subdivide.Bilinear];

  let dense = Plane_generators.grid ~connectivity:Plane_generators.Grid_triangles
      ~columns:4 ~rows:3 ~size:4. () |> get_rdk in
  let exact domains = Parallel.run ~domains (fun () ->
    Subdivide_front.subdivide ~grain:1 ~iterations:2
      ~triangle_policy:Subdivide.Subdivide_triangles_smooth dense |> get_rdk) in
  check (equal_geometry (exact 1) (exact 4))
    "recursive Smooth Triangles differs across one and four domains";
  let selection = Group.ordered ~owner:Group.Primitive ~name:"refine"
      ~length:(Geometry.primitive_count dense) [|0;1;2;3|] |> get_string in
  let local_input = Geometry.with_group selection dense |> get_string in
  let local domains = Parallel.run ~domains (fun () ->
    Subdivide_front.subdivide ~grain:1 ~primitives:selection
      ~triangle_policy:Subdivide.Subdivide_triangles_smooth local_input |> get_rdk) in
  let local_one = local 1 in
  check (equal_geometry local_one (local 4))
    "local Smooth Triangles differs across one and four domains";
  let local_standard = Subdivide_front.subdivide ~grain:1 ~primitives:selection
      ~triangle_policy:Subdivide.Subdivide_triangles_catmull_clark local_input |> get_rdk in
  check (not (equal_geometry local_one local_standard))
    "local Subdivide ignored its Smooth Triangles policy"

let test_face_varying_interpolation () =
  let with_vertex_values geometry make =
    let topology = Topology.View.view (Geometry.topology geometry) in
    let index = Topology_index.create (Geometry.topology geometry)
        |> Topology_index.View.view in
    let values = Array.init (Geometry.vertex_count geometry) (fun vertex ->
      make ~topology ~index ~vertex
        ~primitive:index.primitive_of_vertex.(vertex)
        ~point:topology.vertex_points.(vertex)) in
    Geometry.with_attribute
      (attribute Attribute.Vertex "fvar" (Attribute.Float values)) geometry
      |> get_string in
  let source_corner geometry ~primitive ~point =
    let topology = Topology.View.view (Geometry.topology geometry) in
    let result = ref (-1) in
    for vertex = topology.primitive_offsets.(primitive)
        to topology.primitive_offsets.(primitive + 1) - 1 do
      if topology.vertex_points.(vertex) = point then result := vertex
    done;
    if !result < 0 then fail "test face does not reference the requested point";
    !result in
  let output_original_corner source_vertex = source_vertex * 4 in
  let run ?(iterations = 1) mode geometry =
    Subdivide_front.subdivide ~grain:1 ~iterations
      ~face_varying_interpolation:mode geometry |> get_rdk in
  let continuous () = Plane_generators.grid ~connectivity:Plane_generators.Grid_quads
      ~columns:3 ~rows:3 ~size:3. () |> get_rdk
      |> fun geometry -> with_vertex_values geometry
        (fun ~topology:_ ~index:_ ~vertex:_ ~primitive:_ ~point ->
          let value = float_of_int point in value *. value) in
  let input = continuous () in
  let center_corner = source_corner input ~primitive:0 ~point:5
  and boundary_corner = source_corner input ~primitive:0 ~point:1
  and outer_corner = source_corner input ~primitive:0 ~point:0 in
  let center_output = output_original_corner center_corner
  and boundary_output = output_original_corner boundary_corner
  and outer_output = output_original_corner outer_corner in
  let none = run Subdivide.Subdivide_fvar_none input
  and corners = run Subdivide.Subdivide_fvar_corners_only input
  and plus1 = run Subdivide.Subdivide_fvar_corners_plus1 input
  and plus2 = run Subdivide.Subdivide_fvar_corners_plus2 input
  and boundaries = run Subdivide.Subdivide_fvar_boundaries input
  and all = run Subdivide.Subdivide_fvar_all input in
  let values geometry = float_values Attribute.Vertex "fvar" geometry in
  check ((values all).(center_output) = 25.)
    "Linear All did not pin an interior face-varying value";
  check ((values none).(center_output) <> 25.)
    "Linear None did not smooth a continuous interior face-varying value";
  check ((values none).(outer_output) <> 0.
      && (values corners).(outer_output) = 0.)
    "Corners Only did not pin exactly a one-face region corner";
  check ((values corners).(boundary_output) <> 1.
      && (values boundaries).(boundary_output) = 1.)
    "Boundaries did not linearly constrain a multi-face region boundary";
  check (values corners = values plus1 && values corners = values plus2)
    "Plus policies changed a continuous region without a plus feature";
  let source_values = values input in
  let tupled = input
      |> Geometry.with_attribute (attribute Attribute.Vertex "fvar2"
        (Attribute.Float2 (Packed.Float2.of_owned
          ~x:(Array.copy source_values)
          ~y:(Array.map (fun value -> (2. *. value) +. 1.) source_values)
          |> get_string))) |> get_string
      |> Geometry.with_attribute (attribute Attribute.Vertex "fvar3"
        (Attribute.Float3 (Packed.Float3.View.of_owned_exn
          ~x:(Array.copy source_values)
          ~y:(Array.map (fun value -> value +. 3.) source_values)
          ~z:(Array.map (fun value -> (-2.) *. value) source_values))))
        |> get_string
      |> Geometry.with_attribute (attribute Attribute.Vertex "fvar4"
        (Attribute.Float4 (Packed.Float4.of_owned
          ~x:(Array.copy source_values)
          ~y:(Array.map (fun value -> value +. 5.) source_values)
          ~z:(Array.map (fun value -> value *. 3.) source_values)
          ~w:(Array.map (fun value -> 7. -. value) source_values)
          |> get_string))) |> get_string in
  let tupled = run Subdivide.Subdivide_fvar_none tupled in
  let scalar = (values tupled).(center_output) in
  let fvar2 = match Geometry.find_attribute ~owner:Attribute.Vertex
      "fvar2" tupled |> Option.get |> Attribute.storage with
    | Attribute.Float2 values -> Packed.Float2.View.view values
    | _ -> fail "fvar2 storage changed" in
  let fvar3 = match Geometry.find_attribute ~owner:Attribute.Vertex
      "fvar3" tupled |> Option.get |> Attribute.storage with
    | Attribute.Float3 values -> Packed.Float3.View.view values
    | _ -> fail "fvar3 storage changed" in
  let fvar4 = match Geometry.find_attribute ~owner:Attribute.Vertex
      "fvar4" tupled |> Option.get |> Attribute.storage with
    | Attribute.Float4 values -> Packed.Float4.View.view values
    | _ -> fail "fvar4 storage changed" in
  check (fvar2.x.(center_output) = scalar
      && fvar2.y.(center_output) = (2. *. scalar) +. 1.
      && fvar3.y.(center_output) = scalar +. 3.
      && fvar3.z.(center_output) = (-2.) *. scalar
      && fvar4.y.(center_output) = scalar +. 5.
      && fvar4.z.(center_output) = scalar *. 3.
      && fvar4.w.(center_output) = 7. -. scalar)
    "tuple face-varying components did not share one interpolation plan";

  let center_fixture center_values =
    Plane_generators.grid ~connectivity:Plane_generators.Grid_quads ~columns:2 ~rows:2 ~size:2. ()
    |> get_rdk |> fun geometry -> with_vertex_values geometry
      (fun ~topology:_ ~index:_ ~vertex:_ ~primitive ~point ->
        if point = 4 then center_values.(primitive)
        else float_of_int (point * point)) in
  let junction = center_fixture [|10.;10.;20.;30.|] in
  let junction_corner = source_corner junction ~primitive:0 ~point:4
      |> output_original_corner in
  let junction_corners = run Subdivide.Subdivide_fvar_corners_only junction
  and junction_plus1 = run Subdivide.Subdivide_fvar_corners_plus1 junction in
  check ((values junction_corners).(junction_corner) <> 10.
      && (values junction_plus1).(junction_corner) = 10.)
    "Corners Plus 1 did not pin a junction of three face-varying regions";
  let junction_topology = Topology.View.view
      (Geometry.topology junction_plus1) in
  let distinct = Hashtbl.create 4 in
  Array.iteri (fun vertex point ->
    if point = 4 then Hashtbl.replace distinct (values junction_plus1).(vertex) ())
    junction_topology.vertex_points;
  check (Hashtbl.length distinct = 3)
    "smoothed face-varying interpolation collapsed an authored seam";
  let junction_scalar = values junction in
  let tuple_junction = Geometry.with_attribute
      (attribute Attribute.Vertex "tuple_seam"
        (Attribute.Float2 (Packed.Float2.of_owned
          ~x:(Array.make (Array.length junction_scalar) 0.)
          ~y:(Array.copy junction_scalar) |> get_string))) junction
      |> get_string |> run Subdivide.Subdivide_fvar_corners_plus1 in
  let tuple_values = match Geometry.find_attribute ~owner:Attribute.Vertex
      "tuple_seam" tuple_junction |> Option.get |> Attribute.storage with
    | Attribute.Float2 values -> Packed.Float2.View.view values
    | _ -> fail "tuple seam storage changed" in
  let tuple_topology = Topology.View.view
      (Geometry.topology tuple_junction) in
  let tuple_distinct = Hashtbl.create 4 in
  Array.iteri (fun vertex point ->
    if point = 4 then Hashtbl.replace tuple_distinct tuple_values.y.(vertex) ())
    tuple_topology.vertex_points;
  check (Hashtbl.length tuple_distinct = 3)
    "face-varying seam classification ignored a non-leading tuple component";

  let concave = center_fixture [|20.;10.;10.;10.|] in
  let concave_corner = source_corner concave ~primitive:1 ~point:4
      |> output_original_corner in
  let concave_plus1 = run Subdivide.Subdivide_fvar_corners_plus1 concave
  and concave_plus2 = run Subdivide.Subdivide_fvar_corners_plus2 concave in
  check ((values concave_plus1).(concave_corner) <> 10.
      && (values concave_plus2).(concave_corner) = 10.)
    "Corners Plus 2 did not propagate a concave one-face corner";

  let dart = Plane_generators.grid ~connectivity:Plane_generators.Grid_quads
      ~columns:2 ~rows:2 ~size:2. () |> get_rdk
      |> fun geometry -> with_vertex_values geometry
        (fun ~topology:_ ~index:_ ~vertex:_ ~primitive ~point ->
          if point = 4 then 10.
          else if point = 1 && primitive = 0 then 1.
          else if point = 1 && primitive = 1 then 2.
          else float_of_int (point * point)) in
  let dart_corner = source_corner dart ~primitive:0 ~point:4
      |> output_original_corner in
  let dart_plus1 = run Subdivide.Subdivide_fvar_corners_plus1 dart
  and dart_plus2 = run Subdivide.Subdivide_fvar_corners_plus2 dart in
  check ((values dart_plus1).(dart_corner) <> 10.
      && (values dart_plus2).(dart_corner) = 10.)
    "Corners Plus 2 did not pin a face-varying dart";

  let center_point = 5 in
  let corner_weight = attribute Attribute.Point "cornerweight"
      (Attribute.Float (Array.init (Geometry.point_count input) (fun point ->
        if point = center_point then 1. else 0.))) in
  let corner_input = Geometry.with_attribute corner_weight input |> get_string in
  let corner_output = run Subdivide.Subdivide_fvar_none corner_input in
  check ((values corner_output).(center_output) = 25.)
    "geometry corner sharpness did not take precedence over FVar None";
  let source_index = Topology_index.create (Geometry.topology input) in
  let crease_edge = Topology_index.find_edge source_index ~a:5 ~b:6
      |> Option.get in
  let source_index_view = Topology_index.View.view source_index in
  let crease_weights = Array.init (Geometry.vertex_count input) (fun vertex ->
    if source_index_view.edge_of_vertex.(vertex) = crease_edge then 1. else 0.) in
  let creased_input = Geometry.with_attribute
      (attribute Attribute.Vertex "creaseweight" (Attribute.Float crease_weights))
      input |> get_string in
  let smooth_output = run Subdivide.Subdivide_fvar_none input
  and creased_output = run Subdivide.Subdivide_fvar_none creased_input in
  let directed = source_index_view.edge_vertices.
      (source_index_view.edge_offsets.(crease_edge)) in
  let edge_output = (directed * 4) + 1 in
  check ((values smooth_output).(edge_output)
      <> (values creased_output).(edge_output))
    "geometry edge sharpness did not take precedence over FVar None";

  let recursive_one = Parallel.run ~domains:1 (fun () ->
    run ~iterations:2 Subdivide.Subdivide_fvar_none (continuous ()))
  and recursive_four = Parallel.run ~domains:4 (fun () ->
    run ~iterations:2 Subdivide.Subdivide_fvar_none (continuous ())) in
  check (equal_geometry recursive_one recursive_four)
    "recursive smooth face-varying refinement differs across domains";
  check (Geometry.vertex_count recursive_one = 9 * 16 * 4)
    "recursive face-varying refinement cardinality";

  let loop_input = Plane_generators.grid ~connectivity:Plane_generators.Grid_triangles
      ~columns:3 ~rows:2 ~size:3. () |> get_rdk
      |> fun geometry -> with_vertex_values geometry
        (fun ~topology:_ ~index:_ ~vertex:_ ~primitive ~point ->
          float_of_int ((point * point) + (primitive / 4))) in
  let fvar_modes = [
    Subdivide.Subdivide_fvar_none;
    Subdivide.Subdivide_fvar_corners_only;
    Subdivide.Subdivide_fvar_corners_plus1;
    Subdivide.Subdivide_fvar_corners_plus2;
    Subdivide.Subdivide_fvar_boundaries;
    Subdivide.Subdivide_fvar_all;
  ] in
  List.iter (fun mode ->
    let refine domains = Parallel.run ~domains (fun () ->
      Subdivide_front.subdivide ~grain:1 ~scheme:Subdivide.Loop
        ~face_varying_interpolation:mode loop_input |> get_rdk) in
    check (equal_geometry (refine 1) (refine 4))
      "Loop face-varying policy differs across one and four domains")
    fvar_modes;
  let loop_none = Subdivide_front.subdivide ~grain:1 ~scheme:Subdivide.Loop
      ~face_varying_interpolation:Subdivide.Subdivide_fvar_none loop_input |> get_rdk
  and loop_all = Subdivide_front.subdivide ~grain:1 ~scheme:Subdivide.Loop
      ~face_varying_interpolation:Subdivide.Subdivide_fvar_all loop_input |> get_rdk in
  check (float_values Attribute.Vertex "fvar" loop_none
      <> float_values Attribute.Vertex "fvar" loop_all)
    "Loop FVar None did not smooth values relative to Linear All";

  let local_selection = Group.ordered ~owner:Group.Primitive ~name:"refine"
      ~length:(Geometry.primitive_count input) [|0;1;3;4|] |> get_string in
  let local_input = Geometry.with_group local_selection input |> get_string in
  let local mode domains = Parallel.run ~domains (fun () ->
    Subdivide_front.subdivide ~grain:1 ~primitives:local_selection
      ~face_varying_interpolation:mode local_input |> get_rdk) in
  let local_none_one = local Subdivide.Subdivide_fvar_none 1
  and local_none_four = local Subdivide.Subdivide_fvar_none 4
  and local_all = local Subdivide.Subdivide_fvar_all 1 in
  check (equal_geometry local_none_one local_none_four)
    "local face-varying refinement differs across one and four domains";
  check (float_values Attribute.Vertex "fvar" local_none_one
      <> float_values Attribute.Vertex "fvar" local_all)
    "local Subdivide ignored its face-varying interpolation policy";

  let bilinear_none = Subdivide_front.subdivide ~scheme:Subdivide.Bilinear
      ~face_varying_interpolation:Subdivide.Subdivide_fvar_none input |> get_rdk in
  check (equal_geometry bilinear_none
      (Subdivide_front.subdivide ~scheme:Subdivide.Bilinear
        ~face_varying_interpolation:Subdivide.Subdivide_fvar_all input |> get_rdk))
    "face-varying policy changed bilinear interpolation";
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Subdivide_front.subdivide ~cancel
      ~face_varying_interpolation:Subdivide.Subdivide_fvar_none input with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "cancelled face-varying refinement published geometry")

let curve_network () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 4.|] ~y:[|0.; 2.; 0.|] ~z:[|0.; 0.; 0.|] in
  let topology = Topology.create_owned ~point_count:3
      ~vertex_points:[|0;1; 1;2|] ~primitive_offsets:[|0;2;4|]
      ~primitive_kinds:[|Topology.Open_polyline; Topology.Open_polyline|]
      |> get_string in
  let attributes = [
    attribute Attribute.Point "mass" (Attribute.Float [|0.; 8.; 16.|]);
    attribute Attribute.Point "id" (Attribute.Int [|10;11;12|]);
    attribute Attribute.Point "rows" (Attribute.Int_array
      (Packed.Int_array.create_owned ~offsets:[|0;1;3;4|]
        ~values:[|10;11;111;12|] |> get_string));
    attribute Attribute.Point "N" (Attribute.Float3
      (Packed.Float3.View.of_owned_exn ~x:[|0.;0.;0.|]
        ~y:[|0.;0.;0.|] ~z:[|1.;1.;1.|]));
    attribute Attribute.Vertex "u" (Attribute.Float [|0.;2.; 10.;14.|]);
    attribute Attribute.Vertex "label" (Attribute.Text [|"a";"b";"c";"d"|]);
    attribute Attribute.Primitive "material" (Attribute.Int [|7;9|]);
    attribute Attribute.Detail "tag" (Attribute.Text [|"curves"|]);
  ] in
  let point_group = Group.ordered ~owner:Group.Point ~name:"picked_points"
      ~length:3 [|1;0|] |> get_string
  and vertex_group = Group.ordered ~owner:Group.Vertex ~name:"picked_vertices"
      ~length:4 [|1;0|] |> get_string
  and primitive_group = Group.ordered ~owner:Group.Primitive
      ~name:"picked_primitives" ~length:2 [|1|] |> get_string in
  let edge_index = Topology_index.create topology in
  let edge_builder = Edge_group.Builder.create ~topology ~index:edge_index
      ~name:"first_edge" in
  Edge_group.Builder.set edge_builder
    (Topology_index.find_edge edge_index ~a:0 ~b:1 |> Option.get) true;
  Geometry.create ~positions ~topology ~attributes
    ~groups:[point_group; vertex_group; primitive_group]
    ~edge_groups:[Edge_group.Builder.freeze edge_builder] () |> get_string

let test_polygon_curve_subdivision () =
  let run domains ?(independent = false) ?(iterations = 1)
      ?(scheme = Subdivide.Catmull_clark) () =
    Parallel.run ~domains (fun () ->
      Subdivide_front.subdivide ~grain:1 ~iterations ~scheme
        ~treat_curves_as_independent:independent (curve_network ()) |> get_rdk) in
  let shared = run 1 () and shared_four = run 4 () in
  check (equal_geometry shared shared_four)
    "shared polygon-curve subdivision differs across one and four domains";
  check (Geometry.point_count shared = 5 && Geometry.vertex_count shared = 6
      && Geometry.primitive_count shared = 2)
    "shared polygon-curve subdivision cardinality";
  let topology = Topology.View.view (Geometry.topology shared) in
  check (topology.vertex_points = [|0;3;1; 1;4;2|]
      && Bytes.equal topology.primitive_kinds (Bytes.of_string "\001\001"))
    "shared polygon-curve subdivision topology";
  let positions = Packed.Float3.View.view (Geometry.positions shared) in
  check (positions.x = [|0.;1.25;4.;0.5;2.5|]
      && positions.y = [|0.;1.5;0.;1.;1.|])
    "shared polygon-curve cubic stencil";
  check (float_values Attribute.Point "mass" shared = [|0.;8.;16.;4.;12.|])
    "polygon-curve point attribute stencil";
  check (int_values Attribute.Point "id" shared = [|10;11;12;10;11|])
    "polygon-curve discrete point representatives";
  check (float_values Attribute.Vertex "u" shared
      = [|0.;1.;2.; 10.;12.;14.|])
    "polygon-curve vertex interpolation";
  check (int_values Attribute.Primitive "material" shared = [|7;9|])
    "polygon-curve primitive ancestry";
  let curve_n = float3_values Attribute.Point "N" shared in
  check (Array.for_all (( = ) 0.) curve_n.x
      && Array.for_all (( = ) 0.) curve_n.y
      && Array.for_all (( = ) 1.) curve_n.z)
    "polygon-curve subdivision did not interpolate point normals";
  check (Group.ordered_elements (group Group.Point "picked_points" shared)
      = Some [|1;0;3|]) "polygon-curve ordered point-group ancestry";
  check (Group.mem 3 (group Group.Point "picked_points" shared)
      && not (Group.mem 4 (group Group.Point "picked_points" shared)))
    "polygon-curve point-group midpoint intersection";
  check (Group.ordered_elements (group Group.Vertex "picked_vertices" shared)
      = Some [|2;0;1|]) "polygon-curve ordered vertex-group ancestry";
  let first_edges = Geometry.find_edge_group "first_edge" shared |> Option.get in
  check (Edge_group.cardinality first_edges = 2)
    "polygon-curve native edge ancestry";
  let independent = run 1 ~independent:true ()
  and independent_four = run 4 ~independent:true () in
  check (equal_geometry independent independent_four)
    "independent polygon-curve subdivision differs across domains";
  check (Geometry.point_count independent = 6
      && Geometry.vertex_count independent = 6)
    "independent polygon-curve subdivision did not duplicate shared corners";
  let independent_positions = Packed.Float3.View.view
      (Geometry.positions independent) in
  check (independent_positions.x = [|0.;0.5;1.; 1.;2.5;4.|]
      && independent_positions.y = [|0.;1.;2.; 2.;1.;0.|])
    "independent polygon-curve endpoint rule";
  let recursive = run 1 ~iterations:2 ()
  and recursive_four = run 4 ~iterations:2 () in
  check (equal_geometry recursive recursive_four
      && Geometry.point_count recursive = 9
      && Geometry.vertex_count recursive = 10)
    "recursive shared polygon-curve refinement";
  let recursive_independent = run 1 ~iterations:2 ~independent:true () in
  check (Geometry.point_count recursive_independent = 10
      && Geometry.vertex_count recursive_independent = 10)
    "recursive independent polygon-curve refinement";
  let bilinear = run 1 ~scheme:Subdivide.Bilinear () in
  let bilinear_positions = Packed.Float3.View.view
      (Geometry.positions bilinear) in
  check (bilinear_positions.x.(1) = 1. && bilinear_positions.y.(1) = 2.)
    "bilinear polygon-curve refinement moved an old point";
  let overridden = curve_network ()
      |> with_detail "osd_scheme" (Attribute.Text [|"bilinear"|])
      |> Subdivide_front.subdivide ~scheme:Subdivide.Catmull_clark |> get_rdk
      |> without_details ["osd_scheme"] in
  check (equal_geometry overridden bilinear)
    "polygon-curve subdivision ignored its osd_scheme detail override";
  let local_source = curve_network () in
  let local_selection = Group.ordered ~owner:Group.Primitive ~name:"local_curve"
      ~length:2 [|0|] |> get_string in
  let local domains = Parallel.run ~domains (fun () ->
    Subdivide_front.subdivide ~grain:1 ~primitives:local_selection local_source |> get_rdk) in
  let local_one = local 1 and local_four = local 4 in
  check (equal_geometry local_one local_four
      && Geometry.primitive_count local_one = 2
      && Geometry.vertex_count local_one = 5)
    "local polygon-curve subdivision exactness/cardinality";
  let local_topology = Topology.View.view (Geometry.topology local_one) in
  check (local_topology.primitive_offsets = [|0;3;5|])
    "local polygon-curve subdivision did not restore primitive order";
  let closed_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;2.;2.;0.|] ~y:[|0.;0.;2.;2.|] ~z:[|0.;0.;0.;0.|] in
  let closed_topology = Topology.create_owned ~point_count:4
      ~vertex_points:[|0;1;2;3|] ~primitive_offsets:[|0;4|]
      ~primitive_kinds:[|Topology.Closed_polyline|] |> get_string in
  let closed_source = Geometry.create ~positions:closed_positions
      ~topology:closed_topology () |> get_string in
  let closed domains = Parallel.run ~domains (fun () ->
    Subdivide_front.subdivide ~grain:1 closed_source |> get_rdk) in
  let closed_one = closed 1 and closed_four = closed 4 in
  check (equal_geometry closed_one closed_four
      && Geometry.point_count closed_one = 8
      && Geometry.vertex_count closed_one = 8
      && Topology.primitive_kind (Geometry.topology closed_one) 0
         = Topology.Closed_polyline)
    "closed polygon-curve subdivision exactness/topology";
  let closed_output = Packed.Float3.View.view (Geometry.positions closed_one) in
  check (closed_output.x.(0) = 0.25 && closed_output.y.(0) = 0.25)
    "closed polygon-curve cubic stencil";
  let free_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;9.|] ~y:[|0.;0.;9.|] ~z:[|0.;0.;9.|] in
  let free_topology = Topology.create_owned ~point_count:3
      ~vertex_points:[|0;1|] ~primitive_offsets:[|0;2|]
      ~primitive_kinds:[|Topology.Open_polyline|] |> get_string in
  let free_source = Geometry.create ~positions:free_positions
      ~topology:free_topology () |> get_string in
  List.iter (fun independent ->
    let output = Subdivide_front.subdivide ~treat_curves_as_independent:independent
        free_source |> get_rdk in
    let positions = Packed.Float3.View.view (Geometry.positions output) in
    check (Geometry.point_count output = 4
        && Array.exists (( = ) 9.) positions.x
        && Array.exists (( = ) 9.) positions.y
        && Array.exists (( = ) 9.) positions.z)
      "polygon-curve subdivision did not retain a free point exactly once")
    [false; true];
  let invalid_topology = Topology.create_owned ~point_count:2
      ~vertex_points:[|0;0|] ~primitive_offsets:[|0;2|]
      ~primitive_kinds:[|Topology.Open_polyline|] |> get_string in
  let invalid = Geometry.create
      ~positions:(Packed.Float3.View.of_owned_exn
        ~x:[|0.;1.|] ~y:[|0.;0.|] ~z:[|0.;0.|])
      ~topology:invalid_topology () |> get_string in
  (match Subdivide_front.subdivide invalid with
   | Ok _ -> fail "Subdivide accepted a zero-length curve topology edge"
   | Error error -> check (contains (Error.message error) "zero-length")
       "Subdivide returned the wrong zero-length curve diagnostic");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Subdivide_front.subdivide ~cancel (curve_network ()) with
   | Ok _ -> fail "polygon-curve Subdivide ignored cancellation"
   | Error error -> check (Error.code error = "cancelled")
       "polygon-curve Subdivide cancellation diagnostic");
  (match Subdivide_front.subdivide ~scheme:Subdivide.Loop (curve_network ()) with
   | Ok _ -> fail "Subdivide accepted polygon curves under Loop"
   | Error error ->
       check (contains (Error.message error) "cannot refine polygon curves")
         "Subdivide returned the wrong Loop/curve diagnostic")

let mixed_surface_curves () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.;1.;4.; 0.;2.;2.;0.|]
      ~y:[|0.;2.;0.; 4.;4.;6.;6.|]
      ~z:[|0.;0.;0.; 0.;0.;0.;0.|] in
  let topology = Topology.create_owned ~point_count:7
      ~vertex_points:[|0;1; 3;4;5;6; 1;2|]
      ~primitive_offsets:[|0;2;6;8|]
      ~primitive_kinds:[|Topology.Open_polyline; Topology.Polygon;
        Topology.Open_polyline|] |> get_string in
  let attributes = [
    attribute Attribute.Point "weight"
      (Attribute.Float [|0.;8.;16.; 20.;21.;22.;23.|]);
    attribute Attribute.Point "N" (Attribute.Float3
      (Packed.Float3.View.of_owned_exn ~x:(Array.make 7 0.)
        ~y:(Array.make 7 0.) ~z:(Array.make 7 1.)));
    attribute Attribute.Vertex "u"
      (Attribute.Float [|0.;2.; 3.;4.;5.;6.; 10.;14.|]);
    attribute Attribute.Primitive "source_kind" (Attribute.Int [|1;0;1|]);
    attribute Attribute.Detail "tag" (Attribute.Text [|"mixed"|]);
  ] in
  Geometry.create ~positions ~topology ~attributes () |> get_string

let test_mixed_surface_curve_subdivision () =
  let run domains ?selection ?(independent = false) ?(recompute = false) () =
    Parallel.run ~domains (fun () ->
      let geometry = mixed_surface_curves () in
      Subdivide_front.subdivide ~grain:1 ?primitives:selection
        ~treat_curves_as_independent:independent
        ~recompute_point_normals:recompute geometry |> get_rdk) in
  let one = run 1 () and four = run 4 () in
  check (equal_geometry one four)
    "mixed surface/curve subdivision differs across one and four domains";
  check (Geometry.primitive_count one = 6)
    "mixed surface/curve subdivision primitive cardinality";
  let topology = Topology.View.view (Geometry.topology one) in
  check (Bytes.equal topology.primitive_kinds
      (Bytes.of_string "\001\000\000\000\000\001"))
    "mixed subdivision did not restore source primitive order";
  check (int_values Attribute.Primitive "source_kind" one = [|1;0;0;0;0;1|])
    "mixed subdivision primitive payload order";
  let tag = Geometry.find_attribute ~owner:Attribute.Detail "tag" one
      |> Option.get in
  check (Attribute.storage tag = Attribute.Text [|"mixed"|])
    "mixed subdivision lost detail attributes";
  let selection = Group.ordered ~owner:Group.Primitive ~name:"local"
      ~length:3 [|0;1|] |> get_string in
  let local_one = run 1 ~selection () and local_four = run 4 ~selection () in
  check (equal_geometry local_one local_four)
    "local mixed subdivision differs across domains";
  check (Geometry.primitive_count local_one = 6)
    "local mixed subdivision changed pass-through curve cardinality";
  let local_topology = Topology.View.view (Geometry.topology local_one) in
  check (Bytes.equal local_topology.primitive_kinds
      (Bytes.of_string "\001\000\000\000\000\001"))
    "local mixed subdivision changed primitive order";
  let independent = run 1 ~independent:true () in
  check (Geometry.point_count independent = Geometry.point_count one + 1)
    "mixed independent curves did not separate the shared curve point";
  let recomputed = run 1 ~recompute:true ()
  and recomputed_four = run 4 ~recompute:true () in
  check (equal_geometry recomputed recomputed_four)
    "mixed recomputed Subdivide normals differ across domain counts";
  let topology = Topology.View.view (Geometry.topology recomputed)
  and n = float3_values Attribute.Point "N" recomputed in
  let surface = Bytes.make topology.point_count '\000'
  and curve = Bytes.make topology.point_count '\000' in
  for primitive = 0 to Bytes.length topology.primitive_kinds - 1 do
    let target = if Bytes.get topology.primitive_kinds primitive = '\000'
      then surface else curve in
    for vertex = topology.primitive_offsets.(primitive)
        to topology.primitive_offsets.(primitive + 1) - 1 do
      Bytes.set target topology.vertex_points.(vertex) '\001'
    done
  done;
  for point = 0 to topology.point_count - 1 do
    if Bytes.get surface point <> '\000' then
      check (n.x.(point) = 0. && n.y.(point) = 0.
          && abs_float n.z.(point) = 1.)
        "mixed Subdivide surface normal was not normalized"
    else if Bytes.get curve point <> '\000' then
      check (n.x.(point) = 0. && n.y.(point) = 0. && n.z.(point) = 0.)
        "mixed Subdivide curve normal was not zero"
  done

let test_checked_loop_quad_error () =
  match Subdivide_front.subdivide ~scheme:Subdivide.Loop (quad ()) with
  | Error error when Error.code error = "invalid_topology" -> ()
  | _ -> fail "Loop Subdivide accepted a quad"

let run () =
  test_checked_loop_quad_error ();
  test_selected_loop_validation_scope ();
  test_disconnected_selected_fans ();
  test_chaikin_creasing ();
  test_point_boundary_interpolation ();
  test_triangle_subdivision_policy ();
  test_face_varying_interpolation ();
  test_polygon_curve_subdivision ();
  test_mixed_surface_curve_subdivision ()
