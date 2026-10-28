open Rays
open Rdk
open Rdk_test_support

let equal_storage left right =
  match Attribute.View.storage left, Attribute.View.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.View.view left
      and right = Packed.Int_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.View.view left
      and right = Packed.Float_array.View.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.View.view left
      and right = Packed.Float2.View.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.View.view left
      and right = Packed.Float3.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.View.view left
      and right = Packed.Float4.View.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | _ -> false

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && String.equal (Attribute.name left) (Attribute.name right)
  && equal_storage left right

let equal_group left right =
  Group.owner left = Group.owner right
  && String.equal (Group.name left) (Group.name right)
  && Group.length left = Group.length right
  && Bytes.equal (Group.View.bits_view left) (Group.View.bits_view right)
  && Group.View.order_view left = Group.View.order_view right

let triangle () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|(-10.); 0.; 10.|] ~y:[|0.; 4.; 0.|] ~z:[|0.; 0.; 0.|] in
  let topology = Rdk_test_support.Tb.create ~point_count:3 () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  Geometry.create ~positions ~topology:(Rdk_test_support.Tb.freeze topology) ()
  |> get_string_ok

let with_attribute name storage geometry =
  Geometry.with_attribute
    (Attribute.create_owned ~name ~owner:Attribute.Point storage |> get_string_ok)
    geometry |> get_string_ok

let int_attribute ~owner name geometry =
  match Geometry.find_attribute ~owner name geometry with
  | Some attribute ->
      (match Attribute.View.storage attribute with
       | Attribute.Int values -> values
       | _ -> fail (name ^ " has unexpected storage"))
  | None -> fail ("missing attribute " ^ name)

let shared_selection_fixture () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|(-1.); 0.0005; 1.; (-1.)|]
      ~y:[|0.; 0.; 1.; 1.|] ~z:[|0.; 0.; 0.; 0.|] in
  let topology = Rdk_test_support.Tb.create ~point_count:4 () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  Rdk_test_support.Tb.add_triangle topology 1 3 2;
  let topology = Rdk_test_support.Tb.freeze topology in
  let primitive_id = Attribute.create_owned ~name:"primitive_id"
      ~owner:Attribute.Primitive (Attribute.Int [|10; 20|]) |> get_string_ok in
  Geometry.create ~positions ~topology ~attributes:[primitive_id] ()
  |> get_string_ok

let check_custom_clip_attribute () =
  let source = triangle ()
      |> with_attribute "field" (Attribute.Float [|(-1.); 1.; 1.|]) in
  let clipped = Plane_clip_front.clip ~clip_attribute:"field" ~origin:Rays_math.Vec3.zero
      ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  let bounds = Analysis_front.bounds clipped |> Option.get in
  check (near bounds.min.x (-5.) && near bounds.max.x 10.)
    "scalar Clip Attribute did not control the intersection parameter";
  let clip storage = triangle () |> with_attribute "field" storage
      |> Plane_clip_front.clip ~clip_attribute:"field" ~origin:Rays_math.Vec3.zero
           ~normal:Rays_math.Vec3.unit_x |> get_ok in
  let float2 = clip (Attribute.Float2 (Packed.Float2.of_owned
      ~x:[|(-1.); 1.; 1.|] ~y:[|0.; 0.; 0.|] |> get_string_ok))
  and float3 = clip (Attribute.Float3 (Packed.Float3.View.of_owned_exn
      ~x:[|(-1.); 1.; 1.|] ~y:[|0.; 0.; 0.|] ~z:[|0.; 0.; 0.|]))
  and float4_a = clip (Attribute.Float4 (Packed.Float4.of_owned
      ~x:[|(-1.); 1.; 1.|] ~y:[|0.; 0.; 0.|] ~z:[|0.; 0.; 0.|]
      ~w:[|(-100.); 100.; 500.|] |> get_string_ok))
  and float4_b = clip (Attribute.Float4 (Packed.Float4.of_owned
      ~x:[|(-1.); 1.; 1.|] ~y:[|0.; 0.; 0.|] ~z:[|0.; 0.; 0.|]
      ~w:[|1.; 2.; 3.|] |> get_string_ok))
  and integer = clip (Attribute.Int [|(-1); 1; 1|]) in
  let same_positions left right =
    let left = Packed.Float3.View.view (Geometry.positions left)
    and right = Packed.Float3.View.view (Geometry.positions right) in
    left.x = right.x && left.y = right.y && left.z = right.z in
  List.iter (fun output -> check (same_positions clipped output)
      "Clip Attribute numeric tuple zero-fill/first-three behavior differs")
    [float2; float3; float4_a; float4_b; integer]

let check_distance () =
  let source = Box_generator.box ~size:(Rays_math.Vec3.create 4. 3. 2.) () |> get_ok in
  let offset = Plane_clip_front.clip ~keep:Plane_clip.All ~distance:0.75 ~origin:Rays_math.Vec3.zero
      ~normal:(Rays_math.Vec3.create 8. 0. 0.) source |> get_ok
  and translated = Plane_clip_front.clip ~keep:Plane_clip.All
      ~origin:(Rays_math.Vec3.create 0.75 0. 0.) ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  check (equal_geometry offset translated)
    "Clip distance is not equivalent to translating along the normalized normal"

let check_selection_isolation () =
  let source = shared_selection_fixture () in
  let selected = Group.init ~owner:Group.Primitive ~name:"selected" 2
      (fun primitive -> primitive = 0) in
  let output = Plane_clip_front.clip ~grain:1 ~snapping_tolerance:0.001
      ~selection:(Element_selection.Selected_primitives selected) ~clipped_group:"clipped"
      ~above_group:"above" ~clipped_edge_group:"clip_edges"
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  let ids = int_attribute ~owner:Attribute.Primitive "primitive_id" output in
  let unselected = ref (-1) in
  Array.iteri (fun primitive id -> if id = 20 then unselected := primitive) ids;
  check (!unselected >= 0) "selected Clip deleted the unselected primitive";
  let topology = Topology.View.view (Geometry.topology output)
  and positions = Packed.Float3.View.view (Geometry.positions output) in
  let first = topology.primitive_offsets.(!unselected)
  and last = topology.primitive_offsets.(!unselected + 1) in
  check (last - first = 3) "selected Clip changed unselected primitive arity";
  let expected = [|(0.0005, 0.); ((-1.), 1.); (1., 1.)|] in
  for local = 0 to 2 do
    let point = topology.vertex_points.(first + local) in
    let expected_x, expected_y = expected.(local) in
    check (positions.x.(point) = expected_x && positions.y.(point) = expected_y)
      "selected Clip moved or reordered an unselected primitive corner"
  done;
  check (Array.exists (fun x -> x = 0.) positions.x
      && Array.exists (fun x -> x = 0.0005) positions.x)
    "selected Clip did not isolate a snapped shared point";
  (match Geometry.find_group ~owner:Group.Primitive "clipped" output with
   | Some group -> check (Group.cardinality group = 1)
       "selected Clip clipped-group membership"
   | None -> fail "selected Clip clipped group missing");
  (match Geometry.find_group ~owner:Group.Primitive "above" output with
   | Some group -> check (Group.cardinality group = 1)
       "selected Clip included an unselected primitive in the above group"
   | None -> fail "selected Clip above group missing");
  let existing = Group.init ~owner:Group.Primitive ~name:"clipped" 2
      (fun primitive -> primitive = 1) in
  let source_with_existing = Geometry.with_group existing source |> get_string_ok in
  let unioned = Plane_clip_front.clip ~grain:1 ~snapping_tolerance:0.001
      ~selection:(Element_selection.Selected_primitives selected)
      ~replace_existing_groups:false ~clipped_group:"clipped"
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source_with_existing |> get_ok in
  (match Geometry.find_group ~owner:Group.Primitive "clipped" unioned with
   | Some group -> check (Group.cardinality group = 2)
       "Clip Replace Existing=false did not union primitive membership"
   | None -> fail "unioned Clip primitive group missing");
  let split = Plane_clip_front.clip ~grain:1 ~keep:Plane_clip.All ~split_connectivity:true
      ~snapping_tolerance:0.001 ~selection:(Element_selection.Selected_primitives selected)
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  let split_ids = int_attribute ~owner:Attribute.Primitive "primitive_id" split
  and split_topology = Topology.View.view (Geometry.topology split) in
  let selected_points = Array.make (Geometry.point_count split) false
  and unselected_points = Array.make (Geometry.point_count split) false in
  Array.iteri (fun primitive id ->
    let target = if id = 10 then selected_points else unselected_points in
    for vertex = split_topology.primitive_offsets.(primitive)
        to split_topology.primitive_offsets.(primitive + 1) - 1 do
      target.(split_topology.vertex_points.(vertex)) <- true
    done) split_ids;
  check (not (Array.exists Fun.id (Array.mapi (fun point selected ->
      selected && unselected_points.(point)) selected_points)))
    "selected keep-all Clip shared a point with unselected topology"

let clipped_count selection source =
  let output = Plane_clip_front.clip ~grain:1 ~selection ~clipped_group:"clipped"
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  match Geometry.find_group ~owner:Group.Primitive "clipped" output with
  | Some group -> Group.cardinality group
  | None -> fail "typed selected Clip missing clipped group"

let check_typed_selection_promotion () =
  let source = shared_selection_fixture () in
  let unrestricted = Plane_clip_front.clip ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source
      |> get_ok in
  let points = Group.init ~owner:Group.Point ~name:"shared_point" 4
      (fun point -> point = 1) in
  let point_output = Plane_clip_front.clip ~selection:(Element_selection.Selected_points points)
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  check (equal_geometry unrestricted point_output)
    "point-selected Clip did not promote to every incident primitive";
  let vertices = Group.init ~owner:Group.Vertex ~name:"first_corner" 6
      (fun vertex -> vertex = 0) in
  check (clipped_count (Element_selection.Selected_vertices vertices) source = 1)
    "vertex-selected Clip did not promote to its incident primitive";
  let index = Topology_index.create (Geometry.topology source) in
  let shared_edge = Topology_index.find_edge_index index ~a:1 ~b:2
  and boundary_edge = Topology_index.find_edge_index index ~a:0 ~b:1 in
  let shared = Edge_group.init ~topology:(Geometry.topology source) ~index
      ~name:"shared" (fun edge -> edge = shared_edge)
  and boundary = Edge_group.init ~topology:(Geometry.topology source) ~index
      ~name:"boundary" (fun edge -> edge = boundary_edge) in
  check (clipped_count (Element_selection.Selected_edges shared) source = 2)
    "shared-edge Clip did not promote both incident primitives";
  check (clipped_count (Element_selection.Selected_edges boundary) source = 1)
    "boundary-edge Clip did not promote its incident primitive";
  let foreign = Box_generator.box ~connectivity:Box_generator.Box_quads ~consolidate_points:true
      ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_ok in
  let foreign_index = Topology_index.create (Geometry.topology foreign) in
  let foreign_edges = Edge_group.init ~topology:(Geometry.topology foreign)
      ~index:foreign_index ~name:"foreign" (fun edge -> edge = 0) in
  expect_code "invalid_geometry" (Plane_clip_front.clip
      ~selection:(Element_selection.Selected_edges foreign_edges) ~origin:Rays_math.Vec3.zero
      ~normal:Rays_math.Vec3.unit_x source)

let check_selected_free_points () =
  let source = Line_geometry.points [|(-2.,0.,0.); (-1.,0.,0.); (1.,0.,0.)|]
      |> with_attribute "id" (Attribute.Int [|10; 20; 30|]) in
  let selected = Group.init ~owner:Group.Point ~name:"selected_free" 3
      (fun point -> point = 0) in
  let output = Plane_clip_front.clip ~selection:(Element_selection.Selected_points selected)
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  check (Geometry.point_count output = 2
      && int_attribute ~owner:Attribute.Point "id" output = [|20; 30|])
    "point-selected Clip did not restrict free-point filtering"

let check_selected_caps_and_edge_output () =
  let box center = Box_generator.box ~connectivity:Box_generator.Box_quads ~consolidate_points:true
      ~center ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_ok in
  let first = box Rays_math.Vec3.zero and second = box (Rays_math.Vec3.create 5. 0. 0.) in
  let first_primitives = Geometry.primitive_count first in
  let source = Mesh_merge.run [first; second] |> get_ok in
  let selected = Group.init ~owner:Group.Primitive ~name:"first_box"
      (Geometry.primitive_count source)
      (fun primitive -> primitive < first_primitives) in
  let run domains = Parallel.run ~domains (fun () ->
      Plane_clip_front.clip ~grain:1 ~fill:true
        ~selection:(Element_selection.Selected_primitives selected) ~cap_group:"caps"
        ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_y source |> get_ok) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four)
    "selected filled Clip differs across domain counts";
  let bounds = Analysis_front.bounds one |> Option.get in
  check (near bounds.min.y (-1.) && near bounds.max.y 1.)
    "selected filled Clip changed the unselected closed component";
  (match Geometry.find_group ~owner:Group.Primitive "caps" one with
   | Some group -> check (Group.cardinality group = 1)
       "selected filled Clip cap count"
   | None -> fail "selected filled Clip cap group missing");
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|(-1.); 1.; 1.; 0.; 0.; 0.|]
      ~y:[|0.; 0.; 1.; 2.; 3.; 2.|] ~z:[|0.; 0.; 0.; 0.; 0.; 1.|] in
  let topology = Rdk_test_support.Tb.create ~point_count:6 () in
  Rdk_test_support.Tb.add_triangle topology 0 1 2;
  Rdk_test_support.Tb.add_triangle topology 3 4 5;
  let source = Geometry.create ~positions
      ~topology:(Rdk_test_support.Tb.freeze topology) () |> get_string_ok in
  let selected = Group.init ~owner:Group.Primitive ~name:"first" 2
      (fun primitive -> primitive = 0) in
  let output = Plane_clip_front.clip ~selection:(Element_selection.Selected_primitives selected)
      ~clipped_edge_group:"plane_edges" ~origin:Rays_math.Vec3.zero
      ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  (match Geometry.find_edge_group "plane_edges" output with
   | Some group -> check (Edge_group.cardinality group = 1)
       "selected Clip included an unselected coplanar edge"
   | None -> fail "selected Clip edge output missing")

let check_degenerate_fallback () =
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 1.; 0.|] ~y:[|0.; 0.; 1.; 1.|]
      ~z:[|0.; 0.; 0.; 0.|] in
  let topology = Topology.create_owned ~point_count:4
      ~vertex_points:[|0; 1; 1; 2; 3|] ~primitive_offsets:[|0; 5|]
      ~primitive_kinds:[|Topology.Polygon|] |> get_string_ok in
  let source = Geometry.create ~positions ~topology () |> get_string_ok in
  let output = Plane_clip_front.clip ~origin:(Rays_math.Vec3.create (-10.) 0. 0.)
      ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  check (Geometry.primitive_count output = 1
      && Geometry.vertex_count output = 4)
    "adjacent repeated-corner polygon did not retain duplicate suppression";
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|1.; (-1.); 1.|] ~y:[|0.; 0.; 1.|] ~z:[|0.; 0.; 0.|] in
  let topology = Topology.create_owned ~point_count:3
      ~vertex_points:[|0; 1; 0; 2|] ~primitive_offsets:[|0; 4|]
      ~primitive_kinds:[|Topology.Polygon|] |> get_string_ok in
  let source = Geometry.create ~positions ~topology () |> get_string_ok in
  let output = Plane_clip_front.clip ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  check (Geometry.primitive_count output = 1
      && Geometry.vertex_count output = 4)
    "non-adjacent repeated-corner polygon bypassed general duplicate suppression"

let check_disconnected_concave_fragments () =
  (* The positive-X half of this simple C polygon has two disconnected
     rectangular components. A one-ring half-plane pass must not join them by
     artificial plane edges or require callers to triangulate first. *)
  let positions = Packed.Float3.View.of_owned_exn
      ~x:[|(-2.); 2.; 2.; (-1.); (-1.); 2.; 2.; (-2.)|]
      ~y:[|(-2.); (-2.); (-1.); (-1.); 1.; 1.; 2.; 2.|]
      ~z:(Array.make 8 0.) in
  let topology = Rdk_test_support.Tb.create ~point_count:8 () in
  Rdk_test_support.Tb.add_polygon topology [|0; 1; 2; 3; 4; 5; 6; 7|];
  let source = Geometry.create ~positions
      ~topology:(Rdk_test_support.Tb.freeze topology)
      ~attributes:[Attribute.create_owned ~name:"source_primitive"
        ~owner:Attribute.Primitive (Attribute.Int [|17|]) |> get_string_ok] ()
      |> get_string_ok in
  let run domains = Parallel.run ~domains (fun () ->
      Plane_clip_front.clip ~grain:1 ~clipped_group:"clipped"
        ~clipped_edge_group:"cut_edges" ~origin:Rays_math.Vec3.zero
        ~normal:Rays_math.Vec3.unit_x source |> get_ok) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four)
    "disconnected concave Clip differs across domain counts";
  check (Geometry.primitive_count one = 2
      && Geometry.vertex_count one = 8)
    "concave Clip did not emit two independent rectangular fragments";
  check (int_attribute ~owner:Attribute.Primitive "source_primitive" one
      = [|17; 17|])
    "concave Clip did not preserve primitive payload ancestry";
  let topology = Topology.View.view (Geometry.topology one)
  and positions = Packed.Float3.View.view (Geometry.positions one) in
  for primitive = 0 to Geometry.primitive_count one - 1 do
    check (topology.primitive_offsets.(primitive + 1)
        - topology.primitive_offsets.(primitive) = 4)
      "concave Clip fragment is not a quadrilateral";
    for vertex = topology.primitive_offsets.(primitive)
        to topology.primitive_offsets.(primitive + 1) - 1 do
      check (positions.x.(topology.vertex_points.(vertex)) >= 0.)
        "concave Clip fragment contains a rejected point"
    done
  done;
  (match Geometry.find_group ~owner:Group.Primitive "clipped" one with
   | Some group -> check (Group.cardinality group = 2)
       "concave Clip output group omitted a fragment"
   | None -> fail "concave Clip output group missing");
  (match Geometry.find_edge_group "cut_edges" one with
   | Some group -> check (Edge_group.cardinality group = 2)
       "concave Clip did not emit one cut edge per fragment"
   | None -> fail "concave Clip cut-edge group missing");
  let both = Plane_clip_front.clip ~grain:1 ~keep:Plane_clip.All ~origin:Rays_math.Vec3.zero
      ~normal:Rays_math.Vec3.unit_x source |> get_ok in
  check (Geometry.primitive_count both = 3)
    "keep-all concave Clip did not emit two above and one below fragment";
  let quad_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 2.; 1.; 2.|] ~y:[|1.; 0.; 1.; 2.|]
      ~z:(Array.make 4 0.) in
  let quad_topology = Rdk_test_support.Tb.create ~point_count:4 () in
  Rdk_test_support.Tb.add_polygon quad_topology [|0; 1; 2; 3|];
  let quad = Geometry.create ~positions:quad_positions
      ~topology:(Rdk_test_support.Tb.freeze quad_topology) () |> get_string_ok in
  let quad = Plane_clip_front.clip ~grain:1 ~origin:(Rays_math.Vec3.create 1.5 0. 0.)
      ~normal:Rays_math.Vec3.unit_x quad |> get_ok in
  check (Geometry.primitive_count quad = 2 && Geometry.vertex_count quad = 6)
    "concave quadrilateral did not leave the exact-size fast plan";
  let solid = Poly_extrude.run ~distance:1. source |> get_ok in
  let filled = Plane_clip_front.clip ~grain:1 ~fill:true ~cap_group:"caps"
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x solid |> get_ok in
  match Geometry.find_group ~owner:Group.Primitive "caps" filled with
  | Some group -> check (Group.cardinality group = 2)
      "filled concave Clip did not cap both disconnected fragments"
  | None -> fail "filled concave Clip cap group missing"

let hollow_square_prism () =
  let x = Array.make 16 0. and y = Array.make 16 0.
  and z = Array.make 16 0. in
  let outer = [|(-2., -2.); (2., -2.); (2., 2.); (-2., 2.)|]
  and inner = [|(-1., -1.); (1., -1.); (1., 1.); (-1., 1.)|] in
  for side = 0 to 1 do
    let px = if side = 0 then -1. else 1. in
    for corner = 0 to 3 do
      let outer_point = (side * 4) + corner
      and inner_point = 8 + (side * 4) + corner in
      x.(outer_point) <- px; x.(inner_point) <- px;
      y.(outer_point) <- fst outer.(corner);
      z.(outer_point) <- snd outer.(corner);
      y.(inner_point) <- fst inner.(corner);
      z.(inner_point) <- snd inner.(corner)
    done
  done;
  let topology = Rdk_test_support.Tb.create ~point_count:16 () in
  for corner = 0 to 3 do
    let next = (corner + 1) mod 4 in
    Rdk_test_support.Tb.add_polygon topology
      [|corner; next; 4 + next; 4 + corner|];
    Rdk_test_support.Tb.add_polygon topology
      [|8 + corner; 12 + corner; 12 + next; 8 + next|];
    Rdk_test_support.Tb.add_polygon topology
      [|corner; 8 + corner; 8 + next; next|];
    Rdk_test_support.Tb.add_polygon topology
      [|4 + corner; 4 + next; 12 + next; 12 + corner|]
  done;
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z)
    ~topology:(Rdk_test_support.Tb.freeze topology) () |> get_string_ok

let check_nested_cap_contours () =
  let source = hollow_square_prism () in
  let run domains = Parallel.run ~domains (fun () ->
      Plane_clip_front.clip ~grain:1 ~fill:true ~cap_group:"annulus_caps"
        ~clipped_edge_group:"annulus_edges"
        ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source |> get_ok) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four)
    "nested-contour Clip differs across domain counts";
  let caps = Geometry.find_group ~owner:Group.Primitive "annulus_caps" one
      |> Option.get in
  check (Group.cardinality caps = 8)
    "square-annulus Clip cap triangle count";
  let cap_edges = Geometry.find_edge_group "annulus_edges" one |> Option.get in
  check (Edge_group.cardinality cap_edges = 16)
    "square-annulus Clip did not materialize boundary and internal plane edges";
  let topology = Topology.View.view (Geometry.topology one)
  and positions = Packed.Float3.View.view (Geometry.positions one) in
  let normals = match Geometry.find_attribute ~owner:Attribute.Vertex "N" one with
    | Some attribute ->
        (match Attribute.View.storage attribute with
         | Attribute.Float3 values -> Packed.Float3.View.view values
         | _ -> fail "nested Clip cap N has unexpected storage")
    | None -> fail "nested Clip cap did not create hard vertex N" in
  let area = ref 0. in
  Group.iter (fun primitive ->
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    check (last - first = 3) "nested Clip cap is not triangulated";
    let point local = topology.vertex_points.(first + local) in
    let a = point 0 and b = point 1 and c = point 2 in
    for vertex = first to last - 1 do
      check (near normals.x.(vertex) (-1.) && near normals.y.(vertex) 0.
          && near normals.z.(vertex) 0.)
        "nested Clip cap normal does not face outward"
    done;
    let aby = positions.y.(b) -. positions.y.(a)
    and abz = positions.z.(b) -. positions.z.(a)
    and acy = positions.y.(c) -. positions.y.(a)
    and acz = positions.z.(c) -. positions.z.(a) in
    area := !area +. (abs_float ((aby *. acz) -. (abz *. acy)) *. 0.5);
    let cy = (positions.y.(a) +. positions.y.(b) +. positions.y.(c)) /. 3.
    and cz = (positions.z.(a) +. positions.z.(b) +. positions.z.(c)) /. 3. in
    check (abs_float cy >= 1. || abs_float cz >= 1.)
      "nested Clip triangulation sealed the interior hole") caps;
  check (near !area 12.) "nested Clip cap area does not preserve the hole";
  let index = Topology_index.create (Geometry.topology one) in
  check (Topology_index_front.boundary_edge_count index = 0
      && Topology_index_front.non_manifold_edge_count index = 0)
    "nested Clip cap is not a closed two-manifold";
  let center = Rays_math.Vec3.create 1e150 (-1e150) 5e149 and scale = 1e140 in
  let transformed = Transform_ops_front.transform
      (Rays_math.Mat4.mul (Rays_math.Mat4.translation center)
        (Rays_math.Mat4.scaling (Rays_math.Vec3.create scale scale scale))) source in
  let transformed = Plane_clip_front.clip ~grain:1 ~fill:true ~cap_group:"caps"
      ~origin:center ~normal:Rays_math.Vec3.unit_x transformed |> get_ok in
  let transformed_caps = Geometry.find_group ~owner:Group.Primitive "caps"
      transformed |> Option.get in
  check (Group.cardinality transformed_caps = 8)
    "nested Clip lost scale robustness at large finite coordinates";
  let components = Array.init 8 (fun component ->
      Transform_ops_front.transform (Rays_math.Mat4.translation
        (Rays_math.Vec3.create 0. ((float_of_int component -. 3.5) *. 6.) 0.))
        (hollow_square_prism ())) in
  let components = Mesh_merge.run (Array.to_list components) |> get_ok in
  let run_components domains = Parallel.run ~domains (fun () ->
      Plane_clip_front.clip ~grain:1 ~fill:true ~cap_group:"caps" ~origin:Rays_math.Vec3.zero
        ~normal:Rays_math.Vec3.unit_x components |> get_ok) in
  let component_one = run_components 1 and component_four = run_components 4 in
  check (equal_geometry component_one component_four)
    "parallel independent nested cap components differ across domain counts";
  let component_caps = Geometry.find_group ~owner:Group.Primitive "caps"
      component_one |> Option.get in
  check (Group.cardinality component_caps = 64)
    "parallel independent nested cap component cardinality"

let cap_area_yz geometry group =
  let topology = Topology.View.view (Geometry.topology geometry)
  and positions = Packed.Float3.View.view (Geometry.positions geometry)
  and total = ref 0. in
  Group.iter (fun primitive ->
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1)
    and twice_area = ref 0. in
    for vertex = first to last - 1 do
      let next = if vertex + 1 = last then first else vertex + 1 in
      let a = topology.vertex_points.(vertex)
      and b = topology.vertex_points.(next) in
      twice_area := !twice_area
          +. ((positions.y.(a) *. positions.z.(b))
             -. (positions.y.(b) *. positions.z.(a)))
    done;
    total := !total +. (abs_float !twice_area *. 0.5)) group;
  !total

let check_multiple_nested_cap_contours () =
  let box ?(center = Rays_math.Vec3.zero) y z =
    Box_generator.box ~connectivity:Box_generator.Box_quads ~consolidate_points:true
      ~normals:Box_generator.Box_no_normals ~center
      ~size:(Rays_math.Vec3.create 2. y z) () |> get_ok in
  let outer = box 6. 4.
  and first_hole = box ~center:(Rays_math.Vec3.create 0. (-1.5) 0.) 1. 1.
      |> Reverse_faces.run |> get_ok
  and second_hole = box ~center:(Rays_math.Vec3.create 0. 1.5 0.) 1. 1.
      |> Reverse_faces.run |> get_ok in
  let source = Mesh_merge.run [outer; first_hole; second_hole] |> get_ok in
  let run domains keep = Parallel.run ~domains (fun () ->
      Plane_clip_front.clip ~grain:1 ~keep ~fill:true ~cap_group:"caps"
        ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source |> get_ok) in
  let one = run 1 Plane_clip.Above and four = run 4 Plane_clip.Above in
  check (equal_geometry one four)
    "multiple-hole Clip differs across domain counts";
  let caps = Geometry.find_group ~owner:Group.Primitive "caps" one
      |> Option.get in
  check (Group.cardinality caps = 14)
    "multiple-hole Clip triangle cardinality";
  check (near (cap_area_yz one caps) 22.)
    "multiple-hole Clip cap area";
  let both = run 4 Plane_clip.All in
  let both_caps = Geometry.find_group ~owner:Group.Primitive "caps" both
      |> Option.get in
  check (Group.cardinality both_caps = 28
      && near (cap_area_yz both both_caps) 44.)
    "keep-all multiple-hole Clip did not cap both sides";
  let middle = box 4. 4. |> Reverse_faces.run |> get_ok
  and island = box 2. 2. in
  let nested = Mesh_merge.run [box 6. 6.; middle; island] |> get_ok
      |> Plane_clip_front.clip ~grain:1 ~fill:true ~cap_group:"caps"
           ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x |> get_ok in
  let nested_caps = Geometry.find_group ~owner:Group.Primitive "caps" nested
      |> Option.get in
  check (Group.cardinality nested_caps = 9
      && near (cap_area_yz nested nested_caps) 24.)
    "depth-two nested Clip did not preserve the interior island";
  let nested_solids = Mesh_merge.run [box 6. 6.; box 2. 2.] |> get_ok
      |> Plane_clip_front.clip ~grain:1 ~fill:true ~cap_group:"caps"
           ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x |> get_ok in
  let solid_caps = Geometry.find_group ~owner:Group.Primitive "caps"
      nested_solids |> Option.get in
  check (Group.cardinality solid_caps = 2
      && near (cap_area_yz nested_solids solid_caps) 40.)
    "same-winding nested solids were incorrectly interpreted as a cavity";
  let concave_positions = Packed.Float3.View.of_owned_exn
      ~x:[|(-2.); 2.; 2.; (-1.); (-1.); 2.; 2.; (-2.)|]
      ~y:[|(-2.); (-2.); (-1.); (-1.); 1.; 1.; 2.; 2.|]
      ~z:(Array.make 8 0.) in
  let concave_topology = Rdk_test_support.Tb.create ~point_count:8 () in
  Rdk_test_support.Tb.add_polygon concave_topology [|0;1;2;3;4;5;6;7|];
  let concave = Geometry.create ~positions:concave_positions
      ~topology:(Rdk_test_support.Tb.freeze concave_topology) () |> get_string_ok
      |> Poly_extrude.run ~distance:1. |> get_ok in
  let concave_hole = Box_generator.box ~connectivity:Box_generator.Box_quads
      ~consolidate_points:true ~normals:Box_generator.Box_no_normals
      ~center:(Rays_math.Vec3.create (-1.5) 0. 0.5)
      ~size:(Rays_math.Vec3.create 0.5 0.5 1.) () |> get_ok
      |> Reverse_faces.run |> get_ok in
  let concave_nested = Mesh_merge.run [concave; concave_hole] |> get_ok
      |> Plane_clip_front.clip ~grain:1 ~fill:true ~cap_group:"caps"
           ~origin:(Rays_math.Vec3.create 0. 0. 0.5) ~normal:Rays_math.Vec3.unit_z |> get_ok in
  let concave_caps = Geometry.find_group ~owner:Group.Primitive "caps"
      concave_nested |> Option.get in
  check (Group.cardinality concave_caps = 12)
    "concave outer contour with a hole triangle cardinality";
  let concave_index = Topology_index.create (Geometry.topology concave_nested) in
  check (Topology_index_front.boundary_edge_count concave_index = 0
      && Topology_index_front.non_manifold_edge_count concave_index = 0)
    "concave outer contour with a hole is not a closed two-manifold";
  let overlap_a = box ~center:(Rays_math.Vec3.create 0. (-0.3) 0.) 2. 2.
      |> Reverse_faces.run |> get_ok
  and overlap_b = box ~center:(Rays_math.Vec3.create 0. 0.3 0.) 2. 2.
      |> Reverse_faces.run |> get_ok in
  let overlapping = Mesh_merge.run [outer; overlap_a; overlap_b] |> get_ok in
  expect_code "invalid_geometry" (Plane_clip_front.clip ~grain:1 ~fill:true
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x overlapping);
  let same_winding_overlap = Mesh_merge.run [outer;
      box ~center:(Rays_math.Vec3.create 0. (-0.3) 0.) 2. 2.;
      box ~center:(Rays_math.Vec3.create 0. 0.3 0.) 2. 2.] |> get_ok in
  expect_code "invalid_geometry" (Plane_clip_front.clip ~grain:1 ~fill:true
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x same_winding_overlap)

let check_validation () =
  let source = triangle () in
  expect_code "invalid_geometry" (Plane_clip_front.clip ~clip_attribute:""
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source);
  expect_code "invalid_geometry" (Plane_clip_front.clip ~clip_attribute:"missing"
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source);
  expect_code "invalid_geometry" (Plane_clip_front.clip ~distance:Float.nan
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source);
  expect_code "invalid_geometry" (Plane_clip_front.clip ~clipped_edge_group:" "
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x source);
  let text = with_attribute "field" (Attribute.Text [|"a"; "b"; "c"|]) source in
  expect_code "invalid_geometry" (Plane_clip_front.clip ~clip_attribute:"field"
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x text);
  let non_finite = with_attribute "field"
      (Attribute.Float [|(-1.); Float.infinity; 1.|]) source in
  expect_code "invalid_geometry" (Plane_clip_front.clip ~clip_attribute:"field"
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x non_finite)

let run () =
  check_custom_clip_attribute ();
  check_distance ();
  check_selection_isolation ();
  check_typed_selection_promotion ();
  check_selected_free_points ();
  check_selected_caps_and_edge_output ();
  check_degenerate_fallback ();
  check_disconnected_concave_fragments ();
  check_nested_cap_contours ();
  check_multiple_nested_cap_contours ();
  check_validation ();
  print_endline "clip tests passed"
