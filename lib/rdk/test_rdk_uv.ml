open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error _ -> fail "unexpected error"

let render_uv ~geometry ~positions ~selected ~shared_box ~topology ~triangle_geometry =
  let mesh = Rdk_rays.Rays_mesh.to_mesh triangle_geometry |> get_ok in
  if Mesh.vertex_count mesh <> 4 || Mesh.index_count mesh <> 6 then
    fail "mesh bridge cardinality";
  let source_positions = Packed.Float3.Private.view positions
  and source_topology = Topology.Private.view (Geometry.topology triangle_geometry)
  and mesh_view = Mesh.Private.packed_view mesh in
  if source_positions.x != mesh_view.vertices.x
     || source_positions.y != mesh_view.vertices.y
     || source_positions.z != mesh_view.vertices.z
     || source_topology.vertex_points != mesh_view.indices then
    fail "mesh bridge copied packed render buffers";
  let roundtrip = Rdk_rays.Rays_mesh.of_mesh mesh |> get_ok in
  if not (equal_positions triangle_geometry roundtrip) then fail "mesh bridge positions";
  (* Point Cd runs from [low] at the lowest Y to [high] at the highest. *)
  let low = 1., 0., 0., 1. and high = 0., 0., 1., 1. in
  let color_planes geometry =
    Geometry.find_attribute ~owner:Attribute.Point "Cd" geometry
    |> Option.get |> Attribute.get (Attribute.color ~owner:Attribute.Point)
    |> Option.get |> Packed.Float4.Private.view in
  let colored domains = Parallel.run ~domains (fun () ->
    Color_by_height.run ~grain:1 ~low ~high triangle_geometry |> get_ok) in
  let one = color_planes (colored 1) and many = color_planes (colored 4) in
  if one.x <> many.x || one.y <> many.y || one.z <> many.z
     || one.w <> many.w then fail "color_by_height domain mismatch";
  let bottom = Array.fold_left Float.min infinity source_positions.y
  and top = Array.fold_left Float.max neg_infinity source_positions.y in
  if not (top > bottom) then fail "color_by_height fixture has no Y extent";
  Array.iteri (fun point y ->
    let t = (y -. bottom) /. (top -. bottom) in
    if abs_float (one.x.(point) -. (1. -. t)) > 1e-12 || one.y.(point) <> 0.
       || abs_float (one.z.(point) -. t) > 1e-12 || one.w.(point) <> 1. then
      fail "color_by_height is not the normalized Y ramp") source_positions.y;
  let cancelled_color = Cancel.create () in
  Cancel.cancel cancelled_color;
  (match Color_by_height.run ~cancel:cancelled_color ~low ~high triangle_geometry with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "color_by_height cancellation");
  let vertex_colors =
    let values = Packed.Float4.of_owned
        ~x:[|1.; 0.; 0.; 0.; 1.; 1.|]
        ~y:[|0.; 1.; 0.; 0.; 1.; 0.|]
        ~z:[|0.; 0.; 1.; 1.; 0.; 1.|]
        ~w:(Array.make 6 1.) |> get_ok in
    Attribute.create_key_owned (Attribute.color ~owner:Attribute.Vertex) values
    |> get_ok in
  let seamed = Geometry.with_attribute vertex_colors triangle_geometry |> get_ok in
  let seamed_mesh = Rdk_rays.Rays_mesh.to_mesh seamed |> get_ok in
  if Mesh.vertex_count seamed_mesh <> 6 || Mesh.index_count seamed_mesh <> 6
  then fail "vertex attribute seam was not expanded";
  let diagonal = 1. /. sqrt 2. in
  let vertex_normals = Packed.Float3.Private.of_owned_exn
      ~x:(Array.make 6 diagonal) ~y:(Array.make 6 diagonal) ~z:(Array.make 6 0.) in
  let vertex_normal = Attribute.create_key_owned
      (Attribute.normal ~owner:Attribute.Vertex) vertex_normals |> get_ok in
  let with_vertex_normals = Geometry.with_attribute vertex_normal triangle_geometry |> get_ok in
  let scaled = Transform_ops.transform (Rays_math.Mat4.scaling (Rays_math.Vec3.create 2. 1. 1.))
      with_vertex_normals in
  let scaled_n = Geometry.find_attribute ~owner:Attribute.Vertex "N" scaled
      |> Option.get |> Attribute.get (Attribute.normal ~owner:Attribute.Vertex)
      |> Option.get in
  let nx, ny, _ = Packed.Float3.get scaled_n 0 in
  if abs_float (nx -. 0.4472135955) > 1e-9
     || abs_float (ny -. 0.8944271910) > 1e-9
  then fail "vertex normal inverse-transpose";
  let grid = Plane_generators.grid ~columns:8 ~rows:4 ~size:2. () |> get_ok in
  if Geometry.point_count grid <> 45 || Geometry.primitive_count grid <> 64
  then fail "grid cardinality";
  let box = Box_generator.box ~size:(Rays_math.Vec3.create 2. 4. 6.) () |> get_ok in
  if Geometry.point_count box <> 24 || Geometry.primitive_count box <> 12
  then fail "box cardinality";
  let sphere = Uv_sphere.run ~segments:12 ~rings:6 ~radius:2. () |> get_ok in
  if Geometry.point_count sphere <> 62 || Geometry.primitive_count sphere <> 120
  then fail "UV sphere cardinality";
  if Mesh.index_count (Rdk_rays.Rays_mesh.to_mesh sphere |> get_ok) <> 360
  then fail "UV sphere bridge";
  let circle = Plane_generators.circle ~segments:18 ~radius:2. () |> get_ok in
  let circle_mesh = Rdk_rays.Rays_mesh.to_mesh circle |> get_ok in
  if Mesh.mode circle_mesh <> Mesh.Lines || Mesh.index_count circle_mesh <> 36
  then fail "closed curve bridge";
  let bent = Line_geometry.polyline [|(0.,0.,0.); (1.,0.,0.); (1.,3.,0.)|] |> get_ok in
  let distance_attribute = Attribute.create_owned ~name:"distance"
      ~owner:Attribute.Point (Attribute.Float [|0.; 1.; 4.|]) |> get_ok in
  let bent = Geometry.with_attribute distance_attribute bent |> get_ok in
  (match Attribute.storage distance_attribute with
   | Attribute.Float values -> values.(0) <- 99.
   | _ -> assert false);
  let distance_key = Attribute.key ~name:"distance" ~owner:Attribute.Point
      Attribute.float in
  let borrowed_copy = Attribute.get distance_key distance_attribute |> Option.get in
  borrowed_copy.(1) <- 99.;
  (match Attribute.storage distance_attribute with
   | Attribute.Float values when values = [|0.; 1.; 4.|] -> ()
   | _ -> fail "public attribute access leaked mutable storage");
  let resampled = Resample_curves.run ~segments:4 bent |> get_ok in
  if Geometry.point_count resampled <> 5
     || Topology.primitive_kind (Geometry.topology resampled) 0
        <> Topology.Open_polyline
  then fail "curve resample cardinality/kind";
  let resampled_positions = Geometry.positions resampled in
  let x2, y2, _ = Packed.Float3.get resampled_positions 2
  and _, y4, _ = Packed.Float3.get resampled_positions 4 in
  if x2 <> 1. || y2 <> 1. || y4 <> 3. then fail "curve arc-length resample";
  (match Geometry.find_attribute ~owner:Attribute.Point "distance" resampled with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Float values when values = [|0.; 1.; 2.; 3.; 4.|] -> ()
        | _ -> fail "curve attribute interpolation")
   | None -> fail "curve resample dropped point attribute");
  let swept = Sweep_circle.run ~sides:8 ~radius:0.2 resampled |> get_ok in
  if Geometry.point_count swept <> 40 || Geometry.primitive_count swept <> 32
     || Geometry.vertex_count swept <> 128
  then fail "circle sweep cardinality";
  if Geometry.find_attribute ~owner:Attribute.Point "N" swept = None
     || Geometry.find_attribute ~owner:Attribute.Vertex "uv" swept = None
     || Geometry.find_attribute ~owner:Attribute.Point "distance" swept = None
  then fail "circle sweep attributes";
  if Mesh.index_count (Rdk_rays.Rays_mesh.to_mesh swept |> get_ok) <> 192
  then fail "circle sweep bridge";
  let planar_projection = Uv_ops.Planar {
      origin = Rays_math.Vec3.create 0.5 0.5 0.;
      u_axis = Rays_math.Vec3.unit_x;
      v_axis = Rays_math.Vec3.unit_y;
    } in
  let projected_quad = Uv_ops.project ~grain:1 planar_projection geometry
      |> get_ok in
  let projected_uv = Geometry.find_attribute ~owner:Attribute.Vertex "uv"
      projected_quad |> Option.get
      |> Attribute.get (Attribute.tex_coord ~owner:Attribute.Vertex)
      |> Option.get |> Packed.Float2.Private.view in
  if projected_uv.x <> [|0.; 1.; 1.; 0.|]
     || projected_uv.y <> [|0.; 0.; 1.; 1.|]
  then fail "planar UV projection coordinates";
  if Geometry.payload_bytes projected_quad - Geometry.payload_bytes geometry
     <> Geometry.vertex_count geometry * 16
  then fail "UV projection payload is not exact per-corner float2 storage";
  if Geometry.positions projected_quad != Geometry.positions geometry
     || Geometry.topology projected_quad != Geometry.topology geometry
  then fail "UV projection copied unchanged geometry payload";
  let skewed_quad = Uv_ops.project
      (Uv_ops.Planar { origin = Rays_math.Vec3.zero; u_axis = Rays_math.Vec3.unit_x;
        v_axis = Rays_math.Vec3.create 0.5 1. 0. }) geometry |> get_ok in
  let skewed_uv = Geometry.find_attribute ~owner:Attribute.Vertex "uv"
      skewed_quad |> Option.get
      |> Attribute.get (Attribute.tex_coord ~owner:Attribute.Vertex)
      |> Option.get |> Packed.Float2.Private.view in
  let nearly_array left right =
    Array.length left = Array.length right
    && Array.for_all2 (fun left right -> abs_float (left -. right) < 1e-12)
         left right in
  if not (nearly_array skewed_uv.x [|0.5; 1.5; 1.; 0.|])
     || not (nearly_array skewed_uv.y [|0.5; 0.5; 1.5; 1.5|])
  then fail "skewed planar UV frame solve";
  let existing_uv = Attribute.create_owned ~name:"uv" ~owner:Attribute.Vertex
      (Attribute.Float2 (Packed.Float2.of_owned
        ~x:(Array.make 6 0.25) ~y:(Array.make 6 0.25) |> get_ok)) |> get_ok in
  let restricted_source = Geometry.with_attribute existing_uv triangle_geometry
      |> get_ok in
  let first_face = Group.init ~owner:Group.Primitive ~name:"first_face" 2
      (fun primitive -> primitive = 0) in
  let restricted = Uv_ops.project ~grain:1 ~primitives:first_face
      planar_projection restricted_source |> get_ok in
  let restricted_uv = Geometry.find_attribute ~owner:Attribute.Vertex "uv"
      restricted |> Option.get
      |> Attribute.get (Attribute.tex_coord ~owner:Attribute.Vertex)
      |> Option.get |> Packed.Float2.Private.view in
  if restricted_uv.x <> [|0.; 1.; 1.; 0.25; 0.25; 0.25|]
     || restricted_uv.y <> [|0.; 0.; 1.; 0.25; 0.25; 0.25|]
  then fail "primitive-restricted UV projection did not preserve other corners";
  let selected_uv = Group.init ~owner:Group.Vertex ~name:"selected_uv" 6
      (fun vertex -> vertex = 0 || vertex = 2) in
  let transformed_uv = Uv_ops.transform ~grain:1 ~owner:Attribute.Vertex
      ~selection:selected_uv ~pivot:Rays_math.Vec2.zero ~scale:(Rays_math.Vec2.create 2. 2.)
      ~translate:(Rays_math.Vec2.create 0.1 (-0.2)) restricted |> get_ok in
  let transformed_uv = Geometry.find_attribute ~owner:Attribute.Vertex "uv"
      transformed_uv |> Option.get
      |> Attribute.get (Attribute.tex_coord ~owner:Attribute.Vertex)
      |> Option.get |> Packed.Float2.Private.view in
  if transformed_uv.x <> [|0.1; 1.; 2.1; 0.25; 0.25; 0.25|]
     || transformed_uv.y <> [|-0.2; 0.; 1.8; 0.25; 0.25; 0.25|]
  then fail "restricted UV transform";
  let point_uv = Attribute.create_owned ~name:"point_uv"
      ~owner:Attribute.Point
      (Attribute.Float2 (Packed.Float2.of_owned
        ~x:[|0.; 1.; 1.; 0.|] ~y:[|0.; 0.; 1.; 1.|] |> get_ok)) |> get_ok in
  let point_uv_source = Geometry.with_attribute point_uv geometry |> get_ok in
  let point_uv_output = Uv_ops.transform ~name:"point_uv"
      ~owner:Attribute.Point ~selection:selected
      ~scale:(Rays_math.Vec2.create 0.5 0.5) ~pivot:Rays_math.Vec2.zero point_uv_source |> get_ok in
  let point_uv_values = Geometry.find_attribute ~owner:Attribute.Point "point_uv"
      point_uv_output |> Option.get
      |> Attribute.get (Attribute.key ~name:"point_uv" ~owner:Attribute.Point
           Attribute.float2)
      |> Option.get |> Packed.Float2.Private.view in
  if point_uv_values.x <> [|0.; 1.; 0.5; 0.|]
     || point_uv_values.y <> [|0.; 0.; 0.5; 1.|]
  then fail "point-owned restricted UV transform";
  let angle = 0.1 in
  let cylinder_curve = Line_geometry.polyline ~closed:true [|
      (cos (-.angle), 0.5, sin (-.angle));
      (cos (-.angle), -0.5, sin (-.angle));
      (cos angle, -0.5, sin angle);
      (cos angle, 0.5, sin angle);
    |] |> get_ok in
  let cylindrical = Uv_ops.project ~grain:1
      (Uv_ops.Cylindrical { origin = Rays_math.Vec3.zero; axis = Rays_math.Vec3.unit_y;
        seam = Rays_math.Vec3.unit_x; height = 1. }) cylinder_curve |> get_ok in
  let cylindrical_uv = Geometry.find_attribute ~owner:Attribute.Vertex "uv"
      cylindrical |> Option.get
      |> Attribute.get (Attribute.tex_coord ~owner:Attribute.Vertex)
      |> Option.get |> Packed.Float2.Private.view in
  let cylinder_min = Array.fold_left min infinity cylindrical_uv.x
  and cylinder_max = Array.fold_left max neg_infinity cylindrical_uv.x in
  if cylinder_max -. cylinder_min > 0.04
     || cylindrical_uv.y <> [|1.; 0.; 0.; 1.|]
  then fail "cylindrical UV seam/height projection";
  let huge_frame = Uv_ops.project
      (Uv_ops.Cylindrical { origin = Rays_math.Vec3.zero;
        axis = Rays_math.Vec3.create 0. max_float 0.;
        seam = Rays_math.Vec3.create max_float 0. 0.; height = 1. })
      cylinder_curve |> get_ok in
  let huge_frame_uv = Geometry.find_attribute ~owner:Attribute.Vertex "uv"
      huge_frame |> Option.get
      |> Attribute.get (Attribute.tex_coord ~owner:Attribute.Vertex)
      |> Option.get |> Packed.Float2.Private.view in
  if Array.exists (fun value -> not (Float.is_finite value)) huge_frame_uv.x
     || Array.exists (fun value -> not (Float.is_finite value)) huge_frame_uv.y
  then fail "UV projection did not normalize an extreme finite frame safely";
  let sphere_uv = Uv_sphere.run ~segments:48 ~rings:24 ~radius:1. () |> get_ok
      |> Uv_ops.project ~grain:31
           (Uv_ops.Spherical { origin = Rays_math.Vec3.zero; axis = Rays_math.Vec3.unit_y;
             seam = Rays_math.Vec3.unit_x }) |> get_ok in
  let sphere_values = Geometry.find_attribute ~owner:Attribute.Vertex "uv"
      sphere_uv |> Option.get
      |> Attribute.get (Attribute.tex_coord ~owner:Attribute.Vertex)
      |> Option.get |> Packed.Float2.Private.view
  and sphere_topology = Topology.Private.view (Geometry.topology sphere_uv) in
  for primitive = 0 to Geometry.primitive_count sphere_uv - 1 do
    let first = sphere_topology.primitive_offsets.(primitive)
    and last = sphere_topology.primitive_offsets.(primitive + 1) in
    let minimum = ref infinity and maximum = ref neg_infinity in
    for vertex = first to last - 1 do
      minimum := min !minimum sphere_values.x.(vertex);
      maximum := max !maximum sphere_values.x.(vertex);
      if not (Float.is_finite sphere_values.x.(vertex)
          && Float.is_finite sphere_values.y.(vertex))
         || sphere_values.y.(vertex) < 0. || sphere_values.y.(vertex) > 1.
      then fail "spherical UV produced an invalid coordinate"
    done;
    if !maximum -. !minimum > 0.500000000001 then
      fail "spherical UV primitive crosses the wrap seam"
  done;
  (match Uv_ops.project
      (Uv_ops.Planar { origin = Rays_math.Vec3.zero; u_axis = Rays_math.Vec3.unit_x;
        v_axis = Rays_math.Vec3.unit_x }) geometry with
   | Error error when Error.code error = "invalid_projection" -> ()
   | _ -> fail "UV Project accepted parallel planar axes");
  (match Uv_ops.project ~primitives:selected planar_projection geometry with
   | Error error when Error.code error = "invalid_projection" -> ()
   | _ -> fail "UV Project accepted a point-owned primitive selection");
  let wrong_uv = Attribute.create_owned ~name:"uv" ~owner:Attribute.Vertex
      (Attribute.Float (Array.make 4 0.)) |> get_ok in
  let wrong_uv_geometry = Geometry.with_attribute wrong_uv geometry |> get_ok in
  (match Uv_ops.project planar_projection wrong_uv_geometry with
   | Error error when Error.code error = "invalid_projection" -> ()
   | _ -> fail "UV Project accepted a non-float2 existing UV attribute");
  let invalid_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|nan; 1.; 1.; 0.|] ~y:[|0.; 0.; 1.; 1.|]
      ~z:[|0.; 0.; 0.; 0.|] in
  let invalid_geometry = Geometry.create ~positions:invalid_positions ~topology ()
      |> get_ok in
  (match Uv_ops.project planar_projection invalid_geometry with
   | Error error when Error.code error = "invalid_projection" -> ()
   | _ -> fail "UV Project accepted a non-finite point position");
  let cancelled_uv = Cancel.create () in
  Cancel.cancel cancelled_uv;
  (match Uv_ops.project ~cancel:cancelled_uv planar_projection geometry with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "UV Project ignored cancellation");
  let hard_seams domains = Parallel.run ~domains (fun () ->
    Uv_ops.auto_seam ~grain:1 ~angle:(Float.pi /. 4.)
      ~include_boundaries:false ~island_attribute:"uv_island" shared_box
    |> get_ok) in
  let hard_seams_one = hard_seams 1 and hard_seams_many = hard_seams 4 in
  let seam_group geometry = Geometry.find_group ~owner:Group.Vertex
      "uv_seams" geometry |> Option.get in
  let edge_seam_group geometry = Geometry.find_edge_group "uv_seams" geometry
      |> Option.get in
  let hard_one_group = seam_group hard_seams_one
  and hard_many_group = seam_group hard_seams_many
  and hard_one_edges = edge_seam_group hard_seams_one
  and hard_many_edges = edge_seam_group hard_seams_many in
  for vertex = 0 to Group.length hard_one_group - 1 do
    if Group.mem vertex hard_one_group <> Group.mem vertex hard_many_group then
      fail "UV Auto Seam group differs by domain count"
  done;
  for edge = 0 to Edge_group.length hard_one_edges - 1 do
    if Edge_group.mem edge hard_one_edges <> Edge_group.mem edge hard_many_edges then
      fail "UV Auto Seam native edge group differs by domain count"
  done;
  if Geometry.positions hard_seams_one != Geometry.positions shared_box
     || Geometry.topology hard_seams_one != Geometry.topology shared_box
  then fail "UV Auto Seam copied unchanged geometry payload";
  let sharp_edges domains = Parallel.run ~domains (fun () ->
    Group_mesh.group_edges ~grain:1 ~name:"sharp" ~incidence:Group_mesh.Manifold_edge
      ~min_angle:(Float.pi /. 4.) shared_box |> get_ok) in
  let sharp_one = sharp_edges 1 and sharp_many = sharp_edges 4 in
  let sharp_group geometry = Geometry.find_edge_group "sharp" geometry
      |> Option.get in
  let sharp_one_group = sharp_group sharp_one
  and sharp_many_group = sharp_group sharp_many in
  if Edge_group.cardinality sharp_one_group <> 12
     || Edge_group.cardinality sharp_many_group <> 12 then
    fail "Edge Group sharp cube cardinality";
  for edge = 0 to Edge_group.length sharp_one_group - 1 do
    if Edge_group.mem edge sharp_one_group <> Edge_group.mem edge sharp_many_group
    then fail "Edge Group differs by domain count"
  done;
  let boundary_edges = Group_mesh.group_edges ~name:"boundary"
      ~incidence:Group_mesh.Boundary_edge triangle_geometry |> get_ok
      |> Geometry.find_edge_group "boundary" |> Option.get in
  if Edge_group.cardinality boundary_edges <> 4 then
    fail "Edge Group boundary incidence";
  let unit_edges = Group_mesh.group_edges ~name:"unit" ~min_length:1. ~max_length:1.
      triangle_geometry |> get_ok |> Geometry.find_edge_group "unit"
      |> Option.get in
  if Edge_group.cardinality unit_edges <> 4 then
    fail "Edge Group inclusive length range";
  let selected_edges = Group_mesh.group_edges ~name:"selected_edges"
      ~primitives:first_face triangle_geometry |> get_ok
      |> Geometry.find_edge_group "selected_edges" |> Option.get in
  if Edge_group.cardinality selected_edges <> 3 then
    fail "Edge Group primitive selection";
  (match Geometry.with_edge_group sharp_one_group triangle_geometry with
   | Error _ -> ()
   | Ok _ -> fail "Geometry accepted an edge group from another topology");
  let quad_edges = Group_mesh.group_edges ~name:"quad_edges" geometry |> get_ok in
  let triangulated_edges = Triangulate.run quad_edges |> get_ok in
  let triangulated_edge_group = Geometry.find_edge_group "quad_edges"
      triangulated_edges |> Option.get in
  if Edge_group.cardinality triangulated_edge_group <> 4
     || Edge_group.length triangulated_edge_group <> 5 then
    fail "triangulate did not preserve original edges/exclude its diagonal";
  let reversed_edges = Reverse_faces.run quad_edges |> get_ok
      |> Geometry.find_edge_group "quad_edges" |> Option.get in
  if Edge_group.cardinality reversed_edges <> 4 then
    fail "reverse did not remap native edge membership";
  let quad_edge_group = Geometry.find_edge_group "quad_edges" quad_edges
      |> Option.get
  and quad_edge_index = Topology_index.create (Geometry.topology quad_edges) in
  let reversed_topology = Reverse_faces.run quad_edges |> get_ok |> Geometry.topology in
  (match Edge_group.replicate_exact_copies
      ~source_topology:(Geometry.topology quad_edges)
      ~source_index:quad_edge_index ~target_topology:reversed_topology
      ~copies:1 quad_edge_group with
   | Error _ -> ()
   | Ok _ -> fail "exact-copy edge replication accepted reordered topology");
  let mirrored_edge_geometry = Mirror_geometry.run ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x
      quad_edges |> get_ok in
  let mirrored_edge_group = Geometry.find_edge_group "quad_edges"
      mirrored_edge_geometry |> Option.get in
  if Edge_group.cardinality mirrored_edge_group <> 8
     || Edge_group.length mirrored_edge_group <> 8 then
    fail "mirror did not replicate native edge membership";
  let duplicated_edge_geometry = Instance_copy.duplicate ~copies:2 quad_edges |> get_ok in
  let duplicated_edge_group = Geometry.find_edge_group "quad_edges"
      duplicated_edge_geometry |> Option.get in
  if Edge_group.cardinality duplicated_edge_group <> 12
     || Edge_group.length duplicated_edge_group <> 12 then
    fail "duplicate did not replicate native edge membership";
  let merged_edge_geometry = Mesh_merge.run [quad_edges; quad_edges] |> get_ok in
  let merged_edge_group = Geometry.find_edge_group "quad_edges"
      merged_edge_geometry |> Option.get in
  if Edge_group.cardinality merged_edge_group <> 8
     || Edge_group.length merged_edge_group <> 8 then
    fail "merge did not concatenate native edge membership";
  let copied_edge_geometry = Instance_copy.copy_to_points ~source:quad_edges
      ~targets:(Line_geometry.points [|(0., 0., 0.); (3., 0., 0.)|]) () |> get_ok in
  let copied_edge_group = Geometry.find_edge_group "quad_edges"
      copied_edge_geometry |> Option.get in
  if Edge_group.cardinality copied_edge_group <> 8
     || Edge_group.length copied_edge_group <> 8 then
    fail "copy-to-points did not replicate native edge membership";
  let hard_box_edges = Box_generator.box ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_ok
      |> Group_mesh.group_edges ~name:"all_box_edges" |> get_ok in
  let fused_edge_geometry = Fuse_grid.fuse ~tolerance:0.
      ~attributes:Fuse_reduce.Average_numeric hard_box_edges |> get_ok in
  let fused_edge_group = Geometry.find_edge_group "all_box_edges"
      fused_edge_geometry |> Option.get in
  if Edge_group.cardinality fused_edge_group <> 18
     || Edge_group.length fused_edge_group <> 18 then
    fail "fuse did not union collapsed native edge membership";
  let subdivided_edge_geometry = Subdivide.subdivide ~scheme:Subdivide.Catmull_clark
      ~iterations:2 quad_edges |> get_ok in
  let subdivided_edge_group = Geometry.find_edge_group "quad_edges"
      subdivided_edge_geometry |> Option.get in
  if Edge_group.cardinality subdivided_edge_group <> 16 then
    fail "subdivision did not propagate selected source-edge children";
  let clipped_edge_geometry = Plane_clip.clip ~keep:Plane_clip.Above
      ~origin:(Rays_math.Vec3.create 0.5 0. 0.) ~normal:Rays_math.Vec3.unit_x quad_edges |> get_ok in
  let clipped_edge_group = Geometry.find_edge_group "quad_edges"
      clipped_edge_geometry |> Option.get in
  if Edge_group.cardinality clipped_edge_group <> 3
     || Edge_group.length clipped_edge_group <> 4 then
    fail "clip did not preserve source fragments/exclude its cut edge";
  let extruded_edge_geometry = Poly_extrude.run ~distance:1. quad_edges
      |> get_ok in
  let extruded_edge_group = Geometry.find_edge_group "quad_edges"
      extruded_edge_geometry |> Option.get in
  if Edge_group.cardinality extruded_edge_group <> 8
     || Edge_group.length extruded_edge_group <> 12 then
    fail "poly extrude did not propagate bottom/top source edges";
  let sorted_edge_geometry = Ordering.sort ~owner:Ordering.Points ~key:Ordering.X quad_edges
      |> get_ok in
  let sorted_edge_group = Geometry.find_edge_group "quad_edges"
      sorted_edge_geometry |> Option.get in
  if Edge_group.cardinality sorted_edge_group <> 4 then
    fail "point sort did not remap native edge membership";
  let triangle_edges = Group_mesh.group_edges ~name:"triangle_edges" triangle_geometry
      |> get_ok in
  let sorted_primitives = Ordering.sort ~descending:true ~owner:Ordering.Primitives
      ~key:Ordering.Reverse triangle_edges |> get_ok in
  if Edge_group.cardinality (Geometry.find_edge_group "triangle_edges"
      sorted_primitives |> Option.get) <> 5 then
    fail "primitive sort did not remap native edge membership";
  let remove_first_edge_face = Group.init ~owner:Group.Primitive
      ~name:"remove_first_edge_face" 2 (fun primitive -> primitive = 0) in
  let deleted_edge_geometry = Deletion.delete remove_first_edge_face triangle_edges
      |> get_ok in
  let deleted_edge_group = Geometry.find_edge_group "triangle_edges"
      deleted_edge_geometry |> Option.get in
  if Edge_group.cardinality deleted_edge_group <> 3
     || Edge_group.length deleted_edge_group <> 3 then
    fail "primitive deletion did not remap surviving native edges";
  let continuous_uv = Attribute.create_owned ~name:"existing_uv"
      ~owner:Attribute.Vertex
      (Attribute.Float2 (Packed.Float2.of_owned
        ~x:[|0.; 1.; 1.; 0.; 1.; 0.|]
        ~y:[|0.; 0.; 1.; 0.; 1.; 1.|] |> get_ok)) |> get_ok in
  let continuous_source = Geometry.with_attribute continuous_uv triangle_geometry
      |> get_ok in
  let continuous = Uv_ops.auto_seam ~angle:Float.pi
      ~include_boundaries:false ~existing_uv:"existing_uv" continuous_source
      |> get_ok in
  if Group.cardinality (seam_group continuous) <> 0
     || Edge_group.cardinality (edge_seam_group continuous) <> 0 then
    fail "UV Auto Seam cut a continuous shared edge";
  let discontinuous_uv = Attribute.create_owned ~name:"existing_uv"
      ~owner:Attribute.Vertex
      (Attribute.Float2 (Packed.Float2.of_owned
        ~x:[|0.; 1.; 1.; 0.25; 1.; 0.|]
        ~y:[|0.; 0.; 1.; 0.; 1.; 1.|] |> get_ok)) |> get_ok in
  let discontinuous_source = Geometry.with_attribute discontinuous_uv
      triangle_geometry |> get_ok in
  let discontinuous = Uv_ops.auto_seam ~angle:Float.pi
      ~include_boundaries:false ~existing_uv:"existing_uv" discontinuous_source
      |> get_ok in
  if Group.cardinality (seam_group discontinuous) <> 2
     || Edge_group.cardinality (edge_seam_group discontinuous) <> 1 then
    fail "UV Auto Seam missed an existing-UV discontinuity";
  let nonmanifold_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 0.; 0.; 0.|] ~y:[|0.; 0.; 1.; 0.; -1.|]
      ~z:[|0.; 0.; 0.; 1.; 0.|] in
  let nonmanifold_builder = Tb.create ~point_count:5 () in
  Tb.add_triangle nonmanifold_builder 0 1 2;
  Tb.add_triangle nonmanifold_builder 1 0 3;
  Tb.add_triangle nonmanifold_builder 0 1 4;
  let nonmanifold = Geometry.create ~positions:nonmanifold_positions
      ~topology:(Tb.freeze nonmanifold_builder) () |> get_ok in
  let nonmanifold = Uv_ops.auto_seam ~angle:Float.pi
      ~include_boundaries:false ~include_non_manifold:true nonmanifold |> get_ok in
  if Group.cardinality (seam_group nonmanifold) <> 3
     || Edge_group.cardinality (edge_seam_group nonmanifold) <> 1 then
    fail "UV Auto Seam non-manifold incidence policy";
  let nonmanifold_edges = Group_mesh.group_edges ~name:"nonmanifold"
      ~incidence:Group_mesh.Non_manifold_edge nonmanifold |> get_ok
      |> Geometry.find_edge_group "nonmanifold" |> Option.get in
  if Edge_group.cardinality nonmanifold_edges <> 1 then
    fail "Edge Group non-manifold incidence";
  let strip_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 2.; 0.; 1.; 2.|]
      ~y:[|0.; 0.; 0.; 1.; 1.; 1.|] ~z:(Array.make 6 0.) in
  let strip_builder = Tb.create ~point_count:6 () in
  Tb.add_polygon strip_builder [|0; 1; 4; 3|];
  Tb.add_polygon strip_builder [|1; 2; 5; 4|];
  let strip_uv = Attribute.create_owned ~name:"uv" ~owner:Attribute.Vertex
      (Attribute.Float2 (Packed.Float2.of_owned
        ~x:[|0.; 1.; 1.; 0.; 1.; 2.; 2.; 1.|]
        ~y:[|0.; 0.; 1.; 1.; 0.; 0.; 1.; 1.|] |> get_ok)) |> get_ok in
  let strip = Geometry.create ~positions:strip_positions
      ~topology:(Tb.freeze strip_builder) ~attributes:[strip_uv] ()
      |> get_ok in
  let unitized domains seams = Parallel.run ~domains (fun () ->
    Uv_ops.unitize ~grain:1 ?seams ~uniform:false Uv_ops.Islands strip |> get_ok) in
  let island_unitized_one = unitized 1 None
  and island_unitized_many = unitized 4 None in
  let uv_values geometry = Geometry.find_attribute ~owner:Attribute.Vertex "uv"
      geometry |> Option.get
      |> Attribute.get (Attribute.tex_coord ~owner:Attribute.Vertex)
      |> Option.get |> Packed.Float2.Private.view in
  let island_one_uv = uv_values island_unitized_one
  and island_many_uv = uv_values island_unitized_many in
  if island_one_uv.x <> [|0.; 0.5; 0.5; 0.; 0.5; 1.; 1.; 0.5|]
     || island_one_uv.y <> [|0.; 0.; 1.; 1.; 0.; 0.; 1.; 1.|]
     || island_one_uv.x <> island_many_uv.x
     || island_one_uv.y <> island_many_uv.y
  then fail "UV Unitize island fit/domain determinism";
  let forced_seams = Group.init ~owner:Group.Vertex ~name:"forced" 8
      (fun vertex -> vertex = 1 || vertex = 7) in
  let split_unitized = unitized 4 (Some forced_seams) |> uv_values in
  if split_unitized.x <> [|0.; 1.; 1.; 0.; 0.; 1.; 1.; 0.|]
     || split_unitized.y <> [|0.; 0.; 1.; 1.; 0.; 0.; 1.; 1.|]
  then fail "UV Unitize forced island seam";
  let strip_index = Topology_index.create (Geometry.topology strip) in
  let shared_strip_edge = Topology_index.find_edge strip_index ~a:1 ~b:4
      |> Option.get in
  let native_forced = Edge_group.init ~topology:(Geometry.topology strip)
      ~index:strip_index ~name:"forced_native"
      (fun edge -> edge = shared_strip_edge) in
  let native_split = Uv_ops.unitize ~grain:1 ~edge_seams:native_forced
      ~uniform:false Uv_ops.Islands strip |> get_ok |> uv_values in
  if native_split.x <> split_unitized.x || native_split.y <> split_unitized.y then
    fail "UV Unitize native edge seam differs from compatibility corners";
  (match Uv_ops.unitize ~edge_seams:hard_one_edges Uv_ops.Islands strip with
   | Error error when Error.code error = "invalid_uv" -> ()
   | _ -> fail "UV Unitize accepted an edge group from another topology");
  let face_unitized = Uv_ops.unitize ~uniform:false Uv_ops.Per_face strip
      |> get_ok |> uv_values in
  if face_unitized.x <> split_unitized.x
     || face_unitized.y <> split_unitized.y
  then fail "UV Unitize per-face mode";
  let uniform_unitized = Uv_ops.unitize ~uniform:true Uv_ops.Islands strip
      |> get_ok |> uv_values in
  if uniform_unitized.x <> [|0.; 0.5; 0.5; 0.; 0.5; 1.; 1.; 0.5|]
     || uniform_unitized.y <> [|0.25; 0.25; 0.75; 0.75; 0.25; 0.25; 0.75; 0.75|]
  then fail "UV Unitize uniform aspect preservation";
  let flatten_source = Plane_generators.grid ~columns:2 ~rows:2 ~size:2. () |> get_ok in
  let flattened domains = Parallel.run ~domains (fun () ->
    Uv_ops.flatten ~grain:1 ~iterations:500 ~tolerance:1e-12 flatten_source
    |> get_ok) in
  let flattened_one = flattened 1 and flattened_many = flattened 4 in
  let flat_one_uv = uv_values flattened_one
  and flat_many_uv = uv_values flattened_many in
  if flat_one_uv.x <> flat_many_uv.x || flat_one_uv.y <> flat_many_uv.y then
    fail "UV Flatten differs by domain count";
  Array.iteri (fun corner u ->
    let v = flat_one_uv.y.(corner) in
    if not (Float.is_finite u && Float.is_finite v)
        || u < 0. || u > 1. || v < 0. || v > 1. then
      fail "UV Flatten escaped its packed unit area") flat_one_uv.x;
  let flat_topology = Topology.Private.view (Geometry.topology flattened_one) in
  let winding = ref 0. in
  for primitive = 0 to Geometry.primitive_count flattened_one - 1 do
    let first = flat_topology.primitive_offsets.(primitive) in
    let area = (flat_one_uv.x.(first + 1) -. flat_one_uv.x.(first))
          *. (flat_one_uv.y.(first + 2) -. flat_one_uv.y.(first))
        -. (flat_one_uv.y.(first + 1) -. flat_one_uv.y.(first))
          *. (flat_one_uv.x.(first + 2) -. flat_one_uv.x.(first)) in
    if abs_float area <= 1e-12 then fail "UV Flatten collapsed a triangle";
    if !winding = 0. then winding := area
    else if area *. !winding <= 0. then fail "UV Flatten flipped a triangle"
  done;
  let relaxed domains = Parallel.run ~domains (fun () ->
    Uv_ops.relax ~grain:1 ~iterations:100 ~tolerance:1e-12 flattened_one
    |> get_ok) in
  let relaxed_one = relaxed 1 |> uv_values
  and relaxed_many = relaxed 4 |> uv_values in
  if relaxed_one.x <> relaxed_many.x || relaxed_one.y <> relaxed_many.y then
    fail "UV Relax differs by domain count";
  for corner = 0 to Geometry.vertex_count flattened_one - 1 do
    let point = flat_topology.vertex_points.(corner) in
    if point <> 4 && (relaxed_one.x.(corner) <> flat_one_uv.x.(corner)
        || relaxed_one.y.(corner) <> flat_one_uv.y.(corner)) then
      fail "UV Relax moved an island boundary"
  done;
  let seam_cut_box = Uv_ops.auto_seam ~angle:0.1 shared_box |> get_ok in
  let box_seams = Geometry.find_edge_group "uv_seams" seam_cut_box
      |> Option.get in
  let flattened_box = Uv_ops.flatten ~edge_seams:box_seams seam_cut_box
      |> get_ok |> uv_values in
  if Array.exists (fun value -> not (Float.is_finite value)) flattened_box.x
      || Array.exists (fun value -> not (Float.is_finite value)) flattened_box.y
  then fail "UV Flatten seam-cut box produced non-finite coordinates";
  (match Uv_ops.flatten shared_box with
   | Error error when Error.code error = "invalid_uv" -> ()
   | _ -> fail "UV Flatten accepted a closed island without seams");
  (match Uv_ops.flatten geometry with
   | Error error when Error.code error = "invalid_uv" -> ()
   | _ -> fail "UV Flatten accepted non-triangle polygons");
  (match Uv_ops.flatten ~iterations:1 ~tolerance:1e-15
      (Plane_generators.grid ~columns:20 ~rows:20 ~size:2. () |> get_ok) with
   | Error error when Error.code error = "invalid_uv" -> ()
   | _ -> fail "UV Flatten published a non-converged solve");
  (match Uv_ops.flatten ~edge_seams:hard_one_edges flatten_source with
   | Error error when Error.code error = "invalid_uv" -> ()
   | _ -> fail "UV Flatten accepted seams from another topology");
  let discontinuous_u = Array.copy flat_one_uv.x
  and discontinuous_v = Array.copy flat_one_uv.y in
  let center_corner = ref (-1) and repeated_center_corner = ref (-1) in
  Array.iteri (fun corner point -> if point = 4 then
    if !center_corner < 0 then center_corner := corner
    else if !repeated_center_corner < 0 then repeated_center_corner := corner)
    flat_topology.vertex_points;
  discontinuous_u.(!repeated_center_corner) <-
    discontinuous_u.(!repeated_center_corner) +. 0.25;
  let discontinuous_attribute = Attribute.create_owned ~name:"uv"
      ~owner:Attribute.Vertex
      (Attribute.Float2 (Packed.Float2.of_owned ~x:discontinuous_u
        ~y:discontinuous_v |> get_ok)) |> get_ok in
  let discontinuous_flattened = Geometry.with_attribute discontinuous_attribute
      flattened_one |> get_ok in
  (match Uv_ops.relax discontinuous_flattened with
   | Error error when Error.code error = "invalid_uv" -> ()
   | _ -> fail "UV Relax accepted an undeclared UV discontinuity");
  let cancelled_flatten = Cancel.create () in
  Cancel.cancel cancelled_flatten;
  (match Uv_ops.flatten ~cancel:cancelled_flatten flatten_source with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "UV Flatten ignored cancellation");
  let cancelled_unitize = Cancel.create () in
  Cancel.cancel cancelled_unitize;
  (match Uv_ops.auto_seam ~cancel:cancelled_unitize shared_box with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "UV Auto Seam ignored cancellation");
  (match Uv_ops.unitize ~cancel:cancelled_unitize Uv_ops.Islands strip with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "UV Unitize ignored cancellation");
  (match Group_mesh.group_edges ~cancel:cancelled_unitize strip with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "Edge Group ignored cancellation");
  (match Group_mesh.group_edges ~min_angle:0. cylinder_curve with
   | Error error when Error.code error = "invalid_edge_group" -> ()
   | _ -> fail "Edge Group accepted an angle filter on curves");
  (box, cylinder_curve, cylindrical, grid, strip, wrong_uv_geometry)

let uv_errors ~cylinder_curve ~cylindrical ~geometry ~selected ~shared_box ~strip ~wrong_uv_geometry =
  (match Uv_ops.unitize Uv_ops.Islands geometry with
   | Error error when Error.code error = "invalid_uv" -> ()
   | _ -> fail "UV Unitize accepted missing UVs");
  (match Uv_ops.auto_seam ~primitives:selected shared_box with
   | Error error when Error.code error = "invalid_topology" -> ()
   | _ -> fail "UV Auto Seam accepted a point-owned primitive selection");
  (match Uv_ops.unitize ~seams:selected Uv_ops.Islands strip with
   | Error error when Error.code error = "invalid_uv" -> ()
   | _ -> fail "UV Unitize accepted a point-owned seam group");
  (match Uv_ops.auto_seam ~existing_uv:"uv" wrong_uv_geometry with
   | Error error when Error.code error = "invalid_topology" -> ()
   | _ -> fail "UV Auto Seam accepted non-float2 existing UVs");
  (match Uv_ops.auto_seam cylinder_curve with
   | Error error when Error.code error = "invalid_topology" -> ()
   | _ -> fail "UV Auto Seam accepted a polygon curve");
  (match Uv_ops.unitize Uv_ops.Islands cylindrical with
   | Error error when Error.code error = "invalid_uv" -> ()
   | _ -> fail "UV Unitize accepted a polygon curve");
  let extreme_uv = Attribute.create_owned ~name:"uv" ~owner:Attribute.Vertex
      (Attribute.Float2 (Packed.Float2.of_owned
        ~x:[|-.max_float; max_float; max_float; -.max_float|]
        ~y:[|0.; 0.; 1.; 1.|] |> get_ok)) |> get_ok in
  let extreme_uv_geometry = Geometry.with_attribute extreme_uv geometry |> get_ok in
  (match Uv_ops.unitize Uv_ops.Per_face extreme_uv_geometry with
   | Error error when Error.code error = "invalid_uv" -> ()
   | _ -> fail "UV Unitize accepted an overflowing finite UV extent");
  ()
