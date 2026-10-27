open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error _ -> fail "unexpected error"

let modeling ~geometry ~positions ~selected ~triangle_geometry ~weighted_triangles =
  let fuse_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 1.; 0.; 0.000_000_5|]
      ~y:[|0.; 0.; 1.; 1.; 0.|] ~z:(Array.make 5 0.) in
  let fuse_topology = Tb.create ~point_count:5 () in
  Tb.add_triangle fuse_topology 0 1 2;
  Tb.add_triangle fuse_topology 4 2 3;
  let weights = Attribute.create_owned ~name:"weight" ~owner:Attribute.Point
      (Attribute.Float [|2.; 4.; 6.; 8.; 10.|]) |> get_ok in
  let seam = Group.init ~owner:Group.Point ~name:"seam" 5
      (fun point -> point = 4) in
  let fuse_source = Geometry.create ~positions:fuse_positions
      ~topology:(Tb.freeze fuse_topology)
      ~attributes:[weights] ~groups:[seam] () |> get_ok in
  let fused domains = Parallel.run ~domains (fun () ->
      Fuse_grid.fuse ~grain:1 ~tolerance:1e-6 ~position:Fuse_reduce.Average_position
        ~attributes:Fuse_reduce.Average_numeric fuse_source |> get_ok) in
  let fused_one = fused 1 and fused_many = fused 4 in
  let fused_topology_one = Topology.Private.view (Geometry.topology fused_one)
  and fused_topology_many = Topology.Private.view (Geometry.topology fused_many) in
  if Geometry.point_count fused_one <> 4
     || not (equal_positions fused_one fused_many)
     || fused_topology_one.vertex_points <> fused_topology_many.vertex_points
     || fused_topology_one.vertex_points <> [|0; 1; 2; 0; 2; 3|]
  then fail "fuse cardinality/topology/domain determinism";
  let fused_x, _, _ = Packed.Float3.get (Geometry.positions fused_one) 0 in
  if abs_float (fused_x -. 0.000_000_25) > 1e-15 then
    fail "fuse average position";
  (match Geometry.find_attribute ~owner:Attribute.Point "weight" fused_one with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Float values when values.(0) = 6. -> ()
        | _ -> fail "fuse numeric attribute reduction")
   | None -> fail "fuse dropped point attribute");
  (match Geometry.find_group ~owner:Group.Point "seam" fused_one with
   | Some group when Group.mem 0 group && Group.cardinality group = 1 -> ()
   | _ -> fail "fuse point-group union");
  let directed = Packed.Float3.Private.of_owned_exn
      ~x:(Array.make 4 1.) ~y:(Array.make 4 0.) ~z:(Array.make 4 0.) in
  let direction_attribute = Attribute.create_key_owned
      (Attribute.normal ~owner:Attribute.Point) directed |> get_ok in
  let mirror_source = Geometry.with_attribute direction_attribute
      triangle_geometry |> get_ok in
  let mirrored domains = Parallel.run ~domains (fun () ->
      Mirror_geometry.run ~grain:1 ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x mirror_source
      |> get_ok) in
  let mirrored_one = mirrored 1 and mirrored_many = mirrored 4 in
  let mirror_topology_one = Topology.Private.view (Geometry.topology mirrored_one)
  and mirror_topology_many = Topology.Private.view (Geometry.topology mirrored_many) in
  if Geometry.point_count mirrored_one <> 8
     || Geometry.primitive_count mirrored_one <> 4
     || not (equal_positions mirrored_one mirrored_many)
     || mirror_topology_one.vertex_points <> mirror_topology_many.vertex_points
     || Array.sub mirror_topology_one.vertex_points 6 6 <> [|6; 5; 4; 7; 6; 4|]
  then fail "mirror winding/cardinality/domain determinism";
  let mirror_normals = Geometry.find_attribute ~owner:Attribute.Point "N" mirrored_one
      |> Option.get |> Attribute.get (Attribute.normal ~owner:Attribute.Point)
      |> Option.get in
  let original_nx, _, _ = Packed.Float3.get mirror_normals 0
  and reflected_nx, _, _ = Packed.Float3.get mirror_normals 4 in
  if original_nx <> 1. || reflected_nx <> -1. then fail "mirror normal reflection";
  let shared_box = Box_generator.box ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_ok
      |> Fuse_grid.fuse ~tolerance:0. ~attributes:Fuse_reduce.Average_numeric |> get_ok in
  let shared_positions = Packed.Float3.Private.view (Geometry.positions shared_box) in
  let clip_weight = Attribute.create_owned ~name:"weight" ~owner:Attribute.Point
      (Attribute.Float (Array.copy shared_positions.x)) |> get_ok in
  let clip_source = Geometry.with_attribute clip_weight shared_box |> get_ok in
  let clipped domains = Parallel.run ~domains (fun () ->
      Plane_clip.clip ~grain:1 ~keep:Plane_clip.Above ~fill:true ~cap_group:"cap"
        ~clipped_group:"cut" ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x
        clip_source |> get_ok) in
  let clipped_one = clipped 1 and clipped_many = clipped 4 in
  let cap = Geometry.find_group ~owner:Group.Primitive "cap" clipped_one
      |> Option.get in
  if Group.cardinality cap <> 1 then fail "clip cap primitive group";
  let cap_primitive = ref (-1) in
  Group.iter (fun primitive -> cap_primitive := primitive) cap;
  let first_cap, last_cap = Topology.primitive_vertex_range
      (Geometry.topology clipped_one) !cap_primitive in
  if last_cap - first_cap < 4 then fail "clip cap loop cardinality";
  let clipped_normals = Geometry.find_attribute ~owner:Attribute.Vertex "N"
      clipped_one |> Option.get
      |> Attribute.get (Attribute.normal ~owner:Attribute.Vertex) |> Option.get in
  for vertex = first_cap to last_cap - 1 do
    let x, y, z = Packed.Float3.get clipped_normals vertex in
    if abs_float (x +. 1.) > 1e-12 || abs_float y > 1e-12
       || abs_float z > 1e-12 then fail "clip cap hard normal"
  done;
  let clipped_index = Topology_index.create (Geometry.topology clipped_one) in
  if Topology_index.boundary_edge_count clipped_index <> 0 then
    fail "filled clip is not topologically watertight";
  let clipped_weights geometry = Geometry.find_attribute ~owner:Attribute.Point
      "weight" geometry |> Option.get
      |> Attribute.get (Attribute.key ~name:"weight" ~owner:Attribute.Point
          Attribute.float) |> Option.get in
  if clipped_weights clipped_one <> clipped_weights clipped_many then
    fail "clip interpolated attribute domain determinism";
  for vertex = first_cap to last_cap - 1 do
    let point = Topology.point_of_vertex (Geometry.topology clipped_one) vertex in
    if abs_float (clipped_weights clipped_one).(point) > 1e-12 then
      fail "clip point attribute interpolation"
  done;
  let split = Plane_clip.clip ~keep:Plane_clip.All ~split_connectivity:true
      ~above_group:"above" ~below_group:"below" ~origin:Rays_math.Vec3.zero
      ~normal:Rays_math.Vec3.unit_x clip_source |> get_ok in
  let split_topology = Geometry.topology split
  and above = Geometry.find_group ~owner:Group.Primitive "above" split |> Option.get
  and below = Geometry.find_group ~owner:Group.Primitive "below" split |> Option.get in
  let above_plane = Hashtbl.create 16 in
  Group.iter (fun primitive ->
    let first, last = Topology.primitive_vertex_range split_topology primitive in
    for vertex = first to last - 1 do
      let point = Topology.point_of_vertex split_topology vertex in
      let x, _, _ = Packed.Float3.get (Geometry.positions split) point in
      if abs_float x < 1e-12 then Hashtbl.replace above_plane point ()
    done) above;
  Group.iter (fun primitive ->
    let first, last = Topology.primitive_vertex_range split_topology primitive in
    for vertex = first to last - 1 do
      let point = Topology.point_of_vertex split_topology vertex in
      let x, _, _ = Packed.Float3.get (Geometry.positions split) point in
      if abs_float x < 1e-12 && Hashtbl.mem above_plane point then
        fail "clip split connectivity shared a plane point"
    done) below;
  let clipped_curve = Line_geometry.polyline [|(-1.,0.,0.); (0.,1.,0.); (1.,0.,0.)|]
      |> get_ok |> Plane_clip.clip ~keep:Plane_clip.Above ~origin:Rays_math.Vec3.zero
           ~normal:Rays_math.Vec3.unit_x |> get_ok in
  if Geometry.primitive_count clipped_curve <> 1
     || Geometry.point_count clipped_curve <> 2
     || Topology.primitive_kind (Geometry.topology clipped_curve) 0
        <> Topology.Open_polyline then fail "open-polyline plane clip";
  let point_cloud = Line_geometry.points
      [|(-1.,0.,0.); (0.000_01,0.,0.); (1.,0.,0.)|] in
  let point_ids = Attribute.create_owned ~name:"id" ~owner:Attribute.Point
      (Attribute.Int [|10; 20; 30|]) |> get_ok in
  let point_cloud = Geometry.with_attribute point_ids point_cloud |> get_ok in
  let clipped_points = Plane_clip.clip ~snapping_tolerance:0.001
      ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x point_cloud |> get_ok in
  let clipped_positions = Packed.Float3.Private.view (Geometry.positions clipped_points) in
  check (clipped_positions.x = [|0.;1.|]
      && Geometry.vertex_count clipped_points = 0
      && Geometry.primitive_count clipped_points = 0
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "id" clipped_points |> Option.get) = Attribute.Int [|20;30|])
    "point-cloud clip snaps boundary points and preserves selected payload";
  let vertex_clip_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|-1.; 1.; 1.|] ~y:[|0.; 0.; 1.|] ~z:[|0.; 0.; 0.|] in
  let vertex_clip_topology = Tb.create ~point_count:3 () in
  Tb.add_triangle vertex_clip_topology 0 1 2;
  let vertex_u = Attribute.create_owned ~name:"u" ~owner:Attribute.Vertex
      (Attribute.Float [|0.; 2.; 4.|]) |> get_ok in
  let vertex_clip_source = Geometry.create ~positions:vertex_clip_positions
      ~topology:(Tb.freeze vertex_clip_topology)
      ~attributes:[vertex_u] () |> get_ok in
  let vertex_clipped = Plane_clip.clip ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x
      vertex_clip_source |> get_ok in
  let vertex_u = Geometry.find_attribute ~owner:Attribute.Vertex "u"
      vertex_clipped |> Option.get
      |> Attribute.get (Attribute.key ~name:"u" ~owner:Attribute.Vertex
          Attribute.float) |> Option.get in
  let plane_values = ref [] in
  for vertex = 0 to Geometry.vertex_count vertex_clipped - 1 do
    let point = Topology.point_of_vertex (Geometry.topology vertex_clipped) vertex in
    let x, _, _ = Packed.Float3.get (Geometry.positions vertex_clipped) point in
    if abs_float x < 1e-12 then plane_values := vertex_u.(vertex) :: !plane_values
  done;
  if List.sort Float.compare !plane_values <> [1.; 2.] then
    fail "clip vertex attribute interpolation";
  let closed_curve = Plane_generators.circle ~segments:16 ~radius:1. () |> get_ok
      |> Plane_clip.clip ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x |> get_ok in
  if Geometry.primitive_count closed_curve <> 1
     || Topology.primitive_kind (Geometry.topology closed_curve) 0
        <> Topology.Open_polyline then
    fail "closed-polyline plane clip did not emit an open retained arc";
  let concave = Line_geometry.polyline ~closed:true
      [|(-2.,-2.,0.); (2.,-2.,0.); (2.,2.,0.); (1.,2.,0.);
        (1.,-1.,0.); (-1.,-1.,0.); (-1.,2.,0.); (-2.,2.,0.)|]
      |> get_ok in
  let concave_topology = Topology.Private.view (Geometry.topology concave) in
  let concave_polygon = Topology.Private.create_validated_owned
      ~point_count:(Geometry.point_count concave)
      ~vertex_points:(Array.copy concave_topology.vertex_points)
      ~primitive_offsets:(Array.copy concave_topology.primitive_offsets)
      ~primitive_kinds:(Bytes.make 1 '\000') in
  let concave = Geometry.create ~positions:(Geometry.positions concave)
      ~topology:concave_polygon () |> get_ok in
  let concave_clipped = Plane_clip.clip ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_y
      concave |> get_ok in
  if Geometry.primitive_count concave_clipped <> 2
     || Geometry.vertex_count concave_clipped <> 8 then
    fail "clip did not reconstruct disconnected concave half-plane fragments";
  (match Plane_clip.clip ~split_connectivity:true ~origin:Rays_math.Vec3.zero
      ~normal:Rays_math.Vec3.unit_x clip_source with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "clip accepted split connectivity outside keep-all mode");
  (match Plane_clip.clip ~cap_group:"same" ~above_group:"same" ~origin:Rays_math.Vec3.zero
      ~normal:Rays_math.Vec3.unit_x clip_source with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "clip accepted duplicate output group names");
  let huge_normal_clip = Plane_clip.clip ~origin:Rays_math.Vec3.zero
      ~normal:(Rays_math.Vec3.create 1e300 0. 0.) clip_source |> get_ok
  and unit_normal_clip = Plane_clip.clip ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.unit_x
      clip_source |> get_ok in
  if not (equal_positions huge_normal_clip unit_normal_clip) then
    fail "clip normal normalization overflow";
  (match Plane_clip.clip ~origin:Rays_math.Vec3.zero ~normal:Rays_math.Vec3.zero clip_source with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "clip accepted a zero plane normal");
  (match Plane_clip.clip ~fill:true ~origin:(Rays_math.Vec3.create 0.5 0. 0.) ~normal:Rays_math.Vec3.unit_x
      triangle_geometry with
   | Error error when Error.code error = "invalid_geometry" -> ()
   | _ -> fail "clip filled an open polygon boundary");
  let uv = Attribute.create_owned ~name:"uv" ~owner:Attribute.Vertex
      (Attribute.Float2 (Packed.Float2.of_owned
        ~x:[|0.; 1.; 1.; 0.|] ~y:[|0.; 0.; 1.; 1.|] |> get_ok)) |> get_ok
  and material = Attribute.create_owned ~name:"material"
      ~owner:Attribute.Primitive (Attribute.Int [|7|]) |> get_ok
  and stale_normal = Attribute.create_owned ~name:"N" ~owner:Attribute.Point
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:(Array.make 4 0.) ~y:(Array.make 4 0.) ~z:(Array.make 4 1.)))
      |> get_ok
  and all_corners = Group.init ~owner:Group.Vertex ~name:"all_corners" 4
      (fun _ -> true)
  and selected_face = Group.init ~owner:Group.Primitive ~name:"selected_face" 1
      (fun _ -> true) in
  let subdivision_source = geometry
      |> Geometry.with_attribute uv |> get_ok
      |> Geometry.with_attribute material |> get_ok
      |> Geometry.with_attribute stale_normal |> get_ok
      |> Geometry.with_group all_corners |> get_ok
      |> Geometry.with_group selected_face |> get_ok in
  let catmull domains = Parallel.run ~domains (fun () ->
      Subdivide.subdivide ~grain:1 ~scheme:Subdivide.Catmull_clark subdivision_source
      |> get_ok) in
  let catmull_one = catmull 1 and catmull_many = catmull 4 in
  let catmull_topology = Topology.Private.view (Geometry.topology catmull_one)
  and catmull_topology_many = Topology.Private.view
      (Geometry.topology catmull_many) in
  if Geometry.point_count catmull_one <> 9
     || Geometry.primitive_count catmull_one <> 4
     || Geometry.vertex_count catmull_one <> 16
     || catmull_topology.vertex_points <> catmull_topology_many.vertex_points
     || not (equal_positions catmull_one catmull_many)
  then fail "Catmull-Clark cardinality/domain determinism";
  let c0x, c0y, c0z = Packed.Float3.get (Geometry.positions catmull_one) 0
  and center_x, center_y, center_z = Packed.Float3.get
      (Geometry.positions catmull_one) 8 in
  if abs_float (c0x -. 0.125) > 1e-12
     || abs_float (c0y -. 0.125) > 1e-12 || abs_float c0z > 1e-12
     || abs_float (center_x -. 0.5) > 1e-12
     || abs_float (center_y -. 0.5) > 1e-12 || abs_float center_z > 1e-12
  then fail "Catmull-Clark boundary/face stencils";
  if Array.exists (fun primitive ->
      Topology.primitive_size (Geometry.topology catmull_one) primitive <> 4)
      (Array.init 4 Fun.id) then fail "Catmull-Clark did not emit quads";
  (match Geometry.find_attribute ~owner:Attribute.Point "N" catmull_one with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Float3 values ->
            let values = Packed.Float3.Private.view values in
            if not (Array.for_all (( = ) 0.) values.x
                && Array.for_all (( = ) 0.) values.y
                && Array.for_all (( = ) 1.) values.z) then
              fail "subdivision point-normal stencil"
        | _ -> fail "subdivision changed point-normal storage")
   | None -> fail "subdivision dropped interpolated point normals");
  let catmull_uv geometry = Geometry.find_attribute ~owner:Attribute.Vertex "uv"
      geometry |> Option.get
      |> Attribute.get (Attribute.tex_coord ~owner:Attribute.Vertex)
      |> Option.get |> Packed.Float2.Private.view in
  let uv_one = catmull_uv catmull_one and uv_many = catmull_uv catmull_many in
  if uv_one.x <> uv_many.x || uv_one.y <> uv_many.y
     || Array.sub uv_one.x 0 4 <> [|0.; 0.5; 0.5; 0.|]
     || Array.sub uv_one.y 0 4 <> [|0.; 0.; 0.5; 0.5|]
  then fail "Catmull-Clark face-varying UV refinement";
  (match Geometry.find_attribute ~owner:Attribute.Primitive "material" catmull_one with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Int values when values = [|7; 7; 7; 7|] -> ()
        | _ -> fail "subdivision primitive attribute propagation")
   | None -> fail "subdivision dropped primitive attribute");
  (match Geometry.find_group ~owner:Group.Point "even" catmull_one with
   | Some group when Group.cardinality group = 2 -> ()
   | _ -> fail "subdivision point group propagation");
  (match Geometry.find_group ~owner:Group.Vertex "all_corners" catmull_one with
   | Some group when Group.cardinality group = 16 -> ()
   | _ -> fail "subdivision vertex group propagation");
  (match Geometry.find_group ~owner:Group.Primitive "selected_face" catmull_one with
   | Some group when Group.cardinality group = 4 -> ()
   | _ -> fail "subdivision primitive group propagation");
  let bilinear = Subdivide.subdivide ~scheme:Subdivide.Bilinear ~iterations:2
      subdivision_source |> get_ok in
  if Geometry.point_count bilinear <> 25
     || Geometry.primitive_count bilinear <> 16
     || Geometry.vertex_count bilinear <> 64 then
    fail "bilinear subdivision iteration cardinality";
  for point = 0 to 3 do
    if Packed.Float3.get (Geometry.positions bilinear) point
       <> Packed.Float3.get positions point then
      fail "bilinear subdivision moved a control point"
  done;
  let loop_one = Subdivide.subdivide ~scheme:Subdivide.Loop triangle_geometry |> get_ok in
  if Geometry.point_count loop_one <> 9
     || Geometry.primitive_count loop_one <> 8
     || Geometry.vertex_count loop_one <> 24
     || not (Topology.all_triangles (Geometry.topology loop_one)) then
    fail "Loop subdivision cardinality/topology";
  let tetra_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 0.; 0.|] ~y:[|0.; 0.; 1.; 0.|]
      ~z:[|0.; 0.; 0.; 1.|] in
  let tetra_topology = Tb.create ~point_count:4 () in
  Tb.add_triangle tetra_topology 0 2 1;
  Tb.add_triangle tetra_topology 0 1 3;
  Tb.add_triangle tetra_topology 1 2 3;
  Tb.add_triangle tetra_topology 2 0 3;
  let tetra_topology = Tb.freeze tetra_topology in
  let tetra_index = Topology_index.create tetra_topology in
  let sharp_edge = Topology_index.find_edge tetra_index ~a:0 ~b:1
      |> Option.get in
  let crease_values = Array.init (Topology.vertex_count tetra_topology)
      (fun vertex ->
        if Topology_index.edge_of_vertex tetra_index vertex = sharp_edge
        then 2. else 0.) in
  let creaseweight = Attribute.create_owned ~name:"creaseweight"
      ~owner:Attribute.Vertex (Attribute.Float crease_values) |> get_ok
  and cornerweight = Attribute.create_owned ~name:"cornerweight"
      ~owner:Attribute.Point (Attribute.Float [|0.; 0.; 1.; 0.|]) |> get_ok in
  let creased_source = Geometry.create ~positions:tetra_positions
      ~topology:tetra_topology ~attributes:[creaseweight; cornerweight] ()
      |> get_ok in
  let creased = Subdivide.subdivide creased_source |> get_ok in
  let ex, ey, ez = Packed.Float3.get (Geometry.positions creased)
      (4 + sharp_edge) in
  if abs_float (ex -. 0.5) > 1e-12 || abs_float ey > 1e-12
     || abs_float ez > 1e-12 then
    fail "semi-sharp subdivision did not preserve the sharp edge midpoint";
  if Packed.Float3.get (Geometry.positions creased) 2 <> (0., 1., 0.) then
    fail "subdivision cornerweight did not preserve the control point";
  (match Geometry.find_attribute ~owner:Attribute.Vertex "creaseweight" creased with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Float values when Array.exists ((=) 1.) values -> ()
        | _ -> fail "subdivision did not decay/output creaseweight")
   | None -> fail "subdivision dropped resulting creaseweight");
  let negative_crease = Attribute.create_owned ~name:"creaseweight"
      ~owner:Attribute.Vertex
      (Attribute.Float (Array.make (Topology.vertex_count tetra_topology) (-1.)))
      |> get_ok in
  let negative_crease = Geometry.create ~positions:tetra_positions
      ~topology:tetra_topology ~attributes:[negative_crease] () |> get_ok in
  (match Subdivide.subdivide negative_crease with
   | Error error when Error.code error = "invalid_topology" -> ()
   | _ -> fail "subdivision accepted a negative creaseweight");
  (match Subdivide.subdivide ~scheme:Subdivide.Loop geometry with
   | Error error when Error.code error = "invalid_topology" -> ()
   | _ -> fail "Loop subdivision accepted a non-triangle polygon");
  let nonmanifold_topology = Tb.create ~point_count:4 () in
  Tb.add_triangle nonmanifold_topology 0 1 2;
  Tb.add_triangle nonmanifold_topology 1 0 3;
  Tb.add_triangle nonmanifold_topology 0 1 3;
  let nonmanifold = Geometry.create ~positions
      ~topology:(Tb.freeze nonmanifold_topology) () |> get_ok in
  (match Subdivide.subdivide nonmanifold with
   | Error error when Error.code error = "invalid_topology" -> ()
   | _ -> fail "subdivision accepted a non-manifold edge");
  let bowtie_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.; 1.; 0.; 0.; -1.; 0.; 0.|]
      ~y:[|0.; 0.; 1.; 0.; 0.; -1.; 0.|]
      ~z:[|0.; 0.; 0.; 1.; 0.; 0.; -1.|] in
  let bowtie_topology = Tb.create ~point_count:7 () in
  List.iter (fun (a, b, c) -> Tb.add_triangle bowtie_topology a b c)
    [(0,2,1); (0,1,3); (1,2,3); (2,0,3);
     (0,4,5); (0,6,4); (4,6,5); (5,6,0)];
  let bowtie = Geometry.create ~positions:bowtie_positions
      ~topology:(Tb.freeze bowtie_topology) () |> get_ok in
  (match Subdivide.subdivide bowtie with
   | Error error when Error.code error = "invalid_topology" -> ()
   | _ -> fail "subdivision accepted disconnected vertex fans");
  let subdivision_cancel = Cancel.create () in
  Cancel.cancel subdivision_cancel;
  (match Subdivide.subdivide ~cancel:subdivision_cancel subdivision_source with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "cancelled subdivision published geometry or wrong error");
  let point_ids = Attribute.create_owned ~name:"point_id" ~owner:Attribute.Point
      (Attribute.Int [|10; 20; 30; 40|]) |> get_ok in
  let delete_source = Geometry.with_attribute point_ids subdivision_source
      |> get_ok in
  let remove_point = Group.init ~owner:Group.Point ~name:"remove_point" 4
      (fun point -> point = 1) in
  let remove_nothing = Group.init ~owner:Group.Point ~name:"remove_nothing" 4
      (fun _ -> false) in
  if Deletion.delete remove_nothing delete_source |> get_ok != delete_source then
    fail "no-op deletion did not preserve geometry identity";
  let healed_points domains = Parallel.run ~domains (fun () ->
      Deletion.delete ~grain:1 ~policy:Deletion.Heal_primitives remove_point delete_source
      |> get_ok) in
  let healed_one = healed_points 1 and healed_many = healed_points 4 in
  let healed_topology = Topology.Private.view (Geometry.topology healed_one)
  and healed_topology_many = Topology.Private.view
      (Geometry.topology healed_many) in
  if Geometry.point_count healed_one <> 3
     || Geometry.primitive_count healed_one <> 1
     || Geometry.vertex_count healed_one <> 3
     || healed_topology.vertex_points <> [|0; 1; 2|]
     || healed_topology.vertex_points <> healed_topology_many.vertex_points
     || not (equal_positions healed_one healed_many) then
    fail "point delete/heal cardinality/domain determinism";
  (match Geometry.find_attribute ~owner:Attribute.Point "point_id" healed_one with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Int values when values = [|10; 30; 40|] -> ()
        | _ -> fail "point delete attribute remap")
   | None -> fail "point delete dropped point attribute");
  (match Geometry.find_attribute ~owner:Attribute.Point "point_id" healed_many with
   | Some attribute ->
       (match Attribute.storage attribute with
        | Attribute.Int values when values = [|10; 30; 40|] -> ()
        | _ -> fail "parallel point delete attribute remap")
   | None -> fail "parallel point delete dropped point attribute");
  if Geometry.find_attribute ~owner:Attribute.Point "N" healed_one <> None then
    fail "point healing retained stale normals";
  (match Geometry.find_group ~owner:Group.Point "even" healed_one with
   | Some group when Group.cardinality group = 2
       && Group.mem 0 group && Group.mem 1 group -> ()
   | _ -> fail "point delete group remap");
  (match Geometry.find_group ~owner:Group.Point "even" healed_many with
   | Some group when Group.cardinality group = 2
       && Group.mem 0 group && Group.mem 1 group -> ()
   | _ -> fail "parallel point delete group remap");
  let destroyed = Deletion.delete remove_point delete_source |> get_ok in
  if Geometry.point_count destroyed <> 3
     || Geometry.primitive_count destroyed <> 0 then
    fail "point delete destroy-touched policy";
  let kept_only = Deletion.delete ~selected:false ~policy:Deletion.Heal_primitives
      selected delete_source |> get_ok in
  if Geometry.point_count kept_only <> 2
     || Geometry.primitive_count kept_only <> 0 then
    fail "point delete non-selected inversion";
  let remove_vertex = Group.init ~owner:Group.Vertex ~name:"remove_vertex" 4
      (fun vertex -> vertex = 1) in
  let healed_vertex = Deletion.delete ~policy:Deletion.Heal_primitives remove_vertex
      delete_source |> get_ok in
  if Geometry.point_count healed_vertex <> 4
     || Geometry.vertex_count healed_vertex <> 3
     || (Topology.Private.view (Geometry.topology healed_vertex)).vertex_points
        <> [|0; 2; 3|] then
    fail "vertex delete/heal topology";
  (match Geometry.find_attribute ~owner:Attribute.Vertex "uv" healed_vertex with
   | Some attribute ->
       let values = Attribute.get (Attribute.tex_coord ~owner:Attribute.Vertex)
           attribute |> Option.get |> Packed.Float2.Private.view in
       if values.x <> [|0.; 1.; 0.|] || values.y <> [|0.; 1.; 1.|] then
         fail "vertex delete attribute remap"
   | None -> fail "vertex delete dropped vertex attribute");
  let destroyed_vertex = Deletion.delete remove_vertex delete_source |> get_ok in
  if Geometry.primitive_count destroyed_vertex <> 0
     || Geometry.point_count destroyed_vertex <> 4 then
    fail "vertex delete destroy-touched policy";
  let remove_first_primitive = Group.init ~owner:Group.Primitive
      ~name:"remove_first" 2 (fun primitive -> primitive = 0) in
  let primitive_deleted = Deletion.delete remove_first_primitive weighted_triangles
      |> get_ok in
  if Packed.Float3.data_id (Geometry.positions primitive_deleted)
       <> Packed.Float3.data_id (Geometry.positions weighted_triangles) then
    fail "primitive deletion copied unchanged point storage";
  let compacted_vertex = Deletion.delete ~compact_points:true remove_vertex
      delete_source |> get_ok in
  if Geometry.point_count compacted_vertex <> 0 then
    fail "vertex deletion compact-points policy";
  let open_curve = Line_geometry.polyline
      [|(0.,0.,0.); (1.,0.,0.); (2.,0.,0.); (3.,0.,0.)|] |> get_ok in
  let curve_vertex = Group.init ~owner:Group.Vertex ~name:"middle" 4
      (fun vertex -> vertex = 1) in
  let healed_curve = Deletion.delete ~policy:Deletion.Heal_primitives curve_vertex open_curve
      |> get_ok in
  if Geometry.vertex_count healed_curve <> 3
     || Topology.primitive_kind (Geometry.topology healed_curve) 0
        <> Topology.Open_polyline then
    fail "open-curve vertex healing";
  let wrong_delete_group = Group.init ~owner:Group.Point ~name:"wrong" 3
      (fun _ -> false) in
  (match Deletion.delete wrong_delete_group delete_source with
   | Error error when Error.code error = "invalid_selection" -> ()
   | _ -> fail "delete accepted a mismatched selection length");
  let delete_cancel = Cancel.create () in
  Cancel.cancel delete_cancel;
  (match Deletion.delete ~cancel:delete_cancel remove_point delete_source with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "cancelled delete published geometry or wrong error");
  shared_box
