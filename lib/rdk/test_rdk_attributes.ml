open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error _ -> fail "unexpected error"

let transfer ~geometry ~triangle_geometry =
  let transfer_source = Line_geometry.points [|(0., 0., 0.); (2., 0., 0.)|] in
  let transfer_weight = Attribute.create_owned ~name:"weight"
      ~owner:Attribute.Point (Attribute.Float [|0.; 10.|]) |> get_ok
  and transfer_id = Attribute.create_owned ~name:"id"
      ~owner:Attribute.Point (Attribute.Int [|5; 9|]) |> get_ok in
  let transfer_source = transfer_source
      |> Geometry.with_attribute transfer_weight |> get_ok
      |> Geometry.with_attribute transfer_id |> get_ok in
  let transfer_target = Line_geometry.points
      [|(0., 0., 0.); (1., 0., 0.); (10., 0., 0.)|] in
  let existing_weight = Attribute.create_owned ~name:"weight"
      ~owner:Attribute.Point (Attribute.Float [|100.; 100.; 100.|]) |> get_ok in
  let transfer_target = Geometry.with_attribute existing_weight transfer_target
      |> get_ok in
  let transferred domains = Parallel.run ~domains (fun () ->
      Attribute_ops.transfer_points ~grain:1 ~max_distance:3.
        ~mode:(Attribute_ops.Inverse_distance { neighbors = 2; power = 1. })
        ~source:transfer_source ~target:transfer_target () |> get_ok) in
  let transfer_one = transferred 1 and transfer_many = transferred 4 in
  check (geometry_bytes transfer_one = geometry_bytes transfer_many
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "weight" transfer_one |> Option.get) = Attribute.Float [|0.;5.;100.|]
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "id" transfer_one |> Option.get) = Attribute.Int [|5;5;0|])
    "point transfer exact match/interpolation/ties/miss/domain exactness";
  (match Attribute_ops.transfer_points ~names:["weight"] ~pattern:"*"
      ~source:transfer_source ~target:transfer_target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "point transfer accepted both exact names and a pattern");
  (match Attribute_ops.transfer_points ~pattern:"bad["
      ~source:transfer_source ~target:transfer_target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "point transfer accepted a malformed pattern");
  let source_second = Group.init ~owner:Group.Point ~name:"source_second" 2
      (fun point -> point = 1)
  and target_outer = Group.init ~owner:Group.Point ~name:"target_outer" 3
      (fun point -> point <> 1) in
  let restricted_transfer domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_points ~grain:1 ~max_distance:3.
      ~source_points:source_second ~target_points:target_outer
      ~source:transfer_source ~target:transfer_target () |> get_ok) in
  let restricted_one = restricted_transfer 1
  and restricted_many = restricted_transfer 4 in
  check (geometry_bytes restricted_one = geometry_bytes restricted_many
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "weight" restricted_one |> Option.get)
        = Attribute.Float [|10.;100.;100.|])
    "point transfer source/target restrictions preserve unselected and missed values";
  let primitive_weight = Attribute.create_owned ~name:"primitive_weight"
      ~owner:Attribute.Primitive (Attribute.Float [|10.; 20.|]) |> get_ok
  and primitive_id = Attribute.create_owned ~name:"primitive_id"
      ~owner:Attribute.Primitive (Attribute.Int [|4; 8|]) |> get_ok in
  let primitive_source = triangle_geometry
      (* Exact centroids (2,1)/(1,2) make the target (1.5,1.5) a true tie. *)
      |> Transform_ops.transform (Rays_math.Mat4.scaling (Rays_math.Vec3.create 3. 3. 3.))
      |> Geometry.with_attribute primitive_weight |> get_ok
      |> Geometry.with_attribute primitive_id |> get_ok in
  let primitive_target_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 3.; 1.5|] ~y:[|0.; 0.; 4.5|] ~z:[|0.; 0.; 0.|] in
  let primitive_target_topology = Tb.create ~point_count:3 () in
  Tb.add_triangle primitive_target_topology 0 1 2;
  let primitive_target_geometry = Geometry.create
      ~positions:primitive_target_positions
      ~topology:(Tb.freeze primitive_target_topology) () |> get_ok in
  let primitive_transferred domains = Parallel.run ~domains (fun () ->
      Attribute_ops.transfer_primitives ~grain:1
        ~mode:(Attribute_ops.Inverse_distance { neighbors = 2; power = 1. })
        ~source:primitive_source ~target:primitive_target_geometry () |> get_ok) in
  let primitive_one = primitive_transferred 1
  and primitive_many = primitive_transferred 4 in
  check (geometry_bytes primitive_one = geometry_bytes primitive_many)
    "primitive transfer complete geometry differs by domain count";
  (match Attribute.storage (Geometry.find_attribute ~owner:Attribute.Primitive
      "primitive_id" primitive_one |> Option.get) with
   | Attribute.Int [|value|] -> check (value = 4)
       (Printf.sprintf "primitive transfer discrete tie: got %d, expected 4" value)
   | _ -> fail "primitive transfer integer storage/cardinality");
  (match Attribute.storage (Geometry.find_attribute ~owner:Attribute.Primitive
      "primitive_weight" primitive_one |> Option.get) with
   | Attribute.Float [|value|] -> check (near value 15.)
       "primitive transfer barycenter interpolation"
   | _ -> fail "primitive transfer floating storage/cardinality");
  (match Attribute_ops.transfer_primitives ~source_primitives:source_second
      ~source:primitive_source ~target:primitive_target_geometry () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "primitive transfer accepted a point-owned source group");
  let source_detail = Attribute.create_owned ~name:"asset"
      ~owner:Attribute.Detail (Attribute.Text [|"oak"|]) |> get_ok in
  let detail_source = Geometry.with_attribute source_detail primitive_source
      |> get_ok in
  let detail_target = Attribute_ops.transfer_detail ~source:detail_source
      ~target:primitive_target_geometry () |> get_ok in
  (match Geometry.find_attribute ~owner:Attribute.Detail "asset" detail_target with
   | Some attribute when Attribute.storage_id attribute
       = Attribute.storage_id source_detail ->
       (match Attribute.storage attribute with
        | Attribute.Text [|"oak"|] -> ()
        | _ -> fail "detail transfer payload")
   | _ -> fail "detail transfer did not structurally share source payload");
  let detail_p = Attribute.create_owned ~name:"P" ~owner:Attribute.Detail
      (Attribute.Int [|23|]) |> get_ok in
  let detail_p_source = Geometry.with_attribute detail_p detail_source |> get_ok in
  let detail_pattern_target = Attribute_ops.transfer_detail ~pattern:"^P"
      ~source:detail_p_source ~target:primitive_target_geometry () |> get_ok in
  if Geometry.find_attribute ~owner:Attribute.Detail "asset"
       detail_pattern_target = None
     || Geometry.find_attribute ~owner:Attribute.Detail "P"
          detail_pattern_target <> None then
    fail "detail transfer leading-exclusion pattern";
  (match Attribute_ops.transfer_detail ~names:["P"] ~source:detail_p_source
      ~target:primitive_target_geometry () with
   | Ok geometry ->
       (match Geometry.find_attribute ~owner:Attribute.Detail "P" geometry with
        | Some attribute ->
            (match Attribute.storage attribute with
             | Attribute.Int [|23|] -> ()
             | _ -> fail "detail-owned P transfer payload")
        | None -> fail "detail-owned P transfer missing")
   | Error _ -> fail "detail-owned ordinary P was rejected");
  let indexed_points = Array.init 257 (fun point ->
      let value = float_of_int point in
      (sin (value *. 0.73) *. 7., cos (value *. 1.17) *. 5.,
       sin (value *. 0.19) *. 3.)) in
  let indexed_geometry = Line_geometry.points indexed_points in
  (match Spatial_index.create ~grain:0 (Geometry.positions indexed_geometry) with
   | Error error when Error.code error = "invalid_parameter" -> ()
   | _ -> fail "spatial index accepted a non-positive parallel grain");
  let indexed domains = Parallel.run ~domains (fun () ->
    Spatial_index.create ~grain:7 (Geometry.positions indexed_geometry) |> get_ok) in
  let indexes = [indexed 1; indexed 4] in
  for query = 0 to 127 do
    let value = float_of_int query in
    let qx = sin (value *. 0.31) *. 6.
    and qy = cos (value *. 0.43) *. 4.
    and qz = sin (value *. 0.61) *. 2. in
    let expected = ref 0 and expected_distance = ref Float.infinity in
    Array.iteri (fun point (x, y, z) ->
      let dx = x -. qx and dy = y -. qy and dz = z -. qz in
      let distance = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
      if distance < !expected_distance
         || (distance = !expected_distance && point < !expected) then begin
        expected := point; expected_distance := distance
      end) indexed_points;
    List.iter (fun index ->
      let indices = [|-17|] and distances_squared = [|-17.|] and counts = [|0|] in
      Spatial_index.View.nearest_k_many_into ~grain:1 index
        ~queries:(Packed.Float3.View.of_owned_exn ~x:[|qx|] ~y:[|qy|] ~z:[|qz|])
        ~max_distance_squared:Float.infinity ~capacity:1
        ~indices ~distances_squared ~counts;
      check (counts = [|1|] && indices = [|!expected|]
          && distances_squared = [|!expected_distance|])
        "spatial index nearest/reference/domain exactness and output slice") indexes
  done;
  let surface_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 2.; 0.|] ~y:[|0.; 0.; 2.|] ~z:[|0.; 0.; 0.|] in
  let surface_topology = Tb.create ~point_count:3 () in
  Tb.add_triangle surface_topology 0 1 2;
  let surface_weight = Attribute.create_owned ~name:"weight"
      ~owner:Attribute.Point (Attribute.Float [|0.; 2.; 4.|]) |> get_ok
  and surface_vertex = Attribute.create_owned ~name:"corner_value"
      ~owner:Attribute.Vertex (Attribute.Float [|10.; 20.; 30.|]) |> get_ok
  and surface_piece = Attribute.create_owned ~name:"piece"
      ~owner:Attribute.Primitive (Attribute.Int [|7|]) |> get_ok in
  let surface_source = Geometry.create ~positions:surface_positions
      ~topology:(Tb.freeze surface_topology)
      ~attributes:[surface_weight; surface_vertex; surface_piece] () |> get_ok in
  let vertex_owned_transfer domains = Parallel.run ~domains (fun () ->
      Attribute_ops.transfer_vertices ~grain:1 ~pattern:"corner*"
        ~source:surface_source ~target:triangle_geometry () |> get_ok) in
  let vertex_owned_one = vertex_owned_transfer 1
  and vertex_owned_many = vertex_owned_transfer 4 in
  let transferred_corner geometry = Geometry.find_attribute
      ~owner:Attribute.Vertex "corner_value" geometry |> Option.get
      |> Attribute.get (Attribute.key ~name:"corner_value"
          ~owner:Attribute.Vertex Attribute.float) |> Option.get in
  if not (near_array [|10.; 15.; 25.; 10.; 25.; 20.|]
      (transferred_corner vertex_owned_one))
     || transferred_corner vertex_owned_one
        <> transferred_corner vertex_owned_many then
    fail "vertex closest-surface transfer/domain determinism";
  let one_surface_vertex = Group.init ~owner:Group.Vertex ~name:"one" 3
      (fun vertex -> vertex = 0) in
  let any_corner_surface = Surface_index.create ~vertices:one_surface_vertex
      ~vertex_selection:Surface_index.Any_triangle_vertex surface_source
      |> get_ok in
  if Surface_index.triangle_count any_corner_surface <> 1 then
    fail "surface index any-corner vertex restriction";
  let all_corner_surface = Surface_index.create ~vertices:one_surface_vertex
      ~vertex_selection:Surface_index.All_triangle_vertices surface_source
      |> get_ok in
  if Surface_index.triangle_count all_corner_surface <> 0 then
    fail "surface index all-corners vertex restriction";
  let initial_surface_weight = Attribute.create_owned ~name:"sampled_weight"
      ~owner:Attribute.Point (Attribute.Float [|99.; 99.|]) |> get_ok
  and initial_corner = Attribute.create_owned ~name:"sampled_corner"
      ~owner:Attribute.Point (Attribute.Float [|88.; 88.|]) |> get_ok
  and initial_piece = Attribute.create_owned ~name:"sampled_piece"
      ~owner:Attribute.Point (Attribute.Int [|6; 6|]) |> get_ok
  and initial_distance = Attribute.create_owned ~name:"surface_distance"
      ~owner:Attribute.Point (Attribute.Float [|77.; 77.|]) |> get_ok in
  let duplicate_geometry = Geometry.create ~positions:surface_positions
      ~topology:(Tb.freeze (let builder =
        Tb.create ~point_count:3 () in
        Tb.add_triangle builder 0 1 2;
        Tb.add_triangle builder 0 1 2; builder)) () |> get_ok in
  let restricted_piece = Attribute.create_owned ~name:"piece"
      ~owner:Attribute.Primitive (Attribute.Int [|7; 9|]) |> get_ok in
  let restricted_source = Geometry.with_attribute restricted_piece
      duplicate_geometry |> get_ok
  and restricted_target = Line_geometry.points [|(0.5,0.5,1.); (0.5,0.5,1.)|] in
  let restricted_target = List.fold_left (fun geometry attribute ->
    Geometry.with_attribute attribute geometry |> get_ok) restricted_target
      [initial_surface_weight;initial_corner;initial_piece;initial_distance] in
  let restricted_target_bytes = geometry_bytes restricted_target in
  let target_first = Group.init ~owner:Group.Point ~name:"first" 2
      (fun point -> point = 0) in
  let surface_attributes = List.map (fun (source_owner, source_name, target_name) ->
    Attribute_ops.{source_owner;source_name;target_name}) Attribute.[
      Point,"weight","sampled_weight";
      Vertex,"corner_value","sampled_corner";
      Primitive,"piece","sampled_piece"] in
  let sampled domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_surface ~grain:1 ~target_elements:target_first
      ~distance_attribute:"surface_distance" ~attributes:surface_attributes
      ~source:surface_source ~target:restricted_target () |> get_ok) in
  let sampled_one = sampled 1 and sampled_many = sampled 4 in
  check (geometry_bytes sampled_one = geometry_bytes sampled_many
      && geometry_bytes restricted_target = restricted_target_bytes)
    "surface transfer selected targets/domain exactness/unchanged input";
  List.iter (fun (name, storage) ->
    check (Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
        name sampled_one |> Option.get) = storage)
      ("surface transfer interpolation/unselected value for " ^ name)) Attribute.[
      "sampled_weight",Float [|1.5;99.|];
      "sampled_corner",Float [|17.5;88.|];
      "sampled_piece",Int [|7;6|];
      "surface_distance",Float [|1.;77.|]];
  let source_second = Group.init ~owner:Group.Primitive ~name:"second" 2
      (fun primitive -> primitive = 1) in
  let restricted_sample domains = Parallel.run ~domains (fun () ->
    Attribute_ops.transfer_surface ~grain:1 ~source_primitives:source_second
      ~target_elements:target_first
      ~attributes:[Attribute_ops.{source_owner=Attribute.Primitive;
        source_name="piece";target_name="sampled_piece"}]
      ~source:restricted_source ~target:restricted_target () |> get_ok) in
  let restricted_one = restricted_sample 1
  and restricted_many = restricted_sample 4 in
  check (geometry_bytes restricted_one = geometry_bytes restricted_many
      && geometry_bytes restricted_target = restricted_target_bytes
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "sampled_piece" restricted_one |> Option.get) = Attribute.Int [|9;6|]
      && Attribute.storage (Geometry.find_attribute ~owner:Attribute.Point
          "surface_distance" restricted_one |> Option.get)
        = Attribute.Float [|77.;77.|])
    "surface transfer source primitive restriction overrides coincident tie";
  let degenerate_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 1.; 2.|] ~y:[|0.; 0.; 0.|] ~z:[|0.; 0.; 0.|] in
  let degenerate_topology = Tb.create ~point_count:3 () in
  Tb.add_triangle degenerate_topology 0 1 2;
  let degenerate_surface = Geometry.create ~positions:degenerate_positions
      ~topology:(Tb.freeze degenerate_topology) () |> get_ok in
  (match Surface_index.create degenerate_surface with
   | Error error when Error.code error = "invalid_surface" -> ()
   | _ -> fail "surface index accepted a degenerate triangle");
  let tiny = 1e-100 in
  let tiny_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; tiny; 0.|] ~y:[|0.; 0.; tiny|] ~z:[|0.; 0.; 0.|] in
  let tiny_topology = Tb.create ~point_count:3 () in
  Tb.add_triangle tiny_topology 0 1 2;
  let tiny_surface = Geometry.create ~positions:tiny_positions
      ~topology:(Tb.freeze tiny_topology) () |> get_ok in
  let tiny_index = Surface_index.create tiny_surface |> get_ok in
  if Surface_index.triangle_count tiny_index <> 1 then
    fail "surface index rejected an exact non-collinear subnormal-area triangle";
  let surface_cancel = Cancel.create () in
  Cancel.cancel surface_cancel;
  (match Surface_index.create ~cancel:surface_cancel surface_source with
   | Error error when Error.code error = "cancelled" -> ()
   | _ -> fail "cancelled surface index construction returned the wrong result");
  let quad_surface_weight = Attribute.create_owned ~name:"quad_weight"
      ~owner:Attribute.Point (Attribute.Float [|0.; 1.; 2.; 3.|]) |> get_ok
  and quad_surface_corner = Attribute.create_owned ~name:"quad_corner"
      ~owner:Attribute.Vertex (Attribute.Float [|10.; 20.; 30.; 40.|]) |> get_ok in
  let quad_surface = geometry
      |> Geometry.with_attribute quad_surface_weight |> get_ok
      |> Geometry.with_attribute quad_surface_corner |> get_ok in
  let quad_index = Surface_index.create quad_surface |> get_ok in
  if Surface_index.triangle_count quad_index <> 2 then
    fail "surface index did not triangulate a quad exactly";
  let quad_target = Line_geometry.points [|(0.2, 0.7, 1.); (0.8, 0.7, 1.)|] in
  let quad_transfer = Attribute_ops.transfer_surface ~grain:1
      ~attributes:Attribute_ops.[
        {source_owner = Attribute.Point; source_name = "quad_weight";
         target_name = "quad_weight"};
        {source_owner = Attribute.Vertex; source_name = "quad_corner";
         target_name = "quad_corner"}]
      ~source:quad_surface ~target:quad_target () |> get_ok in
  let surface_float name geometry = Geometry.find_attribute
      ~owner:Attribute.Point name geometry |> Option.get
      |> Attribute.get (Attribute.key ~name ~owner:Attribute.Point Attribute.float)
      |> Option.get in
  if not (near_array [|2.3; 1.9|] (surface_float "quad_weight" quad_transfer))
     || not (near_array [|33.; 29.|] (surface_float "quad_corner" quad_transfer))
  then fail "general-polygon point/vertex surface interpolation";
  let concave_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 2.; 2.; 1.; 0.|] ~y:[|0.; 0.; 2.; 1.; 2.|]
      ~z:(Array.make 5 0.) in
  let concave_topology = Tb.create ~point_count:5 () in
  Tb.add_polygon concave_topology [|0; 1; 2; 3; 4|];
  let concave_geometry = Geometry.create ~positions:concave_positions
      ~topology:(Tb.freeze concave_topology) () |> get_ok in
  let concave_index = Surface_index.create concave_geometry |> get_ok in
  if Surface_index.triangle_count concave_index <> 3 then
    fail "surface index concave polygon triangle cardinality";
  (match Surface_index.raycast concave_index ~origin:(Rays_math.Vec3.create 0.4 1.4 0.75)
      ~direction:(Rays_math.Vec3.neg Rays_math.Vec3.unit_z) with
   | Ok (Some hit) when hit.primitive = 0 && near hit.distance 0.75 -> ()
   | _ -> fail "surface index missed a concave polygon interior");
  let bow_tie_positions = Packed.Float3.View.of_owned_exn
      ~x:[|0.; 2.; 0.; 2.|] ~y:[|0.; 2.; 2.; 0.|]
      ~z:(Array.make 4 0.) in
  let bow_tie_topology = Tb.create ~point_count:4 () in
  Tb.add_polygon bow_tie_topology [|0; 1; 2; 3|];
  let bow_tie = Geometry.create ~positions:bow_tie_positions
      ~topology:(Tb.freeze bow_tie_topology) () |> get_ok in
  (match Surface_index.create bow_tie with
   | Error error when Error.code error = "invalid_surface" -> ()
   | _ -> fail "surface index accepted a self-intersecting polygon");
  let bad_positions = Packed.Float3.View.of_owned_exn
      ~x:[|Float.nan|] ~y:[|0.|] ~z:[|0.|] in
  (match Spatial_index.create bad_positions with
   | Error error when Error.code error = "invalid_position" -> ()
   | _ -> fail "spatial index accepted a non-finite point");
  let conflicting_id = Attribute.create_owned ~name:"id" ~owner:Attribute.Point
      (Attribute.Float (Array.make 3 0.)) |> get_ok in
  let conflicting_target = Geometry.with_attribute conflicting_id
      transfer_target |> get_ok in
  (match Attribute_ops.transfer_points ~names:["id"] ~source:transfer_source
      ~target:conflicting_target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "attribute transfer accepted incompatible target storage");
  (match Attribute_ops.transfer_points
      ~mode:(Attribute_ops.Inverse_distance { neighbors = 0; power = 1. })
      ~source:transfer_source ~target:transfer_target () with
   | Error error when Error.code error = "invalid_transfer" -> ()
   | _ -> fail "attribute transfer accepted invalid neighbor count");
  (transfer_source, transfer_target)
