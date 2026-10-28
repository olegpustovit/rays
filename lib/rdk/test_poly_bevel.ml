open Rdk_test_support
open Rays
open Rdk

let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let equal_storage left right =
  match Attribute.Private.storage left, Attribute.Private.storage right with
  | Attribute.Float left, Attribute.Float right -> left = right
  | Attribute.Int left, Attribute.Int right -> left = right
  | Attribute.Text left, Attribute.Text right -> left = right
  | Attribute.Int_array left, Attribute.Int_array right ->
      let left = Packed.Int_array.Private.view left
      and right = Packed.Int_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float_array left, Attribute.Float_array right ->
      let left = Packed.Float_array.Private.view left
      and right = Packed.Float_array.Private.view right in
      left.offsets = right.offsets && left.values = right.values
  | Attribute.Float2 left, Attribute.Float2 right ->
      let left = Packed.Float2.Private.view left
      and right = Packed.Float2.Private.view right in
      left.x = right.x && left.y = right.y
  | Attribute.Float3 left, Attribute.Float3 right ->
      let left = Packed.Float3.Private.view left
      and right = Packed.Float3.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
  | Attribute.Float4 left, Attribute.Float4 right ->
      let left = Packed.Float4.Private.view left
      and right = Packed.Float4.Private.view right in
      left.x = right.x && left.y = right.y && left.z = right.z
      && left.w = right.w
  | _ -> false

let equal_group left right =
  Group.owner left = Group.owner right
  && String.equal (Group.name left) (Group.name right)
  && Group.ordered_elements left = Group.ordered_elements right
  && Group.length left = Group.length right
  && let same = ref true in
     for element = 0 to Group.length left - 1 do
       if Group.mem element left <> Group.mem element right then same := false
     done;
     !same

let equal_edge_group left right =
  String.equal (Edge_group.name left) (Edge_group.name right)
  && Edge_group.length left = Edge_group.length right
  && let same = ref true in
     for edge = 0 to Edge_group.length left - 1 do
       if Edge_group.mem edge left <> Edge_group.mem edge right then same := false
     done;
     !same

let equal_geometry left right =
  let lp = Packed.Float3.Private.view (Geometry.positions left)
  and rp = Packed.Float3.Private.view (Geometry.positions right)
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp.x = rp.x && lp.y = rp.y && lp.z = rp.z
  && lt.point_count = rt.point_count
  && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.equal (fun left right -> Attribute.owner left = Attribute.owner right
      && String.equal (Attribute.name left) (Attribute.name right)
      && equal_storage left right)
      (Geometry.attributes left) (Geometry.attributes right)
  && List.equal equal_group (Geometry.groups left) (Geometry.groups right)
  && List.equal equal_edge_group (Geometry.edge_groups left)
      (Geometry.edge_groups right)

let box () =
  Box_generator.box ~connectivity:Box_generator.Box_quads ~consolidate_points:true
    ~normals:Box_generator.Box_no_normals ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_rdk

let all_edges geometry name =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  Edge_group.init ~grain:1 ~topology ~index ~name (Fun.const true)

let edge_pair geometry name a b =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  let selected = Topology_index.find_edge_index index ~a ~b in
  check (selected >= 0) "PolyBevel test edge is absent";
  Edge_group.init ~grain:1 ~topology ~index ~name (fun edge -> edge = selected)

let edge_ids geometry name selected =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  Edge_group.init ~grain:1 ~topology ~index ~name (fun edge ->
    Array.mem edge selected)

let with_payload geometry =
  let points = Geometry.point_count geometry
  and vertices = Geometry.vertex_count geometry
  and primitives = Geometry.primitive_count geometry in
  let attribute owner name storage =
    Attribute.create_owned ~owner ~name storage |> Result.get_ok in
  let point_order = Group.ordered ~owner:Group.Point ~name:"point_order"
      ~length:points (Array.init points (fun index -> points - index - 1))
      |> Result.get_ok
  and corner_group = Group.init ~grain:1 ~owner:Group.Vertex ~name:"corners"
      vertices (fun vertex -> vertex land 1 = 0)
  and face_order = Group.ordered ~owner:Group.Primitive ~name:"face_order"
      ~length:primitives (Array.init primitives Fun.id) |> Result.get_ok in
  Geometry.create ~positions:(Geometry.positions geometry)
    ~topology:(Geometry.topology geometry)
    ~attributes:[
      attribute Attribute.Point "id" (Attribute.Int (Array.init points Fun.id));
      attribute Attribute.Point "pscale" (Attribute.Float
        (Array.init points (fun point -> if point land 1 = 0 then 1. else 0.5)));
      attribute Attribute.Vertex "corner_value"
        (Attribute.Float (Array.init vertices float_of_int));
      attribute Attribute.Vertex "corner_name"
        (Attribute.Text (Array.init vertices string_of_int));
      attribute Attribute.Primitive "material"
        (Attribute.Int (Array.init primitives Fun.id));
      attribute Attribute.Detail "tag" (Attribute.Text [|"bevel"|]);
    ] ~groups:[point_order; corner_group; face_order]
    ~edge_groups:[all_edges geometry "source_edges"] () |> Result.get_ok

let validate_closed_manifold geometry =
  let topology = Geometry.topology geometry in
  let index = Topology_index.create topology in
  for edge = 0 to Topology_index.edge_count index - 1 do
    let incidence = Topology_index.edge_incidence_count index edge in
    if incidence <> 2 then begin
      let a, b = Topology_index.edge_points index edge in
      let owners = Array.init incidence (fun local ->
        Topology_index.edge_vertex index ~edge ~local
        |> Topology_index.primitive_of_vertex index) in
      let view = Topology.Private.view topology in
      Array.iter (fun primitive ->
        Printf.eprintf "edge-owner %d:" primitive;
        for vertex = view.primitive_offsets.(primitive)
            to view.primitive_offsets.(primitive + 1) - 1 do
          Printf.eprintf " %d" view.vertex_points.(vertex)
        done;
        Printf.eprintf "\n%!") owners;
      fail (Printf.sprintf
        "PolyBevel output edge %d (%d,%d) has incidence %d instead of 2 (primitives %s)"
        edge a b incidence
        (String.concat "," (Array.to_list (Array.map string_of_int owners))))
    end
  done;
  let used = Bytes.make (Geometry.point_count geometry) '\000' in
  let view = Topology.Private.view topology in
  Array.iter (fun point -> Bytes.set used point '\001') view.vertex_points;
  for point = 0 to Geometry.point_count geometry - 1 do
    check (Bytes.get used point <> '\000') "PolyBevel left an unused point"
  done

let test_all_edges_and_profiles () =
  let source = with_payload (box ()) in
  let edges = Geometry.find_edge_group "source_edges" source |> Option.get in
  let chamfer = Poly_bevel.run ~grain:3 ~edges ~distance:0.2
      ~edge_group:"edge_fillets" ~corner_group:"corner_fillets"
      ~offset_group:"offset_edges" source |> get_rdk in
  check (Geometry.point_count chamfer = 24
      && Geometry.primitive_count chamfer = 26
      && Geometry.vertex_count chamfer = 96)
    "PolyBevel all-edge chamfer cardinality";
  validate_closed_manifold chamfer;
  let edge_faces = Geometry.find_group ~owner:Group.Primitive "edge_fillets"
      chamfer |> Option.get
  and corner_faces = Geometry.find_group ~owner:Group.Primitive "corner_fillets"
      chamfer |> Option.get
  and offsets = Geometry.find_edge_group "offset_edges" chamfer |> Option.get
  and inherited = Geometry.find_edge_group "source_edges" chamfer |> Option.get in
  check (Group.cardinality edge_faces = 12
      && Group.cardinality corner_faces = 8
      && Edge_group.cardinality offsets = 24
      && Edge_group.cardinality inherited = 24)
    "PolyBevel generated/source group ancestry";
  let detail_source = Geometry.find_attribute ~owner:Attribute.Detail "tag" source
      |> Option.get
  and detail_output = Geometry.find_attribute ~owner:Attribute.Detail "tag" chamfer
      |> Option.get in
  check (detail_source == detail_output) "PolyBevel did not share detail payload";
  let round = Poly_bevel.run ~grain:2 ~edges
      ~shape:(Poly_bevel.Bevel_round { convexity = 1. }) ~divisions:3
      ~point_scale_attribute:"pscale" ~distance:0.3
      ~edge_group:"edge_fillets" ~corner_group:"corner_fillets"
      ~offset_group:"offset_edges" source |> get_rdk in
  check (Geometry.point_count round = 72
      && Geometry.primitive_count round = 50
      && Geometry.vertex_count round = 240)
    "PolyBevel divided round cardinality";
  validate_closed_manifold round;
  check (Group.cardinality (Geometry.find_group ~owner:Group.Primitive
      "edge_fillets" round |> Option.get) = 36
      && Edge_group.cardinality (Geometry.find_edge_group "offset_edges" round
        |> Option.get) = 24
      && Edge_group.cardinality (Geometry.find_edge_group "source_edges" round
        |> Option.get) = 48)
    "PolyBevel divided group ancestry";
  let positions = Packed.Float3.Private.view (Geometry.positions round) in
  for point = 0 to Geometry.point_count round - 1 do
    check (Float.is_finite positions.x.(point)
        && Float.is_finite positions.y.(point)
        && Float.is_finite positions.z.(point))
      "PolyBevel round profile produced a non-finite position"
  done

let test_partial_network_and_clamping () =
  let source = box () in
  let selected = edge_pair source "one" 0 1 in
  let one = Poly_bevel.run ~grain:1 ~edges:selected ~distance:0.25 source
      |> get_rdk in
  check (Geometry.point_count one = 12
      && Geometry.primitive_count one = 9
      && Geometry.vertex_count one = 38)
    "PolyBevel single-edge endpoint patches";
  validate_closed_manifold one;
  let huge = Poly_bevel.run ~grain:1 ~edges:selected ~distance:1e200 source
      |> get_rdk in
  let positions = Packed.Float3.Private.view (Geometry.positions huge) in
  for point = 0 to Geometry.point_count huge - 1 do
    check (Float.is_finite positions.x.(point)
        && Float.is_finite positions.y.(point)
        && Float.is_finite positions.z.(point))
      "PolyBevel overlap clamp failed on a huge cutback"
  done;
  validate_closed_manifold huge

let test_connected_network_flat_filter_and_normals () =
  let source = box () in
  let index = Topology_index.create (Geometry.topology source) in
  check (Topology_index.point_edge_count index 0 >= 2)
    "PolyBevel connected-network fixture valence";
  let selected = edge_ids source "corner_pair"
      [|Topology_index.point_edge index ~point:0 ~local:0;
        Topology_index.point_edge index ~point:0 ~local:1|] in
  let connected = Poly_bevel.run ~grain:2 ~edges:selected ~divisions:2
      ~shape:(Poly_bevel.Bevel_round { convexity = 0.5 }) ~distance:0.18
      ~edge_group:"edges" ~corner_group:"corners" source |> get_rdk in
  validate_closed_manifold connected;
  check (Group.cardinality (Geometry.find_group ~owner:Group.Primitive "edges"
      connected |> Option.get) = 4
      && Group.cardinality (Geometry.find_group ~owner:Group.Primitive "corners"
        connected |> Option.get) = 2)
    "PolyBevel connected edge-run corner planning";
  let divided = Box_generator.box ~connectivity:Box_generator.Box_quads ~consolidate_points:true
      ~normals:Box_generator.Box_point_normals ~x_divisions:3 ~y_divisions:2 ~z_divisions:2
      ~size:(Rays_math.Vec3.create 2. 2. 2.) () |> get_rdk in
  let all = all_edges divided "all" in
  let output = Poly_bevel.run ~grain:7 ~edges:all ~ignore_flat_angle:0.
      ~distance:0.12 divided |> get_rdk in
  validate_closed_manifold output;
  (match Geometry.find_attribute ~owner:Attribute.Point "N" output with
   | Some normal when Attribute.length normal = Geometry.point_count output -> ()
   | _ -> fail "PolyBevel did not recompute an existing point normal")

let test_exclusions_identity_and_validation () =
  let source = box () in
  let all = all_edges source "all" in
  check (Poly_bevel.run ~edges:all ~distance:0. source |> get_rdk == source)
    "PolyBevel zero distance did not preserve identity";
  let open_grid = Plane_generators.grid ~counts:Plane_generators.Grid_point_counts
      ~connectivity:Plane_generators.Grid_quads ~columns:2 ~rows:2 ~size:2. () |> get_rdk in
  let boundary = edge_pair open_grid "boundary" 0 1 in
  check (Poly_bevel.run ~edges:boundary ~distance:0.2 open_grid |> get_rdk
      == open_grid) "PolyBevel did not ignore a boundary edge";
  let flat = Plane_generators.grid ~counts:Plane_generators.Grid_point_counts
      ~connectivity:Plane_generators.Grid_alternating_triangles ~columns:2 ~rows:2 ~size:2. ()
      |> get_rdk in
  let flat_index = Topology_index.create (Geometry.topology flat) in
  let interior = ref (-1) in
  for edge = 0 to Topology_index.edge_count flat_index - 1 do
    if Topology_index.edge_incidence_count flat_index edge = 2 then interior := edge
  done;
  let flat_group = Edge_group.init ~topology:(Geometry.topology flat)
      ~index:flat_index ~name:"flat" (fun edge -> edge = !interior) in
  check (Poly_bevel.run ~edges:flat_group ~ignore_flat_angle:0.
      ~distance:0.2 flat |> get_rdk == flat)
    "PolyBevel flat-edge exclusion did not preserve identity";
  let expect code = function
    | Error error when String.equal (Error.code error) code -> ()
    | Error error -> fail ("unexpected PolyBevel error: " ^ Error.to_string error)
    | Ok _ -> fail ("expected PolyBevel error " ^ code) in
  expect "invalid_topology" (Poly_bevel.run ~divisions:0 ~distance:0.1 source);
  expect "invalid_topology" (Poly_bevel.run
    ~shape:(Poly_bevel.Bevel_round { convexity = 2. }) ~distance:0.1 source);
  expect "invalid_topology" (Poly_bevel.run ~ignore_flat_angle:(-0.1)
    ~distance:0.1 source);
  expect "invalid_topology" (Poly_bevel.run ~edges:flat_group
    ~distance:0.1 source);
  let negative_scale = Attribute.create_owned ~owner:Attribute.Point
      ~name:"bad_scale" (Attribute.Float
        (Array.init (Geometry.point_count source) (fun point ->
          if point = 3 then -1. else 1.))) |> Result.get_ok in
  let negative_source = Geometry.with_attribute negative_scale source
      |> Result.get_ok in
  expect "invalid_topology" (Poly_bevel.run ~point_scale_attribute:"bad_scale"
    ~distance:0.1 negative_source);
  let zero_scale = Attribute.create_owned ~owner:Attribute.Point
      ~name:"zero_scale" (Attribute.Float
        (Array.init (Geometry.point_count source) (fun point ->
          if point = 0 then 0. else 1.))) |> Result.get_ok in
  let zero_source = Geometry.with_attribute zero_scale source |> Result.get_ok in
  let zero = Poly_bevel.run ~point_scale_attribute:"zero_scale"
      ~distance:0.1 zero_source |> get_rdk in
  validate_closed_manifold zero;
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  expect "cancelled" (Poly_bevel.run ~cancel:cancelled ~distance:0.1 source)

let test_parallel_exact () =
  let source = with_payload (box ()) in
  let edges = Geometry.find_edge_group "source_edges" source |> Option.get in
  let cook domains = Parallel.run ~domains (fun () ->
    Poly_bevel.run ~grain:1 ~edges ~shape:(Poly_bevel.Bevel_round { convexity = 0.8 })
      ~divisions:5 ~point_scale_attribute:"pscale" ~distance:0.24
      ~edge_group:"edge_fillets" ~corner_group:"corner_fillets"
      ~offset_group:"offset_edges" source |> get_rdk) in
  check (equal_geometry (cook 1) (cook 4))
    "PolyBevel one/four-domain output differs"

(* Every emitted strip and corner patch winds like the faces it joins: a
   chamfered closed box keeps outward polygon normals and (nearly) its
   volume. *)
let test_outward_winding () =
  List.iter (fun (shape, divisions) ->
    let output = Poly_bevel.run ~grain:1 ~shape ~divisions ~distance:0.1 (box ()) |> get_rdk in
    let topology = Geometry.topology output and positions = Geometry.positions output in
    let volume = ref 0. in
    for primitive = 0 to Geometry.primitive_count output - 1 do
      let first, last = Topology.primitive_vertex_range topology primitive in
      let count = last - first in
      let point i = Packed.Float3.get positions
          (Topology.point_of_vertex topology (first + (i mod count))) in
      let nx = ref 0. and ny = ref 0. and nz = ref 0.
      and cx = ref 0. and cy = ref 0. and cz = ref 0. in
      for i = 0 to count - 1 do
        let ax, ay, az = point i and bx, by, bz = point (i + 1) in
        nx := !nx +. ((ay -. by) *. (az +. bz));
        ny := !ny +. ((az -. bz) *. (ax +. bx));
        nz := !nz +. ((ax -. bx) *. (ay +. by));
        cx := !cx +. ax; cy := !cy +. ay; cz := !cz +. az
      done;
      check ((!nx *. !cx) +. (!ny *. !cy) +. (!nz *. !cz) > 0.)
        (Printf.sprintf "PolyBevel primitive %d winds inward" primitive);
      (* Fan triangulation for the signed volume. *)
      let ox, oy, oz = point 0 in
      for i = 1 to count - 2 do
        let ax, ay, az = point i and bx, by, bz = point (i + 1) in
        volume := !volume +. ((ox *. ((ay *. bz) -. (az *. by)))
          +. (oy *. ((az *. bx) -. (ax *. bz))) +. (oz *. ((ax *. by) -. (ay *. bx)))) /. 6.
      done
    done;
    check (!volume > 7.6 && !volume < 8.)
      (Printf.sprintf "PolyBevel chamfered box volume %.3f is not just under 8" !volume))
    [Poly_bevel.Bevel_chamfer, 1; Poly_bevel.Bevel_round { convexity = 0.5 }, 3]

let run () =
  test_outward_winding ();
  test_all_edges_and_profiles ();
  test_partial_network_and_clamping ();
  test_connected_network_flat_filter_and_normals ();
  test_exclusions_identity_and_validation ();
  test_parallel_exact ();
  print_endline "poly bevel tests passed"
