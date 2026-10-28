open Rdk
open Rdk_test_support

let fail format = Printf.ksprintf failwith format
let check condition message = if not condition then fail "%s" message
let get = function Ok value -> value | Error error -> fail "%s" (Error.to_string error)
let get_string = function Ok value -> value | Error message -> fail "%s" message

let geometry points vertex_points primitive_offsets =
  let count = Array.length points in
  let x =
    Array.init count (fun point ->
        let x, _, _ = points.(point) in
        x)
  and y =
    Array.init count (fun point ->
        let _, y, _ = points.(point) in
        y)
  and z =
    Array.init count (fun point ->
        let _, _, z = points.(point) in
        z)
  in
  let topology =
    Topology.polygons_owned ~point_count:count ~vertex_points ~primitive_offsets |> get_string
  in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn ~x ~y ~z) ~topology () |> get_string

let triangles points indices =
  geometry points indices
    (Array.init ((Array.length indices / 3) + 1) (fun primitive -> primitive * 3))

let tetra ~origin:(ox, oy, oz) size =
  triangles
    [| (ox, oy, oz); (ox +. size, oy, oz); (ox, oy +. size, oz); (ox, oy, oz +. size) |]
    [| 0; 2; 1; 0; 1; 3; 1; 2; 3; 2; 0; 3 |]

let tetra_batch count shift =
  let points = Array.make (count * 4) (0., 0., 0.)
  and indices = Array.make (count * 12) 0
  and local = [| 0; 2; 1; 0; 1; 3; 1; 2; 3; 2; 0; 3 |] in
  for item = 0 to count - 1 do
    let first = item * 4 and x = (float_of_int item *. 6.) +. shift in
    points.(first) <- (x, 0., 0.);
    points.(first + 1) <- (x +. 2., 0., 0.);
    points.(first + 2) <- (x, 2., 0.);
    points.(first + 3) <- (x, 0., 2.);
    for corner = 0 to 11 do
      indices.((item * 12) + corner) <- first + local.(corner)
    done
  done;
  triangles points indices

let cube_quads ~origin:(ox, oy, oz) size =
  geometry
    [|
      (ox, oy, oz);
      (ox +. size, oy, oz);
      (ox +. size, oy +. size, oz);
      (ox, oy +. size, oz);
      (ox, oy, oz +. size);
      (ox +. size, oy, oz +. size);
      (ox +. size, oy +. size, oz +. size);
      (ox, oy +. size, oz +. size);
    |]
    [| 0; 3; 2; 1; 4; 5; 6; 7; 0; 1; 5; 4; 3; 7; 6; 2; 0; 4; 7; 3; 1; 2; 6; 5 |]
    [| 0; 4; 8; 12; 16; 20; 24 |]

let signature geometry =
  let positions = Packed.Float3.View.view (Geometry.positions geometry)
  and topology = Topology.View.view (Geometry.topology geometry) in
  ( Array.copy positions.x,
    Array.copy positions.y,
    Array.copy positions.z,
    Array.copy topology.vertex_points,
    Array.copy topology.primitive_offsets,
    Bytes.copy topology.primitive_kinds )

let with_point_weight value geometry =
  let attribute =
    Attribute.create_owned ~owner:Attribute.Point ~name:"weight"
      (Attribute.Float (Array.make (Geometry.point_count geometry) value))
    |> get_string
  in
  Geometry.with_attribute attribute geometry |> get_string

let with_vertex_normals ?(cusp_angle = Float.pi) geometry =
  Normal_ops_front.run ~grain:1 ~owner:Attribute.Vertex ~cusp_angle geometry |> get

let test_solid_products () =
  let left = tetra ~origin:(0., 0., 0.) 2. and right = tetra ~origin:(0.5, 0.2, 0.2) 2. in
  let run domains =
    Parallel.run ~domains (fun () ->
        [|
          Boolean_solid.Union;
          Boolean_solid.Intersection;
          Boolean_solid.Difference;
          Boolean_solid.Reverse_difference;
          Boolean_solid.Xor;
        |]
        |> Array.map (fun operation ->
            Boolean_front.run ~grain:1 ~operation ~right left |> get |> signature))
  in
  let products = run 1 in
  check
    (Array.for_all (fun (_, _, _, vertices, _, _) -> Array.length vertices > 0) products)
    "public Boolean lost a non-empty solid product";
  check (products = run 4) "public solid Boolean differs between one and four domains"

let test_surface_cut_and_payload () =
  let left = cube_quads ~origin:(0., 0., 0.) 2. |> with_point_weight 1.
  and right =
    triangles
      [| (-1., 1., 0.5); (3., 1., 0.5); (-1., 1., 1.5); (3., 1., 1.5) |]
      [| 0; 1; 2; 1; 3; 2 |]
    |> with_point_weight 2.
  in
  let output =
    Boolean_front.run ~grain:1 ~operation:Boolean_solid.Difference
      ~right_treatment:Boolean_solid.Surface ~right left
    |> get
  in
  check
    (Geometry.primitive_count output > 12)
    "public solid-minus-surface Boolean lost its cut walls";
  check
    (Geometry.find_attribute ~owner:Attribute.Point "weight" output = None
    && Geometry.find_attribute ~owner:Attribute.Vertex "weight" output <> None)
    "public Boolean did not promote conflicting point payload";
  match
    Boolean_front.run ~grain:1 ~operation:Boolean_solid.Difference
      ~right_treatment:Boolean_solid.Surface ~point_conflict:Boolean_payload.Reject ~right left
  with
  | Error error when Error.code error = "point_payload_conflict" -> ()
  | Error error -> fail "unexpected public point conflict: %s" (Error.to_string error)
  | Ok _ -> fail "public Boolean ignored its reject point-conflict policy"

let test_normal_payload_orientation () =
  let left = cube_quads ~origin:(0., 0., 0.) 2. |> with_vertex_normals ~cusp_angle:0.1
  and right =
    triangles
      [| (-1., 1., -1.); (3., 1., -1.); (-1., 1., 3.); (3., 1., 3.) |]
      [| 0; 1; 2; 1; 3; 2 |]
    |> with_vertex_normals
  in
  let run domains =
    Parallel.run ~domains (fun () ->
        Boolean_front.run ~grain:1 ~operation:Boolean_solid.Difference
          ~right_treatment:Boolean_solid.Surface ~right left
        |> get)
  in
  let output = run 1 and parallel = run 4 in
  let normal_storage geometry =
    Geometry.find_attribute ~owner:Attribute.Vertex "N" geometry |> Option.get |> Attribute.storage
  in
  check
    (signature output = signature parallel)
    "Boolean normal topology differs between one and four domains";
  let sequential_normals = normal_storage output and parallel_normals = normal_storage parallel in
  let normal_view = function
    | Attribute.Float3 values -> Packed.Float3.View.view values
    | _ -> fail "Boolean vertex normal payload is not float3"
  in
  let a = normal_view sequential_normals and b = normal_view parallel_normals in
  if a.x <> b.x || a.y <> b.y || a.z <> b.z then begin
    let differences = ref 0 and maximum = ref 0. in
    for index = 0 to Array.length a.x - 1 do
      List.iter
        (fun delta ->
          if delta <> 0. then begin
            incr differences;
            maximum := max !maximum delta
          end)
        [
          abs_float (a.x.(index) -. b.x.(index));
          abs_float (a.y.(index) -. b.y.(index));
          abs_float (a.z.(index) -. b.z.(index));
        ]
    done;
    fail "Boolean normal payload differs between one and four domains (%d components, max %.17g)"
      !differences !maximum
  end;
  let normals = normal_storage output in
  let normals =
    match normals with
    | Attribute.Float3 values -> Packed.Float3.View.view values
    | _ -> fail "Boolean vertex normal payload is not float3"
  in
  let positions = Packed.Float3.View.view (Geometry.positions output)
  and topology = Topology.View.view (Geometry.topology output) in
  for primitive = 0 to Geometry.primitive_count output - 1 do
    let first = topology.primitive_offsets.(primitive)
    and last = topology.primitive_offsets.(primitive + 1) in
    check (last - first = 3) "Boolean normal orientation regression expected triangular output";
    let point vertex = topology.vertex_points.(vertex) in
    let a = point first and b = point (first + 1) and c = point (first + 2) in
    let ux = positions.x.(b) -. positions.x.(a)
    and uy = positions.y.(b) -. positions.y.(a)
    and uz = positions.z.(b) -. positions.z.(a)
    and vx = positions.x.(c) -. positions.x.(a)
    and vy = positions.y.(c) -. positions.y.(a)
    and vz = positions.z.(c) -. positions.z.(a) in
    let fx = (uy *. vz) -. (uz *. vy)
    and fy = (uz *. vx) -. (ux *. vz)
    and fz = (ux *. vy) -. (uy *. vx) in
    for vertex = first to last - 1 do
      let dot =
        (fx *. normals.x.(vertex)) +. (fy *. normals.y.(vertex)) +. (fz *. normals.z.(vertex))
      in
      check (dot > 1e-10) "Boolean transferred a normal opposite to its output winding"
    done
  done

let test_surface_fracture_pieces () =
  let left = cube_quads ~origin:(0., 0., 0.) 2.
  and right =
    triangles
      [| (-1., 1., -1.); (3., 1., -1.); (-1., 1., 3.); (3., 1., 3.) |]
      [| 0; 1; 2; 1; 3; 2 |]
  in
  let run domains =
    Parallel.run ~domains (fun () ->
        Boolean_front.run ~grain:1 ~operation:Boolean_solid.Difference
          ~right_treatment:Boolean_solid.Surface ~piece_attribute:"piece" ~require_closed:true
          ~right left
        |> get)
  in
  let output = run 1 in
  let values =
    Geometry.find_attribute ~owner:Attribute.Primitive "piece" output
    |> Option.get |> Attribute.storage
  in
  let values =
    match values with
    | Attribute.Int values -> values
    | _ -> fail "public Boolean fracture piece plane is not integer-valued"
  in
  check
    (Array.fold_left Int.max (-1) values = 1)
    "one surface cutter did not produce exactly two Boolean-cell pieces";
  let topology = Topology.View.view (Geometry.topology output) in
  for piece = 0 to 1 do
    let edges = Hashtbl.create 32 and primitives = ref 0 in
    for primitive = 0 to Geometry.primitive_count output - 1 do
      if values.(primitive) = piece then begin
        incr primitives;
        let first = topology.primitive_offsets.(primitive)
        and last = topology.primitive_offsets.(primitive + 1) in
        for vertex = first to last - 1 do
          let next = if vertex + 1 = last then first else vertex + 1 in
          let a = topology.vertex_points.(vertex) and b = topology.vertex_points.(next) in
          let edge = if a < b then (a, b) else (b, a) in
          Hashtbl.replace edges edge (1 + Option.value ~default:0 (Hashtbl.find_opt edges edge))
        done
      end
    done;
    check (!primitives > 1) "Boolean fracture assigned an individual polygon as a piece";
    Hashtbl.iter
      (fun _ incidence ->
        check (incidence = 2) "Boolean fracture piece is not a closed two-manifold shell")
      edges
  done;
  let values_for geometry =
    Geometry.find_attribute ~owner:Attribute.Primitive "piece" geometry
    |> Option.get |> Attribute.storage
  in
  check
    (values_for output = values_for (run 4))
    "Boolean fracture piece identities differ between one and four domains"

let test_shatter () =
  let left = tetra ~origin:(0., 0., 0.) 2. and right = tetra ~origin:(0.5, 0.2, 0.2) 2. in
  let run domains =
    Parallel.run ~domains (fun () ->
        let output =
          Boolean_front.run ~grain:1 ~operation:Boolean_solid.Shatter ~right left |> get
        in
        let groups =
          [| "boolean_left"; "boolean_overlap"; "boolean_right" |]
          |> Array.map (fun name ->
              let group = Geometry.find_group ~owner:Group.Primitive name output |> Option.get in
              Array.init (Geometry.primitive_count output) (fun primitive ->
                  Group.mem primitive group))
        in
        for primitive = 0 to Geometry.primitive_count output - 1 do
          let memberships =
            Array.fold_left (fun count group -> count + if group.(primitive) then 1 else 0) 0 groups
          in
          check (memberships = 1) "public Boolean shatter groups do not partition output primitives"
        done;
        check
          (Array.for_all (Array.exists Fun.id) groups)
          "public Boolean shatter lost a non-empty product";
        (signature output, groups))
  in
  check (run 1 = run 4) "public Boolean shatter differs between one and four domains";
  (match
     Boolean_front.run ~grain:1 ~operation:Boolean_solid.Shatter ~left_piece_group:(Some "same")
       ~overlap_piece_group:(Some "same") ~right left
   with
  | Error error when Error.code error = "invalid_parameter" -> ()
  | Error error -> fail "unexpected shatter-name error: %s" (Error.to_string error)
  | Ok _ -> fail "public Boolean shatter accepted duplicate group names");
  match
    Boolean_front.run ~grain:1 ~operation:Boolean_solid.Shatter
      ~right_treatment:Boolean_solid.Surface ~right left
  with
  | Error error when Error.code error = "invalid_parameter" -> ()
  | Error error -> fail "unexpected surface-shatter error: %s" (Error.to_string error)
  | Ok _ -> fail "public Boolean shatter accepted a surface operand"

let test_shatter_scale () =
  let left = tetra_batch 16 0. and right = tetra_batch 16 0.5 in
  let run domains =
    Parallel.run ~domains (fun () ->
        let output =
          Boolean_front.run ~grain:1 ~operation:Boolean_solid.Shatter ~right left |> get
        in
        let groups =
          [| "boolean_left"; "boolean_overlap"; "boolean_right" |]
          |> Array.map (fun name ->
              Geometry.find_group ~owner:Group.Primitive name output
              |> Option.get |> Group.cardinality)
        in
        (signature output, groups))
  in
  let one = run 1 in
  check
    (Array.for_all (fun count -> count > 0) (snd one))
    "multi-pair shatter lost a region product";
  check (one = run 4) "multi-pair shatter differs between one and four domains"

let test_output_policies () =
  let left = cube_quads ~origin:(0., 0., 0.) 1. and right = tetra ~origin:(10., 0., 0.) 1. in
  let polygons =
    Boolean_front.run ~grain:1 ~detriangulation:Boolean_materialization.Unchanged_polygons ~right
      left
    |> get
  in
  check
    (Geometry.primitive_count polygons = 10)
    "public Boolean unchanged-polygon detriangulation has wrong cardinality";
  let overlap = cube_quads ~origin:(0.5, 0.5, 0.5) 1. in
  let shared = Boolean_front.run ~grain:1 ~right:overlap left |> get
  and split =
    Boolean_front.run ~grain:1 ~seam_points:Boolean.Split_seam_points ~right:overlap left |> get
  in
  check
    (Geometry.point_count split > Geometry.point_count shared
    && Geometry.primitive_count split = Geometry.primitive_count shared)
    "public Boolean seam-point policy did not split topology components";
  let host =
    Box_generator.box ~grain:1
      ~size:(Rays_math.Vec3.create 2.8 2.2 2.2)
      ~connectivity:Box_generator.Box_quads ~consolidate_points:true ()
    |> get
  and cutter =
    Box_generator.box ~grain:1
      ~size:(Rays_math.Vec3.create 2.2 1.15 1.15)
      ~center:(Rays_math.Vec3.create 0.85 0. 0.)
      ~rotation:(Rays_math.Vec3.create 0.35 0.42 0.12)
      ~connectivity:Box_generator.Box_quads ~consolidate_points:true ()
    |> get
  in
  let cut =
    Boolean_front.run ~grain:1 ~operation:Boolean_solid.Difference
      ~detriangulation:Boolean_materialization.All_polygons ~right:cutter host
    |> get
  in
  ignore (Triangulate_front.run ~grain:1 cut |> get)

let test_errors () =
  let left = tetra ~origin:(0., 0., 0.) 1. and right = tetra ~origin:(0.25, 0.25, 0.25) 1. in
  (match Boolean_front.run ~grain:0 ~right left with
  | Error error when Error.code error = "invalid_parameter" -> ()
  | Error error -> fail "unexpected public Boolean grain error: %s" (Error.to_string error)
  | Ok _ -> fail "public Boolean accepted zero grain");
  (match Boolean_front.run ~grain:1 ~cleanup_max_batches:0 ~right left with
  | Error error when Error.code error = "invalid_parameter" -> ()
  | Error error -> fail "unexpected public cleanup-bound error: %s" (Error.to_string error)
  | Ok _ -> fail "public Boolean accepted a zero cleanup batch bound");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  match Boolean_front.run ~cancel ~grain:1 ~right left with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail "unexpected public Boolean cancellation: %s" (Error.to_string error)
  | Ok _ -> fail "cancelled public Boolean completed"

let test_bounded_cleanup () =
  let left = cube_quads ~origin:(0., 0., 0.) 2.
  and right =
    cube_quads
      ~origin:(Float.next_after 0.6 Float.infinity, 0.60000000000000009, 0.60000000000000009)
      2.
  in
  let threshold = 0.60000000000000009 in
  let run domains =
    Parallel.run ~domains (fun () ->
        Boolean_front.run ~grain:1 ~tiny_seam_threshold:threshold ~cleanup_max_batches:8
          ~strict_cleanup:false ~right left
        |> get |> signature)
  in
  check (run 1 = run 4) "public bounded Boolean cleanup differs between one and four domains"

let test_seam_products () =
  let left = tetra ~origin:(0., 0., 0.) 2. and right = tetra ~origin:(0.5, 0.2, 0.2) 2. in
  let run domains =
    Parallel.run ~domains (fun () ->
        let output = Boolean.seam ~grain:1 ~right left |> get in
        let between =
          Geometry.find_group ~owner:Group.Primitive "boolean_seam" output |> Option.get
        in
        check
          (Geometry.primitive_count output > 0 && Group.cardinality between > 0)
          "public Boolean seam lost its between-input curves";
        ( signature output,
          Array.init (Group.length between) (fun primitive -> Group.mem primitive between) ))
  in
  check (run 1 = run 4) "public Boolean seam curves differ between one and four domains";
  let coincident =
    Boolean.seam ~grain:1 ~output:Boolean.Coincident_patches ~right:left left |> get
  in
  let group =
    Geometry.find_group ~owner:Group.Primitive "boolean_coincident" coincident |> Option.get
  in
  check
    (Geometry.primitive_count coincident > 0
    && Group.cardinality group = Geometry.primitive_count coincident)
    "public Boolean coincident product lost facets or naming";
  match
    Boolean.seam ~grain:1 ~left_self_group:(Some "same") ~between_group:(Some "same") ~right left
  with
  | Error error when Error.code error = "invalid_parameter" -> ()
  | Error error -> fail "unexpected seam naming error: %s" (Error.to_string error)
  | Ok _ -> fail "public Boolean seam accepted duplicate group names"

let run () =
  test_solid_products ();
  test_surface_cut_and_payload ();
  test_normal_payload_orientation ();
  test_surface_fracture_pieces ();
  test_shatter ();
  test_shatter_scale ();
  test_output_policies ();
  test_errors ();
  test_bounded_cleanup ();
  test_seam_products ()
