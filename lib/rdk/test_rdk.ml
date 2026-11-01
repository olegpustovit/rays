open Rays
open Rdk
open Rdk_test_support

let get_ok = function Ok value -> value | Error _ -> fail "unexpected error"

let check_topology_validation () =
  let invalid = "Topology: vertex references an invalid point" in
  List.iter
    (fun count ->
      List.iter
        (fun polygon ->
          let construct domains bad_indices =
            Parallel.run ~domains (fun () ->
                let vertices = Array.init count Fun.id in
                List.iter (fun index -> vertices.(index) <- count) bad_indices;
                let offsets = if count = 0 then [| 0 |] else [| 0; count |] in
                let kinds = if count = 0 then [||] else [| Topology.Open_polyline |] in
                let vertices_before = Array.copy vertices and offsets_before = Array.copy offsets in
                let result =
                  if polygon then
                    Topology.polygons_owned ~point_count:count ~vertex_points:vertices
                      ~primitive_offsets:offsets
                  else
                    Topology.create_owned ~point_count:count ~vertex_points:vertices
                      ~primitive_offsets:offsets ~primitive_kinds:kinds
                in
                check
                  (vertices = vertices_before && offsets = offsets_before)
                  "topology validation must not mutate owned input planes";
                Result.map
                  (fun topology ->
                    let view = Topology.View.view topology in
                    check
                      (view.vertex_points == vertices && view.primitive_offsets == offsets)
                      "topology constructors retain ownership-transfer planes";
                    ( view.point_count,
                      Array.copy view.vertex_points,
                      Array.copy view.primitive_offsets,
                      Bytes.copy view.primitive_kinds ))
                  result)
          in
          let expected = construct 1 [] in
          check (Result.is_ok expected) "valid topology at scan cutoff/block/tail";
          List.iter
            (fun domains ->
              check
                (construct domains [] = expected)
                "topology constructor payloads must be exact at one/four/eight domains";
              if count > 0 then begin
                let indices =
                  List.sort_uniq Int.compare
                    [ 0; min 65535 (count - 1); min 65536 (count - 1); count - 1 ]
                in
                List.iter
                  (fun index ->
                    check
                      (construct domains [ index ] = Error invalid)
                      "invalid vertex at first/block boundary/tail")
                  indices;
                check
                  (construct domains indices = Error invalid)
                  "simultaneous invalid blocks must preserve the public error"
              end)
            [ 1; 4; 8 ])
        [ false; true ])
    [ 0; 3; 32767; 32768; 65535; 65536; 65537; 131073 ];
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          let count = 131073 in
          let vertices = Array.init count Fun.id in
          vertices.(0) <- -1;
          vertices.(65536) <- count;
          let expect message ~point_count ~offsets ~kinds =
            check
              (Topology.create_owned ~point_count ~vertex_points:vertices ~primitive_offsets:offsets
                 ~primitive_kinds:kinds
              = Error message)
              "topology simultaneous-invalid validation precedence"
          in
          expect "Topology: point count is negative" ~point_count:(-1) ~offsets:[||] ~kinds:[||];
          expect "Topology: primitive offsets must contain the initial zero" ~point_count:count
            ~offsets:[||] ~kinds:[||];
          expect "Topology: first primitive offset must be zero" ~point_count:count
            ~offsets:[| 1; count - 1 |]
            ~kinds:[||];
          expect "Topology: final primitive offset must equal vertex count" ~point_count:count
            ~offsets:[| 0; count - 1 |]
            ~kinds:[||];
          expect "Topology: primitive kind count must match primitive count" ~point_count:count
            ~offsets:[| 0; count |] ~kinds:[||];
          let offsets = [| 0; count; count - 1; count |]
          and kinds = Array.make 3 Topology.Polygon in
          expect invalid ~point_count:count ~offsets ~kinds;
          vertices.(0) <- 0;
          vertices.(65536) <- 65536;
          expect "Topology: primitive offsets are not ordered" ~point_count:count ~offsets ~kinds;
          expect "Topology: primitive has too few vertices for its kind" ~point_count:count
            ~offsets:[| 0; 1; count |] ~kinds:(Array.make 2 Topology.Polygon)))
    [ 1; 4; 8 ]

type Packed.Deferred.holder += Topology_test of int

let check_deferred_topology () =
  let source =
    Plane_generators.grid ~connectivity:Grid_quads ~columns:2 ~rows:2 ~size:2. () |> get_rdk
  in
  let owned = Geometry.topology source in
  let expected = Topology.View.view owned in
  let make ?shape ?holder ?release reads =
    Topology.deferred ?shape ?holder ?release ~point_count:(Topology.point_count owned)
      ~vertex_count:(Topology.vertex_count owned) ~primitive_count:(Topology.primitive_count owned)
      (fun () ->
        Atomic.incr reads;
        owned)
  in
  let reads = Atomic.make 0 and releases = ref [] in
  let topology =
    make ~shape:(Polygon, 4) ~holder:(Topology_test 7)
      ~release:(fun ~held -> releases := held :: !releases)
      reads
  in
  let geometry =
    Geometry.create ~positions:(Geometry.positions source) ~topology
      ~attributes:(Geometry.attributes source) ()
    |> get_string
  in
  let id = Topology.data_id topology in
  check
    (Topology.point_count topology = Topology.point_count owned
    && Topology.vertex_count topology = Topology.vertex_count owned
    && Topology.primitive_count topology = Topology.primitive_count owned
    && Topology.payload_bytes topology = Topology.payload_bytes owned
    && Geometry.payload_bytes geometry = Geometry.payload_bytes source
    && (not (Topology.all_triangles topology))
    && Topology.resident topology
    && Topology.holder topology = Some (Topology_test 7)
    && Atomic.get reads = 0)
    "deferred topology metadata or Geometry.create forced payload";
  let release = List.assoc id (Geometry.releasers geometry) in
  release ~held:true;
  Parallel.run ~domains:4 (fun () ->
      Parallel.for_ ~chunk_size:1 ~start:0 ~finish:63 (fun _ ->
          check (Topology.View.view topology = expected) "racing topology readers differ"));
  check
    (Atomic.get reads = 1
    && (not (Topology.resident topology))
    && Topology.holder topology = None
    && Topology.releaser topology = None
    && Topology.data_id topology = id
    && Geometry.releasers geometry = []
    && !releases = [ true ])
    "deferred topology must materialise once and drop its holder";
  release ~held:false;
  check (!releases = [ false; true ]) "retained release handle changed contract";
  check
    (Topology.point_of_vertex topology 2 = Topology.point_of_vertex owned 2
    && Topology.primitive_vertex_range topology 1 = Topology.primitive_vertex_range owned 1
    && Topology.primitive_size topology 1 = 4
    && Topology.primitive_kind topology 1 = Polygon)
    "topology queries read placeholder planes";
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          List.iter
            (fun consume ->
              let reads = Atomic.make 0 in
              let topology = make reads in
              let geometry =
                Geometry.create ~positions:(Geometry.positions source) ~topology
                  ~attributes:(Geometry.attributes source) ()
                |> get_string
              in
              consume topology geometry;
              check (Atomic.get reads = 1) "CPU consumer did not materialise topology once")
            [
              (fun topology _ ->
                check
                  (Point_index.View.view (Point_index.create topology)
                  = Point_index.View.view (Point_index.create owned))
                  "deferred point incidence");
              (fun topology _ ->
                check
                  (Topology_index.View.view (Topology_index.create topology)
                  = Topology_index.View.view (Topology_index.create owned))
                  "deferred edge incidence");
              (fun _ geometry ->
                let run = Triangulate.run { cancel = None; grain = Some 1; primitives = None } in
                check
                  (equal_geometry (run geometry |> get_rdk) (run source |> get_rdk))
                  "deferred triangulation");
              (fun _ geometry ->
                let run =
                  Normal_ops.run
                    {
                      cancel = None;
                      grain = Some 1;
                      selection = None;
                      primitives = None;
                      owner = Point;
                      weighting = Face_area;
                      cusp_angle = Float.pi;
                      attribute = "N";
                    }
                in
                check
                  (equal_geometry (run geometry |> get_rdk) (run source |> get_rdk))
                  "deferred normal attribute operation");
            ]))
    [ 1; 4 ];
  let triangles =
    Triangulate.run { cancel = None; grain = None; primitives = None } source |> get_rdk
  in
  let triangles = Geometry.topology triangles in
  let reads = ref 0 in
  let topology =
    Topology.deferred ~shape:(Polygon, 3) ~point_count:(Topology.point_count triangles)
      ~vertex_count:(Topology.vertex_count triangles)
      ~primitive_count:(Topology.primitive_count triangles) (fun () ->
        incr reads;
        triangles)
  in
  check (Topology.all_triangles topology && !reads = 0) "triangle shape forced payload";
  ignore (Topology.View.view topology);
  check (!reads = 1) "triangle shape materialisation";
  let mismatch =
    Topology.deferred ~point_count:0 ~vertex_count:0 ~primitive_count:0 (fun () -> owned)
  in
  check
    (try
       ignore (Topology.View.view mismatch);
       false
     with Invalid_argument _ -> true)
    "deferred topology accepted changing counts";
  let bad_shape = make ~shape:(Open_polyline, 4) (Atomic.make 0) in
  check
    (try
       ignore (Topology.View.view bad_shape);
       false
     with Invalid_argument _ -> true)
    "deferred topology accepted changing shape";
  let empty_reads = ref 0 in
  let empty =
    Topology.deferred ~shape:(Polygon, 3) ~point_count:0 ~vertex_count:0 ~primitive_count:0
      (fun () ->
        incr empty_reads;
        Topology.empty ~point_count:0)
  in
  check
    (Topology.all_triangles empty
    && Topology.payload_bytes empty = Sys.word_size / 8
    && !empty_reads = 0)
    "empty topology metadata forced payload";
  check
    ((Topology.View.view empty).primitive_offsets = [| 0 |] && !empty_reads = 1)
    "empty topology materialisation";
  let reads = Atomic.make 0 in
  let freed = ref false in
  let unread =
    Topology.deferred ~point_count:0 ~vertex_count:0 ~primitive_count:0
      ~release:(fun ~held -> if not held then freed := true)
      (fun () ->
        Atomic.incr reads;
        if !freed then invalid_arg "released topology";
        Topology.empty ~point_count:0)
  in
  Option.get (Topology.releaser unread) ~held:false;
  check (Atomic.get reads = 0) "release materialised unread topology";
  for _ = 1 to 2 do
    check
      (try
         ignore (Topology.View.view unread);
         false
       with Invalid_argument _ -> true)
      "released topology still materialised"
  done;
  check (Atomic.get reads = 1) "failed materialisation reran its thunk";
  List.iter
    (fun (point_count, vertex_count, primitive_count, shape) ->
      check
        (try
           ignore
             (Topology.deferred ?shape ~point_count ~vertex_count ~primitive_count (fun () ->
                  fail "invalid metadata invoked its materialiser"));
           false
         with Invalid_argument _ -> true)
        "deferred topology accepted invalid metadata")
    [
      (-1, 0, 0, None);
      (0, -1, 0, None);
      (0, 0, -1, None);
      (1, 1, 0, None);
      (4, 4, 1, Some (Polygon, 2));
      (4, 4, 1, Some (Polygon, 3));
    ];
  print_endline "Deferred topology metadata, single materialisation, release and CPU consumers pass"

let run () =
  check_topology_validation ();
  check_deferred_topology ();
  let builder = Packed.Float3.Builder.create 4 in
  Packed.Float3.Builder.set builder 0 0. 0. 0.;
  Packed.Float3.Builder.set builder 1 1. 0. 0.;
  Packed.Float3.Builder.set builder 2 1. 1. 0.;
  Packed.Float3.Builder.set builder 3 0. 1. 0.;
  let positions = Packed.Float3.Builder.freeze builder in
  let topology =
    Topology.polygons_owned ~point_count:4 ~vertex_points:[| 0; 1; 2; 3 |]
      ~primitive_offsets:[| 0; 4 |]
    |> get_ok
  in
  if Topology.vertex_count topology <> 4 || Topology.primitive_size topology 0 <> 4 then
    fail "polygon CSR cardinality";
  let selected = Group.init ~owner:Group.Point ~name:"even" 4 (fun i -> i land 1 = 0) in
  let all = Group.init ~owner:Group.Point ~name:"all" 4 (fun _ -> true) in
  let odd = Group.difference all selected |> get_ok in
  if Group.cardinality odd <> 2 || Group.mem 0 odd || not (Group.mem 1 odd) then
    fail "packed group difference";
  let geometry = Geometry.create ~positions ~topology ~groups:[ selected ] () |> get_ok in
  if Group.cardinality selected <> 2 then fail "packed group cardinality";
  let quad_mesh = Rdk_rays.Rays_mesh.to_mesh geometry |> get_ok in
  if Mesh.index_count quad_mesh <> 6 then fail "quad bridge triangulation";
  let extruded = Poly_extrude_front.run ~distance:2. geometry |> get_ok in
  if
    Geometry.point_count extruded <> 8
    || Geometry.primitive_count extruded <> 6
    || Geometry.vertex_count extruded <> 24
  then fail "poly extrude cardinality";
  if Mesh.index_count (Rdk_rays.Rays_mesh.to_mesh extruded |> get_ok) <> 36 then
    fail "poly extrude bridge";
  (match Geometry.find_group ~owner:Group.Point "even" extruded with
  | Some group when Group.cardinality group = 4 -> ()
  | _ -> fail "poly extrude point group remap");
  let reserved_p =
    Attribute.create_owned ~name:"P" ~owner:Attribute.Point (Attribute.Float3 positions) |> get_ok
  in
  (match Geometry.create ~positions ~topology ~attributes:[ reserved_p ] () with
  | Error _ -> ()
  | Ok _ -> fail "duplicate canonical P was accepted");
  (match
     Topology.polygons_owned ~point_count:2 ~vertex_points:[| 0; 1 |] ~primitive_offsets:[| 0; 2 |]
   with
  | Error _ -> ()
  | Ok _ -> fail "undersized polygon was accepted");
  let cancelled = Cancel.create () in
  Cancel.cancel cancelled;
  (match Plane_generators.grid ~cancel:cancelled ~columns:256 ~rows:256 ~size:2. () with
  | Error error when Error.code error = "cancelled" -> ()
  | Error error -> fail ("unexpected cancellation code: " ^ Error.code error)
  | Ok _ -> fail "cancelled RDK operation published geometry");
  print_endline "rdk tests passed"
