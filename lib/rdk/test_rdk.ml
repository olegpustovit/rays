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

let run () =
  check_topology_validation ();
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
