open Rdk
open Rdk_test_support

let fixture value =
  let scalar = [| value; value; value |] in
  let attributes =
    List.map
      (fun (name, storage) -> get (Attribute.create_owned ~name ~owner:Attribute.Point storage))
      [
        ("f", Attribute.Float scalar);
        ("f2", Attribute.Float2 (get (Packed.Float2.of_owned ~x:scalar ~y:scalar)));
        ("f3", Attribute.Float3 (get (Packed.Float3.of_owned ~x:scalar ~y:scalar ~z:scalar)));
        ( "f4",
          Attribute.Float4 (get (Packed.Float4.of_owned ~x:scalar ~y:scalar ~z:scalar ~w:scalar)) );
        ( "fa",
          Attribute.Float_array
            (get (Packed.Float_array.create_owned ~offsets:[| 0; 1; 2; 3 |] ~values:scalar)) );
        ("i", Attribute.Int [| 1; 2; 3 |]);
        ( "ia",
          Attribute.Int_array
            (get (Packed.Int_array.create_owned ~offsets:[| 0; 1; 2; 3 |] ~values:[| 1; 2; 3 |])) );
        ("text", Attribute.Text [| "a"; "b"; "c" |]);
      ]
  in
  let positions = get (Packed.Float3.of_owned ~x:scalar ~y:scalar ~z:scalar) in
  let topology =
    get
      (Topology.polygons_owned ~point_count:3 ~vertex_points:[| 0; 1; 2 |]
         ~primitive_offsets:[| 0; 3 |])
  in
  let groups = [ get (Group.ordered ~owner:Group.Point ~name:"selected" ~length:3 [| 2; 0 |]) ] in
  let edge_groups =
    [
      Edge_group.init ~topology ~index:(Topology_index.create topology) ~name:"edges" (fun i ->
          i = 0);
    ]
  in
  get (Geometry.create ~positions ~topology ~attributes ~groups ~edge_groups ())

let () =
  let left = fixture 1.
  and same = fixture 1.
  and one = fixture (Float.next_after 1. infinity)
  and two = fixture (Float.next_after (Float.next_after 1. infinity) infinity) in
  check (close_geometry ~ulps:0 left same) "equal geometry fails";
  check (close_geometry ~ulps:1 left one) "one ULP fails";
  check (not (close_geometry ~ulps:0 left one)) "zero ULP accepts drift";
  check (not (close_geometry ~ulps:1 left two)) "one ULP accepts two";
  check (geometry_bytes left <> geometry_bytes one) "byte gate accepts float drift";
  List.iter
    (fun attribute ->
      let name = Attribute.name attribute in
      if List.mem name [ "f"; "f2"; "f3"; "f4"; "fa" ] then begin
        check
          (close_geometry ~ulps:1 left (get (Geometry.with_attribute attribute left)))
          ("one ULP fails in " ^ name);
        let far = Option.get (Geometry.find_attribute ~owner:Attribute.Point name (fixture 2.)) in
        check
          (not (close_geometry ~ulps:4 left (get (Geometry.with_attribute far left))))
          ("float storage ignored: " ^ name)
      end)
    (Geometry.attributes one);
  List.iter
    (fun value ->
      check
        (close_geometry ~ulps:1 (fixture value)
           (fixture (Float.next_after value (if value = Float.max_float then 0. else infinity))))
        "ULP boundary fails")
    [ 0.; Float.ldexp 1. (-1074); Float.min_float; -1.; Float.max_float ];
  check
    (close_geometry ~ulps:1 (fixture (Float.next_after 1. neg_infinity)) left)
    "larger-magnitude ULP across binade boundary fails";
  (* Below 1. the spacing halves: the total difference above is 1.5 ULPs. *)
  check
    (not (close_geometry ~ulps:1 (fixture (Float.next_after 1. neg_infinity)) one))
    "larger-magnitude ULP accepts 1.5 ULPs";
  let changed name storage =
    get
      (Geometry.with_attribute
         (get (Attribute.create_owned ~name ~owner:Attribute.Point storage))
         left)
  in
  let csr_changed =
    changed "fa"
      (Attribute.Float_array
         (get (Packed.Float_array.create_owned ~offsets:[| 0; 0; 2; 3 |] ~values:[| 1.; 1.; 1. |])))
  in
  check (not (close_geometry ~ulps:4 left csr_changed)) "CSR offsets accepted";
  let without_edges = Geometry.without_edge_group "edges" left in
  List.iter
    (fun (vertex_points, kind) ->
      let topology =
        get
          (Topology.create_owned ~point_count:3 ~vertex_points ~primitive_offsets:[| 0; 3 |]
             ~primitive_kinds:[| kind |])
      in
      let changed =
        get
          (Geometry.create ~positions:(Geometry.positions left) ~topology
             ~attributes:(Geometry.attributes left) ~groups:(Geometry.groups left) ())
      in
      check (not (close_geometry ~ulps:4 without_edges changed)) "topology change accepted")
    [ ([| 0; 2; 1 |], Topology.Polygon); ([| 0; 1; 2 |], Topology.Closed_polyline) ];
  check (close_geometry ~ulps:0 (fixture 0.) (fixture (-0.))) "signed zero fails";
  check
    (geometry_bytes (fixture 0.) <> geometry_bytes (fixture (-0.)))
    "byte gate accepts signed zero";
  check
    (try
       ignore (close_geometry ~ulps:(-1) left left);
       false
     with Invalid_argument _ -> true)
    "negative tolerance accepted"
