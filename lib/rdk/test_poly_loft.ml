open Rdk

let fail message = prerr_endline ("test_poly_loft: " ^ message); exit 1
let check condition message = if not condition then fail message
let get = function Ok value -> value | Error message -> fail message
let get_rdk = function Ok value -> value | Error error -> fail (Error.to_string error)

let equal_attribute left right =
  Attribute.owner left = Attribute.owner right
  && Attribute.name left = Attribute.name right
  && match Attribute.Private.storage left, Attribute.Private.storage right with
     | Attribute.Float a, Attribute.Float b -> a = b
     | Attribute.Int a, Attribute.Int b -> a = b
     | Attribute.Text a, Attribute.Text b -> a = b
     | Attribute.Float2 a, Attribute.Float2 b ->
         Packed.Float2.Private.view a = Packed.Float2.Private.view b
     | Attribute.Float3 a, Attribute.Float3 b ->
         Packed.Float3.Private.view a = Packed.Float3.Private.view b
     | Attribute.Float4 a, Attribute.Float4 b ->
         Packed.Float4.Private.view a = Packed.Float4.Private.view b
     | Attribute.Int_array a, Attribute.Int_array b ->
         Packed.Int_array.Private.view a = Packed.Int_array.Private.view b
     | Attribute.Float_array a, Attribute.Float_array b ->
         Packed.Float_array.Private.view a = Packed.Float_array.Private.view b
     | _ -> false

let equal_geometry left right =
  let lp = Packed.Float3.Private.view (Geometry.positions left)
  and rp = Packed.Float3.Private.view (Geometry.positions right)
  and lt = Topology.Private.view (Geometry.topology left)
  and rt = Topology.Private.view (Geometry.topology right) in
  lp = rp && lt.vertex_points = rt.vertex_points
  && lt.primitive_offsets = rt.primitive_offsets
  && Bytes.equal lt.primitive_kinds rt.primitive_kinds
  && List.compare_lengths (Geometry.attributes left)
       (Geometry.attributes right) = 0
  && List.for_all2 equal_attribute (Geometry.attributes left)
       (Geometry.attributes right)
  && List.compare_lengths (Geometry.groups left) (Geometry.groups right) = 0
  && List.for_all2 (fun left right ->
    Group.owner left = Group.owner right && Group.name left = Group.name right
    && Group.length left = Group.length right
    && Group.ordered_elements left = Group.ordered_elements right
    && Group.Private.bits_view left = Group.Private.bits_view right)
      (Geometry.groups left) (Geometry.groups right)
  && List.compare_lengths (Geometry.edge_groups left)
       (Geometry.edge_groups right) = 0
  && List.for_all2 (fun left right ->
    Edge_group.name left = Edge_group.name right
    && Edge_group.length left = Edge_group.length right
    && Array.init (Edge_group.length left) (fun edge -> Edge_group.mem edge left)
       = Array.init (Edge_group.length right) (fun edge -> Edge_group.mem edge right))
      (Geometry.edge_groups left) (Geometry.edge_groups right)

let open_sections () =
  let positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;1.;2.; 0.;0.6;1.4;2.|]
      ~y:[|0.;0.;0.; 1.;1.;1.;1.|]
      ~z:[|0.;0.;0.; 0.;0.2;0.2;0.|] in
  let topology = Topology.create_owned ~point_count:7
      ~vertex_points:[|0;1;2; 3;4;5;6|]
      ~primitive_offsets:[|0;3;7|]
      ~primitive_kinds:[|Topology.Open_polyline; Topology.Open_polyline|]
      |> get in
  let point = Attribute.create_owned ~owner:Attribute.Point ~name:"weight"
      (Attribute.Float (Array.init 7 float_of_int)) |> get
  and vertex = Attribute.create_owned ~owner:Attribute.Vertex ~name:"corner"
      (Attribute.Int_array (Packed.Int_array.create_owned
        ~offsets:(Array.init 8 (fun value -> value * 2))
        ~values:(Array.init 14 (fun value -> 100 + value)) |> get)) |> get
  and primitive = Attribute.create_owned ~owner:Attribute.Primitive ~name:"section"
      (Attribute.Int [|10;11|]) |> get
  and vertices = Group.ordered ~owner:Group.Vertex ~name:"curve_vertices"
      ~length:7 [|6;5;4;3;2;1;0|] |> get
  and primitives = Group.ordered ~owner:Group.Primitive ~name:"sections"
      ~length:2 [|1;0|] |> get in
  let geometry = Geometry.create ~positions ~topology
      ~attributes:[point;vertex;primitive] ~groups:[vertices;primitives] () |> get in
  let index = Topology_index.create topology in
  let edges = Edge_group.init ~grain:1 ~topology ~index ~name:"section_edges"
      (fun _ -> true) in
  Geometry.with_edge_group edges geometry |> get

let int_attribute owner name geometry =
  match Geometry.find_attribute ~owner name geometry |> Option.get
        |> Attribute.Private.storage with
  | Attribute.Int values -> values
  | _ -> fail (name ^ " storage")

let test_open_and_payload () =
  let source = open_sections () in
  let point = Geometry.find_attribute ~owner:Attribute.Point "weight" source
      |> Option.get in
  let output = Poly_loft.run ~output_group:"loft" source |> get_rdk in
  check (Geometry.point_count output = 7 && Geometry.vertex_count output = 15
      && Geometry.primitive_count output = 5) "unequal open cardinality";
  check (Geometry.find_group ~owner:Group.Primitive "loft" output
      |> Option.get |> Group.cardinality = 5) "generated group";
  check (int_attribute Attribute.Primitive "section" output
      = [|10;10;10;10;10|]) "generated primitive ancestry";
  check (Geometry.find_attribute ~owner:Attribute.Vertex "corner" output
      |> Option.get |> Attribute.length = 15) "ragged vertex ancestry";
  check (Geometry.find_group ~owner:Group.Vertex "curve_vertices" output
      |> Option.get |> Group.cardinality = 15) "ordered vertex-group ancestry";
  check (Geometry.find_edge_group "section_edges" output
      |> Option.get |> Edge_group.cardinality = 5) "native edge ancestry";
  let output_point = Geometry.find_attribute ~owner:Attribute.Point "weight" output
      |> Option.get in
  check (Attribute.storage_id point = Attribute.storage_id output_point)
    "point payload was not structurally shared";
  let kept = Poly_loft.run ~keep_primitives:true source |> get_rdk in
  check (Geometry.primitive_count kept = 7 && Geometry.vertex_count kept = 22)
    "keep sections cardinality";
  check (int_attribute Attribute.Primitive "section" kept
      = [|10;11;10;10;10;10;10|]) "kept primitive ancestry";
  let wrapped = Poly_loft.run ~u_wrap:true source |> get_rdk in
  check (Geometry.primitive_count wrapped = 7
      && Geometry.vertex_count wrapped = 21) "U wrap cardinality";
  let empty = Group.init ~grain:1 ~owner:Group.Primitive ~name:"none" 2
      (fun _ -> false) in
  check (Poly_loft.run ~primitives:empty source |> get_rdk == source)
    "empty selection identity"

let ring_sections ?(reverse_second = false)
    ?(kind = Topology.Closed_polyline) count =
  let points = count * 4 in
  let x = Array.make points 0. and y = Array.make points 0.
  and z = Array.make points 0. in
  let corners = [|(-1.,-1.); (1.,-1.); (1.,1.); (-1.,1.)|] in
  for section = 0 to count - 1 do
    for local = 0 to 3 do
      let point = (section * 4) + local in
      x.(point) <- fst corners.(local); y.(point) <- float_of_int section;
      z.(point) <- snd corners.(local)
    done
  done;
  let vertex_points = Array.init points (fun vertex ->
    let section = vertex / 4 and local = vertex mod 4 in
    if reverse_second && section = 1 then section * 4 + ((5 - local) mod 4)
    else vertex) in
  let topology = Topology.create_owned ~point_count:points ~vertex_points
      ~primitive_offsets:(Array.init (count + 1) (fun value -> value * 4))
      ~primitive_kinds:(Array.make count kind) |> get in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology () |> get

let test_closed_alignment_and_v_wrap () =
  let source = ring_sections ~reverse_second:true 2 in
  let output = Poly_loft.run source |> get_rdk in
  check (Geometry.primitive_count output = 8
      && Geometry.vertex_count output = 24) "closed loft cardinality";
  let topology = Geometry.topology output in
  let index = Topology_index.create topology in
  check (Topology_index.non_manifold_edge_count index = 0
      && Topology_index.boundary_edge_count index = 8)
    "closed loft manifold boundary";
  let positions = Packed.Float3.Private.view (Geometry.positions output) in
  for edge = 0 to Topology_index.edge_count index - 1 do
    let a, b = Topology_index.edge_points index edge in
    let length = sqrt (((positions.x.(a) -. positions.x.(b)) ** 2.)
        +. ((positions.y.(a) -. positions.y.(b)) ** 2.)
        +. ((positions.z.(a) -. positions.z.(b)) ** 2.)) in
    if length > 2.2360681 then fail (Printf.sprintf
      "closest seam introduced a twisted edge %d-%d length %.6f" a b length)
  done;
  let faces = Poly_loft.run (ring_sections ~kind:Topology.Polygon 2) |> get_rdk in
  check (Geometry.primitive_count faces = 8
      && Geometry.vertex_count faces = 24) "polygon-face loft";
  let loop = Poly_loft.run ~v_wrap:true (ring_sections 3) |> get_rdk in
  let loop_index = Topology_index.create (Geometry.topology loop) in
  check (Geometry.primitive_count loop = 24
      && Topology_index.boundary_edge_count loop_index = 0
      && Topology_index.non_manifold_edge_count loop_index = 0)
    "V-wrapped loft is not closed manifold"

let test_rest_errors_and_cancellation () =
  let source = open_sections () in
  let mismatch = Line_geometry.points [|0.,0.,0.|] in
  (match Poly_loft.run ~rest:mismatch source with
   | Error error -> check (Error.code error = "invalid_topology")
       "rest mismatch diagnostic"
   | Ok _ -> fail "rest mismatch accepted");
  let positions = Packed.Float3.Private.view (Geometry.positions source) in
  let bad = Packed.Float3.Private.of_owned_exn ~x:(Array.copy positions.x)
      ~y:(Array.copy positions.y) ~z:(Array.copy positions.z) in
  let bad = Packed.Float3.Private.view bad in
  bad.x.(0) <- nan;
  let bad = Geometry.with_positions
      (Packed.Float3.Private.of_shared_exn ~x:bad.x ~y:bad.y ~z:bad.z) source |> get in
  (match Poly_loft.run bad with
   | Error error -> check (Error.code error = "invalid_topology")
       "non-finite guide diagnostic"
   | Ok _ -> fail "non-finite guide accepted");
  (match Poly_loft.run ~collinearity_tolerance:1.1 source with
   | Error error -> check (Error.code error = "invalid_topology")
       "tolerance diagnostic"
   | Ok _ -> fail "invalid tolerance accepted");
  let point_group = Group.init ~grain:1 ~owner:Group.Point ~name:"wrong" 7
      (fun _ -> true) in
  (match Poly_loft.run ~primitives:point_group source with
   | Error error -> check (Error.code error = "invalid_topology")
       "selection owner diagnostic"
   | Ok _ -> fail "point selection accepted");
  let cancel = Cancel.create () in
  Cancel.cancel cancel;
  (match Poly_loft.run ~cancel source with
   | Error error -> check (Error.code error = "cancelled") "cancellation code"
   | Ok _ -> fail "cancelled loft succeeded")

let test_rest_collinearity_and_normals () =
  let rest = ring_sections 2 in
  let rest_positions = Packed.Float3.Private.view (Geometry.positions rest) in
  let x = Array.copy rest_positions.x and y = Array.copy rest_positions.y
  and z = Array.copy rest_positions.z in
  for local = 0 to 3 do
    let source = 4 + ((local + 1) mod 4) and target = 4 + local in
    x.(target) <- rest_positions.x.(source);
    y.(target) <- rest_positions.y.(source);
    z.(target) <- rest_positions.z.(source)
  done;
  let deformed = Geometry.with_positions
      (Packed.Float3.Private.of_owned_exn ~x ~y ~z) rest |> get in
  let guided = Poly_loft.run ~rest deformed |> get_rdk
  and unguided = Poly_loft.run deformed |> get_rdk in
  let gt = Topology.Private.view (Geometry.topology guided)
  and ut = Topology.Private.view (Geometry.topology unguided) in
  check (gt.vertex_points <> ut.vertex_points) "rest guide did not affect pairing";
  let collinear_positions = Packed.Float3.Private.of_owned_exn
      ~x:[|0.;1.;2.; 0.;1.;2.|] ~y:(Array.make 6 0.) ~z:(Array.make 6 0.) in
  let collinear_topology = Topology.create_owned ~point_count:6
      ~vertex_points:[|0;1;2; 3;4;5|] ~primitive_offsets:[|0;3;6|]
      ~primitive_kinds:[|Topology.Open_polyline;Topology.Open_polyline|] |> get in
  let collinear = Geometry.create ~positions:collinear_positions
      ~topology:collinear_topology () |> get in
  let filtered = Poly_loft.run ~collinearity_tolerance:1e-12
      ~output_group:"loft" collinear |> get_rdk in
  check (Geometry.point_count filtered = 6
      && Geometry.primitive_count filtered = 0
      && (Geometry.find_group ~owner:Group.Primitive "loft" filtered
          |> Option.get |> Group.cardinality) = 0)
    "positive collinearity filtering";
  let source = ring_sections 2 in
  let normal = Attribute.create_owned ~owner:Attribute.Point ~name:"N"
      (Attribute.Float3 (Packed.Float3.Private.of_owned_exn
        ~x:(Array.make 8 1.) ~y:(Array.make 8 0.) ~z:(Array.make 8 0.))) |> get in
  let source = Geometry.with_attribute normal source |> get in
  let output = Poly_loft.run source |> get_rdk in
  let normal = Geometry.find_attribute ~owner:Attribute.Point "N" output
      |> Option.get in
  check (Attribute.length normal = 8) "normal cardinality";
  match Attribute.Private.storage normal with
  | Attribute.Float3 values ->
      let values = Packed.Float3.Private.view values in
      check (Array.for_all (fun point ->
        let squared = (values.x.(point) ** 2.) +. (values.y.(point) ** 2.)
            +. (values.z.(point) ** 2.) in
        abs_float (squared -. 1.) < 1e-10)
          (Array.init 8 Fun.id)) "recomputed normal values"
  | _ -> fail "normal storage"

let many_sections () =
  let sections = 96 and per_section = 129 in
  let count = sections * per_section in
  let x = Array.make count 0. and y = Array.make count 0.
  and z = Array.make count 0. in
  for section = 0 to sections - 1 do
    for local = 0 to per_section - 1 do
      let point = (section * per_section) + local
      and angle = 2. *. Float.pi *. float_of_int local
          /. float_of_int per_section in
      let radius = 1. +. (0.08 *. sin (float_of_int section *. 0.17)) in
      x.(point) <- radius *. cos angle;
      y.(point) <- float_of_int section *. 0.025;
      z.(point) <- radius *. sin angle
    done
  done;
  let topology = Topology.create_owned ~point_count:count
      ~vertex_points:(Array.init count Fun.id)
      ~primitive_offsets:(Array.init (sections + 1)
        (fun value -> value * per_section))
      ~primitive_kinds:(Array.make sections Topology.Closed_polyline) |> get in
  let rows = Packed.Int_array.create_owned
      ~offsets:(Array.init (count + 1) (fun value -> value))
      ~values:(Array.init count (fun value -> value mod 31)) |> get in
  let attribute = Attribute.create_owned ~owner:Attribute.Vertex ~name:"row"
      (Attribute.Int_array rows) |> get
  and group = Group.ordered ~owner:Group.Primitive ~name:"ordered_sections"
      ~length:sections (Array.init sections (fun value -> sections - value - 1))
      |> get in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
    ~topology ~attributes:[attribute] ~groups:[group] () |> get

let test_parallel () =
  let source = many_sections () in
  let run domains = Parallel.run ~domains (fun () ->
      Poly_loft.run ~grain:97 ~minimize:Poly_loft.Three_point_distance
        ~output_group:"loft" source |> get_rdk) in
  let one = run 1 and four = run 4 in
  check (equal_geometry one four) "one/four-domain loft differs";
  let expected = 95 * (129 + 129) in
  check (Geometry.primitive_count one = expected
      && Geometry.vertex_count one = expected * 3)
    "large loft cardinality"

let run () =
  test_open_and_payload ();
  test_closed_alignment_and_v_wrap ();
  test_rest_errors_and_cancellation ();
  test_rest_collinearity_and_normals ();
  test_parallel ();
  print_endline "test_poly_loft: ok"
