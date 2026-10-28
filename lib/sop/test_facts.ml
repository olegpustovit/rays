open Sop
open Rdk
open Rdk_test_support

let context domains = Context.create ~domains ~grain:37 () |> get
let session ?(entries = 64) () = Session.create ~max_entries:entries ~max_payload_bytes:16_000_000 |> get
let cook session domains node = match Session.cook session ~context:(context domains) node with
  | Ok output -> output
  | Error error -> fail (Diagnostic.error_to_string error)

let scalar ~owner name count value =
  Attribute.create_owned ~owner ~name (Attribute.Float (Array.make count value)) |> get

let fixture () =
  let geometry = Plane_generators.grid ~columns:8 ~rows:6 ~size:2. () |> get_rdk in
  let geometry = Normal_ops.run ~owner:Attribute.Point geometry |> get_rdk in
  let point_normal = Geometry.find_attribute ~owner:Attribute.Point "N" geometry |> Option.get in
  let geometry = Normal_ops.run ~owner:Attribute.Vertex geometry |> get_rdk in
  Geometry.with_attribute point_normal geometry |> get

let with_color value geometry =
  Geometry.with_attribute (scalar ~owner:Attribute.Point "Cd" (Geometry.point_count geometry) value) geometry |> get

let facts node reads writes = Node.Build.with_facts
  {(Node.facts node) with elementwise = Node.Points; topology = Node.Preserved;
    exact = false; reads; writes} node

let counted source calls reads writes transform =
  Node.Build.make_geometry ~label:"counted" ~operation:"facts_test" ~version:1 ~parameters:""
    ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
    ~inputs:[|source|] (fun ~node_id:_ _ inputs ->
      incr calls;
      Ok Node.Build.{geometry = transform inputs.(0); instances = None})
  |> fun node -> facts node reads writes

let rewire node geometry = Node.Build.rebuild_with_inputs node [|(Lisp_sop.snapshot (geometry))|]

let test_reuse volatile =
  let original = fixture () |> with_color 1. in
  let source = Lisp_sop.snapshot original in
  let node = Lisp_sop.node ~with_:["source", source]
      (Printf.sprintf {|(sop/transform (sop/ext_source) :translate %s)|} (Lisp_sop.vec3 Rays_math.Vec3.unit_x)) in
  let evaluator = session () in
  if volatile then Session.set_volatile evaluator (fun id -> id = Node.id node);
  let first = cook evaluator 1 node in
  let first_stats = Session.stats evaluator in
  let updated = with_color 2. original in
  let node = rewire node updated in
  let second = cook evaluator 8 node in
  let stats = Session.stats evaluator in
  check (stats.cooks = first_stats.cooks + 1 && stats.hits = first_stats.hits + 1)
    "color edit recooked the transform";
  check (Geometry.positions (Result.get_ok (Sop.Payload.geometry first.payload)) == Geometry.positions (Result.get_ok (Sop.Payload.geometry second.payload)))
    "cache hit did not share computed positions";
  check (Geometry.topology (Result.get_ok (Sop.Payload.geometry second.payload)) == Geometry.topology original)
    "cache hit replaced topology";
  check (Geometry.find_attribute ~owner:Attribute.Point "Cd" (Result.get_ok (Sop.Payload.geometry second.payload)) =
    Geometry.find_attribute ~owner:Attribute.Point "Cd" updated) "cache hit returned stale color";
  let cold = session () in
  check (geometry_bytes (Result.get_ok (Sop.Payload.geometry second.payload)) = geometry_bytes (Result.get_ok (Sop.Payload.geometry (cook cold 1 node).payload)))
    "refreshed output differs from a cold cook";
  Session.close cold;
  let again = cook evaluator 8 node in
  check ((Result.get_ok (Sop.Payload.geometry again.payload)) == (Result.get_ok (Sop.Payload.geometry second.payload))) "warm hit churned geometry identity";
  let removed = Geometry.without_attribute ~owner:Attribute.Point "Cd" updated in
  let extra = scalar ~owner:Attribute.Detail "extra" 1 3. in
  let added = Geometry.with_attribute extra removed |> get in
  let output = cook evaluator 1 (rewire node added) in
  check (Geometry.find_attribute ~owner:Attribute.Point "Cd" (Result.get_ok (Sop.Payload.geometry output.payload)) = None
      && Geometry.find_attribute ~owner:Attribute.Detail "extra" (Result.get_ok (Sop.Payload.geometry output.payload)) = Some extra)
    "cache hit did not carry an attribute addition/removal";
  let renamed = Geometry.without_attribute ~owner:Attribute.Detail "extra" added
    |> Geometry.with_attribute (scalar ~owner:Attribute.Detail "renamed" 1 3.) |> get in
  let output = cook evaluator 1 (rewire node renamed) in
  check (Geometry.find_attribute ~owner:Attribute.Detail "extra" (Result.get_ok (Sop.Payload.geometry output.payload)) = None
      && Geometry.find_attribute ~owner:Attribute.Detail "renamed" (Result.get_ok (Sop.Payload.geometry output.payload)) <>
         None) "cache hit did not carry a rename";
  Session.clear evaluator;
  check ((Session.stats evaluator).retained_payload_bytes = 0
    && (Session.stats evaluator).retained_entries = 0
    && (Session.stats evaluator).volatile_entries = 0) "clear retained component payloads";
  Session.close evaluator

let test_dependencies () =
  let geometry = fixture () |> with_color 1. in
  let calls = ref 0 in
  let node = counted (Lisp_sop.snapshot (geometry)) calls ["P"; "N"] [] Fun.id in
  let evaluator = session () in
  ignore (cook evaluator 1 node);
  ignore (cook evaluator 1 (rewire node (with_color 2. geometry)));
  check (!calls = 1) "unread color changed a component key";
  let normals = Packed.Float3.of_owned ~x:(Array.make (Geometry.point_count geometry) 1.)
    ~y:(Array.make (Geometry.point_count geometry) 0.) ~z:(Array.make (Geometry.point_count geometry) 0.) |> get in
  let normal = Attribute.create_key_owned (Attribute.normal ~owner:Attribute.Point) normals |> get in
  let changed_n = Geometry.with_attribute normal geometry |> get in
  ignore (cook evaluator 1 (rewire node changed_n));
  check (!calls = 2) "read N did not invalidate";
  let changed_p = Geometry.with_positions normals changed_n |> get in
  ignore (cook evaluator 1 (rewire node changed_p));
  check (!calls = 3) "read P did not invalidate";
  let topology = Geometry.topology (fixture ()) in
  let changed_topology = Geometry.create ~positions:(Geometry.positions changed_p)
    ~topology ~attributes:(Geometry.attributes changed_p) () |> get in
  ignore (cook evaluator 1 (rewire node changed_topology));
  check (!calls = 4) "topology did not invalidate";
  let group = Group.init ~owner:Group.Point ~name:"selection" (Geometry.point_count geometry) (fun _ -> true) in
  let changed_group = Geometry.with_group group changed_topology |> get in
  ignore (cook evaluator 1 (rewire node changed_group));
  check (!calls = 5) "group did not invalidate";
  let edges = Edge_group.init ~topology ~index:(Topology_index.create topology)
    ~name:"edges" (fun _ -> true) in
  ignore (cook evaluator 1 (rewire node (Geometry.with_edge_group edges changed_group |> get)));
  check (!calls = 6) "edge group did not invalidate";
  let opaque = Node.Build.make_geometry ~operation:"opaque" ~version:1 ~parameters:""
    ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
    ~inputs:[|(Lisp_sop.snapshot (geometry))|] (fun ~node_id:_ _ inputs ->
      incr calls; Ok Node.Build.{geometry = inputs.(0); instances = None}) in
  check ((Node.facts opaque).elementwise = Node.None && (Node.facts opaque).topology = Node.Changed
      && (Node.facts opaque).exact) "opaque declaration is not conservative";
  ignore (cook evaluator 1 opaque);
  ignore (cook evaluator 1 (rewire opaque (with_color 3. geometry)));
  check (!calls = 8) "opaque node used a component key";
  let identity_calls = ref 0 in
  let identity = counted (Lisp_sop.snapshot (geometry)) identity_calls [] [] Fun.id in
  ignore (cook evaluator 1 identity);
  let changed = changed_p |> with_color 4. in
  let output = cook evaluator 1 (rewire identity changed) in
  check (!identity_calls = 1 && Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload)) == Geometry.positions changed
      && equal_geometry (Result.get_ok (Sop.Payload.geometry output.payload)) changed) "read-free cache hit did not carry fresh P/attributes";
  Session.close evaluator

let test_write_footprint () =
  let geometry = fixture () in
  let remove = Geometry.without_attribute ~owner:Attribute.Point "N" in
  let absent = remove geometry in
  let calls = ref 0 in
  let node = counted (Lisp_sop.snapshot (absent)) calls ["P"] ["N"] remove in
  let evaluator = session () in
  ignore (cook evaluator 1 node);
  let output = cook evaluator 1 (rewire node geometry) in
  check (!calls = 2 && Geometry.find_attribute ~owner:Attribute.Point "N" (Result.get_ok (Sop.Payload.geometry output.payload)) = None)
    "adding an unread written attribute reused an incomplete delta";
  let cd = counted (Lisp_sop.snapshot ((with_color 1. geometry))) calls ["P"] ["Cd"]
    (with_color 4.) in
  let first = cook evaluator 1 cd in
  let changed = geometry |> with_color 2.
    |> Geometry.with_attribute (scalar ~owner:Attribute.Primitive "Cd" (Geometry.primitive_count geometry) 3.) |> get in
  let second = cook evaluator 1 (rewire cd changed) in
  check (Geometry.find_attribute ~owner:Attribute.Primitive "Cd" (Result.get_ok (Sop.Payload.geometry second.payload)) =
      Geometry.find_attribute ~owner:Attribute.Primitive "Cd" changed)
    "writer discarded a different owner's attribute";
  check (equal_attribute (Geometry.find_attribute ~owner:Attribute.Point "Cd" (Result.get_ok (Sop.Payload.geometry first.payload)) |> Option.get)
      (Geometry.find_attribute ~owner:Attribute.Point "Cd" (Result.get_ok (Sop.Payload.geometry second.payload)) |> Option.get))
    "writer did not preserve its computed attribute";
  Session.close evaluator

let test_refusals ?(entries=64) () =
  let geometry = fixture () in
  let node transform = counted (Lisp_sop.snapshot (geometry)) (ref 0) ["P"] [] transform in
  let evaluator = session ~entries () in
  let rejects node = match Session.cook evaluator ~context:(context 1) node with
    | Error error -> check (error.code = "E_NODE_FACTS" && List.exists
        (fun trace -> trace.Diagnostic.node_id = Node.id node) error.trace)
        "incorrect facts did not return a traced E_NODE_FACTS"
    | Ok _ -> fail "incorrect facts were accepted" in
  rejects (node (fun geometry -> Geometry.with_attribute
    (scalar ~owner:Attribute.Point "undeclared" (Geometry.point_count geometry) 1.) geometry |> get));
  rejects (node (Geometry.without_attribute ~owner:Attribute.Point "N"));
  rejects (node (fun _ -> fixture ()));
  rejects (node (fun geometry -> Geometry.with_group
    (Group.init ~owner:Group.Point ~name:"added" (Geometry.point_count geometry) (fun _ -> true)) geometry |> get));
  let packed = Node.Build.make_geometry ~operation:"packed" ~version:1 ~parameters:""
    ~cook_mode:(Node.Duplicate_input 0) ~dependencies:Context.Dependencies.static
    ~inputs:[|(Lisp_sop.snapshot (geometry))|] (fun ~node_id:_ _ inputs ->
      Ok Node.Build.{geometry = inputs.(0); instances = Some [|Rays_math.Mat4.identity|]})
    |> fun node -> facts node ["P"] [] in
  rejects packed;
  Session.close evaluator

let test_normals_cache_refresh () =
  let geometry = fixture () |> with_color 1. in
  let source_node geometry =
    Node.Build.make_geometry ~operation:"source" ~version:1
      ~parameters:(string_of_int (Geometry.data_id geometry))
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _ -> Ok Node.Build.{geometry; instances = None}) in
  let original = source_node geometry in
  let node = Lisp_sop.node ~with_:["original", (original)] {|(sop/normals (sop/ext_original))|} in
  let evaluator = session () in
  ignore (cook evaluator 1 node);
  let source = source_node (with_color 2. geometry) in
  let node = Node.Build.rebuild_with_inputs node [|source|] in
  let before = Session.stats evaluator in
  let output = cook evaluator 1 node in
  check ((Session.stats evaluator).hits = before.hits + 1) "normals recooked after a color payload edit";
  let cold = session () in
  check (geometry_bytes (Result.get_ok (Sop.Payload.geometry output.payload)) = geometry_bytes (Result.get_ok (Sop.Payload.geometry (cook cold 8 node).payload)))
    "normal cache refresh changed attribute order";
  Session.close cold;
  Session.close evaluator

let test_payload_budget () =
  let geometry = fixture () |> with_color 1. in
  let node = Lisp_sop.node ~with_:["geometry", (Lisp_sop.snapshot (geometry))] (Printf.sprintf {|(sop/transform (sop/ext_geometry) :translate %s)|} ((Lisp_sop.vec3 Rays_math.Vec3.unit_x))) in
  let evaluator = session ~entries:2 () in
  let output = cook evaluator 1 node in
  let fresh = with_color 2. geometry in
  let fresh_source = Lisp_sop.snapshot (fresh) in
  let next = Node.Build.rebuild_with_inputs node [|fresh_source|] in
  let updated = cook evaluator 1 next in
  check (Geometry.positions (Result.get_ok (Sop.Payload.geometry updated.payload)) == Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))) "budget test missed reuse";
  let components = Hashtbl.create 8 in
  List.iter (fun geometry -> List.iter (fun (id, bytes) -> Hashtbl.replace components id bytes)
    (Geometry.payload_components geometry)) [fresh; (Result.get_ok (Sop.Payload.geometry updated.payload))];
  let bytes = Hashtbl.fold (fun _ bytes total -> total + bytes) components 0 in
  check ((Session.stats evaluator).retained_payload_bytes = bytes)
    "refresh retained the previous unread payload or lost reference counts";
  Session.close evaluator

let test_parameter_reads () =
  let geometry = fixture () in
  let source = Lisp_sop.snapshot (geometry) in
  let peak = Lisp_sop.node ~with_:["source", (source)] {|(sop/peak (sop/ext_source) :direction_attribute "")|} in
  check (List.mem "N" (Node.facts peak).reads) "blank peak direction omitted fallback normals";
  let mountain = Lisp_sop.node ~with_:["source", (source)] {|(sop/mountain (sop/ext_source) :height_attribute "height")|} in
  check (List.mem "N" (Node.facts mountain).reads && List.mem "height" (Node.facts mountain).reads)
    "mountain omitted fallback normals or existing height values";
  let selected = Group.init ~owner:Group.Point ~name:"selected" (Geometry.point_count geometry)
    (fun point -> point = 0) in
  let geometry = Geometry.with_group selected geometry |> get in
  let height value = Geometry.with_attribute
    (scalar ~owner:Attribute.Point "height" (Geometry.point_count geometry) value) geometry |> get in
  let mountain = Lisp_sop.node ~with_:["in1781", (Lisp_sop.snapshot ((height 1.)))] {|(sop/mountain (sop/ext_in1781) :group "selected" :height_attribute "height")|} in
  let evaluator = session () in
  ignore (cook evaluator 1 mountain);
  let before = Session.stats evaluator in
  let mountain = rewire mountain (height 2.) in
  let output = cook evaluator 1 mountain in
  check ((Session.stats evaluator).cooks = before.cooks + 2)
    "existing height edit reused stale unselected mountain values";
  let cold = session () in
  check (geometry_bytes (Result.get_ok (Sop.Payload.geometry output.payload)) = geometry_bytes (Result.get_ok (Sop.Payload.geometry (cook cold 8 mountain).payload)))
    "mountain selected height output differs from a cold cook";
  Session.close cold;
  Session.close evaluator;
  let defaults = Lisp_sop.node ~with_:["geometry", (Lisp_sop.snapshot (geometry))] {|(sop/mountain (sop/ext_geometry))|} in
  let evaluator = session () in
  let first = cook evaluator 1 defaults in
  let count = Geometry.point_count geometry in
  let normals = Packed.Float3.of_owned ~x:(Array.make count 1.)
    ~y:(Array.make count 0.) ~z:(Array.make count 0.) |> get in
  let normal = Attribute.create_key_owned (Attribute.normal ~owner:Attribute.Point) normals |> get in
  let changed = Geometry.with_attribute normal geometry |> get in
  let before = Session.stats evaluator in
  let defaults = rewire defaults changed in
  let output = cook evaluator 1 defaults in
  check ((Session.stats evaluator).cooks = before.cooks + 2
      && not (equal_geometry (Result.get_ok (Sop.Payload.geometry first.payload)) (Result.get_ok (Sop.Payload.geometry output.payload))))
    "default mountain reused stale fallback normals";
  let cold = session () in
  check (geometry_bytes (Result.get_ok (Sop.Payload.geometry output.payload)) = geometry_bytes (Result.get_ok (Sop.Payload.geometry (cook cold 8 defaults).payload)))
    "default mountain normal edit differs from a cold cook";
  Session.close cold;
  Session.close evaluator

let () =
  test_reuse false;
  test_reuse true;
  test_dependencies ();
  test_write_footprint ();
  test_refusals ();
  test_refusals ~entries:0 ();
  test_normals_cache_refresh ();
  test_payload_budget ();
  test_parameter_reads ();
  print_endline "component cache facts tests passed"
