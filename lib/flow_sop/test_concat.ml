(* sop/concat is Rdk.Mesh_merge.run on cooked geometries: bytes equal the native merge for
   one, two and three inputs with matching, mismatched and padded attribute/group sets. *)
open Rdk
open Rdk_test_support
module L = Flow_sop.Lower
module Edit = Sop.Edit_graph
let flow = function Ok x->x|Error d->failwith(Diagnostic.to_string d)
let grid c r = Plane_generators.grid ~connectivity:Grid_quads ~columns:c ~rows:r ~size:2. () |> get_ok
let with_float name owner k g =
  let n = match owner with Attribute.Point->Geometry.point_count g|Vertex->Geometry.vertex_count g
    |Primitive->Geometry.primitive_count g|Detail->1 in
  Geometry.with_attribute (Attribute.create_owned ~name ~owner
    (Attribute.Float(Array.init n (fun i->float (i+k)/.3.))) |> get_string_ok) g |> get_string_ok
let with_vec3 name k g =
  let n = Geometry.point_count g in
  Geometry.with_attribute (Attribute.create_owned ~name ~owner:Attribute.Point
    (Attribute.Float3(Packed.Float3.View.of_owned_exn ~x:(Array.init n (fun i->float (i+k)))
      ~y:(Array.make n (-0.)) ~z:(Array.make n 0.5))) |> get_string_ok) g |> get_string_ok
let with_text name g =
  let n = Geometry.primitive_count g in
  Geometry.with_attribute (Attribute.create_owned ~name ~owner:Attribute.Primitive
    (Attribute.Text(Array.init n (fun i->if i mod 2=0 then "猫" else "x\000"))) |> get_string_ok) g |> get_string_ok
let with_group name owner phase g =
  let n = match owner with Group.Point->Geometry.point_count g|Vertex->Geometry.vertex_count g
    |Primitive->Geometry.primitive_count g in
  Geometry.with_group (Group.init ~owner ~name n (fun i->i mod 2=phase)) g |> get_string_ok
let with_ordered name g =
  let n = Geometry.point_count g in
  Geometry.with_group (Group.ordered ~owner:Group.Point ~name ~length:n (Array.init n (fun i->n-1-i))
    |> get_string_ok) g |> get_string_ok
let a () = grid 3 2 |> with_float "h" Attribute.Point 0 |> with_vec3 "v" 0 |> with_group "g" Group.Point 0 |> with_ordered "o"
let b () = grid 2 2 |> with_float "h" Attribute.Point 7 |> with_vec3 "v" 5 |> with_group "g" Group.Point 1
let c () = grid 1 4 |> with_float "h" Attribute.Point 2 |> with_vec3 "v" 1 |> with_text "t"
let mismatched = [ "float-vs-vec3", (fun () -> grid 2 2 |> with_float "h" Attribute.Point 0), (fun () -> grid 2 2 |> with_vec3 "h" 0);
  "missing-attribute", (fun () -> grid 2 2 |> with_float "h" Attribute.Point 0), (fun () -> grid 2 2);
  "detail", (fun () -> grid 2 2 |> with_float "d" Attribute.Detail 0), (fun () -> grid 2 2 |> with_float "d" Attribute.Detail 1) ]
let factory key geometry = Edit.factory ~key ~label:key ~category:["Test"] ~arity:0 (fun _->Ok (Lisp_sop.snapshot geometry))
let cook ?(reference=false) ?(domains=1) ?(grain=97) names extra=
  let geometries = List.map (fun (k,g)->k,g) names in
  let factories = List.map (fun (k,g)->factory k g) geometries @ Sop.Nodes.factories in
  let calls = String.concat " " (List.map (fun (k,_)->"(sop/"^k^")") geometries) in
  let text = Printf.sprintf "(workspace concat_test (graph g :context sop (sop/concat (list %s)%s)))" calls extra in
  let lowered = L.workspace ~reference ~factories ~inputs:["g",[]] (Flow.Syntax.parse text |> flow) |> flow in
  let graph = List.hd lowered.graphs in
  let node = Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok in
  let session = Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000 |> get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    Sop.Session.cook session ~context:(Sop.Context.create ~domains ~grain () |> get_string_ok) node)
let geometry = function Ok (o:Sop.Session.output)->Ok (Sop.Payload.geometry o.payload |> Result.get_ok)
  |Error (d:Diagnostic.t)->Error d
let same what expected actual = match expected,actual with
  |Ok e,Ok a->check (geometry_bytes e=geometry_bytes a) (what^": bytes differ from native merge")
  |Error e,Error (a:Diagnostic.t)->check (Rdk.Error.code e=a.code) (what^": error code differs")
  |_->fail (what^": result kind differs")
let ()=
  let case what sources extra expected =
    let named = List.mapi (fun i g->(List.nth ["src";"dst";"aux"] i), g) sources in
    List.iter (fun reference->List.iter (fun domains->List.iter (fun grain->
      let before = List.map (fun (_,g)->geometry_bytes g) named in
      same what expected (geometry (cook ~reference ~domains ~grain named extra));
      check (before = List.map (fun (_,g)->geometry_bytes g) named) (what^": inputs mutated"))
      [1;97;4096]) [1;4]) [false;true] in
  let native ?source_attribute ?source_base gs = Mesh_merge_front.run ?source_attribute ?source_base gs in
  List.iter (fun (what,gs) -> case what gs "" (native gs))
    ["one", [a ()]; "two", [a (); b ()]; "three-padded", [a (); b (); c ()]; "empty", [];
     "reordered", [c (); b (); a ()]];
  case "tagged" [a (); b (); c ()] " :source_attribute \"src\" :source_base 5"
    (native ~source_attribute:"src" ~source_base:5 [a (); b (); c ()]);
  case "blank-tag" [a (); b ()] " :source_attribute \"  \"" (native [a (); b ()]);
  List.iter (fun (what,x,y) -> case what [x (); y ()] "" (native [x (); y ()])) mismatched;
  print_endline "sop/concat: bytes equal native merge, errors share codes, inputs untouched, reference/CPU/domains/grains"
