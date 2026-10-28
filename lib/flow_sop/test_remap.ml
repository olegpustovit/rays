open Rdk
open Rdk_test_support
module E = Flow.Eval
module L = Flow_sop.Lower
module Edit = Sop.Edit_graph
let flow = function Ok x->x|Error d->failwith(Diagnostic.to_string d)
let parse s=Flow.Syntax.parse s |>flow
let words=[|0L;Int64.min_int;1L;0x7ff0000000000000L;0xfff0000000000000L;
  0x7ff8000000000011L;0x7ff8000000000022L|]
(* Weighted arithmetic does not define which NaN payload survives, so its test canonicalises them. *)
let canonical_nan=ref false
let raw i=let v=Int64.float_of_bits words.(i mod Array.length words) in
  if !canonical_nan && Float.is_nan v then Int64.float_of_bits 0x7ff8000000000000L else v
let source count =
  let topology=if count<3 then Topology.empty ~point_count:count else if count=4 then
    Topology.create_owned ~point_count:count ~vertex_points:[|0;1;2;1;2;3|]
      ~primitive_offsets:[|0;3;6|] ~primitive_kinds:[|Topology.Polygon;Topology.Polygon|] |>get_string_ok else
    Topology.create_owned ~point_count:count ~vertex_points:[|0;1;2|]
      ~primitive_offsets:[|0;3|] ~primitive_kinds:[|Topology.Polygon|] |>get_string_ok in
  let attrs=List.concat_map(fun owner->let n=match owner with Attribute.Point->count
      |Vertex->Topology.vertex_count topology|Primitive->Topology.primitive_count topology|Detail->1 in
    let floats k=Array.init n(fun i->raw(i+k)) in
    let offsets=Array.init(n+1)(fun i->i*16) in
    let kinds=["N",Attribute.Float3(Packed.Float3.Private.of_owned_exn ~x:(Array.init n(fun i->float i-.1.))
        ~y:(Array.init n(fun i->0.25*.float(i mod 3))) ~z:(Array.init n(fun i->if i mod 4=0 then 0. else 2.)));
      "float",Attribute.Float(floats 0);"int",Int(Array.init n(fun i->(1 lsl 55)+i));
      "text",Text(Array.init n(fun i->if i mod 2=0 then "猫\255" else "😀\000"));
      "vec2",Float2(Packed.Float2.of_owned ~x:(floats 1) ~y:(floats 2)|>get_string_ok);
      "vec3",Float3(Packed.Float3.Private.of_owned_exn ~x:(floats 3) ~y:(floats 4) ~z:(floats 5));
      "vec4",Float4(Packed.Float4.of_owned ~x:(floats 2) ~y:(floats 3) ~z:(floats 4) ~w:(floats 5)|>get_string_ok);
      "mat16",Float_array(Packed.Float_array.create_owned ~offsets
        ~values:(Array.init(n*16)raw)|>get_string_ok);
      "ragged_float",Float_array(Packed.Float_array.create_owned
        ~offsets:(Array.init(n+1)(fun i->i/2)) ~values:(Array.init(n/2)raw)|>get_string_ok);
      "ragged",Int_array(Packed.Int_array.create_owned
        ~offsets:(Array.init(n+1)(fun i->3*(i/3)+(i mod 3)*(i mod 3-1)/2))
        ~values:(Array.init(3*(n/3)+(n mod 3)*(n mod 3-1)/2)(fun i->(1 lsl 55)+i))|>get_string_ok)] in
    List.map(fun(name,storage)->Attribute.create_owned ~name ~owner storage |>get_string_ok)kinds)
    [Attribute.Point;Vertex;Primitive;Detail] in
  let groups=List.concat_map(fun owner->let n=match owner with Group.Point->count
      |Vertex->Topology.vertex_count topology|Primitive->Topology.primitive_count topology in
    [Group.init ~owner ~name:"plain" n(fun i->i mod 2=0);
     Group.ordered ~owner ~name:"ordered" ~length:n (Array.init n(fun i->n-1-i))|>get_string_ok;
     Group.ordered ~owner ~name:"ordered_empty" ~length:n [||]|>get_string_ok])
    [Group.Point;Vertex;Primitive] in
  let index=Topology_index.create topology in
  let edge_groups=[Edge_group.init ~topology ~index ~name:"edges"(fun i->i mod 2=0)] in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn
    ~x:(Array.init count raw) ~y:(Array.init count(fun i->raw(i+1))) ~z:(Array.init count(fun i->raw(i+2))))
    ~topology ~attributes:attrs ~groups ~edge_groups () |>get_string_ok
let target copies source =
  let t=Topology.Private.view(Geometry.topology source) in
  let points=Geometry.point_count source and vertices=Geometry.vertex_count source
  and primitives=Geometry.primitive_count source in
  let topology=Topology.create_owned ~point_count:(copies*points)
    ~vertex_points:(Array.init(copies*vertices)(fun i->t.vertex_points.(i mod vertices)+(i/vertices)*points))
    ~primitive_offsets:(Array.init(copies*primitives+1)(fun i->if i=copies*primitives then copies*vertices
      else t.primitive_offsets.(i mod primitives)+(i/primitives)*vertices))
    ~primitive_kinds:(Array.init(copies*primitives)(fun _->Topology.Polygon)) |>get_string_ok in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn ~x:(Array.make(copies*points)3.)
    ~y:(Array.make(copies*points)(-0.)) ~z:(Array.make(copies*points)7.)) ~topology () |>get_string_ok
let factory key geometry=Edit.factory ~key ~label:key ~category:["Test"] ~arity:0(fun _->Lisp_sop.snapshot geometry)
let source_text ?(edge=true) body=Printf.sprintf {|(workspace remap_test
  (graph g :context sop [(points : (array int) (array/int 0))
    (vertices : (array int) (array/int 0)) (primitives : (array int) (array/int 0))
    (positions : text "target")
    (pa : (array int) (array/int 0)) (pb : (array int) (array/int 0)) (pt : (array float) (array/float 0 0.0))
    (va : (array int) (array/int 0)) (vb : (array int) (array/int 0)) (vt : (array float) (array/float 0 0.0))
    (ra : (array int) (array/int 0)) (rb : (array int) (array/int 0)) (rt : (array float) (array/float 0 0.0))
    (edge_map : {:offsets (array int) :values (array int)} {:offsets (array/int 1) :values (array/int 0)})]
    (let* [source (sop/src) target (sop/dst) tested %s] tested)))|}
    (if body="" then "(sop/remap source target :points points :vertices vertices :primitives primitives :positions positions"^
      (if edge then " :edges edge_map)" else ")") else body)
let compile ?(reference=false) ?(edge=true) ?(body="") source target args=
  let factories=factory "src" source::factory "dst" target::Sop.Nodes.factories in
  let lowered=L.workspace ~reference ~factories ~inputs:["g",args](parse(source_text ~edge body)) |>flow in
  let graph=List.hd lowered.graphs in
  Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>get_string_ok
let run ?(domains=1) ?(grain=97) ?cancel node=
  let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    Sop.Session.cook session ~context:(Sop.Context.create ~domains ~grain ?cancel ()|>get_string_ok) node)
let geometry=function Ok(o:Sop.Session.output)->Sop.Payload.geometry o.payload |>Result.get_ok
  |Error d->failwith(Diagnostic.error_to_string d)
let args p v prim offsets values policy=["points",E.Int_array p;"vertices",E.Int_array v;
  "primitives",E.Int_array prim;"positions",E.Text policy;
  "edge_map",E.Record["offsets",E.Int_array offsets;"values",E.Int_array values]]
let oracle source target p v prim offsets ancestors mapped=
  let attrs=Rdk_core.Topology_remap.attributes ~grain:97 ~point_map:p ~vertex_map:v ~primitive_map:prim source
  and groups=Rdk_core.Topology_remap.groups ~grain:97 ~point_map:p ~vertex_map:v ~primitive_map:prim source in
  let topology=Geometry.topology target and index=Topology_index.create(Geometry.topology target) in
  let edges=List.map(fun group->Edge_group.init ~topology ~index ~name:(Edge_group.name group)
    (fun edge->let member=ref false in for at=offsets.(edge) to offsets.(edge+1)-1 do
      member:= !member || Edge_group.mem ancestors.(at) group done;!member))(Geometry.edge_groups source) in
  let positions=if not mapped then Geometry.positions target else
    let view=Packed.Float3.Private.view(Geometry.positions source) in
    Packed.Float3.Private.of_owned_exn ~x:(Array.map(fun i->view.x.(i))p)
      ~y:(Array.map(fun i->view.y.(i))p) ~z:(Array.map(fun i->view.z.(i))p) in
  Geometry.create ~positions ~topology ~attributes:attrs ~groups ~edge_groups:edges () |>get_string_ok
let ()=
  List.iter(fun count->let src=source count in
    List.iter(fun copies->let dst=target copies src in
      let p=Array.init(Geometry.point_count dst)(fun i->i mod count)
      and v=Array.init(Geometry.vertex_count dst)(fun i->i mod Geometry.vertex_count src)
      and prim=Array.init(Geometry.primitive_count dst)(fun i->i mod Geometry.primitive_count src) in
      let edges=Topology_index.edge_count(Topology_index.create(Geometry.topology dst))
      and se=Topology_index.edge_count(Topology_index.create(Geometry.topology src)) in
      let offsets=Array.init(edges+1)Fun.id and ancestors=Array.init edges(fun i->i mod se) in
      List.iter(fun policy->let expected=oracle src dst p v prim offsets ancestors(policy="mapped") in
        List.iter(fun reference->let node=compile ~reference src dst(args p v prim offsets ancestors policy) in
          List.iter(fun domains->List.iter(fun grain->
            let before=geometry_bytes src and target_before=geometry_bytes dst in
            let actual=geometry(run ~domains ~grain node) in
            check(geometry_bytes actual=geometry_bytes expected)"remap native raw storage/order/edge bytes differ";
            check(geometry_bytes src=before && geometry_bytes dst=target_before)"remap mutated inputs";
            List.iter(fun a->if Attribute.owner a=Detail then
              check((Geometry.find_attribute ~owner:Detail(Attribute.name a)actual |>Option.get)==a)"Detail payload changed")
              (Geometry.attributes src)) [1;97;4096]) [1;4;8]) [false;true]) ["target";"mapped"])
      [0;1;2]) [0;3];
  print_endline "C3 all owner/storage raw bytes, ordinary order and edge copies across reference/CPU/domains/grains"
let ()=
  (* A source index of -1 is a created element: every storage at its default, in no group.  Other rows
     are the native copy (the same Topology_remap helper with -1 replaced by row 0). *)
  let bits=Int64.bits_of_float in
  let src=source 3 in
  List.iter(fun copies->let dst=target copies src in
    let created n=fun i->i mod 3=1 && i<n in
    let mk n real=Array.init n(fun i->if created n i then -1 else real i) in
    let np=Geometry.point_count dst and nv=Geometry.vertex_count dst and nr=Geometry.primitive_count dst in
    let p=mk np(fun i->(i+2) mod 3) and v=mk nv(fun i->i mod 3) and prim=mk nr(fun i->i mod Geometry.primitive_count src) in
    let prim=if nr>0 && copies=1 then [|-1|] else prim in
    let zero map=Array.map(fun i->max i 0)map in
    let edges=Topology_index.edge_count(Topology_index.create(Geometry.topology dst))
    and se=Topology_index.edge_count(Topology_index.create(Geometry.topology src)) in
    let offsets=Array.init(edges+1)Fun.id and ancestors=Array.init edges(fun i->i mod se) in
    let native=oracle src dst(zero p)(zero v)(zero prim)offsets ancestors true in
    let attribute_map=function Attribute.Point->p|Attribute.Vertex->v|Attribute.Primitive->prim|Attribute.Detail->[||]
    and group_map=function Group.Point->p|Group.Vertex->v|Group.Primitive->prim in
    let row_ok map i=map.(i)>=0 in
    List.iter(fun reference->let node=compile ~reference src dst(args p v prim offsets ancestors "mapped") in
      List.iter(fun domains->
        let actual=geometry(run ~domains ~grain:97 node) in
        let eqf map a b=Array.iteri(fun i x->check(bits x=bits(if row_ok map i then b.(i) else 0.))
          "created float row is not zero or copied row differs")a in
        let ragged map offs vals expected_offs expected_vals cmp=Array.iteri(fun i _->if i<Array.length map then begin
          let w=offs.(i+1)-offs.(i) in
          if row_ok map i then begin
            check(w=expected_offs.(i+1)-expected_offs.(i))"copied ragged row width differs";
            for k=0 to w-1 do check(cmp vals.(offs.(i)+k) expected_vals.(expected_offs.(i)+k))"copied ragged value differs" done
          end else check(w=0)"created ragged row is not empty" end)map in
        List.iter(fun a->
          let map=attribute_map(Attribute.owner a) in
          let n=Option.get(Geometry.find_attribute ~owner:(Attribute.owner a)(Attribute.name a)native) in
          if Attribute.owner a=Detail then () else
          match Attribute.Private.storage a,Attribute.Private.storage n with
          |Float x,Float y->eqf map x y
          |Int x,Int y->Array.iteri(fun i e->check(e=(if row_ok map i then y.(i) else 0))"created int row is not 0")x
          |Text x,Text y->Array.iteri(fun i e->check(e=(if row_ok map i then y.(i) else ""))"created text row is not empty")x
          |Float2 x,Float2 y->let x=Packed.Float2.Private.view x and y=Packed.Float2.Private.view y in eqf map x.x y.x;eqf map x.y y.y
          |Float3 x,Float3 y->let x=Packed.Float3.Private.view x and y=Packed.Float3.Private.view y in
            eqf map x.x y.x;eqf map x.y y.y;eqf map x.z y.z
          |Float4 x,Float4 y->let x=Packed.Float4.Private.view x and y=Packed.Float4.Private.view y in
            eqf map x.x y.x;eqf map x.y y.y;eqf map x.z y.z;eqf map x.w y.w
          |Int_array x,Int_array y->let x=Packed.Int_array.Private.view x and y=Packed.Int_array.Private.view y in
            ragged map x.offsets x.values y.offsets y.values (=)
          |Float_array x,Float_array y->let x=Packed.Float_array.Private.view x and y=Packed.Float_array.Private.view y in
            ragged map x.offsets x.values y.offsets y.values (fun a b->bits a=bits b)
          |_->fail "remap changed a storage kind")(Geometry.attributes actual);
        List.iter(fun g->let map=group_map(Group.owner g) in
          let n=Option.get(Geometry.find_group ~owner:(Group.owner g)(Group.name g)native) in
          Array.iteri(fun i _->check(Group.mem i g=(row_ok map i && Group.mem i n))"created row is in a group or copied membership differs")map)
          (Geometry.groups actual);
        let view=Packed.Float3.Private.view(Geometry.positions actual) and expect=Packed.Float3.Private.view(Geometry.positions native) in
        eqf p view.x expect.x;eqf p view.y expect.y;eqf p view.z expect.z) [1;8]) [false;true]) [1;2];
  print_endline "remap -1 rows: defaults for every owner and storage, in no group, 1 and 8 domains"
let ()=
  (* Independent permutation and many-to-one edge ancestry; empty CSR rows and
     repeated ancestors must be valid. *)
  let src=source 3 and dst=target 1(source 3) in
  let p=[|2;1;0|] and v=[|1;2;0|] and prim=[|0|]
  and offsets=[|0;0;2;4|] and ancestors=[|0;1;1;1|] in
  let expected=oracle src dst p v prim offsets ancestors true in
  List.iter(fun reference->let node=compile ~reference src dst(args p v prim offsets ancestors "mapped") in
    check(geometry_bytes(geometry(run node))=geometry_bytes expected)"permutation/edge OR bytes differ") [false;true];
  let src=source 3 in
  let identity=args [|0;1;2|][|0;1;2|][|0|][|0;1;2;3|][|0;1;2|]"mapped" in
  check(geometry_bytes(geometry(run(compile ~edge:false src src identity)))=geometry_bytes src)"identity changed payload";
  (* Actual Lisp composition retains native copy-major ordered groups. *)
  let dst=target 2 src in
  let inputs=args [|0;1;2;0;1;2|][|0;1;2;0;1;2|][|0;0|]
    [|0;1;2;3;4;5;6|][|0;1;2;0;1;2|]"mapped" in
  let body={|(let* [out (sop/remap source target :points points :vertices vertices :primitives primitives
      :positions positions :edges edge_map) old_order (sop/group_order source :name "ordered" :owner "point")]
    (sop/with_group out (sop/group out :name "ordered" :owner "point") :name "ordered" :owner "point"
      :order (array/concat old_order.indices (exact (map (fn [(i : int)] (int/add i 3)) old_order.indices)))))|} in
  List.iter(fun reference->let actual=geometry(run(compile ~reference ~body src dst inputs)) in
    let order=Geometry.find_group ~owner:Group.Point "ordered" actual |>Option.get |>Group.ordered_elements |>Option.get in
    check(order=[|2;1;0;5;4;3|])"existing Lisp group-order override is not copy-major") [false;true]
let ()=
  let src=source 3 and dst=target 1(source 3) in
  let valid=args [|0;1;2|][|0;1;2|][|0|][|0;1;2;3|][|0;1;2|]"target" in
  let replace key value=List.map(fun(k,old)->k,if key=k then value else old)valid in
  List.iter(fun(key,value,code)->match run(compile src dst(replace key value)) with
    |Error d->check(d.code=code)"remap typed validation code changed"
    |Ok _->fail "remap accepted malformed argument")
    ["positions",E.Text"other","E_REMAP_POLICY";
     "points",E.Int_array[||],"E_REMAP_COUNT";
     "points",E.Int_array[|-2;1;2|],"E_REMAP_INDEX";
     "vertices",E.Int_array[|0;1;9|],"E_REMAP_INDEX";
     "primitives",E.Int_array[|1|],"E_REMAP_INDEX";
     "edge_map",E.Record["offsets",E.Int_array[|0;2;1;3|];"values",E.Int_array[|0;1;2|]],"E_REMAP_EDGES"];
  (match run(compile ~edge:false src dst valid) with Error d->check(d.code="E_REMAP_EDGES")"edge relation omission code"
    |Ok _->fail "changed edge topology accepted implicit ancestry");
  let cancel=Cancel.create() in Cancel.cancel cancel;
  (match run ~cancel(compile src dst valid) with Error d->check(d.code="cancelled" || d.code="E_CANCELLED")"cancellation changed"
    |Ok _->fail "cancelled remap cooked")
let ()=
  let src=source 3 and dst=target 1(source 3) in
  let inputs=args [|0;1;2|][|0;1;2|][|0|][|0;1;2;3|][|0;1;2|]"target" in
  let node=compile src dst inputs in
  let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    let context=Sop.Context.create()|>get_string_ok in
    ignore(Sop.Session.cook session ~context node);let first=Sop.Session.stats session in
    ignore(Sop.Session.cook session ~context node);
    check((Sop.Session.stats session).misses=first.misses)"unchanged remap cache missed";
    let payload=Geometry.with_attribute(Attribute.create_owned ~owner:Attribute.Point ~name:"fresh"
      (Attribute.Int[|1;2;3|])|>get_string_ok)src|>get_string_ok in
    let rebuild=Sop.Node.Private.rebuild_with_inputs node[|Lisp_sop.snapshot payload;Lisp_sop.snapshot dst|]
      |>Sop.Node.Private.adopt_identity ~source:node in
    let actual=Sop.Session.cook session ~context rebuild |>geometry in
    check(Option.is_some(Geometry.find_attribute ~owner:Attribute.Point "fresh" actual))"cached remap stale source payload";
    check((Sop.Session.stats session).misses>first.misses)"metadata-only source update did not miss")
let ()=
  let src=source 3 and dst=target 1(source 3) in
  let factories=factory "src" src::factory "dst" dst::Sop.Nodes.factories in
  let catalog=Flow_sop.Catalog.of_factories ~version:1 factories|>flow in
  let forms=parse(source_text "") in
  let checker=Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all in
  let checked=match checker catalog forms with Some checked,ds when not(List.exists(fun(d:Diagnostic.t)->d.severity=Diagnostic.Error_severity)ds)->checked
    |_->fail "remap projection did not check" in
  let projection=Flow_graph.Projection.of_graph catalog checked "g" in
  check(List.exists(fun(n:Flow_graph.Projection.node)->n.head="sop/remap")projection.nodes)"remap has no graph card";
  List.iter(fun(key,text)->ignore(flow(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"tested"];key=Kw key;sub=[];value=List.hd(parse text)}))))
    ["points","(array/int 0)";"vertices","(array/int 0)";"primitives","(array/int 0)";
     "positions","\"mapped\"";"edges","{:offsets (array/int 1) :values (array/int 0)}"];
  check(Result.is_error(Flow_graph.Flow_edit.apply_checked ~checker catalog forms
    (Set_arg{node=["g";"tested"];key=Kw"points";sub=[];value=List.hd(parse"(array/text 0)")})))
    "remap graph type-changing malformed map edit accepted"

let ()=
  (* Crossing the packed cutoff with all native storage kept opaque. *)
  let src=source 32769 in
  let dst=Geometry.with_positions(Packed.Float3.Private.of_owned_exn
    ~x:(Array.make 32769 1.) ~y:(Array.make 32769 2.) ~z:(Array.make 32769 3.))src |>get_string_ok in
  let p=Array.init 32769(fun i->(i+1)mod 32769) and v=[|0;1;2|] and prim=[|0|]
  and offsets=[|0;1;2;3|] and ancestors=[|0;1;2|] in
  let expected=oracle src dst p v prim offsets ancestors true in
  List.iter(fun reference->let node=compile ~reference src dst(args p v prim offsets ancestors "mapped") in
    List.iter(fun domains->List.iter(fun grain->check
      (geometry_bytes(geometry(run ~domains ~grain node))=geometry_bytes expected)
      "32769-row all-storage remap differs across CPU/reference/domains/grains") [1;97;4096]) [1;4;8]) [false;true]
let ()=
  let src=source 3 and dst=target 2(source 3) in
  let inputs=args [|0;1;2;0;1;2|][|0;1;2;0;1;2|][|0;0|]
    [|0;1;2;3;4;5;6|][|0;1;2;0;1;2|]"mapped" in
  let actual=geometry(run(compile src dst inputs)) and before=geometry_bytes src in
  List.iter(fun a->let copied=Geometry.find_attribute ~owner:(Attribute.owner a)(Attribute.name a)actual |>Option.get in
    if Attribute.owner a=Detail then check(copied==a)"Detail storage identity changed"
    else check(Attribute.storage_id copied<>Attribute.storage_id a)"duplicated owner storage is not owned")
    (Geometry.attributes src);
  (match Geometry.find_attribute ~owner:Attribute.Point "float" actual |>Option.get |>Attribute.Private.storage with
   |Float values->values.(0)<-99.|_->assert false);
  check(geometry_bytes src=before)"mutating an owned output changed source bytes";
  let body={|(if false
    (sop/remap source target :points (array/int 1 -1) :vertices vertices :primitives primitives :edges edge_map)
    source)|} in
  List.iter(fun reference->check(geometry_bytes(geometry(run(compile ~reference ~body src dst inputs)))=before)
    "untaken malformed remap branch was evaluated") [false;true];
  let body={|(let* [apply_remap (fn [(p : (array int))]
    (sop/remap source target :points p :vertices vertices :primitives primitives :edges edge_map :positions "mapped"))]
    (apply_remap (exact (map (fn [(i : int)] i) points))))|} in
  List.iter(fun reference->ignore(geometry(run(compile ~reference ~body src dst inputs)))) [false;true];
  let body={|(let* [chosen (if (< t 1.0) points (exact (map (fn [(i : int)] (int/mod (int/add i 1) 3)) points)))]
    (sop/remap source target :points chosen :vertices vertices :primitives primitives :edges edge_map :positions "mapped"))|} in
  let factories=factory "src" src::factory "dst" dst::Sop.Nodes.factories in
  let lowered=L.workspace ~factories ~inputs:["g",inputs](parse(source_text body))|>flow in
  let graph=List.hd lowered.graphs in
  let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    let network=graph.network in
    let node=Edit.compile_node network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
    let first=Sop.Session.cook session ~context:(Sop.Context.create ~time:0.()|>get_string_ok)node|>geometry in
    let second=Sop.Session.cook session ~context:(Sop.Context.create ~time:2.()|>get_string_ok)node|>geometry in
    check(geometry_bytes first<>geometry_bytes second)"live remap map remained stale")

let ()=
  (* The strict public defnode path computes the maps through genuine typed
     integer kernels, then transfers raw source payload without numeric reads. *)
  let src=source 3 and dst=target 10923(source 3) in
  let text={|(workspace strict_remap
    (defnode sop/strict_remap [(src : geometry) (dst : geometry)]
      (let* [points (exact (map (fn [(i : int)] (int/mod i 3))
                    (array/range (sop/count dst :points) :type int)))
             vertices (exact (map (fn [(i : int)] (int/mod i 3))
                      (array/range (sop/count dst :vertices) :type int)))
             primitives (array/int (sop/count dst :primitives) 0)
             edge_count (sop/count dst :edges)
             ancestry (exact (map (fn [(i : int)] (int/mod i 3))
                      (array/range edge_count :type int)))]
        (sop/remap src dst :points points :vertices vertices :primitives primitives
          :positions "mapped" :edges {:offsets (array/range (int/add edge_count 1) :type int) :values ancestry})))
    (graph g :context sop (sop/strict_remap (sop/src) (sop/dst))))|} in
  let p=Array.init 32769(fun i->i mod 3) and prim=Array.make 10923 0
  and offsets=Array.init 32770 Fun.id in
  let expected=oracle src dst p p prim offsets p true in
  List.iter(fun reference->let lowered=L.workspace ~reference
    ~factories:(factory "src" src::factory "dst" dst::Sop.Nodes.factories)(parse text)|>flow in
    let graph=List.hd lowered.graphs in
    let node=Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
    List.iter(fun domains->List.iter(fun grain->check
      (geometry_bytes(geometry(run ~domains ~grain node))=geometry_bytes expected)
      "strict generated maps/raw payload differ from native oracle") [1;97;4096]) [1;4;8]) [false;true]

let ()=
  (* A real failed child remains the first failure, with its diagnostic envelope
     and provenance unchanged; remap does not recast it as a mapping error. *)
  let child=Diagnostic.error ~cause:"raw child cause" ~hints:["keep this hint"]
    ~code:"E_CHILD" "child failed" in
  let bad=Sop.Node.Private.make_geometry ~label:"Child" ~operation:"test.remap_child"
    ~version:1 ~parameters:"" ~cook_mode:Sop.Node.Generator
    ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _ _->Error child) in
  let src=source 3 and dst=target 1(source 3) in
  let node=compile src dst(args [|0;1;2|][|0;1;2|][|0|][|0;1;2;3|][|0;1;2|]"target") in
  let node=Sop.Node.Private.rebuild_with_inputs node [|bad;Lisp_sop.snapshot dst|] in
  List.iter(fun domains->match run ~domains node with
    |Ok _->fail "remap swallowed child error"
    |Error diagnostic->check(diagnostic.code=child.code && diagnostic.message=child.message
       && diagnostic.cause=child.cause && diagnostic.hints=child.hints
       && List.exists(fun(trace:Diagnostic.trace)->trace.operation="test.remap_child")diagnostic.trace)
       "remap changed a failed child diagnostic") [1;4;8]

let ()=
  (* Target connectivity need not be obtainable by permuting the source. It is
     supplied explicitly; only owner payload and edge ancestry come from src. *)
  let src=source 3 in
  let topology=Topology.create_owned ~point_count:4 ~vertex_points:[|0;1;2;3;3;2|]
    ~primitive_offsets:[|0;4;6|] ~primitive_kinds:[|Topology.Closed_polyline;Open_polyline|]|>get_string_ok in
  let dst=Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn
    ~x:[|1.;2.;3.;4.|] ~y:[|0.;0.;0.;0.|] ~z:[|-0.;0.;-0.;0.|]) ~topology ()|>get_string_ok in
  let p=[|2;1;0;2|] and v=[|2;1;0;2;1;0|] and prim=[|0;0|] in
  let edges=Topology_index.edge_count(Topology_index.create topology) in
  let offsets=Array.init(edges+1)(fun i->2*i) and ancestors=Array.init(2*edges)(fun i->i mod 3) in
  List.iter(fun policy->let expected=oracle src dst p v prim offsets ancestors(policy="mapped") in
    List.iter(fun reference->let node=compile ~reference src dst(args p v prim offsets ancestors policy) in
      List.iter(fun domains->List.iter(fun grain->check
        (geometry_bytes(geometry(run ~domains ~grain node))=geometry_bytes expected)
        "explicit authored target topology or raw P policy differs") [1;97;4096]) [1;4;8]) [false;true])
    ["target";"mapped"]

let ()=
  (* Preserve one logical consumer identity while changing every semantic input
     independently; the warm result must equal a fresh-session oracle. *)
  let src=source 3 and dst=target 1(source 3) in
  let base=args [|0;1;2|][|0;1;2|][|0|][|0;1;2;3|][|0;1;2|]"target" in
  let replace key value=List.map(fun(name,old)->name,if name=key then value else old)base in
  let target_p=Geometry.with_positions(Packed.Float3.Private.of_owned_exn
    ~x:[|9.;8.;7.|] ~y:[|0.;0.;0.|] ~z:[|0.;0.;0.|])dst|>get_string_ok in
  let topology=Topology.create_owned ~point_count:3 ~vertex_points:[|2;1;0|]
    ~primitive_offsets:[|0;3|] ~primitive_kinds:[|Topology.Polygon|]|>get_string_ok in
  let target_topology=Geometry.create ~positions:(Geometry.positions dst) ~topology ()|>get_string_ok in
  let source_order=Geometry.with_group(Group.ordered ~owner:Group.Point ~name:"ordered"
    ~length:3 [|1;0;2|]|>get_string_ok)src|>get_string_ok in
  let source_edges=Geometry.with_edge_group(Edge_group.init ~topology:(Geometry.topology src)
    ~index:(Topology_index.create(Geometry.topology src)) ~name:"edges"(fun i->i=1))src|>get_string_ok in
  let variants=[src,dst,replace "points"(E.Int_array[|1;2;0|]);
    src,dst,replace "vertices"(E.Int_array[|1;2;0|]);
    src,dst,replace "positions"(E.Text"mapped");
    src,dst,replace "edge_map"(E.Record["offsets",E.Int_array[|0;1;2;3|];"values",E.Int_array[|2;1;0|]]);
    src,target_p,base;src,target_topology,base;source_order,dst,base;source_edges,dst,base] in
  let node=compile src dst base in
  let session=Sop.Session.create ~max_entries:128 ~max_payload_bytes:64_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    let context=Sop.Context.create()|>get_string_ok in
    ignore(Sop.Session.cook session ~context node);
    List.iter(fun(source,target,values)->
      let changed=compile source target values|>Sop.Node.Private.adopt_identity ~source:node in
      let before=Sop.Session.stats session in
      let output=Sop.Session.cook session ~context changed|>geometry in
      check((Sop.Session.stats session).misses>before.misses)"changed remap semantic input hit stale cache";
      check(geometry_bytes output=geometry_bytes(geometry(run changed)))"warm remap differs from fresh output";
      let warm=Sop.Session.stats session in ignore(Sop.Session.cook session ~context changed);
      check((Sop.Session.stats session).misses=warm.misses)"unchanged remap variant did not hit")variants)

let ()=
  let src=source 3 in
  let changed_p=Geometry.with_positions(Packed.Float3.Private.of_owned_exn
    ~x:[|8.;7.;6.|] ~y:[|-0.;0.;-0.|] ~z:[|2.;3.;4.|])src|>get_string_ok in
  let inputs=args [|0;1;2|][|0;1;2|][|0|][|0;1;2;3|][|0;1;2|]"target" in
  List.iter(fun reference->let actual=geometry(run(compile ~reference ~edge:false src changed_p inputs)) in
    check(geometry_bytes actual=geometry_bytes changed_p)"same topology/changed P lost implicit edge affinity";
    match run(compile ~reference ~edge:false src(target 1 src)inputs)with
    |Error d->check(d.code="E_REMAP_EDGES")"equal distinct topology did not refuse implicit ancestry"
    |Ok _->fail "value-equal distinct topology implicitly inherited edge affinity") [false;true]

let ()=
  let original=source 3 and repeated=target 2(source 3) in
  let map=[|0;1;2;0;1;2|] and offsets=Array.init 7 Fun.id in
  let src=oracle original repeated map map [|0;0|] offsets map true in
  let src=Geometry.with_attribute(Attribute.create_owned ~owner:Attribute.Primitive ~name:"float"
    (Attribute.Float[|raw 5;raw 6|])|>get_string_ok)src|>get_string_ok in
  let src=Geometry.with_group(Group.ordered ~owner:Group.Primitive ~name:"ordered"
    ~length:2 [|1;0|]|>get_string_ok)src|>get_string_ok in
  let dst=target 1 src and identity=Array.init 6 Fun.id in
  let base=args identity identity [|0;1|] offsets identity "mapped"
  and permuted=args identity identity [|1;0|] offsets identity "mapped" in
  let expected=oracle src dst identity identity [|1;0|] offsets identity true in
  let session=Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000|>get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    let node=compile src dst base in
    let context=Sop.Context.create()|>get_string_ok in
    ignore(Sop.Session.cook session ~context node);let before=Sop.Session.stats session in
    let node=compile src dst permuted|>Sop.Node.Private.adopt_identity ~source:node in
    let actual=Sop.Session.cook session ~context node|>geometry in
    check((Sop.Session.stats session).misses>before.misses)"primitive ancestry cache input ignored";
    check(geometry_bytes actual=geometry_bytes expected)"primitive NaN payload/order permutation differs");
  List.iter(fun reference->let node=compile ~reference src dst permuted in
    List.iter(fun domains->List.iter(fun grain->check
      (geometry_bytes(geometry(run ~domains ~grain node))=geometry_bytes expected)
      "primitive permutation differs by domain/grain/reference") [1;97;4096]) [1;4;8]) [false;true]

let ()=
  (* Valid geometry-host values cannot reach these native helper trust guards. *)
  let src=source 3 in
  let dst=target 1 src and unrelated=source 3 in
  let source_index=Topology_index.create(Geometry.topology src)
  and target_index=Topology_index.create(Geometry.topology dst) in
  let offsets=[|0;1;2;3|] and ancestors=[|0;1;2|] in
  let invoke ~grain ~topology groups=Rdk_core.Topology_remap.edge_groups_from_ancestry
    ~grain ~source_index ~target_topology:topology ~target_index ~offsets ~ancestors groups in
  List.iter(fun(result,message)->match result with
    |Error actual->check(actual="Topology_remap.edge_groups_from_ancestry: "^message)
        "edge ancestry helper trust-boundary diagnostic changed"
    |Ok _->fail "edge ancestry helper accepted an invalid native boundary")
    [invoke ~grain:0 ~topology:(Geometry.topology dst)(Geometry.edge_groups src),"grain must be positive";
     invoke ~grain:(-1) ~topology:(Geometry.topology dst)(Geometry.edge_groups src),"grain must be positive";
     invoke ~grain:97 ~topology:(Geometry.topology src)(Geometry.edge_groups src),"target index affinity mismatch";
     invoke ~grain:97 ~topology:(Geometry.topology dst)(Geometry.edge_groups unrelated),"source group affinity mismatch"]

(* Weighted carry: every owner and storage against an independent reference, the shared helper's
   bytes against Curve_join.interpolate_attribute on point and vertex attributes, mixed plain and
   weighted owners, reference/CPU, 1 and 8 domains, and the refusals. *)
let () =
  let mix_of ~count ~shift n =
    let weights=[|0.;0.25;0.5;0.75;1.;0.4999999999;1.5;-0.5;0.5000000001|] in
    Array.init n (fun i -> (i*shift+shift) mod count), Array.init n (fun i -> (i*2+1+shift) mod count),
    Array.init n (fun i -> weights.((i+shift) mod Array.length weights)) in
  let mix_args (pa,pb,pt) (va,vb,vt) (ra,rb,rt) policy offsets ancestors=
    ["pa",E.Int_array pa;"pb",E.Int_array pb;"pt",E.Float_array pt;
     "va",E.Int_array va;"vb",E.Int_array vb;"vt",E.Float_array vt;
     "ra",E.Int_array ra;"rb",E.Int_array rb;"rt",E.Float_array rt;
     "positions",E.Text policy;
     "edge_map",E.Record["offsets",E.Int_array offsets;"values",E.Int_array ancestors]] in
  let body ~points ~vertices ~primitives=Printf.sprintf
    "(sop/remap source target %s %s %s :positions positions :edges edge_map)"
    (if points then ":point_mix {:a pa :b pb :t pt}" else ":points pa")
    (if vertices then ":vertex_mix {:a va :b vb :t vt}" else ":vertices va")
    (if primitives then ":primitive_mix {:a ra :b rb :t rt}" else ":primitives ra") in
  let nearest (l,r,t)=Array.mapi(fun i w->if w<0.5 then l.(i) else r.(i))t in
  let mix a b t=let d=b-.a in if Float.is_finite d then a+.d*.t else a*.(1.-.t)+.b*.t in
  let blend_plane (l,r,t) plane=Array.mapi(fun i w->mix plane.(l.(i)) plane.(r.(i)) w)t in
  let reference_attribute carry attribute=
    let (l,r,t)=carry in
    let plane=blend_plane carry in
    let storage=match Attribute.Private.storage attribute with
      |Attribute.Float v->Attribute.Float(plane v)
      |Float2 v->let v=Packed.Float2.Private.view v in
        Float2(Packed.Float2.of_owned ~x:(plane v.x) ~y:(plane v.y)|>get_string_ok)
      |Float3 v->let v=Packed.Float3.Private.view v in
        let x=plane v.x and y=plane v.y and z=plane v.z in
        if Attribute.name attribute="N" then
          Array.iteri(fun i _->let length=sqrt(x.(i)*.x.(i)+.y.(i)*.y.(i)+.z.(i)*.z.(i)) in
            if length>1e-20 then (x.(i)<-x.(i)/.length;y.(i)<-y.(i)/.length;z.(i)<-z.(i)/.length))x;
        Float3(Packed.Float3.Private.of_owned_exn ~x ~y ~z)
      |Float4 v->let v=Packed.Float4.Private.view v in
        Float4(Packed.Float4.of_owned ~x:(plane v.x) ~y:(plane v.y) ~z:(plane v.z) ~w:(plane v.w)|>get_string_ok)
      |_->Attribute.Private.storage(Rdk_core.Topology_remap.attribute ~grain:97 (nearest(l,r,t)) attribute) in
    ignore (l,r);
    Attribute.create_owned ~name:(Attribute.name attribute) ~owner:(Attribute.owner attribute) storage|>get_string_ok in
  canonical_nan:=true;
  let src=source 4 in
  List.iter(fun copies->
    let dst=target copies src in
    let np=Geometry.point_count dst and nv=Geometry.vertex_count dst and nr=Geometry.primitive_count dst in
    let pc=mix_of ~count:(Geometry.point_count src) ~shift:1 np
    and vc=mix_of ~count:(Geometry.vertex_count src) ~shift:2 nv
    and rc=mix_of ~count:(Geometry.primitive_count src) ~shift:3 nr in
    let edges=Topology_index.edge_count(Topology_index.create(Geometry.topology dst))
    and se=Topology_index.edge_count(Topology_index.create(Geometry.topology src)) in
    let offsets=Array.init(edges+1)Fun.id and ancestors=Array.init edges(fun i->i mod se) in
    let plain (l,_,_)=l in
    List.iter(fun (points,vertices,primitives)->
      let carry owner flag=match owner,flag with
        |Attribute.Point,true->pc|Vertex,true->vc|Primitive,true->rc
        |Point,false->(plain pc,plain pc,Array.make np 0.)|Vertex,false->(plain vc,plain vc,Array.make nv 0.)
        |_,_->(plain rc,plain rc,Array.make nr 0.) in
      let flag=function Attribute.Point->points|Vertex->vertices|Primitive->primitives|Detail->false in
      let group_flag=function Group.Point->points|Vertex->vertices|Primitive->primitives in
      let base=oracle src dst(plain pc)(plain vc)(plain rc)offsets ancestors true in
      let attributes=List.map(fun a->match Attribute.owner a with
        |Detail->a
        |owner->if flag owner then reference_attribute(carry owner true)a
          else Attribute.create_owned ~name:(Attribute.name a) ~owner(Attribute.Private.storage
            (Rdk_core.Topology_remap.attribute ~grain:97 (match owner with Point->plain pc|Vertex->plain vc|_->plain rc) a))
            |>get_string_ok)(Geometry.attributes src) in
      let groups=List.map(fun g->let owner=Group.owner g in
        let (l,r,t)=match owner with Group.Point->pc|Vertex->vc|Primitive->rc in
        Rdk_core.Topology_remap.group ~grain:97 (if group_flag owner then nearest(l,r,t) else l) g)(Geometry.groups src) in
      let positions=
        let v=Packed.Float3.Private.view(Geometry.positions src) in
        if points then Packed.Float3.Private.of_owned_exn ~x:(blend_plane pc v.x) ~y:(blend_plane pc v.y) ~z:(blend_plane pc v.z)
        else Geometry.positions base in
      let positions=if points then positions else
        let v=Packed.Float3.Private.view(Geometry.positions src) in
        let l=plain pc in
        Packed.Float3.Private.of_owned_exn ~x:(Array.map(fun i->v.x.(i))l) ~y:(Array.map(fun i->v.y.(i))l)
          ~z:(Array.map(fun i->v.z.(i))l) in
      let expected=Geometry.create ~positions ~topology:(Geometry.topology dst) ~attributes ~groups
        ~edge_groups:(Geometry.edge_groups base) ()|>get_string_ok in
      (* the shared helper is Curve_join' rule, byte for byte *)
      if points && vertices then
        List.iter(fun a->match Attribute.owner a with
          |Attribute.Point|Vertex->
              let (pl,pr,pw)=pc and (vl,vr,vw)=vc in
              let curve=Curve_join.interpolate_attribute pl pr pw vl vr vw a in
              let got=Option.get(Geometry.find_attribute ~owner:(Attribute.owner a)(Attribute.name a)expected) in
              let attach attribute=geometry_bytes(Geometry.create ~positions:(Geometry.positions base)
                ~topology:(Geometry.topology base) ~attributes:[attribute] ()|>get_string_ok) in
              check(attach curve=attach got)
                (Printf.sprintf "weighted remap differs from Curve_join.interpolate_attribute (%s copies %d)" (Attribute.name a) copies)
          |_->())(Geometry.attributes src);
      List.iter(fun reference->
        let node=compile ~reference ~body:(Printf.sprintf "(let* [out %s] out)" (body ~points ~vertices ~primitives))
            src dst(mix_args pc vc rc "mapped" offsets ancestors) in
        List.iter(fun domains->List.iter(fun grain->
          let before=geometry_bytes src in
          check(geometry_bytes(geometry(run ~domains ~grain node))=geometry_bytes expected)
            "weighted remap bytes differ from the reference";
          check(geometry_bytes src=before)"weighted remap mutated its source") [1;97]) [1;8]) [false;true])
      [true,true,true; true,false,false; false,true,false; false,false,true; true,true,false])
    [0;1;2];
  (* refusals *)
  let dst=target 1 src in
  let np=Geometry.point_count dst and nv=Geometry.vertex_count dst and nr=Geometry.primitive_count dst in
  let ok_args=mix_args (mix_of ~count:4 ~shift:1 np)(mix_of ~count:6 ~shift:2 nv)(mix_of ~count:2 ~shift:3 nr)"target"
    (Array.init(Topology_index.edge_count(Topology_index.create(Geometry.topology dst))+1)Fun.id)
    (Array.init(Topology_index.edge_count(Topology_index.create(Geometry.topology dst)))(fun i->i mod 3)) in
  let with_ key value=List.map(fun(k,old)->k,if k=key then value else old)ok_args in
  let all=body ~points:true ~vertices:true ~primitives:true in
  List.iter(fun(key,value,code)->match run(compile ~body:all src dst(with_ key value)) with
    |Error d->check(d.code=code)("weighted remap " ^ key ^ " code " ^ d.code ^ " is not " ^ code)
    |Ok _->fail ("weighted remap accepted a bad " ^ key))
    ["pt",E.Float_array(Array.make(np-1)0.5),"E_REMAP_COUNT";
     "pa",E.Int_array(Array.make np 4),"E_REMAP_INDEX";
     "pb",E.Int_array(Array.make np (-1)),"E_REMAP_INDEX";
     "va",E.Int_array(Array.make nv 6),"E_REMAP_INDEX";
     "rb",E.Int_array(Array.make nr (-1)),"E_REMAP_INDEX"];
  (* a non-finite weight never reaches the kernel: the array input is refused first *)
  List.iter(fun weight->match (try Ok(compile ~body:all src dst(with_ "pt" (E.Float_array(Array.make np weight)))) with Failure m->Error m) with
    |Error m->check(String.starts_with ~prefix:"E_NONFINITE" m)"non-finite weight refused with another code"
    |Ok node->(match run node with Error _->() |Ok _->fail "non-finite weight accepted"))
    [Float.nan;Float.infinity];
  List.iter(fun text->match Flow.Syntax.parse text with
    |Error d->failwith(Diagnostic.to_string d)
    |Ok forms->
      let factories=factory "src" src::factory "dst" dst::Sop.Nodes.factories in
      check(Result.is_error(L.workspace ~factories forms)) "malformed weighted remap lowered")
    ["(workspace w (graph g :context sop (sop/remap (sop/src) (sop/dst) :points (array/int 0) :point_mix {:a (array/int 0) :b (array/int 0) :t (array/float 0 0.0)} :vertices (array/int 0) :primitives (array/int 0))))";
     "(workspace w (graph g :context sop (sop/remap (sop/src) (sop/dst) :vertices (array/int 0) :primitives (array/int 0))))";
     "(workspace w (graph g :context sop (sop/remap (sop/src) (sop/dst) :point_mix {:a (array/int 0) :b (array/int 0)} :vertices (array/int 0) :primitives (array/int 0))))"];
  print_endline "Weighted remap: every owner and storage, N renormalisation, Curve_join bytes, mixed owners, reference/CPU, 1 and 8 domains, refusals"

(* [:lead_groups true] lists the target's own groups and edge groups ahead of the carried ones. *)
let () =
  let src = source 3 in
  let dst = target 1 src in
  let dst = Geometry.with_group (Group.init ~owner:Group.Point ~name:"lead" 3 (fun i -> i = 1)) dst |> get_string_ok in
  let topology = Geometry.topology dst in
  let dst = Geometry.with_edge_group (Edge_group.init ~topology ~index:(Topology_index.create topology)
    ~name:"lead_edges" (fun i -> i = 0)) dst |> get_string_ok in
  let identity = [|0;1;2|] in
  let inputs = args identity identity [|0|] [|0;1;2;3|] [|0;1;2|] "target" in
  let lead = "(sop/remap source target :points points :vertices vertices :primitives primitives :positions positions \
    :edges edge_map :lead_groups true)" in
  List.iter (fun reference ->
    let carried = geometry (run (compile ~reference src dst inputs)) in
    let led = geometry (run (compile ~reference ~body:lead src dst inputs)) in
    let names = List.map Group.name in
    check (names (Geometry.groups led) = "lead" :: names (Geometry.groups carried)) "lead_groups did not list the target groups first";
    check (not (List.mem "lead" (names (Geometry.groups carried)))) "remap kept target groups without lead_groups";
    check (List.map Edge_group.name (Geometry.edge_groups led) = "lead_edges" :: List.map Edge_group.name (Geometry.edge_groups carried))
      "lead_groups did not list the target edge groups first") [false; true]
