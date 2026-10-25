open Rdk
open Rdk_test_support
module N = Sop.Node
module P = Param

let flow_ok = function Ok x -> x | Error d -> fail (Flow.Diagnostic.to_string d)
let source_file = "../flow_sop/nodes/noise_displace.rays"
let source = In_channel.with_open_bin source_file In_channel.input_all
let declarations = Flow.Syntax.parse source |> flow_ok

let node ?(reference=false) input keywords =
  let factory=Sop.Edit_graph.factory ~key:"port_input" ~label:"Port input" ~category:["Test"] ~arity:0 (fun _ -> input) in
  let factories=factory::List.filter (fun f -> Sop.Edit_graph.factory_key f<>"noise_displace") Sop_catalog.Editor.factories in
  let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |> flow_ok in
  let forms=Flow.Syntax.parse ("(workspace port (graph g :context sop (sop/noise_displace (sop/port_input) "^keywords^")))") |> flow_ok in
  let checked=match Flow_sop.Catalog.check_workspace ~sources:declarations ~ops:Flow_sop.Operators.all catalog forms with
    | Some w, ds when not (List.exists (fun (d:Flow.Diagnostic.t)->d.severity=Error) ds)->w
    | _,ds->fail (String.concat "\n" (List.map Flow.Diagnostic.to_string ds)) in
  let lowered=Flow_sop.Lower.of_checked ~reference ~factories checked |> flow_ok in
  let graph=List.hd lowered.graphs in
  Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok

let oracle input label changes =
  let native=Sop.Edit_graph.instantiate Sop.Nodes.Noise_displace.factory [input] |> get_string_ok in
  check (N.label native=label) "Lisp default node label differs from the native factory key";
  fst (N.apply_parameters native changes |> get_string_ok)
let session ()=Sop.Session.create ~max_entries:64 ~max_payload_bytes:256_000_000 |> get_string_ok
let context ?(seed=42L) domains grain=Sop.Context.create ~seed ~domains ~grain () |> get_string_ok
let cooked session context node = match Sop.Session.cook session ~context node with
  | Ok output->Sop.Payload.geometry output.payload |> (function Ok g->g | Error d->fail (Sop.Diagnostic.error_to_string d))
  | Error d->fail (Sop.Diagnostic.error_to_string d)

let points count = Kernel.generate_point_ranges count (fun ~first ~last ~x ~y ~z ->
  for i=first to last-1 do x.(i)<-float (i mod 65)*.0.13-.4.;y.(i)<-sin (float i*.0.27);z.(i)<-float (i/65)*.0.19-.3. done)
let payload geometry =
  let vector count f=Packed.Float3.Private.of_owned_exn ~x:(Array.init count (fun i->f i))
    ~y:(Array.make count 0.37) ~z:(Array.make count (-1.2)) in
  let add name owner storage g=Geometry.with_attribute (Attribute.create_owned ~name ~owner storage |> get_string_ok) g |> get_string_ok in
  geometry
  |> add "N" Attribute.Point (Attribute.Float3 (vector (Geometry.point_count geometry) (fun i->cos (float i*.0.23))))
  |> add "N" Attribute.Vertex (Attribute.Float3 (vector (Geometry.vertex_count geometry) (fun _->1.)))
  |> add "id" Attribute.Point (Attribute.Int (Array.init (Geometry.point_count geometry) Fun.id))
  |> add "weight" Attribute.Primitive (Attribute.Float (Array.init (Geometry.primitive_count geometry) float))
  |> add "tag" Attribute.Detail (Attribute.Text [|"preserved"|])
  |> Geometry.with_group (Group.init ~owner:Group.Point ~name:"alternating" (Geometry.point_count geometry) (fun i->i mod 2=0))
  |> get_string_ok
let mixed () =
  let topology=Topology.create_owned ~point_count:6 ~vertex_points:[|0;1;2;3;1;4;5|]
    ~primitive_offsets:[|0;4;7|] ~primitive_kinds:[|Topology.Polygon;Open_polyline|] |> get_string_ok in
  Geometry.create ~positions:(Geometry.positions (points 6)) ~topology () |> get_string_ok |> payload
  |> Geometry.with_group (Group.init ~owner:Group.Vertex ~name:"corners" 7 (fun i->i mod 3=0)) |> get_string_ok
  |> Geometry.with_group (Group.init ~owner:Group.Primitive ~name:"polygon" 2 (fun i->i=0)) |> get_string_ok
  |> Geometry.with_edge_group (Edge_group.init ~topology ~index:(Topology_index.create topology)
    ~name:"selected_edges" (fun i->i mod 2=0)) |> get_string_ok

let cases = [
  "", [];
  ":mode \"normal_3d\"", ["mode",P.Choice_value "normal_3d"];
  ":seed 17", ["seed",P.Int_value 17];
  ":amplitude -0.7", ["amplitude",P.Float_value (-0.7)];
  ":frequency 0.16", ["frequency",P.Float_value 0.16];
  ":context_seed true", ["context_seed",P.Bool_value true];
  ":mode \"normal_3d\" :seed 311 :amplitude 0.8 :frequency 0.16", ["mode",P.Choice_value "normal_3d";"seed",P.Int_value 311;"amplitude",P.Float_value 0.8;"frequency",P.Float_value 0.16];
  ":mode \"normal_3d\" :context_seed true :amplitude 0.5 :frequency 0.25", ["mode",P.Choice_value "normal_3d";"context_seed",P.Bool_value true;"amplitude",P.Float_value 0.5;"frequency",P.Float_value 0.25];
  ":amplitude 0 :frequency 0", ["amplitude",P.Float_value 0.;"frequency",P.Float_value 0.]
]

let noise_displace () =
  let grid=Plane_generators.grid ~connectivity:Grid_quads ~columns:64 ~rows:64 ~size:8. ()
    |> get_ok |> payload in
  check (Geometry.point_count grid=4225) "pilot grid must have 4,225 points";
  List.iter (fun geometry ->
    let input=Lisp_sop.snapshot geometry and before=geometry_bytes geometry in
    List.iter (fun (keywords,changes)->
      let lisp=node input keywords and reference=node ~reference:true input keywords in
      let native=oracle input (N.label lisp) changes in
      check (N.parameter_fields lisp=N.parameter_fields native) "noise_displace parameter metadata changed";
      check (N.parameter_key lisp=N.parameter_key native) (Printf.sprintf "noise_displace parameter cache key changed: %S <> %S" (N.parameter_key lisp) (N.parameter_key native));
      let s=session () in
      Fun.protect ~finally:(fun()->Sop.Session.close s) (fun()->
        let baseline=cooked s (context 1 97) lisp in
        List.iter (fun domains->List.iter (fun grain->
          let c=context domains grain in
          Sop.Session.clear s;
          let actual=cooked s c lisp and expected=cooked s c native and walked=cooked s c reference in
          if not (close_geometry ~ulps:4 actual expected) then begin
            let a=Packed.Float3.Private.view (Geometry.positions actual) and b=Packed.Float3.Private.view (Geometry.positions expected) in
            let maximum=ref (0.,0,0.,0.) in
            List.iter (fun (a,b)->Array.iteri (fun i x->let y=b.(i) in
              let _,exponent=Float.frexp (Float.max (abs_float x) (abs_float y)) in
              let ulps=abs_float (x-.y)/.Float.ldexp 1. (max (-1074) (exponent-53)) in
              let best,_,_,_= !maximum in if ulps>best then maximum:=ulps,i,x,y) a) [a.x,b.x;a.y,b.y;a.z,b.z];
            let ulps,index,x,y= !maximum in fail (Printf.sprintf "noise_displace oracle mismatch: %s max_ulp=%g point=%d actual=%.17g expected=%.17g" keywords ulps index x y)
          end;
          check (geometry_bytes baseline=geometry_bytes actual) "noise_displace domains/grains changed bits";
          check (geometry_bytes actual=geometry_bytes walked) "noise_displace reference/kernel changed bits";
          check (Geometry.find_attribute ~owner:Attribute.Point "N" actual=None
            && Geometry.find_attribute ~owner:Attribute.Vertex "N" actual=None) "noise_displace retained normals") [1;97;4096]) [1;4];
        List.iter (fun seed->
          let c=context ~seed 4 97 in
          check (close_geometry ~ulps:4 (cooked s c lisp) (cooked s c native)) "noise_displace changed context seed differs from oracle") [0L;(-17L);0x7fffffffffffffffL]);
      check (before=geometry_bytes geometry) "noise_displace mutated its input") cases)
    [payload (points 0);payload (points 1);payload (points 4225);grid;mixed ()];
  (* More than two packed chunks exercises worker domains, unlike tiny fixtures. *)
  let geometry=payload (points 32769) in
  let input=Lisp_sop.snapshot geometry and before=geometry_bytes geometry in
  List.iter (fun mode ->
    let keywords=Printf.sprintf ":mode %S :seed 311 :amplitude 0.8 :frequency 0.16" mode in
    let lisp=node input keywords and reference=node ~reference:true input keywords in
    let native=oracle input (N.label lisp) ["mode",P.Choice_value mode;"seed",P.Int_value 311;
      "amplitude",P.Float_value 0.8;"frequency",P.Float_value 0.16] in
    let s=session () in
    Fun.protect ~finally:(fun()->Sop.Session.close s) (fun()->
      let baseline=cooked s (context 1 4096) lisp in
      check (close_geometry ~ulps:4 baseline (cooked s (context 1 4096) native))
        "multi-block pilot differs from oracle";
      check (geometry_bytes baseline=geometry_bytes (cooked s (context 1 4096) reference))
        "multi-block pilot differs from reference bits";
      List.iter (fun domains -> List.iter (fun grain ->
        Sop.Session.clear s;
        check (geometry_bytes baseline=geometry_bytes (cooked s (context domains grain) lisp))
          "multi-block pilot differs across worker domains") [97;4096]) [1;4;8]))
    ["height_2d";"normal_3d"];
  check (before=geometry_bytes geometry) "multi-block pilot mutated its input";
  let input=Lisp_sop.snapshot (payload (points 129)) in
  let lisp=node input "" in
  let native=oracle input (N.label lisp) [] in
  let invalid field value=match N.apply_parameters lisp [field,value],N.apply_parameters native [field,value] with
    |Error actual,Error expected->check (actual=expected) ("noise_displace changed validation error for "^field)
    |Ok (actual,_),Ok (expected,_)->check (N.parameter_key actual=N.parameter_key expected
        && N.parameter_fields actual=N.parameter_fields expected)
        ("noise_displace changed parameter coercion for "^field)
    |actual,expected->let state=function Ok _->"accepted"|Error e->"rejected: "^e in
        fail ("noise_displace validation differs for "^field^": Lisp "^state actual^"; native "^state expected) in
  List.iter (fun field->List.iter (fun n->
    invalid field (P.Float_value n))
    [nan;infinity;neg_infinity]) ["amplitude";"frequency"];
  List.iter (fun (field,value)->invalid field value)
    ["frequency",P.Float_value (-1.);"mode",P.Choice_value "unknown";
     "context_seed",P.Int_value 1;"seed",P.Text_value "bad"];
  let native_catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version
    (Sop.Nodes.Noise_displace.factory::Sop_catalog.Editor.factories) |> flow_ok in
  let lisp_catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version
    Sop_catalog.Editor.factories |> flow_ok in
  List.iter (fun input ->
    let forms=Flow.Syntax.parse ("(workspace wrong_input (graph g :context sop (sop/noise_displace "^input^")))") |> flow_ok in
    let codes (workspace,diagnostics)=
      check (workspace=None) "noise_displace accepted a wrongly typed geometry input";
      List.filter_map (fun (d:Flow.Diagnostic.t)->if d.severity=Error then Some d.code else None) diagnostics in
    let expected=codes (Flow.Workspace.check ~ops:Flow_sop.Operators.all native_catalog forms)
    and actual=codes (Flow_sop.Catalog.check_workspace ~sources:declarations ~ops:Flow_sop.Operators.all lisp_catalog forms) in
    check (actual=expected && List.mem "E_TYPE" actual) "noise_displace changed wrong input diagnostic")
    ["17";"(image/noise :width 1 :height 1)"];
  let s=session () in
  Fun.protect ~finally:(fun()->Sop.Session.close s) (fun()->
    let c=context 1 97 in
    let first=cooked s c lisp in
    check (cooked s c lisp==first) "unchanged noise_displace missed cache";
    let misses=(Sop.Session.stats s).misses in
    check (cooked s (context ~seed:999L 1 97) lisp==first)
      "noise_displace context_seed=false missed when context seed changed";
    check ((Sop.Session.stats s).misses=misses)
      "noise_displace context_seed=false depends on context seed";
    let seeded=fst (N.apply_parameters lisp ["context_seed",P.Bool_value true] |> get_string_ok) in
    let seeded_first=cooked s c seeded in
    let misses=(Sop.Session.stats s).misses in
    let seeded_next=cooked s (context ~seed:999L 1 97) seeded in
    check ((Sop.Session.stats s).misses=misses+1)
      "noise_displace context_seed=true did not invalidate for context seed";
    check (geometry_bytes seeded_first<>geometry_bytes seeded_next)
      "noise_displace context_seed=true did not use changed context seed";
    List.iter (fun label ->
      let renamed=N.relabel label seeded in
      let native=N.relabel label (oracle input (N.label lisp) ["context_seed",P.Bool_value true]) in
      check (N.id renamed=N.id seeded) "node relabel changed logical identity";
      let restored=N.relabel (N.label seeded) renamed in
      List.iter (fun seed ->
        let context=context ~seed 1 97 in
        let original=cooked s context seeded in
        let misses=(Sop.Session.stats s).misses in
        check (cooked s context renamed==original) "display relabel changed the cached cook identity";
        check (cooked s context restored==original) "restoring the label missed the cached cook identity";
        check ((Sop.Session.stats s).misses=misses) "display relabel caused a cache miss";
        check (close_geometry ~ulps:4 original (cooked s context native))
          "renamed Lisp node differs from the native context seed protocol") [999L;42L];
      let rebuild node=fst (N.apply_parameters node ["amplitude",P.Float_value 0.8] |> get_string_ok) in
      let rebuilt=rebuild renamed and native_rebuilt=rebuild native in
      let rebuilt_session=session () in
      Fun.protect ~finally:(fun()->Sop.Session.close rebuilt_session) (fun()->
        List.iter (fun seed ->
          let context=context ~seed 1 97 in
          check (close_geometry ~ulps:4 (cooked rebuilt_session context rebuilt)
            (cooked rebuilt_session context native_rebuilt))
            "parameter rebuild after relabel differs from the native seed namespace") [42L;999L]))
      ["seeded terrain";"déplacement 雪"];
    Sop.Session.clear s;
    ignore (cooked s c lisp);
    List.iter (fun (field,value)->
      let before=(Sop.Session.stats s).misses in
      let changed=fst (N.apply_parameters lisp [field,value] |> get_string_ok) in
      let result=cooked s c changed in
      check ((Sop.Session.stats s).misses=before+1) ("noise_displace changed "^field^" did not miss one cache entry");
      check (cooked s c changed==result) "edited noise_displace did not hit cache")
      ["mode",P.Choice_value "normal_3d";"context_seed",P.Bool_value true;"seed",P.Int_value 7;
       "amplitude",P.Float_value 0.8;"frequency",P.Float_value 0.25]);
  print_endline "Lisp noise_displace: modes, parameter contracts, metadata, all geometry payloads, 4-ULP oracle, reference/kernel, domains/grains and cache identity pass"

let benchmark () =
  let count=1_000_000 in
  let geometry=Kernel.generate_point_ranges count (fun ~first ~last ~x ~y ~z->
    for i=first to last-1 do x.(i)<-float (i mod 1000)*.0.01;y.(i)<-sin (float i*.0.01);z.(i)<-float (i/1000)*.0.01 done) |> payload in
  let input=Lisp_sop.snapshot geometry in
  List.iter (fun mode->
  let amplitude,frequency=if mode="normal_3d" then 0.8,0.16 else 0.1,1. in
  Printf.printf "mode=%s amplitude=%.17g frequency=%.17g grain=4096\n%!" mode amplitude frequency;
  let packed=ref None in
  let trace= !Flow_ir.Packed.Private.trace in
  Flow_ir.Packed.Private.trace:=(fun result->trace result;match result with
    | Ok p when Array.exists (function Flow_ir.Packed.Noise3 _->true|_->false)
        (Flow_ir.Packed.Private.view p).code -> packed:=Some p
    | _->());
  let lisp=Fun.protect ~finally:(fun()->Flow_ir.Packed.Private.trace:=trace)
    (fun()->node input (Printf.sprintf ":mode %S :amplitude %.17g :frequency %.17g" mode amplitude frequency)) in
  let native=oracle input (N.label lisp) ["mode",P.Choice_value mode;"amplitude",P.Float_value amplitude;"frequency",P.Float_value frequency] in
  List.iter (fun domains->
    let run label node =
      let trials=Array.init 3 (fun _->
        let s=session () in
        Fun.protect ~finally:(fun()->Sop.Session.close s) (fun()->
          Gc.full_major ();let gc=Gc.allocated_bytes () and start=Unix.gettimeofday () in
          ignore(cooked s (context domains 4096) node);
          Unix.gettimeofday ()-.start,Gc.allocated_bytes ()-.gc)) in
      Array.sort (fun (a,_) (b,_)->Float.compare a b) trials;
      let seconds,words=trials.(1) in
      Printf.printf "%s points=%d domains=%d median_seconds=%.6f allocated_bytes=%.0f\n%!" label count domains seconds words;
      seconds in
    let native=run "OCaml" native and lisp=run "Lisp" lisp in
    Printf.printf "Lisp/OCaml=%.3f\n%!" (lisp/.native);
    let p=Packed.Float3.Private.view (Geometry.positions geometry) in
    let noise=Rays_math.Noise.create 0 and output=Array.make count 0.
    and zeros=Array.make count 0. in
    Gc.full_major ();let start=Unix.gettimeofday () in
    Rays_math.Parallel.run ~domains (fun()->Rays_math.Parallel.for_ ~chunk_size:1
      ~start:0 ~finish:((count-1)/4096) (fun block->
        let first=block*4096 and last=min count ((block+1)*4096) in
        Rays_math.Noise.Private.sample3_into noise ~first ~last ~frequency
          ~x:p.x ~y:(if mode="normal_3d" then p.y else p.z)
          ~z:(if mode="normal_3d" then p.z else zeros) ~output ()));
    Printf.printf "Bulk noise alone domains=%d seconds=%.6f\n%!" domains (Unix.gettimeofday ()-.start);
    Option.iter (fun packed->
      let array positions=let p=Packed.Float3.Private.view positions in
        Flow.Eval.Vec3_array (Array.init (count*3) (fun i-> match i mod 3 with
          |0->p.x.(i/3)|1->p.y.(i/3)|_->p.z.(i/3))) in
      let positions=array (Geometry.positions geometry) and normals=array
        (Geometry.find_attribute ~owner:Attribute.Point "N" geometry |> Option.get
         |> Attribute.get (Attribute.key ~name:"N" ~owner:Attribute.Point Attribute.float3) |> Option.get) in
      let resolve=function
        |Flow.Eval.Struct ("sop/attr",_,args)->Ok (if List.assoc "attribute" args=Flow.Eval.Text "P" then positions else normals)
        |_->fail "Unexpected isolated packed source" in
      Gc.full_major ();let start=Unix.gettimeofday () in
      Rays_math.Parallel.run ~domains (fun()->ignore (Flow_ir.Packed.force ~strict:true ~resolve packed ~live:(Frame_input.at_time 0.) |> flow_ok));
      Printf.printf "Packed isolated domains=%d registers=%d seconds=%.6f\n%!" domains
        (Array.length (Flow_ir.Packed.Private.view packed).code) (Unix.gettimeofday ()-.start)) !packed) [1;4]) ["normal_3d";"height_2d"]

let setter_port key =
  let all_keys=["set_float";"set_int";"set_vector";"set_orient";"set_transform"] in
  let keys=[key] in
  let native_factory=function
    |"set_float"->Sop.Nodes.Set_float.factory |"set_int"->Sop.Nodes.Set_int.factory
    |"set_vector"->Sop.Nodes.Set_vector.factory |"set_orient"->Sop.Nodes.Set_orient.factory
    |"set_transform"->Sop.Nodes.Set_transform.factory
    |_->assert false in
  let sources=Flow_sop.Catalog.sources() |> flow_ok in
  let authored ~reference input key keywords =
    let input_factory=Sop.Edit_graph.factory ~key:"setter_port_input" ~label:"Setter input"
      ~category:["Test"] ~arity:0(fun _->input) in
    let factories=input_factory::List.filter(fun factory->
      not(List.mem(Sop.Edit_graph.factory_key factory)all_keys))Sop_catalog.Editor.factories in
    let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |> flow_ok in
    let forms=Flow.Syntax.parse(Printf.sprintf
      "(workspace setter_port (graph g :context sop (sop/%s (sop/setter_port_input) %s)))" key keywords) |> flow_ok in
    let checked=match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms with
      |Some workspace,diagnostics when not(List.exists(fun(d:Flow.Diagnostic.t)->d.severity=Error)diagnostics)->workspace
      |_,diagnostics->fail(String.concat "\n"(List.map Flow.Diagnostic.to_string diagnostics)) in
    let lowered=Flow_sop.Lower.of_checked ~reference ~factories checked |> flow_ok in
    let actual=List.find(fun factory->Sop.Edit_graph.factory_key factory=key)lowered.factories in
    check(Flow_sop.Catalog.descriptor actual=Flow_sop.Catalog.descriptor(native_factory key))
      (key^": native factory descriptor differs");
    let graph=List.hd lowered.graphs in
    Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok in
  let native input key changes =
    let initial=Sop.Edit_graph.instantiate(native_factory key)[input] |> get_string_ok in
    fst(N.apply_parameters initial changes |> get_string_ok) in
  let cases=List.map(fun key->key,"",[])keys
    @List.concat_map(fun owner->
      let prefix=Printf.sprintf ":owner %S " owner and change="owner",P.Choice_value owner in
      ["set_float",prefix^":name \"new float\" :value -2.5",
         [change;"name",P.Text_value "new float";"value",P.Float_value(-2.5)];
       "set_int",prefix^":name \"id\" :value 4611686018427387903",
         [change;"name",P.Text_value "id";"value",P.Int_value max_int];
       "set_vector",prefix^":value [2 -3 0.5]",
         [change;"x",P.Float_value 2.;"y",P.Float_value(-3.);"z",P.Float_value 0.5]])
       ["Point";"Vertex";"Primitive";"Detail"]
    @["set_orient",":w 0",["w",P.Float_value 0.];
      "set_orient",":x 1 :y 2 :z 3 :w -4",
        ["x",P.Float_value 1.;"y",P.Float_value 2.;"z",P.Float_value 3.;"w",P.Float_value(-4.)];
      "set_orient",":x 1e308",["x",P.Float_value 1e308];
      "set_transform",":m01 -2 :m03 4",["m01",P.Float_value(-2.);"m03",P.Float_value 4.]]
    |>List.filter(fun(candidate,_,_)->candidate=key) in
  let grid=Plane_generators.grid ~connectivity:Grid_quads ~columns:64 ~rows:64 ~size:8.() |> get_ok |> payload in
  List.iter(fun geometry->
    let input=Lisp_sop.snapshot geometry and before=geometry_bytes geometry in
    List.iter(fun(key,keywords,changes)->
      let actual=authored ~reference:false input key keywords
      and walked=authored ~reference:true input key keywords and expected=native input key changes in
      check(N.operation actual=N.operation expected && N.version actual=N.version expected
        && N.label actual=N.label expected && N.parameter_fields actual=N.parameter_fields expected
        && N.parameter_key actual=N.parameter_key expected)(key^": node identity/schema differs");
      let s=session() in
      Fun.protect ~finally:(fun()->Sop.Session.close s)(fun()->
        let baseline=cooked s(context 1 97)actual in
        check(close_geometry ~ulps:4 baseline(cooked s(context 1 97)expected))(key^": native oracle differs");
        check(geometry_bytes baseline=geometry_bytes(cooked s(context 1 97)walked))(key^": reference bytes differ");
        List.iter(fun (domains,grain)->Sop.Session.clear s;
          check(geometry_bytes baseline=geometry_bytes(cooked s(context domains grain)actual))
            (key^": worker/grain bytes differ"))[1,97;8,97;4,4096]);
      check(before=geometry_bytes geometry)(key^": input mutated"))cases)
    [payload(points 0);payload(points 1);grid;mixed();payload(points 32769)];
  let input=Lisp_sop.snapshot(mixed()) in
  List.iter(fun key->
    let initial=authored ~reference:false input key "" and oracle=native input key [] in
    List.iter(fun(field:P.field_view)->
      let value=match field.current with
        |P.Choice_value _->P.Choice_value "Vertex" |Text_value _->Text_value "edited"
        |Int_value value->Int_value(value+7)
        |Float_value value->Float_value(value+.(if List.mem field.name ["m30";"m31";"m32";"m33"]then 1e-13 else 0.25))
        |Bool_value _->assert false in
      let actual=fst(N.apply_parameters initial[field.name,value] |> get_string_ok)
      and expected=fst(N.apply_parameters oracle[field.name,value] |> get_string_ok) in
      check(N.parameter_key actual=N.parameter_key expected)(key^": edited cache key differs");
      let s=session() in Fun.protect ~finally:(fun()->Sop.Session.close s)(fun()->
        ignore(cooked s(context 1 97)initial);let before=Sop.Session.stats s in
        let result=cooked s(context 1 97)actual in let after=Sop.Session.stats s in
        check(after.misses=before.misses+1)(key^": edited public miss count differs");
        ignore(cooked s(context 1 97)actual);let warm=Sop.Session.stats s in
        check(warm.misses=after.misses && warm.hits>after.hits)(key^": warm cache missed");
        check(close_geometry ~ulps:4 result(cooked s(context 1 97)expected))(key^": edited oracle differs")))
      (N.parameter_fields initial))keys;
  List.iter(fun(key,changes)->
    let actual=authored ~reference:false input key "" and expected=native input key [] in
    let apply node=match N.apply_parameters node changes with Error message->Error message|Ok _->Ok() in
    let observed=apply actual in
    check(observed=apply expected && Result.is_error observed)(key^": rejected edit differs"))
    (List.filter(fun(candidate,_)->candidate=key)
    (List.concat_map(fun key->[key,["name",P.Text_value " \t\n"];key,["name",P.Text_value "P"]])
       ["set_float";"set_int";"set_vector"]
     @["set_orient",["x",P.Float_value infinity]]
     @List.concat_map(fun name->List.map(fun value->"set_transform",[name,P.Float_value value])
         [Float.next_after 1e-12 infinity;Float.next_after (-1e-12) neg_infinity]) ["m30";"m31";"m32"]
     @["set_transform",["m33",P.Float_value(Float.next_after (1.+.1e-12) infinity)];
       "set_transform",["m33",P.Float_value(Float.next_after (1.-.1e-12) neg_infinity)]]));
  let initial=authored ~reference:false input key "" and expected=native input key [] in
  List.iter(fun(field:P.field_view)->match field.current with
    |P.Float_value _->List.iter(fun value->
        let apply node=match N.apply_parameters node[field.name,P.Float_value value] with
          |Error message->Error message |Ok _->Ok() in
        let observed=apply initial in
        check(observed=apply expected && Result.is_error observed)(key^": nonfinite edit differs"))
        [nan;infinity;neg_infinity]
    |_->())(N.parameter_fields initial);
  let factories=List.map native_factory all_keys @ List.filter(fun factory->
    not(List.mem(Sop.Edit_graph.factory_key factory)all_keys))Sop_catalog.Editor.factories in
  let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |> flow_ok in
  List.iter(fun argument->
    let forms=Flow.Syntax.parse(Printf.sprintf
      "(workspace invalid_setter (graph g :context sop (sop/%s %s)))" key argument) |> flow_ok in
    let errors(workspace,diagnostics)=
      check(workspace=None)(key^": wrongly typed geometry accepted");
      List.filter_map(fun(d:Flow.Diagnostic.t)->if d.severity=Error then Some d.code else None)diagnostics in
    let native_errors=errors(Flow.Workspace.check ~ops:Flow_sop.Operators.all catalog forms)
    and actual_errors=errors(Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms) in
    check(actual_errors=native_errors && List.mem "E_TYPE" actual_errors)(key^": geometry refusal code differs"))
    ["17";"(image/noise :width 1 :height 1)"];
  print_endline(key^": whole native/reference/schema/cache/domain/grain/refusal recipe passed")

let set_float ()=setter_port "set_float"

let set_int ()=setter_port "set_int"

let set_vector ()=setter_port "set_vector"

let set_orient ()=setter_port "set_orient"

let set_transform ()=setter_port "set_transform"

let metadata_port ?reference_input ?(extra_geometries=[]) ~key ~native_factory ~cases ~initial_changes ~edit () =
  let sources=Flow_sop.Catalog.sources()|>flow_ok in
  (* one catalog for every case: the input factory reads [current] when the node is compiled *)
  let current=ref (Lisp_sop.snapshot (payload (points 0))) in
  let factory=Sop.Edit_graph.factory ~key:"edge_port_input" ~label:"Edge input" ~category:["Test"] ~arity:0(fun _-> !current) in
  let reference_factory=Option.map(fun node->Sop.Edit_graph.factory ~key:"metadata_reference"
    ~label:"Reference" ~category:["Test"] ~arity:0(fun _->node))reference_input in
  let factories=factory::Option.to_list reference_factory@
    List.filter(fun factory->Sop.Edit_graph.factory_key factory<>key)Sop_catalog.Editor.factories in
  let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories|>flow_ok in
  let authored ~reference input keywords =
    current:=input;
    let forms=Flow.Syntax.parse(Printf.sprintf
      "(workspace edge_port (graph g :context sop (sop/%s (sop/edge_port_input) %s)))"
      key ((if Option.is_none reference_input then "" else ":reference (sop/metadata_reference) ")^keywords))|>flow_ok in
    let checked=match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms with
      |Some workspace,ds when not(List.exists(fun(d:Flow.Diagnostic.t)->d.severity=Error)ds)->workspace
      |_,ds->fail(String.concat "\n"(List.map Flow.Diagnostic.to_string ds)) in
    let lowered=Flow_sop.Lower.of_checked ~reference ~factories checked|>flow_ok in
    let actual=List.find(fun factory->Sop.Edit_graph.factory_key factory=key)lowered.factories in
    check(Flow_sop.Catalog.descriptor actual=Flow_sop.Catalog.descriptor native_factory)(key^": descriptor");
    let graph=List.hd lowered.graphs in
    Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root)|>get_string_ok in
  let native input changes =
    let initial=(if key="delete_attributes" then Sop.Edit_graph.instantiate_optional native_factory
      [Some input;reference_input] else Sop.Edit_graph.instantiate native_factory[input])|>get_string_ok in
    fst(N.apply_parameters initial changes|>get_string_ok) in
  let cooked_bytes session context node=match Sop.Session.cook session ~context node with
    |Error d->Error(d.Sop.Diagnostic.code,d.message,d.cause)
    |Ok output->(match Sop.Payload.geometry output.payload with
        |Ok geometry->Ok(geometry_bytes geometry)|Error d->Error(d.code,d.message,d.cause)) in
  let grid=Plane_generators.grid ~connectivity:Grid_quads ~columns:64 ~rows:64 ~size:8.()|>get_ok|>payload in
  List.iter(fun geometry->let before=geometry_bytes geometry and input=Lisp_sop.snapshot geometry in
    List.iter(fun(keywords,changes)->let actual=authored ~reference:false input keywords
      and walked=authored ~reference:true input keywords and expected=native input changes in
      check(N.operation actual=N.operation expected && N.version actual=N.version expected
        && N.label actual=N.label expected && N.parameter_fields actual=N.parameter_fields expected
        && N.parameter_key actual=N.parameter_key expected)(key^": identity/schema/key");
      let s=session() in Fun.protect ~finally:(fun()->Sop.Session.close s)(fun()->
        let baseline=cooked_bytes s(context 1 97)expected in
        List.iter(fun (domains,grain)->Sop.Session.clear s;
          let observed=cooked_bytes s(context domains grain)actual in
          if baseline<>observed then begin
            let describe=function
              |Ok bytes->Digest.to_hex(Digest.string bytes)
              |Error(code,message,cause)->code^": "^message^Option.value ~default:"" cause in
            let write suffix value=Out_channel.with_open_bin ("/tmp/rays-port-"^key^"-"^suffix^".marshal")
              (fun channel->output_string channel(Marshal.to_string value [])) in
            write "native" baseline;write "actual" observed;
            fail(Printf.sprintf "%s: native/domain/grain bytes/errors points=%d vertices=%d primitives=%d domains=%d grain=%d keywords=%S native=%s actual=%s"
              key (Geometry.point_count geometry) (Geometry.vertex_count geometry)
              (Geometry.primitive_count geometry) domains grain keywords (describe baseline) (describe observed))
          end;
          check(baseline=cooked_bytes s(context domains grain)walked)(key^": reference bytes/errors"))
          [1,97;8,97;4,4096]))(let stride=if extra_geometries=[] then 1 else if List.memq geometry extra_geometries then (if List.length cases>200 then 2 else 1) else 8 in
       List.filteri(fun i _->i mod stride=0)cases);  (* rule tables: the geometries that hold the named items get every case (every 2nd of 200+), the plain ones every 8th *)
    check(before=geometry_bytes geometry)(key^": input mutation"))
    (([payload(points 0);payload(points 1);mixed()]@(if extra_geometries=[] then [grid;payload(points 32769)] else []))@extra_geometries);
  let input=Lisp_sop.snapshot(mixed()) in
  let initial=fst(N.apply_parameters(authored ~reference:false input "")initial_changes|>get_string_ok)
  and oracle=native input initial_changes in
  let edited=fst(N.apply_parameters initial edit|>get_string_ok) in
  let s=session() in Fun.protect ~finally:(fun()->Sop.Session.close s)(fun()->
    ignore(cooked s(context 1 97)initial);let before=Sop.Session.stats s in
    let result=cooked s(context 1 97)edited in let after=Sop.Session.stats s in
    check(after.misses=before.misses+1)(key^": edit public miss");
    check(geometry_bytes result=geometry_bytes(cooked s(context 1 97)(native input(initial_changes@edit))))(key^": edited native");
    let before=Sop.Session.stats s in ignore(cooked s(context 1 97)edited);let after=Sop.Session.stats s in
    check(after.misses=before.misses && after.hits>before.hits)(key^": warm hit");
    List.iter(fun label->let relabeled=N.relabel label edited in
      check(N.id relabeled=N.id edited && N.parameter_key relabeled=N.parameter_key edited)(key^": relabel identity");
      check(geometry_bytes(cooked s(context 1 97)relabeled)=geometry_bytes result)(key^": relabel cook"))
      ["Edge selection";"é猫"]);
  List.iter(fun(field:P.field_view)->List.iter(fun value->let apply node=match N.apply_parameters node[field.name,value]with
    |Error message->Error message|Ok(node,_)->Ok(N.parameter_key node,N.parameter_fields node) in
    check(apply initial=apply oracle)(key^": wrong parameter diagnostic"))
    [P.Bool_value true;P.Int_value 17;P.Float_value nan;P.Float_value infinity])(N.parameter_fields initial);
  let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version
    (native_factory::List.filter(fun factory->Sop.Edit_graph.factory_key factory<>key)Sop_catalog.Editor.factories)|>flow_ok in
  let wrong_inputs=["17","";"(image/noise :width 1 :height 1)",""]@
    (if key="delete_attributes" then ["(sop/null)"," :reference 17";
      "(sop/null)"," :reference (image/noise :width 1 :height 1)"] else []) in
  List.iter(fun(input,keywords)->let forms=Flow.Syntax.parse(Printf.sprintf
      "(workspace invalid_edges (graph g :context sop (sop/%s %s%s)))"key input keywords)|>flow_ok in
    let errors(workspace,ds)=check(workspace=None)(key^": wrong input accepted");
      List.filter_map(fun(d:Flow.Diagnostic.t)->if d.severity=Error then Some d.code else None)ds in
    let expected=errors(Flow.Workspace.check ~ops:Flow_sop.Operators.all catalog forms)
    and actual=errors(Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms) in
    check(actual=expected && List.mem "E_TYPE" actual)(key^": wrong input code"))
    wrong_inputs;
  print_endline(key^": whole native/reference/schema/cache/domain/grain/refusal recipe passed")

let delete_edge_group () =
  let cases=("",[])::List.map(fun name->
    ":name "^Flow.Lisp.flat(Flow.Syntax.make(Str name)),["name",P.Text_value name])
    ["edges";"selected_edges";"absent";"";" selected_edges ";"*"] in
  metadata_port ~key:"delete_edge_group" ~native_factory:Sop.Nodes.Delete_edge_group.factory ~cases
    ~initial_changes:[] ~edit:["name",P.Text_value "selected_edges"] ()

let rename_edge_group () =
  let cases=("",[])::List.concat_map(fun from->List.map(fun into->
    Printf.sprintf ":from %s :into %s" (Flow.Lisp.flat(Flow.Syntax.make(Str from)))
      (Flow.Lisp.flat(Flow.Syntax.make(Str into))),["from",P.Text_value from;"into",P.Text_value into])
    ["renamed";"selected_edges";"";" ";"é猫\""])["edges";"selected_edges";"absent";" selected_edges ";"*"] in
  metadata_port ~key:"rename_edge_group" ~native_factory:Sop.Nodes.Rename_edge_group.factory ~cases
    ~initial_changes:["from",P.Text_value "selected_edges"] ~edit:["into",P.Text_value "é猫\""] ()

let delete_attributes () =
  let patterns=["point_pattern";"vertex_pattern";"primitive_pattern";"detail_pattern"] in
  let cases=("",[])::(":delete_non_selected true",["delete_non_selected",P.Bool_value true])::
    List.concat_map(fun field->List.concat_map(fun pattern->List.map(fun inverse->
      Printf.sprintf ":%s %s :delete_non_selected %b" field
        (Flow.Lisp.flat(Flow.Syntax.make(Str pattern)))inverse,
      [field,P.Text_value pattern;"delete_non_selected",P.Bool_value inverse])[false;true])
      ["*";"N id";"* ^id";"P";"absent";" \t ";"é猫";"id\\ ";"\\ id"] )patterns in
  List.iter(fun reference_input->metadata_port ?reference_input ~key:"delete_attributes"
    ~native_factory:Sop.Nodes.Delete_attributes.factory ~initial_changes:[]
    ~cases:(if reference_input=None then cases else List.filteri(fun i _->i mod 4=0)cases)  (* the reference input changes only the second port, not the glob rows *)
    ~edit:["point_pattern",P.Text_value "N"] ())
    [None;Some(Lisp_sop.snapshot(points 0));Some(Lisp_sop.snapshot(mixed()))];
  let input=Lisp_sop.snapshot(mixed()) in
  let sources=Flow_sop.Catalog.sources()|>flow_ok in
  let factory=Sop.Edit_graph.factory ~key:"delete_input" ~label:"Input" ~category:["Test"] ~arity:0(fun _->input) in
  let factories=factory::List.filter(fun factory->Sop.Edit_graph.factory_key factory<>"delete_attributes")Sop_catalog.Editor.factories in
  let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories|>flow_ok in
  let forms=Flow.Syntax.parse "(workspace delete (graph g :context sop (sop/delete_attributes (sop/delete_input))))"|>flow_ok in
  let checked=match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms with
    |Some workspace,ds when not(List.exists(fun(d:Flow.Diagnostic.t)->d.severity=Error)ds)->workspace
    |_,ds->fail(String.concat "\n"(List.map Flow.Diagnostic.to_string ds)) in
  let lowered=Flow_sop.Lower.of_checked ~factories checked|>flow_ok in
  let initial=List.find(fun factory->Sop.Edit_graph.factory_key factory="delete_attributes")lowered.factories
    |>fun factory->Sop.Edit_graph.instantiate_optional factory[Some input;None]|>get_string_ok in
  let oracle=Sop.Edit_graph.instantiate_optional Sop.Nodes.Delete_attributes.factory[Some input;None]|>get_string_ok in
  List.iter(fun field->List.iter(fun pattern->
    let apply node=match N.apply_parameters node[field,P.Text_value pattern]with
      |Error message->Error message|Ok _->Ok() in
    check(apply initial=apply oracle && Result.is_error(apply initial))"Delete Attributes invalid glob rebuild diagnostic")
    ["[";"\\";"[z-a]"])patterns

let rename_attributes () =
  let table=Param.String_table.encode in
  let extra=List.fold_left(fun geometry(owner,count)->
    let array value=Array.make count value in
    let float_csr=Packed.Float_array.create_owned ~offsets:(Array.init(count+1)(fun i->i*2))
      ~values:(Array.make(count*2)(-0.))|>get_string_ok
    and int_csr=Packed.Int_array.create_owned ~offsets:(Array.init(count+1)(fun i->i*2))
      ~values:(Array.make(count*2)max_int)|>get_string_ok in
    List.fold_left(fun geometry(name,storage)->Geometry.with_attribute
      (Attribute.create_owned ~owner ~name storage|>get_string_ok)geometry|>get_string_ok)geometry
      ["old_float",Attribute.Float(array(-0.));"old_int",Int(array max_int);
       "old_vec2",Float2(Packed.Float2.of_owned ~x:(array(-0.))~y:(array 2.)|>get_string_ok);
       "old_vec3",Float3(Packed.Float3.of_owned ~x:(array 1.)~y:(array(-0.))~z:(array 3.)|>get_string_ok);
       "old_vec4",Float4(Packed.Float4.of_owned ~x:(array(-0.))~y:(array 2.)~z:(array 3.)~w:(array 4.)|>get_string_ok);
       "old_text",Text(array "猫\t😀");"old_float_array",Float_array float_csr;"old_int_array",Int_array int_csr])
    (mixed())[Attribute.Point,6;Vertex,7;Primitive,2;Detail,1] in
  let positions=Packed.Float3.Private.view(Geometry.positions extra) in
  let x=Array.copy positions.x in x.(0)<-nan;x.(1)<-infinity;x.(2)<-neg_infinity;
  let raw=Geometry.with_positions(Packed.Float3.of_owned ~x ~y:(Array.copy positions.y)
    ~z:(Array.copy positions.z)|>get_string_ok)extra|>get_string_ok in
  let raw=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"raw_float"
    (Float[|nan;infinity;neg_infinity;-0.;0.;1.|])|>get_string_ok)raw|>get_string_ok in
  let rows=[]::List.concat_map(fun owner->List.concat_map(fun policy->
    List.map(fun(pattern,replacement)->[[owner;pattern;replacement;policy]])
      ["old_*","new_*";"old_*","old_*";"missing","new";
       "old_float","old_int";"old_float","\\ ";"old_float","P";
       "old_?nt","new_?nt";"old_[fi]*","new_[fi]*";"N","normal";
       "id","N";"weight","renamed_weight";"tag","é猫"])
    ["skip";"error";"overwrite"])["point";"vertex";"primitive";"detail";" ANY ";"*"]@
    [[["any";"old_*";"mid_*";"skip"];["any";"mid_*";"new_*";"overwrite"]];
     [["point";"old_float";"old_int";"overwrite"];["point";"old_int";"old_vec2";"overwrite"]]] in
  let cases=List.map(fun rows->let encoded=table rows in
    ":rules "^Flow.Lisp.flat(Flow.Syntax.make(Str encoded)),["rules",P.Text_value encoded])rows in
  metadata_port ~key:"rename_attributes" ~native_factory:Sop.Nodes.Rename_attributes.factory ~cases
    ~extra_geometries:[extra;raw] ~initial_changes:[]
    ~edit:["rules",P.Text_value(table[[" POINTS ";"id";"identifier";" SKIP "]])] ()

let group_delete () =
  let encoded rows=Param.String_table.encode rows in
  let rows=[]::List.concat_map(fun owner->List.map(fun pattern->[[owner;pattern]])
    ["*";"absent";"alternating corners polygon selected_edges";"* ^alternating";" \t ";"[";"\\"])
    [" POINTS ";"vertices";"primitives";"edges";"any";"*"]@
    [[["points";"alternating"];["vertices";"corners"];["edges";"selected_edges"]];
     [["any";"*"];["any";"*"]];[["point";"["];["edge";"\\"]];
     [["edge";"\\"];["point";"["]]] in
  let cases=List.concat_map(fun rows->List.map(fun unused->
    let text=encoded rows in
    Printf.sprintf ":rules %s :delete_unused %b"(Flow.Lisp.flat(Flow.Syntax.make(Str text)))unused,
    ["rules",P.Text_value text;"delete_unused",P.Bool_value unused])[false;true])rows in
  let extra=mixed() in
  let extra=List.fold_left(fun geometry(owner,count)->
    Geometry.with_group(Group.ordered ~owner ~name:"ordered" ~length:count[|count-1;0|]|>get_string_ok)geometry|>get_string_ok)
    extra [Group.Point,6;Vertex,7;Primitive,2] in
  let extra=Geometry.with_group(Group.init ~owner:Point ~name:"unused" 6(fun _->false))extra|>get_string_ok in
  let extra=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"raw"
    (Float[|nan;infinity;neg_infinity;-0.;0.;1.|])|>get_string_ok)extra|>get_string_ok in
  metadata_port ~key:"group_delete" ~native_factory:Sop.Nodes.Group_delete.factory
    ~extra_geometries:[extra] ~cases ~initial_changes:[]
    ~edit:["rules",P.Text_value(encoded[["points";"alternating"]])] ()

let group_rename () =
  let geometry=List.fold_left(fun geometry(owner,count)->
    let array value=Array.make count value in
    let float_csr=Packed.Float_array.create_owned ~offsets:(Array.init(count+1)(fun i->i*2))
      ~values:(Array.make(count*2)(-0.))|>get_string_ok
    and int_csr=Packed.Int_array.create_owned ~offsets:(Array.init(count+1)(fun i->i*2))
      ~values:(Array.make(count*2)max_int)|>get_string_ok in
    List.fold_left(fun geometry(name,storage)->Geometry.with_attribute
      (Attribute.create_owned ~owner ~name storage|>get_string_ok)geometry|>get_string_ok)geometry
      ["old_float",Attribute.Float(array(-0.));"old_int",Int(array max_int);
       "old_vec2",Float2(Packed.Float2.of_owned ~x:(array(-0.))~y:(array 2.)|>get_string_ok);
       "old_vec3",Float3(Packed.Float3.of_owned ~x:(array 1.)~y:(array(-0.))~z:(array 3.)|>get_string_ok);
       "old_vec4",Float4(Packed.Float4.of_owned ~x:(array(-0.))~y:(array 2.)~z:(array 3.)~w:(array 4.)|>get_string_ok);
       "old_text",Text(array "猫\t😀");"old_float_array",Float_array float_csr;"old_int_array",Int_array int_csr])
    (mixed())[Attribute.Point,6;Vertex,7;Primitive,2;Detail,1] in
  let extra=List.fold_left(fun geometry(owner,count,source,target)->
    Geometry.with_group(Group.ordered ~owner ~name:"old" ~length:count source|>get_string_ok)geometry|>get_string_ok
    |>fun geometry->Geometry.with_group(Group.ordered ~owner ~name:"target" ~length:count target|>get_string_ok)geometry|>get_string_ok)
    geometry [Group.Point,6,[|4;1;3|],[|2;4|];Vertex,7,[|6;0|],[|0;4|];Primitive,2,[|1|],[|0|]] in
  let topology=Geometry.topology extra in let index=Topology_index.create topology in
  let extra=List.fold_left(fun geometry(name,selected)->Geometry.with_edge_group
    (Edge_group.init ~topology ~index ~name selected)geometry|>get_string_ok)
    extra ["old",(fun i->i<2);"target",(fun i->i=1||i=2)] in
  let extra=List.fold_left(fun geometry name->Geometry.with_group
    (Group.ordered ~owner:Point ~name ~length:6[|4;1|]|>get_string_ok)geometry|>get_string_ok)
    extra ["é猫";"a-b";"back\\slash";"a b";"[x]";"*"] in
  let positions=Packed.Float3.Private.view(Geometry.positions extra) in
  let x=Array.copy positions.x in x.(0)<-nan;x.(1)<-infinity;x.(2)<-neg_infinity;
  let raw=Geometry.with_positions(Packed.Float3.of_owned ~x ~y:(Array.copy positions.y)
    ~z:(Array.copy positions.z)|>get_string_ok)extra|>get_string_ok in
  let raw=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"raw_float"
    (Float[|nan;infinity;neg_infinity;-0.;0.;1.|])|>get_string_ok)raw|>get_string_ok in
  let rows=[]::List.concat_map(fun owner->List.concat_map(fun policy->
    List.map(fun(pattern,replacement)->[[owner;pattern;replacement;policy]])
      ["old","target";"old","old";"missing","new";"old","\\ ";
       "old*","new*";"[","new";"\\","new";"*","new";
       "alternating","renamed";"corners","renamed";"polygon","renamed";"selected_edges","renamed"])
    ["skip";"error";"overwrite";"union"])["point";"vertex";"primitive";"edge";" ANY ";"*"]@
    [[["any";"old";"target";"union"];["any";"target";"renamed";"overwrite"]];
     [["point";"old";"target";"error"];["edge";"[";"new";"skip"]];
     [["edge";"[";"new";"skip"];["point";"old";"target";"error"]]]@
    List.map(fun pattern->[["point";pattern;"renamed";"skip"]])
      ["é猫";"a-b";"back\\\\slash";"a\\ b";"\\[x\\]";"\\*"] in
  let cases=List.map(fun rows->let encoded=Param.String_table.encode rows in
    ":rules "^Flow.Lisp.flat(Flow.Syntax.make(Str encoded)),["rules",P.Text_value encoded])rows in
  metadata_port ~key:"group_rename" ~native_factory:Sop.Nodes.Group_rename.factory ~cases
    ~extra_geometries:[extra;raw] ~initial_changes:[]
    ~edit:["rules",P.Text_value(Param.String_table.encode[[" POINTS ";"alternating";"selected";" UNION "]])] ()

let swap_attributes () =
  let extra=List.fold_left(fun geometry(owner,count)->
    let array value=Array.make count value in
    let float_csr=Packed.Float_array.create_owned ~offsets:(Array.init(count+1)(fun i->i*2))
      ~values:(Array.make(count*2)(-0.))|>get_string_ok
    and int_csr=Packed.Int_array.create_owned ~offsets:(Array.init(count+1)(fun i->i*2))
      ~values:(Array.make(count*2)max_int)|>get_string_ok in
    List.fold_left(fun geometry(name,storage)->Geometry.with_attribute
      (Attribute.create_owned ~owner ~name storage|>get_string_ok)geometry|>get_string_ok)geometry
      ["old_float",Attribute.Float(array(-0.));"old_int",Int(array max_int);
       "old_vec2",Float2(Packed.Float2.of_owned ~x:(array(-0.))~y:(array 2.)|>get_string_ok);
       "old_vec3",Float3(Packed.Float3.of_owned ~x:(array 1.)~y:(array(-0.))~z:(array 3.)|>get_string_ok);
       "old_vec4",Float4(Packed.Float4.of_owned ~x:(array(-0.))~y:(array 2.)~z:(array 3.)~w:(array 4.)|>get_string_ok);
       "old_text",Text(array "猫\t😀");"old_float_array",Float_array float_csr;"old_int_array",Int_array int_csr])
    (mixed())[Attribute.Point,6;Vertex,7;Primitive,2;Detail,1] in
  let extra=Geometry.with_attribute(Attribute.create_owned ~owner:Vertex ~name:"P"
    (Float(Array.make 7(-0.)))|>get_string_ok)extra|>get_string_ok in
  let extra=List.fold_left(fun geometry name->Geometry.with_attribute
    (Attribute.create_owned ~owner:Point ~name(Text(Array.make 6 "猫\t😀"))|>get_string_ok)geometry|>get_string_ok)
    extra ["é猫";"a-b";"back\\slash";"a b";"[x]";"*"] in
  let positions=Packed.Float3.Private.view(Geometry.positions extra) in
  let x=Array.copy positions.x in x.(0)<-nan;x.(1)<-infinity;x.(2)<-neg_infinity;
  let raw=Geometry.with_positions(Packed.Float3.of_owned ~x ~y:(Array.copy positions.y)
    ~z:(Array.copy positions.z)|>get_string_ok)extra|>get_string_ok in
  let raw=Geometry.with_attribute(Attribute.create_owned ~owner:Point ~name:"raw_float"
    (Float[|nan;infinity;neg_infinity;-0.;0.;1.|])|>get_string_ok)raw|>get_string_ok in
  let methods=["swap";"move";"copy"] in
  let kinds=["float";"int";"vec2";"vec3";"vec4";"text";"float_array";"int_array"] in
  let rows=[]::List.concat_map(fun owner->List.concat_map(fun method_->
    List.map(fun(source,destination)->[[owner;source;destination;method_]])
      (["old_*","new_*";"old_*","old_*";"missing","old_float";
        "old_float","missing";"missing","missing2";"old_float","old_int";
        "old_float","old_float";"absent_*","old_*";"old_?nt","new_?nt";
        "old_[fi]*","new_[fi]*";"old_float","\\ "]@
       List.map(fun kind->"old_"^kind,"new_"^kind)kinds))methods)
    [" POINTS ";"vertices";"primitives";"detail"]@
    List.concat_map(fun method_->List.map(fun(source,destination)->[["point";source;destination;method_]])
      (["P","P";"P","new_P";"missing","P";"P","old_vec3";"old_vec3","P"]@
       List.concat_map(fun kind->["old_"^kind,"P";"P","old_"^kind])kinds))methods@
    [[ ["point";"old_float";"old_int";"swap"];["point";"old_int";"old_vec2";"swap"] ];
     [ ["point";"old_*";"mid_*";"copy"];["point";"mid_*";"new_*";"move"] ];
     [ ["point";"old_vec3";"P";"move"];["point";"P";"new_P";"copy"] ];
     [ ["vertex";"P";"new_P";"swap"] ]]@
    List.concat_map(fun method_->List.map(fun pattern->[["point";pattern;"renamed";method_]])
      ["é猫";"a-b";"back\\\\slash";"a\\ b";"\\[x\\]";"\\*"])methods in
  let cases=List.map(fun rows->let encoded=Param.String_table.encode rows in
    ":rules "^Flow.Lisp.flat(Flow.Syntax.make(Str encoded)),["rules",P.Text_value encoded])rows in
  metadata_port ~key:"swap_attributes" ~native_factory:Sop.Nodes.Swap_attributes.factory ~cases
    ~extra_geometries:[extra;raw] ~initial_changes:[]
    ~edit:["rules",P.Text_value(Param.String_table.encode[[" POINTS ";"id";"N";" SWAP "]])] ()

let set_color () =
  let owners=["Point",Attribute.Point,Group.Point;
    "Vertex",Attribute.Vertex,Group.Vertex;
    "Primitive",Attribute.Primitive,Group.Primitive] in
  let count geometry=function Attribute.Point->Geometry.point_count geometry
    |Vertex->Geometry.vertex_count geometry|Primitive->Geometry.primitive_count geometry
    |Detail->1 in
  let base=List.fold_left(fun geometry(_,owner,group_owner)->
    let n=count geometry owner in
    let geometry=Geometry.with_group(Group.ordered ~owner:group_owner ~name:"ordered"
      ~length:n(Array.init((n+1)/2)(fun i->((n-1)/2-i)*2))|>get_string_ok)geometry|>get_string_ok in
    List.fold_left(fun geometry(name,selected)->Geometry.with_group
      (Group.init ~owner:group_owner ~name n selected)geometry|>get_string_ok)geometry
      ["selected",(fun i->i mod 2=0);"full",(fun _->true);"empty",(fun _->false)])
    (mixed())owners in
  let with_cd make=List.fold_left(fun geometry owner->Geometry.with_attribute
    (Attribute.create_owned ~owner ~name:"Cd"(make(count geometry owner))|>get_string_ok)
    geometry|>get_string_ok)base[Attribute.Point;Vertex;Primitive;Detail] in
  let array n value=Array.make n value in
  let storages=[
    (fun n->Attribute.Float(array n 0.1));
    (fun n->Attribute.Int(array n max_int));
    (fun n->Attribute.Text(array n "wrong \t猫😀"));
    (fun n->Attribute.Float2(Packed.Float2.of_owned ~x:(array n 1.) ~y:(array n(-0.))|>get_string_ok));
    (fun n->Attribute.Float3(Packed.Float3.of_owned ~x:(array n 1.) ~y:(array n(-0.))
      ~z:(array n 3.)|>get_string_ok));
    (fun n->Attribute.Float4(Packed.Float4.of_owned ~x:(array n 0.1) ~y:(array n(-0.))
      ~z:(array n 0.75) ~w:(array n 0.5)|>get_string_ok));
    (fun n->Attribute.Float_array(Packed.Float_array.create_owned
      ~offsets:(Array.init(n+1)Fun.id) ~values:(array n(-0.))|>get_string_ok));
    (fun n->Attribute.Int_array(Packed.Int_array.create_owned
      ~offsets:(Array.init(n+1)Fun.id) ~values:(array n max_int)|>get_string_ok))] in
  (* On each ordinary owner rows 0 and 1 are selected/unselected respectively.
     Four channels times three exceptional values times these two rows and
     three owners exercise all 72 raw cases without changing the source body. *)
  let raw=List.concat_map(fun channel->List.map(fun value->with_cd(fun n->
    let planes=Array.init 4(fun component->Array.init n(fun i->
      if component=channel && i<2 then value else if component=1 then -0. else 0.5)) in
    Attribute.Float4(Packed.Float4.of_owned ~x:planes.(0) ~y:planes.(1)
      ~z:planes.(2) ~w:planes.(3)|>get_string_ok)))
    [Int64.float_of_bits 0x7ff8000000000123L;infinity;neg_infinity])[0;1;2;3] in
  let quote text=Flow.Lisp.flat(Flow.Syntax.make(Str text)) in
  let cases=("",[])::List.concat_map(fun owner->List.map(fun group->
    Printf.sprintf ":owner %s :group %s :color [0.2 0.4 0.6] :alpha 0.8"
      (quote owner)(quote group),
    ["owner",P.Choice_value owner;"group",P.Text_value group;
     "color_r",P.Float_value 0.2;"color_g",P.Float_value 0.4;
     "color_b",P.Float_value 0.6;"alpha",P.Float_value 0.8])
    (if owner="Detail" then["";" \t\r\n "]
     else["";" \t\r\n ";"selected";"ordered";"full";"empty";"missingé";" selected "]))
    ["Point";"Vertex";"Primitive";"Detail"] in
  metadata_port ~key:"set_color" ~native_factory:Sop.Nodes.Set_color.factory
    ~cases ~extra_geometries:(base::List.map with_cd storages@raw)
    ~initial_changes:[] ~edit:["color_r",P.Float_value 0.25] ();
  (* Header refusal stays a parameter-rebuild comparison, not a cook case:
     metadata_port already compares malformed physical field types/values,
     and the retained Color alias checks every valid edit/cache/graph gesture. *)
  let input=Lisp_sop.snapshot base in
  let sources=Flow_sop.Catalog.sources()|>flow_ok in
  let factories=Sop.Nodes.Set_color.factory::List.filter(fun factory->
    Sop.Edit_graph.factory_key factory<>"set_color")Sop_catalog.Editor.factories in
  let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories|>flow_ok in
  let forms=Flow.Syntax.parse"(workspace color_header (graph g :context sop (sop/set_color (sop/grid))))"|>flow_ok in
  let checked=match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms with
    |Some checked,ds when not(List.exists(fun(d:Flow.Diagnostic.t)->d.severity=Error)ds)->checked
    |_,ds->fail(String.concat"\n"(List.map Flow.Diagnostic.to_string ds)) in
  let lowered=Flow_sop.Lower.of_checked ~factories checked|>flow_ok in
  let factory=List.find(fun factory->Sop.Edit_graph.factory_key factory="set_color")lowered.factories in
  let initial=Sop.Edit_graph.instantiate factory[input]|>get_string_ok
  and oracle=Sop.Edit_graph.instantiate Sop.Nodes.Set_color.factory[input]|>get_string_ok in
  let rejected node edits=match N.apply_parameters node edits with
    |Error message->Error message|Ok _->Ok() in
  List.iter(fun edits->let actual=rejected initial edits and expected=rejected oracle edits in
    check(actual=expected && Result.is_error actual)"Set Color header rebuild refusal")
    [["owner",P.Choice_value"Detail";"group",P.Text_value"selected"];
     ["owner",P.Choice_value"Edge"]]

let line () =
  check(not(List.exists(fun factory->Sop.Edit_graph.factory_key factory="line")
    Sop_catalog.Editor.factories)) "Line still registered as an OCaml body";
  let sources=Flow_sop.Catalog.sources() |>flow_ok in
  let factories=Sop_catalog.Editor.factories in
  let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |>flow_ok in
  List.iter(fun(kind,count)->
    let forms=Flow.Syntax.parse(Printf.sprintf
      "(workspace line_port (graph g :context sop (sop/line :kind %S :points %d :origin [-2.5 1.25 -0.75] :direction [0.25 -0.5 0.75] :length 3.75)))"
      kind count) |>flow_ok in
    let workspace=match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms with
      |Some workspace,ds when not(List.exists(fun(d:Flow.Diagnostic.t)->d.severity=Error)ds)->workspace
      |_,ds->fail(String.concat "\n"(List.map Flow.Diagnostic.to_string ds)) in
    let native=Sop.Edit_graph.instantiate Sop.Nodes.Line.factory [] |>get_string_ok in
    let native=fst(N.apply_parameters native ["kind",P.Choice_value kind;"points",P.Int_value count;
      "origin_x",P.Float_value(-2.5);"origin_y",P.Float_value 1.25;"origin_z",P.Float_value(-0.75);
      "direction_x",P.Float_value 0.25;"direction_y",P.Float_value(-0.5);"direction_z",P.Float_value 0.75;
      "length",P.Float_value 3.75] |>get_string_ok) in
    List.iter(fun reference->
      let lowered=Flow_sop.Lower.of_checked ~reference ~factories workspace |>flow_ok in
      let graph=List.hd lowered.graphs in
      let actual=Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |>get_string_ok in
      check(N.parameter_fields actual=N.parameter_fields native && N.parameter_key actual=N.parameter_key native)
        "Production Line schema/cache key";
      let s=session() in Fun.protect ~finally:(fun()->Sop.Session.close s)(fun()->
        List.iter(fun domains->let c=context domains 97 in
          check(geometry_bytes(cooked s c actual)=geometry_bytes(cooked s c native))
            "Production Line native/reference/domain bytes") [1;4;8])) [false;true])
    ["Polygon curve",4225;"Points",1;"Points",32769]

let points_port () =
  check(not(List.exists(fun factory->Sop.Edit_graph.factory_key factory="points")
    Sop_catalog.Editor.factories)) "Points still registered as an OCaml body";
  let sources=Flow_sop.Catalog.sources() |>flow_ok in
  (* point_generate, the separate input-dependent generator Points builds on, is itself a Lisp node now. *)
  check(not(List.exists(fun factory->Sop.Edit_graph.factory_key factory="point_generate")
    Sop_catalog.Editor.factories)) "Point Generate still registered as an OCaml body";
  check(List.exists(fun form->match Flow.Syntax.children form with
      |{Flow.Syntax.node=Flow.Syntax.Sym "defnode";_}::{node=Sym "sop/point_generate";_}::_->true|_->false)
    sources) "Points port removed the separate input-dependent generator";
  let factories=Sop_catalog.Editor.factories in
  let catalog=Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |>flow_ok in
  let modes=["", "sourcepoint", "sourceindex";" \t\r\n", " sourceé ", " index猫 ";
    " \t madeé猫 \n", "sourcepoint", "sourceindex";
    " same ", "same", " same ";"\194\160", "\194\160", "\226\128\131";
    "P", " P ", "p";"\000", " a\\\"é猫\n ", "\t b\000 "] in
  List.iter(fun(count,(group,source_name,index_name))->
    let forms=Flow.Syntax.parse(Printf.sprintf
      "(workspace points_port (graph g :context sop (sop/points :points %d :generated_group %S :source_point_attribute %S :source_index_attribute %S)))"
      count group source_name index_name) |>flow_ok in
    let workspace=match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms with
      |Some workspace,ds when not(List.exists(fun(d:Flow.Diagnostic.t)->d.severity=Error)ds)->workspace
      |_,ds->fail(String.concat "\n"(List.map Flow.Diagnostic.to_string ds)) in
    let native=Sop.Edit_graph.instantiate Sop.Nodes.Point_generate.factory [] |>get_string_ok in
    let native=fst(N.apply_parameters native ["points",P.Int_value count;"generated_group",P.Text_value group;
      "source_point_attribute",P.Text_value source_name;"source_index_attribute",P.Text_value index_name] |>get_string_ok) in
    List.iter(fun reference->
      let lowered=Flow_sop.Lower.of_checked ~reference ~factories workspace |>flow_ok in
      let factory=List.find(fun factory->Sop.Edit_graph.factory_key factory="points")lowered.factories in
      check(Flow_sop.Catalog.descriptor factory=Flow_sop.Catalog.descriptor Sop.Nodes.Point_generate.factory)
        "Production Points complete descriptor";
      let graph=List.hd lowered.graphs in
      let actual=Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |>get_string_ok in
      check(N.parameter_fields actual=N.parameter_fields native && N.parameter_key actual=N.parameter_key native
        && N.operation actual=N.operation native && N.version actual=N.version native && N.label actual=N.label native
        && N.inputs actual=[] && N.dependencies actual=N.dependencies native) "Production Points identity/schema/key";
      let s=session() in Fun.protect ~finally:(fun()->Sop.Session.close s)(fun()->
        List.iter(fun (domains,grain)->Sop.Session.clear s;let c=context domains grain in
          check(geometry_bytes(cooked s c actual)=geometry_bytes(cooked s c native))
            (Printf.sprintf "Production Points native/reference/domain bytes count=%d group=%S source=%S index=%S reference=%b domains=%d grain=%d"
              count group source_name index_name reference domains grain)) [1,97;8,97;4,4096])) [false;true])
    (List.concat_map(fun count->List.map(fun mode->count,mode)modes)[1;7;50])

let ()=if Array.exists ((=) "--bench") Sys.argv then benchmark ()
  else if Array.exists ((=) "--rename-attributes") Sys.argv then rename_attributes ()
  else (noise_displace ();set_float ();set_int ();set_vector ();set_orient ();set_transform ();delete_edge_group ();rename_edge_group ();delete_attributes ();rename_attributes ();group_delete ();group_rename ();swap_attributes ();set_color ();line ();points_port ())
