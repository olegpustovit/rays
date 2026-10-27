(* Public Rewire Vertices before/after baseline for the shared CSR edge consumer. *)
open Rdk
open Rdk_test_support

let ok=function Ok value->value|Error message->failwith message
let get=function Ok value->value|Error error->failwith(Error.to_string error)
let add owner name storage geometry=
  Geometry.with_attribute(ok(Attribute.create_owned ~owner ~name storage))geometry|>ok
let blocks=ref 174_763 and trials=ref 7 and grain=ref 4096
let record=ref None and verify=ref None
let machine=ref "unspecified" and profile=ref "unspecified" and revision=ref "unspecified"
let check condition message=if not condition then failwith message
let raw i=Int64.float_of_bits [|0x8000000000000000L;0x7ff8000000000042L;
  0xfff0000000000000L;0x7ff0000000000000L;1L;0L|].(i mod 6)

let source blocks=
  let points=4*blocks and vertices=6*blocks and primitives=2*blocks in
  let topology=Topology.polygons_owned ~point_count:points
    ~vertex_points:(Array.init vertices(fun i->4*(i/6)+[|0;1;2;2;1;3|].(i mod 6)))
    ~primitive_offsets:(Array.init(primitives+1)(fun i->3*i))|>ok in
  let index=Topology_index.create topology in
  let edge_groups=List.map(fun(name,pick)->Edge_group.init ~topology ~index ~name(fun edge->
    let a,b=Topology_index.edge_points index edge in pick(a mod 4)(b mod 4)))
    ["from_zero",(fun a b->a=0 && b=1);"from_three",(fun a b->a=1 && b=3);
     "all",(fun _ _->true);"empty",(fun _ _->false)] in
  let groups=[Group.ordered ~owner:Group.Point ~name:"reverse" ~length:points
      (Array.init points(fun i->points-i-1))|>ok;
    Group.init ~owner:Group.Vertex ~name:"corners" vertices(fun i->i mod 3=0);
    Group.init ~owner:Group.Primitive ~name:"first_face" primitives(fun i->i mod 2=0)]in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn
      ~x:(Array.init points raw) ~y:(Array.make points(-0.)) ~z:(Array.make points(Float.next_after 0. 1.)))
    ~topology ~groups ~edge_groups ()|>ok
  |>add Attribute.Point "raw"(Attribute.Float(Array.init points raw))
  |>add Attribute.Point "text"(Attribute.Text(Array.init points(fun i->
    [|"raw\255\000";"é日本😀";"\000\128\254"|].(i mod 3))))
  |>add Attribute.Vertex "corner"(Attribute.Int(Array.init vertices Fun.id))
  |>add Attribute.Detail "detail"(Attribute.Float[|raw 1|])

let without_edges geometry=Geometry.create ~positions:(Geometry.positions geometry)
  ~topology:(Geometry.topology geometry) ~attributes:(Geometry.attributes geometry)
  ~groups:(Geometry.groups geometry)()|>ok

type mode=Identity|Permute|Coalesce
let input mode geometry=
  add Attribute.Point "target"(Attribute.Int(Array.init(Geometry.point_count geometry)(fun point->
    let base=point-point mod 4 in match mode,point mod 4 with
    |Identity,_->(-1)|Permute,0->base+3|Permute,3->base
    |Coalesce,0->base+3|_->(-1))))geometry
let run ~keep input=Rewire_vertices.run ~grain:!grain ~keep_unused_points:keep
  ~original_point_attribute:"original" ~owner:Attribute.Point ~target_attribute:"target" input|>get
let baseline_file directory name=Filename.concat directory(name^".geometry-bytes")
let load path=let channel=open_in_bin path in
  Fun.protect ~finally:(fun()->close_in channel)(fun()->really_input_string channel(in_channel_length channel))
let save path bytes=let channel=open_out_gen[Open_wronly;Open_creat;Open_excl;Open_binary]0o600 path in
  Fun.protect ~finally:(fun()->close_out channel)(fun()->output_string channel bytes)

let validate name mode keep input output=
  let n= !blocks in
  check(Geometry.point_count output=(if keep || mode<>Coalesce then 4*n else 3*n))
    (name^": wrong retained point count");
  check(Topology.vertex_count(Geometry.topology output)=6*n && Geometry.primitive_count output=2*n)
    (name^": topology cardinality changed");
  let expected=[|0;1;2;2;1;3|] in
  let expected=Array.map(fun p->match mode with Identity->p|Permute->if p=0 then 3 else if p=3 then 0 else p
    |Coalesce->if p=0 then 3 else p)expected in
  let expected=if keep then expected else Array.map(fun p->p-1)expected in
  let topology=Topology.Private.view(Geometry.topology output)in
  check(Array.sub topology.vertex_points 0 6=expected)(name^": target corners changed");
  (match Geometry.find_attribute ~owner:Attribute.Vertex "original" output with
   |Some attribute->(match Attribute.Private.storage attribute with
     |Attribute.Int values->check(Array.sub values 0 6=[|0;1;2;2;1;3|])(name^": ancestry lost")
     |_->failwith "original-point ancestry has wrong storage")|None->failwith "ancestry missing");
  (match Geometry.edge_groups output with []->check(Geometry.edge_groups input=[])(name^": groups dropped")
   |groups->let index=Topology_index.create(Geometry.topology output)in
     check(Topology_index.edge_count index=(if mode=Coalesce then 3*n else 5*n))(name^": wrong unique-edge count");
     check(List.map Edge_group.name groups=["from_zero";"from_three";"all";"empty"])(name^": edge group order changed");
     let endpoint a b=if keep then a,b else a-1,b-1 in
     let a,b=endpoint(if mode=Identity then 0 else 3)1 in
     let edge=Topology_index.find_edge_index index ~a ~b in
     check(edge>=0 && Edge_group.mem edge(List.hd groups))(name^": retained/OR edge ancestry lost");
     let a,b=endpoint 1(if mode=Permute then 0 else 3)in
     let edge=Topology_index.find_edge_index index ~a ~b in
     check(edge>=0 && Edge_group.mem edge(List.nth groups 1))(name^": second source ancestry lost");
     check(Edge_group.cardinality(List.nth groups 0)=n && Edge_group.cardinality(List.nth groups 1)=n
       && Edge_group.cardinality(List.nth groups 2)=Topology_index.edge_count index
       && Edge_group.cardinality(List.nth groups 3)=0)(name^": edge membership changed"))

type sample={seconds:float;allocated:float;promoted:float;major:float;top_heap_words:int}
let measure directory recording name mode keep input=
  let source_index=if Geometry.edge_groups input=[] then None
    else Some(Topology_index.create(Geometry.topology input))in
  let immutable=geometry_bytes input in
  let first=Parallel.run ~domains:1(fun()->run ~keep input)in
  validate name mode keep input first;
  let bytes=geometry_bytes first in
  let path=baseline_file directory name in
  let expected=if recording then(save path bytes;bytes)else load path in
  check(bytes=expected)(name^": BEFORE/AFTER native payload bytes differ");
  let digest=Digest.to_hex(Digest.string expected)in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    (* Domain pool startup is outside timing; every output gets a fresh topology.
       The immutable source index is already warm, as required by edge groups. *)
    let preflight=run ~keep input in validate name mode keep input preflight;
    check(geometry_bytes preflight=expected)(name^": domain preflight differs");
    let samples=Array.init !trials(fun trial->
      Gc.full_major();
      let before=Gc.quick_stat()and allocated=Gc.allocated_bytes()in
      let started=Unix.gettimeofday()in
      let output=run ~keep input in
      let seconds=Unix.gettimeofday()-.started in
      let after=Gc.quick_stat()in
      let sample={seconds;allocated=Gc.allocated_bytes()-.allocated;
        promoted=(after.promoted_words-.before.promoted_words)*.float Sys.word_size/.8.;
        major=(after.major_words-.before.major_words)*.float Sys.word_size/.8.;top_heap_words=after.top_heap_words}in
      check(geometry_bytes output=expected)(name^": timed output differs from captured native baseline");
      check(geometry_bytes input=immutable)(name^": immutable input mutated");
      Printf.printf "sample case=%s domains=%d grain=%d trial=%d seconds=%.9f initial_domain_allocated_bytes=%.0f initial_domain_promoted_bytes=%.0f initial_domain_major_bytes=%.0f process_top_heap_words=%d\n%!"
        name domains !grain(trial+1)sample.seconds sample.allocated sample.promoted sample.major sample.top_heap_words;
      sample)in
    Array.sort(fun a b->Float.compare a.seconds b.seconds)samples;
    let median=samples.(!trials/2)in
    Printf.printf "median case=%s points=%d vertices=%d primitives=%d source_edge_groups=%d domains=%d grain=%d trials=%d seconds=%.9f initial_domain_allocated_bytes=%.0f initial_domain_promoted_bytes=%.0f initial_domain_major_bytes=%.0f process_top_heap_words=%d exact_bytes=%d native_digest=%s\n%!"
      name(Geometry.point_count input)(Topology.vertex_count(Geometry.topology input))(Geometry.primitive_count input)
      (List.length(Geometry.edge_groups input))domains !grain !trials median.seconds median.allocated median.promoted median.major
      median.top_heap_words(String.length expected)digest)) [1;4];
  ignore(Sys.opaque_identity source_index)

let ()=
  Arg.parse ["--record",Arg.String(fun directory->record:=Some directory),"DIR Capture original native bytes (exclusive writes)";
    "--verify",Arg.String(fun directory->verify:=Some directory),"DIR Compare changed implementation to original native bytes";
    "--blocks",Arg.Set_int blocks,"N Two-triangle blocks; default174763 (~1M corners)";
    "--trials",Arg.Set_int trials,"N Odd positive trial count; default7";
    "--grain",Arg.Set_int grain,"N Explicit positive grain; default4096";
    "--machine",Arg.Set_string machine,"NAME Actual machine description";
    "--profile",Arg.Set_string profile,"NAME Actual Dune compiler profile";
    "--revision",Arg.Set_string revision,"HASH Actual code revision"]
    (fun _->raise(Arg.Bad "unexpected argument"))"bench_rewire_payload --record DIR | --verify DIR";
  if !blocks<1 || !blocks>Sys.max_array_length/6 || !trials<1 || !trials mod 2=0 || !grain<1 then
    invalid_arg "positive bounded blocks/grain and odd positive trials required";
  let directory,recording=match !record,!verify with Some d,None->d,true|None,Some d->d,false
    |_->invalid_arg "choose exactly one of --record DIR or --verify DIR"in
  if recording && not(Sys.file_exists directory)then Unix.mkdir directory 0o700;
  Printf.printf "metadata machine=%S profile=%S revision=%S ocaml=%s os=%s word_size=%d allocation_scope=initial-domain heap_high_water_scope=process-lifetime\n%!"
    !machine !profile !revision Sys.ocaml_version Sys.os_type Sys.word_size;
  let with_edges=source !blocks in
  List.iter(fun(name,mode,keep,geometry)->measure directory recording name mode keep(input mode geometry))
    ["no_edges_permute",Permute,true,without_edges with_edges;
     "edges_metadata_only",Identity,true,with_edges;
     "edges_permute",Permute,true,with_edges;
     "edges_coalesce",Coalesce,true,with_edges;
     "edges_coalesce_compact",Coalesce,false,with_edges]
