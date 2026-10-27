(* Public native Merge before/after evidence for payload_builder factoring. *)
open Rdk
open Rdk_test_support
let ok=function Ok value->value|Error message->failwith message
let check condition message=if not condition then failwith message
let large_blocks=ref 43_691 and payload_blocks=ref 4097 and trials=ref 7 and grain=ref 4096
let record=ref None and verify=ref None
let machine=ref "unspecified" and profile=ref "unspecified" and revision=ref "unspecified"
let words=[|0L;Int64.min_int;1L;0x7ff0000000000000L;0xfff0000000000000L;
  0x7ff8000000000011L;0x7ff8000000000022L|]
let raw i=Int64.float_of_bits words.(i mod Array.length words)
let text i=[|"raw\255\000";"\128\254";"é日本😀";"\192\128";"�"|].(i mod 5)
let add owner name storage geometry=Geometry.with_attribute(ok(Attribute.create_owned ~owner ~name storage))geometry|>ok
let source ~payload ~seed blocks=
  let points=4*blocks and vertices=6*blocks and primitives=2*blocks in
  let topology=Topology.polygons_owned ~point_count:points
    ~vertex_points:(Array.init vertices(fun i->4*(i/6)+[|0;1;2;2;1;3|].(i mod 6)))
    ~primitive_offsets:(Array.init(primitives+1)(fun i->3*i))|>ok in
  let attributes=if not payload then [] else List.concat_map(fun owner->
    let count=match owner with Attribute.Point->points|Vertex->vertices|Primitive->primitives|Detail->1 in
    let floats offset=Array.init count(fun i->raw(seed+i+offset))in
    let csr_offsets=Array.init(count+1)(fun i->i/2)in
    List.map(fun(name,storage)->Attribute.create_owned ~owner ~name storage|>ok)
      ["float",Attribute.Float(floats 0);"int",Int(Array.init count(fun i->(1 lsl 55)+seed+i));
       "text",Text(Array.init count(fun i->text(seed+i)));
       "vec2",Float2(Packed.Float2.of_owned ~x:(floats 1) ~y:(floats 2)|>ok);
       "vec3",Float3(Packed.Float3.Private.of_owned_exn ~x:(floats 2) ~y:(floats 3) ~z:(floats 4));
       "vec4",Float4(Packed.Float4.of_owned ~x:(floats 3) ~y:(floats 4) ~z:(floats 5) ~w:(floats 6)|>ok);
       "ragged_int",Int_array(Packed.Int_array.create_owned ~offsets:csr_offsets
         ~values:(Array.init(count/2)(fun i->(1 lsl 56)+seed+i))|>ok);
       "ragged_float",Float_array(Packed.Float_array.create_owned ~offsets:(Array.copy csr_offsets)
         ~values:(Array.init(count/2)(fun i->raw(seed+i)))|>ok);
       "mat16",Float_array(Packed.Float_array.create_owned
         ~offsets:(Array.init(count+1)(fun i->16*i)) ~values:(Array.init(count*16)(fun i->raw(seed+i)))|>ok)])
      [Attribute.Point;Vertex;Primitive]in
  let groups=if not payload then [] else List.concat_map(fun owner->
    let count=match owner with Group.Point->points|Vertex->vertices|Primitive->primitives in
    [Group.init ~owner ~name:"plain" count(fun i->(i+seed)mod 3=0);
     Group.ordered ~owner ~name:"reverse" ~length:count(Array.init count(fun i->count-i-1))|>ok;
     Group.ordered ~owner ~name:"ordered_empty" ~length:count[||]|>ok])
    [Group.Point;Vertex;Primitive]in
  let edge_groups=if not payload then [] else
    let index=Topology_index.create topology in
    [Edge_group.init ~topology ~index ~name:"selected"(fun i->(i+seed)mod 3=0);
     Edge_group.init ~topology ~index ~name:"all"(fun _->true);
     Edge_group.init ~topology ~index ~name:"empty"(fun _->false)]in
  Geometry.create ~positions:(Packed.Float3.Private.of_owned_exn
      ~x:(Array.init points(fun i->raw(seed+i))) ~y:(Array.make points(-0.))
      ~z:(Array.make points(Float.next_after 0. 1.)))
    ~topology ~attributes ~groups ~edge_groups()|>ok
let padded geometry=
  let geometry=Geometry.without_group ~owner:Group.Point "reverse" geometry
    |>Geometry.without_edge_group "selected" in
  let geometry=Geometry.with_group(Group.init ~owner:Point ~name:"extra"
    (Geometry.point_count geometry)(fun i->i mod 5=0))geometry|>ok in
  let topology=Geometry.topology geometry in
  Geometry.with_edge_group(Edge_group.init ~topology ~index:(Topology_index.create topology)
    ~name:"extra"(fun i->i mod 5=0))geometry|>ok
let reordered geometry=Geometry.create ~positions:(Geometry.positions geometry)
  ~topology:(Geometry.topology geometry) ~attributes:(List.rev(Geometry.attributes geometry))
  ~groups:(Geometry.groups geometry) ~edge_groups:(Geometry.edge_groups geometry)()|>ok
let provenance geometry=add Attribute.Primitive "__bench_source"
  (Attribute.Int(Array.init(Geometry.primitive_count geometry)(fun i->700+i)))geometry

type case={name:string;sources:Geometry.t list;pad:bool;tag:bool}
let run case=Mesh_merge.merge ~grain:!grain ~pad_groups:case.pad
  ?source_attribute:(if case.tag then Some "__bench_source"else None) ~source_base:100 case.sources
let load path=let channel=open_in_bin path in
  Fun.protect ~finally:(fun()->close_in channel)(fun()->really_input_string channel(in_channel_length channel))
let save path bytes=let channel=open_out_gen[Open_wronly;Open_creat;Open_excl;Open_binary]0o600 path in
  Fun.protect ~finally:(fun()->close_out channel)(fun()->output_string channel bytes)
let capture directory recording name suffix bytes=
  let path=Filename.concat directory(name^suffix)in
  let expected=if recording then(save path bytes;bytes)else load path in
  check(bytes=expected)(name^": changed native bytes/message differ from original baseline");expected
let validate case output=
  let sum count=List.fold_left(fun total geometry->total+count geometry)0 case.sources in
  check(Geometry.point_count output=sum Geometry.point_count &&
    Geometry.vertex_count output=sum Geometry.vertex_count &&
    Geometry.primitive_count output=sum Geometry.primitive_count)(case.name^": counts changed");
  let topology=Topology.Private.view(Geometry.topology output)in
  let px=(Packed.Float3.Private.view(Geometry.positions output)).x in
  let points=ref 0 and vertices=ref 0 and primitives=ref 0 in
  List.iteri(fun block source->
    let p=Geometry.point_count source and v=Geometry.vertex_count source and n=Geometry.primitive_count source in
    let source_topology=Topology.Private.view(Geometry.topology source)in
    if p>0 then check(Int64.bits_of_float px.(!points)=
      Int64.bits_of_float(Packed.Float3.Private.view(Geometry.positions source)).x.(0))
      (case.name^": raw source P block/order changed");
    if v>0 then check(topology.vertex_points.(!vertices)=source_topology.vertex_points.(0)+ !points)
      (case.name^": connectivity offset changed");
    if n>0 then check(topology.primitive_offsets.(!primitives+n)=source_topology.primitive_offsets.(n)+ !vertices)
      (case.name^": primitive source block lost");
    if case.tag && n>0 then begin
      let expected=match Geometry.find_attribute ~owner:Attribute.Primitive "__bench_source"source with
        |None->100+block|Some a->(match Attribute.Private.storage a with Int x->x.(0)|_->failwith "bad input tag")in
      let actual=Geometry.find_attribute ~owner:Attribute.Primitive "__bench_source"output|>Option.get in
      check((match Attribute.Private.storage actual with Int x->x.(!primitives)=expected|_->false))
        (case.name^": existing/generated source provenance changed")
    end;
    List.iter(fun attribute->let count,offset=match Attribute.owner attribute with
      |Attribute.Point->p, !points|Vertex->v, !vertices|Primitive->n, !primitives|Detail->0,0 in
      if count>0 && Attribute.name attribute="text"then
        let actual=Geometry.find_attribute ~owner:(Attribute.owner attribute) "text"output|>Option.get in
        check((match Attribute.Private.storage attribute,Attribute.Private.storage actual with
          |Text a,Text b->a.(0)=b.(offset)|_->false))(case.name^": exact Text bytes changed"))
      (Geometry.attributes source);
    points:= !points+p;vertices:= !vertices+v;primitives:= !primitives+n)case.sources;
  List.iter(fun owner->match Geometry.find_group ~owner "reverse"output with
    |None->()|Some group->
      check(Group.is_ordered group)(case.name^": explicit order discarded");
      let count geometry=match owner with Group.Point->Geometry.point_count geometry
        |Vertex->Geometry.vertex_count geometry|Primitive->Geometry.primitive_count geometry in
      let expected=Array.concat(List.filter_map(fun(source,offset)->match Geometry.find_group ~owner "reverse"source with
        |None->None|Some group->let order=Option.get(Group.ordered_elements group)in
          Some(Array.map(fun i->offset+i)order))
        (let offset=ref 0 in List.map(fun source->let at= !offset in offset:=at+count source;source,at)case.sources))in
      check(Group.ordered_elements group=Some expected)(case.name^": ordered source blocks changed"))
    [Group.Point;Vertex;Primitive];
  if Geometry.edge_groups output<>[]then
    let index=Topology_index.create(Geometry.topology output)in
    let group=Geometry.find_edge_group "all"output|>Option.get in
    check(Edge_group.cardinality group=Topology_index.edge_count index)(case.name^": disjoint edge membership lost")

type sample={seconds:float;allocated:float;promoted:float;major:float;top_heap_words:int}
let measure directory recording case=
  (* Strong source indices and inputs stay alive across full GCs. Target topology
     and payload are allocated by a fresh public Merge call every sample. *)
  let indices=List.filter_map(fun g->if Geometry.edge_groups g=[]then None
    else Some(Topology_index.create(Geometry.topology g)))case.sources in
  let immutable=List.map geometry_bytes case.sources in
  let first=Parallel.run ~domains:1(fun()->run case|>ok)in
  validate case first;
  let expected=capture directory recording case.name ".geometry-bytes"(geometry_bytes first)in
  let digest=Digest.to_hex(Digest.string expected)in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    let preflight=run case|>ok in validate case preflight;
    check(geometry_bytes preflight=expected)(case.name^": domain preflight bytes differ");
    let samples=Array.init !trials(fun trial->
      Gc.full_major();let before=Gc.quick_stat()and allocated=Gc.allocated_bytes()in
      let started=Unix.gettimeofday()in
      let output=run case|>ok in let seconds=Unix.gettimeofday()-.started in
      let after=Gc.quick_stat()in
      let sample={seconds;allocated=Gc.allocated_bytes()-.allocated;
        promoted=(after.promoted_words-.before.promoted_words)*.float Sys.word_size/.8.;
        major=(after.major_words-.before.major_words)*.float Sys.word_size/.8.;top_heap_words=after.top_heap_words}in
      check(geometry_bytes output=expected)(case.name^": timed bytes differ from full native baseline");
      check(List.map geometry_bytes case.sources=immutable)(case.name^": input mutated");
      Printf.printf "sample case=%s domains=%d grain=%d trial=%d seconds=%.9f initial_domain_allocated_bytes=%.0f initial_domain_promoted_bytes=%.0f initial_domain_major_bytes=%.0f process_top_heap_words=%d\n%!"
        case.name domains !grain(trial+1)sample.seconds sample.allocated sample.promoted sample.major sample.top_heap_words;sample)in
    Array.sort(fun a b->Float.compare a.seconds b.seconds)samples;let median=samples.(!trials/2)in
    Printf.printf "median case=%s sources=%d points=%d vertices=%d primitives=%d domains=%d grain=%d trials=%d seconds=%.9f initial_domain_allocated_bytes=%.0f initial_domain_promoted_bytes=%.0f initial_domain_major_bytes=%.0f process_top_heap_words=%d exact_bytes=%d native_digest=%s\n%!"
      case.name(List.length case.sources)(Geometry.point_count first)(Geometry.vertex_count first)(Geometry.primitive_count first)
      domains !grain !trials median.seconds median.allocated median.promoted median.major median.top_heap_words(String.length expected)digest)) [1;4];
  ignore(Sys.opaque_identity indices)
let ()=
  Arg.parse ["--record",Arg.String(fun d->record:=Some d),"DIR Capture original native outputs (exclusive writes)";
    "--verify",Arg.String(fun d->verify:=Some d),"DIR Verify changed implementation against saved native outputs";
    "--large-blocks",Arg.Set_int large_blocks,"N Blocks per control input; default43691";
    "--payload-blocks",Arg.Set_int payload_blocks,"N Blocks per all-storage input; default4097";
    "--trials",Arg.Set_int trials,"N Odd positive trial count; default7";
    "--grain",Arg.Set_int grain,"N Positive unchanged native grain; default4096";
    "--machine",Arg.Set_string machine,"NAME Actual machine";
    "--profile",Arg.Set_string profile,"NAME Actual Dune profile";
    "--revision",Arg.Set_string revision,"HASH Actual code revision"]
    (fun _->raise(Arg.Bad "unexpected argument"))"bench_merge_payload --record DIR | --verify DIR";
  if !large_blocks<1 || !payload_blocks<1 || !large_blocks>Sys.max_array_length/24 ||
    !payload_blocks>Sys.max_array_length/384 || !trials<1 || !trials mod 2=0 || !grain<1 then
    invalid_arg "positive bounded block counts/grain and odd positive trials required";
  let directory,recording=match !record,!verify with Some d,None->d,true|None,Some d->d,false
    |_->invalid_arg "choose exactly one of --record or --verify"in
  if recording && not(Sys.file_exists directory)then Unix.mkdir directory 0o700;
  Printf.printf "metadata machine=%S profile=%S revision=%S ocaml=%s os=%s word_size=%d allocation_scope=initial-domain heap_high_water_scope=process-lifetime\n%!"
    !machine !profile !revision Sys.ocaml_version Sys.os_type Sys.word_size;
  let plain=source ~payload:false ~seed:0 !large_blocks in
  let a=source ~payload:true ~seed:0 !payload_blocks and b=source ~payload:true ~seed:3 !payload_blocks in
  let empty=source ~payload:true ~seed:0 0 in
  let cases=[{name="plain_repeated";sources=[plain;plain;plain;plain];pad=true;tag=false};
    {name="raw_matching";sources=[a;b;a;b];pad=true;tag=false};
    {name="raw_reordered_schema";sources=[a;reordered b;a];pad=true;tag=false};
    {name="raw_real_empty";sources=[empty;a;empty;b];pad=true;tag=false};
    {name="raw_padded_union";sources=[a;padded b;empty;a];pad=true;tag=false};
    {name="raw_exact_groups";sources=[a;b;a;b];pad=false;tag=false};
    {name="provenance_generated";sources=[a;empty;b;a];pad=true;tag=true};
    {name="provenance_retained";sources=[provenance a;b;provenance a;empty];pad=true;tag=true}]in
  List.iter(measure directory recording)cases;
  List.iter(fun(name,sources,pad)->
    let result=Mesh_merge.merge ~grain:!grain ~pad_groups:pad sources in
    let message=match result with Error message->message|Ok _->failwith(name^": expected native refusal")in
    ignore(capture directory recording name ".error-bytes" message);
    Printf.printf "native_error case=%s message=%S\n%!"name message)
    ["attribute_refusal",[a;Geometry.without_attribute ~owner:Point "float"b],true;
     "group_refusal",[a;Geometry.without_group ~owner:Point "reverse"b],false;
     "edge_refusal",[a;Geometry.without_edge_group "selected"b],false;
     "detail_refusal",[add Attribute.Detail "detail"(Float[|raw 5|])a],true];
  List.iter(fun(name,sources)->let output=Mesh_merge.merge ~grain:!grain sources|>ok in
    ignore(capture directory recording name ".geometry-bytes"(geometry_bytes output)))
    ["empty_list",[];"real_empty",[empty]]
