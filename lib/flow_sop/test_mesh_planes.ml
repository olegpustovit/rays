open Rdk
open Rdk_test_support
module E = Flow.Eval

let fields count =
  ["x",E.Float_array(Array.init count float);
   "y",E.Float_array(Array.init count(fun i->if i land 1=0 then -0. else Float.next_after 0. 1.));
   "z",E.Float_array(Array.make count 0.125)]
let replace name value = List.map(fun(key,old)->key,if key=name then value else old)
let bad ?(number=nan) value = match value with
  | E.Float_array xs -> let xs=Array.copy xs in xs.(0)<-number;E.Float_array xs
  | _ -> assert false
let args points = ["points",points;"vertex_points",E.Int_array[||];
  "primitive_offsets",E.Int_array[|0|];"kinds",E.Int_array[||]]
let stage points =
  try Flow_sop.Topology_kernel.mesh_points points with
  | Diagnostic.Error d -> Error d
let expected count fields =
  let plane name=match List.assoc name fields with E.Float_array xs->xs|_->assert false in
  Geometry.create ~positions:(Packed.Float3.View.of_owned_exn
    ~x:(Array.copy(plane "x")) ~y:(Array.copy(plane "y")) ~z:(Array.copy(plane "z")))
    ~topology:(Topology.empty ~point_count:count) () |>get_string_ok |>geometry_bytes
let diagnostic code message=Diagnostic.error ~code message
let nonfinite=diagnostic "E_NONFINITE" "array input produced a nonfinite value."
let count_error=diagnostic "E_ARRAY_COUNT" "Mesh coordinate planes need the same element count."
let record_error=diagnostic "E_TOPOLOGY_TYPE" "Mesh planes require exactly x, y and z float arrays."
let storage_error=diagnostic "E_TOPOLOGY_TYPE"
  "Mesh requires vec3 points and integer vertex_points, primitive_offsets and kinds arrays."

let checks () =
  List.iter(fun count->let input=fields count in
    let points=E.Record input and bytes=expected count input in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      let actual=Flow_sop.Topology_kernel.mesh(args points)|>function
        |Ok geometry->geometry |Error d->fail(Diagnostic.to_string d) in
      check(geometry_bytes actual=bytes)"mesh-plane stage exact payload/domain bytes";
      let output=Packed.Float3.View.view(Geometry.positions actual) in
      List.iter(fun(name,values)->match values with E.Float_array values->
        let copied=match name with "x"->output.x|"y"->output.y|_->output.z in
        (* Empty arrays can share the canonical atom and have no mutable cells. *)
        check(count=0 || copied!=values)"mesh-plane stage retains owned copies";
        if count>0 then values.(0)<-99.
        |_->assert false)input;
      check(geometry_bytes actual=bytes)"mesh-plane output must not borrow caller buffers";
      (* Restore input before the next domain's independent construction. *)
      List.iter(fun(name,values)->match values with E.Float_array values when count>0->
        values.(0)<-(match name with "x"->0.|"y"-> -0.|_->0.125)|_->())input)) [1;4;8])
    [0;3;65536;65537];
  let count=65537 in
  let input=fields count in
  let x=List.assoc "x" input and y=List.assoc "y" input and z=List.assoc "z" input in
  let shorter=E.Float_array(Array.make(count-1)0.) in
  let cases=[
    "first-plane",args(E.Record(replace "x"(bad x)input)),Error nonfinite;
    "second-before-count",args(E.Record(replace "y"(bad y)(replace "z" shorter input))),Error nonfinite;
    "third-plane",args(E.Record(replace "z"(bad ~number:infinity z)input)),Error nonfinite;
    "multiple-before-count",args(E.Record["z",bad ~number:neg_infinity z;"y",bad y;"x",shorter]),Error nonfinite;
    "finite-count",args(E.Record(replace "z" shorter input)),Error count_error;
    "malformed-before-finite",args(E.Record["x",bad x;"y",E.Int_array[||];"z",z]),Error record_error;
    "missing-field",args(E.Record["x",x;"y",y]),Error record_error;
    "duplicate-field",args(E.Record["x",bad x;"x",x;"z",z]),Error record_error;
    "outer-storage-before-finite",replace "vertex_points"(E.Float_array[||])(args(E.Record(replace "x"(bad x)input))),Error storage_error;
    "finite-before-kind",replace "kinds"(E.Int_array[|3|])(args(E.Record(replace "x"(bad x)input))),Error nonfinite;
    "count-before-kind",replace "kinds"(E.Int_array[|3|])(args(E.Record(replace "z" shorter input))),Error count_error;
    "kind-before-topology",replace "kinds"(E.Int_array[|3|])(args(E.Record input)),
      Error(diagnostic "E_TOPOLOGY_KIND" "Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline).");
    "topology-before-copy",["points",E.Record input;"vertex_points",E.Int_array[|count|];
      "primitive_offsets",E.Int_array[|0;1|];"kinds",E.Int_array[|1|]],
      Error(diagnostic "E_TOPOLOGY" "Topology: vertex references an invalid point")] in
  List.iter(fun(name,values,expected)->List.iter(fun domains->Parallel.run ~domains(fun()->
    check(Result.map geometry_bytes(Flow_sop.Topology_kernel.mesh values)=expected)
      ("mesh-plane stage exact diagnostic precedence: "^name))) [1;4;8])cases;
  print_endline "mesh-plane stage authored error precedence, exact bytes and ownership at 1/4/8 domains"

let flag name =
  let rec find=function key::value::_ when key=name->Some value|_::rest->find rest|[]->None in
  find(Array.to_list Sys.argv)
let benchmark () =
  if flag "--capture" <> None && flag "--compare" <> None then
    invalid_arg "choose capture or compare, not both";
  let count=1_000_000 and input=fields 1_000_000 in
  let first_bad=replace "x"(bad(List.assoc "x" input))input in
  let multiple=replace "y"(bad(List.assoc "y" input))
    (replace "z"(bad(E.Float_array(Array.make(count-1)0.)))input) in
  let cases=["valid",E.Record input,Ok count;
    "first_plane_nonfinite",E.Record first_bad,Error nonfinite;
    "multiple_planes_bad_count",E.Record multiple,Error nonfinite] in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:1(fun _->());
    List.iter(fun(name,points,expected_stage)->
      let stage_result=Result.map fst(stage points) in
      check(stage_result=expected_stage)("mesh-plane baseline stage outcome "^name);
      let complete=Result.map geometry_bytes(Flow_sop.Topology_kernel.mesh(args points)) in
      (match complete with
       |Ok bytes->check(bytes=expected count input)"mesh-plane baseline complete bytes"
       |Error d->check(Error d=expected_stage)"mesh-plane baseline complete diagnostic");
      let snapshot=Marshal.to_string(stage_result,complete)[] in
      let file directory=Filename.concat directory(Printf.sprintf "%s-d%d.bin" name domains) in
      Option.iter(fun directory->let channel=open_out_gen
        [Open_wronly;Open_creat;Open_excl;Open_binary]0o600(file directory) in
        Fun.protect ~finally:(fun()->close_out channel)(fun()->output_string channel snapshot))
        (flag "--capture");
      Option.iter(fun directory->let channel=open_in_bin(file directory) in
        let before=Fun.protect ~finally:(fun()->close_in channel)(fun()->
          really_input_string channel(in_channel_length channel)) in
        check(before=snapshot)"mesh-plane before/after snapshot mismatch") (flag "--compare");
      let samples=List.init 7(fun _->
        Gc.full_major();let before=Gc.quick_stat() and allocated=Gc.allocated_bytes() in
        let start=Unix.gettimeofday() in
        let result=stage points in
        let seconds=Unix.gettimeofday()-.start and after=Gc.quick_stat() in
        let bytes=Gc.allocated_bytes()-.allocated in
        check(Result.map fst result=expected_stage)"mesh-plane sample outcome changed";
        seconds,bytes,after.promoted_words-.before.promoted_words,
        after.major_words-.before.major_words)
        |>List.sort(fun(a,_,_,_)(b,_,_,_)->Float.compare a b) in
      let seconds,bytes,promoted,major=List.nth samples 3 in
      Printf.printf "mesh_plane_validation points=%d case=%s domains=%d trials=7 median_seconds=%.9f allocated_bytes=%.0f promoted_words=%.0f major_words=%.0f\n%!"
        count name domains seconds bytes promoted major)cases)) [1;4]
let () =
  if Array.exists((=)"--bench-mesh-planes")Sys.argv then benchmark() else checks()
