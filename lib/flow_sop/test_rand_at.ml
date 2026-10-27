module E = Flow.Eval
module P = Flow_ir.Packed
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let sources =
  let text = In_channel.with_open_text "nodes/prelude.rays" In_channel.input_all in
  let workspace = ok (Flow.Syntax.parse text) |> List.hd in
  Flow.Syntax.children workspace |> List.filter (fun form -> Flow.Syntax.head form = Some "defn"
    (* sop helpers may call Lisp-registered nodes, absent from this bare catalog *)
    && (match Flow.Syntax.children form with
        | _ :: _ :: { node = Kw "context"; _ } :: { node = Sym "sop"; _ } :: _ -> false
        | _ -> true))
let checked ?seed body =
  let binding=match seed with None->"" | Some seed->Printf.sprintf "seed %d " seed in
  let source = ok (Flow.Syntax.parse
    ("(workspace random (graph g :context host (let* [" ^ binding ^ "tested " ^ body ^ "] 0.0)))")) in
  match Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all ~library:false ~sources
    Flow.Check.{version=1;kinds=[]} source with
  | Some workspace, [] -> workspace
  | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
let value ?seed body = snd (List.hd (List.assoc ["g";"tested"]
  (ok (E.static ~record:true (checked ?seed body))).records))
let same expected actual = match actual with
  | E.Float_array actual ->
      assert (Array.length actual=Array.length expected);
      Array.iteri (fun i expected ->
        if Int64.bits_of_float expected <> Int64.bits_of_float actual.(i) then
          failwith (Printf.sprintf "rand_at differs at index %d: %h <> %h" i expected actual.(i))) expected
  | _ -> failwith "rand_at map must return a float array"
let compile = function
  | E.Residual residual ->
      (match P.compile_result residual (E.Residual.residual_view residual).term with
       | Ok program -> program
       | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds)))
  | _ -> failwith "mapped live value must be residual"
let map ?seed ty input body =
  let source = "(fn [(i : " ^ Flow.Ty.to_string ty ^ ")] " ^ body ^ ")" in
  let fn = match value ?seed source with
    | E.Fn fn -> fn | _ -> assert false in
  let mapped = ok (E.Private.map_function ~signature:Flow.Ty.{params=[ty];result=Float} fn [input]) in
  mapped,compile mapped
let js_atom = function
  | E.Int value -> string_of_int value ^ "n"
  | E.Float value -> Flow.Lisp.float value
  | _ -> failwith "numeric JS input expected"
let js_array values = "[" ^ String.concat "," (Array.to_list (Array.map js_atom values)) ^ "]"
let js_input = function
  | E.Int_array values -> "BigInt64Array.from([" ^
      String.concat "," (Array.to_list (Array.map (fun n -> string_of_int n ^ "n") values)) ^ "])"
  | _ -> failwith "rand_at requires an exact integer JS input"
let compare_js program ~live expected =
  let inputs = ok (P.Private.prepare_values program ~live) in
  let script = Buffer.create (Array.length expected * 48) in
  Buffer.add_string script (Flow_web.Js.region program);
  Printf.bprintf script "const inputs=[%s], uniforms=[%s], outputs=[new Float64Array(%d)];\n"
    (String.concat "," (Array.to_list (Array.map js_input inputs.arrays)))
    (String.concat "," (Array.to_list (Array.map (fun value -> js_array [|value|]) inputs.uniforms)
      @ [js_array inputs.frame])) (Array.length expected);
  Printf.bprintf script "if(region(inputs,uniforms,outputs,0,%d)!==0 || region(inputs,uniforms,outputs,%d,%d)!==0) throw Error('region failed');\n"
    (inputs.count/2) (inputs.count/2) inputs.count;
  Buffer.add_string script "const expected=[";
  Array.iteri (fun i x -> if i>0 then Buffer.add_char script ',';
    Printf.bprintf script "0x%Lxn" (Int64.bits_of_float x)) expected;
  Buffer.add_string script "]; const bits=new DataView(outputs[0].buffer);\nfor(let i=0;i<expected.length;i++) if(bits.getBigUint64(i*8,true)!==expected[i]) throw Error('rand_at JS bits at '+i);\n";
  let file = Filename.temp_file "rand_at" ".js" in
  Fun.protect ~finally:(fun () -> Sys.remove file) (fun () ->
    Out_channel.with_open_bin file (fun channel -> output_string channel (Buffer.contents script));
    assert (Sys.command ("node " ^ Filename.quote file)=0))
let benchmark operator =
  let count=1_000_000 and grain=4096 in
  let mapped=value (Printf.sprintf
    "(map (fn [i] (%s (frame/index) i)) (array/range %d :type int))" operator count) in
  let program=compile mapped and live={(Input.at_time 0.) with frame=42} in
  let expected=Array.init count (fun index -> Rays_math.Rand.float_at (Rays_math.Rand.seed 42) ~index) in
  let native () =
    let data=Array.make count 0. and seed=Rays_math.Rand.seed 42 in
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:((count-1)/grain) (fun block ->
      for index=block*grain to min count ((block+1)*grain)-1 do
        data.(index)<-Rays_math.Rand.float_at seed ~index
      done);
    E.Float_array data in
  let packed ()=ok (P.force ~strict:true program ~live) in
  let measure label domains run = Parallel.run ~domains (fun () ->
    same expected (run ());
    let measurements=Array.init 3 (fun _ ->
      Gc.full_major ();
      let before=Gc.allocated_bytes () and started=Unix.gettimeofday () in
      let result=run () in
      let seconds=Unix.gettimeofday ()-.started and allocated=Gc.allocated_bytes ()-.before in
      same expected result;
      seconds,allocated) in
    Array.sort (fun (a,_) (b,_)->Float.compare a b) measurements;
    let seconds,allocated=measurements.(1) in
    Printf.printf "%s %s count=%d grain=%d domains=%d median_seconds=%.6f allocated_bytes=%.0f\n%!"
      operator label count grain domains seconds allocated) in
  List.iter (fun domains->measure "native" domains native;measure "packed" domains packed) [1;4]

let () =
  if Array.exists ((=) "--bench-rand") Sys.argv then (benchmark "rand_at";exit 0);
  if Array.exists ((=) "--bench-rand-primitive") Sys.argv then (benchmark "rand/at";exit 0);
  let count=100000 in
  let mapped=value (Printf.sprintf
    "(map (fn [i] (rand_at (frame/index) i)) (array/range %d :type int))" count) in
  let packed=compile mapped in
  List.iter (fun seed ->
    let live={(Input.at_time 0.) with frame=seed} in
    let expected=Array.init count (fun index -> Rays_math.Rand.float_at (Rays_math.Rand.seed seed) ~index) in
    same expected (ok (E.Private.force_reference mapped ~live));
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      same expected (ok (P.force ~strict:true packed ~live)))) [1;8]) [0;1;42;max_int];
  List.iter (fun seed -> List.iter (fun index ->
    let actual=value (Printf.sprintf "(rand_at %d %d)" seed index) in
    match actual with E.Float actual ->
      assert (Int64.bits_of_float actual=Int64.bits_of_float
        (Rays_math.Rand.float_at (Rays_math.Rand.seed seed) ~index))
    | _ -> assert false) [min_int;min_int+1;-(1 lsl 53)-1;0;(1 lsl 53)+1;max_int])
    [min_int;min_int+1;0;1;42;max_int];
  let bases = [|min_int;min_int+1;-(1 lsl 53)-1;0;(1 lsl 53)+1;max_int|] in
  let indices = Array.init count (fun i -> bases.(i mod Array.length bases) + i/Array.length bases) in
  let mapped,program = map Flow.Ty.Int (E.Int_array indices) "(rand_at (frame/index) i)" in
  List.iter (fun seed ->
    (* Frame indices are nonnegative; signed seeds remain exact captured Int
       uniforms rather than invalid frame facts. *)
    let mapped,program,live = if seed<0 then
        let mapped,program=map ~seed Flow.Ty.Int (E.Int_array indices) "(rand_at seed i)" in
        mapped,program,Input.at_time 0.
      else mapped,program,{(Input.at_time 0.) with frame=seed} in
    let expected=Array.map (fun index -> Rays_math.Rand.float_at (Rays_math.Rand.seed seed) ~index) indices in
    same expected (ok (E.Private.force_reference mapped ~live));
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      same expected (ok (P.force ~strict:true program ~live)))) [1;8];
    compare_js program ~live expected) [min_int;0;42;max_int];
  print_endline "rand_at: 100,000 native/reference/packed/JavaScript exact integer keys agree at one/eight domains; signed boundaries and indices beyond 2^53 agree"

let () =
  let count=100000 in
  let bases=[|min_int;min_int+1;-(1 lsl 53)-1;0;(1 lsl 53)+1;max_int|] in
  let indices=Array.init count(fun i->bases.(i mod Array.length bases)+i/Array.length bases) in
  List.iter(fun seed ->
    let mapped,program=map ~seed Flow.Ty.Int(E.Int_array indices) "(rand/at (int/add seed (frame/index)) i)" in
    let instructions=(P.Private.view program).code in
    assert(Array.length instructions<=6);
    assert(Array.exists(function P.Rand_at _->true|_->false)instructions);
    assert(P.gpu_refusals program<>[]);
    let live=Input.at_time 0. in
    let expected=Array.map(fun index->Rays_math.Rand.float_at(Rays_math.Rand.seed seed)~index)indices in
    same expected(ok(E.Private.force_reference mapped ~live));
    List.iter(fun domains->Parallel.run ~domains(fun()->
      same expected(ok(P.force ~strict:true program ~live)))) [1;8];
    compare_js program ~live expected;
    List.iter(fun index->match value(Printf.sprintf "(rand/at %d %d)" seed index) with
      |E.Float actual->assert(Int64.bits_of_float actual=Int64.bits_of_float
        (Rays_math.Rand.float_at(Rays_math.Rand.seed seed)~index))
      |_->assert false)(Array.to_list bases)) [min_int;0;42;max_int];
  let mapped=value "(map (fn [(i : int)] [(rand/at (int/add 42 (frame/index)) i) (rand/at (int/add 43 (frame/index)) i) (rand/at (int/add 44 (frame/index)) i)]) (array/range 100000 :type int))" in
  let program=compile mapped and live=Input.at_time 0. in
  assert(Array.length(P.Private.view program).code<16);
  let expected=Array.init(count*3)(fun i->Rays_math.Rand.float_at(Rays_math.Rand.seed(42+i mod 3))~index:(i/3)) in
  List.iter(fun domains->Parallel.run ~domains(fun()->
    match ok(P.force ~strict:true program ~live) with
    |E.Vec3_array actual->same expected(E.Float_array actual)|_->assert false)) [1;8];
  compare_js program ~live expected;
  let seeds=Array.init count(fun i->bases.((i/7) mod Array.length bases)+i/Array.length bases) in
  let fn=match value "(fn [(s : int) (i : int)] (if (int/lt s 0) (rand/at s (int/add i (frame/index))) (rand/at s (int/sub i (frame/index)))))" with
    |E.Fn fn->fn|_->assert false in
  let mapped=ok(E.Private.map_function ~signature:Flow.Ty.{params=[Int;Int];result=Float}
    fn [E.Int_array seeds;E.Int_array indices]) in
  let program=compile mapped in
  let expected=Array.mapi(fun i seed->Rays_math.Rand.float_at(Rays_math.Rand.seed seed)~index:indices.(i))seeds in
  same expected(ok(E.Private.force_reference mapped ~live));
  List.iter(fun domains->Parallel.run ~domains(fun()->
    same expected(ok(P.force ~strict:true program ~live)))) [1;8];
  compare_js program ~live expected;
  print_endline "rand/at primitive: 100,000 exact signed63 keys/native/reference/CPU/JS agree; three-axis field fits registers; GPU refuses"

let () =
  let count=100000 in
  let tiny=Int64.float_of_bits 1L in
  let special=[|0.,0.,0.;-0.,-0.,-0.;tiny,-.tiny,tiny;
    1e-300,-1e-300,1e-300;-.0.37,0.73,-1.25;-1.,-2.,-3.|] in
  let points = Array.init count (fun i -> if i<Array.length special then special.(i)
    else float i *. 0.137 -. 256.2,sin (float i *. 0.37),float (i mod 19) *. (-0.31)) in
  let native_samples=ref 0 and native_bit_differences=ref 0 and native_max_ulps=ref 0. in
  let check ?(lacunarity=2.) ?(gain=0.5) points seed octaves =
    let input = E.Vec3_array (Array.init (Array.length points*3) (fun i ->
      let x,y,z=points.(i/3) in match i mod 3 with 0->x|1->y|_->z)) in
    let mapped,program = map Flow.Ty.Vec3 input
      (Printf.sprintf "(noise3 (+ i [t 0.0 0.0]) :seed %d :octaves %d :lacunarity %.17g :gain %.17g)" seed octaves lacunarity gain) in
    let live=Input.at_time 0. in
    let expected=match ok (E.Private.force_reference mapped ~live) with
      | E.Float_array values->values | _->assert false in
    List.iter (fun domains -> Parallel.run ~domains (fun () ->
      same expected (ok (P.force ~strict:true program ~live)))) [1;8];
    let noise=Rays_math.Noise.create seed in
    Array.iteri (fun i (x,y,z) ->
      let native=if lacunarity=2. && gain=0.5 then Rays_math.Noise.fbm3 ~octaves noise ~x ~y ~z
        else Rays_math.Noise.Private.fbm3_with_scratch (Rays_math.Noise.Private.create_fbm3_scratch ()) noise ~octaves ~lacunarity ~gain ~x ~y ~z in
      incr native_samples;
      if Int64.bits_of_float native <> Int64.bits_of_float expected.(i) then incr native_bit_differences;
      let _,exponent=Float.frexp (max (abs_float native) (abs_float expected.(i))) in
      let ulps=abs_float (native-.expected.(i)) /. Float.ldexp 1. (max (-1074) (exponent-53)) in
      native_max_ulps:=max !native_max_ulps ulps;
      if ulps>4. then failwith (Printf.sprintf "noise3 seed%d/octaves%d/index%d native %.17g vs Lisp %.17g: %.17g ULP"
        seed octaves i native expected.(i) ulps)) points in
  List.iter (check points 42) [1;4;32];
  List.iter (fun seed -> List.iter (check special seed) [1;4;32]) [0;1;42];
  List.iter (fun (lacunarity,gain) -> List.iter (check ~lacunarity ~gain points 7) [2;5;32])
    [1.7,0.62; 3.,0.; 0.5,1.; 2.,1.; 2.31,0.25];
  List.iter (fun source -> match value source with
    | exception Failure message when String.starts_with ~prefix:"E_NOISE_RANGE" message -> ()
    | _ -> failwith ("out-of-range noise shape accepted: " ^ source))
    ["(noise3 [0.1 -0.2 0.3] :octaves 3 :lacunarity 0.0)"; "(noise3 [0.1 -0.2 0.3] :octaves 3 :gain 1.5)";
     "(noise3 [0.1 -0.2 0.3] :octaves 3 :gain -0.5)"];
  List.iter (fun octaves ->
    match value (Printf.sprintf "(noise3 [0.1 -0.2 0.3] :octaves %d)" octaves) with
    | exception Failure message when String.starts_with ~prefix:"E_NOISE_RANGE" message -> ()
    | _ -> failwith "out-of-range noise octaves accepted") [0;65];
  Printf.printf "noise3: %d native/reference samples, %d native bit differences, max %.17g ULP; strict packed/reference bytes agree at one/eight domains for octaves1/4/32 and seeds0/1/42\n"
    !native_samples !native_bit_differences !native_max_ulps
