module N = Rays_math.Noise
module P = Parallel
let bits x y = Int64.bits_of_float x = Int64.bits_of_float y
let rejects f = match f () with
  | exception Invalid_argument _ -> ()
  | () -> failwith "accepted an invalid noise span"

let () =
  let count = 2051 in
  let x = Array.init count (fun i -> float i *. 0.137 -. 256.2)
  and y = Array.init count (fun i -> sin (float i *. 0.37))
  and z = Array.init count (fun i -> float (i mod 19) *. (-0.31)) in
  List.iter (fun seed ->
    let noise = N.create seed in
    let reference = Array.init count (fun i -> N.sample3 noise ~x:x.(i) ~y:y.(i) ~z:z.(i)) in
    let interleaved=Array.init (count*3) (fun i->match i mod 3 with
      |0->x.(i/3)|1->y.(i/3)|_->z.(i/3)) in
    let strided=Array.make count 0. in
    N.Private.sample3_into noise ~first:0 ~last:count
      ~x:interleaved ~y:interleaved ~z:interleaved ~y_offset:1 ~z_offset:2
      ~x_stride:3 ~y_stride:3 ~z_stride:3 ~output:strided ();
    assert (Array.for_all2 bits strided reference);
    rejects (fun()->N.Private.sample3_into noise ~first:0 ~last:count
      ~x ~y ~z ~x_stride:0 ~output:strided ());
    rejects (fun()->N.Private.sample3_into noise ~first:0 ~last:count
      ~x ~y ~z ~x_stride:max_int ~output:strided ());
    List.iter (fun domains -> P.run ~domains (fun () ->
      let out = Array.make count (-1.) in
      P.for_ ~chunk_size:1 ~start:0 ~finish:2 (fun block ->
        let first = block * 1024 and last = min count ((block + 1) * 1024) in
        N.Private.sample3_into noise ~first ~last ~x ~y ~z ~output:out ());
      assert (Array.for_all2 bits out reference))) [1; 8];
    let scratch = Array.make (count * 4) (-1.) in
    Array.blit x 0 scratch 0 count;
    Array.blit y 0 scratch count count;
    Array.blit z 0 scratch (count * 2) count;
    N.Private.sample3_into noise ~first:1 ~last:(count - 1)
      ~x:scratch ~y:scratch ~z:scratch ~output:scratch
      ~y_offset:count ~z_offset:(count * 2) ~output_offset:(count * 3) ();
    assert (scratch.(count * 3) = -1. && scratch.(count * 4 - 1) = -1.);
    for i = 1 to count - 2 do assert (bits scratch.(count * 3 + i) reference.(i)) done;
    let in_place = Array.copy x in
    N.Private.sample3_into noise ~first:0 ~last:count ~x:in_place ~y ~z ~output:in_place ();
    assert (Array.for_all2 bits in_place reference);
    List.iter (fun frequency ->
      let out = Array.make count 0. in
      N.Private.sample3_into noise ~first:0 ~last:count ~frequency ~x ~y ~z ~output:out ();
      Array.iteri (fun i sample -> assert (bits sample (N.sample3 noise
        ~x:(x.(i) *. frequency) ~y:(y.(i) *. frequency) ~z:(z.(i) *. frequency)))) out)
      [0.; -0.; 0.16; -3.7];
    let out = [| -1. |] in
    (* Exceptional int_of_float results are unspecified integers, but the
       permutation indexes stay masked. Every coordinate must still propagate
       nonfinite data to NaN, through both the z=0 and full 3D paths. *)
    List.iter (fun z -> List.iter (fun axis ->
      List.iter (fun (invalid,frequency) ->
        let coordinates=[|0.37;-1.25;z|] in
        coordinates.(axis)<-invalid;
        N.Private.sample3_into noise ~first:0 ~last:1 ~frequency
          ~x:[|coordinates.(0)|] ~y:[|coordinates.(1)|] ~z:[|coordinates.(2)|] ~output:out ();
        assert (Float.is_nan out.(0)))
        [nan,1.;infinity,1.;neg_infinity,1.;Float.max_float,2.;-.Float.max_float,2.]) [0;1;2]) [0.;0.73];
    List.iter (fun z -> List.iter (fun value -> List.iter (fun axis ->
      let coordinates=[|0.37;-1.25;z|] in coordinates.(axis)<-value;
      N.Private.sample3_into noise ~first:0 ~last:1
        ~x:[|coordinates.(0)|] ~y:[|coordinates.(1)|] ~z:[|coordinates.(2)|] ~output:out ();
      assert (bits out.(0) (N.sample3 noise ~x:coordinates.(0) ~y:coordinates.(1) ~z:coordinates.(2))))
      [0;1;2]) [0.;-0.;Float.max_float;-.Float.max_float;1e300;-1e300]) [0.;0.73];
    rejects (fun () -> N.Private.sample3_into noise ~first:(-1) ~last:1 ~x ~y ~z ~output:in_place ());
    rejects (fun () -> N.Private.sample3_into noise ~first:2 ~last:1 ~x ~y ~z ~output:in_place ());
    rejects (fun () -> N.Private.sample3_into noise ~first:0 ~last:count ~x_offset:max_int ~x ~y ~z ~output:in_place ());
    rejects (fun () -> N.Private.sample3_into noise ~first:0 ~last:(count - 1)
      ~x:in_place ~y ~z ~output:in_place ~output_offset:1 ());
    N.Private.sample3_into noise ~first:count ~last:count ~x ~y ~z ~output:in_place ())
  [0; 42; -17];
  List.iter (fun (op : Flow.Op.t) ->
    assert (match Flow.Op.find op.name Flow.Context.value with Some found -> found == op | None -> false);
    let result = op.body ~live:(Input.at_time 0.)
      ~node:(fun _ _ -> failwith "noise created a geometry node")
      ["position", Flow.Value.Vec3 (0.37, -1.25, 17.4)] in
    assert (Flow.Ty.fits (Flow.Value.ty_of result) (op.out [Flow.Ty.Vec3]));
    match result with
    | Flow.Value.Float n -> assert (bits n (N.sample3 (N.create 0) ~x:0.37 ~y:(-1.25) ~z:17.4))
    | _ -> assert false) [Flow.Op.noise3];
  print_endline "Packed noise spans, scalar bits and one/eight-domain parity passed"

let () =
  let forms=Result.get_ok (Flow.Syntax.parse "(workspace noise_scaling (graph g :context value
    (let* [tested (map (fn [p] (if (> t 1e308) (noise3 (* p t)) 0.0))
      (array/vec3 2051 [2.0 3.0 4.0]))] 0.0)))") in
  let workspace=match Flow.Workspace.check ~ops:[] {Flow.Catalog.version=1;kinds=[]} forms with
    |Some w,ds when not (List.exists (fun (d:Diagnostic.t)->d.severity=Diagnostic.Error_severity) ds)->w
    |_,ds->failwith (String.concat "\n" (List.map Diagnostic.to_string ds)) in
  let evaluation=Result.get_ok (Flow.Eval.static ~record:true workspace) in
  let value=List.assoc ["g";"tested"] evaluation.records |> List.hd |> snd in
  let packed=match value with Flow.Eval.Residual r->
    Flow_ir.Packed.compile r (Flow.Eval.Residual.residual_view r).term |> Option.get
    |_->assert false in
  (match Flow_ir.Packed.force ~strict:true packed ~live:(Input.at_time 1e308) with
   | Ok (Flow.Eval.Float_array values) -> assert (Array.for_all ((=) 0.) values)
   | _ -> failwith "Untaken scaled noise branch was evaluated");
  assert (Result.is_error (Flow_ir.Packed.force ~strict:true packed ~live:(Input.at_time Float.max_float)));
  print_endline "Packed scaled noise skips untaken overflow and refuses selected overflow"

let () =
  let noise = Flow.Op.noise3 in
  assert (Flow.Op.packed_kind noise = Some Flow.Op.Noise3);
  assert (noise.packed_extension = Some Flow.Op.Noise3);
  List.iter (fun (op : Flow.Op.t) ->
    assert (op.packed_extension = None && Flow.Op.packed_kind op <> Some Flow.Op.Noise3))
    (List.filter (fun (op : Flow.Op.t) ->
      List.mem op.name ["color/hsl"; "color/hsla"; "color/gradient"]) Flow.Op.all);
  let reversed = {noise with signature = {noise.signature with kw = List.rev noise.signature.kw}} in
  assert (Flow.Op.packed_kind reversed = Some Flow.Op.Noise3);
  List.iter (fun (op : Flow.Op.t) ->
    assert (Flow.Op.packed_kind op = None);
    match Flow.Op.validate [op] with
    | Some d -> assert (d.code = "E_OP_DECLARATION" && String.length d.message > 0)
    | None -> assert false)
    [{noise with signature = {noise.signature with pos = ["position", Flow.Ty.Vec2]}};
     {noise with out = (fun _ -> Flow.Ty.Vec3)};
     {noise with signature = {noise.signature with kw = ["seed", Flow.Ty.Int; "seed", Flow.Ty.Int]}};
     {noise with signature = {noise.signature with kw = ["seed", Flow.Ty.Int; "octaves", Flow.Ty.Float]}};
     {noise with name = "other_noise"}; {noise with live = true};
     {noise with name = "toy_intrinsic"; packed_extension = Some Flow.Op.Fma}];
  let sine = Option.get (Flow.Op.find "sin" Flow.Context.value) in
  let counterfeit = {sine with name = "noise3"} in
  assert (Option.fold ~none:false ~some:(fun (d : Diagnostic.t) -> d.code = "E_OP_DECLARATION")
    (Flow.Op.validate [counterfeit]) && Flow.Op.packed_kind counterfeit = None);
  let compile (op : Flow.Op.t) body source =
    let text = "(workspace w (graph g :context value (let* [tested (map (fn [p] "
      ^ body ^ ") " ^ source ^ ")] 0.0)))" in
    let forms = Result.get_ok (Flow.Syntax.parse text) in
    let ops = if op.name = "noise3" then [] else [op] in
    let workspace = match Flow.Workspace.check ~ops {Flow.Catalog.version=1;kinds=[]} forms with
      | Some w, [] -> w
      | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds)) in
    let evaluated = Result.get_ok (Flow.Eval.static ~record:true workspace) in
    match List.assoc ["g";"tested"] evaluated.records |> List.hd |> snd with
    | Flow.Eval.Residual residual ->
        Flow_ir.Packed.compile residual (Flow.Eval.Residual.residual_view residual).term
    | _ -> failwith "capability test did not defer its map" in
  assert (Option.is_some (compile noise "(noise3 (+ p [t 0 0]))" "(array/vec3 2051 [0.3 0.7 -0.2])"));
  let custom = {noise with name = "toy_noise3"; packed_extension = None} in
  assert (compile custom "(toy_noise3 (+ p [t 0 0]))" "(array/vec3 2051 [0.3 0.7 -0.2])" = None)
