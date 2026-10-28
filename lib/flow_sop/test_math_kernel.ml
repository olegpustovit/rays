module E = Flow.Eval
module P = Flow_ir.Packed
open Test_support

let function_value ?(sources = []) input_ty body =
  let source =
    Flow.Syntax.parse
      ("(workspace math (graph result :context value (let* [tested (fn [(x : "
     ^ Flow.Ty.to_string input_ty ^ ")] " ^ body ^ ")] 0.0)))")
    |> ok
  in
  let workspace =
    match
      Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all ~library:false ~sources
        Flow.Catalog.{ version = 1; kinds = [] }
        source
    with
    | Some w, [] -> w
    | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let evaluation = ok (E.static ~record:true workspace) in
  match snd (List.hd (List.assoc [ "result"; "tested" ] evaluation.records)) with
  | E.Fn f -> f
  | _ -> assert false

let compile ?sources input_ty output_ty input body =
  let fn = function_value ?sources input_ty body in
  let value =
    ok
      (E.Host.map_function
         ~signature:Flow.Ty.{ params = [ input_ty ]; result = output_ty }
         fn [ input ])
  in
  match value with
  | E.Residual residual ->
      let term = (E.Residual.residual_view residual).term in
      let program =
        match P.compile_result residual term with
        | Ok p -> p
        | Error ds -> failwith (body ^ "\n" ^ String.concat "\n" (List.map Diagnostic.to_string ds))
      in
      (value, program)
  | _ -> failwith (body ^ ": live packed value expected")

let bytes = fun value -> Marshal.to_string value [ Marshal.No_sharing ]

let () =
  let count = 32769 in
  let scalar =
    E.Float_array (Array.init count (fun i -> if i = 0 then -0. else sin (float i *. 0.31) *. 0.8))
  in
  let vectors =
    E.Vec3_array
      (Array.init (count * 3) (fun i ->
           if i < 3 then -0. else sin (float i *. 0.73) *. Float.ldexp 1. ((i mod 40) - 20)))
  in
  let cases =
    List.map
      (fun body -> (Flow.Ty.Float, Flow.Ty.Float, scalar, body))
      [
        "(tan (+ x t))";
        "(atan2 x t)";
        "(asin (* (+ x t) 0.25))";
        "(acos (* (+ x t) 0.25))";
        "(exp (+ x t))";
        "(log (+ (+ x t) 4.0))";
        "(sign (+ x t))";
        "(hypot x t)";
      ]
    @ [
        (Flow.Ty.Vec3, Float, vectors, "(dot x [t 0.3 -0.2])");
        (Flow.Ty.Vec3, Vec3, vectors, "(cross x [t 0.3 -0.2])");
        (Flow.Ty.Vec3, Vec3, vectors, "(normalize (+ x [t 0.0 0.0]))");
        (Flow.Ty.Vec3, Vec3, vectors, "(normalize (+ x [(float (frame/index)) t 0.0]))");
      ]
  in
  List.iter
    (fun (input_ty, output_ty, input, body) ->
      let value, program = compile input_ty output_ty input body in
      List.iter
        (fun time ->
          let live = Input.at_time time in
          let expected = ok (E.Host.force_reference value ~live) |> bytes in
          List.iter
            (fun domains ->
              Parallel.run ~domains (fun () ->
                  let actual = ok (P.force ~strict:true program ~live) |> bytes in
                  if actual <> expected then
                    failwith (Printf.sprintf "%s differs at %g on %d domains" body time domains)))
            [ 1; 8 ])
        [ -0.; 0.; 0.37; 1. ])
    cases;
  List.iter
    (fun (input, body) ->
      let value, program = compile Flow.Ty.Float Flow.Ty.Float input body in
      let live = Input.at_time 0. in
      let expected = ok (E.Host.force_reference value ~live) |> bytes in
      List.iter
        (fun domains ->
          Parallel.run ~domains (fun () ->
              assert (bytes (ok (P.force ~strict:true program ~live)) = expected)))
        [ 1; 8 ])
    [ (E.Float_array [| 1e308; 1e-300; Int64.float_of_bits 1L; -0. |], "(hypot x (+ x t))") ];
  List.iter
    (fun body ->
      let value, program = compile Flow.Ty.Float Flow.Ty.Float (E.Float_array [| 2.; -2. |]) body in
      let live = Input.at_time 0. in
      let nonfinite = function
        | Error d ->
            if d.Diagnostic.code <> "E_NONFINITE" then
              failwith (body ^ ": " ^ Diagnostic.to_string d)
        | Ok _ -> failwith (body ^ ": expected nonfinite error")
      in
      nonfinite (E.Host.force_reference value ~live);
      nonfinite (P.force ~strict:true program ~live))
    [ "(asin (+ x t))"; "(acos (+ x t))"; "(log (+ x t))"; "(exp (+ (* x 1000.0) t))" ];
  print_endline
    "Packed math matches native reference bytes on one/eight domains; invalid domains refuse"

let () =
  let channel = open_in "nodes/prelude.rays" in
  let text =
    Fun.protect
      ~finally:(fun () -> close_in channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  let sources =
    Flow.Syntax.parse text |> ok |> List.hd |> Flow.Syntax.children
    |> List.filter (fun form ->
        Flow.Syntax.head form = Some "defn"
        (* sop helpers may call Lisp-registered nodes, absent from this bare catalog *)
        &&
        match Flow.Syntax.children form with
        | _ :: _ :: { node = Kw "context"; _ } :: { node = Sym "sop"; _ } :: _ -> false
        | _ -> true)
  in
  let limit = sqrt Float.max_float in
  let cases =
    [|
      (-0., 0., 0., 0.);
      (1e-12, 0., 0., 0.);
      (1e308, -1e308, 0., -0.);
      (limit, 0., 0., -0.);
      (Float.next_after limit infinity, 0., 0., -0.);
      (limit *. 0.5, limit *. 0.25, 0., -.limit *. 0.25);
      (limit, limit, -.limit, limit);
    |]
  in
  let input =
    E.Vec4_array
      (Array.init (32769 * 4) (fun index ->
           let x, y, z, w = cases.(index / 4 mod Array.length cases) in
           match index mod 4 with 0 -> x | 1 -> y | 2 -> z | _ -> w))
  in
  let value, program =
    compile ~sources Flow.Ty.Vec4 Flow.Ty.Vec4 input "(quat_normalize [x.x x.y x.z (+ x.w t)])"
  in
  let live = Input.at_time 0. in
  let expected = bytes (ok (E.Host.force_reference value ~live)) in
  List.iter
    (fun domains ->
      Parallel.run ~domains (fun () ->
          assert (bytes (ok (P.force ~strict:true program ~live)) = expected)))
    [ 1; 8 ];
  print_endline
    "Quaternion finite-overflow guards match strict reference bytes at one/eight domains on 32769 \
     elements"

let () =
  let text = In_channel.with_open_text "nodes/prelude.rays" In_channel.input_all in
  let sources =
    Flow.Syntax.parse text |> ok |> List.hd |> Flow.Syntax.children
    |> List.filter (fun form ->
        Flow.Syntax.head form = Some "defn"
        (* sop helpers may call Lisp-registered nodes, absent from this bare catalog *)
        &&
        match Flow.Syntax.children form with
        | _ :: _ :: { node = Kw "context"; _ } :: { node = Sym "sop"; _ } :: _ -> false
        | _ -> true)
  in
  let count = 5001 and live = Input.at_time 0. in
  let cases =
    [|
      Rays_math.Vec3.zero;
      Rays_math.Vec3.create (-0.) 0. (-0.);
      Rays_math.Vec3.create 0.3 (-0.7) 1.2;
      Rays_math.Vec3.create 1e-150 (-2e-150) 3e-150;
    |]
  in
  let vectors =
    Array.init (count * 3) (fun i ->
        let v = cases.(i / 3 mod Array.length cases) in
        match i mod 3 with 0 -> v.x | 1 -> v.y | _ -> v.z)
  in
  let script = Buffer.create 200000 in
  let array values =
    "Float64Array.from(["
    ^ String.concat "," (Array.to_list (Array.map Flow.Lisp.float values))
    ^ "])"
  in
  let js_value = function
    | E.Float x -> Flow.Lisp.float x
    | Int n -> string_of_int n ^ "n"
    | Vec2 (a, b) -> "[" ^ String.concat "," (List.map Flow.Lisp.float [ a; b ]) ^ "]"
    | Vec3 (a, b, c) -> "[" ^ String.concat "," (List.map Flow.Lisp.float [ a; b; c ]) ^ "]"
    | Vec4 (a, b, c, d) -> "[" ^ String.concat "," (List.map Flow.Lisp.float [ a; b; c; d ]) ^ "]"
    | _ -> failwith "numeric FMA helper uniform expected"
  in
  Buffer.add_string script
    "const b=new DataView(new ArrayBuffer(8)); const bits=x=>{b.setFloat64(0,x,true);return \
     b.getBigUint64(0,true)};\n";
  let proof input_ty output_ty input body native =
    let value, program = compile ~sources input_ty output_ty input body in
    let compiled = Flow_ir.Executor.compile ~strict:true value |> ok in
    let expected = native in
    let check = function
      | E.Vec3_array values | Vec4_array values ->
          assert (Array.length values = Array.length expected);
          Array.iteri
            (fun i x ->
              if Int64.bits_of_float x <> Int64.bits_of_float expected.(i) then
                failwith
                  (Printf.sprintf "Native FMA helper %s component%d: %h <> %h" body i x expected.(i)))
            values
      | _ -> failwith "FMA helper vector array expected"
    in
    check (ok (E.Host.force_reference value ~live));
    List.iter
      (fun domains ->
        Parallel.run ~domains (fun () ->
            check (ok (P.force ~strict:true program ~live));
            check (ok (Flow_ir.Executor.force ~strict:true compiled ~live))))
      [ 1; 4; 8 ];
    assert (P.gpu_refusals program <> []);
    let inputs = P.Host.prepare_values program ~live |> ok in
    Buffer.add_string script "{\n";
    Buffer.add_string script (Flow_web.Js.region program);
    let arrays =
      Array.map
        (function E.Vec3_array a | Vec4_array a | Float_array a -> array a | _ -> assert false)
        inputs.arrays
    in
    let uniforms =
      Array.to_list
        (Array.map
           (function
             | (E.Vec2 _ | Vec3 _ | Vec4 _) as v -> js_value v | v -> "[" ^ js_value v ^ "]")
           inputs.uniforms)
      @ [ "[" ^ String.concat "," (Array.to_list (Array.map js_value inputs.frame)) ^ "]" ]
    in
    Printf.bprintf script
      "const out=new Float64Array(%d);if(region([%s],[%s],[out],0,%d)!==0)throw Error('FMA helper \
       refused');\n"
      (Array.length expected)
      (String.concat "," (Array.to_list arrays))
      (String.concat "," uniforms) inputs.count;
    Printf.bprintf script
      "const expected=[%s];for(let i=0;i<out.length;i++)if(bits(out[i])!==expected[i])throw \
       Error('FMA helper bits '+i);}\n"
      (String.concat ","
         (Array.to_list
            (Array.map (fun x -> Printf.sprintf "0x%Lxn" (Int64.bits_of_float x)) expected)))
  in
  let expected =
    Array.init (count * 3) (fun i ->
        let v = cases.(i / 3 mod Array.length cases) in
        let v = Rays_math.Vec3.cross v v in
        match i mod 3 with 0 -> v.x | 1 -> v.y | _ -> v.z)
  in
  List.iter
    (fun body -> proof Flow.Ty.Vec3 Flow.Ty.Vec3 (E.Vec3_array vectors) body expected)
    [ "(cross x x)"; "(vec3_cross x x)" ];
  let right =
    Rays_math.Mat4.of_rows (2., 3., -1., 4.) (5., -2., 6., -3.) (1., 4., 3., 2.) (0.2, -0.1, 0.4, 2.)
  in
  let rows = [| (-0., 0., -0., 0.); (0.3, -0.7, 1.2, -0.5); (1e-150, -2e-150, 3e-150, -4e-150) |] in
  let input =
    Array.init (count * 4) (fun i ->
        let a, b, c, d = rows.(i / 4 mod Array.length rows) in
        match i mod 4 with 0 -> a | 1 -> b | 2 -> c | _ -> d)
  in
  let expected =
    Array.init (count * 4) (fun i ->
        let left =
          Rays_math.Mat4.of_rows
            rows.(i / 4 mod Array.length rows)
            (0., 0., 0., 0.) (0., 0., 0., 0.) (0., 0., 0., 0.)
        in
        Rays_math.Mat4.get (Rays_math.Mat4.mul left right) ~row:0 ~column:(i mod 4))
  in
  proof Flow.Ty.Vec4 Flow.Ty.Vec4 (E.Vec4_array input)
    "(get (mat4_mul {:r0 x :r1 [0.0 0.0 0.0 0.0] :r2 [0.0 0.0 0.0 0.0] :r3 [0.0 0.0 0.0 0.0]} {:r0 \
     [2.0 3.0 -1.0 4.0] :r1 [5.0 -2.0 6.0 -3.0] :r2 [1.0 4.0 3.0 2.0] :r3 [0.2 -0.1 0.4 2.0]}) \
     :r0)"
    expected;
  let path = Filename.temp_file "native-fma-helpers" ".js" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun channel -> output_string channel (Buffer.contents script));
      assert (Sys.command ("node " ^ Filename.quote path) = 0));
  List.iter
    (fun body ->
      let value, program =
        compile Flow.Ty.Vec3 Flow.Ty.Vec3 (E.Vec3_array [| 0.; Float.max_float; 0. |]) body
      in
      List.iter
        (fun result ->
          match result with
          | Error d -> assert (d.Diagnostic.code = "E_NONFINITE")
          | Ok _ -> failwith "Cross overflow accepted")
        [ E.Host.force_reference value ~live; P.force ~strict:true program ~live ])
    [ "(cross x [0.0 0.0 2.0])" ];
  print_endline
    "Explicit-FMA cross and matrix products:5001 exact native/reference/strict Core/packed/JS \
     rows, signed zeros/cancellation/subnormals, one/four/eight domains and overflow refusal"
