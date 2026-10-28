module E = Flow.Eval
module M = Rays_math.Mat4
module V = Rays_math.Vec3
module Q = Rays_math.Quat
open Test_support

let parse x = Flow.Syntax.parse x |> ok

let sources =
  let channel = open_in "nodes/prelude.rays" in
  let text =
    Fun.protect
      ~finally:(fun () -> close_in channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  let workspace = parse text |> List.hd in
  Flow.Syntax.children workspace
  |> List.filter (fun form ->
      Flow.Syntax.head form = Some "defn"
      (* sop helpers may call Lisp-registered nodes, absent from this bare catalog *)
      &&
      match Flow.Syntax.children form with
      | _ :: _ :: { node = Kw "context"; _ } :: { node = Sym "sop"; _ } :: _ -> false
      | _ -> true)

let evaluate body =
  let source = parse ("(workspace math_helpers (graph result :context host " ^ body ^ "))") in
  let workspace =
    match
      Flow_sop.Catalog.check_workspace ~ops:Flow_sop.Operators.all ~library:false ~sources
        Flow.Catalog.{ version = 1; kinds = [] }
        source
    with
    | Some workspace, [] -> workspace
    | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  List.assoc "result" (ok (E.static workspace)).results

let number = Flow.Lisp.float
let vector values = "[" ^ String.concat " " (List.map number values) ^ "]"
let triple v = vector [ v.V.x; v.y; v.z ]
let row (a, b, c, d) = vector [ a; b; c; d ]

let matrix m =
  let a, b, c, d = M.to_rows m in
  Printf.sprintf "{:r0 %s :r1 %s :r2 %s :r3 %s}" (row a) (row b) (row c) (row d)

let differences = ref []
let native_samples = ref 0
let native_differences = ref []
let native_max_ulps = ref 0.
let native_max_error = ref 0.

let distance expected actual =
  if expected = actual then 0.
  else if not (Float.is_finite expected && Float.is_finite actual) then infinity
  else
    let _, exponent = Float.frexp (max (abs_float expected) (abs_float actual)) in
    let unit = Float.ldexp 1. (max (-1074) (exponent - 53)) in
    abs_float (expected -. actual) /. unit

let describe label expected actual =
  Printf.sprintf "%s: expected %.17g (%Lx), got %.17g (%Lx)" label expected
    (Int64.bits_of_float expected) actual (Int64.bits_of_float actual)

let observe_native label expected actual =
  incr native_samples;
  let ulps = distance expected actual in
  native_max_ulps := max !native_max_ulps ulps;
  native_max_error := max !native_max_error (abs_float (expected -. actual));
  if ulps > 4. then
    native_differences :=
      Printf.sprintf "%s; %.17g ULP" (describe label expected actual) ulps :: !native_differences

let same ?(ulps = 0) label expected actual =
  if ulps > 0 then observe_native label expected actual;
  let agrees =
    if ulps = 0 then Int64.bits_of_float expected = Int64.bits_of_float actual
    else distance expected actual <= float ulps
  in
  if not agrees then differences := describe label expected actual :: !differences

let floats = function
  | E.Vec3 (a, b, c) -> [ a; b; c ]
  | E.Vec4 (a, b, c, d) -> [ a; b; c; d ]
  | E.Float x -> [ x ]
  | _ -> failwith "numeric helper result expected"

let check_vector ?(ulps = 0) label expected actual =
  List.iter2 (same ~ulps label) expected (floats actual)

let check_matrix ?(ulps = 4) label expected = function
  | E.Record fields ->
      List.iteri
        (fun i name ->
          let values = floats (List.assoc name fields) in
          List.iteri
            (fun j actual -> same ~ulps label (M.get expected ~row:i ~column:j) actual)
            values)
        [ "r0"; "r1"; "r2"; "r3" ]
  | _ -> failwith (label ^ ": matrix record expected")

let call name args = evaluate ("(" ^ name ^ " " ^ String.concat " " args ^ ")")

let check_v3 label expected actual =
  check_vector ~ulps:4 label [ expected.V.x; expected.y; expected.z ] actual

let qvalue q = vector [ q.Q.x; q.y; q.z; q.w ]
let check_q label q result = check_vector ~ulps:4 label [ q.Q.x; q.y; q.z; q.w ] result

(* Keep the prior separately rounded arithmetic as an independent audit.
   Ordinary language multiplication/addition retain that contract. *)
let product a b = Sys.opaque_identity (a *. b)

let cross_unfused a b =
  V.create
    (product a.V.y b.V.z -. product a.z b.y)
    (product a.z b.x -. product a.x b.z)
    (product a.x b.y -. product a.y b.x)

let mul_unfused a b =
  let row i =
    let component j =
      let result = ref 0. in
      for k = 0 to 3 do
        result := !result +. product (M.get a ~row:i ~column:k) (M.get b ~row:k ~column:j)
      done;
      !result
    in
    (component 0, component 1, component 2, component 3)
  in
  M.of_rows (row 0) (row 1) (row 2) (row 3)

let unfused_differences = ref 0
let audit_unfused expected actual = if distance expected actual > 4. then incr unfused_differences

let () =
  List.iter
    (fun (name, operation, samples) ->
      List.iter (fun x -> check_vector name [ operation x ] (call name [ number x ])) samples)
    [
      ("tan", tan, [ 0.; -0.; 0.37; Float.pi; 1e19 ]);
      ("asin", asin, [ -1.; -0.37; -0.; 0.; 0.37; 1. ]);
      ("acos", acos, [ -1.; -0.37; -0.; 0.; 0.37; 1. ]);
      ("exp", exp, [ -700.; -0.; 0.; 0.37; 700. ]);
      ("log", log, [ Int64.float_of_bits 1L; 1e-300; 1.; Float.max_float ]);
      ( "sign",
        (fun x -> if x > 0. then 1. else if x < 0. then -1. else x),
        [
          -.Float.max_float;
          -.Int64.float_of_bits 1L;
          -0.;
          0.;
          Int64.float_of_bits 1L;
          Float.max_float;
        ] );
    ];
  List.iter
    (fun (a, b) ->
      check_vector "atan2" [ atan2 a b ] (call "atan2" [ number a; number b ]);
      check_vector "hypot" [ Float.hypot a b ] (call "hypot" [ number a; number b ]))
    [
      (0., 0.);
      (-0., -0.);
      (0., -0.);
      (-0., 0.);
      (1., -1.);
      (-1., -1.);
      (1e308, 1e308);
      (1e-300, 1e-300);
      (Int64.float_of_bits 1L, Int64.float_of_bits 1L);
    ];
  List.iter
    (fun body ->
      try
        ignore (evaluate body);
        failwith ("Nonfinite math result accepted: " ^ body)
      with Failure message when String.starts_with ~prefix:"E_NONFINITE" message -> ())
    [
      "(asin 1.0000000000000002)";
      "(acos -1.0000000000000002)";
      "(log 0.0)";
      "(log -1.0)";
      "(exp 1000.0)";
    ];
  assert (call "floor" [ "-1.5" ] = E.Int (-2));
  assert (call "ceil" [ "-1.5" ] = E.Int (-1));
  assert (call "round" [ "-1.5" ] = E.Int (-2));
  check_matrix "identity" M.identity (call "mat4_identity" []);
  let vectors =
    [ V.zero; V.create (-0.) 2. (-3.); V.create 0.3 (-0.7) 1.2; V.create 1e-150 (-2e-150) 3e-150 ]
  in
  List.iter
    (fun v ->
      check_v3 "normalize" (V.normalize v) (call "vec3_normalize" [ triple v ]);
      check_v3 "normalize intrinsic" (V.normalize v) (call "normalize" [ triple v ]);
      check_matrix "translation" (M.translation v) (call "mat4_translation" [ triple v ]);
      check_matrix "scaling" (M.scaling v) (call "mat4_scaling" [ triple v ]))
    vectors;
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          check_vector ~ulps:4 "dot" [ V.dot a b ] (call "vec3_dot" [ triple a; triple b ]);
          check_vector ~ulps:4 "dot intrinsic" [ V.dot a b ] (call "dot" [ triple a; triple b ]);
          List.iter
            (fun name ->
              let result = call name [ triple a; triple b ] in
              let native = V.cross a b and unfused = cross_unfused a b in
              List.iter2 audit_unfused [ native.x; native.y; native.z ]
                [ unfused.x; unfused.y; unfused.z ];
              List.iter2 (observe_native name) [ native.x; native.y; native.z ] (floats result);
              check_vector name [ native.x; native.y; native.z ] result)
            [ "vec3_cross"; "cross" ];
          let unfused = cross_unfused a b in
          check_vector "ordinary unfused products"
            [ unfused.x; unfused.y; unfused.z ]
            (evaluate
               (Printf.sprintf
                  "(let* [a %s b %s] [(- (* a.y b.z) (* a.z b.y)) (- (* a.z b.x) (* a.x b.z)) (- \
                   (* a.x b.y) (* a.y b.x))])"
                  (triple a) (triple b))))
        vectors)
    vectors;
  List.iter
    (fun angle ->
      List.iter
        (fun (name, native) -> check_matrix name (native angle) (call name [ number angle ]))
        [
          ("mat4_rotation_x", M.rotation_x);
          ("mat4_rotation_y", M.rotation_y);
          ("mat4_rotation_z", M.rotation_z);
        ];
      List.iter
        (fun axis ->
          check_matrix "Rodrigues" (M.rotation ~axis angle)
            (call "mat4_rotation" [ triple axis; number angle ]))
        vectors)
    [ 0.; -0.; 0.37; Float.pi; -.Float.pi ];
  let asymmetric =
    M.of_rows (2., 3., -1., 4.) (5., -2., 6., -3.) (1., 4., 3., 2.) (0.2, -0.1, 0.4, 2.)
  in
  let swap = M.of_rows (0., 1., 0., 0.) (1., 0., 0., 0.) (0., 0., 2., 0.) (0., 0., 0., 1.) in
  let matrices =
    [
      M.identity;
      asymmetric;
      swap;
      M.mul (M.translation (V.create 2. (-4.) 7.)) (M.rotation ~axis:(V.create 1. 2. 3.) 0.7);
      M.scaling (V.create 0. 1. 2.);
      M.scaling (V.create 1e-12 1. 1.);
      M.scaling (V.create (Float.next_after 1e-12 infinity) 1. 1.);
      M.of_rows (1., 2., 3., 4.) (2., 4., 6., 8.) (0., 0., 1., 0.) (0., 0., 0., 1.);
      M.of_rows (-0., 0., 0., 0.) (0., -0., 0., 0.) (0., 0., -0., 0.) (0., 0., 0., -0.);
    ]
  in
  List.iter
    (fun m ->
      check_matrix "transpose" (M.transpose m) (call "mat4_transpose" [ matrix m ]);
      List.iter
        (fun right ->
          let result = call "mat4_mul" [ matrix m; matrix right ] in
          let native = M.mul m right and unfused = mul_unfused m right in
          for i = 0 to 3 do
            for j = 0 to 3 do
              audit_unfused (M.get native ~row:i ~column:j) (M.get unfused ~row:i ~column:j)
            done
          done;
          (match result with
          | E.Record fields ->
              List.iteri
                (fun i name ->
                  List.iteri
                    (fun j actual ->
                      observe_native
                        (Printf.sprintf "multiply[%d,%d]" i j)
                        (M.get (M.mul m right) ~row:i ~column:j)
                        actual)
                    (floats (List.assoc name fields)))
                [ "r0"; "r1"; "r2"; "r3" ]
          | _ -> failwith "matrix record expected");
          check_matrix ~ulps:0 "multiply native FMA" native result)
        [ M.identity; asymmetric ];
      List.iter
        (fun v ->
          check_v3 "point" (M.transform_point m v)
            (call "mat4_transform_point" [ matrix m; triple v ]);
          check_v3 "direction" (M.transform_direction m v)
            (call "mat4_transform_direction" [ matrix m; triple v ]);
          let a, b, c, d = M.transform m (v.x, v.y, v.z, -0.5) in
          check_vector ~ulps:4 "transform4" [ a; b; c; d ]
            (call "mat4_transform" [ matrix m; vector [ v.x; v.y; v.z; -0.5 ] ]))
        vectors;
      match (M.inverse m, call "mat4_inverse" [ matrix m ]) with
      | None, E.List values -> assert (Array.length values = 0)
      | Some expected, E.List [| actual |] -> check_matrix "inverse" expected actual
      | _ -> failwith "inverse singularity disagrees with native")
    matrices;
  List.iter
    (fun w ->
      let m = M.of_rows (1., 0., 0., 2.) (0., 1., 0., 3.) (0., 0., 1., 4.) (0., 0., 0., w) in
      let v = V.create 0.3 0.4 0.5 in
      check_v3 "projective threshold" (M.transform_point m v)
        (call "mat4_transform_point" [ matrix m; triple v ]))
    [ 0.; 1e-12; -1e-12; Float.next_after 1e-12 infinity; 2. ];
  let quats =
    [
      Q.create ~x:0. ~y:0. ~z:0. ~w:1.;
      Q.create ~x:(-0.) ~y:0. ~z:0. ~w:0.;
      Q.create ~x:1e-12 ~y:0. ~z:0. ~w:0.;
      Q.create ~x:(Float.next_after 1e-12 infinity) ~y:0. ~z:0. ~w:0.;
      Q.create ~x:0.2 ~y:(-0.7) ~z:0.3 ~w:1.2;
    ]
  in
  List.iter
    (fun q ->
      check_q "quat normalize" (Q.normalize q) (call "quat_normalize" [ qvalue q ]);
      List.iter
        (fun other ->
          check_q "quat product" (Q.mul q other) (call "quat_mul" [ qvalue q; qvalue other ]))
        quats)
    quats;
  List.iter
    (fun x ->
      check_vector "float floor" [ Float.floor x ] (call "float_floor" [ number x ]);
      check_vector "float ceil" [ Float.ceil x ] (call "float_ceil" [ number x ]))
    [
      0.;
      -0.;
      0.1;
      -0.1;
      1.5;
      -1.5;
      Int64.float_of_bits 1L;
      -.Int64.float_of_bits 1L;
      4503599627370496.;
      -4503599627370496.;
      Float.next_after 4503599627370496. neg_infinity;
      Float.next_after (-4503599627370496.) infinity;
      Float.max_float;
      -.Float.max_float;
    ];
  if !differences <> [] then failwith (String.concat "\n" (List.rev !differences));
  Printf.printf
    "Native compound audit: %d/%d components exceed four ULP; max %.17g ULP, max absolute error \
     %.17g\n"
    (List.length !native_differences) !native_samples !native_max_ulps !native_max_error;
  List.iter print_endline (List.rev !native_differences);
  if !native_differences <> [] then failwith "Native compound four-ULP gate failed";
  Printf.printf
    "Prior unfused cross/multiply audit retained: %d components exceed native four ULP\n"
    !unfused_differences;
  print_endline
    "Scalar math/rounding and explicit-FMA cross/multiply match exact bytes; ordinary products \
     remain unfused; compound native four-ULP gate passes"

let () =
  let neighbors value =
    [ Float.next_after value neg_infinity; value; Float.next_after value infinity ]
  in
  let samples =
    [ -0.; 0.; Int64.float_of_bits 1L; 1e-12; 1.; 1e308 ]
    @ neighbors (sqrt Float.max_float)
    @ neighbors (Float.ldexp 1. 511)
    @ neighbors (Float.ldexp 1. 512)
  in
  let checked = ref 0 in
  List.iter
    (fun x ->
      List.iter
        (fun (y, z, w) ->
          let value = Q.create ~x ~y ~z ~w in
          let expected = Q.normalize value in
          let actual =
            try floats (call "quat_normalize" [ qvalue value ])
            with Failure message -> failwith (qvalue value ^ ": " ^ message)
          in
          List.iter2
            (fun expected actual ->
              let agrees =
                if expected = 0. then Int64.bits_of_float expected = Int64.bits_of_float actual
                else distance expected actual <= 4.
              in
              if not agrees then
                failwith
                  (describe ("quaternion overflow/threshold " ^ qvalue value) expected actual))
            [ expected.x; expected.y; expected.z; expected.w ]
            actual;
          incr checked)
        [ (0., 0., -0.); (x, -.x, x); (0., 0., -.x); (x *. 0.5, 0., -.x *. 0.5) ])
    samples;
  List.iter
    (fun left ->
      List.iter
        (fun right ->
          let expected = not (Float.is_finite (left +. right)) in
          assert (
            evaluate
              (Printf.sprintf "(>= (+ (* %s 0.25) (* %s 0.25)) 4.49423283715579e307)" (number left)
                 (number right))
            = E.Bool expected))
        (neighbors (Float.ldexp 1. 970)))
    (List.filter Float.is_finite (neighbors Float.max_float));
  Printf.printf
    "Quaternion native threshold checks: %d cases, exact overflow/signed zeros and four-ULP finite \
     components\n"
    !checked
