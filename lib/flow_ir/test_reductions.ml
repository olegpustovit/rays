module E = Flow.Eval
module V = Flow.Value
module O = Flow.Op
module I = Flow_ir
open Test_support

let same a b =
  Marshal.to_string a [ Marshal.No_sharing ] = Marshal.to_string b [ Marshal.No_sharing ]

let reject code f =
  try
    ignore (f ());
    failwith ("accepted " ^ code)
  with Diagnostic.Error d -> assert (d.code = code)

let value ?(declarations = "") ?inputs body =
  let source =
    "(workspace w (graph g :context host " ^ declarations ^ " (if (> t -1000.) " ^ body ^ " " ^ body
    ^ ")))"
  in
  let workspace =
    match
      Flow.Workspace.check { Flow.Catalog.version = 1; kinds = [] } (ok (Flow.Syntax.parse source))
    with
    | Some workspace, [] -> workspace
    | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  List.assoc "g" (ok (E.static ?inputs workspace)).results

let () =
  let ints = V.Int_array [| min_int; max_int; 9007199254740993; -9007199254740993 |] in
  assert (O.reduce_array O.Minimum ints = V.Int min_int);
  assert (O.reduce_array O.Maximum ints = V.Int max_int);
  assert (O.reduce_array O.Sum ~offsets:(V.Int_array [| 0; 2; 4 |]) ints = V.Int_array [| -1; 0 |]);
  List.iter
    (fun wrap ->
      let zero = O.reduce_array O.Minimum (wrap [| -0.; 0. |])
      and positive = O.reduce_array O.Maximum (wrap [| -0.; 0. |]) in
      assert (same zero (V.Float (-0.)) && same positive (V.Float 0.)))
    [ (fun xs -> V.Float_array xs) ];
  assert (same (O.reduce_array O.Minimum (V.Vec2_array [| 3.; -0.; 1.; 0. |])) (V.Vec2 (1., -0.)));
  assert (same (O.reduce_array O.Maximum (V.Vec2_array [| 3.; -0.; 1.; 0. |])) (V.Vec2 (3., 0.)));
  assert (
    O.reduce_array O.Sum ~offsets:(V.Int_array [| 0; 0; 0 |]) (V.Int_array [||])
    = V.Int_array [| 0; 0 |]);
  assert (
    same
      (O.reduce_array O.Sum ~offsets:(V.Int_array [| 0; 0; 0 |]) (V.Vec4_array [||]))
      (V.Vec4_array (Array.make 8 0.)));
  List.iter
    (fun operation ->
      reject "E_ARRAY_EMPTY" (fun () -> O.reduce_array operation (V.Float_array [||]));
      reject "E_ARRAY_EMPTY" (fun () ->
          O.reduce_array operation ~offsets:(V.Int_array [| 0; 0; 1 |]) (V.Float_array [| 1. |]));
      assert (
        O.reduce_array operation ~offsets:(V.Int_array [| 0 |]) (V.Float_array [||])
        = V.Float_array [||]))
    [ O.Minimum; Maximum ];
  List.iter
    (fun offsets ->
      reject "E_ARRAY_OFFSETS" (fun () ->
          O.reduce_array O.Sum ~offsets:(V.Int_array offsets) (V.Float_array [| 1.; 2. |])))
    [ [||]; [| 1; 2 |]; [| 0; 1 |]; [| 0; 3; 2 |]; [| 0; -1; 2 |]; [| 0; max_int; 2 |] ];
  reject "E_ARRAY_TYPE" (fun () ->
      O.reduce_array O.Sum ~offsets:(V.Float_array [| 0.; 2. |]) (V.Int_array [| 1; 2 |]));
  reject "E_ARRAY_TYPE" (fun () -> O.reduce_array O.Minimum (V.Text_array [| "a" |]));
  reject "E_NONFINITE" (fun () -> O.reduce_array O.Maximum (V.Float_array [| nan |]));
  reject "E_NONFINITE" (fun () ->
      O.reduce_array O.Sum
        ~offsets:(V.Int_array [| 0; 2 |])
        (V.Float_array [| Float.max_float; Float.max_float |]));
  let count = 32769 in
  let offsets = V.Int_array (Array.init (count + 1) (fun i -> i * 3)) in
  List.iter
    (fun operation ->
      let data =
        V.Vec3_array
          (Array.init (count * 9) (fun i -> [| 1e16; 1.; -1e16; -0.; 0.; 1e-100 |].(i mod 6)))
      in
      let before = Marshal.to_string (data, offsets) [ Marshal.No_sharing ] in
      let reference = O.reduce_array operation ~offsets data in
      List.iter
        (fun domains ->
          let result =
            Parallel.run ~domains (fun () ->
                O.reduce_array operation ~offsets data ~rows:(fun ~start ~finish f ->
                    Parallel.for_ ~chunk_size:97 ~start ~finish f))
          in
          assert (same result reference))
        [ 1; 8 ];
      assert (Marshal.to_string (data, offsets) [ Marshal.No_sharing ] = before))
    [ O.Sum; Minimum; Maximum ];
  List.iter
    (fun body ->
      let value = value body in
      let residual = match value with E.Residual r -> r | _ -> failwith "not live" in
      let core =
        match I.Core_lower.lower ~strict:true residual with
        | Ok p -> p
        | Error form -> failwith (body ^ ": " ^ form)
      in
      let executable = ok (I.Executor.compile ~strict:true value) in
      List.iter
        (fun time ->
          let live = Input.at_time time in
          let reference = ok (E.Host.force_reference value ~live) in
          List.iter
            (fun domains ->
              let direct, compiled =
                Parallel.run ~domains (fun () ->
                    ( ok (Test_program.run ~root:residual core ~live),
                      ok (I.Executor.force ~strict:true executable ~live) ))
              in
              assert (same direct reference && same compiled reference))
            [ 1; 8 ])
        [ -0.; 0.; 0.25; 2. ])
    [
      "(array/min (array/vec3 5001 [t 3 -0.]))";
      "(array/max (array/int 5001 (int/add 9007199254740993 (frame/index))))";
      "(array/segment_sum (map (fn [x] [x t 1.]) (array/range 5001)) (map (fn [x] (int x)) \
       (array/range 5002)))";
      "(array/segment_min (array/float 5001 t) (map (fn [x] (int x)) (array/range 5002)))";
      "(array/segment_max (array/int 5001 (frame/index)) (map (fn [x] (int x)) (array/range 5002)))";
      "(array/segment_argmax (map (fn [(i : int)] (int/mod (int/mul i 5) 7)) (array/range 5001 \
       :type int)) (map (fn [(i : int)] (int/mul i 3)) (array/range 1668 :type int)))";
      "(array/segment_argmax (map (fn [(i : int)] (* t (float (int/mod (int/mul i 5) 7)))) \
       (array/range 5001 :type int)) (map (fn [(i : int)] (int/mul i 3)) (array/range 1668 :type \
       int)))";
      "(let* [minimum (array/min (array/float 5001 t))] (map (fn [x] (+ x minimum)) (array/range \
       11)))";
      "(map (fn [x] (+ x t)) (array/segment_sum (array/float 5001 1.) (map (fn [x] (int x)) \
       (array/range 5002))))";
      "(let* [threshold (int/add (frame/index) 9007199254740993)] (map (fn [x] (int/add x \
       threshold)) (array/range 5001 :type int)))";
      "(let* [bias (array/min (array/float 7 t)) shift (fn [x] (+ x bias))] (map shift \
       (array/range 5001)))";
      "(let* [column (map (fn [x] (+ x t)) (array/range 5001)) minimum (array/min column)\n\
      \         sample {:nested {:column column :bias minimum}} shift (fn [x] (+ x \
       sample.nested.bias))]\n\
      \         (map shift sample.nested.column))";
    ];
  let empty = value "(map (fn [x] (if (> t 0.) (array/min (array/float 0)) 0.)) (array/range 0))" in
  assert (ok (E.Host.force_reference empty ~live:(Input.at_time 1.)) = E.Float_array [||]);
  (match I.Executor.compile ~strict:true empty with
  | Error _ -> ()
  | Ok _ -> failwith "strict compilation accepted an empty body reduction");
  let captured =
    value
      "(let* [minimum (array/min (array/float 7 t))] (map (fn [x] (+ x minimum)) (array/range 11)))"
  in
  let core =
    match captured with
    | E.Residual r -> Result.get_ok (I.Core_lower.lower ~strict:true r)
    | _ -> assert false
  in
  let packed = core.regions.(0) in
  assert (I.Packed.Host.argument_types packed = [| Flow.Ty.Float |]);
  List.iter
    (fun arguments ->
      match I.Packed.Host.bind_arguments packed arguments with
      | Error d -> assert (d.code = "E_CORE_ARGUMENT")
      | Ok _ -> failwith "invalid argument accepted")
    [ [||]; [| E.Int 1 |]; [| E.Float 1.; E.Float 2. |] ];
  (match I.Packed.Host.bind_arguments packed [| E.Float nan |] with
  | Error d -> assert (d.code = "E_NONFINITE")
  | Ok _ -> failwith "nonfinite argument accepted");
  let bound = ok (I.Packed.Host.bind_arguments packed [| E.Float 3. |]) in
  assert (
    ok (I.Packed.force ~strict:true bound ~live:(Input.at_time 1.))
    = E.Float_array (Array.init 11 (fun i -> float i +. 3.)));
  let source =
    value ~declarations:"[(data : (array float) (array/float 0))]"
      ~inputs:[ ("g", [ ("data", E.Source ("fixture/column", Flow.Ty.Array Flow.Ty.Float, [])) ]) ]
      "(let* [minimum (array/min data) sample {:nested {:column data :bias minimum}}\n\
      \      shift (fn [x] (+ x sample.nested.bias))] (map shift sample.nested.column))"
  in
  let compiled = ok (I.Executor.compile ~strict:true source) in
  List.iter
    (fun domain ->
      Parallel.run ~domains:domain (fun () ->
          let data = E.Float_array (Array.init 5001 (fun i -> float (i mod 17) -. 5.)) in
          let before = Marshal.to_string data [ Marshal.No_sharing ] in
          let resolve = function
            | E.Source ("fixture/column", _, []) -> Ok data
            | _ ->
                Error
                  (Diagnostic.error ~code:"E_DATA_SOURCE" "Unexpected reduction fixture source.")
          in
          let live = Input.at_time 0.25 in
          assert (
            same
              (ok (I.Executor.force ~strict:true ~resolve compiled ~live))
              (ok (E.Host.force_reference ~resolve source ~live)));
          assert (before = Marshal.to_string data [ Marshal.No_sharing ])))
    [ 1; 8 ];
  let unsupported = value "(array/min (map (fn [x] (value/rand (+ x t))) (array/range 5001)))" in
  (match I.Executor.compile ~strict:true unsupported with
  | Error d -> assert (d.code = "E_KERNEL_FORM")
  | Ok _ -> failwith "unsupported producer accepted");
  print_endline
    "Numeric reductions: exact extrema/CSR/order, empty and malformed ranges, strict \
     Core/reference and one/eight-domain bytes pass"

let () =
  if Array.exists (( = ) "--bench") Sys.argv then begin
    let count = 1_000_000 and rows = 100_000 in
    let data = Array.init count (fun i -> float (i mod 97) -. 48.) in
    let values = V.Float_array data
    and offsets = V.Int_array (Array.init (rows + 1) (fun i -> i * 10)) in
    let measure name f =
      let samples =
        Array.init 5 (fun _ ->
            Gc.compact ();
            let before = Gc.allocated_bytes () and start = Unix.gettimeofday () in
            ignore (Sys.opaque_identity (f ()));
            (Unix.gettimeofday () -. start, Gc.allocated_bytes () -. before))
      in
      Array.sort (fun (a, _) (b, _) -> Float.compare a b) samples;
      let seconds, bytes = samples.(2) in
      Printf.printf "%s values=%d seconds=%.6f allocated_bytes=%.0f\n%!" name count seconds bytes
    in
    measure "global_min_reference" (fun () -> Array.fold_left Float.min data.(0) data);
    measure "global_min_intrinsic" (fun () -> O.reduce_array O.Minimum values);
    measure "segment_sum_reference" (fun () ->
        Array.init rows (fun row ->
            let sum = ref 0. in
            for i = row * 10 to (row * 10) + 9 do
              sum := !sum +. data.(i)
            done;
            !sum));
    List.iter
      (fun domains ->
        Parallel.run ~domains (fun () ->
            measure (Printf.sprintf "segment_sum_intrinsic_domains%d" domains) (fun () ->
                O.reduce_array O.Sum ~offsets values ~rows:(fun ~start ~finish f ->
                    Parallel.for_ ~start ~finish f))))
      [ 1; 8 ]
  end
