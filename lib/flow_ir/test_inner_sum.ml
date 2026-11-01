module E = Flow.Eval
module P = Flow_ir.Packed

let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)
let bytes value = Marshal.to_string value [ Marshal.No_sharing ]

let checked body =
  let forms = ok (Flow.Syntax.parse ("(workspace w (graph g :context host " ^ body ^ "))")) in
  match Flow.Workspace.check Flow.Catalog.{ version = 1; kinds = [] } forms with
  | Some workspace, [] -> workspace
  | _, diagnostics -> failwith (String.concat "; " (List.map Diagnostic.to_string diagnostics))

let mapped ?(data = "(array/range 80 :type int)") ?(bindings = "") ?(result = Flow.Ty.Int) body
    indices =
  let workspace =
    checked ("(let* [data " ^ data ^ " " ^ bindings ^ " tested (fn [(i : int)] " ^ body ^ ")] 0)")
  in
  match
    snd (List.hd (List.assoc [ "g"; "tested" ] (ok (E.static ~record:true workspace)).records))
  with
  | E.Fn fn ->
      ok
        (E.Host.map_function
           ~signature:Flow.Ty.{ params = [ Int ]; result }
           fn [ E.Int_array indices ])
  | _ -> failwith "expected a function"

let compile = function
  | E.Residual residual -> P.compile_result residual (E.Residual.residual_view residual).term
  | _ -> failwith "expected a residual"

let parity value =
  let reference = E.Host.force_reference value ~live:(Input.at_time 0.) in
  let packed =
    match compile value with
    | Ok packed -> packed
    | Error ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
  in
  List.iter
    (fun domains ->
      let result =
        Parallel.run ~domains (fun () -> P.force ~strict:true packed ~live:(Input.at_time 0.))
      in
      match (reference, result) with
      | Ok a, Ok b -> assert (bytes a = bytes b)
      | Error a, Error b ->
          assert (a.Diagnostic.code = b.code && a.message = b.message && a.cause = b.cause)
      | _ -> failwith "inner sum differs from the walker")
    [ 1; 8 ]

let () =
  List.iter
    (fun count ->
      parity
        (mapped
           (Printf.sprintf
              "(array/sum (map (fn [(j : int)] (int/add (int/mul i 3) j)) (array/range %d :type \
               int)))"
              count)
           [| 0; 1; max_int |]);
      parity
        (mapped (Printf.sprintf "(array/sum (array/slice data i %d))" count) [| 0; 1; 80 - count |]))
    [ 0; 1; 64 ];
  parity (mapped ~data:"(array/int 0)" "(array/sum (array/slice data i 0))" [| 0 |]);
  parity (mapped ~data:"(array/int 0)" "(array/sum (array/slice data i 0))" [| 1 |]);
  parity
    (mapped
       ~bindings:
         "add-slice (fn [(table : (array int)) (start : int)] (array/sum (array/slice table start \
          3)))"
       "(add-slice data i)"
       (Array.init 32769 (fun i -> i mod 77)));
  List.iter
    (fun (ty, values) ->
      List.iter
        (fun count ->
          parity
            (mapped ~result:ty
               ~data:("(map (fn [j] " ^ values ^ ") (array/range 80))")
               (Printf.sprintf "(array/sum (array/slice data i %d))" count)
               [| 0; 1; 80 - count |]);
          parity
            (mapped ~result:ty
               (Printf.sprintf "(array/sum (map (fn [j] %s) (array/range %d)))" values count)
               [| 0; 1; 2 |]))
        [ 0; 1; 64 ])
    [
      (Flow.Ty.Float, "-0.0");
      (Vec2, "[-0.0 5e-324]");
      (Vec3, "[-0.0 5e-324 -0.25]");
      (Vec4, "[-0.0 5e-324 -0.25 0.75]");
    ];
  List.iter
    (fun count ->
      List.iter
        (fun first ->
          parity (mapped (Printf.sprintf "(array/sum (array/slice data i %d))" count) [| first |]))
        [ -1; 81; max_int ])
    [ 0; 1; 64 ];
  parity
    (mapped
       "(if (< i 2) (array/sum (array/slice data i 3))\n\
       \          (array/sum (array/slice data 100 0)))"
       [| 0; 1 |]);
  parity
    (mapped ~result:Flow.Ty.Float
       "(array/sum (map (fn [(j : int)]\n\
       \          (if (= j 2) (error \"E_LATE_MAP\" \"map before sum\") 1e308))\n\
       \          (array/range 3 :type int)))"
       [| 0 |]);
  List.iter
    (fun body ->
      match compile (mapped body [| 1 |]) with
      | Error ds -> assert (List.exists (fun (d : Diagnostic.t) -> d.code = "E_PACKED_CONSTANT") ds)
      | Ok _ -> failwith "dynamic or oversized inner count compiled")
    [
      "(array/sum (map (fn [j] j) (array/range 65 :type int)))";
      "(array/sum (array/slice data i 65))";
      "(array/sum (map (fn [j] j) (array/range i :type int)))";
      "(array/sum (array/slice data 0 i))";
    ];
  let workspace =
    checked
      "(map (fn [(i : int)]\n\
      \         (array/sum (map (fn [(j : int)] (+ i j)) (array/range 3 :type int))))\n\
      \         (array/range 4101 :type int))"
  in
  let profile = Flow_ir.Profile.create ~clock:(fun () -> 0.) in
  let fast =
    ok (Flow_ir.Profile.sample profile (fun () -> Flow_ir.static ~verify:true workspace))
  in
  assert (bytes fast.E.results = bytes (ok (E.static workspace)).results);
  assert (
    List.exists
      (fun (map : Flow_ir.Profile.static_map) ->
        map.count = 4101 && map.head_elements = 0 && map.tail_elements = 0)
      (Flow_ir.Profile.static_maps profile));
  List.iter
    (fun count ->
      let workspace =
        checked
          (Printf.sprintf
             "(let* [data (array/int 80)] (map (fn [(i : int)]\n\
             \               (if (= i 4098) (array/sum (array/slice data 81 %d)) 0))\n\
             \               (array/range 4101 :type int)))"
             count)
      in
      match (E.static workspace, Flow_ir.static workspace) with
      | Error reference, Error actual -> assert (reference = actual)
      | _ -> failwith "expected the original late slice diagnostic")
    [ 0; 3 ];
  print_endline
    "Inner sums: K=0/1/64, exact Int/vector bits, helper tables, guarded empty/bounds errors, \
     eager map errors, constant refusals and one/eight domains agree"
