let ok = function Ok value -> value | Error message -> failwith message

let parse text =
  match Flow.Syntax.parse text with
  | Ok forms -> forms
  | Error d -> failwith (Diagnostic.to_string d)

let lower ?(reference = false) text =
  match Flow_sop.Lower.workspace ~reference (parse text) with
  | Error d -> failwith (Diagnostic.to_string d)
  | Ok lowered ->
      let graph = List.find (fun (graph : Flow_sop.Lower.graph) -> graph.default) lowered.graphs in
      ok (Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root))

let with_session fn =
  let session = ok (Sop.Session.create ~max_entries:64 ~max_payload_bytes:(64 * 1024 * 1024)) in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () -> fn session)

let cook session ?(domains = 1) node =
  match Sop.Session.cook session ~context:(ok (Sop.Context.create ~domains ~grain:257 ())) node with
  | Ok output -> (
      match Sop.Payload.geometry output.payload with
      | Ok geometry -> geometry
      | Error d -> failwith (Diagnostic.error_to_string d))
  | Error d -> failwith (Diagnostic.error_to_string d)

let check = Test_support.check
let points = 40

let template =
  {|(workspace iterate
  (defnode sop/smooth_loop [(geo : geometry) (passes : int 1 :min 0 :max 100000000)]
    (let* [n (sop/count geo :points)
           lo (map (fn [(i : int)] (int/mod (int/add i (int/sub n 1)) n)) (array/range n :type int))
           hi (map (fn [(i : int)] (int/mod (int/add i 1) n)) (array/range n :type int))]
      (sop/with_attr geo :P (iterate passes (sop/attr geo :P)
        (fn [p] (exact (map (fn [(i : int)] (* 0.5 (+ (array/nth p (array/nth lo i)) (array/nth p (array/nth hi i)))))
          (array/range n :type int))))))))
  (defnode sop/relax_loop [(geo : geometry) (passes : int 1 :min 0 :max 100000000)]
    (let* [n (sop/count geo :points)
           lo (map (fn [(i : int)] (int/mod (int/add i (int/sub n 1)) n)) (array/range n :type int))
           hi (map (fn [(i : int)] (int/mod (int/add i 1) n)) (array/range n :type int))
           spike (exact (map (fn [(i : int)] (if (int/eq i 0) 1.0 0.0)) (array/range n :type int)))]
      (sop/with_attr geo :w :type float (iterate passes spike
        (fn [w] (exact (map (fn [(i : int)] (/ (+ (+ (array/nth w (array/nth lo i)) (array/nth w i)) (array/nth w (array/nth hi i))) 3.0))
          (array/range n :type int))))
        :until (fn [(prev : (array float)) (next : (array float))] (< (array/max (exact (map (fn [a b] (abs (- a b))) prev next))) 0.01))))))
  (graph g :context sop (%s (sop/line :points %d) :passes %d)))|}

let text ?(n = points) kind passes =
  Printf.sprintf (Scanf.format_from_string template "%s%d%d") kind n passes

let neighbours n = Array.init n (fun i -> ((i + n - 1) mod n, (i + 1) mod n))

let relax_step (lo_hi : (int * int) array) a =
  Array.mapi
    (fun i x ->
      let lo, hi = lo_hi.(i) in
      (a.(lo) +. x +. a.(hi)) /. 3.)
    a

let smooth_step (lo_hi : (int * int) array) a =
  Array.mapi
    (fun i _ ->
      let lo, hi = lo_hi.(i) in
      0.5 *. (a.(lo) +. a.(hi)))
    a

let line_positions geometry =
  let view = Rdk.Packed.Float3.View.view (Rdk.Geometry.positions geometry) in
  (view.x, view.y, view.z)

let () =
  let base = with_session (fun s -> cook s (lower (text "sop/smooth_loop" 0))) in
  let x0, y0, z0 = line_positions base in
  let lo_hi = neighbours points in
  let expected passes =
    let rec go k (x, y, z) =
      if k = 0 then (x, y, z)
      else go (k - 1) (smooth_step lo_hi x, smooth_step lo_hi y, smooth_step lo_hi z)
    in
    let x, y, z = go passes (x0, y0, z0) in
    Rdk_test_support.geometry_bytes
      (ok (Rdk.Geometry.with_positions (Rdk.Packed.Float3.View.of_owned_exn ~x ~y ~z) base))
  in
  List.iter
    (fun passes ->
      let want = expected passes in
      List.iter
        (fun reference ->
          List.iter
            (fun domains ->
              let actual =
                with_session (fun s ->
                    cook s ~domains (lower ~reference (text "sop/smooth_loop" passes)))
              in
              check
                (Rdk_test_support.geometry_bytes actual = want)
                (Printf.sprintf
                   "iterate passes=%d reference=%b domains=%d differs from the OCaml smoothing"
                   passes reference domains))
            [ 1; 8 ])
        [ false; true ])
    [ 0; 1; 7; 500 ];
  (* the pass count is data, not graph structure: the plan has the same nodes for 1 and 5000 passes *)
  let plan_nodes passes =
    match Flow_sop.Lower.workspace ~reference:false (parse (text "sop/smooth_loop" passes)) with
    | Ok lowered -> Array.length lowered.plan.nodes
    | Error d -> failwith (Diagnostic.to_string d)
  in
  check (plan_nodes 1 = plan_nodes 5000) "the pass count changed the number of cook nodes";
  (* a negative count returns the seed *)
  check
    (Rdk_test_support.geometry_bytes
       (with_session (fun s -> cook s (lower (text "sop/smooth_loop" (-3)))))
    = expected 0)
    "a negative count changed the seed";
  (* a parameter change reruns the same prepared node *)
  let node = lower (text "sop/smooth_loop" 1) in
  with_session (fun s ->
      check (Rdk_test_support.geometry_bytes (cook s node) = expected 1) "one pass";
      let changed, _ = ok (Sop.Node.apply_parameters node [ ("passes", Param.Int_value 7) ]) in
      check
        (Rdk_test_support.geometry_bytes (cook s changed) = expected 7)
        "parameter change to seven passes");
  print_endline
    "iterate: 0, 1, 7 and 500 passes in one cook node match OCaml, reference and one/eight domains"

let () =
  let lo_hi = neighbours points in
  let w0 = Array.init points (fun i -> if i = 0 then 1. else 0.) in
  let stop_after =
    let rec go k w =
      let next = relax_step lo_hi w in
      let delta =
        Array.fold_left max neg_infinity (Array.mapi (fun i a -> Float.abs (w.(i) -. a)) next)
      in
      if delta < 0.01 then (k + 1, next) else go (k + 1) next
    in
    go 0 w0
  in
  let weights geometry =
    match
      Rdk.Attribute.storage
        (Option.get (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "w" geometry))
    with
    | Rdk.Attribute.Float w -> w
    | _ -> failwith "w is not a float attribute"
  in
  let passes_until, want = stop_after in
  check (passes_until > 5 && passes_until < 500) "the :until fixture must stop mid-run";
  List.iter
    (fun reference ->
      List.iter
        (fun domains ->
          List.iter
            (fun passes ->
              let w =
                weights
                  (with_session (fun s ->
                       cook s ~domains (lower ~reference (text "sop/relax_loop" passes))))
              in
              let target =
                if passes < passes_until then
                  let rec go k w = if k = 0 then w else go (k - 1) (relax_step lo_hi w) in
                  go passes w0
                else want
              in
              check
                (Array.for_all2
                   (fun a b -> Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b))
                   w target)
                (Printf.sprintf ":until passes=%d reference=%b domains=%d did not stop at pass %d"
                   passes reference domains passes_until))
            [ 1; passes_until - 1; passes_until; passes_until + 3; 500 ])
        [ 1; 8 ])
    [ false; true ];
  print_endline "iterate :until stops after the pass that satisfies it"
