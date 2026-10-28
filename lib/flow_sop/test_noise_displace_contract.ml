open Rdk
open Rdk_test_support
module N = Sop.Node
open Test_support

let ok_string = function Ok value -> value | Error error -> failwith error

let geometry () =
  Rdk.Geometry.generate_point_ranges 129 (fun ~first ~last ~x ~y ~z ->
      for i = first to last - 1 do
        x.(i) <- (float i *. 0.13) -. 0.4;
        y.(i) <- sin (float i *. 0.27);
        z.(i) <- (float (i / 65) *. 0.19) -. 0.3
      done)

let node input keywords =
  let factory =
    Sop.Edit_graph.factory ~key:"contract_input" ~label:"Contract input" ~category:[ "Test" ]
      ~arity:0 (fun _ -> Ok input)
  in
  let factories =
    factory
    :: List.filter
         (fun factory -> Sop.Edit_graph.factory_key factory <> "noise_displace")
         Sop.Nodes.factories
  in
  let catalog =
    Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |> ok
  in
  let forms =
    Printf.sprintf
      "(workspace noise-contract (graph g :context sop (sop/noise_displace (sop/contract_input) \
       %s)))"
      keywords
    |> Flow.Syntax.parse |> ok
  in
  let sources = Flow_sop.Catalog.sources () |> ok in
  let workspace =
    match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog forms with
    | Some workspace, diagnostics
      when not
             (List.exists
                (fun (diagnostic : Diagnostic.t) -> diagnostic.severity = Diagnostic.Error_severity)
                diagnostics) ->
        workspace
    | _, diagnostics -> failwith (String.concat "\n" (List.map Diagnostic.to_string diagnostics))
  in
  let lowered = Flow_sop.Lower.of_checked ~factories workspace |> ok in
  let graph = List.hd lowered.graphs in
  Sop.Edit_graph.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> ok_string

let cook session context node =
  match Sop.Session.cook session ~context node with
  | Error error -> failwith (Diagnostic.error_to_string error)
  | Ok output -> Sop.Payload.geometry output.payload |> ok

let context ?(domains = 1) seed = Sop.Context.create ~seed ~domains ~grain:97 () |> ok_string

let capture_once ~max_entries context node =
  let session = Sop.Session.create ~max_entries ~max_payload_bytes:1_000_000 |> ok_string in
  Fun.protect ~finally:(fun () -> Sop.Session.close session) (fun () -> cook session context node)

let session_benchmark node expected =
  let run_fresh ~max_entries context =
    Gc.full_major ();
    let before = Gc.allocated_bytes () and started = Unix.gettimeofday () in
    for index = 1 to 200 do
      let actual = capture_once ~max_entries context node in
      if index = 1 then assert (geometry_bytes actual = expected)
    done;
    let elapsed = Unix.gettimeofday () -. started in
    (elapsed, Gc.allocated_bytes () -. before)
  in
  List.iter
    (fun domains ->
      let context = context ~domains 42L in
      let cached = run_fresh ~max_entries:64 context
      and uncached = run_fresh ~max_entries:0 context in
      Printf.printf
        "Capture session benchmark (200 cooks, 4225 points, %d domains): cache64 %.3fs/%.0f bytes; \
         cache0 %.3fs/%.0f bytes\n\
         %!"
        domains (fst cached) (snd cached) (fst uncached) (snd uncached))
    [ 1; 4 ]

let () =
  let input = Lisp_sop.snapshot (geometry ()) in
  let plain = node input "" in
  let seeded =
    N.apply_parameters plain [ ("context_seed", Param.Bool_value true) ] |> ok_string |> fst
  in
  let session = Sop.Session.create ~max_entries:16 ~max_payload_bytes:1_000_000 |> ok_string in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      let c42 = context 42L and c999 = context 999L in
      let first = cook session c42 plain in
      assert (cook session c42 plain == first);
      assert (geometry_bytes (capture_once ~max_entries:0 c42 plain) = geometry_bytes first);
      let c42_four = context ~domains:4 42L in
      assert (
        geometry_bytes (capture_once ~max_entries:0 c42_four seeded)
        = geometry_bytes (capture_once ~max_entries:16 c42_four seeded));
      if Sys.getenv_opt "RAYS_CAPTURE_SESSION_BENCH" <> None then begin
        let benchmark_input =
          Rdk.Geometry.generate_point_ranges 4225 (fun ~first ~last ~x ~y ~z ->
              for i = first to last - 1 do
                x.(i) <- (float i *. 0.13) -. 0.4;
                y.(i) <- sin (float i *. 0.27);
                z.(i) <- (float (i / 65) *. 0.19) -. 0.3
              done)
          |> Lisp_sop.snapshot
        in
        let benchmark_node = node benchmark_input "" in
        let expected = capture_once ~max_entries:0 c42 benchmark_node |> geometry_bytes in
        session_benchmark benchmark_node expected
      end;
      let misses = (Sop.Session.stats session).misses in
      assert (cook session c999 plain == first);
      assert ((Sop.Session.stats session).misses = misses);

      let seeded42 = cook session c42 seeded in
      let misses = (Sop.Session.stats session).misses in
      let seeded999 = cook session c999 seeded in
      assert ((Sop.Session.stats session).misses = misses + 1);
      assert (geometry_bytes seeded42 <> geometry_bytes seeded999);

      let relabeled = N.relabel "déplacement 雪" seeded in
      let restored = N.relabel (N.label seeded) relabeled in
      assert (N.id relabeled = N.id seeded);
      List.iter
        (fun (context, expected) ->
          let misses = (Sop.Session.stats session).misses in
          assert (cook session context relabeled == expected);
          assert (cook session context restored == expected);
          assert ((Sop.Session.stats session).misses = misses))
        [ (c42, seeded42); (c999, seeded999) ]);
  print_endline "Lisp noise_displace SOP cache, context-seed dependency and relabel identity pass"
