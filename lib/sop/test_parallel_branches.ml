open Sop
open Rdk_test_support

let ok = function Ok value -> value | Error _ -> fail "unexpected error"
let context domains = Context.create ~domains () |> ok

let session ?(entries = 64) () =
  Session.create ~max_entries:entries ~max_payload_bytes:(64 * 1024 * 1024) |> ok

let cook session domains node =
  match Session.cook session ~context:(context domains) node with
  | Ok output -> output
  | Error error -> fail (Diagnostic.error_to_string error)

let node ?(inputs = [||]) ?(run = fun _ -> ()) label geometry =
  Node.Build.make_geometry ~label ~operation:"branch_test" ~version:1 ~parameters:label
    ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static ~inputs
    (fun ~node_id:_ context _ ->
      run context;
      Ok Node.Build.{ geometry; instances = None })

let counters session =
  let s = Session.stats session in
  ( s.cooks,
    s.hits,
    s.misses,
    s.evictions,
    s.volatile_hits,
    s.volatile_misses,
    s.retained_entries,
    s.volatile_entries,
    s.retained_payload_bytes )

let exact ?(entries = 64) ?(prepare = fun _ -> ()) graph =
  let one = session ~entries () and eight = session ~entries () in
  Fun.protect
    ~finally:(fun () ->
      Session.close one;
      Session.close eight)
    (fun () ->
      prepare one;
      prepare eight;
      Session.set_parallel_override one (Some true);
      Session.set_parallel_override eight (Some true);
      let a = cook one 1 graph and b = cook eight 8 graph in
      check
        (geometry_bytes (Result.get_ok (Sop.Payload.geometry a.payload))
        = geometry_bytes (Result.get_ok (Sop.Payload.geometry b.payload)))
        "branch geometry changed";
      check (counters one = counters eight) "input-order cache counters changed";
      check
        (Session.Private.cache_keys one = Session.Private.cache_keys eight)
        "cache identities changed for shared immutable output planes";
      (a, b, Session.Private.fanouts eight))

let run () =
  let seed = session () in
  let geometry =
    Result.get_ok
      (Sop.Payload.geometry
         (cook seed 1
            (Lisp_sop.snapshot (Rdk.Line_geometry.points [| (0., 0., 0.); (1., 0., 0.) |])))
           .payload)
  in
  Session.close seed;
  let source_a = node "source_a" geometry and source_b = node "source_b" geometry in
  let a = node ~inputs:[| source_a |] "a" geometry
  and b = node ~inputs:[| source_b |] "b" geometry in
  let root = node ~inputs:[| a; b |] "join" geometry in
  let _, _, fanouts = exact root in
  check (fanouts = 1) "two independent branches were not forked";
  let shared_calls = Atomic.make 0 in
  let shared =
    node ~run:(fun _ -> ignore (Atomic.fetch_and_add shared_calls 1)) "shared" geometry
  in
  let left = node ~inputs:[| shared |] "left" geometry
  and right = node ~inputs:[| shared |] "right" geometry in
  let _, _, fanouts = exact (node ~inputs:[| left; right |] "shared_join" geometry) in
  check
    (Atomic.get shared_calls = 2)
    "an uncached shared ancestor cooked more than once per session";
  check (fanouts = 1) "shared prefix prevented independent suffix work";
  let preceding = node "preceding" geometry in
  ignore
    (exact ~entries:2
       (node
          ~inputs:[| node ~inputs:[| preceding; shared |] "prefix_left" geometry; right |]
          "prefix_order_join" geometry));

  (* A later worker initially hits old_a; the preceding insertion evicts it.
     Replaying the stale hit would change counters and CLOCK state. *)
  let old_a = node "old_a" geometry and old_b = node "old_b" geometry in
  let new_a = node "new_a" geometry in
  let later = node ~inputs:[| old_a |] "later" geometry in
  let prepare session =
    ignore (cook session 1 old_a);
    ignore (cook session 1 old_b)
  in
  let _, _, fanouts =
    exact ~entries:2 ~prepare (node ~inputs:[| new_a; later |] "eviction_join" geometry)
  in
  check (fanouts = 1) "eviction regression did not exercise a parallel transaction";
  ignore (exact ~entries:0 root);
  Atomic.set shared_calls 0;
  ignore (exact ~entries:0 (node ~inputs:[| left; right |] "zero_shared_join" geometry));
  check (Atomic.get shared_calls = 2) "zero-retention shared prefix cooked more than once";
  ignore (exact ~entries:0 ~prepare:(fun s -> Session.set_volatile s (fun _ -> true)) root);
  ignore (exact ~prepare:(fun s -> Session.set_volatile s (fun _ -> true)) root);

  let cached = session () in
  Session.set_parallel_override cached (Some true);
  ignore (cook cached 8 root);
  let before = Session.Private.fanouts cached in
  ignore (cook cached 8 root);
  check (Session.Private.fanouts cached = before) "warm cache hits were forked";
  Session.close cached;
  let reused = Node.Build.restore_id (Node.id a) b |> ok in
  ignore (exact (node ~inputs:[| a; reused |] "reused_id_join" geometry));
  ignore (exact ~entries:0 (node ~inputs:[| a; reused |] "zero_reused_id_join" geometry));

  let learned = session () in
  let slow label = node ~run:(fun _ -> Unix.sleepf 0.004) label geometry in
  let suffix label = node ~inputs:[| slow (label ^ "_source") |] label geometry in
  let costly_graph = node ~inputs:[| suffix "cost_a"; suffix "cost_b" |] "cost_join" geometry in
  ignore (cook learned 8 costly_graph);
  check (Session.Private.fanouts learned = 0) "cold unknown costs bypassed the automatic gate";
  Session.Private.clear_cache_keep_timings learned;
  ignore (cook learned 8 costly_graph);
  check (Session.Private.fanouts learned = 1) "cheap final operators hid measured heavy subtrees";
  Session.close learned;

  let with_color value =
    let color =
      Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"Cd"
        (Rdk.Attribute.Float (Array.make (Rdk.Geometry.point_count geometry) value))
      |> ok
    in
    Rdk.Geometry.with_attribute color geometry |> ok
  in
  let source = Lisp_sop.snapshot (with_color 1.) in
  let a =
    Lisp_sop.node ~with_:[ ("source", source) ] {|(sop/noise_displace (sop/ext_source) :seed 1)|}
  and b =
    Lisp_sop.node ~with_:[ ("source", source) ] {|(sop/noise_displace (sop/ext_source) :seed 2)|}
  in
  let refreshing = session () in
  Session.set_parallel_override refreshing (Some true);
  ignore
    (cook refreshing 8
       (Lisp_sop.node ~with_:[ ("a", a); ("b", b) ] {|(sop/merge (sop/ext_a) (sop/ext_b))|}));
  let source = Lisp_sop.snapshot (with_color 2.) in
  ignore (cook refreshing 8 source);
  let a = Node.Build.rebuild_with_inputs a [| source |]
  and b = Node.Build.rebuild_with_inputs b [| source |] in
  let before = Session.Private.fanouts refreshing in
  ignore
    (cook refreshing 8
       (Lisp_sop.node ~with_:[ ("a", a); ("b", b) ] {|(sop/merge (sop/ext_a) (sop/ext_b))|}));
  check
    (Session.Private.fanouts refreshing = before)
    "component cache refreshes were forked as cold cooks";
  Session.close refreshing;

  (* This barrier fails if the first branch runs synchronously before the
     remaining branches are scheduled (Parallel.map_array's first element). *)
  let ready = Atomic.make 0 in
  let rendezvous _ =
    ignore (Atomic.fetch_and_add ready 1);
    let deadline = Unix.gettimeofday () +. 5. in
    while Atomic.get ready < 2 && Unix.gettimeofday () < deadline do
      Domain.cpu_relax ()
    done;
    check (Atomic.get ready = 2) "two coarse branches did not execute concurrently"
  in
  let parallel = session () in
  Session.set_parallel_override parallel (Some true);
  ignore
    (cook parallel 8
       (node
          ~inputs:
            [|
              node ~run:rendezvous "barrier_a" geometry; node ~run:rendezvous "barrier_b" geometry;
            |]
          "barrier_join" geometry));
  Session.close parallel;

  let broken label =
    Node.Build.make_geometry ~label ~operation:"broken" ~version:1 ~parameters:label
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _ -> Error (Diagnostic.error ~code:label label))
  in
  let errors = node ~inputs:[| broken "first"; broken "second" |] "errors" geometry in
  let one = session () and eight = session () in
  Session.set_parallel_override eight (Some true);
  let first s domains =
    match Session.cook s ~context:(context domains) errors with
    | Error error -> error
    | Ok _ -> fail "broken branches succeeded"
  in
  check (first one 1 = first eight 8) "first input error/trace changed";
  check (counters one = counters eight) "later failed branch changed parent counters";
  Session.close one;
  Session.close eight;

  let cancel = Context.Cancel.create () in
  let cancelled = node ~run:(fun _ -> Context.Cancel.cancel cancel) "cancel_now" geometry in
  let cancelled_graph =
    node ~inputs:[| cancelled; node "other" geometry |] "cancel_join" geometry
  in
  let cancelled_session = session () in
  Session.set_parallel_override cancelled_session (Some true);
  let cancelled_context = Context.create ~domains:8 ~cancel () |> ok in
  (match Session.cook cancelled_session ~context:cancelled_context cancelled_graph with
  | Error error -> check (error.code = "cancelled") "cancellation lost its typed diagnostic"
  | Ok _ -> fail "cancelled branch succeeded");
  check
    (Session.Private.cache_keys cancelled_session = [])
    "cancelled fanout inserted cache entries";
  Session.close cancelled_session;

  let cancel = Context.Cancel.create () and entered = Atomic.make false in
  let first =
    Node.Build.make_geometry ~operation:"first_error" ~version:1 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _ ->
        Atomic.set entered true;
        Error (Diagnostic.error ~code:"first_error" "first error"))
  in
  let cancel_after_first _ =
    let deadline = Unix.gettimeofday () +. 5. in
    while (not (Atomic.get entered)) && Unix.gettimeofday () < deadline do
      Domain.cpu_relax ()
    done;
    check (Atomic.get entered) "first error branch was not scheduled";
    Context.Cancel.cancel cancel
  in
  let cancelled_session = session () in
  Session.set_parallel_override cancelled_session (Some true);
  let graph =
    node
      ~inputs:[| first; node ~run:cancel_after_first "later_cancel" geometry |]
      "first_error_join" geometry
  in
  (match
     Session.cook cancelled_session ~context:(Context.create ~domains:8 ~cancel () |> ok) graph
   with
  | Error error ->
      check (error.code = "first_error") "later cancellation hid the earlier branch error"
  | Ok _ -> fail "error/cancellation succeeded");
  Session.close cancelled_session;

  (* Real allocating RDK kernels: byte identity, independently of runtime data
     IDs (fresh allocations intentionally get different cache identities). *)
  let chain seed =
    Lisp_sop.node
      (Printf.sprintf
         {|(-> (sop/grid :columns 39 :rows 39)
     (sop/noise_displace :seed %d :frequency 0.2)
     (sop/noise_displace :seed %d :frequency 0.3))|}
         seed (seed + 1))
  in
  let real =
    Lisp_sop.node
      ~with_:[ ("in1796", chain 1); ("in1797", chain 3) ]
      {|(sop/merge (sop/ext_in1796) (sop/ext_in1797))|}
  in
  let one = session () and eight = session () in
  Session.set_parallel_override eight (Some true);
  check
    (geometry_bytes (Result.get_ok (Sop.Payload.geometry (cook one 1 real).payload))
    = geometry_bytes (Result.get_ok (Sop.Payload.geometry (cook eight 8 real).payload)))
    "allocating two-chain geometry differs at eight domains";
  check (counters one = counters eight) "two-chain cache accounting differs";
  Session.close one;
  Session.close eight;

  let packed =
    Node.Build.make_geometry ~operation:"packed_source" ~version:1 ~parameters:""
      ~cook_mode:Node.Generator ~dependencies:Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _ ->
        Ok
          Node.Build.
            {
              geometry;
              instances =
                Some
                  [|
                    Rays_math.Mat4.identity;
                    Rays_math.Mat4.translation (Rays_math.Vec3.create 2. 0. 0.);
                  |];
            })
  in
  let consume label =
    Node.Build.make_geometry ~label ~operation:"consume_packed" ~version:1 ~parameters:label
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static ~inputs:[| packed |]
      (fun ~node_id:_ _ inputs -> Ok Node.Build.{ geometry = inputs.(0); instances = None })
  in
  let graph =
    Lisp_sop.node
      ~with_:[ ("in1798", consume "packed_a"); ("in1799", consume "packed_b") ]
      {|(sop/merge (sop/ext_in1798) (sop/ext_in1799))|}
  in
  let one = session () and eight = session () in
  Session.set_parallel_override eight (Some true);
  check
    (geometry_bytes (Result.get_ok (Sop.Payload.geometry (cook one 1 graph).payload))
    = geometry_bytes (Result.get_ok (Sop.Payload.geometry (cook eight 8 graph).payload)))
    "shared packed materialization changed branch bytes";
  check (counters one = counters eight) "materialization replay changed cache accounting";
  Session.close one;
  Session.close eight;
  ignore (exact ~entries:0 graph);

  let cancel = Context.Cancel.create () in
  let cancelled = node ~run:(fun _ -> Context.Cancel.cancel cancel) "zero_cancel" geometry in
  let uncached = session ~entries:0 () in
  (match Session.cook uncached ~context:(Context.create ~domains:8 ~cancel () |> ok) cancelled with
  | Error error -> check (error.code = "cancelled") "zero-retention cook lost cancellation"
  | Ok _ -> fail "zero-retention cancelled cook succeeded");
  check (Session.Private.cache_keys uncached = []) "zero-retention cancellation retained a key";
  Session.close uncached;

  let common = Lisp_sop.node {|(sop/grid :columns 9 :rows 9)|} in
  let zone =
    Zone.node ~kind:Zone.Points ~source_attribute:"source" ~source_base:0
      ~inputs:
        [|
          Lisp_sop.snapshot
            (Rdk.Line_geometry.points [| (0., 0., 0.); (1., 0., 0.); (2., 0., 0.); (3., 0., 0.) |]);
          common;
        |]
      ~body:(fun ~inputs ~context:_ element ->
        Lisp_sop.node
          ~with_:[ ("in1801", inputs.(1)) ]
          (Printf.sprintf {|(sop/noise_displace (sop/ext_in1801) :seed %d)|} element.index))
      ()
  in
  let one = session () and eight = session () in
  Session.set_parallel_override eight (Some true);
  check
    (geometry_bytes (Result.get_ok (Sop.Payload.geometry (cook one 1 zone).payload))
    = geometry_bytes (Result.get_ok (Sop.Payload.geometry (cook eight 8 zone).payload)))
    "zone geometry differs at eight domains";
  check (counters one = counters eight) "zone cache accounting differs";
  check (Session.Private.fanouts eight > 0) "expanded zone roots were not forked";
  Session.close one;
  Session.close eight;
  print_endline "parallel branch transactions: exact bytes, keys, counters and cancellation"

let () = run ()
