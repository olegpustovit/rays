(* W8: loops over geometry.  A Garden-like graph (scatter, then a for over its
   points) cooks deterministically at 1 and 3 domains; moving one point recooks
   only its element; pieces, keys and provenance. *)
open Flow_sop
module Edit = Sop.Edit_graph
module Session = Sop.Session

let fail message = failwith ("test_workspace_zone: " ^ message)
let check condition message = if not condition then fail message
let geometry_bytes = Test_workspace_cook.geometry_bytes
let garden ?(result = "(sop/merge bed dots)") ~count () = Printf.sprintf {|
(workspace garden
  (graph garden :context sop [(seed : int 1) (count : int %d)]
    (let* [bed (sop/tube :top_radius 1 :bottom_radius 1 :height 0.04 :columns 12)
           spots (sop/scatter bed :count count :seed seed)
           dots (for [p (sop/point_list spots)]
                  (let* [ball (sop/uv_sphere :radius 0.06 :segments 8 :rings 4)
                         moved (sop/transform ball :translate p)]
                    moved))
           result %s]
      result)))|} count result

let lower source = match Flow.Syntax.parse source with
  | Error d -> fail (Diagnostic.to_string d)
  | Ok forms -> (match Lower.workspace ~extra:Editor_document.Kinds.descriptors forms with
      | Ok lowered -> lowered
      | Error d -> fail (Diagnostic.to_string d))

let context ~domains = Result.get_ok
  (Sop.Context.create ~domains ~grain:97 ~seed:42L ())
let session ?(entries = 512) () =
  Result.get_ok (Session.create ~max_entries:entries ~max_payload_bytes:(256 * 1024 * 1024))

let cook ?(domains = 1) session graph =
  let root = Option.get (graph : Lower.graph).root in
  let compiled = Result.get_ok (Edit.compile_node graph.network.geometry ~node_id:root) in
  match Session.cook session ~context:(context ~domains) compiled with
  | Ok output -> (Result.get_ok (Sop.Payload.geometry output.payload))
  | Error error -> fail (Diagnostic.error_to_string error)

let prims = Rdk.Geometry.primitive_count

let previews () =
  let verify source path iter choices expected_x expected_prims =
    let lowered = lower source in
    let graph = List.hd lowered.graphs in
    let target = Array.find_opt (fun (node : Flow.Eval.node) -> node.site = path && node.iter = iter) lowered.plan.nodes
      |> Option.get in
    let s = session () in
    Fun.protect ~finally:(fun () -> Session.close s) (fun () ->
      let original = cook s graph in
      let records = Flow_graph.Probe.make ~dynamic:(Lower.zone_count lowered) lowered.evaluated in
      List.iter (fun (probes, expected_x) ->
        check (Flow_graph.Probe.plan_node records path ~probes = Some target.id)
          "a geometry preview resolves the template at the selected tuple";
        let network, root = Option.get (lowered.preview ~node:target.id ~probes graph.network) in
        let viewed = {graph with network; root = Some root} in
        check (geometry_bytes original = geometry_bytes (cook s {graph with network}))
          "the scratch preview disconnected the authored result's inputs";
        let output = cook s viewed in
        check (prims output = expected_prims) "a preview contains only the selected element";
        let x = let positions = Rdk.Packed.Float3.View.view (Rdk.Geometry.positions output) in
          Array.fold_left (+.) 0. positions.x /. float (Array.length positions.x) in
        check (Float.abs (x -. expected_x) < 1e-6)
          (Printf.sprintf "preview %s at %s: expected %.1f, got %.1f"
            (String.concat "/" path) (String.concat "," (List.map string_of_int probes)) expected_x x);
        check (geometry_bytes output = geometry_bytes (cook ~domains:3 s viewed))
          "a preview is exact across domains") (List.map2 (fun probes x -> probes, x) choices expected_x);
      check (geometry_bytes original = geometry_bytes (cook s graph))
        "preview cache entries contaminated the authored result";
      check (lowered.preview ~node:target.id ~probes:[] graph.network = None)
        "a preview accepted an incomplete iteration tuple";
      let network, root = Option.get (lowered.preview ~node:target.id
        ~probes:(List.map (fun _ -> -1) iter) graph.network) in
      check (prims (cook s {graph with network; root = Some root}) = 0)
        "a missing geometry element did not produce an empty preview") in
  verify {|(workspace preview (graph g :context sop
    (let* [field (sop/curve (list [0 0 0] [10 0 0] [20 0 0]))
           a (sop/box :center [2 0 0])
           dots (for [p (sop/point_list field)]
             (let* [unused (sop/transform a :translate p) shown (sop/box :center [100 0 0])] shown))]
      (sop/merge dots))))|} ["g"; "dots"; "unused"] [0] [[0]; [2]; [1]; [0]] [2.; 22.; 12.; 2.] 6;
  verify {|(workspace preview (graph g :context sop
    (let* [field (sop/curve (list [0 0 0] [10 0 0] [20 0 0])) a (sop/box :center [2 0 0])
           dots (for [p (sop/point_list field)]
             (let* [inner (for [q (sop/point_list field)]
                       (let* [moved (sop/transform a :translate (+ p q))] moved))]
               (sop/merge inner)))] (sop/merge dots))))|}
    ["g"; "dots"; "inner"; "moved"] [0;0] [[0;0];[2;1];[1;2]] [2.;32.;32.] 6;
  verify {|(workspace preview (graph g :context sop
    (let* [field (sop/curve (list [0 0 0] [10 0 0] [20 0 0]))
           rows (for [i (range 2)]
             (let* [dots (for [p (sop/point_list field)]
                       (sop/transform (sop/box) :translate (+ p [(* i 100) 0 0])))]
               (sop/merge dots)))] (sop/merge rows))))|}
    ["g";"rows";"dots";"@result"] [1;0] [[1;0];[1;2];[1;1]] [100.;120.;110.] 6;
  verify {|(workspace preview (graph g :context sop
    (let* [a (sop/merge (sop/box) (sop/box :center [10 0 0]))
           pieces (for [piece (sop/piece_list a)]
             (let* [moved (sop/transform piece :translate [2 0 0])] moved))]
      (sop/merge pieces))))|} ["g";"pieces";"moved"] [0] [[0];[6]] [2.5;12.5] 1;
  (* Live arguments and folds use the same frame and environment as the authored
     graph. Switching selectors cannot advance or reset the fold. *)
  let lowered = lower {|(workspace live_preview (graph g :context sop
    (let* [ticks (state [n 0.0] (+ n (frame/dt)))
           field (sop/curve (list [0 0 0] [10 0 0]))
           dots (for [p (sop/point_list field)]
             (let* [moved (sop/transform (sop/box) :translate (+ p [ticks t 0]))] moved))]
      (sop/merge dots))))|} in
  let graph = List.hd lowered.graphs in
  let target = Array.find_opt (fun (n : Flow.Eval.node) -> n.site = ["g";"dots";"moved"])
    lowered.plan.nodes |> Option.get in
  let cache_forks () =
    let seed = Flow.Eval.create_state () in
    let left = Value_lane.create ~state:(Flow.Eval.fork_state seed) ()
    and right = Value_lane.create ~state:(Flow.Eval.fork_state seed) ()
    and s = session () in
    Fun.protect ~finally:(fun () -> Session.close s) (fun () ->
      let resolve lane frame dt =
        let input = {(Input.at_time (float frame *. 0.25)) with frame; dt} in
        let network, root = Option.get (lowered.preview ~node:target.id ~probes:[0] graph.network) in
        let resolved = Result.get_ok (Value_lane.resolve lane ~live:input ~time:input.t network) in
        let compiled = Result.get_ok (Edit.compile_node resolved.geometry ~node_id:root) in
        let context = Result.get_ok (Sop.Context.create ~domains:1 ~grain:1 ~input ~time:input.t
          ~frame:(Int64.of_int frame) ()) in
        let output = Result.get_ok (Session.cook s ~context compiled) in
        let positions = Rdk.Packed.Float3.View.view
          (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))) in
        Array.fold_left (+.) 0. positions.x /. float (Array.length positions.x) in
      ignore (resolve left 1 0.25);
      ignore (resolve right 1 0.5);
      let left_x = resolve left 2 0.25 and right_x = resolve right 2 0.25 in
      check (left_x = 0.5 && right_x = 0.75)
        "the same session reused a zone result across divergent fold histories") in
  cache_forks ();
  let play domains =
    let lane = Value_lane.create () and s = session () in
    Fun.protect ~finally:(fun () -> Session.close s) (fun () ->
      Array.init 3 (fun index ->
        let input = {(Input.at_time (float (index + 1) *. 0.25)) with frame = index + 1; dt = 0.25} in
        let probes = [index mod 2] in
        let network, root = Option.get (lowered.preview ~node:target.id ~probes graph.network) in
        let resolved = Result.get_ok (Value_lane.resolve lane ~live:input ~time:input.t network) in
        let compiled = Result.get_ok (Edit.compile_node resolved.geometry ~node_id:root) in
        let context = Result.get_ok (Sop.Context.create ~domains ~grain:1 ~input ~time:input.t
          ~frame:(Int64.of_int input.frame) ()) in
        let output = Result.get_ok (Session.cook s ~context compiled) in
        let positions = Rdk.Packed.Float3.View.view (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload))) in
        let mean values = Array.fold_left (+.) 0. values /. float (Array.length values) in
        check (mean positions.x = float (index mod 2 * 10) +. input.t && mean positions.y = input.t)
          "preview selectors changed the live frame or fold's accumulated value";
        geometry_bytes (Result.get_ok (Sop.Payload.geometry output.payload)))) in
  check (play 1 = play 3) "live previews and frame folds differ across domains"

(* the graph with its zero-input node [operation] replaced by a snapshot of its cook whose
   point [index] is lifted by [by] *)
let move_point session (graph : Lower.graph) operation ~index ~by =
  let field = List.find (fun (n : Edit.node_info) -> n.operation = operation)
      (Edit.inspect graph.network.geometry) in
  let base = (Result.get_ok (Sop.Payload.geometry (Result.get_ok (Session.cook session ~context:(context ~domains:1)
    (Result.get_ok (Edit.compile_node graph.network.geometry ~node_id:field.id)))).payload)) in
  let view = Rdk.Packed.Float3.View.view (Rdk.Geometry.positions base) in
  let moved = Result.get_ok (Rdk.Geometry.with_positions
    (Rdk.Packed.Float3.of_owned ~x:(Array.copy view.x)
       ~y:(Array.mapi (fun i v -> if i = index then v +. by else v) view.y)
       ~z:(Array.copy view.z) |> Result.get_ok) base) in
  let replaced = Result.get_ok (Sop.Node.Build.restore_id field.id
    (Lisp_sop.snapshot (moved))) in
  base, Result.get_ok (Edit.replace_node replaced graph.network.geometry)

let run () =
  previews ();
  let source ticks = Printf.sprintf {|(workspace frames
    (graph g :context sop
      (let* [ticks %s
             bed (sop/curve (list [0 0 0] [1 0 0]))
             dots (for [p (sop/point_list bed)]
                    (sop/transform (sop/uv_sphere :radius 0.1 :segments 8 :rings 4)
                      :translate (+ p [ticks 0 0])))] (sop/merge dots))))|} ticks in
  let live = lower (source "(state [n 0.0] (+ n (frame/dt)))") in
  let graph = List.hd live.graphs in
  check (Network.Int_map.cardinal graph.network.frame_nodes = 1) "zone captures the fold snapshot";
  let playback domains =
    let lane = Value_lane.create () and s = session () in
    Fun.protect ~finally:(fun () -> Session.close s) (fun () ->
      let frame i =
        let input = {(Input.at_time (float i *. 0.25)) with frame = i; dt = 0.25} in
        let resolved = Result.get_ok (Value_lane.resolve lane ~live:input ~time:input.t graph.network) in
        let compiled = Result.get_ok (Edit.compile_node resolved.geometry ~node_id:(Option.get graph.root)) in
        let context = Result.get_ok (Sop.Context.create ~domains ~grain:1
          ~input ~time:input.t ~frame:(Int64.of_int i) ()) in
        let output = Result.get_ok (Session.cook s ~context compiled) in
        geometry_bytes (Result.get_ok (Sop.Payload.geometry output.payload)) in
      let values = Array.init 3 (fun i -> frame (i + 1)) in
      Array.iteri (fun i bytes ->
        let oracle = List.hd (lower (source (Printf.sprintf "%.17g" (float (i + 1) *. 0.25)))).graphs in
        let oracle_session = session () in
        Fun.protect ~finally:(fun () -> Session.close oracle_session) (fun () ->
          check (bytes = geometry_bytes (cook oracle_session oracle)) "captured fold accumulates in the zone")) values;
      Value_lane.reset lane;
      check (frame 3 = values.(0)) "fold reset invalidates the same-frame zone cache";
      values) in
  check (playback 1 = playback 3) "captured fold is byte-exact across domains";
  let nested = {|(workspace invalid (graph g :context sop
    (let* [bed (sop/curve (list [0 0 0]))]
      (sop/merge (for [p (sop/point_list bed)]
        (let* [ticks (state [n 0.0] (+ n 1))]
          (sop/transform (sop/box) :translate (+ p [ticks 0 0]))))))))|} in
  (match Lower.workspace ~extra:Editor_document.Kinds.descriptors
     (Result.get_ok (Flow.Syntax.parse nested)) with
   | Error d -> check (d.Diagnostic.code = "E_STATE_ELEMENT") "per-element fold diagnostic"
   | Ok _ -> fail "a cooked-geometry worker cannot own a frame fold");
  let lowered = lower (garden ~count:4 ()) in
  let graph = List.hd lowered.graphs in
  check (Lower.zone_count lowered ["garden"; "dots"] = None) "no count before a cook";
  let one = cook (session ()) graph and three = cook ~domains:3 (session ()) graph in
  check (geometry_bytes one = geometry_bytes three) "zone differs at 1 and 3 domains";
  check (geometry_bytes one = geometry_bytes (cook (session ()) (List.hd (lower (garden ~count:4 ())).graphs)))
    "zone is not deterministic";
  check (Lower.zone_count lowered ["garden"; "dots"] = Some 4) "count after the cook";
  let bed = prims (cook (session ()) (List.hd (lower (garden ~result:"bed" ~count:1 ())).graphs)) in
  let ball = prims (cook (session ()) (List.hd (lower
    "(workspace b (graph b :context sop (sop/uv_sphere :radius 0.06 :segments 8 :rings 4)))").graphs)) in
  check (prims one = bed + 4 * ball) (Printf.sprintf "%d prims, want %d" (prims one) (bed + 4 * ball));
  (* only the template is lowered: the zone node, not four copies of its body *)
  let ops = List.map (fun (n : Edit.node_info) -> n.operation) (Edit.inspect graph.network.geometry) in
  check (List.length (List.filter (( = ) "zone") ops) = 1 && not (List.mem "uv_sphere" ops))
    "the template body stays out of the network";
  (* provenance: every tag names an element of the zone, at its index *)
  let attribute = Option.get (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive
    Lower.source_attribute one) in
  let tags = match Rdk.Attribute.storage attribute with
    | Rdk.Attribute.Int values -> values | _ -> fail "tag storage" in
  let elements = Array.to_list tags |> List.sort_uniq compare
    |> List.filter_map (fun tag -> match Lower.origin lowered tag with
        | Some (o : Lower.origin) when o.site = ["garden"; "dots"; "moved"] -> Some o.iter | _ -> None) in
  check (elements = List.init 4 (fun i -> [ i ])) "every element resolves to its iteration";
  check (List.length (Lower.tags lowered ~site:["garden"; "dots"; "moved"] ~iter:[ 3 ]) = 1) "one tag per element";
  (* the four elements, and the tag the final merge gave its input (its primitives keep the zone's) *)
  check (List.length (Lower.tags lowered ~site:["garden"; "dots"] ~iter:[]) = 5) "the loop lights every element";
  (* cache hits after moving one point: the collection is a grid whose point 5 moves *)
  let source = {|
(workspace grove
  (graph grove :context sop
    (let* [field (sop/uv_sphere :radius 1 :segments 5 :rings 3)
           trees (for [p (sop/point_list field)]
                   (sop/transform (sop/uv_sphere :radius 0.05 :segments 6 :rings 3) :translate p))]
      (sop/merge field trees))))|} in
  let lowered = lower source in
  let graph = List.hd lowered.graphs in
  let s = session () in
  let first = cook s graph in
  let a = Session.stats s in
  let base, graph' = move_point s graph "uv_sphere" ~index:5 ~by:0.25 in
  let root = Option.get graph.root in
  let compiled = Result.get_ok (Edit.compile_node graph' ~node_id:root) in
  let before = Session.stats s in
  let second = match Session.cook s ~context:(context ~domains:1) compiled with
    | Ok output -> (Result.get_ok (Sop.Payload.geometry output.payload)) | Error e -> fail (Diagnostic.error_to_string e) in
  let after = Session.stats s in
  let n = Rdk.Geometry.point_count base in
  check (a.cooks > 0 && n > 4) "the collection has points";
  check (geometry_bytes first <> geometry_bytes second) "moving a point changes the result";
  let misses = after.misses - before.misses and hits = after.hits - before.hits in
  (* the snapshot, the zone, its merge, the moved element's transform, the final merge *)
  Printf.printf "moved one point of %d: %d misses, %d hits\n" n misses hits;
  check (misses <= 5) (Printf.sprintf "%d misses after moving one point" misses);
  check (hits >= n - 1) (Printf.sprintf "only %d hits after moving one point" hits);
  (* a body that reads t: the zone depends on the time, its elements are cooked at it, and the
     result is what the same graph gives with t replaced by a number; the elements' copies share
     their template's volatile slot, so the cache does not churn *)
  let buzz t_text = lower (Printf.sprintf {|
(workspace buzz
  (graph buzz :context sop
    (let* [field (sop/uv_sphere :radius 1 :segments 6 :rings 3)
           bees (for [p (sop/point_list field)]
                  (sop/transform (sop/uv_sphere :radius 0.05 :segments 6 :rings 3)
                                 :translate (+ p [0 (* 0.3 (sin %s)) 0])))]
      (sop/merge field bees))))|} t_text) in
  let live = buzz "t" in
  let live_graph = List.hd live.graphs in
  check (fst (Lower.counts live) > 0 && snd (Lower.counts live) >= 0) "a live loop body is counted as live";
  let at time graph lowered =
    let s = session () in
    Session.set_volatile s (Lower.is_volatile lowered);
    let context = Result.get_ok (Sop.Context.create ~domains:1 ~grain:97 ~seed:42L ~time ()) in
    let root = Option.get (graph : Lower.graph).root in
    let compiled = Result.get_ok (Edit.compile_node graph.network.geometry ~node_id:root) in
    match Session.cook s ~context compiled with
    | Ok output -> (Result.get_ok (Sop.Payload.geometry output.payload)), Session.stats s
    | Error e -> fail (Diagnostic.error_to_string e) in
  let g0, _ = at 0. live_graph live and g1, stats = at 1.25 live_graph live in
  check (geometry_bytes g0 <> geometry_bytes g1) "a loop body that reads t did not move with t";
  let fixed = buzz "1.25" in
  check (fst (Lower.counts fixed) = 0) "a body with a number is not live";
  let f1, _ = at 0. (List.hd fixed.graphs) fixed in
  check (geometry_bytes g1 = geometry_bytes f1) "a live loop differs from the same loop with t replaced";
  check (stats.volatile_entries <= 6) (Printf.sprintf "a live zone keeps %d volatile slots" stats.volatile_entries);
  (* pieces: each primitive of two boxes is one element *)
  let pieces = lower {|
(workspace bricks
  (graph bricks :context sop
    (let* [two (sop/merge (sop/box) (sop/transform (sop/box) :translate [3 0 0]))
           lifted (for [q (sop/piece_list two)] (sop/transform q :translate [0 1 0]))]
      (sop/merge lifted))))|} in
  let g = cook (session ()) (List.hd pieces.graphs) in
  check (prims g = 12) (Printf.sprintf "12 pieces in, %d out" (prims g));
  check (Lower.zone_count pieces ["bricks"; "lifted"] = Some 12) "piece count";
  check (geometry_bytes g = geometry_bytes (cook ~domains:3 (session ()) (List.hd pieces.graphs)))
    "pieces differ at 1 and 3 domains";
  (* keys order the elements *)
  let ids = Rdk.Attribute.create_owned ~name:"id" ~owner:Rdk.Attribute.Point
    (Rdk.Attribute.Int (Array.init n (fun i -> n - 1 - i))) |> Result.get_ok in
  let keyed = Result.get_ok (Rdk.Geometry.with_attribute ids base) in
  let ordered = Result.get_ok (Sop.Zone.elements Points ~key:"id" keyed) in
  check (ordered.(0).key = 0 && ordered.(0).position = Rdk.Packed.Float3.get
    (Rdk.Geometry.positions base) (n - 1)) "elements follow the key attribute";
  let plain = Result.get_ok (Sop.Zone.elements Points keyed) in
  check (plain.(0).position = Rdk.Packed.Float3.get (Rdk.Geometry.positions base) 0) "index order without a key";
  (* Live Curve points remain a typed Vec3 array in Network.drives and are
     installed without text serialization. Simultaneous option drives survive. *)
  let moving_curve = lower {|
(workspace moving_curve
  (graph g :context sop
    (sop/curve (list [t 0 0] [1 0 0] [1 1 0]))))|} in
  let curve_graph = List.hd moving_curve.graphs in
  let curve_id = Option.get curve_graph.root in
  let points_port = Port.{node = curve_id; path = Curve.parameter} in
  (match Port.Map.find_opt points_port curve_graph.network.drives with
   | Some (Flow.Eval.List values) ->
       check (Array.for_all (function Flow.Eval.Vec3 _ | Flow.Eval.Residual _ -> true | _ -> false) values)
         "curve drive did not retain a typed Vec3 list"
   | Some value -> fail ("curve points drive is not a list: " ^ Flow.Eval.show value)
   | None -> fail "curve points are absent from Network.drives");
  let drives = Port.Map.add Port.{node = curve_id; path = Curve.closed_parameter}
      (Flow.Eval.Bool true) curve_graph.network.drives
    |> Port.Map.add Port.{node = curve_id; path = Curve.filled_parameter}
      (Flow.Eval.Bool true) in
  let curve_network = Result.get_ok (Network.with_drives drives curve_graph.network) in
  let live = Input.at_time 0.5 in
  let resolved = Value_lane.resolve ~live (Value_lane.create ()) ~time:0.5 curve_network
    |> Result.get_ok in
  let cook_result geometry node_id time =
    let node = Result.get_ok (Edit.compile_node geometry ~node_id) in
    let session = session () in
    Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
      let context = Result.get_ok (Sop.Context.create ~domains:1 ~grain:97 ~seed:42L ~time ()) in
      match Session.cook session ~context node with
      | Ok output -> Sop.Payload.geometry output.payload
      | Error diagnostic -> Error diagnostic) in
  let expected_curve = lower {|
(workspace expected_curve
  (graph g :context sop
    (sop/curve (list [0.5 0 0] [1 0 0] [1 1 0]) :closed true :filled true)))|} in
  let expected_graph = List.hd expected_curve.graphs in
  let actual = Result.get_ok (cook_result resolved.geometry curve_id 0.5)
  and expected = Result.get_ok (cook_result expected_graph.network.geometry
    (Option.get expected_graph.root) 0.5) in
  check (geometry_bytes actual = geometry_bytes expected)
    "typed live curve points/options differ from authored Lisp curve";
  let actual_node = Edit.find resolved.geometry ~node_id:curve_id |> Option.get in
  check (String.starts_with ~prefix:"flow.curve\000" (Sop.Node.parameter_key actual_node))
    "live curve points were text-encoded into the parameter key";
  let nonfinite = Port.Map.add points_port
      (Flow.Eval.List [|Flow.Eval.Vec3 (Float.infinity, 0., 0.)|]) drives
    |> fun drives -> Result.get_ok (Network.with_drives drives curve_graph.network) in
  let nonfinite = Value_lane.resolve ~live (Value_lane.create ()) ~time:0.5 nonfinite
    |> Result.get_ok in
  let static_node = Edit.find expected_graph.network.geometry
      ~node_id:(Option.get expected_graph.root) |> Option.get in
  let static_node, _ = Result.get_ok (Sop.Node.apply_parameters static_node
      [Curve.parameter, Param.Text_value (Curve.encode [|Float.infinity, 0., 0.|])]) in
  let static_geometry = Result.get_ok (Edit.replace_node static_node
      expected_graph.network.geometry) in
  (match cook_result nonfinite.geometry curve_id 0.5,
      cook_result static_geometry (Option.get expected_graph.root) 0.5 with
   | Error actual, Error expected ->
       check (actual.code = expected.code && actual.message = expected.message)
         "nonfinite typed curve refusal differs from authored curve"
   | _ -> fail "nonfinite curve points were not refused consistently");
  let malformed = Port.Map.add points_port (Flow.Eval.List [|Flow.Eval.Int 1|]) drives
    |> fun drives -> Result.get_ok (Network.with_drives drives curve_graph.network) in
  (match Value_lane.resolve ~live (Value_lane.create ()) ~time:0.5 malformed with
   | Error diagnostic -> check (diagnostic.code = "E_LIVE") "malformed curve list error code"
   | Ok _ -> fail "malformed live curve points were accepted");
  let both_bad = Port.Map.add points_port (Flow.Eval.List [|Flow.Eval.Int 1|])
      (Port.Map.add Port.{node = curve_id; path = Curve.closed_parameter}
        (Flow.Eval.Vec3 (1., 0., 0.)) drives)
    |> fun drives -> Result.get_ok (Network.with_drives drives curve_graph.network) in
  (match Value_lane.resolve ~live (Value_lane.create ()) ~time:0.5 both_bad with
   | Error diagnostic -> check (diagnostic.code = "E_TYPE")
       "curve point error ran before the earlier typed option drive"
   | Ok _ -> fail "invalid option and curve points were accepted");
  (* a body that reads t only is live; one that reads only its element is not *)
  let counts text = Lower.counts (lower text) in
  check (fst (counts {|
(workspace w (graph g :context sop
  (let* [f (sop/grid)] (sop/merge (for [p (sop/point_list f)] (sop/transform (sop/box) :translate [t 0 0]))))))|}) > 0)
    "a body that reads t is not live";
  check (fst (counts {|
(workspace w (graph g :context sop
  (let* [f (sop/grid)] (sop/merge (for [p (sop/point_list f)] (sop/transform (sop/box) :translate p))))))|}) = 0)
    "a body that reads its element is live";
  print_endline "workspace zone: ok"
