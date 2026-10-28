(* W2b: live drives, volatile cache slots, fixed-step determinism, realtime
   edits and the editor's cook path. *)
open Flow_sop
module Edit = Sop.Edit_graph
module Session = Sop.Session
module Cook = Rays_editor.Private.Cook
module Timeline = Rays_editor.Timeline

let fail message = failwith ("test_workspace_live: " ^ message)
let check condition message = if not condition then fail message
open Test_text

let cases = "../../../specification/workspace/cases"
let factories = Sop.Nodes.factories
let geometry_bytes = Test_support.Workspace_cook.geometry_bytes

let lower ?compiled_ids ?sites source =
  match Flow.Syntax.parse source with
  | Error d -> fail (Diagnostic.to_string d)
  | Ok forms -> (match Lower.workspace ~extra:Editor_document.Contexts.descriptors ~factories ?compiled_ids ?sites forms with
      | Ok lowered -> lowered
      | Error d -> fail (Diagnostic.to_string d))

let case name = read (Filename.concat cases (name ^ ".lisp"))

(* the symbol [t] replaced by a literal: the static oracle of a live value *)
let at_time source time =
  let literal = Printf.sprintf "%.17g" time in
  let buffer = Buffer.create (String.length source) in
  let separator c = c = ' ' || c = '(' || c = ')' || c = '[' || c = ']' || c = '\n' in
  String.iteri (fun i c ->
    if c = 't' && i > 0 && i + 1 < String.length source
       && separator source.[i - 1] && separator source.[i + 1]
    then Buffer.add_string buffer literal else Buffer.add_char buffer c) source;
  Buffer.contents buffer

type player = {
  lowered : Lower.t; graph : Lower.graph; root : int; lane : Value_lane.t;
  session : Session.t; mutable previous : Edit.compiled option; domains : int;
}

let player ?(domains = 1) lowered =
  let graph = List.find (fun (g : Lower.graph) -> g.root <> None) lowered.Lower.graphs in
  let session = Session.create ~max_entries:512 ~max_payload_bytes:(256 * 1024 * 1024)
    |> Result.get_ok in
  Session.set_volatile session (Lower.is_volatile lowered);
  { lowered; graph; root = Option.get graph.root; lane = Value_lane.create ();
    session; previous = None; domains }

let frame p ~time =
  let resolved = match Value_lane.resolve p.lane ~time p.graph.network with
    | Ok r -> r | Error d -> fail (Diagnostic.to_string d) in
  let compiled = Edit.compile_all ?previous:p.previous resolved.geometry in
  p.previous <- Some compiled;
  let node = Result.get_ok (Edit.compiled_node compiled ~node_id:p.root) in
  let context = Sop.Context.create ~domains:p.domains ~grain:97 ~seed:42L ()
    |> Result.get_ok in
  match Session.cook p.session ~context node with
  | Ok output -> resolved, (Result.get_ok (Sop.Payload.geometry output.payload))
  | Error e -> fail (Diagnostic.error_to_string e)

let small = {|(workspace w
  (graph g :context sop []
    (let* [c (sop/uv_sphere :radius 0.3 :segments 8 :rings 4)
           a (sop/uv_sphere :radius (if (> t 1) 1.5 (+ 0.5 t)) :segments 8 :rings 4)
           b (sop/uv_sphere :radius (+ 0.5 (* 0.1 t)) :segments 8 :rings 4)]
      (sop/merge a b c))))|}

let spheres graph = List.filter_map (fun (n : Edit.node_info) ->
  if n.operation = "uv_sphere" then Some n.id else None) (Edit.inspect graph)
  |> List.sort Int.compare

let run () =
  (* --- Value_lane over a lowered workspace ------------------------------ *)
  let lowered = lower small in
  let graph = List.hd lowered.graphs in
  let c, a, b = match spheres graph.network.geometry with
    | [x; y; z] -> x, y, z | _ -> fail "three spheres" in
  (* sources are lowered in plan order: c is the static one, a and b live *)
  check (List.length lowered.pending = 2) "two live arguments";
  check (not (Lower.is_volatile lowered c) && Lower.is_volatile lowered a
    && Lower.is_volatile lowered b) "volatile: live nodes only";
  check (Lower.is_volatile lowered (Option.get graph.root)) "volatile: downstream merge";
  check (Lower.counts lowered = (3, 1)) "counts: 3 live, 1 cached";
  let lane = Value_lane.create () in
  let resolve time = match Value_lane.resolve lane ~time graph.network with
    | Ok r -> r | Error d -> fail (Diagnostic.to_string d) in
  let r2 = resolve 2. in
  check r2.time_dependent "a live network is time dependent";
  check (resolve 2. == r2) "same time reuses the resolution";
  let r3 = resolve 3. in
  let node r id = Option.get (Edit.find r.Value_lane.geometry ~node_id:id) in
  check (node r3 a == node r2 a) "a port that did not change was applied again";
  check (node r3 b != node r2 b) "a changed port was not applied";
  check (node r3 c == node r2 c) "a static node was touched";
  check (r3.geometry != r2.geometry) "the geometry did not change";
  let r5 = resolve 5. in
  check (r5.geometry != r3.geometry && node r5 a == node r3 a) "only b changes at t=5";
  (* a static network is resolved once *)
  let static = lower (case "sunflower") in
  let sgraph = List.hd static.graphs in
  let slane = Value_lane.create () in
  let s0 = Result.get_ok (Value_lane.resolve slane ~time:0. sgraph.network) in
  check (not s0.time_dependent
    && Result.get_ok (Value_lane.resolve slane ~time:9. sgraph.network) == s0)
    "a static network was resolved again";
  check (Lower.counts static = (0, 241)) "sunflower has no live nodes";
  (* --- the live value equals the static evaluation at that time ---------- *)
  let sunflower_live = {|(workspace sunflower
  (graph sunflower :context sop [(seeds : int 60) (spread : float 0.062)]
    (let* [seeds_each (for [i (range seeds)]
                        (let* [r (* (* spread (+ 1 (* 0.3 (sin t)))) (sqrt i))
                               a (* i 2.4)]
                          (sop/uv_sphere :radius 0.02 :center (value/polar r a 0)
                                         :segments 6 :rings 4)))]
      (sop/merge seeds_each))))|} in
  List.iter (fun (name, source) ->
    let live = player (lower source) in
    let previous_bytes = ref "" in
    List.iter (fun time ->
      let _, geometry = frame live ~time in
      let oracle = player (lower (at_time source time)) in
      let _, expected = frame oracle ~time:0. in
      check (geometry_bytes geometry = geometry_bytes expected)
        (Printf.sprintf "%s live at t=%g differs from the static evaluation" name time);
      check (geometry_bytes geometry <> !previous_bytes) (name ^ " did not animate");
      previous_bytes := geometry_bytes geometry;
      (* going back in time recooks correctly (one volatile slot) *)
      Session.clear oracle.session)
      [0.; 2.25; 0.5];
    Session.close live.session) ["orrery", case "orrery";
      "sunflower", sunflower_live];
  (* --- Orrery playback: 8 frames at Fixed 1/60 -------------------------- *)
  let orrery = lower (case "orrery") in
  let live_nodes, cached_nodes = Lower.counts orrery in
  let p = player orrery in
  let frames = 8 in
  for n = 0 to frames - 1 do ignore (frame p ~time:(float n /. 60.)) done;
  let stats = Session.stats p.session in
  check (cached_nodes = 4) "orrery caches base (tube, noise, normals) and plinth";
  check (stats.misses - stats.volatile_misses = cached_nodes)
    (Printf.sprintf "static nodes cook once (%d misses)" (stats.misses - stats.volatile_misses));
  check (stats.hits - stats.volatile_hits = cached_nodes * (frames - 1))
    "static nodes always hit after the first frame";
  check (stats.volatile_misses = live_nodes * frames && stats.volatile_hits = 0)
    (Printf.sprintf "live nodes miss every frame (%d misses of %d)"
      stats.volatile_misses (live_nodes * frames));
  check (stats.evictions = 0) "static entries were evicted";
  check (stats.retained_entries = cached_nodes && stats.volatile_entries = live_nodes)
    "retained: static entries in the LRU, one slot per live node";
  Session.close p.session;
  (* --- Orrery fixed-step determinism at 1 and 3 domains ------------------- *)
  let fixed domains =
    let p = player ~domains (lower (case "orrery")) in
    let bytes = List.map (fun n ->
      geometry_bytes (snd (frame p ~time:(float n /. 60.)))) [0; 1; 2; 137; 599] in
    Session.close p.session; bytes in
  check (fixed 1 = fixed 3) "orrery fixed-step frames differ at 1 and 3 domains";
  (* --- realtime edit: only the edited node's downstream cone recooks ------ *)
  let chain amount = Printf.sprintf {|(workspace w
  (graph g :context sop []
    (let* [a (sop/uv_sphere :radius 0.5 :segments 8 :rings 4)
           b (sop/transform a :translate [%s 0 0])
           c (sop/set_color b :color "#336699")
           d (sop/set_color (sop/uv_sphere :radius 0.2 :segments 8 :rings 4) :color "#336699")]
      (sop/merge c d))))|} amount in
  let first = lower (chain "1") in
  let p = player first in
  let _, original = frame p ~time:0. in
  let before = Session.stats p.session in
  check (before.misses = 6 && before.hits = 0) "chain cooks six nodes";
  (* scrub: re-lower with the ids kept, same session, same compile chain *)
  let scrubbed = lower ~compiled_ids:first.compiled_ids ~sites:first.sites (chain "2") in
  let p' = { p with lowered = scrubbed; graph = List.hd scrubbed.graphs;
    root = Option.get (List.hd scrubbed.graphs).root; lane = Value_lane.create () } in
  let _, scrubbed_geometry = frame p' ~time:0. in
  let after = Session.stats p.session in
  (* Set Color reads topology/counts, so its cached Cd carries fresh P through
     the existing component refresh. Only Transform and Merge must recook. *)
  check (after.misses - before.misses = 2)
    (Printf.sprintf "the transform and merge recook (%d misses)"
      (after.misses - before.misses));
  check (after.hits - before.hits = 4)
    "the upstream node, unchanged sibling chain and component-cached color hit";
  let fresh = player (lower (chain "2")) in
  let _, expected = frame fresh ~time:0. in
  check (geometry_bytes scrubbed_geometry = geometry_bytes expected)
    "component-cached color did not carry the transformed positions";
  check (geometry_bytes scrubbed_geometry <> geometry_bytes original)
    "the transform scrub returned the previous geometry";
  Session.close fresh.session;
  Session.close p.session;
  (* --- the editor's cook path: Lower.objects, timeline time, await -------- *)
  let orrery = lower (case "orrery") in
  let objects = Lower.objects orrery in
  let digest geometry = Digest.string (geometry_bytes geometry) in
  let cook = ref (Result.get_ok (Cook.create ~await:true
    ~prepare:(fun _ output -> Ok (digest (Result.get_ok (Sop.Payload.geometry output.Session.payload))))
    ~seed:42L ~grain:97 ~domains:1 ~max_entries:512
    ~max_payload_bytes:(256 * 1024 * 1024) ())) in
  Cook.set_volatile !cook (Lower.is_volatile orrery);
  let reference = player orrery in
  let timeline = ref (fst (Timeline.stop (Timeline.create ()))) in
  List.iter (fun n ->
    let next, changes = Timeline.seek !timeline ~frame:(Int64.of_int n) in
    timeline := next;
    let update = Cook.update ~live:false !cook ~settings:Rays_editor.Settings.none ~objects
      ~edit_error:None ~effects:Param.no_effects
      ~timeline_changes:changes ~timeline:next
      ~frame:(let frame = Test_editor_input.frame (0., 0.) [] 0 in
        { frame with input = { frame.input with dt = 0. } })
      ~frame_request:None in
    cook := update.cook;
    check (update.edit_error = None) "editor cook: edit error";
    check (Cook.status !cook = Sop.Async_cook.Idle && update.prepared_changed)
      "await: the cook of frame n is published in frame n";
    let expected = digest (snd (frame reference ~time:(Timeline.time next))) in
    match Cook.pieces !cook with
    | [piece] -> check (piece.prepared = expected)
        (Printf.sprintf "editor cook at frame %d shows another frame's geometry" n)
    | _ -> fail "editor cook: one piece") [0; 1; 2; 30; 7];
  let stats = Cook.stats !cook in
  check (stats.evictions = 0 && stats.volatile_entries = fst (Lower.counts orrery))
    "editor cook: live nodes have one slot each";
  Cook.close !cook;
  Session.close reference.session;
  print_endline "workspace live tests passed"
