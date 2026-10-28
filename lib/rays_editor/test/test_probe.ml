(* W5: recorded values, footers, iteration counts, inspector rows and the
   viewport header text. *)
open Flow_graph
open Flow_sop
module P = Projection
module T = Test_support.Projection
module E = Flow_edit
module Scope = Pxui_graph.Scope

let fail message = failwith ("test_probe: " ^ message)
let check condition message = if not condition then fail message
let recorded ?time ?geometry w = Probe.make ?time ?geometry (Result.get_ok (Flow.Eval.static ~record:true w))
let footer records (n : P.node) ~probes = Probe.footer records n ~probes
open Test_text

let bounds () =
  (* two loops call one function 4,096 times each: 8,192 records at the parameter, 4,096 kept *)
  let w = T.workspace_of "(workspace w (defn f :context value [(x : int)] (+ x 1)) \
    (graph g :context value (let* [a (for [i (range 4096)] (f i)) b (for [j (range 4096)] (f j))] (count a))))" in
  let eval = Result.get_ok (Flow.Eval.static ~record:true w) in
  check (eval.records <> [] && List.for_all (fun (_, l) -> List.length l <= 4096) eval.records)
    "a path recorded more than 4,096 values";
  check (List.exists (fun (_, l) -> List.length l = 4096) eval.records) "the bound was never reached";
  (* geometry is a reference, never geometry; the cook's counts fill it in *)
  let sunflower = T.load "sunflower" in
  let eval = Result.get_ok (Flow.Eval.static ~record:true sunflower) in
  let head = List.assoc [ "sunflower"; "head" ] eval.records in
  check (match head with [ ([], Flow.Eval.Deferred ((Flow.Ty.Named "geometry"), _)) ] -> true | _ -> false) "geometry records a plan node id only"

let sunflower () =
  let w = T.load "sunflower" in
  let scope = T.scope w "sunflower" in
  let r = recorded w in
  let zone = [ "sunflower"; "seeds_each" ] in
  let node name = T.node w "sunflower" [ "seeds_each"; name ] in
  (* the value at the probe, a sparkline across the loop, ×n *)
  let f = footer r (node "r") ~probes:[ 5 ] in
  check (f.value = "0.139" && f.runs = Some 240 && not f.invariant) ("r footer: " ^ Probe.text f);
  (match f.spark with
   | Some (values, at) -> check (Array.length values = 240 && at = 5) "r sparkline spans the 240 iterations"
   | None -> fail "r has no sparkline");
  check (Probe.text f = "0.139 · ×240") "r footer text";
  (* the invariant one offers to leave the loop and draws no sparkline *)
  let f = footer r (node "turn") ~probes:[ 5 ] in
  check (f.invariant && f.spark = None && f.runs = None && has (Probe.text f) "↑ same each time")
    ("turn footer: " ^ Probe.text f);
  (* outside a zone: the type when nothing was recorded there, the count for geometry *)
  let head = T.node w "sunflower" [ "head" ] in
  let f = footer r head ~probes:[] in
  check (f.value = "geometry" && f.runs = None && not f.invariant) "head: geometry without counts";
  let r' = recorded w ~geometry:(fun _ -> Some { Probe.points = 1204; prims = 1440; groups = [ "a"; "b" ];  extent = None; seconds = Some 0.003; attributes = [] }) in
  check ((footer r' head ~probes:[]).value = "1440 prims · groups a, b · 1 204 pts · 0.003 s") "head: cooked point and primitive counts beside timing";
  check ((footer r' { head with path = [ "nowhere" ] } ~probes:[]).value = "geometry") "an unrecorded node shows its type";
  (* iterations: the inspector list *)
  let its = Probe.iterations r (node "r") ~probes:[ 0 ] in
  check (Array.length its = 240 && its.(0) = "0" && its.(1) = "0.062" && its.(239) <> its.(1)) "iteration list";
  check (Probe.readouts r (node "r") ~probes:[ 5 ] = [ "value at probe", "0.139"; "cook", "cached" ]) "inspector rows";
  check (Probe.readouts r head ~probes:[] = [ "value", "geometry"; "cook", "cached" ]) "inspector rows outside a zone";
  (* counts under the probes *)
  check (Probe.counts r scope ~probe:(fun _ -> 0) = [ zone, 240 ]) "zone count";
  ignore zone

let tree () =
  let w = T.load "tree" in
  let r = recorded w in
  let up = T.node w "tree" [ "crown"; "up" ] in
  let f = footer r up ~probes:[ 2 ] in
  check (f.invariant && f.value = "[0 1 0]") ("up footer: " ^ Probe.text f);
  let a = T.node w "tree" [ "crown"; "a" ] in
  check (not (footer r a ~probes:[ 2 ]).invariant) "a changes with the level";
  (* the fold's accumulator: one value per step *)
  check (Probe.counts r (T.scope w "tree") ~probe:(fun _ -> 0) = [ [ "tree"; "crown" ], 5 ]) "fold steps"

let branches () =
  (* if: how many iterations took each arm *)
  let w = T.load "tiles" in
  let r = recorded w in
  let angle = T.node w "tiles" [ "cells_each"; "angle" ] in
  let f = footer r angle ~probes:[ 3 ] in
  (match f.branch with
   | Some text -> Scanf.sscanf text "then %d · else %d" (fun a b -> check (a + b = 64 && a > 0 && b > 0) "then and else add up to the runs")
   | None -> fail "an if on a name shows then/else");
  check (f.runs = Some 64) "tiles: 64 runs";
  (* filter: kept a of b *)
  let w = T.load "garland" in
  let r = recorded w in
  let big = T.node w "garland" [ "big" ] in
  let f = footer r big ~probes:[] in
  let eval = Result.get_ok (Flow.Eval.static ~record:true w) in
  let length path = match List.assoc path eval.records with
    | [ (_, Flow.Eval.List xs) ] -> Array.length xs | _ -> fail "not a list" in
  check (f.kept = Some (Printf.sprintf "kept %d of %d" (length [ "garland"; "big" ]) (length [ "garland"; "sizes" ]))
         && length [ "garland"; "sizes" ] = 14) ("filter footer: " ^ Probe.text f)

let live () =
  let w = T.load "orrery" in
  let spin = T.node w "orrery" [ "spin" ] and base = T.node w "orrery" [ "base" ] in
  check (spin.live && not base.live) "orrery liveness";
  let at time = footer (recorded ~time w) spin ~probes:[] in
  check ((at 1.).value = "0.8" && (at 2.).value = "1.6" && (at 1.).live) "a live value is forced at the time shown";
  check (has (Probe.text (at 1.)) " · t") "the live tag";
  check (not (footer (recorded ~time:1. w) base ~probes:[]).live) "a cached node carries no tag";
  let bob = T.node w "orrery" [ "moons_each"; "bob" ] in
  (match (footer (recorded ~time:1. w) bob ~probes:[ 3 ]).spark, (footer (recorded ~time:2. w) bob ~probes:[ 3 ]).spark with
   | Some (a, _), Some (b, _) -> check (Array.length a = 24 && a <> b) "a live sparkline moves with t"
   | _ -> fail "bob has no sparkline");
  check (Probe.readouts (recorded ~time:1. w) spin ~probes:[] = [ "value", "0.8"; "cook", "live, recooks every frame" ])
    "live inspector rows";
  (* the viewport header *)
  let lowered = match Lower.workspace ~extra:Editor_document.Kinds.descriptors ~factories:Sop.Nodes.factories
      (Result.get_ok (Flow.Syntax.parse (T.read (Filename.concat T.cases "orrery.lisp")))) with
    | Ok l -> l | Error d -> fail (Diagnostic.to_string d) in
  let live_count, cached = Lower.counts lowered in
  check (live_count > 0 && cached > 0) "orrery has live and cached nodes";
  check (Lower.status lowered ~seconds:0.0013 = Some (Printf.sprintf "t %d live · %d cached · cook 1.3 ms" live_count cached))
    "viewport header text";
  let lowered = Result.get_ok (Lower.workspace ~extra:Editor_document.Kinds.descriptors ~factories:Sop.Nodes.factories
      (Result.get_ok (Flow.Syntax.parse (T.read (Filename.concat T.cases "sunflower.lisp"))))) in
  check (Lower.status lowered ~seconds:0.001 = None) "a static document has no live header"

let nested () =
  (* an inner zone counts per outer iteration, not the whole product *)
  let w = T.workspace_of "(workspace w (graph g :context sop [(n : int 3)] \
    (let* [rows (for [i (range n)] \
                  (let* [cols (for [j (range (+ i 1))] (sop/box))] (sop/merge cols)))] \
       (sop/merge rows))))" in
  let scope = T.scope w "g" in
  let r = recorded w in
  let outer = [ "g"; "rows" ] and inner = [ "g"; "rows"; "cols" ] in
  let counts k = Probe.counts r scope ~probe:(fun p -> if p = outer then k else 0) in
  check (counts 0 = [ outer, 3; inner, 1 ] && counts 1 = [ outer, 3; inner, 2 ] && counts 2 = [ outer, 3; inner, 3 ])
    "nested counts follow the outer probe";
  check (List.length (Array.to_list (Probe.records r (inner @ [ ":j" ]))) = 6) "the whole product is 6 records"

(* the hoist button of an invariant footer is one request *)
let hoist () =
  let w = T.load "sunflower" in
  let view, _ = Test_support.Graph_pane.scope_view ~probe:(fun _ -> 5) w "sunflower" in
  let view, _ = Test_support.Graph_pane.step view (Test_support.Graph_pane.frame ()) in
  let turn = [ "sunflower"; "seeds_each"; "turn" ] in
  let x, y, tw, th = Option.get (Scope.Private.box_of view turn) in
  let z = Scope.zoom view in
  let point = int_of_float (x +. tw -. 30. *. z), int_of_float (y +. th -. 10. *. z) in
  let _, changes = Test_support.Graph_pane.click view point in
  ignore changes;  (* the footer's hoist button moved to the context menu *)
  (* a node that varies has none *)
  let r = [ "sunflower"; "seeds_each"; "r" ] in
  let x, y, tw, th = Option.get (Scope.Private.box_of view r) in
  let _, changes = Test_support.Graph_pane.click view (int_of_float (x +. tw -. 30. *. z), int_of_float (y +. th -. 10. *. z)) in
  check (not (List.exists (function Scope.Syntax_edit (E.Hoist _) -> true | _ -> false) changes)) "r offered a hoist"

(* a selection that an edit removed does not survive the next layout *)
let selection () =
  let w = T.load "sunflower" in
  let view, scope = Test_support.Graph_pane.scope_view w "sunflower" in
  let turn = [ "sunflower"; "seeds_each"; "turn" ] in
  let view = Scope.select [ turn; [ "sunflower"; "gone" ] ] view in
  let view = Scope.with_scope ~key:"sunflower" scope view in
  check (Scope.selected view = [ turn ]) "the selection kept a path that no longer exists"

(* W8: a loop over geometry has one template record; the count comes from the cook *)
let geometry_zone () =
  let w = T.workspace_of "(workspace w (graph g :context sop (let* [f (sop/grid) \
    d (for [p (sop/point_list f)] (let* [b (sop/box) m (sop/transform b :translate p)] m)) r (sop/merge f d)] r)))" in
  let scope = T.scope w "g" in
  let zone = [ "g"; "d" ] in
  let dynamic path = if path = zone then Some 100 else None in
  let eval = Result.get_ok (Flow.Eval.static ~record:true w) in
  check (Probe.counts (Probe.make eval) scope ~probe:(fun _ -> 0) = [ zone, 1 ]) "template count before a cook";
  let r = Probe.make ~dynamic eval in
  check (Probe.counts r scope ~probe:(fun _ -> 0) = [ zone, 100 ]) "the cook's element count";
  let m = T.node w "g" [ "d"; "m" ] in
  check ((footer r m ~probes:[ 41 ]).value <> "not run here") "every element reads the template record";
  check ((footer (Probe.make eval) m ~probes:[ 41 ]).value = "not run here") "without a count only iteration 0 ran";
  (* a value that reads the element is forced for the element the probe names, once the zone cooked *)
  let w = T.workspace_of "(workspace w (graph g :context sop (let* [f (sop/grid) \
    d (for [p (sop/point_list f)] (let* [q (+ p [1 0 0]) m (sop/transform (sop/box) :translate q)] m)) r (sop/merge f d)] r)))" in
  let eval = Result.get_ok (Flow.Eval.static ~record:true w) in
  let q = T.node w "g" [ "d"; "q" ] in
  check ((footer (Probe.make ~dynamic eval) q ~probes:[ 3 ]).value = "?") "an element-dependent value reads ? before any element is known";
  let key = Option.get (List.find_map (fun (n : Flow.Eval.node) -> match n.kind, List.assoc_opt "element" n.args with
    | "zone/points", Some (Flow.Eval.Text k) -> Some k | _ -> None) (Array.to_list eval.plan.nodes)) in
  let element path k = if path = zone then Some [ key, Flow.Eval.Vec3 (float k, 0., 0.) ] else None in
  let r = Probe.make ~dynamic ~element eval in
  check ((footer r q ~probes:[ 3 ]).value = "[4 0 0]" && (footer r q ~probes:[ 41 ]).value = "[42 0 0]")
    "an element-dependent value is not forced for the probed element";
  (* and the inspector's list of iterations has every element's value *)
  let every = Probe.iterations r q ~probes:[ 3 ] in
  check (Array.length every = 100 && every.(41) = "[42 0 0]" && every.(0) = "[1 0 0]")
    "the per-iteration list of a loop over geometry is not forced for every element"

(* a footer counts a node that is not upstream of the display too: the cook is asked for it, and a
   failure there never fails the display (Async_cook.submit_some) *)
let off_display () =
  let module Cook = Rays_editor.Private.Cook in
  let text = "(workspace w (graph g :context sop (let* [a (sop/box) orphan (sop/uv_sphere) r (sop/transform a)] r)))" in
  let forms = match Flow.Syntax.parse text with Ok f -> f | Error d -> fail (Diagnostic.to_string d) in
  let lowered = match Lower.workspace ~extra:Editor_document.Kinds.descriptors
      ~factories:Sop.Nodes.factories forms with Ok l -> l | Error d -> fail (Diagnostic.to_string d) in
  let compiled site = List.find_map (fun (n : Flow.Eval.node) ->
    if n.site = site then Network.Int_map.find_opt n.id lowered.compiled else None)
    (Array.to_list lowered.plan.nodes) in
  let orphan = Option.get (compiled [ "g"; "orphan" ]) and shown = Option.get (compiled [ "g"; "r" ]) in
  let cook = Result.get_ok (Cook.create ~await:true ~prepare:(fun _ output -> Ok (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload)))
    ~seed:1L ~grain:97 ~domains:1 ~max_entries:512 ~max_payload_bytes:(256 * 1024 * 1024) ()) in
  let update probes = Cook.update ~live:false ~probes cook ~settings:Rays_editor.Settings.none
    ~objects:(Lower.objects lowered) ~edit_error:None ~effects:Param.no_effects
    ~timeline_changes:[] ~timeline:(Rays_editor.Timeline.create ())
    ~frame:(let frame = Test_editor_input.frame (0., 0.) [] 0 in
      { frame with input = { frame.input with dt = 0. } }) ~frame_request:None in
  let object_id = let id, _, _ = List.hd (Lower.objects lowered) in id in
  let updated = update [ object_id, shown; object_id, orphan ] in
  check (updated.edit_error = None) "the probed nodes failed the display";
  let counted node = Cook.geometry updated.cook ~object_id ~node_id:node in
  check (Option.fold ~none:false ~some:(fun (g : Probe.geometry) -> g.prims > 0) (counted orphan))
    "a node off the display has no count";
  check (Option.fold ~none:false ~some:(fun (g : Probe.geometry) -> g.prims > 0) (counted shown))
    "a node on the display has no count";
  Cook.close updated.cook

(* register L16: a skipped iteration is still an iteration of the selector (the tuples do not
   move), its body has no record, and the sparkline marks where the probe sits among the ones that ran *)
let skips () =
  let w = T.workspace_of "(workspace w (graph g :context value \
    (let* [xs (for [i (range 5)] :skip [1 3] (let* [sq (* i i)] sq))] (count xs))))" in
  let scope = T.scope w "g" in
  let r = recorded w in
  let sq = T.node w "g" [ "xs"; "sq" ] in
  check (Probe.counts r scope ~probe:(fun _ -> 0) = [ [ "g"; "xs" ], 5 ]) "the selector counts the skipped iterations";
  check (List.length (Array.to_list (Probe.records r [ "g"; "xs"; ":i" ])) = 5 && List.length (Array.to_list (Probe.records r [ "g"; "xs"; "sq" ])) = 3)
    "the loop variable is recorded 5 times, the body 3";
  let f = footer r sq ~probes:[ 2 ] in
  check (f.value = "4" && f.runs = Some 3) ("footer at a run iteration: " ^ Probe.text f);
  check ((footer r sq ~probes:[ 1 ]).value = "not run here") "footer at a skipped iteration";
  (match f.spark, (footer r sq ~probes:[ 4 ]).spark with
   | Some (values, at), Some (_, last) -> check (values = [| 0.; 4.; 16. |] && at = 1 && last = 2) "the sparkline places the probe among the iterations that ran"
   | _ -> fail "no sparkline");
  check (Array.length (Probe.iterations r sq ~probes:[ 0 ]) = 3) "the inspector lists the iterations that ran";
  (* geometry: the plan node of a probed iteration, none for a skipped one *)
  let g = T.workspace_of "(workspace w (graph g :context sop \
    (let* [b (for [i (range 4)] :skip [1] (sop/box :size (+ 1 i)))] (sop/merge b))))" in
  let rg = recorded g in
  let plan = (Result.get_ok (Flow.Eval.static ~record:true g)).plan in
  let node probes = Option.map (fun id -> plan.nodes.(id)) (Probe.plan_node rg [ "g"; "b"; "@result" ] ~probes) in
  check (node [ 1 ] = None) "a skipped iteration has no plan node to pick";
  List.iter (fun k -> match node [ k ] with
    | Some n -> check (n.kind = "sop/box" && n.iter = [ k ] && n.site = [ "g"; "b"; "@result" ]) "a plan node is at its own tuple"
    | None -> fail "an iteration that ran has no plan node") [ 0; 2; 3 ]

let compiled_inputs () =
  let w = T.workspace_of {|(workspace compiled_probe
    (graph g :context value
      (let* [tick (state [n 0] (+ n 1))
             xs (map (fn [(x : float)] (+ x (frame/dt)))
                     (array/float (+ 5000 (frame/index)) 1.0))
             ys (map (fn [(x : float)]
                       (let* [value (+ (+ x (frame/width)) tick)] value)) xs)]
        (array/count ys))))|} in
  let evaluated = Flow.Eval.static ~record:true w |> Result.get_ok in
  let scope = T.scope w "g" in
  let state = Flow.Eval.create_state () and calls = ref 0 in
  let force : Probe.force = fun ?state ?elems ?resolve value ~live ->
    incr calls;
    Result.bind (Flow_ir.Executor.compile value) (fun program ->
      Flow_ir.Executor.force ?state ?elems ?resolve program ~live) in
  List.iter (fun frame ->
    let live = {(Input.at_time (float frame /. 60.)) with frame; dt=0.125; size=321,123} in
    let stamp = Flow.Eval.state_stamp state in
    let old = Probe.make ~state ~live evaluated and compiled = Probe.make ~force ~state ~live evaluated in
    let count records = Probe.counts records scope ~probe:(fun _ -> 4999) in
    check (count old = count compiled && List.mem (["g"; "ys#0"], 5000 + frame) (count compiled))
      "compiled probe inputs changed live selector lengths";
    let node = T.node w "g" ["ys#0"; "value"] in
    List.iter (fun index ->
      check (Probe.at old node.path ~probes:[index] = Probe.at compiled node.path ~probes:[index])
        "compiled inputs changed the selected reference body";
      check (Probe.footer old node ~probes:[index] = Probe.footer compiled node ~probes:[index])
        "compiled inputs changed the sampled sparkline/footer") [0;4095;4999];
    check (Flow.Eval.state_stamp state = stamp) "compiled probes advanced the host fold";
    let tick = List.assoc ["g";"tick"] evaluated.records |> List.hd |> snd in
    ignore (Flow.Eval.Host.force_reference ~state tick ~live |> Result.get_ok)) [0;1;3;0];
  check (!calls > 0) "live map inputs bypassed the host compiler"

let run () =
  let images=T.workspace_of "(workspace w (graph g :context value (let* [picture (image/noise :width 3 :height 2)] 0)))" in
  let evaluated=Result.get_ok(Flow.Eval.static ~record:true images) in
  let preview=Probe.make evaluated in
  let node=T.node images "g" ["picture"] in
  check ((Probe.footer preview node ~probes:[]).value="3 × 2 image") "image footer reports noise dimensions";
  let dimensions=ref(Some(3,2))in
  let borrowed=Probe.make ~image:(fun _-> !dimensions) evaluated in
  ignore(Probe.footer borrowed node ~probes:[]);
  dimensions:=Some(7,5);
  check((Probe.footer borrowed node ~probes:[]).value="7 × 5 image") "borrowed image dimensions refresh without cooking in a probe";
  let w = T.workspace_of "(workspace w (graph g :context value (let* [tick (state [n 0] (+ n 1))] tick)))" in
  let evaluated = Result.get_ok (Flow.Eval.static ~record:true w) in
  let state = Flow.Eval.create_state () in
  let preview = Probe.make ~state ~live:(Input.at_time 0.) evaluated in
  check (Array.exists (fun (_, v) -> v = Probe.Value (Flow.Eval.Int 1))
      (Probe.records preview ["g"; "tick"]))
    "the probe reads a fold snapshot";
  let result = Result.get_ok (Flow.Eval.run ~state ~live:{(Input.at_time 1.) with frame = 1} ~time:1. w) in
  check (List.assoc "g" result.results = Flow.Eval.Int 1) "a probe never advances the environment fold";
  compiled_inputs (); skips (); off_display (); geometry_zone (); bounds (); sunflower (); tree (); branches (); live (); nested (); hoist (); selection ();
  print_endline "probe tests passed"
