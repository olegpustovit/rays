(* W7: the workspace text pane.  Selection-scoped text (closure and marked
   span), then the pane driven through the editor: draft, Check & apply,
   Discard, an invalid draft (document unchanged, error at its line) and undo
   labels. *)
module T = Rays_editor.Private.Text_pane

let fail message = failwith ("test_text_pane: " ^ message)
(* the pitch of a line of the text area at the tests' 11 points ([Ui.text_line_height]); the tab
   row above it is a 24-point control row *)
let line_pitch = 17.
let check condition message = if not condition then fail message
open Test_text

(* Every edit re-lowers the workspace and the cost follows the number of sphere calls (240 seeds: 16 s a
   cook, 8: 0.5 s); the text pane does not care how many shapes there are. *)
let small_sunflower () = Str.global_replace (Str.regexp_string "(seeds : int 240)") "(seeds : int 8)" (case "sunflower")

let small_rosette () = List.fold_left (fun text (from, by) -> Str.global_replace (Str.regexp_string from) by text) (case "rosette")
  [ "(petals : int 12)", "(petals : int 2)"; ":segments 12", ":segments 4"; ":rings 6", ":rings 3";
    ":segments 8", ":segments 4"; ":rings 4", ":rings 2"; "(radial j\n                         6", "(radial j\n                         1" ]

let source name = match Flow.Syntax.parse (case name) with
  | Ok forms -> forms | Error d -> fail (Diagnostic.to_string d)

let contains text part =
  let n = String.length part in
  let rec at i = i + n <= String.length text && (String.sub text i n = part || at (i + 1)) in
  at 0
let marked (shown : T.shown) = match shown.mark with
  | Some (a, b) -> String.sub shown.text a (b - a) | None -> fail "nothing marked"

let replace text ~from ~by =
  let n = String.length from in
  let rec at i = if i + n > String.length text then fail ("no " ^ from)
    else if String.sub text i n = from then i else at (i + 1) in
  let i = at 0 in
  String.sub text 0 i ^ by ^ String.sub text (i + n) (String.length text - i - n)

let token_scrubs () =
  let module S = Flow.Syntax in
  let module F = Flow_graph.Flow_edit in
  let source = S.parse {|(workspace tokens
    (graph g :context sop
      (let* [a (sop/box :size [11 22 33])
             repeat (for [i (range 2)] (sop/uv_sphere :radius 0.5))
             n 77
             ys (map (fn [(x : float)] (let* [shift (+ x 0.25)] (* shift 2.0))) (array/float 3 0.5))
             b (sop/transform a :uniform_scale (* 3 4))]
        (sop/transform (sop/uv_sphere :radius 0.6) :uniform_scale 0.7))))|} |> Result.get_ok in
  let rec child form = function [] -> form
    | i :: rest -> child (List.nth (S.children form) i) rest in
  List.iter (fun (path, key, sub) ->
    let atom = child (F.arg_text source path key |> Option.get) sub in
    List.iter (fun tab ->
      let shown = T.make_shown source "g" (Some path) tab in
      let span = List.assoc atom.id shown.spans in
      let op, phases = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
        T.scrub_op shown (span.start, span.finish) "1234" |> Option.get) in
      check (phases.entries = [])
        "finding a numeric token ran the workspace pipeline";
      check (match op with F.Set_arg {node; key = k; sub = s; value} ->
        node = path && k = key && s = sub && value.node = S.Num "1234" | _ -> false)
        "a printed numeric token targeted the wrong card or argument") [T.Selection; Graph; Document])
    [ ["g";"a"], F.Kw "size", [1];
      ["g";"repeat";"@result"], F.Kw "radius", [];
      ["g";"n"], F.Whole, [];
      ["g";"ys#0";"shift"], F.Pos 1, [];
      ["g";"ys#0";"@result"], F.Pos 1, [];
      ["g";"b"], F.Kw "uniform_scale", [1];
      ["g";"@result#0"], F.Kw "radius", [] ];
  let shown = T.make_shown source "g" (Some ["g";"ys#0";"shift"]) T.Graph in
  let atom = F.arg_text source ["g";"ys#0";"shift"] (Pos 1) |> Option.get in
  let span = List.assoc atom.id shown.spans in
  check (T.binding_at shown span.start = Some ["g";"ys#0";"shift"])
    "the caret in an inline function's body did not select its card";
  check (T.binding source ["g";"ys#0";"shift"] <> None)
    "an inline function body path did not find its source binding";
  let catalog = Editor_document.Contexts.catalog ~version:1 Sop_catalog.Editor.factories |> Result.get_ok in
  let doc = Editor_document.Workspace_doc.of_text catalog
    "(workspace w (graph g :context sop (sop/box))) (view :zoom 999)" |> Result.get_ok in
  let form = S.parse "(graph g :context sop (let* [a (sop/box :size [2 3 4]) b (sop/transform a)] b))"
    |> Result.get_ok |> List.hd in
  let doc = Editor_document.Workspace_doc.edit catalog doc (F.Set_graph {name = "g"; form}) |> Result.get_ok in
  let printed, spans = Editor_document.Workspace_doc.print doc in
  check (List.length spans = List.length (List.sort_uniq Int.compare (List.map fst spans)))
    "document metadata reused source IDs after a structural edit";
  let shown = { (T.make_shown doc.source "g" None Document) with text = printed; spans } in
  let last = List.find_map (fun (_, (span : Diagnostic.span)) ->
    if String.sub printed span.start (span.finish - span.start) = "999" then Some span else None) spans |> Option.get in
  check (T.scrub_op shown (last.start, last.finish) "1000" = None) "metadata token aliased a source literal"

let selection_text () =
  let src = source "sunflower" in
  let sel path = T.make_shown src "sunflower" (Some path) T.Selection in
  (* a binding inside a loop: its top-level ancestor, no unrelated bindings *)
  let s = sel [ "sunflower"; "seeds_each"; "r" ] in
  check (contains s.text "(let* [seeds_each (for [i (range seeds)]") "sunflower closure lost the ancestor";
  check (not (contains s.text "head")) "sunflower closure kept a downstream binding";
  check (not (contains s.text "(graph")) "the closure is a let*, not the graph";
  check (contains s.text "inputs seeds, spread") "the closure names the inputs it reads";
  check (marked s = "r (* spread (sqrt i))") ("marked " ^ marked s);
  check (s.key = [ "sunflower"; "seeds_each"; "r" ]) "the selection's key";
  check ((match T.binding src [ "sunflower"; "seeds_each"; "r" ] with
          | Some (_, v) -> String.trim (fst (Flow.Lisp.print [ v ])) | None -> "")
         = "(* spread (sqrt i))") "binding text";
  (* a binding that reads another: the upstream closure comes with it *)
  let s = sel [ "sunflower"; "head" ] in
  check (contains s.text "with 1 upstream binding") "head has one upstream binding";
  check (contains s.text "seeds_each (for" && contains s.text "head (sop/merge seeds_each)") "head closure";
  check (marked s = "head (sop/merge seeds_each)") "head mark";
  (* the marked text is where the printer put it: printing again finds it *)
  let bloom = source "bloom" in
  let sel path = T.make_shown bloom "flower" (Some path) T.Selection in
  let s = sel [ "flower"; "ring"; "leaf" ] in
  check (marked s = "leaf (petal :length (+ 0.9 wobble) :width 0.22)") ("bloom leaf mark " ^ marked s);
  check (contains s.text "ring (for" && not (contains s.text "bloom (sop/merge")
    && not (contains s.text "heart")) "bloom closure of ring is the loop alone";
  let s = sel [ "flower"; "result" ] in
  check (contains s.text "bloom (sop/merge ring)" && contains s.text "heart (sop/uv_sphere"
    && contains s.text "ring (for" && contains s.text "with 3 upstream bindings") "bloom result closure";
  check (marked s |> fun m -> String.length m > 0 && String.sub m 0 6 = "result") "bloom result mark";
  (* no selection: the whole graph, nothing marked; Graph marks the selection *)
  let s = T.make_shown bloom "flower" None T.Selection in
  check (contains s.text "(graph flower" && s.mark = None && s.key = [ "flower" ]) "unselected shows the graph";
  let g = T.make_shown bloom "flower" (Some [ "flower"; "ring"; "leaf" ]) T.Graph in
  check (contains g.text "(graph flower" && contains g.text "heart" && g.mark <> None) "graph tab";
  check (not (contains g.text "(defn petal")) "the graph tab shows the active graph only";
  let d = T.make_shown bloom "flower" None T.Document in
  check (contains d.text "(defn petal" && contains d.text "(graph world") "document tab is the whole workspace";
  (* the caret in a binding is that binding's path; outside every binding it is none *)
  let offset part = let rec at i = if String.sub g.text i (String.length part) = part then i else at (i + 1) in at 0 in
  check (T.binding_at g (offset "leaf (" + 6) = Some [ "flower"; "ring"; "leaf" ]) "binding_at: a nested binding";
  check (T.binding_at g 0 = None) "binding_at: the header is no binding";
  (* a call written in an input is a node: the caret in it selects it, and its path finds it *)
  check (T.binding_at g (offset "(value/hsv" + 3) = Some [ "flower"; "ring"; "tint#:color" ])
    "binding_at: a nested node";
  check ((match T.binding bloom [ "flower"; "ring"; "tint#:color" ] with
          | Some (None, { Flow.Syntax.node = List ({ node = Sym "value/hsv"; _ } :: _); _ }) -> true | _ -> false))
    "binding: a nested node is the call written in the input";
  check (T.binding_at d 5 = None) "binding_at: the Document tab has none";
  let offset_in text part = let rec at i =
    if i + String.length part > String.length text then fail ("missing token " ^ part)
    else if String.sub text i (String.length part) = part then i else at (i + 1) in at 0 in
  List.iter (fun tab ->
    let shown = T.make_shown bloom "flower" (Some ["flower"; "ring"; "leaf"]) tab in
    check (T.binding_at shown (offset_in shown.text "leaf (" + 6) = Some ["flower"; "ring"; "leaf"])
      "all Lisp tabs must select the nested node under the caret";
    check (T.definition_at shown (offset_in shown.text "(petal :length" + 2) = Some ["def:petal"])
      "a function call head did not resolve its definition") [T.Selection; Graph; Document];
  let lexical = Flow.Syntax.parse {|(workspace nav
    (defmacro panel [x] x)
    (graph g :context value
      (let* [motion {:rx 1.0}
             outer motion.rx
             inner (let* [motion {:rx 2.0} leaf motion.rx] (panel leaf))]
        inner)))|} |> Result.get_ok in
  check (T.reference_target lexical ~from:["g"; "outer"] "motion.rx" = Some ["g"; "motion"])
    "a dotted output did not resolve its owning binding";
  check (T.reference_target lexical ~from:["g"; "inner"; "leaf"] "motion.rx" = Some ["g"; "inner"; "motion"])
    "a shadowed name did not resolve in its inner scope";
  let shown = T.make_shown lexical "g" (Some ["g"; "inner"]) Graph in
  check (T.definition_at shown (offset_in shown.text "(panel leaf)" + 2) = Some ["macro:panel"])
    "a macro call head did not resolve its template";
  let rails = Flow.Syntax.parse {|(workspace rails
    (graph g [(x : float 1.0)] :context value
      (let* [outer x
             loop (for [x (range 2)] (let* [leaf (+ x 1)] leaf))
             f (fn [(x : float)] (let* [leaf (+ x 1.0)] leaf))]
        outer)))|} |> Result.get_ok in
  check (T.reference_target rails ~from:["g"; "outer"] "x" = Some ["g"; ":x"])
    "a graph input did not resolve its input card";
  List.iter (fun tab ->
    check (marked (T.make_shown rails "g" (Some ["g"; ":x"]) tab) = "(x : float 1.0)")
      "a referenced graph input was not revealed in Lisp") [T.Graph; Document];
  List.iter (fun owner ->
    check (T.reference_target rails ~from:["g"; owner; "leaf"] "x" = Some ["g"; owner])
      "a local rail binder jumped to a shadowed graph input") ["loop"; "f"];
  (* Command-click, colour chips and the document-aware completions *)
  let module L = Rays_editor.Private.Lisp_text in
  let sample = "(sop/material geo :material (ref cobalt) :tint \"#ff8000\" :note \"#zzzzzz\")" in
  check (L.ref_at sample 30 = Some "cobalt" && L.ref_at sample 40 = Some "cobalt") "ref_at: inside the form";
  check (L.ref_at sample 5 = None) "ref_at: elsewhere";
  (match L.color_chips sample with
   | [ (a, b, c) ] -> check (String.sub sample a (b - a) = "\"#ff8000\"" && Rays_math.Color.to_tuple c = (255, 128, 0, 255)) "color_chips: the literal"
   | _ -> fail "color_chips: only the valid literal");
  let names = { L.materials = [ "cobalt"; "brass" ]; cameras = [ "cam" ]; layouts = [ "View | Graph"; "Lisp" ];
    graphs = [ "cobalt"; "brass"; "shards" ] } in
  let labels text = List.map (fun (c : Pxui.Ui.completion) -> c.label, c.insert)
    (L.complete ~names (L.vocab []) text (String.length text)) in
  check (labels "(sop/material geo :material " = [ "brass", "(ref brass)"; "cobalt", "(ref cobalt)" ]
         || labels "(sop/material geo :material " = [ "cobalt", "(ref cobalt)"; "brass", "(ref brass)" ])
    "completion: :material lists the material graphs";
  check (labels "(scene/root m :camera " = [ "cam", "cam" ]) "completion: :camera lists the cameras";
  check (labels "(ui/switch a b :active " = [ "0 View | Graph", "0"; "1 Lisp", "1" ]) "completion: :active lists the layouts";
  check (List.mem ("shards", "shards") (labels "(sop/material geo :material (ref sh")) "completion: ref offers every graph";
  let imports = ["twice", "lib.rays"; "shards", "lib.rays"] in
  let imported text = L.complete ~names ~imports (L.vocab []) text (String.length text) in
  let detail label text = List.find_map (fun (c : Pxui.Ui.completion) ->
    if c.label = label then Some c.detail else None) (imported text) in
  check (detail "twice" "(twi" = Some "import · lib.rays") "completion: imported function lost its file";
  check (detail "shards" "(ref sha" = Some "graph · lib.rays") "completion: imported graph lost its file";
  check (detail "shards" "(sha" = None) "completion: a graph was offered as a function head";
  (* an error's line: a position, else the span *)
  let d = Diagnostic.error ~position:{ line = 4; col = 2 } ~code:"E_X" "x" in
  check (T.line_of "a\nb\nc\nd" d = Some 4) "line from position";
  let d = Diagnostic.error ~span:{ start = 4; finish = 5 } ~code:"E_X" "x" in
  check (T.line_of "a\nb\nc\nd" d = Some 3) "line from span"

(* the pure parts of find and go to: matches, stepping, replacing, symbols, lines *)
let find_pure () =
  let find = { T.query = "ab"; replace = "X"; case = false; replacing = true; at = 0; grab = false } in
  let text = "ab AB xab\nab" in
  let matches = T.find_all ~case:false "ab" text in
  check (matches = [ 0, 2; 3, 5; 7, 9; 10, 12 ]) "matches fold ASCII case";
  check (T.find_all ~case:true "ab" text = [ 0, 2; 7, 9; 10, 12 ]) "matches can count case";
  check (T.find_all ~case:false "" text = [] && T.find_all ~case:false "aa" "aaaa" = [ 0, 2; 2, 4 ])
    "an empty query finds nothing and matches do not overlap";
  check (T.find_current find matches = Some (0, 2)) "the current match is the first from the byte";
  check (T.find_current { find with at = 5 } matches = Some (7, 9)) "the current match skips those before";
  check (T.find_current { find with at = 11 } matches = Some (0, 2)) "the current match wraps";
  let next = T.find_step find matches `Next in
  check (T.find_current next matches = Some (3, 5)) "Next goes to the following match";
  check (T.find_current (T.find_step { find with at = 10 } matches `Next) matches = Some (0, 2)) "Next wraps";
  check (T.find_current (T.find_step next matches `Previous) matches = Some (0, 2)) "Previous goes back";
  check (T.find_current (T.find_step find matches `Previous) matches = Some (10, 12)) "Previous wraps";
  (match T.find_replace { find with at = 3 } matches ~all:false with
   | Some (r, bar) ->
       check (r.splice = [ 3, 5, "X" ] && r.select = (4, 4) && bar.at = 4) "replace one takes the current match"
   | None -> fail "replace one found nothing");
  (match T.find_replace find matches ~all:true with
   | Some (r, _) -> check (List.length r.splice = 4) "replace all takes every match"
   | None -> fail "replace all found nothing");
  check (T.find_replace { find with query = "zz" } [] ~all:true = None) "nothing to replace";
  let symbols = T.symbols "(workspace w (graph g :context value (let* [a 1 (b : int) 2 c (+ a 1)] c)) (defn f [x] x))" in
  check (List.map (fun (n, k, _) -> n, k) symbols = [ "g", "graph"; "a", "let*"; "c", "let*"; "f", "defn" ])
    "symbols are graphs, let* names and functions";
  check (T.symbols "(graph g" = []) "text that does not read has no symbols";
  let lines = "one\n\xc3\xa9 two\nthree" in
  check (T.line_target lines "2" = Some 4 && T.line_target lines "3:2" = Some 12) "a line and a column";
  check (T.line_target lines "2:3" = Some 7) "a column counts characters";
  check (T.line_target lines "99" = Some 11 && T.line_target lines "1:99" = Some 3) "past the end is clamped";
  check (T.line_target lines "x" = None && T.line_target lines "0" = None && T.line_target lines "2:y" = None)
    "anything else is refused"

let frame ?(mouse = (450., 320.)) ?(keys = []) count events : Rays.Frame.t = {
  input = { (Input.at_time (float count /. 60.)) with dt = 1. /. 60.;
    frame = count; tick = count; size = 900, 640; pointer = mouse; keys; events };
  pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. }

module E = Rays_editor.Editor

(* The centre of a tab (Selection, Graph, Document) in the header over a text pane's body: the
   tabs end at [right], 12 points apart, in the test face's 7-point glyphs.  A graph panel in
   text view keeps its own Graph / List / Text after them (159 points with the collapse
   button); a Lisp panel only the button (36: 8, 20 and 8). *)
(* a panel under 400 points wide labels the third tab "Doc": 5 glyphs shorter, so the row starts 35 later *)
let tab_at ?(narrow = false) ~right ~top index =
  float (right - 178 + List.nth (if narrow then [ 66; 127; 168 ] else [ 31; 92; 150 ]) index), float (top - 12)

(* an editor over the workspace [text], and the line the crash dump's editor.txt has for [name] *)
let start_env text =
  let open Rays in
  let catalog = Editor_document.Contexts.catalog ~version:Flow_sop.Manifest.version
      Sop_catalog.Editor.factories |> Result.get_ok in
  let workspace = Rays_editor.Workspace_doc.of_text catalog text |> Result.get_ok in
  let presets = Filename.temp_dir "rays-text-presets" "" in
  ref (E.create ~ui_text_size:11 ?ui_font:(Sys.getenv_opt "RAYS_UI_FONT") ~presets ~await:true ~workspace
      ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
        |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) () |> Result.get_ok)

let dump_line env name =
  let directory = Filename.temp_dir "rays-text-pane" "" in
  Fun.protect ~finally:(fun () ->
    Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
    Unix.rmdir directory) (fun () ->
    E.crash_dump !env directory;
    let dump = In_channel.with_open_bin (Filename.concat directory "editor.txt") In_channel.input_all in
    match List.find_opt (fun l -> String.starts_with ~prefix:(name ^ ": ") l) (String.split_on_char '\n' dump) with
    | Some l -> l | None -> fail ("no " ^ name ^ " in the dump"))

let editor_text () =
  let open Rays in
  let catalog = Editor_document.Contexts.catalog ~version:Flow_sop.Manifest.version
      Sop_catalog.Editor.factories |> Result.get_ok in
  let workspace = Rays_editor.Workspace_doc.of_text catalog (small_sunflower ()) |> Result.get_ok in
  let presets = Filename.temp_dir "rays-text-presets" "" in
  let env = ref (E.create ~ui_text_size:11 ?ui_font:(Sys.getenv_opt "RAYS_UI_FONT") ~presets ~await:true ~workspace
      ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
        |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) () |> Result.get_ok) in
  let count = ref 0 in
  let step ?keys ?(at = (450., 320.)) events =
    incr count; env := E.update !env (frame ~mouse:at ?keys !count events) in
  let click (x, y) = step ~at:(x, y) [ Input.Pointer_moved (x, y) ];
    step ~at:(x, y) [ Input.Pointer_pressed (Input.LeftButton, (x, y));
      Input.Pointer_released (Input.LeftButton, (x, y)) ] in
  let key k = Input.Key_pressed k and char c = Input.Key_pressed (Input.KeyChar c) in
  let dump () =
    let directory = Filename.temp_dir "rays-text-pane" "" in
    Fun.protect ~finally:(fun () ->
      Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
      Unix.rmdir directory) (fun () ->
      E.crash_dump !env directory;
      In_channel.with_open_bin (Filename.concat directory "editor.txt") In_channel.input_all) in
  let settle () = for _ = 1 to 4 do step [] done in
  settle ();
  let gx, gy, gw, gh = (E.panes !env (frame 0 [])).graph in
  (* enter the sunflower object, then / l t: the graph panel shows its text *)
  click (float (gx + 50), float (gy + 100));
  step [ key Input.Home ]; step [ char 'i' ]; settle ();
  step [ key (Input.KeyChar '/'); char 'l'; char 't' ]; step [];
  check (contains (dump ()) "projection: text") ("/ l t did not reach the text pane\n" ^ dump ());
  let ws () = E.workspace !env in
  let original = ws () in
  let base = E.undo_label !env in
  let applied = Flow.Lisp.print original.source |> fst in
  let tab_document = tab_at ~narrow:(gw < 400) ~right:(gx + gw - 159) ~top:gy 2 in
  let area = float (gx + 200), float (gy + 24 + 60) in
  (* the bar: 33 points under a narrow panel (hairline and 32), 58 under a wide one (and the status row) *)
  let bar_y = float (gy + gh - (if gw < 400 then 16 else 40)) in
  let apply = float (gx + 32), bar_y and discard = float (gx + 180), bar_y in
  click tab_document;
  check (contains (dump ()) "document tab, draft no") ("Document tab\n" ^ dump ());
  (* typing makes a draft; every other pane keeps the applied document *)
  let type_text text =
    click area;
    step ~keys:[ Input.Meta ] [ char 'a' ];
    step [ Input.Text_input text ] in
  type_text (replace applied ~from:"(seeds : int 8)" ~by:"(seeds : int 7)");
  check (contains (dump ()) "draft yes") ("a draft was not kept\n" ^ dump ());
  check (ws () == original && E.undo_label !env = base) "a draft changed the document";
  (* Discard reverts the draft *)
  click discard;
  check (contains (dump ()) "draft no") "Discard kept the draft";
  check (ws () == original) "Discard changed the document";
  (* an invalid draft stays; the document does not change; the error is at its line *)
  let bad = replace applied ~from:"(sqrt i)" ~by:"(sqrt nosuch)" in
  let line = 1 + List.length (String.split_on_char '\n'
    (String.sub bad 0 (let n = String.length "(sqrt nosuch)" in
       let rec at i = if String.sub bad i n = "(sqrt nosuch)" then i else at (i + 1) in at 0))) - 1 in
  type_text bad;
  click apply;
  let text = dump () in
  check (contains text "draft yes") ("an invalid draft left the pane\n" ^ text);
  check (contains text (Printf.sprintf "error at line %d:" line)) (Printf.sprintf "error not at line %d\n%s" line text);
  check (ws () == original && E.undo_label !env = base) "an invalid apply changed the document";
  (* typing above the mark drops it (it cannot drift away from the line it named); typing below
     it keeps it, on its line *)
  click area; step [ Input.Text_input "\n" ];
  check (not (contains (dump ()) ", error")) ("an edit left its stale error mark\n" ^ dump ());
  click apply;
  check (contains (dump ()) "error at line") "the next apply marks its line again";
  let marked () = List.find (String.starts_with ~prefix:"text: ") (String.split_on_char '\n' (dump ())) in
  let before = marked () in
  click area; step ~keys:[ Input.Meta ] [ key Input.ArrowDown ]; step [ Input.Text_input "\n" ];
  check (contains (marked ()) "error at line" && marked () = before)
    ("typing below the error cleared it or moved it\n" ^ dump ());
  (* a draft that rests is parsed: an unterminated string is reported without an apply, and
     typing again drops the parse of the old text *)
  click discard;
  type_text (applied ^ " \"open");
  check (not (contains (dump ()) ", error")) "a draft was parsed before it rested";
  for _ = 1 to 30 do step [] done;
  check (contains (dump ()) ", error") ("a rested draft was not parsed\n" ^ dump ());
  step [ Input.Text_input "\"" ];
  check (not (contains (dump ()) ", error")) ("the parse of an older text outlived an edit\n" ^ dump ());
  for _ = 1 to 30 do step [] done;
  check (not (contains (dump ()) ", error")) ("a balanced draft was reported\n" ^ dump ());
  click discard;
  (* a valid edit applies atomically, as one history entry *)
  type_text (replace applied ~from:"(seeds : int 8)" ~by:"(seeds : int 7)");
  click apply;
  check (contains (dump ()) "draft no") ("Check & apply kept the draft\n" ^ dump ());
  check (ws () != original && contains (Flow.Lisp.print (ws ()).source |> fst) "(seeds : int 7)")
    "Check & apply did not change the document";
  check (E.undo_label !env = Some "Edit text") "the history label is not Edit text";
  step ~keys:[ Input.Meta ] [ char 'z' ];
  check (Flow.Lisp.print (ws ()).source |> fst = applied) "undo did not restore the text";
  check (E.redo_label !env = Some "Edit text") "redo label";
  (* the Graph tab edits one graph through the same apply; Command-Enter is the button *)
  let original = ws () in
  let tab_graph = tab_at ~narrow:(gw < 400) ~right:(gx + gw - 159) ~top:gy 1 in
  click tab_graph;
  check (contains (dump ()) "graph tab, draft no") ("Graph tab\n" ^ dump ());
  let graph_text = (Rays_editor.Private.Text_pane.make_shown original.source "sunflower" None Graph).text in
  check (contains graph_text "(graph sunflower") "the Graph tab does not show the graph";
  click area;
  step ~keys:[ Input.Meta ] [ char 'a' ];
  step [ Input.Text_input (replace graph_text ~from:"(seeds : int 8)" ~by:"(seeds : int 6)") ];
  check (contains (dump ()) "graph tab, draft yes") ("the Graph tab kept no draft\n" ^ dump ());
  check (ws () == original) "a Graph draft changed the document";
  step ~keys:[ Input.Meta ] [ key Input.Enter ];
  check (contains (dump ()) "graph tab, draft no") ("Command-Enter did not apply\n" ^ dump ());
  check (contains (Flow.Lisp.print (ws ()).source |> fst) "(seeds : int 6)"
         && E.undo_label !env = Some "Edit text") "the Graph tab's apply is not one Edit text entry";
  (* Command-Z past a draft's own undo steps does not undo the document under it *)
  let labelled = E.undo_label !env in
  click area;
  step ~keys:[ Input.Meta ] [ char 'a' ];
  step [ Input.Text_input (replace graph_text ~from:"(seeds : int 8)" ~by:"(seeds : int 5)") ];
  check (contains (dump ()) "graph tab, draft yes") "the draft was not made";
  step ~keys:[ Input.Meta ] [ char 'z' ];
  step ~keys:[ Input.Meta ] [ char 'z' ];
  step ~keys:[ Input.Meta ] [ char 'z' ];
  step [];
  check (E.undo_label !env = labelled && contains (Flow.Lisp.print (ws ()).source |> fst) "(seeds : int 6)")
    ("an extra Command-Z undid the document under a draft: " ^ dump ());
  click discard;
  (* a graph that does not check stays in the pane, the document does not change *)
  let applied_graph = ws () in
  click area;
  step ~keys:[ Input.Meta ] [ char 'a' ];
  step [ Input.Text_input (replace graph_text ~from:"(sqrt i)" ~by:"(sqrt nosuch)") ];
  step ~keys:[ Input.Meta ] [ key Input.Enter ];
  check (contains (dump ()) "graph tab, draft yes, error") ("a refused graph left the pane\n" ^ dump ());
  check (ws () == applied_graph) "a refused Graph apply changed the document";
  click discard;
  let current_graph = (T.make_shown (ws ()).source "sunflower" None Graph).text in
  type_text (replace current_graph ~from:"(seeds : int 6)" ~by:"(seeds : int 3)");
  step [key Input.Escape]; step ~keys:[Input.Meta] [char 'z'];
  let undone = ws () and undo_label = E.undo_label !env in
  click apply;
  check (ws () == undone && E.undo_label !env = undo_label)
    "an old Graph draft overwrote undo";
  check (contains (dump ()) "draft yes" && contains (dump ()) "E_DRAFT_CONFLICT")
    ("an old Graph draft was not retained as a conflict: " ^ dump ());
  click discard;
  click tab_document;
  type_text (replace (Rays_editor.Workspace_doc.to_text (ws ()))
    ~from:"(seeds : int 8)" ~by:"(seeds : int 2)");
  env := E.edit !env (Flow_graph.Flow_edit.Set_input_default {form = "sunflower";
    input = "seeds"; value = Flow.Syntax.make (Flow.Syntax.Num "4")}) |> Result.get_ok;
  let host_edited = ws () and host_label = E.undo_label !env in
  click apply;
  check (ws () == host_edited && E.undo_label !env = host_label)
    "an old Document draft overwrote a host edit";
  check (contains (dump ()) "draft yes" && contains (dump ()) "E_DRAFT_CONFLICT")
    ("an old Document draft was not kept as a conflict: " ^ dump ());
  (* a draft and a host edit of different values merge on the syntax tree: both are kept *)
  click discard;
  type_text (replace (Rays_editor.Workspace_doc.to_text (ws ()))
    ~from:"(spread : float 0.062)" ~by:"(spread : float 0.07)");
  env := E.edit !env (Flow_graph.Flow_edit.Set_input_default {form = "sunflower";
    input = "seeds"; value = Flow.Syntax.make (Flow.Syntax.Num "9")}) |> Result.get_ok;
  click apply;
  let merged = Rays_editor.Workspace_doc.to_text (ws ()) in
  check (contains merged "(seeds : int 9)" && contains merged "(spread : float 0.07)"
         && contains (dump ()) "draft no")
    ("a draft and a host edit of different values did not merge: " ^ dump () ^ merged);
  (* the host adds a keyword argument to a call the draft edits: arguments pair by name *)
  type_text (replace merged ~from:":segments 6" ~by:":segments 8");
  env := E.edit !env (Flow_graph.Flow_edit.Set_arg { node = [ "sunflower"; "seeds_each"; "@result" ]; key = Kw "uniform_scale";
    sub = []; value = Flow.Syntax.make (Flow.Syntax.Num "0.5") }) |> (function Ok e -> e | Error m -> failwith m);
  click apply;
  let merged = Rays_editor.Workspace_doc.to_text (ws ()) in
  check (contains merged ":segments 8" && contains merged ":uniform_scale 0.5" && contains (dump ()) "draft no")
    ("an added keyword argument and a draft did not merge: " ^ dump () ^ merged);
  (* Command-S with the caret in the pane (E10): the draft is applied, then saved (there is no
     source file here, so as a preset) *)
  type_text (replace merged ~from:":segments 8" ~by:":segments 9");
  step ~keys:[ Input.Meta ] [ char 's' ]; step [];
  check (contains (Rays_editor.Workspace_doc.to_text (ws ())) ":segments 9" && contains (dump ()) "draft no")
    ("Command-S with a draft in the text pane did not apply it: " ^ dump ());
  check (List.length (Rays_editor.Private.Preset.list ~directory:presets) = 1)
    "Command-S with the caret in the text pane saved nothing";
  E.close !env; Test_workspace_source.remove_tree presets

(* A checker error from a binding apply is marked on the binding text's line, and typing clears it. *)
let editor_binding () =
  let open Rays in
  let catalog = Editor_document.Contexts.catalog ~version:Flow_sop.Manifest.version
      Sop_catalog.Editor.factories |> Result.get_ok in
  let workspace = Rays_editor.Workspace_doc.of_text catalog (small_sunflower ()) |> Result.get_ok in
  let presets = Filename.temp_dir "rays-text-presets" "" in
  let env = ref (E.create ~ui_text_size:11 ?ui_font:(Sys.getenv_opt "RAYS_UI_FONT") ~presets ~await:true ~workspace
      ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
        |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) () |> Result.get_ok) in
  let count = ref 0 in
  let step ?keys ?(at = (450., 320.)) events =
    incr count; env := E.update !env (frame ~mouse:at ?keys !count events) in
  let click (x, y) = step ~at:(x, y) [ Input.Pointer_moved (x, y) ];
    step ~at:(x, y) [ Input.Pointer_pressed (Input.LeftButton, (x, y));
      Input.Pointer_released (Input.LeftButton, (x, y)) ] in
  let key k = Input.Key_pressed k and char c = Input.Key_pressed (Input.KeyChar c) in
  let dump () =
    let directory = Filename.temp_dir "rays-text-pane" "" in
    Fun.protect ~finally:(fun () ->
      Array.iter (fun f -> Sys.remove (Filename.concat directory f)) (Sys.readdir directory);
      Unix.rmdir directory) (fun () ->
      E.crash_dump !env directory;
      In_channel.with_open_bin (Filename.concat directory "editor.txt") In_channel.input_all) in
  let settle () = for _ = 1 to 4 do step [] done in
  settle ();
  let gx, gy, gw, gh = (E.panes !env (frame 0 [])).graph in
  click (float (gx + 50), float (gy + 100));
  step [ key Input.Home ]; step [ char 'i' ]; settle ();
  (* walk to a binding, then show the text pane: its Selection tab edits the closure shown *)
  for _ = 1 to 4 do step [ key Input.ArrowRight ] done;
  step [ key (Input.KeyChar '/'); char 'l'; char 't' ]; step [];
  check (contains (dump ()) "projection: text") "/ l t did not reach the text pane";
  let area = float (gx + 200), float (gy + 24 + 60) and apply = float (gx + 32), float (gy + gh - (if gw < 400 then 16 else 40)) in
  let type_closure text = click area; step ~keys:[ Input.Meta ] [ char 'a' ]; step [ Input.Text_input text ] in
  let ws () = E.workspace !env in
  let original = ws () in
  (* a binding that does not check: refused, marked on line 1 *)
  type_closure "(let* [head (sop/merge nosuch)] head)";
  click apply;
  check (contains (dump ()) "error at line 1:") ("a refused closure has no line\n" ^ dump ());
  check (ws () == original) "a refused closure changed the document";
  click area; step [ Input.Text_input " " ];
  check (not (contains (dump ()) ", error")) "typing in the closure left its error mark";
  (* a name that is not a root binding is refused with its name *)
  type_closure "(let* [nosuch 1] nosuch)";
  click apply;
  check (contains (dump ()) "nosuch is not a binding of sunflower") ("an unknown binding\n" ^ dump ());
  (* a changed binding is written: one history entry *)
  type_closure "(let* [head (sop/merge seeds_each seeds_each)] seeds_each)";
  click apply;
  check (E.undo_label !env = Some "Edit text" && contains (fst (Flow.Lisp.print (ws ()).source)) "(sop/merge seeds_each seeds_each)")
    ("the closure edit was not applied\n" ^ dump ());
  let before = ws () in
  let printed_binding source path = Option.map (fun (_, v) -> fst (Flow.Lisp.print [v])) (T.binding source path) in
  let omitted = printed_binding original.source ["sunflower"; "seeds_each"] in
  check (omitted <> None && printed_binding before.source ["sunflower"; "seeds_each"] = omitted)
    "Selection removed or changed an omitted binding";
  let refuse text message =
    type_closure text; click apply;
    check (ws () == before) "refused Selection edit changed the document";
    check (contains (dump ()) message) ("missing Selection refusal: " ^ dump ()) in
  refuse "(let* [head (sop/box)] head)" "result must stay unchanged";
  refuse "(let* [head (sop/box) head (sop/box)] seeds_each)" "Duplicate binding";
  (* seeds_each changes from a list to geometry, together with its consumer.
     Neither replacement checks against the old sibling binding. *)
  type_closure "(let* [seeds_each (sop/box) head (sop/transform seeds_each)] seeds_each)";
  click apply;
  let after = ws () in
  check (after != before && contains (dump ()) "draft no")
    ("coherent cross-binding type change was refused: " ^ dump ());
  check (T.binding after.source ["sunflower"; "seeds_each"] <> T.binding before.source ["sunflower"; "seeds_each"])
    "producer was not patched";
  let reloaded = Rays_editor.Workspace_doc.of_text catalog
    (Rays_editor.Workspace_doc.to_text after) |> Result.get_ok in
  check (printed_binding reloaded.source ["sunflower"; "head"] = printed_binding after.source ["sunflower"; "head"])
    "Selection patch did not survive serialized reload";
  click (float (gx + 30), float (gy + gh - 70));
  step [key Input.Escape]; step ~keys:[Input.Meta] [char 'z'];
  check (ws () == before) "Selection's related edits were not one undo entry";
  type_closure "(let* [head (sop/merge seeds_each)] seeds_each)";
  env := E.edit !env (Flow_graph.Flow_edit.Set_arg {node = ["sunflower"; "head"];
    key = Whole; sub = []; value = (match Flow.Syntax.parse "(sop/merge seeds_each seeds_each seeds_each)" with
      | Ok [form] -> form | _ -> fail "test expression")}) |> Result.get_ok;
  let host_edited = ws () and host_label = E.undo_label !env in
  click apply;
  check (ws () == host_edited && E.undo_label !env = host_label)
    ("an old Selection draft overwrote a host edit: " ^ dump ());
  check (contains (dump ()) "draft yes" && contains (dump ()) "E_DRAFT_CONFLICT")
    ("an old Selection draft was not kept as a conflict: " ^ dump ());
  E.close !env; Test_workspace_source.remove_tree presets

(* The editor's Lisp as the text area's language: indentation and bracket pairs. *)
(* E6: a paste pairs names with bindings by position and renames at once *)
let paste () =
  let catalog = Editor_document.Contexts.catalog ~version:Flow_sop.Manifest.version
      Sop_catalog.Editor.factories |> Result.get_ok in
  let workspace = Rays_editor.Workspace_doc.of_text catalog
      "(workspace w (graph g :context sop (let* [bed (sop/box)] bed)))" |> Result.get_ok in
  let pasted text =
    let ops = match T.paste_ops workspace.source ~graph:"g" ~scope:[ "g" ] text with
      | Ok ops -> ops | Error message -> fail message in
    let after = List.fold_left (fun ws op ->
      match Rays_editor.Workspace_doc.edit catalog ws op with
      | Ok ws -> ws | Error d -> fail ("paste of " ^ text ^ ": " ^ Diagnostic.to_string d)) workspace ops in
    String.concat " " (List.filter (( <> ) "") (String.split_on_char ' '
      (String.map (function '\n' -> ' ' | c -> c) (Rays_editor.Workspace_doc.to_text after)))) in
  (* two bare forms with one head are two bindings *)
  let two = pasted "(sop/box) (sop/box :size [2 2 2])" in
  check (contains two "box (sop/box)" && contains two "box_1 (sop/box :size [2 2 2])") ("two forms of one head: " ^ two);
  (* [bed] becomes [bed_2]; the pasted [bed_2] is another binding and keeps reading the pasted bed *)
  let chain = pasted "bed (sop/box) bed_2 (sop/transform bed)" in
  check (contains chain "bed_2 (sop/box)" && contains chain "bed_2_1 (sop/transform bed_2)")
    ("a pasted name that is another's new name: " ^ chain)

let lisp_text () =
  let module L = Rays_editor.Private.Lisp_text in
  let indent text = String.length (L.indent text (String.length text)) in
  check (indent "(let* [a 1" = 7) "a vector's elements line up under the first";
  check (indent "(graph g :context sop" = 2) "a body form indents two in";
  check (indent "  (let* [a 1]" = 4) "let* is a body form";
  check (indent "(sop/box :size 1" = 9) "a call's arguments line up under the first";
  check (indent "(sop/merge" = 1) "a call broken after its head indents one in";
  check (indent "(let* [a 1] (+ 1 2)) " = 0) "a closed form indents nothing";
  check (indent "(+ 1 ; comment (\n" = 3) "an open bracket in a comment does not count";
  check (indent "(str \"(\" " = 5) "an open bracket in a string does not count";
  check (indent "(str \"\xc3\xa9\" (+ 1" = 12) "indentation counts characters, not bytes";
  let in_ text marker =
    let at = String.index text marker in L.context_at (String.sub text 0 at ^ String.sub text (at + 1) (String.length text - at - 1)) at in
  check (in_ "(f |x)" '|' = `Code) "the code between brackets is code";
  check (in_ "(f \"a|b\")" '|' = `String) "inside a string";
  check (in_ "(f \"ab|\")" '|' = `String) "before the closing quote is still inside the string";
  check (in_ "(f \"ab\"|)" '|' = `Code) "after the closing quote is code";
  check (in_ "(f 1) ; a|b" '|' = `Comment) "inside a comment";
  check (in_ "(f 1) ; ab|" '|' = `Comment) "the end of a comment's line is inside it";
  check (in_ "(f 1) ;| ab\n(g)" '|' = `Comment) "right after the semicolon is inside the comment";
  check (in_ "(f 1) ; ab\n|(g)" '|' = `Code) "the next line is code";
  check (L.brackets "(a [b] c)" = [ (0, 8); (3, 5) ] || L.brackets "(a [b] c)" = [ (3, 5); (0, 8) ]) "matched pairs";
  check (L.brackets "(a ]" = []) "a mismatched closer pairs with nothing";
  (* the lexing is kept per text version: a changed text, and the same text again, read right *)
  let lang = L.language Pxui.Theme.default in
  check (lang.comment = ";") "the Lisp comments with a semicolon";
  let a_ = lang.colorize "(a b)" in
  check (lang.colorize "(a b c d)" <> a_ && lang.colorize "(a b)" = a_ && L.brackets "(a b c d)" = [ (0, 8) ]
         && L.brackets "(a b)" = [ (0, 4) ]) "a cached lexing answered for another text";
  let colors = (L.language Pxui.Theme.default).colorize "(sop/box :size 1.5 \"s\" ; c" in
  check (List.length colors = 6) (Printf.sprintf "head, keyword, number, string, comment and the paren: %d runs" (List.length colors));
  let malformed = "(f \"bad\\q\" :next)" in
  let later = String.index malformed ':' in
  check (List.exists (fun (start, stop, _) -> start <= later && later < stop)
      (lang.colorize malformed)) "the tolerant text lexer colors after a malformed escape";
  (* completions over the catalog: ranked, for the token at the caret *)
  let vocab = L.vocab (Flow_sop.Catalog.with_library
      (List.map Flow_sop.Catalog.descriptor Sop_catalog.Editor.factories)
    @ Editor_document.Contexts.descriptors) in
  let labels text = List.map (fun (c : Pxui.Ui.completion) -> c.label) (L.complete vocab text (String.length text)) in
  let first text = match labels text with l :: _ -> l | [] -> "" in
  let doc = "(workspace w (graph g :context sop [(depth : int 5)] (let* [ring (sop/box :size_x 2) b (" in
  (* a head: the context's kinds first, prefix matches before fuzzy ones *)
  check (first (doc ^ "sop/tra") = "sop/transform") ("head completion: " ^ first (doc ^ "sop/tra"));
  check (List.for_all (fun l -> String.length l >= 7 && String.sub l 0 7 = "sop/tra"
      || Pxui.Ui.fuzzy_match ~query:"sop/tra" l) (labels (doc ^ "sop/tra"))) "every head completion matches";
  check (List.mem "let*" (labels (doc ^ "le")) && List.mem "sop/merge" (labels (doc ^ "me")))
    "forms and kinds complete at a head";
  (* every special form of the checker completes at a head, and nothing the language lacks does *)
  List.iter (fun form -> check (List.mem form (labels (doc ^ form))) ("the form " ^ form ^ " does not complete"))
    Flow.Workspace.special_forms;
  check (not (List.mem "when" (labels (doc ^ "whe"))) && indent "(do" = 1 && indent "(let*" = 2)
    "a form the language does not have is offered or indented as one";
  (* an operator's own name being typed has no argument place yet (E1: it raised) *)
  check (List.mem "max" (labels (doc ^ "max")) && List.mem "+" (labels (doc ^ "+")))
    "an operator's head completes";
  (* the empty head lists the kinds the text uses first *)
  check (first doc = "sop/box") ("the used kind ranks first: " ^ first doc);
  (* a keyword: the kind's parameters, a vec3 group before its components, present ones left out *)
  check (first (doc ^ "sop/box :si") = ":size") ("keyword completion: " ^ first (doc ^ "sop/box :si"));
  check (List.mem ":size_x" (labels (doc ^ "sop/box :si"))) "the group's components follow";
  check (not (List.mem ":size" (labels (doc ^ "sop/box :size [1 1 1] :s")))) "a keyword already given is left out";
  check (first (doc ^ "sop/transform ring :rot") = ":rotate") ("rotate group: " ^ first (doc ^ "sop/transform ring :rot"));
  (* a choice after its keyword, with or without the opening quote *)
  check (List.mem "Quads" (labels (doc ^ "sop/box :connectivity \"Qu")) && List.mem "Quads" (labels (doc ^ "sop/box :connectivity Qu")))
    "choices complete after their keyword";
  (* the text choices of the layout forms: a keyword's, and the split axis at its place *)
  let ui = "(workspace w (graph e :context editor (" in
  let sorted text = List.sort compare (labels text) in
  check (sorted (ui ^ "ui/lisp :tab \"") = [ "document"; "graph"; "selection" ])
    ("a lisp panel's tabs complete: " ^ String.concat "," (labels (ui ^ "ui/lisp :tab \"")));
  check (labels (ui ^ "ui/graph :view \"l") = [ "list" ]) "a graph panel's views complete";
  check (sorted (ui ^ "ui/split \"") = [ "horizontal"; "vertical" ]
         && labels (ui ^ "ui/split-at \"v") = [ "vertical" ]) "the split axis completes";
  check (labels (ui ^ "ui/split \"vertical\" a b :first_size \"") = []) "a size completes no text";
  (match L.complete vocab (doc ^ "sop/box :connectivity \"Qu") (String.length doc + 24) with
   | c :: _ -> check (c.insert = "\"Quads\"") ("a choice inserts its quotes: " ^ c.insert)
   | [] -> fail "no choice");
  (* an argument: the bindings and inputs in scope before constants; naming a new binding offers nothing *)
  check (first (doc ^ "sop/transform ri") = "ring") ("binding completion: " ^ first (doc ^ "sop/transform ri"));
  check (List.mem "depth" (labels (doc ^ "sop/transform ring :uniform_scale de"))) "graph inputs complete";
  check (List.mem "t" (labels (doc ^ "sop/transform ring :uniform_scale t"))) "t completes";
  check (labels "(workspace w (graph g :context sop [] (let* [ri" = []) "a new binding's name is not completed";
  check (labels (doc ^ "sop/box :size 1") = []) "a number completes nothing";
  (* the operator's keyword and the context keyword *)
  check (List.mem ":closed" (labels (doc ^ "sop/curve ring :cl"))) "an operator's keyword";
  check (first "(workspace w (graph g :context sc" = "scene") "contexts after :context";
  (* descriptions *)
  let describe text at = match L.describe vocab text at with Some (_, _, d) -> d | None -> "" in
  let index_of part = let n = String.length part in
    let rec at i = if String.sub doc i n = part then i else at (i + 1) in at 0 in
  check (contains (describe doc (index_of "sop/box" + 2)) "Box") ("a kind's description: " ^ describe doc (index_of "sop/box" + 2));
  let at = index_of ":size_x" + 3 in
  check (contains (describe doc at) "Size X") ("a parameter's description: " ^ describe doc at);
  check (contains (describe (doc ^ "sop/box :size [1 1 1])") (String.length doc + 10)) "Size")
    ("a group's description: " ^ describe (doc ^ "sop/box :size [1 1 1])") (String.length doc + 10));
  check (contains (describe doc 15) "network") ("graph form: " ^ describe doc 15);
  check (describe doc (String.length doc - 1) = "") "an open paren has no description";
  check (contains (describe (doc ^ "sop/transform ring)") (String.length doc + 15)) "let* binding")
    ("a binding's description: " ^ describe (doc ^ "sop/transform ring)") (String.length doc + 15));
  check (contains (describe doc (index_of "int 5" + 4)) "drag") ("a number's description: " ^ describe doc (index_of "int 5" + 4));
  (* numbers *)
  check (L.number_at "(sop/box :size_x 1.25 :x 3)" 19 = Some (17, 21)) "the number at a byte";
  check (L.number_at "(sop/box :size_x 1.25 :x 3)" 16 = None) "a space is no number";
  (* parinfer: closers follow indentation, the printed workspaces are already in that shape *)
  let p text = fst (L.parinfer_text text 0) in
  List.iter (fun name ->
    let printed = fst (Flow.Lisp.print (source name)) in
    check (p printed = printed) (name ^ ": parinfer changed the printed workspace")) [ "bloom"; "sunflower"; "rosette" ];
  check (p "(a\n  b" = "(a\n  b)") "an open form closes at its last line";
  check (p "(a\nb)" = "(a)\nb") "a dedented line closes the form before it";
  check (p "(a (b)\n  c)" = "(a (b)\n  c)") "a closer in the middle of a line stays";
  check (p "(a \"(\" ; )\n  b" = "(a \"(\" ; )\n  b)") "strings and comments are not brackets";
  check (p "(a]" = "(a)") "a closer matching nothing is replaced";
  (* E22: a string runs over line breaks: nothing inside it is a bracket or an indentation *)
  check (p "(a \"x\n(y]\" b" = "(a \"x\n(y]\" b)") ("a string over two lines: " ^ p "(a \"x\n(y]\" b");
  check (p "(a\n  (b \"x\ny\")\n(c)" = "(a\n  (b \"x\ny\"))\n(c)") ("closers after a string's last line: " ^ p "(a\n  (b \"x\ny\")\n(c)");
  check (L.brackets "(a \"x\n)\" b)" = [ (0, 10) ]) "a bracket on a string's second line is not one";
  check (p "(let* [a 1\n       b 2]\n  a)" = "(let* [a 1\n       b 2]\n  a)") "a vector over two lines";
  check (p "(a\n  )" = "(a)\n") "a line of closers alone becomes blank";
  check (L.parinfer_text "(a \n  )" 6 = ("(a) \n  ", 7)) "Enter before a closer keeps the caret's indentation";
  check (L.parinfer_text "(a" 2 = ("(a)", 2)) "the caret stays before an inferred closer";
  check (L.parinfer_text "(a\nb)" 4 = ("(a)\nb", 5)) "the caret follows a moved closer";
  check (L.parinfer_text "(a  )" 3 = ("(a )", 3)) "a blank typed before a closer survives on the caret's line";
  check ((L.language ~parinfer:true Pxui.Theme.default).rewrite <> None
         && (L.language Pxui.Theme.default).rewrite = None) "the language rewrites only with parinfer on"

(* W9 through the editor: a 1400x800 window, Rosette, the graph
   pane zoomed once (frame 12) so the cards sit where FLOW_W9 clicks them: the B flag of
   `soft`, its note in the inspector, and the make-macro dialog (m, Create macro).  The frame's
   mouse stays at the window centre (the pointer is the events'), as the export driver's does. *)
let editor_w9 () =
  let open Rays in
  let catalog = Editor_document.Contexts.catalog ~version:Flow_sop.Manifest.version
      Sop_catalog.Editor.factories |> Result.get_ok in
  let soft = [ "rosette"; "soft" ] and petals = [ "rosette"; ":petals" ] in
  let workspace = Rays_editor.Workspace_doc.of_text catalog (small_rosette ()) |> Result.get_ok in
  let presets = Filename.temp_dir "rays-text-presets" "" in
  let start () = E.create ~ui_text_size:11 ?ui_font:(Sys.getenv_opt "RAYS_UI_FONT") ~presets ~await:true ~workspace
        ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
          |> Result.map_error Rdk.Error.to_string)
        ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) () |> Result.get_ok in
  let mouse = ref (640., 360.) in
  (* Every scenario's first edit is at frame 22 or later and an edit costs a workspace lowering (1.3 s), so the
     frames before it (the zoom, the selection) run once and each scenario continues from their result (the UI handle is
     shared, so the scenarios run in an order that leaves no dialog or scroll behind). *)
  let frames env0 first last script =
    let env = ref env0 in
    for n = first to last do
      (* a card's header, where the graph pane put it (kit rev 3 cards: 24-point header) *)
      let head path (dx, dy) = match E.node_box !env path with
        | Some (x, y, w, _) -> float x +. (if dx < 0. then float w +. dx else dx), float y +. dy
        | None -> failwith "the card has no box" in
      let click p =
        [ Input.Pointer_moved p; Input.Pointer_pressed (Input.LeftButton, p); Input.Pointer_released (Input.LeftButton, p) ] in
      let events = match List.assoc_opt n script with
        | Some events -> events click head
        | None -> match n with
          | 4 -> click (900., 304.) | 6 -> [ Input.Key_pressed Input.ArrowDown ]
          | 8 -> [ Input.Key_pressed (Input.KeyChar 'i') ]
          | 12 -> [ Input.Pointer_moved (780., 380.); Input.Scrolled (0., 9.) ]
          | 16 -> click (960., 470.)
          | _ -> [] in
      let f : Frame.t = { input = { (Input.at_time (float n /. 60.)) with
          dt = 1. /. 60.; frame = n; tick = n; size = 1400, 800; pointer = !mouse;
          events };
        pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. } in
      env := E.update !env f
    done;
    !env in
  let prefix = frames (start ()) 1 21 [] in
  let scenario ?(prefix = prefix) script =
    let env = frames prefix 22 40 script in
    Flow.Lisp.print (E.workspace env).source |> fst, E.undo_label env in
  let untouched = Flow.Lisp.print (E.workspace prefix).source |> fst in
  check (contains untouched "^:bypass (sop/subdivide inner") "soft starts bypassed";
  (* the flag *)
  let text, label = scenario [ 22, (fun click head -> click (head soft (60., 12.))); 24, (fun _ _ -> [ Input.Key_pressed (Input.KeyChar 'b') ]) ] in
  check (not (contains text "^:bypass") && label = Some "Bypass")
    (Printf.sprintf "the b key: %s, bypass %b" (Option.value label ~default:"-") (contains text "^:bypass"));
  (* the inspector's Bypass button is the key's request too: the 280-wide inspector starts at
     x 1120, under its 28-point header; its head is 153 tall (chips, name, detail, buttons), the
     buttons row 124 down, View 51.5 wide, Bypass after it and 4 more: its middle is
     1120 + 63.5 + 32; the name is the display line at 64..112 of the head; a graph input's head is
     125 tall, so its first row is 12 into the rows under it, and the first row of a node is the note *)
  let text, label = scenario [ 22, (fun click head -> click (head soft (60., 12.))); 26, (fun click _ -> click (1216., 162.)) ] in
  check (not (contains text "^:bypass") && label = Some "Bypass")
    (Printf.sprintf "the inspector's Bypass toggle: %s" (Option.value label ~default:"-"));
  (* and its name field is the pane's rename *)
  let text, label = scenario [ 22, (fun click head -> click (head soft (60., 12.))); 26, (fun click _ -> click (1220., 92.));
    28, (fun _ _ -> [ Input.Key_pressed (Input.KeyChar 'a') ]); 30, (fun _ _ -> [ Input.Text_input "gentle" ]);
    32, (fun _ _ -> [ Input.Key_pressed Input.Enter ]) ] in
  check (contains text "gentle ^:bypass (sop/subdivide" && not (contains text "soft ^:bypass") && label = Some "Rename")
    ("the inspector's name field: " ^ Option.value label ~default:"-");
  (* a graph input's default is edited in the inspector too: a number scrubs there as on its card, so
     typing is the kit's Option-click *)
  let text, label = scenario [ 22, (fun click head -> click (head petals (60., 12.)));
    26, (fun click _ -> Input.Key_pressed Input.Alt :: click (1290., 165.) @ [ Input.Key_released Input.Alt ]);
    28, (fun _ _ -> [ Input.Key_pressed (Input.KeyChar 'a') ]); 30, (fun _ _ -> [ Input.Text_input "3" ]);
    32, (fun _ _ -> [ Input.Key_pressed Input.Enter ]) ] in
  check (contains text "(petals : int 3)" && label = Some "Input default")
    ("the inspector's input default: " ^ Option.value label ~default:"-");
  (* the dialog: m over the selection, then Create macro *)
  let text, label = scenario [ 22, (fun click head -> click (head soft (60., 12.)));
    26, (fun _ _ -> [ Input.Key_pressed (Input.KeyChar 'm') ]); 30, (fun click _ -> click (500., 461.)) ] in
  check (contains text "(defmacro soft_tpl [p1 inner]" && label = Some "Make macro")
    ("the make-macro dialog: " ^ Option.value label ~default:"-");
  (* Enter in the name field creates the macro too; the dialog keeps its state in the UI handle the frames share, so it starts from a handle of its own *)
  let text, label = scenario ~prefix:(frames (start ()) 1 21 []) [ 22, (fun click head -> click (head soft (60., 12.)));
    26, (fun _ _ -> [ Input.Key_pressed (Input.KeyChar 'm') ]); 30, (fun click _ -> click (700., 437.));
    34, (fun _ _ -> [ Input.Key_pressed Input.Enter ]) ] in
  check (contains text "(defmacro soft_tpl [p1 inner]" && label = Some "Make macro")
    ("Enter in the make-macro dialog: " ^ Option.value label ~default:"-");
  (* the inspector note: click the field, type, Enter *)
  (* the note is the last section, closed, and below the fold of a node with this many rows: the
     wheel over a value field scrolls the inspector to its end, where the 24-point Note header sits
     on the pane's bottom edge (y 776 less the 1-point rule: middle 763); open, the same scroll puts
     its row there.  Two notches a frame reach the end (59 points, 83 with the note open) and go
     little past it: the inspector stretches past its end as every scroll box does, so a hard wheel
     would hold the rows off their places for the frames this script has *)
  let wheel _ _ = [ Input.Pointer_moved (1260., 405.); Input.Scrolled (0., -2.) ] in
  let text, label = scenario [ 22, (fun click head -> click (head soft (60., 12.)));
    24, wheel; 25, wheel; 26, (fun click _ -> click (1250., 763.));
    28, wheel; 29, wheel; 30, (fun click _ -> click (1290., 763.));
    32, (fun _ _ -> [ Input.Key_pressed (Input.KeyChar 'a') ]); 34, (fun _ _ -> [ Input.Text_input "a fresh note" ]);
    36, (fun _ _ -> [ Input.Key_pressed Input.Enter ]) ] in
  check (contains text "; a fresh note" && label = Some "Note") ("the note edit: " ^ Option.value label ~default:"-")

(* Scrubbing :active in the Document text: dragging the number after [:active] sideways switches
   the layout on every frame of the drag and the whole drag is one "Edit text" entry. *)
let editor_active_scrub () =
  let open Rays in
  let catalog = Editor_document.Contexts.catalog ~version:Flow_sop.Manifest.version
      Sop_catalog.Editor.factories |> Result.get_ok in
  let text = {|(workspace sw
  (graph g :context sop (sop/box))
  (graph scene :context scene (scene/merge (scene/geometry (ref g))))
  (graph editor :context editor
    (let* [wide (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp))
           split (ui/split-at "horizontal" 0.5 (ui/graph) (ui/inspector))
           three (ui/split-at "horizontal" 0.3 (ui/inspector) (ui/graph))]
      (ui/workspace (ui/switch wide split three :active 0)))))|} in
  let workspace = Rays_editor.Workspace_doc.of_text catalog text |> Result.get_ok in
  let presets = Filename.temp_dir "rays-text-presets" "" in
  let env = ref (E.create ~ui_text_size:11 ?ui_font:(Sys.getenv_opt "RAYS_UI_FONT") ~presets ~await:true ~workspace
      ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
        |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) () |> Result.get_ok) in
  let count = ref 0 and mouse = ref (450., 320.) in
  let step ?(buttons = []) ?(keys = []) events =
    incr count;
    let frame = frame ~mouse:!mouse ~keys !count events in
    env := E.update !env { frame with input = { frame.input with buttons } } in
  let at ?buttons point events = mouse := point; step ?buttons events in
  let char c = Input.Key_pressed (Input.KeyChar c) in
  for _ = 1 to 4 do step [] done;
  let gx, gy, gw, gh = (E.panes !env (frame 0 [])).graph in
  (* the Lisp panel sits under the graph: a splitter of one point, the 4-point margin and the 24-point header *)
  let gy = gy + gh + 1 + 28 in
  (* the Document tab *)
  let tab = tab_at ~narrow:(gw < 400) ~right:(gx + gw - 36) ~top:gy 2 in
  at tab [ Input.Pointer_moved tab ];
  at tab [ Input.Pointer_pressed (Input.LeftButton, tab); Input.Pointer_released (Input.LeftButton, tab) ]; step [];
  let printed = Flow.Lisp.print (E.workspace !env).source |> fst in
  let lines = String.split_on_char '\n' printed in
  let line = Option.get (List.find_index (fun l -> contains l ":active 0") lines) in
  let col = let l = List.nth lines line in
    let rec find i = if String.sub l i 9 = ":active 0" then i + 8 else find (i + 1) in find 0 in
  (* the digit sits [col] glyphs into its line: the 36-point gutter, the 6 points above the first row,
     the line's indentation; a press within half a glyph of its left edge is on it *)
  let char_w = 6.95 in
  (* a number is dragged by its underline, the low part of its row *)
  let number = float gx +. 36. +. float col *. char_w, float gy +. 6. +. 8. +. float line *. line_pitch in
  let before = E.workspace !env and history = E.undo_label !env in
  let nx, ny = number in
  at number [ Input.Pointer_moved number ];
  at number ~buttons:[ Input.LeftButton ] [ Input.Pointer_pressed (Input.LeftButton, number) ];
  at (nx +. 10., ny) ~buttons:[ Input.LeftButton ] [ Input.Pointer_moved (nx +. 10., ny) ];
  check (contains (Flow.Lisp.print (E.workspace !env).source |> fst) ":active 2")
    ("a drag of ten points did not set :active 2 live: " ^ (Flow.Lisp.print (E.workspace !env).source |> fst));
  at (nx +. 10., ny) [ Input.Pointer_released (Input.LeftButton, (nx +. 10., ny)) ]; step [];
  check (E.undo_label !env = Some "Edit text") ("the drag is one Edit text entry: " ^ Option.value ~default:"-" (E.undo_label !env));
  step ~keys:[ Input.Meta ] [ char 'z' ];
  check (E.workspace !env == before && E.undo_label !env = history) "one undo returns the whole drag";
  E.close !env

let editor_literal_scrub () =
  let open Rays in
  let text = {|(workspace literal
    (graph g :context sop (let* [a (sop/points :points 1)] a))
    (graph scene :context scene (scene/merge (scene/geometry (ref g))))
    (graph editor :context editor
      (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph "g") (ui/lisp)))))|} in
  let catalog = Editor_document.Contexts.catalog ~version:1 Sop_catalog.Editor.factories |> Result.get_ok in
  List.iter (fun (tab_index, tab) ->
    let workspace = Rays_editor.Workspace_doc.of_text catalog text |> Result.get_ok in
    let presets = Filename.temp_dir "rays-token-presets" "" in
    let env = ref (E.create ~ui_text_size:11 ?ui_font:(Sys.getenv_opt "RAYS_UI_FONT") ~presets ~await:true ~domains:1 ~workspace ~prepare:(fun _ _ -> Ok ())
      ~draw:(fun _ () -> Scene.D3.empty) () |> Result.get_ok) in
    Fun.protect ~finally:(fun () -> E.close !env) (fun () ->
      let count = ref 0 and mouse = ref (450.,320.) in
      let step ?(buttons = []) ?(keys = []) events =
        incr count; let frame = frame ~mouse:!mouse ~keys !count events in
        env := E.update !env { frame with input = { frame.input with buttons } } in
      let at ?buttons point events = mouse := point; step ?buttons events in
      for _ = 1 to 4 do step [] done;
      let x,y,w,h = E.node_box !env ["g";"a"] |> Option.get in
      let point = float (x + w / 2), float (y + min 12 (h / 2)) in
      at point [Input.Pointer_moved point];
      at point [Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point)]; step [];
      let gx,gy,gw,gh = (E.panes !env (frame 0 [])).graph in
      let top = gy + gh + 1 + 28 in
      let tab_point = tab_at ~narrow:(gw < 400) ~right:(gx + gw - 36) ~top tab_index in
      at tab_point [Input.Pointer_moved tab_point];
      at tab_point [Input.Pointer_pressed (Input.LeftButton, tab_point); Input.Pointer_released (Input.LeftButton, tab_point)]; step [];
      let shown = T.make_shown (E.workspace !env).source "g" (Some ["g";"a"]) tab in
      let value = Flow_graph.Flow_edit.arg_text shown.source ["g";"a"] (Kw "points") |> Option.get in
      let span = List.assoc value.id shown.spans in
      let position = Diagnostic.position_of_offset shown.text span.start in
      let number = float gx +. 36. +. float (position.col - 1) *. 6.95 +. 1.,
        float top +. 14. +. float (position.line - 1) *. line_pitch in
      let nx,ny = number in
      let before = E.workspace !env and label = E.undo_label !env in
      at number [Input.Pointer_moved number];
      at number ~buttons:[Input.LeftButton] [Input.Pointer_pressed (Input.LeftButton, number)];
      let drag dx = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
        at (nx +. dx,ny) ~buttons:[Input.LeftButton] [Input.Pointer_moved (nx +. dx,ny)]) |> snd in
      let first = drag 10. in
      List.iter (fun phase -> check (Flow.Phase_timer.calls first phase = 0)
        ("text token scrub ran " ^ phase)) ["print"; "parse"; "check"; "evaluate"; "lower"; "project"; "layout"];
      let next = drag 20. in
      List.iter (fun phase -> check (Flow.Phase_timer.calls next phase = 0)
        ("repeated text token scrub ran " ^ phase)) ["print"; "parse"; "check"; "evaluate"; "lower"; "project"; "layout"];
      let wider = drag 220. in
      List.iter (fun phase -> check (Flow.Phase_timer.calls wider phase = 0)
        ("wider text token scrub ran " ^ phase)) ["print"; "parse"; "check"; "evaluate"; "lower"; "project"; "layout"];
      let valid = E.workspace !env in
      check ((Flow_graph.Flow_edit.arg_text valid.source ["g";"a"] (Kw "points") |> Option.get).node = Flow.Syntax.Num "45")
        "a growing token lost its repaired span";
      let source, checked = Flow_graph.Flow_edit.apply_checked catalog before.source
        (Set_arg {node = ["g";"a"]; key = Kw "points"; sub = []; value = Flow.Syntax.make (Num "45")}) |> Result.get_ok in
      check (Rays_editor.Workspace_doc.to_text valid = Rays_editor.Workspace_doc.to_text {valid with source; checked})
        "fast text token scrub differs from the full syntax edit";
      ignore (drag 520.); (* 105 exceeds the field's hard maximum 50. *)
      check (E.workspace !env == valid) "an invalid text token scrub changed the document";
      ignore (drag 20.); (* Recovery goes through the retained draft's existing check path. *)
      let ws = E.workspace !env in
      let points = Flow_graph.Flow_edit.arg_text ws.source ["g";"a"] (Kw "points") |> Option.get in
      check (points.node = Flow.Syntax.Num "5") (Printf.sprintf "text token scrub in tab %d did not apply its latest number: %s (at %.1f,%.1f; text %S)"
        tab_index (Flow.Lisp.flat points) nx ny shown.text);
      let source, checked = Flow_graph.Flow_edit.apply_checked catalog before.source
        (Set_arg {node = ["g";"a"]; key = Kw "points"; sub = []; value = Flow.Syntax.make (Num "5")}) |> Result.get_ok in
      check (Rays_editor.Workspace_doc.to_text ws = Rays_editor.Workspace_doc.to_text {ws with source; checked})
        "text token scrub differs from the full syntax edit";
      at (nx +. 20.,ny) [Input.Pointer_released (Input.LeftButton, (nx +. 20.,ny))]; step [];
      check (E.undo_label !env = Some "Edit text") "text token scrub lost its history label";
      (* Leave the text area's own edit-undo focus before the document undo. *)
      let focus_graph = float (gx + gw - 12), float (gy + min 10 (gh - 1)) in
      at focus_graph [Input.Pointer_moved focus_graph];
      at focus_graph [Input.Pointer_pressed (Input.LeftButton, focus_graph); Input.Pointer_released (Input.LeftButton, focus_graph)]; step [];
      step ~keys:[Input.Meta] [Input.Key_pressed (Input.KeyChar 'z')];
      check (E.workspace !env == before && E.undo_label !env = label)
        (Printf.sprintf "text token scrub in tab %d was not one undo entry: identity %b, label %s (was %s), redo %b, points %s"
          tab_index (E.workspace !env == before) (Option.value ~default:"-" (E.undo_label !env))
          (Option.value ~default:"-" label) (E.can_redo !env)
          (Flow_graph.Flow_edit.arg_text (E.workspace !env).source ["g";"a"] (Kw "points") |> Option.get |> Flow.Lisp.flat))))
    [0,T.Selection; 1,T.Graph; 2,T.Document]

(* A graph held by [y] and put on the text pane: [(ref a)] is inserted at the byte under the pointer
   and the text must check (Graph tab: one Set_graph; Document tab: the whole text), else refused
   with the checker's words; an unapplied draft refuses it too. *)
(* [~carry_budget:infinity]: a carry shows its preview only when applying and the cook took under the budget (0.5 s of
   the real clock); on a loaded machine the cook of this workspace exceeded it and the preview was only described. *)
let editor_text_drop () =
  let open Rays in
  let catalog = Editor_document.Contexts.catalog ~version:Flow_sop.Manifest.version
      Sop_catalog.Editor.factories |> Result.get_ok in
  let text = {|(workspace drop
  (graph a :context sop (sop/box))
  (graph b :context sop (sop/merge (sop/box)))
  (graph scene :context scene (scene/merge (scene/geometry (ref b))))
  (graph editor :context editor
    (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp)))))|} in
  let workspace = Rays_editor.Workspace_doc.of_text catalog text |> Result.get_ok in
  let presets = Filename.temp_dir "rays-text-presets" "" in
  let env = ref (E.create ~ui_text_size:11 ?ui_font:(Sys.getenv_opt "RAYS_UI_FONT") ~presets ~await:true ~carry_budget:Float.infinity ~workspace
      ~prepare:(fun _ output -> Rdk_rays.Rays_mesh.to_mesh (Result.get_ok (Sop.Payload.geometry output.Sop.Session.payload))
        |> Result.map_error Rdk.Error.to_string)
      ~draw:(fun _ mesh -> Scene.D3.create [ Scene.D3.mesh mesh ]) () |> Result.get_ok) in
  let count = ref 0 and mouse = ref (450., 320.) in
  let step ?(buttons = []) ?(keys = []) events =
    incr count;
    let frame = frame ~mouse:!mouse ~keys !count events in
    env := E.update !env { frame with input = { frame.input with buttons } } in
  let at ?buttons point events = mouse := point; step ?buttons events in
  let key k = Input.Key_pressed k and char c = Input.Key_pressed (Input.KeyChar c) in
  let jump name =
    step [ key (Input.KeyChar '/'); char 'j' ]; step [ Input.Text_input name ]; step [ key Input.Enter ]; step [] in
  let line () = Option.value ~default:"-" (E.carry_line !env) in
  let source () = Flow.Lisp.print (E.workspace !env).source |> fst in
  for _ = 1 to 4 do step [] done;
  let gx, gy, gw, gh = (E.panes !env (frame 0 [])).graph in
  let top = gy + gh + 1 + 28 in
  let click point = at point [ Input.Pointer_moved point ];
    at point [ Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point) ];
    step [] in
  (* the Graph tab, graph b open, graph a held *)
  jump "b";
  click (tab_at ~narrow:(gw < 400) ~right:(gx + gw - 36) ~top 1);
  jump "a";
  step [ char 'y' ]; step [];
  check (E.carrying !env = Some ("sop", "(ref a)")) "y holds graph a";
  jump "b";
  check (E.carrying !env <> None) "the carry survives the jump";
  let original = E.workspace !env and history = E.undo_label !env in
  let shown tab =
    (Rays_editor.Private.Text_pane.make_shown (E.workspace !env).source "b" None tab).text in
  let where tab needle =
    let lines = String.split_on_char '\n' (shown tab) in
    let line = Option.get (List.find_index (fun l -> contains l needle) lines) in
    let l = List.nth lines line in
    let rec find i = if String.sub l i (String.length needle) = needle then i else find (i + 1) in
    line, find 0 in
  let char_w = 6.95 in
  let point tab needle ~after =
    let line, col = where tab needle in
    float gx +. 36. +. float (col + after) *. char_w, float top +. 6. +. 2. +. float line *. line_pitch in
  (* hover between the call and its first argument: the text is the edit, the strip says where *)
  let over = point Rays_editor.Private.Text_pane.Graph "(sop/merge" ~after:10 in
  at over [ Input.Pointer_moved over ]; at over [];
  check (contains (line ()) "(ref a) at line") ("the strip says where it goes: " ^ line ());
  check (contains (source ()) "(sop/merge (ref a) (sop/box)") ("the preview is the edit: " ^ source ());
  check (E.undo_label !env = history) "a preview writes nothing to the history";
  (* a release is the put: one entry named Put *)
  at over [ Input.Pointer_pressed (Input.LeftButton, over); Input.Pointer_released (Input.LeftButton, over) ]; step []; step [];
  check (E.carrying !env = None && E.undo_label !env = Some "Put" && contains (source ()) "(sop/merge (ref a) (sop/box)")
    ("a release writes it as one Put entry: " ^ source ());
  step ~keys:[ Input.Meta ] [ char 'z' ];
  check (E.workspace !env == original) "one undo gives it back";
  (* inside a keyword the text does not check: refused, nothing written *)
  jump "a"; step [ char 'y' ]; step []; jump "b";
  let inside = point Rays_editor.Private.Text_pane.Graph "(sop/merge" ~after:5 in
  at inside [ Input.Pointer_moved inside ]; at inside [];
  check (contains (line ()) "Refused") ("a put that does not check is refused: " ^ line ());
  check (E.workspace !env == original) "and the picture is the original";
  step [ key Input.Escape ]; step [];
  check (E.carrying !env = None && E.workspace !env == original) "Escape drops it";
  (* an unapplied draft: its bytes are not the document's *)
  at (float (gx + 200), float (top + 24 + 60)) [];
  click (float (gx + 200), float (top + 24 + 60));
  step [ Input.Text_input "x" ]; step [ key Input.Escape ]; step [];
  jump "a"; step [ char 'y' ]; step []; jump "b";
  let over = point Rays_editor.Private.Text_pane.Graph "(sop/merge" ~after:10 in
  at over [ Input.Pointer_moved over ]; at over [];
  check (contains (line ()) "unapplied draft") ("a draft refuses: " ^ line ());
  step [ key Input.Escape ]; step [];
  (* the Document tab: the whole text is checked and installed *)
  click (tab_at ~narrow:(gw < 400) ~right:(gx + gw - 36) ~top 2);
  let original = E.workspace !env in
  jump "a"; step [ char 'y' ]; step [];
  let over = point Rays_editor.Private.Text_pane.Document "(sop/merge" ~after:10 in
  at over [ Input.Pointer_moved over ]; at over [];
  check (contains (line ()) "(ref a) at line") ("the Document text takes it too: " ^ line ());
  at over [ Input.Pointer_pressed (Input.LeftButton, over); Input.Pointer_released (Input.LeftButton, over) ]; step []; step [];
  check (E.undo_label !env = Some "Put" && contains (source ()) "(sop/merge (ref a) (sop/box)")
    ("a put at a caret of the Document text is one Put entry: " ^ source ());
  step ~keys:[ Input.Meta ] [ char 'z' ];
  check (E.workspace !env == original) "and one undo gives it back";
  E.close !env

(* Command-click on a name read in the Document text selects its binding: the pane's selection (and so the
   inspector's subject) is the path of the binding, not the node under the caret *)
let editor_command_click () =
  let open Rays in
  let text = {|(workspace cc
  (graph g :context value (let* [a 1.0 b (+ 2.0 (* 3.0 a)) c (ref h)] (+ a b)))
  (graph h :context value (let* [target 7.0] target))
  (graph editor :context editor
    (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp)))))|} in
  let env = start_env text in
  let count = ref 0 and mouse = ref (450., 320.) in
  let step ?(buttons = []) ?(keys = []) events =
    incr count;
    let frame = frame ~mouse:!mouse ~keys !count events in
    env := E.update !env { frame with input = { frame.input with buttons } } in
  let at ?buttons ?keys point events = mouse := point; step ?buttons ?keys events in
  let dump_line = dump_line env in
  for _ = 1 to 4 do step [] done;
  let gx, gy, gw, gh = (E.panes !env (frame 0 [])).graph in
  let gy = gy + gh + 1 + 28 in
  let tab = tab_at ~narrow:(gw < 400) ~right:(gx + gw - 36) ~top:gy 2 in
  at tab [ Input.Pointer_moved tab ];
  at tab [ Input.Pointer_pressed (Input.LeftButton, tab); Input.Pointer_released (Input.LeftButton, tab) ]; step [];
  let printed = Flow.Lisp.print (E.workspace !env).source |> fst in
  let lines = String.split_on_char '\n' printed in
  let line = Option.get (List.find_index (fun l -> contains l "(+ a b)") lines) in
  let col = let l = List.nth lines line in
    let rec find i = if String.sub l i 7 = "(+ a b)" then i + 3 else find (i + 1) in find 0 in
  let char_w = 6.95 in
  let name = float gx +. 36. +. float col *. char_w +. 3., float gy +. 6. +. 2. +. float line *. line_pitch in
  at name [ Input.Pointer_moved name ];
  at name [ Input.Pointer_pressed (Input.LeftButton, name); Input.Pointer_released (Input.LeftButton, name) ];
  step [];
  check (E.Private.inspector_subject !env = Some ["g"; "@result"])
    "an ordinary click in Document did not select its containing node";
  at ~keys:[ Input.Meta ] name [ Input.Pointer_pressed (Input.LeftButton, name); Input.Pointer_released (Input.LeftButton, name) ];
  step []; step [];
  check (dump_line "scope selected" = "scope selected: g/a")
    ("a Command-click on a name did not select its binding: " ^ dump_line "scope selected");
  check (E.Private.inspector_subject !env = Some [ "g"; "a" ]) "the inspector does not show the Command-clicked binding";
  let graph_tab = tab_at ~narrow:(gw < 400) ~right:(gx + gw - 36) ~top:gy 1 in
  at graph_tab [Input.Pointer_moved graph_tab];
  at graph_tab [Input.Pointer_pressed (Input.LeftButton, graph_tab); Input.Pointer_released (Input.LeftButton, graph_tab)];
  step [];
  let graph = T.make_shown (E.workspace !env).source "g" None T.Graph in
  let lines = String.split_on_char '\n' graph.text in
  let line = Option.get (List.find_index (fun line -> contains line "(ref h)") lines) in
  let col = let text = List.nth lines line in
    let rec find i = if String.sub text i 7 = "(ref h)" then i + 5 else find (i + 1) in find 0 in
  let reference = float gx +. 36. +. float col *. char_w +. 3., float gy +. 8. +. float line *. line_pitch in
  at reference [Input.Pointer_moved reference];
  at ~keys:[Input.Ctrl] reference [Input.Pointer_pressed (Input.LeftButton, reference); Input.Pointer_released (Input.LeftButton, reference)];
  step []; step [];
  check (dump_line "pane graph" = "pane graph: h") "Ctrl-click did not open the referenced graph";
  check (contains (dump_line "text") "document tab") "an out-of-Graph definition did not switch to Document";
  E.close !env

(* Find, go to symbol, go to line and go to definition through the editor: the bar's state is read
   from the crash dump's text line, the caret from where the next Command-F starts *)
let editor_text_find () =
  let open Rays in
  let text = {|(workspace ff
  (graph g :context value (let* [a 1.0 b (+ 2.0 (* 3.0 a)) c (ref h)] (+ a b)))
  (graph h :context value (let* [target 7.0] target))
  (graph editor :context editor
    (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp)))))|} in
  let env = start_env text in
  let count = ref 0 and mouse = ref (450., 320.) in
  let step ?(keys = []) events = incr count; env := E.update !env (frame ~mouse:!mouse ~keys !count events) in
  let at point events = mouse := point; step events in
  let key k = Input.Key_pressed k and char c = Input.Key_pressed (Input.KeyChar c) in
  let command c = step ~keys:[ Input.Meta ] [ char c ]; step [] in
  let dump_line = dump_line env in
  (* the byte the bar looks from, as the dump says it *)
  let bar_at () =
    let line = dump_line "text" in
    let rec after = function
      | "at" :: n :: rest -> (match int_of_string_opt n with Some _ as v -> v | None -> after rest)
      | _ :: rest -> after rest | [] -> None in
    match after (String.split_on_char ' ' line) with
    | Some byte -> byte
    | None -> fail ("the bar is not open: " ^ line) in
  for _ = 1 to 4 do step [] done;
  let gx, gy, gw, gh = (E.panes !env (frame 0 [])).graph in
  let gy = gy + gh + 1 + 28 in
  let tab = tab_at ~narrow:(gw < 400) ~right:(gx + gw - 36) ~top:gy 2 in
  at tab [ Input.Pointer_moved tab ];
  at tab [ Input.Pointer_pressed (Input.LeftButton, tab); Input.Pointer_released (Input.LeftButton, tab) ]; step [];
  let document () = Lazy.force (T.make_shown (E.workspace !env).source "g" None T.Document).applied in
  let click point = at point [ Input.Pointer_moved point ];
    at point [ Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point) ]; step [] in
  click (float (gx + 200), float (gy + 8));
  (* Command-F opens the bar on the query field; typing finds, Enter and Shift-Enter step *)
  command 'f';
  check (contains (dump_line "text") "find \"\"") ("Command-F did not open the bar: " ^ dump_line "text");
  step [ Input.Text_input "(graph" ]; step [];
  check (contains (dump_line "text") "find \"(graph\"") ("typing did not set the query: " ^ dump_line "text");
  let matches = T.find_all ~case:false "(graph" (document ()) in
  check (List.length matches = 3) "the workspace has three graphs";
  let first_from at = match List.find_opt (fun (a, _) -> a >= at) matches with Some m -> m | None -> List.hd matches in
  let start = bar_at () in
  let m0 = first_from start in
  step [ key Input.Enter ]; step [];
  check (bar_at () = fst m0 + 1) ("Enter did not step past the current match: " ^ dump_line "text");
  let m1 = first_from (fst m0 + 1) in
  check (m1 <> m0) "the next match is another one";
  step ~keys:[ Input.Shift ] [ key Input.Enter ]; step [];
  check (bar_at () = fst m0) ("Shift-Enter did not go back: " ^ dump_line "text");
  step [ key Input.Escape ]; step [];
  check (not (contains (dump_line "text") "find")) ("Escape did not close the bar: " ^ dump_line "text");
  (* Command-P lists the bindings; the pick puts the caret on the name (Command-F reads the caret) *)
  let symbols = T.symbols (document ()) in
  check (List.map (fun (name, kind, _) -> name, kind) symbols
         = [ "g", "graph"; "a", "let*"; "b", "let*"; "c", "let*"; "h", "graph"; "target", "let*"; "editor", "graph" ])
    "the symbols are the graphs and the let* names, in order";
  command 'p';
  check (contains (dump_line "prompt") "go to symbol") ("Command-P did not open the symbols: " ^ dump_line "prompt");
  step [ Input.Text_input "target" ]; step [ key Input.Enter ]; step []; step [];
  let target = let _, _, byte = List.find (fun (n, _, _) -> n = "target") symbols in byte in
  command 'f';
  check (bar_at () = target) ("the symbol pick did not place the caret: " ^ dump_line "text");
  step [ key Input.Escape ]; step [];
  (* Shift-F12 on the name lists its uses; Enter on the second goes there *)
  check (T.symbol_at (document ()) target = Some "target" && T.symbol_at (document ()) 0 = None)
    "symbol_at reads the name under a byte";
  check (List.map fst (T.references (document ()) "target") <> [] && List.length (T.references (document ()) "target") = 2)
    "references finds both uses of target";
  step ~keys:[ Input.Shift ] [ key Input.F12 ]; step [];
  check (contains (dump_line "prompt") "references") ("Shift-F12 did not open the references: " ^ dump_line "prompt");
  step [ key Input.ArrowDown ]; step [ key Input.Enter ]; step []; step [];
  check (not (contains (dump_line "prompt") "references")) "picking a use did not close the references";
  command 'f';
  check (bar_at () = fst (List.nth (T.references (document ()) "target") 1))
    ("the pick did not place the caret on the second use: " ^ dump_line "text");
  step [ key Input.Escape ]; step [];
  (* Command-L: line and column; F12 on the (ref h) goes to its graph *)
  let lines = String.split_on_char '\n' (document ()) in
  let line = Option.get (List.find_index (fun l -> contains l "(ref h)") lines) in
  let col = let l = List.nth lines line in
    let rec find i = if String.sub l i 7 = "(ref h)" then i + 5 else find (i + 1) in find 0 in
  command 'l';
  check (contains (dump_line "prompt") "go to line") ("Command-L did not open the line prompt: " ^ dump_line "prompt");
  step [ Input.Text_input (Printf.sprintf "%d:%d" (line + 1) (col + 1)) ]; step [ key Input.Enter ]; step []; step [];
  check (T.line_target (document ()) (Printf.sprintf "%d:%d" (line + 1) (col + 1))
         = Some (List.fold_left (fun n l -> n + String.length l + 1) 0 (List.take line lines) + col))
    "line_target counts the line and column";
  step [ key Input.F12 ]; step []; step [];
  check (dump_line "pane graph" = "pane graph: h") ("F12 did not follow the reference: " ^ dump_line "pane graph");
  E.close !env

(* Each graph's Graph tab keeps its own unapplied text: typing in a second graph does not replace
   the first graph's draft, and discarding one leaves the other. *)
let editor_graph_drafts () =
  let open Rays in
  let text = {|(workspace drafts
  (graph a :context sop (sop/box))
  (graph b :context sop (sop/merge (sop/box)))
  (graph scene :context scene (scene/merge (scene/geometry (ref b))))
  (graph editor :context editor
    (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp)))))|} in
  let env = start_env text in
  let count = ref 0 and mouse = ref (450., 320.) in
  let step ?(keys = []) events = incr count; env := E.update !env (frame ~mouse:!mouse ~keys !count events) in
  let at point events = mouse := point; step events in
  let key k = Input.Key_pressed k and char c = Input.Key_pressed (Input.KeyChar c) in
  let jump name =
    step [ key Input.Escape ];
    step [ key (Input.KeyChar '/'); char 'j' ]; step [ Input.Text_input name ]; step [ key Input.Enter ]; step [] in
  let summary () = dump_line env "text" in
  for _ = 1 to 4 do step [] done;
  let gx, gy, gw, gh = (E.panes !env (frame 0 [])).graph in
  let top = gy + gh + 1 + 28 in
  let click point = at point [ Input.Pointer_moved point ];
    at point [ Input.Pointer_pressed (Input.LeftButton, point); Input.Pointer_released (Input.LeftButton, point) ];
    step [] in
  let area = float gx +. 120., float top +. 40. in
  let draft_in name =
    jump name;
    click (tab_at ~narrow:(gw < 400) ~right:(gx + gw - 36) ~top 1);
    click area; step ~keys:[ Input.Meta ] [ char 'a' ]; step [ Input.Text_input ("(sop/box) ; " ^ name) ] in
  draft_in "a";
  check (contains (summary ()) "graph drafts: a") ("a draft was not kept: " ^ summary ());
  draft_in "b";
  check (contains (summary ()) "graph drafts: a b")
    ("the second graph's draft replaced the first's: " ^ summary ());
  jump "a";
  check (contains (summary ()) "graph drafts: a b") ("leaving a graph dropped its draft: " ^ summary ());
  E.close !env

(* Every fn frame starts open, its body in view, however many the graph has (c folds one). *)
let editor_fn_frames () =
  let open Rays in
  let height maps =
    let text = Printf.sprintf {|(workspace fz
  (graph g :context value (let* [%s] 1.0))
  (graph editor :context editor
    (ui/workspace (ui/split-at "vertical" 0.12 (ui/graph) (ui/lisp)))))|} maps in
    let env = start_env text in
    let count = ref 0 in
    let step events = incr count; env := E.update !env (frame ~mouse:(450., 320.) !count events) in
    for _ = 1 to 4 do step [] done;
    step [ Input.Key_pressed Input.Escape ];
    step [ Input.Key_pressed (Input.KeyChar '/'); Input.Key_pressed (Input.KeyChar 'j') ];
    step [ Input.Text_input "g" ]; step [ Input.Key_pressed Input.Enter ];
    for _ = 1 to 4 do step [] done;
    let box = E.node_box !env [ "g"; "a" ] in
    E.close !env;
    match box with Some (_, _, _, h) -> h | None -> fail "the fn frame has no box" in
  let one = height "a (fn [k] (* k 2.0))" in
  let two = height "a (fn [k] (* k 2.0)) b (fn [k] (+ k 1.0))" in
  check (two = one) (Printf.sprintf "a second fn frame folded the first (%d, %d)" one two)

let run () =
  selection_text ();
  paste ();
  lisp_text ();
  find_pure ();
  editor_text ();
  editor_binding ();
  editor_w9 ();
  editor_active_scrub ();
  editor_command_click ();
  token_scrubs ();
  editor_literal_scrub ();
  editor_text_drop ();
  editor_graph_drafts ();
  editor_text_find ();
  editor_fn_frames ();
  print_endline "text pane tests passed"
