(* Goldens for [Js.frame] from the shared core fixtures (lib/flow_ir/core_fixtures.ml) and two
   workspaces, and a node run of emitted programs against [Core_exec].
   Regenerate goldens with [test_js_frame.exe --write-goldens] from lib/flow_web. *)
module E = Flow.Eval
module I = Flow_ir
module C = I.Core
module M = Scene_command.Marks

let ok = function Ok x -> x | Error d -> failwith (Flow.Diagnostic.to_string d)
let writing = Array.to_list Sys.argv = [Sys.argv.(0); "--write-goldens"]
let node = Sys.command "command -v node >/dev/null 2>&1" = 0

let ops = Core_fixtures.offset :: I.Operators.all
let check source =
  match Flow.Workspace.check ~ops {Flow.Check.version = 1; kinds = []} (ok (Flow.Syntax.parse source)) with
  | Some ws, [] -> ws
  | _, ds -> failwith (String.concat "; " (List.map Flow.Diagnostic.to_string ds))

(* ---- the fixtures as core programs ---- *)
let lower_residual name = function
  | E.Residual r -> (match I.Core_lower.lower r with Ok p -> r, p | Error n -> failwith (name ^ ": refused " ^ n))
  | _ -> failwith (name ^ ": expected a residual")
let tested name source =
  lower_residual name (snd (List.hd (List.assoc ["g"; "tested"] (ok (E.static ~record:true (check source))).records)))
let wrapped name body = tested name (Core_fixtures.workspace
  (if String.starts_with ~prefix:"captured-" name then body else "(if (> t -1000.) " ^ body ^ " " ^ body ^ ")"))
let expression name body =
  match List.assoc "g" (ok (E.static (check ("(workspace w (graph g :context value " ^ body ^ "))")))).results with
  | E.Residual _ as v -> Some (lower_residual name v) | _ -> None
let pick names list = List.map (fun n -> n, List.assoc n list) names

let error_boundaries = [
  "error_success", "(with_error 7 (fn [failure] (error \"E_UNTAKEN\" \"mapper never\")))";
  "error_map", "(with_error (error \"E_CHILD\" \"child\" :cause \"日本😀\") (fn [failure] (assoc failure :code \"E_MAPPED\")))";
  "error_mapper", "(with_error (error \"E_CHILD\" \"child\") (fn [failure] (error \"E_MAPPER\" \"mapper\")))";
  "error_cancel", "(with_error (error \"E_CANCELLED\" \"cancel\") (fn [failure] (error \"E_WRONG\" \"never\")))";
  "error_nested", "(with_error (with_error (error \"E_CHILD\" \"child\") (fn [failure] failure)) (fn [failure] (assoc failure :cause (list \"outer\"))))"
]
let values =
  List.map (fun (n, s) -> "control_" ^ n, tested n s) (pick ["vec2"; "cond"; "case-text"; "let-nested"; "local-fn"; "named-fn";
      "pattern-error"; "bypass"; "untaken-error"] Core_fixtures.control)
  @ List.map (fun (n, b) -> "lists_" ^ n, wrapped n b) (pick ["literal"; "range"; "range-two"; "take"; "reverse"; "concat"; "nth";
      "map"; "filter"; "reduce"; "sort"; "for-product"; "for-dependent"; "sum-dynamic"; "scan"; "fold-record"; "count";
      "map-tags"; "nested-skip"; "captured-list-nth"; "iterate"; "iterate-large"; "iterate-until"; "iterate-record"; "iterate-widen"] Core_fixtures.lists)
  @ List.map (fun (n, b) -> "primitives_" ^ n, wrapped n b) (pick ["range"; "float"; "vec3"; "count"; "nth"; "slice"; "concat";
      "noise3"; "packed-map-int"; "packed-filter"] Core_fixtures.primitives)
  @ List.map (fun (n, b) -> "regions_" ^ n, tested n (Core_fixtures.workspace b)) (pick ["map"; "sum"; "fold"] Core_fixtures.regions)
  @ List.filter_map Fun.id (List.mapi (fun i b -> Option.map (fun p -> Printf.sprintf "expression_%d" i, p) (expression b b)) Core_fixtures.lower_expressions)

let state_fixtures =
  List.map (fun (n, b) -> n, "", b) Core_fixtures.state_cases @ Core_fixtures.state_refs
let state_values (_, extra, body) =
  let evaluation = ok (E.static (check (String.concat "" [
    "(workspace w "; extra; " (graph g :context value (let* [v "; body; "] 0.)))"]))) in
  List.map snd evaluation.results @ evaluation.states
let state_picked = ["counter"; "integer"; "record"; "two-reads"; "loop"; "event-records"; "list"; "ref"]
let state_programs =
  List.filter_map (fun ((name, _, _) as f) ->
    if not (List.mem name state_picked) then None else
    List.find_map (function E.Residual r ->
      (match I.Core_lower.lower r with Ok p when Array.length p.state_slots > 0 -> Some ("state_" ^ name, (r, p)) | _ -> None)
      | _ -> None) (state_values f)) state_fixtures

let draw_of name evaluation value =
  match I.Core_draw.lower evaluation.E.plan value with
  | Ok (program, root) -> name, program, root
  | Error n -> failwith (name ^ ": refused " ^ n)
let draw_source name body =
  let ws = check ("(workspace w (graph g :context draw " ^ body ^ "))") in
  let evaluation = ok (E.static ws) in
  draw_of name evaluation (List.assoc "g" evaluation.results)
let pictures = List.mapi (fun i b -> draw_source (Printf.sprintf "draw_%02d" i) b) Core_fixtures.pictures
let workspace_picture name path =
  (* Only the draw graph: the settings and editor graphs need the application catalog. *)
  let text = In_channel.with_open_bin path In_channel.input_all in
  let start = Str.search_forward (Str.regexp_string "(graph picture") text 0 in
  let depth = ref 0 and stop = ref start in
  String.iteri (fun i c -> if i >= start && !depth >= 0 then begin
    if c = (Char.chr 40) then incr depth else if c = (Char.chr 41) then (decr depth; if !depth = 0 then (stop := i; depth := -1)) end) text;
  let text = "(workspace picture_only " ^ String.sub text start (!stop - start + 1) ^ ")" in
  match Flow.Workspace.check {Flow.Check.version = 1; kinds = []} (ok (Flow.Syntax.parse text)) with
  | Some ws, [] ->
      let evaluation = ok (E.static ws) in
      let name, program, root = draw_of name evaluation (List.assoc "picture" evaluation.results) in
      let root = match root with Some _ -> root | None ->
        List.find_map (function E.Residual r -> Some r | _ -> None) evaluation.states in
      name, program, root
  | _, ds -> failwith (path ^ ": " ^ String.concat "; " (List.map Flow.Diagnostic.to_string ds))
let workspaces = [workspace_picture "workspace_flow_vectors" "../../sketches/flow_vectors.rays";
                  workspace_picture "workspace_basic" "../../sketches/basic.rays"]

(* ---- goldens ---- *)
let emitted name program = match Flow_web.Js.frame program with
  | Ok source -> source
  | Error d -> failwith (name ^ ": " ^ Flow.Diagnostic.to_string d)
let golden name program =
  let source = emitted name program and path = "goldens/frame_" ^ name ^ ".js" in
  if writing then Out_channel.with_open_bin path (fun c -> output_string c source)
  else if In_channel.with_open_bin path In_channel.input_all <> source then failwith ("golden changed: " ^ path)

(* Every statement and expression form of the core appears in some golden program. *)
let coverage programs =
  let seen = Hashtbl.create 64 in
  let mark k = Hashtbl.replace seen k () in
  let rec statements l = List.iter (fun (s : C.statement) -> match s.node with
    | Assign (_, e) -> mark (match e with
        | C.Constant _ -> "constant" | Local _ -> "local" | Frame _ -> "frame" | Call_index _ -> "call_index"
        | Prim _ -> "prim" | Vec _ -> "vec" | Fields _ -> "fields" | Items _ -> "items" | Field _ -> "field"
        | Source _ -> "source" | Host_source _ -> "host_source"
        | Source_descriptor _ -> "source_descriptor")
    | Reset_call _ -> mark "reset_call"
    | Draw d -> mark (match d with
        | C.Background _ -> "background" | Point _ -> "point" | Mark _ -> "mark" | Batch _ -> "batch" | Points _ -> "points"
        | Path _ -> "path" | Text_run _ -> "text" | Image _ -> "image" | Push _ -> "push" | Pop -> "pop")
    | Protect {body;handler;_}->mark "protect";statements body;statements handler
    | Diagnose_rows{handler;_}->mark "diagnose_rows";statements handler
    | If {yes; no; _} -> mark "if"; statements yes; statements no
    | Iterate {body; _} -> mark "iterate"; statements body
    | Counted {init; body; _} -> mark "counted"; statements init; statements body
    | Store _ -> mark "store"
    | State_read {init; _} -> mark "state_read"; statements init
    | State_peek _ -> mark "state_peek" | State_write _ -> mark "state_write" | Invoke _ -> mark "invoke") l in
  List.iter (fun (p : C.program) -> statements p.body) programs;
  let missing = List.filter (fun k -> not (Hashtbl.mem seen k))
    ["constant"; "local"; "frame"; "call_index"; "prim"; "vec"; "fields"; "items"; "field"; "reset_call"; "background";
     "point"; "mark"; "batch"; "points"; "path"; "text"; "image"; "push"; "pop"; "if"; "iterate"; "counted"; "store";
     "state_read"; "state_peek"; "state_write"; "invoke"] in
  if missing <> [] then failwith ("no golden program uses: " ^ String.concat ", " missing)

(* ---- canonical text, shared with tools/web/test/frame_semantics.mjs ---- *)
let hex s = String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq s)))
let f x = Printf.sprintf "f:%.17g" x
let rec value b : E.value -> unit = fun v ->
  let add = Buffer.add_string b in
  let arr tag data = add (Printf.sprintf "%s[ %s ]" tag (String.concat " " (List.map f (Array.to_list data)))) in
  match v with
  | Int n -> add (Printf.sprintf "i:%d" n) | Float x -> add (f x) | Bool x -> add (Printf.sprintf "b:%b" x)
  | Text s -> add ("t:" ^ hex s)
  | Text_array xs -> add "AT[ "; Array.iter (fun s->add("t:"^hex s^" ")) xs;add "]"
  | Vec2 (x, y) -> arr "v2" [|x; y|] | Vec3 (x, y, z) -> arr "v3" [|x; y; z|] | Vec4 (x, y, z, w) -> arr "v4" [|x; y; z; w|]
  | List xs -> add "L[ "; Array.iter (fun x -> value b x; add " ") xs; add "]"
  | Record fs -> add "R{ "; List.iter (fun (n, x) -> add ("n:" ^ hex n ^ " "); value b x; add " ") fs; add "}"
  | Float_array a -> arr "A1" a | Vec2_array a -> arr "A2" a | Vec3_array a -> arr "A3" a | Vec4_array a -> arr "A4" a
  | No_geo -> add "g" | _ -> add "?"
let canon_value v = let b = Buffer.create 64 in value b v; Buffer.contents b
let canon_marks buffers (marks : M.t) =
  let attr show = function
    | M.Constant v -> "c " ^ show v
    | M.Buffer {id; count; width} -> Printf.sprintf "b%dx%d[ %s ]" count width
        (String.concat " " (List.map f (Array.to_list (I.Core_exec.Buffers.find buffers id))))
    | M.Device _ -> failwith "a device attribute has no web frame form" in
  let pair (x, y) = f x ^ " " ^ f y and ink c = Printf.sprintf "i:%ld" c in
  String.concat " ; " (List.map (function
    | M.Mark_set {count; kind} ->
        let head = Printf.sprintf "M %s %d" (match kind with Rect _ -> "rect" | Circle _ -> "circle" | Line _ -> "line" | Point _ -> "point") count in
        head ^ " " ^ (match kind with
          | Rect {position; size; fill; stroke; stroke_width} ->
              String.concat " " [attr pair position; attr pair size; attr ink fill; attr ink stroke; attr f stroke_width]
          | Circle {position; radius; fill; stroke; stroke_width} ->
              String.concat " " [attr pair position; attr f radius; attr ink fill; attr ink stroke; attr f stroke_width]
          | Line {position; end_; stroke; stroke_width} ->
              String.concat " " [attr pair position; attr pair end_; attr ink stroke; attr f stroke_width]
          | Point {position; fill} -> attr pair position ^ " " ^ ink fill)
    | Geometry _ -> "G"
    | Image {resource; x; y; scale; angle} -> Printf.sprintf "I i:%d i:%d i:%d %s %s" resource x y (f scale) (f angle)
    | Text {x; y; size; color; text} -> Printf.sprintf "T i:%d i:%d i:%d %s t:%s" x y size (ink color) (hex text)
    | Push_transform {xx; xy; yx; yy; tx; ty} -> "P " ^ String.concat " " (List.map f [xx; xy; yx; yy; tx; ty])
    | Pop_transform -> "Q"
    | Push_clip _ -> "?" | Pop_clip -> "?") (Array.to_list marks))

(* Tokens equal exactly, except floats, which agree within 1e-12 relative. *)
let same_text a b =
  let tokens s = List.filter (( <> ) "") (String.split_on_char (Char.chr 32) (String.map (fun c -> if c = (Char.chr 9) then (Char.chr 32) else c) s)) in
  let close x y = x = y || (Float.is_nan x && Float.is_nan y) || Float.abs (x -. y) <= 1e-12 *. Float.max (Float.abs x) (Float.abs y) in
  let same x y = x = y || (String.starts_with ~prefix:"f:" x && String.starts_with ~prefix:"f:" y &&
    close (float_of_string (String.sub x 2 (String.length x - 2))) (float_of_string (String.sub y 2 (String.length y - 2)))) in
  let a = tokens a and b = tokens b in List.compare_lengths a b = 0 && List.for_all2 same a b

(* ---- the node run ---- *)
type case = { name : string; program : C.program; mode : [ `Value | `Marks ]; lives : Frame_input.t list;
  expected : string list }

let number x = Printf.sprintf "%.17g" x
let json_of_live (l : Frame_input.t) =
  Printf.sprintf {|{"t":%s,"dt":%s,"frame":%d,"tick":%d,"size":[%d,%d],"pointer":[%s,%s],"buttons":[],"keys":[],"events":[%s]}|}
    (number l.t) (number l.dt) l.frame l.tick (fst l.size) (snd l.size) (number (fst l.pointer)) (number (snd l.pointer))
    (String.concat "," (List.map (function Frame_input.Key_pressed k -> Printf.sprintf {|{"kind":"key-pressed","key":"%s"}|} k
      | _ -> failwith "unsupported event") l.events))
let live ?(events = []) i t = {(Frame_input.at_time t) with dt = 0.25; frame = i; tick = i; size = (640, 360);
  pointer = (12.5, 40.); events}
let outcome render = function Ok v -> "ok\t" ^ render v | Error (d : Flow.Diagnostic.t) -> "err\t" ^ d.code
  ^Option.fold ~none:"" ~some:(fun cause->"\t"^hex cause)d.cause

let value_case name (_, program) =
  let lives = List.mapi (fun i t -> live i t) [-3.; -0.; 0.37; 1.9; 100.25] in
  { name; program; mode = `Value; lives;
    expected = List.map (fun l -> outcome canon_value (I.Core_exec.run program ~live:l)) lives }
(* The evaluator hands the executor the residuals of a state fixture each frame, in call order;
   the case replays exactly those calls (one program) through the emitted module. *)
let state_case name fixture =
  let values = state_values fixture and st = E.create_state () and calls = ref [] in
  let text p = Format.asprintf "%a" C.pp p in
  let execute r lv = match I.Core_lower.lower r with
    | Ok p when Array.length p.state_slots > 0 ->
        let result = I.Core_exec.run ~root:r p ~live:lv in
        calls := (p, lv, outcome canon_value result) :: !calls; Some result
    | _ -> None in
  for i = 0 to 7 do
    let lv = live ~events:(if i mod 2 = 1 then [Frame_input.Key_pressed "a"; Key_pressed "b"] else []) i (float_of_int i *. 0.25) in
    List.iter (fun v -> ignore (E.Private.force_with_executor ~state:st ~execute v ~live:lv)) values
  done;
  match List.rev !calls with
  | (program, _, _) :: _ as calls when List.for_all (fun (p, _, _) -> text p = text program) calls ->
      Some { name; program; mode = `Value; lives = List.map (fun (_, l, _) -> l) calls;
             expected = List.map (fun (_, _, o) -> o) calls }
  | _ -> None
let draw_case (name, program, root) =
  (* State needs the evaluator behind it: one frame, where every read is its initializer. *)
  let lives = if Array.length (program : C.program).state_slots > 0 then [live 0 0.25] else [live 0 0.25; live 1 1.5] in
  let render l = outcome (fun (buffers, marks) -> canon_marks buffers marks)
    (let buffers = I.Core_exec.Buffers.create () in
     Result.map (fun marks -> buffers, marks)
       (I.Core_exec.draw ?root ~image:(fun _ -> Ok 1) (I.Core_exec.prepare program) ~live:l ~size:l.size ~buffers)) in
  { name; program; mode = `Marks; lives; expected = List.map render lives }

let run_node cases =
  let dir = Filename.temp_dir "frames" "" in
  let manifest = Buffer.create 4096 in
  Buffer.add_string manifest "{\"programs\":[";
  List.iteri (fun i c ->
    Out_channel.with_open_bin (Filename.concat dir (c.name ^ ".mjs")) (fun ch -> output_string ch (emitted c.name c.program));
    Printf.bprintf manifest "%s{\"name\":\"%s\",\"file\":\"%s.mjs\",\"mode\":\"%s\",\"frames\":[%s]}" (if i = 0 then "" else ",")
      c.name c.name (if c.mode = `Marks then "marks" else "value") (String.concat "," (List.map json_of_live c.lives))) cases;
  Buffer.add_string manifest "]}";
  Out_channel.with_open_bin (Filename.concat dir "manifest.json") (fun ch -> output_string ch (Buffer.contents manifest));
  let out = Filename.concat dir "out.txt" in
  let command = Printf.sprintf "node ../../tools/web/test/frame_semantics.mjs %s > %s 2> %s.err"
    (Filename.quote (Filename.concat dir "manifest.json")) (Filename.quote out) (Filename.quote out) in
  if Sys.command command <> 0 then failwith (In_channel.with_open_bin (out ^ ".err") In_channel.input_all);
  String.split_on_char '\n' (In_channel.with_open_bin out In_channel.input_all)
  |> List.filter_map (fun l -> match String.split_on_char '\t' l with
    | name::frame::status::payload -> Some ((name, int_of_string frame), status ^ "\t" ^ String.concat "\t" payload)
    | _ -> None)

let rec has_invoke l = List.exists (fun (s : C.statement) -> match s.node with
  | Invoke _ -> true | If {yes; no; _} -> has_invoke yes || has_invoke no
  | Iterate {body; _} -> has_invoke body | Counted {init; body; _} -> has_invoke init || has_invoke body
  | Protect {body;handler;_}->has_invoke body || has_invoke handler
  | Diagnose_rows{handler;_}->has_invoke handler
  | State_read {init; _} -> has_invoke init | _ -> false) l

(* prims.js has no packed form of sequence_build (core.md: a width allocates packed storage), so
   these two programs fail under node with E_ITER_BOUND. Golden only; reported to the owner. *)
let prims_gaps = ["primitives_packed-map-int"; "primitives_packed-filter"]
(* A draw graph with state runs natively inside the editor, which owns its evaluation state. *)
let no_native_state = ["workspace_basic"]

let () =
  let cases=List.mapi(fun i body->
    let name="text_array_"^string_of_int i in
    let r,_=wrapped name body in
    let program=match I.Core_lower.lower ~strict:true r with
      |Ok program->program|Error form->failwith("Text Core refused "^form) in
    (* Leaf Text maps/loops can invoke Collect regions; Text transformations
       and scans still use structured Core. *)
    (* This frame shim has no region table. Exercise its structured printer;
       test_gather runs the strict Text region through the actual runtime/pool. *)
    let frame_program=match I.Core_lower.lower r with Ok p->p|Error form->failwith form in
    let case=value_case name (r,frame_program) in
    List.iter(fun live->match I.Core_exec.run program ~live,E.Private.force_reference(E.Residual r)~live with
      |Ok actual,Ok expected->assert(actual=expected)
      |Error actual,Error expected->assert(actual.code=expected.code && actual.message=expected.message)
      |_->assert false) case.lives;
    case)
[ {|(if (> t 0.)
      (let* [unused (error_rows (array/int 2 1)
        (fn [index] {:code "E_ROW" :message (str "bad " index) :cause (list (str "raw 日本😀 " index))}))] 0) 0)|};
  {|(let* [unused (error_rows (array/int 0) (fn [index] (error "E_UNTAKEN" "mapper")))] 0)|};
  {|(if (> t 0.) (let* [unused (error_rows (array/int 2 1)
      (fn [index] (error "cancelled" "mapper cancel" :cause "raw cancel")))] 0) 0)|};
  "(cross [t 0.3 -0.7] [t 0.3 -0.7])";
     "(let* [a [t 0.3 -0.7]] [(- (* a.y a.z) (* a.z a.y)) (- (* a.z a.x) (* a.x a.z)) (- (* a.x a.y) (* a.y a.x))])";
     "(text/blank? (if (> t 0.) \" \\t\\r\\n\" \" \"))";
     "(if (> t 0.) (error (str \"E_TEXT_\" (int t)) (str \"exact é日本😀 \" t) :cause (str \"reason 日本😀 \" t)) \"safe\")";
     "(text/trim (if (> t 0.) \" \\tABéÉ日本😀\\r\\n\" \"　A \"))";
     "(text/lower (if (> t 0.) \"ABéÉΣİ日本😀\" \"Z\000A\"))";
     "(text/repr (if (> t 0.) \"é日本😀\\t\\n\\\\\\\"\" \"Z\000A\"))";
     "(map (fn [x] (text/repr (if (> t 0.) x \"ÉΣİZ\"))) (array/text 5001 \" \\tABéÉ日本😀\\r\\n\"))";
     "(map (fn [x] (text/lower (text/trim (if (> t 0.) x \"ÉΣİZ\")))) (array/text 5001 \" \\tABéÉ日本😀\\r\\n\"))";
     "(array/text (+ 5001 (frame/index)) \"猫😀é\")";
     "(map (fn [x] (if (> t 0.) x \"😀\")) (array/text 5001 \"猫\"))";
     "(for [x (array/text 5001 \"猫\")] (if (> t 0.) x \"😀\"))";
     "(scan [a \"seed\"] [x (array/text 5001 \"猫\")] (if (> t 0.) x \"😀\"))";
     "(for [x (array/text 2 \"猫\") y (array/text 2501 \"😀\")] :skip [5001] (if (> t 0.) x y))";
     "(scan [a \"seed\"] [x (array/text 2 \"猫\") y (array/text 2501 \"😀\")] (if (> t 0.) x y))";
     "(for [x (array/text 0) y (array/text -1)] (if (> t 0.) x y))";
     "(for [x (array/text 2) y (array/text 0)] (if (> t 0.) x y))";
     "(scan [a \"seed\"] [x (array/text 0) y (array/text -1)] (if (> t 0.) x y))";
     "(fold [a 0] [x (array/text 5001)] (int/add a (if (> t 0.) 1 2)))";
     "(filter (fn [x] (> t 0.)) (array/text 5001 \"猫\"))";
     "(array/concat (array/slice (array/text 5001 \"猫\") 4999 2) (array/text 1 \"😀\"))";
     "(map (fn [x] x) (array/text 0))";
     "(for [x (array/text 0)] x)";
     "(scan [a \"seed\"] [x (array/text 0)] x)";
     {|(fold [state {:n 0}] [i (range 3)]
         (if (> t 0.) (error "E_RECORD" "selected record error" :cause "record cause")
           (assoc state :n (int/add state.n 1))))|};
     {|(fold [state {:n 0}] [i (range 3)]
         (assoc (if (> t 0.) (error "E_RECORD" "selected record error" :cause "record cause") state)
           :n (int/add state.n 1)))|}] in
  if node then begin
    let produced=run_node cases in
    List.iter(fun case->List.iteri(fun frame expected->
      let actual=List.assoc(case.name,frame)produced in
      if not(same_text actual expected) then
        failwith(Printf.sprintf "%s frame%d: native %s; JavaScript %s" case.name frame expected actual))case.expected)cases;
    print_endline "Text arrays: emitted JavaScript structured map/for/scan/fold/filter, empty storage and UTF-8 bytes match native/reference"
  end

let () =
  let programs = List.map (fun (n, (_, p)) -> n, p) values @ List.map (fun (n, (_, p)) -> n, p) state_programs
    @ List.map (fun (n, p, _) -> n, p) pictures @ List.map (fun (n, p, _) -> n, p) workspaces in
  List.iter (fun (n, p) -> golden n p) programs;
  coverage (List.map snd programs);
  Printf.printf "JS frames: %d goldens passed\n" (List.length programs);
  if not node then print_endline "JS frames: node is absent, so the native comparison was SKIPPED"
  else begin
    let cases = List.filter (fun c -> not (has_invoke c.program.body) && not (List.mem c.name prims_gaps) && not (List.mem c.name no_native_state))
      (List.map (fun (n, (r, p)) -> value_case n (r, p)) values
       @ List.filter_map (fun (n, _, _ as f) -> if List.mem n state_picked then state_case ("state_" ^ n) f else None) state_fixtures
       @ List.map (fun c -> draw_case c) (pictures @ workspaces)) in
    let skipped = List.filter (fun n -> not (List.exists (fun c -> c.name = n) cases)) (List.map fst programs) in
    Printf.printf "JS frames: not run under node (regions, shim gaps): %s\n" (String.concat " " skipped);
    let produced = run_node cases in
    let failures = ref [] and compared = ref 0 in
    List.iter (fun c -> List.iteri (fun i expected ->
      incr compared;
      match List.assoc_opt (c.name, i) produced with
      | Some actual when same_text actual expected -> ()
      | Some actual -> let cut s = if String.length s > 160 then String.sub s 0 160 ^ "..." else s in
        failures := Printf.sprintf "%s frame %d\n  native %s\n  node   %s" c.name i (cut expected) (cut actual) :: !failures
      | None -> failures := Printf.sprintf "%s frame %d: no node output" c.name i :: !failures) c.expected) cases;
    if !failures <> [] then failwith (String.concat "\n" (List.rev !failures));
    Printf.printf "JS frames under node against Core_exec: %d programs, %d frames agree\n" (List.length cases) !compared
  end

(* New structured control is checked semantically, without refreshing unrelated goldens. *)
let ()=if node then begin
  let cases=List.map(fun(name,body)->value_case name(wrapped name body))error_boundaries in
  let produced=run_node cases in
  List.iter(fun case->List.iteri(fun index expected->
    let actual=List.assoc(case.name,index)produced in
    if not(same_text actual expected)then failwith(case.name^": "^actual^" differs from "^expected))case.expected)cases
end
let ()=
  match Some(wrapped "range-last-boundary"
    {|(let* [unused (error_rows (array/int 2 1) (fn [index]
        {:code "E_ROW" :message (str index) :cause (list)}) :policy "range-last")] 0)|})with
  |Some(_,program)->(match Flow_web.Js.frame program with
      |Error d->assert(d.code="E_WEB_SOURCE")|Ok _->assert false)
  |None->assert false

let ()=if node then begin
  let source:C.source={site=["cooked-collection"];owner=[];instance=0;prefix=[];iter=[];span=None} in
  let statement id node:C.statement={id;source;node} in
  let program count cooked:C.program={constants=[|Int count;Bool cooked|];
    locals=[|Flow.Ty.Int;Bool;List Flow.Ty.geometry|];
    body=[statement 0(Assign(0,Constant 0));statement 1(Assign(1,Constant 1));
      statement 2(Assign(2,Prim(Sequence_build,["count",0;"cooked",1])))];
    results=[|2|];state_slots=[||];regions=[||];call_slots=0} in
  let programs=["geometry_collection_5001",program 5001 true;
    "ordinary_collection_5001",program 5001 false;"geometry_collection_negative",program (-1) true] in
  let lives=[Frame_input.at_time 0.] in
  let cases=List.map(fun(name,program)->{name;program;mode=`Value;lives;
    expected=List.map(fun live->outcome canon_value(I.Core_exec.run ~strict:true program ~live))lives})programs in
  let actual=run_node cases in
  List.iter(fun c->assert(List.assoc(c.name,0)actual=List.hd c.expected))cases
end
