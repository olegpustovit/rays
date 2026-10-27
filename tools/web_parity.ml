(* Frame parity of the web export without a browser (plan P5.12).  For each workspace directory: export its
   draw graph (Web_export), run 120 fixed-step frames natively (Core_exec.draw) at a pinned size and pointer,
   write each frame's mark table as JSON, and let tools/web/test/frames.mjs run the exported program.js under
   node for the same frames and compare: structure exactly, floats within 1e-6.
   Usage: web_parity.exe [--frames FILE] [--keep] DIR...  (cwd: the repository root).
   Prints one line per workspace: ok, refused CODE at SPAN, or failed. Exit 1 on a failure. *)
module E = Flow.Eval
module C = Flow_ir.Core_exec
module M = Scene_command.Marks

let frames = 120
let size = 640, 480
let dt = 1. /. 60.

let num x = Param.float_text x
let list f xs = "[" ^ String.concat "," (List.map f xs) ^ "]"
let pair (x, y) = list num [x; y]
(* A buffer of more than 4,096 numbers is compared by its length, its first and last 256 and its sum. *)
let sampled data =
  let n = Array.length data in
  if n <= 4096 then list num (Array.to_list data)
  else Printf.sprintf {|{"length":%d,"head":%s,"tail":%s,"sum":%s}|} n (list num (Array.to_list (Array.sub data 0 256)))
    (list num (Array.to_list (Array.sub data (n - 256) 256))) (num (Array.fold_left (+.) 0. data))
let attr (buffers : C.Buffers.t) f = function
  | M.Constant v -> Printf.sprintf {|{"constant":%s}|} (f v)
  | Buffer b ->
      let data = Array.sub (C.Buffers.find buffers b.id) 0 (b.count * b.width) in
      Printf.sprintf {|{"buffer":%s,"count":%d,"width":%d}|} (sampled data) b.count b.width
  | Device _ -> failwith "a device attribute has no web frame form"

let entry buffers = function
  | M.Mark_set {count; kind} ->
      let a f v = attr buffers f v and i = Int32.to_string in
      let fields = match kind with
        | M.Rect r -> ["rect", [ "position", a pair r.position; "size", a pair r.size; "fill", a i r.fill;
            "stroke", a i r.stroke; "stroke_width", a num r.stroke_width ]]
        | Circle r -> ["circle", [ "position", a pair r.position; "radius", a num r.radius; "fill", a i r.fill;
            "stroke", a i r.stroke; "stroke_width", a num r.stroke_width ]]
        | Line r -> ["line", [ "position", a pair r.position; "end", a pair r.end_; "stroke", a i r.stroke;
            "stroke_width", a num r.stroke_width ]]
        | Point r -> ["point", [ "position", a pair r.position; "fill", i r.fill ]] in
      let kind, fields = List.hd fields in
      Printf.sprintf {|{"type":"mark_set","kind":"%s","count":%d,%s}|} kind count
        (String.concat "," (List.map (fun (k, v) -> Printf.sprintf {|"%s":%s|} k v) fields))
  | Geometry g -> Printf.sprintf {|{"type":"geometry","vertices":%s,"indices":%s,"color":%ld}|}
      (list num (Array.to_list g.vertices)) (list string_of_int (Array.to_list g.indices)) g.color
  | Image i -> Printf.sprintf {|{"type":"image","resource":%d,"x":%d,"y":%d,"scale":%s,"angle":%s}|} i.resource i.x i.y (num i.scale) (num i.angle)
  | Text t -> Printf.sprintf {|{"type":"text","x":%d,"y":%d,"size":%d,"color":%ld,"text":%s}|} t.x t.y t.size t.color
      (Flow_web.Kernel_table.json_string t.text)
  | Push_transform t -> Printf.sprintf {|{"type":"push_transform","xx":%s,"xy":%s,"yx":%s,"yy":%s,"tx":%s,"ty":%s}|}
      (num t.xx) (num t.xy) (num t.yx) (num t.yy) (num t.tx) (num t.ty)
  | Pop_transform -> {|{"type":"pop_transform"}|}
  | Push_clip _ | Pop_clip -> {|{"type":"clip"}|}

(* The native frames, as the editor runs them (lib/rays_editor/drawing.ml render_core). *)
let native (lowered : Web_export.lowered) =
  let prepared = C.prepare lowered.program and state = E.create_state () and buffers = C.Buffers.create () in
  let image = function E.Deferred (_, id) -> Ok (id + 1)
    | _ -> Error (Diagnostic.error ~code:"E_IMAGE" "Expected an image plan node.") in
  List.init frames (fun k ->
    let i = k + 1 in
    let live = { (Input.at_time (float i *. dt)) with dt; frame = i; tick = i; size; pointer = (100., 100.) } in
    C.Buffers.clear buffers;
    let drawn = ref (Error (Diagnostic.error ~code:"E_CORE" "The draw program did not run.")) in
    (match lowered.root with
     | None -> drawn := C.draw ~image prepared ~live ~size ~buffers
     | Some root ->
         ignore (Result.get_ok (E.Residual.bind_live ~state root ~live));
         drawn := C.draw ~root ~image prepared ~live ~size ~buffers);
    match !drawn with
    | Ok marks -> Printf.sprintf {|{"ok":%s}|} (list (entry buffers) (Array.to_list marks))
    | Error d -> Printf.sprintf {|{"error":"%s"}|} d.code)

let node_available () = Sys.command "command -v node >/dev/null 2>&1" = 0

let span_text text (d : Diagnostic.t) = match d.span with
  | Some s -> let p = Diagnostic.position_of_offset text s.start in Printf.sprintf "%d:%d" p.line p.col
  | None -> "no span"

(* One workspace: [`Ok], [`Refused text] or [`Failed text]. *)
let check ~frames_mjs ~keep directory =
  let path = directory in
  let text, doc = Web_export.load path in
  match doc with
  | Error (d :: _) -> `Failed ("does not check: " ^ Diagnostic.to_string d)
  | Error [] -> `Failed "does not check"
  | Ok doc ->
      match Result.bind (Web_export.lower doc) (fun l -> Result.map (fun f -> l, f) (Web_export.files doc l)) with
      | Error d -> `Refused (Printf.sprintf "%s at %s" d.code (span_text text d))
      | Ok (lowered, files) ->
          let dir = Filename.temp_file "web_parity" "" in
          Sys.remove dir; Sys.mkdir dir 0o755; Sys.mkdir (Filename.concat dir "runtime") 0o755;
          List.iter (fun (name, contents) ->
            Out_channel.with_open_bin (Filename.concat dir name) (fun oc -> output_string oc contents)) files;
          let expected = Filename.concat dir "native.json" in
          Out_channel.with_open_bin expected (fun oc -> output_string oc (list Fun.id (native lowered)));
          let status = Sys.command (Printf.sprintf "node %s %s %s" (Filename.quote frames_mjs) (Filename.quote dir) (Filename.quote expected)) in
          if not keep then ignore (Sys.command ("rm -rf " ^ Filename.quote dir)) else Printf.printf "  kept %s\n" dir;
          if status = 0 then `Ok else `Failed (Printf.sprintf "node exited %d" status)

let () =
  let rec parse frames_mjs keep dirs = function
    | "--frames" :: file :: rest -> parse file keep dirs rest
    | "--keep" :: rest -> parse frames_mjs true dirs rest
    | dir :: rest -> parse frames_mjs keep (dir :: dirs) rest
    | [] -> frames_mjs, keep, List.rev dirs in
  let frames_mjs, keep, dirs = parse "tools/web/test/frames.mjs" false [] (List.tl (Array.to_list Sys.argv)) in
  if dirs = [] then (prerr_endline "usage: web_parity.exe [--frames FILE] [--keep] FILE.rays..."; exit 2);
  if not (node_available ()) then (print_endline "web_parity: skipped (node not found)"; exit 0);
  let failed = List.fold_left (fun failed dir ->
    match check ~frames_mjs ~keep dir with
    | `Ok -> Printf.printf "%-34s ok (%d frames)\n%!" dir frames; failed
    | `Refused why -> Printf.printf "%-34s refused %s\n%!" dir why; failed
    | `Failed why -> Printf.printf "%-34s FAILED %s\n%!" dir why; true) false dirs in
  if failed then exit 1
