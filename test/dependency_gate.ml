(* One dependency gate over the real library graph, read from every
   lib/**/dune. Rules are "may never reach" constraints on the transitive
   closure, "depends only on" whitelists of direct dependencies, and token
   scans for Metal outside its backends (lib, tools and
   test) and for any alternate renderer or backend selector. A known
   violation would be listed with the plan item that removes it; there is
   none. Run from the repo root. *)

(* ---- a tiny s-expression reader: enough for dune files ---- *)
type sexp = Atom of string | List of sexp list

let parse text =
  let n = String.length text and i = ref 0 in
  let rec skip () =
    if !i < n then match text.[!i] with
      | ' ' | '\n' | '\t' | '\r' -> incr i; skip ()
      | ';' -> while !i < n && text.[!i] <> '\n' do incr i done; skip ()
      | _ -> () in
  let rec item () =
    skip ();
    match text.[!i] with
    | '(' -> incr i; let rec items acc = skip ();
               if text.[!i] = ')' then (incr i; List (List.rev acc))
               else items (item () :: acc) in items []
    | '"' -> incr i; let start = !i in
        while text.[!i] <> '"' do (if text.[!i] = '\\' then incr i); incr i done;
        incr i; Atom (String.sub text start (!i - start - 1))
    | _ -> let start = !i in
        while !i < n && not (String.contains " \n\t\r()\";" text.[!i]) do incr i done;
        Atom (String.sub text start (!i - start)) in
  let rec all acc = skip (); if !i >= n then List.rev acc else all (item () :: acc) in
  all []

open Test_text

let contains text needle =
  try ignore (Str.search_forward (Str.regexp_string needle) text 0); true
  with Not_found -> false

let rec files dir = Sys.readdir dir |> Array.to_list |> List.concat_map (fun name ->
  let path = Filename.concat dir name in
  if Sys.file_exists path && Sys.is_directory path then
    (if name = "_build" then [] else files path) else [path])

(* library name -> direct dependencies (public names resolved; a library from
   outside the repo keeps its name and has no entry of its own) *)
let graph roots =
  let stanzas = List.concat_map files roots
    |> List.filter (fun p -> Filename.basename p = "dune")
    |> List.concat_map (fun p -> parse (read p) |> List.filter_map (function
      | List (Atom "library" :: fields) ->
          let field key = List.find_map (function
            | List (Atom k :: values) when k = key -> Some values | _ -> None) fields in
          let atoms = function Some v -> List.filter_map (function Atom a -> Some a | _ -> None) v | None -> [] in
          (match atoms (field "name") with
           | [name] -> Some (name, atoms (field "public_name"), atoms (field "libraries"))
           | _ -> None)
      | _ -> None)) in
  let resolve = Hashtbl.create 64 in
  List.iter (fun (name, public, _) ->
    Hashtbl.replace resolve name name; List.iter (fun p -> Hashtbl.replace resolve p name) public) stanzas;
  List.map (fun (name, _, deps) -> name,
    List.map (fun dep -> Option.value ~default:dep (Hashtbl.find_opt resolve dep)) deps) stanzas

let reach graph =
  let memo = Hashtbl.create 64 in
  let rec go name = match Hashtbl.find_opt memo name with
    | Some set -> set
    | None -> Hashtbl.replace memo name [];
        let direct = Option.value ~default:[] (List.assoc_opt name graph) in
        let set = List.sort_uniq compare (direct @ List.concat_map go direct) in
        Hashtbl.replace memo name set; set in
  go

let gpu = ["sdl3"; "sdl3_image"; "sdl3_ttf"; "sdl3_mixer"; "metal"; "ogpu_core"; "ogpu"; "ogpu_mock"; "ogpu_metal"; "ogpu_metal_native";
           "runtime"; "runtime_input"; "runtime_resources"; "scene_execution"]
let upper = ["param"; "flow"; "flow_ir"; "flow_web"; "flow_gpu"; "flow_graph"; "flow_sop"; "rays"; "editor_core"; "pxui"; "pxui_shell"; "pxui_graph"; "sop"; "rdk";
             "sop_catalog"; "rays_editor"]
let foundational = ["sdl3"; "sdl3_image"; "sdl3_ttf"; "sdl3_mixer"; "metal"; "ogpu_core"; "ogpu";
                    "native_layer_token"; "scene_command"; "lru"]

(* (library, libraries it may never reach) *)
let rules =
  List.map (fun lib -> lib, "runtime" :: "runtime_resources"
                            :: "rays_execution" :: upper) foundational
  @ [ "param", "flow" :: "rays" :: "rays_math" :: "rdk" :: "sop" :: "editor_core" :: "pxui" :: gpu ]
  @ [ "frame_input", upper @ gpu ]
  @ List.map (fun library -> library, "rays" :: "rays_math" :: "rdk" :: "sop" :: "editor_core"
       :: "pxui" :: "pxui_shell" :: "pxui_graph" :: "sop_catalog" :: "editor_document"
       :: "rays_editor" :: gpu) ["flow"; "ppx_rays"]
  @ ["flow_graph", ["sop"; "rdk"; "rays"; "pxui"; "pxui_shell"; "pxui_graph"; "sop_catalog";
       "editor_document"; "rays_editor"] @ gpu]
  @ ["flow_ir", ["flow_gpu"; "rays"; "pxui"; "pxui_shell"; "pxui_graph"; "sop";
       "rdk_core"; "rdk_exact"; "rdk_spatial"; "rdk_attrib"; "rdk_gen"; "rdk_curve";
       "rdk_mesh"; "rdk_boolean"; "rdk"; "rdk_rays"; "sop_catalog";
       "editor_document"; "rays_editor"] @ gpu]
  @ ["flow_sop", ["rays"; "pxui"; "pxui_shell"; "pxui_graph"; "sop_catalog";
       "editor_document"; "rays_editor"] @ gpu]
  @ ["flow_gpu", ["sop"; "rdk"; "rays_editor"; "pxui"; "pxui_shell"; "pxui_graph";
       "flow_sop"; "editor_document"; "sop_catalog"]]
  @ ["flow_web", ["rays"; "sop"; "rdk"; "flow_sop"; "flow_gpu"; "rays_editor"; "pxui";
       "pxui_shell"; "pxui_graph"; "editor_document"; "sop_catalog"] @ gpu]
  @ [ "ogpu_core", ["sdl3"; "metal"; "ogpu_metal_native"; "ogpu_metal"];
      "ogpu", ["sdl3"; "metal"; "ogpu_metal_native"; "ogpu_metal"];
      "ogpu_mock", ["sdl3"; "metal"; "ogpu_metal_native"; "ogpu_metal"];
      "runtime", ["metal"; "ogpu_metal_native"; "ogpu_metal"];
      "scene_execution", ["metal"; "ogpu_metal_native"; "ogpu_metal"];
      "rays_execution", ["metal"; "ogpu_metal_native"; "ogpu_metal"];
      "rays", ["metal"; "ogpu_metal_native"; "ogpu_metal"];
      "ogpu_metal_native", ["sdl3"; "runtime"; "rays"; "scene_execution"];
      "ogpu_metal", ["sdl3"; "runtime"; "rays"; "scene_execution"];
      "runtime", upper; "runtime_input", upper;
      "rays_execution", ["runtime_input"];
      "rays", ["pxui"; "pxui_shell"; "pxui_graph"; "sop"; "rdk";
                  "sop_catalog"; "rays_editor"];
      "rays_math", ["rays"; "rdk_core"; "rdk_exact"; "rdk_spatial"; "rdk_attrib"; "rdk_gen"; "rdk_curve"; "rdk_mesh"; "rdk_boolean"; "rdk"; "rdk_rays"; "sop"] @ gpu;
      "rdk_core", "rdk_exact" :: "rdk_spatial" :: "rdk_attrib" :: "rdk_gen" :: "rdk_curve" :: "rdk_mesh" :: "rdk_boolean" :: "rays" :: "rdk" :: "rdk_rays" :: "sop" :: gpu;
      "rdk_exact", "rdk_spatial" :: "rdk_attrib" :: "rdk_gen" :: "rdk_curve" :: "rdk_mesh" :: "rdk_boolean" :: "rays" :: "rdk" :: "rdk_rays" :: "sop" :: gpu;
      "rdk_spatial", "rdk_attrib" :: "rdk_gen" :: "rdk_curve" :: "rdk_mesh" :: "rdk_boolean" :: "rays" :: "rdk" :: "rdk_rays" :: "sop" :: gpu;
      "rdk_attrib", "rdk_gen" :: "rdk_curve" :: "rdk_mesh" :: "rdk_boolean" :: "rays" :: "rdk" :: "rdk_rays" :: "sop" :: gpu;
      "rdk_gen", "rdk_curve" :: "rdk_mesh" :: "rdk_boolean" :: "rays" :: "rdk" :: "rdk_rays" :: "sop" :: gpu;
      "rdk_curve", "rdk_gen" :: "rdk_mesh" :: "rdk_boolean" :: "rays" :: "rdk" :: "rdk_rays" :: "sop" :: gpu;
      "rdk_mesh", "rdk_boolean" :: "rays" :: "rdk" :: "rdk_rays" :: "sop" :: gpu;
      "rdk_boolean", "rays" :: "rdk" :: "rdk_rays" :: "sop" :: gpu;
      "rdk", "rays" :: "rdk_rays" :: "sop" :: "pxui" :: "pxui_shell" :: "sop_catalog" :: gpu;
      "sop", "rays" :: "rdk_rays" :: "pxui" :: "pxui_shell" :: "pxui_graph" :: "sop_catalog"
                    :: "rays_editor" :: gpu;
      "editor_core", ["pxui"; "pxui_shell"; "pxui_graph"; "rays_editor"; "sop";
                 "rdk"; "sop_catalog"];
      "editor_document", ["pxui"; "pxui_shell"; "pxui_graph"; "rays_editor"];
      "pxui", ["editor_core"; "pxui_shell"; "sop"; "rdk"; "pxui_graph";
               "rays_editor"];
      "pxui_shell", ["sop"; "rdk"; "sop_catalog";
                     "pxui_graph"; "rays_editor"];
      "pxui_graph", ["sop"; "rdk"; "pxui_shell"; "rays_editor"; "sop_catalog"];
      "sop_catalog", "rays" :: "rays_execution" :: "pxui" :: "pxui_shell" :: "pxui_graph"
                     :: "rays_editor" :: gpu;
      "rays_pathtracer", ["metal"; "ogpu_metal_native"; "ogpu_metal"; "ogpu_mock"; "sop";
                          "sop_catalog"; "flow"; "editor_core"; "editor_document"; "pxui"; "pxui_shell";
                          "pxui_graph"; "rays_editor"];
      "rdk_rays", ["metal"; "ogpu_metal_native"; "ogpu_metal"; "rdk_boolean"; "rdk"; "sop"; "sop_catalog";
                   "flow"; "editor_core"; "editor_document"; "pxui"; "pxui_shell"; "pxui_graph";
                   "rays_pathtracer"; "rays_editor"];
      "scene_execution_fixtures", "metal" :: "ogpu_metal_native" :: "ogpu_metal" :: "sdl3" :: "runtime"
                                  :: "runtime_resources" :: "rays_execution" :: upper ]

(* "Depends only on": every direct dependency a library lists, in or out of
   the repo, is one of these. A new edge is a boundary change: add it here,
   in specification/backend.md and with a test at the boundary. *)
let only =
  [ "param", []; "native_layer_token", []; "lru", [];
    "frame_input", [];
    "flow", ["param"; "frame_input"];
    "flow_ir", ["flow"; "param"; "rays_math"; "scene_command"];
    "flow_graph", ["flow"; "param"];
    "ogpu_core", ["native_layer_token"];
    "ogpu", ["ogpu_core"];
    "ogpu_mock", ["ogpu_core"];
    "metal", ["threads"; "native_layer_token"];
    "ogpu_metal_native", ["ogpu_core"; "metal"; "lru"];
    "ogpu_metal", ["ogpu_metal_native"; "metal"];
    "scene_execution_fixtures", ["scene_execution"; "ogpu"];
    "pxui_shell", ["rays"; "editor_core"; "pxui"];
    "sop_catalog", ["rays_math"; "rdk"; "sop"];
    "flow_gpu", ["flow"; "flow_ir"; "param"; "rays_math"; "ogpu_core"; "ogpu"; "rays_execution"; "lru"];
    "flow_web", ["flow"; "flow_ir"; "param"; "rays_math"; "scene_command"] ]

(* Files outside the Metal backend that name Metal on purpose: the binding
   tooling and the Metal conformance driver. *)
let metal_allowed path =
  String.starts_with ~prefix:"lib/metal/" path
  || String.starts_with ~prefix:"lib/ogpu_metal/" path
  || List.mem path
       [ "tools/codemod/metal_registry.ml"; "test/ogpu_conformance/test_metal.ml";
         "test/dependency_gate.ml" ]

(* Known violations: (library, reached, plan item that removes it). *)
let reach_exceptions : (string * string * string) list = []

(* identifiers outside comments and string literals *)
let code_tokens text =
  let n = String.length text and out = Buffer.create (String.length text) in
  let rec go i depth =
    if i < n then
      if i + 1 < n && text.[i] = '(' && text.[i+1] = '*' then (Buffer.add_char out ' '; go (i + 2) (depth + 1))
      else if depth > 0 && i + 1 < n && text.[i] = '*' && text.[i+1] = ')' then go (i + 2) (depth - 1)
      else if depth > 0 then go (i + 1) depth
      else if text.[i] = '"' then
        let rec skip j = if j >= n then j else if text.[j] = '\\' then skip (j + 2)
          else if text.[j] = '"' then j + 1 else skip (j + 1) in
        (Buffer.add_char out ' '; go (skip (i + 1)) 0)
      else (Buffer.add_char out text.[i]; go (i + 1) 0) in
  go 0 0; Buffer.contents out

let uses_metal text =
  let code = code_tokens text in
  let rec find i = match String.index_from_opt code i 'M', String.index_from_opt code i 'O' with
    | None, None -> false
    | a, b ->
        let j = min (Option.value a ~default:max_int) (Option.value b ~default:max_int) in
        let starts word = String.length code >= j + String.length word
          && String.sub code j (String.length word) = word
          && (j = 0 || not (match code.[j-1] with 'a'..'z'|'A'..'Z'|'0'..'9'|'_'|'.' -> true | _ -> false)) in
        (starts "Metal." || starts "Ogpu_metal." || starts "Ogpu_metal_native.") || find (j + 1) in
  find 0 || List.exists (fun w -> List.mem w ["Metal"; "Ogpu_metal"; "Ogpu_metal_native"])
    (let words = String.split_on_char ' ' (String.map (function '\n'|'\t'|';' -> ' ' | c -> c) code) in
     let rec opens = function "open" :: w :: rest -> w :: opens rest | _ :: rest -> opens rest | [] -> [] in
     opens words)

let uses_key_pressed text =
  contains (code_tokens text) "KeyPressed"

(* Native-only rendering: no second renderer, GL/Vulkan window, or backend
   selector anywhere in source text, strings and comments included. *)
let forbidden_native =
  [ "Rgba_presenter"; "create_rgba_presenter";
    "SDL_CreateRenderer"; "SDL_CreateSoftwareRenderer";
    "SDL_CreateWindowAndRenderer"; "SDL_RenderPresent";
    "SDL_RenderTexture"; "SDL_RenderGeometry"; "SDL_GL_";
    "SDL_WINDOW_OPENGL"; "SDL_WINDOW_VULKAN";
    "RAYS_RENDER_TARGET"; "RAYS_HEADLESS"; "RAYS_WEB";
    "RAYS_RENDERER"; "RAYS_BACKEND"; "RAYS_OPENGL";
    "--renderer"; "--backend"; ("--head" ^ "less"); ("--open" ^ "gl"); "Dynlink" ]

let violations graph ~scan =
  let reach = reach graph in
  let direct_errors = List.filter_map (fun (lib, dep, message) ->
    if List.mem dep (Option.value ~default:[] (List.assoc_opt lib graph))
    then Some message else None)
    (["pxui", "sdl3", "pxui depends directly on sdl3 (text input belongs to Scene/runtime)";
      "pxui", "scene_command", "pxui depends directly on scene_command (UI batches belong to Rays.Scene)"]
     @ List.map (fun sdl -> "rays", sdl,
         "rays depends directly on " ^ sdl ^ " (SDL services belong to runtime)")
         ["sdl3"; "sdl3_image"; "sdl3_ttf"; "sdl3_mixer"]) in
  let edge_errors = List.concat_map (fun (lib, forbidden) ->
    List.filter_map (fun target ->
      if List.mem target (reach lib)
         && not (List.exists (fun (l, t, _) -> l = lib && t = target) reach_exceptions)
      then Some (Printf.sprintf "%s reaches forbidden library %s" lib target) else None)
      forbidden) rules in
  let only_errors = List.concat_map (fun (lib, allowed) ->
    List.filter_map (fun dep -> if List.mem dep allowed then None
      else Some (Printf.sprintf "%s depends on %s (it may depend only on: %s)" lib dep
                   (String.concat ", " allowed)))
      (Option.value ~default:[] (List.assoc_opt lib graph))) only in
  let product path = not (String.starts_with ~prefix:"tools/" path || String.starts_with ~prefix:"test/" path) in
  let native_errors = List.concat_map (fun (path, text) ->
    if not (product path) then [] else
    List.filter_map (fun needle -> if contains text needle
      then Some (Printf.sprintf "%s names %s (Metal is the only renderer)" path needle)
      else None) forbidden_native) scan in
  let ocaml path = Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli" in
  let token_errors = List.filter_map (fun (path, text) ->
    if not (ocaml path) then None else
    if not (metal_allowed path) && uses_metal text then Some (path ^ " uses Metal outside lib/metal and lib/ogpu_metal")
    else if String.starts_with ~prefix:"lib/pxui_graph/" path
        (* its tests build key events to feed the pane *)
        && not (String.starts_with ~prefix:"lib/pxui_graph/test_" path)
        && uses_key_pressed text then
      Some (path ^ " matches KeyPressed inside a presentation adapter")
    else None) scan in
  direct_errors @ edge_errors @ only_errors @ token_errors @ native_errors

let run () =
  List.iter (fun (text, needle, expected) ->
    assert (contains text needle = expected))
    ["", "", true; "", "x", false; "x", "", true;
     "prefix Metal.suffix", "Metal.", true;
     "aaaab", "aaab", true; "a.b", ".", true;
     "a\\b", "\\", true; "abc", "abcd", false; "abc", "d", false];
  (* Exercise the actual dune reader, including public-name resolution and
     a forbidden edge hidden behind an otherwise innocuous private bridge. *)
  let fixture = Filename.temp_dir "rays-dependency-gate" "" in
  Fun.protect ~finally:(fun () -> Sys.remove (Filename.concat fixture "dune");
    Unix.rmdir fixture) (fun () ->
    Out_channel.with_open_text (Filename.concat fixture "dune") (fun out ->
      output_string out "; ignored comment\n(library (name editor_document) (package rays)\n\
        (libraries \"fixture.bridge\"))\n\
        (library (name bridge) (public_name \"fixture.bridge\") (libraries pxui))\n\
        (library (name pxui))\n(executable (name ignored) (libraries editor_document))\n");
    let fixture_graph = graph [fixture] in
    if List.assoc "editor_document" fixture_graph <> ["bridge"]
        || List.assoc "bridge" fixture_graph <> ["pxui"]
        || List.length fixture_graph <> 3 then
      failwith "dune reader lost quoted/public/private library dependencies";
    if violations fixture_graph ~scan:[] = [] then
      failwith "gate accepted a parsed transitive presentation dependency";
    let allowed = List.map (fun (name, dependencies) ->
      name, if name = "bridge" then [] else dependencies) fixture_graph in
    if violations allowed ~scan:[] <> [] then
      failwith "gate rejected the UI-free private document fixture");
  let has_library_field path name key value =
    parse (read path) |> List.exists (function
      | List (Atom "library" :: fields) ->
          List.mem (List [Atom "name"; Atom name]) fields
          && List.mem (List [Atom key; Atom value]) fields
      | _ -> false) in
  if not (has_library_field "lib/ogpu/dune" "ogpu" "virtual_modules" "Impl"
      && has_library_field "lib/ogpu/dune" "ogpu" "default_implementation" "ogpu_metal"
      && has_library_field "lib/ogpu_metal/dune" "ogpu_metal" "implements" "ogpu"
      && has_library_field "lib/ogpu_mock/dune" "ogpu_mock" "implements" "ogpu") then
    failwith "OGPU virtual implementations or default selection missing";
  let graph = graph ["lib"; "ppx"] in
  if List.length graph < 20 then failwith "dependency gate found too few libraries (wrong cwd?)";
  (* injected violations must fire *)
  let inject lib dep = List.map (fun (l, d) -> l, if l = lib then dep :: d else d) graph in
  List.iter (fun (lib, dep) ->
    if violations (inject lib dep) ~scan:[] = [] then
      failwith (Printf.sprintf "gate accepted injected edge %s -> %s" lib dep))
    ["ogpu_core", "metal"; "ogpu", "ogpu_metal_native"; "ogpu_mock", "metal"; "rays", "pxui"; "pxui", "sop"; "pxui", "sdl3"; "pxui", "scene_command"; "sdl3", "rays";
     "rdk", "rays"; "rdk_core", "rdk_exact";
     "rdk_exact", "rdk_boolean"; "rdk_spatial", "rdk_attrib";
     "rdk_attrib", "rdk_boolean"; "rdk_gen", "rdk_curve";
     "rdk_curve", "rdk_gen"; "rdk_mesh", "rdk_boolean"; "rdk_boolean", "rdk";
     "rdk_core", "rays";
     "rays_math", "rays"; "rays_execution", "runtime_input";
     "rays", "sdl3_ttf";
     "editor_document", "pxui"; "editor_document", "pxui_shell";
     "editor_document", "pxui_graph";
     "editor_document", "rays_editor";
     "flow", "pxui"; "flow", "sop"; "flow", "rays_math";
     "flow", "flow_ir"; "flow_ir", "rays"; "flow_ir", "sop";
     "flow_ir", "rdk"; "flow_ir", "pxui"; "flow_ir", "editor_document";
     "flow_ir", "rays_editor"; "flow_ir", "ogpu";
     "flow_ir", "flow_gpu"; "flow_gpu", "sop"; "flow_gpu", "rdk";
     "flow_gpu", "rays_editor"; "flow_gpu", "pxui";
     "flow", "rays"; "param", "flow"; "sdl3", "flow";
     "ppx_rays", "sop"; "ppx_rays", "pxui";
     "flow_sop", "pxui"; "flow_sop", "sop_catalog"; "flow_sop", "rays";
     "flow_sop", "editor_document"; "flow_sop", "ogpu"; "sdl3", "flow_sop";
     "flow", "flow_sop"; "ppx_rays", "flow_sop";
     "flow_web", "rays"; "flow_web", "flow_gpu"; "flow_web", "sop"; "flow_web", "rdk";
     "flow_web", "pxui"; "flow_web", "rays_editor"; "flow_web", "ogpu"; "flow_web", "unix";
     "scene_command", "flow_web";
     "sop_catalog", "rays"; "sop_catalog", "runtime"; "sop_catalog", "metal";
     "rays_pathtracer", "ogpu_metal"; "rays_pathtracer", "sop"; "rays_pathtracer", "pxui";
     "rdk_rays", "sop"; "rdk_rays", "metal"; "rdk_rays", "pxui";
     "scene_execution_fixtures", "metal"; "scene_execution_fixtures", "runtime";
     "scene_execution_fixtures", "rays";
     (* "depends only on": an edge the reach rules would let through *)
     "pxui_shell", "editor_document"; "pxui_shell", "scene_command"; "ogpu_core", "lru";
     "flow_ir", "lru"; "scene_command", "flow_ir";
     "ogpu", "native_layer_token"; "ogpu_mock", "lru"; "param", "unix"; "flow", "unix";
     "ogpu_metal_native", "native_layer_token"; "ogpu_metal", "lru"; "metal", "lru";
     "sop_catalog", "flow"];
  List.iter (fun (lib, _) -> if not (List.mem_assoc lib graph) then
    failwith ("dependency gate has a rule for unknown library " ^ lib)) (rules @ only);
  (* The Metal build takes its preprocessor from ppx/, never from tools/. *)
  if contains (read "lib/metal/dune") "tools/" then
    failwith "lib/metal/dune depends on tools/";
  if violations graph ~scan:["lib/rays/injected.ml", "let x = Metal.Device.system_default"] = []
     || violations graph ~scan:["lib/rays/injected.ml", "open Ogpu_metal_native"] = []
     || violations graph ~scan:["lib/rays/injected.ml", "open Ogpu_metal"] = []
     || violations graph ~scan:["tools/injected.ml", "let x = Metal.Device.system_default"] = []
     || violations graph ~scan:["test/injected.ml", "open Ogpu_metal_native"] = [] then
    failwith "gate accepted injected Metal reference";
  if violations graph ~scan:["lib/rays/ok.ml", "(* Metal.foo *) let s = \"Metal.framework\""] <> [] then
    failwith "gate flagged Metal inside a comment or string";
  if violations graph ~scan:["tools/codemod/metal_registry.ml", "let x = Metal.Device.system_default";
                              "test/ogpu_conformance/test_metal.ml", "let x = Metal.pp_error"] <> [] then
    failwith "gate flagged a listed Metal tooling file";
  if violations graph ~scan:["lib/pxui_graph/injected.ml", "Event.KeyPressed key"] = [] then
    failwith "gate accepted adapter key handling";
  if violations graph ~scan:["lib/pxui_graph/ok.ml", "(* KeyPressed *) let s = \"KeyPressed\""] <> [] then
    failwith "gate flagged KeyPressed inside a comment or string";
  if violations graph ~scan:["lib/sdl3/injected.c", "SDL_CreateRenderer(window, 0)"] = []
     || violations graph ~scan:["lib/rays/injected.ml", "Sys.getenv \"RAYS_BACKEND\""] = [] then
    failwith "gate accepted an injected alternate renderer or backend selector";
  let scan = List.concat_map files ["lib"; "tools"; "test"]
    |> List.filter (fun p -> not (Filename.check_suffix p ".pp.ml"))
    |> List.filter (fun p -> List.exists (Filename.check_suffix p)
      [".ml"; ".mli"; ".c"; ".h"; ".m"])
    |> List.map (fun p -> p, read p) in
  (* The one native path stays wired: SDL Metal view -> OGPU driver. *)
  List.iter (fun (path, anchor) -> if not (contains (read path) anchor) then
    failwith (path ^ " lost native anchor " ^ anchor))
    [ "lib/sdl3/sdl3_stubs.c", "SDL_Metal_CreateView";
      "lib/runtime/runtime.ml", "Ogpu.Impl.create_driver";
      "lib/rays_execution/rays_execution.ml", "Runtime.create" ];
  match violations graph ~scan with
  | [] -> Printf.printf "dependency gate: %d libraries, %d rules, %d whitelists, %d listed exceptions\n"
            (List.length graph) (List.length rules) (List.length only)
            (List.length reach_exceptions)
  | errors -> List.iter prerr_endline errors; exit 1
