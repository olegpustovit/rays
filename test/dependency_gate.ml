(* One dependency gate over the real library graph, read from Dune files in
   lib, ppx, tools, test and bin when present. Rules are transitive "may never reach"
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

let read path = In_channel.with_open_bin path In_channel.input_all
open Parsetree

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
           "runtime"; "runtime_input"; "runtime_resources"; "renderer"]
let upper = ["param"; "flow"; "flow_ir"; "flow_web"; "flow_gpu"; "flow_graph"; "flow_sop"; "flow_native"; "rays"; "editor_core"; "pxui"; "pxui_shell"; "pxui_graph"; "sop"; "rdk";
             "sop_catalog"; "rays_editor"]
let foundational = ["sdl3"; "sdl3_image"; "sdl3_ttf"; "sdl3_mixer"; "metal";
                    "native_layer_token"; "scene_command"; "lru"; "rays_diagnostic"]

(* (library, libraries it may never reach) *)
let rules =
  List.map (fun lib -> lib, "runtime" :: "runtime_resources"
                            :: "renderer" :: upper) foundational
  @ ["ppx_rays", "rays" :: "rdk" :: "sop" :: "editor_core"
       :: "pxui" :: "pxui_shell" :: "pxui_graph" :: "sop_catalog" :: "editor_document"
       :: "rays_editor" :: gpu]
  @ [ "runtime", ["metal"; "ogpu_metal_native"; "ogpu_metal"];
      "renderer", ["metal"; "ogpu_metal_native"; "ogpu_metal"];
      "rays", ["metal"; "ogpu_metal_native"; "ogpu_metal"];
      "ogpu_metal_native", ["sdl3"; "runtime"; "rays"; "renderer"];
      "ogpu_metal", ["sdl3"; "runtime"; "rays"; "renderer"];
      "runtime", upper; "runtime_input", upper;
      "flow_native", ["pxui"; "pxui_shell"; "pxui_graph"; "rays_editor"];
      "web_export", ["pxui"; "pxui_shell"; "pxui_graph"; "rays_editor"];
      "lisp_sop", ["editor_document"; "rays"; "runtime"; "pxui"; "rays_editor"];
      "renderer", ["runtime_input"];
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
      "sop_catalog", "rays" :: "renderer" :: "pxui" :: "pxui_shell" :: "pxui_graph"
                     :: "rays_editor" :: gpu;
      "rays_pathtracer", ["metal"; "ogpu_metal_native"; "ogpu_metal"; "ogpu_mock"; "rdk_core"; "sop";
                          "sop_catalog"; "flow_sop"; "editor_core"; "editor_document"; "pxui"; "pxui_shell";
                          "pxui_graph"; "rays_editor"];
      "rdk_rays", ["metal"; "ogpu_metal_native"; "ogpu_metal"; "rdk_boolean"; "rdk"; "sop"; "sop_catalog";
                   "flow"; "editor_core"; "editor_document"; "pxui"; "pxui_shell"; "pxui_graph";
                   "rays_pathtracer"; "rays_editor"];
      "scene_execution_fixtures", "metal" :: "ogpu_metal_native" :: "ogpu_metal" :: upper ]

(* "Depends only on": every direct dependency a library lists, in or out of
   the repo, is one of these. A new edge is a boundary change: add it here,
   in specification/backend.md and with a test at the boundary. *)
let only =
  [ "param", []; "native_layer_token", []; "lru", []; "rays_diagnostic", [];
    "input", [];
    "runtime", ["sdl3"; "ogpu"];
    "runtime_input", ["sdl3"; "input"];
    "renderer", ["ogpu"; "scene_command"; "runtime"; "lru"; "rays_math"; "runtime_resources"];
    "flow", ["param"; "input"; "rays_diagnostic"; "rays_math"];
    "sop", ["param"; "input"; "rays_diagnostic"; "rays_math"; "rdk"; "lru"; "unix"; "parallel"];
    "flow_sop", ["flow"; "flow_ir"; "lru"; "param"; "sop"; "rays_diagnostic"; "rays_math"; "unix"; "parallel"];
    "flow_ir", ["flow"; "param"; "rays_math"; "scene_command"; "lru"; "parallel"];
    "flow_graph", ["flow"; "param"];
    "flow_native", ["editor_document"; "rays"; "rays_math"; "input"; "rdk"; "flow"; "flow_ir";
                    "flow_gpu"; "flow_sop"; "scene_command"; "renderer"; "lru";
                    "runtime_resources"; "parallel"; "sop"; "unix"];
    "ogpu_core", ["native_layer_token"];
    "ogpu", ["ogpu_core"];
    "ogpu_mock", ["ogpu_core"];
    "metal", ["threads"; "native_layer_token"];
    "ogpu_metal_native", ["ogpu_core"; "metal"; "lru"];
    "ogpu_metal", ["ogpu_metal_native"; "metal"];
    "scene_execution_fixtures", ["renderer"; "ogpu"];
    "pxui", ["rays"; "rays_math"];
    "pxui_shell", ["rays"; "rays_math"; "editor_core"; "pxui"];
    "sop_catalog", ["rays_math"; "rdk"; "sop"];
    "flow_gpu", ["flow"; "flow_ir"; "param"; "rays_math"; "ogpu_core"; "ogpu"; "renderer"; "lru"; "parallel"];
    "flow_web", ["flow"; "flow_ir"; "param"; "rays_math"; "scene_command"; "parallel"];
    "web_export", ["flow"; "flow_ir"; "flow_web"; "flow_sop"; "editor_document";
                   "sop_catalog"; "input"; "param"; "scene_command"; "sdl3_image"];
    "rays_pathtracer", ["rays"; "ogpu"; "renderer"; "flow"; "flow_ir"; "flow_gpu";
                       "runtime_resources"; "rays_math"; "unix"];
    "lisp_sop", ["flow"; "flow_sop"; "sop_catalog"; "sop"; "rdk"; "rays_math"] ]

(* A backend's modules stay within that backend: its modules, the directories
   that own them, and the files that name it on purpose (binding tooling,
   the conformance driver). A new backend adds one row. *)
let backends =
  [ ["Metal"; "Ogpu_metal"; "Ogpu_metal_native"], ["lib/metal/"; "lib/ogpu_metal/"],
    ["tools/codemod/metal_registry.ml"; "lib/ogpu/conformance/test_metal.ml"; "test/dependency_gate.ml"] ]

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

(* Reject refs allocated during module initialization. Function bodies are
   skipped so ordinary call-local refs stay legal. *)
let module_initialization_refs source =
  let structure = Parse.implementation (Lexing.from_string source) in
  let refs = ref [] in
  let is_ref = function
    | Longident.Lident "ref"
    | Longident.Ldot (Longident.Lident "Stdlib", "ref")
    | Longident.Ldot (Longident.Lident "Pervasives", "ref") -> true
    | _ -> false in
  let iterator = { Ast_iterator.default_iterator with expr = (fun self expression ->
    match expression.pexp_desc with
    | Pexp_function _ -> ()
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt; _ }; _ }, _) when is_ref txt ->
        refs := expression.pexp_loc.loc_start.pos_lnum :: !refs;
        Ast_iterator.default_iterator.expr self expression
    | _ -> Ast_iterator.default_iterator.expr self expression) } in
  iterator.structure iterator structure;
  List.rev !refs

let uses_backend modules text =
  let code = code_tokens text in
  let rec find i = match String.index_from_opt code i 'M', String.index_from_opt code i 'O' with
    | None, None -> false
    | a, b ->
        let j = min (Option.value a ~default:max_int) (Option.value b ~default:max_int) in
        let starts word = String.length code >= j + String.length word
          && String.sub code j (String.length word) = word
          && (j = 0 || not (match code.[j-1] with 'a'..'z'|'A'..'Z'|'0'..'9'|'_'|'.' -> true | _ -> false)) in
        List.exists (fun m -> starts (m ^ ".")) modules || find (j + 1) in
  find 0 || List.exists (fun w -> List.mem w modules)
    (let words = String.split_on_char ' ' (String.map (function '\n'|'\t'|';' -> ' ' | c -> c) code) in
     let rec opens = function "open" :: w :: rest -> w :: opens rest | _ :: rest -> opens rest | [] -> [] in
     opens words)

let uses_key_pressed text =
  contains (code_tokens text) "Key_pressed"

(* Test stanzas get Dune's [runtest] alias implicitly. Hand-written test
   executables need an explicit alias; their dispatcher labels must also be
   named by a reachable rule. Keep this inventory based on Dune stanzas and
   [test_main] dispatch tables so ordinary tools and benchmarks are ignored. *)
let test_lanes = ["runtest"; "runtest-native"; "qualification";
                  "qualification-scale"; "qualification-web"]

let field key = function
  | List (_ :: fields) -> List.find_map (function
      | List (Atom name :: values) when name = key -> Some values
      | _ -> None) fields
  | _ -> None

let atoms = function
  | Some values -> List.filter_map (function Atom value -> Some value | _ -> None) values
  | None -> []

let stanza_names stanza =
  let names = atoms (field "name" stanza) in
  let names = if names <> [] then names else atoms (field "names" stanza) in
  names

let stanza_name stanza = match stanza_names stanza with
  | [name] -> Some name | _ -> None

let alias_references sexp =
  let rec visit = function
    | List [Atom "alias"; Atom name] -> [name]
    | List values -> List.concat_map visit values
    | Atom _ -> [] in
  visit sexp

let normalize_path path =
  let absolute = String.starts_with ~prefix:"/" path in
  let parts = String.split_on_char '/' path |> List.fold_left (fun acc part ->
    match part, acc with
    | ("" | "."), _ -> acc
    | "..", head :: rest when head <> ".." -> rest
    | "..", _ when absolute -> acc
    | "..", _ -> ".." :: acc
    | _, _ -> part :: acc) [] |> List.rev in
  (if absolute then "/" else "") ^ String.concat "/" parts

let run_invocations dir sexp =
  let executable atom =
    let prefix = "%{exe:" and suffix = ".exe}" in
    let n = String.length atom in
    if n > String.length prefix + String.length suffix
       && String.sub atom 0 (String.length prefix) = prefix
       && String.sub atom (n - String.length suffix) (String.length suffix) = suffix
    then Some (normalize_path (Filename.concat dir (String.sub atom (String.length prefix)
      (n - String.length prefix - String.length suffix))))
    else None in
  let rec visit = function
    | List (Atom "run" :: Atom target :: args) ->
        let args = List.filter_map (function Atom value -> Some value | _ -> None) args in
        (match executable target with Some name -> [name, args] | None -> [])
    | List values -> List.concat_map visit values
    | Atom _ -> [] in
  visit sexp

let dispatcher_entries source =
  let marker = "let tests = [" in
  match Str.search_forward (Str.regexp_string marker) source 0 with
  | exception Not_found -> []
  | start ->
      let body_start = start + String.length marker in
      let body = String.sub source body_start (String.length source - body_start) in
      let lines = String.split_on_char '\n' body in
      let rec take acc = function
        | [] -> List.rev acc
        | line :: rest when String.trim line = "]" -> List.rev acc
        | line :: rest ->
            let rec strings i acc =
              let i = ref i in
              while !i < String.length line &&
                    (line.[!i] = ' ' || line.[!i] = '\t' || line.[!i] = ',') do incr i done;
              if !i < String.length line && line.[!i] = '"' then begin
                let finish = String.index_from line (!i + 1) '"' in
                let value = String.sub line (!i + 1) (finish - !i - 1) in
                strings (finish + 1) (value :: acc)
              end else List.rev acc in
            let entry = strings 0 [] in
            take (if entry = [] then acc else entry :: acc) rest in
      take [] lines

type test_rule = { dir : string; aliases : string list; invocations : (string * string list) list }

let backend_selections graph dune_files =
  let stanzas = List.concat_map (fun path ->
    List.map (fun stanza -> path, stanza) (parse (read path))) dune_files in
  let aliases = Hashtbl.create 64 in
  List.iter (function
    | _, (List (Atom "library" :: _) as stanza) ->
        Option.iter (fun name ->
          List.iter (fun public -> Hashtbl.replace aliases public name)
            (atoms (field "public_name" stanza))) (stanza_name stanza)
    | _ -> ()) stanzas;
  let resolve name = Option.value ~default:name (Hashtbl.find_opt aliases name) in
  let reachable = reach graph in
  List.filter_map (function
    | path, (List (Atom ("executable" | "executables" | "test" | "tests") :: _) as stanza) ->
        let dependencies = List.map resolve (atoms (field "libraries" stanza)) in
        let uses_ogpu = List.exists (fun name ->
          name = "ogpu" || List.mem "ogpu" (reachable name)) dependencies in
        let selected = List.filter (fun name -> name = "ogpu_metal" || name = "ogpu_mock")
          dependencies |> List.sort_uniq String.compare in
        if uses_ogpu && List.length selected <> 1 then
          Some (Printf.sprintf "%s: %s must explicitly select one OGPU implementation"
            path (String.concat ", " (stanza_names stanza)))
        else None
    | _ -> None) stanzas

let library_copies path stanzas =
  List.concat_map (fun stanza -> match stanza with
    | List (Atom ("copy_files" | "copy_files#") :: values) ->
        let patterns = match values with [Atom pattern] -> [pattern]
          | _ -> atoms (field "files" stanza) in
        List.filter_map (fun pattern ->
          let source = normalize_path (Filename.concat (Filename.dirname path) pattern) in
          if String.starts_with ~prefix:"lib/" source
             && List.exists (Filename.check_suffix source) [".ml"; ".mli"] then
            Some (path ^ ": copies library source " ^ source)
          else None) patterns
    | _ -> []) stanzas

let test_support_dependencies graph products =
  let reachable = reach graph in
  List.filter_map (fun name ->
    if name <> "flow_ir_test_support"
       && List.mem "flow_ir_test_support" (reachable name) then
      Some (name ^ " reaches test-only flow_ir_test_support")
    else None) products

let lane_aliases dune_files =
  let edges = Hashtbl.create 128 and actions = ref [] and test_executables = ref [] in
  let coupled = ref [] in
  let executable_stanzas = ref [] in
  let add_edge source target =
    let old = Option.value ~default:[] (Hashtbl.find_opt edges source) in
    Hashtbl.replace edges source (target :: old) in
  let key dir alias = normalize_path (Filename.concat dir alias) in
  let add_rule dir aliases deps action =
    List.iter (fun alias ->
      let source = key dir alias in
      List.iter (fun dep -> add_edge source (key dir dep)) deps) aliases;
    if aliases <> [] then
      actions := {dir; aliases; invocations = run_invocations dir action} :: !actions in
  List.iter (fun path ->
    let dir = Filename.dirname path in
    parse (read path) |> List.iter (function
      | (List (Atom "test" :: _) as stanza) ->
          Option.iter (fun name -> test_executables := (dir, name, true) :: !test_executables)
            (stanza_name stanza)
      | (List (Atom ("executable" | "executables") :: _) as stanza) ->
          List.iter (fun name -> executable_stanzas := (dir, name) :: !executable_stanzas)
            (stanza_names stanza)
      | (List (Atom "rule" :: _) as stanza) ->
          let aliases = atoms (field "alias" stanza) @ atoms (field "aliases" stanza) in
          if List.length aliases > 1 then
            coupled := (path ^ ": coupled rule aliases; use one focused alias and lane dependencies") :: !coupled;
          let deps = alias_references (List (Option.value ~default:[] (field "deps" stanza))) in
          let action = List (Option.value ~default:[] (field "action" stanza)) in
          add_rule dir aliases deps action
      | (List (Atom "alias" :: _) as stanza) ->
          let aliases = atoms (field "name" stanza) in
          let deps = alias_references (List (Option.value ~default:[] (field "deps" stanza))) in
          add_rule dir aliases deps (List [])
      | _ -> ())) dune_files;
  let reachable = Hashtbl.create 128 in
  let rec mark alias = if not (Hashtbl.mem reachable alias) then begin
    Hashtbl.add reachable alias ();
    List.iter mark (Option.value ~default:[] (Hashtbl.find_opt edges alias))
  end in
  List.iter (fun path -> List.iter (fun lane -> mark (key (Filename.dirname path) lane)) test_lanes) dune_files;
  let is_reachable_rule rule = List.exists (fun alias -> Hashtbl.mem reachable (key rule.dir alias)) rule.aliases in
  List.iter (fun (dir, name) ->
    let test_alias rule = is_reachable_rule rule || List.exists (fun alias ->
      String.starts_with ~prefix:"test_" alias || List.mem alias test_lanes) rule.aliases in
    let referenced = List.exists (fun rule -> rule.dir = dir && test_alias rule
      && List.exists (fun (exe, _) -> exe = normalize_path (Filename.concat dir name)) rule.invocations) !actions in
    let utility = String.starts_with ~prefix:"bench_" name || String.starts_with ~prefix:"dump_" name in
    let named_test = not utility && (name = "test_main" || String.starts_with ~prefix:"test_" name
      || String.ends_with ~suffix:"_tests" (Filename.basename dir)) in
    if named_test || (referenced && not utility) then
      test_executables := (dir, name, false) :: !test_executables) !executable_stanzas;
  let errors = ref !coupled in
  List.iter (fun (path, name, implicit_runtest) ->
    if not implicit_runtest then begin
      let calls = List.filter (fun rule -> rule.dir = path && is_reachable_rule rule
        && List.exists (fun (exe, _) -> exe = normalize_path (Filename.concat path name)) rule.invocations) !actions in
      if calls = [] then errors := (Filename.concat path (name ^ ".exe") ^ " is not reachable from a test lane") :: !errors
    end;
    if name = "test_main" then begin
      let source = Filename.concat path "test_main.ml" in
      if Sys.file_exists source then begin
        let entries = dispatcher_entries (read source) in
        let seen = Hashtbl.create 32 in
        List.iter (fun labels ->
          let identity = List.hd (List.rev labels) in
          let utility = String.starts_with ~prefix:"bench_" identity || String.starts_with ~prefix:"dump_" identity in
          if not utility && Hashtbl.mem seen identity then
            errors := (Printf.sprintf "%s has duplicate dispatcher entry %S" source identity) :: !errors
          else if not utility then Hashtbl.add seen identity ();
          let called reachable = List.exists (fun rule -> rule.dir = path &&
            (not reachable || is_reachable_rule rule)
            && List.exists (fun (exe, args) -> exe = normalize_path (Filename.concat path name) && List.exists (fun label -> List.mem label args) labels) rule.invocations) !actions in
          if not utility && not (called true) then
            errors := (Printf.sprintf "%s dispatch entry %S is not reachable from a test lane" source identity) :: !errors;
          if not utility && not (called false) then
            errors := (Printf.sprintf "%s dispatch entry %S has no Dune invocation" source identity) :: !errors)
          entries
      end
    end) !test_executables;
  List.rev !errors

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
      then Some (Printf.sprintf "%s reaches forbidden library %s" lib target) else None)
      forbidden) rules in
  let only_errors = List.concat_map (fun (lib, allowed) ->
    List.filter_map (fun dep -> if List.mem dep allowed then None
      else Some (Printf.sprintf "%s depends on %s (it may depend only on: %s)" lib dep
                   (String.concat ", " allowed)))
      (Option.value ~default:[] (List.assoc_opt lib graph))) only in
  let product path = not (String.starts_with ~prefix:"tools/" path || String.starts_with ~prefix:"test/" path || contains path "/test/") in
  let native_errors = List.concat_map (fun (path, text) ->
    if not (product path) then [] else
    List.filter_map (fun needle ->
      let in_code = contains (code_tokens text) needle in
      let selector = String.starts_with ~prefix:"RAYS_" needle
        || String.starts_with ~prefix:"--" needle in
      if in_code || (selector && contains text needle)
      then Some (Printf.sprintf "%s names %s (Metal is the only renderer)" path needle)
      else None) forbidden_native) scan in
  let ocaml path = Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli" in
  let token_errors = List.filter_map (fun (path, text) ->
    if not (ocaml path) then None else
    match List.find_opt (fun (modules, dirs, files) ->
        not (List.exists (fun d -> String.starts_with ~prefix:d path) dirs || List.mem path files)
        && uses_backend modules text) backends with
    | Some (modules, dirs, _) ->
        Some (Printf.sprintf "%s uses %s outside %s" path (List.hd modules) (String.concat " and " dirs))
    | None ->
    if String.starts_with ~prefix:"lib/pxui_graph/" path
        (* its tests build key events to feed the pane *)
        && not (String.starts_with ~prefix:"lib/pxui_graph/test_" path)
        && uses_key_pressed text then
      Some (path ^ " matches Key_pressed inside a presentation adapter")
    else None) scan in
  (* the executable parses the environment once; library code takes explicit arguments *)
  let env_errors = List.filter_map (fun (path, text) ->
    if not (String.starts_with ~prefix:"lib/" path && Filename.check_suffix path ".ml")
       || String.starts_with ~prefix:"test_" (Filename.basename path) || contains path "/test/" then None else
    if List.exists (fun line -> contains line "getenv" && contains line "\"RAYS_")
         (String.split_on_char '\n' text)
    then Some (path ^ " reads a RAYS_ environment variable; bin/rays.ml parses it and passes it down")
    else None) scan in
  let module_ref_errors = List.concat_map (fun (path, text) ->
    if not (String.starts_with ~prefix:"lib/rays/" path)
       || not (Filename.check_suffix path ".ml") then []
    else List.map (fun line ->
      Printf.sprintf "%s:%d allocates a module-level ref; put mutable state in its run or renderer owner"
        path line) (module_initialization_refs text)) scan in
  direct_errors @ edge_errors @ only_errors @ token_errors @ native_errors @ env_errors @ module_ref_errors

(* [X.Private] across libraries is an API in disguise. A reference whose first module is not
   defined in the referencing library must be in Private_allowlist, and the list may not keep
   rows for references that are gone. *)
let private_errors scan =
  let owner path = match String.split_on_char '/' path with
    | "lib" :: dir :: _ :: _ -> Some dir
    | ("tools" | "bin" | "ppx" as top) :: _ -> Some top
    | _ -> None in
  let product path = (Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli")
    && owner path <> None && not (contains path "/test/" || contains path "/conformance/")
    && not (List.exists (fun p -> String.starts_with ~prefix:p (Filename.basename path)) ["test_"; "bench_"]) in
  let defined = Hashtbl.create 512 in
  List.iter (fun (path, _) -> if product path && Filename.check_suffix path ".ml" then
    Hashtbl.replace defined (String.capitalize_ascii (Filename.remove_extension (Filename.basename path)), owner path) ()) scan;
  let re = Str.regexp "\\b\\([A-Z][A-Za-z_0-9]*\\)\\(\\.[A-Z][A-Za-z_0-9]*\\)*\\.Private\\b" in
  let found = Hashtbl.create 128 in
  List.iter (fun (path, text) -> if product path then begin
    let rec go i = match Str.search_forward re text i with
      | exception Not_found -> ()
      | _ ->
          let m = Str.matched_string text and first = Str.matched_group 1 text in
          if not (Hashtbl.mem defined (first, owner path)) then
            Hashtbl.replace found (Option.get (owner path), m) ();
          go (Str.match_end ()) in
    go 0 end) scan;
  let allowed = Private_allowlist.entries in
  List.filter_map (fun (d, m) -> if List.mem (d, m) allowed then None
    else Some (Printf.sprintf "%s: new cross-library %s (give it a public name; the allowlist only shrinks)" d m))
    (Hashtbl.fold (fun k () acc -> k :: acc) found [])
  @ List.filter_map (fun (d, m) -> if Hashtbl.mem found (d, m) then None
    else Some (Printf.sprintf "%s: %s is gone; delete its row from test/private_allowlist.ml" d m)) allowed

let run () =
  if module_initialization_refs
      "let global = ref 0\nlet local () = ref 1\nlet deferred = fun () -> ref 2\n"
      <> [1] then
    failwith "module-ref scan must reject initializer refs and allow local refs";
  List.iter (fun (text, needle, expected) ->
    assert (contains text needle = expected))
    ["", "", true; "", "x", false; "x", "", true;
     "prefix Metal.suffix", "Metal.", true;
     "aaaab", "aaab", true; "a.b", ".", true;
     "a\\b", "\\", true; "abc", "abcd", false; "abc", "d", false];
  let copies = library_copies "lib/flow_gpu/dune" (parse
      "(copy_files ../flow_ir/test_program.ml)\n\
       (copy_files# (files ../flow_ir/*.mli))\n(copy_files goldens/*.metal)\n") in
  if List.length copies <> 2 then failwith "library-source copy guard missed a form";
  if test_support_dependencies ["product", ["bridge"]; "bridge", ["flow_ir_test_support"]]
       ["product"] = [] then
    failwith "gate accepted a transitive product dependency on test fixtures";
  (* The test census follows custom aliases through an aggregate lane and
     rejects an orphan dispatcher entry without scanning tool executables. *)
  let fixture = Filename.temp_dir "rays-test-lanes" "" in
  Fun.protect ~finally:(fun () ->
    Sys.remove (Filename.concat fixture "dune");
    Sys.remove (Filename.concat fixture "test_main.ml");
    Unix.rmdir fixture) (fun () ->
    Out_channel.with_open_text (Filename.concat fixture "dune") (fun out ->
      output_string out "(executable (name test_main))\n\
        (executable (name test_orphan))\n\
        (executables (names run_core bench_example))\n\
        (rule (alias test_core) (action (run %{exe:./test_main.exe} core)))\n\
        (rule (alias test_other) (action (run %{exe:test_main.exe} other)))\n\
        (rule (alias test_nodes) (action (run %{exe:run_core.exe})))\n\
        (rule (alias test_orphan) (action (run %{exe:test_orphan.exe})))\n\
        (rule (aliases runtest-native test_coupled) (action (run %{exe:bench_example.exe})))\n\
        (alias (name runtest) (deps (alias test_core) (alias test_other) (alias test_nodes)))\n");
    Out_channel.with_open_text (Filename.concat fixture "test_main.ml") (fun out ->
      output_string out "let tests = [\n  \"core\", \"test_one\", run;\n  \"core\", \"test_two\", run;\n  \"other\", \"test_one\", run;\n  \"core\", \"dump_data\", run;\n  \"orphan\", \"test_unselected\", run;\n]\n");
    let errors = lane_aliases [Filename.concat fixture "dune"] in
    if not (List.exists (fun error -> contains error "coupled rule aliases") errors)
       || not (List.exists (fun error -> contains error "duplicate dispatcher entry \"test_one\"") errors)
       || not (List.exists (fun error -> contains error "test_orphan.exe is not reachable") errors)
       || List.exists (fun error -> contains error "dispatch entry \"test_two\"") errors
       || not (List.exists (fun error -> contains error "dispatch entry \"test_unselected\" is not reachable") errors)
       || List.exists (fun error -> contains error "run_core.exe") errors
       || List.exists (fun error -> contains error "bench_example") errors
       || not (List.exists (fun error -> contains error "dispatch entry \"test_unselected\" has no Dune invocation") errors) then
      failwith ("test-lane census missed grouped, duplicate, or orphan test entries: "
        ^ String.concat "; " errors);
    Out_channel.with_open_text (Filename.concat fixture "dune") (fun out ->
      output_string out "(library (name ogpu))\n\
        (library (name client) (public_name fixture.client) (libraries ogpu))\n\
        (library (name ogpu_mock))\n(library (name ogpu_metal))\n\
        (executable (name missing) (libraries fixture.client))\n\
        (executable (name ambiguous) (libraries client ogpu_mock ogpu_metal))\n\
        (executable (name explicit) (libraries fixture.client ogpu_mock))\n\
        (executable (name cpu) (libraries unix))\n");
    let errors = backend_selections (graph [fixture]) [Filename.concat fixture "dune"] in
    if List.length errors <> 2
       || not (List.exists (fun error -> contains error "missing must explicitly select") errors)
       || not (List.exists (fun error -> contains error "ambiguous must explicitly select") errors) then
      failwith ("backend-selection check missed absent/ambiguous implementations or public aliases: "
        ^ String.concat "; " errors));
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
      && has_library_field "lib/ogpu_metal/dune" "ogpu_metal" "implements" "ogpu"
      && has_library_field "lib/ogpu_mock/dune" "ogpu_mock" "implements" "ogpu") then
    failwith "OGPU virtual implementations missing";
  if List.exists (fun stanza -> field "default_implementation" stanza <> None)
      (parse (read "lib/ogpu/dune")) then
    failwith "OGPU implementation must be selected by each executable, not by the virtual library";
  let roots = List.filter Sys.file_exists ["lib"; "ppx"; "tools"; "test"; "bin"] in
  let products = List.map fst (graph ["lib"; "ppx"]) in
  let graph = graph roots in
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
     "rays_math", "rays"; "renderer", "runtime_input";
     "rays", "sdl3_ttf";
     "editor_document", "pxui"; "editor_document", "pxui_shell";
     "editor_document", "pxui_graph";
     "editor_document", "rays_editor";
     "flow", "pxui"; "flow", "sop";
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
     "flow_native", "pxui"; "flow_native", "pxui_graph"; "flow_native", "rays_editor";
     "web_export", "pxui"; "web_export", "rays_editor"; "web_export", "unix";
     "lisp_sop", "editor_document"; "lisp_sop", "rays"; "lisp_sop", "unix";
     "flow_web", "rays"; "flow_web", "flow_gpu"; "flow_web", "sop"; "flow_web", "rdk";
     "flow_web", "pxui"; "flow_web", "rays_editor"; "flow_web", "ogpu"; "flow_web", "unix";
     "scene_command", "flow_web";
     "sop_catalog", "rays"; "sop_catalog", "runtime"; "sop_catalog", "metal";
     "rays_pathtracer", "ogpu_metal"; "rays_pathtracer", "sop"; "rays_pathtracer", "pxui";
     "rays_pathtracer", "rdk"; "rays_pathtracer", "rdk_core"; "rays_pathtracer", "rdk_rays";
     "rdk_rays", "sop"; "rdk_rays", "metal"; "rdk_rays", "pxui";
     "scene_execution_fixtures", "metal"; "scene_execution_fixtures", "runtime";
     "scene_execution_fixtures", "rays";
     (* "depends only on": an edge the reach rules would let through *)
     "pxui_shell", "editor_document"; "pxui_shell", "scene_command"; "ogpu_core", "lru";
     "scene_command", "flow_ir";
     "ogpu", "native_layer_token"; "ogpu_mock", "lru"; "param", "unix"; "flow", "unix";
     "ogpu_metal_native", "native_layer_token"; "ogpu_metal", "lru"; "metal", "lru";
     "sop_catalog", "flow"; "rays_diagnostic", "unix";
     "flow", "unix"; "sop", "flow"; "flow_sop", "scene_command"];
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
  let comment_and_string = violations graph
    ~scan:["lib/rays/ok.ml", "(* Metal.foo *) let s = \"Metal.framework\""] in
  if comment_and_string <> [] then
    failwith ("gate flagged Metal inside a comment or string: " ^ String.concat "; " comment_and_string);
  if violations graph ~scan:["tools/codemod/metal_registry.ml", "let x = Metal.Device.system_default";
                              "lib/ogpu/conformance/test_metal.ml", "let x = Metal.pp_error"] <> [] then
    failwith "gate flagged a listed Metal tooling file";
  if violations graph ~scan:["lib/pxui_graph/injected.ml", "Input.Key_pressed key"] = [] then
    failwith "gate accepted adapter key handling";
  if violations graph ~scan:["lib/pxui_graph/ok.ml", "(* Key_pressed *) let s = \"Key_pressed\""] <> [] then
    failwith "gate flagged Key_pressed inside a comment or string";
  if violations graph ~scan:["lib/sdl3/injected.c", "SDL_CreateRenderer(window, 0)"] = []
     || violations graph ~scan:["lib/rays/injected.ml", "Sys.getenv \"RAYS_BACKEND\""] = [] then
    failwith "gate accepted an injected alternate renderer or backend selector";
  if violations graph ~scan:["lib/rays/injected.ml", "let d = Sys.getenv_opt \"RAYS_CRASH_DIR\""] = []
     || violations graph ~scan:["lib/rays/test_ok.ml", "let d = Sys.getenv_opt \"RAYS_SLOW\""] <> [] then
    failwith "gate env check: library RAYS_ read accepted or test read rejected";
  let scan = List.concat_map files roots
    |> List.filter (fun p -> not (Filename.check_suffix p ".pp.ml"))
    |> List.filter (fun p -> List.exists (Filename.check_suffix p)
      [".ml"; ".mli"; ".c"; ".h"; ".m"])
    |> List.map (fun p -> p, read p) in
  let dune_files = List.concat_map files roots
    |> List.filter (fun path -> Filename.basename path = "dune") in
  let test_errors = lane_aliases dune_files @ backend_selections graph dune_files
    @ List.concat_map (fun path -> library_copies path (parse (read path))) dune_files
    @ test_support_dependencies graph products in
  (* The one native path stays wired: SDL Metal view -> OGPU driver. *)
  List.iter (fun (path, anchor) -> if not (contains (read path) anchor) then
    failwith (path ^ " lost native anchor " ^ anchor))
    [ "lib/sdl3/sdl3_stubs.c", "SDL_Metal_CreateView";
      "lib/renderer/renderer.ml", "Ogpu.Impl.create_driver";
      "lib/renderer/renderer.ml", "Runtime.create" ];
  match violations graph ~scan @ test_errors @ private_errors scan with
  | [] -> Printf.printf "dependency gate: %d libraries, %d rules, %d whitelists\n"
            (List.length graph) (List.length rules) (List.length only)
  | errors -> List.iter prerr_endline errors; exit 1
