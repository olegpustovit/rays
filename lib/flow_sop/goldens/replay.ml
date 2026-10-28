open Rdk
open Flow.Syntax

let fail message = raise (Failure message)
let get_flow = function Ok value -> value | Error error -> fail (Diagnostic.to_string error)
let get_string = function Ok value -> value | Error error -> fail error
let string = function { Flow.Syntax.node = Str value; _ } -> value | _ -> fail "expected string"

let children tag = function
  | { Flow.Syntax.node = List ({ node = Sym found; _ } :: values); _ } when found = tag -> values
  | _ -> fail ("expected " ^ tag)

let path directory path =
  if
    Filename.is_relative path
    && Filename.dirname path = "inputs"
    && Filename.basename path <> ".."
    && not (String.contains path '\\')
  then Filename.concat directory path
  else fail ("invalid corpus path: " ^ path)

(* Corpus files may be stored as FILE.gz; a fixture name's digest is of the decompressed text. *)
let resolve path = if Sys.file_exists path then path else path ^ ".gz"

let read_file path =
  let path = resolve path in
  if Filename.check_suffix path ".gz" then begin
    let channel = Unix.open_process_args_in "gzip" [| "gzip"; "-dc"; path |] in
    let contents = In_channel.input_all channel in
    if Unix.close_process_in channel <> Unix.WEXITED 0 then fail ("gzip failed: " ^ path);
    contents
  end
  else In_channel.with_open_bin path In_channel.input_all

let input_location directory kind relative =
  let filename = Filename.basename relative in
  let prefix = kind ^ "-" and suffix = ".sexp" in
  if
    Filename.dirname relative <> "inputs"
    || (not (String.starts_with ~prefix filename))
    || not (String.ends_with ~suffix filename)
  then fail ("invalid " ^ kind ^ " fixture path: " ^ relative);
  let digest_length = String.length filename - String.length prefix - String.length suffix in
  if digest_length <> 32 then fail ("invalid " ^ kind ^ " fixture digest: " ^ relative);
  let expected = String.sub filename (String.length prefix) digest_length in
  (expected, path directory relative)

let read_input directory kind relative =
  let expected, input_path = input_location directory kind relative in
  if not (Sys.file_exists (resolve input_path)) then
    fail ("missing " ^ kind ^ " fixture: " ^ relative);
  let contents = read_file input_path in
  if Digest.to_hex (Digest.string contents) <> expected then
    fail ("tampered " ^ kind ^ " fixture: " ^ relative);
  contents

module Input_cache = Lru.Make (String)

let input_payload cache directory kind relative =
  let _, filename = input_location directory kind relative in
  let stamp () =
    let info =
      try Unix.stat (resolve filename)
      with Unix.Unix_error (Unix.ENOENT, _, _) ->
        fail ("missing " ^ kind ^ " fixture: " ^ relative)
    in
    (info.Unix.st_dev, info.st_ino, info.st_size, info.st_mtime, info.st_ctime)
  in
  let before = stamp () in
  match Input_cache.find_opt cache filename with
  | Some (saved, payload) when saved = before -> payload
  | _ ->
      let contents = read_input directory kind relative in
      let payload =
        match kind with
        | "geometry" -> Sop.Payload.Geometry (Fixture_codec.decode contents)
        | "image" -> Sop.Payload.Image (Fixture_codec.decode_image contents)
        | _ -> fail ("unknown fixture kind: " ^ kind)
      in
      if stamp () <> before then fail ("fixture changed while reading: " ^ relative);
      Input_cache.add cache filename (before, payload);
      payload

let marker ~name payload =
  Sop.Node.Build.make ~operation:"lisp_sop_ext" ~version:1 ~parameters:name
    ~cook_mode:Sop.Node.Generator ~dependencies:Sop.Context.Dependencies.static ~inputs:[||]
    (fun ~node_id:_ _ _ -> Ok Sop.Node.Build.{ payload; instances = None })

let external_input name node =
  let image_name =
    if String.starts_with ~prefix:"image/" name then
      Some (String.sub name 6 (String.length name - 6))
    else None
  in
  let key =
    match image_name with Some image -> "lisp_sop_image_ext_" ^ image | None -> "ext_" ^ name
  in
  let factory =
    Sop.Edit_graph.factory ~key ~label:("Ext " ^ name) ~category:[ "Test" ] ~arity:0 (fun _ ->
        Ok node)
  in
  ( factory,
    Option.map
      (fun image ->
        let prototype = Option.get (Flow.Op.find "image/noise" Flow.Context.value) in
        {
          prototype with
          Flow.Op.name = "image/ext_" ^ image;
          signature = { pos = []; opt = []; rest = None; kw = [] };
          check = (fun _ -> ());
          packed_extension = None;
          body = (fun ~live:_ ~node _ -> node ("sop/" ^ key) []);
        })
      image_name )

let native_factories key =
  List.filter (fun factory -> Sop.Edit_graph.factory_key factory <> key) Sop.Nodes.factories

let inputs ~key cache directory forms =
  let geometry = ref 0 and image = ref 0 in
  let values, factories, ops =
    List.fold_left
      (fun (values, factories, ops) form ->
        match form.Flow.Syntax.node with
        | List [ { node = Sym "geometry"; _ }; filename ] -> (
            let name = Printf.sprintf "port_in%d" !geometry in
            incr geometry;
            let node = marker ~name (input_payload cache directory "geometry" (string filename)) in
            let factory, op = external_input name node in
            ( (name, node) :: values,
              factory :: factories,
              match op with None -> ops | Some op -> op :: ops ))
        | List [ { node = Sym "image"; _ }; filename ] -> (
            let name = Printf.sprintf "image/port_img%d" !image in
            incr image;
            let node = marker ~name (input_payload cache directory "image" (string filename)) in
            let factory, op = external_input name node in
            ( (name, node) :: values,
              factory :: factories,
              match op with None -> ops | Some op -> op :: ops ))
        | _ -> fail "malformed golden input row")
      ([], [], []) forms
  in
  let inputs = List.rev factories in
  (List.rev values, inputs @ native_factories key, List.rev ops)

module Prepared_cache = Lru.Make (String)
module Lowered_cache = Lru.Make (String)

type replay_cache = {
  inputs : ((int * int * int * float * float) * Sop.Payload.t) Input_cache.t;
  prepared : prepared Prepared_cache.t;
  lowered : Sop.Node.t Lowered_cache.t;
}

and prepared = {
  base : int;
  source_forms : Flow.Syntax.t list;
  schema : string;
  ops : Flow.Op.t list;
  graph_catalog : Flow.Catalog.catalog;
  library : Flow.Workspace.t;
  diagnostics : Diagnostic.t list;
}

let create_cache () =
  {
    inputs = Input_cache.create 256;
    prepared = Prepared_cache.create 4;
    lowered = Lowered_cache.create 64;
  }

let stable_error message =
  match String.index_opt message '[' with
  | Some bracket -> (
      match String.index_from_opt message (bracket + 1) ']' with
      | Some close ->
          ( String.sub message (bracket + 1) (close - bracket - 1),
            String.trim (String.sub message (close + 1) (String.length message - close - 1)) )
      | None -> ("E_LISP", message))
  | None -> (
      let code_end = ref 0 in
      while
        !code_end < String.length message
        && match message.[!code_end] with 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false
      do
        incr code_end
      done;
      if !code_end >= 3 && String.starts_with ~prefix:"E_" message then begin
        let rest = String.trim (String.sub message !code_end (String.length message - !code_end)) in
        if String.starts_with ~prefix:"at " rest then
          let second =
            Option.bind (String.index_opt rest ':') (fun first ->
                String.index_from_opt rest (first + 1) ':')
          in
          match second with
          | Some colon ->
              ( String.sub message 0 !code_end,
                String.trim (String.sub rest (colon + 1) (String.length rest - colon - 1)) )
          | None -> (String.sub message 0 !code_end, rest)
        else
          ( String.sub message 0 !code_end,
            if String.starts_with ~prefix:":" rest then
              String.trim (String.sub rest 1 (String.length rest - 1))
            else rest )
      end
      else
        match String.index_opt message ':' with
        | Some colon ->
            ( String.sub message 0 colon,
              String.trim (String.sub message (colon + 1) (String.length message - colon - 1)) )
        | None -> ("E_LISP", message))

let source_workspace_and_call source =
  match Flow.Syntax.parse ("(workspace golden (graph g :context sop " ^ source ^ "))") with
  | Ok
      [
        ({
           Flow.Syntax.node =
             Flow.Syntax.List
               [ _; _; { Flow.Syntax.node = Flow.Syntax.List [ _; _; _; _; call ]; _ } ];
           _;
         } as form);
      ]
    when match call.Flow.Syntax.node with Flow.Syntax.List (_ :: _) -> true | _ -> false ->
      ([ form ], call)
  | Ok _ -> fail "source workspace must contain one SOP call"
  | Error diagnostic -> fail (Diagnostic.to_string diagnostic)

let source_workspace source = fst (source_workspace_and_call source)

let rec next_id (form : Flow.Syntax.t) =
  List.fold_left
    (fun highest child -> max highest (next_id child))
    (form.id + 1) (Flow.Syntax.children form)

let workspace_with_children name children =
  match Flow.Syntax.parse ("(workspace " ^ name ^ ")") with
  | Ok [ ({ Flow.Syntax.node = Flow.Syntax.List (head :: workspace_name :: _); _ } as form) ] ->
      [ { form with Flow.Syntax.node = Flow.Syntax.List (head :: workspace_name :: children) } ]
  | Ok _ -> fail "workspace constructor returned an unexpected form"
  | Error diagnostic -> fail (Diagnostic.to_string diagnostic)

let literal_node_call key forms =
  match forms with
  | [ ({ Flow.Syntax.node = Flow.Syntax.List ({ node = Flow.Syntax.Sym _; _ } :: _); _ } as form) ]
    -> (
      let rec external_calls x =
        match x.Flow.Syntax.node with
        | Flow.Syntax.List ({ node = Flow.Syntax.Sym head; _ } :: args) ->
            if head = "sop/" ^ key then false
            else if
              String.starts_with ~prefix:"sop/ext_" head
              || String.starts_with ~prefix:"image/ext_" head
            then args = []
            else false
        | _ -> List.for_all external_calls (Flow.Syntax.children x)
      in
      match form.node with
      | Flow.Syntax.List ({ node = Flow.Syntax.Sym head; _ } :: args) ->
          head = "sop/" ^ key && List.for_all external_calls args
      | _ -> false)
  | _ -> false

let fingerprint value = Digest.to_hex (Digest.string (Marshal.to_string value []))

let merge_packed first second =
  let roots = Hashtbl.create (List.length first + List.length second) in
  let add (path, value) =
    let value =
      match (Hashtbl.find_opt roots path, value) with
      | Some (Flow.Workspace.Candidate old), Flow.Workspace.Candidate next ->
          Flow.Workspace.Candidate (Flow.Workspace.Paths.union old next)
      | Some (Flow.Workspace.Candidate _ as old), Flow.Workspace.Refused _ -> old
      | _, Flow.Workspace.Candidate _ -> value
      | Some (Flow.Workspace.Refused old), Flow.Workspace.Refused next ->
          Flow.Workspace.Refused (List.sort_uniq compare (old @ next))
      | None, _ -> value
    in
    Hashtbl.replace roots path value
  in
  List.iter add first;
  List.iter add second;
  Hashtbl.fold (fun path value entries -> (path, value) :: entries) roots [] |> List.sort compare

let merge_kind_fns first second =
  List.fold_left
    (fun all ((name, _) as item) -> if List.mem_assoc name all then all else all @ [ item ])
    first second

let merge_workspace library graph =
  let module W = Flow.Workspace in
  let packed = merge_packed library.W.packed graph.W.packed in
  {
    graph with
    W.defs = library.W.defs;
    nodes = library.W.nodes;
    macros = library.W.macros;
    live = W.Paths.union library.W.live graph.W.live;
    invariant = W.Paths.union library.W.invariant graph.W.invariant;
    approx = W.Paths.union library.W.approx graph.W.approx;
    packed;
    packed_roots =
      List.sort
        (fun (a, p) (b, q) ->
          let n = compare p q in
          if n = 0 then compare a.Flow.Syntax.id b.Flow.Syntax.id else n)
        (library.W.packed_roots @ graph.W.packed_roots);
    approx_reasons = List.sort_uniq compare (library.W.approx_reasons @ graph.W.approx_reasons);
    kind_fns = merge_kind_fns library.W.kind_fns graph.W.kind_fns;
  }

let prepare_library cache key schema base sources factories ops =
  match Prepared_cache.find_opt cache.prepared key with
  | Some prepared
    when prepared.base = base && prepared.schema = schema && prepared.ops == ops
         && prepared.source_forms = sources ->
      prepared
  | _ ->
      let nodes = Flow_sop.Catalog.Private.definitions_of_sources sources |> get_flow in
      let base_catalog =
        Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories
        |> get_flow
      in
      let graph_catalog =
        Flow_sop.Catalog.of_factories ~nodes ~version:Flow_sop.Manifest.version factories
        |> get_flow
      in
      (* the library forms' fixed ids, as Catalog.check_workspace splices them *)
      let bases =
        Flow_sop.Catalog.Private.library_bases (Flow_sop.Catalog.sources () |> get_flow)
      in
      let renamed =
        List.map (fun source -> fst (Flow.Syntax.renumber (List.assq source bases) source)) sources
      in
      let library_forms = workspace_with_children "golden_library" renamed in
      let library, diagnostics =
        Flow.Workspace.check ~ops ~library:true base_catalog library_forms
      in
      let library =
        match library with
        | Some library
          when not
                 (List.exists
                    (fun d -> d.Diagnostic.severity = Diagnostic.Error_severity)
                    diagnostics) ->
            library
        | _ -> fail (String.concat "\n" (List.map Diagnostic.to_string diagnostics))
      in
      let prepared =
        { base; source_forms = sources; schema; ops; graph_catalog; library; diagnostics }
      in
      Prepared_cache.add cache.prepared key prepared;
      prepared

let prepared_check ?parsed_source cache source factories ops key forms =
  let parsed_source =
    match parsed_source with
    | Some form -> form
    | None -> (
        match Flow.Syntax.parse source with
        | Ok [ form ] -> form
        | Ok _ -> fail "source must be one Lisp call"
        | Error diagnostic -> fail (Diagnostic.to_string diagnostic))
  in
  if ops != Flow_sop.Operators.all || not (literal_node_call key [ parsed_source ]) then None
  else
    let sources =
      Flow_sop.Catalog.sources () |> get_flow |> fun sources ->
      Flow_sop.Catalog.Private.reachable_sources sources forms
    in
    if sources = [] then None
    else
      let base = List.fold_left (fun highest form -> max highest (next_id form)) 0 forms in
      let schema = fingerprint (List.map Flow_sop.Catalog.descriptor factories) in
      let cache_key = Printf.sprintf "%s:%d:%s" key base schema in
      let prepared = prepare_library cache cache_key schema base sources factories ops in
      let graph, diagnostics = Flow.Workspace.check ~ops prepared.graph_catalog forms in
      let graph =
        match graph with
        | Some graph
          when not
                 (List.exists
                    (fun d -> d.Diagnostic.severity = Diagnostic.Error_severity)
                    diagnostics) ->
            graph
        | _ -> fail (String.concat "\n" (List.map Diagnostic.to_string diagnostics))
      in
      let checked = merge_workspace prepared.library graph in
      match Flow_sop.Encoded_parameters.checked checked with
      | Ok checked -> Some (checked, prepared.diagnostics @ diagnostics)
      | Error diagnostic -> fail (Diagnostic.to_string diagnostic)

let ordinary_checked factories ops forms =
  let catalog =
    Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |> get_flow
  in
  let checked, diagnostics =
    Flow_sop.Catalog.check_workspace
      ~sources:(Flow_sop.Catalog.sources () |> get_flow)
      ~ops catalog forms
  in
  match checked with
  | Some checked
    when not (List.exists (fun d -> d.Diagnostic.severity = Diagnostic.Error_severity) diagnostics)
    ->
      (checked, diagnostics)
  | _ -> fail (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let compare_error_diagnostics cache source factories ops key forms =
  let catalog =
    Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories |> get_flow
  in
  let sources = Flow_sop.Catalog.sources () |> get_flow in
  let ordinary, ordinary_diagnostics =
    Flow_sop.Catalog.check_workspace ~sources ~ops catalog forms
  in
  let reachable = Flow_sop.Catalog.Private.reachable_sources sources forms in
  let base = List.fold_left (fun highest form -> max highest (next_id form)) 0 forms in
  let schema = fingerprint (List.map Flow_sop.Catalog.descriptor factories) in
  let cache_key = Printf.sprintf "%s:%d:%s" key base schema in
  let prepared = prepare_library cache cache_key schema base reachable factories ops in
  let graph, graph_diagnostics = Flow.Workspace.check ~ops prepared.graph_catalog forms in
  let prepared_diagnostics = prepared.diagnostics @ graph_diagnostics in
  if ordinary <> None || graph <> None || ordinary_diagnostics <> prepared_diagnostics then
    fail ("prepared diagnostics differ for " ^ key ^ " (" ^ source ^ ")")

type replay_times = {
  mutable rows : int;
  mutable inputs : float;
  mutable check : float;
  mutable lower : float;
  mutable compile : float;
  mutable cook : float;
}

let create_replay_times () =
  { rows = 0; inputs = 0.; check = 0.; lower = 0.; compile = 0.; cook = 0. }

let timed (times : replay_times option) add action =
  let started = Unix.gettimeofday () in
  Fun.protect
    ~finally:(fun () ->
      Option.iter (fun times -> add times (Unix.gettimeofday () -. started)) times)
    action

let node_key (form : Flow.Syntax.t) =
  match form.Flow.Syntax.node with
  | Flow.Syntax.List ({ node = Flow.Syntax.Sym name; _ } :: _) -> (
      match String.index_opt name '/' with
      | Some slash -> String.sub name (slash + 1) (String.length name - slash - 1)
      | None -> name)
  | _ -> fail "source must be one node call"

let lower ?(reference = false) ?cache ?times ?parsed source factories ops with_ =
  (* The lowered graph holds only named input markers (swapped for [with_] below), so rows that
     share an expression share one lowering whatever their fixture. *)
  let build () =
    let checked =
      timed times
        (fun times elapsed -> times.check <- times.check +. elapsed)
        (fun () ->
          let forms, parsed_source =
            match parsed with Some parsed -> parsed | None -> source_workspace_and_call source
          in
          let checked =
            match cache with
            | Some cache ->
                prepared_check ~parsed_source cache source factories ops (node_key parsed_source)
                  forms
            | None -> None
          in
          let checked =
            match checked with
            | Some (checked, diagnostics) ->
                if
                  List.exists
                    (fun d -> d.Diagnostic.severity = Diagnostic.Error_severity)
                    diagnostics
                then fail (String.concat "\n" (List.map Diagnostic.to_string diagnostics));
                checked
            | None -> fst (ordinary_checked factories ops forms)
          in
          checked)
    in
    let lowered =
      timed times
        (fun times elapsed -> times.lower <- times.lower +. elapsed)
        (fun () ->
          Flow_sop.Lower.of_checked ~provenance_tags:false ~reference ~factories checked |> get_flow)
    in
    let node =
      timed times
        (fun times elapsed -> times.compile <- times.compile +. elapsed)
        (fun () ->
          let graph =
            match
              List.find_opt (fun (graph : Flow_sop.Lower.graph) -> graph.default) lowered.graphs
            with
            | Some graph -> graph
            | None -> fail "workspace has no default SOP graph"
          in
          let root =
            match graph.root with Some root -> root | None -> fail "SOP graph has no result"
          in
          Sop.Edit_graph.compile_node graph.network.geometry ~node_id:root |> get_string)
    in
    node
  in
  let node =
    match cache with
    | None -> build ()
    | Some cache -> (
        let key = string_of_bool reference ^ source in
        match Lowered_cache.find_opt cache.lowered key with
        | Some node -> node
        | None ->
            let node = build () in
            Lowered_cache.add cache.lowered key node;
            node)
  in
  (* the graph is a DAG (an inlined body reads its input many times): each node is rebuilt once *)
  let restored = Hashtbl.create 64 in
  let rec restore node =
    match Hashtbl.find_opt restored (Sop.Node.id node) with
    | Some node -> node
    | None ->
        let result =
          if Sop.Node.operation node = "lisp_sop_ext" then
            List.assoc (Sop.Node.parameters node) with_
          else
            match Sop.Node.inputs node with
            | [] -> node
            | children ->
                let rebuilt = List.map restore children in
                if List.for_all2 ( == ) children rebuilt then node
                else Sop.Node.Build.rebuild_with_inputs node (Array.of_list rebuilt)
        in
        Hashtbl.add restored (Sop.Node.id node) result;
        result
  in
  restore node

let output node =
  let session = Sop.Session.create ~max_entries:8 ~max_payload_bytes:512_000_000 |> get_string in
  Fun.protect
    ~finally:(fun () -> Sop.Session.close session)
    (fun () ->
      let context = Sop.Context.create ~domains:1 ~grain:97 () |> get_string in
      match Sop.Session.cook session ~context node with
      | Error error -> `Error (error.Diagnostic.code, error.message)
      | Ok output -> (
          match Sop.Payload.geometry output.payload with
          | Error error -> `Error (error.Diagnostic.code, error.message)
          | Ok geometry ->
              let geometry =
                match output.instances with
                | None -> geometry
                | Some transforms ->
                    Rdk.Instance_copy.materialize_instances ~transforms geometry
                    |> Result.map_error (fun error -> Rdk.Error.to_string error)
                    |> get_string
              in
              `Ok (Digest.to_hex (Digest.string (Rdk_test_support.geometry_bytes geometry)))))

let same_workspace (a : Flow.Workspace.t) (b : Flow.Workspace.t) =
  a.name = b.name && a.graphs = b.graphs && a.defs = b.defs && a.nodes = b.nodes
  && a.macros = b.macros && a.source = b.source && a.live = b.live && a.invariant = b.invariant
  && a.approx = b.approx && a.packed = b.packed && a.packed_roots = b.packed_roots
  && a.approx_reasons = b.approx_reasons
  && a.kind_fns = b.kind_fns

let compare_check cache source factories with_ =
  let ops = Flow_sop.Operators.all in
  let forms = source_workspace source in
  let key =
    match Flow.Syntax.parse source with
    | Ok [ { Flow.Syntax.node = Flow.Syntax.List ({ node = Flow.Syntax.Sym name; _ } :: _); _ } ]
      -> (
        match String.index_opt name '/' with
        | Some slash -> String.sub name (slash + 1) (String.length name - slash - 1)
        | None -> name)
    | _ -> fail "invalid differential source"
  in
  let attempt check = try `Ok (check ()) with Failure message -> `Error message in
  let ordinary = attempt (fun () -> ordinary_checked factories ops forms) in
  let prepared =
    attempt (fun () ->
        match prepared_check cache source factories ops key forms with
        | Some checked -> checked
        | None -> fail ("prepared checker declined supported source " ^ key))
  in
  match (ordinary, prepared) with
  | `Error expected, `Error actual when expected = actual ->
      compare_error_diagnostics cache source factories ops key forms
  | `Error expected, `Error actual ->
      fail
        ("prepared error differs for " ^ key ^ " (ordinary " ^ expected ^ "; prepared " ^ actual
       ^ ")")
  | `Error expected, `Ok _ ->
      fail ("prepared checker accepted " ^ key ^ " (ordinary " ^ expected ^ ")")
  | `Ok _, `Error actual -> fail ("prepared checker refused " ^ key ^ " (" ^ actual ^ ")")
  | `Ok (ordinary, ordinary_diagnostics), `Ok (prepared, prepared_diagnostics) ->
      if not (same_workspace ordinary prepared) then fail ("prepared workspace differs for " ^ key);
      if ordinary_diagnostics <> prepared_diagnostics then
        fail ("prepared diagnostics differ for " ^ key);
      let normal = output (lower source factories ops with_)
      and cached = output (lower ~cache source factories ops with_) in
      if normal <> cached then fail ("prepared cook differs for " ^ key)

let check_row ?capture ?(prepared = true) ?(times : replay_times option) (cache : replay_cache)
    directory row =
  Fun.protect ~finally:Flow_sop.Lower.Private.clear_cache (fun () ->
      match children "golden" row with
      | [ name; fixture; case; source; input_forms; expected ] ->
          let name = string name and fixture = string fixture and case = string case in
          let label = Printf.sprintf "%s/%s/%s" name fixture case in
          let parsed =
            try source_workspace_and_call (string source)
            with Failure message -> fail (label ^ ": malformed Lisp source: " ^ message)
          in
          let with_, factories, ops =
            timed times
              (fun times elapsed -> times.inputs <- times.inputs +. elapsed)
              (fun () -> inputs ~key:name cache.inputs directory (children "inputs" input_forms))
          in
          Option.iter (fun times -> times.rows <- times.rows + 1) times;
          let actual =
            try
              let node =
                if prepared then
                  lower ~cache ~parsed ?times (string source) factories
                    (ops @ Flow_sop.Operators.all) with_
                else
                  lower ~parsed ?times (string source) factories (ops @ Flow_sop.Operators.all)
                    with_
              in
              timed times
                (fun times elapsed -> times.cook <- times.cook +. elapsed)
                (fun () -> output node)
              |> function
              | `Ok digest -> `Ok digest
              | `Error (code, message) -> `Error (code, message)
            with Failure message ->
              let code, message = stable_error message in
              `Error (code, message)
          in
          let matches =
            match (expected.Flow.Syntax.node, actual) with
            | List [ { node = Sym "ok"; _ }; digest ], `Ok actual ->
                let expected = string digest in
                String.length expected = 32
                && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) expected
                && expected = actual
            | List [ { node = Sym "error"; _ }; code; message ], `Error (actual_code, actual_message)
              ->
                string code <> "" && string code = actual_code && string message = actual_message
            | _ -> false
          in
          if (not matches) && capture <> None then
            Option.iter (fun capture -> capture actual) capture
          else if not matches then
            let describe = function
              | `Ok digest -> "ok " ^ digest
              | `Error (code, message) -> "error " ^ code ^ " " ^ message
            in
            let expected =
              match expected.Flow.Syntax.node with
              | List [ { node = Sym "ok"; _ }; digest ] -> "ok " ^ string digest
              | List [ { node = Sym "error"; _ }; code; message ] ->
                  "error " ^ string code ^ " " ^ string message
              | _ -> "malformed outcome"
            in
            let reference =
              try
                let node =
                  if prepared then
                    lower ~reference:true ~cache (string source) factories
                      (ops @ Flow_sop.Operators.all) with_
                  else
                    lower ~reference:true (string source) factories (ops @ Flow_sop.Operators.all)
                      with_
                in
                describe (output node)
              with Failure message -> "lower error " ^ message
            in
            fail
              (label ^ ": Lisp golden changed (expected " ^ expected ^ ", got " ^ describe actual
             ^ "; reference " ^ reference ^ ")")
      | _ -> fail "malformed golden row")

let key_of tag row =
  match (tag, children tag row) with
  | "case", [ node; fixture; case ] | "golden", node :: fixture :: case :: _ ->
      String.concat "\000" [ string node; string fixture; string case ]
  | _ -> fail ("malformed " ^ tag ^ " coverage row")

let unique_rows tag rows =
  let seen = Hashtbl.create (List.length rows) in
  List.iter
    (fun row ->
      let key = key_of tag row in
      if Hashtbl.mem seen key then
        fail ("duplicate " ^ tag ^ " row: " ^ String.concat "/" (String.split_on_char '\000' key));
      Hashtbl.add seen key ())
    rows;
  seen

let check_coverage coverage rows =
  let expected = unique_rows "case" coverage and actual = unique_rows "golden" rows in
  if
    Hashtbl.length expected <> Hashtbl.length actual
    || not (Hashtbl.fold (fun key () equal -> equal && Hashtbl.mem actual key) expected true)
  then fail "golden rows do not match the full captured registry coverage"

let rows_for_key tag key rows =
  List.filter
    (fun row ->
      match children tag row with
      | node :: _ -> string node = key
      | _ -> fail ("malformed " ^ tag ^ " row"))
    rows

let current_node_sources () =
  let executable_dir = Filename.dirname Sys.executable_name in
  let candidates =
    [
      "lib/flow_sop/nodes";
      Filename.concat executable_dir "../nodes";
      Filename.concat executable_dir "../../../../../lib/flow_sop/nodes";
    ]
  in
  let directory =
    match
      List.find_opt
        (fun directory -> Sys.file_exists (Filename.concat directory "prelude.rays"))
        candidates
    with
    | Some directory -> directory
    | None -> fail "cannot locate current flow_sop/nodes source directory"
  in
  Sys.readdir directory |> Array.to_list
  |> List.filter (fun file -> Filename.check_suffix file ".rays")
  |> List.sort String.compare
  |> List.map (fun file -> (file, Digest.file (Filename.concat directory file) |> Digest.to_hex))

let validate_provenance status executable recorded current =
  if not (List.mem status [ "running"; "complete" ]) then
    fail ("unusable golden provenance status: " ^ status);
  let digest value =
    String.length value = 32
    && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) value
  in
  if not (digest executable) then fail "malformed capture executable digest";
  if
    List.exists
      (fun (file, hash) ->
        Filename.basename file <> file
        || (not (String.ends_with ~suffix:".rays" file))
        || not (digest hash))
      recorded
  then fail "malformed source provenance row";
  if recorded <> current then
    fail "golden Lisp source hashes differ from the current node source set";
  status

let verify_provenance directory =
  let rows =
    match Flow.Syntax.parse (read_file (Filename.concat directory "provenance.sexp")) with
    | Ok rows -> rows
    | Error diagnostic -> fail ("malformed golden provenance: " ^ Diagnostic.to_string diagnostic)
  in
  let root =
    match rows with [ root ] -> root | _ -> fail "golden provenance must have one capture record"
  in
  let fields = children "capture" root in
  let field tag =
    match List.find_opt (fun form -> Flow.Syntax.head form = Some tag) fields with
    | Some form -> children tag form
    | None -> fail ("golden provenance is missing " ^ tag)
  in
  let status =
    match field "status" with
    | [ status ] -> string status
    | _ -> fail "malformed provenance status"
  in
  let executable =
    match field "executable" with
    | [ digest ] -> string digest
    | _ -> fail "malformed capture executable digest"
  in
  let recorded =
    field "lisp_sources"
    |> List.map (fun source ->
        match children "source" source with
        | [ file; digest ] -> (string file, string digest)
        | _ -> fail "malformed source provenance row")
    |> List.sort compare
  in
  let current = current_node_sources () in
  validate_provenance status executable recorded current

let check_key cache directory key rows coverage =
  let key_rows = rows_for_key "golden" key rows
  and key_coverage = rows_for_key "case" key coverage in
  if key_rows = [] || key_coverage = [] then fail ("no complete golden key in corpus: " ^ key);
  check_coverage key_coverage key_rows;
  List.iter (check_row cache directory) key_rows;
  Printf.printf "checked key %s: %d Lisp golden rows\n%!" key (List.length key_rows)

let check_fixture ?(prepared = true) ?(profile = false) (cache : replay_cache) directory key fixture
    rows coverage =
  let belongs tag row =
    match (tag, children tag row) with
    | "golden", node :: source_fixture :: _ | "case", [ node; source_fixture; _ ] ->
        string node = key && string source_fixture = fixture
    | _ -> fail ("malformed " ^ tag ^ " coverage row")
  in
  let fixture_rows = List.filter (belongs "golden") rows
  and fixture_coverage = List.filter (belongs "case") coverage in
  if fixture_rows = [] || fixture_coverage = [] then
    fail (Printf.sprintf "no captured fixture in corpus: %s/%s" key fixture);
  check_coverage fixture_coverage fixture_rows;
  let times = if profile then Some (create_replay_times ()) else None in
  List.iter (check_row ~prepared ?times cache directory) fixture_rows;
  Printf.printf "checked fixture %s/%s: %d Lisp golden rows (partial key, %s checking)\n%!" key
    fixture (List.length fixture_rows)
    (if prepared then "prepared" else "ordinary");
  Option.iter
    (fun times ->
      Printf.printf
        "profile %d rows: inputs %.3f s, source/check %.3f s, Lower.of_checked %.3f s, graph \
         compile %.3f s, cook/geometry-encode %.3f s\n\
         %!"
        times.rows times.inputs times.check times.lower times.compile times.cook)
    times

let read_corpus directory =
  let read name =
    match Flow.Syntax.parse (read_file (Filename.concat directory name)) with
    | Ok rows -> rows
    | Error diagnostic -> fail (Diagnostic.to_string diagnostic)
  in
  let rows = read "ports.sexp" and coverage = read "coverage.sexp" in
  if rows = [] then fail "golden corpus is empty";
  if coverage = [] then fail "golden coverage is empty";
  (rows, coverage)

let syntax node = Flow.Syntax.make node

let sexp_tag tag values =
  Flow.Lisp.flat (syntax (Flow.Syntax.List (syntax (Flow.Syntax.Sym tag) :: values)))

let sexp_string value = syntax (Flow.Syntax.Str value)

let write_gz path text =
  let temporary = path ^ ".tmp" in
  Out_channel.with_open_bin temporary (fun channel -> output_string channel text);
  let out = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
  let pid =
    Unix.create_process "gzip" [| "gzip"; "-9"; "-n"; "-c"; temporary |] Unix.stdin out Unix.stderr
  in
  let status = snd (Unix.waitpid [] pid) in
  Unix.close out;
  Sys.remove temporary;
  if status <> Unix.WEXITED 0 then fail ("gzip failed: " ^ path)

(* Rewrites the outcome of every row of [nodes] ([] = all) in DIR/ports.sexp.gz from what the Lisp
   nodes cook now: the promote step after an intentional node change. One row per line; the outcome
   is the last (ok ...) / (error ...) form. *)
let update directory nodes =
  let cache = create_cache () and changed = ref 0 in
  let rfind sub line =
    let rec go i =
      if i < 0 then -1 else if String.sub line i (String.length sub) = sub then i else go (i - 1)
    in
    go (String.length line - String.length sub)
  in
  let lines = String.split_on_char '\n' (read_file (Filename.concat directory "ports.sexp")) in
  let lines =
    List.map
      (fun line ->
        if line = "" then line
        else
          let row =
            match Flow.Syntax.parse line with Ok [ row ] -> row | _ -> fail "malformed golden row"
          in
          match children "golden" row with
          | node :: _ when nodes = [] || List.mem (string node) nodes -> (
              let actual = ref None in
              check_row ~capture:(fun a -> actual := Some a) cache directory row;
              match !actual with
              | None -> line
              | Some outcome ->
                  incr changed;
                  let start = max (rfind "(ok \"" line) (rfind "(error \"" line) in
                  if start < 0 then fail "golden row has no outcome";
                  String.sub line 0 start
                  ^ (match outcome with
                    | `Ok digest -> sexp_tag "ok" [ sexp_string digest ]
                    | `Error (code, message) ->
                        sexp_tag "error" [ sexp_string code; sexp_string message ])
                  ^ ")")
          | _ -> line)
      lines
  in
  write_gz (Filename.concat directory "ports.sexp.gz") (String.concat "\n" lines);
  Printf.printf "updated %d golden rows in %s\n%!" !changed directory

let expect_rejection label action =
  match
    try
      action ();
      None
    with Failure message -> Some message
  with
  | Some _ -> ()
  | None -> fail ("replay self-test accepted " ^ label)

let self_test () =
  let factory_keys key = native_factories key |> List.map Sop.Edit_graph.factory_key in
  let snap_keys = factory_keys "snap_to_grid" in
  if List.mem "snap_to_grid" snap_keys then
    fail "native snap_to_grid twin leaked into the replay catalog";
  if not (List.mem "fuse" snap_keys) then
    fail "snap_to_grid replay catalog is missing its native fuse dependency";
  let cache = create_cache () in
  let temporary = Filename.temp_file "rays-golden-replay" ".tmp" in
  Sys.remove temporary;
  Unix.mkdir temporary 0o700;
  let inputs_dir = Filename.concat temporary "inputs" in
  Unix.mkdir inputs_dir 0o700;
  let cleanup () =
    Array.iter
      (fun filename -> Sys.remove (Filename.concat inputs_dir filename))
      (Sys.readdir inputs_dir);
    Unix.rmdir inputs_dir;
    Sys.rmdir temporary
  in
  Fun.protect ~finally:cleanup (fun () ->
      let write path contents =
        Out_channel.with_open_bin path (fun channel -> output_string channel contents)
      in
      let line_source = "(sop/line :points 3 :length 2.0)" in
      compare_check cache line_source [] [];
      compare_check cache "(sop/line :points 4 :length 2.0)" [] [];
      compare_check cache "(sop/line :points 4 :length 2.0 :unknown 1)" [] [];
      compare_check cache "(sop/line :points 4 :length 2.0 :name \"changed-base\")" [] [];
      let line_forms = source_workspace line_source in
      let extra_ops =
        match Flow_sop.Operators.all with
        | op :: _ -> { op with Flow.Op.name = "sop/golden_unused" } :: Flow_sop.Operators.all
        | [] -> fail "SOP operator list is empty"
      in
      (match prepared_check cache line_source [] extra_ops "line" line_forms with
      | None -> ()
      | Some _ -> fail "prepared checker accepted a noncanonical operator set");
      ignore (ordinary_checked [] extra_ops line_forms);
      let line_expected =
        match output (lower line_source [] Flow_sop.Operators.all []) with
        | `Ok digest -> sexp_tag "ok" [ sexp_string digest ]
        | `Error (code, message) -> fail ("line self-test failed: " ^ code ^ " " ^ message)
      in
      let line_row =
        sexp_tag "golden"
          [
            sexp_string "line";
            sexp_string "none";
            sexp_string "success";
            sexp_string line_source;
            Flow.Syntax.parse (sexp_tag "inputs" []) |> get_flow |> List.hd;
            Flow.Syntax.parse line_expected |> get_flow |> List.hd;
          ]
      in
      let line = Flow.Syntax.parse line_row |> get_flow |> List.hd in
      check_row cache temporary line;
      let coverage =
        Flow.Syntax.parse
          "(case \"line\" \"none\" \"success\") (case \"line\" \"none\" \"pending\")"
        |> get_flow
      in
      check_key cache temporary "line" [ line ] [ List.hd coverage ];
      check_fixture cache temporary "line" "none" [ line ] [ List.hd coverage ];
      (* --update restores a changed digest byte for byte. *)
      let stored = Filename.concat temporary "ports.sexp.gz" in
      let build outcome =
        String.concat ""
          [
            String.sub line_row 0 (String.length line_row - String.length line_expected - 1);
            outcome;
            ")\n";
          ]
      in
      write_gz stored (build line_expected);
      let original = In_channel.with_open_bin stored In_channel.input_all in
      write_gz stored (build (sexp_tag "ok" [ sexp_string (String.make 32 '0') ]));
      update temporary [ "line" ];
      if In_channel.with_open_bin stored In_channel.input_all <> original then
        fail "--update did not restore the stored golden";
      Sys.remove stored;
      expect_rejection "partial capture checkpoint" (fun () -> check_coverage coverage [ line ]);
      expect_rejection "partial per-key checkpoint" (fun () ->
          check_key cache temporary "line" [ line ] coverage);
      expect_rejection "partial per-fixture checkpoint" (fun () ->
          check_fixture cache temporary "line" "none" [ line ] coverage);
      expect_rejection "changed source provenance" (fun () ->
          ignore
            (validate_provenance "running" (String.make 32 '0')
               [ ("node.rays", String.make 32 '1') ]
               [ ("node.rays", String.make 32 '2') ]));
      let positions = Packed.Float3.of_owned ~x:[| 0. |] ~y:[| 0. |] ~z:[| 0. |] |> get_string in
      let topology =
        Topology.create_owned ~point_count:1 ~vertex_points:[||] ~primitive_offsets:[| 0 |]
          ~primitive_kinds:[||]
        |> get_string
      in
      let geometry = Geometry.create ~positions ~topology () |> get_string in
      let fixture = Fixture_codec.encode geometry in
      let digest = Digest.to_hex (Digest.string fixture) in
      let relative = "inputs/geometry-" ^ digest ^ ".sexp" in
      write (Filename.concat temporary relative) fixture;
      let payload = input_payload cache.inputs temporary "geometry" relative in
      assert (input_payload cache.inputs temporary "geometry" relative == payload);
      let input_forms =
        sexp_tag "inputs"
          [
            ( sexp_tag "geometry" [ sexp_string relative ] |> fun source ->
              Flow.Syntax.parse source |> get_flow |> List.hd );
          ]
      in
      let color_source = "(sop/set_color (sop/ext_port_in0) :owner \"Point\" :group \"missing\")" in
      let with_, factories, ops =
        inputs ~key:"set_color" cache.inputs temporary
          (children "inputs" (Flow.Syntax.parse input_forms |> get_flow |> List.hd))
      in
      let color_outcome =
        output (lower color_source factories (ops @ Flow_sop.Operators.all) with_)
      in
      let error_expected =
        match color_outcome with
        | `Error (code, message) -> sexp_tag "error" [ sexp_string code; sexp_string message ]
        | `Ok _ -> fail "set_color self-test did not produce a cook refusal"
      in
      let color_row =
        sexp_tag "golden"
          [
            sexp_string "set_color";
            sexp_string "single";
            sexp_string "missing-group";
            sexp_string color_source;
            Flow.Syntax.parse input_forms |> get_flow |> List.hd;
            Flow.Syntax.parse error_expected |> get_flow |> List.hd;
          ]
      in
      check_row cache temporary (Flow.Syntax.parse color_row |> get_flow |> List.hd);
      expect_rejection "malformed golden row" (fun () ->
          check_row cache temporary (Flow.Syntax.parse "(wrong)" |> get_flow |> List.hd));
      expect_rejection "duplicate golden rows" (fun () ->
          ignore
            (unique_rows "golden"
               [
                 Flow.Syntax.parse color_row |> get_flow |> List.hd;
                 Flow.Syntax.parse color_row |> get_flow |> List.hd;
               ]));
      write (Filename.concat temporary relative) "tampered";
      expect_rejection "tampered cached fixture" (fun () ->
          check_row cache temporary (Flow.Syntax.parse color_row |> get_flow |> List.hd));
      Sys.remove (Filename.concat temporary relative);
      expect_rejection "missing cached fixture" (fun () ->
          check_row cache temporary (Flow.Syntax.parse color_row |> get_flow |> List.hd));
      Printf.printf
        "replay self-test checked output, refusal, malformed/duplicate rows, partial checkpoints, \
         cached inputs and tampered/missing fixtures\n\
         %!")

let benchmark_inputs directory rounds =
  if rounds < 1 then fail "benchmark rounds must be positive";
  let files =
    Sys.readdir (Filename.concat directory "inputs")
    |> Array.to_list
    |> List.filter (fun file -> String.ends_with ~suffix:".sexp" file)
    |> List.sort String.compare
  in
  let cache = Input_cache.create 256 in
  let allocated = Gc.allocated_bytes () and started = Unix.gettimeofday () in
  for _ = 1 to rounds do
    List.iter
      (fun file ->
        let kind =
          if String.starts_with ~prefix:"geometry-" file then "geometry"
          else if String.starts_with ~prefix:"image-" file then "image"
          else fail ("unknown fixture file: " ^ file)
        in
        ignore (Sys.opaque_identity (input_payload cache directory kind ("inputs/" ^ file))))
      files
  done;
  Printf.printf "%d fixtures x %d rounds: %.3f s, %.1f MB allocated\n%!" (List.length files) rounds
    (Unix.gettimeofday () -. started)
    ((Gc.allocated_bytes () -. allocated) /. 1_000_000.)

let () =
  match Array.to_list Sys.argv with
  | [ _; "--self-test" ] -> self_test ()
  | _ :: "--update" :: directory :: nodes when nodes <> [] -> update directory nodes
  | [ _; "--update-all"; directory ] -> update directory []
  | [ _; "--benchmark-inputs"; directory; rounds ] ->
      benchmark_inputs directory (int_of_string rounds)
  | _ :: "--keys" :: directory :: keys when keys <> [] ->
      let status = verify_provenance directory in
      let unique_keys = List.sort_uniq String.compare keys in
      if List.length unique_keys <> List.length keys then fail "duplicate --keys entry";
      let rows, coverage = read_corpus directory in
      let cache = create_cache () in
      List.iter (fun key -> check_key cache directory key rows coverage) unique_keys;
      (* The keys are checked independently against their own exact case identities;
         full-corpus mode below retains the registry-wide completeness requirement. *)
      Printf.printf "checked %d selected keys from %s (provenance %s)\n%!" (List.length unique_keys)
        directory status
  | [ _; "--fixture"; directory; key; fixture ] ->
      let status = verify_provenance directory in
      let rows, coverage = read_corpus directory in
      check_fixture (create_cache ()) directory key fixture rows coverage;
      Printf.printf "selected fixture provenance %s\n%!" status
  | [ _; "--fixture-ordinary"; directory; key; fixture ] ->
      let status = verify_provenance directory in
      let rows, coverage = read_corpus directory in
      check_fixture ~prepared:false (create_cache ()) directory key fixture rows coverage;
      Printf.printf "selected fixture provenance %s\n%!" status
  | [ _; "--fixture-profile"; directory; key; fixture ] ->
      let status = verify_provenance directory in
      let rows, coverage = read_corpus directory in
      check_fixture ~profile:true (create_cache ()) directory key fixture rows coverage;
      Printf.printf "selected fixture provenance %s\n%!" status
  | [ _; "--fixture-ordinary-profile"; directory; key; fixture ] ->
      let status = verify_provenance directory in
      let rows, coverage = read_corpus directory in
      check_fixture ~prepared:false ~profile:true (create_cache ()) directory key fixture rows
        coverage;
      Printf.printf "selected fixture provenance %s\n%!" status
  | [ _; directory ] ->
      let rows, coverage = read_corpus directory in
      check_coverage coverage rows;
      let nodes =
        List.fold_left
          (fun nodes row ->
            match children "case" row with
            | [ name; _; _ ] -> name :: nodes
            | _ -> fail "malformed coverage row")
          [] coverage
        |> List.map string |> List.sort_uniq String.compare
      in
      let expected_nodes =
        Flow_sop.Catalog.node_definitions ()
        |> get_flow
        |> List.map (fun node ->
            let qualified = node.Flow.Defnode.kind.Flow.Catalog.qualified in
            match String.index_opt qualified '/' with
            | Some slash -> String.sub qualified (slash + 1) (String.length qualified - slash - 1)
            | None -> fail ("unqualified Lisp node key: " ^ qualified))
        |> List.sort_uniq String.compare
      in
      if nodes <> expected_nodes then begin
        let absent = List.filter (fun key -> not (List.mem key nodes)) expected_nodes
        and extra = List.filter (fun key -> not (List.mem key expected_nodes)) nodes in
        fail
          (Printf.sprintf
             "golden corpus does not cover every Lisp node (%d expected); absent=[%s] extra=[%s]"
             (List.length expected_nodes) (String.concat "," absent) (String.concat "," extra))
      end;
      (* runtest replays the first row of each node; REPLAY_ALL=1 (@qualification-scale) replays every row *)
      let rows =
        if Sys.getenv_opt "REPLAY_ALL" = Some "1" then rows
        else
          let seen = Hashtbl.create 128 in
          List.filter
            (fun row ->
              match children "golden" row with
              | node :: _ when not (Hashtbl.mem seen (string node)) ->
                  Hashtbl.add seen (string node) ();
                  true
              | _ -> false)
            rows
      in
      (* REPLAY_JOBS=J forks J workers, each owning the nodes that hash to it (so a node's prepared library is built once). *)
      let jobs =
        match Sys.getenv_opt "REPLAY_JOBS" with Some j -> max 1 (int_of_string j) | None -> 1
      in
      let children =
        List.init
          (if jobs = 1 then 0 else jobs)
          (fun job ->
            match Unix.fork () with
            | 0 ->
                let cache = create_cache () in
                let code =
                  try
                    List.iter
                      (fun row ->
                        match children "golden" row with
                        | node :: _ when Hashtbl.hash (string node) mod jobs = job ->
                            check_row cache directory row
                        | _ -> ())
                      rows;
                    0
                  with Failure message ->
                    prerr_endline message;
                    1
                in
                Unix._exit code
            | pid -> pid)
      in
      List.iter
        (fun pid ->
          if snd (Unix.waitpid [] pid) <> Unix.WEXITED 0 then fail "golden replay worker failed")
        children;
      (if jobs = 1 then
         let cache = create_cache () in
         List.iter (check_row cache directory) rows);
      Printf.printf "checked %d Lisp SOP golden rows without native factories\n%!"
        (List.length rows)
  | _ ->
      fail
        "usage: replay.exe [DIR | --update DIR NODE... | --update-all DIR | --keys DIR KEY... | \
         --fixture DIR KEY FIXTURE | --fixture-ordinary DIR KEY FIXTURE | --fixture-profile DIR \
         KEY FIXTURE | --fixture-ordinary-profile DIR KEY FIXTURE | --benchmark-inputs DIR ROUNDS \
         | --self-test]"
