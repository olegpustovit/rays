module Edit = Sop.Edit_graph

type descriptor = {
  qualified : string;
  key : string;
  operation : string;
  label : string;
  category : string list;
  slots : (string * Edit.input_requirement) list;
  slot_types : string list;
  keyword_inputs : string list;
  fields : Param.field_view list;
}

let descriptor factory =
  {
    qualified = "sop/" ^ Edit.factory_key factory;
    key = Edit.factory_key factory;
    operation = Edit.factory_operation factory;
    label = Edit.factory_label factory;
    category = Edit.factory_category factory;
    slots = List.combine (Edit.factory_slot_names factory) (Edit.factory_inputs factory);
    slot_types = Edit.factory_input_types factory;
    keyword_inputs = Edit.factory_keyword_inputs factory;
    fields = Edit.factory_fields factory;
  }

let of_defnode (node : Flow.Defnode.t) =
  {
    qualified = node.kind.qualified;
    key =
      (match String.index_opt node.kind.qualified '/' with
      | Some i -> String.sub node.kind.qualified (i + 1) (String.length node.kind.qualified - i - 1)
      | None -> node.kind.qualified);
    operation = node.operation;
    label = node.label;
    category = node.category;
    slots =
      List.map
        (fun (slot : Flow.Catalog.slot) ->
          ( slot.name,
            match (slot.required, slot.rest) with
            | true, true -> Edit.Rest
            | false, true -> Optional_rest
            | true, false -> Required
            | false, false -> Optional ))
        node.slots;
    slot_types =
      List.map
        (fun (slot : Flow.Catalog.slot) ->
          Option.fold ~none:"geometry" ~some:Flow.Ty.to_string slot.ty)
        node.slots;
    keyword_inputs = node.keyword_inputs;
    fields = node.fields;
  }

let context_of = Flow.Context.of_qualified

let embedded_sources =
  lazy
    (List.fold_left
       (fun result (file, text) ->
         Result.bind result (fun loaded ->
             Result.bind (Flow.Syntax.parse text) (fun forms ->
                 let forms =
                   List.concat_map
                     (fun (form : Flow.Syntax.t) ->
                       match form.node with
                       | Flow.Syntax.List
                           ({ node = Flow.Syntax.Sym "workspace"; _ } :: _ :: children) ->
                           children
                       | _ -> [ form ])
                     forms
                 in
                 match
                   List.find_opt
                     (fun form ->
                       not
                         (List.mem (Flow.Syntax.head form)
                            [ Some "defnode"; Some "defn"; Some "defmacro" ]))
                     forms
                 with
                 | Some form ->
                     Error
                       (Diagnostic.error ~span:form.span ~code:"E_FORM"
                          (file ^ ": node sources contain only defnode, defn and defmacro forms"))
                 | None -> Ok (loaded @ forms))))
       (Ok []) Node_sources.files)

let sources () = Lazy.force embedded_sources

let definitions_of_sources forms =
  List.fold_left
    (fun result form ->
      Result.bind result (fun nodes ->
          if Flow.Syntax.head form <> Some "defnode" then Ok nodes
          else Result.map (fun node -> node :: nodes) (Flow.Defnode.parse form)))
    (Ok []) forms
  |> Result.map List.rev

(* The embedded library never changes; custom source lists remain caller-owned and uncached. *)
let embedded_node_definitions = lazy (Result.bind (sources ()) definitions_of_sources)
let node_definitions () = Lazy.force embedded_node_definitions

let library_descriptors =
  lazy (match node_definitions () with Ok nodes -> List.map of_defnode nodes | Error _ -> [])

let with_library present =
  present
  @ List.filter
      (fun d -> not (List.exists (fun p -> p.qualified = d.qualified) present))
      (Lazy.force library_descriptors)

(* Names a form mentions, as a set; a definition is keyed by the name it binds. *)
let rec mentions into (form : Flow.Syntax.t) =
  (match form.node with Flow.Syntax.Sym name -> Hashtbl.replace into name () | _ -> ());
  List.iter (mentions into) (Flow.Syntax.children form)

let definition_name (form : Flow.Syntax.t) =
  match form.node with
  | Flow.Syntax.List
      ({ node = Sym ("defnode" | "defn" | "defmacro"); _ } :: { node = Sym name; _ } :: _) ->
      Some name
  | _ -> None

(* The library definitions reachable from [forms] through the names they mention, in library order.
   Syntactic and conservative: a name anywhere in a form counts, so shadowing and duplicate-name errors
   are still reported. *)
type source_index = {
  source_list : Flow.Syntax.t list;
  source_forms : Flow.Syntax.t array;
  by_name : (string, int) Hashtbl.t;
  references : string list array;
}

let prepare_source_index source_list =
  let source_forms = Array.of_list source_list in
  let by_name = Hashtbl.create 256 in
  let references =
    Array.mapi
      (fun index form ->
        Option.iter (fun name -> Hashtbl.add by_name name index) (definition_name form);
        let names = Hashtbl.create 16 in
        mentions names form;
        Hashtbl.fold (fun name () names -> name :: names) names [])
      source_forms
  in
  { source_list; source_forms; by_name; references }

(* The one embedded library is immutable; keep its dependency index with its existing lazy AST,
   and leave caller-supplied libraries on the uncached path. *)
let embedded_source_index =
  lazy
    (match sources () with
    | Ok source_list -> Some (prepare_source_index source_list)
    | Error _ -> None)

let reachable_index index forms =
  let seen = Hashtbl.create 64 and queue = Queue.create () in
  let visit name =
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.add seen name ();
      Queue.add name queue
    end
  in
  let scan form =
    let names = Hashtbl.create 16 in
    mentions names form;
    Hashtbl.iter (fun name () -> visit name) names
  in
  List.iter scan forms;
  let keep = Array.make (Array.length index.source_forms) false in
  while not (Queue.is_empty queue) do
    List.iter
      (fun source_index ->
        if not keep.(source_index) then begin
          keep.(source_index) <- true;
          List.iter visit index.references.(source_index)
        end)
      (Hashtbl.find_all index.by_name (Queue.pop queue))
  done;
  Array.to_list index.source_forms
  |> List.filteri (fun i form -> definition_name form = None || keep.(i))

let reachable_uncached sources forms =
  let sources = Array.of_list sources in
  let by_name = Hashtbl.create 256 in
  Array.iteri
    (fun index form ->
      Option.iter (fun name -> Hashtbl.add by_name name index) (definition_name form))
    sources;
  let seen = Hashtbl.create 64 and queue = Queue.create () in
  let visit name =
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.add seen name ();
      Queue.add name queue
    end
  in
  let scan form =
    let names = Hashtbl.create 16 in
    mentions names form;
    Hashtbl.iter (fun name () -> visit name) names
  in
  List.iter scan forms;
  let keep = Array.make (Array.length sources) false in
  while not (Queue.is_empty queue) do
    List.iter
      (fun index ->
        if not keep.(index) then begin
          keep.(index) <- true;
          scan sources.(index)
        end)
      (Hashtbl.find_all by_name (Queue.pop queue))
  done;
  List.filteri
    (fun index form -> definition_name form = None || keep.(index))
    (Array.to_list sources)

let reachable sources forms =
  match Lazy.force embedded_source_index with
  | Some index when index.source_list == sources -> reachable_index index forms
  | _ -> reachable_uncached sources forms

module Private = struct
  let reachable_sources = reachable
  let definitions_of_sources = definitions_of_sources
end

let check_workspace_raw ?ops ?library ?sources:override catalog forms =
  let source_forms = match override with Some sources -> Ok sources | None -> sources () in
  match
    Result.bind source_forms (fun source_forms ->
        let nodes =
          match override with
          | None -> node_definitions ()
          | Some supplied -> (
              match sources () with
              | Ok embedded when supplied == embedded -> node_definitions ()
              | _ -> definitions_of_sources source_forms)
        in
        Result.map (fun nodes -> (source_forms, nodes)) nodes)
  with
  | Error diagnostic -> (None, [ diagnostic ])
  | Ok (sources, nodes) ->
      let sources, nodes =
        match override with
        | Some _ when library = Some true -> (sources, nodes)
        | Some _ ->
            (* An explicit library is pruned the same way: checking all of it per workspace made
               every test case pay for every Lisp node. *)
            let sources = reachable sources forms in
            ( sources,
              List.filter
                (fun (node : Flow.Defnode.t) ->
                  List.exists (fun form -> definition_name form = Some node.kind.qualified) sources)
                nodes )
        | None ->
            let nodes =
              List.filter
                (fun (node : Flow.Defnode.t) ->
                  List.exists
                    (fun (kind : Flow.Catalog.kind) -> kind.qualified = node.kind.qualified)
                    catalog.Flow.Catalog.kinds)
                nodes
            in
            let sources =
              if nodes = [] then []
              else
                List.filter
                  (fun form ->
                    match form.Flow.Syntax.node with
                    | List ({ node = Sym "defnode"; _ } :: { node = Sym name; _ } :: _) ->
                        List.exists
                          (fun (node : Flow.Defnode.t) -> node.kind.qualified = name)
                          nodes
                    | _ -> true)
                  sources
            in
            (* A library definition nothing reachable from the authored forms names is neither
               checked nor spliced; its kind stays in the catalog, so names still collide. *)
            let sources = reachable sources forms in
            let nodes =
              List.filter
                (fun (node : Flow.Defnode.t) ->
                  List.exists
                    (fun form ->
                      match form.Flow.Syntax.node with
                      | List ({ node = Sym "defnode"; _ } :: { node = Sym name; _ } :: _) ->
                          name = node.kind.qualified
                      | _ -> false)
                    sources)
                nodes
            in
            (sources, nodes)
      in
      if sources = [] then Flow.Workspace.check ?ops ?library catalog forms
      else
        let authored = forms in
        let rec next_id (form : Flow.Syntax.t) =
          List.fold_left
            (fun highest child -> max highest (next_id child))
            (form.id + 1) (Flow.Syntax.children form)
        in
        let first = List.fold_left (fun highest form -> max highest (next_id form)) 0 forms in
        let sources, _ =
          List.fold_left
            (fun (reversed, next) source ->
              let source, next = Flow.Syntax.renumber next source in
              (source :: reversed, next))
            ([], first) sources
        in
        let sources = List.rev sources in
        let forms =
          List.map
            (fun (form : Flow.Syntax.t) ->
              match form.node with
              | Flow.Syntax.List
                  (({ node = Flow.Syntax.Sym "workspace"; _ } as head) :: name :: children) ->
                  { form with node = Flow.Syntax.List ((head :: name :: sources) @ children) }
              | _ -> form)
            forms
        in
        let catalog =
          {
            catalog with
            Flow.Catalog.kinds =
              List.filter
                (fun (kind : Flow.Catalog.kind) ->
                  not
                    (List.exists
                       (fun (node : Flow.Defnode.t) -> node.kind.qualified = kind.qualified)
                       nodes))
                catalog.Flow.Catalog.kinds;
          }
        in
        let workspace, diagnostics = Flow.Workspace.check ?ops ?library catalog forms in
        ( Option.map
            (fun (workspace : Flow.Workspace.t) -> { workspace with source = authored })
            workspace,
          diagnostics )

let check_workspace ?ops ?library ?sources catalog forms =
  match check_workspace_raw ?ops ?library ?sources catalog forms with
  | None, diagnostics -> (None, diagnostics)
  | Some workspace, diagnostics -> (
      match Encoded_parameters.checked workspace with
      | Ok workspace -> (Some workspace, diagnostics)
      | Error diagnostic -> (None, diagnostics @ [ diagnostic ]))

(* The catalog describes a kind at its default parameters: the [:facts] names that read a parameter
   ([$param], [$param|fallback], [$param~prefix]) resolve against the defaults, as the manifest prints them. *)
let described_kind (node : Flow.Defnode.t) =
  let resolve =
    Flow.Catalog.resolve_component_names (fun name ->
        List.find_map
          (fun (field : Param.field_view) ->
            match field.default with
            | Param.Text_value text when field.name = name -> Some text
            | _ -> None)
          node.fields)
  in
  {
    node.kind with
    facts =
      Option.map
        (fun (f : Flow.Catalog.kernel_facts) ->
          { f with reads = resolve f.reads; writes = resolve f.writes })
        node.kind.facts;
  }

let of_factories ~version ?(extra = []) ?nodes factories =
  let declarations = Hashtbl.create (List.length factories) in
  List.iter
    (fun factory ->
      let facts = Edit.factory_facts factory in
      Hashtbl.replace declarations
        ("sop/" ^ Edit.factory_key factory)
        Flow.Catalog.
          {
            elementwise =
              (match facts.elementwise with
              | Points -> Points
              | Primitives -> Primitives
              | None -> Irregular);
            reads = facts.reads;
            writes = facts.writes;
            preserves_topology = facts.topology = Sop.Node.Preserved;
            exact = facts.exact;
          })
    factories;
  let seen = Hashtbl.create 64 in
  let rec build reversed = function
    | [] -> Ok Flow.Catalog.{ version; kinds = List.rev reversed }
    | (entry : descriptor) :: rest ->
        if entry.slot_types <> [] && List.length entry.slot_types <> List.length entry.slots then
          Error
            (Diagnostic.error ~code:"E_CATALOG" ("Input type count differs in " ^ entry.qualified))
        else if
          not
            (List.for_all
               (fun name ->
                 match Flow.Ty.of_string name with
                 | Some (Flow.Ty.Named _ | Flow.Ty.Fn (Some _)) -> true
                 | _ -> false)
               entry.slot_types)
        then Error (Diagnostic.error ~code:"E_CATALOG" ("Unknown input type in " ^ entry.qualified))
        else if
          List.length (List.sort_uniq String.compare entry.keyword_inputs)
          <> List.length entry.keyword_inputs
          || List.exists
               (fun name ->
                 match List.find_index (fun (slot, _) -> slot = name) entry.slots with
                 | None -> true
                 | Some index -> (
                     snd (List.nth entry.slots index) <> Edit.Required
                     ||
                     match Option.bind (List.nth_opt entry.slot_types index) Flow.Ty.of_string with
                     | Some (Flow.Ty.Fn (Some _)) -> false
                     | Some ty when ty = Flow.Ty.image -> false
                     | _ -> true))
               entry.keyword_inputs
        then
          Error
            (Diagnostic.error ~code:"E_CATALOG"
               ("Keyword inputs require distinct required fn/image slots in " ^ entry.qualified))
        else if context_of entry.qualified = None then
          Error
            (Diagnostic.error ~code:"E_CATALOG" ("Unknown context prefix in " ^ entry.qualified))
        else if Hashtbl.mem seen entry.qualified then
          Error (Diagnostic.error ~code:"E_CATALOG" ("Duplicate kind " ^ entry.qualified))
        else begin
          let types =
            if entry.slot_types = [] then List.map (fun _ -> None) entry.slots
            else
              List.map
                (fun name -> if name = "geometry" then None else Flow.Ty.of_string name)
                entry.slot_types
          in
          let inputs =
            List.map2
              (fun (name, requirement) ty ->
                Flow.Catalog.
                  {
                    name;
                    required = requirement = Edit.Required || requirement = Edit.Rest;
                    rest = requirement = Edit.Rest || requirement = Edit.Optional_rest;
                    ty;
                  })
              entry.slots types
          in
          let keywords, slots =
            List.partition
              (fun (s : Flow.Catalog.slot) -> List.mem s.name entry.keyword_inputs)
              inputs
          in
          let keywords =
            List.map
              (fun (s : Flow.Catalog.slot) ->
                let ty =
                  match s.ty with
                  | Some (Flow.Ty.Fn (Some signature)) -> Flow.Port_type.Fn signature
                  | Some ty when ty = Flow.Ty.image -> Flow.Port_type.Image
                  | _ -> invalid_arg ("Typed keyword input must be fn or image: " ^ s.name)
                in
                Flow.Catalog.
                  {
                    name = s.name;
                    label = s.name;
                    ty = Some ty;
                    fields = [];
                    folder = [];
                    primary = true;
                    unit = None;
                  })
              keywords
          in
          Hashtbl.add seen entry.qualified ();
          Result.bind (Port.parameters entry.fields) (fun ports ->
              if List.exists (fun p -> List.mem p.Port.path entry.keyword_inputs) ports then
                Error
                  (Diagnostic.error ~code:"E_CATALOG"
                     ("Keyword input clashes with a scalar parameter in " ^ entry.qualified))
              else
                let parameters =
                  List.map
                    (fun port ->
                      let label =
                        match port.Port.fields with
                        | (field : Param.field_view) :: _ -> field.label
                        | [] -> port.path
                      in
                      Flow.Catalog.
                        {
                          name = port.path;
                          label;
                          ty = port.ty;
                          fields =
                            List.map
                              (fun (field : Param.field_view) ->
                                (field.name, field.kind, field.default))
                              port.fields;
                          folder =
                            (match port.Port.fields with
                            | (f : Param.field_view) :: _ -> f.folder
                            | [] -> []);
                          primary =
                            (match port.Port.fields with
                            | (f : Param.field_view) :: _ -> f.primary
                            | [] -> false);
                          unit =
                            (match port.Port.fields with
                            | [ (f : Param.field_view) ] -> f.unit
                            | _ -> None);
                        })
                    ports
                in
                let context = Option.get (context_of entry.qualified) in
                let outputs =
                  if context = Flow.Context.sop then [ ("geo", Flow.Port_type.Geometry) ] else []
                in
                build
                  (Flow.Catalog.
                     {
                       qualified = entry.qualified;
                       context;
                       slots;
                       parameters = parameters @ keywords;
                       outputs;
                       facts = Hashtbl.find_opt declarations entry.qualified;
                     }
                  :: reversed)
                  rest)
        end
  in
  Result.bind
    (build [] (List.map descriptor factories @ extra))
    (fun catalog ->
      Result.bind
        (match nodes with Some nodes -> Ok nodes | None -> node_definitions ())
        (fun nodes ->
          let rec append catalog = function
            | [] -> Ok catalog
            | (node : Flow.Defnode.t) :: rest ->
                if
                  List.exists
                    (fun (kind : Flow.Catalog.kind) -> kind.qualified = node.kind.qualified)
                    catalog.Flow.Catalog.kinds
                then
                  Error
                    (Diagnostic.error ~code:"E_CATALOG" ("Duplicate kind " ^ node.kind.qualified))
                else append { catalog with kinds = catalog.kinds @ [ described_kind node ] } rest
          in
          Result.map
            (fun (checked : Flow.Catalog.catalog) ->
              let native, extra =
                List.partition
                  (fun (kind : Flow.Catalog.kind) -> Hashtbl.mem declarations kind.qualified)
                  catalog.kinds
              in
              { checked with kinds = native @ List.map described_kind nodes @ extra })
            (append catalog nodes)))
