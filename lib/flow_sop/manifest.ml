let version = 202609

let quote value =
  let buffer = Buffer.create (String.length value + 2) in
  Buffer.add_char buffer '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | character -> Buffer.add_char buffer character)
    value;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let group name values =
  "(" ^ name ^ (if values = [] then "" else " " ^ String.concat " " values) ^ ")"

let strings name values = group name (List.map quote values)

let number number =
  if not (Float.is_finite number) then invalid_arg "Manifest.number: non-finite";
  let text = Flow.Lisp.float number in
  if String.ends_with ~suffix:".0" text then String.sub text 0 (String.length text - 2) else text

let optional render = function None -> "nil" | Some value -> render value

let literal = function
  | Param.Bool_value value -> group "bool" [ string_of_bool value ]
  | Param.Int_value value -> group "int" [ string_of_int value ]
  | Param.Float_value value -> group "float" [ number value ]
  | Param.Text_value value -> group "text" [ quote value ]
  | Param.Choice_value value -> group "choice" [ quote value ]

let field (field : Param.field_view) =
  let kind =
    match field.kind with
    | Param.Toggle_view -> group "bool" []
    | Param.Integer_view range ->
        group "int"
          [
            group "soft" [ string_of_int range.soft_min; string_of_int range.soft_max ];
            group "hard"
              [ optional string_of_int range.hard_min; optional string_of_int range.hard_max ];
          ]
    | Param.Floating_view range ->
        group "float"
          [
            group "soft" [ number range.soft_min; number range.soft_max ];
            group "hard" [ optional number range.hard_min; optional number range.hard_max ];
          ]
    | Param.Text_view -> group "text" []
    | Param.Choice_view labels -> strings "choice" (Array.to_list labels)
  in
  group "field"
    ([
       quote field.name;
       quote field.label;
       strings "folder" field.folder;
       kind;
       literal field.default;
       group "primary" [ string_of_bool field.primary ];
       (match field.vec3 with
       | None -> group "vec3" []
       | Some (name, index) -> group "vec3" [ quote name; string_of_int index ]);
     ]
    @ List.map (fun unit -> group "unit" [ quote unit ]) (Option.to_list field.unit)
    @ List.map (fun text -> group "description" [ quote text ]) (Option.to_list field.description)
    @
    match field.impact with
    | Cook -> []
    | View -> [ group "impact" [ "view" ] ]
    | Export -> [ group "impact" [ "export" ] ])

let outputs ports =
  group "outputs"
    (List.map
       (fun (name, ty) ->
         let name_ty = String.lowercase_ascii (Flow.Port_type.name ty) in
         group "output"
           [ quote name; (if Flow.Symbol.valid_name name_ty then name_ty else quote name_ty) ])
       ports)

let facts (facts : Sop.Node.facts) =
  let mode =
    match facts.cook_mode with
    | Generator -> [ "generator" ]
    | Duplicate_input index -> [ "duplicate-input"; string_of_int index ]
    | In_place index -> [ "in-place"; string_of_int index ]
    | Instance_input index -> [ "instance-input"; string_of_int index ]
    | Passthrough index -> [ "passthrough"; string_of_int index ]
    | Generic -> [ "generic" ]
  in
  group "facts"
    [
      group "cook-mode" mode;
      group "elementwise"
        [
          (match facts.elementwise with
          | Points -> "points"
          | Primitives -> "primitives"
          | None -> "none");
        ];
      strings "reads" facts.reads;
      strings "writes" facts.writes;
      group "topology"
        [ (match facts.topology with Preserved -> "preserved" | Changed -> "changed") ];
      group "exact" [ string_of_bool facts.exact ];
    ]

let entry ?facts:declaration ?(keyword_inputs = []) ?(properties = []) ~qualified ~key ~operation
    ~label ~category ~slots ~fields ~result () =
  let fields =
    match fields with
    | [] -> group "fields" []
    | fields -> "(fields\n        " ^ String.concat "\n        " (List.map field fields) ^ ")"
  in
  let properties =
    [
      group "key" [ quote key ];
      group "operation" [ quote operation ];
      group "label" [ quote label ];
      strings "category" category;
      group "slots" slots;
      fields;
      outputs result;
    ]
    @ (if keyword_inputs = [] then [] else [ strings "keyword-inputs" keyword_inputs ])
    @ List.map facts (Option.to_list declaration)
    @ properties
  in
  "(kind " ^ quote qualified ^ "\n      " ^ String.concat "\n      " properties ^ ")"

let descriptor ?facts (d : Catalog.descriptor) =
  let module Edit = Sop.Edit_graph in
  let types = if d.slot_types = [] then List.map (fun _ -> "geometry") d.slots else d.slot_types in
  let slots =
    List.map2
      (fun (name, requirement) ty ->
        group "slot"
          [
            quote name;
            (match requirement with
            | Edit.Required -> "required"
            | Edit.Optional -> "optional"
            | Edit.Rest -> "rest"
            | Edit.Optional_rest -> "optional-rest");
          ]
        |> fun slot ->
        if ty = "geometry" then slot
        else
          String.sub slot 0 (String.length slot - 1)
          ^ " "
          ^ (if Flow.Symbol.valid_name ty then ty else quote ty)
          ^ ")")
      d.slots types
  in
  entry ?facts ~qualified:d.qualified ~key:d.key ~operation:d.operation ~label:d.label
    ~category:d.category ~slots ~fields:d.fields ~keyword_inputs:d.keyword_inputs
    ~result:
      (if String.starts_with ~prefix:"sop/" d.qualified then [ ("geo", Flow.Port_type.Geometry) ]
       else [])
    ()

let lisp_node ~calls (node : Flow.Defnode.t) =
  let key = Flow.Catalog.short node.kind.qualified in
  let slots =
    List.map
      (fun (slot : Flow.Catalog.slot) ->
        let ty = Flow.Ty.to_string (Flow.Workspace.slot_ty ~slot node.kind) in
        let parts =
          [
            quote slot.name;
            (match (slot.required, slot.rest) with
            | true, false -> "required"
            | false, false -> "optional"
            | true, true -> "rest"
            | false, true -> "optional-rest");
          ]
        in
        group "slot"
          (if ty = "geometry" then parts
           else parts @ [ (if Flow.Symbol.valid_name ty then ty else quote ty) ]))
      node.slots
  in
  let resolve =
    Flow.Catalog.resolve_component_names (fun name ->
        List.find_map
          (fun (field : Param.field_view) ->
            match field.default with
            | Param.Text_value text when field.name = name -> Some text
            | _ -> None)
          node.fields)
  in
  let declaration =
    Option.map
      (fun (f : Flow.Catalog.kernel_facts) ->
        Sop.Node.
          {
            cook_mode = (if node.slots = [] then Generator else Generic);
            elementwise =
              (match f.elementwise with
              | Points -> Points
              | Primitives -> Primitives
              | Irregular -> None);
            reads = resolve f.reads;
            writes = resolve f.writes;
            topology = (if f.preserves_topology then Preserved else Changed);
            exact = f.exact;
          })
      node.kind.facts
  in
  entry ?facts:declaration ~qualified:node.kind.qualified ~key ~operation:node.operation
    ~label:node.label ~category:node.category ~slots ~fields:node.fields
    ~keyword_inputs:node.keyword_inputs ~result:node.kind.outputs
    ~properties:[ group "body" [ "lisp" ]; strings "calls" calls ]
    ()

(* The digest covers the sop kinds, the catalog the PPX links against; the
   generated scene, world and settings kinds ([extra]) follow them and are outside it. *)
let generate ?extra ?nodes factories =
  let supplied = Option.is_some nodes in
  Result.bind
    (match nodes with Some nodes -> Ok nodes | None -> Catalog.node_definitions ())
    (fun nodes ->
      Result.bind (Catalog.of_factories ~version ?extra ~nodes factories) (fun catalog ->
          Result.bind
            (if supplied then Ok (List.map (fun (node : Flow.Defnode.t) -> node.form) nodes)
             else Catalog.sources ())
            (fun sources ->
              let workspace = Flow.Syntax.parse "(workspace sop_nodes)" |> Result.get_ok in
              let checked, diagnostics =
                Catalog.check_workspace ~ops:Operators.all ~library:true ~sources catalog workspace
              in
              let checked =
                match checked with
                | Some checked -> Ok checked
                | None -> Error (List.hd diagnostics)
              in
              Result.map
                (fun (checked : Flow.Workspace.t) ->
                  let kinds =
                    List.map
                      (fun f ->
                        descriptor ~facts:(Sop.Edit_graph.factory_facts f) (Catalog.descriptor f))
                      factories
                    @ List.map
                        (fun (node : Flow.Defnode.t) ->
                          let definition =
                            List.find
                              (fun (definition : Flow.Workspace.node_definition) ->
                                definition.kind.qualified = node.kind.qualified)
                              checked.nodes
                          in
                          lisp_node
                            ~calls:(Flow.Workspace.kind_calls definition.body.body)
                            { node with fields = definition.fields; kind = definition.kind })
                        nodes
                  in
                  let payload =
                    group "version" [ string_of_int version ] ^ "\n" ^ group "kinds" kinds
                  in
                  let digest = Digest.to_hex (Digest.string payload) in
                  let all = kinds @ List.map descriptor (Option.value extra ~default:[]) in
                  let text =
                    "(flow_manifest\n  "
                    ^ group "version" [ string_of_int version ]
                    ^ "\n  "
                    ^ group "digest" [ quote digest ]
                    ^ "\n  (kinds\n    " ^ String.concat "\n    " all ^ "))\n"
                  in
                  (text, digest))
                checked)))
