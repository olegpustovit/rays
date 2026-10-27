open Sop
open Rdk_test_support

let merge_of inputs =
  let names = List.mapi (fun i n -> Printf.sprintf "m%d" i, n) inputs in
  Lisp_sop.node ~with_:names (Printf.sprintf "(sop/merge %s)"
    (String.concat " " (List.map (fun (k, _) -> Printf.sprintf "(sop/ext_%s)" k) names)))

let run () =
  let source = Lisp_sop.node {|(sop/box :normals "Auto" :connectivity "Triangles" :size [1.0 1.0 1.0])|} in
  let metadata_schema = Param.schema ~name:"metadata_sharing" ~default:0.
      [Param.field ~name:"value" ~kind:(Param.floating ~min:(-1.) ~max:1. ())
        ~default:0. ~get:Fun.id ~set:(fun value _ -> value) ()] in
  let metadata = List.hd (Param.view metadata_schema 0.) in
  let signed = { metadata with name="signed"; current=Param.Float_value (-0.) }
  and text = { metadata with name="text"; kind=Param.Text_view;
    default=Param.Text_value (String.make 256 'a');
    current=Param.Text_value (String.make 256 'a') } in
  let fields = [metadata; signed; text] in
  let factories = [
    Edit_graph.factory ~fields ~key:"metadata" ~label:"Metadata"
      ~category:["Test"] ~arity:0 (fun _ -> source);
    Edit_graph.factory_slots ~fields ~key:"metadata_slots" ~label:"Metadata slots"
      ~category:["Test"] ~inputs:[Edit_graph.Required] (fun _ -> source);
  ] in
  List.iter (fun factory -> match Edit_graph.factory_fields factory with
    | [value; signed; text] ->
        check (value.default == value.current && text.default == text.current)
          "factory metadata retained duplicate immutable default values";
        (match signed.current with
         | Param.Float_value current ->
             check (Int64.bits_of_float current = Int64.bits_of_float (-0.))
               "factory metadata sharing changed signed-zero bits"
         | _ -> fail "factory metadata sharing changed the value kind")
    | _ -> fail "factory metadata sharing changed fields") factories;
  let middle = Lisp_sop.node ~with_:["source", (source)] {|(sop/null (sop/ext_source))|} in
  let output = Lisp_sop.node ~with_:["middle", (middle)] {|(sop/null (sop/ext_middle))|} in
  let document = Edit_graph.of_graph output in
  check (Edit_graph.node_slot_names document ~node_id:(Node.id middle) = Some ["in0"]
      && Edit_graph.node_slot_names document ~node_id:(-1) = None)
    "editable entry did not expose canonical slots or reject a missing node";
  check (Edit_graph.replace_node middle document |> get == document
      && fst (Edit_graph.apply_parameters document ~node_id:(Node.id middle) [] |> get) == document)
    "unchanged parameter writes rebuilt the editable graph";
  check (Edit_graph.root document = Some (Node.id output)
      && List.length (Edit_graph.inspect document) = 3)
    "editable graph did not import the compiled DAG";
  let disconnected = Edit_graph.disconnect ~consumer:(Node.id middle)
      ~input_index:0 document |> get in
  check (Result.is_error (Edit_graph.compile disconnected)
      && Result.is_ok (Edit_graph.compile_node disconnected
        ~node_id:(Node.id source)))
    "editable graph did not retain a cookable branch around a disconnected slot";
  let reconnected = Edit_graph.connect ~source:(Node.id source)
      ~consumer:(Node.id middle) ~input_index:0 disconnected |> get in
  check (Result.is_ok (Edit_graph.compile reconnected))
    "editable graph did not compile after reconnecting its slot";
  check (Result.is_error (Edit_graph.connect ~source:(Node.id middle)
      ~consumer:(Node.id middle) ~input_index:0 reconnected))
    "editable graph accepted a self cycle";
  let compiled = Edit_graph.compile_all reconnected in
  let node compiled id = Edit_graph.compiled_node compiled ~node_id:id |> get in
  let again = Edit_graph.compile_all ~previous:compiled reconnected in
  check (node again (Node.id output) == node compiled (Node.id output)
      && Node.id (node again (Node.id output))
         = Node.id (Edit_graph.compile reconnected |> get))
    "incremental compile rebuilt an unchanged document";
  let rewired = Edit_graph.disconnect ~consumer:(Node.id middle) ~input_index:0
      reconnected |> get in
  check (Result.is_error (Edit_graph.compiled_node
      (Edit_graph.compile_all ~previous:compiled rewired) ~node_id:(Node.id output)))
    "incremental compile reused a node across a disconnected input";
  let rewired = Edit_graph.connect ~source:(Node.id source)
      ~consumer:(Node.id middle) ~input_index:0 rewired |> get in
  let recompiled = Edit_graph.compile_all ~previous:compiled rewired in
  check (node recompiled (Node.id source) == node compiled (Node.id source)
      && Node.id (node recompiled (Node.id output)) = Node.id output)
    "incremental compile did not reuse the unchanged upstream node";
  let removed = Edit_graph.remove_nodes [Node.id source] reconnected in
  check (Edit_graph.find removed ~node_id:(Node.id source) = None
      && Edit_graph.inputs removed ~node_id:(Node.id middle) = Some [|None|]
      && Result.is_error (Edit_graph.compile removed))
    "editable graph node deletion did not leave an explicit disconnected slot";
  let box_factory = Edit_graph.factory ~key:"box" ~label:"Box"
      ~category:["Create"] ~arity:0 (function
        | [] -> Lisp_sop.node {|(sop/box :normals "Auto" :connectivity "Triangles" :size [1.0 1.0 1.0])|}
        | _ -> assert false) in
  check (Edit_graph.factory_arity box_factory = 0
      && Result.is_ok (Edit_graph.instantiate box_factory []))
    "editable graph factory did not instantiate its declared arity";
  let null_factory = Edit_graph.factory ~key:"null" ~label:"Null"
      ~category:["Utility"] ~arity:1 (function
        | [input] -> Lisp_sop.node ~with_:["input", (input)] {|(sop/null (sop/ext_input))|} | _ -> assert false) in
  check (Edit_graph.factory_slot_names null_factory = ["in0"])
    "unnamed factory inputs did not use canonical port names";
  let loose_null = Edit_graph.instantiate_optional null_factory [None] |> get in
  let loose_document = Edit_graph.add_node ~inputs:[|None|] loose_null document
      |> get in
  check (Edit_graph.inputs loose_document ~node_id:(Node.id loose_null)
      = Some [|None|])
    "factory could not create a node with a disconnected input";
  let match_size_factory = Edit_graph.factory_slots ~key:"match_size"
      ~slots:["input"; "target"]
      ~label:"Match Size" ~category:["Modify"]
      ~inputs:[Edit_graph.Required; Optional] (function
        | [Some input; None] -> Lisp_sop.node ~with_:["input", input] {|(sop/match_size (sop/ext_input))|}
        | [Some input; Some target] ->
          Lisp_sop.node ~with_:["input", input; "target", target]
            {|(sop/match_size (sop/ext_input) (sop/ext_target))|}
        | _ -> invalid_arg "Match Size requires its geometry input") in
  check (Edit_graph.factory_slot_names match_size_factory = ["input"; "target"])
    "optional factory did not retain named input slots";
  List.iter (fun slots ->
    match Edit_graph.factory ~slots ~key:"invalid" ~label:"Invalid"
        ~category:["Test"] ~arity:1 (fun _ -> source) with
    | _ -> fail "factory accepted missing or blank slot names"
    | exception Invalid_argument _ -> ()) [[]; [""]; ["a"; "a"]];
  let loose_match = Edit_graph.instantiate_optional match_size_factory
      [None; None] |> get in
  let optional_document = Edit_graph.add_node ~factory:match_size_factory
      ~inputs:[|None; None|] loose_match document |> get
    |> Edit_graph.connect ~source:(Node.id source)
         ~consumer:(Node.id loose_match) ~input_index:0 |> get in
  let without_target = Edit_graph.compile_node optional_document
      ~node_id:(Node.id loose_match) |> get in
  check (Edit_graph.node_slot_names optional_document ~node_id:(Node.id loose_match)
      = Some ["input"; "target"])
    "editable entry lost its factory's named slots";
  check (List.length (Node.inputs without_target) = 1
      && Edit_graph.factory_inputs match_size_factory
         = [Edit_graph.Required; Optional])
    "optional factory slot was treated as a disconnected required input";
  let optional_document = Edit_graph.connect ~source:(Node.id middle)
      ~consumer:(Node.id loose_match) ~input_index:1 optional_document |> get in
  let with_target = Edit_graph.compile_node optional_document
      ~node_id:(Node.id loose_match) |> get in
  check (Node.id with_target = Node.id loose_match
      && List.length (Node.inputs with_target) = 2)
    "connecting an optional slot did not rebuild the physical SOP inputs";
  let optional_document = Edit_graph.disconnect ~consumer:(Node.id loose_match)
      ~input_index:1 optional_document |> get in
  let without_target_again = Edit_graph.compile_node optional_document
      ~node_id:(Node.id loose_match) |> get in
  check (Node.id without_target_again = Node.id loose_match
      && List.length (Node.inputs without_target_again) = 1)
    "disconnecting an optional slot did not restore the unary SOP";
  (* a rest slot grows by connecting one past its last input *)
  let rest_factory = Edit_graph.factory_slots ~key:"merge" ~label:"Merge"
      ~slots:["input"] ~category:["Test"] ~inputs:[Edit_graph.Rest]
      (fun inputs -> merge_of (List.filter_map Fun.id inputs)) in
  check (Result.is_error (Edit_graph.instantiate_optional rest_factory [])
      && Result.is_ok (Edit_graph.instantiate_optional rest_factory [Some source; None; Some middle])
      && (try ignore (Edit_graph.factory_slots ~key:"bad" ~label:"Bad" ~category:["T"]
            ~inputs:[Edit_graph.Rest; Required] (fun _ -> source)); false
          with Invalid_argument _ -> true))
    "rest slot arity or position was not validated";
  let rest_node = Edit_graph.instantiate_optional rest_factory [Some source] |> get in
  let rest_document = Edit_graph.add_node ~factory:rest_factory ~inputs:[|None|]
      rest_node document |> get in
  let grow index source_node d = Edit_graph.connect ~source:(Node.id source_node)
      ~consumer:(Node.id rest_node) ~input_index:index d in
  let rest_document = grow 0 source rest_document |> get |> grow 1 middle |> get
    |> grow 2 source |> get in
  check (Edit_graph.node_slot_names rest_document ~node_id:(Node.id rest_node)
      = Some ["input"; "input_2"; "input_3"]
      && Result.is_error (grow 4 source rest_document)
      && List.length (Node.inputs (Edit_graph.compile_node rest_document
           ~node_id:(Node.id rest_node) |> get)) = 3)
    "rest slot did not grow, name its extras or rebuild through the factory";
  let stored_rest = Edit_graph.find rest_document ~node_id:(Node.id rest_node) |> Option.get in
  check (Edit_graph.compile_node rest_document ~node_id:(Node.id rest_node) |> get == stored_rest)
    "compiling unchanged connected rest inputs rebuilt the stored node";
  let rest_document = Edit_graph.disconnect ~consumer:(Node.id rest_node)
      ~input_index:1 rest_document |> get in
  check (List.length (Node.inputs (Edit_graph.compile_node rest_document
      ~node_id:(Node.id rest_node) |> get)) = 2)
    "a disconnected rest input was not skipped";
  let optional_rest_factory = Edit_graph.factory_slots ~key:"optional_rest" ~label:"Optional rest"
    ~slots:["base";"fixed";"extras"] ~category:["Test"]
    ~inputs:[Required;Optional;Optional_rest]
    (fun inputs -> merge_of (List.filter_map Fun.id inputs)) in
  check (Result.is_error (Edit_graph.instantiate_optional optional_rest_factory [Some source])
    && Result.is_ok (Edit_graph.instantiate_optional optional_rest_factory [Some source;None])
    && Result.is_ok (Edit_graph.instantiate_optional optional_rest_factory [Some source;None;None]))
    "optional rest validates the fixed prefix and permits zero extras";
  let optional_rest_node=Edit_graph.instantiate_optional optional_rest_factory [Some source;None] |> get in
  let optional_rest_doc=Edit_graph.add_node ~factory:optional_rest_factory ~inputs:[|Some (Node.id source);None|]
    optional_rest_node document |> get in
  let optional_rest_doc=Edit_graph.connect ~source:(Node.id middle) ~consumer:(Node.id optional_rest_node)
    ~input_index:2 optional_rest_doc |> get in
  let optional_rest_doc=Edit_graph.connect ~source:(Node.id source) ~consumer:(Node.id optional_rest_node)
    ~input_index:3 optional_rest_doc |> get in
  check (Edit_graph.node_slot_names optional_rest_doc ~node_id:(Node.id optional_rest_node)
      = Some ["base";"fixed";"extras";"extras_2"]
    && List.length (Node.inputs (Edit_graph.compile_node optional_rest_doc
      ~node_id:(Node.id optional_rest_node) |> get))=3)
    "optional rest grows after a disconnected fixed slot";
  let optional_rest_doc=Edit_graph.disconnect ~consumer:(Node.id optional_rest_node)
    ~input_index:2 optional_rest_doc |> get
    |> Edit_graph.disconnect ~consumer:(Node.id optional_rest_node) ~input_index:3 |> get in
  let compiled_optional_rest=Edit_graph.compile_node optional_rest_doc ~node_id:(Node.id optional_rest_node) |> get in
  check (Node.id compiled_optional_rest=Node.id optional_rest_node && List.length (Node.inputs compiled_optional_rest)=1)
    "optional rest disconnect preserves identity and the fixed prefix";
  print_endline "editable graph tests passed"
