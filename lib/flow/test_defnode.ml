open Flow

let catalog = match Check.catalog_of_manifest (In_channel.with_open_bin "../flow_sop/flow_manifest.sexp" In_channel.input_all) with
  | Ok (c,_) -> c | Error d -> failwith (Diagnostic.to_string d)
let parse text = Result.get_ok (Syntax.parse text)
let source node call = "(workspace w "^node^" (graph g :context sop "^call^"))"
let good text = match Workspace.check catalog (parse text) with
  | Some w, _ -> w
  | _, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
let bad code text = match Workspace.check catalog (parse text) with
  | None,ds when List.exists (fun (d:Diagnostic.t) -> d.code=code) ds -> ()
  | _,ds -> failwith ("Expected "^code^": "^String.concat "\n" (List.map Diagnostic.to_string ds))
let node ps = "(defnode sop/test_node :label \"Test\" :category [\"Deform\" \"Test\"] "^ps^" geo)"

let () =
  let context_seed=Option.get (Op.find "length" Context.value) in
  let context_seed={context_seed with Op.name="sop/context_seed";
    signature={pos=["namespace",Ty.Text];opt=[];rest=None;kw=[]};
    out=(fun _->Ty.Int);any_num=false;arithmetic=None;packed_extension=None} in
  let context_check text=Workspace.check ~ops:[context_seed] catalog (parse text) in
  let context_bad code text=match context_check text with
    |None,ds when List.exists (fun (d:Diagnostic.t)->d.code=code) ds->()
    |_,ds->failwith ("Expected "^code^": "^String.concat "\n" (List.map Diagnostic.to_string ds)) in
  context_bad "E_NODE_CONTEXT" "(workspace w (graph g :context value (sop/context_seed \"fixture\")))";
  List.iter (fun declaration -> context_bad "E_NODE_CONTEXT" (source declaration "nil")) [
    "(defnode sop/context_fixture :validate (= (sop/context_seed \"fixture\") 0) [(geo : geometry)] geo)";
    "(defn seeded :context value [] (sop/context_seed \"fixture\"))\n
      (defnode sop/context_fixture :validate (= (seeded) 0) [(geo : geometry)] geo)" ];
  List.iter (fun namespace->context_bad "E_SEED_NAMESPACE" (source
    ("(defnode sop/context_fixture [(geo : geometry) (namespace : text \"fixture\")]\n"
     ^"(let* [seed (sop/context_seed "^namespace^")] geo))") "(sop/context_fixture nil)"))
    ["namespace";"\" \""];
  (match context_check (source "(defnode sop/context_fixture [(geo : geometry)]\n
    (let* [seed (sop/context_seed \"fixture\")] geo))" "(sop/context_fixture nil)") with
   |Some _,_->()|_,ds->failwith (String.concat "\n" (List.map Diagnostic.to_string ds)));
  let w=good (source "(defnode sop/test_node :label \"Test\" :category [\"Deform\" \"Test\"] :description \"A checked body\"
    :facts {:elementwise points :reads [\"P\" \"N\"] :writes [\"P\"] :topology preserved :exact true}
    :validate (> count 0) :code \"E_COUNT\" :message \"Count must be positive\"
    [(geo : geometry :required false)
     (distance : float 0.1 :label \"Distance\" :description \"Displacement\" :folder [\"Shape\"]
       :min -10 :max 10 :hard_min -20 :hard_max 20 :primary true :unit \"m\" :impact cook
       :validate (< distance 20) :code \"E_DISTANCE\" :message \"Distance is too large\")
     (count : int 2 :min 1 :max 16 :hard-min 1 :hard-max 100)
     (active : bool true :kind toggle)
     (name : text \"abc\" :kind text)
     (ramp : text \"0:0,1:1\" :kind ramp)
     (mode : text \"First\" :kind choice :choices [\"First\" \"Second\"])
     (scale : vec3 [1 2 3] :min 0 :max 10)] geo)" "(sop/test_node nil :distance 2 :count 3)") in
  let d=List.hd w.nodes in
  assert (d.label="Test" && d.category=["Deform";"Test"] && d.description=Some "A checked body");
  assert (List.length d.fields=9 && List.length d.validations=2);
  let distance=List.hd d.fields in
  assert (distance.label="Distance" && distance.description=Some "Displacement" && distance.folder=["Shape"]
    && distance.primary && distance.unit=Some "m" && distance.impact=Param.Cook);
  assert ((Option.get d.kind.facts).preserves_topology);
  assert ((List.hd d.kind.slots).ty=None && not (List.hd d.kind.slots).required);
  (match (List.hd w.graphs).body.node with Workspace.Call {kind="sop/test_node";_}->() | _->assert false);
  ignore (good (source "(defnode sop/test_node [(geo : geometry :rest true)] (sop/merge geo))" "(sop/test_node nil nil)"));
  let grouped=good (source "(defnode sop/test_node [(geo : geometry)
    (scale_x : float 1 :vec3 [\"scale\" 0]) (scale_y : float 2 :vec3 [\"scale\" 1])
    (scale_z : float 3 :vec3 [\"scale\" 2])] geo)" "(sop/test_node nil :scale [4 5 6])") in
  assert (List.map (fun (p:Check.parameter) -> p.name) (List.hd grouped.nodes).kind.parameters=["scale"]);
  ignore (good (source "(defnode sop/typed_slots [(geo : geometry) (image : image) (field : (fn [vec3] float))] geo)"
    "(sop/typed_slots nil (image/noise :width 1 :height 1) (fn [(p : vec3)] p.x))"));
  let keyword_slots=good (source "(defnode sop/keyword_slots [(geo : geometry)
    (image : image :keyword true) (field : (fn [vec3] float) :keyword true)] geo)"
    "(sop/keyword_slots nil :image (image/noise :width 1 :height 1) :field (fn [(p : vec3)] p.x))") in
  let keyword_slots=List.hd keyword_slots.nodes in
  assert (List.length keyword_slots.slots=3 && List.length keyword_slots.kind.slots=1);
  assert (keyword_slots.keyword_inputs=["image";"field"]);
  assert (List.for_all (fun (p:Check.parameter)->p.fields=[] && p.primary) keyword_slots.kind.parameters);
  let typed_map=good (source "(defnode sop/typed_map [(geo : geometry) (field : (fn [vec3] vec3))]
    (let* [mapped (map field (array/vec3 1))] geo))" "nil") in
  (match (List.hd typed_map.nodes).body.body.node with
   | Workspace.Let ([_, values], _) -> assert (values.ty=Ty.Array Ty.Vec3)
   | _ -> assert false);
  List.iter (fun call -> ignore (good (source
    "(defnode sop/rest_fields [(geo : geometry) (fields : (fn [vec3] vec3) :rest true)] geo)" call))) [
    "(let* [field (fn [p] (+ p [1 0 0]))] (sop/rest_fields nil field field))";
    "(sop/rest_fields nil (list (fn [p] (+ p [1 0 0])) (fn [p] (* p 2))))" ];
  List.iter (fun (call,code)->bad code (source
    ("(defnode sop/typed_call [(geo : geometry) (field : (fn [vec3] vec3))]\n"
     ^"(let* [value "^call^"] geo))") "nil"))
    ["(field [0 0 0] [1 1 1])","E_ARITY";"(field \"bad\")","E_TYPE"];
  List.iter (fun ps -> bad "E_SHAPE" (source (node ps) "(sop/test_node nil)")) [
    "[(geo : geometry) (x : float 0 :unknown 1)]";
    "[(geo : geometry) (x : float 0 :min 1 :min 2)]";
    "[(geo : geometry) (x : float 0 :min)]";
    "[(geo : geometry) (x : not_a_type 0)]";
    "[(geo : geometry) (x : float)] bad";
    "[(geo : geometry) (geo : float 0)]";
    "[(geo : geometry) (x : float 0 :label false)]";
    "[(geo : geometry) (x : float 0 :description 1)]";
    "[(geo : geometry) (x : float 0 :folder [1])]";
    "[(geo : geometry) (x : float 0 :primary 1)]";
    "[(geo : geometry) (x : float 0 :unit true)]";
    "[(geo : geometry) (x : float 0 :impact invalid)]";
    "[(geo : geometry) (x : float 0 :vec3 [\"a\" 3])]";
    "[(geo : geometry) (x : float 0 :vec3 [\"a\" 0])]";
    "[(geo : geometry) (x : float 0 :kind bool)]";
    "[(geo : geometry) (x : float 0 :min 2 :max 1)]";
    "[(geo : geometry) (x : float 0 :hard_min 2 :hard_max 1)]";
    "[(geo : geometry) (x : float 0 :hard_min 1)]";
    "[(geo : geometry) (x : int 0.5)]";
    "[(geo : geometry) (x : bool 0)]";
    "[(geo : geometry) (x : text 0)]";
    "[(geo : geometry) (x : text \"A\" :choices [])]";
    "[(geo : geometry) (x : text \"A\" :choices [\"A\" \"A\"])]";
    "[(geo : geometry) (x : text \"B\" :choices [\"A\"])]";
    "[(geo : geometry) (x : float 0 :choices [\"A\"])]";
    "[(geo : geometry) (x : vec3 [1 2])]";
    "[(geo : geometry 0)]";
    "[(geo : geometry :rest true) (other : geometry)]";
    "[(geo : geometry :required 0)]";
    "[(geo : geometry) (x : float 0 :rest true)]"
    ; "[(geo : geometry :keyword true)]"
    ; "[(geo : geometry) (image : image :keyword true :required false)]"
    ; "[(geo : geometry) (field : (fn [vec3] float) :keyword true :rest true)]"
    ; "[(geo : geometry) (field : (fn [vec3] float) :keyword 1)]"
    ; "[(geo : geometry) (x : float 0 :keyword true)]"
  ];
  List.iter (fun definition -> bad "E_SHAPE" (source definition "nil")) [
    "(defnode sop/test_node)";
    "(defnode sop/test_node [] nil nil)";
    "(defnode sop/test_node :facts [] [] nil)";
    "(defnode sop/test_node :facts {:elementwise vertices} [] nil)";
    "(defnode sop/test_node :facts {:reads [1]} [] nil)";
    "(defnode sop/test_node :facts {:topology false} [] nil)";
    "(defnode sop/test_node :facts {:exact 1} [] nil)";
    "(defnode sop/test_node :facts {:other 1} [] nil)";
    "(defnode sop/test_node :category [1] [] nil)"
  ];
  bad "E_NAME" (source "(defnode sop/box [] nil)" "nil");
  bad "E_NAME" (source "(defnode test_node [] nil)" "nil");
  bad "E_TYPE" (source "(defnode sop/test_node [] 1)" "nil");
  bad "E_TYPE" (source "(defnode sop/test_node :validate 1 [] nil)" "nil");
  bad "E_RECURSION" (source "(defnode sop/test_node [(geo : geometry)] (sop/test_node geo))" "nil");
  bad "E_RECURSION" (source "(defnode sop/test_node [(geo : geometry)] (sop/other_node geo))
    (defnode sop/other_node [(geo : geometry)] (sop/test_node geo))" "nil");
  bad "E_TYPE" (source (node "[(geo : geometry)]") "(sop/test_node 1)");
  bad "E_HARD_RANGE" (source (node "[(geo : geometry) (x : float 0 :hard_min -1 :hard_max 1)]") "(sop/test_node nil :x 2)");
  bad "E_UNKNOWN_PARAM" (source (node "[(geo : geometry)]") "(sop/test_node nil :bad 1)");
  Printf.printf "Defnode: typed slots, metadata, validations, identity and malformed declarations pass\n"

let () =
  let declaration metadata = "(defnode sop/versioned " ^ metadata ^ " [(geo : geometry)] geo)" in
  List.iter (fun (metadata,expected) ->
    let form=List.hd(parse(declaration metadata)) in
    let parsed=Result.get_ok(Defnode.parse form) in
    let checked=good(source(declaration metadata)"(sop/versioned nil)") in
    assert(parsed.version=expected && (List.hd checked.nodes).version=expected);
    let restored=good(Lisp.flat(List.hd checked.source)) in
    assert((List.hd restored.nodes).version=expected))
    ["",1;":version 0",0;":version 2",2;":version " ^ string_of_int max_int,max_int];
  List.iter(fun metadata->bad "E_SHAPE"(source(declaration metadata)"(sop/versioned nil)"))
    [":version -1";":version 2.5";":version 2.0";":version true";":version \"2\"";
     ":version 9223372036854775808";":version 2 :version 3"];
  print_endline "Defnode version: exact nonnegative integer metadata, default, source and malformed declarations"
