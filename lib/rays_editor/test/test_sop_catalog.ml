open Rays
open Sop
open Test_support

let registered_factories = lazy (
  let native = Sop_catalog.Editor.factories in
  let catalog = Flow_sop.Catalog.of_factories ~version:Flow_sop.Manifest.version native
    |> Result.get_ok in
  let source = Flow.Syntax.parse "(workspace registered_nodes)" |> Result.get_ok in
  (* An explicit library keeps every definition; the default prunes to what the forms reach. *)
  let workspace = match Flow_sop.Catalog.check_workspace ~library:true
      ~sources:(Flow_sop.Catalog.sources () |> Result.get_ok)
      ~ops:Flow_sop.Operators.all catalog source with
    | Some workspace, [] -> workspace
    | _, diagnostics -> fail (String.concat "\n" (List.map Diagnostic.to_string diagnostics)) in
  let lowered = Flow_sop.Lower.of_checked ~factories:native workspace |> Result.get_ok in
  native @ lowered.factories)

let check_metadata factory fields =
  let key = Edit_graph.factory_key factory in
  let names = Edit_graph.factory_slot_names factory in
  check (List.length names = Edit_graph.factory_arity factory
      && List.length (List.sort_uniq String.compare names) = List.length names)
    (key ^ " has invalid geometry port names");
  List.iter (fun name -> check
      (not (List.exists (fun (field : Param.field_view) ->
         field.name = name || Option.fold ~none:false
           ~some:(fun (group, _) -> group = name) field.vec3) fields))
      (key ^ " has a geometry port shadowing a parameter port")) names;
  if List.length names > 1 then
    check (names <> List.mapi (fun index _ -> "in" ^ string_of_int index) names)
      (key ^ " still has unnamed multiple inputs");
  let floating (field : Param.field_view) = match field.kind with
    | Param.Floating_view _ -> true | _ -> false in
  List.fold_left (fun count (x : Param.field_view) ->
    if not (floating x) || not (Filename.check_suffix x.name "_x") then count
    else
      let prefix = String.sub x.name 0 (String.length x.name - 2) in
      let find suffix = List.find_opt (fun (field : Param.field_view) ->
        field.name = prefix ^ suffix && floating field) fields in
      match find "_y", find "_z" with
      | Some y, Some z ->
          let group = if List.exists (fun (field : Param.field_view) ->
              field.name = prefix) fields then prefix ^ "_vector" else prefix in
          List.iteri (fun component (field : Param.field_view) ->
            check (field.vec3 = Some (group, component) && field.folder = x.folder)
              (key ^ "." ^ field.name ^ " lost its vector port metadata")) [x; y; z];
          count + 1
      | _ -> count) 0 fields
let edit_parameters graph ~node_id changes =
  Result.bind (Edit_graph.apply_parameters (Edit_graph.of_graph graph)
      ~node_id changes) (fun (document, effects) ->
    Result.map (fun graph -> graph, effects) (Edit_graph.compile document))

let cook session node = match Session.cook session
    ~context:(Context.create () |> Result.get_ok) node with
  | Ok output -> (Result.get_ok (Sop.Payload.geometry output.Session.payload))
  | Error error -> fail (Diagnostic.error_to_string error)

let gallery_grid_warning () =
  let catalog = Rays_editor.workspace_catalog () |> Result.get_ok in
  let text = In_channel.with_open_bin "../../../sketches/sop_gallery.rays" In_channel.input_all in
  let _, warnings = Editor_document.Workspace_doc.check_text catalog text |> Result.get_ok in
  check (List.map (fun (d : Diagnostic.t) -> d.code, d.severity) warnings
    = ["W_SOFT_RANGE", Diagnostic.Warning; "W_SOFT_RANGE", Diagnostic.Warning])
    "gallery soft-range warnings changed or became errors";
  let node = Lisp_sop.node "(sop/grid :columns 100 :rows 100 :size 10 :width 10 :height 10)" in
  List.iter (fun name ->
    let field = List.find (fun (field : Param.field_view) -> field.name = name)
        (Node.parameter_fields node) in
    check (field.current = Param.Int_value 100) "a soft grid range clamped the gallery")
    ["columns"; "rows"];
  let session = Session.create ~max_entries:8 ~max_payload_bytes:16_777_216 |> Result.get_ok in
  Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
    let geometry = cook session node in
    check (Rdk.Geometry.point_count geometry = 10_201 && Rdk.Geometry.primitive_count geometry = 20_000)
      "the gallery's 100x100 grid did not cook at its authored resolution")

let catalog_node key inputs changes =
  let factory = List.find (fun factory -> Edit_graph.factory_key factory = key)
      (Lazy.force registered_factories) in
  match Result.bind (Edit_graph.instantiate_optional factory inputs)
      (fun node -> Node.apply_parameters node changes) with
  | Ok (node, _) -> node
  | Error message -> fail message

let float3 name geometry =
  match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point name geometry with
  | Some attribute ->
      (match Rdk.Attribute.Private.storage attribute with
       | Rdk.Attribute.Float3 values -> Rdk.Packed.Float3.Private.view values
       | _ -> fail (name ^ " has wrong storage"))
  | None -> fail ("missing " ^ name)

(* Blend Shapes, Attribute Composite, Skin and Group Ordered as nodes: the
   sketches/ws_morph workspace lowers and each graph cooks to the geometry its
   node promises, the same at one and four domains. *)
let test_morph_sketch () =
  let text = In_channel.with_open_bin "../../../sketches/ws_morph.rays"
      In_channel.input_all in
  let lowered = match Flow.Syntax.parse text with
    | Error d -> fail (Diagnostic.to_string d)
    | Ok forms ->
        match Flow_sop.Lower.workspace
            ~extra:Editor_document.Contexts.descriptors
            ~factories:Sop_catalog.Editor.factories forms with
        | Ok lowered -> lowered
        | Error d -> fail (Diagnostic.to_string d) in
  let cook_graph ~domains name =
    let graph = List.find (fun (g : Flow_sop.Lower.graph) -> g.name = name)
        lowered.graphs in
    let node = Edit_graph.compile_node graph.network.geometry
        ~node_id:(Option.get graph.root) |> Result.get_ok in
    let session = Session.create ~max_entries:64
        ~max_payload_bytes:64_000_000 |> Result.get_ok in
    let context = Context.create ~domains ~grain:97 () |> Result.get_ok in
    let geometry = match Session.cook session ~context node with
      | Ok output -> (Result.get_ok (Sop.Payload.geometry output.Session.payload))
      | Error error -> fail (name ^ ": " ^ Diagnostic.error_to_string error) in
    Session.close session;
    geometry in
  let positions geometry =
    Rdk.Packed.Float3.Private.view (Rdk.Geometry.positions geometry) in
  let both name =
    let one = cook_graph ~domains:1 name and four = cook_graph ~domains:4 name in
    check (positions one = positions four
        && Rdk.Topology.Private.view (Rdk.Geometry.topology one)
           = Rdk.Topology.Private.view (Rdk.Geometry.topology four))
      (name ^ " differs between one and four domains");
    one in
  (* sheet: 28x28 quads keep their topology; the blend lifts the flat sheet
     part of the way to its two shapes. *)
  let sheet = both "sheet" in
  let p = positions sheet in
  let low = Array.fold_left Float.min infinity p.y
  and high = Array.fold_left Float.max neg_infinity p.y in
  check (Rdk.Geometry.point_count sheet = 29 * 29
      && Rdk.Geometry.primitive_count sheet = 28 * 28)
    "Blend Shapes changed the sheet's topology";
  check (Array.for_all Float.is_finite p.x && Array.for_all Float.is_finite p.y
      && high -. low > 0.2 && high -. low < 3.)
    (Printf.sprintf "Blend Shapes left the sheet flat or blew it up (y %g..%g)"
      low high);
  (* tinted: the mean of #2f6fb5 at weight 1 and #f2a03d at weight 0.35. *)
  let tinted = both "tinted" in
  let cd = match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "Cd"
      tinted |> Option.map Rdk.Attribute.Private.storage with
    | Some (Rdk.Attribute.Float4 values) -> Rdk.Packed.Float4.Private.view values
    | _ -> fail "Attribute Composite dropped or retyped Cd" in
  let cool = Rays_math.Color.hex_exn "#2f6fb5" and warm = Rays_math.Color.hex_exn "#f2a03d" in
  let mean a b = ((float a +. (0.35 *. float b)) /. 255.) /. 1.35 in
  check (Array.length cd.x = 29 * 29
      && Array.for_all (fun r -> Float.abs (r -. mean cool.r warm.r) < 1e-6) cd.x
      && Array.for_all (fun g -> Float.abs (g -. mean cool.g warm.g) < 1e-6) cd.y
      && Array.for_all (fun b -> Float.abs (b -. mean cool.b warm.b) < 1e-6) cd.z)
    (Printf.sprintf "Attribute Composite Cd is %g %g %g, expected %g %g %g"
      cd.x.(0) cd.y.(0) cd.z.(0) (mean cool.r warm.r) (mean cool.g warm.g)
      (mean cool.b warm.b));
  check (positions tinted = positions sheet)
    "Attribute Composite moved points although P was not allowed";
  (* vase: six 32-point rings skin into five bands of 32 quads. *)
  let vase = both "vase" in
  let skin = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "skin" vase in
  let members group = let count = ref 0 in
    for index = 0 to Rdk.Group.length group - 1 do
      if Rdk.Group.mem index group then incr count done; !count in
  check (Rdk.Geometry.point_count vase = 6 * 32
      && Rdk.Geometry.primitive_count vase = 5 * 32
      && Option.map members skin = Some (5 * 32))
    (Printf.sprintf "Skin made %d primitives over %d points"
      (Rdk.Geometry.primitive_count vase) (Rdk.Geometry.point_count vase));
  let index = Rdk.Topology_index.create (Rdk.Geometry.topology vase) in
  check (Rdk.Topology_index.non_manifold_edge_count index = 0)
    "Skin wall has non-manifold edges";
  check (Option.bind (Rdk.Geometry.find_group ~owner:Rdk.Group.Point "rim" vase)
           Rdk.Group.ordered_elements = Some [|0; 8; 16; 24|])
    "Group Ordered lost its authored order";
  (* an unwired Blend Shapes passes its input through *)
  let session = Session.create ~max_entries:8 ~max_payload_bytes:1_000_000
      |> Result.get_ok in
  let grid = Lisp_sop.node {|(sop/grid :width_mode "Auto" :height_mode "Auto" :rows 2 :columns 2)|} in
  let idle = catalog_node "blend_shapes" [Some grid; None; None; None; None] [] in
  check (Node.operation idle = "blend_shapes"
      && positions (cook session idle) = positions (cook session grid))
    "unwired Blend Shapes is not a passthrough";
  Session.close session

(* The inputs of the Blend Shapes, Attribute Composite and Skin nodes that the
   ws_morph sketch leaves alone: masks, point IDs, alpha and the rest guide.
   Each cooks through its catalog factory, the same at one and four domains. *)
let test_node_inputs () =
  let add owner name storage geometry =
    Rdk.Attribute.create_owned ~owner ~name storage |> Result.get_ok
    |> fun attribute -> Rdk.Geometry.with_attribute attribute geometry
    |> Result.get_ok in
  let line xs = Rdk.Line_geometry.points (Array.map (fun x -> x, 0., 0.) xs) in
  let point = Rdk.Attribute.Point in
  let xs geometry =
    (Rdk.Packed.Float3.Private.view (Rdk.Geometry.positions geometry)).x in
  let cook_at domains node =
    let session = Session.create ~max_entries:16 ~max_payload_bytes:8_000_000
        |> Result.get_ok in
    let context = Context.create ~domains ~grain:1 () |> Result.get_ok in
    let geometry = match Session.cook session ~context node with
      | Ok output -> (Result.get_ok (Sop.Payload.geometry output.Session.payload))
      | Error error -> fail (Diagnostic.error_to_string error) in
    Session.close session;
    geometry in
  let both message node =
    let one = cook_at 1 node and four = cook_at 4 node in
    check (xs one = xs four
        && Rdk.Topology.Private.view (Rdk.Geometry.topology one)
           = Rdk.Topology.Private.view (Rdk.Geometry.topology four)
        && List.for_all2 (fun left right ->
             Rdk.Attribute.storage left = Rdk.Attribute.storage right)
             (Rdk.Geometry.attributes one) (Rdk.Geometry.attributes four))
      (message ^ " differs between one and four domains");
    one in
  let near left right = Array.for_all2 (fun a b -> Float.abs (a -. b) < 1e-12)
      left right in
  (* Blend Shapes masks: the first input's mask, then the shape's own *)
  let source = line [|0.; 1.; 2.|] |> add point "mask" (Rdk.Attribute.Float [|0.; 0.5; 1.|])
  and target = line [|10.; 11.; 12.|]
      |> add point "mask" (Rdk.Attribute.Float [|1.; 0.5; 0.|]) in
  let blend ?(inputs = [Some (Lisp_sop.snapshot (source)); Some (Lisp_sop.snapshot (target));
                         None; None; None]) changes =
    catalog_node "blend_shapes" inputs changes in
  let scaled source_name = both "masked Blend Shapes" (blend [
      "weight1", Param.Float_value 0.5;
      "masking", Param.Choice_value "Scale from attribute";
      "mask_source", Param.Choice_value source_name]) in
  check (near (xs (scaled "First input")) [|0.; 3.5; 7.|])
    "Blend Shapes did not scale by the first input's mask";
  check (near (xs (scaled "Shape")) [|5.; 3.5; 2.|])
    "Blend Shapes did not scale by the shape's mask";
  let set = both "set-mask Blend Shapes" (blend [
      "weight1", Param.Float_value 0.125;
      "masking", Param.Choice_value "Set from attribute"]) in
  check (near (xs set) [|0.; 6.; 12.|])
    "Blend Shapes did not take its weight from the mask";
  (* Blend Shapes point IDs: the shape lists the same points in another order *)
  let ordered = line [|0.; 1.; 2.|] |> add point "id" (Rdk.Attribute.Int [|10; 20; 30|])
  and shuffled = line [|12.; 10.; 11.|]
      |> add point "id" (Rdk.Attribute.Int [|30; 10; 20|]) in
  let inputs = [Some (Lisp_sop.snapshot (ordered)); Some (Lisp_sop.snapshot (shuffled));
                None; None; None] in
  let by_id = both "ID-matched Blend Shapes" (blend ~inputs [
      "point_id_attribute", Param.Text_value "id";
      "attributes", Param.Text_value "P"])
  and by_index = both "index-matched Blend Shapes" (blend ~inputs [
      "attributes", Param.Text_value "P"]) in
  check (near (xs by_id) [|10.; 11.; 12.|] && near (xs by_index) [|12.; 10.; 11.|])
    "Blend Shapes point ID attribute did not match points by ID";
  (* Attribute Composite alpha: each input's alpha weighs its values *)
  let layer positions values alpha = line positions
      |> add point "value" (Rdk.Attribute.Float values)
      |> add point "alpha" (Rdk.Attribute.Float alpha) in
  let composite = both "alpha Attribute Composite" (catalog_node "attribute_composite"
      [Some (Lisp_sop.snapshot ((layer [|0.; 1.; 2.|] [|2.; 4.; 6.|] [|1.; 1.; 0.|])));
       Some (Lisp_sop.snapshot ((layer [|10.; 11.; 12.|] [|10.; 20.; 30.|] [|1.; 0.; 1.|])));
       None; None; None] [
      "point_attributes", Param.Text_value "P value";
      "allow_position", Param.Bool_value true;
      "alpha_attribute", Param.Text_value "alpha"]) in
  let values = match Rdk.Geometry.find_attribute ~owner:point "value" composite
      |> Option.map Rdk.Attribute.storage with
    | Some (Rdk.Attribute.Float values) -> values
    | _ -> fail "Attribute Composite dropped or retyped value" in
  check (near (xs composite) [|5.; 1.; 12.|] && near values [|6.; 4.; 30.|])
    "Attribute Composite did not weigh its inputs by alpha";
  (* Skin rest guide: the second ring's points are turned a quarter; pairing
     on the rest positions gives the wall of the unturned rings *)
  let ring_points turn = Array.init 8 (fun index ->
      let section = index / 4 and local = index mod 4 in
      let angle = Float.pi *. 0.5 *. float (local + (if section = 1 then turn else 0)) in
      cos angle, float section, sin angle) in
  let rings turn =
    let points = ring_points turn in
    let topology = Rdk.Topology.create_owned ~point_count:8
        ~vertex_points:(Array.init 8 Fun.id) ~primitive_offsets:[|0; 4; 8|]
        ~primitive_kinds:(Array.make 2 Rdk.Topology.Closed_polyline) |> Result.get_ok in
    Rdk.Geometry.create
      ~positions:(Rdk.Packed.Float3.Private.of_owned_exn
        ~x:(Array.map (fun (x, _, _) -> x) points)
        ~y:(Array.map (fun (_, y, _) -> y) points)
        ~z:(Array.map (fun (_, _, z) -> z) points))
      ~topology () |> Result.get_ok in
  let skin inputs = both "Skin" (catalog_node "skin" inputs []) in
  let corners geometry =
    (Rdk.Topology.Private.view (Rdk.Geometry.topology geometry)).vertex_points in
  let straight = skin [Some (Lisp_sop.snapshot ((rings 0))); None]
  and turned = skin [Some (Lisp_sop.snapshot ((rings 1))); None]
  and guided = skin [Some (Lisp_sop.snapshot ((rings 1))); Some (Lisp_sop.snapshot ((rings 0)))] in
  check (Rdk.Geometry.primitive_count guided = 4
      && corners guided = corners straight && corners guided <> corners turned
      && xs guided = xs turned)
    "Skin did not pair its sections on the rest input's positions"

(* Catalog-owned motion operators: Rest Position and Point Velocity. *)
let test_motion () =
  let session = Session.create ~max_entries:32 ~max_payload_bytes:16_000_000
      |> Result.get_ok in
  let previous = Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0.,0.,0.); (1.,1.,1.)|])
  and current = Lisp_sop.snapshot (Rdk.Line_geometry.points [|(1.,2.,3.); (3.,5.,7.)|]) in
  let rested = catalog_node "rest_position" [Some current; None] [] in
  let graph = catalog_node "point_velocity" [Some rested; Some previous; None]
      Param.["dt", Float_value 0.5; "add_x", Float_value 1.;
                 "add_z", Float_value (-1.)] in
  let output = cook session graph in
  let rest = float3 "rest" output and velocity = float3 "v" output in
  check (rest.x = [|1.;3.|] && rest.y = [|2.;5.|] && rest.z = [|3.;7.|])
    "Rest Position SOP output";
  check (velocity.x = [|3.;5.|] && velocity.y = [|4.;8.|]
      && velocity.z = [|5.;11.|]) "Point Velocity SOP output";
  let invalid = catalog_node "point_velocity" [Some current; Some previous; None]
      Param.["group", Text_value "missing"] in
  (match Session.cook session ~context:(Context.create () |> Result.get_ok)
      invalid with
   | Error error -> check (error.Diagnostic.code = "missing_group")
       ("unexpected diagnostic " ^ error.code)
   | Ok _ -> fail "Point Velocity SOP accepted a missing group");
  Session.close session

let set_color_test () =
  let session = Session.create ~max_entries:8 ~max_payload_bytes:1_000_000
    |> Result.get_ok in
  let box = catalog_node "box" [] [] in
  let colors node =
    match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "Cd"
        (cook session node) with
    | Some attribute -> (match Rdk.Attribute.Private.storage attribute with
        | Rdk.Attribute.Float4 values -> Rdk.Packed.Float4.Private.view values
        | _ -> fail "set_color did not write a float4 Cd")
    | None -> fail "set_color wrote no Cd" in
  let plain = colors (catalog_node "set_color" [Some box] []) in
  check (Array.for_all (( = ) 1.) plain.x && Array.for_all (( = ) 1.) plain.w)
    "set_color defaults are not opaque white";
  let grouped = Lisp_sop.node ~with_:["box", (box)] {|(sop/group_range (sop/ext_box) :name "g" :end_ 0)|} in
  let tinted = colors (catalog_node "set_color" [Some grouped]
    Param.["group", Text_value "g"; "color_r", Float_value 0.25;
      "alpha", Float_value 0.5]) in
  check (tinted.x.(0) = 0.25 && tinted.w.(0) = 0.5 && tinted.x.(1) = 1.
      && tinted.w.(1) = 1.) "set_color group did not restrict the write";
  Session.close session

let run ?(exhaustive = false) () =
  gallery_grid_warning ();
  test_motion ();
  let labelled label ?with_ text = Node.relabel label (Lisp_sop.node ?with_ text) in
  let source = labelled "box"
    {|(sop/box :size [2.0 2.0 2.0] :connectivity "Quads" :consolidate_points true)|} in
  let ordinary_chain = labelled "normal" ~with_:["source", source]
    {|(sop/normals (sop/ext_source))|} in
  List.iter (fun node -> check (Node.has_parameters node)
      ("catalog node has no parameters: " ^ Node.label node))
    [ordinary_chain; List.hd (Node.inputs ordinary_chain)];
  let grid = labelled "grid" {|(sop/grid :width 3.0 :height 3.0 :columns 2 :rows 2 :size 3.0)|} in
  let plane = labelled "mountain" ~with_:["grid", grid]
    {|(sop/mountain (sop/ext_grid) :seed 3 :height 0.2 :frequency [0.2 1.0 0.2] :octaves 2 :lacunarity 2.0 :roughness 0.5)|} in
  let origin = labelled "points" {|(sop/points :points 4)|} in
  let orient_node = labelled "orient" ~with_:["origin", origin]
    {|(sop/attribute_noise_quaternion (sop/ext_origin) :seed 4 :owner "Point" :name "orient" :location "element-number" :frequency [0.2 0.2 0.2] :octaves 2)|} in
  let targets = labelled "jitter" ~with_:["orient", orient_node]
    {|(sop/point_jitter (sop/ext_orient) :seed 5 :scale 0.1)|} in
  let cutters = labelled "copy" ~with_:["plane", plane; "targets", targets]
    {|(sop/copy_to_points (sop/ext_plane) (sop/ext_targets))|} in
  let graph = labelled "exploded-view" ~with_:["fracture",
      labelled "fracture" ~with_:["source", source; "cutters", cutters]
        {|(sop/boolean_fracture (sop/ext_source) (sop/ext_cutters))|}]
    {|(sop/exploded_view (sop/ext_fracture))|} in
  let fracture = List.hd (Node.inputs graph) in
  let orient = List.hd (Node.inputs targets) in
  let custom_noise = Lisp_sop.node ~with_:["origin", List.hd (Node.inputs orient)]
    {|(sop/attribute_noise_quaternion (sop/ext_origin) :owner "Point" :name "orient" :seed 4 :frequency [0.2 0.2 0.2] :octaves 2 :location "attribute:rest position" :range "min-max;vec4,0.0,0.1,0.2,0.3;vec4,0.7,0.8,0.9,1.0")|} in
  let field node name = List.find (fun value -> value.Param.name = name)
      (Node.parameter_fields node) in
  List.iter (fun (node, names) -> List.iter (fun name ->
    let value = field node name in
    check (value.current = value.default)
      ("catalog create/menu default differs: " ^ name)) names)
    [plane, ["group"; "direction_attribute"; "mask_attribute";
             "height_attribute"; "recompute_normals"];
     orient, ["group"; "owner"; "name"; "location"; "range"];
     targets, ["group"; "mask_attribute"; "id_attribute";
               "axis_x"; "axis_y"; "axis_z"];
     fracture, ["resolve_cutter_self_intersections";
                "detriangulation"; "require_closed"; "piece_attribute"];
     (let box = Lisp_sop.node {|(sop/box)|} in box, List.map (fun value -> value.Param.name)
       (Node.parameter_fields box))];
  let changed_graph, effects = edit_parameters targets
      ~node_id:(Node.id orient)
      ["location", (field custom_noise "location").current;
       "range", (field custom_noise "range").current] |> Result.get_ok in
  let changed = Node.find changed_graph ~node_id:(Node.id orient)
      |> Option.get in
  check (effects.cook && Node.id changed = Node.id orient
      && (field changed "location").current
         = (field custom_noise "location").current
      && (field changed "range").current
         = (field custom_noise "range").current)
    "quaternion noise location/range were not editable schema fields";
  List.iter (fun (node, field, expected) ->
    let before = List.find (fun value -> value.Param.name = field)
        (Node.parameter_fields node) in
    check (before.current = Param.Text_value expected)
      ("catalog hidden parameter has wrong default: " ^ field);
    let edited, effects = Node.apply_parameters node
        [field, Param.Text_value "edited"] |> Result.get_ok in
    let after = List.find (fun value -> value.Param.name = field)
        (Node.parameter_fields edited) in
    check (effects.cook && Node.id edited = Node.id node
        && after.current = Param.Text_value "edited")
      ("catalog field did not rebuild: " ^ field))
    [plane, "height_attribute", ""; targets, "id_attribute", "";
     orient, "name", "orient"; fracture, "piece_attribute", "piece"];
  let parameterized = Node.inspect graph
      |> List.filter (fun info -> info.Node.has_parameters)
      |> List.map (fun info -> info.Node.label) in
  List.iter (fun label -> check (List.mem label parameterized)
      ("catalog node has no PPX inspector metadata: " ^ label))
    ["box"; "grid"; "mountain"; "points"; "orient"; "jitter"; "copy";
     "fracture"; "exploded-view"];
  let point_node = Node.inspect graph
      |> List.find (fun info -> info.Node.label = "points") in
  let edited, effects = edit_parameters graph ~node_id:point_node.id
      ["points", Param.Int_value 999] |> Result.get_ok in
  let edited_points = Node.find edited ~node_id:point_node.id |> Option.get in
  let value = Node.parameter_fields edited_points
      |> List.find (fun field -> field.Param.name = "points") in
  check (effects.cook && value.current = Param.Int_value 50)
    "catalog point count did not enforce its PPX hard maximum";
  let cube = labelled "cube" {|(sop/box)|}
  and dodecahedron = labelled "dodecahedron"
    {|(sop/platonic :kind "Dodecahedron" :radius 1.0 :normals "None")|} in
  let switched = labelled "source-switch" ~with_:["cube", cube; "dodecahedron", dodecahedron]
    {|(sop/switch (sop/ext_cube) (sop/ext_dodecahedron))|} in
  check (Node.operation switched = "switch" && Node.has_parameters switched)
    "catalog Switch is not the standard parameterized SOP switch";
  let switched, switch_effects = edit_parameters switched
      ~node_id:(Node.id switched)
      ["input", Param.Int_value 1]
      |> Result.get_ok in
  let switch_value = Node.parameter_fields switched
      |> List.find (fun field -> field.Param.name = "input") in
  check (switch_effects.cook
      && switch_value.current
         = Param.Int_value 1)
    "catalog Switch did not retain its selected labeled input";
  let session = Session.create ~max_entries:8 ~max_payload_bytes:1_000_000
      |> Result.get_ok in
  let cube_topology = Rdk.Geometry.topology (cook session cube) in
  check (Rdk.Topology.primitive_count cube_topology = 6
      && Rdk.Topology.primitive_size cube_topology 0 = 4)
    "catalog Box default must retain six quad faces";
  check (Rdk.Geometry.primitive_count (cook session switched)
      = Rdk.Geometry.primitive_count (cook session dodecahedron))
    "catalog Switch did not cook the selected dodecahedron branch";
  let replacement = labelled "replacement-grid"
    {|(sop/grid :width 1.0 :height 1.0 :columns 2 :rows 2 :size 1.0)|} in
  let document = Edit_graph.of_graph switched
      |> Edit_graph.add_node replacement |> Result.get_ok
      |> Edit_graph.connect ~source:(Node.id replacement)
           ~consumer:(Node.id switched) ~input_index:1 |> Result.get_ok in
  let rebuilt_switch = Edit_graph.compile document |> Result.get_ok in
  let switch_field = Node.parameter_fields rebuilt_switch
      |> List.find (fun field -> field.Param.name = "input") in
  check (match switch_field.kind with
    | Param.Choice_view options -> Array.exists
        (String.equal "1 · replacement-grid") options
    | _ -> false)
    "rewiring a Switch did not rebuild its input-dependent parameter labels";
  check (switch_field.current = Param.Int_value 1
      && Node.parameter_key switched = Node.parameter_key rebuilt_switch)
    "rewiring Switch labels must retain its integer selection and parameter key";
  Session.close session;
  let factory_keys = List.map Edit_graph.factory_key
      (Lazy.force registered_factories) in
  (* 162 SOP nodes and the three native carriers of ported ones (image_texels, pack_instances,
     subdivide_cracks) *)
  check (List.length factory_keys = 165
      && List.length (List.sort_uniq String.compare factory_keys) = 165)
    "combined SOP registry has a missing or duplicate factory key";
  check (List.mem "material" factory_keys) "SOP editor catalog is missing material";
  check (not (List.mem "delete_attribute" factory_keys))
    "duplicate Delete Attribute factory remains registered";
  check (not (List.mem "bounding_box" factory_keys))
    "duplicate Bounding Box factory remains registered";
  check (not (List.mem "rename_group" factory_keys))
    "duplicate Rename Group factory remains registered";
  check (not (List.mem "rename_attribute" factory_keys))
    "duplicate Rename Attribute factory remains registered";
  check (not (List.mem "group_promote" factory_keys))
    "duplicate Group Promote factory remains registered";
  check (not (List.mem "promote_attribute" factory_keys))
    "duplicate Promote Attribute factory remains registered";
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor catalog is missing " ^ key))
    ["box"; "grid"; "platonic"; "points"; "switch";
     "copy_to_points"; "boolean_fracture"; "boolean"; "boolean_seam";
     "boolean_detect"; "intersection_analysis";
     "attribute_noise_quaternion";
     "point_jitter"; "null"; "normals"; "transform"; "triangulate";
     "mountain"; "line"; "circle"; "spiral"; "uv_sphere"; "torus"; "tube";
     "match_size"; "mirror"; "clip"; "exploded_view"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor topology catalog is missing " ^ key))
    ["crease"; "subdivide"; "edge_divide"; "edge_collapse"; "dissolve";
     "poly_bevel"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor deformation catalog is missing " ^ key))
    ["peak"; "bend"; "smooth"; "reverse"; "clean"; "facet"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor edge/piece catalog is missing " ^ key))
    ["separate_pieces"; "edge_flip"; "edge_cusp"; "edge_straighten";
     "circle_from_edges"; "edge_equalize"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor modeling catalog is missing " ^ key))
    ["snap_to_grid"; "remesh"; "poly_extrude"; "poly_fill";
     "convert_line"; "resample"; "carve"; "ends"; "join_curves";
     "poly_path"; "poly_reduce"; "measure_curvature";
     "attribute_laplacian"; "polyframe"; "duplicate"; "match_axis";
     "convex_hull"; "extract_centroid"; "bound"];
  check (List.mem "fuse" factory_keys) "SOP editor catalog is missing fuse";
  check (List.mem "ray" factory_keys) "SOP editor catalog is missing ray";
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor distance catalog is missing " ^ key))
    ["distance_along_geometry"; "distance_from_geometry";
     "distance_from_target"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor topology/attribute catalog is missing " ^ key))
    ["point_split"; "poly_bridge"; "graph_color"; "edge_relax";
     "poly_loft"; "revolve"; "sweep"; "polywire"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor attribute catalog is missing " ^ key))
    ["measure"; "connectivity"; "set_float"; "set_int"; "set_vector";
     "set_orient"; "set_transform"; "set_color";
     "rest_position"; "enumerate"; "attribute_blur"; "attr_from_image"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor UV catalog is missing " ^ key))
    ["uv_project"; "uv_transform"; "uv_auto_seam"; "uv_unitize";
     "uv_flatten"; "uv_relax"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor group catalog is missing " ^ key))
    ["group_edges"; "group_random"; "group_bounds"; "group_normal";
     "group_non_planar"; "group_backface"; "group_edge_depth";
     "group_unshared"; "group_boundary_components";
     "group_from_attribute_boundary"; "group_promote_boundary";
     "group_promotions"; "group_invert"; "group_delete"; "group_rename";
     "group_find_path";
     "group_copy"; "group_transfer"; "groups_from_name";
     "group_combine"; "group_expand"; "group_range"; "group_ranges";
     "name_from_groups"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor general catalog is missing " ^ key))
    ["poly_cut"; "sort"; "noise_displace"; "color_by_height"; "scatter"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor attribute generator is missing " ^ key))
    ["attribute_noise"; "attribute_remap"; "attribute_randomize";
     "attribute_mirror"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor transport catalog is missing " ^ key))
    ["rewire_vertices"; "edge_transport"; "edge_transport_curves";
     "edge_transport_parent"];
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor metadata utility is missing " ^ key))
    ["compact_points"; "bound"; "group_rename";
     "delete_edge_group"; "rename_edge_group"; "delete_attributes";
     "rename_attributes"; "swap_attributes";
     "triangulate_2d"; "extract_point_from_curve"; "soft_transform";
     "point_generate"; "point_replicate"; "attribute_fade";
     "blast_by_attribute"; "blast"];
  check (List.mem "point_velocity" factory_keys)
    "SOP editor catalog is missing point_velocity";
  List.iter (fun key -> check (List.mem key factory_keys)
      ("SOP editor transfer catalog is missing " ^ key))
    ["attribute_transfer"; "attribute_transfer_all";
     "attribute_transfer_surface"; "attribute_copy";
     "attribute_interpolate";
     "promote_attributes"];
  let vector_groups = ref 0 in
  let flow_catalog = match Flow_sop.Catalog.of_factories ~version:1
      Sop_catalog.Editor.factories with
    | Ok catalog -> catalog
    | Error diagnostic -> fail (Diagnostic.to_string diagnostic) in
  let manifest, digest = Flow_sop.Manifest.generate Sop_catalog.Editor.factories
    |> Result.get_ok in
  let catalog_from_manifest, read_digest = Flow.Check.catalog_of_manifest manifest
    |> Result.get_ok in
  check (read_digest = digest
    && catalog_from_manifest.kinds = flow_catalog.kinds)
    "Flow manifest checker descriptor differs from the live catalog";
  let transform = List.find (fun (kind : Flow.Check.kind) -> kind.qualified = "sop/transform")
    catalog_from_manifest.kinds in
  check (match transform.facts with Some facts -> facts.elementwise = Flow.Check.Points
      && facts.reads = ["P"; "N"] && facts.writes = ["P"; "N"]
      && facts.preserves_topology && not facts.exact | None -> false)
    "Flow manifest omitted declared transform facts";
  let stale_manifest = Bytes.of_string manifest in
  let version_digit = String.index manifest '2' in
  Bytes.set stale_manifest version_digit '3';
  (match Flow.Check.catalog_of_manifest (Bytes.to_string stale_manifest) with
   | Error diagnostic -> check (diagnostic.code = "E_CATALOG"
       && diagnostic.message = "Flow manifest digest does not match its contents")
       "Flow manifest reader reported the wrong digest error"
   | Ok _ -> fail "Flow manifest reader accepted changed contents");
  check (Flow_sop.Manifest.generate Sop_catalog.Editor.factories
    = Ok (manifest, digest)) "Flow catalog manifest is not deterministic";
  let children name (form : Flow.Syntax.t) = match form.node with
    | List ({node = Sym head; _} :: children) when head = name -> children
    | _ -> fail ("Malformed Flow manifest " ^ name) in
  let forms = Flow.Syntax.parse manifest |> Result.get_ok in
  (match forms with
   | [root] ->
       (match children "flow_manifest" root with
        | [version; hash; kinds] ->
            (match children "version" version, children "digest" hash with
             | [{node = Num number; _}],
               [{node = Str value; _}] ->
                 check (int_of_string number = Flow_sop.Manifest.version
                   && value = digest) "Flow manifest header changed"
             | _ -> fail "Malformed Flow manifest header");
            let keys = children "kinds" kinds |> List.map (fun kind ->
              match children "kind" kind with
              | {node = Str name; _} :: _ -> name
              | _ -> fail "Malformed Flow manifest kind") in
            check (List.length keys = List.length (Lazy.force registered_factories))
              "Flow manifest omitted kinds";
            List.iter (fun key -> check (List.mem ("sop/" ^ key) keys)
              ("Flow manifest omitted SOP " ^ key)) factory_keys
        | _ -> fail "Malformed Flow manifest body")
   | _ -> fail "Flow manifest needs one top-level form");
  (* The schema sweep reads the factory's fields; building all 165 default nodes (a body lowering each, about 25 s)
     is the exhaustive run's, and the default one builds Box. *)
  List.iter (fun factory ->
    vector_groups := !vector_groups + check_metadata factory (Edit_graph.factory_fields factory);
    let slots = List.init (Edit_graph.factory_arity factory) (fun _ -> None) in
    if exhaustive || Edit_graph.factory_key factory = "box" then
    match Edit_graph.instantiate_optional factory slots with
    | Ok node ->
        check (Edit_graph.factory_facts factory = Node.facts node)
          ("default factory facts differ from the instantiated node: " ^ Edit_graph.factory_key factory);
        if Edit_graph.factory_key factory = "box" then
          check (List.filter_map (fun (field : Param.field_view) ->
              if field.primary then Some field.name else None) (Node.parameter_fields node)
              = ["size_x"; "size_y"; "size_z"])
            "Box primary rows still expose topology instead of size";
        check
        (Node.operation node = Edit_graph.factory_operation factory)
        (Printf.sprintf "registered SOP %s advertises operation %s but builds %s"
          (Edit_graph.factory_key factory)
          (Edit_graph.factory_operation factory) (Node.operation node));
    | Error message -> fail (Printf.sprintf
        "registered SOP %s could not be constructed: %s"
        (Edit_graph.factory_key factory) message))
    (Lazy.force registered_factories);
  check (!vector_groups = 104) "catalog vector inventory changed";
  (* Every cook-impact schema field of every registered SOP participates in
     the cache identity: perturbing any single field changes the node's
     parameter key, so a field forgotten by an operator's own key string can
     never produce a stale cache hit. *)
  if exhaustive then begin
  let perturbed (view : Param.field_view) =
    let within_int (range : Param.int_range) value =
      Option.fold ~none:true ~some:(fun low -> value >= low) range.hard_min
      && Option.fold ~none:true ~some:(fun high -> value <= high) range.hard_max
    and within_float (range : Param.float_range) value =
      Option.fold ~none:true ~some:(fun low -> value >= low) range.hard_min
      && Option.fold ~none:true ~some:(fun high -> value <= high) range.hard_max
    in
    match view.kind, view.current with
    | Param.Toggle_view, Param.Bool_value value ->
        Some (Param.Bool_value (not value))
    | Param.Integer_view range, Param.Int_value value ->
        List.find_opt (fun candidate -> candidate <> value
            && within_int range candidate)
          [value + 1; value - 1; range.soft_min; range.soft_max]
        |> Option.map (fun value -> Param.Int_value value)
    | Param.Floating_view range, Param.Float_value value ->
        List.find_opt (fun candidate -> candidate <> value
            && within_float range candidate)
          [value +. 0.25; value -. 0.25;
           (range.soft_min +. range.soft_max) /. 2.;
           range.soft_min; range.soft_max]
        |> Option.map (fun value -> Param.Float_value value)
    | Param.Text_view, Param.Text_value value ->
        Some (Param.Text_value (if value = "" then "p" else value ^ "p"))
    | Param.Choice_view options, Param.Choice_value value ->
        Array.to_list options |> List.find_opt (fun option -> option <> value)
        |> Option.map (fun option -> Param.Choice_value option)
    | Param.Choice_view options, Param.Int_value value ->
        (* An index choice (sop/switch :input) names its options by position. *)
        List.find_opt (fun candidate -> candidate <> value
            && candidate >= 0 && candidate < Array.length options)
          [value + 1; value - 1; 0]
        |> Option.map (fun value -> Param.Int_value value)
    | _ -> None in
  let perturbed_fields = ref 0 and rejected_fields = ref 0 in
  List.iter (fun factory ->
    let key = Edit_graph.factory_key factory in
    let slots = List.init (Edit_graph.factory_arity factory) (fun _ -> None) in
    let node = Edit_graph.instantiate_optional factory slots |> Result.get_ok in
    check (not (Node.has_parameters node) || Node.parameter_key node <> "")
      (key ^ " has no schema-derived cache identity");
    List.iter (fun (view : Param.field_view) ->
      if view.impact = Param.Cook then
        match perturbed view with
        | None -> fail (Printf.sprintf "%s.%s has no perturbable value" key
            view.name)
        | Some value ->
            match Node.apply_parameters node [view.name, value] with
            | exception (Invalid_argument _ | Failure _) -> incr rejected_fields
            | Error _ -> incr rejected_fields
            | Ok (_, effects) when not effects.cook -> incr rejected_fields
            | Ok (edited, _) ->
                incr perturbed_fields;
                check (Node.id edited = Node.id node
                    && Node.parameter_key edited <> Node.parameter_key node)
                  (Printf.sprintf
                    "changing %s.%s did not change its cache key" key
                    view.name))
      (Node.parameter_fields node))
    (Lazy.force registered_factories);
  Printf.printf "catalog cache-key perturbation: %d fields changed, %d rejected\n"
    !perturbed_fields !rejected_fields;
  check (!perturbed_fields > 10 * !rejected_fields)
    (Printf.sprintf "only %d catalog fields were perturbable (%d rejected)"
      !perturbed_fields !rejected_fields)
  end;
  let key_session = Session.create ~max_entries:8 ~max_payload_bytes:1_000_000
      |> Result.get_ok in
  let exploded = labelled "key-view" ~with_:["key_box", labelled "key-box" {|(sop/box)|}]
    {|(sop/exploded_view (sop/ext_key_box))|} in
  ignore (cook key_session exploded);
  let misses = (Session.stats key_session).misses in
  let exploded, _ = Node.apply_parameters exploded
      ["piece_attribute", Param.Text_value "shard"] |> Result.get_ok in
  ignore (cook key_session exploded);
  check ((Session.stats key_session).misses = misses + 1)
    "a schema field ignored by the operator key did not miss the session cache";
  Session.close key_session;
  let match_size_factory = List.find (fun factory ->
      Edit_graph.factory_key factory = "match_size")
      (Lazy.force registered_factories) in
  check (Edit_graph.factory_inputs match_size_factory
      = [Edit_graph.Required; Optional]
      && Result.is_ok (Edit_graph.instantiate_optional match_size_factory
        [None; None]))
    "PPX descriptor lost Match Size's optional target input signature";
  List.iter (fun key ->
    let factory = List.find (fun factory ->
        Edit_graph.factory_key factory = key) (Lazy.force registered_factories) in
    check (Edit_graph.factory_inputs factory
        = [Edit_graph.Required; Optional]
        && Result.is_ok (Edit_graph.instantiate_optional factory [None; None]))
      (key ^ " lost its optional collision input signature"))
    ["boolean_detect"; "intersection_analysis"; "fuse"; "poly_loft";
     "rest_position"];
  let delete_attributes_factory = List.find (fun factory ->
      Edit_graph.factory_key factory = "delete_attributes")
      (Lazy.force registered_factories) in
  check (Edit_graph.factory_inputs delete_attributes_factory
      = [Edit_graph.Required; Optional]
      && Result.is_ok (Edit_graph.instantiate_optional
        delete_attributes_factory [None; None]))
    "Delete Attributes lost its optional reference input signature";
  let point_replicate_factory = List.find (fun factory ->
      Edit_graph.factory_key factory = "point_replicate")
      (Lazy.force registered_factories) in
  check (Edit_graph.factory_inputs point_replicate_factory
      = [Edit_graph.Required; Optional]
      && Result.is_ok (Edit_graph.instantiate_optional point_replicate_factory
        [None; None]))
    "Point Replicate lost its optional custom-shape input signature";
  let attribute_fade_factory = List.find (fun factory ->
      Edit_graph.factory_key factory = "attribute_fade")
      (Lazy.force registered_factories) in
  check (Edit_graph.factory_inputs attribute_fade_factory
      = [Edit_graph.Required; Optional; Optional]
      && Result.is_ok (Edit_graph.instantiate_optional attribute_fade_factory
        [None; None; None]))
    "Attribute Fade lost its two optional source input signatures";
  let fade_source = labelled "fade-source" {|(sop/box)|}
  and fade_hold = labelled "fade-hold" {|(sop/box)|} in
  let fade = Edit_graph.instantiate_optional attribute_fade_factory
      [Some fade_source; None; Some fade_hold] |> Result.get_ok in
  let fade, _ = Node.apply_parameters fade
      ["fade_in", Param.Float_value 3.] |> Result.get_ok in
  check (List.map Node.id (Node.inputs fade)
      = [Node.id fade_source; Node.id fade_hold])
    "Attribute Fade parameter edit reassigned its sparse hold-source port";
  let point_velocity_factory = List.find (fun factory ->
      Edit_graph.factory_key factory = "point_velocity")
      (Lazy.force registered_factories) in
  check (Edit_graph.factory_inputs point_velocity_factory
      = [Edit_graph.Required; Optional; Optional]
      && Result.is_ok (Edit_graph.instantiate_optional point_velocity_factory
        [None; None; None]))
    "Point Velocity lost its previous/next optional input signatures";
  let velocity = Edit_graph.instantiate_optional point_velocity_factory
      [Some fade_source; None; Some fade_hold] |> Result.get_ok in
  let velocity, _ = Node.apply_parameters velocity
      ["dt", Param.Float_value 0.1] |> Result.get_ok in
  check (List.map Node.id (Node.inputs velocity)
      = [Node.id fade_source; Node.id fade_hold])
    "Point Velocity parameter edit reassigned its sparse next-sample port";
  let points_factory = List.find (fun factory ->
      Edit_graph.factory_key factory = "points") (Lazy.force registered_factories) in
  let points = Edit_graph.instantiate points_factory [] |> Result.get_ok in
  check (Node.operation points = "point_generate"
      && Edit_graph.factory_arity points_factory = 0)
    "point-generation editor factory has the wrong node or arity";
  let point_session = Session.create ~max_entries:2
      ~max_payload_bytes:1_000_000 |> Result.get_ok in
  let point_mesh = cook point_session points |> Rdk_rays.Rays_mesh.to_mesh
      |> Result.map_error Rdk.Error.to_string |> Result.get_ok in
  check (Mesh.mode point_mesh = Mesh.Points && Mesh.vertex_count point_mesh = 50)
    "point-only SOP output did not retain point rendering mode";
  Session.close point_session;
  let explosion = Rdk_rays.Packed_pieces.explosion
      ~operation:(Node.operation graph) ~fields:(Node.parameter_fields graph) |> Option.get in
  check (Node.operation graph = "exploded_view" && explosion.amount = 0.32
      && explosion.piece_attribute = "piece")
    "standard Exploded View node lost its operation or PPX defaults";
  set_color_test ();
  test_morph_sketch ();
  test_node_inputs ();
  print_endline "SOP catalog tests passed"
