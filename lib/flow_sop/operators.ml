let checked = function
  | Ok value -> value
  | Error (d : Diagnostic.t) -> Flow.Value.raise_diagnostic d

let type_argument args =
  match List.find_opt (fun (name, _, _) -> name = "type") args with
  | None -> "vec3"
  | Some (_, _, Some (Param.Text_value kind)) ->
      ignore (checked (Attribute_kernel.value_type kind));
      kind
  | Some _ -> Flow.Value.fail "E_ATTR_TYPE" "Attribute type must be a literal storage type."

let attribute_type args = checked (Attribute_kernel.value_type (type_argument args))

let planar_values kind = function
  | Flow.Ty.Record fields ->
      let names =
        match kind with
        | "vec2" -> [ "x"; "y" ]
        | "vec3" -> [ "x"; "y"; "z" ]
        | "vec4" -> [ "x"; "y"; "z"; "w" ]
        | _ -> []
      in
      names <> []
      && List.sort String.compare (List.map fst fields) = List.sort String.compare names
      && List.for_all (fun (_, ty) -> Flow.Ty.fits ty (Flow.Ty.Array Flow.Ty.Float)) fields
  | _ -> false

let validate_args args =
  (match List.assoc "attribute" args with
  | Flow.Value.Text name when String.trim name <> "" -> ()
  | _ -> Flow.Value.fail "E_ATTR_NAME" "An attribute needs a nonblank name.");
  Option.iter
    (function
      | Flow.Value.Text owner -> ignore (checked (Attribute_kernel.owner owner))
      | _ -> Flow.Value.fail "E_ATTR_OWNER" "Attribute owner must be text.")
    (List.assoc_opt "owner" args);
  Option.iter
    (function
      | Flow.Value.Text kind -> ignore (checked (Attribute_kernel.value_type kind))
      | _ -> Flow.Value.fail "E_ATTR_TYPE" "Attribute type must be text.")
    (List.assoc_opt "type" args)

let attr : Flow.Op.t =
  {
    name = "sop/attr";
    ctx = Flow.Context.sop;
    signature =
      {
        pos = [ ("geometry", Flow.Ty.geometry); ("attribute", Flow.Ty.Text) ];
        opt = [];
        rest = None;
        kw = [ ("owner", Flow.Ty.Text); ("type", Flow.Ty.Text) ];
      };
    out = (fun _ -> Flow.Ty.Array Flow.Ty.Vec3);
    out_args = Some attribute_type;
    any_num = false;
    choices = [];
    shape = Flow.Op.Struct { splice = false };
    live = false;
    category = "Attributes";
    arithmetic = None;
    packed_extension = None;
    check = validate_args;
    body =
      (fun ~live:_ ~node:_ args ->
        let named =
          List.map
            (fun (name, v) ->
              ( name,
                Flow.Value.ty_of v,
                match v with Flow.Value.Text text -> Some (Param.Text_value text) | _ -> None ))
            args
        in
        let ty = attribute_type named in
        match ty with
        | Flow.Ty.Record fields ->
            Flow.Value.Record
              (List.map
                 (fun (name, ty) ->
                   ( name,
                     Flow.Value.Struct
                       ("sop/attr", ty, args @ [ ("component", Flow.Value.Text name) ]) ))
                 fields)
        | _ -> Flow.Value.Struct ("sop/attr", ty, args));
  }

let checked_attribute_type args =
  let kind = type_argument args in
  if not (List.mem kind [ "float"; "vec2"; "vec3"; "vec4" ]) then
    Flow.Value.fail "E_ATTR_TYPE" "Checked attributes require float, vec2, vec3 or vec4 storage.";
  Flow.Ty.Record [ ("values", attribute_type args); ("valid", Flow.Ty.Array Flow.Ty.Int) ]

let attr_checked : Flow.Op.t =
  {
    attr with
    name = "sop/attr_checked";
    out = (fun _ -> Flow.Ty.Record [ ("values", Array Vec3); ("valid", Array Int) ]);
    out_args = Some checked_attribute_type;
    check =
      (fun args ->
        validate_args args;
        let kind =
          match List.assoc_opt "type" args with Some (Flow.Value.Text kind) -> kind | _ -> "vec3"
        in
        if not (List.mem kind [ "float"; "vec2"; "vec3"; "vec4" ]) then
          Flow.Value.fail "E_ATTR_TYPE"
            "Checked attributes require float, vec2, vec3 or vec4 storage.");
    body =
      (fun ~live:_ ~node:_ args ->
        let named =
          List.map
            (fun (name, v) ->
              ( name,
                Flow.Value.ty_of v,
                match v with Flow.Value.Text text -> Some (Param.Text_value text) | _ -> None ))
            args
        in
        match checked_attribute_type named with
        | Flow.Ty.Record fields ->
            Flow.Value.Record
              (List.map
                 (fun (name, ty) ->
                   ( name,
                     Flow.Value.Source
                       ("sop/attr_checked", ty, args @ [ ("component", Flow.Value.Text name) ]) ))
                 fields)
        | _ -> assert false);
  }

(* Float-family attribute as planes with a cook-time width: x, y, z, w (planes past [width] are zero). *)
let planes_type =
  Flow.Ty.Record
    [
      ("width", Int); ("x", Array Float); ("y", Array Float); ("z", Array Float); ("w", Array Float);
    ]

let attr_planes : Flow.Op.t =
  {
    attr with
    name = "sop/attr_planes";
    signature = { attr.signature with kw = [ ("owner", Flow.Ty.Text) ] };
    out = (fun _ -> planes_type);
    out_args = None;
    check = validate_args;
    body =
      (fun ~live:_ ~node:_ args ->
        match planes_type with
        | Flow.Ty.Record fields ->
            Flow.Value.Record
              (List.map
                 (fun (name, ty) ->
                   ( name,
                     Flow.Value.Source
                       ("sop/attr_planes", ty, args @ [ ("component", Flow.Value.Text name) ]) ))
                 fields)
        | _ -> assert false);
  }

let with_attr : Flow.Op.t =
  {
    attr with
    name = "sop/with_attr";
    signature =
      {
        attr.signature with
        pos = attr.signature.pos @ [ ("values", Flow.Ty.Any) ];
        kw =
          attr.signature.kw
          @ [
              ("selection", Flow.Ty.Array Flow.Ty.Int);
              ("default", Flow.Ty.Any);
              ("when", Flow.Ty.Bool);
            ];
      };
    out = (fun _ -> Flow.Ty.geometry);
    out_args =
      Some
        (fun args ->
          let ty = attribute_type args in
          let present name = List.exists (fun (key, _, _) -> key = name) args in
          if present "selection" <> present "default" then
            Flow.Value.fail "E_ATTR_SELECTION" "Selected writes require both selection and default.";
          if present "selection" then begin
            ignore (checked_attribute_type args);
            let expected = Option.get (Flow.Ty.elem ty) in
            Option.iter
              (fun (_, actual, _) ->
                if not (Flow.Ty.fits actual expected) then
                  Flow.Value.fail "E_ATTR_TYPE"
                    ("Attribute default requires " ^ Flow.Ty.to_string expected ^ "."))
              (List.find_opt (fun (name, _, _) -> name = "default") args)
          end;
          Option.iter
            (fun (_, actual, _) ->
              if
                not
                  (Flow.Ty.fits actual ty
                  || ((not (present "selection")) && planar_values (type_argument args) actual))
              then
                Flow.Value.fail "E_ATTR_TYPE"
                  ("Attribute values require " ^ Flow.Ty.to_string ty ^ "."))
            (List.find_opt (fun (name, _, _) -> name = "values") args);
          Flow.Ty.geometry);
    shape = Flow.Op.Scalar;
    check =
      (fun args ->
        validate_args args;
        if List.mem_assoc "selection" args <> List.mem_assoc "default" args then
          Flow.Value.fail "E_ATTR_SELECTION" "Selected writes require both selection and default.");
    body =
      (fun ~live:_ ~node args ->
        (* [:when] guards the write: a static guard decides it here, a cooked one when it cooks *)
        match List.assoc_opt "when" args with
        | Some (Flow.Value.Bool false) -> List.assoc "geometry" args
        | Some (Flow.Value.Bool true) -> node "sop/with_attr" (List.remove_assoc "when" args)
        | _ -> node "sop/with_attr" args);
  }

let has_attr : Flow.Op.t =
  {
    attr with
    name = "sop/has_attr";
    out = (fun _ -> Flow.Ty.Bool);
    out_args =
      Some
        (fun args ->
          if List.exists (fun (name, _, _) -> name = "type") args then ignore (attribute_type args);
          Flow.Ty.Bool);
    body = (fun ~live:_ ~node:_ args -> Flow.Value.Source ("sop/has_attr", Flow.Ty.Bool, args));
  }

let delete_attr : Flow.Op.t =
  {
    attr with
    name = "sop/delete_attr";
    signature = { attr.signature with kw = [ ("owner", Flow.Ty.Text) ] };
    out = (fun _ -> Flow.Ty.geometry);
    out_args = None;
    shape = Flow.Op.Scalar;
    choices = [ ("owner", [ "point"; "vertex"; "primitive"; "detail" ]) ];
    check =
      (fun args ->
        (match List.assoc "attribute" args with
        | Flow.Value.Text pattern when String.trim pattern <> "" -> (
            match Rdk.Attribute_pattern.compile pattern with
            | Ok _ -> ()
            | Error message -> Flow.Value.fail "E_ATTR_PATTERN" message)
        | _ -> Flow.Value.fail "E_ATTR_NAME" "An attribute pattern must be nonblank.");
        Option.iter
          (function
            | Flow.Value.Text owner when List.mem owner [ "point"; "vertex"; "primitive"; "detail" ]
              ->
                ()
            | _ ->
                Flow.Value.fail "E_ATTR_OWNER"
                  "Attribute owner is point, vertex, primitive or detail.")
          (List.assoc_opt "owner" args));
    body = (fun ~live:_ ~node args -> node "sop/delete_attr" args);
  }

let count : Flow.Op.t =
  {
    attr with
    name = "sop/count";
    category = "Topology";
    signature =
      {
        pos = [ ("geometry", Flow.Ty.geometry); ("element", Flow.Ty.Text) ];
        opt = [];
        rest = None;
        kw = [];
      };
    out = (fun _ -> Flow.Ty.Int);
    out_args = None;
    choices = [ ("element", [ "points"; "vertices"; "primitives"; "edges" ]) ];
    check =
      (fun args ->
        match List.assoc "element" args with
        | Flow.Value.Text element
          when List.mem element [ "points"; "vertices"; "primitives"; "edges" ] ->
            ()
        | _ ->
            Flow.Value.fail "E_TOPOLOGY_COUNT"
              "Count element is points, vertices, primitives or edges.");
    body = (fun ~live:_ ~node:_ args -> Flow.Value.Source ("sop/count", Flow.Ty.Int, args));
  }

let edges : Flow.Op.t =
  {
    count with
    name = "sop/edges";
    signature = { count.signature with pos = [ ("geometry", Flow.Ty.geometry) ] };
    out = (fun _ -> Flow.Ty.Record [ ("a", Array Int); ("b", Array Int) ]);
    choices = [];
    check = (fun _ -> ());
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Record
          (List.map
             (fun component ->
               ( component,
                 Flow.Value.Source
                   ( "sop/edges",
                     Flow.Ty.Array Flow.Ty.Int,
                     args @ [ ("component", Flow.Value.Text component) ] ) ))
             [ "a"; "b" ]));
  }

let topology_type =
  Flow.Ty.Record
    [ ("vertex_points", Array Int); ("primitive_offsets", Array Int); ("kinds", Array Int) ]

let topology : Flow.Op.t =
  {
    edges with
    name = "sop/topology";
    out = (fun _ -> topology_type);
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Record
          (List.map
             (fun component ->
               ( component,
                 Flow.Value.Source
                   ( "sop/topology",
                     Flow.Ty.Array Flow.Ty.Int,
                     args @ [ ("component", Flow.Value.Text component) ] ) ))
             [ "vertex_points"; "primitive_offsets"; "kinds" ]));
  }

let adjacency_fields =
  [
    "primitive_of_vertex";
    "next_vertex";
    "previous_vertex";
    "edge_of_vertex";
    "opposite_vertex";
    "edge_a";
    "edge_b";
    "point_edge_offsets";
    "point_edges";
    "point_vertex_offsets";
    "point_vertices";
  ]

let adjacency : Flow.Op.t =
  {
    edges with
    name = "sop/adjacency";
    out =
      (fun _ ->
        Flow.Ty.Record (List.map (fun name -> (name, Flow.Ty.Array Flow.Ty.Int)) adjacency_fields));
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Record
          (List.map
             (fun component ->
               ( component,
                 Flow.Value.Source
                   ( "sop/adjacency",
                     Flow.Ty.Array Flow.Ty.Int,
                     args @ [ ("component", Flow.Value.Text component) ] ) ))
             adjacency_fields));
  }

let triangles_fields = [ "offsets"; "ok"; "primitive"; "a"; "b"; "c" ]

let triangles : Flow.Op.t =
  {
    edges with
    name = "sop/triangles";
    out =
      (fun _ ->
        Flow.Ty.Record (List.map (fun name -> (name, Flow.Ty.Array Flow.Ty.Int)) triangles_fields));
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Record
          (List.map
             (fun component ->
               ( component,
                 Flow.Value.Source
                   ( "sop/triangles",
                     Flow.Ty.Array Flow.Ty.Int,
                     args @ [ ("component", Flow.Value.Text component) ] ) ))
             triangles_fields));
  }

let components_type = Flow.Ty.Record [ ("id", Array Int); ("count", Int) ]

let components : Flow.Op.t =
  {
    edges with
    name = "sop/components";
    signature =
      {
        pos = [ ("geometry", Flow.Ty.geometry) ];
        opt = [];
        rest = None;
        kw =
          [
            ("over", Flow.Ty.Text);
            ("mask", Flow.Ty.Array Flow.Ty.Int);
            ("seams", Flow.Ty.Array Flow.Ty.Int);
          ];
      };
    out = (fun _ -> components_type);
    choices = [ ("over", [ "edges"; "points" ]) ];
    check =
      (fun args ->
        Option.iter
          (function
            | Flow.Value.Text ("edges" | "points") -> ()
            | _ -> Flow.Value.fail "E_COMPONENTS_OVER" "Components :over is edges or points.")
          (List.assoc_opt "over" args));
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Record
          (List.map
             (fun (component, ty) ->
               ( component,
                 Flow.Value.Source
                   ("sop/components", ty, args @ [ ("component", Flow.Value.Text component) ]) ))
             [ ("id", Flow.Ty.Array Flow.Ty.Int); ("count", Flow.Ty.Int) ]));
  }

(* graph/ operators: pure array functions over the adjacency record. *)
let adjacency_type =
  Flow.Ty.Record (List.map (fun name -> (name, Flow.Ty.Array Flow.Ty.Int)) adjacency_fields)

let rows_type = Flow.Ty.Record [ ("offsets", Array Int); ("values", Array Int) ]

let graph_op name ~pos ~kw ~out ~fields : Flow.Op.t =
  {
    count with
    name;
    category = "Topology";
    signature = { pos; opt = []; rest = None; kw };
    out = (fun _ -> out);
    choices = [];
    check = (fun _ -> ());
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Record
          (List.map
             (fun (component, ty) ->
               ( component,
                 Flow.Value.Source (name, ty, args @ [ ("component", Flow.Value.Text component) ])
               ))
             fields));
  }

let shortest_paths =
  graph_op "graph/shortest_paths"
    ~pos:
      [
        ("adjacency", adjacency_type);
        ("lengths", Flow.Ty.Array Flow.Ty.Float);
        ("roots", Flow.Ty.Array Flow.Ty.Int);
      ]
    ~kw:
      [
        ("radius", Flow.Ty.Float);
        ("hops", Flow.Ty.Bool);
        ("blocked_points", Flow.Ty.Array Flow.Ty.Int);
        ("blocked_edges", Flow.Ty.Array Flow.Ty.Int);
        ("target", Flow.Ty.Int);
        ("ties", Flow.Ty.Text);
      ]
    ~out:
      (Flow.Ty.Record
         [
           ("distance", Array Float);
           ("parent", Array Int);
           ("path", Array Int);
           ("order", Array Int);
         ])
    ~fields:
      [
        ("distance", Flow.Ty.Array Flow.Ty.Float);
        ("parent", Flow.Ty.Array Flow.Ty.Int);
        ("path", Flow.Ty.Array Flow.Ty.Int);
        ("order", Flow.Ty.Array Flow.Ty.Int);
      ]

let forest =
  graph_op "graph/forest"
    ~pos:[ ("parents", Flow.Ty.Array Flow.Ty.Int) ]
    ~kw:[ ("mask", Flow.Ty.Array Flow.Ty.Int) ]
    ~out:
      (Flow.Ty.Record
         [
           ("order", Array Int);
           ("depth", Array Int);
           ("root", Array Int);
           ("offsets", Array Int);
           ("children", Array Int);
         ])
    ~fields:
      [
        ("order", Flow.Ty.Array Flow.Ty.Int);
        ("depth", Flow.Ty.Array Flow.Ty.Int);
        ("root", Flow.Ty.Array Flow.Ty.Int);
        ("offsets", Flow.Ty.Array Flow.Ty.Int);
        ("children", Flow.Ty.Array Flow.Ty.Int);
      ]

let color =
  graph_op "graph/color"
    ~pos:[ ("adjacency", rows_type); ("order", Flow.Ty.Array Flow.Ty.Int) ]
    ~kw:[]
    ~out:(Flow.Ty.Record [ ("color", Array Int); ("count", Int) ])
    ~fields:[ ("color", Flow.Ty.Array Flow.Ty.Int); ("count", Flow.Ty.Int) ]

let chains =
  graph_op "graph/chains"
    ~pos:[ ("adjacency", adjacency_type) ]
    ~kw:[ ("mask", Flow.Ty.Array Flow.Ty.Int) ]
    ~out:(Flow.Ty.Record [ ("offsets", Array Int); ("values", Array Int); ("closed", Array Int) ])
    ~fields:
      [
        ("offsets", Flow.Ty.Array Flow.Ty.Int);
        ("values", Flow.Ty.Array Flow.Ty.Int);
        ("closed", Flow.Ty.Array Flow.Ty.Int);
      ]

let mesh_fields =
  [
    ("points", Flow.Ty.Array Flow.Ty.Vec3);
    ("vertex_points", Flow.Ty.Array Flow.Ty.Int);
    ("primitive_offsets", Flow.Ty.Array Flow.Ty.Int);
    ("kinds", Flow.Ty.Array Flow.Ty.Int);
  ]

let mesh : Flow.Op.t =
  {
    count with
    name = "sop/mesh";
    signature = { pos = []; opt = []; rest = None; kw = mesh_fields };
    out = (fun _ -> Flow.Ty.geometry);
    out_args =
      Some
        (fun args ->
          List.iter
            (fun (name, _) ->
              if not (List.exists (fun (key, _, _) -> key = name) args) then
                Flow.Value.fail "E_ARGS" ("sop/mesh needs :" ^ name ^ "."))
            mesh_fields;
          Flow.Ty.geometry);
    shape = Flow.Op.Scalar;
    choices = [];
    check =
      (fun args ->
        List.iter
          (fun (name, _) ->
            if not (List.mem_assoc name args) then
              Flow.Value.fail "E_ARGS" ("sop/mesh needs :" ^ name ^ "."))
          mesh_fields);
    body = (fun ~live:_ ~node args -> node "sop/mesh" args);
  }

(* A separate typed constructor keeps existing mesh/error polymorphism intact.
   Both representations use the same owned native topology boundary. *)
let mesh_planes_type =
  Flow.Ty.Record (List.map (fun name -> (name, Flow.Ty.Array Flow.Ty.Float)) [ "x"; "y"; "z" ])

let mesh_planes_fields = ("points", mesh_planes_type) :: List.tl mesh_fields

let mesh_planes : Flow.Op.t =
  {
    mesh with
    name = "sop/mesh_planes";
    signature = { pos = []; opt = []; rest = None; kw = mesh_planes_fields };
    out_args =
      Some
        (fun args ->
          List.iter
            (fun (name, _) ->
              if not (List.exists (fun (key, _, _) -> key = name) args) then
                Flow.Value.fail "E_ARGS" ("sop/mesh_planes needs :" ^ name ^ "."))
            mesh_planes_fields;
          (match List.find_opt (fun (name, _, _) -> name = "points") args with
          | Some (_, Flow.Ty.Record fields, _) when List.length fields <> 3 ->
              Flow.Value.fail "E_TYPE" "Mesh planes require exactly x, y and z float-array fields."
          | _ -> ());
          Flow.Ty.geometry);
    check =
      (fun args ->
        List.iter
          (fun (name, _) ->
            if not (List.mem_assoc name args) then
              Flow.Value.fail "E_ARGS" ("sop/mesh_planes needs :" ^ name ^ "."))
          mesh_planes_fields);
    body = (fun ~live:_ ~node args -> node "sop/mesh_planes" args);
  }

let group_check args =
  (match List.assoc_opt "name" args with
  | Some (Flow.Value.Text name) when String.trim name <> "" -> ()
  | _ -> Flow.Value.fail "E_GROUP_NAME" "Group name must not be blank.");
  Option.iter
    (function
      | Flow.Value.Text owner -> ignore (checked (Group_kernel.owner owner))
      | _ -> Flow.Value.fail "E_GROUP_OWNER" "Group owner is point, vertex, primitive or edge.")
    (List.assoc_opt "owner" args);
  Option.iter
    (function
      | Flow.Value.Text "point" -> ()
      | _ -> Flow.Value.fail "E_GROUP_TARGET" "Group promotion target is point.")
    (List.assoc_opt "as_owner" args)

let group : Flow.Op.t =
  {
    attr with
    name = "sop/group";
    category = "Groups";
    signature =
      {
        pos = [ ("geometry", Flow.Ty.geometry) ];
        opt = [];
        rest = None;
        kw = [ ("name", Flow.Ty.Text); ("owner", Flow.Ty.Text); ("as_owner", Flow.Ty.Text) ];
      };
    out = (fun _ -> Flow.Ty.Array Flow.Ty.Int);
    out_args = None;
    choices = [ ("owner", [ "point"; "vertex"; "primitive"; "edge" ]); ("as_owner", [ "point" ]) ];
    check = group_check;
    body =
      (fun ~live:_ ~node:_ args -> Flow.Value.Source ("sop/group", Flow.Ty.Array Flow.Ty.Int, args));
  }

let group_order_type = Flow.Ty.Record [ ("ordered", Bool); ("indices", Array Int) ]

let group_order : Flow.Op.t =
  {
    group with
    name = "sop/group_order";
    out = (fun _ -> group_order_type);
    signature = { group.signature with kw = List.remove_assoc "as_owner" group.signature.kw };
    choices = List.remove_assoc "as_owner" group.choices;
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Record
          (List.map
             (fun (component, ty) ->
               ( component,
                 Flow.Value.Source
                   ("sop/group_order", ty, args @ [ ("component", Flow.Value.Text component) ]) ))
             [ ("ordered", Flow.Ty.Bool); ("indices", Flow.Ty.Array Int) ]));
  }

let group_names : Flow.Op.t =
  {
    group with
    name = "sop/group_names";
    choices = List.remove_assoc "as_owner" group.choices;
    signature =
      {
        group.signature with
        pos = [ ("geometry", Flow.Ty.geometry); ("pattern", Flow.Ty.Text) ];
        kw = [ ("owner", Flow.Ty.Text) ];
      };
    out = (fun _ -> Flow.Ty.Array Flow.Ty.Text);
    check =
      (fun args ->
        Option.iter
          (function
            | Flow.Value.Text owner -> ignore (checked (Group_kernel.owner owner))
            | _ ->
                Flow.Value.fail "E_GROUP_OWNER" "Group owner is point, vertex, primitive or edge.")
          (List.assoc_opt "owner" args);
        match List.assoc "pattern" args with
        | Flow.Value.Text pattern -> (
            match Rdk.Attribute_pattern.compile pattern with
            | Ok _ -> ()
            | Error message -> Flow.Value.fail "E_GROUP_PATTERN" message)
        | _ -> Flow.Value.fail "E_GROUP_PATTERN" "Group pattern must be text.");
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Source ("sop/group_names", Flow.Ty.Array Flow.Ty.Text, args));
  }

let group_entries_type = Flow.Ty.Record [ ("owners", Array Text); ("names", Array Text) ]

let group_entries : Flow.Op.t =
  {
    group with
    name = "sop/group_entries";
    signature =
      {
        pos = [ ("geometry", Flow.Ty.geometry); ("pattern", Flow.Ty.Text) ];
        opt = [];
        rest = None;
        kw = [ ("owner", Flow.Ty.Text) ];
      };
    out = (fun _ -> group_entries_type);
    choices = [ ("owner", [ "point"; "vertex"; "primitive"; "edge"; "any" ]) ];
    check = (fun _ -> ());
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Record
          (List.map
             (fun component ->
               ( component,
                 Flow.Value.Source
                   ( "sop/group_entries",
                     Flow.Ty.Array Flow.Ty.Text,
                     args @ [ ("component", Flow.Value.Text component) ] ) ))
             [ "owners"; "names" ]));
  }

let with_group : Flow.Op.t =
  {
    group with
    name = "sop/with_group";
    signature =
      {
        group.signature with
        pos = [ ("geometry", Flow.Ty.geometry); ("values", Flow.Ty.Array Int) ];
        kw = group.signature.kw @ [ ("order", Flow.Ty.Any) ];
      };
    out = (fun _ -> Flow.Ty.geometry);
    shape = Flow.Op.Scalar;
    out_args =
      Some
        (fun args ->
          Option.iter
            (fun (_, ty, _) ->
              if not (Flow.Ty.fits ty (Flow.Ty.Array Int) || Flow.Ty.fits ty group_order_type) then
                Flow.Value.fail "E_GROUP_ORDER"
                  "Group order requires an integer array or an ordered/indices record.")
            (List.find_opt (fun (name, _, _) -> name = "order") args);
          Flow.Ty.geometry);
    body = (fun ~live:_ ~node args -> node "sop/with_group" args);
  }

(* Inside an inlined defnode body (the host adds the card's cook label as [$label]) a context
   operator is a cook-time source the kernel resolves from its cook context; elsewhere it is refused. *)
let context_label = Attribute_kernel.context_label
let provenance_sentinel = min_int

let context_source what ~live:_ ~node:_ (args : (string * (_, _) Flow.Value.t) list) =
  match List.assoc_opt context_label args with
  | Some (Flow.Value.Text _) when what = "Provenance base" ->
      (* a lowering fact: the sentinel the lowering of the body's sop/concat replaces by the base it
         allocates (Lower), as it did for the card *)
      Flow.Value.Int provenance_sentinel
  | Some (Flow.Value.Text _) ->
      let name, ty =
        match what with
        | "Context seed" -> ("sop/context_seed", Flow.Ty.Int)
        | "Provenance base" -> ("sop/provenance_base", Flow.Ty.Int)
        | "Context frame" -> ("sop/context_frame", Flow.Ty.Float)
        | _ -> ("sop/context_time", Flow.Ty.Float)
      in
      Flow.Value.Source (name, ty, args)
  | _ -> Flow.Value.fail "E_NODE_CONTEXT" (what ^ " is available inside a defnode body.")

let context_seed : Flow.Op.t =
  {
    attr with
    name = "sop/context_seed";
    ctx = Flow.Context.value;
    category = "Random";
    signature =
      {
        pos = [ ("namespace", Flow.Ty.Text) ];
        opt = [];
        rest = None;
        kw = [ ("salt", Flow.Ty.Int) ];
      };
    out = (fun _ -> Flow.Ty.Int);
    out_args = None;
    shape = Flow.Op.Scalar;
    check =
      (fun args ->
        (match List.assoc "namespace" args with
        | Flow.Value.Text namespace when String.trim namespace <> "" -> ()
        | _ -> Flow.Value.fail "E_SEED_NAMESPACE" "A context seed namespace must be nonblank.");
        match List.assoc_opt "salt" args with
        | None | Some (Flow.Value.Int _) -> ()
        | Some _ -> Flow.Value.fail "E_SEED_SALT" "A context seed salt must be an integer.");
    body = (fun ~live ~node args -> context_source "Context seed" ~live ~node args);
  }

let provenance_base : Flow.Op.t =
  {
    attr with
    name = "sop/provenance_base";
    ctx = Flow.Context.value;
    category = "Copy";
    signature = { pos = []; opt = []; rest = None; kw = [] };
    out = (fun _ -> Flow.Ty.Int);
    out_args = None;
    shape = Flow.Op.Scalar;
    check = (fun _ -> ());
    body = (fun ~live ~node args -> context_source "Provenance base" ~live ~node args);
  }

let context_frame : Flow.Op.t =
  {
    attr with
    name = "sop/context_frame";
    ctx = Flow.Context.value;
    category = "Random";
    signature = { pos = []; opt = []; rest = None; kw = [] };
    out = (fun _ -> Flow.Ty.Float);
    out_args = None;
    shape = Flow.Op.Scalar;
    check = (fun _ -> ());
    body = (fun ~live ~node args -> context_source "Context frame" ~live ~node args);
  }

let context_time : Flow.Op.t =
  {
    attr with
    name = "sop/context_time";
    ctx = Flow.Context.value;
    category = "Random";
    signature = { pos = []; opt = []; rest = None; kw = [] };
    out = (fun _ -> Flow.Ty.Float);
    out_args = None;
    shape = Flow.Op.Scalar;
    check = (fun _ -> ());
    body = (fun ~live ~node args -> context_source "Context time" ~live ~node args);
  }

let seed_for = Attribute_kernel.seed_for

let metadata_text args name =
  match List.assoc name args with
  | Flow.Value.Text text -> text
  | _ -> Flow.Value.fail "E_METADATA_TEXT" "Metadata names and patterns must be text."

let metadata_owner parse args =
  Option.iter
    (fun value ->
      match value with
      | Flow.Value.Text owner -> ignore (checked (parse owner))
      | _ -> Flow.Value.fail "E_METADATA_OWNER" "Metadata owner must be text.")
    (List.assoc_opt "owner" args)

let rewrite_check parse conflict args =
  metadata_owner parse args;
  Option.iter
    (function
      | Flow.Value.Text policy -> ignore (checked (conflict policy))
      | _ -> Flow.Value.fail "E_METADATA_CONFLICT" "Conflict policy must be text.")
    (List.assoc_opt "conflict" args);
  match
    Rdk.Attribute_pattern.compile_rewrite ~pattern:(metadata_text args "pattern")
      ~replacement:(metadata_text args "replacement")
  with
  | Ok _ -> ()
  | Error message -> Flow.Value.fail "E_METADATA_PATTERN" message

let rename_attr : Flow.Op.t =
  {
    delete_attr with
    name = "sop/rename_attr";
    category = "Metadata";
    signature =
      {
        pos = [ ("geometry", Flow.Ty.geometry); ("pattern", Text); ("replacement", Text) ];
        opt = [];
        rest = None;
        kw = [ ("owner", Text); ("conflict", Text) ];
      };
    choices =
      [
        ("owner", [ "point"; "vertex"; "primitive"; "detail"; "any" ]);
        ("conflict", [ "skip"; "error"; "overwrite" ]);
      ];
    check =
      (fun args ->
        rewrite_check Attribute_metadata.attribute_owner Attribute_metadata.attr_conflict args);
    body = (fun ~live:_ ~node args -> node "sop/rename_attr" args);
  }

let rename_group : Flow.Op.t =
  {
    rename_attr with
    name = "sop/rename_group";
    choices =
      [
        ("owner", [ "point"; "vertex"; "primitive"; "edge"; "any" ]);
        ("conflict", [ "skip"; "error"; "overwrite"; "union" ]);
      ];
    check =
      (fun args ->
        metadata_owner Attribute_metadata.group_owner args;
        Option.iter
          (function
            | Flow.Value.Text policy -> ignore (checked (Attribute_metadata.group_conflict policy))
            | _ -> Flow.Value.fail "E_METADATA_CONFLICT" "Conflict policy must be text.")
          (List.assoc_opt "conflict" args));
    body = (fun ~live:_ ~node args -> node "sop/rename_group" args);
  }

let swap_attr : Flow.Op.t =
  {
    rename_attr with
    name = "sop/swap_attr";
    signature =
      {
        rename_attr.signature with
        pos = [ ("geometry", Flow.Ty.geometry); ("source", Text); ("destination", Text) ];
        kw = [ ("owner", Text); ("method", Text) ];
      };
    choices =
      [
        ("owner", [ "point"; "vertex"; "primitive"; "detail" ]);
        ("method", [ "swap"; "move"; "copy" ]);
      ];
    check =
      (fun args ->
        metadata_owner Attribute_metadata.attribute_owner args;
        Option.iter
          (function
            | Flow.Value.Text method_
              when List.mem
                     (String.lowercase_ascii (String.trim method_))
                     [ "swap"; "move"; "copy" ] ->
                ()
            | _ -> Flow.Value.fail "E_METADATA_METHOD" "Attribute method is swap, move or copy.")
          (List.assoc_opt "method" args));
    body = (fun ~live:_ ~node args -> node "sop/swap_attr" args);
  }

let delete_attrs : Flow.Op.t =
  {
    delete_attr with
    name = "sop/delete_attrs";
    category = "Metadata";
    signature =
      {
        pos = [ ("geometry", Flow.Ty.geometry) ];
        opt = [];
        rest = None;
        kw =
          [
            ("reference", Flow.Ty.geometry);
            ("inverse", Bool);
            ("point", Text);
            ("vertex", Text);
            ("primitive", Text);
            ("detail", Text);
          ];
      };
    choices = [];
    check =
      (fun args ->
        List.iter
          (fun name ->
            Option.iter
              (function
                | Flow.Value.Text pattern when String.trim pattern = "" -> ()
                | Flow.Value.Text pattern -> (
                    match Rdk.Attribute_pattern.compile pattern with
                    | Ok _ -> ()
                    | Error message -> Flow.Value.fail "E_METADATA_PATTERN" message)
                | _ -> Flow.Value.fail "E_METADATA_PATTERN" "Delete patterns must be text.")
              (List.assoc_opt name args))
          [ "point"; "vertex"; "primitive"; "detail" ]);
    body = (fun ~live:_ ~node args -> node "sop/delete_attrs" args);
  }

let delete_group : Flow.Op.t =
  {
    delete_attrs with
    name = "sop/delete_group";
    signature =
      {
        pos = [ ("geometry", Flow.Ty.geometry); ("pattern", Text) ];
        opt = [];
        rest = None;
        kw = [ ("owner", Text); ("unused", Bool) ];
      };
    choices = [ ("owner", [ "point"; "vertex"; "primitive"; "edge"; "any" ]) ];
    check = (fun args -> metadata_owner Attribute_metadata.group_owner args);
    body = (fun ~live:_ ~node args -> node "sop/delete_group" args);
  }

let rename_edge : Flow.Op.t =
  {
    rename_attr with
    name = "sop/rename_edge";
    signature = { rename_attr.signature with kw = [] };
    choices = [];
    check = (fun _ -> ());
    body = (fun ~live:_ ~node args -> node "sop/rename_edge" args);
  }

let delete_edge : Flow.Op.t =
  {
    rename_edge with
    name = "sop/delete_edge";
    signature =
      { rename_edge.signature with pos = [ ("geometry", Flow.Ty.geometry); ("name", Text) ] };
    body = (fun ~live:_ ~node args -> node "sop/delete_edge" args);
  }

let attr_type : Flow.Op.t =
  {
    attr with
    name = "sop/attr_type";
    category = "Metadata";
    signature = { attr.signature with kw = [ ("owner", Text) ] };
    out = (fun _ -> Flow.Ty.Text);
    out_args = None;
    body = (fun ~live:_ ~node:_ args -> Flow.Value.Source ("sop/attr_type", Flow.Ty.Text, args));
  }

let attr_names : Flow.Op.t =
  {
    attr_type with
    name = "sop/attr_names";
    signature =
      { attr_type.signature with pos = [ ("geometry", Flow.Ty.geometry); ("pattern", Text) ] };
    out = (fun _ -> Flow.Ty.Array Text);
    check =
      (fun args ->
        metadata_owner Attribute_metadata.attribute_owner args;
        match Rdk.Attribute_pattern.compile (metadata_text args "pattern") with
        | Ok _ -> ()
        | Error message -> Flow.Value.fail "E_ATTR_PATTERN" message);
    body =
      (fun ~live:_ ~node:_ args -> Flow.Value.Source ("sop/attr_names", Flow.Ty.Array Text, args));
  }

let attr_entries_type =
  Flow.Ty.Record [ ("name", Array Text); ("type", Array Text); ("owner", Array Text) ]

let attr_entries : Flow.Op.t =
  {
    attr_type with
    name = "sop/attr_entries";
    signature = { attr_type.signature with pos = [ ("geometry", Flow.Ty.geometry) ] };
    out = (fun _ -> attr_entries_type);
    check = (fun args -> metadata_owner Attribute_metadata.attribute_owner args);
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Record
          (List.map
             (fun component ->
               ( component,
                 Flow.Value.Source
                   ( "sop/attr_entries",
                     Flow.Ty.Array Text,
                     args @ [ ("component", Flow.Value.Text component) ] ) ))
             [ "name"; "type"; "owner" ]));
  }

let glob_error : Flow.Op.t =
  {
    attr_type with
    name = "text/glob_error";
    ctx = Flow.Context.value;
    category = "Text";
    signature = { pos = [ ("pattern", Text) ]; opt = []; rest = None; kw = [] };
    choices = [];
    check = (fun _ -> ());
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Text
          (match Rdk.Attribute_pattern.compile (metadata_text args "pattern") with
          | Ok _ -> ""
          | Error message -> message));
  }

let glob_rewrite_error : Flow.Op.t =
  {
    glob_error with
    name = "text/glob_rewrite_error";
    signature = { glob_error.signature with pos = [ ("pattern", Text); ("replacement", Text) ] };
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Text
          (match
             Rdk.Attribute_pattern.compile_rewrite ~pattern:(metadata_text args "pattern")
               ~replacement:(metadata_text args "replacement")
           with
          | Ok _ -> ""
          | Error message -> message));
  }

let glob_rewrite_set_error : Flow.Op.t =
  {
    glob_error with
    name = "text/glob_rewrite_set_error";
    signature = { glob_error.signature with pos = [ ("pattern", Text); ("replacement", Text) ] };
    body =
      (fun ~live:_ ~node:_ args ->
        Flow.Value.Text
          (match
             Rdk.Attribute_pattern.compile_rewrite_set ~pattern:(metadata_text args "pattern")
               ~replacement:(metadata_text args "replacement")
           with
          | Ok _ -> ""
          | Error message -> message));
  }

(* Symbolic prototype arguments retain the ordinary typed Source boundary,
   so these pure text operations also run inside structured cooked loops. *)
let pure_source ty wrap name args run =
  if
    List.exists
      (fun (_, value) -> match value with Flow.Value.Source _ | Residual _ -> true | _ -> false)
      args
  then Flow.Value.Source (name, ty, args)
  else wrap (run ())

let text_source name args run = pure_source Flow.Ty.Text (fun s -> Flow.Value.Text s) name args run

let glob_rewrite : Flow.Op.t =
  {
    glob_error with
    name = "text/glob_rewrite";
    signature =
      {
        glob_error.signature with
        pos = [ ("pattern", Text); ("replacement", Text); ("text", Text) ];
      };
    body =
      (fun ~live:_ ~node:_ args ->
        text_source "text/glob_rewrite" args (fun () ->
            let pattern = metadata_text args "pattern"
            and replacement = metadata_text args "replacement"
            and text = metadata_text args "text" in
            checked (Attribute_metadata.glob_rewrite ~pattern ~replacement text)));
  }

let glob_rewrite_set : Flow.Op.t =
  {
    glob_error with
    name = "text/glob_rewrite_set";
    signature =
      {
        glob_error.signature with
        pos = [ ("pattern", Text); ("replacement", Text); ("text", Text) ];
      };
    body =
      (fun ~live:_ ~node:_ args ->
        text_source "text/glob_rewrite_set" args (fun () ->
            let pattern = metadata_text args "pattern"
            and replacement = metadata_text args "replacement"
            and text = metadata_text args "text" in
            checked (Attribute_metadata.glob_rewrite_set ~pattern ~replacement text)));
  }

let glob_rewrite_set_matches : Flow.Op.t =
  {
    glob_error with
    name = "text/glob_rewrite_set_matches";
    signature =
      {
        glob_error.signature with
        pos = [ ("pattern", Text); ("replacement", Text); ("text", Text) ];
      };
    out = (fun _ -> Flow.Ty.Bool);
    body =
      (fun ~live:_ ~node:_ args ->
        pure_source Flow.Ty.Bool
          (fun b -> Flow.Value.Bool b)
          "text/glob_rewrite_set_matches" args
          (fun () ->
            let pattern = metadata_text args "pattern"
            and replacement = metadata_text args "replacement"
            and text = metadata_text args "text" in
            checked (Attribute_metadata.glob_rewrite_set_matches ~pattern ~replacement text)));
  }

let glob_literal : Flow.Op.t =
  {
    glob_error with
    name = "text/glob_literal";
    signature = { glob_error.signature with pos = [ ("text", Text) ] };
    body =
      (fun ~live:_ ~node:_ args ->
        text_source "text/glob_literal" args (fun () ->
            checked (Attribute_metadata.glob_literal (metadata_text args "text"))));
  }

let group_name_valid : Flow.Op.t =
  {
    glob_error with
    name = "text/group_name_valid";
    signature = { glob_error.signature with pos = [ ("text", Text) ] };
    out = (fun _ -> Flow.Ty.Bool);
    body =
      (fun ~live:_ ~node:_ args ->
        pure_source Flow.Ty.Bool
          (fun b -> Flow.Value.Bool b)
          "text/group_name_valid" args
          (fun () -> Rdk.Group_ops.valid_group_name (metadata_text args "text")));
  }

let group_name_forced : Flow.Op.t =
  {
    glob_error with
    name = "text/group_name_forced";
    signature = { glob_error.signature with pos = [ ("text", Text) ] };
    body =
      (fun ~live:_ ~node:_ args ->
        text_source "text/group_name_forced" args (fun () ->
            Rdk.Group_ops.force_valid_group_name (metadata_text args "text")));
  }

let with_planes : Flow.Op.t =
  {
    mesh with
    name = "sop/with_planes";
    signature =
      {
        pos = [ ("geometry", Flow.Ty.geometry); ("attribute", Text); ("planes", planes_type) ];
        opt = [];
        rest = None;
        kw = [ ("owner", Text) ];
      };
    out_args = None;
    check = validate_args;
    body = (fun ~live:_ ~node args -> node "sop/with_planes" args);
  }

(* Weighted carry for one owner: new row i mixes source rows a.(i) and b.(i) at t.(i). *)
let mix_type =
  Flow.Ty.Record [ ("a", Flow.Ty.Array Int); ("b", Flow.Ty.Array Int); ("t", Flow.Ty.Array Float) ]

let remap : Flow.Op.t =
  {
    mesh with
    name = "sop/remap";
    out_args = None;
    signature =
      {
        pos = [ ("source", Flow.Ty.geometry); ("target", Flow.Ty.geometry) ];
        opt = [];
        rest = None;
        kw =
          [
            ("points", Flow.Ty.Array Int);
            ("vertices", Flow.Ty.Array Int);
            ("primitives", Flow.Ty.Array Int);
            ("point_mix", mix_type);
            ("vertex_mix", mix_type);
            ("primitive_mix", mix_type);
            ("positions", Text);
            ("edges", Record [ ("offsets", Array Int); ("values", Array Int) ]);
            ("lead", Bool);
            ("lead_groups", Bool);
          ];
      };
    choices = [ ("positions", [ "target"; "mapped" ]) ];
    check =
      (fun args ->
        List.iter
          (fun (name, mix) ->
            match (List.mem_assoc name args, List.mem_assoc mix args) with
            | true, false | false, true -> ()
            | false, false ->
                Flow.Value.fail "E_ARGS" ("sop/remap needs :" ^ name ^ " or :" ^ mix ^ ".")
            | true, true ->
                Flow.Value.fail "E_ARGS" ("sop/remap takes :" ^ name ^ " or :" ^ mix ^ ", not both."))
          [ ("points", "point_mix"); ("vertices", "vertex_mix"); ("primitives", "primitive_mix") ]);
    body = (fun ~live:_ ~node args -> node "sop/remap" args);
  }

let concat : Flow.Op.t =
  {
    mesh with
    name = "sop/concat";
    signature =
      {
        pos = [ ("sources", Flow.Ty.List Flow.Ty.geometry) ];
        opt = [];
        rest = None;
        kw = [ ("source_attribute", Text); ("source_base", Int); ("tag_base", Int) ];
      };
    out_args = None;
    check = (fun _ -> ());
    body = (fun ~live:_ ~node args -> node "sop/concat" args);
  }

let all =
  [
    attr;
    attr_checked;
    with_attr;
    has_attr;
    delete_attr;
    count;
    edges;
    topology;
    adjacency;
    triangles;
    components;
    shortest_paths;
    forest;
    color;
    chains;
    mesh;
    mesh_planes;
    remap;
    concat;
    attr_planes;
    with_planes;
    group;
    group_order;
    group_names;
    group_entries;
    with_group;
    context_seed;
    provenance_base;
    context_frame;
    context_time;
    rename_attr;
    swap_attr;
    delete_attrs;
    rename_group;
    delete_group;
    rename_edge;
    delete_edge;
    attr_type;
    attr_names;
    attr_entries;
    glob_error;
    glob_rewrite_error;
    glob_rewrite;
    glob_rewrite_set_error;
    glob_rewrite_set;
    glob_rewrite_set_matches;
    glob_literal;
    group_name_valid;
    group_name_forced;
  ]

type lowering =
  | Geometry_arrays
  | Metadata
  | Attribute_write
  | Group_write
  | Attribute_delete
  | Curve
  | Zone
  | Resource

let lowering = function
  | "sop/mesh" | "sop/mesh_planes" | "sop/remap" | "sop/concat" | "sop/with_planes" ->
      Some Geometry_arrays
  | "sop/rename_attr" | "sop/swap_attr" | "sop/delete_attrs" | "sop/rename_group"
  | "sop/delete_group" | "sop/rename_edge" | "sop/delete_edge" ->
      Some Metadata
  | "sop/with_attr" -> Some Attribute_write
  | "sop/with_group" -> Some Group_write
  | "sop/delete_attr" -> Some Attribute_delete
  | "sop/curve" -> Some Curve
  | "zone/points" | "zone/pieces" -> Some Zone
  | "image/load" | "image/render" | "image/map" | "exact" -> Some Resource
  | _ -> None
