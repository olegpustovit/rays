let glob_rewrite ~pattern ~replacement text =
  Result.map_error
    (Diagnostic.error ~code:"E_METADATA_PATTERN")
    (Result.map
       (fun rewrite -> Option.value ~default:text (Rdk.Attribute_pattern.rewrite rewrite text))
       (Rdk.Attribute_pattern.compile_rewrite ~pattern ~replacement))

let glob_rewrite_set ~pattern ~replacement text =
  Result.map_error
    (Diagnostic.error ~code:"E_METADATA_PATTERN")
    (Result.map
       (fun rewrite -> Option.value ~default:text (Rdk.Attribute_pattern.rewrite_set rewrite text))
       (Rdk.Attribute_pattern.compile_rewrite_set ~pattern ~replacement))

let glob_rewrite_set_matches ~pattern ~replacement text =
  Result.map_error
    (Diagnostic.error ~code:"E_METADATA_PATTERN")
    (Result.map
       (fun rewrite -> Option.is_some (Rdk.Attribute_pattern.rewrite_set rewrite text))
       (Rdk.Attribute_pattern.compile_rewrite_set ~pattern ~replacement))

let glob_literal text =
  if text = "" then
    Error (Diagnostic.error ~code:"E_METADATA_PATTERN" "A literal glob must not be empty.")
  else
    let out = Buffer.create (String.length text) in
    String.iter
      (fun c ->
        (match c with
        | '*' | '?' | '[' | ']' | '\\' | '^' | ' ' | '\t' | '\n' | '\r' | '\012' ->
            Buffer.add_char out '\\'
        | _ -> ());
        Buffer.add_char out c)
      text;
    Ok (Buffer.contents out)

let delete ?guard ~owner ~pattern input =
  let literal =
    pattern <> ""
    && not
         (String.exists
            (function
              | '*' | '?' | '[' | '\\' | '^' | ' ' | '\t' | '\n' | '\r' | '\012' -> true
              | _ -> false)
            pattern)
  in
  let delete geometry cancel =
    let pattern_for selected = if owner = selected then pattern else "" in
    Rdk.Attribute_ops.delete
      {
        cancel = Some cancel;
        reference = None;
        delete_non_selected = false;
        point_pattern = Some (pattern_for "point");
        vertex_pattern = Some (pattern_for "vertex");
        primitive_pattern = Some (pattern_for "primitive");
        detail_pattern = Some (pattern_for "detail");
      }
      geometry
  in
  Sop.Node.Build.make_geometry ~operation:"flow.delete_attr" ~version:1
    ~parameters:
      (Flow.Lisp.flat
         (Flow.Syntax.make (Vec [ Flow.Syntax.make (Str owner); Flow.Syntax.make (Str pattern) ]))
      ^ Option.fold ~none:"" ~some:(fun (key, _, _, _) -> ";when " ^ key) guard)
    ~cook_mode:(Sop.Node.Duplicate_input 0)
    ~dependencies:
      (Option.fold ~none:Sop.Context.Dependencies.static
         ~some:(fun (_, dependencies, _, _) -> dependencies)
         guard)
    ~inputs:
      (Array.of_list (input :: Option.fold ~none:[] ~some:(fun (_, _, extra, _) -> extra) guard))
    (fun ~node_id:_ context inputs ->
      (* a guarded deletion decides as it cooks: the guard reads the frame or cooked data *)
      match
        Option.fold ~none:(Ok true) ~some:(fun (_, _, _, decide) -> decide context inputs) guard
      with
      | Error d -> Error d
      | Ok false -> Ok Sop.Node.Build.{ geometry = inputs.(0); instances = None }
      | Ok true -> (
          match delete inputs.(0) (Sop.Context.cancel_token context) with
          | Ok geometry -> Ok Sop.Node.Build.{ geometry; instances = None }
          | Error error ->
              Error (Diagnostic.error ~code:"E_ATTR_DELETE" (Rdk.Error.to_string error))))
  |> Sop.Node.Build.with_facts
       Sop.Node.
         {
           cook_mode = Duplicate_input 0;
           elementwise = Points;
           reads = (if literal then [] else [ "*" ]);
           writes = (if literal then [ pattern ] else [ "*" ]);
           topology = Preserved;
           exact = true;
         }

module E = Flow.Eval
module A = Rdk.Attribute
module G = Rdk.Geometry
module N = Sop.Node

let error code message = Error (Diagnostic.error ~code message)

let attribute_owner text =
  match String.lowercase_ascii (String.trim text) with
  | "point" | "points" -> Ok (Some A.Point)
  | "vertex" | "vertices" -> Ok (Some A.Vertex)
  | "primitive" | "primitives" -> Ok (Some A.Primitive)
  | "detail" -> Ok (Some A.Detail)
  | "any" | "*" -> Ok None
  | _ -> error "E_ATTR_OWNER" (Printf.sprintf "Unknown attribute owner %S." text)

let group_owner text =
  match String.lowercase_ascii (String.trim text) with
  | "point" | "points" -> Ok (Some Rdk.Group_ops.Group_points)
  | "vertex" | "vertices" -> Ok (Some Rdk.Group_ops.Group_vertices)
  | "primitive" | "primitives" -> Ok (Some Rdk.Group_ops.Group_primitives)
  | "edge" | "edges" -> Ok (Some Rdk.Group_ops.Group_edges)
  | "any" | "*" -> Ok None
  | _ -> error "E_GROUP_OWNER" (Printf.sprintf "Unknown group owner %S." text)

let attr_conflict text =
  match String.lowercase_ascii (String.trim text) with
  | "skip" -> Ok Rdk.Attribute_lifecycle.Attribute_rename_skip
  | "error" -> Ok Rdk.Attribute_lifecycle.Attribute_rename_error
  | "overwrite" -> Ok Rdk.Attribute_lifecycle.Attribute_rename_overwrite
  | _ -> error "E_METADATA_CONFLICT" (Printf.sprintf "Unknown attribute rename conflict %S." text)

let group_conflict text =
  match String.lowercase_ascii (String.trim text) with
  | "skip" -> Ok Rdk.Group_ops.Rename_skip
  | "error" -> Ok Rdk.Group_ops.Rename_error
  | "overwrite" -> Ok Rdk.Group_ops.Rename_overwrite
  | "union" -> Ok Rdk.Group_ops.Rename_union
  | _ -> error "E_METADATA_CONFLICT" (Printf.sprintf "Unknown group rename conflict %S." text)

let checked = function
  | Ok value -> value
  | Error (diagnostic : Diagnostic.t) -> Flow.Value.raise_diagnostic diagnostic

let structured_error error =
  Diagnostic.error ~code:(Rdk.Error.code error) ~cause:(Rdk.Error.to_string error)
    ~hints:(Rdk.Error.hints error)
    (Rdk.Error.operation error ^ " could not produce valid geometry")

let node operation parameters inputs cook =
  N.Build.make_geometry ~operation ~version:1 ~parameters ~cook_mode:N.Generic
    ~dependencies:Sop.Context.Dependencies.static ~inputs:(Array.of_list inputs)
    (fun ~node_id:_ context inputs ->
      Result.map (fun geometry -> N.Build.{ geometry; instances = None }) (cook context inputs))
  |> N.Build.with_facts
       N.
         {
           cook_mode = Generic;
           elementwise = None;
           reads = [ "*" ];
           writes = [ "*" ];
           topology = Preserved;
           exact = true;
         }

let key cells =
  Flow.Lisp.flat (Flow.Syntax.make (Vec (List.map (fun cell -> Flow.Syntax.make (Str cell)) cells)))

let rename ~owner ~pattern ~replacement ~conflict input =
  let owner = checked (attribute_owner owner) and conflict = checked (attr_conflict conflict) in
  let rule =
    Rdk.Attribute_lifecycle.
      {
        rename_attribute_owner = owner;
        rename_attribute_pattern = pattern;
        rename_attribute_replacement = replacement;
        rename_attribute_conflict = conflict;
      }
  in
  node "flow.rename_attr"
    (key
       [
         pattern;
         replacement;
         (match owner with
         | None -> "any"
         | Some A.Point -> "point"
         | Some Vertex -> "vertex"
         | Some Primitive -> "primitive"
         | Some Detail -> "detail");
         (match conflict with
         | Attribute_rename_skip -> "skip"
         | Attribute_rename_error -> "error"
         | Attribute_rename_overwrite -> "overwrite");
       ])
    [ input ]
    (fun context inputs ->
      Result.map_error structured_error
        (Rdk.Attribute_ops.rename
           ~cancel:(Sop.Context.cancel_token context)
           ~rules:[ rule ] inputs.(0)))

let swap ~owner ~source ~destination ~method_ input =
  let owner =
    match checked (attribute_owner owner) with
    | Some owner -> owner
    | None -> Flow.Value.fail "E_ATTR_OWNER" "Attribute swap requires one owner."
  in
  let method_ =
    match String.lowercase_ascii (String.trim method_) with
    | "swap" -> Rdk.Attribute_lifecycle.Attribute_swap
    | "move" -> Attribute_move
    | "copy" -> Attribute_copy
    | _ -> Flow.Value.fail "E_METADATA_METHOD" "Attribute method is swap, move or copy."
  in
  let rule =
    Rdk.Attribute_lifecycle.
      {
        swap_attribute_owner = owner;
        swap_attribute_source = source;
        swap_attribute_destination = destination;
        swap_attribute_method = method_;
      }
  in
  node "flow.swap_attr"
    (key
       [
         source;
         destination;
         (match owner with
         | A.Point -> "point"
         | Vertex -> "vertex"
         | Primitive -> "primitive"
         | Detail -> "detail");
         (match method_ with
         | Attribute_swap -> "swap"
         | Attribute_move -> "move"
         | Attribute_copy -> "copy");
       ])
    [ input ]
    (fun context inputs ->
      Result.map_error structured_error
        (Rdk.Attribute_ops.swap
           ~cancel:(Sop.Context.cancel_token context)
           ~rules:[ rule ] inputs.(0)))

let delete_attributes ?reference ~inverse ~point ~vertex ~primitive ~detail input =
  let inputs = input :: Option.to_list reference in
  node "flow.delete_attrs"
    (key [ string_of_bool inverse; point; vertex; primitive; detail ])
    inputs
    (fun context inputs ->
      Result.map_error structured_error
        (Rdk.Attribute_ops.delete
           {
             cancel = Some (Sop.Context.cancel_token context);
             reference = (if Array.length inputs = 2 then Some inputs.(1) else None);
             delete_non_selected = inverse;
             point_pattern = Some point;
             vertex_pattern = Some vertex;
             primitive_pattern = Some primitive;
             detail_pattern = Some detail;
           }
           inputs.(0)))

let rename_group ~owner ~pattern ~replacement ~conflict input =
  let selected = checked (group_owner owner) and policy = checked (group_conflict conflict) in
  let rule =
    Rdk.Group_ops.
      {
        rename_owner = selected;
        rename_pattern = pattern;
        rename_replacement = replacement;
        rename_conflict = policy;
      }
  in
  node "flow.rename_group"
    (key [ owner; pattern; replacement; conflict ])
    [ input ]
    (fun _ inputs ->
      Result.map_error structured_error (Rdk.Group_ops.rename ~rules:[ rule ] inputs.(0)))

let delete_group ~owner ~pattern ~unused input =
  let selected = checked (group_owner owner) in
  let rule = Rdk.Group_ops.{ delete_owner = selected; delete_pattern = pattern } in
  node "flow.delete_group"
    (key [ owner; pattern; string_of_bool unused ])
    [ input ]
    (fun _ inputs ->
      Result.map_error structured_error
        (Rdk.Group_ops.delete { rules = [ rule ]; delete_unused = unused } inputs.(0)))

let rename_edge ~from ~into input =
  node "flow.rename_edge"
    (key [ from; into ])
    [ input ]
    (fun _ inputs ->
      Result.map_error
        (fun message ->
          Diagnostic.error ~code:"rename_edge_group_failed" ~cause:message
            "rename_edge_group could not produce valid geometry")
        (G.rename_edge_group ~from ~into inputs.(0)))

let delete_edge ~name input =
  node "flow.delete_edge" (key [ name ]) [ input ] (fun _ inputs ->
      Ok (G.without_edge_group name inputs.(0)))

let storage_kind = function
  | A.Float _ -> "float"
  | Int _ -> "int"
  | Float2 _ -> "vec2"
  | Float3 _ -> "vec3"
  | Float4 _ -> "vec4"
  | Text _ -> "text"
  | Float_array _ -> "float_array"
  | Int_array _ -> "int_array"

(* an attribute's type without reading its storage (a device-resident one stays there) *)
let kind ~owner ~name geometry =
  if owner = A.Point && name = "P" then Some "vec3"
  else
    Option.map
      (fun attribute ->
        match A.kind_name attribute with
        | "float2" -> "vec2"
        | "float3" -> "vec3"
        | "float4" -> "vec4"
        | kind -> kind)
      (G.find_attribute ~owner name geometry)

let type_of ~owner ~name geometry =
  match kind ~owner ~name geometry with
  | Some kind -> Ok (E.Text kind)
  | None -> error "E_ATTR_TYPE" (Printf.sprintf "Attribute %S is missing." name)

let names ~owner ~pattern geometry =
  Result.bind
    (Result.map_error
       (Diagnostic.error ~code:"E_ATTR_PATTERN")
       (Rdk.Attribute_pattern.compile pattern))
    (fun compiled ->
      let names =
        G.View.attributes geometry |> Array.to_seq
        |> Seq.filter_map (fun attribute ->
            if
              A.owner attribute = owner && Rdk.Attribute_pattern.matches compiled (A.name attribute)
            then Some (A.name attribute)
            else None)
        |> Array.of_seq
      in
      Ok (E.Text_array names))

(* Parallel name/type/owner columns of the stored attributes, in native attribute order. *)
let entries ~owner ~component geometry =
  let owner_text attribute =
    match A.owner attribute with
    | A.Point -> "point"
    | Vertex -> "vertex"
    | Primitive -> "primitive"
    | Detail -> "detail"
  in
  let column =
    match component with
    | "name" -> Ok A.name
    | "type" -> Ok (fun attribute -> storage_kind (A.View.storage attribute))
    | "owner" -> Ok owner_text
    | _ -> error "E_ATTR_ENTRIES" "Attribute entry component is name, type or owner."
  in
  Result.map
    (fun column ->
      E.Text_array
        (G.View.attributes geometry |> Array.to_seq
        |> Seq.filter (fun attribute -> owner = None || owner = Some (A.owner attribute))
        |> Seq.map column |> Array.of_seq))
    column

let resolve ~geometry = function
  | E.Source ("sop/attr_entries", _, args) -> (
      match List.assoc_opt "geometry" args with
      | Some (E.Deferred (ty, id)) when Flow.Ty.is_geometry ty -> (
          match geometry id with
          | None -> error "E_DATA_SOURCE" "Metadata source has not been cooked."
          | Some geometry ->
              let owner =
                match List.assoc_opt "owner" args with
                | None -> Ok (Some A.Point)
                | Some (E.Text owner) -> attribute_owner owner
                | _ -> error "E_ATTR_OWNER" "Attribute owner must be text."
              in
              Result.bind owner (fun owner ->
                  match List.assoc_opt "component" args with
                  | Some (E.Text component) -> entries ~owner ~component geometry
                  | _ -> error "E_ATTR_ENTRIES" "Attribute entry component is name, type or owner.")
          )
      | _ -> error "E_DATA_SOURCE" "Metadata source requires cooked geometry.")
  | E.Source ((("sop/attr_type" | "sop/attr_names") as operation), _, args) -> (
      match List.assoc_opt "geometry" args with
      | Some (E.Deferred (ty, id)) when Flow.Ty.is_geometry ty -> (
          match geometry id with
          | None -> error "E_DATA_SOURCE" "Metadata source has not been cooked."
          | Some geometry ->
              let owner =
                match List.assoc_opt "owner" args with
                | None -> Ok (Some A.Point)
                | Some (E.Text owner) -> attribute_owner owner
                | _ -> error "E_ATTR_OWNER" "Attribute owner must be text."
              in
              Result.bind owner (function
                | None -> error "E_ATTR_OWNER" "Metadata queries require one owner."
                | Some owner -> (
                    match
                      ( operation,
                        List.assoc_opt
                          (if operation = "sop/attr_type" then "attribute" else "pattern")
                          args )
                    with
                    | "sop/attr_type", Some (E.Text name) -> type_of ~owner ~name geometry
                    | "sop/attr_names", Some (E.Text pattern) -> names ~owner ~pattern geometry
                    | _ -> error "E_ATTR_NAME" "Metadata names and patterns must be text.")))
      | _ -> error "E_DATA_SOURCE" "Metadata source requires cooked geometry.")
  | _ -> error "E_DATA_SOURCE" "Unknown metadata source."
