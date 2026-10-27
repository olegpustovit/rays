open Ppxlib

let declaration fields attributes =
  let source = "type parameters = { " ^ fields ^ " } " ^ attributes in
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "metadata.ml";
  match Parse.implementation lexbuf with
  | [{ pstr_desc = Pstr_type (_, [declaration]); _ }] -> declaration
  | _ -> assert false

let reject expected check =
  match check () with
  | _ -> failwith ("accepted invalid declaration: " ^ expected)
  | exception error ->
      (match Location.Error.of_exn error with
       | Some error ->
           assert (Location.Error.message error = expected);
           assert ((Location.Error.get_location error).loc_start.pos_fname = "metadata.ml")
       | None -> raise error)

let float name group folder = Printf.sprintf
    "%s : float [@sop.default 0.] [@sop.min (-1.)] [@sop.max 1.] %s %s;"
    name group folder

let vector = List.map (fun name -> float name "[@sop.vec3 \"center\"]" "")
    ["center_x"; "center_y"; "center_z"] |> String.concat "\n"

let node key slots = Printf.sprintf
    "[@@sop.node_key %S] [@@sop.node_label \"Fixture\"]\n\
     [@@sop.node_category \"Test\"] [@@sop.node_inputs 2] %s" key slots

let floating_calls declaration =
  let count = ref 0 in
  let visitor = object
    inherit Ast_traverse.iter as super
    method! expression expression =
      (match expression.pexp_desc with
       | Pexp_apply ({pexp_desc=Pexp_ident {txt=Longident.Ldot
           (Longident.Lident "Param", "floating"); _}; _}, _) ->
           incr count
       | _ -> ());
      super#expression expression
  end in
  List.iter visitor#structure_item (Ppx_rays.generate_type declaration);
  !count

let () =
  let lexbuf = Lexing.from_string
      "type parameters = unit [@@sop.node_key \"empty\"] [@@sop.node_label \"Empty\"]
       [@@sop.node_category \"Test\"] [@@sop.node_inputs 1]
" in
  let unit_declaration = match Parse.implementation lexbuf with
    | [{ pstr_desc = Pstr_type (_, [declaration]); _ }] -> declaration
    | _ -> assert false in
  assert (List.length (Ppx_rays.generate_type unit_declaration) = 2);
  ignore (Ppx_rays.generate_node_type unit_declaration);
  let valid = declaration vector "" in
  assert (List.length (Ppx_rays.generate_type valid) = 2);
  assert (floating_calls valid = 1);
  let pair minimum_a minimum_b = declaration
      (Printf.sprintf
        "a : float [@sop.default 0.] [@sop.min %s] [@sop.max 1.];\n\
         b : float [@sop.default 0.] [@sop.min %s] [@sop.max 1.];"
        minimum_a minimum_b) "" in
  assert (floating_calls (pair "(-0.)" "0.") = 2);
  assert (floating_calls (pair "(computed_min ())" "(computed_min ())") = 2);
  let bad_vector = "sop.vec3 \"center\" requires exactly three consecutive float fields in one folder" in
  List.iter (fun fields -> reject bad_vector (fun () ->
      Ppx_rays.generate_type (declaration fields "")))
    [float "x" "[@sop.vec3 \"center\"]" "";
     vector ^ float "w" "[@sop.vec3 \"center\"]" "";
     float "x" "[@sop.vec3 \"center\"]" ""
       ^ float "gap" "" ""
       ^ float "y" "[@sop.vec3 \"center\"]" ""
       ^ float "z" "[@sop.vec3 \"center\"]" "";
     float "x" "[@sop.vec3 \"center\"]" ""
       ^ float "y" "[@sop.vec3 \"center\"]" "[@sop.folder \"Other\"]"
       ^ float "z" "[@sop.vec3 \"center\"]" "";
     "x : bool [@sop.default false] [@sop.vec3 \"center\"];"
       ^ float "y" "[@sop.vec3 \"center\"]" ""
       ^ float "z" "[@sop.vec3 \"center\"]" "";
     vector ^ "hidden : float [@sop.default 0.] [@sop.ignore] [@sop.vec3 \"center\"];" ];
  reject "sop.vec3 \"center\" clashes with a parameter name" (fun () ->
    Ppx_rays.generate_type (declaration (vector ^ float "center" "" "") ""));
  reject "sop.vec3 \"\" must not be blank" (fun () ->
    Ppx_rays.generate_type (declaration (float "x" "[@sop.vec3 \"\"]" "") ""));
  List.iter (fun key -> reject
      (Printf.sprintf "sop.node_key must match [a-z][a-z0-9_]*, not %S" key)
      (fun () -> Ppx_rays.node_metadata (declaration vector (node key ""))))
    ["Upper"; "bad-key"; "9start"; "white space"];
  List.iter (fun slots -> reject
      "sop.node_slots requires 2 distinct names matching [a-z][a-z0-9_]*"
      (fun () -> Ppx_rays.node_metadata
        (declaration vector (node "fixture" ("[@@sop.node_slots " ^ slots ^ "]")))))
    ["\"input\""; "\"input, input\""; "\"input, Target\""; "\"input, \""];
  reject "sop.node_slots name \"center\" clashes with a parameter name" (fun () ->
    Ppx_rays.node_metadata (declaration vector
      (node "fixture" "[@@sop.node_slots \"center, target\"]")));
  reject "sop.node_slots name \"in0\" clashes with a parameter name" (fun () ->
    Ppx_rays.node_metadata (declaration (float "in0" "" "") (node "fixture" "")));
  let _, _, _, _, _, _, slots, _ = Ppx_rays.node_metadata
      (declaration vector (node "fixture" "")) in
  assert (slots = ["in0"; "in1"]);
  let _, _, _, _, _, _, slots, _ = Ppx_rays.node_metadata
      (declaration vector (node "fixture" "[@@sop.node_slots \"input, target\"]")) in
  assert (slots = ["input"; "target"]);
  List.iter (fun types -> reject "sop.node_types requires 2 valid type names" (fun () ->
    Ppx_rays.generate_node_type (declaration vector
      (node "fixture" ("[@@sop.node_types " ^ types ^ "]")))))
    ["\"geometry\""; "\"geometry, bad type\""; "\"geometry, Image\""];
  ignore (Ppx_rays.generate_node_type (declaration vector
    (node "fixture" "[@@sop.node_types \"geometry, image\"]")));
  let keyword = declaration vector (node "fixture"
      "[@@sop.node_slots \"input, field\"] [@@sop.node_keywords \"field\"]
       [@@sop.node_types [\"geometry\"; \"fn(vec3,float)->float\"]]") in
  ignore (Ppx_rays.generate_node_type keyword);
  List.iter (fun attrs -> reject "sop.node_keywords must name distinct required slots"
      (fun () -> Ppx_rays.generate_node_type (declaration vector (node "fixture" attrs))))
    ["[@@sop.node_keywords \"missing\"]";
     "[@@sop.node_keywords \"in1, in1\"]";
     "[@@sop.node_keywords \"in1\"] [@@sop.node_optional \"1\"]"];
  List.iter (fun attributes -> reject
      "sop.node_rest must name the final slot after required inputs" (fun () ->
        Ppx_rays.node_metadata (declaration vector (node "fixture" attributes))))
    ["[@@sop.node_rest 0]"; "[@@sop.node_rest 2]";
     "[@@sop.node_rest 1] [@@sop.node_optional \"0\"]"];
  let rest = declaration vector (node "fixture"
      "[@@sop.node_rest 1]") in
  let _, _, _, _, _, _, _, rest_slot = Ppx_rays.node_metadata rest in
  assert (rest_slot = Some 1);
  ignore (Ppx_rays.generate_node_type rest)
;
  let optional_rest=declaration vector (node "fixture"
    "[@@sop.node_rest 1] [@@sop.node_optional \"0,1\"]") in
  let _,_,_,_,_,optional,_,rest_slot=Ppx_rays.node_metadata optional_rest in
  assert (optional=[0;1] && rest_slot=Some 1);
  ignore (Ppx_rays.generate_node_type optional_rest);
  let facts = "[@@sop.node_facts {elementwise = Sop.Node.Points; reads = [\"P\"];\n\
    writes = [\"P\"]; topology = Sop.Node.Preserved; exact = false}]" in
  ignore (Ppx_rays.generate_node_type (declaration vector (node "fixture" facts)));
  List.iter (fun fields -> reject
      "sop.node_facts requires elementwise, reads, writes, topology and exact exactly once"
      (fun () -> Ppx_rays.generate_node_type (declaration vector
        (node "fixture" ("[@@sop.node_facts {" ^ fields ^ "}]")))))
    ["reads = [\"P\"]; writes = [\"P\"]";
     "elementwise = Points; reads = []; writes = []; topology = Preserved; exact = true; cook_mode = Generator";
     "elementwise = Points; reads = []; writes = []; topology = Preserved; exact = true; exact = false"];
  reject "sop.node_facts expects {elementwise; reads; writes; topology; exact}" (fun () ->
    Ppx_rays.generate_node_type (declaration vector (node "fixture" "[@@sop.node_facts make_facts]")))
