module S = Flow.Syntax
module W = Flow.Workspace
module P = Flow_graph.Projection
module E = Flow_graph.Flow_edit

let ok = Result.get_ok
let check condition message = if not condition then failwith message
let parse text = match S.parse text with
  | Ok forms -> forms | Error d -> failwith (Diagnostic.to_string d)
let checked catalog forms = match W.check ~ops:Flow_sop.Operators.all catalog forms with
  | Some w, _ -> w
  | None, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))

let field ?vec3 name label kind default =
  Param.field ~name ~label ~description:("Edit " ^ label) ~folder:["Shape"]
    ~primary:true ~unit:"m" ?vec3 ~kind ~default
    ~get:(fun values -> List.assoc name values)
    ~set:(fun value values -> List.map (fun (key, old) ->
      key, if key = name then value else old) values) ()

(* OCaml schema is independent of the declaration, as a metadata oracle. *)
let schema = Param.schema ~name:"test_card" ~default:[
  "distance", 0.5; "scale_x", 1.; "scale_y", 2.; "scale_z", 3.] [
  field "distance" "Distance"
    (Param.floating ~min:(-10.) ~max:10. ~hard_min:(-20.) ~hard_max:20. ()) 0.5;
  field ~vec3:("scale", 0) "scale_x" "Scale X"
    (Param.floating ~min:0. ~max:10. ()) 1.;
  field ~vec3:("scale", 1) "scale_y" "Scale Y"
    (Param.floating ~min:0. ~max:10. ()) 2.;
  field ~vec3:("scale", 2) "scale_z" "Scale Z"
    (Param.floating ~min:0. ~max:10. ()) 3.]

let definition maximum = Printf.sprintf {|(defnode sop/test_card :label "Test card" :category ["Test"]
  [(geo : geometry)
   (distance : float 0.5 :label "Distance" :description "Edit Distance"
     :folder ["Shape"] :primary true :unit "m" :min -10 :max %d :hard_min -20 :hard_max 20)
   (scale_x : float 1 :label "Scale X" :description "Edit Scale X"
     :folder ["Shape"] :primary true :unit "m" :min 0 :max 10 :vec3 ["scale" 0])
   (scale_y : float 2 :label "Scale Y" :description "Edit Scale Y"
     :folder ["Shape"] :primary true :unit "m" :min 0 :max 10 :vec3 ["scale" 1])
   (scale_z : float 3 :label "Scale Z" :description "Edit Scale Z"
     :folder ["Shape"] :primary true :unit "m" :min 0 :max 10 :vec3 ["scale" 2])]
  geo)|} maximum

let graph = {|(graph g :context sop
  (let* [src (sop/grid :columns 2 :rows 2)
         amount 2.0
         card (sop/test_card src :distance 2.0 :scale [1.0 2.0 3.0])]
    card))|}
let source maximum = "(workspace w " ^ definition maximum ^ " " ^ graph ^ ")"
let catalog factories = ok (Flow_sop.Catalog.of_factories
  ~version:Flow_sop.Manifest.version factories)
let base = catalog Sop.Nodes.factories

let native_node values input =
  let rec rebuild ~label:_ ~inputs values =
    Sop.Node.parameterize ~schema ~values ~rebuild (List.hd inputs) in
  rebuild ~label:"Test card" ~inputs:[input] values
let factory = Sop.Edit_graph.factory ~key:"test_card" ~label:"Test card" ~category:["Test"]
  ~slots:["geo"] ~fields:(Param.view schema (Param.default schema)) ~arity:1
  (function [input] -> native_node (Param.default schema) input | _ -> assert false)
let native_catalog = catalog (factory :: Sop.Nodes.factories)

(* Form ids and source spans vary with the declaration prefix; authored content
   and all card metadata must remain structurally equal. *)
let normalize (card : P.node) = {card with rows = List.map (fun (row : P.row) ->
  {row with expr = Option.map (fun expr -> List.hd (parse (Flow.Lisp.flat expr))) row.expr}) card.rows}
let card catalog workspace = Option.get (P.find (P.of_graph catalog workspace "g") ["g"; "card"])

let () =
  let forms = parse (source 10) in
  let workspace = checked base forms
  and native = checked native_catalog (parse ("(workspace w " ^ graph ^ ")")) in
  let actual = card base workspace and expected = card native_catalog native in
  check (normalize actual = normalize expected) "Lisp and OCaml cards differ";
  check (List.map (fun (row : P.row) -> row.label) actual.rows = ["geo"; "distance"; "scale"])
    "Lisp card omits its schema rows";
  check ((List.hd actual.rows).head && P.bypassable actual) "Lisp header geometry input differs";
  check (P.lines Full (normalize actual) = P.lines Full (normalize expected))
    "Lisp full card and inspector folder rows differ";
  let node = Lisp_sop.workspace_node (source 10) in
  let native_fields = Param.view schema ["distance", 2.; "scale_x", 1.; "scale_y", 2.; "scale_z", 3.] in
  check (Sop.Node.parameter_fields node = native_fields) "Lisp inspector metadata differs from OCaml schema";
  let edited, changed = ok (E.apply_checked ~ops:Flow_sop.Operators.all base forms
    (Set_arg {node = actual.path; key = Kw "distance"; sub = []; value = S.make (Num "3.0")})) in
  let row = List.find (fun (row : P.row) -> row.key = E.Kw "distance") (card base changed).rows in
  check (Option.map Flow.Lisp.flat row.expr = Some "3.0") "scalar scrub did not project back";
  let _, changed = ok (E.apply_checked ~ops:Flow_sop.Operators.all base edited
    (Set_arg {node = actual.path; key = Kw "scale"; sub = [1]; value = S.make (Num "4.0")})) in
  let row = List.find (fun (row : P.row) -> row.key = E.Kw "scale") (card base changed).rows in
  check (Option.map Flow.Lisp.flat row.expr = Some "[1.0 4.0 3.0]") "vector scrub did not project back";
  let _, connected = ok (E.apply_checked ~ops:Flow_sop.Operators.all base forms
    (Connect {node = actual.path; key = Kw "distance"; src = "amount"; iter = false})) in
  check (P.sources (List.find (fun (row : P.row) -> row.key = E.Kw "distance")
    (card base connected).rows) = ["amount"]) "parameter drive did not project back";
  check (Result.is_error (E.apply_checked ~ops:Flow_sop.Operators.all base forms
    (Set_arg {node = actual.path; key = Kw "distance"; sub = []; value = S.make (Num "21")})))
    "parameter edit ignored hard bound";
  check (not (P.same_graph workspace (checked base (parse (source 9))) "g"))
    "changed Lisp schema retained stale projection";
  let _, bypassed = ok (E.apply_checked ~ops:Flow_sop.Operators.all base forms
    (Toggle_bypass {node = actual.path})) in
  check ((card base bypassed).bypass) "Lisp card bypass gesture failed";
  let nested = parse ("(workspace w " ^ definition 10 ^ {| (graph g :context sop
    (sop/test_card (sop/test_card (sop/grid) :distance 1.0) :distance 2.0)))|}) in
  let workspace = checked base nested in
  let path = ["g"; E.nested_leaf "@result" (Pos 0)] in
  let projected = P.of_graph base workspace "g" in
  check (List.length projected.nodes = 3) "nested Lisp node lacks a graph card";
  let node = Option.get (P.find projected path) in
  check (P.anonymous node && List.length node.rows = 3) "nested Lisp card lost typed parameter rows";
  let _, changed = ok (E.apply_checked ~ops:Flow_sop.Operators.all base nested
    (Set_arg {node = path; key = Kw "distance"; sub = []; value = S.make (Num "4.0")})) in
  let node = Option.get (P.find (P.of_graph base changed "g") path) in
  check (Option.map Flow.Lisp.flat (List.find (fun (row : P.row) -> row.key = Kw "distance") node.rows).expr
    = Some "4.0") "nested Lisp parameter gesture failed"
