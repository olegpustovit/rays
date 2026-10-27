(* sop/attribute_composite: a base input, four weighted layers and a rest slot (pending draft
   nodes/attribute_composite.pending). Every input carries float, vec2/3/4 and alpha attributes on every owner
   (salted values), plus an int attribute that is never composited and a salted extra attribute; later inputs
   drop some attributes so the union of names, absent sources and every operation are exercised. *)
open Rdk
open Rdk_test_support
open Port_gate
module P = Param
let fl name x = name, P.Float_value x
let tx name value = name, P.Text_value value
let bo name b = name, P.Bool_value b
let ch name value = name, P.Choice_value value

let hash i salt = float ((i * 7919 + salt * 104729 + 13) mod 1000) /. 500. -. 1.
let add owner name storage g = Geometry.with_attribute
  (Attribute.create_owned ~owner ~name storage |> get_string_ok) g |> get_string_ok
let count g = function Attribute.Point -> Geometry.point_count g | Vertex -> Geometry.vertex_count g
  | Primitive -> Geometry.primitive_count g | Detail -> 1
(* [skip] names are left out of this input. *)
let layer ?(skip = []) ?(scale = 1.) salt g =
  let on owner g =
    let n = count g owner in
    let plane k = Array.init n (fun i -> scale *. hash i (salt * 7 + k)) in
    let put name storage g = if List.mem name skip then g else add owner name storage g in
    g |> put "f" (Attribute.Float (plane 1))
      |> put "v2" (Attribute.Float2 (Packed.Float2.of_owned ~x:(plane 2) ~y:(plane 3) |> get_string_ok))
      |> put "v3" (Attribute.Float3 (Packed.Float3.of_owned ~x:(plane 4) ~y:(plane 5) ~z:(plane 6) |> get_string_ok))
      |> put "v4" (Attribute.Float4 (Packed.Float4.of_owned ~x:(plane 7) ~y:(plane 8) ~z:(plane 9) ~w:(plane 10)
           |> get_string_ok))
      |> put "alpha" (Attribute.Float (Array.init n (fun i -> float ((i * 37 + salt * 11) mod 11) /. 10.)))
      |> put "i" (Attribute.Int (Array.init n (fun i -> i + salt)))
      |> put (Printf.sprintf "only%d" salt) (Attribute.Float (plane 11)) in
  g |> on Attribute.Point |> on Vertex |> on Primitive |> on Detail
let spread salt g =
  let n = Geometry.point_count g in
  let plane k = Array.init n (fun i -> hash i (salt * 5 + k)) in
  Geometry.with_positions (Packed.Float3.of_owned ~x:(plane 1) ~y:(plane 2) ~z:(plane 3) |> get_string_ok) g
  |> get_string_ok
let mixed () = polygon_polyline ()
let small_grid () = Plane_generators.grid ~connectivity:Grid_quads ~columns:6 ~rows:5 ~size:8. () |> get_ok
(* Normals on points and vertices so that the allow-P rule is visible. *)
let with_normals g =
  let n = Geometry.point_count g and v = Geometry.vertex_count g in
  g |> add Attribute.Point "N" (Attribute.Float3 (Packed.Float3.of_owned ~x:(Array.make n 1.) ~y:(Array.make n 0.)
         ~z:(Array.make n 0.) |> get_string_ok))
    |> add Attribute.Vertex "N" (Attribute.Float3 (Packed.Float3.of_owned ~x:(Array.make v 0.) ~y:(Array.make v 1.)
         ~z:(Array.make v 0.) |> get_string_ok))
let stack base = [ base |> layer 0 |> spread 0 |> with_normals;
                   base |> layer ~skip:["v4"; "alpha"] 1 |> spread 1 |> with_normals;
                   base |> layer ~skip:["f"; "v2"] 2 |> spread 2;
                   base |> layer ~skip:["v3"] ~scale:0.5 3 |> spread 3 |> with_normals;
                   base |> layer 4 |> spread 4;
                   base |> layer ~skip:["alpha"] 5 |> spread 5 |> with_normals ]
let take = List.take
let inputs = [
  "six mixed", stack (mixed ());
  "single", [mixed () |> layer 0];
  "two mixed", take 2 (stack (mixed ()));
  "three mixed", take 3 (stack (mixed ()));
  "three grid", take 3 (stack (small_grid ()));
  "kind mismatch", [mixed () |> layer 0; (let g = mixed () in
      add Attribute.Point "f" (Attribute.Float2 (Packed.Float2.of_owned ~x:(Array.make 6 1.) ~y:(Array.make 6 2.)
        |> get_string_ok)) g)];
  "int attribute", [mixed () |> layer 0; (let g = mixed () in
      add Attribute.Point "f" (Attribute.Int (Array.make 6 1)) g)];
  "cardinality mismatch", [mixed () |> layer 0; small_grid () |> layer 1];
  "overflow", [mixed () |> layer ~scale:1e308 0; mixed () |> layer ~scale:1e308 1];
  "alpha vec", [mixed () |> layer 0; mixed () |> layer 1] ]

let ops = ["Mean"; "Maximum"; "Minimum"; "Over"; "Under"]
let cases =
  List.concat_map (fun op -> List.map (fun (label, extra) -> op ^ " " ^ label, ch "operation" op :: extra) [
    "plain", [];
    "weights", [fl "weight" 0.5; fl "weight1" 0.25; fl "weight2" 0.75; fl "weight3" 2.; fl "weight4" 0.5];
    "extra weights", [tx "weights" "0.5\n3"; fl "weight1" 0.5];
    "alpha", [tx "alpha_attribute" "alpha"];
    "alpha weights", [tx "alpha_attribute" "alpha"; fl "weight" 0.5; fl "weight1" 0.25; fl "weight2" 0.75];
    "alpha padded", [tx "alpha_attribute" " alpha "];
    "alpha missing", [tx "alpha_attribute" "nothing"];
    "zero weights", [fl "weight" 0.; fl "weight1" 0.; fl "weight2" 0.; fl "weight3" 0.; fl "weight4" 0.];
    "negative weight", [fl "weight" (-0.5)];
    "allow P", [bo "allow_position" true];
    "allow P alpha", [bo "allow_position" true; tx "alpha_attribute" "alpha"];
    "P pattern only", [bo "allow_position" true; tx "point_attributes" "P"; tx "vertex_attributes" "";
                       tx "primitive_attributes" ""; tx "detail_attributes" ""];
    "P without allow", [tx "point_attributes" "P"];
    "names", [tx "point_attributes" "v*"; tx "vertex_attributes" "f"; tx "primitive_attributes" "[fa]*";
              tx "detail_attributes" "only*"];
    "only extras", [tx "point_attributes" "only*"; tx "vertex_attributes" "only*"; tx "primitive_attributes" "only*"];
    "nothing", [tx "point_attributes" ""; tx "vertex_attributes" ""; tx "primitive_attributes" "";
                tx "detail_attributes" ""];
    "points only", [tx "vertex_attributes" ""; tx "primitive_attributes" ""; tx "detail_attributes" ""];
    "detail only", [tx "point_attributes" ""; tx "vertex_attributes" ""; tx "primitive_attributes" ""];
    "alpha is vec", [tx "alpha_attribute" "v3"];
    "alpha int", [tx "alpha_attribute" "i"] ]) ops
  @ [ "bad pattern", [tx "point_attributes" "["];
      "bad vertex pattern", [tx "vertex_attributes" "["];
      "bad primitive pattern", [tx "primitive_attributes" "["];
      "bad detail pattern", [tx "detail_attributes" "["];
      "bad weights", [tx "weights" "x"];
      "bad weight columns", [tx "weights" "1\t2"];
      "infinite weight", [tx "weights" "inf"];
      "blank alpha", [tx "alpha_attribute" "  "] ]

(* One million points: two layers with a float and a vec3 attribute. *)
let bench () =
  let n = 1_000_000 in
  let layer salt = points n
    |> add Attribute.Point "f" (Attribute.Float (Array.init n (fun i -> float ((i + salt) mod 97) /. 97.)))
    |> add Attribute.Point "v" (Attribute.Float3 (Packed.Float3.of_owned ~x:(Array.init n (fun i -> float ((i + salt) mod 13)))
         ~y:(Array.make n 1.) ~z:(Array.make n 2.) |> get_string_ok)) in
  [layer 0; layer 1], [ch "operation" "Mean"]
let () = register { key = "attribute_composite"; native = Sop.Nodes.Attribute_composite.factory; inputs; cases;
  ulps = 4; bench = Some bench }
