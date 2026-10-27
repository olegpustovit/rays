(* sop/blend_shapes: a base input and shapes in four slots plus the rest slot (pending draft
   nodes/blend_shapes.pending). Every geometry carries point attributes of float, vec2/3/4 storage, a vec3 N, a
   scalar mask, an integer and a text ID, an int attribute that is never blended and a vertex N. Fixtures cover
   one to ten shapes, shapes with fewer points matched by ID, missing and mismatched attributes, non-finite
   masks and overflow. *)
open Rdk
open Rdk_test_support
open Port_gate
module P = Param
let fl name x = name, P.Float_value x
let tx name value = name, P.Text_value value
let ch name value = name, P.Choice_value value

let hash i salt = float ((i * 7919 + salt * 104729 + 13) mod 1000) /. 500. -. 1.
let add owner name storage g = Geometry.with_attribute
  (Attribute.create_owned ~owner ~name storage |> get_string_ok) g |> get_string_ok
let spread n salt g =
  let plane k = Array.init n (fun i -> hash i (salt * 5 + k)) in
  Geometry.with_positions (Packed.Float3.of_owned ~x:(plane 1) ~y:(plane 2) ~z:(plane 3) |> get_string_ok) g
  |> get_string_ok
(* [ids] are the integer IDs, text IDs derive from them. [skip] names are left out. *)
let attrs ?(skip = []) ?(scale = 1.) ?ids n salt g =
  let ids = match ids with Some ids -> ids | None -> Array.init n (fun i -> 10 + i) in
  let plane k = Array.init n (fun i -> scale *. hash i (salt * 7 + k)) in
  let put name storage g = if List.mem name skip then g else add Attribute.Point name storage g in
  g |> put "f" (Attribute.Float (plane 1))
    |> put "v2" (Attribute.Float2 (Packed.Float2.of_owned ~x:(plane 2) ~y:(plane 3) |> get_string_ok))
    |> put "v3" (Attribute.Float3 (Packed.Float3.of_owned ~x:(plane 4) ~y:(plane 5) ~z:(plane 6) |> get_string_ok))
    |> put "v4" (Attribute.Float4 (Packed.Float4.of_owned ~x:(plane 7) ~y:(plane 8) ~z:(plane 9) ~w:(plane 10)
         |> get_string_ok))
    |> put "N" (Attribute.Float3 (Packed.Float3.of_owned ~x:(plane 11) ~y:(plane 12) ~z:(plane 13) |> get_string_ok))
    |> put "mask" (Attribute.Float (Array.init n (fun i -> float ((i * 37 + salt * 11) mod 11) /. 10.)))
    |> put "i" (Attribute.Int (Array.init n (fun i -> i + salt)))
    |> put "id" (Attribute.Int ids)
    |> put "tid" (Attribute.Text (Array.map (Printf.sprintf "t%d") ids))
    |> spread n salt
let vertex_n g =
  let v = Geometry.vertex_count g in
  add Attribute.Vertex "N" (Attribute.Float3 (Packed.Float3.of_owned ~x:(Array.make v 0.) ~y:(Array.make v 1.)
    ~z:(Array.make v 0.) |> get_string_ok)) g
let base () = polygon_polyline () |> fixture_groups |> attrs 6 0 |> vertex_n
let shape ?skip ?scale salt = polygon_polyline () |> attrs ?skip ?scale 6 salt
let small_grid () = Plane_generators.grid ~connectivity:Grid_quads ~columns:3 ~rows:3 ~size:4. () |> get_ok
let idshape ?skip salt ids = let n = Array.length ids in points n |> attrs ?skip ~ids n salt
let take = List.take
let shapes k = List.init k (fun i -> shape ~skip:(if i mod 3 = 1 then ["v4"; "N"] else if i mod 3 = 2 then ["f"] else []) (i + 1))
let with_bad_mask g = match Geometry.find_attribute ~owner:Attribute.Point "mask" g with
  | Some _ -> let g = Geometry.without_attribute ~owner:Attribute.Point "mask" g in
      add Attribute.Point "mask" (Attribute.Float (Array.init (Geometry.point_count g) (fun i -> if i = 2 then infinity else 0.5))) g
  | None -> g
let inputs = [
  "empty", [points 0; points 0];
  "no shapes", [base ()];
  "id shapes", [base (); idshape 1 [|15; 13; 99; 10|]; idshape ~skip:["v2"] 2 [|10; 11; 12; 13; 14; 15; 16|];
                shape 3 |> fun g -> g];
  "id shapes small", [base (); idshape 1 [|15; 13; 99; 10|]];
  "wrong count", [base (); (let g = small_grid () in attrs (Geometry.point_count g) 1 g)];
  "kind mismatch", [base (); (let g = polygon_polyline () in
      add Attribute.Point "f" (Attribute.Float2 (Packed.Float2.of_owned ~x:(Array.make 6 1.) ~y:(Array.make 6 2.)
        |> get_string_ok)) g)];
  "overflow", [base (); shape ~scale:1e308 1; shape ~scale:1e308 2];
  "one shape", [base (); shape 1];
  "four shapes", base () :: shapes 4;
  "ten shapes", base () :: shapes 10;
  "six shapes", base () :: shapes 6 ]

let modes = ["Normalized"; "Differencing"]
let cases =
  List.concat_map (fun mode -> List.map (fun (label, extra) -> mode ^ " " ^ label, ch "mode" mode :: extra) [
    "plain", [];
    "weights", [fl "weight1" 0.4; fl "weight2" 0.3; fl "weight3" 0.2; fl "weight4" 0.5];
    "negative", [fl "weight1" (-0.5); fl "weight2" 0.8];
    "zero weights", [fl "weight1" 0.];
    "only second", [fl "weight1" 0.; fl "weight2" 0.7];
    "extra weights", [tx "weights" "0.5\n0.25\n3\n0.1\n0.2\n0.3\n0.4\n0.5"; fl "weight2" 0.2];
    "group", [tx "group" "a"; fl "weight2" 0.5];
    "group padded", [tx "group" " a "; fl "weight2" 0.5];
    "missing group", [tx "group" "nothing"];
    "pattern", [tx "attributes" "v*"; fl "weight2" 0.5];
    "pattern N", [tx "attributes" "N"; fl "weight2" 0.5];
    "no attributes", [tx "attributes" ""; fl "weight2" 0.5];
    "set mask", [ch "masking" "Set from attribute"; fl "weight2" 0.5];
    "scale mask", [ch "masking" "Scale from attribute"; fl "weight2" 0.5; fl "weight3" 1.5];
    "set mask shape", [ch "masking" "Set from attribute"; ch "mask_source" "Shape"];
    "scale mask shape", [ch "masking" "Scale from attribute"; ch "mask_source" "Shape"; fl "weight2" 0.6];
    "mask missing", [ch "masking" "Set from attribute"; tx "mask_attribute" "nothing"];
    "mask blank", [ch "masking" "Scale from attribute"; tx "mask_attribute" " "; fl "weight2" 0.5];
    "mask ignored", [tx "mask_attribute" "v3"; fl "weight2" 0.5];
    "mask vec", [ch "masking" "Set from attribute"; tx "mask_attribute" "v3"];
    "mask int", [ch "masking" "Set from attribute"; tx "mask_attribute" "i"];
    "mask grouped", [ch "masking" "Scale from attribute"; tx "group" "a"; fl "weight2" 0.5];
    "overrides", [ch "masking" "Scale from attribute"; tx "shape_masks" "1\tmask\tshape\n2\t\tfirst\n3\tf\t"; fl "weight2" 0.5];
    "overrides unused", [tx "shape_masks" "1\tmask\tshape"; fl "weight2" 0.5];
    "overrides blank name", [ch "masking" "Set from attribute"; tx "shape_masks" "2\t \tshape"];
    "ids int", [tx "point_id_attribute" "id"; fl "weight2" 0.5; fl "weight3" 0.25];
    "ids text", [tx "point_id_attribute" "tid"; fl "weight2" 0.5];
    "ids padded", [tx "point_id_attribute" " id "; fl "weight2" 0.5];
    "ids mask shape", [tx "point_id_attribute" "id"; ch "masking" "Set from attribute"; ch "mask_source" "Shape"];
    "ids mask first", [tx "point_id_attribute" "id"; ch "masking" "Scale from attribute"; fl "weight2" 0.5];
    "ids grouped", [tx "point_id_attribute" "id"; tx "group" "a"; fl "weight2" 0.5];
    "ids missing", [tx "point_id_attribute" "nothing"];
    "ids P", [tx "point_id_attribute" "P"];
    "ids float", [tx "point_id_attribute" "f"];
    "ids vec", [tx "point_id_attribute" "v3"];
    "ids blank", [tx "point_id_attribute" "  "] ]) modes
  @ [ "bad pattern", [tx "attributes" "["];
      "bad weights", [tx "weights" "x"];
      "bad weight columns", [tx "weights" "1\t2"];
      "infinite weight", [tx "weights" "inf"];
      "bad mask rows", [tx "shape_masks" "1\tmask"];
      "bad mask index", [tx "shape_masks" "0\tmask\tshape"];
      "bad mask index text", [tx "shape_masks" "x\tmask\tshape"];
      "duplicate mask index", [tx "shape_masks" "1\ta\tshape\n1\tb\tfirst"];
      "bad mask source", [tx "shape_masks" "1\tmask\tbogus"] ]

(* One million points: a base and one shape with a float and a vec3 attribute. *)
let bench () =
  let n = 1_000_000 in
  let layer salt = points n
    |> add Attribute.Point "f" (Attribute.Float (Array.init n (fun i -> float ((i + salt) mod 97) /. 97.)))
    |> add Attribute.Point "v" (Attribute.Float3 (Packed.Float3.of_owned ~x:(Array.init n (fun i -> float ((i + salt) mod 13)))
         ~y:(Array.make n 1.) ~z:(Array.make n 2.) |> get_string_ok)) in
  [layer 0; layer 1], [fl "weight1" 0.25; fl "weight2" 0.5]
let () = register { key = "blend_shapes"; native = Sop.Nodes.Blend_shapes.factory; inputs; cases;
  ulps = 4; bench = Some bench }
