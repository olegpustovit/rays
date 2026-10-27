(* sop/attr_from_image: slots geometry + image. The image rides on both sides as a trailing input
   (Port_gate.register_images): 5x3 by default, 4x1 for "uv3", 256x256 for the bench. Fixtures
   carry Point "uv" (Float2 or Float3, inside and far outside [0,1]), a second Float2 "st", and
   one with a non-finite UV; the standard fixtures lack "uv" (missing-attribute error) and the
   mixed one has "vec2"/"vec3" point attributes selectable through :uv. *)
open Rdk
open Rdk_test_support
open Port_gate
module P = Param
let image width height =
  let rgba = Array.init (width * height * 4) (fun i ->
    let k = float ((i * 7919 + i / 4 * 104729) mod 1000) /. 999. in
    if i mod 4 = 3 && i mod 8 = 3 then 1. else k) in
  (match Sop.Image.create ~width ~height ~rgba with Ok i -> i | Error e -> failwith (Diagnostic.error_to_string e))
let () = register_images "attr_from_image" (function
  | "uv3" -> [image 4 1] | "bench" -> [image 256 256] | _ -> [image 5 3])
let with_uv ~dims ?(bad = false) count =
  let geometry = points count in
  let u = Array.init count (fun i -> if i mod 11 = 0 then -0.75 +. float i *. 0.01
    else if i mod 13 = 0 then 1.5 else float (i * 37 mod 101) /. 100.)
  and v = Array.init count (fun i -> if i mod 7 = 0 then 2.25 else float (i * 53 mod 97) /. 96.) in
  if bad && count > 2 then u.(2) <- Float.nan;
  let uv = if dims = 2 then Attribute.Float2 (Packed.Float2.of_owned ~x:u ~y:v |> get_string_ok)
    else Float3 (Packed.Float3.of_owned ~x:u ~y:v ~z:(Array.make count 0.5) |> get_string_ok) in
  let st = Attribute.Float2 (Packed.Float2.of_owned ~x:v ~y:u |> get_string_ok) in
  List.fold_left (fun g (name, storage) -> Geometry.with_attribute
    (Attribute.create_owned ~owner:Attribute.Point ~name storage |> get_string_ok) g |> get_string_ok)
    geometry ["uv", uv; "st", st]
let inputs = standard_inputs @ List.map (fun (label, g) -> label, [g]) [
  "uv_empty", with_uv ~dims:2 0; "uv_nan", with_uv ~dims:2 ~bad:true 8;
  "uv3", with_uv ~dims:3 300; "uv2", with_uv ~dims:2 4225]
let cases = ["attribute", [ "attribute", P.Text_value "sample"]]
  @ List.map (fun c -> "channel_" ^ c, ["channel", P.Choice_value c]) ["g"; "b"; "a"; "luminance"]
  @ [ "uv_st", ["uv", P.Text_value "st"; "channel", P.Choice_value "luminance"];
      "uv_vec2", ["uv", P.Text_value "vec2"]; "uv_vec3", ["uv", P.Text_value "vec3"];
      "uv_missing", ["uv", P.Text_value "nope"];
      "attribute_p", ["attribute", P.Text_value "P"];
      "attribute_blank", ["attribute", P.Text_value "  "];
      "overwrite_uv", ["attribute", P.Text_value "st"] ]
let bench () = [with_uv ~dims:2 1_000_000], ["channel", P.Choice_value "luminance"]
let () = register { key = "attr_from_image"; native = Sop.Nodes.Attr_from_image.factory;
  inputs; cases; ulps = 4; bench = Some bench }
