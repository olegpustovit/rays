(* sop/material: one geometry input (pending draft). *)
open Port_gate
module P = Param
let t name v = name, P.Text_value v
let f name v = name, P.Float_value v
let cases =
  ["defaults", [];
   "named", [t "material" "metal"];
   "named_utf8", [t "material" "\195\169\231\140\171"];
   "color", [f "color_r" 0.25; f "color_g" 0.5; f "color_b" 0.75];
   "roughness", [f "roughness" 0.9; t "material" "m"];
   "roughness0", [f "roughness" 0.];
   "round", [f "round" 0.07; t "material" "r"];
   "round_group", [t "group" " a "; f "round" 0.25];
   "emission", [f "emission_r" 1.; f "emission_g" 0.5; f "emission_b" 0.125];
   "group", [t "group" " a "; t "material" "g"; f "color_r" 0.];
   "group_missing", [t "group" "zzz"];
   "group_blank", [t "group" "  "; t "material" "all"];
   "group_literal_glob", [t "group" "literal *?[]^\\"]]
let () = register { key = "material"; native = Sop.Nodes.Material.factory; inputs = standard_inputs;
  cases; ulps = 4; bench = None (* the bench fixture needs 1,000,000 primitives: see the commit body *) }
