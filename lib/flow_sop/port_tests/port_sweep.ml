(* sop/sweep: two inputs (backbone, cross_section) of polyline curves. Fixtures: open and closed backbones and
   profiles of irregular lengths with attributes and groups on every owner (carried under the prefix), backbones
   carrying the optional frame attributes (pscale, scale, N, up, orient) and every refusal (polygon, zero-length
   edge, reversal, bad or non-finite attributes and positions). Dropped on purpose: a NUL in the prefix (the
   checker cannot spell one) and NaN attribute values (the gate compares NaN attributes unequal even to
   themselves, so a successful cook with one never matches native). *)
open Rdk
open Port_gate
open Port_loft_support
module P = Param

let tx name v = name, P.Text_value v
let fl name v = name, P.Float_value v
let bo name v = name, P.Bool_value v
let ch name v = name, P.Choice_value v

let ok = Rdk_test_support.get_string_ok
let add owner name storage geometry =
  Geometry.with_attribute (Attribute.create_owned ~owner ~name storage |> ok) geometry |> ok

let backbone () = decorate (build [sec ~kind:open_k 5 0.; sec ~kind:closed_k 4 1. ~radius:1.5; sec ~kind:open_k 3 2.])
let profile () = decorate (build [sec ~kind:closed_k 4 0. ~radius:0.3; sec ~kind:open_k 3 0. ~radius:0.2])
let small_backbone () = build [sec ~kind:open_k 4 0.]
let small_profile () = build [sec ~kind:closed_k 5 0. ~radius:0.3]
(* An open profile along the three frame axes, scaled so that the sweep positions expose the frame vectors. *)
let axes_profile () =
  let positions = Packed.Float3.of_owned ~x:[|1e6; 0.; 0.|] ~y:[|0.; 1e6; 0.|] ~z:[|0.; 0.; 1e6|] |> ok in
  let topology = Topology.create_owned ~point_count:3 ~vertex_points:[|0;1;2|] ~primitive_offsets:[|0;3|]
    ~primitive_kinds:[|Topology.Open_polyline|] |> ok in
  Geometry.create ~positions ~topology () |> ok

(* Backbones carrying the optional frame attributes (point owner), over the curves of [backbone ()]. *)
let planes n f = Array.init n f
let with_point name storage geometry = add Attribute.Point name storage geometry
let f3 n fx fy fz = Attribute.Float3 (Packed.Float3.of_owned ~x:(planes n fx) ~y:(planes n fy) ~z:(planes n fz) |> ok)
let f4 n fx fy fz fw = Attribute.Float4 (Packed.Float4.of_owned ~x:(planes n fx) ~y:(planes n fy) ~z:(planes n fz)
  ~w:(planes n fw) |> ok)
let npoints g = Geometry.point_count g
let with_pscale g = with_point "pscale" (Attribute.Float (planes (npoints g) (fun i -> 0.6 +. 0.15 *. float (i mod 4)))) g
let with_scale g = with_point "scale" (f3 (npoints g) (fun i -> 0.8 +. 0.1 *. float (i mod 3)) (fun i -> 1.2 -. 0.05 *. float (i mod 5))
  (fun i -> 0.5 +. 0.2 *. float (i mod 2))) g
let with_normals g = with_point "N" (f3 (npoints g) (fun i -> 0.2 *. sin (float i)) (fun i -> 0.3 +. 0.1 *. float (i mod 3))
  (fun i -> 1. +. 0.1 *. cos (float i))) g
let with_up g = with_point "up" (f3 (npoints g) (fun i -> 0.1 *. float (i mod 4)) (fun i -> 1. -. 0.1 *. float (i mod 3))
  (fun i -> 0.3 *. sin (float (i + 1)))) g
let with_orient g = with_point "orient" (f4 (npoints g) (fun i -> 0.3 *. sin (float i)) (fun i -> 0.2 *. cos (float (2 * i)))
  (fun i -> 0.1 *. float (i mod 3)) (fun i -> 0.8 +. 0.1 *. float (i mod 2))) g
let attr_backbone () = backbone () |> with_pscale |> with_scale

(* Refusals: a polygon, a zero-length edge, a reversal, bad optional attributes. *)
let polygon_backbone () = decorate (polygon_polyline ())
let degenerate_backbone () =
  let positions = Packed.Float3.of_owned ~x:[|0.; 1.; 1.; 2.|] ~y:[|0.; 0.; 0.; 1.|] ~z:[|0.; 0.; 0.; 0.|] |> ok in
  let topology = Topology.create_owned ~point_count:4 ~vertex_points:[|0;1;2;3|] ~primitive_offsets:[|0;4|]
    ~primitive_kinds:[|Topology.Open_polyline|] |> ok in
  Geometry.create ~positions ~topology () |> ok
let reversal_backbone () =
  let positions = Packed.Float3.of_owned ~x:[|0.; 1.; 0.2|] ~y:[|0.; 0.; 0.|] ~z:[|0.; 0.; 0.|] |> ok in
  let topology = Topology.create_owned ~point_count:3 ~vertex_points:[|0;1;2|] ~primitive_offsets:[|0;3|]
    ~primitive_kinds:[|Topology.Open_polyline|] |> ok in
  Geometry.create ~positions ~topology () |> ok
let nan_position () =
  let positions = Packed.Float3.of_owned ~x:[|0.; 1.; Float.nan; 3.|] ~y:[|0.; 0.; 0.; 1.|] ~z:[|0.; 0.; 0.; 0.|] |> ok in
  let topology = Topology.create_owned ~point_count:4 ~vertex_points:[|0;1;2;3|] ~primitive_offsets:[|0;4|]
    ~primitive_kinds:[|Topology.Open_polyline|] |> ok in
  Geometry.create ~positions ~topology () |> ok
let int_pscale () = with_point "pscale" (Attribute.Int (Array.make 4 1)) (small_backbone ())
let zero_normal () = with_point "N" (f3 4 (fun _ -> 0.) (fun _ -> 0.) (fun _ -> 0.)) (small_backbone ())
let parallel_up () = (* the up vector equals the first tangent direction approximately: exactly along the edge *)
  let g = small_backbone () in
  let v = Packed.Float3.Private.view (Geometry.positions g) in
  with_point "up" (f3 4 (fun i -> v.x.(min (i + 1) 3) -. v.x.(i)) (fun i -> v.y.(min (i + 1) 3) -. v.y.(i))
    (fun i -> v.z.(min (i + 1) 3) -. v.z.(i))) g
let zero_orient () = with_point "orient" (f4 4 (fun _ -> 0.) (fun _ -> 0.) (fun _ -> 0.) (fun _ -> 0.)) (small_backbone ())
let wrong_type_normal () = with_point "N" (Attribute.Float (Array.make 4 1.)) (small_backbone ())
let empty_geometry () = points 0

let inputs = [
  "small", [small_backbone (); small_profile ()];
  "axes", [small_backbone (); axes_profile ()];
  "pscale scale", [attr_backbone (); profile ()];
  "normals", [backbone () |> with_normals; profile ()];
  "up", [backbone () |> with_up; profile ()];
  "orient", [backbone () |> with_orient; profile ()];
  "orient and normals", [backbone () |> with_orient |> with_normals |> with_up; profile ()];
  "polygon backbone", [polygon_backbone (); profile ()];
  "polygon profile", [backbone (); polygon_backbone ()];
  "zero edge", [degenerate_backbone (); profile ()];
  "zero edge profile", [backbone (); degenerate_backbone ()];
  "reversal", [reversal_backbone (); profile ()];
  "nan position", [nan_position (); small_profile ()];
  "int pscale", [int_pscale (); small_profile ()];
  "zero normal", [zero_normal (); small_profile ()];
  "parallel up", [parallel_up (); small_profile ()];
  "zero orient", [zero_orient (); small_profile ()];
  "wrong type normal", [wrong_type_normal (); small_profile ()];
  "empty backbone", [empty_geometry (); profile ()];
  "empty profile", [backbone (); empty_geometry ()];
  "closed backbone only", [build [sec ~kind:closed_k 6 0. ~radius:2.]; small_profile ()];
  "main", [backbone (); profile ()] ]

let connectivities = ["Points"; "Rows"; "Columns"; "Rows and columns"; "Quads"; "Triangles"; "Alternating triangles";
                      "Reverse triangles"]
let tangents = ["Average edges"; "Central difference"; "Previous edge"; "Next edge"; "Z axis"]
let cases =
  List.map (fun c -> "connectivity " ^ c, [ch "connectivity" c]) connectivities
  @ List.map (fun t -> "tangent " ^ t, [ch "tangent" t]) tangents
  @ List.concat_map (fun t -> List.map (fun c -> Printf.sprintf "%s %s reversed" t c,
      [ch "tangent" t; ch "connectivity" c; bo "reverse_cross_sections" true]) ["Rows"; "Quads"; "Alternating triangles"])
      ["Average edges"; "Next edge"]
  @ [
  "continuous_closed off", [bo "continuous_closed" false];
  "continuous_closed off twist", [bo "continuous_closed" false; fl "twist" 1.3];
  "transform_attributes off", [bo "transform_attributes" false];
  "reverse cross sections", [bo "reverse_cross_sections" true];
  "reverse columns", [bo "reverse_cross_sections" true; ch "connectivity" "Columns"];
  "reverse points", [bo "reverse_cross_sections" true; ch "connectivity" "Points"];
  "scale", [fl "scale" 1.7];
  "scale negative", [fl "scale" (-0.6)];
  "scale zero", [fl "scale" 0.];
  "roll", [fl "roll" 0.6];
  "twist", [fl "twist" 1.3];
  "roll twist", [fl "roll" (-0.4); fl "twist" 2.9];
  "twist full turn", [fl "twist" 6.283185307179586];
  "caps", [bo "caps" true];
  "caps triangles", [bo "caps" true; ch "connectivity" "Triangles"];
  "caps alternating", [bo "caps" true; ch "connectivity" "Alternating triangles"; tx "cap_group" "ends"];
  "caps reverse", [bo "caps" true; ch "connectivity" "Reverse triangles"; bo "reverse_cross_sections" true];
  "caps blank group", [bo "caps" true; tx "cap_group" " "];
  "caps group trimmed", [bo "caps" true; tx "cap_group" "  lids "];
  "caps group off", [bo "caps" false; tx "cap_group" "ends"];
  "caps rows refused", [bo "caps" true; ch "connectivity" "Rows"];
  "caps points refused", [bo "caps" true; ch "connectivity" "Points"];
  "caps columns refused", [bo "caps" true; ch "connectivity" "Columns"];
  "caps rows and columns refused", [bo "caps" true; ch "connectivity" "Rows and columns"];
  "uv renamed", [tx "uv_attribute" "st"];
  "uv blank", [tx "uv_attribute" ""];
  "uv trimmed", [tx "uv_attribute" " st "];
  "uv points", [tx "uv_attribute" "st"; ch "connectivity" "Points"];
  "uv P refused", [tx "uv_attribute" "P"];
  "uv shadows backbone float", [tx "uv_attribute" "float"];
  "uv shadows prefixed", [tx "uv_attribute" "cross_section_float"];
  "uv shadows prefixed points", [tx "uv_attribute" "cross_section_float"; ch "connectivity" "Points"];
  "uv shadows backbone N", [tx "uv_attribute" "N"];
  "prefix", [tx "cross_section_prefix" "p_"];
  "prefix empty", [tx "cross_section_prefix" ""];
  "prefix space", [tx "cross_section_prefix" " x "];
  "prefix uv", [tx "cross_section_prefix" "c"; tx "uv_attribute" "cfloat"];
  "prefix shifts names", [tx "cross_section_prefix" "int"; tx "uv_attribute" "intinteger"];
  "backbone group", [tx "backbone_group" "a"];
  "backbone group dest_a", [tx "backbone_group" "dest_a"; ch "connectivity" "Rows and columns"];
  "backbone group spaced", [tx "backbone_group" " a "];
  "cross group", [tx "cross_section_group" "a"];
  "cross group dest_a", [tx "cross_section_group" "dest_a"; bo "caps" true];
  "both groups", [tx "backbone_group" "dest_a"; tx "cross_section_group" "a"];
  "missing backbone group", [tx "backbone_group" "nope"];
  "missing cross group", [tx "cross_section_group" "nope"];
  "missing both", [tx "backbone_group" "nope"; tx "cross_section_group" "nope2"];
  "all", [ch "connectivity" "Alternating triangles"; ch "tangent" "Central difference"; bo "caps" true;
          bo "reverse_cross_sections" true; fl "scale" 1.2; fl "roll" 0.3; fl "twist" 0.7; tx "uv_attribute" "st";
          tx "cross_section_prefix" "x_"; tx "cap_group" "lid"; tx "backbone_group" "dest_a"];
  "nan scale refused", [fl "scale" Float.nan];
]

(* 1,000,000 output points: a 1,000-vertex open backbone swept with a 1,000-vertex closed profile (quads). *)
let bench () = [build [sec ~kind:open_k 1000 0.]; build [sec ~kind:closed_k 1000 0. ~radius:0.3]], []
let node = match Sys.getenv_opt "SWEEP_INPUT", Sys.getenv_opt "SWEEP_CASES" with
  | None, None -> { key = "sweep"; native = Sop.Nodes.Sweep.factory; inputs; cases; ulps = 4; bench = Some bench }
  | input, selected_cases ->
      let inputs = match input with
        | None -> inputs
        | Some label -> List.filter (fun (name, _) -> name = label) inputs in
      let cases = match selected_cases with
        | None -> cases
        | Some prefixes ->
            let prefixes = String.split_on_char ',' prefixes in
            List.filter (fun (label, _) -> List.exists (fun prefix ->
              prefix = "*" || String.starts_with ~prefix label) prefixes) cases in
      { key = "sweep"; native = Sop.Nodes.Sweep.factory; inputs; cases; ulps = 4; bench = None }
let () = register node
