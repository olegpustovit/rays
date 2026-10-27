(* Nodes declared once ([Sop.Nodes]): the Lisp node
   ([Lisp_sop.node]) and the editor factory cook byte-identical geometry at one and four domains,
   and the schema key they share hits the session cache for equal values and
   misses it for every changed field. *)

open Rays_math
open Sop
open Rdk_test_support

let failures = ref 0
let check condition message = if not condition then (incr failures; prerr_endline ("FAILED: " ^ message))

let get = function Ok value -> value | Error message -> fail message

(* The Lisp surface refuses an invalid node either while lowering (the node's own validation raises
   [sop/<kind>: ...]) or when it cooks.  [Lisp_sop] is shadowed to remember the kind of the call a
   test builds last (the outermost one): a rejection counts only when its message names it, so a
   typo or an unrelated checker error cannot pass for the validation under test. *)
let last_kind = ref "" and last_text = ref ""
module Lisp_sop = struct
  include Lisp_sop
  let node_result ?with_ text =
    last_text := text;
    (match String.index_opt text '(' with
     | Some start when String.length text > start + 5 && String.sub text (start + 1) 4 = "sop/" ->
         let stop = ref (start + 1) in
         while !stop < String.length text && not (String.contains " )\n" text.[!stop]) do incr stop done;
         last_kind := String.sub text (start + 1) (!stop - start - 1)
     | _ -> ());
    node_result ?with_ text
  let node ?with_ text = match node_result ?with_ text with
    | Ok node -> node
    | Error message -> failwith ("Lisp_sop: " ^ message ^ "\n" ^ text)
end

(* [message] names [kind] ('sop/grid'): as [sop/grid] or as the bare key [grid] at a word boundary,
   the form a cook diagnostic prints its operation in. *)
let names message kind =
  let key = String.sub kind 4 (String.length kind - 4) in
  let word c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_' in
  let rec go i = match String.index_from_opt message i key.[0] with
    | None -> false
    | Some start ->
        let stop = start + String.length key in
        (stop <= String.length message && String.sub message start (String.length key) = key
         && (start = 0 || not (word message.[start - 1]))
         && (stop = String.length message || not (word message.[stop])))
        || go (start + 1) in
  kind <> "" && go 0

(* The Lisp surface itself refuses a value its declaration excludes: a keyword outside its hard
   range, a choice that is not an option, a refusal of the lowering naming its own node.  Other
   checker errors (an unbound name, a type slip, a reader error) are mistakes of the test, not a
   rejection under test. *)
let declared_refusal message =
  let prefix p = String.length message >= String.length p && String.sub message 0 (String.length p) = p in
  prefix "Lisp_sop: E_HARD_RANGE" || prefix "Lisp_sop: E_LOWER"
  || (prefix "Lisp_sop: E_TYPE" && (let contains fragment =
        let n = String.length fragment in
        let rec go i = i + n <= String.length message && (String.sub message i n = fragment || go (i + 1)) in
        go 0 in
      contains "must be one of"))

let cook_error node =
  let context = Context.create ~domains:1 ~grain:97 ~seed:42L () |> get in
  let session = Session.create ~max_entries:8 ~max_payload_bytes:200_000_000 |> get in
  let error = match Session.cook session ~context node with
    | Ok _ -> None
    | Error error -> Some (Diagnostic.error_to_string error) in
  Session.close session; error

let rejected make =
  last_kind := "";
  let strip message =
    let tail = "\n" ^ !last_text in
    let m = String.length message and t = String.length tail in
    if m >= t && String.sub message (m - t) t = tail then String.sub message 0 (m - t) else message in
  let message = match make () with
    | node -> cook_error node
    | exception (Invalid_argument message | Failure message) -> Some (strip message) in
  match message with
  | None -> false
  | Some message -> names message !last_kind || declared_refusal message

(* A Lisp SOP call built from keyword literals: [sop ~inputs:[input] "grid" ["columns", ki 4]]. *)
let ks key value = key, Printf.sprintf "%S" value
let kf key value = key, Lisp_sop.float value
let ki key value = key, string_of_int value
let kb key value = key, string_of_bool value
let kv key value = key, Lisp_sop.vec3 value

(* [optional] are inputs passed by slot keyword, for an optional slot that follows an empty one. *)
let sop ?(inputs = []) ?(optional = []) kind args =
  let named = List.mapi (fun index node -> Printf.sprintf "in%d" index, node) inputs in
  let keyed = List.map (fun (slot, node) -> slot, ("kw_" ^ slot, node)) optional in
  let call = String.concat " " ((("sop/" ^ kind) :: List.map (fun (name, _) -> "(sop/ext_" ^ name ^ ")") named)
    @ List.map (fun (key, value) -> ":" ^ key ^ " " ^ value) args
    @ List.map (fun (slot, (name, _)) -> ":" ^ slot ^ " (sop/ext_" ^ name ^ ")") keyed) in
  Lisp_sop.node ~with_:(named @ List.map snd keyed) ("(" ^ call ^ ")")

let with_optional kind ?(args = []) first slots =
  sop ~inputs:[first]
    ~optional:(List.filter_map (fun (slot, node) -> Option.map (fun node -> slot, node) node) slots) kind args

(* Attribute composite: fixed optional layers by slot keyword, the rest layers as a list. *)
let layered kind prefix rest_slot ?(args = []) base l1 l2 l3 l4 rest =
  let fixed = List.filter_map (fun (slot, node) -> Option.map (fun node -> slot, node) node)
    [prefix ^ "1", l1; prefix ^ "2", l2; prefix ^ "3", l3; prefix ^ "4", l4] in
  let rested = List.mapi (fun index node -> Printf.sprintf "rest%d" index, node) rest in
  let call = String.concat " " (["(sop/" ^ kind ^ " (sop/ext_base)"]
    @ List.map (fun (slot, _) -> ":" ^ slot ^ " (sop/ext_" ^ slot ^ ")") fixed
    @ (if rest = [] then [] else
          [":" ^ rest_slot ^ " (list " ^ String.concat " " (List.map (fun (name, _) -> "(sop/ext_" ^ name ^ ")") rested) ^ ")"])
    @ List.map (fun (key, value) -> ":" ^ key ^ " " ^ value) args
    @ [")"]) in
  Lisp_sop.node ~with_:(("base", base) :: fixed @ rested) call

let composite = layered "attribute_composite" "layer" "layers"
let blend_shapes = layered "blend_shapes" "shape" "shapes"

let point_velocity ?args current previous next =
  with_optional "point_velocity" ?args current ["previous", previous; "next", next]

(* The group [name] over [indices] of [owner] ("Points", "Vertices" or "Primitives"). *)
let group_indices owner name indices node =
  match List.sort_uniq Int.compare (Array.to_list indices) with
  | [] -> sop ~inputs:[node] "group_range"
      [ks "owner" owner; ks "name" name; ks "range_mode" "Start and length"; ki "length" 0]
  | first :: _ as all when List.length all = List.nth all (List.length all - 1) - first + 1 ->
      sop ~inputs:[node] "group_range"
        [ks "owner" owner; ks "name" name; ki "start" first; ki "end_" (List.nth all (List.length all - 1))]
  | all -> sop ~inputs:[node] "ordered_group"
      [ks "owner" owner; ks "name" name; ks "elements" (String.concat " " (List.map string_of_int all))]

(* Hand-built geometry that Lisp cannot say, shared by the sections below. *)
module Sources = struct
  let duplicate () =
    let positions = Rdk.Packed.Float3.Private.of_owned_exn
        ~x:[|0.;1.;2.;0.;1.;2.;9.|] ~y:[|0.;0.;0.;1.;1.;1.;9.|]
        ~z:(Array.make 7 0.) in
    let topology = Rdk.Topology.polygons_owned ~point_count:7
        ~vertex_points:[|0;1;4;3; 1;2;5;4|]
        ~primitive_offsets:[|0;4;8|] |> Result.get_ok in
    let selected = Rdk.Group.ordered ~owner:Rdk.Group.Primitive ~name:"right"
        ~length:2 [|1|] |> Result.get_ok
    and id = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"id"
        (Rdk.Attribute.Int (Array.init 7 (fun point -> 10 + point)))
        |> Result.get_ok in
    Rdk.Geometry.create ~positions ~topology ~attributes:[id] ~groups:[selected] ()
      |> Result.get_ok

  let point_replicate () =
    let geometry = Rdk.Line_geometry.points [|0.,0.,0.; 2.,0.,0.|] in
    let density = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point
        ~name:"density" (Rdk.Attribute.Float [|2.;1.|]) |> Result.get_ok
    and id = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"id"
        (Rdk.Attribute.Int [|11;22|]) |> Result.get_ok
    and pscale = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point
        ~name:"pscale" (Rdk.Attribute.Float [|2.;2.|]) |> Result.get_ok
    and flow = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point
        ~name:"flow"
        (Rdk.Attribute.Float3 (Rdk.Packed.Float3.Private.of_owned_exn
          ~x:[|1.;1.|] ~y:[|2.;2.|] ~z:[|3.;3.|])) |> Result.get_ok
    and selected = Rdk.Group.init ~grain:1 ~owner:Rdk.Group.Point ~name:"emit"
        2 (fun point -> point = 0) in
    geometry |> Rdk.Geometry.with_attribute density |> Result.get_ok
    |> Rdk.Geometry.with_attribute id |> Result.get_ok
    |> Rdk.Geometry.with_attribute pscale |> Result.get_ok
    |> Rdk.Geometry.with_attribute flow |> Result.get_ok
    |> Rdk.Geometry.with_group selected |> Result.get_ok

  let bevel () =
    Rdk.Box_generator.box ~connectivity:Rdk.Box_generator.Box_quads ~consolidate_points:true
      ~normals:Rdk.Box_generator.Box_no_normals ~size:(Vec3.create 2. 2. 2.) ()
    |> function Ok value -> value | Error error -> fail (Rdk.Error.to_string error)

  let bevel_edges geometry =
    let topology = Rdk.Geometry.topology geometry in
    let index = Rdk.Topology_index.create topology in
    let group = Rdk.Edge_group.init ~topology ~index ~name:"bevel_edges" (Fun.const true) in
    Rdk.Geometry.with_edge_group group geometry |> get

  let curve_cut () =
    let attribute owner name storage =
      Rdk.Attribute.create_owned ~owner ~name storage |> Result.get_ok in
    let positions = Rdk.Packed.Float3.Private.of_owned_exn
        ~x:[|0.;2.;10.;14.|] ~y:(Array.make 4 0.) ~z:(Array.make 4 0.) in
    let topology = Rdk.Topology.create_owned ~point_count:4
        ~vertex_points:[|0;1;2;3|] ~primitive_offsets:[|0;2;4|]
        ~primitive_kinds:[|Rdk.Topology.Open_polyline;
          Rdk.Topology.Open_polyline|] |> Result.get_ok in
    let first = Rdk.Group.init ~owner:Rdk.Group.Primitive ~name:"first" 2
        (fun primitive -> primitive = 0) in
    Rdk.Geometry.create ~positions ~topology ~groups:[first] ~attributes:[
      attribute Rdk.Attribute.Point "distance"
        (Rdk.Attribute.Float [|-1.;1.;-1.;1.|]);
      attribute Rdk.Attribute.Point "weight"
        (Rdk.Attribute.Float [|0.;20.;100.;140.|]);
      attribute Rdk.Attribute.Primitive "cut"
        (Rdk.Attribute.Float [|0.;0.5|]);
      attribute Rdk.Attribute.Primitive "material"
        (Rdk.Attribute.Int [|7;9|])
    ] () |> Result.get_ok

  let point_generate () =
    let geometry = Rdk.Line_geometry.points [|0., 0., 0.; 1., 0., 0.; 2., 0., 0.|] in
    let density = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point
        ~name:"density" (Rdk.Attribute.Float [|1.; 2.; 3.|]) |> Result.get_ok
    and id = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"id"
        (Rdk.Attribute.Int [|10; 20; 30|]) |> Result.get_ok
    and selected = Rdk.Group.init ~grain:1 ~owner:Rdk.Group.Point
        ~name:"emit" 3 (fun point -> point <> 1) in
    geometry |> Rdk.Geometry.with_attribute density |> Result.get_ok
    |> Rdk.Geometry.with_attribute id |> Result.get_ok
    |> Rdk.Geometry.with_group selected |> Result.get_ok

  let rewire count =
    let count = count - (count mod 3) in
    let primitive_count = count / 3 in
    let topology = Rdk.Topology.polygons_owned ~point_count:count
        ~vertex_points:(Array.init count Fun.id)
        ~primitive_offsets:(Array.init (primitive_count + 1) (fun primitive ->
          primitive * 3)) |> Result.get_ok in
    let geometry = Rdk.Geometry.create
        ~positions:(Rdk.Packed.Float3.Private.of_owned_exn
          ~x:(Array.init count float_of_int) ~y:(Array.make count 0.)
          ~z:(Array.make count 0.)) ~topology () |> Result.get_ok in
    let target = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"target"
        (Rdk.Attribute.Int (Array.init count (fun point ->
          if point mod 3 = 0 then point + 1 else -1))) |> Result.get_ok
    and selected = Rdk.Group.init ~grain:257 ~owner:Rdk.Group.Point ~name:"selected"
        count (fun point -> point mod 3 = 0) in
    geometry |> Rdk.Geometry.with_attribute target |> Result.get_ok
    |> Rdk.Geometry.with_group selected |> Result.get_ok

  let point_split () =
    let positions = Rdk.Packed.Float3.Private.of_owned_exn
        ~x:[|0.;1.;1.;0.|] ~y:[|0.;0.;1.;1.|] ~z:(Array.make 4 0.) in
    let topology = Rdk.Topology.polygons_owned ~point_count:4
        ~vertex_points:[|0;1;2;0;2;3|] ~primitive_offsets:[|0;3;6|]
        |> Result.get_ok in
    let uv = Rdk.Packed.Float2.of_owned
        ~x:[|0.;1.;1.;2.;3.;0.|] ~y:[|0.;0.;1.;2.;3.;1.|]
        |> Result.get_ok in
    let uv = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Vertex ~name:"uv"
        (Rdk.Attribute.Float2 uv) |> Result.get_ok
    and selected = Rdk.Group.init ~grain:1 ~owner:Rdk.Group.Point
        ~name:"split_points" 4 (fun point -> point = 0 || point = 2)
    and seam_face = Rdk.Group.init ~grain:1 ~owner:Rdk.Group.Primitive
        ~name:"seam_face" 2 (fun primitive -> primitive = 0) in
    Rdk.Geometry.create ~positions ~topology ~attributes:[uv]
      ~groups:[selected;seam_face] ()
    |> Result.get_ok

  let curvature () =
    let geometry = Rdk.Uv_sphere.run
        ~connectivity:Rdk.Uv_sphere.Sphere_alternating_triangles
        ~segments:160 ~rings:80 ~radius:2. () |> Result.get_ok in
    let selected = Rdk.Group.init ~grain:257 ~owner:Rdk.Group.Point
        ~name:"upper" (Rdk.Geometry.point_count geometry) (fun point ->
          let positions = Rdk.Packed.Float3.Private.view
              (Rdk.Geometry.positions geometry) in
          positions.y.(point) >= 0.) in
    Rdk.Geometry.with_group selected geometry |> Result.get_ok

  let hull () = Lisp_sop.snapshot (Rdk.Line_geometry.points [|
    -1.,-1.,-1.; 1.,-1.,-1.; 1.,1.,-1.; -1.,1.,-1.;
    -1.,-1.,1.; 1.,-1.,1.; 1.,1.,1.; -1.,1.,1.;
    0.,0.,0.; 0.2,-0.3,0.4 |])

  let bridge () =
    let positions = Rdk.Packed.Float3.Private.of_owned_exn
        ~x:[|-1.;1.;1.;-1.; -1.;1.;1.;-1.|]
        ~y:[|0.;0.;0.;0.; 1.;1.;1.;1.|]
        ~z:[|-1.;-1.;1.;1.; -1.;-1.;1.;1.|] in
    let topology = Rdk.Topology.create_owned ~point_count:8
        ~vertex_points:[|0;1;2;3; 4;5;6;7|]
        ~primitive_offsets:[|0;4;8|]
        ~primitive_kinds:[|Rdk.Topology.Polygon;Rdk.Topology.Polygon|] |> get in
    let index = Rdk.Topology_index.create topology in
    let source = Rdk.Edge_group.init ~grain:1 ~topology ~index ~name:"source"
        (fun edge -> let a, _ = Rdk.Topology_index.edge_points index edge in a < 4)
    and destination = Rdk.Edge_group.init ~grain:1 ~topology ~index
        ~name:"destination" (fun edge ->
          let a, _ = Rdk.Topology_index.edge_points index edge in a >= 4) in
    Rdk.Geometry.create ~positions ~topology ~edge_groups:[source;destination] ()
    |> get

  let loops count vertices_per_loop =
    let point_count = count * vertices_per_loop in
    let x = Array.make point_count 0. and y = Array.make point_count 0.
    and z = Array.make point_count 0. in
    for component = 0 to count - 1 do
      let cx = float_of_int (component mod 200) *. 4.
      and cy = float_of_int (component / 200) *. 4. in
      for local = 0 to vertices_per_loop - 1 do
        let point = (component * vertices_per_loop) + local
        and angle = 2. *. Float.pi *. float_of_int local
            /. float_of_int vertices_per_loop in
        let radius = 1. +. (0.12 *. cos (3. *. angle)) in
        x.(point) <- cx +. (radius *. cos angle);
        y.(point) <- cy +. (radius *. sin angle);
        z.(point) <- 0.04 *. sin (2. *. angle)
      done
    done;
    let topology = Rdk.Topology.create_owned ~point_count
        ~vertex_points:(Array.init point_count Fun.id)
        ~primitive_offsets:(Array.init (count + 1)
          (fun primitive -> primitive * vertices_per_loop))
        ~primitive_kinds:(Array.make count Rdk.Topology.Closed_polyline)
        |> Result.get_ok in
    let geometry = Rdk.Geometry.create
        ~positions:(Rdk.Packed.Float3.Private.of_owned_exn ~x ~y ~z)
        ~topology () |> Result.get_ok in
    let index = Rdk.Topology_index.create topology in
    let edges = Rdk.Edge_group.init ~grain:127 ~topology ~index
        ~name:"loops" (Fun.const true) in
    Rdk.Geometry.with_edge_group edges geometry |> Result.get_ok
end

let edge_group name ?(incidence = "Boundary") node =
  sop ~inputs:[node] "group_edges" [ks "name" name; ks "incidence" incidence]

let cook ?(grain = 97) domains graph =
  let context = Context.create ~domains ~grain ~seed:42L () |> get in
  let session = Session.create ~max_entries:8 ~max_payload_bytes:200_000_000 |> get in
  let output = match Session.cook session ~context graph with
    | Ok value -> (Result.get_ok (Sop.Payload.geometry value.payload))
    | Error error -> fail (Diagnostic.error_to_string error) in
  Session.close session;
  output

(* The factory node carrying [values], built on [inputs]. *)
let from_factory ?optional_inputs factory values inputs =
  let node = (match optional_inputs with
    | None -> Edit_graph.instantiate factory inputs
    | Some inputs -> Edit_graph.instantiate_optional factory inputs) |> get in
  fst (Node.apply_parameters node values |> get)

let nonfinite_fields = ref 0
let reject_nonfinite name node =
  let fields = Node.parameter_fields node and key = Node.parameter_key node in
  List.iter (fun (field : Param.field_view) -> match field.kind with
    | Param.Floating_view range ->
        incr nonfinite_fields;
        let current = match field.current with Param.Float_value n -> n | _ -> assert false in
        let valid = Param.Float_value
            (if Some current = range.hard_max then current -. 0.25 else current +. 0.25) in
        List.iter (fun value ->
          (* A valid edit preceding the invalid one must not escape either. *)
          match Node.apply_parameters node [field.name, valid;
              field.name, Param.Float_value value] with
          | Error message ->
              check (contains message "finite")
                (name ^ ": " ^ field.name ^ " rejected for an unrelated reason");
              check (Node.parameter_key node = key && Node.parameter_fields node = fields)
                (name ^ ": rejected nonfinite edit mutated its node")
          | Ok _ -> fail (name ^ ": " ^ field.name ^ " accepted a nonfinite parameter"))
          [Float.nan; Float.infinity; Float.neg_infinity]
    | _ -> ()) fields

(* Randomize blends a sample into the existing attribute with a different multiply-add rounding
   than native, a four-ULP port (port_attribute_randomize). A blend that cancels towards zero keeps
   its absolute error at one ULP of the operands, which is many ULP of the result, so the
   comparison also accepts 1e-12 absolute on these O(1) fixtures (Quaternion noise over three
   octaves differs from native by up to 3e-14). *)
let close_blend left right =
  let near a b = a = b || (Float.is_finite a && Float.is_finite b && abs_float (a -. b) <= 1e-12) in
  close_geometry ~ulps:4 left right
  || compare_geometry (fun l r -> Array.length l = Array.length r && Array.for_all2 near l r) left right

(* Ports whose gate declares four ULP against the native oracle and that differ from it at the last
   bits on these fixtures (largest observed difference 2e-14). *)
let blend_ports = ["attribute_randomize"; "attribute_noise"; "attribute_remap"; "uv_sphere"; "bound"; "smooth";
  "attribute_blur"; "measure_curvature"]

let same_node ?(same_key = true) name ~typed ~catalog =
  reject_nonfinite name typed;
  reject_nonfinite name catalog;
  check (not same_key || (Node.parameter_key typed = Node.parameter_key catalog && Node.parameter_key typed <> ""))
    (name ^ ": Lisp node and factory share the schema key");
  let one = cook 1 typed in
  let ported_noise = Node.operation typed = "noise_displace" || List.mem (Node.operation typed) blend_ports in
  let four = cook 4 typed in
  check (if ported_noise then geometry_bytes one = geometry_bytes four else equal_geometry one four)
    (name ^ ": Lisp node at four domains cooks the same geometry");
  (* The port uses separately rounded arithmetic; only its native oracle has
     the approved four-ULP tolerance. Domain self-consistency stays bit-exact. *)
  let equal_oracle = if not ported_noise then equal_geometry
    else if List.mem (Node.operation typed) blend_ports then close_blend else close_geometry ~ulps:4 in
  List.iter (fun (what, geometry) ->
    let worst = lazy (let worst = ref 0. and shape = ref true in
      ignore (compare_geometry (fun l r ->
        if Array.length l <> Array.length r then shape := false
        else Array.iter2 (fun a b -> if a <> b && Float.is_finite a && Float.is_finite b then worst := Float.max !worst (abs_float (a -. b))
          else if a <> b then shape := false) l r; true) one geometry);
      Printf.sprintf " (largest float difference %g%s)" !worst (if !shape then "" else ", other planes differ")) in
    check (equal_oracle one geometry)
    (name ^ ": " ^ what ^ " cooks the same geometry" ^ if equal_oracle one geometry then "" else Lazy.force worst))
    [ "factory at one domain", cook 1 catalog;
      "factory at four domains", cook 4 catalog ]

let same_cook ?(inputs = []) ?optional_inputs name ~typed ~factory values input =
  same_node name ~typed ~catalog:(from_factory ?optional_inputs factory values (input :: inputs))

let same_generator name ~typed ~factory values =
  same_node name ~typed ~catalog:(from_factory factory values [])

(* A value that differs from [current] and stays inside the hard range. *)
let changed (view : Param.field_view) = match view.kind, view.current with
  | Param.Toggle_view, Param.Bool_value value -> Param.Bool_value (not value)
  | Integer_view range, Int_value value ->
      Int_value (if Some value = range.hard_max then value - 1 else value + 1)
  | Floating_view range, Float_value value ->
      Float_value (if Some value = range.hard_max then value -. 0.25 else value +. 0.25)
  | Text_view, Text_value value -> Text_value (value ^ "_changed")
  | Choice_view options, Choice_value value ->
      Choice_value (Option.get (Array.find_opt (fun option -> option <> value) options))
  | _ -> fail (view.name ^ ": view and value disagree")

let cache_identity ?(changes = []) ?(companions = []) name node =
  reject_nonfinite name node;
  let session = Session.create ~max_entries:64 ~max_payload_bytes:200_000_000 |> get in
  let context = Context.create ~domains:1 ~grain:97 ~seed:42L () |> get in
  (* a changed text may name a group the input lacks: that cook fails, which
     is still a miss, but nothing is cached to hit afterwards *)
  let cook node = Result.is_ok (Session.cook session ~context node) in
  let misses () = (Session.stats session).misses in
  check (cook node) (name ^ ": cooks");
  let before = misses () in
  ignore (cook node);
  check (misses () = before) (name ^ ": equal parameters hit the session cache");
  List.iter (fun (view : Param.field_view) ->
    let value = match List.assoc_opt view.name changes with
      | Some value -> value | None -> changed view in
    match Node.apply_parameters node ((view.name, value) ::
        Option.value ~default:[] (List.assoc_opt view.name companions)) with
    | Error message -> fail (name ^ ": cannot probe " ^ view.name ^ ": " ^ message)
    | result ->
    let edited = fst (get result) in
    let before = misses () in
    let cooked = cook edited in
    check (misses () = before + 1) (name ^ ": changed " ^ view.name ^ " misses the cache");
    ignore (cook edited);
    check (misses () = before + (if cooked then 1 else 2))
      (name ^ ": edited " ^ view.name ^ " then hits the cache"))
    (Node.parameter_fields node);
  Session.close session

let transport_cache_companions =
  ["integrate_constant", ["operation", Param.Choice_value "Total"];
   "scale_by_edge_length", ["operation", Param.Choice_value "Total"];
   "operation", ["integrate_constant", Param.Bool_value false; "scale_by_edge_length", Bool_value false]]

let run () =
  let field = Node.Private.make ~operation:"test.field" ~version:1 ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _ ->
        let kernel = Kernel.create ~payload_bytes:0 (function
          | [Kernel.Vec3s positions] -> Ok (fun _ -> Ok (Kernel.Floats
              (Array.init (Array.length positions / 3) (fun i ->
                let x = positions.(3*i) and y = positions.(3*i+1) and z = positions.(3*i+2) in
                sqrt ((x *. x +. y *. y) +. z *. z) -. 1.))))
          | _ -> assert false) in
        Ok Node.Private.{payload = Payload.Kernel kernel; instances = None}) in
  let resolution = Vec3.create 8. 9. 7. in
  (* The Lisp field is a [fn]; the factory takes the OCaml kernel [field] (or the lowered Lisp kernel) as input. *)
  let iso_factory ?(min = Vec3.create (-2.) (-2.) (-2.)) ?(max = Vec3.create 2. 2. 2.) field (resolution : Vec3.t) =
    from_factory Nodes.Iso_surface.factory
      ["resolution_x", Param.Float_value resolution.x; "resolution_y", Float_value resolution.y;
       "resolution_z", Float_value resolution.z; "min_x", Float_value min.x; "min_y", Float_value min.y;
       "min_z", Float_value min.z; "max_x", Float_value max.x; "max_y", Float_value max.y;
       "max_z", Float_value max.z] [field] in
  let iso_lisp = Lisp_sop.node (Printf.sprintf {|(sop/iso_surface :field (fn [p] (- (length p) 1.0)) :resolution %s)|}
    (Lisp_sop.vec3 resolution)) in
  same_node "iso_surface" ~typed:iso_lisp
    ~catalog:(iso_factory (List.hd (Node.inputs iso_lisp)) resolution);
  let checked_lattice (rx, ry, rz) (min : Vec3.t) (max : Vec3.t) =
    let nx = rx + 1 and ny = ry + 1 in
    let prepared = ref 0 in
    let field = Node.Private.make ~operation:"test.field-lattice" ~version:1 ~parameters:""
      ~cook_mode:Node.Generic ~dependencies:Context.Dependencies.static ~inputs:[||]
      (fun ~node_id:_ _ _ ->
        let kernel = Kernel.create ~payload_bytes:0 (function
          | [Kernel.Vec3s positions] ->
              incr prepared;
              assert (Array.length positions = 3 * nx * ny * (rz + 1));
              Array.iteri (fun j coordinate ->
                let i = j / 3 in
                let expected = match j mod 3 with
                  | 0 -> Float.fma (float_of_int (i mod nx)) ((max.x -. min.x) /. float rx) min.x
                  | 1 -> Float.fma (float_of_int ((i / nx) mod ny)) ((max.y -. min.y) /. float ry) min.y
                  | _ -> Float.fma (float_of_int (i / (nx * ny))) ((max.z -. min.z) /. float rz) min.z in
                assert (Int64.bits_of_float coordinate = Int64.bits_of_float expected)) positions;
              Ok (fun _ -> Ok (Kernel.Floats
                (Array.init (Array.length positions / 3) (fun i -> positions.(3*i)))))
          | _ -> assert false) in
        Ok Node.Private.{payload = Payload.Kernel kernel; instances = None}) in
    field, prepared in
  List.iter (fun (resolution, min, max, grains) ->
    let field, prepared = checked_lattice resolution min max in
    let rx, ry, rz = resolution in
    let node = iso_factory ~min ~max field (Vec3.create (float rx) (float ry) (float rz)) in
    let expected = Rdk.Iso_surface.extract_dense ~resolution ~min ~max ~iso:0.
      ~field:(Rdk.Iso_surface.Field.custom (fun p -> p.(0))) () |> get_ok |> geometry_bytes in
    List.iter (fun grain -> List.iter (fun domains ->
      check (geometry_bytes (cook ~grain domains node) = expected)
        "iso_surface: actual row-chunk lattice and complete geometry match independent FMA samples")
      [1;8]) grains;
    List.iter (fun domains ->
      let cancel = Context.Cancel.create () in
      Context.Cancel.cancel cancel;
      let context = Context.create ~domains ~grain:97 ~cancel () |> get in
      let session = Session.create ~max_entries:0 ~max_payload_bytes:0 |> get in
      let before = !prepared in
      Fun.protect ~finally:(fun () -> Session.close session) (fun () ->
        (match Session.cook session ~context node with
         | Error error -> assert (error.code = "cancelled")
         | Ok _ -> fail "iso_surface: precancelled cook succeeded");
        assert (!prepared = before))) [1;8])
    [(129,256,2), Vec3.create (-2.) (-1.5) (-1.), Vec3.create 3. 2. 1.7, [97];
     (7,5,9), Vec3.create (-2.) (-1.4) (-1.2), Vec3.create 2.5 1.7 2.1, [97;240;max_int]];
  let facts = Node.facts (iso_factory field resolution) in
  check (facts.elementwise = Node.None && facts.topology = Changed && facts.exact)
    "iso_surface: irregular, topology-changing, exact";
  List.iter (fun resolution ->
    check (rejected (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/iso_surface :field (fn [p] (- (length p) 1.0)) :resolution %s)|} (Lisp_sop.vec3 resolution))))
      "iso_surface: invalid resolution rejected")
    [Vec3.create 0. 2. 2.; Vec3.create 1.5 2. 2.;
     Vec3.create 1e15 1e15 1e15];
  check (rejected (fun () -> Lisp_sop.node {|(sop/iso_surface :field (fn [p] (- (length p) 1.0)) :min [2.0 2.0 2.0])|}))
    "iso_surface: invalid bounds rejected";
  let projection_context = Context.create ~frame:7L ~time:2.5 ~seed:11L
      ~domains:1 ~grain:3 () |> get in
  let projection_facts = Context.Dependencies.[
      Frame, "f7;"; Time, "t4612811918334230528;"; Seed, "s11;";
      Domains, "d1;"; Grain, "g3;";
    ] in
  let rec check_projections dependencies expected = function
    | [] -> if Context.cache_projection dependencies projection_context <> expected
        then fail "context cache projection changed"
    | (fact, token) :: rest ->
        check_projections dependencies expected rest;
        check_projections
          (Context.Dependencies.union dependencies (Context.Dependencies.one fact))
          (expected ^ token) rest in
  check_projections Context.Dependencies.static "" projection_facts;
  let transform_source = Lisp_sop.node {|(-> (sop/grid :columns 4 :rows 3)
     (sop/normals)
     (sop/group_range :name "selected" :end_ 1)
     (sop/group_range :owner "Vertices" :name "selected" :end_ 1)
     (sop/group_range :owner "Primitives" :name "selected" :end_ 0)
     (sop/group_edges :name "selected" :incidence "Boundary"))|} in
  let transform_geometry = cook 1 transform_source in
  let transform_input = Lisp_sop.snapshot (transform_geometry) in
  let transform_default = Lisp_sop.node ~with_:["transform_input", (transform_input)] {|(sop/transform (sop/ext_transform_input))|} in
  same_cook "transform defaults" ~typed:transform_default ~factory:Nodes.Transform.factory [] transform_input;
  check (Node.parameter_key (Lisp_sop.node ~with_:["transform_input", (transform_input)] {|(sop/transform (sop/ext_transform_input))|}) = Node.parameter_key transform_default)
    "transform aliases share Lisp defaults and schema";
  cache_identity "transform all fields" transform_default;
  let transform_selections =
    ["Point",Rdk.Transform_ops.Selected_points (Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected" transform_geometry |> Option.get);
     "Vertex",Selected_vertices (Rdk.Geometry.find_group ~owner:Rdk.Group.Vertex "selected" transform_geometry |> Option.get);
     "Primitive",Selected_primitives (Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "selected" transform_geometry |> Option.get);
     "Edge",Selected_edges (Rdk.Geometry.find_edge_group "selected" transform_geometry |> Option.get)] in
  let transform_orders = [Rdk.Transform_ops.Transform_srt,"SRT";Transform_str,"STR";Transform_rst,"RST";Transform_rts,"RTS";Transform_tsr,"TSR";Transform_trs,"TRS"]
  and transform_rotations = [Rdk.Transform_ops.Transform_xyz,"XYZ";Transform_xzy,"XZY";Transform_yxz,"YXZ";Transform_yzx,"YZX";Transform_zxy,"ZXY";Transform_zyx,"ZYX"] in
  let translate = Vec3.create 0.2 0.3 (-0.1) and rotate = Vec3.create 0.1 0.2 0.3
  and scale = Vec3.create 1.1 0.9 1.2 and pivot = Vec3.create 0.1 (-0.1) 0.2
  and pivot_rotation = Vec3.create 0.2 0.1 (-0.1) in
  let transform_values = ["translate_x",Param.Float_value translate.x;"translate_y",Float_value translate.y;"translate_z",Float_value translate.z;
    "rotate_x",Float_value rotate.x;"rotate_y",Float_value rotate.y;"rotate_z",Float_value rotate.z;
    "scale_x",Float_value scale.x;"scale_y",Float_value scale.y;"scale_z",Float_value scale.z;
    "shear_xy",Float_value 0.1;"shear_xz",Float_value 0.2;"shear_yz",Float_value (-0.1);"uniform_scale",Float_value 0.8;
    "pivot_x",Float_value pivot.x;"pivot_y",Float_value pivot.y;"pivot_z",Float_value pivot.z;
    "pivot_rotation_x",Float_value pivot_rotation.x;"pivot_rotation_y",Float_value pivot_rotation.y;"pivot_rotation_z",Float_value pivot_rotation.z] in
  List.iter (fun (order,order_name) -> List.iter (fun (rotation_order,rotation_name) ->
    List.iter (fun (owner_name,selection) -> List.iter (fun invert ->
      List.iter (fun preserve_normal_length -> List.iter (fun recompute_normals ->
        let typed = sop ~inputs:[transform_input] "transform"
            [ks "order" order_name; ks "rotation_order" rotation_name; kv "translate" translate; kv "rotate" rotate;
             kv "scale" scale; kv "pivot" pivot; kv "pivot_rotation" pivot_rotation;
             kf "shear_xy" 0.1; kf "shear_xz" 0.2; kf "shear_yz" (-0.1); kf "uniform_scale" 0.8; kb "invert" invert;
             ks "group_owner" owner_name; ks "group" "selected"; kb "preserve_normal_length" preserve_normal_length;
             kb "recompute_normals" recompute_normals] in
        same_cook "transform TRS controls" ~typed ~factory:Nodes.Transform.factory
          (transform_values @ ["order",Param.Choice_value order_name;"rotation_order",Choice_value rotation_name;
            "group_owner",Choice_value owner_name;"group",Text_value "selected";"invert",Bool_value invert;
            "preserve_normal_length",Bool_value preserve_normal_length;"recompute_normals",Bool_value recompute_normals]) transform_input;
        let matrix = Rdk.Transform_ops.compose_transform ~order ~rotation_order ~translate ~rotate ~scale ~pivot ~pivot_rotation
            ~shear:(Vec3.create 0.1 0.2 (-0.1)) ~uniform_scale:0.8 ~invert () |> Result.get_ok in
        let native = Rdk.Transform_ops.transform_selected ~selection ~preserve_normal_length ~recompute_normals matrix transform_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "transform TRS controls match native") [false;true]) [false;true]) [false;true])
      transform_selections) transform_rotations) transform_orders;
  List.iter (fun matrix -> List.iter (fun (owner_name,selection) ->
    List.iter (fun preserve_normal_length -> List.iter (fun recompute_normals ->
      let m row column = Mat4.get matrix ~row ~column in
      let typed = sop ~inputs:[transform_input] "transform"
          ([ks "mode" "Matrix"; ks "group_owner" owner_name; ks "group" "selected";
            kb "preserve_normal_length" preserve_normal_length; kb "recompute_normals" recompute_normals]
           @ List.init 16 (fun index -> kf (Printf.sprintf "m%d%d" (index/4) (index mod 4)) (m (index/4) (index mod 4)))) in
      let values = List.init 16 (fun index -> Printf.sprintf "m%d%d" (index/4) (index mod 4),Param.Float_value (m (index/4) (index mod 4))) in
      same_cook "transform matrix controls" ~typed ~factory:Nodes.Transform.factory
        (["mode",Param.Choice_value "Matrix";"group_owner",Choice_value owner_name;"group",Text_value "selected";
          "preserve_normal_length",Bool_value preserve_normal_length;"recompute_normals",Bool_value recompute_normals] @ values) transform_input;
      let native = Rdk.Transform_ops.transform_selected ~selection ~preserve_normal_length ~recompute_normals matrix transform_geometry |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "transform matrix controls match native") [false;true]) [false;true]) transform_selections)
    [Mat4.identity;Mat4.translation translate;Mat4.rotation_x 0.2;Mat4.scaling (Vec3.create 1. 0. 1.);
     Mat4.of_rows (1.,0.1,0.,0.2) (0.,1.,0.2,0.3) (0.,0.,1.,0.4) (0.1,0.,0.,1.)];
  same_cook "transform blank selection" ~typed:(Lisp_sop.node ~with_:["transform_input", (transform_input)] {|(sop/transform (sop/ext_transform_input) :group " ")|})
    ~factory:Nodes.Transform.factory ["group",Param.Text_value " "] transform_input;
  List.iter (fun construct -> check (rejected construct)
      "transform refuses invalid construction")
    [(fun () -> Lisp_sop.node ~with_:["transform_input", (transform_input)] (Printf.sprintf {|(sop/transform (sop/ext_transform_input) :scale %s :invert true)|} ((Lisp_sop.vec3 Vec3.zero))));
     (fun () -> Lisp_sop.node ~with_:["transform_input", (transform_input)] (Printf.sprintf {|(sop/transform
   (sop/ext_transform_input)
   :scale [%s 1.0 1.0]
   :uniform_scale %s)|} ((Lisp_sop.float Float.max_float)) ((Lisp_sop.float Float.max_float))))];
  List.iter (fun (field : Param.field_view) -> match field.kind with
    | Param.Floating_view _ -> check (Result.is_error (Node.apply_parameters transform_default [field.name,Param.Float_value Float.nan]))
        "transform inspector refuses every nonfinite field"
    | _ -> ()) (Node.parameter_fields transform_default);
  let intrinsic_schema = Param.schema ~name:"snapshot_wrapper" ~default:() [] in
  let wrapped_snapshot geometry = Custom.node ~operation:"snapshot" ~schema:intrinsic_schema ~values:() []
      (fun ~label:_ ~inputs:_ ~parameters:() -> Lisp_sop.snapshot (geometry)) in
  let intrinsic_first = wrapped_snapshot (Rdk.Line_geometry.points [|1.,0.,0.|]) in
  let intrinsic_second = wrapped_snapshot (Rdk.Line_geometry.points [|2.,0.,0.|])
      |> Node.Private.adopt_identity ~source:intrinsic_first in
  check (Node.parameter_key intrinsic_first = Node.parameter_key intrinsic_second
      && Node.Private.cache_parameters intrinsic_first <> Node.Private.cache_parameters intrinsic_second)
    "custom schemas retain distinct intrinsic snapshot identity";
  let intrinsic_session = Session.create ~max_entries:8 ~max_payload_bytes:200_000 |> Result.get_ok in
  let intrinsic_context = Context.create ~domains:1 () |> Result.get_ok in
  let first = Session.cook intrinsic_session ~context:intrinsic_context intrinsic_first |> Result.get_ok in
  let before = Session.stats intrinsic_session in
  let second = Session.cook intrinsic_session ~context:intrinsic_context intrinsic_second |> Result.get_ok in
  let after = Session.stats intrinsic_session in
  check (after.misses > before.misses && not (equal_geometry (Result.get_ok (Sop.Payload.geometry first.payload)) (Result.get_ok (Sop.Payload.geometry second.payload))))
    "custom snapshot data changes miss despite equal logical and schema identity";
  let repeated = Session.cook intrinsic_session ~context:intrinsic_context intrinsic_second |> Result.get_ok in
  check ((Result.get_ok (Sop.Payload.geometry repeated.payload)) == (Result.get_ok (Sop.Payload.geometry second.payload)) && (Session.stats intrinsic_session).hits > after.hits)
    "custom snapshot repeated intrinsic identity hits";
  Session.close intrinsic_session;
  let schema_only = Lisp_sop.node {|(sop/grid)|} in
  (* A Lisp node's intrinsic identity is its body digest; its schema values are the separate
     parameter key, so editing a parameter moves the key and leaves the intrinsic part alone. *)
  let schema_edited, _ = Node.apply_parameters schema_only ["columns", Param.Int_value 3] |> Result.get_ok in
  check (Node.Private.cache_parameters schema_only <> "" && Node.parameters schema_only <> ""
      && Node.parameter_key schema_only <> ""
      && Node.Private.cache_parameters schema_edited = Node.Private.cache_parameters schema_only
      && Node.parameter_key schema_edited <> Node.parameter_key schema_only)
    "schema-only nodes keep display text outside intrinsic cache identity";
  let quat_base = Lisp_sop.node {|(-> (sop/grid :columns 3 :rows 3)
     (sop/group_range :name "selected" :range_mode "From ends")
     (sop/group_range :owner "Vertices" :name "selected" :range_mode "From ends")
     (sop/group_range :owner "Primitives" :name "selected" :range_mode "From ends")
     (sop/set_vector :name "loc" :value [0.2 0.3 0.4])
     (sop/set_vector :owner "Vertex" :name "loc" :value [0.2 0.3 0.4])
     (sop/set_vector :owner "Primitive" :name "loc" :value [0.2 0.3 0.4])
     (sop/set_vector :owner "Detail" :name "loc" :value [0.2 0.3 0.4]))|} in
  let quat_geometry = cook 1 quat_base in
  let quat_source = Lisp_sop.snapshot (quat_geometry) in
  let randomized = Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_randomize (sop/ext_quat_source))|} in
  same_cook "randomize defaults" ~typed:randomized ~factory:Nodes.Attribute_randomize.factory [] quat_source;
  cache_identity ~companions:["direction_bias",["distribution",Param.Choice_value "Direction";
    "kind",Choice_value "Vector 3";"a_x",Float_value 1.]] "randomize all fields" randomized;
  let a = Vec3.create 0.2 0.3 0.4 and b = Vec3.create 1.1 1.2 1.3 in
  let native_a = Rdk.Attribute_ops.Vec3 a and native_b = Rdk.Attribute_ops.Vec3 b in
  let distributions = [
    "Constant",Rdk.Attribute_ops.Random_constant native_a;
    "Two values",Random_two_values {a=native_a;b=native_b;probability_b=0.5};
    "Uniform",Random_uniform {min=native_a;max=native_b};
    "Uniform discrete",Random_uniform_discrete {min=native_a;max=native_b;step=Vec3 (Vec3.create 1. 1. 1.)};
    "Normal",Random_normal {middle=native_a;scale=native_b};
    "Exponential",Random_exponential {median=native_a};
    "Log normal",Random_log_normal {median=native_a;stddev=native_b};
    "Cauchy",Random_cauchy {median=native_a;scale=Vec3 (Vec3.create 1.1 1.1 1.1)};
    "Direction",Random_direction {direction=native_a;cone_angle=0.7853981633974483};
    "Inside sphere",Random_inside_sphere {dimensions=3};
    "Inside sphere cone",Random_inside_sphere_cone {direction=native_a;cone_angle=0.7853981633974483};
    "Custom ramp",Random_custom_ramp {ramp=[0.,0.;1.,1.];fit_min=native_a;fit_max=native_b};
    "Custom discrete",Random_custom_discrete [native_a,1.;native_b,2.]] in
  List.iter (fun (choice,native_distribution) ->
    List.iter (fun (owner,owner_choice) ->
      let b = if choice = "Cauchy" then Vec3.create 1.1 1.1 1.1 else b in
      let typed = sop ~inputs:[quat_source] "attribute_randomize"
          [ks "owner" owner_choice; ks "kind" "Vector 3"; ks "distribution" choice; kv "a" a; kv "b" b;
           ks "entries" "0.2\t0.3\t0.4\t1\n1.1\t1.2\t1.3\t2"] in
      same_cook "randomize distributions" ~typed ~factory:Nodes.Attribute_randomize.factory
        ["owner",Param.Choice_value owner_choice;"kind",Choice_value "Vector 3";"distribution",Choice_value choice;
         "a_x",Float_value 0.2;"a_y",Float_value 0.3;"a_z",Float_value 0.4;
         "b_x",Float_value b.x;"b_y",Float_value b.y;"b_z",Float_value b.z;
         "entries",Text_value "0.2\t0.3\t0.4\t1\n1.1\t1.2\t1.3\t2"] quat_source;
      let native = Rdk.Attribute_ops.randomize ~seed:(Rand.seed 0) ~owner ~name:"random" native_distribution quat_geometry |> Result.get_ok in
      check (close_blend native (cook 1 typed)) "flat randomize distributions match native")
      [Rdk.Attribute.Point,"Point";Vertex,"Vertex";Primitive,"Primitive";Detail,"Detail"])
    distributions;
  same_cook "randomize custom text" ~typed:(Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_randomize (sop/ext_quat_source) :distribution "Custom discrete text")|})
    ~factory:Nodes.Attribute_randomize.factory ["distribution",Param.Choice_value "Custom discrete text"] quat_source;
  List.iter (fun construct -> check (rejected construct)
      "randomize refuses invalid construction")
    [(fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_randomize (sop/ext_quat_source) :distribution "Direction")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_randomize
   (sop/ext_quat_source)
   :distribution "Custom discrete"
   :entries "")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_randomize
   (sop/ext_quat_source)
   :distribution "Custom discrete"
   :entries "1	-1")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_randomize
   (sop/ext_quat_source)
   :distribution "Custom discrete text"
   :operation "Add")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] (Printf.sprintf {|(sop/attribute_randomize
   (sop/ext_quat_source)
   :distribution "Uniform discrete"
   :step %s)|} ((Lisp_sop.vec3 Vec3.zero))));
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_randomize
   (sop/ext_quat_source)
   :use_minimum true
   :use_maximum true
   :minimum 2.0)|})];
  let random_owners = [Rdk.Attribute.Point,"Point";Vertex,"Vertex";Primitive,"Primitive";Detail,"Detail"] in
  let random_kinds = ["Scalar",1;"Vector 2",2;"Vector 3",3;"Vector 4",4] in
  let numeric dimension x y z w = match dimension with
    | 1 -> Rdk.Attribute_ops.Scalar x | 2 -> Vec2 (Vec2.create x y)
    | 3 -> Vec3 (Vec3.create x y z) | _ -> Vec4 (x,y,z,w) in
  let random_geometry = List.fold_left (fun geometry (owner,_) ->
    let count = match owner with Rdk.Attribute.Point -> Rdk.Geometry.point_count geometry
      | Vertex -> Rdk.Geometry.vertex_count geometry | Primitive -> Rdk.Geometry.primitive_count geometry | Detail -> 1 in
    let install name storage = Rdk.Attribute.create_owned ~owner ~name storage |> Result.get_ok
      |> fun attribute -> Rdk.Geometry.with_attribute attribute geometry |> Result.get_ok in
    let geometry = install "seed_values" (Rdk.Attribute.Int (Array.init count (fun i -> 3*i+7))) in
    List.fold_left (fun geometry dimension ->
      let values value = Array.make count value in
      let storage = match dimension with
        | 1 -> Rdk.Attribute.Float (values 0.25)
        | 2 -> Rdk.Attribute.Float2 (Rdk.Packed.Float2.of_owned ~x:(values 0.25) ~y:(values 0.5) |> Result.get_ok)
        | 3 -> Rdk.Attribute.Float3 (Rdk.Packed.Float3.of_owned ~x:(values 0.25) ~y:(values 0.5) ~z:(values 0.75) |> Result.get_ok)
        | _ -> Rdk.Attribute.Float4 (Rdk.Packed.Float4.of_owned ~x:(values 0.25) ~y:(values 0.4) ~z:(values 0.6) ~w:(values 0.75) |> Result.get_ok) in
      let attribute = Rdk.Attribute.create_owned ~owner ~name:("fraction" ^ string_of_int dimension) storage |> Result.get_ok in
      Rdk.Geometry.with_attribute attribute geometry |> Result.get_ok) geometry [1;2;3;4]) quat_geometry random_owners in
  let random_input = Lisp_sop.snapshot (random_geometry) in
  let random_operations = [Rdk.Attribute_ops.Random_set,"Set";Random_add,"Add";Random_minimum,"Minimum";Random_maximum,"Maximum";Random_multiply,"Multiply"] in
  let modes = List.map fst distributions in
  let expected_random_seed () =
    let mixed = Int64.logxor 42L (Int64.mul 0x3b6ee50efd923331L 0x9e3779b97f4a7c15L) in
    Int64.to_int (Int64.logxor mixed (Int64.shift_right_logical mixed 32)) in
  List.iter (fun (kind_label,dimension) ->
    let a = Vec3.create 0.2 0.3 0.4 and b = Vec3.create 1.1 1.1 1.1 in
    let av = numeric dimension 0.2 0.3 0.4 0.5 and bv = numeric dimension 1.1 1.1 1.1 1.1 in
    let entries = String.concat "\n" [String.concat "\t" ((List.init dimension (fun i -> List.nth ["0.2";"0.3";"0.4";"0.5"] i)) @ ["1"]);
      String.concat "\t" (List.init dimension (fun _ -> "1.1") @ ["2"])] in
    List.iter (fun distribution_label ->
      let directional = distribution_label = "Direction" || distribution_label = "Inside sphere cone" in
      if dimension > 1 || not (directional || distribution_label = "Inside sphere") then (
      let native_distribution = match distribution_label with
        | "Constant" -> Rdk.Attribute_ops.Random_constant av
        | "Two values" -> Random_two_values {a=av;b=bv;probability_b=0.5}
        | "Uniform" -> Random_uniform {min=av;max=bv}
        | "Uniform discrete" -> Random_uniform_discrete {min=av;max=bv;step=numeric dimension 1. 1. 1. 1.}
        | "Normal" -> Random_normal {middle=av;scale=bv}
        | "Exponential" -> Random_exponential {median=av}
        | "Log normal" -> Random_log_normal {median=av;stddev=bv}
        | "Cauchy" -> Random_cauchy {median=av;scale=bv}
        | "Direction" -> Random_direction {direction=av;cone_angle=0.7}
        | "Inside sphere" -> Random_inside_sphere {dimensions=dimension}
        | "Inside sphere cone" -> Random_inside_sphere_cone {direction=av;cone_angle=0.7}
        | "Custom ramp" -> Random_custom_ramp {ramp=[0.,0.;0.5,0.2;1.,1.];fit_min=av;fit_max=bv}
        | "Custom discrete" -> Random_custom_discrete [av,1.;bv,2.]
        | _ -> assert false in
      List.iter (fun (owner,owner_label) ->
        let count = match owner with Rdk.Attribute.Point -> Rdk.Geometry.point_count random_geometry
          | Vertex -> Rdk.Geometry.vertex_count random_geometry | Primitive -> Rdk.Geometry.primitive_count random_geometry | Detail -> 1 in
        let values value = Array.make count value in
        let storage = match dimension with
          | 1 -> Rdk.Attribute.Float (values 0.6)
          | 2 -> Rdk.Attribute.Float2 (Rdk.Packed.Float2.of_owned ~x:(values 0.6) ~y:(values 0.7) |> Result.get_ok)
          | 3 -> Rdk.Attribute.Float3 (Rdk.Packed.Float3.of_owned ~x:(values 0.6) ~y:(values 0.7) ~z:(values 0.8) |> Result.get_ok)
          | _ -> Rdk.Attribute.Float4 (Rdk.Packed.Float4.of_owned ~x:(values 0.6) ~y:(values 0.7) ~z:(values 0.8) ~w:(values 0.9) |> Result.get_ok) in
        let attribute = Rdk.Attribute.create_owned ~owner ~name:"random" storage |> Result.get_ok in
        let random_geometry = Rdk.Geometry.with_attribute attribute random_geometry |> Result.get_ok in
        let random_input = Lisp_sop.snapshot (random_geometry) in
        List.iter (fun (operation,operation_label) ->
      List.iter (fun sampling ->
        let context_seed = sampling = 1 and use_fraction = sampling = 2 in
        let fraction_dimension = match distribution_label with
          | "Two values" | "Custom discrete" -> 1
          | "Direction" -> dimension - 1 | _ -> dimension in
        let fraction_attribute = if use_fraction then "fraction" ^ string_of_int fraction_dimension else "" in
        let seed_attribute = if sampling = 0 || use_fraction then "seed_values" else "" in
        let bias = if directional then 0.4 else 0. in
        let typed = sop ~inputs:[random_input] "attribute_randomize"
          [ks "owner" owner_label; ks "kind" kind_label; ks "distribution" distribution_label; ks "operation" operation_label;
           kb "context_seed" context_seed; ki "seed" 17; ks "seed_attribute" seed_attribute;
           ks "fraction_attribute" fraction_attribute; kv "a" a; kf "a_w" 0.5; kv "b" b; kf "b_w" 1.1;
           kf "cone_angle" 0.7; ki "dimensions" (max 2 dimension); ks "ramp" "0:0,0.5:0.2,1:1"; ks "entries" entries;
           kf "direction_bias" bias; kf "scale" 0.8; kb "use_minimum" true; kf "minimum" (-2.);
           kb "use_maximum" true; kf "maximum" 2.] in
        same_cook "randomize dimension/operation/sampling" ~typed ~factory:Nodes.Attribute_randomize.factory
          ["owner",Param.Choice_value owner_label;"kind",Choice_value kind_label;"distribution",Choice_value distribution_label;
           "operation",Choice_value operation_label;"context_seed",Bool_value context_seed;"seed",Int_value 17;
           "seed_attribute",Text_value seed_attribute;"fraction_attribute",Text_value fraction_attribute;
           "a_x",Float_value 0.2;"a_y",Float_value 0.3;"a_z",Float_value 0.4;"a_w",Float_value 0.5;
           "b_x",Float_value 1.1;"b_y",Float_value 1.1;"b_z",Float_value 1.1;"b_w",Float_value 1.1;
           "cone_angle",Float_value 0.7;"dimensions",Int_value (max 2 dimension);"ramp",Text_value "0:0,0.5:0.2,1:1";
           "entries",Text_value entries;"direction_bias",Float_value bias;"scale",Float_value 0.8;
           "use_minimum",Bool_value true;"minimum",Float_value (-2.);"use_maximum",Bool_value true;"maximum",Float_value 2.] random_input;
        let seed = Rand.seed (if use_fraction then 0 else if context_seed then expected_random_seed () else 17) in
        let native = Rdk.Attribute_ops.randomize ~seed ~owner ~name:"random" ~operation ~direction_bias:bias ~scale:0.8
          ?seed_attribute:(if sampling = 0 then Some "seed_values" else None)
          ?fraction_attribute:(if use_fraction then Some fraction_attribute else None)
          ~minimum:(numeric dimension (-2.) (-2.) (-2.) (-2.)) ~maximum:(numeric dimension 2. 2. 2. 2.) native_distribution random_geometry |> Result.get_ok in
        check (close_blend native (cook 1 typed)) "expanded randomize controls match native";
        check (Context.Dependencies.mem Context.Dependencies.Seed (Node.dependencies typed) = context_seed)
          "randomize declares only the active context seed dependency") [0;1;2]) random_operations) random_owners)) modes) random_kinds;
  let selection_source = Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(-> (sop/group_range (sop/ext_quat_source) :name "partial" :end_ 0)
     (sop/group_range :owner "Vertices" :name "partial" :end_ 0)
     (sop/group_range :owner "Primitives" :name "partial" :end_ 0)
     (sop/group_edges :name "partial" :incidence "Boundary"))|} in
  let selection_geometry = cook 1 selection_source in
  let selection_input = Lisp_sop.snapshot (selection_geometry) in
  let selectors = [
    "Point",Rdk.Attribute_ops.Random_points (Rdk.Geometry.find_group ~owner:Rdk.Group.Point "partial" selection_geometry |> Option.get);
    "Vertex",Random_vertices (Rdk.Geometry.find_group ~owner:Rdk.Group.Vertex "partial" selection_geometry |> Option.get);
    "Primitive",Random_primitives (Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "partial" selection_geometry |> Option.get);
    "Edge",Random_edges (Rdk.Geometry.find_edge_group "partial" selection_geometry |> Option.get)] in
  List.iter (fun (kind_label,dimension) ->
    List.iter (fun (owner,owner_label) -> if owner <> Rdk.Attribute.Detail then
    List.iter (fun (selection_label,element_selection) ->
    List.iter (fun vector_limits ->
      let typed = sop ~inputs:[selection_input] "attribute_randomize"
        [ks "owner" owner_label; ks "kind" kind_label; ks "selection_owner" selection_label;
         ks "selection_group" "partial"; ki "seed" 17; kb "use_minimum" true; kf "minimum" (-1.);
         kb "use_maximum" true; kf "maximum" 1.; kb "use_vector_limits" vector_limits;
         kv "minimum_vector" (Vec3.create (-1.) (-2.) (-3.)); kf "minimum_w" (-4.);
         kv "maximum_vector" (Vec3.create 1. 2. 3.); kf "maximum_w" 4.] in
      same_cook "randomize selection and component limits" ~typed ~factory:Nodes.Attribute_randomize.factory
        ["owner",Param.Choice_value owner_label;"kind",Choice_value kind_label;"selection_owner",Choice_value selection_label;
         "selection_group",Text_value "partial";"seed",Int_value 17;"use_minimum",Bool_value true;"minimum",Float_value (-1.);
         "use_maximum",Bool_value true;"maximum",Float_value 1.;"use_vector_limits",Bool_value vector_limits;
         "minimum_vector_x",Float_value (-1.);"minimum_vector_y",Float_value (-2.);"minimum_vector_z",Float_value (-3.);"minimum_w",Float_value (-4.);
         "maximum_vector_x",Float_value 1.;"maximum_vector_y",Float_value 2.;"maximum_vector_z",Float_value 3.;"maximum_w",Float_value 4.] selection_input;
      let minimum = if vector_limits then numeric dimension (-1.) (-2.) (-3.) (-4.) else numeric dimension (-1.) (-1.) (-1.) (-1.)
      and maximum = if vector_limits then numeric dimension 1. 2. 3. 4. else numeric dimension 1. 1. 1. 1. in
      let native = Rdk.Attribute_ops.randomize ~element_selection ~seed:(Rand.seed 17) ~owner ~name:"random" ~minimum ~maximum
          (Rdk.Attribute_ops.Random_uniform {min=numeric dimension 0. 0. 0. 0.;max=numeric dimension 1. 1. 1. 1.}) selection_geometry |> Result.get_ok in
      check (close_blend native (cook 1 typed)) "randomize selection and component limits match native") [false;true]) selectors) random_owners) random_kinds;
  List.iter (fun (owner,owner_label) ->
    let text = "low\t1\nhigh\\tvalue\t3" in
    let typed = sop ~inputs:[random_input] "attribute_randomize"
      [ks "owner" owner_label; ks "distribution" "Custom discrete text"; ks "text_entries" text;
       ks "fraction_attribute" "fraction1"; ks "seed_attribute" "ignored"; kb "context_seed" true; ki "seed" 99] in
    same_cook "randomize text fractions ignore seeds" ~typed ~factory:Nodes.Attribute_randomize.factory
      ["owner",Param.Choice_value owner_label;"distribution",Choice_value "Custom discrete text";"text_entries",Text_value text;
       "fraction_attribute",Text_value "fraction1";"seed_attribute",Text_value "ignored";"context_seed",Bool_value true;"seed",Int_value 99] random_input;
    let native = Rdk.Attribute_ops.randomize ~seed:(Rand.seed 0) ~owner ~name:"random" ~fraction_attribute:"fraction1"
      (Rdk.Attribute_ops.Random_custom_discrete_text ["low",1.;"high\tvalue",3.]) random_geometry |> Result.get_ok in
    check (close_blend native (cook 1 typed)) "randomize weighted text matches native";
    check (not (Context.Dependencies.mem Context.Dependencies.Seed (Node.dependencies typed))) "fraction text ignores context seed") random_owners;
  let session = Session.create ~max_entries:32 ~max_payload_bytes:2_000_000 |> Result.get_ok in
  let context seed = Context.create ~domains:1 ~seed () |> Result.get_ok in
  List.iter (fun mode ->
    let node = Lisp_sop.node ~with_:["random_input", (random_input)] (Printf.sprintf {|(sop/attribute_randomize
   (sop/ext_random_input)
   :context_seed %b
   :seed 17
   :fraction_attribute %S
   :seed_attribute %S)|} ((mode <> 0)) ((if mode = 2 then "fraction1" else "")) ((if mode = 2 then "ignored" else ""))) in
    let first = Session.cook session ~context:(context 42L) node |> Result.get_ok
    and second = Session.cook session ~context:(context 43L) node |> Result.get_ok in
    check (if mode = 1 then not (equal_geometry (Result.get_ok (Sop.Payload.geometry first.payload)) (Result.get_ok (Sop.Payload.geometry second.payload))) else (Result.get_ok (Sop.Payload.geometry first.payload)) == (Result.get_ok (Sop.Payload.geometry second.payload)))
      "randomize context invalidation follows the active sampling mode") [0;1;2];
  Session.close session;
  same_cook "randomize blank optional names" ~typed:(Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :group " "
   :selection_group " "
   :seed_attribute " "
   :fraction_attribute " ")|})
    ~factory:Nodes.Attribute_randomize.factory ["group",Param.Text_value " ";"selection_group",Text_value " ";"seed_attribute",Text_value " ";"fraction_attribute",Text_value " "] random_input;
  List.iter (fun (field : Param.field_view) -> match field.kind with
    | Param.Floating_view _ -> check (Result.is_error (Node.apply_parameters randomized [field.name,Param.Float_value Float.nan]))
        "randomize inspector refuses every nonfinite field"
    | _ -> ()) (Node.parameter_fields randomized);
  List.iter (fun construct -> check (rejected construct)
      "randomize rejects invalid distribution controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize (sop/ext_random_input) :name " ")|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :owner "Detail"
   :selection_group "partial")|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize (sop/ext_random_input) :group "a" :selection_group "b")|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :kind "Vector 3"
   :distribution "Cauchy"
   :b [1.0 2.0 3.0])|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize (sop/ext_random_input) :distribution "Normal" :b [-1.0 0.0 0.0])|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize (sop/ext_random_input) :distribution "Exponential")|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :distribution "Log normal"
   :a [0.1 0.1 0.1]
   :b [-1.0 0.0 0.0])|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] (Printf.sprintf {|(sop/attribute_randomize
   (sop/ext_random_input)
   :kind "Vector 3"
   :distribution "Direction"
   :a %s
   :cone_angle %s)|} ((Lisp_sop.vec3 Vec3.unit_x)) ((Lisp_sop.float (Float.pi +. 0.1)))));
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :distribution "Inside sphere"
   :dimensions 1)|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize (sop/ext_random_input) :direction_bias 0.2)|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] (Printf.sprintf {|(sop/attribute_randomize
   (sop/ext_random_input)
   :kind "Vector 3"
   :distribution "Direction"
   :a %s
   :direction_bias -1.0)|} ((Lisp_sop.vec3 Vec3.unit_x))));
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :distribution "Custom ramp"
   :ramp "0:0,0:1")|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :distribution "Custom discrete"
   :entries "1	0
2	0")|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :distribution "Custom discrete"
   :entries "1	inf")|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :kind "Vector 3"
   :distribution "Custom discrete"
   :entries "1	1")|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :distribution "Custom discrete text"
   :scale 0.5)|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :distribution "Custom discrete text"
   :use_minimum true)|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize (sop/ext_random_input) :name "P")|});
     (fun () -> Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize
   (sop/ext_random_input)
   :distribution "Inside sphere"
   :dimensions 4
   :use_minimum true)|})];
  cache_identity ~companions:["direction_bias",["distribution",Param.Choice_value "Direction";
    "kind",Choice_value "Vector 3";"a_x",Float_value 1.]] "randomize context-seed fields"
    (Lisp_sop.node ~with_:["random_input", (random_input)] {|(sop/attribute_randomize (sop/ext_random_input) :context_seed true :seed 17)|});
  List.iter (fun (operation,operation_label) ->
    let typed = sop ~inputs:[selection_input] "attribute_randomize"
      [ks "name" "P"; ks "kind" "Vector 3"; ks "operation" operation_label] in
    same_cook "randomize positions" ~typed ~factory:Nodes.Attribute_randomize.factory
      ["name",Param.Text_value "P";"kind",Choice_value "Vector 3";"operation",Choice_value operation_label] selection_input;
    let native = Rdk.Attribute_ops.randomize ~seed:(Rand.seed 0) ~owner:Rdk.Attribute.Point ~name:"P" ~operation
      (Rdk.Attribute_ops.Random_uniform {min=Vec3 Vec3.zero;max=Vec3 (Vec3.create 1. 1. 1.)}) selection_geometry |> Result.get_ok in
    check (close_blend native (cook 1 typed)) "randomize position writes match native") random_operations;
  let noise_input = quat_source and noise_geometry = quat_geometry in
  let noise = Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input))|} in
  same_cook "attribute noise defaults" ~typed:noise ~factory:Nodes.Attribute_noise.factory [] noise_input;
  cache_identity ~changes:["blend",Param.Float_value 0.75] "attribute noise all fields" noise;
  let kinds = [Rdk.Attribute_ops.Noise_float,"Float",1;Noise_vector,"Vector",3;Noise_quaternion,"Quaternion",4] in
  let operations = [Rdk.Attribute_ops.Noise_set_initial,"Set initial";Noise_set,"Set";Noise_add,"Add";
    Noise_subtract,"Subtract";Noise_multiply,"Multiply";Noise_minimum,"Minimum";Noise_maximum,"Maximum"] in
  let noise_owners = [Rdk.Attribute.Point,"Point",Some Rdk.Group.Point;
    Rdk.Attribute.Vertex,"Vertex",Some Rdk.Group.Vertex;Rdk.Attribute.Primitive,"Primitive",Some Rdk.Group.Primitive;
    Rdk.Attribute.Detail,"Detail",None] in
  let expected_noise_seed context_seed =
    let hash = Rays_math.Hash.string "attribute_noise:attribute_noise" in
    let mixed = Int64.logxor context_seed (Int64.mul hash 0x9e3779b97f4a7c15L) in
    Int64.to_int (Int64.logxor mixed (Int64.shift_right_logical mixed 32)) in
  List.iter (fun (owner,owner_choice,group_owner) ->
    List.iter (fun (kind,kind_choice,dimension) ->
    List.iter (fun (operation,operation_choice) ->
    if kind <> Rdk.Attribute_ops.Noise_quaternion || operation = Rdk.Attribute_ops.Noise_set || operation = Rdk.Attribute_ops.Noise_set_initial then (
    List.iter (fun (location_choice,native_location) ->
    List.iter (fun (range_choice,native_range) ->
    List.iter (fun context_seed ->
      let typed = sop ~inputs:[noise_input] "attribute_noise"
          [ks "owner" owner_choice; ks "name" "sample"; ks "kind" kind_choice; ks "operation" operation_choice;
           ks "location" location_choice; ks "location_attribute" "loc"; ks "range" range_choice;
           kv "min" (Vec3.create (-0.5) (-0.4) (-0.3)); kf "min_w" (-0.2); kv "max" (Vec3.create 0.5 0.6 0.7); kf "max_w" 0.8;
           kb "context_seed" context_seed; ki "seed" 17; kf "blend" 0.6; kv "frequency" (Vec3.create 0.5 1. 1.5);
           kv "offset" (Vec3.create 0.1 0.2 0.3); ki "octaves" 3; kf "lacunarity" 1.7; kf "roughness" 0.3] in
      same_cook "attribute noise controls" ~typed ~factory:Nodes.Attribute_noise.factory
        ["owner",Param.Choice_value owner_choice;"name",Text_value "sample";"kind",Choice_value kind_choice;
         "operation",Choice_value operation_choice;"location",Choice_value location_choice;"location_attribute",Text_value "loc";
         "range",Choice_value range_choice;"min_x",Float_value (-0.5);"min_y",Float_value (-0.4);"min_z",Float_value (-0.3);"min_w",Float_value (-0.2);
         "max_x",Float_value 0.5;"max_y",Float_value 0.6;"max_z",Float_value 0.7;"max_w",Float_value 0.8;
         "context_seed",Bool_value context_seed;"seed",Int_value 17;"blend",Float_value 0.6;
         "frequency_x",Float_value 0.5;"frequency_y",Float_value 1.;"frequency_z",Float_value 1.5;
         "offset_x",Float_value 0.1;"offset_y",Float_value 0.2;"offset_z",Float_value 0.3;
         "octaves",Int_value 3;"lacunarity",Float_value 1.7;"roughness",Float_value 0.3] noise_input;
      let seed = if context_seed then expected_noise_seed 42L else 17 in
      let native = Rdk.Attribute_ops.noise ~owner ~name:"sample" ~kind ~operation ~location:native_location ~range:native_range
          ~seed ~blend:0.6 ~frequency:(Vec3.create 0.5 1. 1.5) ~offset:(Vec3.create 0.1 0.2 0.3)
          ~octaves:3 ~lacunarity:1.7 ~roughness:0.3 noise_geometry |> Result.get_ok in
      check (close_blend native (cook 1 typed)) "attribute noise controls match native";
      check (Context.Dependencies.mem Context.Dependencies.Seed (Node.dependencies typed) = context_seed)
        "attribute noise declares its context seed dependency") [false;true])
      ["Positive",Rdk.Attribute_ops.Noise_positive;
       "Zero centered",Rdk.Attribute_ops.Noise_zero_centered;
       "Minimum / maximum",Rdk.Attribute_ops.Noise_min_max
        ((match dimension with 1 -> Rdk.Attribute_ops.Scalar (-0.5) | 3 -> Rdk.Attribute_ops.Vec3 (Vec3.create (-0.5) (-0.4) (-0.3)) | _ -> Vec4 (-0.5,-0.4,-0.3,-0.2)),
         (match dimension with 1 -> Rdk.Attribute_ops.Scalar 0.5 | 3 -> Rdk.Attribute_ops.Vec3 (Vec3.create 0.5 0.6 0.7) | _ -> Vec4 (0.5,0.6,0.7,0.8)))])
      ["Position",Rdk.Attribute_ops.Noise_position;
       "Element number",Rdk.Attribute_ops.Noise_element_number;
       "Attribute",Rdk.Attribute_ops.Noise_attribute "loc"];
    Option.iter (fun group_owner ->
      let typed = sop ~inputs:[noise_input] "attribute_noise"
        [ks "owner" owner_choice; ks "kind" kind_choice; ks "operation" operation_choice; ks "group" "selected"] in
      same_cook "attribute noise selected group" ~typed ~factory:Nodes.Attribute_noise.factory
        ["owner",Param.Choice_value owner_choice;"kind",Choice_value kind_choice;
          "operation",Choice_value operation_choice;"group",Text_value "selected"] noise_input;
      let selection = Rdk.Geometry.find_group ~owner:group_owner "selected" noise_geometry |> Option.get in
      let native = Rdk.Attribute_ops.noise ~selection ~seed:0 ~owner ~name:"noise" ~kind ~operation noise_geometry |> Result.get_ok in
      check (close_blend native (cook 1 typed)) "attribute noise selected group matches native") group_owner
    )) operations) kinds) noise_owners;
  List.iter (fun (owner,owner_choice,_) -> List.iter (fun (kind,kind_choice,dimension) ->
    let count = match owner with Rdk.Attribute.Point -> Rdk.Geometry.point_count noise_geometry
      | Vertex -> Rdk.Geometry.vertex_count noise_geometry | Primitive -> Rdk.Geometry.primitive_count noise_geometry | Detail -> 1 in
    let values value = Array.make count value in
    let storage = match dimension with
      | 1 -> Rdk.Attribute.Float (values 0.4)
      | 3 -> Rdk.Attribute.Float3 (Rdk.Packed.Float3.of_owned ~x:(values 0.2) ~y:(values 0.3) ~z:(values 0.4) |> Result.get_ok)
      | _ -> Rdk.Attribute.Float4 (Rdk.Packed.Float4.of_owned ~x:(values 0.) ~y:(values 0.) ~z:(values 0.) ~w:(values 1.) |> Result.get_ok) in
    let attribute = Rdk.Attribute.create_owned ~owner ~name:"existing" storage |> Result.get_ok in
    let geometry = Rdk.Geometry.with_attribute attribute noise_geometry |> Result.get_ok in
    let input = Lisp_sop.snapshot (geometry) in
    List.iter (fun (operation,operation_choice) ->
      if kind <> Rdk.Attribute_ops.Noise_quaternion || operation = Rdk.Attribute_ops.Noise_set || operation = Rdk.Attribute_ops.Noise_set_initial then (
        let typed = sop ~inputs:[input] "attribute_noise"
          [ks "owner" owner_choice; ks "kind" kind_choice; ks "name" "existing"; ks "operation" operation_choice; kf "blend" 0.5] in
        same_cook "attribute noise existing target" ~typed ~factory:Nodes.Attribute_noise.factory
          ["owner",Param.Choice_value owner_choice;"kind",Choice_value kind_choice;"name",Text_value "existing";
            "operation",Choice_value operation_choice;"blend",Float_value 0.5] input;
        let native = Rdk.Attribute_ops.noise ~seed:0 ~owner ~kind ~name:"existing" ~operation ~blend:0.5 geometry |> Result.get_ok in
        check (close_blend native (cook 1 typed)) "attribute noise existing target matches native")) operations
    ) kinds) noise_owners;
  List.iter (fun (operation,operation_choice) ->
    let typed = sop ~inputs:[noise_input] "attribute_noise"
      [ks "kind" "Vector"; ks "name" "P"; ks "operation" operation_choice] in
    same_cook "attribute noise positions" ~typed ~factory:Nodes.Attribute_noise.factory
      ["kind",Param.Choice_value "Vector";"name",Text_value "P";"operation",Choice_value operation_choice] noise_input;
    let native = Rdk.Attribute_ops.noise ~seed:0 ~owner:Rdk.Attribute.Point ~kind:Rdk.Attribute_ops.Noise_vector ~name:"P" ~operation noise_geometry |> Result.get_ok in
    check (close_blend native (cook 1 typed)) "attribute noise positions match native") operations;
  List.iter (fun (_kind,kind_choice) ->
    let typed = sop ~inputs:[noise_input] "attribute_noise" [ks "name" "P"; ks "kind" kind_choice; ks "operation" "Set initial"] in
    same_cook "attribute noise initial P keeps geometry" ~typed
      ~factory:Nodes.Attribute_noise.factory ["name",Param.Text_value "P";"kind",Choice_value kind_choice;
        "operation",Choice_value "Set initial"] noise_input;
    check (equal_geometry (cook 1 typed) noise_geometry) "attribute noise set-initial on existing P changed the geometry")
    [Rdk.Attribute_ops.Noise_float,"Float";Noise_quaternion,"Quaternion"];
  List.iter (fun roughness -> List.iter (fun blend -> List.iter (fun octaves ->
    let frequency = Vec3.create (-1.) 0. 2. in
    let typed = Lisp_sop.node ~with_:["noise_input", (noise_input)] (Printf.sprintf {|(sop/attribute_noise
   (sop/ext_noise_input)
   :frequency %s
   :lacunarity 0.5
   :roughness %s
   :blend %s
   :octaves %d)|} ((Lisp_sop.vec3 frequency)) ((Lisp_sop.float roughness)) ((Lisp_sop.float blend)) (octaves)) in
    same_cook "attribute noise boundaries" ~typed ~factory:Nodes.Attribute_noise.factory
      ["frequency_x",Param.Float_value (-1.);"frequency_y",Float_value 0.;"frequency_z",Float_value 2.;
        "lacunarity",Float_value 0.5;"roughness",Float_value roughness;"blend",Float_value blend;"octaves",Int_value octaves] noise_input;
    let native = Rdk.Attribute_ops.noise ~seed:0 ~owner:Rdk.Attribute.Point ~name:"noise" ~kind:Rdk.Attribute_ops.Noise_float
        ~frequency ~lacunarity:0.5 ~roughness ~blend ~octaves noise_geometry |> Result.get_ok in
    check (close_blend native (cook 1 typed)) "attribute noise boundaries match native") [1;64]) [0.;1.]) [0.;1.];
  same_cook "attribute noise blank group" ~typed:(Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :group " ")|})
    ~factory:Nodes.Attribute_noise.factory ["group",Param.Text_value " "] noise_input;
  let fixed = Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :seed 17)|}
  and seeded = Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :context_seed true :seed 17)|} in
  cache_identity ~changes:["blend",Param.Float_value 0.75] "attribute noise context-seed fields" seeded;
  let session = Session.create ~max_entries:16 ~max_payload_bytes:1_000_000 |> Result.get_ok in
  let context seed = Context.create ~domains:1 ~seed () |> Result.get_ok in
  let first = Session.cook session ~context:(context 42L) fixed |> Result.get_ok in
  let second = Session.cook session ~context:(context 43L) fixed |> Result.get_ok in
  check ((Result.get_ok (Sop.Payload.geometry first.payload)) == (Result.get_ok (Sop.Payload.geometry second.payload))) "explicit noise seed hits across context changes";
  let first = Session.cook session ~context:(context 42L) seeded |> Result.get_ok in
  let second = Session.cook session ~context:(context 43L) seeded |> Result.get_ok in
  check (not (equal_geometry (Result.get_ok (Sop.Payload.geometry first.payload)) (Result.get_ok (Sop.Payload.geometry second.payload)))) "context noise seed changes the cooked output";
  Session.close session;
  List.iter (fun construct -> check (rejected construct)
      "attribute noise refuses invalid construction")
    [(fun () -> Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :name " ")|});
     (fun () -> Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :owner "Detail" :group "selected")|});
     (fun () -> Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :location "Attribute" :location_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :blend 1.1)|});
     (fun () -> Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :octaves 65)|});
     (fun () -> Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :lacunarity 0.0)|});
     (fun () -> Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :roughness -0.1)|});
     (fun () -> Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :kind "Quaternion" :operation "Add")|});
     (fun () -> Lisp_sop.node ~with_:["noise_input", (noise_input)] {|(sop/attribute_noise (sop/ext_noise_input) :range "Minimum / maximum" :min [2.0 0.0 0.0])|})];
  List.iter (fun (field : Param.field_view) -> match field.kind with
    | Param.Floating_view _ -> check (Result.is_error (Node.apply_parameters noise [field.name,Param.Float_value Float.nan]))
        "attribute noise inspector refuses every nonfinite field"
    | _ -> ()) (Node.parameter_fields noise);
  let quat = Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source))|} in
  same_cook "quaternion noise defaults" ~typed:quat ~factory:Nodes.Attribute_noise_quaternion.factory [] quat_source;
  cache_identity "quaternion noise all fields"
    ~changes:["location",Param.Text_value "position";"range",Text_value "positive"] quat;
  List.iter (fun (owner,owner_choice,group_owner) -> List.iter (fun selected ->
    List.iter (fun (location,native_location) -> List.iter (fun (range,native_range) ->
    List.iter (fun seed -> List.iter (fun octaves ->
      let group = if selected then "selected" else "" in
      let frequency = Vec3.create 0.5 1. 1.5 in
      let typed = sop ~inputs:[quat_source] "attribute_noise_quaternion"
        [ks "group" group; ks "owner" owner_choice; ks "name" "orient"; ks "location" location; ks "range" range;
         ki "seed" seed; kv "frequency" frequency; ki "octaves" octaves] in
      same_cook "quaternion noise controls" ~typed ~factory:Nodes.Attribute_noise_quaternion.factory
        ["group",Param.Text_value group;"owner",Choice_value owner_choice;"name",Text_value "orient";
         "location",Text_value location;"range",Text_value range;"seed",Int_value seed;"frequency_x",Float_value 0.5;
         "frequency_y",Float_value 1.;"frequency_z",Float_value 1.5;"octaves",Int_value octaves] quat_source;
      let selection = if selected then Option.bind group_owner (fun owner -> Rdk.Geometry.find_group ~owner "selected" quat_geometry) else None in
      let native = Rdk.Attribute_ops.noise ~grain:97 ?selection ~seed ~owner ~name:"orient" ~kind:Rdk.Attribute_ops.Noise_quaternion
        ~location:native_location ~range:native_range ~operation:Rdk.Attribute_ops.Noise_set ~blend:1. ~frequency ~offset:Vec3.zero
        ~octaves ~lacunarity:2. ~roughness:0.5 quat_geometry |> Result.get_ok in
      check (close_blend native (cook 1 typed)) "quaternion noise flat encodings match native"
    ) [1;3]) [0;17])
      ["positive",Rdk.Attribute_ops.Noise_positive;"zero-centered",Rdk.Attribute_ops.Noise_zero_centered;
       "min-max;vec4,-0.5,-0.4,-0.3,-0.2;vec4,0.5,0.6,0.7,0.8",
       Rdk.Attribute_ops.Noise_min_max (Rdk.Attribute_ops.Vec4 (-0.5,-0.4,-0.3,-0.2),Rdk.Attribute_ops.Vec4 (0.5,0.6,0.7,0.8))])
      ["position",Rdk.Attribute_ops.Noise_position;"element-number",Rdk.Attribute_ops.Noise_element_number;
       "attribute:loc",Rdk.Attribute_ops.Noise_attribute "loc"])
      (if owner = Rdk.Attribute.Detail then [false] else [false;true]))
    [Rdk.Attribute.Point,"Point",Some Rdk.Group.Point;Rdk.Attribute.Vertex,"Vertex",Some Rdk.Group.Vertex;
     Rdk.Attribute.Primitive,"Primitive",Some Rdk.Group.Primitive;Rdk.Attribute.Detail,"Detail",None];
  same_cook "quaternion noise blank group" ~typed:(Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :group " ")|})
    ~factory:Nodes.Attribute_noise_quaternion.factory ["group",Param.Text_value " "] quat_source;
  check (Node.dependencies quat = Context.Dependencies.static) "quaternion noise explicit seed is static";
  List.iter (fun construct -> check (rejected construct)
    "quaternion noise refuses invalid construction")
    [(fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :name " ")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :name "P")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :owner "Detail" :group "selected")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :location "unknown")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :location "attribute: ")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :range "unknown")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :range "min-max;scalar,0;scalar,1")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion
   (sop/ext_quat_source)
   :range "min-max;vec4,nan,0,0,0;vec4,1,1,1,1")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion
   (sop/ext_quat_source)
   :range "min-max;vec4,2,0,0,0;vec4,1,1,1,1")|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :frequency [-1.0 0.0 0.0])|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :octaves 0)|});
     (fun () -> Lisp_sop.node ~with_:["quat_source", (quat_source)] {|(sop/attribute_noise_quaternion (sop/ext_quat_source) :octaves 65)|})];
  check (Result.is_error (Node.apply_parameters quat ["range",Param.Text_value "invalid"])) "quaternion noise inspector rejects malformed range";
  let wire_geometry =
    let positions = Rdk.Packed.Float3.Private.of_owned_exn
        ~x:[|0.;2.;0.;2.|] ~y:[|0.;0.;3.;3.|] ~z:(Array.make 4 0.) in
    let topology = Rdk.Topology.create_owned ~point_count:4
        ~vertex_points:[|0;1;2;3|] ~primitive_offsets:[|0;2;4|]
        ~primitive_kinds:[|Rdk.Topology.Open_polyline; Rdk.Topology.Open_polyline|] |> get in
    let selection = Rdk.Group.ordered ~owner:Rdk.Group.Primitive ~name:"first" ~length:2 [|0|] |> get
    and divisions = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"div"
        (Rdk.Attribute.Int [|4;6;5;5|]) |> get
    and segments = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"seg"
        (Rdk.Attribute.Int [|1;3;1;1|]) |> get in
    Rdk.Geometry.create ~positions ~topology ~attributes:[divisions;segments] ~groups:[selection] () |> get in
  let wire_source = Lisp_sop.snapshot (wire_geometry) in
  List.iter (fun is_sweep ->
    let factory,operation = if is_sweep then Nodes.Sweep_circle.factory,"sweep_circle" else Nodes.Polywire.factory,"polywire" in
    let construct args = sop ~inputs:[wire_source] operation args in
    let wire = construct [] in
    check (Node.operation wire = operation) "circular wire retains its runtime identity";
    check (Node.label wire = operation) "circular wire retains its default label";
    same_cook (operation ^ " defaults") ~typed:wire ~factory [] wire_source;
    cache_identity ~companions:["maximum_joint_scale_attribute",["prevent_joint_buckling",Param.Bool_value true]]
      (operation ^ " all fields") wire;
    for flags = 0 to 511 do
      let flag bit = flags land (1 lsl bit) <> 0 in
      let use_sides = flag 0 and use_segment_scales = flag 1 and prevent_joint_buckling = flag 2
      and smooth_point = flag 3 and use_max_valence = flag 4 and generate_uv = flag 5 and caps = flag 6
      and use_u_range = flag 7 and use_v_range = flag 8 in
      let typed = construct [kf "radius" 0.2; kb "use_sides" use_sides; ki "sides" 6; ki "segments" 1;
        kb "use_segment_scales" use_segment_scales; kf "first_segment_scale" 0.2; kf "last_segment_scale" 0.8;
        kb "prevent_joint_buckling" prevent_joint_buckling; kf "maximum_joint_scale" 2.; kb "smooth_point" smooth_point;
        kb "use_max_valence" use_max_valence; ki "max_valence" 3; ki "seam_offset" (-1); kb "generate_uv" generate_uv;
        kb "use_u_range" use_u_range; kf "u_min" (-1.); kf "u_max" 2.; kb "use_v_range" use_v_range; kf "v_min" 2.;
        kf "v_max" 4.; kb "caps" caps; ks "cap_group" "ends"] in
      same_cook (operation ^ " controls") ~typed ~factory
        ["radius",Param.Float_value 0.2;"use_sides",Bool_value use_sides;"sides",Int_value 6;"segments",Int_value 1;
         "use_segment_scales",Bool_value use_segment_scales;"first_segment_scale",Float_value 0.2;"last_segment_scale",Float_value 0.8;
         "prevent_joint_buckling",Bool_value prevent_joint_buckling;"maximum_joint_scale",Float_value 2.;"smooth_point",Bool_value smooth_point;
         "use_max_valence",Bool_value use_max_valence;"max_valence",Int_value 3;"seam_offset",Int_value (-1);"generate_uv",Bool_value generate_uv;
         "use_u_range",Bool_value use_u_range;"u_min",Float_value (-1.);"u_max",Float_value 2.;"use_v_range",Bool_value use_v_range;
         "v_min",Float_value 2.;"v_max",Float_value 4.;"caps",Bool_value caps;"cap_group",Text_value "ends"] wire_source;
      let sides = if use_sides then Some 6 else None and segment_scales = if use_segment_scales then Some (0.2,0.8) else None
      and max_valence = if use_max_valence then Some 3 else None and u_range = if use_u_range then Some (-1.,2.) else None
      and v_range = if use_v_range then Some (2.,4.) else None and cap_group = if caps then Some "ends" else None in
      let native = Rdk.Sweep_circle.run ~grain:97 ~radius:0.2 ?sides ~segments:1 ?segment_scales ~prevent_joint_buckling
        ~maximum_joint_scale:2. ~smooth_point ?max_valence ~seam_offset:(-1) ~generate_uv ?u_range ?v_range ~caps ?cap_group wire_geometry |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "circular wire flat controls match native"
    done;
    same_cook (operation ^ " unset names") ~typed:(construct [ks "group" " "; ks "divisions_attribute" " "; ks "segments_attribute" " ";
      ks "segment_scales_attribute" " "; ks "maximum_joint_scale_attribute" " "; ks "smooth_attribute" " "; ks "scale_attribute" " ";
      ks "seam_attribute" " "; ks "segment_seam_attribute" " "; ks "v_attribute" " "; ks "up_attribute" " "; ks "uv_range_attribute" " ";
      kb "caps" true; ks "cap_group" " "])
      ~factory ["group",Param.Text_value " ";"divisions_attribute",Text_value " ";"segments_attribute",Text_value " ";
       "segment_scales_attribute",Text_value " ";"maximum_joint_scale_attribute",Text_value " ";"smooth_attribute",Text_value " ";
       "scale_attribute",Text_value " ";"seam_attribute",Text_value " ";"segment_seam_attribute",Text_value " ";"v_attribute",Text_value " ";
       "up_attribute",Text_value " ";"uv_range_attribute",Text_value " ";"caps",Bool_value true;"cap_group",Text_value " "] wire_source;
    List.iter (fun invalid -> check (rejected invalid) "circular wire refuses invalid construction")
      [(fun () -> construct [kf "radius" 0.]);(fun () -> construct [ki "sides" 2]);(fun () -> construct [ki "segments" 0]);
       (fun () -> construct [kb "use_segment_scales" true; kf "first_segment_scale" 0.9; kf "last_segment_scale" 0.1]);
       (fun () -> construct [kf "maximum_joint_scale" 0.5]);(fun () -> construct [ki "max_valence" 0]);
       (fun () -> construct [ks "maximum_joint_scale_attribute" "limit"])];
    check (Result.is_error (Node.apply_parameters wire ["radius",Param.Float_value Float.nan])) "circular wire inspector refuses invalid radius"
  ) [false;true];
  let with_wire_attribute owner name storage geometry =
    let attribute = Rdk.Attribute.create_owned ~owner ~name storage |> Result.get_ok in
    Rdk.Geometry.with_attribute attribute geometry |> Result.get_ok in
  let f2 = Rdk.Packed.Float2.of_owned ~x:(Array.make 4 0.1) ~y:(Array.make 4 0.9) |> Result.get_ok
  and f3 = Rdk.Packed.Float3.of_owned ~x:(Array.make 4 0.) ~y:(Array.make 4 1.) ~z:(Array.make 4 0.) |> Result.get_ok
  and f4 = Rdk.Packed.Float4.of_owned ~x:(Array.make 4 0.) ~y:(Array.make 4 1.) ~z:(Array.make 4 2.) ~w:(Array.make 4 3.) |> Result.get_ok in
  let override_geometry = wire_geometry
    |> with_wire_attribute Rdk.Attribute.Point "limit" (Rdk.Attribute.Float (Array.make 4 2.))
    |> with_wire_attribute Rdk.Attribute.Point "scale" (Rdk.Attribute.Float (Array.make 4 0.8))
    |> with_wire_attribute Rdk.Attribute.Point "smooth" (Rdk.Attribute.Float (Array.make 4 1.))
    |> with_wire_attribute Rdk.Attribute.Point "seam" (Rdk.Attribute.Int (Array.make 4 1))
    |> with_wire_attribute Rdk.Attribute.Point "v" (Rdk.Attribute.Float [|0.;1.;0.;1.|])
    |> with_wire_attribute Rdk.Attribute.Point "up" (Rdk.Attribute.Float3 f3)
    |> with_wire_attribute Rdk.Attribute.Vertex "scales" (Rdk.Attribute.Float2 f2)
    |> with_wire_attribute Rdk.Attribute.Vertex "segment_seam" (Rdk.Attribute.Int (Array.make 4 1))
    |> with_wire_attribute Rdk.Attribute.Vertex "ranges" (Rdk.Attribute.Float4 f4) in
  let override_source = Lisp_sop.snapshot (override_geometry) in
  List.iter (fun is_sweep ->
    let factory,operation = if is_sweep then Nodes.Sweep_circle.factory,"sweep_circle" else Nodes.Polywire.factory,"polywire" in
    let construct args = sop ~inputs:[override_source] operation args in
    List.iter (fun selected ->
    let group = if selected then "first" else "" in
    let typed = construct [ks "group" group; ks "divisions_attribute" "div"; ki "segments" 2; ks "segments_attribute" "seg";
      ks "segment_scales_attribute" "scales"; kb "prevent_joint_buckling" true; ks "maximum_joint_scale_attribute" "limit";
      ks "smooth_attribute" "smooth"; ks "scale_attribute" "scale"; ks "seam_attribute" "seam";
      ks "segment_seam_attribute" "segment_seam"; ks "v_attribute" "v"; ks "up_attribute" "up";
      ks "uv_range_attribute" "ranges"; kb "caps" true] in
    same_cook "circular wire attribute overrides" ~typed ~factory
      ["group",Param.Text_value group;"divisions_attribute",Text_value "div";"segments",Int_value 2;"segments_attribute",Text_value "seg";
       "segment_scales_attribute",Text_value "scales";"prevent_joint_buckling",Bool_value true;"maximum_joint_scale_attribute",Text_value "limit";
       "smooth_attribute",Text_value "smooth";"scale_attribute",Text_value "scale";"seam_attribute",Text_value "seam";"segment_seam_attribute",Text_value "segment_seam";
       "v_attribute",Text_value "v";"up_attribute",Text_value "up";"uv_range_attribute",Text_value "ranges";"caps",Bool_value true] override_source;
    let primitives = if selected then Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "first" override_geometry else None in
    let native = Rdk.Sweep_circle.run ~grain:97 ?primitives ~sides:8 ~radius:0.1 ~divisions_attribute:"div" ~segments:2
      ~segments_attribute:"seg" ~segment_scales_attribute:"scales" ~prevent_joint_buckling:true ~maximum_joint_scale_attribute:"limit"
      ~smooth_attribute:"smooth" ~scale_attribute:"scale" ~seam_attribute:"seam" ~segment_seam_attribute:"segment_seam" ~v_attribute:"v"
      ~up_attribute:"up" ~u_range:(0.,1.) ~v_range:(0.,1.) ~uv_range_attribute:"ranges" ~caps:true ~cap_group:"caps" override_geometry |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "circular wire attribute overrides match native"
  ) [false;true]) [false;true];
  let tri2_node = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 1.0] [1.0 1.0 3.0] [0.0 1.0 2.0]) :closed true)
     (sop/group_range :name "selected" :range_mode "From ends")
     (sop/group_range :owner "Primitives" :name "constraints" :end_ 0)
     (sop/group_edges))|} in
  let tri2_geometry = cook 1 tri2_node in
  let uv = Rdk.Packed.Float2.of_owned ~x:[|0.;1.;1.;0.|] ~y:[|0.;0.;1.;1.|] |> Result.get_ok in
  let uv = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"uv" (Rdk.Attribute.Float2 uv) |> Result.get_ok in
  let tri2_geometry = Rdk.Geometry.with_attribute uv tri2_geometry |> Result.get_ok in
  let tri2_source = Lisp_sop.snapshot (tri2_geometry) in
  let tri2 = Lisp_sop.node ~with_:["tri2_source", (tri2_source)] {|(sop/triangulate_2d (sop/ext_tri2_source))|} in
  same_cook "triangulate 2d defaults" ~typed:tri2 ~factory:Nodes.Triangulate_2d.factory [] tri2_source;
  cache_identity "triangulate 2d all fields" tri2;
  List.iter (fun (choice,native_projection) -> List.iter (fun refine ->
    List.iter (fun use_maximum_area -> List.iter (fun use_target_edge_length ->
    List.iter (fun preserve_point_payload -> List.iter (fun restore_original_point_positions ->
      let plane_origin = Vec3.create 0.2 0.3 0.4 and plane_normal = Vec3.create 1. 1. 1. in
      let typed = sop ~inputs:[tri2_source] "triangulate_2d"
        [ks "projection" choice; kv "plane_origin" plane_origin; kv "plane_normal" plane_normal; ks "point_attribute" "uv";
         ki "seed" 17; kb "refine" refine; kf "minimum_angle" 0.1; kb "use_maximum_area" use_maximum_area;
         kf "maximum_area" 0.5; kb "use_target_edge_length" use_target_edge_length; kf "target_edge_length" 1.;
         kf "minimum_edge_length" 0.01; ki "maximum_new_points" 256; ki "regularization_steps" 1;
         kb "preserve_point_payload" preserve_point_payload; kb "restore_original_point_positions" restore_original_point_positions;
         ks "split_point_group" "split"; ks "refinement_point_group" "refined"; ks "triangle_group" "triangles";
         ks "constraint_group" "output_constraints"] in
      same_cook "triangulate 2d projection refinement controls" ~typed ~factory:Nodes.Triangulate_2d.factory
        ["projection",Param.Choice_value choice;"plane_origin_x",Float_value 0.2;"plane_origin_y",Float_value 0.3;"plane_origin_z",Float_value 0.4;
         "plane_normal_x",Float_value 1.;"plane_normal_y",Float_value 1.;"plane_normal_z",Float_value 1.;"point_attribute",Text_value "uv";"seed",Int_value 17;
         "refine",Bool_value refine;"minimum_angle",Float_value 0.1;"use_maximum_area",Bool_value use_maximum_area;"maximum_area",Float_value 0.5;
         "use_target_edge_length",Bool_value use_target_edge_length;"target_edge_length",Float_value 1.;"minimum_edge_length",Float_value 0.01;
         "maximum_new_points",Int_value 256;"regularization_steps",Int_value 1;"preserve_point_payload",Bool_value preserve_point_payload;
         "restore_original_point_positions",Bool_value restore_original_point_positions;"split_point_group",Text_value "split";
         "refinement_point_group",Text_value "refined";"triangle_group",Text_value "triangles";"constraint_group",Text_value "output_constraints"] tri2_source;
      let maximum_area = if use_maximum_area then Some 0.5 else None and target_edge_length = if use_target_edge_length then Some 1. else None in
      let native = Rdk.Triangulate2d.run ~grain:97 ~projection:native_projection ~seed:17L ~refine ~minimum_angle:0.1 ?maximum_area ?target_edge_length
        ~minimum_edge_length:0.01 ~maximum_new_points:256 ~regularization_steps:1 ~preserve_point_payload ~restore_original_point_positions
        ~split_point_group:"split" ~refinement_point_group:"refined" ~triangle_group:"triangles" ~constraint_group:"output_constraints"
        tri2_geometry |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "triangulate 2d flat projection and refinement match native"
    ) [false;true]) [false;true]) [false;true]) [false;true]) [false;true])
    ["Best fit",Rdk.Triangulate2d.Best_fit;
     "XY",Rdk.Triangulate2d.Plane_xy;"YZ",Rdk.Triangulate2d.Plane_yz;"ZX",Rdk.Triangulate2d.Plane_zx;
     "Custom plane",Rdk.Triangulate2d.Plane {origin=Vec3.create 0.2 0.3 0.4;normal=Vec3.create 1. 1. 1.};
     "Point attribute",Rdk.Triangulate2d.Point_attribute "uv"];
  for flags = 0 to 255 do
    let flag bit = flags land (1 lsl bit) <> 0 in
    let split_crossing_constraints = flag 0 and flood_from_hull_boundary = flag 1 and remove_outside_constraint_polygons = flag 2
    and silhouette_constraints = flag 3 and remove_outside_silhouette = flag 4 and ignore_non_constraint_points = flag 5
    and remove_duplicate_points = flag 6 and keep_primitives = flag 7 in
    let typed = Lisp_sop.node ~with_:["tri2_source", (tri2_source)] (Printf.sprintf {|(sop/triangulate_2d
   (sop/ext_tri2_source)
   :point_group "selected"
   :constraint_edge_group "edges"
   :constraint_primitive_group "constraints"
   :projection "XY"
   :split_crossing_constraints %b
   :flood_from_hull_boundary %b
   :remove_outside_constraint_polygons %b
   :silhouette_constraints %b
   :remove_outside_silhouette %b
   :ignore_non_constraint_points %b
   :remove_duplicate_points %b
   :keep_primitives %b
   :remove_unused_points true
   :recompute_point_normals true
   :allow_constraint_splitting false
   :allow_movement_of_interior_input_points true)|} (split_crossing_constraints) (flood_from_hull_boundary) (remove_outside_constraint_polygons) (silhouette_constraints) (remove_outside_silhouette) (ignore_non_constraint_points) (remove_duplicate_points) (keep_primitives)) in
    same_cook "triangulate 2d constraint cleanup controls" ~typed ~factory:Nodes.Triangulate_2d.factory
      ["point_group",Param.Text_value "selected";"constraint_edge_group",Text_value "edges";"constraint_primitive_group",Text_value "constraints";
       "projection",Choice_value "XY";"split_crossing_constraints",Bool_value split_crossing_constraints;"flood_from_hull_boundary",Bool_value flood_from_hull_boundary;
       "remove_outside_constraint_polygons",Bool_value remove_outside_constraint_polygons;"silhouette_constraints",Bool_value silhouette_constraints;
       "remove_outside_silhouette",Bool_value remove_outside_silhouette;"ignore_non_constraint_points",Bool_value ignore_non_constraint_points;
       "remove_duplicate_points",Bool_value remove_duplicate_points;"keep_primitives",Bool_value keep_primitives;"remove_unused_points",Bool_value true;
       "recompute_point_normals",Bool_value true;"allow_constraint_splitting",Bool_value false;"allow_movement_of_interior_input_points",Bool_value true] tri2_source;
    let selection = Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected" tri2_geometry |> Option.get |> fun g -> Rdk.Transform_ops.Selected_points g in
    let constraint_edges = Rdk.Geometry.find_edge_group "edges" tri2_geometry |> Option.get
    and constraint_primitives = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "constraints" tri2_geometry |> Option.get in
    let native = Rdk.Triangulate2d.run ~grain:97 ~selection ~constraint_edges ~constraint_primitives ~projection:Rdk.Triangulate2d.Plane_xy
      ~split_crossing_constraints ~flood_from_hull_boundary ~remove_outside_constraint_polygons ~silhouette_constraints ~remove_outside_silhouette
      ~ignore_non_constraint_points ~remove_duplicate_points ~keep_primitives ~remove_unused_points:true ~recompute_point_normals:true
      ~allow_constraint_splitting:false ~allow_movement_of_interior_input_points:true tri2_geometry |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "triangulate 2d constraint controls match native"
  done;
  same_cook "triangulate 2d blank names" ~typed:(Lisp_sop.node ~with_:["tri2_source", (tri2_source)] {|(sop/triangulate_2d
   (sop/ext_tri2_source)
   :point_group " "
   :constraint_edge_group " "
   :constraint_primitive_group " "
   :split_point_group " "
   :refinement_point_group " "
   :triangle_group " "
   :constraint_group " ")|})
    ~factory:Nodes.Triangulate_2d.factory ["point_group",Param.Text_value " ";"constraint_edge_group",Text_value " ";"constraint_primitive_group",Text_value " ";
      "split_point_group",Text_value " ";"refinement_point_group",Text_value " ";"triangle_group",Text_value " ";"constraint_group",Text_value " "] tri2_source;
  List.iter (fun construct -> check (rejected construct) "triangulate 2d refuses invalid construction")
    [(fun () -> Lisp_sop.node ~with_:["tri2_source", (tri2_source)] (Printf.sprintf {|(sop/triangulate_2d
   (sop/ext_tri2_source)
   :projection "Custom plane"
   :plane_normal %s)|} ((Lisp_sop.vec3 Vec3.zero))));
     (fun () -> Lisp_sop.node ~with_:["tri2_source", (tri2_source)] {|(sop/triangulate_2d
   (sop/ext_tri2_source)
   :projection "Point attribute"
   :point_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["tri2_source", (tri2_source)] {|(sop/triangulate_2d (sop/ext_tri2_source) :refine true :minimum_angle 0.0)|});
     (fun () -> Lisp_sop.node ~with_:["tri2_source", (tri2_source)] (Printf.sprintf {|(sop/triangulate_2d (sop/ext_tri2_source) :refine true :minimum_angle %s)|} ((Lisp_sop.float (Float.pi /. 3.)))));
     (fun () -> Lisp_sop.node ~with_:["tri2_source", (tri2_source)] {|(sop/triangulate_2d (sop/ext_tri2_source) :use_maximum_area true :maximum_area 0.0)|});
     (fun () -> Lisp_sop.node ~with_:["tri2_source", (tri2_source)] {|(sop/triangulate_2d
   (sop/ext_tri2_source)
   :use_target_edge_length true
   :target_edge_length 0.0)|});
     (fun () -> Lisp_sop.node ~with_:["tri2_source", (tri2_source)] {|(sop/triangulate_2d (sop/ext_tri2_source) :maximum_new_points -1)|});
     (fun () -> Lisp_sop.node ~with_:["tri2_source", (tri2_source)] {|(sop/triangulate_2d (sop/ext_tri2_source) :regularization_steps -1)|})];
  check (Result.is_error (Node.apply_parameters tri2 ["minimum_angle",Param.Float_value Float.nan])) "triangulate 2d inspector refuses nonfinite angles";
  let duplicate_geometry = Sources.duplicate () in
  let duplicate_source = Lisp_sop.snapshot (duplicate_geometry) in
  let duplicate = Lisp_sop.node ~with_:["duplicate_source", (duplicate_source)] {|(sop/duplicate (sop/ext_duplicate_source))|} in
  same_cook "duplicate defaults" ~typed:duplicate ~factory:Nodes.Duplicate.factory [] duplicate_source;
  cache_identity "duplicate all fields" duplicate;
  let matrices = [Mat4.identity;Mat4.translation (Vec3.create 2. 3. 4.);Mat4.rotation_z 0.7;
    Mat4.of_rows (2.,0.3,0.,1.) (0.,0.5,0.2,2.) (0.1,0.,1.5,3.) (0.01,0.02,0.03,1.);
    Mat4.of_rows (-1.,0.,0.,0.) (0.,1.,0.,0.) (0.,0.,1.,0.) (0.,0.,0.,2.)] in
  List.iter (fun matrix -> List.iter (fun copies -> List.iter (fun cumulative ->
    List.iter (fun selected -> List.iter (fun preserve_groups -> List.iter (fun named ->
      let m row column = Mat4.get matrix ~row ~column in
      let group = if selected then "right" else "" and copy_group_prefix = if named then "copy_" else "" in
      let typed = Lisp_sop.node ~with_:["duplicate_source", (duplicate_source)] (Printf.sprintf {|(sop/duplicate
   (sop/ext_duplicate_source)
   :copies %d
   :cumulative %b
   :group %S
   :copy_group_prefix %S
   :preserve_groups %b
   :m00 %s
   :m01 %s
   :m02 %s
   :m03 %s
   :m10 %s
   :m11 %s
   :m12 %s
   :m13 %s
   :m20 %s
   :m21 %s
   :m22 %s
   :m23 %s
   :m30 %s
   :m31 %s
   :m32 %s
   :m33 %s)|} (copies) (cumulative) (group) (copy_group_prefix) (preserve_groups) ((Lisp_sop.float (m 0 0))) ((Lisp_sop.float (m 0 1))) ((Lisp_sop.float (m 0 2))) ((Lisp_sop.float (m 0 3))) ((Lisp_sop.float (m 1 0))) ((Lisp_sop.float (m 1 1))) ((Lisp_sop.float (m 1 2))) ((Lisp_sop.float (m 1 3))) ((Lisp_sop.float (m 2 0))) ((Lisp_sop.float (m 2 1))) ((Lisp_sop.float (m 2 2))) ((Lisp_sop.float (m 2 3))) ((Lisp_sop.float (m 3 0))) ((Lisp_sop.float (m 3 1))) ((Lisp_sop.float (m 3 2))) ((Lisp_sop.float (m 3 3)))) in
      let values = List.init 16 (fun i -> Printf.sprintf "m%d%d" (i/4) (i mod 4),Param.Float_value (m (i/4) (i mod 4))) in
      same_cook "duplicate controls" ~typed ~factory:Nodes.Duplicate.factory
        (["copies",Param.Int_value copies;"cumulative",Bool_value cumulative;"group",Text_value group;
          "copy_group_prefix",Text_value copy_group_prefix;"preserve_groups",Bool_value preserve_groups] @ values) duplicate_source;
      let primitives = if selected then Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "right" duplicate_geometry else None in
      let copy_group_prefix = if named then Some copy_group_prefix else None in
      let native = Rdk.Instance_copy.duplicate ~grain:97 ~copies ~cumulative ~transform:matrix
        ?primitives ?copy_group_prefix ~preserve_groups duplicate_geometry |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "duplicate flat matrix controls match native"
    ) [false;true]) [false;true]) [false;true]) [false;true]) [0;1;3]) matrices;
  same_cook "duplicate blank names" ~typed:(Lisp_sop.node ~with_:["duplicate_source", (duplicate_source)] {|(sop/duplicate (sop/ext_duplicate_source) :group " " :copy_group_prefix " ")|})
    ~factory:Nodes.Duplicate.factory ["group",Param.Text_value " ";"copy_group_prefix",Text_value " "] duplicate_source;
  List.iter (fun construct -> check (rejected construct)
      "duplicate refuses invalid construction")
    [(fun () -> Lisp_sop.node ~with_:["duplicate_source", (duplicate_source)] {|(sop/duplicate (sop/ext_duplicate_source) :copies -1)|});
     (fun () -> Lisp_sop.node ~with_:["duplicate_source", (duplicate_source)] (Printf.sprintf {|(sop/duplicate (sop/ext_duplicate_source) :copies %d)|} (Sys.max_array_length)))];
  for index = 0 to 15 do
    check (Result.is_error (Node.apply_parameters duplicate
      [Printf.sprintf "m%d%d" (index/4) (index mod 4),Param.Float_value Float.nan]))
      "duplicate inspector refuses nonfinite matrix entries"
  done;
  let detect_source_node = Lisp_sop.node {|(-> (sop/grid :columns 3 :rows 3)
     (sop/group_range :owner "Primitives" :name "selected" :end_ 1))|} in
  let detect_collision_node = (let migration_matrix = Mat4.rotation_x (Float.pi /. 2.) in
Lisp_sop.node ~with_:["detect_source_node", (detect_source_node)] (Printf.sprintf {|(sop/transform
   (sop/ext_detect_source_node)
   :mode "Matrix"
   :m11 %s
   :m12 %s
   :m21 %s
   :m22 %s)|} ((Lisp_sop.float (Mat4.get migration_matrix ~row:1 ~column:1))) ((Lisp_sop.float (Mat4.get migration_matrix ~row:1 ~column:2))) ((Lisp_sop.float (Mat4.get migration_matrix ~row:2 ~column:1))) ((Lisp_sop.float (Mat4.get migration_matrix ~row:2 ~column:2))))) in
  let detect_source_geometry = cook 1 detect_source_node and detect_collision_geometry = cook 1 detect_collision_node in
  let detect_source = Lisp_sop.snapshot (detect_source_geometry) and detect_collision = Lisp_sop.snapshot (detect_collision_geometry) in
  let detect = Lisp_sop.node ~with_:["detect_source", (detect_source); "detect_collision", (detect_collision)] {|(sop/boolean_detect (sop/ext_detect_source) (sop/ext_detect_collision))|} in
  same_cook "boolean detect defaults" ~optional_inputs:[Some detect_source;Some detect_collision]
    ~typed:detect ~factory:Nodes.Boolean_detect.factory [] detect_source;
  same_cook "boolean detect disconnected defaults" ~optional_inputs:[Some detect_source;None]
    ~typed:(Lisp_sop.node ~with_:["detect_source", (detect_source)] {|(sop/boolean_detect (sop/ext_detect_source))|}) ~factory:Nodes.Boolean_detect.factory [] detect_source;
  cache_identity "boolean detect all fields" detect;
  cache_identity "boolean detect inactive collision fields" (Lisp_sop.node ~with_:["detect_source", (detect_source)] {|(sop/boolean_detect (sop/ext_detect_source))|});
  List.iter (fun use_collision -> List.iter (fun include_coplanar -> List.iter (fun use_groups ->
    for outputs = 1 to 63 do
      if use_collision || outputs land 56 <> 0 then (
        let name bit value = if outputs land (1 lsl bit) <> 0 then value else "" in
        let intersecting_group = name 0 "cross" and intersections_attribute = name 1 "cross_rows"
        and count_attribute = name 2 "cross_count" and self_intersecting_group = name 3 "self"
        and self_intersections_attribute = name 4 "self_rows" and self_count_attribute = name 5 "self_count" in
        let source_group = if use_groups then "selected" else ""
        and collision_group = if use_groups then "selected" else "" in
        let collision = if use_collision then Some detect_collision else None in
        let typed = sop ~inputs:(detect_source :: Option.to_list collision) "boolean_detect"
          [ks "source_group" source_group; ks "collision_group" collision_group; kf "tolerance" 1e-9;
           kb "include_coplanar" include_coplanar; ks "intersecting_group" intersecting_group;
           ks "intersections_attribute" intersections_attribute; ks "count_attribute" count_attribute;
           ks "self_intersecting_group" self_intersecting_group; ks "self_intersections_attribute" self_intersections_attribute;
           ks "self_count_attribute" self_count_attribute] in
        same_cook "boolean detect controls" ~optional_inputs:[Some detect_source;collision] ~typed
          ~factory:Nodes.Boolean_detect.factory
          ["source_group",Param.Text_value source_group;"collision_group",Text_value collision_group;
           "tolerance",Float_value 1e-9;"include_coplanar",Bool_value include_coplanar;
           "intersecting_group",Text_value intersecting_group;"intersections_attribute",Text_value intersections_attribute;
           "count_attribute",Text_value count_attribute;"self_intersecting_group",Text_value self_intersecting_group;
           "self_intersections_attribute",Text_value self_intersections_attribute;"self_count_attribute",Text_value self_count_attribute] detect_source;
        let optional name = if name = "" then None else Some name in
        let cross name = if use_collision then optional name else None in
        let source_primitives = if use_groups then Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "selected" detect_source_geometry else None
        and collision_primitives = if use_groups && use_collision then Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "selected" detect_collision_geometry else None in
        let collision_geometry = if use_collision then detect_collision_geometry else detect_source_geometry in
        let native = Rdk.Boolean_detect.run_checked ~grain:97 ?source_primitives ?collision_primitives
          ~tolerance:1e-9 ~include_coplanar ~intersecting_group:(cross intersecting_group)
          ?intersections_attribute:(cross intersections_attribute) ?count_attribute:(cross count_attribute)
          ?self_intersecting_group:(optional self_intersecting_group)
          ?self_intersections_attribute:(optional self_intersections_attribute) ?self_count_attribute:(optional self_count_attribute)
          ~collision:collision_geometry detect_source_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "boolean detect flat controls match native")
    done) [false;true]) [false;true]) [false;true];
  let inactive = Lisp_sop.node ~with_:["detect_source", (detect_source)] {|(sop/boolean_detect
   (sop/ext_detect_source)
   :collision_group "missing"
   :intersecting_group "same"
   :intersections_attribute "duplicate"
   :count_attribute "duplicate"
   :self_intersecting_group "same")|} in
  check (equal_geometry (cook 1 inactive) (cook 1 (Lisp_sop.node ~with_:["detect_source", (detect_source)] {|(sop/boolean_detect (sop/ext_detect_source) :self_intersecting_group "same")|})))
    "boolean detect ignores disconnected collision fields";
  same_cook "boolean detect unset names" ~optional_inputs:[Some detect_source;Some detect_collision]
    ~typed:(Lisp_sop.node ~with_:["detect_source", (detect_source); "detect_collision", (detect_collision)] {|(sop/boolean_detect
   (sop/ext_detect_source)
   (sop/ext_detect_collision)
   :source_group " "
   :collision_group " "
   :intersecting_group " "
   :intersections_attribute " "
   :count_attribute " "
   :self_intersections_attribute " "
   :self_count_attribute " ")|})
    ~factory:Nodes.Boolean_detect.factory ["source_group",Param.Text_value " ";"collision_group",Text_value " ";
      "intersecting_group",Text_value " ";"intersections_attribute",Text_value " ";"count_attribute",Text_value " ";
      "self_intersections_attribute",Text_value " ";"self_count_attribute",Text_value " "] detect_source;
  List.iter (fun construct -> check (rejected construct)
      "boolean detect refuses invalid construction")
    [ (fun () -> Lisp_sop.node ~with_:["detect_source", (detect_source)] {|(sop/boolean_detect (sop/ext_detect_source) :tolerance -0.1)|});
      (fun () -> Lisp_sop.node ~with_:["detect_source", (detect_source)] {|(sop/boolean_detect (sop/ext_detect_source) :self_intersecting_group "")|});
      (fun () -> Lisp_sop.node ~with_:["detect_source", (detect_source); "detect_collision", (detect_collision)] {|(sop/boolean_detect
   (sop/ext_detect_source)
   (sop/ext_detect_collision)
   :intersecting_group "same"
   :self_intersecting_group "same")|});
      (fun () -> Lisp_sop.node ~with_:["detect_source", (detect_source); "detect_collision", (detect_collision)] {|(sop/boolean_detect
   (sop/ext_detect_source)
   (sop/ext_detect_collision)
   :intersections_attribute "same"
   :self_count_attribute "same")|}) ];
  check (Result.is_error (Node.apply_parameters detect ["tolerance",Param.Float_value Float.nan]))
    "boolean detect inspector refuses invalid tolerance";
  let boolean_left_geometry = cook 1 (Lisp_sop.node {|(sop/box
   :normals "Auto"
   :size [2.0 2.0 2.0]
   :connectivity "Triangles"
   :consolidate_points true)|})
      |> Rdk.Geometry.without_attribute ~owner:Rdk.Attribute.Point "N"
  and boolean_right_geometry = cook 1 (Lisp_sop.node {|(sop/box
   :normals "Auto"
   :size [2.0 2.0 2.0]
   :center [0.5 0.5 0.5]
   :connectivity "Triangles"
   :consolidate_points true)|})
      |> Rdk.Geometry.without_attribute ~owner:Rdk.Attribute.Point "N" in
  let boolean_left = Lisp_sop.snapshot (boolean_left_geometry) and boolean_right = Lisp_sop.snapshot (boolean_right_geometry) in
  let boolean = Lisp_sop.node ~with_:["boolean_left", (boolean_left); "boolean_right", (boolean_right)] {|(sop/boolean (sop/ext_boolean_left) (sop/ext_boolean_right))|} in
  same_cook "boolean defaults" ~inputs:[boolean_right] ~typed:boolean ~factory:Nodes.Boolean.factory [] boolean_left;
  cache_identity "boolean all fields" boolean;
  List.iter (fun (operation,operation_choice) -> List.iter (fun (left_treatment,left_choice) ->
    List.iter (fun (right_treatment,right_choice) -> List.iter (fun (seam_points,seam_choice) ->
      List.iter (fun (detriangulation,polygon_choice) -> List.iter (fun (point_conflict,conflict_choice) ->
        if operation <> Rdk.Boolean.Shatter || left_treatment = Rdk.Boolean.Solid && right_treatment = Rdk.Boolean.Solid then (
        let typed = sop ~inputs:[boolean_left; boolean_right] "boolean"
            [ks "operation" operation_choice; ks "left_treatment" left_choice; ks "right_treatment" right_choice;
             ks "seam_points" seam_choice; ks "detriangulation" polygon_choice; ks "point_conflict" conflict_choice;
             ks "require_closed" "Allow open"; ks "piece_attribute" "cell"; ks "left_piece_group" "a";
             ks "overlap_piece_group" "overlap"; ks "right_piece_group" "b"; kf "point_tolerance" 1e-9;
             kf "tiny_seam_threshold" 1e-12] in
        same_cook "boolean operation and products" ~inputs:[boolean_right] ~typed ~factory:Nodes.Boolean.factory
          ["operation",Param.Choice_value operation_choice;"left_treatment",Choice_value left_choice;"right_treatment",Choice_value right_choice;
           "seam_points",Choice_value seam_choice;"detriangulation",Choice_value polygon_choice;"point_conflict",Choice_value conflict_choice;
           "require_closed",Choice_value "Allow open";"piece_attribute",Text_value "cell";"left_piece_group",Text_value "a";
           "overlap_piece_group",Text_value "overlap";"right_piece_group",Text_value "b";
           "point_tolerance",Float_value 1e-9;"tiny_seam_threshold",Float_value 1e-12] boolean_left;
        let native = Rdk.Boolean.run ~operation ~left_treatment ~right_treatment ~seam_points ~detriangulation ~point_conflict
            ~require_closed:false ~piece_attribute:"cell" ~left_piece_group:(Some "a") ~overlap_piece_group:(Some "overlap")
            ~right_piece_group:(Some "b") ~point_tolerance:1e-9 ~tiny_seam_threshold:1e-12 ~right:boolean_right_geometry
            boolean_left_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "boolean flat operation and product controls match native"))
      [Rdk.Boolean.Reject,"Reject conflict";Promote_to_vertex,"Promote to vertex"])
      [Rdk.Boolean.Triangles,"Triangles";Unchanged_polygons,"Unchanged polygons";All_polygons,"All polygons"])
      [Rdk.Boolean.Shared_seam_points,"Shared";Split_seam_points,"Split"])
      [Rdk.Boolean.Solid,"Solid";Surface,"Surface"])
      [Rdk.Boolean.Solid,"Solid";Surface,"Surface"])
      [Rdk.Boolean.Union,"Union";Intersection,"Intersection";Difference,"Subtract B from A";Reverse_difference,"Subtract A from B";
       Xor,"Exclusive or";Shatter,"Shatter"];
  List.iter (fun (closed_choice,native_closed) -> List.iter (fun resolve_left_self_intersections ->
    List.iter (fun resolve_right_self_intersections -> List.iter (fun strict_cleanup -> List.iter (fun assume_flat ->
      List.iter (fun cleanup_max_batches ->
        let typed = sop ~inputs:[boolean_left; boolean_right] "boolean"
            [ks "require_closed" closed_choice; kb "resolve_left_self_intersections" resolve_left_self_intersections;
             kb "resolve_right_self_intersections" resolve_right_self_intersections; kb "strict_cleanup" strict_cleanup;
             kb "assume_flat" assume_flat; ki "cleanup_max_batches" cleanup_max_batches] in
        same_cook "boolean closed and cleanup" ~inputs:[boolean_right] ~typed ~factory:Nodes.Boolean.factory
          ["require_closed",Param.Choice_value closed_choice;"resolve_left_self_intersections",Bool_value resolve_left_self_intersections;
           "resolve_right_self_intersections",Bool_value resolve_right_self_intersections;"strict_cleanup",Bool_value strict_cleanup;
           "assume_flat",Bool_value assume_flat;"cleanup_max_batches",Int_value cleanup_max_batches] boolean_left;
        let native = Rdk.Boolean.run ?require_closed:native_closed ~resolve_left_self_intersections ~resolve_right_self_intersections
            ~strict_cleanup ~assume_flat ~cleanup_max_batches ~right:boolean_right_geometry boolean_left_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "boolean flat closed and cleanup controls match native") [4;8])
      [false;true]) [false;true]) [false;true]) [false;true])
      ["Operation default",None;"Require closed",Some true;"Allow open",Some false];
  same_cook "boolean unset shatter groups" ~inputs:[boolean_right]
    ~typed:(Lisp_sop.node ~with_:["boolean_left", (boolean_left); "boolean_right", (boolean_right)] {|(sop/boolean
   (sop/ext_boolean_left)
   (sop/ext_boolean_right)
   :operation "Shatter"
   :piece_attribute " "
   :left_piece_group " "
   :overlap_piece_group " "
   :right_piece_group " ")|}) ~factory:Nodes.Boolean.factory
    ["operation",Param.Choice_value "Shatter";"piece_attribute",Text_value " ";"left_piece_group",Text_value " ";
      "overlap_piece_group",Text_value " ";"right_piece_group",Text_value " "] boolean_left;
  same_cook "boolean inactive duplicate groups" ~inputs:[boolean_right]
    ~typed:(Lisp_sop.node ~with_:["boolean_left", (boolean_left); "boolean_right", (boolean_right)] {|(sop/boolean
   (sop/ext_boolean_left)
   (sop/ext_boolean_right)
   :left_piece_group "same"
   :overlap_piece_group "same"
   :right_piece_group "same")|})
    ~factory:Nodes.Boolean.factory ["left_piece_group",Param.Text_value "same";"overlap_piece_group",Text_value "same";
      "right_piece_group",Text_value "same"] boolean_left;
  List.iter (fun construct -> check (rejected construct)
      "boolean refuses invalid construction")
    [ (fun () -> Lisp_sop.node ~with_:["boolean_left", (boolean_left); "boolean_right", (boolean_right)] {|(sop/boolean (sop/ext_boolean_left) (sop/ext_boolean_right) :point_tolerance -0.1)|});
      (fun () -> Lisp_sop.node ~with_:["boolean_left", (boolean_left); "boolean_right", (boolean_right)] {|(sop/boolean (sop/ext_boolean_left) (sop/ext_boolean_right) :tiny_seam_threshold -0.1)|});
      (fun () -> Lisp_sop.node ~with_:["boolean_left", (boolean_left); "boolean_right", (boolean_right)] {|(sop/boolean (sop/ext_boolean_left) (sop/ext_boolean_right) :cleanup_max_batches 0)|});
      (fun () -> Lisp_sop.node ~with_:["boolean_left", (boolean_left); "boolean_right", (boolean_right)] {|(sop/boolean
   (sop/ext_boolean_left)
   (sop/ext_boolean_right)
   :operation "Shatter"
   :left_treatment "Surface")|});
      (fun () -> Lisp_sop.node ~with_:["boolean_left", (boolean_left); "boolean_right", (boolean_right)] {|(sop/boolean
   (sop/ext_boolean_left)
   (sop/ext_boolean_right)
   :operation "Shatter"
   :left_piece_group "same"
   :right_piece_group "same")|}) ];
  check (Result.is_error (Node.apply_parameters boolean ["point_tolerance",Param.Float_value Float.nan]))
    "boolean inspector refuses invalid tolerance";
  let subdivision_source_node = Lisp_sop.node {|(-> (sop/grid :columns 2 :rows 2 :size 2.0)
     (sop/group_range :owner "Primitives" :name "selected" :end_ 3))|}
      |> group_indices "Primitives" "subdivision_hole" [||]
      |> group_indices "Primitives" "holes" [|7|] in
  let subdivision_geometry = cook 1 subdivision_source_node in
  let uv = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Vertex ~name:"uv"
      (Rdk.Attribute.Float2 (Rdk.Packed.Float2.of_owned
        ~x:(Array.init (Rdk.Geometry.vertex_count subdivision_geometry) (fun vertex -> float_of_int vertex /. 10.))
        ~y:(Array.init (Rdk.Geometry.vertex_count subdivision_geometry) (fun vertex -> float_of_int (vertex mod 3))) |> Result.get_ok)) |> Result.get_ok in
  let subdivision_geometry = Rdk.Geometry.with_attribute uv subdivision_geometry |> Result.get_ok in
  let subdivision_source = Lisp_sop.snapshot (subdivision_geometry) in
  let subdivision_crease_node = Lisp_sop.node ~with_:["subdivision_geometry", (Lisp_sop.snapshot (subdivision_geometry))] {|(-> (sop/set_float
       (sop/ext_subdivision_geometry)
       :owner "Vertex"
       :name "creaseweight"
       :value 2.0)
     (sop/group_range :owner "Primitives" :name "creases" :range_mode "From ends"))|} in
  let subdivision_crease_geometry = cook 1 subdivision_crease_node in
  let subdivision_crease = Lisp_sop.snapshot (subdivision_crease_geometry) in
  same_cook "subdivide defaults" ~optional_inputs:[Some subdivision_source;None]
    ~typed:(Lisp_sop.node ~with_:["subdivision_source", (subdivision_source)] {|(sop/subdivide (sop/ext_subdivision_source))|}) ~factory:Nodes.Subdivide.factory [] subdivision_source;
  cache_identity ~companions:["resulting_crease_group",["generate_resulting_creases",Param.Bool_value true]]
    "subdivide all fields" (Lisp_sop.node ~with_:["subdivision_source", (subdivision_source); "subdivision_crease", (subdivision_crease)] {|(sop/subdivide (sop/ext_subdivision_source) (sop/ext_subdivision_crease))|});
  let subdivision_selection = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "selected" subdivision_geometry |> Option.get
  and subdivision_holes = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "subdivision_hole" subdivision_geometry |> Option.get
  and subdivision_crease_selection = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "creases" subdivision_crease_geometry |> Option.get in
  let subdivision_cracks = ["Do not close",Rdk.Subdivide.Subdivide_do_not_close;
    "Pull, no edge division",Subdivide_pull_no_edge_division;
    "Pull, divide edges",Subdivide_pull_divide_edges 0.75;
    "Pull, triangulate",Subdivide_pull_triangulate 0.75;
    "Stitch, no edge division",Subdivide_stitch_no_edge_division;
    "Stitch, divide edges",Subdivide_stitch_divide_edges;
    "Stitch, triangulate",Subdivide_stitch_triangulate] in
  List.iter (fun (scheme,scheme_choice) -> List.iter (fun (cracks_choice,native_cracks) ->
    List.iter (fun (explicit_weight,weight_choice) -> List.iter (fun use_creases ->
      List.iter (fun generate_resulting_creases -> List.iter (fun consistent_topology ->
        let creases = if use_creases then Some subdivision_crease else None in
        let crease_group = if use_creases then "creases" else "" in
        let resulting_crease_group = if generate_resulting_creases then "resulting" else "" in
        let typed = sop ~inputs:(subdivision_source :: Option.to_list creases) "subdivide"
            [ks "group" "selected"; ks "scheme" scheme_choice; ks "cracks" cracks_choice; kf "crack_bias" 0.75;
             ks "crease_weight_mode" weight_choice; kf "crease_weight" 2.5; ks "crease_group" crease_group;
             kb "generate_resulting_creases" generate_resulting_creases; ks "resulting_crease_group" resulting_crease_group;
             kb "consistent_topology" consistent_topology] in
        same_cook "subdivide cracks and creases" ~optional_inputs:[Some subdivision_source;creases] ~typed ~factory:Nodes.Subdivide.factory
          ["group",Param.Text_value "selected";"scheme",Choice_value scheme_choice;"cracks",Choice_value cracks_choice;
           "crack_bias",Float_value 0.75;"crease_weight_mode",Choice_value weight_choice;"crease_weight",Float_value 2.5;
           "crease_group",Text_value crease_group;"generate_resulting_creases",Bool_value generate_resulting_creases;
           "resulting_crease_group",Text_value resulting_crease_group;"consistent_topology",Bool_value consistent_topology] subdivision_source;
        let creases = Option.map (fun _ -> subdivision_crease_geometry) creases in
        let crease_primitives = if use_creases then Some subdivision_crease_selection else None
        and crease_weight = if explicit_weight then Some 2.5 else None
        and resulting_crease_group = if generate_resulting_creases then Some "resulting" else None in
        let native = Rdk.Subdivide.subdivide ~scheme ~primitives:subdivision_selection ~cracks:native_cracks
            ~consistent_topology ?creases ?crease_primitives ?crease_weight ~generate_resulting_creases ?resulting_crease_group
            ~hole_primitives:subdivision_holes ~remove_holes:false ~boundary_interpolation:Rdk.Subdivide.Subdivide_boundary_edge_and_corner
            ~face_varying_interpolation:Rdk.Subdivide.Subdivide_fvar_boundaries subdivision_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "subdivide flat crack and crease controls match native") [false;true]) [false;true])
      [false;true]) [true,"Explicit";false,"Auto"]) subdivision_cracks)
      [Rdk.Subdivide.Catmull_clark,"Catmull-Clark";Loop,"Loop";Bilinear,"Bilinear"];
  let holes = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "holes" subdivision_geometry |> Option.get in
  List.iter (fun (boundary_interpolation,boundary_choice) -> List.iter (fun (face_varying_interpolation,fvar_choice) ->
    List.iter (fun (triangle_policy,triangle_choice) -> List.iter (fun (creasing_method,creasing_choice) ->
      List.iter (fun remove_holes -> List.iter (fun treat_curves_as_independent -> List.iter (fun recompute_point_normals ->
        let typed = sop ~inputs:[subdivision_source] "subdivide"
            [ks "boundary_interpolation" boundary_choice; ks "face_varying_interpolation" fvar_choice;
             ks "triangle_policy" triangle_choice; ks "creasing_method" creasing_choice; ks "hole_group" "holes";
             kb "remove_holes" remove_holes; kb "treat_curves_as_independent" treat_curves_as_independent;
             kb "recompute_point_normals" recompute_point_normals] in
        same_cook "subdivide interpolation" ~optional_inputs:[Some subdivision_source;None] ~typed ~factory:Nodes.Subdivide.factory
          ["boundary_interpolation",Param.Choice_value boundary_choice;"face_varying_interpolation",Choice_value fvar_choice;
           "triangle_policy",Choice_value triangle_choice;"creasing_method",Choice_value creasing_choice;
           "hole_group",Text_value "holes";"remove_holes",Bool_value remove_holes;
           "treat_curves_as_independent",Bool_value treat_curves_as_independent;"recompute_point_normals",Bool_value recompute_point_normals] subdivision_source;
        let native = Rdk.Subdivide.subdivide ~boundary_interpolation ~face_varying_interpolation ~triangle_policy ~creasing_method
            ~hole_primitives:holes ~remove_holes ~treat_curves_as_independent ~recompute_point_normals
            ~crease_weight:1. ~generate_resulting_creases:false subdivision_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "subdivide interpolation controls match native") [false;true]) [false;true])
        [false;true]) [Rdk.Subdivide.Subdivide_creasing_uniform,"Uniform";Subdivide_creasing_chaikin,"Chaikin"])
        [Rdk.Subdivide.Subdivide_triangles_catmull_clark,"Catmull-Clark";Subdivide_triangles_smooth,"Smooth"])
        [Rdk.Subdivide.Subdivide_fvar_none,"None";Subdivide_fvar_corners_only,"Corners only";Subdivide_fvar_corners_plus1,"Corners plus 1";
         Subdivide_fvar_corners_plus2,"Corners plus 2";Subdivide_fvar_boundaries,"Boundaries";Subdivide_fvar_all,"All"])
        [Rdk.Subdivide.Subdivide_boundary_none,"None";Subdivide_boundary_edge_only,"Edge only";Subdivide_boundary_edge_and_corner,"Edge and corner"];
  same_cook "subdivide unset names" ~optional_inputs:[Some subdivision_source;None]
    ~typed:(Lisp_sop.node ~with_:["subdivision_source", (subdivision_source)] {|(sop/subdivide
   (sop/ext_subdivision_source)
   :group " "
   :crease_group " "
   :resulting_crease_group " "
   :hole_group " ")|})
    ~factory:Nodes.Subdivide.factory ["group",Param.Text_value " ";"crease_group",Text_value " ";
      "resulting_crease_group",Text_value " ";"hole_group",Text_value " "] subdivision_source;
  List.iter (fun construct -> check (rejected construct)
      "subdivide refuses invalid construction")
    [ (fun () -> Lisp_sop.node ~with_:["subdivision_source", (subdivision_source)] {|(sop/subdivide (sop/ext_subdivision_source) :iterations 0)|});
      (fun () -> Lisp_sop.node ~with_:["subdivision_source", (subdivision_source)] {|(sop/subdivide (sop/ext_subdivision_source) :crack_bias 1.1)|});
      (fun () -> Lisp_sop.node ~with_:["subdivision_source", (subdivision_source)] {|(sop/subdivide (sop/ext_subdivision_source) :crease_weight -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["subdivision_source", (subdivision_source)] {|(sop/subdivide (sop/ext_subdivision_source) :resulting_crease_group "creases")|});
      (fun () -> Lisp_sop.node ~with_:["subdivision_source", (subdivision_source)] {|(sop/subdivide (sop/ext_subdivision_source) :crease_group "creases")|}) ];
  check (Result.is_error (Node.apply_parameters (Lisp_sop.node ~with_:["subdivision_source", (subdivision_source)] {|(sop/subdivide (sop/ext_subdivision_source))|})
      ["crease_weight",Param.Float_value Float.nan])) "subdivide inspector refuses invalid crease weight";
  let fuse_source_node = Lisp_sop.node ~with_:["in174", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0.,0.,0.);(0.1,0.,0.);(1.,0.,0.);(1.1,0.,0.)|]))] (Printf.sprintf {|(-> (sop/set_vector (sop/ext_in174) :name "Cd" :value %s)
     (sop/set_float :name "weight" :value 2.0)
     (sop/set_float :name "radius" :value 1.0)
     (sop/set_float :name "match" :value 1.0)
     (sop/set_int :name "targetpoint")
     (sop/group_range :name "selected" :end_ 3))|} ((Lisp_sop.vec3 Vec3.unit_x))) in
  let fuse_target_node = Lisp_sop.node ~with_:["in175", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0.,0.,0.);(1.,0.,0.)|]))] (Printf.sprintf {|(-> (sop/set_vector (sop/ext_in175) :name "Cd" :value %s)
     (sop/set_float :name "weight" :value 2.0)
     (sop/set_float :name "radius" :value 1.0)
     (sop/set_float :name "match" :value 1.0)
     (sop/set_int :name "targetpoint")
     (sop/group_range :name "selected" :end_ 1))|} ((Lisp_sop.vec3 Vec3.unit_x))) in
  let fuse_source_geometry = cook 1 fuse_source_node and fuse_target_geometry = cook 1 fuse_target_node in
  let fuse_source = Lisp_sop.snapshot (fuse_source_geometry) and fuse_target = Lisp_sop.snapshot (fuse_target_geometry) in
  let fuse = Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source))|} in
  same_cook "fuse defaults" ~optional_inputs:[Some fuse_source;None] ~typed:fuse ~factory:Nodes.Fuse.factory [] fuse_source;
  cache_identity ~changes:["attribute_rules",Param.Text_value "Cd\taverage\t";"group_rules",Text_value "selected*\tunion"]
    ~companions:["match_condition",["match_attribute",Param.Text_value "match"];
      "match_tolerance",["match_attribute",Param.Text_value "match"]]
    "fuse all fields" fuse;
  let fuse_rule_source = Lisp_sop.node ~with_:["in179", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0.,0.,0.);(0.,0.,0.)|]))] {|(-> (sop/set_float (sop/ext_in179) :value 2.0)
     (sop/group_range :name "selected_subset" :end_ 0))|} in
  let fuse_rule_geometry = cook 1 fuse_rule_source in
  let fuse_rule_source = Lisp_sop.snapshot (fuse_rule_geometry) in
  let fuse_rule_node = Lisp_sop.node ~with_:["fuse_rule_source", (fuse_rule_source)] {|(sop/fuse
   (sop/ext_fuse_rule_source)
   :attribute_rules "value	sum	"
   :group_rules "selected*	intersection")|} in
  same_cook "fuse rule table payload" ~optional_inputs:[Some fuse_rule_source;None] ~typed:fuse_rule_node ~factory:Nodes.Fuse.factory
    ["attribute_rules",Param.Text_value "value\tsum\t";"group_rules",Text_value "selected*\tintersection"] fuse_rule_source;
  let fuse_rule_result = cook 1 fuse_rule_node in
  check (match Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Point "value" fuse_rule_result with
    | Some attribute -> (match Rdk.Attribute.Private.storage attribute with Rdk.Attribute.Float values -> values = [|4.|] | _ -> false)
    | None -> false) "fuse attribute table changes reduced payload";
  check (match Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected_subset" fuse_rule_result with
    | Some group -> Rdk.Group.cardinality group = 0 | None -> false) "fuse group table changes reduced membership";
  let cleanup_geometry = cook 1 (Lisp_sop.node {|(sop/curve (list [0.0 0.0 0.0] [0.0 0.0 0.0]))|}) in
  let cleanup_source = Lisp_sop.snapshot (cleanup_geometry) in
  List.iter (fun remove_degenerate_primitives -> List.iter (fun remove_unused_points_from_degenerate_primitives ->
    let typed = Lisp_sop.node ~with_:["cleanup_source", (cleanup_source)] (Printf.sprintf {|(sop/fuse
   (sop/ext_cleanup_source)
   :remove_degenerate_primitives %b
   :remove_unused_points_from_degenerate_primitives %b)|} (remove_degenerate_primitives) (remove_unused_points_from_degenerate_primitives)) in
    same_cook "fuse degenerate cleanup" ~optional_inputs:[Some cleanup_source;None] ~typed ~factory:Nodes.Fuse.factory
      ["remove_degenerate_primitives",Param.Bool_value remove_degenerate_primitives;
       "remove_unused_points_from_degenerate_primitives",Bool_value remove_unused_points_from_degenerate_primitives] cleanup_source;
    let native = Rdk.Fuse_grid.fuse ~tolerance:0.001 ~remove_degenerate_primitives
        ~remove_unused_points_from_degenerate_primitives cleanup_geometry |> Result.get_ok in
    let result = cook 1 typed in
    check (equal_geometry native result) "fuse cleanup preserves native geometry";
    check (Rdk.Geometry.primitive_count result = (if remove_degenerate_primitives then 0 else 1)
      && Rdk.Geometry.point_count result = (if remove_degenerate_primitives && remove_unused_points_from_degenerate_primitives then 0 else 1))
      "fuse cleanup changes degenerate cardinality") [false;true]) [false;true];
  let attribute_rules = "Cd\taverage\t" and group_rules = "selected*\tunion" in
  let colors = Rdk.Packed.Float3.Private.of_owned_exn
    ~x:[|0.;2.;6.;10.|] ~y:[|0.;4.;8.;12.|] ~z:[|0.;6.;10.;14.|] in
  let rule_geometry = Rdk.Geometry.with_attribute
      (Rdk.Attribute.create_owned ~owner:Point ~name:"Cd" (Float3 colors) |> get)
      fuse_source_geometry |> get in
  let rule_geometry = Rdk.Geometry.with_group
      (Rdk.Group.init ~owner:Point ~name:"selected_union" 4 (fun point -> point mod 2 = 1))
      rule_geometry |> get in
  let rule_bytes = geometry_bytes rule_geometry in
  let rule_source = Lisp_sop.snapshot rule_geometry in
  let rule_node = sop ~inputs:[rule_source] "fuse"
    [kf "tolerance" 0.3;ks "attribute_rules" attribute_rules;ks "group_rules" group_rules] in
  same_cook "fuse average/union rules" ~optional_inputs:[Some rule_source;None]
    ~typed:rule_node ~factory:Nodes.Fuse.factory
    ["tolerance",Param.Float_value 0.3;"attribute_rules",Text_value attribute_rules;
     "group_rules",Text_value group_rules] rule_source;
  let native = Rdk.Fuse_grid.fuse ~tolerance:0.3
    ~attribute_rules:[{Rdk.Fuse_reduce.pattern="Cd";method_=Attribute_average;weight_attribute=None}]
    ~group_rules:[{Rdk.Fuse_reduce.group_pattern="selected*";group_method=Group_union}]
    rule_geometry |> Result.get_ok in
  let one = cook 1 rule_node and many = cook 4 rule_node in
  check (geometry_bytes one = geometry_bytes native && geometry_bytes one = geometry_bytes many)
    "fuse average/union rules preserve native bytes across domains";
  let colors = Rdk.Geometry.find_attribute ~owner:Point "Cd" one |> Option.get
    |> Rdk.Attribute.get (Rdk.Attribute.key ~owner:Point ~name:"Cd" Rdk.Attribute.float3)
    |> Option.get |> Rdk.Packed.Float3.Private.view in
  let union = Rdk.Geometry.find_group ~owner:Point "selected_union" one |> Option.get in
  check (colors.x = [|1.;8.|] && colors.y = [|2.;10.|] && colors.z = [|3.;12.|]
      && Rdk.Group.length union = 2 && Rdk.Group.cardinality union = 2
      && Rdk.Group.mem 0 union && Rdk.Group.mem 1 union)
    "fuse rules average distinct colors and union non-first members";
  check (geometry_bytes rule_geometry = rule_bytes) "fuse rules mutated their source";
  let native_attribute_rules = [{Rdk.Fuse_reduce.pattern="Cd";method_=Attribute_average;weight_attribute=None}]
  and native_group_rules = [{Rdk.Fuse_reduce.group_pattern="selected*";group_method=Group_union}] in
  let source_selection = Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected" fuse_source_geometry |> Option.get in
  let fuse_positions = [Rdk.Fuse_reduce.First_position,"First";Least_point_position,"Least point";Greatest_point_position,"Greatest point";
    Average_position,"Average";Minimum_position,"Minimum";Maximum_position,"Maximum";Mode_position,"Mode";Median_position,"Median";
    Sum_position,"Sum";Sum_squares_position,"Sum squares";Root_mean_square_position,"Root mean square";
    Weighted_average_position,"Weighted average";Weighted_sum_position,"Weighted sum";Minimum_weight_position,"Minimum weight";
    Maximum_weight_position,"Maximum weight"] in
  List.iter (fun use_target -> List.iter (fun (specified,targeting_choice) -> List.iter (fun (using,using_choice) ->
    List.iter (fun (metric,metric_choice) -> List.iter (fun (position,position_choice) ->
      let target = if use_target then Some fuse_target else None in
      let radius_attribute,match_attribute,match_tolerance = if not specified then "radius","match",0.1 else "","",0. in
      let typed = sop ~inputs:(fuse_source :: Option.to_list target) "fuse"
          [ks "group" "selected"; ks "target_group" "selected"; ks "targeting" targeting_choice;
           ks "target_attribute" "targetpoint"; ks "using" using_choice; ks "metric" metric_choice; kf "tolerance" 0.3;
           ks "position" position_choice; ks "weight_attribute" "weight"; ks "attributes" "Average numeric";
           ks "attribute_rules" "Cd\taverage\t"; ks "group_rules" "selected*\tunion";
           ks "radius_attribute" radius_attribute; ks "match_attribute" match_attribute; kf "match_tolerance" match_tolerance;
           ks "snapped_group" "snapped"; ks "snapped_destination_attribute" "destination"] in
      same_cook "fuse targeting and reducers" ~optional_inputs:[Some fuse_source;target] ~typed ~factory:Nodes.Fuse.factory
        ["group",Param.Text_value "selected";"target_group",Text_value "selected";"targeting",Choice_value targeting_choice;
         "target_attribute",Text_value "targetpoint";"using",Choice_value using_choice;"metric",Choice_value metric_choice;
         "tolerance",Float_value 0.3;"position",Choice_value position_choice;"weight_attribute",Text_value "weight";
         "attributes",Choice_value "Average numeric";"attribute_rules",Text_value "Cd\taverage\t";"group_rules",Text_value "selected*\tunion";
         "radius_attribute",Text_value radius_attribute;"match_attribute",Text_value match_attribute;"match_tolerance",Float_value match_tolerance;
         "snapped_group",Text_value "snapped";"snapped_destination_attribute",Text_value "destination"] fuse_source;
      let targeting = if specified then Rdk.Fuse_grid.Specified_points "targetpoint" else Rdk.Fuse_grid.Near_points in
      let target_geometry = if use_target then fuse_target_geometry else fuse_source_geometry in
      let target_selection = Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected" target_geometry |> Option.get in
      let target = Option.map (fun _ -> target_geometry) target in
      let optional name = if name = "" then None else Some name in
      let native = Rdk.Fuse_grid.fuse ~selection:source_selection ~target_selection ~targeting ~using ~metric ~tolerance:0.3
          ~position ~weight_attribute:"weight" ~attributes:Rdk.Fuse_reduce.Average_numeric ~attribute_rules:native_attribute_rules
          ~group_rules:native_group_rules ?radius_attribute:(optional radius_attribute) ?match_attribute:(optional match_attribute)
          ~match_tolerance ~snapped_group:"snapped" ~snapped_destination_attribute:"destination" ~remove_degenerate_primitives:true
          ~remove_unused_points_from_degenerate_primitives:true ?target fuse_source_geometry |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "fuse targeting and reducers match native") fuse_positions)
      [Rdk.Fuse_grid.Euclidean,"Euclidean";Componentwise,"Componentwise"])
      [Rdk.Fuse_grid.Least_target_point,"Least target point";Closest_target_point,"Closest target point"])
      [false,"Near points";true,"Specified points"]) [false;true];
  List.iter (fun inclusive -> List.iter (fun match_attributes -> List.iter (fun (match_condition,condition_choice) ->
    List.iter (fun modify_target -> List.iter (fun fuse_points -> List.iter (fun keep_fused_points ->
      List.iter (fun remove_degenerate_primitives -> List.iter (fun remove_unused_points_from_degenerate_primitives ->
        List.iter (fun remove_all_unused_points ->
          if fuse_points || not keep_fused_points then (
          let target = if modify_target then None else Some fuse_target in
          let typed = sop ~inputs:(fuse_source :: Option.to_list target) "fuse"
              [kb "inclusive" inclusive; kb "match_attributes" match_attributes; ks "match_condition" condition_choice;
               kb "modify_target" modify_target; kb "fuse_points" fuse_points; kb "keep_fused_points" keep_fused_points;
               kb "remove_degenerate_primitives" remove_degenerate_primitives;
               kb "remove_unused_points_from_degenerate_primitives" remove_unused_points_from_degenerate_primitives;
               kb "remove_all_unused_points" remove_all_unused_points; kf "tolerance" 0.3;
               ks "match_attribute" "match"; kf "match_tolerance" 0.1] in
          same_cook "fuse matching and cleanup" ~optional_inputs:[Some fuse_source;target] ~typed ~factory:Nodes.Fuse.factory
            ["inclusive",Param.Bool_value inclusive;"match_attributes",Bool_value match_attributes;
             "match_condition",Choice_value condition_choice;"modify_target",Bool_value modify_target;"fuse_points",Bool_value fuse_points;
             "keep_fused_points",Bool_value keep_fused_points;"remove_degenerate_primitives",Bool_value remove_degenerate_primitives;
             "remove_unused_points_from_degenerate_primitives",Bool_value remove_unused_points_from_degenerate_primitives;
             "remove_all_unused_points",Bool_value remove_all_unused_points;"tolerance",Float_value 0.3;
             "match_attribute",Text_value "match";"match_tolerance",Float_value 0.1] fuse_source;
          let target = Option.map (fun _ -> fuse_target_geometry) target in
          let native = Rdk.Fuse_grid.fuse ~inclusive ~match_attributes ~match_condition ~modify_target ~fuse_points ~keep_fused_points
              ~remove_degenerate_primitives ~remove_unused_points_from_degenerate_primitives ~remove_all_unused_points
              ~tolerance:0.3 ~match_attribute:"match" ~match_tolerance:0.1 ?target fuse_source_geometry |> Result.get_ok in
          check (equal_geometry native (cook 1 typed)) "fuse matching and cleanup match native")) [false;true]) [false;true])
        [false;true]) [false;true]) [false;true]) [false;true])
        [Rdk.Fuse_grid.Equal_attribute_values,"Equal";Unequal_attribute_values,"Unequal"]) [false;true]) [false;true];
  same_cook "fuse unset names" ~optional_inputs:[Some fuse_source;None]
    ~typed:(Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse
   (sop/ext_fuse_source)
   :group " "
   :target_group " "
   :weight_attribute " "
   :radius_attribute " "
   :match_attribute " "
   :snapped_group " "
   :snapped_destination_attribute " ")|}) ~factory:Nodes.Fuse.factory
    ["group",Param.Text_value " ";"target_group",Text_value " ";"weight_attribute",Text_value " ";"radius_attribute",Text_value " ";
     "match_attribute",Text_value " ";"snapped_group",Text_value " ";"snapped_destination_attribute",Text_value " "] fuse_source;
  List.iter (fun construct -> check (rejected construct)
      "fuse refuses invalid construction")
    [ (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source) :tolerance -0.1)|});
      (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source) :targeting "Specified points" :target_attribute " ")|});
      (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source) :targeting "Specified points" :radius_attribute "radius")|});
      (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source) :match_tolerance 0.1)|});
      (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source) :match_condition "Unequal")|});
      (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source) :fuse_points false :keep_fused_points true)|});
      (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source); "fuse_target", (fuse_target)] {|(sop/fuse (sop/ext_fuse_source) (sop/ext_fuse_target) :modify_target true)|});
      (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source) :position "Weighted average")|});
      (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source) :attribute_rules "Cd	unknown	")|});
      (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source) :attribute_rules "Cd	weighted_average	")|});
      (fun () -> Lisp_sop.node ~with_:["fuse_source", (fuse_source)] {|(sop/fuse (sop/ext_fuse_source) :group_rules "selected*	unknown")|}) ];
  check (Result.is_error (Node.apply_parameters fuse ["match_tolerance",Param.Float_value Float.nan]))
    "fuse inspector refuses invalid tolerance";
  let analysis_source_node = Lisp_sop.node {|(-> (sop/curve (list [-1.0 0.0 0.0] [1.0 0.0 0.0]))
     (-> (sop/curve (list [0.0 0.0 -1.0] [0.0 0.0 1.0])) (sop/merge))
     (sop/group_range :owner "Primitives" :name "selected" :end_ 1))|} in
  let analysis_collision_node = Lisp_sop.node {|(-> (sop/curve (list [0.0 -1.0 0.0] [0.0 1.0 0.0]))
     (sop/group_range :owner "Primitives" :name "selected" :end_ 0))|} in
  let analysis_source_geometry = cook 1 analysis_source_node and analysis_collision_geometry = cook 1 analysis_collision_node in
  let analysis_source = Lisp_sop.snapshot (analysis_source_geometry) and analysis_collision = Lisp_sop.snapshot (analysis_collision_geometry) in
  let analysis = Lisp_sop.node ~with_:["analysis_source", (analysis_source)] {|(sop/intersection_analysis (sop/ext_analysis_source))|} in
  same_cook "intersection analysis defaults" ~optional_inputs:[Some analysis_source;None]
    ~typed:analysis ~factory:Nodes.Intersection_analysis.factory [] analysis_source;
  cache_identity "intersection analysis all fields" analysis;
  List.iter (fun use_collision -> List.iter (fun include_coplanar -> List.iter (fun use_groups ->
    for outputs = 0 to 15 do
      let name bit value = if outputs land (1 lsl bit) <> 0 then value else "" in
      let input_attribute = name 0 "input" and primitive_attribute = name 1 "primitive"
      and primitive_uvw_attribute = name 2 "uvw" and point_attribute = name 3 "point" in
      let source_group = if use_groups then "selected" else "" in
      let collision_group = if use_groups then "selected" else "" in
      let collision = if use_collision then Some analysis_collision else None in
      let typed = sop ~inputs:(analysis_source :: Option.to_list collision) "intersection_analysis"
          [ks "source_group" source_group; ks "collision_group" collision_group; kf "tolerance" 1e-9;
           kb "include_coplanar" include_coplanar; ks "input_attribute" input_attribute;
           ks "primitive_attribute" primitive_attribute; ks "primitive_uvw_attribute" primitive_uvw_attribute;
           ks "point_attribute" point_attribute] in
      same_cook "intersection analysis controls" ~optional_inputs:[Some analysis_source;collision]
        ~typed ~factory:Nodes.Intersection_analysis.factory
        ["source_group",Param.Text_value source_group;"collision_group",Text_value collision_group;
         "tolerance",Float_value 1e-9;"include_coplanar",Bool_value include_coplanar;
         "input_attribute",Text_value input_attribute;"primitive_attribute",Text_value primitive_attribute;
         "primitive_uvw_attribute",Text_value primitive_uvw_attribute;"point_attribute",Text_value point_attribute] analysis_source;
      let optional name = if name = "" then None else Some name in
      let source_primitives = if use_groups then Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "selected" analysis_source_geometry else None
      and collision_primitives = if use_groups && use_collision then Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "selected" analysis_collision_geometry else None in
      let collision = Option.map (fun _ -> analysis_collision_geometry) collision in
      let native = Rdk.Intersection_analysis.run ?source_primitives ?collision_primitives ~tolerance:1e-9 ~include_coplanar
          ~input_attribute:(optional input_attribute) ~primitive_attribute:(optional primitive_attribute)
          ~primitive_uvw_attribute:(optional primitive_uvw_attribute) ~point_attribute:(optional point_attribute)
          ?collision analysis_source_geometry |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "intersection analysis flat controls match native"
    done) [false;true]) [false;true]) [false;true];
  same_cook "intersection analysis unset names" ~optional_inputs:[Some analysis_source;Some analysis_collision]
    ~typed:(Lisp_sop.node ~with_:["analysis_source", (analysis_source); "analysis_collision", (analysis_collision)] {|(sop/intersection_analysis
   (sop/ext_analysis_source)
   (sop/ext_analysis_collision)
   :source_group " "
   :collision_group " "
   :input_attribute " "
   :primitive_attribute " "
   :primitive_uvw_attribute " "
   :point_attribute " ")|})
    ~factory:Nodes.Intersection_analysis.factory ["source_group",Param.Text_value " ";"collision_group",Text_value " ";
      "input_attribute",Text_value " ";"primitive_attribute",Text_value " ";"primitive_uvw_attribute",Text_value " ";"point_attribute",Text_value " "] analysis_source;
  List.iter (fun construct -> check (rejected construct)
      "intersection analysis refuses invalid construction")
    [ (fun () -> Lisp_sop.node ~with_:["analysis_source", (analysis_source)] {|(sop/intersection_analysis (sop/ext_analysis_source) :tolerance -0.1)|});
      (fun () -> Lisp_sop.node ~with_:["analysis_source", (analysis_source)] {|(sop/intersection_analysis (sop/ext_analysis_source) :input_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["analysis_source", (analysis_source)] {|(sop/intersection_analysis (sop/ext_analysis_source) :input_attribute "sourceprim")|}) ];
  check (Result.is_error (Node.apply_parameters analysis ["tolerance",Param.Float_value Float.nan]))
    "intersection analysis inspector refuses invalid tolerance";
  let fracture_source_node = Lisp_sop.node {|(sop/box :size [2.0 2.0 2.0] :consolidate_points true :normals "Auto")|} in
  let fracture_cutter_node = Lisp_sop.node {|(sop/grid :columns 1 :rows 1 :size 3.0)|} in
  let fracture_source_geometry = cook 1 fracture_source_node |> Rdk.Geometry.without_attribute ~owner:Rdk.Attribute.Point "N"
  and fracture_cutter_geometry = cook 1 fracture_cutter_node |> Rdk.Geometry.without_attribute ~owner:Rdk.Attribute.Point "N" in
  let fracture_source = Lisp_sop.snapshot (fracture_source_geometry) and fracture_cutter = Lisp_sop.snapshot (fracture_cutter_geometry) in
  let fracture = Lisp_sop.node ~with_:["fracture_source", (fracture_source); "fracture_cutter", (fracture_cutter)] {|(sop/boolean_fracture (sop/ext_fracture_source) (sop/ext_fracture_cutter))|} in
  same_cook "boolean fracture defaults" ~inputs:[fracture_cutter] ~typed:fracture ~factory:Nodes.Boolean_fracture.factory [] fracture_source;
  cache_identity "boolean fracture all fields" fracture;
  List.iter (fun resolve_cutter_self_intersections -> List.iter (fun (detriangulation,polygon_choice) ->
    List.iter (fun require_closed -> List.iter (fun (point_conflict,conflict_choice) ->
      List.iter (fun assume_flat -> List.iter (fun strict_cleanup -> List.iter (fun cleanup_max_batches ->
        let typed = sop ~inputs:[fracture_source; fracture_cutter] "boolean_fracture"
            [kb "resolve_cutter_self_intersections" resolve_cutter_self_intersections;
             ks "detriangulation" polygon_choice; kb "require_closed" require_closed;
             ks "point_conflict" conflict_choice; kb "assume_flat" assume_flat; kb "strict_cleanup" strict_cleanup;
             ki "cleanup_max_batches" cleanup_max_batches; ks "piece_attribute" "cell"; kf "point_tolerance" 1e-9;
             kf "tiny_seam_threshold" 1e-12] in
        same_cook "boolean fracture controls" ~inputs:[fracture_cutter] ~typed ~factory:Nodes.Boolean_fracture.factory
          ["resolve_cutter_self_intersections",Param.Bool_value resolve_cutter_self_intersections;
           "detriangulation",Choice_value polygon_choice;"require_closed",Bool_value require_closed;
           "point_conflict",Choice_value conflict_choice;"assume_flat",Bool_value assume_flat;
           "strict_cleanup",Bool_value strict_cleanup;"cleanup_max_batches",Int_value cleanup_max_batches;
           "piece_attribute",Text_value "cell";"point_tolerance",Float_value 1e-9;"tiny_seam_threshold",Float_value 1e-12] fracture_source;
        let native = Rdk.Boolean.run ~operation:Rdk.Boolean.Difference ~left_treatment:Rdk.Boolean.Solid ~right_treatment:Rdk.Boolean.Surface
            ~resolve_right_self_intersections:resolve_cutter_self_intersections ~detriangulation ~require_closed
            ~point_conflict ~assume_flat ~strict_cleanup ~cleanup_max_batches ~piece_attribute:"cell"
            ~point_tolerance:1e-9 ~tiny_seam_threshold:1e-12 ~seam_points:Rdk.Boolean.Shared_seam_points
            ~right:fracture_cutter_geometry fracture_source_geometry |> Result.get_ok in
        let geometry = cook 1 typed in
        check (equal_geometry native geometry) "boolean fracture flat controls match native";
        check (Node.operation typed = "boolean" && Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive "cell" geometry <> None)
          "boolean fracture retains operation identity and piece payload") [4;8]) [false;true]) [false;true])
      [Rdk.Boolean.Reject,"Reject conflict";Promote_to_vertex,"Promote to vertex"])
      [false;true]) [Rdk.Boolean.Triangles,"Triangles";Unchanged_polygons,"Unchanged polygons";All_polygons,"All polygons"]) [false;true];
  List.iter (fun construct -> check (rejected construct)
      "boolean fracture refuses invalid construction")
    [ (fun () -> Lisp_sop.node ~with_:["fracture_source", (fracture_source); "fracture_cutter", (fracture_cutter)] {|(sop/boolean_fracture
   (sop/ext_fracture_source)
   (sop/ext_fracture_cutter)
   :point_tolerance -0.1)|});
      (fun () -> Lisp_sop.node ~with_:["fracture_source", (fracture_source); "fracture_cutter", (fracture_cutter)] {|(sop/boolean_fracture
   (sop/ext_fracture_source)
   (sop/ext_fracture_cutter)
   :tiny_seam_threshold -0.1)|});
      (fun () -> Lisp_sop.node ~with_:["fracture_source", (fracture_source); "fracture_cutter", (fracture_cutter)] {|(sop/boolean_fracture
   (sop/ext_fracture_source)
   (sop/ext_fracture_cutter)
   :cleanup_max_batches 0)|});
      (fun () -> Lisp_sop.node ~with_:["fracture_source", (fracture_source); "fracture_cutter", (fracture_cutter)] {|(sop/boolean_fracture
   (sop/ext_fracture_source)
   (sop/ext_fracture_cutter)
   :piece_attribute " ")|}) ];
  check (Result.is_error (Node.apply_parameters fracture ["tiny_seam_threshold",Param.Float_value Float.nan]))
    "boolean fracture inspector refuses invalid tolerance";
  let replication_geometry = Sources.point_replicate () in
  let replication_source = Lisp_sop.node ~with_:["replication_geometry", (Lisp_sop.snapshot (replication_geometry))] {|(sop/set_vector (sop/ext_replication_geometry) :value [0.2 0.3 0.4])|} in
  let replication_geometry = cook 1 replication_source in
  let replication_custom = Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0.,0.,0.);(0.,0.,1.)|]) in
  let replication_custom_geometry = cook 1 replication_custom in
  same_cook "point replicate defaults" ~optional_inputs:[Some replication_source;None] ~typed:(Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate (sop/ext_replication_source))|})
    ~factory:Nodes.Point_replicate.factory [] replication_source;
  cache_identity "point replicate all fields" (Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate (sop/ext_replication_source))|});
  let replication_points = Rdk.Geometry.find_group ~owner:Rdk.Group.Point "emit" replication_geometry |> Option.get in
  List.iter (fun (shape,shape_choice) -> List.iter (fun (velocity_stretch,velocity_choice) ->
    List.iter (fun use_noise -> List.iter (fun quasi_stratified -> List.iter (fun keep_input ->
      List.iter (fun context_seed -> List.iter (fun noise_context_seed ->
        let custom_shape = if shape = Rdk.Point_replication.Replicate_custom then Some replication_custom else None in
        let typed = sop ~inputs:(replication_source :: Option.to_list custom_shape) "point_replicate"
            [ks "group" "emit"; kf "points_per_point" 2.; ks "scale_attribute" "density";
             kb "context_seed" context_seed; ki "seed" 7; ks "shape" shape_choice;
             ks "velocity_stretch" velocity_choice; kb "use_noise" use_noise;
             kb "noise_context_seed" noise_context_seed; ki "noise_seed" 9; kb "quasi_stratified" quasi_stratified;
             kb "keep_input" keep_input; kb "keep_source_attributes" true; ks "generated_group" "cloud";
             kv "center" (Vec3.create 0.1 0.2 0.3); kv "size" (Vec3.create 0.8 0.9 1.);
             kv "orientation" (Vec3.create 0.2 0.1 0.3); kf "uniform_scale" 0.7; kf "velocity_scale" 0.4;
             kf "inherit_velocity" 0.6; kf "radial_velocity" 0.2; ks "transform_attributes" "flow"] in
        same_cook "point replicate controls" ~optional_inputs:[Some replication_source;custom_shape] ~typed
          ~factory:Nodes.Point_replicate.factory
          ["group",Param.Text_value "emit";"points_per_point",Float_value 2.;"scale_attribute",Text_value "density";
           "context_seed",Bool_value context_seed;"seed",Int_value 7;"shape",Choice_value shape_choice;
           "velocity_stretch",Choice_value velocity_choice;"use_noise",Bool_value use_noise;
           "noise_context_seed",Bool_value noise_context_seed;"noise_seed",Int_value 9;
           "quasi_stratified",Bool_value quasi_stratified;"keep_input",Bool_value keep_input;
           "keep_source_attributes",Bool_value true;"generated_group",Text_value "cloud";
           "center_x",Float_value 0.1;"center_y",Float_value 0.2;"center_z",Float_value 0.3;
           "size_x",Float_value 0.8;"size_y",Float_value 0.9;"size_z",Float_value 1.;
           "orientation_x",Float_value 0.2;"orientation_y",Float_value 0.1;"orientation_z",Float_value 0.3;
           "uniform_scale",Float_value 0.7;"velocity_scale",Float_value 0.4;"inherit_velocity",Float_value 0.6;
           "radial_velocity",Float_value 0.2;"transform_attributes",Text_value "flow"] replication_source;
        check (Context.Dependencies.mem Context.Dependencies.Seed (Node.dependencies typed)
          = (context_seed || use_noise && noise_context_seed)) "point replicate context seed dependency";
        if not context_seed && not (use_noise && noise_context_seed) then (
          let custom_shape = Option.map (fun _ -> replication_custom_geometry) custom_shape in
          let noise_amplitude = if use_noise then Some (Vec3.create 0.1 0.1 0.1) else None in
          let native = Rdk.Point_replication.run ~points:replication_points ~points_per_point:2. ~scale_attribute:"density"
              ~seed:(Rand.seed 7) ~shape ~velocity_stretch ?noise_amplitude ~noise_seed:9
              ~quasi_stratified ~keep_input ~keep_source_attributes:true ~generated_group:"cloud"
              ~center:(Vec3.create 0.1 0.2 0.3) ~size:(Vec3.create 0.8 0.9 1.) ~orientation:(Vec3.create 0.2 0.1 0.3)
              ~uniform_scale:0.7 ~velocity_scale:0.4 ~inherit_velocity:0.6 ~radial_velocity:0.2
              ~transform_attributes:"flow" ?custom_shape replication_geometry |> Result.get_ok in
          check (equal_geometry native (cook 1 typed)) "point replicate flat controls match native")) [false;true]) [false;true])
      [false;true]) [false;true]) [false;true])
      [Rdk.Point_replication.Replicate_no_velocity_stretch,"None";Replicate_scaled_velocity,"Scaled velocity";Replicate_velocity_only,"Velocity only"])
      [Rdk.Point_replication.Replicate_point,"Point";Replicate_box,"Box";Replicate_sphere,"Sphere";Replicate_disk,"Disk";Replicate_line,"Line";Replicate_custom,"Custom"];
  let inactive_custom = Lisp_sop.node ~with_:["replication_source", (replication_source); "replication_custom", (replication_custom)] {|(sop/point_replicate (sop/ext_replication_source) (sop/ext_replication_custom))|} in
  check (List.length (Node.inputs inactive_custom) = 2) "point replicate retains inactive optional connection";
  same_cook "point replicate inactive custom input" ~optional_inputs:[Some replication_source;Some replication_custom]
    ~typed:inactive_custom ~factory:Nodes.Point_replicate.factory [] replication_source;
  same_cook "point replicate zero count and unset names" ~optional_inputs:[Some replication_source;None] ~typed:(Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate
   (sop/ext_replication_source)
   :points_per_point 0.0
   :group " "
   :scale_attribute " "
   :generated_group " ")|}) ~factory:Nodes.Point_replicate.factory
    ["points_per_point",Param.Float_value 0.;"group",Text_value " ";"scale_attribute",Text_value " ";"generated_group",Text_value " "] replication_source;
  List.iter (fun construct -> check (rejected construct)
      "point replicate refuses invalid construction")
    [ (fun () -> Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate (sop/ext_replication_source) :points_per_point -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate (sop/ext_replication_source) :size [-1.0 1.0 1.0])|});
      (fun () -> Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate (sop/ext_replication_source) :noise_roughness 1.1)|});
      (fun () -> Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate (sop/ext_replication_source) :noise_attenuation 0.0)|});
      (fun () -> Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate (sop/ext_replication_source) :noise_turbulence 0)|});
      (fun () -> Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate (sop/ext_replication_source) :id_attribute " ")|});
      (fun () -> Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate (sop/ext_replication_source) :shape "Custom")|});
      (fun () -> Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate
   (sop/ext_replication_source)
   :keep_source_attributes true
   :source_point_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate
   (sop/ext_replication_source)
   :keep_source_attributes true
   :source_index_attribute "sourcepoint")|}) ];
  check (Result.is_error (Node.apply_parameters (Lisp_sop.node ~with_:["replication_source", (replication_source)] {|(sop/point_replicate (sop/ext_replication_source))|})
      ["noise_frequency_x",Param.Float_value Float.nan])) "point replicate inspector refuses nonfinite vector";
  let bevel_geometry = Sources.bevel () |> Sources.bevel_edges in
  let bevel_source = Lisp_sop.node ~with_:["bevel_geometry", (Lisp_sop.snapshot (bevel_geometry))] {|(sop/set_float (sop/ext_bevel_geometry) :name "pscale" :value 0.8)|} in
  let bevel_geometry = cook 1 bevel_source in
  same_cook "poly bevel defaults" ~typed:(Lisp_sop.node ~with_:["bevel_source", (bevel_source)] {|(sop/poly_bevel (sop/ext_bevel_source))|}) ~factory:Nodes.Poly_bevel.factory [] bevel_source;
  cache_identity "poly bevel all fields" (Lisp_sop.node ~with_:["bevel_source", (bevel_source)] {|(sop/poly_bevel (sop/ext_bevel_source))|});
  let bevel_edges = Rdk.Geometry.find_edge_group "bevel_edges" bevel_geometry |> Option.get in
  List.iter (fun (shape,choice) -> List.iter (fun divisions -> List.iter (fun clamp_overlap ->
    List.iter (fun ignore_flat_angle -> List.iter (fun recompute_point_normals -> List.iter (fun use_scale ->
      let point_scale_attribute = if use_scale then "pscale" else "" in
      let typed = sop ~inputs:[bevel_source] "poly_bevel"
          [ks "group" "bevel_edges"; ks "shape" choice; kf "convexity" 0.75; kf "distance" 0.2; ki "divisions" divisions;
           kb "clamp_overlap" clamp_overlap; kf "ignore_flat_angle" ignore_flat_angle;
           kb "recompute_point_normals" recompute_point_normals; ks "point_scale_attribute" point_scale_attribute;
           ks "edge_group" "edge_fillets"; ks "corner_group" "corner_fillets"; ks "offset_group" "offset_edges"] in
      same_cook "poly bevel controls" ~typed ~factory:Nodes.Poly_bevel.factory
        ["group",Param.Text_value "bevel_edges";"shape",Choice_value choice;"convexity",Float_value 0.75;
         "distance",Float_value 0.2;"divisions",Int_value divisions;"clamp_overlap",Bool_value clamp_overlap;
         "ignore_flat_angle",Float_value ignore_flat_angle;"recompute_point_normals",Bool_value recompute_point_normals;
         "point_scale_attribute",Text_value point_scale_attribute;"edge_group",Text_value "edge_fillets";
         "corner_group",Text_value "corner_fillets";"offset_group",Text_value "offset_edges"] bevel_source;
      let shape = if shape then Rdk.Poly_bevel.Bevel_chamfer else Rdk.Poly_bevel.Bevel_round {convexity=0.75} in
      let ignore_flat_angle = if ignore_flat_angle > 0. then Some ignore_flat_angle else None in
      let point_scale_attribute = if use_scale then Some "pscale" else None in
      let native = Rdk.Poly_bevel.run ~edges:bevel_edges ~shape ~distance:0.2 ~divisions ~clamp_overlap
          ?ignore_flat_angle ~recompute_point_normals ?point_scale_attribute ~edge_group:"edge_fillets"
          ~corner_group:"corner_fillets" ~offset_group:"offset_edges" bevel_geometry |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "poly bevel flat controls match native") [false;true])
      [false;true]) [0.;0.1]) [false;true]) [1;3]) [true,"Chamfer";false,"Round"];
  same_cook "poly bevel unset names" ~typed:(Lisp_sop.node ~with_:["bevel_source", (bevel_source)] {|(sop/poly_bevel (sop/ext_bevel_source))|}) ~factory:Nodes.Poly_bevel.factory [] bevel_source;
  List.iter (fun construct ->
    check (rejected construct) "poly bevel refuses invalid construction")
    [ (fun () -> Lisp_sop.node ~with_:["bevel_source", (bevel_source)] {|(sop/poly_bevel (sop/ext_bevel_source) :distance -0.1)|});
      (fun () -> Lisp_sop.node ~with_:["bevel_source", (bevel_source)] {|(sop/poly_bevel (sop/ext_bevel_source) :divisions 0)|});
      (fun () -> Lisp_sop.node ~with_:["bevel_source", (bevel_source)] {|(sop/poly_bevel (sop/ext_bevel_source) :convexity 1.1)|});
      (fun () -> Lisp_sop.node ~with_:["bevel_source", (bevel_source)] (Printf.sprintf {|(sop/poly_bevel (sop/ext_bevel_source) :ignore_flat_angle %s)|} ((Lisp_sop.float (Float.pi +. 0.1)))));
      (fun () -> Lisp_sop.node ~with_:["bevel_source", (bevel_source)] {|(sop/poly_bevel (sop/ext_bevel_source) :ignore_flat_angle -0.1)|}) ];
  check (Result.is_error (Node.apply_parameters (Lisp_sop.node ~with_:["bevel_source", (bevel_source)] {|(sop/poly_bevel (sop/ext_bevel_source))|})
      ["distance",Param.Float_value Float.nan])) "poly bevel inspector refuses invalid distance";
  let ray_source_node = Lisp_sop.node {|(-> (sop/grid :columns 3 :rows 2)
     (sop/transform :mode "Matrix" :m03 0.0 :m13 1.0 :m23 0.0)
     (sop/set_vector :name "N" :value [0.0 -1.0 0.0])
     (sop/set_vector :name "dir" :value [0.0 -1.0 0.0])
     (sop/group_range :name "selected" :end_ 1)
     (sop/group_range :owner "Vertices" :name "selected" :end_ 1)
     (sop/group_range :owner "Primitives" :name "selected" :end_ 0)
     (sop/group_edges :name "selected" :incidence "Boundary"))|} in
  let ray_collision_node = Lisp_sop.node {|(-> (sop/grid :columns 4 :rows 3)
     (sop/color_by_height)
     (sop/set_float :owner "Vertex" :name "corner" :value 3.0)
     (sop/set_int :owner "Primitive" :name "material" :value 4)
     (sop/set_int :owner "Detail" :name "revision" :value 5)
     (sop/group_range :owner "Primitives" :name "collision" :range_mode "From ends")
     (sop/group_range :name "collision_points" :end_ 5))|} in
  let ray_source_geometry = cook 1 ray_source_node and ray_collision_geometry = cook 1 ray_collision_node in
  let ray_source = Lisp_sop.snapshot (ray_source_geometry) and ray_collision = Lisp_sop.snapshot (ray_collision_geometry) in
  let ray = Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray (sop/ext_ray_source) (sop/ext_ray_collision))|} in
  same_cook "ray defaults" ~inputs:[ray_collision] ~typed:ray ~factory:Nodes.Ray.factory [] ray_source;
  cache_identity ~companions:["match_groups",["point_pattern",Param.Text_value "Cd"];
    "source_vertex_numbers_attribute",["source_vertex_weights_attribute",Param.Text_value "weights"];
    "source_vertex_weights_attribute",["source_vertex_numbers_attribute",Param.Text_value "numbers"]] "ray all fields" ray;
  let direction_vector = Vec3.create 0. (-1.) 0. in
  let selection = Rdk.Transform_ops.Selected_points (Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected" ray_source_geometry |> Option.get)
  and collision_primitives = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "collision" ray_collision_geometry |> Option.get in
  List.iter (fun (method_,method_choice,sample_counts) -> List.iter (fun (direction_choice,native_direction) ->
    List.iter (fun (direction_mode,mode_choice) -> List.iter (fun (surface_hit,hit_choice) ->
      List.iter (fun (combine,combine_choice) -> List.iter (fun samples -> List.iter (fun limit_max_distance ->
        let typed = sop ~inputs:[ray_source; ray_collision] "ray"
            [ks "method_" method_choice; ks "direction" direction_choice; kv "direction_vector" direction_vector;
             ks "direction_attribute" "dir"; ks "direction_mode" mode_choice; ks "surface_hit" hit_choice;
             ks "combine" combine_choice; ki "samples" samples; kf "jitter_scale" 0.03; ki "seed" 7;
             kf "min_distance" 0.1; kb "limit_max_distance" limit_max_distance; kf "max_distance" 2.;
             kf "tolerance" 1e-9; kf "scale" 0.8; kf "lift" 0.01; ks "group" "selected"; ks "collision_group" "collision";
             ks "distance_attribute" "distance"; ks "primitive_attribute" "primitive";
             ks "source_vertex_numbers_attribute" "numbers"; ks "source_vertex_weights_attribute" "weights";
             ks "hit_group" "hits"; ks "normal_attribute" "hit_normal"; ks "point_pattern" "Cd collision_points";
             ks "vertex_pattern" "corner"; ks "primitive_pattern" "material collision"; ks "detail_pattern" "revision"] in
        same_cook ("ray " ^ method_choice ^ direction_choice ^ mode_choice ^ hit_choice ^ combine_choice) ~inputs:[ray_collision]
          ~typed ~factory:Nodes.Ray.factory ["method_",Param.Choice_value method_choice;"direction",Choice_value direction_choice;
            "direction_y",Float_value (-1.);"direction_attribute",Text_value "dir";"direction_mode",Choice_value mode_choice;
            "surface_hit",Choice_value hit_choice;"combine",Choice_value combine_choice;"samples",Int_value samples;
            "jitter_scale",Float_value 0.03;"seed",Int_value 7;"min_distance",Float_value 0.1;"limit_max_distance",Bool_value limit_max_distance;
            "max_distance",Float_value 2.;"tolerance",Float_value 1e-9;"scale",Float_value 0.8;"lift",Float_value 0.01;
            "group",Text_value "selected";"collision_group",Text_value "collision";"distance_attribute",Text_value "distance";
            "primitive_attribute",Text_value "primitive";"source_vertex_numbers_attribute",Text_value "numbers";
            "source_vertex_weights_attribute",Text_value "weights";"hit_group",Text_value "hits";"normal_attribute",Text_value "hit_normal";
            "point_pattern",Text_value "Cd collision_points";"vertex_pattern",Text_value "corner";
            "primitive_pattern",Text_value "material collision";"detail_pattern",Text_value "revision"] ray_source;
        let max_distance = if limit_max_distance then Some 2. else None in
        let native = Rdk.Ray.run ~method_ ~direction:native_direction ~direction_mode ~surface_hit ~combine ~samples
            ~jitter_scale:0.03 ~seed:7 ~min_distance:0.1 ?max_distance ~tolerance:1e-9 ~scale:0.8 ~lift:0.01 ~selection ~collision_primitives
            ~distance_attribute:"distance" ~primitive_attribute:"primitive" ~source_vertex_numbers_attribute:"numbers"
            ~source_vertex_weights_attribute:"weights" ~hit_group:"hits" ~normal_attribute:"hit_normal"
            ~point_pattern:"Cd collision_points" ~vertex_pattern:"corner" ~primitive_pattern:"material collision" ~detail_pattern:"revision"
            ~source:ray_source_geometry ~collision:ray_collision_geometry () |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "ray flat controls match native") [false;true]) sample_counts)
      [Rdk.Ray.Ray_average,"Average";Ray_median,"Median";Ray_shortest,"Shortest";Ray_longest,"Longest"])
      [Rdk.Ray.Ray_first_surface,"First surface";Ray_last_surface,"Last surface"])
      [Rdk.Ray.Ray_forward,"Forward";Ray_reverse,"Reverse";Ray_bidirectional_closest,"Bidirectional closest";Ray_bidirectional_farthest,"Bidirectional farthest"])
    ["Vector",Rdk.Ray.Ray_vector direction_vector;"Normal",Rdk.Ray.Ray_normal;
     "Attribute",Rdk.Ray.Ray_attribute "dir"])
    [Rdk.Ray.Ray_project,"Project rays",[1;3];Ray_minimum_distance,"Minimum distance",[1]];
  List.iter (fun (owner_choice,selection) ->
    let typed = sop ~inputs:[ray_source; ray_collision] "ray"
      [ks "group_owner" owner_choice; ks "group" "selected"; ks "point_pattern" "Cd collision_points"; kb "match_groups" true] in
    same_cook "ray selection owners and group matching" ~inputs:[ray_collision] ~typed ~factory:Nodes.Ray.factory
      ["group_owner",Param.Choice_value owner_choice;"group",Text_value "selected";
       "point_pattern",Text_value "Cd collision_points";"match_groups",Bool_value true] ray_source;
    let native = Rdk.Ray.run ~selection ~point_pattern:"Cd collision_points" ~match_groups:true
        ~source:ray_source_geometry ~collision:ray_collision_geometry () |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "ray selection owners match native")
    ["Point",Rdk.Transform_ops.Selected_points (Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected" ray_source_geometry |> Option.get);
     "Vertex",Selected_vertices (Rdk.Geometry.find_group ~owner:Rdk.Group.Vertex "selected" ray_source_geometry |> Option.get);
     "Primitive",Selected_primitives (Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "selected" ray_source_geometry |> Option.get);
     "Edge",Selected_edges (Rdk.Geometry.find_edge_group "selected" ray_source_geometry |> Option.get)];
  same_cook "ray inactive zero direction" ~inputs:[ray_collision]
    ~typed:(Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] (Printf.sprintf {|(sop/ray
   (sop/ext_ray_source)
   (sop/ext_ray_collision)
   :method_ "Minimum distance"
   :direction "Vector"
   :direction_vector %s)|} ((Lisp_sop.vec3 Vec3.zero))))
    ~factory:Nodes.Ray.factory ["method_",Param.Choice_value "Minimum distance";"direction",Choice_value "Vector";"direction_y",Float_value 0.] ray_source;
  same_cook "ray unset names" ~inputs:[ray_collision]
    ~typed:(Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray
   (sop/ext_ray_source)
   (sop/ext_ray_collision)
   :group " "
   :collision_group " "
   :distance_attribute " "
   :source_vertex_numbers_attribute " "
   :source_vertex_weights_attribute " "
   :point_pattern " ")|}) ~factory:Nodes.Ray.factory
    ["group",Param.Text_value " ";"collision_group",Text_value " ";"distance_attribute",Text_value " ";
     "source_vertex_numbers_attribute",Text_value " ";"source_vertex_weights_attribute",Text_value " ";"point_pattern",Text_value " "] ray_source;
  List.iter (fun make -> check (rejected make) "ray refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray (sop/ext_ray_source) (sop/ext_ray_collision) :samples 1025)|});
     (fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray
   (sop/ext_ray_source)
   (sop/ext_ray_collision)
   :method_ "Minimum distance"
   :samples 2)|});
     (fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] (Printf.sprintf {|(sop/ray
   (sop/ext_ray_source)
   (sop/ext_ray_collision)
   :direction "Vector"
   :direction_vector %s)|} ((Lisp_sop.vec3 Vec3.zero))));
     (fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray
   (sop/ext_ray_source)
   (sop/ext_ray_collision)
   :direction "Attribute"
   :direction_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray
   (sop/ext_ray_source)
   (sop/ext_ray_collision)
   :limit_max_distance true
   :min_distance 2.0
   :max_distance 1.0)|});
     (fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] (Printf.sprintf {|(sop/ray (sop/ext_ray_source) (sop/ext_ray_collision) :tolerance %s)|} ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray
   (sop/ext_ray_source)
   (sop/ext_ray_collision)
   :source_vertex_numbers_attribute "numbers")|});
     (fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray (sop/ext_ray_source) (sop/ext_ray_collision) :normal_attribute "P")|});
     (fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray
   (sop/ext_ray_source)
   (sop/ext_ray_collision)
   :normal_attribute "same"
   :distance_attribute "same")|});
     (fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray (sop/ext_ray_source) (sop/ext_ray_collision) :match_groups true)|});
     (fun () -> Lisp_sop.node ~with_:["ray_source", (ray_source); "ray_collision", (ray_collision)] {|(sop/ray (sop/ext_ray_source) (sop/ext_ray_collision) :point_pattern "broken[")|})];
  check (Result.is_error (Node.apply_parameters ray ["tolerance",Param.Float_value nan])) "ray refuses invalid inspector edit";
  let copy_fields input = Lisp_sop.node ~with_:["input", (input)] {|(-> (sop/enumerate (sop/ext_input))
     (sop/enumerate :owner "Vertex")
     (sop/enumerate :owner "Primitive")
     (sop/enumerate :name "source")
     (sop/enumerate :owner "Vertex" :name "source")
     (sop/enumerate :owner "Primitive" :name "source")
     (sop/group_range :name "selected" :end_ 2)
     (sop/group_range :owner "Vertices" :name "selected" :end_ 2)
     (sop/group_range :owner "Primitives" :name "selected" :end_ 1))|} in
  let copy_source = Lisp_sop.node ~with_:["fields", copy_fields (Lisp_sop.node {|(sop/grid :columns 4 :rows 3)|})]
    {|(-> (sop/set_float (sop/ext_fields) :name "value" :value 2.0)
     (sop/set_float :owner "Vertex" :name "value" :value 3.0)
     (sop/set_int :owner "Primitive" :name "value" :value 4)
     (sop/set_int :owner "Detail" :name "value" :value 5))|} in
  let copy_target = Lisp_sop.node {|(sop/grid :columns 3 :rows 2)|} |> copy_fields in
  let copy_source_geometry = cook 1 copy_source and copy_target_geometry = cook 1 copy_target in
  let copy_source = Lisp_sop.snapshot (copy_source_geometry) and copy_target = Lisp_sop.snapshot (copy_target_geometry) in
  let copied = Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy (sop/ext_copy_source) (sop/ext_copy_target))|} in
  same_cook "attribute copy defaults" ~inputs:[copy_target] ~typed:copied ~factory:Nodes.Attribute_copy.factory [] copy_source;
  cache_identity ~changes:["rules",Param.Text_value "point\tid\tid_copy"] "attribute copy all fields" copied;
  let copy_rules = "point\tvalue\tvalue_copy\nvertex\tvalue\tvalue_copy\nprimitive\tvalue\tvalue_copy\ndetail\tvalue\tvalue_copy\npoint\tP\t" in
  let native_copy_rules = List.map (fun owner ->
      {Rdk.Attribute_ops.copy_owner=owner;copy_pattern="value";copy_into=Some "value_copy"})
      [Rdk.Attribute.Point;Vertex;Primitive;Detail]
    @ [{Rdk.Attribute_ops.copy_owner=Rdk.Attribute.Point;copy_pattern="P";copy_into=None}] in
  List.iter (fun (group_owner,owner_choice) -> List.iter (fun (match_choice,native_match) ->
    List.iter (fun allow_position ->
      if group_owner <> Rdk.Group.Vertex || match_choice <> "By attribute values" then begin
      let typed = sop ~inputs:[copy_source; copy_target] "attribute_copy"
          [ks "group_owner" owner_choice; ks "match_" match_choice; kb "allow_position" allow_position;
           ks "rules" copy_rules; ks "source_group" "selected"; ks "target_group" "selected"] in
      same_cook ("attribute copy " ^ owner_choice ^ match_choice) ~inputs:[copy_target] ~typed ~factory:Nodes.Attribute_copy.factory
        ["group_owner",Param.Choice_value owner_choice;"match_",Choice_value match_choice;"allow_position",Bool_value allow_position;
         "rules",Text_value copy_rules;"source_group",Text_value "selected";"target_group",Text_value "selected"] copy_source;
      let source_group = Rdk.Geometry.find_group ~owner:group_owner "selected" copy_source_geometry |> Option.get
      and target_group = Rdk.Geometry.find_group ~owner:group_owner "selected" copy_target_geometry |> Option.get in
      let native = Rdk.Attribute_ops.copy ~group_owner ~match_:native_match ~allow_position ~rules:native_copy_rules ~source_group ~target_group
          ~source:copy_source_geometry ~target:copy_target_geometry () |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "attribute copy flat controls match native"
      end) [false;true])
    ["Cyclic",Rdk.Attribute_ops.Cyclic;
     "By attribute values",By_values {source_attribute="id";target_attribute="id"};
     "To source element",To_element {target_attribute="source"}])
    [Rdk.Group.Point,"Points";Vertex,"Vertices";Primitive,"Primitives"];
  same_cook "attribute copy group pattern precedence" ~inputs:[copy_target]
    ~typed:(Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy
   (sop/ext_copy_source)
   (sop/ext_copy_target)
   :source_group "ignored"
   :source_group_pattern "sel*"
   :target_group "ignored"
   :target_group_pattern "sel*")|}) ~factory:Nodes.Attribute_copy.factory ["source_group",Param.Text_value "ignored";
      "source_group_pattern",Text_value "sel*";"target_group",Text_value "ignored";"target_group_pattern",Text_value "sel*"] copy_source;
  same_cook "attribute copy unset groups" ~inputs:[copy_target]
    ~typed:(Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy
   (sop/ext_copy_source)
   (sop/ext_copy_target)
   :source_group " "
   :source_group_pattern " "
   :target_group " "
   :target_group_pattern " ")|})
    ~factory:Nodes.Attribute_copy.factory ["source_group",Param.Text_value " ";"source_group_pattern",Text_value " ";
      "target_group",Text_value " ";"target_group_pattern",Text_value " "] copy_source;
  List.iter (fun make -> check (rejected make)
      "attribute copy refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy (sop/ext_copy_source) (sop/ext_copy_target) :rules "")|});
     (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy (sop/ext_copy_source) (sop/ext_copy_target) :rules "broken")|});
     (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy (sop/ext_copy_source) (sop/ext_copy_target) :rules "unknown	value	")|});
     (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy (sop/ext_copy_source) (sop/ext_copy_target) :rules "point	broken[	")|});
     (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy
   (sop/ext_copy_source)
   (sop/ext_copy_target)
   :match_ "By attribute values"
   :source_match_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy
   (sop/ext_copy_source)
   (sop/ext_copy_target)
   :group_owner "Vertices"
   :match_ "By attribute values")|});
     (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy
   (sop/ext_copy_source)
   (sop/ext_copy_target)
   :match_ "To source element"
   :target_element_attribute "P")|});
     (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_target", (copy_target)] {|(sop/attribute_copy
   (sop/ext_copy_source)
   (sop/ext_copy_target)
   :source_group_pattern "broken[")|})];
  check (Result.is_error (Node.apply_parameters copied ["rules",Param.Text_value "broken"]))
    "attribute copy refuses invalid inspector edit";
  let surface_source = Lisp_sop.node {|(-> (sop/grid :columns 4 :rows 3)
     (sop/color_by_height)
     (sop/set_float :owner "Vertex" :name "corner" :value 3.0)
     (sop/set_int :owner "Primitive" :name "material" :value 4)
     (sop/group_range :owner "Primitives" :name "source" :end_ 1)
     (sop/group_range :owner "Vertices" :name "corners" :end_ 4))|} in
  let surface_target = Lisp_sop.node {|(-> (sop/grid :columns 3 :rows 2)
     (sop/group_range :name "target" :end_ 1)
     (sop/group_range :owner "Vertices" :name "target" :end_ 1)
     (sop/group_range :owner "Primitives" :name "target" :end_ 0))|} in
  let surface_geometry = cook 1 surface_source and target_geometry = cook 1 surface_target in
  let surface_source = Lisp_sop.snapshot (surface_geometry) and surface_target = Lisp_sop.snapshot (target_geometry) in
  let surface_transfer = Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface (sop/ext_surface_source) (sop/ext_surface_target))|} in
  same_cook "surface transfer defaults" ~inputs:[surface_target] ~typed:surface_transfer
    ~factory:Nodes.Attribute_transfer_surface.factory [] surface_source;
  cache_identity "surface transfer all fields" surface_transfer;
  let surface_attributes = "point\tCd\tcolor\nvertex\tcorner\tcorner_copy\nprimitive\tmaterial\tmaterial_copy" in
  let native_surface_attributes = List.map (fun (source_owner,source_name,target_name) ->
      {Rdk.Attribute_ops.source_owner;source_name;target_name})
    [Rdk.Attribute.Point,"Cd","color";Vertex,"corner","corner_copy";Primitive,"material","material_copy"] in
  List.iter (fun (target_owner,owner_choice,group_owner) -> List.iter (fun (falloff_choice,native_falloff) ->
    List.iter (fun (distance_choice,max_distance,blend_width) ->
      List.iter (fun (unmatched,unmatched_choice) -> List.iter (fun (source_vertex_selection,selection_choice) ->
        let typed = sop ~inputs:[surface_source; surface_target] "attribute_transfer_surface"
            [ks "target_owner" owner_choice; ks "attributes" surface_attributes; ks "distance_mode" distance_choice;
             kf "max_distance" 1.; kf "blend_width" blend_width; ks "falloff" falloff_choice; kf "uniform_bias" 0.7;
             ks "unmatched" unmatched_choice; ks "distance_attribute" "distance"; ks "source_group" "source";
             ks "source_vertex_group" "corners"; ks "source_vertex_selection" selection_choice; ks "target_group" "target"] in
        same_cook ("surface transfer " ^ owner_choice ^ falloff_choice ^ distance_choice) ~inputs:[surface_target]
          ~typed ~factory:Nodes.Attribute_transfer_surface.factory ["target_owner",Param.Choice_value owner_choice;
            "attributes",Text_value surface_attributes;"distance_mode",Choice_value distance_choice;"max_distance",Float_value 1.;
            "blend_width",Float_value blend_width;"falloff",Choice_value falloff_choice;"uniform_bias",Float_value 0.7;
            "unmatched",Choice_value unmatched_choice;"distance_attribute",Text_value "distance";
            "source_group",Text_value "source";"source_vertex_group",Text_value "corners";
            "source_vertex_selection",Choice_value selection_choice;"target_group",Text_value "target"] surface_source;
        let source_primitives = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "source" surface_geometry |> Option.get
        and source_vertices = Rdk.Geometry.find_group ~owner:Rdk.Group.Vertex "corners" surface_geometry |> Option.get
        and target_elements = Rdk.Geometry.find_group ~owner:group_owner "target" target_geometry |> Option.get in
        let native = Rdk.Attribute_ops.transfer_surface ~target_owner ~attributes:native_surface_attributes ?max_distance ~blend_width
            ~falloff:native_falloff ~unmatched ~distance_attribute:"distance" ~source_primitives ~source_vertices
            ~source_vertex_selection ~target_elements ~source:surface_geometry ~target:target_geometry () |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "surface transfer flat controls match native")
        [Rdk.Attribute_ops.All_triangle_vertices,"All triangle vertices";Any_triangle_vertex,"Any triangle vertex"])
      [Rdk.Attribute_ops.Keep_target,"Keep target";Default_value,"Default value"])
      ["Explicit",Some 1.,0.2;"Auto",None,0.])
    ["Linear",Rdk.Attribute_ops.Linear;"Smoothstep",Smoothstep;"Uniform",Uniform 0.7])
    [Rdk.Attribute.Point,"Point",Rdk.Group.Point;Vertex,"Vertex",Vertex;Primitive,"Primitive",Primitive];
  same_cook "surface transfer group patterns" ~inputs:[surface_target]
    ~typed:(Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :source_group "ignored"
   :source_group_pattern "sour*"
   :source_vertex_group "ignored"
   :source_vertex_group_pattern "corn*"
   :target_group "ignored"
   :target_group_pattern "targ*")|}) ~factory:Nodes.Attribute_transfer_surface.factory
    ["source_group",Param.Text_value "ignored";"source_group_pattern",Text_value "sour*";
     "source_vertex_group",Text_value "ignored";"source_vertex_group_pattern",Text_value "corn*";
     "target_group",Text_value "ignored";"target_group_pattern",Text_value "targ*"] surface_source;
  same_cook "surface transfer unset and empty rules" ~inputs:[surface_target]
    ~typed:(Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :attributes " "
   :source_group " "
   :source_vertex_group " "
   :target_group " "
   :distance_attribute " ")|}) ~factory:Nodes.Attribute_transfer_surface.factory
    ["attributes",Param.Text_value " ";"source_group",Text_value " ";"source_vertex_group",Text_value " ";
     "target_group",Text_value " ";"distance_attribute",Text_value " "] surface_source;
  List.iter (fun make -> check (rejected make)
      "surface transfer refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :attributes "broken")|});
     (fun () -> Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :attributes "detail	x	x")|});
     (fun () -> sop ~inputs:[surface_source; surface_target] "attribute_transfer_surface" [ks "target_owner" "Detail"]);
     (fun () -> Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :attributes "point	P	copy")|});
     (fun () -> Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :attributes "point	Cd	P")|});
     (fun () -> Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :attributes "point	Cd	x
vertex	corner	x")|});
     (fun () -> Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :distance_attribute "Cd")|});
     (fun () -> Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :distance_mode "Auto"
   :blend_width 1.0)|});
     (fun () -> Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :uniform_bias 1.1)|});
     (fun () -> Lisp_sop.node ~with_:["surface_source", (surface_source); "surface_target", (surface_target)] {|(sop/attribute_transfer_surface
   (sop/ext_surface_source)
   (sop/ext_surface_target)
   :target_group_pattern "broken[")|})];
  check (Result.is_error (Node.apply_parameters surface_transfer ["attributes",Param.Text_value "broken"]))
    "surface transfer refuses invalid inspector edit";
  let seam_left = Lisp_sop.node {|(sop/box
   :normals "Auto"
   :size [2.0 2.0 2.0]
   :connectivity "Triangles"
   :consolidate_points true)|} in
  let seam_right = Lisp_sop.node {|(sop/box
   :normals "Auto"
   :size [2.0 2.0 2.0]
   :center [0.5 0.5 0.5]
   :connectivity "Triangles"
   :consolidate_points true)|} in
  let seam_left_geometry = cook 1 seam_left and seam_right_geometry = cook 1 seam_right in
  let seam_left = Lisp_sop.snapshot (seam_left_geometry) and seam_right = Lisp_sop.snapshot (seam_right_geometry) in
  let seam = Lisp_sop.node ~with_:["seam_left", (seam_left); "seam_right", (seam_right)] {|(sop/boolean_seam (sop/ext_seam_left) (sop/ext_seam_right))|} in
  same_cook "boolean seam defaults" ~inputs:[seam_right] ~typed:seam ~factory:Nodes.Boolean_seam.factory [] seam_left;
  cache_identity "boolean seam all fields" seam;
  List.iter (fun (output,output_choice) -> List.iter (fun (left_treatment,left_choice) ->
    List.iter (fun (right_treatment,right_choice) -> List.iter (fun resolve_left_self_intersections ->
      List.iter (fun resolve_right_self_intersections ->
        let typed = sop ~inputs:[seam_left; seam_right] "boolean_seam"
            [ks "output" output_choice; ks "left_treatment" left_choice; ks "right_treatment" right_choice;
             kb "resolve_left_self_intersections" resolve_left_self_intersections;
             kb "resolve_right_self_intersections" resolve_right_self_intersections; ks "left_self_group" "left";
             ks "between_group" "between"; ks "right_self_group" "right"; ks "coincident_group" "coincident"] in
        same_cook ("boolean seam " ^ output_choice ^ left_choice ^ right_choice) ~inputs:[seam_right] ~typed ~factory:Nodes.Boolean_seam.factory
          ["output",Param.Choice_value output_choice;"left_treatment",Choice_value left_choice;"right_treatment",Choice_value right_choice;
           "resolve_left_self_intersections",Bool_value resolve_left_self_intersections;
           "resolve_right_self_intersections",Bool_value resolve_right_self_intersections;
           "left_self_group",Text_value "left";"between_group",Text_value "between";"right_self_group",Text_value "right";
           "coincident_group",Text_value "coincident"] seam_left;
        let native = Rdk.Boolean.seam ~output ~left_treatment ~right_treatment ~resolve_left_self_intersections
            ~resolve_right_self_intersections ~left_self_group:(Some "left") ~between_group:(Some "between")
            ~right_self_group:(Some "right") ~coincident_group:(Some "coincident") ~right:seam_right_geometry seam_left_geometry
            |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "boolean seam flat controls match native") [false;true]) [false;true])
      [Rdk.Boolean.Solid,"Solid";Surface,"Surface"]) [Rdk.Boolean.Solid,"Solid";Surface,"Surface"])
    [Rdk.Boolean.Seam_curves,"Seam curves";Coincident_patches,"Coincident patches"];
  let coincident_right_geometry = cook 1 (Lisp_sop.node {|(sop/box
   :normals "Auto"
   :size [2.0 2.0 2.0]
   :center [0.5 0.0 0.0]
   :connectivity "Triangles"
   :consolidate_points true)|}) in
  let coincident_right = Lisp_sop.snapshot (coincident_right_geometry) in
  let coincident = Lisp_sop.node ~with_:["seam_left", (seam_left); "coincident_right", (coincident_right)] {|(sop/boolean_seam
   (sop/ext_seam_left)
   (sop/ext_coincident_right)
   :output "Coincident patches"
   :coincident_group "patches")|} in
  same_cook "boolean seam coincident area" ~inputs:[coincident_right] ~typed:coincident ~factory:Nodes.Boolean_seam.factory
    ["output",Param.Choice_value "Coincident patches";"coincident_group",Text_value "patches"] seam_left;
  let native_coincident = Rdk.Boolean.seam ~output:Rdk.Boolean.Coincident_patches ~coincident_group:(Some "patches")
      ~right:coincident_right_geometry seam_left_geometry |> Result.get_ok in
  let cooked_coincident = cook 1 coincident in
  check (equal_geometry native_coincident cooked_coincident) "boolean seam coincident area matches native";
  check (Rdk.Geometry.primitive_count cooked_coincident > 0) "boolean seam coincident fixture has area";
  same_cook "boolean seam unset groups" ~inputs:[seam_right]
    ~typed:(Lisp_sop.node ~with_:["seam_left", (seam_left); "seam_right", (seam_right)] {|(sop/boolean_seam
   (sop/ext_seam_left)
   (sop/ext_seam_right)
   :left_self_group " "
   :between_group " "
   :right_self_group " "
   :coincident_group " ")|})
    ~factory:Nodes.Boolean_seam.factory ["left_self_group",Param.Text_value " ";"between_group",Text_value " ";
      "right_self_group",Text_value " ";"coincident_group",Text_value " "] seam_left;
  same_cook "boolean seam inactive group names" ~inputs:[coincident_right]
    ~typed:(Lisp_sop.node ~with_:["seam_left", (seam_left); "coincident_right", (coincident_right)] {|(sop/boolean_seam
   (sop/ext_seam_left)
   (sop/ext_coincident_right)
   :output "Coincident patches"
   :left_self_group "same"
   :between_group "same"
   :right_self_group "same")|}) ~factory:Nodes.Boolean_seam.factory ["output",Param.Choice_value "Coincident patches";
      "left_self_group",Text_value "same";"between_group",Text_value "same";"right_self_group",Text_value "same"] seam_left;
  check (rejected (fun () -> Lisp_sop.node ~with_:["seam_left", (seam_left); "seam_right", (seam_right)] {|(sop/boolean_seam
   (sop/ext_seam_left)
   (sop/ext_seam_right)
   :left_self_group "same"
   :between_group "same")|})) "boolean seam refuses duplicate active group names";
  check (Result.is_error (Node.apply_parameters seam ["between_group",Param.Text_value "boolean_left_self_seam"]))
    "boolean seam refuses invalid inspector edit";
  let sweep_backbone_source = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [0.3 0.0 1.0] [0.0 0.0 2.0]))
     (-> (sop/curve (list [3.0 0.0 0.0] [4.0 0.0 1.0] [3.0 1.0 2.0]) :closed true) (sop/merge))
     (sop/set_float :name "payload" :value 2.0)
     (sop/group_range :owner "Primitives" :name "selected" :end_ 0))|} in
  let sweep_profile_source = Lisp_sop.node (Printf.sprintf {|(-> (sop/curve (list [-0.2 -0.2 0.0] [0.2 -0.2 0.0] [0.2 0.2 0.0] [-0.2 0.2 0.0]) :closed true)
     (sop/set_vector :value %s)
     (sop/set_int :owner "Primitive" :name "material" :value 3)
     (sop/group_range :owner "Primitives" :name "selected" :end_ 0))|} ((Lisp_sop.vec3 Vec3.unit_x))) in
  let sweep_backbone_geometry = cook 1 sweep_backbone_source and sweep_profile_geometry = cook 1 sweep_profile_source in
  let sweep_backbone = Lisp_sop.snapshot (sweep_backbone_geometry) and sweep_profile = Lisp_sop.snapshot (sweep_profile_geometry) in
  let swept = Lisp_sop.node ~with_:["sweep_backbone", (sweep_backbone); "sweep_profile", (sweep_profile)] {|(sop/sweep (sop/ext_sweep_backbone) (sop/ext_sweep_profile))|} in
  same_cook "sweep defaults" ~inputs:[sweep_profile] ~typed:swept ~factory:Nodes.Sweep.factory [] sweep_backbone;
  cache_identity "sweep all fields" swept;
  List.iter (fun (connectivity,connectivity_choice,surface) ->
    List.iter (fun (tangent,tangent_choice) -> List.iter (fun continuous_closed ->
      List.iter (fun transform_attributes -> List.iter (fun reverse_cross_sections ->
        List.iter (fun caps ->
          let typed = sop ~inputs:[sweep_backbone; sweep_profile] "sweep"
              [ks "connectivity" connectivity_choice; ks "tangent" tangent_choice;
               kb "continuous_closed" continuous_closed; kb "transform_attributes" transform_attributes;
               kb "reverse_cross_sections" reverse_cross_sections; kf "scale" 0.8; kf "roll" 0.2; kf "twist" 0.3;
               kb "caps" caps; ks "cap_group" "ends"; ks "uv_attribute" "st"; ks "cross_section_prefix" "profile_"] in
          same_cook ("sweep " ^ connectivity_choice ^ tangent_choice) ~inputs:[sweep_profile] ~typed ~factory:Nodes.Sweep.factory
            ["connectivity",Param.Choice_value connectivity_choice;"tangent",Choice_value tangent_choice;
             "continuous_closed",Bool_value continuous_closed;"transform_attributes",Bool_value transform_attributes;
             "reverse_cross_sections",Bool_value reverse_cross_sections;"scale",Float_value 0.8;"roll",Float_value 0.2;
             "twist",Float_value 0.3;"caps",Bool_value caps;"cap_group",Text_value "ends";"uv_attribute",Text_value "st";
             "cross_section_prefix",Text_value "profile_"] sweep_backbone;
          let cap_group = if caps then Some "ends" else None in
          let native = Rdk.Sweep_modeling.sweep ~connectivity ~tangent ~continuous_closed ~transform_attributes ~reverse_cross_sections
              ~scale:0.8 ~roll:0.2 ~twist:0.3 ~caps ?cap_group ~uv_attribute:(Some "st") ~cross_section_prefix:"profile_"
              ~backbone:sweep_backbone_geometry ~cross_section:sweep_profile_geometry () |> Result.get_ok in
          check (equal_geometry native (cook 1 typed)) "sweep flat controls match native") (if surface then [false;true] else [false]))
        [false;true]) [false;true]) [false;true])
      [Rdk.Sweep_modeling.Sweep_average_edges,"Average edges";Sweep_central_difference,"Central difference";
       Sweep_previous_edge,"Previous edge";Sweep_next_edge,"Next edge";Sweep_z_axis,"Z axis"])
    [Rdk.Plane_generators.Grid_points,"Points",false;Grid_rows,"Rows",false;Grid_columns,"Columns",false;
     Grid_rows_and_columns,"Rows and columns",false;Grid_quads,"Quads",true;Grid_triangles,"Triangles",true;
     Grid_alternating_triangles,"Alternating triangles",true;Grid_reverse_triangles,"Reverse triangles",true];
  List.iter (fun backbone_selected -> List.iter (fun profile_selected ->
    let backbone_group = if backbone_selected then "selected" else " "
    and cross_section_group = if profile_selected then "selected" else " " in
    let typed = Lisp_sop.node ~with_:["sweep_backbone", (sweep_backbone); "sweep_profile", (sweep_profile)] (Printf.sprintf {|(sop/sweep
   (sop/ext_sweep_backbone)
   (sop/ext_sweep_profile)
   :backbone_group %S
   :cross_section_group %S
   :caps true)|} (backbone_group) (cross_section_group)) in
    same_cook "sweep selections" ~inputs:[sweep_profile] ~typed ~factory:Nodes.Sweep.factory
      ["backbone_group",Param.Text_value backbone_group;"cross_section_group",Text_value cross_section_group;"caps",Bool_value true]
      sweep_backbone;
    let backbones = if backbone_selected then Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "selected" sweep_backbone_geometry else None
    and cross_sections = if profile_selected then Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "selected" sweep_profile_geometry else None in
    let native = Rdk.Sweep_modeling.sweep ?backbones ?cross_sections ~caps:true ~cap_group:"caps"
        ~backbone:sweep_backbone_geometry ~cross_section:sweep_profile_geometry () |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "sweep selections match native") [false;true]) [false;true];
  same_cook "sweep unset outputs" ~inputs:[sweep_profile]
    ~typed:(Lisp_sop.node ~with_:["sweep_backbone", (sweep_backbone); "sweep_profile", (sweep_profile)] {|(sop/sweep
   (sop/ext_sweep_backbone)
   (sop/ext_sweep_profile)
   :caps true
   :cap_group " "
   :uv_attribute " ")|})
    ~factory:Nodes.Sweep.factory ["caps",Param.Bool_value true;"cap_group",Text_value " ";"uv_attribute",Text_value " "] sweep_backbone;
  List.iter (fun make -> check (rejected make)
      "sweep refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["sweep_backbone", (sweep_backbone); "sweep_profile", (sweep_profile)] {|(sop/sweep
   (sop/ext_sweep_backbone)
   (sop/ext_sweep_profile)
   :caps true
   :connectivity "Points")|});
     (fun () -> Lisp_sop.node ~with_:["sweep_backbone", (sweep_backbone); "sweep_profile", (sweep_profile)] {|(sop/sweep (sop/ext_sweep_backbone) (sop/ext_sweep_profile) :uv_attribute "P")|})];
  check (Result.is_error (Node.apply_parameters swept ["twist",Param.Float_value nan])) "sweep refuses invalid inspector edit";
  let scatter_source = Lisp_sop.node {|(-> (sop/grid :columns 4 :rows 3)
     (sop/set_float :name "density" :value 0.5)
     (sop/set_float :owner "Vertex" :name "density" :value 0.6)
     (sop/set_float :owner "Primitive" :name "density" :value 0.7)
     (sop/set_float :owner "Detail" :name "density" :value 0.8)
     (sop/set_float :name "point_value" :value 2.0)
     (sop/set_float :owner "Vertex" :name "vertex_value" :value 3.0)
     (sop/set_float :owner "Primitive" :name "primitive_value" :value 4.0)
     (sop/set_float :owner "Detail" :name "detail_value" :value 5.0)
     (sop/group_range :name "points" :end_ 2)
     (sop/group_range :owner "Vertices" :name "vertices" :end_ 2)
     (sop/group_range :owner "Primitives" :name "surface" :end_ 1))|} in
  let scatter_geometry = cook 1 scatter_source in
  let scatter_input = Lisp_sop.snapshot (scatter_geometry) in
  let scattered = Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter (sop/ext_scatter_input))|} in
  same_cook "scatter defaults" ~typed:scattered ~factory:Nodes.Scatter.factory [] scatter_input;
  cache_identity ~companions:["match_groups",["point_pattern",Param.Text_value "point_value"];
    "source_vertex_numbers_attribute",["source_vertex_weights_attribute",Param.Text_value "weights"];
    "source_vertex_weights_attribute",["source_vertex_numbers_attribute",Param.Text_value "numbers"]]
    "scatter all fields" scattered;
  List.iter (fun (density_owner,owner_choice) -> List.iter (fun use_density ->
    List.iter (fun context_seed -> List.iter (fun match_groups ->
      let typed = sop ~inputs:[scatter_input] "scatter"
          [ki "count" 9; ki "seed" 7; kb "context_seed" context_seed; ks "group" "surface"; kb "use_density" use_density;
           ks "density_owner" owner_choice; ks "density_attribute" "density"; ks "point_pattern" "point_value points";
           ks "vertex_pattern" "vertex_value vertices"; ks "primitive_pattern" "primitive_value surface";
           ks "detail_pattern" "detail_value"; kb "match_groups" match_groups;
           ks "source_primitive_attribute" "primitive"; ks "source_vertex_numbers_attribute" "numbers";
           ks "source_vertex_weights_attribute" "weights"] in
      same_cook ("scatter " ^ owner_choice) ~typed ~factory:Nodes.Scatter.factory
        ["count",Param.Int_value 9;"seed",Int_value 7;"context_seed",Bool_value context_seed;"group",Text_value "surface";
         "use_density",Bool_value use_density;"density_owner",Choice_value owner_choice;"density_attribute",Text_value "density";
         "point_pattern",Text_value "point_value points";"vertex_pattern",Text_value "vertex_value vertices";
         "primitive_pattern",Text_value "primitive_value surface";"detail_pattern",Text_value "detail_value";
         "match_groups",Bool_value match_groups;"source_primitive_attribute",Text_value "primitive";
         "source_vertex_numbers_attribute",Text_value "numbers";"source_vertex_weights_attribute",Text_value "weights"] scatter_input;
      check (Context.Dependencies.mem Context.Dependencies.Seed (Node.dependencies typed) = context_seed)
        "scatter context seed owns its dependency";
      if not context_seed then begin
        let density = if use_density then Some (Rdk.Scatter.density ~owner:density_owner "density") else None in
        let primitives = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "surface" scatter_geometry |> Option.get in
        let native = Rdk.Scatter.run ~count:9 ~seed:7 ~primitives ?density ~point_pattern:"point_value points"
            ~vertex_pattern:"vertex_value vertices" ~primitive_pattern:"primitive_value surface" ~detail_pattern:"detail_value"
            ~match_groups ~source_primitive_attribute:"primitive" ~source_vertex_numbers_attribute:"numbers"
            ~source_vertex_weights_attribute:"weights" scatter_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "scatter flat controls match native"
      end) [false;true]) [false;true]) [false;true])
    [Rdk.Attribute.Point,"Point";Vertex,"Vertex";Primitive,"Primitive";Detail,"Detail"];
  same_cook "scatter zero count" ~typed:(Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter (sop/ext_scatter_input) :count 0)|})
    ~factory:Nodes.Scatter.factory ["count",Param.Int_value 0] scatter_input;
  same_cook "scatter unset names" ~typed:(Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter
   (sop/ext_scatter_input)
   :group " "
   :point_pattern " "
   :vertex_pattern " "
   :primitive_pattern " "
   :detail_pattern " "
   :source_primitive_attribute " "
   :source_vertex_numbers_attribute " "
   :source_vertex_weights_attribute " ")|}) ~factory:Nodes.Scatter.factory
    ["group",Param.Text_value " ";"point_pattern",Text_value " ";"vertex_pattern",Text_value " ";
     "primitive_pattern",Text_value " ";"detail_pattern",Text_value " ";"source_primitive_attribute",Text_value " ";
     "source_vertex_numbers_attribute",Text_value " ";"source_vertex_weights_attribute",Text_value " "] scatter_input;
  List.iter (fun make -> check (rejected make)
      "scatter refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter (sop/ext_scatter_input) :count -1)|});
     (fun () -> Lisp_sop.node ~with_:["scatter_input", (scatter_input)] (Printf.sprintf {|(sop/scatter (sop/ext_scatter_input) :count %d)|} (max_int)));
     (fun () -> Lisp_sop.node ~with_:["scatter_input", (scatter_input)] (Printf.sprintf {|(sop/scatter (sop/ext_scatter_input) :count %d :point_pattern "point_value")|} ((Sys.max_array_length / 3 + 1))));
     (fun () -> Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter (sop/ext_scatter_input) :use_density true :density_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter (sop/ext_scatter_input) :use_density true :density_attribute "P")|});
     (fun () -> Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter (sop/ext_scatter_input) :point_pattern "broken[")|});
     (fun () -> Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter (sop/ext_scatter_input) :match_groups true)|});
     (fun () -> Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter (sop/ext_scatter_input) :source_primitive_attribute "P")|});
     (fun () -> Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter (sop/ext_scatter_input) :source_vertex_numbers_attribute "numbers")|});
     (fun () -> Lisp_sop.node ~with_:["scatter_input", (scatter_input)] {|(sop/scatter
   (sop/ext_scatter_input)
   :source_vertex_numbers_attribute "same"
   :source_vertex_weights_attribute "same")|})];
  check (Result.is_error (Node.apply_parameters scattered ["source_vertex_numbers_attribute",Param.Text_value "numbers"]))
    "scatter refuses invalid inspector edit";
  let soft_source = Lisp_sop.node {|(-> (sop/grid :columns 4 :rows 3)
     (sop/set_float :name "mask" :value 0.4)
     (sop/group_range :name "seed" :end_ 0)
     (sop/group_range :owner "Vertices" :name "seed" :end_ 0)
     (sop/group_range :owner "Primitives" :name "seed" :end_ 0)
     (sop/group_edges :name "seed" :incidence "Boundary"))|} in
  let soft_geometry = cook 1 soft_source in
  let soft_input = Lisp_sop.snapshot (soft_geometry) in
  let soft = sop ~inputs:[soft_input] "soft_transform" [] in
  same_cook "soft transform defaults" ~typed:soft ~factory:Nodes.Soft_transform.factory [] soft_input;
  cache_identity "soft transform all fields" soft;
  let translate = Vec3.create 0.2 0.3 (-0.1) and rotate = Vec3.create 0.1 0.2 0.3
  and scale = Vec3.create 1.1 0.9 1.2 and pivot = Vec3.create 0.1 (-0.1) 0.2
  and pivot_rotation = Vec3.create 0.2 0.1 (-0.1) in
  let transform_values = ["translate_x",Param.Float_value translate.x;"translate_y",Float_value translate.y;
    "translate_z",Float_value translate.z;"rotate_x",Float_value rotate.x;"rotate_y",Float_value rotate.y;
    "rotate_z",Float_value rotate.z;"scale_x",Float_value scale.x;"scale_y",Float_value scale.y;
    "scale_z",Float_value scale.z;"shear_xy",Float_value 0.1;"shear_xz",Float_value 0.2;"shear_yz",Float_value (-0.1);
    "uniform_scale",Float_value 0.8;"pivot_x",Float_value pivot.x;"pivot_y",Float_value pivot.y;
    "pivot_z",Float_value pivot.z;"pivot_rotation_x",Float_value pivot_rotation.x;
    "pivot_rotation_y",Float_value pivot_rotation.y;"pivot_rotation_z",Float_value pivot_rotation.z] in
  let matrix ?order ?rotation_order ~invert () = Rdk.Transform_ops.compose_transform ?order ?rotation_order
      ~translate ~rotate ~scale ~shear:(Vec3.create 0.1 0.2 (-0.1)) ~uniform_scale:0.8 ~pivot ~pivot_rotation ~invert ()
      |> Result.get_ok in
  List.iter (fun (owner_choice,selection) ->
    List.iter (fun metric_choice -> List.iter (fun apply_rolloff ->
      List.iter (fun (falloff,falloff_choice) -> List.iter (fun invert ->
        let typed = sop ~inputs:[soft_input] "soft_transform"
            [kv "translate" translate; kv "rotate" rotate; kv "scale" scale; kf "shear_xy" 0.1; kf "shear_xz" 0.2;
             kf "shear_yz" (-0.1); kf "uniform_scale" 0.8; kv "pivot" pivot; kv "pivot_rotation" pivot_rotation;
             kb "invert" invert; ks "group_owner" owner_choice; ks "group" "seed"; ks "metric" metric_choice;
             ks "metric_attribute" "mask"; kb "apply_rolloff" apply_rolloff; ks "falloff" falloff_choice;
             kf "radius" 2.; ks "falloff_attribute" "weight"; kb "recompute_normals" false] in
        same_cook ("soft transform " ^ owner_choice ^ metric_choice ^ falloff_choice) ~typed ~factory:Nodes.Soft_transform.factory
          (transform_values @ ["group_owner",Param.Choice_value owner_choice;"group",Text_value "seed";
            "metric",Choice_value metric_choice;"metric_attribute",Text_value "mask";"apply_rolloff",Bool_value apply_rolloff;
            "falloff",Choice_value falloff_choice;"radius",Float_value 2.;"falloff_attribute",Text_value "weight";
            "recompute_normals",Bool_value false;"invert",Bool_value invert]) soft_input;
        let metric = match metric_choice with "Radius" -> Rdk.Transform_ops.Soft_radius
          | "Edge distance" -> Rdk.Transform_ops.Soft_edge | _ -> Rdk.Transform_ops.Soft_attribute {attribute="mask";apply_rolloff} in
        let native = Rdk.Transform_ops.soft_transform ~selection ~metric ~falloff ~radius:2.
            ~falloff_attribute:"weight" ~recompute_normals:false (matrix ~invert ()) soft_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "soft transform flat controls match native") [false;true])
        [Rdk.Transform_ops.Soft_linear,"Linear";Soft_quadratic,"Quadratic";Soft_cubic,"Cubic"])
        [false;true]) ["Radius";"Edge distance";"Attribute"])
    ["Point",Rdk.Transform_ops.Selected_points (Rdk.Geometry.find_group ~owner:Rdk.Group.Point "seed" soft_geometry |> Option.get);
     "Vertex",Selected_vertices (Rdk.Geometry.find_group ~owner:Rdk.Group.Vertex "seed" soft_geometry |> Option.get);
     "Primitive",Selected_primitives (Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "seed" soft_geometry |> Option.get);
     "Edge",Selected_edges (Rdk.Geometry.find_edge_group "seed" soft_geometry |> Option.get)];
  List.iter (fun (order,order_choice) -> List.iter (fun (rotation_order,rotation_choice) ->
    let typed = sop ~inputs:[soft_input] "soft_transform"
        [ks "order" order_choice; ks "rotation_order" rotation_choice; kv "translate" translate; kv "rotate" rotate;
         kv "scale" scale; kf "shear_xy" 0.1; kf "shear_xz" 0.2; kf "shear_yz" (-0.1); kf "uniform_scale" 0.8;
         kv "pivot" pivot; kv "pivot_rotation" pivot_rotation] in
    same_cook "soft transform orders" ~typed ~factory:Nodes.Soft_transform.factory
      (transform_values @ ["order",Param.Choice_value order_choice;"rotation_order",Choice_value rotation_choice]) soft_input;
    let native = Rdk.Transform_ops.soft_transform (matrix ~order ~rotation_order ~invert:false ()) soft_geometry |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "soft transform orders match native")
      [Rdk.Transform_ops.Transform_xyz,"XYZ";Transform_xzy,"XZY";Transform_yxz,"YXZ";Transform_yzx,"YZX";Transform_zxy,"ZXY";Transform_zyx,"ZYX"])
    [Rdk.Transform_ops.Transform_srt,"SRT";Transform_str,"STR";Transform_rst,"RST";Transform_rts,"RTS";Transform_tsr,"TSR";Transform_trs,"TRS"];
  same_cook "soft transform raw mask zero radius"
    ~typed:(sop ~inputs:[soft_input] "soft_transform" [ks "metric" "Attribute"; kb "apply_rolloff" false; kf "radius" 0.])
    ~factory:Nodes.Soft_transform.factory ["metric",Param.Choice_value "Attribute";"apply_rolloff",Bool_value false;"radius",Float_value 0.] soft_input;
  same_cook "soft transform unset names" ~typed:(sop ~inputs:[soft_input] "soft_transform" [ks "group" " "; ks "falloff_attribute" " "])
    ~factory:Nodes.Soft_transform.factory ["group",Param.Text_value " ";"falloff_attribute",Text_value " "] soft_input;
  List.iter (fun make -> check (rejected make)
      "soft transform refuses invalid controls at construction")
    [(fun () -> sop ~inputs:[soft_input] "soft_transform" [kv "scale" Vec3.zero; kb "invert" true]);
     (fun () -> sop ~inputs:[soft_input] "soft_transform" [kv "scale" (Vec3.create Float.max_float 1. 1.); kf "uniform_scale" Float.max_float]);
     (fun () -> sop ~inputs:[soft_input] "soft_transform" [kf "radius" 0.]);
     (fun () -> sop ~inputs:[soft_input] "soft_transform" [ks "metric" "Attribute"; ks "metric_attribute" " "]);
     (fun () -> sop ~inputs:[soft_input] "soft_transform" [ks "metric" "Attribute"; ks "metric_attribute" "P"]);
     (fun () -> sop ~inputs:[soft_input] "soft_transform" [ks "falloff_attribute" "P"])];
  check (Result.is_error (Node.apply_parameters soft ["translate_x",Param.Float_value nan]))
    "soft transform refuses invalid inspector edit";
  let reduce_source = Lisp_sop.node {|(sop/grid :columns 6 :rows 5)|}
      |> group_indices "Primitives" "selected" (Array.init 30 Fun.id)
      |> group_indices "Points" "hard_points" [|0;5|]
      |> edge_group "hard_edges" in
  let reduce_geometry = cook 1 reduce_source in
  let reduce_input = Lisp_sop.snapshot (reduce_geometry) in
  let reduced = Lisp_sop.node ~with_:["reduce_input", (reduce_input)] {|(sop/poly_reduce (sop/ext_reduce_input))|} in
  same_cook "poly reduce defaults" ~typed:reduced ~factory:Nodes.Poly_reduce.factory [] reduce_input;
  cache_identity "poly reduce all fields" reduced;
  List.iter (fun (choice,native_target) ->
    List.iter (fun preserve_boundary -> List.iter (fun only_original_positions ->
      List.iter (fun limit_normal_deviation ->
        let typed = sop ~inputs:[reduce_input] "poly_reduce"
            [ks "target_mode" choice; kf "ratio" 0.6; ki "primitive_count" 24; kb "preserve_boundary" preserve_boundary;
             kb "only_original_positions" only_original_positions; kf "equalize_lengths" 1e-8;
             kb "limit_normal_deviation" limit_normal_deviation; kf "max_normal_deviation" 0.7;
             ks "output_group" "reduced"; kb "recompute_point_normals" false] in
        same_cook ("poly reduce " ^ choice) ~typed ~factory:Nodes.Poly_reduce.factory
          ["target_mode",Param.Choice_value choice;"ratio",Float_value 0.6;"primitive_count",Int_value 24;
           "preserve_boundary",Bool_value preserve_boundary;"only_original_positions",Bool_value only_original_positions;
           "equalize_lengths",Float_value 1e-8;"limit_normal_deviation",Bool_value limit_normal_deviation;
           "max_normal_deviation",Float_value 0.7;"output_group",Text_value "reduced";"recompute_point_normals",Bool_value false] reduce_input;
        let max_normal_deviation = if limit_normal_deviation then Some 0.7 else None in
        let native = Rdk.Poly_reduce.run ~target:native_target ~preserve_boundary ~only_original_positions
            ~equalize_lengths:1e-8 ?max_normal_deviation ~output_group:"reduced" ~recompute_point_normals:false
            reduce_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "poly reduce flat target matches native") [false;true]) [false;true]) [false;true])
    ["Percentage",Rdk.Poly_reduce.Reduce_ratio 0.6;
     "Primitive count",Rdk.Poly_reduce.Reduce_primitive_count 24];
  List.iter (fun (group,hard_point_group,hard_edge_group) ->
    let typed = Lisp_sop.node ~with_:["reduce_input", (reduce_input)] (Printf.sprintf {|(sop/poly_reduce
   (sop/ext_reduce_input)
   :group %S
   :hard_point_group %S
   :hard_edge_group %S)|} (group) (hard_point_group) (hard_edge_group)) in
    same_cook "poly reduce constraints" ~typed ~factory:Nodes.Poly_reduce.factory
      ["group",Param.Text_value group;"hard_point_group",Text_value hard_point_group;"hard_edge_group",Text_value hard_edge_group]
      reduce_input;
    let find owner name = if name = "" then None else Rdk.Geometry.find_group ~owner name reduce_geometry in
    let primitives = find Rdk.Group.Primitive group and hard_points = find Rdk.Group.Point hard_point_group
    and hard_edges = if hard_edge_group = "" then None else Rdk.Geometry.find_edge_group hard_edge_group reduce_geometry in
    let native = Rdk.Poly_reduce.run ~target:(Rdk.Poly_reduce.Reduce_ratio 0.5) ?primitives ?hard_points ?hard_edges
        reduce_geometry |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "poly reduce constraints match native")
    ["selected","","";"","hard_points","";"","","hard_edges";"selected","hard_points","hard_edges"];
  List.iter (fun ratio -> same_cook "poly reduce ratio endpoints" ~typed:(Lisp_sop.node ~with_:["reduce_input", (reduce_input)] (Printf.sprintf {|(sop/poly_reduce (sop/ext_reduce_input) :ratio %s)|} ((Lisp_sop.float ratio))))
      ~factory:Nodes.Poly_reduce.factory ["ratio",Param.Float_value ratio] reduce_input) [0.;1.];
  List.iter (fun primitive_count -> same_cook "poly reduce count endpoints"
      ~typed:(Lisp_sop.node ~with_:["reduce_input", (reduce_input)] (Printf.sprintf {|(sop/poly_reduce
   (sop/ext_reduce_input)
   :target_mode "Primitive count"
   :primitive_count %d)|} (primitive_count)))
      ~factory:Nodes.Poly_reduce.factory ["target_mode",Param.Choice_value "Primitive count";"primitive_count",Int_value primitive_count]
      reduce_input) [0;100];
  same_cook "poly reduce unset groups" ~typed:(Lisp_sop.node ~with_:["reduce_input", (reduce_input)] {|(sop/poly_reduce
   (sop/ext_reduce_input)
   :group " "
   :hard_point_group " "
   :hard_edge_group " "
   :output_group " ")|})
    ~factory:Nodes.Poly_reduce.factory ["group",Param.Text_value " ";"hard_point_group",Text_value " ";
      "hard_edge_group",Text_value " ";"output_group",Text_value " "] reduce_input;
  List.iter (fun make -> check (rejected make)
      "poly reduce refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["reduce_input", (reduce_input)] {|(sop/poly_reduce (sop/ext_reduce_input) :ratio 1.1)|});
     (fun () -> Lisp_sop.node ~with_:["reduce_input", (reduce_input)] {|(sop/poly_reduce (sop/ext_reduce_input) :primitive_count -1)|});
     (fun () -> Lisp_sop.node ~with_:["reduce_input", (reduce_input)] {|(sop/poly_reduce (sop/ext_reduce_input) :equalize_lengths -1.0)|});
     (fun () -> Lisp_sop.node ~with_:["reduce_input", (reduce_input)] {|(sop/poly_reduce (sop/ext_reduce_input) :max_normal_deviation 4.0)|})];
  check (Result.is_error (Node.apply_parameters reduced ["max_normal_deviation",Param.Float_value nan]))
    "poly reduce refuses invalid inspector edit";
  let cut_geometry = Sources.curve_cut () in
  let cut_points = Rdk.Group.init ~owner:Rdk.Group.Point ~name:"selected" 4 (fun i -> i mod 2 = 0) in
  let topology = Rdk.Geometry.topology cut_geometry in
  let cut_edges = Rdk.Edge_group.init ~topology ~index:(Rdk.Topology_index.create topology)
      ~name:"selected" (fun i -> i mod 2 = 0) in
  let cut_geometry = Rdk.Geometry.with_group cut_points cut_geometry |> get |> Rdk.Geometry.with_edge_group cut_edges |> get in
  let cut_input = Lisp_sop.snapshot (cut_geometry) in
  let poly_cut = Lisp_sop.node ~with_:["cut_input", (cut_input)] {|(sop/poly_cut (sop/ext_cut_input))|} in
  same_cook "poly cut defaults" ~typed:poly_cut ~factory:Nodes.Poly_cut.factory [] cut_input;
  cache_identity "poly cut all fields" poly_cut;
  List.iter (fun (element,element_choice,cut_points,cut_edges) ->
    List.iter (fun (strategy,strategy_choice) ->
      List.iter (fun (detection_choice,native_detection) ->
        List.iter (fun keep_closed ->
          let typed = sop ~inputs:[cut_input] "poly_cut"
              [ks "element" element_choice; ks "strategy" strategy_choice; ks "detection" detection_choice;
               ks "attribute" "distance"; kf "value" 0.; kf "threshold" 0.5; kb "keep_closed" keep_closed;
               ks "group" "first"; ks "cut_group" "selected"] in
          same_cook ("poly cut " ^ element_choice ^ strategy_choice ^ detection_choice) ~typed ~factory:Nodes.Poly_cut.factory
            ["element",Param.Choice_value element_choice;"strategy",Choice_value strategy_choice;
             "detection",Choice_value detection_choice;"attribute",Text_value "distance";"value",Float_value 0.;"threshold",Float_value 0.5;
             "keep_closed",Bool_value keep_closed;"group",Text_value "first";"cut_group",Text_value "selected"] cut_input;
          let primitives = Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "first" cut_geometry |> Option.get in
          let native = Rdk.Poly_cut.cut ~element ~strategy ~detection:native_detection ~keep_closed ~primitives
              ?cut_points ?cut_edges cut_geometry |> Result.get_ok in
          check (equal_geometry native (cook 1 typed)) "poly cut flat detection matches native") [false;true])
        ["All selected",Rdk.Poly_cut.Poly_cut_all;
         "Attribute crossing",Poly_cut_crossing {attribute="distance";value=0.};
         "Attribute change",Poly_cut_change {attribute="distance";threshold=0.5}])
      [Rdk.Poly_cut.Poly_cut_remove,"Remove";Poly_cut_cut,"Cut"])
    [Rdk.Poly_cut.Poly_cut_points,"Points",Some cut_points,None;Poly_cut_edges,"Edges",None,Some cut_edges];
  same_cook "poly cut unset groups" ~typed:(Lisp_sop.node ~with_:["cut_input", (cut_input)] {|(sop/poly_cut (sop/ext_cut_input) :group " " :cut_group " ")|})
    ~factory:Nodes.Poly_cut.factory ["group",Param.Text_value " ";"cut_group",Text_value " "] cut_input;
  List.iter (fun make -> check (rejected make)
      "poly cut refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["cut_input", (cut_input)] {|(sop/poly_cut (sop/ext_cut_input) :threshold -1.0)|});
     (fun () -> Lisp_sop.node ~with_:["cut_input", (cut_input)] {|(sop/poly_cut (sop/ext_cut_input) :detection "Attribute crossing" :attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["cut_input", (cut_input)] {|(sop/poly_cut (sop/ext_cut_input) :detection "Attribute crossing" :attribute "P")|});
     (fun () -> Lisp_sop.node ~with_:["cut_input", (cut_input)] {|(sop/poly_cut (sop/ext_cut_input) :detection "Attribute change" :strategy "Cut")|})];
  check (Result.is_error (Node.apply_parameters poly_cut ["threshold",Param.Float_value nan]))
    "poly cut refuses invalid inspector edit";
  let generation_geometry = Sources.point_generate () in
  let probability = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"probability"
      (Rdk.Attribute.Float [|0.25;0.75;1.|]) |> get in
  let generation_geometry = Rdk.Geometry.with_attribute probability generation_geometry |> get in
  let generation_input = Lisp_sop.snapshot (generation_geometry) in
  let generated = Lisp_sop.node ~with_:["generation_input", (generation_input)] {|(sop/point_generate (sop/ext_generation_input))|} in
  same_cook "point generate defaults" ~typed:generated ~factory:Nodes.Point_generate_from_input.factory [] generation_input;
  cache_identity "point generate all fields" generated;
  List.iter (fun (choice,native_mode) ->
    List.iter (fun keep_input -> List.iter (fun context_seed ->
      let typed = sop ~inputs:[generation_input] "point_generate"
          [ks "mode" choice; ki "total" 7; kf "points_per_point" 1.5; ks "scale_attribute" "density";
           ks "probability_attribute" "probability"; kb "keep_input" keep_input; kb "context_seed" context_seed;
           ki "seed" 7; ks "group" "emit"; ks "generated_group" "made"; ks "source_point_attribute" "from";
           ks "source_index_attribute" "index"; ks "copy_point_attributes" "id"] in
      same_cook ("point generate " ^ choice) ~typed ~factory:Nodes.Point_generate_from_input.factory
        ["mode",Param.Choice_value choice;"total",Int_value 7;"points_per_point",Float_value 1.5;
         "scale_attribute",Text_value "density";"probability_attribute",Text_value "probability";
         "keep_input",Bool_value keep_input;"context_seed",Bool_value context_seed;"seed",Int_value 7;
         "group",Text_value "emit";"generated_group",Text_value "made";"source_point_attribute",Text_value "from";
         "source_index_attribute",Text_value "index";"copy_point_attributes",Text_value "id"] generation_input;
      if not context_seed then begin
        let points = Rdk.Geometry.find_group ~owner:Rdk.Group.Point "emit" generation_geometry |> Option.get in
        let native = Rdk.Point_generate.run ~mode:native_mode ~points ~keep_input ~seed:(Rand.seed 7)
            ~generated_group:"made" ~source_point_attribute:"from" ~source_index_attribute:"index"
            ~copy_point_attributes:"id" generation_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "point generate flat modes match native"
      end) [false;true]) [false;true])
    ["Total count",Rdk.Point_generate.Generate_total 7;
     "Per point",Generate_per_point {points_per_point=1.5;scale_attribute=Some "density"};
     "Probability attribute",Generate_probability {attribute="probability"}];
  same_cook "point generate unset names" ~typed:(Lisp_sop.node ~with_:["generation_input", (generation_input)] {|(sop/point_generate
   (sop/ext_generation_input)
   :group " "
   :scale_attribute " "
   :generated_group " "
   :copy_point_attributes " "
   :copy_detail_attributes " ")|})
    ~factory:Nodes.Point_generate_from_input.factory ["group",Param.Text_value " ";"scale_attribute",Text_value " ";
      "generated_group",Text_value " ";"copy_point_attributes",Text_value " ";"copy_detail_attributes",Text_value " "] generation_input;
  List.iter (fun make -> check (rejected make)
      "point generate refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["generation_input", (generation_input)] {|(sop/point_generate (sop/ext_generation_input) :total -1)|});
     (fun () -> Lisp_sop.node ~with_:["generation_input", (generation_input)] (Printf.sprintf {|(sop/point_generate (sop/ext_generation_input) :points_per_point %s)|} ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node ~with_:["generation_input", (generation_input)] {|(sop/point_generate (sop/ext_generation_input) :source_point_attribute "P")|});
     (fun () -> Lisp_sop.node ~with_:["generation_input", (generation_input)] {|(sop/point_generate
   (sop/ext_generation_input)
   :source_point_attribute "same"
   :source_index_attribute "same")|});
     (fun () -> Lisp_sop.node ~with_:["generation_input", (generation_input)] {|(sop/point_generate
   (sop/ext_generation_input)
   :mode "Probability attribute"
   :probability_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["generation_input", (generation_input)] {|(sop/point_generate (sop/ext_generation_input) :copy_detail_attributes "broken[")|})];
  check (Result.is_error (Node.apply_parameters generated ["points_per_point",Param.Float_value nan]))
    "point generate refuses invalid inspector edit";
  let extract_geometry = Sources.curve_cut () in
  let extract_input = Lisp_sop.snapshot (extract_geometry) in
  let extracted = Lisp_sop.node ~with_:["extract_input", (extract_input)] {|(sop/extract_point_from_curve (sop/ext_extract_input))|} in
  same_cook "extract point curve defaults" ~typed:extracted ~factory:Nodes.Extract_point_from_curve.factory [] extract_input;
  cache_identity "extract point curve all fields" extracted;
  List.iter (fun (cut_choice,native_cut) ->
    List.iter (fun copy_primitive_attributes -> List.iter (fun selected ->
      let group = if selected then "first" else " " in
      let typed = sop ~inputs:[extract_input] "extract_point_from_curve"
          [ks "cut" cut_choice; kf "constant" 0.25; ks "primitive_attribute" "cut"; ks "group" group;
           ks "point_attributes" "weight"; kb "copy_primitive_attributes" copy_primitive_attributes;
           ks "primitive_attributes" "material"; ks "curve_u_attribute" "u"; ks "number_cuts_attribute" "cuts";
           ks "curve_number_attribute" "curve"] in
      same_cook ("extract point curve " ^ cut_choice) ~typed ~factory:Nodes.Extract_point_from_curve.factory
        ["cut",Param.Choice_value cut_choice;"constant",Float_value 0.25;"primitive_attribute",Text_value "cut";
         "group",Text_value group;"point_attributes",Text_value "weight";"copy_primitive_attributes",Bool_value copy_primitive_attributes;
         "primitive_attributes",Text_value "material";"curve_u_attribute",Text_value "u";
         "number_cuts_attribute",Text_value "cuts";"curve_number_attribute",Text_value "curve"] extract_input;
      let primitives = if selected then Rdk.Geometry.find_group ~owner:Rdk.Group.Primitive "first" extract_geometry else None in
      let native = Rdk.Curve_topology.extract_point_from_curve ?primitives ~cut:native_cut ~point_attributes:"weight"
          ~copy_primitive_attributes ~primitive_attributes:"material" ~curve_u_attribute:"u" ~number_cuts_attribute:"cuts"
          ~curve_number_attribute:"curve" ~distance_attribute:"distance" extract_geometry |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "extract point curve flat cut matches native") [false;true]) [false;true])
    ["Constant",Rdk.Curve_topology.Extract_cut_constant 0.25;
     "Primitive attribute",Extract_cut_primitive_attribute "cut";
     "Current time",Extract_cut_constant 0.];
  same_cook "extract point curve unset diagnostics" ~typed:(Lisp_sop.node ~with_:["extract_input", (extract_input)] {|(sop/extract_point_from_curve
   (sop/ext_extract_input)
   :group " "
   :curve_u_attribute " "
   :number_cuts_attribute " "
   :curve_number_attribute " ")|})
    ~factory:Nodes.Extract_point_from_curve.factory ["group",Param.Text_value " ";
      "curve_u_attribute",Text_value " ";"number_cuts_attribute",Text_value " ";"curve_number_attribute",Text_value " "] extract_input;
  List.iter (fun make -> check (rejected make)
      "extract point curve refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["extract_input", (extract_input)] {|(sop/extract_point_from_curve
   (sop/ext_extract_input)
   :cut "Primitive attribute"
   :primitive_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["extract_input", (extract_input)] {|(sop/extract_point_from_curve (sop/ext_extract_input) :distance_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["extract_input", (extract_input)] {|(sop/extract_point_from_curve (sop/ext_extract_input) :curve_u_attribute "P")|});
     (fun () -> Lisp_sop.node ~with_:["extract_input", (extract_input)] {|(sop/extract_point_from_curve
   (sop/ext_extract_input)
   :curve_u_attribute "same"
   :curve_number_attribute "same")|});
     (fun () -> Lisp_sop.node ~with_:["extract_input", (extract_input)] {|(sop/extract_point_from_curve (sop/ext_extract_input) :point_attributes "broken[")|});
     (fun () -> Lisp_sop.node ~with_:["extract_input", (extract_input)] {|(sop/extract_point_from_curve (sop/ext_extract_input) :primitive_attributes "broken[")|})];
  check (Result.is_error (Node.apply_parameters extracted ["constant",Param.Float_value nan]))
    "extract point curve refuses invalid inspector edit";

  let mountain_source = Lisp_sop.node {|(-> (sop/grid :columns 4 :rows 3)
     (sop/set_vector :name "direction" :value [1.0 2.0 3.0])
     (sop/set_float :name "mask" :value 0.5)
     (sop/group_range :name "selected" :end_ 2))|} in
  let mountain = Lisp_sop.node ~with_:["mountain_source", (mountain_source)] {|(sop/mountain (sop/ext_mountain_source))|} in
  same_cook "mountain defaults" ~typed:mountain ~factory:Nodes.Mountain.factory [] mountain_source;
  cache_identity "mountain all fields" mountain;
  let mountain_geometry = cook 1 mountain_source in
  List.iter (fun seed_choice ->
    List.iter (fun normalize_direction -> List.iter (fun recompute_normals ->
      let typed = sop ~inputs:[mountain_source] "mountain"
          [ks "seed_mode" seed_choice; ki "seed" 23; ks "group" "selected"; ks "direction_attribute" "direction";
           ks "mask_attribute" "mask"; kb "normalize_direction" normalize_direction; kf "height" 0.7;
           kv "frequency" (Vec3.create 0.4 0.8 0.55); kv "offset" (Vec3.create 2. 3. 5.); ki "octaves" 5;
           kf "lacunarity" 2.05; kf "roughness" 0.48; ks "height_attribute" "height";
           kb "recompute_normals" recompute_normals] in
      same_cook ("mountain " ^ seed_choice) ~typed ~factory:Nodes.Mountain.factory
        ["seed_mode",Param.Choice_value seed_choice;"seed",Int_value 23;"group",Text_value "selected";
         "direction_attribute",Text_value "direction";"mask_attribute",Text_value "mask";
         "normalize_direction",Bool_value normalize_direction;"height",Float_value 0.7;
         "frequency_x",Float_value 0.4;"frequency_y",Float_value 0.8;"frequency_z",Float_value 0.55;
         "offset_x",Float_value 2.;"offset_y",Float_value 3.;"offset_z",Float_value 5.;"octaves",Int_value 5;
         "lacunarity",Float_value 2.05;"roughness",Float_value 0.48;"height_attribute",Text_value "height";
         "recompute_normals",Bool_value recompute_normals] mountain_source;
      if seed_choice = "Explicit" then begin
        let selected = Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected" mountain_geometry |> Option.get in
        let native = Rdk.Deform.mountain ~seed:23 ~selection:(Rdk.Transform_ops.Selected_points selected)
            ~direction_attribute:"direction" ~mask_attribute:"mask" ~normalize_direction ~height:0.7
            ~frequency:(Vec3.create 0.4 0.8 0.55) ~offset:(Vec3.create 2. 3. 5.) ~octaves:5 ~lacunarity:2.05
            ~roughness:0.48 ~height_attribute:"height" ~recompute_normals mountain_geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "mountain flat controls match native"
      end) [false;true]) [false;true])
    ["Explicit";"Auto"];
  same_cook "mountain unset names" ~typed:(Lisp_sop.node ~with_:["mountain_source", (mountain_source)] {|(sop/mountain
   (sop/ext_mountain_source)
   :group " "
   :direction_attribute " "
   :mask_attribute " "
   :height_attribute " "
   :height 0.0)|})
    ~factory:Nodes.Mountain.factory ["group",Param.Text_value " ";"direction_attribute",Text_value " ";
      "mask_attribute",Text_value " ";"height_attribute",Text_value " ";"height",Float_value 0.] mountain_source;
  List.iter (fun make -> check (rejected make)
      "mountain refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["mountain_source", (mountain_source)] {|(sop/mountain (sop/ext_mountain_source) :height -1.0)|});
     (fun () -> Lisp_sop.node ~with_:["mountain_source", (mountain_source)] {|(sop/mountain (sop/ext_mountain_source) :frequency [-1.0 1.0 1.0])|});
     (fun () -> Lisp_sop.node ~with_:["mountain_source", (mountain_source)] {|(sop/mountain (sop/ext_mountain_source) :octaves 0)|});
     (fun () -> Lisp_sop.node ~with_:["mountain_source", (mountain_source)] {|(sop/mountain (sop/ext_mountain_source) :octaves 65)|});
     (fun () -> Lisp_sop.node ~with_:["mountain_source", (mountain_source)] {|(sop/mountain (sop/ext_mountain_source) :lacunarity 0.0)|});
     (fun () -> Lisp_sop.node ~with_:["mountain_source", (mountain_source)] {|(sop/mountain (sop/ext_mountain_source) :roughness 1.1)|});
     (fun () -> Lisp_sop.node ~with_:["mountain_source", (mountain_source)] {|(sop/mountain (sop/ext_mountain_source) :height_attribute "P")|})];
  check (Result.is_error (Node.apply_parameters mountain ["roughness",Param.Float_value nan]))
    "mountain refuses invalid inspector edit";

  let tube = Lisp_sop.node {|(sop/tube)|} in
  same_generator "tube defaults" ~typed:tube ~factory:Nodes.Tube.factory [];
  cache_identity ~companions:["end_caps",["cap_group",Param.Text_value ""]] "tube all fields" tube;
  List.iter (fun (connectivity,connectivity_choice,polygon) ->
    List.iter (fun (normals,normals_choice) ->
      List.iter (fun (normals_mode_choice,native_normals) ->
        if connectivity <> Rdk.Parametric_generators.Tube_points || native_normals <> Some Rdk.Parametric_generators.Tube_vertex_normals then
          List.iter (fun end_caps -> List.iter (fun consolidate_cap_points ->
            List.iter (fun (top_radius,bottom_radius) ->
              let cap_group = if end_caps then "caps" else " " in
              let typed = sop "tube"
                  [ks "connectivity" connectivity_choice; ks "normals" normals_choice; ks "normals_mode" normals_mode_choice;
                   kb "end_caps" end_caps; kb "consolidate_cap_points" consolidate_cap_points; ks "cap_group" cap_group;
                   ki "rows" 3; ki "columns" 5; kf "top_radius" top_radius; kf "bottom_radius" bottom_radius;
                   kf "height" 2.; kf "radius_scale" 1.25] in
              same_generator ("tube " ^ connectivity_choice ^ normals_choice ^ normals_mode_choice)
                ~typed ~factory:Nodes.Tube.factory ["connectivity",Param.Choice_value connectivity_choice;
                  "normals",Choice_value normals_choice;"normals_mode",Choice_value normals_mode_choice;
                  "end_caps",Bool_value end_caps;"consolidate_cap_points",Bool_value consolidate_cap_points;
                  "cap_group",Text_value cap_group;"rows",Int_value 3;"columns",Int_value 5;
                  "top_radius",Float_value top_radius;"bottom_radius",Float_value bottom_radius;"height",Float_value 2.;"radius_scale",Float_value 1.25];
              let native = Rdk.Parametric_generators.tube ~connectivity ?normals:native_normals ~end_caps ~consolidate_cap_points
                  ?cap_group:(if end_caps then Some "caps" else None) ~rows:3 ~columns:5 ~top_radius ~bottom_radius
                  ~height:2. ~radius_scale:1.25 ~uv_attribute:"uv" () |> Result.get_ok in
              check (equal_geometry native (cook 1 typed)) "tube flat caps and tips match native")
              [1.,1.;0.,1.;1.,0.]) [false;true]) (if polygon then [false;true] else [false]))
        ["Explicit",Some normals;"Auto",None])
      [Rdk.Parametric_generators.Tube_no_normals,"None";Tube_point_normals,"Point";Tube_vertex_normals,"Vertex"])
    [Rdk.Parametric_generators.Tube_triangles,"Triangles",true;Tube_alternating_triangles,"Alternating triangles",true;
     Tube_quads,"Quads",true;Tube_rows,"Rows",false;Tube_columns,"Columns",false;
     Tube_rows_and_columns,"Rows and columns",false;Tube_points,"Points",false];
  let tube_axis = Vec3.create 1. 2. 3. and tube_center = Vec3.create 2. (-1.) 3.
  and tube_rotation = Vec3.create 0.2 0.3 0.4 in
  List.iter (fun (orientation_choice,native_orientation) ->
    List.iter (fun (rotation_order,rotation_choice) ->
      let typed = sop "tube"
          [ks "orientation" orientation_choice; kv "axis" tube_axis; ks "rotation_order" rotation_choice;
           kv "center" tube_center; kv "rotation" tube_rotation; ki "rows" 3; ki "columns" 5;
           ks "uv_attribute" " "; ks "cap_group" " "] in
      same_generator ("tube orientation " ^ orientation_choice ^ rotation_choice)
        ~typed ~factory:Nodes.Tube.factory ["orientation",Param.Choice_value orientation_choice;
          "axis_x",Float_value 1.;"axis_y",Float_value 2.;"axis_z",Float_value 3.;
          "rotation_order",Choice_value rotation_choice;"center_x",Float_value 2.;"center_y",Float_value (-1.);"center_z",Float_value 3.;
          "rotation_x",Float_value 0.2;"rotation_y",Float_value 0.3;"rotation_z",Float_value 0.4;
          "rows",Int_value 3;"columns",Int_value 5;"uv_attribute",Text_value " ";"cap_group",Text_value " "];
      let native = Rdk.Parametric_generators.tube ~orientation:native_orientation ~rotation_order ~center:tube_center ~rotation:tube_rotation
          ~connectivity:Rdk.Parametric_generators.Tube_triangles ~end_caps:true ~consolidate_cap_points:false
          ~rows:3 ~columns:5 ~top_radius:1. ~bottom_radius:1. ~height:2. () |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "tube flat orientation and unset names match native")
      [Rdk.Parametric_generators.Tube_xyz,"XYZ";Tube_xzy,"XZY";Tube_yxz,"YXZ";Tube_yzx,"YZX";Tube_zxy,"ZXY";Tube_zyx,"ZYX"])
    ["X axis",Rdk.Parametric_generators.Tube_x;"Y axis",Rdk.Parametric_generators.Tube_y;"Z axis",Rdk.Parametric_generators.Tube_z;
     "Custom axis",Rdk.Parametric_generators.Tube_axis tube_axis];
  List.iter (fun make -> check (rejected make)
      "tube refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node {|(sop/tube :top_radius 0.0 :bottom_radius 0.0)|});
     (fun () -> Lisp_sop.node {|(sop/tube :top_radius -1.0)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/tube :radius_scale %s :top_radius 2.0)|} ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node {|(sop/tube :height 0.0)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/tube :orientation "Custom axis" :axis %s)|} ((Lisp_sop.vec3 Vec3.zero))));
     (fun () -> Lisp_sop.node {|(sop/tube :rows 1)|});
     (fun () -> Lisp_sop.node {|(sop/tube :columns 2)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/tube :rows %d)|} (Sys.max_array_length)));
     (fun () -> Lisp_sop.node {|(sop/tube :uv_attribute "N")|});
     (fun () -> Lisp_sop.node {|(sop/tube :connectivity "Points" :end_caps false :cap_group "" :normals "Vertex")|});
     (fun () -> Lisp_sop.node {|(sop/tube :connectivity "Rows")|});
     (fun () -> Lisp_sop.node {|(sop/tube :end_caps false)|})];
  check (Result.is_error (Node.apply_parameters tube ["end_caps",Param.Bool_value false]))
    "tube refuses invalid cap inspector edit";

  let torus = Lisp_sop.node {|(sop/torus)|} in
  same_generator "torus defaults" ~typed:torus ~factory:Nodes.Torus.factory [];
  cache_identity ~companions:["u_end_caps",["u_wrap",Param.Bool_value false];
    "v_end_cap",["v_wrap",Param.Bool_value false;"v_end",Param.Float_value Float.pi]]
    "torus all fields" torus;
  List.iter (fun (connectivity,connectivity_choice,polygon) ->
    List.iter (fun (normals,normals_choice) ->
      List.iter (fun (normals_mode_choice,native_normals) ->
        if connectivity <> Rdk.Parametric_generators.Torus_points || native_normals <> Some Rdk.Parametric_generators.Torus_vertex_normals then
          List.iter (fun u_wrap -> List.iter (fun v_wrap ->
            List.iter (fun u_end_caps -> List.iter (fun v_end_cap ->
              let typed = sop "torus"
                  [ks "connectivity" connectivity_choice; ks "normals" normals_choice; ks "normals_mode" normals_mode_choice;
                   kb "u_wrap" u_wrap; kb "v_wrap" v_wrap; kb "u_end_caps" u_end_caps; kb "v_end_cap" v_end_cap;
                   kf "u_start" 0.1; kf "u_end" 2.4; kf "v_start" (-0.8); kf "v_end" 1.8; ki "rows" 4; ki "columns" 5;
                   kf "major_radius" 3.; kf "minor_radius" 1.] in
              same_generator ("torus " ^ connectivity_choice ^ normals_choice ^ normals_mode_choice)
                ~typed ~factory:Nodes.Torus.factory ["connectivity",Param.Choice_value connectivity_choice;
                  "normals",Choice_value normals_choice;"normals_mode",Choice_value normals_mode_choice;
                  "u_wrap",Bool_value u_wrap;"v_wrap",Bool_value v_wrap;"u_end_caps",Bool_value u_end_caps;"v_end_cap",Bool_value v_end_cap;
                  "u_start",Float_value 0.1;"u_end",Float_value 2.4;"v_start",Float_value (-0.8);"v_end",Float_value 1.8;
                  "rows",Int_value 4;"columns",Int_value 5;"major_radius",Float_value 3.;"minor_radius",Float_value 1.];
              let native = Rdk.Parametric_generators.torus ~connectivity ?normals:native_normals ~u_wrap ~v_wrap ~u_end_caps ~v_end_cap
                  ~u_start:0.1 ~u_end:2.4 ~v_start:(-0.8) ~v_end:1.8 ~rows:4 ~columns:5 ~major_radius:3. ~minor_radius:1.
                  ~uv_attribute:"uv" () |> Result.get_ok in
              check (equal_geometry native (cook 1 typed)) "torus flat controls match native")
              (if polygon && not v_wrap then [false;true] else [false]))
              (if polygon && not u_wrap then [false;true] else [false])) [false;true]) [false;true])
        ["Explicit",Some normals;"Auto",None])
      [Rdk.Parametric_generators.Torus_no_normals,"None";Torus_point_normals,"Point";Torus_vertex_normals,"Vertex"])
    [Rdk.Parametric_generators.Torus_triangles,"Triangles",true;Torus_alternating_triangles,"Alternating triangles",true;
     Torus_quads,"Quads",true;Torus_rows,"Rows",false;Torus_columns,"Columns",false;
     Torus_rows_and_columns,"Rows and columns",false;Torus_points,"Points",false];
  let torus_axis = Vec3.create 1. 2. 3. and torus_center = Vec3.create 2. (-1.) 3.
  and torus_rotation = Vec3.create 0.2 0.3 0.4 in
  List.iter (fun (orientation_choice,native_orientation) ->
    List.iter (fun (rotation_order,rotation_choice) ->
      let typed = sop "torus"
          [ks "orientation" orientation_choice; kv "axis" torus_axis; ks "rotation_order" rotation_choice;
           kv "center" torus_center; kv "rotation" torus_rotation; kf "uniform_scale" 1.25; ki "rows" 4;
           ki "columns" 5; ks "uv_attribute" " "] in
      same_generator ("torus orientation " ^ orientation_choice ^ rotation_choice)
        ~typed ~factory:Nodes.Torus.factory ["orientation",Param.Choice_value orientation_choice;
          "axis_x",Float_value 1.;"axis_y",Float_value 2.;"axis_z",Float_value 3.;
          "rotation_order",Choice_value rotation_choice;"center_x",Float_value 2.;"center_y",Float_value (-1.);"center_z",Float_value 3.;
          "rotation_x",Float_value 0.2;"rotation_y",Float_value 0.3;"rotation_z",Float_value 0.4;
          "uniform_scale",Float_value 1.25;"rows",Int_value 4;"columns",Int_value 5;"uv_attribute",Text_value " "];
      let native = Rdk.Parametric_generators.torus ~orientation:native_orientation ~rotation_order ~center:torus_center ~rotation:torus_rotation
          ~uniform_scale:1.25 ~rows:4 ~columns:5 ~major_radius:1. ~minor_radius:0.25 () |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "torus flat orientation and unset UV match native")
      [Rdk.Parametric_generators.Torus_xyz,"XYZ";Torus_xzy,"XZY";Torus_yxz,"YXZ";Torus_yzx,"YZX";Torus_zxy,"ZXY";Torus_zyx,"ZYX"])
    ["X axis",Rdk.Parametric_generators.Torus_x;"Y axis",Rdk.Parametric_generators.Torus_y;"Z axis",Rdk.Parametric_generators.Torus_z;
     "Custom axis",Rdk.Parametric_generators.Torus_axis torus_axis];
  List.iter (fun make -> check (rejected make)
      "torus refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node {|(sop/torus :major_radius 0.0)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/torus :uniform_scale %s :major_radius 2.0)|} ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/torus :orientation "Custom axis" :axis %s)|} ((Lisp_sop.vec3 Vec3.zero))));
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/torus :u_start %s :u_end %s)|} ((Lisp_sop.float (-.Float.max_float))) ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node {|(sop/torus :u_start 1.0 :u_end 1.0)|});
     (fun () -> Lisp_sop.node {|(sop/torus :rows 2)|});
     (fun () -> Lisp_sop.node {|(sop/torus :u_wrap false :v_wrap false :columns 2 :u_end_caps true)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/torus :rows %d)|} (Sys.max_array_length)));
     (fun () -> Lisp_sop.node {|(sop/torus :uv_attribute "P")|});
     (fun () -> Lisp_sop.node {|(sop/torus :connectivity "Points" :normals "Vertex")|});
     (fun () -> Lisp_sop.node {|(sop/torus :connectivity "Rows" :u_wrap false :u_end_caps true)|});
     (fun () -> Lisp_sop.node {|(sop/torus :u_end_caps true)|});
     (fun () -> Lisp_sop.node {|(sop/torus :v_wrap false :v_end_cap true)|})];
  check (Result.is_error (Node.apply_parameters torus ["u_end_caps",Param.Bool_value true]))
    "torus refuses invalid cap inspector edit";

  let uv_sphere = Lisp_sop.node {|(sop/uv_sphere)|} in
  same_generator "UV sphere defaults" ~typed:uv_sphere ~factory:Nodes.Uv_sphere.factory [];
  cache_identity "UV sphere all fields" uv_sphere;
  List.iter (fun (connectivity,connectivity_choice) ->
    List.iter (fun (normals,normals_choice) ->
      List.iter (fun (normals_mode_choice,native_normals) ->
        if connectivity <> Rdk.Uv_sphere.Sphere_points || native_normals <> Some Rdk.Uv_sphere.Sphere_vertex_normals then
          List.iter (fun unique_points_per_pole ->
            List.iter (fun triangular_poles ->
              let typed = sop "uv_sphere"
                  [kv "radius" (Vec3.create 1.5 2. 0.75); ks "connectivity" connectivity_choice; ks "normals" normals_choice;
                   ks "normals_mode" normals_mode_choice; kb "unique_points_per_pole" unique_points_per_pole;
                   kb "triangular_poles" triangular_poles; ki "segments" 8; ki "rings" 4] in
              same_generator ("UV sphere " ^ connectivity_choice ^ normals_choice ^ normals_mode_choice)
                ~typed ~factory:Nodes.Uv_sphere.factory ["connectivity",Param.Choice_value connectivity_choice;
                  "normals",Choice_value normals_choice;"normals_mode",Choice_value normals_mode_choice;
                  "unique_points_per_pole",Bool_value unique_points_per_pole;"triangular_poles",Bool_value triangular_poles;
                  "segments",Int_value 8;"rings",Int_value 4;"radius_x",Float_value 1.5;"radius_y",Float_value 2.;"radius_z",Float_value 0.75];
              let native = Rdk.Uv_sphere.run ~connectivity ?normals:native_normals ~unique_points_per_pole ~triangular_poles
                  ~segments:8 ~rings:4 ~radius_x:1.5 ~radius_y:2. ~radius_z:0.75 ~uv_attribute:"uv" ~radius:1. () |> Result.get_ok in
              check (equal_geometry native (cook 1 typed)) "UV sphere flattened normals match native") [false;true]) [false;true])
        ["Explicit",Some normals;"Auto",None])
      [Rdk.Uv_sphere.Sphere_no_normals,"None";Sphere_point_normals,"Point";Sphere_vertex_normals,"Vertex"])
    [Rdk.Uv_sphere.Sphere_triangles,"Triangles";Sphere_alternating_triangles,"Alternating triangles";
     Sphere_quads,"Quads";Sphere_rows,"Rows";Sphere_columns,"Columns";Sphere_rows_and_columns,"Rows and columns";Sphere_points,"Points"];
  let sphere_axis = Vec3.create 1. 2. 3. and sphere_center = Vec3.create 2. (-1.) 3.
  and sphere_rotation = Vec3.create 0.2 0.3 0.4 in
  List.iter (fun (orientation_choice,native_orientation) ->
    List.iter (fun (rotation_order,rotation_choice) ->
      let typed = sop "uv_sphere"
          [ks "orientation" orientation_choice; kv "axis" sphere_axis; ks "rotation_order" rotation_choice;
           kv "center" sphere_center; kv "rotation" sphere_rotation; kf "uniform_scale" 1.25; ki "segments" 8; ki "rings" 4] in
      same_generator ("UV sphere orientation " ^ orientation_choice ^ rotation_choice)
        ~typed ~factory:Nodes.Uv_sphere.factory ["orientation",Param.Choice_value orientation_choice;
          "axis_x",Float_value 1.;"axis_y",Float_value 2.;"axis_z",Float_value 3.;
          "rotation_order",Choice_value rotation_choice;"center_x",Float_value 2.;"center_y",Float_value (-1.);"center_z",Float_value 3.;
          "rotation_x",Float_value 0.2;"rotation_y",Float_value 0.3;"rotation_z",Float_value 0.4;
          "uniform_scale",Float_value 1.25;"segments",Int_value 8;"rings",Int_value 4];
      let native = Rdk.Uv_sphere.run ~orientation:native_orientation ~rotation_order ~center:sphere_center ~rotation:sphere_rotation
          ~uniform_scale:1.25 ~segments:8 ~rings:4 ~uv_attribute:"uv" ~radius:1. () |> Result.get_ok in
      check (close_blend native (cook 1 typed)) "UV sphere flat orientation matches native")
      [Rdk.Uv_sphere.Sphere_xyz,"XYZ";Sphere_xzy,"XZY";Sphere_yxz,"YXZ";Sphere_yzx,"YZX";Sphere_zxy,"ZXY";Sphere_zyx,"ZYX"])
    ["X axis",Rdk.Uv_sphere.Sphere_x;"Y axis",Rdk.Uv_sphere.Sphere_y;"Z axis",Rdk.Uv_sphere.Sphere_z;
     "Custom axis",Rdk.Uv_sphere.Sphere_axis sphere_axis];
  List.iter (fun (x_choice,x) ->
    List.iter (fun (y_choice,y) ->
      List.iter (fun (z_choice,z) ->
        let typed = sop "uv_sphere"
            [kv "radius" (Vec3.create 1.5 2. 0.75); ks "radius_x_mode" x_choice; ks "radius_y_mode" y_choice;
             ks "radius_z_mode" z_choice; kf "base_radius" 3.; ki "segments" 8; ki "rings" 4; ks "uv_attribute" " "] in
        same_generator "UV sphere per-axis fallback" ~typed ~factory:Nodes.Uv_sphere.factory
          ["radius_x_mode",Param.Choice_value x_choice;"radius_y_mode",Choice_value y_choice;"radius_z_mode",Choice_value z_choice;
           "radius_x",Float_value 1.5;"radius_y",Float_value 2.;"radius_z",Float_value 0.75;"base_radius",Float_value 3.;
           "segments",Int_value 8;"rings",Int_value 4;"uv_attribute",Text_value " "];
        let native = Rdk.Uv_sphere.run ?radius_x:x ?radius_y:y ?radius_z:z ~radius:3. ~segments:8 ~rings:4 () |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "UV sphere fallback radii and unset UV match native")
        ["Explicit",Some 0.75;"Auto",None])
      ["Explicit",Some 2.;"Auto",None])
    ["Explicit",Some 1.5;"Auto",None];
  List.iter (fun make -> check (rejected make)
      "UV sphere refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node {|(sop/uv_sphere :base_radius 0.0)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/uv_sphere :radius [1.0 2.0 1.0] :uniform_scale %s)|} ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/uv_sphere :orientation "Custom axis" :axis %s)|} ((Lisp_sop.vec3 Vec3.zero))));
     (fun () -> Lisp_sop.node {|(sop/uv_sphere :segments 2)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/uv_sphere :rings %d)|} (max_int)));
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/uv_sphere :segments %d :rings 2)|} (Sys.max_array_length)));
     (fun () -> Lisp_sop.node {|(sop/uv_sphere :uv_attribute "N")|});
     (fun () -> Lisp_sop.node {|(sop/uv_sphere :connectivity "Points" :normals "Vertex")|})];
  check (Result.is_error (Node.apply_parameters uv_sphere ["radius_x",Param.Float_value nan]))
    "UV sphere refuses invalid inspector edit";

  let transfer_geometry = cook 1 (Lisp_sop.node {|(sop/box)|}) in
  let transfer_source = List.fold_left (fun geometry (owner,name,value) ->
      let count = match owner with Rdk.Attribute.Point -> Rdk.Geometry.point_count geometry
        | Vertex -> Rdk.Geometry.vertex_count geometry | Primitive -> Rdk.Geometry.primitive_count geometry | Detail -> 1 in
      Rdk.Attribute.create_owned ~owner ~name (Rdk.Attribute.Float (Array.make count value)) |> get
      |> fun attribute -> Rdk.Geometry.with_attribute attribute geometry |> get) transfer_geometry
      [Rdk.Attribute.Point,"point_value",2.;Vertex,"vertex_value",3.;Primitive,"primitive_value",4.;Detail,"detail_value",5.] in
  let transfer_source_input = Lisp_sop.snapshot (transfer_source) and transfer_target_input = Lisp_sop.snapshot (transfer_geometry) in
  let transferred_all = Lisp_sop.node ~with_:["transfer_source_input", (transfer_source_input); "transfer_target_input", (transfer_target_input)] {|(sop/attribute_transfer_all
   (sop/ext_transfer_source_input)
   (sop/ext_transfer_target_input))|} in
  same_cook ~inputs:[transfer_target_input] "transfer all defaults" ~typed:transferred_all
    ~factory:Nodes.Attribute_transfer_all.factory [] transfer_source_input;
  cache_identity "transfer all all fields" transferred_all;
  List.iter (fun (mode_choice,native_mode) ->
    List.iter (fun (falloff_choice,native_falloff) ->
      List.iter (fun (distance_choice,distance,blend_width) ->
        List.iter (fun (unmatched,unmatched_choice) ->
          let typed = sop ~inputs:[transfer_source_input; transfer_target_input] "attribute_transfer_all"
              [ks "point_pattern" "point_*"; ks "vertex_pattern" "vertex_*"; ks "primitive_pattern" "primitive_*";
               ks "detail_pattern" "detail_*"; ks "mode" mode_choice; ki "neighbors" 3; kf "power" 1.5;
               kf "kernel_radius" 2.; ks "distance_mode" distance_choice; kf "max_distance" 2.;
               kf "blend_width" blend_width; ks "falloff" falloff_choice; kf "uniform_bias" 0.7;
               ks "unmatched" unmatched_choice] in
          same_cook ~inputs:[transfer_target_input] ("transfer all " ^ mode_choice ^ falloff_choice ^ distance_choice ^ unmatched_choice)
            ~typed ~factory:Nodes.Attribute_transfer_all.factory
            ["point_pattern",Param.Text_value "point_*";"vertex_pattern",Text_value "vertex_*";
             "primitive_pattern",Text_value "primitive_*";"detail_pattern",Text_value "detail_*";
             "mode",Choice_value mode_choice;"neighbors",Int_value 3;"power",Float_value 1.5;
             "kernel_radius",Float_value 2.;"distance_mode",Choice_value distance_choice;"max_distance",Float_value 2.;
             "blend_width",Float_value blend_width;"falloff",Choice_value falloff_choice;
             "uniform_bias",Float_value 0.7;"unmatched",Choice_value unmatched_choice] transfer_source_input;
          let native = Rdk.Attribute_ops.transfer_all ~point_pattern:"point_*" ~vertex_pattern:"vertex_*"
              ~primitive_pattern:"primitive_*" ~detail_pattern:"detail_*" ~mode:native_mode ?max_distance:distance
              ~blend_width ~falloff:native_falloff ~unmatched ~source:transfer_source ~target:transfer_geometry () |> Result.get_ok in
          check (equal_geometry native (cook 1 typed)) "transfer all flattened controls match native")
        [Rdk.Attribute_ops.Keep_target,"Keep target";Default_value,"Default value"])
      ["Explicit",Some 2.,0.25;"Auto",None,0.])
    ["Linear",Rdk.Attribute_ops.Linear;"Smoothstep",Rdk.Attribute_ops.Smoothstep;
     "Uniform",Rdk.Attribute_ops.Uniform 0.7])
    ["Nearest",Rdk.Attribute_ops.Nearest;
     "Inverse distance",Rdk.Attribute_ops.Inverse_distance {neighbors=3;power=1.5};
     "Links kernel",Rdk.Attribute_ops.Kernel {neighbors=3;radius=2.;kernel=Links};
     "RenderMan kernel",Rdk.Attribute_ops.Kernel {neighbors=3;radius=2.;kernel=RenderMan};
     "Hart kernel",Rdk.Attribute_ops.Kernel {neighbors=3;radius=2.;kernel=Hart}];
  List.iter (fun pattern ->
    let values = List.map (fun name -> name,Param.Text_value (if name=pattern then "*" else " "))
        ["point_pattern";"vertex_pattern";"primitive_pattern";"detail_pattern"] in
    let owner_pattern name = if name = pattern then "*" else " " in
    let typed = Lisp_sop.node ~with_:["transfer_source_input", (transfer_source_input); "transfer_target_input", (transfer_target_input)] (Printf.sprintf {|(sop/attribute_transfer_all
   (sop/ext_transfer_source_input)
   (sop/ext_transfer_target_input)
   :point_pattern %S
   :vertex_pattern %S
   :primitive_pattern %S
   :detail_pattern %S)|} ((owner_pattern "point_pattern")) ((owner_pattern "vertex_pattern")) ((owner_pattern "primitive_pattern")) ((owner_pattern "detail_pattern"))) in
    let patterns = List.map (fun name -> if name=pattern then Some "*" else None)
        ["point_pattern";"vertex_pattern";"primitive_pattern";"detail_pattern"] in
    match patterns with
    | [point_pattern;vertex_pattern;primitive_pattern;detail_pattern] ->
        let native = Rdk.Attribute_ops.transfer_all ?point_pattern ?vertex_pattern ?primitive_pattern ?detail_pattern
            ~max_distance:1. ~falloff:Rdk.Attribute_ops.Linear ~source:transfer_source ~target:transfer_geometry () |> Result.get_ok in
        same_cook ~inputs:[transfer_target_input] ("transfer all owner " ^ pattern)
          ~typed ~factory:Nodes.Attribute_transfer_all.factory values transfer_source_input;
        check (equal_geometry native (cook 1 typed)) "transfer all blank owner patterns are unset"
    | _ -> assert false) ["point_pattern";"vertex_pattern";"primitive_pattern";"detail_pattern"];
  List.iter (fun make -> check (rejected make)
      "transfer all refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["transfer_source_input", (transfer_source_input); "transfer_target_input", (transfer_target_input)] {|(sop/attribute_transfer_all
   (sop/ext_transfer_source_input)
   (sop/ext_transfer_target_input)
   :point_pattern " ")|});
     (fun () -> Lisp_sop.node ~with_:["transfer_source_input", (transfer_source_input); "transfer_target_input", (transfer_target_input)] {|(sop/attribute_transfer_all
   (sop/ext_transfer_source_input)
   (sop/ext_transfer_target_input)
   :vertex_pattern "broken[")|});
     (fun () -> Lisp_sop.node ~with_:["transfer_source_input", (transfer_source_input); "transfer_target_input", (transfer_target_input)] {|(sop/attribute_transfer_all
   (sop/ext_transfer_source_input)
   (sop/ext_transfer_target_input)
   :neighbors 0)|});
     (fun () -> Lisp_sop.node ~with_:["transfer_source_input", (transfer_source_input); "transfer_target_input", (transfer_target_input)] {|(sop/attribute_transfer_all
   (sop/ext_transfer_source_input)
   (sop/ext_transfer_target_input)
   :mode "Inverse distance"
   :power 0.0)|});
     (fun () -> Lisp_sop.node ~with_:["transfer_source_input", (transfer_source_input); "transfer_target_input", (transfer_target_input)] (Printf.sprintf {|(sop/attribute_transfer_all
   (sop/ext_transfer_source_input)
   (sop/ext_transfer_target_input)
   :kernel_radius %s)|} ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node ~with_:["transfer_source_input", (transfer_source_input); "transfer_target_input", (transfer_target_input)] {|(sop/attribute_transfer_all
   (sop/ext_transfer_source_input)
   (sop/ext_transfer_target_input)
   :distance_mode "Auto"
   :blend_width 0.1)|});
     (fun () -> Lisp_sop.node ~with_:["transfer_source_input", (transfer_source_input); "transfer_target_input", (transfer_target_input)] {|(sop/attribute_transfer_all
   (sop/ext_transfer_source_input)
   (sop/ext_transfer_target_input)
   :uniform_bias 1.1)|});
     (fun () -> Lisp_sop.node ~with_:["transfer_source_input", (transfer_source_input); "transfer_target_input", (transfer_target_input)] (Printf.sprintf {|(sop/attribute_transfer_all
   (sop/ext_transfer_source_input)
   (sop/ext_transfer_target_input)
   :max_distance %s
   :blend_width %s)|} ((Lisp_sop.float (sqrt Float.max_float))) ((Lisp_sop.float (sqrt Float.max_float)))))];
  check (Result.is_error (Node.apply_parameters transferred_all ["power",Param.Float_value nan]))
    "transfer all refuses invalid inspector edit";

  let facet_geometry = cook 1 (Lisp_sop.node {|(sop/box)|}) in
  let facet_input = Lisp_sop.snapshot (facet_geometry) in
  let faceted = Lisp_sop.node ~with_:["facet_input", (facet_input)] {|(sop/facet (sop/ext_facet_input))|} in
  same_cook "facet defaults" ~typed:faceted ~factory:Nodes.Facet.factory [] facet_input;
  cache_identity "facet all fields" faceted;
  check (equal_geometry (Rdk.Facet.run ~cusp_angle:Float.pi facet_geometry |> Result.get_ok) (cook 1 faceted))
    "facet valid Lisp default matches native";
  List.iter (fun (owner_choice,geometry,selection) ->
    let input = Lisp_sop.snapshot (geometry) in
    List.iter (fun (consolidation_choice,point_distance,normal_distance) ->
      List.iter (fun (cusp_choice,cusp) ->
        List.iter (fun unique_points ->
          let typed = sop ~inputs:[input] "facet"
              [ks "group_owner" owner_choice; ks "group" "selection"; ks "consolidation" consolidation_choice;
               kf "consolidate_distance" 0.01; kf "consolidate_normals_distance" 0.02; ks "cusp_mode" cusp_choice;
               kf "cusp_angle" 0.8; kb "unique_points" unique_points; kb "pre_compute_normals" true;
               kb "make_normals_unit_length" true; kb "post_compute_normals" true; kb "reverse_normals" true;
               kb "orient_polygons" true; kb "remove_degenerate" true; kb "make_planar" true] in
          same_cook ("facet " ^ owner_choice ^ consolidation_choice ^ cusp_choice)
            ~typed ~factory:Nodes.Facet.factory ["group_owner",Param.Choice_value owner_choice;
              "group",Text_value "selection";"consolidation",Choice_value consolidation_choice;
              "consolidate_distance",Float_value 0.01;"consolidate_normals_distance",Float_value 0.02;
              "cusp_mode",Choice_value cusp_choice;"cusp_angle",Float_value 0.8;"unique_points",Bool_value unique_points;
              "pre_compute_normals",Bool_value true;"make_normals_unit_length",Bool_value true;
              "post_compute_normals",Bool_value true;"reverse_normals",Bool_value true;
              "orient_polygons",Bool_value true;"remove_degenerate",Bool_value true;"make_planar",Bool_value true] input;
          let native = Rdk.Facet.run ~selection ?consolidate_distance:point_distance
              ?consolidate_normals_distance:normal_distance ?cusp_angle:cusp ~unique_points
              ~pre_compute_normals:true ~make_normals_unit_length:true ~post_compute_normals:true
              ~reverse_normals:true ~orient_polygons:true ~remove_degenerate:true ~make_planar:true geometry |> Result.get_ok in
          check (equal_geometry native (cook 1 typed)) "facet flattened controls match native") [false;true])
        ["Explicit",Some 0.8;"Auto",None])
      ["None",None,None;"Points",Some 0.01,None;
       "Normals",None,Some 0.02])
    (let ordinary owner count = Rdk.Group.init ~owner ~name:"selection" count (fun i -> i mod 2 = 0) in
     let point = ordinary Rdk.Group.Point (Rdk.Geometry.point_count facet_geometry)
     and vertex = ordinary Rdk.Group.Vertex (Rdk.Geometry.vertex_count facet_geometry)
     and primitive = ordinary Rdk.Group.Primitive (Rdk.Geometry.primitive_count facet_geometry) in
     let topology = Rdk.Geometry.topology facet_geometry in
     let edge = Rdk.Edge_group.init ~topology ~index:(Rdk.Topology_index.create topology)
         ~name:"selection" (fun i -> i mod 2 = 0) in
     ["Point",Rdk.Geometry.with_group point facet_geometry |> get,Rdk.Transform_ops.Selected_points point;
      "Vertex",Rdk.Geometry.with_group vertex facet_geometry |> get,Selected_vertices vertex;
      "Primitive",Rdk.Geometry.with_group primitive facet_geometry |> get,Selected_primitives primitive;
      "Edge",Rdk.Geometry.with_edge_group edge facet_geometry |> get,Selected_edges edge]);
  same_cook "facet inline removal" ~typed:(Lisp_sop.node ~with_:["facet_input", (facet_input)] {|(sop/facet (sop/ext_facet_input) :remove_inline_points true :inline_distance 0.1)|})
    ~factory:Nodes.Facet.factory ["remove_inline_points",Param.Bool_value true;"inline_distance",Float_value 0.1] facet_input;
  check (equal_geometry (Rdk.Facet.run ~remove_inline_points:true ~inline_distance:0.1 ~cusp_angle:Float.pi facet_geometry
      |> Result.get_ok) (cook 1 (Lisp_sop.node ~with_:["facet_input", (facet_input)] {|(sop/facet (sop/ext_facet_input) :remove_inline_points true :inline_distance 0.1)|})))
    "facet inline control matches native";
  same_cook "facet unset group" ~typed:(Lisp_sop.node ~with_:["facet_input", (facet_input)] {|(sop/facet (sop/ext_facet_input) :group " ")|})
    ~factory:Nodes.Facet.factory ["group",Param.Text_value " "] facet_input;
  List.iter (fun make -> check (rejected make)
      "facet refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["facet_input", (facet_input)] {|(sop/facet (sop/ext_facet_input) :consolidate_normals_distance -1.0)|});
     (fun () -> Lisp_sop.node ~with_:["facet_input", (facet_input)] (Printf.sprintf {|(sop/facet (sop/ext_facet_input) :cusp_mode "Auto" :cusp_angle %s)|} ((Lisp_sop.float (Float.pi +. 1.)))))];
  check (Result.is_error (Node.apply_parameters faceted ["consolidate_distance",Param.Float_value nan]))
    "facet refuses invalid inspector edit";

  let blast_geometry = cook 1 (Lisp_sop.node {|(sop/box)|}) in
  let blast_source attribute_owner group_owner =
    let count = match attribute_owner with Rdk.Attribute.Point -> Rdk.Geometry.point_count blast_geometry
      | Primitive -> Rdk.Geometry.primitive_count blast_geometry | _ -> assert false in
    let storage = match attribute_owner with
      | Rdk.Attribute.Point -> Rdk.Attribute.Float (Array.init count (fun i -> float_of_int (i mod 3) -. 1.))
      | _ -> Rdk.Attribute.Int (Array.init count (fun i -> i mod 3 - 1)) in
    let attribute = Rdk.Attribute.create_owned ~owner:attribute_owner ~name:"mask" storage |> get in
    let base = Rdk.Group.init ~owner:group_owner ~name:"base" count (fun i -> i < count - 1) in
    Rdk.Geometry.with_attribute attribute blast_geometry |> get |> Rdk.Geometry.with_group base |> get in
  let blast_input = Lisp_sop.snapshot ((blast_source Rdk.Attribute.Point Rdk.Group.Point)) in
  let attribute_blast = Lisp_sop.node ~with_:["blast_input", (blast_input)] {|(sop/blast_by_attribute (sop/ext_blast_input))|} in
  same_cook "blast by attribute defaults" ~typed:attribute_blast ~factory:Nodes.Blast_by_attribute.factory [] blast_input;
  cache_identity ~companions:["remove_unused_points",["owner",Param.Choice_value "Primitives"]]
    "blast by attribute all fields" attribute_blast;
  List.iter (fun (owner,attribute_owner,group_owner,owner_choice) ->
    let geometry = blast_source attribute_owner group_owner in
    let input = Lisp_sop.snapshot (geometry) in
    List.iter (fun (mode_choice,native_mode) ->
      List.iter (fun (output_choice,native_output) ->
        List.iter (fun invert -> List.iter (fun remove_unused_points ->
          let typed = sop ~inputs:[input] "blast_by_attribute"
              [ks "owner" owner_choice; ks "mode" mode_choice; kf "threshold" 0.25; kf "minimum" (-0.5);
               kf "maximum" 0.5; kf "center" 0.; kf "width" 1.; ks "group" "base"; kb "invert" invert;
               ks "output" output_choice; ks "output_group" "picked"; kb "remove_unused_points" remove_unused_points] in
          same_cook ("blast attribute " ^ owner_choice ^ mode_choice ^ output_choice)
            ~typed ~factory:Nodes.Blast_by_attribute.factory ["owner",Param.Choice_value owner_choice;
              "mode",Choice_value mode_choice;"threshold",Float_value 0.25;"minimum",Float_value (-0.5);
              "maximum",Float_value 0.5;"center",Float_value 0.;"width",Float_value 1.;
              "group",Text_value "base";"invert",Bool_value invert;"output",Choice_value output_choice;
              "output_group",Text_value "picked";"remove_unused_points",Bool_value remove_unused_points] input;
          let native = Rdk.Blast_by_attribute.blast ~owner ~attribute:"mask" ~mode:native_mode ~output:native_output
              ~base:(Rdk.Geometry.find_group ~owner:group_owner "base" geometry |> Option.get)
              ~invert ~remove_unused_points geometry |> Result.get_ok in
          check (equal_geometry native (cook 1 typed)) "blast attribute flattened fields match native")
          (if owner = Rdk.Blast_by_attribute.Blast_primitives && output_choice = "Delete elements" then [false;true] else [false])) [false;true])
        ["Delete elements",Rdk.Blast_by_attribute.Blast_delete;
         "Create group",Rdk.Blast_by_attribute.Blast_group "picked"])
      ["Below threshold",Rdk.Blast_by_attribute.Blast_below 0.25;
       "Range",Rdk.Blast_by_attribute.Blast_range {minimum=(-0.5);maximum=0.5};
       "Center and width",Rdk.Blast_by_attribute.Blast_width {center=0.;width=1.}])
    [Rdk.Blast_by_attribute.Blast_points,Rdk.Attribute.Point,Rdk.Group.Point,"Points";
     Blast_primitives,Primitive,Primitive,"Primitives"];
  same_cook "blast attribute unset base and inactive output name"
    ~typed:(Lisp_sop.node ~with_:["blast_input", (blast_input)] {|(sop/blast_by_attribute (sop/ext_blast_input) :group " " :output_group " ")|})
    ~factory:Nodes.Blast_by_attribute.factory ["group",Param.Text_value " ";"output_group",Text_value " "] blast_input;
  List.iter (fun make -> check (rejected make)
      "blast attribute refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["blast_input", (blast_input)] {|(sop/blast_by_attribute (sop/ext_blast_input) :attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["blast_input", (blast_input)] {|(sop/blast_by_attribute (sop/ext_blast_input) :remove_unused_points true)|});
     (fun () -> Lisp_sop.node ~with_:["blast_input", (blast_input)] {|(sop/blast_by_attribute
   (sop/ext_blast_input)
   :owner "Primitives"
   :output "Create group"
   :remove_unused_points true)|});
     (fun () -> Lisp_sop.node ~with_:["blast_input", (blast_input)] {|(sop/blast_by_attribute (sop/ext_blast_input) :output "Create group" :output_group " ")|});
     (fun () -> Lisp_sop.node ~with_:["blast_input", (blast_input)] {|(sop/blast_by_attribute (sop/ext_blast_input) :mode "Range" :minimum 2.0)|});
     (fun () -> Lisp_sop.node ~with_:["blast_input", (blast_input)] {|(sop/blast_by_attribute (sop/ext_blast_input) :width -1.0)|});
     (fun () -> Lisp_sop.node ~with_:["blast_input", (blast_input)] (Printf.sprintf {|(sop/blast_by_attribute
   (sop/ext_blast_input)
   :mode "Center and width"
   :center %s
   :width %s)|} ((Lisp_sop.float Float.max_float)) ((Lisp_sop.float Float.max_float))))];
  check (Result.is_error (Node.apply_parameters attribute_blast ["threshold",Param.Float_value nan]))
    "blast attribute refuses non-finite inspector edit";
  let gridded = Lisp_sop.node {|(sop/grid)|} in
  same_generator "grid defaults" ~typed:gridded ~factory:Nodes.Grid.factory [];
  cache_identity "grid all fields" gridded;
  List.iter (fun (counts, count_choice) ->
    List.iter (fun (connectivity, connectivity_choice) ->
      List.iter (fun (plane_choice, native_orientation) ->
        List.iter (fun (width_auto, width_choice) -> List.iter (fun (height_auto, height_choice) ->
          let typed = sop "grid"
              [ks "counts" count_choice; ks "connectivity" connectivity_choice; ks "orientation" plane_choice;
               kv "horizontal" (Vec3.create 1. 2. 0.5); kv "vertical" (Vec3.create (-0.25) 0.75 2.);
               ks "width_mode" width_choice; ks "height_mode" height_choice; kf "width" 6.; kf "height" 2.;
               kf "size" 3.; ki "columns" 4; ki "rows" 3; kv "center" (Vec3.create 2. 3. 4.); kf "rotation" 0.25;
               ks "uv_attribute" "st"] in
          same_generator ("grid " ^ count_choice ^ connectivity_choice ^ plane_choice ^ width_choice ^ height_choice)
            ~typed ~factory:Nodes.Grid.factory ["counts", Param.Choice_value count_choice;
              "connectivity", Choice_value connectivity_choice; "orientation", Choice_value plane_choice;
              "horizontal_x", Float_value 1.; "horizontal_y", Float_value 2.; "horizontal_z", Float_value 0.5;
              "vertical_x", Float_value (-0.25); "vertical_y", Float_value 0.75; "vertical_z", Float_value 2.;
              "width_mode", Choice_value width_choice; "height_mode", Choice_value height_choice;
              "width", Float_value 6.; "height", Float_value 2.; "size", Float_value 3.; "columns", Int_value 4; "rows", Int_value 3;
              "center_x", Float_value 2.; "center_y", Float_value 3.; "center_z", Float_value 4.;
              "rotation", Float_value 0.25; "uv_attribute", Text_value "st"];
          let native = Rdk.Plane_generators.grid ~counts ~connectivity ~orientation:native_orientation
              ?width:(if width_auto then None else Some 6.)
              ?height:(if height_auto then None else Some 2.) ~size:3. ~columns:4 ~rows:3
              ~center:(Vec3.create 2. 3. 4.) ~rotation:0.25 ~uv_attribute:"st" () |> Result.get_ok in
          check (equal_geometry native (cook 1 typed)) "grid flattened fields and Auto dimensions match native")
          [false,"Explicit"; true,"Auto"])
        [false,"Explicit"; true,"Auto"])
      ["XY",Rdk.Plane_generators.Grid_xy; "XZ",Rdk.Plane_generators.Grid_xz; "YZ",Rdk.Plane_generators.Grid_yz;
       "Custom axes",Rdk.Plane_generators.Grid_axes {horizontal=Vec3.create 1. 2. 0.5;vertical=Vec3.create (-0.25) 0.75 2.}])
    [Rdk.Plane_generators.Grid_points,"Points";Grid_rows,"Rows";Grid_columns,"Columns";
     Grid_rows_and_columns,"Rows and columns";Grid_quads,"Quads";Grid_triangles,"Triangles";
     Grid_alternating_triangles,"Alternating triangles";Grid_reverse_triangles,"Reverse triangles"])
    [Rdk.Plane_generators.Grid_divisions,"Divisions";Grid_point_counts,"Point counts"];
  same_generator "grid singleton point counts and unset UV"
    ~typed:(Lisp_sop.node {|(sop/grid
   :counts "Point counts"
   :connectivity "Points"
   :columns 1
   :rows 1
   :uv_attribute " ")|}) ~factory:Nodes.Grid.factory
    ["counts",Param.Choice_value "Point counts";"connectivity",Choice_value "Points";
     "columns",Int_value 1;"rows",Int_value 1;"uv_attribute",Text_value " "];
  List.iter (fun make -> check (rejected make)
      "grid refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node {|(sop/grid :columns 0)|}); (fun () -> Lisp_sop.node {|(sop/grid :rows 0)|});
     (fun () -> Lisp_sop.node {|(sop/grid :counts "Point counts" :columns 1)|});
     (fun () -> Lisp_sop.node {|(sop/grid :counts "Point counts" :connectivity "Rows" :columns 1)|});
     (fun () -> Lisp_sop.node {|(sop/grid :counts "Point counts" :connectivity "Columns" :rows 1)|});
     (fun () -> Lisp_sop.node {|(sop/grid :size 0.0)|}); (fun () -> Lisp_sop.node {|(sop/grid :width 0.0)|}); (fun () -> Lisp_sop.node {|(sop/grid :height 0.0)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/grid :orientation "Custom axes" :horizontal %s :vertical %s)|} ((Lisp_sop.vec3 Vec3.unit_x)) ((Lisp_sop.vec3 Vec3.unit_x))));
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/grid :columns %d)|} (max_int)));
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/grid :columns %d)|} (Sys.max_array_length)));
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/grid :counts "Point counts" :columns %d :rows 2)|} (Sys.max_array_length)));
     (fun () -> Lisp_sop.node {|(sop/grid :uv_attribute "P")|}); (fun () -> Lisp_sop.node {|(sop/grid :uv_attribute "N")|})];
  check (Result.is_error (Node.apply_parameters gridded ["size",Param.Float_value 0.]))
    "grid refuses zero size inspector edit";
  let circled = Lisp_sop.node {|(sop/circle)|} in
  same_generator "circle defaults" ~typed:circled ~factory:Nodes.Circle.factory [];
  cache_identity "circle all fields" circled;
  List.iter (fun (arc_choice, native_arc) ->
    List.iter (fun (orientation_choice, native_orientation) ->
      List.iter (fun (x_auto, x_choice) -> List.iter (fun (y_auto, y_choice) ->
        List.iter (fun reverse ->
          let typed = sop "circle"
              [ks "arc" arc_choice; kf "start_angle" (-0.4); kf "end_angle" 2.2; ks "orientation" orientation_choice;
               kv "horizontal" (Vec3.create 1. 2. 0.5); kv "vertical" (Vec3.create (-0.25) 0.75 2.);
               kb "reverse" reverse; kv "center" (Vec3.create 2. 3. 4.); kf "radius" 2.;
               ks "radius_x_mode" x_choice; kf "radius_x" 3.; ks "radius_y_mode" y_choice; kf "radius_y" 1.5;
               kf "rotation" 0.25; kf "uniform_scale" 1.2; ki "segments" 12] in
          same_generator ("circle " ^ arc_choice ^ orientation_choice ^ x_choice ^ y_choice)
            ~typed ~factory:Nodes.Circle.factory ["arc", Param.Choice_value arc_choice;
              "start_angle", Float_value (-0.4); "end_angle", Float_value 2.2;
              "orientation", Choice_value orientation_choice; "horizontal_x", Float_value 1.;
              "horizontal_y", Float_value 2.; "horizontal_z", Float_value 0.5;
              "vertical_x", Float_value (-0.25); "vertical_y", Float_value 0.75; "vertical_z", Float_value 2.;
              "reverse", Bool_value reverse; "center_x", Float_value 2.; "center_y", Float_value 3.; "center_z", Float_value 4.;
              "radius", Float_value 2.; "radius_x_mode", Choice_value x_choice; "radius_x", Float_value 3.;
              "radius_y_mode", Choice_value y_choice; "radius_y", Float_value 1.5;
              "rotation", Float_value 0.25; "uniform_scale", Float_value 1.2; "segments", Int_value 12];
          let native = Rdk.Plane_generators.circle ~arc:native_arc ~orientation:native_orientation
              ~reverse ~center:(Vec3.create 2. 3. 4.) ~radius:2.
              ?radius_x:(if x_auto then None else Some 3.)
              ?radius_y:(if y_auto then None else Some 1.5)
              ~rotation:0.25 ~uniform_scale:1.2 ~segments:12 () |> Result.get_ok in
          check (equal_geometry native (cook 1 typed)) "circle flattened fields and Auto radii match native") [false;true])
        [false,"Explicit"; true,"Auto"])
      [false,"Explicit"; true,"Auto"])
    ["XY",Rdk.Plane_generators.Circle_xy; "XZ",Rdk.Plane_generators.Circle_xz;
     "YZ",Rdk.Plane_generators.Circle_yz; "Custom axes",Rdk.Plane_generators.Circle_axes {
       horizontal=Vec3.create 1. 2. 0.5; vertical=Vec3.create (-0.25) 0.75 2.}])
    ["Closed",Rdk.Plane_generators.Circle_closed;
     "Open arc",Rdk.Plane_generators.Circle_open_arc {start_angle=(-0.4);end_angle=2.2};
     "Chord closed",Rdk.Plane_generators.Circle_closed_arc {start_angle=(-0.4);end_angle=2.2};
     "Sliced",Rdk.Plane_generators.Circle_sliced_arc {start_angle=(-0.4);end_angle=2.2}];
  List.iter (fun make -> check (rejected make)
      "circle refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node {|(sop/circle :radius 0.0)|}); (fun () -> Lisp_sop.node {|(sop/circle :radius_x 0.0)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/circle :radius_x %s :uniform_scale 2.0)|} ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/circle :radius_x %s :uniform_scale %s)|} ((Lisp_sop.float Float.min_float)) ((Lisp_sop.float Float.min_float))));
     (fun () -> Lisp_sop.node {|(sop/circle :radius_y 0.0)|}); (fun () -> Lisp_sop.node {|(sop/circle :uniform_scale 0.0)|});
     (fun () -> Lisp_sop.node {|(sop/circle :segments 2)|}); (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/circle :segments %d)|} (max_int)));
     (fun () -> Lisp_sop.node {|(sop/circle :arc "Sliced" :start_angle 1.0 :end_angle 1.0)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/circle :arc "Open arc" :start_angle %s :end_angle %s)|} ((Lisp_sop.float (-.Float.max_float))) ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/circle :orientation "Custom axes" :horizontal %s)|} ((Lisp_sop.vec3 Vec3.zero))));
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/circle :orientation "Custom axes" :horizontal %s :vertical %s)|} ((Lisp_sop.vec3 Vec3.unit_x)) ((Lisp_sop.vec3 Vec3.unit_x))))];
  check (Result.is_error (Node.apply_parameters circled ["radius_x", Param.Float_value 0.]))
    "circle refuses zero-radius inspector edit";
  let spiraled = Lisp_sop.node {|(sop/spiral)|} in
  same_generator "spiral defaults" ~typed:spiraled ~factory:Nodes.Spiral.factory [];
  cache_identity ~changes:["height_ramp", Param.Text_value "0:1,1:0.8";
      "radius_ramp", Text_value "0:1,1:1.2"] "spiral all fields" spiraled;
  List.iter (fun (extent_choice, extent) ->
    List.iter (fun (radius_choice, radius) ->
      List.iter (fun (divisions_choice, divisions) ->
        List.iter (fun uniform_angle -> List.iter (fun (direction, direction_choice) ->
          let typed = sop "spiral"
              [ks "extent_mode" extent_choice; kf "turns" 2.; kf "height" (-2.); kf "pitch" (-1.);
               ks "radius_mode" radius_choice; kf "start_radius" 0.5; kf "end_radius" 2.; kf "radius_change" 0.2;
               kf "logarithmic_scale" 1.2; ks "height_ramp" "0:0.8,0.5:1.2,1:1"; ks "radius_ramp" "0:1,0.4:0.7,1:1.1";
               kf "radius_scale" 1.3; ks "direction" direction_choice; kf "start_angle" 0.3;
               ks "divisions_mode" divisions_choice; ki "divisions" 12; kb "uniform_angle" uniform_angle;
               ki "spiral_count" 2; ks "orientation" "Custom axis"; kv "axis" (Vec3.create 1. 2. 3.);
               kv "center" (Vec3.create 2. 3. 4.); kv "rotation" (Vec3.create 0.2 0.3 0.4);
               ks "rotation_order" "ZXY"; kf "uniform_scale" 1.2; ks "angle_attribute" "angle";
               ks "x_axis_attribute" "xaxis"; ks "y_axis_attribute" "yaxis"; ks "tangent_attribute" "tangent";
               ks "orient_attribute" "orient"; ks "distance_attribute" "distance"] in
          same_generator ("spiral " ^ extent_choice ^ radius_choice ^ divisions_choice ^ direction_choice)
            ~typed ~factory:Nodes.Spiral.factory ["extent_mode", Param.Choice_value extent_choice;
              "turns", Float_value 2.; "height", Float_value (-2.); "pitch", Float_value (-1.);
              "radius_mode", Choice_value radius_choice; "start_radius", Float_value 0.5;
              "end_radius", Float_value 2.; "radius_change", Float_value 0.2; "logarithmic_scale", Float_value 1.2;
              "height_ramp", Text_value "0:0.8,0.5:1.2,1:1"; "radius_ramp", Text_value "0:1,0.4:0.7,1:1.1";
              "radius_scale", Float_value 1.3; "direction", Choice_value direction_choice;
              "start_angle", Float_value 0.3; "divisions_mode", Choice_value divisions_choice; "divisions", Int_value 12;
              "uniform_angle", Bool_value uniform_angle; "spiral_count", Int_value 2;
              "orientation", Choice_value "Custom axis"; "axis_x", Float_value 1.; "axis_y", Float_value 2.; "axis_z", Float_value 3.;
              "center_x", Float_value 2.; "center_y", Float_value 3.; "center_z", Float_value 4.;
              "rotation_x", Float_value 0.2; "rotation_y", Float_value 0.3; "rotation_z", Float_value 0.4;
              "rotation_order", Choice_value "ZXY"; "uniform_scale", Float_value 1.2;
              "angle_attribute", Text_value "angle"; "x_axis_attribute", Text_value "xaxis"; "y_axis_attribute", Text_value "yaxis";
              "tangent_attribute", Text_value "tangent"; "orient_attribute", Text_value "orient"; "distance_attribute", Text_value "distance"];
          let native = Rdk.Spiral.run ~extent ~radius ~height_ramp:[0.,0.8;0.5,1.2;1.,1.]
              ~radius_ramp:[0.,1.;0.4,0.7;1.,1.1] ~radius_scale:1.3 ~direction ~start_angle:0.3
              ~divisions ~uniform_angle ~spiral_count:2 ~orientation:(Rdk.Spiral.Spiral_axis (Vec3.create 1. 2. 3.))
              ~center:(Vec3.create 2. 3. 4.) ~rotation:(Vec3.create 0.2 0.3 0.4)
              ~rotation_order:Rdk.Spiral.Spiral_zxy ~uniform_scale:1.2 ~angle_attribute:"angle"
              ~x_axis_attribute:"xaxis" ~y_axis_attribute:"yaxis" ~tangent_attribute:"tangent"
              ~orient_attribute:"orient" ~distance_attribute:"distance" () |> Result.get_ok in
          check (equal_geometry native (cook 1 typed)) "spiral flattened fields match native")
          [Rdk.Spiral.Spiral_counterclockwise, "Counterclockwise"; Spiral_clockwise, "Clockwise"])
          [false; true])
        ["Per curve", Rdk.Spiral.Spiral_divisions_per_curve 12;
         "Per turn", Rdk.Spiral.Spiral_divisions_per_turn 12])
      ["Archimedean change", Rdk.Spiral.Spiral_archimedean_change {start_radius=0.5; increase_per_turn=0.2};
       "Archimedean end", Rdk.Spiral.Spiral_archimedean_end {start_radius=0.5; end_radius=2.};
       "Logarithmic change", Rdk.Spiral.Spiral_logarithmic_change {start_radius=0.5; scale_per_turn=1.2};
       "Logarithmic end", Rdk.Spiral.Spiral_logarithmic_end {start_radius=0.5; end_radius=2.}])
    ["Turns and height", Rdk.Spiral.Spiral_turns {turns=2.; height=(-2.)};
     "Height and pitch", Rdk.Spiral.Spiral_height_pitch {height=(-2.); pitch=(-1.)}];
  List.iter (fun choice -> List.iter (fun order_choice ->
    same_generator ("spiral orientation " ^ choice ^ order_choice)
      ~typed:(sop "spiral" [ks "orientation" choice; ks "rotation_order" order_choice; kv "rotation" (Vec3.create 0.2 0.3 0.4)])
      ~factory:Nodes.Spiral.factory ["orientation", Param.Choice_value choice;
        "rotation_order", Choice_value order_choice; "rotation_x", Float_value 0.2;
        "rotation_y", Float_value 0.3; "rotation_z", Float_value 0.4])
    ["XYZ"; "XZY"; "YXZ"; "YZX"; "ZXY"; "ZYX"])
    ["X axis"; "Y axis"; "Z axis"; "Custom axis"];
  let constant_ramps = Lisp_sop.node {|(sop/spiral :height_ramp "0.4:0.8" :radius_ramp "0.2:1.2")|} in
  same_generator "spiral constant ramps" ~typed:constant_ramps ~factory:Nodes.Spiral.factory
    ["height_ramp", Param.Text_value "0.4:0.8"; "radius_ramp", Text_value "0.2:1.2"];
  check (equal_geometry (cook 1 constant_ramps)
      (Rdk.Spiral.run ~height_ramp:[0.4,0.8] ~radius_ramp:[0.2,1.2] () |> Result.get_ok))
    "spiral single-knot ramps retain native constant behavior";
  same_generator "spiral unset names and ramps" ~typed:(Lisp_sop.node {|(sop/spiral
   :height_ramp " "
   :radius_ramp " "
   :angle_attribute " "
   :x_axis_attribute " "
   :y_axis_attribute " "
   :tangent_attribute " "
   :orient_attribute " "
   :distance_attribute " ")|}) ~factory:Nodes.Spiral.factory
    (List.map (fun name -> name, Param.Text_value " ") ["height_ramp";"radius_ramp";
      "angle_attribute";"x_axis_attribute";"y_axis_attribute";"tangent_attribute";"orient_attribute";"distance_attribute"]);
  List.iter (fun make -> check (rejected make)
      "spiral refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node {|(sop/spiral :turns 0.0)|});
     (fun () -> Lisp_sop.node {|(sop/spiral :extent_mode "Height and pitch" :pitch 0.0)|});
     (fun () -> Lisp_sop.node {|(sop/spiral :extent_mode "Height and pitch" :pitch -1.0)|});
     (fun () -> Lisp_sop.node {|(sop/spiral :radius_change -1.0)|});
     (fun () -> Lisp_sop.node {|(sop/spiral :radius_mode "Logarithmic change" :logarithmic_scale 0.0)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/spiral :radius_mode "Logarithmic change" :logarithmic_scale %s)|} ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node {|(sop/spiral :radius_mode "Logarithmic end" :end_radius 0.0)|});
     (fun () -> Lisp_sop.node {|(sop/spiral :radius_scale 0.0)|}); (fun () -> Lisp_sop.node {|(sop/spiral :uniform_scale 0.0)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/spiral :orientation "Custom axis" :axis %s)|} ((Lisp_sop.vec3 Vec3.zero))));
     (fun () -> Lisp_sop.node {|(sop/spiral :divisions 0)|});
     (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/spiral :divisions %d)|} (max_int))); (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/spiral :spiral_count %d)|} (max_int)));
     (fun () -> Lisp_sop.node {|(sop/spiral :height_ramp "0:1,0.5:1,0.5:2,1:1")|});
     (fun () -> Lisp_sop.node {|(sop/spiral :radius_ramp "0:1,0.8:1")|});
     (fun () -> Lisp_sop.node {|(sop/spiral :radius_ramp "0.2:nan")|});
     (fun () -> Lisp_sop.node {|(sop/spiral :angle_attribute "P")|});
     (fun () -> Lisp_sop.node {|(sop/spiral :angle_attribute "same" :orient_attribute "same")|})];
  check (Result.is_error (Node.apply_parameters spiraled ["turns", Param.Float_value 0.]))
    "spiral rejects zero-turn inspector edit";
  let velocity_sample offset = Lisp_sop.node ~with_:["in517", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(offset, 0., 0.); (offset +. 1., 2., 3.); (offset +. 2., 4., 6.)|]))] {|(-> (sop/set_vector (sop/ext_in517) :value [1.0 2.0 3.0])
     (sop/set_vector :name "incoming" :value [2.0 3.0 4.0])
     (sop/enumerate))|}
      |> group_indices "Points" "selected" [|0; 2|] in
  let current_velocity = velocity_sample 1. and previous_velocity = velocity_sample 0.
  and next_velocity = velocity_sample 3. in
  let velocity = Lisp_sop.node ~with_:["current_velocity", (current_velocity); "previous_velocity", (previous_velocity)] {|(sop/point_velocity (sop/ext_current_velocity) (sop/ext_previous_velocity))|} in
  same_cook ~optional_inputs:[Some current_velocity; Some previous_velocity; None]
    "point velocity defaults" ~typed:velocity ~factory:Nodes.Point_velocity.factory [] current_velocity;
  cache_identity ~companions:["compute_acceleration", ["approximation", Param.Choice_value "Central difference"]]
    "point velocity all fields" velocity;
  List.iter (fun (approximation, choice) ->
    List.iter (fun (unmatched, unmatched_choice) -> List.iter (fun acceleration ->
      let typed = point_velocity current_velocity (Some previous_velocity) (Some next_velocity)
          ~args:[ks "approximation" choice; kf "dt" 0.5; ks "match_attribute" "id"; ks "unmatched" unmatched_choice;
                 ks "group" "selected"; kv "add" (Vec3.create 1. 2. 3.); kb "compute_acceleration" acceleration;
                 ks "velocity_attribute" "computed"; ks "acceleration_attribute" "computed_accel"] in
      same_cook ~optional_inputs:[Some current_velocity; Some previous_velocity; Some next_velocity]
        ("velocity deformation " ^ choice ^ unmatched_choice) ~typed ~factory:Nodes.Point_velocity.factory
        ["approximation", Param.Choice_value choice; "dt", Float_value 0.5;
         "match_attribute", Text_value "id"; "unmatched", Choice_value unmatched_choice;
         "group", Text_value "selected"; "add_x", Float_value 1.; "add_y", Float_value 2.; "add_z", Float_value 3.;
         "compute_acceleration", Bool_value acceleration; "velocity_attribute", Text_value "computed";
         "acceleration_attribute", Text_value "computed_accel"] current_velocity;
      let geometry = cook 1 current_velocity in
      let native = Rdk.Motion.point_velocity ~approximation ~dt:0.5 ~match_attribute:"id" ~unmatched
          ~points:(Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected" geometry |> Option.get)
          ~add_velocity:(Vec3.create 1. 2. 3.) ~compute_acceleration:acceleration
          ~velocity_attribute:"computed" ~acceleration_attribute:"computed_accel"
          ~previous:(cook 1 previous_velocity) ~next:(cook 1 next_velocity) geometry |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "velocity deformation matches native")
      (if approximation = Rdk.Motion.Central_difference then [false; true] else [false]))
      [Rdk.Motion.Velocity_unmatched_error, "Error"; Velocity_unmatched_zero, "Zero"])
    [Rdk.Motion.Backward_difference, "Backward difference"; Central_difference, "Central difference";
     Forward_difference, "Forward difference"];
  List.iter (fun (choice, native_initialization) ->
    List.iter (fun previous -> List.iter (fun next ->
      let typed = point_velocity current_velocity previous next
          ~args:[ks "initialization" choice; kv "set" (Vec3.create 4. 5. 6.); ks "source_attribute" "incoming";
                 kf "source_scale" 0.5; kv "add" Vec3.unit_y] in
      same_cook ~optional_inputs:[Some current_velocity; previous; next]
        ("velocity initialization " ^ choice) ~typed ~factory:Nodes.Point_velocity.factory
        ["initialization", Param.Choice_value choice; "set_x", Float_value 4.; "set_y", Float_value 5.;
         "set_z", Float_value 6.; "source_attribute", Text_value "incoming"; "source_scale", Float_value 0.5;
         "add_y", Float_value 1.] current_velocity;
      let native = Rdk.Motion.point_velocity ~initialization:native_initialization
          ~add_velocity:Vec3.unit_y ?previous:(Option.map (cook 1) previous)
          ?next:(Option.map (cook 1) next) (cook 1 current_velocity) |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "velocity initialization matches native")
      [None; Some next_velocity]) [None; Some previous_velocity])
    ["Keep incoming", Rdk.Motion.Keep_incoming;
     "Set value", Rdk.Motion.Set_value (Vec3.create 4. 5. 6.);
     "From attribute", Rdk.Motion.From_attribute {name="incoming"; scale=0.5}];
  same_cook ~optional_inputs:[Some current_velocity; None; Some next_velocity]
    "velocity forward-only unset selection/match" ~typed:(point_velocity current_velocity None (Some next_velocity)
      ~args:[ks "approximation" "Forward difference"; ks "group" " "; ks "match_attribute" " "])
    ~factory:Nodes.Point_velocity.factory ["approximation", Param.Choice_value "Forward difference";
      "group", Text_value " "; "match_attribute", Text_value " "] current_velocity;
  let partial_previous = Lisp_sop.node ~with_:["in525", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0., 0., 0.)|]))] {|(sop/enumerate (sop/ext_in525))|} in
  let zero_unmatched = Lisp_sop.node ~with_:["current_velocity", (current_velocity); "partial_previous", (partial_previous)] {|(sop/point_velocity
   (sop/ext_current_velocity)
   (sop/ext_partial_previous)
   :match_attribute "id"
   :unmatched "Zero")|} in
  same_cook ~optional_inputs:[Some current_velocity; Some partial_previous; None] "velocity unmatched zero"
    ~typed:zero_unmatched ~factory:Nodes.Point_velocity.factory
    ["match_attribute", Param.Text_value "id"; "unmatched", Choice_value "Zero"] current_velocity;
  let native_zero = Rdk.Motion.point_velocity ~match_attribute:"id" ~unmatched:Rdk.Motion.Velocity_unmatched_zero
      ~previous:(cook 1 partial_previous) (cook 1 current_velocity) |> Result.get_ok in
  check (equal_geometry native_zero (cook 1 zero_unmatched)) "velocity unmatched policy matches native";
  let unmatched_error = Lisp_sop.node ~with_:["current_velocity", (current_velocity); "partial_previous", (partial_previous)] {|(sop/point_velocity
   (sop/ext_current_velocity)
   (sop/ext_partial_previous)
   :match_attribute "id")|} in
  let session = Session.create ~max_entries:8 ~max_payload_bytes:200_000_000 |> get in
  let context = Context.create ~domains:1 () |> get in
  check (match Session.cook session ~context unmatched_error with
    | Error error -> error.code = "invalid_deformation" | Ok _ -> false)
    "velocity missing matches retain structured cook error";
  Session.close session;
  let velocity_leaf = Lisp_sop.snapshot ((cook 1 current_velocity))
  and previous_leaf = Lisp_sop.snapshot ((cook 1 previous_velocity))
  and next_leaf = Lisp_sop.snapshot ((cook 1 next_velocity)) in
  let dynamic_velocity = Node.relabel "sample roles" (Lisp_sop.node ~with_:["velocity_leaf", (velocity_leaf); "previous_leaf", (previous_leaf)] {|(sop/point_velocity (sop/ext_velocity_leaf) (sop/ext_previous_leaf))|}) in
  let document = List.fold_left (fun document node -> Edit_graph.add_node node document |> get)
      Edit_graph.empty [velocity_leaf; previous_leaf; next_leaf]
      |> Edit_graph.add_node ~factory:Nodes.Point_velocity.factory
          ~inputs:[|Some (Node.id velocity_leaf); Some (Node.id previous_leaf); None|] dynamic_velocity |> get
      |> Edit_graph.disconnect ~consumer:(Node.id dynamic_velocity) ~input_index:1 |> get
      |> Edit_graph.connect ~source:(Node.id next_leaf) ~consumer:(Node.id dynamic_velocity) ~input_index:2 |> get in
  let rebuilt = Edit_graph.compile_node document ~node_id:(Node.id dynamic_velocity) |> get in
  let edited = fst (Node.apply_parameters rebuilt ["approximation", Param.Choice_value "Forward difference"] |> get) in
  same_node "velocity rewiring and parameter edit retain next role"
    ~typed:(point_velocity velocity_leaf None (Some next_leaf) ~args:[ks "approximation" "Forward difference"]) ~catalog:edited;
  check (Node.id edited = Node.id dynamic_velocity && Node.label edited = "sample roles")
    "velocity rewiring retains node identity and label";
  List.iter (fun make -> check (rejected make)
      "point velocity refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["current_velocity", (current_velocity)] {|(sop/point_velocity (sop/ext_current_velocity) :dt 0.0)|});
     (fun () -> Lisp_sop.node ~with_:["current_velocity", (current_velocity)] {|(sop/point_velocity (sop/ext_current_velocity) :velocity_attribute "P")|});
     (fun () -> Lisp_sop.node ~with_:["current_velocity", (current_velocity)] {|(sop/point_velocity (sop/ext_current_velocity) :velocity_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["current_velocity", (current_velocity)] {|(sop/point_velocity
   (sop/ext_current_velocity)
   :compute_acceleration true
   :acceleration_attribute "v")|});
     (fun () -> Lisp_sop.node ~with_:["current_velocity", (current_velocity)] {|(sop/point_velocity
   (sop/ext_current_velocity)
   :compute_acceleration true
   :acceleration_attribute "P")|});
     (fun () -> Lisp_sop.node ~with_:["current_velocity", (current_velocity)] {|(sop/point_velocity (sop/ext_current_velocity) :compute_acceleration true)|});
     (fun () -> Lisp_sop.node ~with_:["current_velocity", (current_velocity)] {|(sop/point_velocity
   (sop/ext_current_velocity)
   :compute_acceleration true
   :approximation "Central difference"
   :initialization "Keep incoming")|});
     (fun () -> Lisp_sop.node ~with_:["current_velocity", (current_velocity)] {|(sop/point_velocity
   (sop/ext_current_velocity)
   :initialization "From attribute"
   :source_attribute " ")|})];
  check (Result.is_error (Node.apply_parameters velocity ["dt", Param.Float_value 0.]))
    "velocity refuses zero time step inspector edit";
  let fade_input = Lisp_sop.node ~with_:["in548", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0., 0., 0.); (1., 0., 0.); (2., 0., 0.)|]))] (Printf.sprintf {|(-> (sop/set_float (sop/ext_in548) :name "start" :value 2.0)
     (sop/set_float :name "hold" :value 3.0)
     (sop/set_float :name "fade" :value 0.8)
     (sop/set_color :color %s)
     (sop/group_range :name "selected" :range_mode "From ends"))|} ((Lisp_sop.vec3 Vec3.unit_x))) in
  let faded = Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/attribute_fade (sop/ext_fade_input))|} in
  same_cook ~optional_inputs:[Some fade_input; None; None] "attribute fade defaults"
    ~typed:faded ~factory:Nodes.Attribute_fade.factory [] fade_input;
  cache_identity ~changes:["fade_in_ramp", Param.Text_value "0:0,0.5:0.3,1:1";
      "fade_out_ramp", Text_value "0:1,0.5:0.7,1:0"] "attribute fade all fields" faded;
  let fade_start = Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/set_float (sop/ext_fade_input) :name "start" :value 1.0)|}
  and fade_hold = Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/set_float (sop/ext_fade_input) :name "hold" :value 0.5)|} in
  List.iter (fun start_source -> List.iter (fun hold_source ->
    let typed = with_optional "attribute_fade" fade_input ["start_source", start_source; "hold_source", hold_source]
        ~args:[ks "group" "selected"; ks "start_attribute" "start"; kf "start_retime_offset" 1.;
               kf "start_retime_scale" 0.75; ks "hold_scale_attribute" "hold"; kf "frame_offset" (-10.);
               kf "fade_in" 8.; kf "fade_hold" 6.; kf "fade_out" 16.; ks "fade_in_ramp" "0:0,0.3:0.08,0.72:0.9,1:1";
               ks "fade_out_ramp" "0:1,0.25:0.96,0.65:0.18,1:0"; kb "visualize" true] in
    same_cook ~optional_inputs:[Some fade_input; start_source; hold_source] "fade optional inputs and ramps"
      ~typed ~factory:Nodes.Attribute_fade.factory ["group", Param.Text_value "selected";
        "start_attribute", Text_value "start"; "start_retime_offset", Float_value 1.;
        "start_retime_scale", Float_value 0.75; "hold_scale_attribute", Text_value "hold";
        "frame_offset", Float_value (-10.); "fade_in", Float_value 8.; "fade_hold", Float_value 6.;
        "fade_out", Float_value 16.; "fade_in_ramp", Text_value "0:0,0.3:0.08,0.72:0.9,1:1";
        "fade_out_ramp", Text_value "0:1,0.25:0.96,0.65:0.18,1:0"; "visualize", Bool_value true] fade_input;
    let geometry = cook 1 fade_input in
    let native = Rdk.Attribute_fade.fade ~points:(Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected" geometry |> Option.get)
        ?start_source:(Option.map (cook 1) start_source) ?hold_source:(Option.map (cook 1) hold_source)
        ~start_attribute:"start" ~start_retime:(1., 0.75) ~hold_scale_attribute:"hold"
        ~frame:0. ~frame_offset:(-10.) ~fade_in:8. ~fade_hold:6. ~fade_out:16.
        ~fade_in_ramp:[0.,0.;0.3,0.08;0.72,0.9;1.,1.]
        ~fade_out_ramp:[0.,1.;0.25,0.96;0.65,0.18;1.,0.] ~visualize:true geometry |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "fade matches native timing, inputs and ramps")
    [None; Some fade_hold]) [None; Some fade_start];
  same_cook ~optional_inputs:[Some fade_input; None; None] "fade unset names and ramps"
    ~typed:(Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/attribute_fade
   (sop/ext_fade_input)
   :group " "
   :start_attribute " "
   :hold_scale_attribute " "
   :fade_in_ramp " "
   :fade_out_ramp " ")|})
    ~factory:Nodes.Attribute_fade.factory ["group", Param.Text_value " ";
      "start_attribute", Text_value " "; "hold_scale_attribute", Text_value " ";
      "fade_in_ramp", Text_value " "; "fade_out_ramp", Text_value " "] fade_input;
  List.iter (fun make -> check (rejected make)
      "fade refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/attribute_fade (sop/ext_fade_input) :fade_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/attribute_fade (sop/ext_fade_input) :fade_attribute "P")|});
     (fun () -> Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/attribute_fade (sop/ext_fade_input) :fade_attribute "Cd" :visualize true)|});
     (fun () -> Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/attribute_fade (sop/ext_fade_input) :fade_in -1.0)|});
     (fun () -> Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/attribute_fade (sop/ext_fade_input) :fade_hold -1.0)|});
     (fun () -> Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/attribute_fade (sop/ext_fade_input) :fade_out -1.0)|});
     (fun () -> Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/attribute_fade (sop/ext_fade_input) :fade_in_ramp "0:0,0.5:1,0.5:0,1:1")|});
     (fun () -> Lisp_sop.node ~with_:["fade_input", (fade_input)] {|(sop/attribute_fade (sop/ext_fade_input) :fade_out_ramp "0:1,0.8:0")|})];
  check (Result.is_error (Node.apply_parameters faded ["fade_in_ramp", Param.Text_value "bad"]))
    "fade refuses malformed inspector ramp";
  let fade_leaf = Lisp_sop.snapshot ((cook 1 fade_input))
  and start_leaf = Lisp_sop.snapshot ((cook 1 fade_start))
  and hold_leaf = Lisp_sop.snapshot ((cook 1 fade_hold)) in
  let dynamic_fade = Node.relabel "optional fade" (Lisp_sop.node ~with_:["fade_leaf", (fade_leaf); "start_leaf", (start_leaf)] {|(sop/attribute_fade
   (sop/ext_fade_leaf)
   (sop/ext_start_leaf)
   :start_attribute "start"
   :hold_scale_attribute "hold"
   :fade_hold 3.0)|}) in
  let document = List.fold_left (fun document node -> Edit_graph.add_node node document |> get)
      Edit_graph.empty [fade_leaf; start_leaf; hold_leaf]
      |> Edit_graph.add_node ~inputs:[|Some (Node.id fade_leaf); Some (Node.id start_leaf); None|]
          ~factory:Nodes.Attribute_fade.factory dynamic_fade |> get
      |> Edit_graph.disconnect ~consumer:(Node.id dynamic_fade) ~input_index:1 |> get
      |> Edit_graph.connect ~source:(Node.id hold_leaf) ~consumer:(Node.id dynamic_fade) ~input_index:2 |> get in
  let rebuilt = Edit_graph.compile_node document ~node_id:(Node.id dynamic_fade) |> get in
  same_node "fade optional rewiring retains hold role" ~typed:(with_optional "attribute_fade" fade_leaf ["hold_source", Some hold_leaf]
      ~args:[ks "start_attribute" "start"; ks "hold_scale_attribute" "hold"; kf "fade_hold" 3.]) ~catalog:rebuilt;
  check (Node.id rebuilt = Node.id dynamic_fade && Node.label rebuilt = "optional fade")
    "fade rewiring retains node identity and label";
  let edited = fst (Node.apply_parameters rebuilt ["fade_hold", Param.Float_value 5.] |> get) in
  same_node "fade parameter edit retains optional roles" ~typed:(with_optional "attribute_fade" fade_leaf ["hold_source", Some hold_leaf]
      ~args:[ks "start_attribute" "start"; ks "hold_scale_attribute" "hold"; kf "fade_hold" 5.]) ~catalog:edited;
  let mirror_geometry = cook 1 (Lisp_sop.node {|(sop/box)|}) in
  let mirror_source owner group_owner =
    let count = match owner with
      | Rdk.Attribute.Point -> Rdk.Geometry.point_count mirror_geometry
      | Vertex -> Rdk.Geometry.vertex_count mirror_geometry
      | Primitive -> Rdk.Geometry.primitive_count mirror_geometry | Detail -> assert false in
    let values offset = Array.init count (fun index -> float_of_int index +. offset) in
    let attributes = ["value", Rdk.Attribute.Float4 (Rdk.Packed.Float4.of_owned
        ~x:(values 1.) ~y:(values 2.) ~z:(values 3.) ~w:(values 4.) |> get);
      "text", Rdk.Attribute.Text (Array.init count (fun index -> "L" ^ string_of_int index));
      "map", Rdk.Attribute.Int (Array.init count (fun index -> if index < count / 2 then -1 else count - index - 1))] in
    let geometry = List.fold_left (fun geometry (name, storage) ->
      Rdk.Geometry.with_attribute (Rdk.Attribute.create_owned ~owner ~name storage |> get) geometry |> get)
      mirror_geometry attributes in
    let destination = Rdk.Group.init ~owner:group_owner ~name:"destination" count (fun index -> index >= count / 2) in
    let selection = Rdk.Group.init ~owner:group_owner ~name:"selection" count (fun _ -> true) in
    Rdk.Geometry.with_group destination geometry |> get |> Rdk.Geometry.with_group selection |> get in
  let mirror_input = Lisp_sop.snapshot ((mirror_source Rdk.Attribute.Point Rdk.Group.Point)) in
  let mirrored = Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror (sop/ext_mirror_input))|} in
  same_cook "attribute mirror defaults" ~typed:mirrored ~factory:Nodes.Attribute_mirror.factory [] mirror_input;
  cache_identity ~companions:["owner", ["method_", Param.Choice_value "Mapping attribute"]]
    "attribute mirror all fields" mirrored;
  List.iter (fun (owner, attribute_owner, group_owner, owner_choice) ->
    let geometry = mirror_source attribute_owner group_owner in
    let input = Lisp_sop.snapshot (geometry) in
    List.iter (fun (group_use, group_choice) ->
      List.iter (fun (transform_choice, native_transform) ->
        let typed = sop ~inputs:[input] "attribute_mirror"
            [ks "owner" owner_choice; ks "method_" "Mapping attribute"; ks "mapping_attribute" "map";
             ks "mapping_destination_group" "destination"; ks "attributes" "value text"; ks "group" "selection";
             ks "group_use" group_choice; ks "transform" transform_choice; kf "uv_origin_u" 0.2; kf "uv_origin_v" 0.3;
             kf "uv_direction_u" 2.; kf "uv_direction_v" 3.; kb "replace_strings" true; ks "string_search" "L";
             ks "string_replacement" "Right"; ks "output_mapping" "pair"; ks "source_group" "sources";
             ks "destination_group" "destinations"] in
        same_cook ("mirror mapping " ^ owner_choice ^ group_choice ^ transform_choice) ~typed
          ~factory:Nodes.Attribute_mirror.factory ["owner", Param.Choice_value owner_choice;
            "method_", Choice_value "Mapping attribute"; "mapping_attribute", Text_value "map";
            "mapping_destination_group", Text_value "destination"; "attributes", Text_value "value text";
            "group", Text_value "selection"; "group_use", Choice_value group_choice;
            "transform", Choice_value transform_choice; "uv_origin_u", Float_value 0.2; "uv_origin_v", Float_value 0.3;
            "uv_direction_u", Float_value 2.; "uv_direction_v", Float_value 3.; "replace_strings", Bool_value true;
            "string_search", Text_value "L"; "string_replacement", Text_value "Right";
            "output_mapping", Text_value "pair"; "source_group", Text_value "sources";
            "destination_group", Text_value "destinations"] input;
        let find name = Rdk.Geometry.find_group ~owner:group_owner name geometry |> Option.get in
        let native = Rdk.Attribute_mirror.run ~owner ~group:(find "selection") ~group_use
            ~attributes:"value text" ~transform:native_transform ~string_replace:("L", "Right")
            ~output_mapping:"pair" ~source_group:"sources" ~destination_group:"destinations"
            ~method_:(Rdk.Attribute_mirror.Mirror_by_mapping {mapping_attribute="map"; destination_group=find "destination"})
            geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "mirror mapping matches native fields")
        ["Copy", Rdk.Attribute_mirror.Mirror_copy;
         "UV", Rdk.Attribute_mirror.Mirror_uv {origin_u=0.2; origin_v=0.3; direction_u=2.; direction_v=3.}])
      [Rdk.Attribute_mirror.Mirror_group_as_source, "Group is source";
       Mirror_group_as_destination, "Group is destination"];
    if owner <> Rdk.Attribute_mirror.Mirror_vertex_attributes then
      List.iter (fun (choice, native_transform) ->
        let typed = sop ~inputs:[input] "attribute_mirror"
            [ks "owner" owner_choice; ks "attributes" "value"; ks "transform" choice; kf "distance" 0.;
             kv "normal" (Vec3.create 2. 0. 0.); kf "tolerance" 0.01] in
        same_cook ("mirror plane " ^ owner_choice ^ choice) ~typed ~factory:Nodes.Attribute_mirror.factory
          ["owner", Param.Choice_value owner_choice; "attributes", Text_value "value";
           "transform", Choice_value choice; "distance", Float_value 0.; "normal_x", Float_value 2.;
           "tolerance", Float_value 0.01] input;
        let native = Rdk.Attribute_mirror.run ~owner ~attributes:"value" ~transform:native_transform
            ~method_:(Rdk.Attribute_mirror.Mirror_by_plane {origin=Vec3.zero; normal=Vec3.create 2. 0. 0.;
              distance=0.; tolerance=0.01}) geometry |> Result.get_ok in
        check (equal_geometry native (cook 1 typed)) "mirror plane matches native fields")
        ["Copy", Rdk.Attribute_mirror.Mirror_copy;
         "UV", Rdk.Attribute_mirror.Mirror_uv {origin_u=0.; origin_v=0.; direction_u=1.; direction_v=0.};
         "Vector", Rdk.Attribute_mirror.Mirror_vector; "Point", Rdk.Attribute_mirror.Mirror_point])
    [Rdk.Attribute_mirror.Mirror_point_attributes, Rdk.Attribute.Point, Rdk.Group.Point, "Point";
     Mirror_vertex_attributes, Vertex, Vertex, "Vertex"; Mirror_primitive_attributes, Primitive, Primitive, "Primitive"];
  same_cook "mirror unset names" ~typed:(Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror
   (sop/ext_mirror_input)
   :group " "
   :output_mapping " "
   :source_group " "
   :destination_group " ")|}) ~factory:Nodes.Attribute_mirror.factory
    ["group", Param.Text_value " "; "output_mapping", Text_value " ";
     "source_group", Text_value " "; "destination_group", Text_value " "] mirror_input;
  List.iter (fun make -> check (rejected make)
      "mirror refuses invalid controls at construction")
    [(fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror (sop/ext_mirror_input) :distance -1.0)|});
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] (Printf.sprintf {|(sop/attribute_mirror (sop/ext_mirror_input) :tolerance %s)|} ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] (Printf.sprintf {|(sop/attribute_mirror (sop/ext_mirror_input) :normal %s)|} ((Lisp_sop.vec3 Vec3.zero))));
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] (Printf.sprintf {|(sop/attribute_mirror
   (sop/ext_mirror_input)
   :origin [%s 0.0 0.0]
   :distance %s)|} ((Lisp_sop.float Float.max_float)) ((Lisp_sop.float Float.max_float))));
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror (sop/ext_mirror_input) :owner "Vertex")|});
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror
   (sop/ext_mirror_input)
   :method_ "Mapping attribute"
   :mapping_attribute " ")|});
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror
   (sop/ext_mirror_input)
   :method_ "Mapping attribute"
   :mapping_destination_group " ")|});
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror
   (sop/ext_mirror_input)
   :method_ "Mapping attribute"
   :transform "Vector")|});
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror (sop/ext_mirror_input) :transform "UV" :uv_direction_u 0.0)|});
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror (sop/ext_mirror_input) :replace_strings true :string_search "")|});
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror
   (sop/ext_mirror_input)
   :source_group "same"
   :destination_group "same")|});
     (fun () -> Lisp_sop.node ~with_:["mirror_input", (mirror_input)] {|(sop/attribute_mirror (sop/ext_mirror_input) :attributes "[")|})];
  check (Result.is_error (Node.apply_parameters mirrored ["normal_x", Param.Float_value 0.]))
    "mirror refuses zero normal inspector edit";
  let remap_geometry = cook 1 (Lisp_sop.node {|(sop/box)|}) in
  let remap_source owner kind =
    let count = match owner with
      | Rdk.Attribute.Point -> Rdk.Geometry.point_count remap_geometry
      | Vertex -> Rdk.Geometry.vertex_count remap_geometry
      | Primitive -> Rdk.Geometry.primitive_count remap_geometry | Detail -> 1 in
    let values () = Array.init count (fun index -> float_of_int (index mod 4) -. 1.) in
    let storage = match kind with
      | "Scalar" -> Rdk.Attribute.Float (values ())
      | "Vector 2" -> Rdk.Attribute.Float2 (Rdk.Packed.Float2.of_owned ~x:(values ()) ~y:(values ()) |> get)
      | "Vector 3" -> Rdk.Attribute.Float3 (Rdk.Packed.Float3.of_owned ~x:(values ()) ~y:(values ()) ~z:(values ()) |> get)
      | _ -> Rdk.Attribute.Float4 (Rdk.Packed.Float4.of_owned ~x:(values ()) ~y:(values ()) ~z:(values ()) ~w:(values ()) |> get) in
    let attribute = Rdk.Attribute.create_owned ~owner ~name:"value" storage |> get in
    Rdk.Geometry.with_attribute attribute remap_geometry |> get in
  let remap_input = Lisp_sop.snapshot ((remap_source Rdk.Attribute.Point "Scalar")) in
  let remapped = Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/attribute_remap (sop/ext_remap_input))|} in
  same_cook "attribute remap defaults" ~typed:remapped ~factory:Nodes.Attribute_remap.factory [] remap_input;
  cache_identity ~changes:["ramp", Param.Text_value "0:0,0.5:0.2,1:1"]
    "attribute remap all fields" remapped;
  List.iter (fun (owner, owner_choice) ->
    List.iter (fun (kind_choice, output_min, output_max) ->
      let geometry = remap_source owner kind_choice in
      let input_node = Lisp_sop.snapshot (geometry) in
      List.iter (fun (policy, policy_choice) ->
        List.iter (fun (range_choice, input) ->
          let typed = sop ~inputs:[input_node] "attribute_remap"
              [ks "owner" owner_choice; ks "kind" kind_choice; ks "into" "mapped"; ks "policy" policy_choice;
               ks "input_range" range_choice; kv "output_min" (Vec3.create (-2.) (-3.) (-4.)); kf "output_min_w" (-5.);
               kv "output_max" (Vec3.create 2. 3. 4.); kf "output_max_w" 5.; ks "ramp" "0:0,0.5:0.2,1:1"] in
          same_cook ("remap " ^ owner_choice ^ " " ^ kind_choice ^ " " ^ policy_choice ^ " " ^ range_choice)
            ~typed ~factory:Nodes.Attribute_remap.factory ["owner", Param.Choice_value owner_choice;
              "kind", Choice_value kind_choice; "into", Text_value "mapped"; "policy", Choice_value policy_choice;
              "input_range", Choice_value range_choice; "output_min_x", Float_value (-2.);
              "output_min_y", Float_value (-3.); "output_min_z", Float_value (-4.); "output_min_w", Float_value (-5.);
              "output_max_x", Float_value 2.; "output_max_y", Float_value 3.; "output_max_z", Float_value 4.;
              "output_max_w", Float_value 5.; "ramp", Text_value "0:0,0.5:0.2,1:1"] input_node;
          let native = Rdk.Attribute_ops.remap ~owner ~name:"value" ~into:"mapped" ~input
              ~output_min ~output_max ~policy ~ramp:[0., 0.; 0.5, 0.2; 1., 1.] geometry
              |> Rdk.Error.unguard |> get in
          check (close_blend native (cook 1 typed)) "remap flat ranges/ramp preserve native channels")
          ["Automatic", Rdk.Attribute_ops.Remap_auto;
           "Explicit", Rdk.Attribute_ops.Remap_explicit {
             min = (match kind_choice with "Scalar" -> Rdk.Attribute_ops.Scalar 0. | "Vector 2" -> Rdk.Attribute_ops.Vec2 Vec2.zero
               | "Vector 3" -> Rdk.Attribute_ops.Vec3 Vec3.zero | _ -> Rdk.Attribute_ops.Vec4 (0., 0., 0., 0.));
             max = (match kind_choice with "Scalar" -> Rdk.Attribute_ops.Scalar 1. | "Vector 2" -> Rdk.Attribute_ops.Vec2 (Vec2.create 1. 1.)
               | "Vector 3" -> Rdk.Attribute_ops.Vec3 (Vec3.create 1. 1. 1.) | _ -> Rdk.Attribute_ops.Vec4 (1., 1., 1., 1.)) }])
        [Rdk.Attribute_ops.Remap_clamp, "Clamp"; Remap_cycle, "Cycle"; Remap_extrapolate, "Extrapolate"])
      ["Scalar", Rdk.Attribute_ops.Scalar (-2.), Rdk.Attribute_ops.Scalar 2.;
       "Vector 2", Rdk.Attribute_ops.Vec2 (Vec2.create (-2.) (-3.)), Vec2 (Vec2.create 2. 3.);
       "Vector 3", Rdk.Attribute_ops.Vec3 (Vec3.create (-2.) (-3.) (-4.)), Vec3 (Vec3.create 2. 3. 4.);
       "Vector 4", Rdk.Attribute_ops.Vec4 (-2., -3., -4., -5.), Vec4 (2., 3., 4., 5.)])
    [Rdk.Attribute.Point, "Point"; Vertex, "Vertex"; Primitive, "Primitive"; Detail, "Detail"];
  let selected_remap = Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/group_range (sop/ext_remap_input) :name "selected" :end_ 1)|} in
  same_cook "remap selection and unset destination"
    ~typed:(Lisp_sop.node ~with_:["selected_remap", (selected_remap)] {|(sop/attribute_remap (sop/ext_selected_remap) :group "selected" :into " " :ramp " ")|})
    ~factory:Nodes.Attribute_remap.factory ["group", Param.Text_value "selected";
      "into", Text_value " "; "ramp", Text_value " "] selected_remap;
  same_cook "remap unset group" ~typed:(Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/attribute_remap (sop/ext_remap_input) :group " ")|})
    ~factory:Nodes.Attribute_remap.factory ["group", Param.Text_value " "] remap_input;
  let remap_positions = Lisp_sop.node {|(sop/box)|} in
  same_cook "remap canonical P" ~typed:(Lisp_sop.node ~with_:["remap_positions", (remap_positions)] {|(sop/attribute_remap (sop/ext_remap_positions) :name "P" :kind "Vector 3")|})
    ~factory:Nodes.Attribute_remap.factory ["name", Param.Text_value "P";
      "kind", Choice_value "Vector 3"] remap_positions;
  List.iter (fun make -> check (rejected make)
      "remap refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/attribute_remap (sop/ext_remap_input) :name " ")|});
      (fun () -> Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/attribute_remap (sop/ext_remap_input) :owner "Detail" :group "selected")|});
      (fun () -> Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/attribute_remap (sop/ext_remap_input) :into "P")|});
      (fun () -> Lisp_sop.node ~with_:["remap_input", (remap_input)] (Printf.sprintf {|(sop/attribute_remap (sop/ext_remap_input) :input_range "Explicit" :input_max %s)|} ((Lisp_sop.vec3 Vec3.zero))));
      (fun () -> Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/attribute_remap (sop/ext_remap_input) :ramp "0:0,0.5:0.2,0.5:0.7,1:1")|});
      (fun () -> Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/attribute_remap (sop/ext_remap_input) :ramp "0.2:0,1:1")|});
      (fun () -> Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/attribute_remap (sop/ext_remap_input) :ramp "0:0,0.8:1")|});
      (fun () -> Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/attribute_remap (sop/ext_remap_input) :ramp "0:nan,1:1")|});
      (fun () -> Lisp_sop.node ~with_:["remap_input", (remap_input)] {|(sop/attribute_remap (sop/ext_remap_input) :ramp "bad")|}) ];
  let copy_source = Lisp_sop.node ~with_:["in612", (List.fold_left (fun input owner ->
      sop ~inputs:[input] "set_float" [ks "owner" owner; ks "name" "weight"; kf "value" 10.]) (Lisp_sop.node {|(sop/box)|})
      ["Point"; "Vertex"; "Primitive"])] {|(-> (sop/set_int (sop/ext_in612) :owner "Primitive" :name "variant" :value 10)
     (sop/group_range :owner "Primitives" :name "source" :end_ 0))|} in
  let copy_targets = Lisp_sop.node ~with_:["in613", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0., 0., 0.); (2., 0., 0.); (4., 0., 0.)|]))] {|(-> (sop/set_float (sop/ext_in613) :name "weight" :value 3.0)
     (sop/set_int :name "variant" :value 10))|}
      |> group_indices "Points" "targets" [|0; 2|] in
  let copied = Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points (sop/ext_copy_source) (sop/ext_copy_targets))|} in
  same_cook ~inputs:[copy_targets] "copy to points defaults" ~typed:copied
    ~factory:Nodes.Copy_to_points.factory [] copy_source;
  cache_identity ~changes:["target_attributes", Param.Text_value "weight\tpoints\tcopy"]
    "copy to points all fields" copied;
  same_cook ~inputs:[copy_targets] "copy to points group restrictions"
    ~typed:(Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :source_group "source"
   :target_group "targets")|})
    ~factory:Nodes.Copy_to_points.factory ["source_group", Param.Text_value "source";
      "target_group", Text_value "targets"] copy_source;
  same_cook ~inputs:[copy_targets] "copy to points piece matching"
    ~typed:(Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :piece_attribute "variant")|})
    ~factory:Nodes.Copy_to_points.factory ["piece_attribute", Param.Text_value "variant"] copy_source;
  same_cook ~inputs:[copy_targets] "copy to points unset names"
    ~typed:(Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :source_group " "
   :target_group " "
   :piece_attribute " "
   :target_attributes " ")|})
    ~factory:Nodes.Copy_to_points.factory ["source_group", Param.Text_value " ";
      "target_group", Text_value " "; "piece_attribute", Text_value " "; "target_attributes", Text_value " "] copy_source;
  let source_geometry = cook 1 copy_source and target_geometry = cook 1 copy_targets in
  List.iter (fun (owner_token, copy_target_owner) ->
    List.iter (fun (operation_token, copy_target_operation) ->
      let target_attributes = "weight\t" ^ owner_token ^ "\t" ^ operation_token in
      let typed = Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] (Printf.sprintf {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :target_attributes %S)|} (target_attributes)) in
      same_cook ~inputs:[copy_targets] ("copy transfer " ^ owner_token ^ " " ^ operation_token)
        ~typed ~factory:Nodes.Copy_to_points.factory
        ["target_attributes", Param.Text_value target_attributes] copy_source;
      let native = Rdk.Instance_copy.copy_to_points
          ~target_attributes:[{ Rdk.Instance_copy.copy_target_pattern = "weight";
            copy_target_owner; copy_target_operation }]
          ~source:source_geometry ~targets:target_geometry () |> Rdk.Error.unguard |> get in
      check (equal_geometry native (cook 1 typed)) "copy target table preserves native operations")
      ["nothing", Rdk.Instance_copy.Copy_target_nothing; "copy", Copy_target_copy;
       "add", Copy_target_add; "subtract", Copy_target_subtract; "multiply", Copy_target_multiply])
    ["points", Rdk.Instance_copy.Copy_target_points; "vertices", Copy_target_vertices;
     "primitives", Copy_target_primitives];
  let packed = Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :pack true
   :target_group "targets")|} in
  let packed_factory = from_factory Nodes.Copy_to_points.factory
      ["pack", Param.Bool_value true; "target_group", Text_value "targets"] [copy_source; copy_targets] in
  same_node "copy packed geometry" ~typed:packed ~catalog:packed_factory;
  let transforms domains graph =
    let context = Context.create ~domains ~grain:97 ~seed:42L () |> get in
    let session = Session.create ~max_entries:8 ~max_payload_bytes:200_000_000 |> get in
    let output = Session.cook session ~context graph |> Result.get_ok in
    Session.close session;
    Option.get output.instances in
  let expected = transforms 1 packed in
  List.iter (fun actual -> check (Marshal.to_string actual [Marshal.No_sharing]
      = Marshal.to_string expected [Marshal.No_sharing] && Array.length actual = 2)
      "copy packed transforms are byte-identical at both doors and domain counts")
    [transforms 4 packed; transforms 1 packed_factory; transforms 4 packed_factory];
  List.iter (fun make -> check (rejected make)
      "copy to points refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :target_attributes "weight")|});
      (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :target_attributes "weight	detail	copy")|});
      (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :target_attributes "weight	points	bogus")|});
      (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :target_attributes "[	points	copy")|});
      (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :pack true
   :source_group "source")|});
      (fun () -> Lisp_sop.node ~with_:["copy_source", (copy_source); "copy_targets", (copy_targets)] {|(sop/copy_to_points
   (sop/ext_copy_source)
   (sop/ext_copy_targets)
   :pack true
   :piece_attribute "variant")|}) ];
  let exploded_input = Lisp_sop.node {|(sop/box)|} in
  let exploded = Lisp_sop.node ~with_:["exploded_input", (exploded_input)] {|(sop/exploded_view (sop/ext_exploded_input))|} in
  same_cook "exploded view defaults" ~typed:exploded ~factory:Nodes.Exploded_view.factory [] exploded_input;
  same_cook "exploded view advanced controls"
    ~typed:(Lisp_sop.node ~with_:["exploded_input", (exploded_input)] {|(sop/exploded_view
   (sop/ext_exploded_input)
   :amount -0.2
   :scale [0.0 2.0 -1.0]
   :piece_attribute "shard"
   :noise_amount 0.5
   :noise_frequency 0.0
   :noise_seed 7)|})
    ~factory:Nodes.Exploded_view.factory ["amount", Param.Float_value (-0.2);
      "scale_x", Float_value 0.; "scale_y", Float_value 2.; "scale_z", Float_value (-1.);
      "piece_attribute", Text_value "shard"; "noise_amount", Float_value 0.5;
      "noise_frequency", Float_value 0.; "noise_seed", Int_value 7] exploded_input;
  check (equal_geometry (cook 1 exploded_input) (cook 1 exploded))
    "exploded view preserves source geometry";
  List.iter (fun make -> check (rejected make)
      "exploded view refuses invalid controls at construction")
    [ (fun () -> Lisp_sop.node ~with_:["exploded_input", (exploded_input)] {|(sop/exploded_view (sop/ext_exploded_input) :noise_frequency -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["exploded_input", (exploded_input)] {|(sop/exploded_view (sop/ext_exploded_input) :piece_attribute " ")|}) ];
  let join_input = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0]))
     (-> (sop/curve (list [2.0 0.0 0.0] [1.0 0.0 0.0]))
         (-> (sop/curve (list [2.0 0.0 0.0] [3.0 0.0 0.0])) (sop/merge))))|}
      |> group_indices "Primitives" "selected" [|2; 0|] in
  let joined = Lisp_sop.node ~with_:["join_input", (join_input)] {|(sop/join_curves (sop/ext_join_input))|} in
  same_cook "join curves defaults" ~typed:joined ~factory:Nodes.Join_curves.factory [] join_input;
  cache_identity ~changes:["picked_ends", Param.Text_value "0:start"]
    "join curves all fields" joined;
  List.iter (fun orient_closest -> List.iter (fun connect_closest_ends ->
    List.iter (fun wrap ->
      same_cook "join curves orientation, ordering and wrapping"
        ~typed:(Lisp_sop.node ~with_:["join_input", (join_input)] (Printf.sprintf {|(sop/join_curves
   (sop/ext_join_input)
   :orient_closest %b
   :connect_closest_ends %b
   :wrap %b)|} (orient_closest) (connect_closest_ends) (wrap)))
        ~factory:Nodes.Join_curves.factory ["orient_closest", Param.Bool_value orient_closest;
          "connect_closest_ends", Bool_value connect_closest_ends; "wrap", Bool_value wrap] join_input)
      [false; true]) [false; true]) [false; true];
  List.iter (fun group_size ->
    same_cook "join curves grouped limits and retained originals"
      ~typed:(Lisp_sop.node ~with_:["join_input", (join_input)] (Printf.sprintf {|(sop/join_curves
   (sop/ext_join_input)
   :group "selected"
   :use_group_size true
   :group_size %d
   :keep_originals true
   :only_connected true
   :tolerance 0.1)|} (group_size)))
      ~factory:Nodes.Join_curves.factory ["group", Param.Text_value "selected";
        "use_group_size", Bool_value true; "group_size", Int_value group_size;
        "keep_originals", Bool_value true; "only_connected", Bool_value true;
        "tolerance", Float_value 0.1] join_input) [1; 2; 5];
  let join_geometry = cook 1 join_input in
  List.iter (fun (picked_ends, picks) ->
    let typed = Lisp_sop.node ~with_:["join_input", (join_input)] (Printf.sprintf {|(sop/join_curves (sop/ext_join_input) :picked_ends %S)|} (picked_ends)) in
    same_cook "join curves explicit picked traversal" ~typed ~factory:Nodes.Join_curves.factory
      ["picked_ends", Param.Text_value picked_ends] join_input;
    let native = Rdk.Curve_topology.join_curves ~picked_ends:picks join_geometry
        |> Rdk.Error.unguard |> get in
    check (equal_geometry native (cook 1 typed)) "picked text preserves native traversal")
    ["2:end,0:start,1:end", [|
        { Rdk.Curve_topology.primitive = 2; end_ = Join_curve_end };
        { Rdk.Curve_topology.primitive = 0; end_ = Join_curve_start };
        { Rdk.Curve_topology.primitive = 1; end_ = Join_curve_end }|];
     "[]", [||]];
  same_cook "join curves unset selection" ~typed:(Lisp_sop.node ~with_:["join_input", (join_input)] {|(sop/join_curves (sop/ext_join_input) :group " " :picked_ends " ")|})
    ~factory:Nodes.Join_curves.factory ["group", Param.Text_value " "; "picked_ends", Text_value " "] join_input;
  List.iter (fun make -> check (rejected make)
      "join curves refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["join_input", (join_input)] {|(sop/join_curves (sop/ext_join_input) :group_size 0)|});
      (fun () -> Lisp_sop.node ~with_:["join_input", (join_input)] {|(sop/join_curves (sop/ext_join_input) :tolerance -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["join_input", (join_input)] {|(sop/join_curves (sop/ext_join_input) :picked_ends "0:start,0:end")|});
      (fun () -> Lisp_sop.node ~with_:["join_input", (join_input)] {|(sop/join_curves (sop/ext_join_input) :picked_ends "-1:start")|});
      (fun () -> Lisp_sop.node ~with_:["join_input", (join_input)] {|(sop/join_curves (sop/ext_join_input) :picked_ends "0:middle")|});
      (fun () -> Lisp_sop.node ~with_:["join_input", (join_input)] {|(sop/join_curves (sop/ext_join_input) :picked_ends "bad")|});
      (fun () -> Lisp_sop.node ~with_:["join_input", (join_input)] {|(sop/join_curves (sop/ext_join_input) :picked_ends "0:start" :group "selected")|});
      (fun () -> Lisp_sop.node ~with_:["join_input", (join_input)] {|(sop/join_curves (sop/ext_join_input) :picked_ends "0:start" :connect_closest_ends true)|}) ];
  check (Result.is_error (Node.apply_parameters joined
      ["picked_ends", Param.Text_value "0:start,0:end"]))
    "join curves inspector refuses duplicate picks";
  let jitter_input = Lisp_sop.node {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 3 :rows 2 :size 2.0)
     (sop/set_float :name "mask" :value 0.5)
     (sop/set_float :name "pscale" :value 2.0)
     (sop/set_int :name "id" :value 17)
     (sop/group_range :name "selected" :end_ 1))|} in
  let jittered = Lisp_sop.node ~with_:["jitter_input", (jitter_input)] {|(sop/point_jitter (sop/ext_jitter_input))|} in
  same_cook "point jitter defaults" ~typed:jittered ~factory:Nodes.Point_jitter.factory [] jitter_input;
  cache_identity "point jitter all fields" jittered;
  List.iter (fun choice ->
    List.iter (fun use_point_scale ->
      same_cook ("point jitter " ^ choice ^ " point scale " ^ string_of_bool use_point_scale)
        ~typed:(sop ~inputs:[jitter_input] "point_jitter"
          [ks "seed_mode" choice; ki "seed" 23; kb "use_point_scale" use_point_scale; ks "group" "selected";
           ks "mask_attribute" "mask"; ks "id_attribute" "id"; kf "scale" 0.4; kv "axis" (Vec3.create 1. 0.5 0.25)])
        ~factory:Nodes.Point_jitter.factory ["seed_mode", Param.Choice_value choice;
          "seed", Int_value 23; "use_point_scale", Bool_value use_point_scale;
          "group", Text_value "selected"; "mask_attribute", Text_value "mask";
          "id_attribute", Text_value "id"; "scale", Float_value 0.4;
          "axis_y", Float_value 0.5; "axis_z", Float_value 0.25] jitter_input) [false; true])
    ["Explicit"; "Auto"];
  same_cook "point jitter unset names" ~typed:(Lisp_sop.node ~with_:["jitter_input", (jitter_input)] (Printf.sprintf {|(sop/point_jitter
   (sop/ext_jitter_input)
   :group " "
   :mask_attribute " "
   :id_attribute " "
   :scale 0.0
   :axis %s)|} ((Lisp_sop.vec3 Vec3.zero))))
    ~factory:Nodes.Point_jitter.factory ["group", Param.Text_value " "; "mask_attribute", Text_value " ";
      "id_attribute", Text_value " "; "scale", Float_value 0.; "axis_x", Float_value 0.;
      "axis_y", Float_value 0.; "axis_z", Float_value 0.] jitter_input;
  List.iter (fun make -> check (rejected make)
      "point jitter refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["jitter_input", (jitter_input)] {|(sop/point_jitter (sop/ext_jitter_input) :scale -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["jitter_input", (jitter_input)] {|(sop/point_jitter (sop/ext_jitter_input) :axis [-1.0 1.0 1.0])|}) ];
  let snap_input = Lisp_sop.node ~with_:["in666", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|(0.2, 0., 0.); (0.3, 0., 0.); (1.2, 0., 0.)|]))] {|(-> (sop/set_float (sop/ext_in666) :value 2.0)
     (sop/set_float :name "w" :value 1.0)
     (sop/group_range :name "selected" :end_ 2))|} in
  let snapped = Lisp_sop.node ~with_:["snap_input", (snap_input)] {|(sop/snap_to_grid (sop/ext_snap_input))|} in
  same_cook "snap to grid defaults" ~typed:snapped ~factory:Nodes.Snap_to_grid.factory [] snap_input;
  cache_identity ~changes:["attribute_rules", Param.Text_value "value\tmaximum\t";
      "group_rules", Text_value "selected\tunion"] "snap to grid all fields" snapped;
  let snap_geometry = cook 1 snap_input in
  let snap_selection = Rdk.Geometry.find_group ~owner:Rdk.Group.Point "selected" snap_geometry |> Option.get in
  List.iter (fun (rounding, choice) ->
    List.iter (fun limit_distance ->
      let typed = sop ~inputs:[snap_input] "snap_to_grid"
          [ks "group" "selected"; ks "rounding" choice; kb "limit_distance" limit_distance; kf "max_distance" 0.25;
           kv "spacing" (Vec3.create 1. 2. 3.); kv "offset" (Vec3.create 0.25 0.5 0.75); ks "snapped_group" "snapped"] in
      check (equal_geometry (cook 1 typed)
          (Rdk.Fuse_grid.snap_to_grid ~selection:snap_selection ~spacing:(Vec3.create 1. 2. 3.)
             ~offset:(Vec3.create 0.25 0.5 0.75) ~rounding ?max_distance:(if limit_distance then Some 0.25 else None)
             ~snapped_group:"snapped" snap_geometry |> Rdk.Error.unguard |> get))
        "snap to grid rounding and distance limit match native";
      same_cook ("snap to grid " ^ choice ^ " limited " ^ string_of_bool limit_distance)
        ~typed
        ~factory:Nodes.Snap_to_grid.factory ["group", Param.Text_value "selected";
          "rounding", Choice_value choice; "limit_distance", Bool_value limit_distance;
          "max_distance", Float_value 0.25; "spacing_y", Float_value 2.; "spacing_z", Float_value 3.;
          "offset_x", Float_value 0.25; "offset_y", Float_value 0.5; "offset_z", Float_value 0.75;
          "snapped_group", Text_value "snapped"] snap_input) [false; true])
    [Rdk.Fuse_grid.Grid_nearest, "Nearest"; Grid_down, "Down"; Grid_up, "Up"];
  List.iter (fun (position, choice) ->
    let typed = sop ~inputs:[snap_input] "snap_to_grid" [kb "fuse_points" true; ks "position" choice; ks "weight_attribute" "w"] in
    check (equal_geometry (cook 1 typed)
        (Rdk.Fuse_grid.snap_to_grid ~fuse_points:true ~position ~weight_attribute:"w" snap_geometry
         |> Rdk.Error.unguard |> get))
      ("snap to grid position " ^ choice ^ " matches native");
    same_cook ("snap to grid position " ^ choice)
      ~typed
      ~factory:Nodes.Snap_to_grid.factory ["fuse_points", Param.Bool_value true;
        "position", Choice_value choice; "weight_attribute", Text_value "w"] snap_input)
    [Rdk.Fuse_reduce.First_position, "First"; Least_point_position, "Least point";
     Greatest_point_position, "Greatest point"; Average_position, "Average";
     Minimum_position, "Minimum"; Maximum_position, "Maximum"; Mode_position, "Mode";
     Median_position, "Median"; Sum_position, "Sum"; Sum_squares_position, "Sum squares";
     Root_mean_square_position, "Root mean square"; Weighted_average_position, "Weighted average";
     Weighted_sum_position, "Weighted sum"; Minimum_weight_position, "Minimum weight";
     Maximum_weight_position, "Maximum weight"];
  List.iter (fun (token, method_) ->
    let attribute_rules = "value\t" ^ token ^ "\tw" in
    let typed = Lisp_sop.node ~with_:["snap_input", (snap_input)] (Printf.sprintf {|(sop/snap_to_grid (sop/ext_snap_input) :fuse_points true :attribute_rules %S)|} (attribute_rules)) in
    same_cook ("snap to grid attribute rule " ^ token) ~typed
      ~factory:Nodes.Snap_to_grid.factory ["fuse_points", Param.Bool_value true;
        "attribute_rules", Text_value attribute_rules] snap_input;
    let native = Rdk.Fuse_grid.snap_to_grid ~fuse_points:true
        ~attribute_rules:[{ Rdk.Fuse_reduce.pattern = "value"; method_; weight_attribute = Some "w" }]
        snap_geometry |> Rdk.Error.unguard |> get in
    check (equal_geometry native (cook 1 typed)) ("snap attribute table preserves native " ^ token))
    [ "average", Rdk.Fuse_reduce.Attribute_average; "least", Attribute_least_point;
      "greatest", Attribute_greatest_point; "maximum", Attribute_maximum; "minimum", Attribute_minimum;
      "mode", Attribute_mode; "median", Attribute_median; "sum", Attribute_sum;
      "sum_squares", Attribute_sum_squares; "root_mean_square", Attribute_root_mean_square;
      "concatenate", Attribute_concatenate; "weighted_average", Attribute_weighted_average;
      "weighted_sum", Attribute_weighted_sum; "minimum_weight", Attribute_minimum_weight;
      "maximum_weight", Attribute_maximum_weight; "concatenate_weight_order", Attribute_concatenate_weight_order ];
  List.iter (fun (token, group_method) ->
    let group_rules = "selected\t" ^ token in
    let typed = Lisp_sop.node ~with_:["snap_input", (snap_input)] (Printf.sprintf {|(sop/snap_to_grid (sop/ext_snap_input) :fuse_points true :group_rules %S)|} (group_rules)) in
    same_cook ("snap to grid group rule " ^ token) ~typed
      ~factory:Nodes.Snap_to_grid.factory ["fuse_points", Param.Bool_value true;
        "group_rules", Text_value group_rules] snap_input;
    let native = Rdk.Fuse_grid.snap_to_grid ~fuse_points:true
        ~group_rules:[{ Rdk.Fuse_reduce.group_pattern = "selected"; group_method }]
        snap_geometry |> Rdk.Error.unguard |> get in
    check (equal_geometry native (cook 1 typed)) ("snap group table preserves native " ^ token))
    ["least", Rdk.Fuse_reduce.Group_least_point; "greatest", Group_greatest_point;
     "union", Group_union; "intersection", Group_intersection; "most_common", Group_most_common];
  same_cook "snap to grid average attributes and unset names"
    ~typed:(Lisp_sop.node ~with_:["snap_input", (snap_input)] {|(sop/snap_to_grid
   (sop/ext_snap_input)
   :fuse_points true
   :attributes "Average numeric"
   :group " "
   :weight_attribute " "
   :snapped_group " "
   :attribute_rules " "
   :group_rules " ")|})
    ~factory:Nodes.Snap_to_grid.factory ["fuse_points", Param.Bool_value true;
      "attributes", Choice_value "Average numeric"; "group", Text_value " "; "weight_attribute", Text_value " ";
      "snapped_group", Text_value " "; "attribute_rules", Text_value " "; "group_rules", Text_value " "] snap_input;
  List.iter (fun make -> check (rejected make)
      "snap to grid refuses invalid values at construction")
    [ (fun () -> Lisp_sop.node ~with_:["snap_input", (snap_input)] (Printf.sprintf {|(sop/snap_to_grid (sop/ext_snap_input) :spacing %s)|} ((Lisp_sop.vec3 Vec3.zero))));
      (fun () -> Lisp_sop.node ~with_:["snap_input", (snap_input)] {|(sop/snap_to_grid (sop/ext_snap_input) :offset [-0.1 0.0 0.0])|});
      (fun () -> Lisp_sop.node ~with_:["snap_input", (snap_input)] {|(sop/snap_to_grid (sop/ext_snap_input) :offset [0.0 1.1 0.0])|});
      (fun () -> Lisp_sop.node ~with_:["snap_input", (snap_input)] {|(sop/snap_to_grid (sop/ext_snap_input) :max_distance -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["snap_input", (snap_input)] {|(sop/snap_to_grid (sop/ext_snap_input) :attribute_rules "value")|});
      (fun () -> Lisp_sop.node ~with_:["snap_input", (snap_input)] {|(sop/snap_to_grid (sop/ext_snap_input) :attribute_rules "value	bogus	")|});
      (fun () -> Lisp_sop.node ~with_:["snap_input", (snap_input)] {|(sop/snap_to_grid (sop/ext_snap_input) :attribute_rules "value	weighted_average	")|});
      (fun () -> Lisp_sop.node ~with_:["snap_input", (snap_input)] {|(sop/snap_to_grid (sop/ext_snap_input) :attribute_rules "[	average	")|});
      (fun () -> Lisp_sop.node ~with_:["snap_input", (snap_input)] {|(sop/snap_to_grid (sop/ext_snap_input) :group_rules "selected	bogus")|}) ];
  let clean_input = Lisp_sop.node {|(sop/box)|} in
  let cleaned = Lisp_sop.node ~with_:["clean_input", (clean_input)] {|(sop/clean (sop/ext_clean_input))|} in
  same_cook "clean Lisp defaults" ~typed:cleaned ~factory:Nodes.Clean.factory [] clean_input;
  cache_identity "clean all fields" cleaned;
  let clean_geometry = cook 1 clean_input in
  List.iter (fun (epsilon, epsilon_choice) ->
    List.iter (fun (consolidate_distance, consolidate_choice) ->
      List.iter (fun (overlaps, overlap_choice) ->
        let typed = sop ~inputs:[clean_input] "clean"
            [ks "epsilon_mode" epsilon_choice; ks "consolidate_mode" consolidate_choice; ks "overlaps" overlap_choice;
             kf "epsilon" 0.; kf "consolidate_distance" 0.01; kb "reverse_winding" true; kb "remove_nan_points" false;
             kb "remove_unused_points" false; kb "delete_unused_groups" false] in
        check (equal_geometry (cook 1 typed)
            (Rdk.Clean.run ?epsilon ?consolidate_distance ?overlaps ~reverse_winding:true clean_geometry |> Rdk.Error.unguard |> get))
          "clean epsilon, consolidation and overlap modes match native";
        same_cook ("clean " ^ epsilon_choice ^ " " ^ consolidate_choice ^ " " ^ overlap_choice)
          ~typed
          ~factory:Nodes.Clean.factory ["epsilon_mode", Param.Choice_value epsilon_choice;
            "consolidate_mode", Choice_value consolidate_choice; "overlaps", Choice_value overlap_choice;
            "epsilon", Float_value 0.; "consolidate_distance", Float_value 0.01;
            "reverse_winding", Bool_value true; "remove_nan_points", Bool_value false;
            "remove_unused_points", Bool_value false; "delete_unused_groups", Bool_value false] clean_input)
        [Some Rdk.Clean.Keep_first_overlap, "Keep first"; Some Rdk.Clean.Delete_overlap_pairs, "Delete pairs"; None, "Auto"])
      [Some 0.01, "Explicit"; None, "Auto"])
    [Some 0., "Explicit"; None, "Auto"];
  let clean_attributes = List.fold_left (fun input owner ->
      sop ~inputs:[input] "set_float" [ks "owner" owner; ks "name" "unused"; kf "value" 2.]) clean_input
      ["Point"; "Vertex"; "Primitive"; "Detail"] in
  let deleted_attributes = Lisp_sop.node ~with_:["clean_attributes", (clean_attributes)] {|(sop/clean
   (sop/ext_clean_attributes)
   :point_attributes "unused*"
   :vertex_attributes "unused*"
   :primitive_attributes "unused*"
   :detail_attributes "unused*")|} in
  List.iter (fun owner -> check
      (Rdk.Geometry.find_attribute ~owner "unused" (cook 1 deleted_attributes) = None)
      "clean deletes selected attributes on every owner")
    [Rdk.Attribute.Point; Vertex; Primitive; Detail];
  same_cook "clean deletion patterns and unset names"
    ~typed:(Lisp_sop.node ~with_:["clean_attributes", (clean_attributes)] {|(sop/clean
   (sop/ext_clean_attributes)
   :point_attributes "unused*"
   :vertex_attributes "unused*"
   :primitive_attributes "unused*"
   :detail_attributes "unused*"
   :point_groups " "
   :vertex_groups " "
   :primitive_groups " "
   :edge_groups " ")|})
    ~factory:Nodes.Clean.factory ["point_attributes", Param.Text_value "unused*";
      "vertex_attributes", Text_value "unused*"; "primitive_attributes", Text_value "unused*";
      "detail_attributes", Text_value "unused*"; "point_groups", Text_value " ";
      "vertex_groups", Text_value " "; "primitive_groups", Text_value " "; "edge_groups", Text_value " "] clean_attributes;
  List.iter (fun make -> check (rejected make)
      "clean refuses invalid values at construction")
    [ (fun () -> Lisp_sop.node ~with_:["clean_input", (clean_input)] {|(sop/clean (sop/ext_clean_input) :epsilon -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["clean_input", (clean_input)] {|(sop/clean (sop/ext_clean_input) :consolidate_distance -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["clean_input", (clean_input)] {|(sop/clean (sop/ext_clean_input) :point_attributes "[")|});
      (fun () -> Lisp_sop.node ~with_:["clean_input", (clean_input)] {|(sop/clean (sop/ext_clean_input) :edge_groups "[")|}) ];
  List.iter (fun field -> check
      (Result.is_error (Node.apply_parameters cleaned [field, Param.Text_value "["]))
      ("clean refuses invalid " ^ field ^ " patterns during inspector rebuild"))
    ["point_attributes"; "vertex_attributes"; "primitive_attributes"; "detail_attributes";
     "point_groups"; "vertex_groups"; "primitive_groups"; "edge_groups"];
  (* LISP GAP: [sop/merge] ignores :source_attribute and :source_base (the lowering owns them), so merges
     with a tag are built from the factory. *)
  let tagged_merge ?(attribute = "source") base inputs = from_factory Nodes.Merge.factory
    ["source_attribute", Param.Text_value attribute; "source_base", Int_value base] inputs in
  let merge_inputs = List.init 5 (fun index -> Lisp_sop.node (Printf.sprintf {|(sop/line :points %d :origin [%s 0.0 0.0])|} ((index + 2)) ((Lisp_sop.float (float_of_int index))))) in
  List.iter (fun count ->
    let inputs = List.take count merge_inputs in
    (* The editor lowering tags a Lisp merge with provenance itself (sop/provenance_base), where the
       native factory carried the tag as its visible source_attribute: same bytes, not the same schema key. *)
    same_node ~same_key:false (Printf.sprintf "merge %d inputs" count)
      ~typed:(sop ~inputs "merge" [])
      ~catalog:(from_factory Nodes.Merge.factory ["source_attribute", Param.Text_value "__flow_src"] inputs);
    same_node (Printf.sprintf "merge %d tagged inputs" count)
      ~typed:(tagged_merge (-3) inputs)
      ~catalog:(from_factory Nodes.Merge.factory ["source_attribute", Param.Text_value "source";
        "source_base", Int_value (-3)] inputs)) [1; 3; 5];
  let merged = Node.relabel "ordered merge" (tagged_merge 7 merge_inputs) in
  cache_identity "merge all fields" merged;
  same_node "merge unset source name" ~typed:(tagged_merge ~attribute:" " 0 merge_inputs)
    ~catalog:(from_factory Nodes.Merge.factory ["source_attribute", Param.Text_value " "] merge_inputs);
  check (rejected (fun () -> Lisp_sop.node {|(sop/merge)|}))
    "merge requires its first rest input at construction";
  let initial_inputs = List.take 3 merge_inputs in
  let dynamic_merge = Node.relabel "ordered merge" (tagged_merge 7 initial_inputs) in
  let document = List.fold_left (fun document node -> Edit_graph.add_node node document |> get)
      Edit_graph.empty merge_inputs
      |> Edit_graph.add_node ~factory:Nodes.Merge.factory dynamic_merge |> get in
  let extra = List.nth merge_inputs 3 in
  let document = document
      |> Edit_graph.connect ~source:(Node.id extra) ~consumer:(Node.id dynamic_merge) ~input_index:3 |> get
      |> Edit_graph.disconnect ~consumer:(Node.id dynamic_merge) ~input_index:1 |> get in
  let rebuilt = Edit_graph.compile_node document ~node_id:(Node.id dynamic_merge) |> get in
  let retained_inputs = List.filteri (fun index _ -> index = 0 || index = 2 || index = 3) merge_inputs in
  same_node "merge rest append and sparse disconnection"
    ~typed:(tagged_merge 7 retained_inputs) ~catalog:rebuilt;
  check (Node.id rebuilt = Node.id dynamic_merge && Node.label rebuilt = "ordered merge")
    "merge rest rewiring preserves node identity and label";
  let edited = fst (Node.apply_parameters rebuilt
      ["source_base", Param.Int_value 11] |> get) in
  same_node "merge parameter rebuild retains rewired inputs"
    ~typed:(tagged_merge 11 retained_inputs) ~catalog:edited;
  let disconnected = Edit_graph.disconnect ~consumer:(Node.id dynamic_merge)
      ~input_index:0 document |> get in
  check (Result.is_error (Edit_graph.compile_node disconnected ~node_id:(Node.id dynamic_merge)))
    "merge refuses a disconnected required rest input";
  let platonic = Lisp_sop.node {|(sop/platonic)|} in
  same_generator "platonic Lisp defaults" ~typed:platonic ~factory:Nodes.Platonic.factory [];
  cache_identity "platonic all fields" platonic;
  List.iter (fun (kind, choice) ->
    List.iter (fun (normals, normal_choice) ->
      let typed = sop "platonic" [ks "kind" choice; ks "normals" normal_choice; kf "radius" 2.] in
      same_generator ("platonic " ^ choice ^ " " ^ normal_choice)
        ~typed ~factory:Nodes.Platonic.factory
        ["kind", Param.Choice_value choice; "normals", Choice_value normal_choice; "radius", Float_value 2.];
      check (equal_geometry (cook 1 typed)
          (Rdk.Parametric_generators.platonic ~kind ~normals ~orientation:Rdk.Parametric_generators.Platonic_y
             ~radius:2. () |> Result.get_ok))
        "platonic kind and normals match native")
      [Rdk.Parametric_generators.Platonic_no_normals, "None"; Platonic_point_normals, "Point";
        Platonic_vertex_normals, "Vertex"])
    [Rdk.Parametric_generators.Platonic_tetrahedron, "Tetrahedron"; Platonic_cube, "Cube";
      Platonic_octahedron, "Octahedron"; Platonic_icosahedron, "Icosahedron";
      Platonic_dodecahedron, "Dodecahedron"; Platonic_soccer_ball, "Soccer ball"];
  List.iter (fun choice ->
    List.iter (fun (rotation_order, order_choice) ->
      let typed = sop "platonic" [ks "orientation" choice; kv "axis" (Vec3.create 1. 2. 3.);
          kv "center" (Vec3.create 2. 3. 4.); kv "rotation" (Vec3.create 0.1 0.2 0.3);
          ks "rotation_order" order_choice; ks "face_groups" "faces"] in
      let orientation = match choice with
        | "X axis" -> Rdk.Parametric_generators.Platonic_x | "Y axis" -> Platonic_y | "Z axis" -> Platonic_z
        | _ -> Platonic_axis (Vec3.create 1. 2. 3.) in
      check (equal_geometry (cook 1 typed)
          (Rdk.Parametric_generators.platonic ~kind:Rdk.Parametric_generators.Platonic_dodecahedron
             ~normals:Rdk.Parametric_generators.Platonic_vertex_normals ~orientation ~center:(Vec3.create 2. 3. 4.)
             ~rotation:(Vec3.create 0.1 0.2 0.3) ~rotation_order ~face_groups:"faces" ~radius:1. () |> Result.get_ok))
        "platonic orientation and rotation order match native";
      same_generator ("platonic " ^ choice ^ " " ^ order_choice)
        ~typed ~factory:Nodes.Platonic.factory
        ["orientation", Param.Choice_value choice; "axis_x", Float_value 1.; "axis_y", Float_value 2.;
          "axis_z", Float_value 3.; "center_x", Float_value 2.; "center_y", Float_value 3.; "center_z", Float_value 4.;
          "rotation_x", Float_value 0.1; "rotation_y", Float_value 0.2; "rotation_z", Float_value 0.3;
          "rotation_order", Choice_value order_choice; "face_groups", Text_value "faces"])
      [Rdk.Parametric_generators.Platonic_xyz, "XYZ"; Platonic_xzy, "XZY"; Platonic_yxz, "YXZ";
        Platonic_yzx, "YZX"; Platonic_zxy, "ZXY"; Platonic_zyx, "ZYX"])
    ["X axis"; "Y axis"; "Z axis"; "Custom axis"];
  same_generator "platonic unset prefix" ~typed:(Lisp_sop.node {|(sop/platonic :face_groups " ")|})
    ~factory:Nodes.Platonic.factory ["face_groups", Param.Text_value " "];
  List.iter (fun make ->
    check (rejected make)
      "platonic refuses invalid values at construction")
    [ (fun () -> Lisp_sop.node {|(sop/platonic :radius 0.0)|}); (fun () -> Lisp_sop.node (Printf.sprintf {|(sop/platonic :orientation "Custom axis" :axis %s)|} ((Lisp_sop.vec3 Vec3.zero)))) ];
  let origins = Lisp_sop.node {|(sop/points)|} in
  same_generator "origin point generate defaults" ~typed:origins ~factory:Nodes.Point_generate.factory [];
  cache_identity "origin point generate all fields" origins;
  same_generator "origin point generate metadata" ~typed:(Lisp_sop.node {|(sop/points
   :points 7
   :generated_group "made"
   :source_point_attribute "from_point"
   :source_index_attribute "from_index")|})
    ~factory:Nodes.Point_generate.factory ["points", Param.Int_value 7;
      "generated_group", Text_value "made"; "source_point_attribute", Text_value "from_point";
      "source_index_attribute", Text_value "from_index"];
  same_generator "origin point generate unset group" ~typed:(Lisp_sop.node {|(sop/points :generated_group " ")|})
    ~factory:Nodes.Point_generate.factory ["generated_group", Param.Text_value " "];
  List.iter (fun make ->
    check (rejected make)
      "origin point generate refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node {|(sop/points :points 0)|});
      (fun () -> Lisp_sop.node {|(sop/points :points 51)|});
      (fun () -> Lisp_sop.node {|(sop/points :source_point_attribute " ")|});
      (fun () -> Lisp_sop.node {|(sop/points :source_index_attribute "P")|});
      (fun () -> Lisp_sop.node {|(sop/points :source_point_attribute "same" :source_index_attribute "same")|}) ];
  let color_input = Lisp_sop.node (Printf.sprintf {|(sop/line :origin %s :direction %s :length 2.0)|} ((Lisp_sop.vec3 Vec3.zero)) ((Lisp_sop.vec3 Vec3.unit_y))) in
  let profile = Lisp_sop.node {|(-> (sop/curve (list [1.0 -1.0 0.0] [2.0 0.0 0.0] [1.0 1.0 0.0]))
     (sop/group_range :owner "Primitives" :name "profile" :range_mode "From ends"))|} in
  let edge_input = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [3.0 0.0 0.0]))
     (sop/set_float :value 2.0)
     (sop/group_range :name "tip" :start 2 :end_ 2)
     (sop/group_range :name "all" :range_mode "From ends"))|} in
  let edge_transport = Lisp_sop.node ~with_:["edge_input", (edge_input)] {|(sop/edge_transport (sop/ext_edge_input))|} in
  same_cook "edge transport defaults" ~typed:edge_transport ~factory:Nodes.Edge_transport.factory [] edge_input;
  cache_identity ~companions:transport_cache_companions
    "edge transport all fields" edge_transport;
  List.iter (fun root_choice ->
    List.iter (fun direction_choice ->
      List.iter (fun (operation, choice) ->
        same_cook ("edge transport " ^ root_choice ^ " " ^ direction_choice ^ " " ^ choice)
          ~typed:(sop ~inputs:[edge_input] "edge_transport"
            [ks "roots" root_choice; ks "root_group" "tip"; ks "point_group" "all"; ks "direction" direction_choice;
             ks "operation" choice; ks "root_value" "Hold";
             kb "scale_by_edge_length" (operation = Rdk.Edge_transport.Transport_total)])
          ~factory:Nodes.Edge_transport.factory ["roots", Param.Choice_value root_choice;
            "root_group", Text_value "tip"; "point_group", Text_value "all";
            "direction", Choice_value direction_choice; "operation", Choice_value choice;
            "root_value", Choice_value "Hold";
            "scale_by_edge_length", Bool_value (operation = Rdk.Edge_transport.Transport_total)] edge_input)
        [Rdk.Edge_transport.Transport, "Transport"; Transport_from_root, "From root"; Transport_total, "Total";
          Transport_maximum, "Maximum"; Transport_minimum, "Minimum"])
      ["Forward"; "Backward"])
    ["First point"; "Last point"; "Root group"];
  List.iter (fun split_choice ->
    List.iter (fun merge_choice ->
      List.iter (fun choice ->
        same_cook ("edge transport " ^ split_choice ^ " " ^ merge_choice ^ " " ^ choice)
          ~typed:(sop ~inputs:[edge_input] "edge_transport"
            [ks "operation" "Total"; kb "integrate_constant" true; ks "split" split_choice; ks "merge" merge_choice;
             ks "normalization" choice; ks "attribute" "distance"])
          ~factory:Nodes.Edge_transport.factory ["operation", Param.Choice_value "Total";
            "integrate_constant", Bool_value true; "split", Choice_value split_choice;
            "merge", Choice_value merge_choice; "normalization", Choice_value choice;
            "attribute", Text_value "distance"] edge_input)
        ["None"; "Per component"; "Global"])
      ["Add"; "Maximum"; "Minimum"])
    ["Copy"; "Split"];
  same_cook "edge transport unset point group" ~typed:(Lisp_sop.node ~with_:["edge_input", (edge_input)] {|(sop/edge_transport (sop/ext_edge_input) :point_group " ")|})
    ~factory:Nodes.Edge_transport.factory ["point_group", Param.Text_value " "] edge_input;
  List.iter (fun make ->
    check (rejected make)
      "edge transport refuses invalid attributes at construction")
    [ (fun () -> Lisp_sop.node ~with_:["edge_input", (edge_input)] {|(sop/edge_transport (sop/ext_edge_input) :attribute "")|});
      (fun () -> Lisp_sop.node ~with_:["edge_input", (edge_input)] {|(sop/edge_transport (sop/ext_edge_input) :attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["edge_input", (edge_input)] {|(sop/edge_transport (sop/ext_edge_input) :integrate_constant true)|});
      (fun () -> Lisp_sop.node ~with_:["edge_input", (edge_input)] {|(sop/edge_transport (sop/ext_edge_input) :scale_by_edge_length true)|});
      (fun () -> Lisp_sop.node ~with_:["edge_input", (edge_input)] {|(sop/edge_transport_curves (sop/ext_edge_input) :integrate_constant true)|});
      (fun () -> Lisp_sop.node ~with_:["edge_input", (edge_input)] {|(sop/edge_transport_curves (sop/ext_edge_input) :scale_by_edge_length true)|});
      (fun () -> Lisp_sop.node ~with_:["edge_input", (edge_input)] {|(sop/edge_transport_parent (sop/ext_edge_input) :integrate_constant true)|});
      (fun () -> Lisp_sop.node ~with_:["edge_input", (edge_input)] {|(sop/edge_transport_parent (sop/ext_edge_input) :scale_by_edge_length true)|}) ];
  let revolved = Lisp_sop.node ~with_:["profile", (profile)] {|(sop/revolve (sop/ext_profile))|} in
  same_cook "revolve defaults" ~typed:revolved ~factory:Nodes.Revolve.factory [] profile;
  cache_identity "revolve all fields" revolved;
  List.iter (fun type_choice ->
    List.iter (fun choice ->
      same_cook ("revolve " ^ type_choice ^ " " ^ choice) ~typed:(sop ~inputs:[profile] "revolve"
          [ks "revolve_type" type_choice; ks "connectivity" choice; ks "group" "profile"; ki "divisions" 8;
           kf "start_angle" 0.2; kf "end_angle" 2.5; kb "reverse_cross_sections" true;
           kv "origin" (Vec3.create 0.1 0.2 0.3); kv "axis" (Vec3.create 1. 2. 3.); ks "uv_attribute" "st"])
        ~factory:Nodes.Revolve.factory ["revolve_type", Param.Choice_value type_choice;
          "connectivity", Choice_value choice; "group", Text_value "profile"; "divisions", Int_value 8;
          "start_angle", Float_value 0.2; "end_angle", Float_value 2.5; "reverse_cross_sections", Bool_value true;
          "origin_x", Float_value 0.1; "origin_y", Float_value 0.2; "origin_z", Float_value 0.3;
          "axis_x", Float_value 1.; "axis_y", Float_value 2.; "axis_z", Float_value 3.; "uv_attribute", Text_value "st"] profile)
      ["Points"; "Rows"; "Columns"; "Rows and columns"; "Quads"; "Triangles"; "Alternating triangles"; "Reverse triangles"])
    ["Closed"; "Open arc"];
  same_cook "revolve caps" ~typed:(Lisp_sop.node ~with_:["profile", (profile)] {|(sop/revolve (sop/ext_profile) :caps true :cap_group "endcaps")|})
    ~factory:Nodes.Revolve.factory ["caps", Param.Bool_value true; "cap_group", Text_value "endcaps"] profile;
  same_cook "revolve unset names" ~typed:(Lisp_sop.node ~with_:["profile", (profile)] {|(sop/revolve (sop/ext_profile) :group " " :cap_group " " :uv_attribute " ")|})
    ~factory:Nodes.Revolve.factory ["group", Param.Text_value " "; "cap_group", Text_value " "; "uv_attribute", Text_value " "] profile;
  List.iter (fun make ->
    check (rejected make)
      "revolve refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["profile", (profile)] {|(sop/revolve (sop/ext_profile) :divisions 2)|});
      (fun () -> Lisp_sop.node ~with_:["profile", (profile)] {|(sop/revolve (sop/ext_profile) :revolve_type "Open arc" :end_angle 0.0)|});
      (fun () -> Lisp_sop.node ~with_:["profile", (profile)] (Printf.sprintf {|(sop/revolve (sop/ext_profile) :axis %s)|} ((Lisp_sop.vec3 Vec3.zero))));
      (fun () -> Lisp_sop.node ~with_:["profile", (profile)] {|(sop/revolve (sop/ext_profile) :caps true :connectivity "Points")|});
      (fun () -> Lisp_sop.node ~with_:["profile", (profile)] {|(sop/revolve (sop/ext_profile) :caps true :revolve_type "Open arc")|});
      (fun () -> Lisp_sop.node ~with_:["profile", (profile)] {|(sop/revolve (sop/ext_profile) :uv_attribute "P")|}) ];
  let rewire_input = Lisp_sop.node ~with_:["in720", (Lisp_sop.snapshot ((Sources.rewire 24)))] {|(-> (sop/set_int (sop/ext_in720) :owner "Vertex" :name "target" :value -1)
     (sop/set_int :owner "Primitive" :name "target" :value -1)
     (sop/group_range :owner "Vertices" :name "selected" :end_ 2)
     (sop/group_range :owner "Primitives" :name "selected" :end_ 0)
     (sop/group_edges :name "selected"))|} in
  let rewired = Lisp_sop.node ~with_:["rewire_input", (rewire_input)] {|(sop/rewire_vertices (sop/ext_rewire_input))|} in
  same_cook "rewire vertices defaults" ~typed:rewired ~factory:Nodes.Rewire_vertices.factory [] rewire_input;
  cache_identity ~companions:["recursive", ["owner", Param.Choice_value "Point"]]
    "rewire vertices all fields" rewired;
  List.iter (fun owner_choice ->
    List.iter (fun choice ->
      same_cook ("rewire vertices " ^ owner_choice ^ " " ^ choice)
        ~typed:(sop ~inputs:[rewire_input] "rewire_vertices"
          [ks "owner" owner_choice; ks "selection_owner" choice; ks "selection" "selected";
           kb "delete_target_attribute" false; kb "keep_unused_points" true; ks "original_point_attribute" "original"])
        ~factory:Nodes.Rewire_vertices.factory ["owner", Param.Choice_value owner_choice;
          "selection_owner", Choice_value choice; "selection", Text_value "selected";
          "delete_target_attribute", Bool_value false; "keep_unused_points", Bool_value true;
          "original_point_attribute", Text_value "original"] rewire_input)
      ["Point"; "Vertex"; "Primitive"; "Edge"])
    ["Point"; "Vertex"; "Primitive"];
  same_cook "rewire vertices recursive" ~typed:(Lisp_sop.node ~with_:["rewire_input", (rewire_input)] {|(sop/rewire_vertices (sop/ext_rewire_input) :owner "Point" :recursive true)|})
    ~factory:Nodes.Rewire_vertices.factory ["owner", Param.Choice_value "Point"; "recursive", Bool_value true] rewire_input;
  same_cook "rewire vertices unset names" ~typed:(Lisp_sop.node ~with_:["rewire_input", (rewire_input)] {|(sop/rewire_vertices (sop/ext_rewire_input) :selection " " :original_point_attribute " ")|})
    ~factory:Nodes.Rewire_vertices.factory ["selection", Param.Text_value " "; "original_point_attribute", Text_value " "] rewire_input;
  List.iter (fun make ->
    check (rejected make)
      "rewire vertices refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["rewire_input", (rewire_input)] {|(sop/rewire_vertices (sop/ext_rewire_input) :target_attribute " ")|});
      (fun () -> sop ~inputs:[rewire_input] "rewire_vertices" [ks "owner" "Detail"]);
      (fun () -> Lisp_sop.node ~with_:["rewire_input", (rewire_input)] {|(sop/rewire_vertices (sop/ext_rewire_input) :recursive true)|});
      (fun () -> Lisp_sop.node ~with_:["rewire_input", (rewire_input)] {|(sop/rewire_vertices (sop/ext_rewire_input) :original_point_attribute "N")|}) ];
  let rest = Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/rest_position (sop/ext_color_input))|} in
  same_cook ~optional_inputs:[Some color_input; None] "rest position defaults" ~typed:rest
    ~factory:Nodes.Rest_position.factory [] color_input;
  cache_identity "rest position all fields" rest;
  let rest_input = Lisp_sop.node ~with_:["color_input", (color_input)] (Printf.sprintf {|(-> (sop/set_vector (sop/ext_color_input) :name "N" :value %s)
     (sop/set_vector :name "normal" :value %s)
     (sop/rest_position :normals "Always"))|} ((Lisp_sop.vec3 Vec3.unit_y)) ((Lisp_sop.vec3 Vec3.unit_z))) in
  List.iter (fun choice ->
    same_cook ~optional_inputs:[Some rest_input; None] ("rest position " ^ choice)
      ~typed:(sop ~inputs:[rest_input] "rest_position" [ks "mode" choice]) ~factory:Nodes.Rest_position.factory
      ["mode", Param.Choice_value choice] rest_input)
    ["Store"; "Extract"; "Swap"];
  let rest_reference = Lisp_sop.node ~with_:["color_input", (color_input)] (Printf.sprintf {|(sop/set_vector (sop/ext_color_input) :name "normal" :value %s)|} ((Lisp_sop.vec3 Vec3.unit_x))) in
  List.iter (fun choice ->
    same_cook ~optional_inputs:[Some rest_input; Some rest_reference] ("rest position normals " ^ choice)
      ~typed:(sop ~inputs:[rest_input; rest_reference] "rest_position"
        [ks "normals" choice; ks "rest_attribute" "reference_rest"; ks "normal_attribute" "normal";
         ks "rest_normal_attribute" "reference_normal"])
      ~factory:Nodes.Rest_position.factory ["normals", Param.Choice_value choice;
        "rest_attribute", Text_value "reference_rest"; "normal_attribute", Text_value "normal";
        "rest_normal_attribute", Text_value "reference_normal"] rest_input)
    ["None"; "If present"; "Always"];
  List.iter (fun make ->
    check (rejected make)
      "rest position refuses invalid attribute names at construction")
    [ (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/rest_position (sop/ext_color_input) :rest_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/rest_position (sop/ext_color_input) :normals "Always" :normal_attribute " ")|}) ];
  let float_set = Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_float (sop/ext_color_input))|} and int_set = Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_int (sop/ext_color_input))|} in
  same_cook "set float defaults" ~typed:float_set ~factory:Nodes.Set_float.factory [] color_input;
  same_cook "set int defaults" ~typed:int_set ~factory:Nodes.Set_int.factory [] color_input;
  cache_identity "set float all fields" float_set;
  cache_identity "set int all fields" int_set;
  let vector_set = Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_vector (sop/ext_color_input))|} in
  same_cook "set vector defaults" ~typed:vector_set ~factory:Nodes.Set_vector.factory [] color_input;
  cache_identity "set vector all fields" vector_set;
  let oriented = Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_orient (sop/ext_color_input))|} in
  let transformed = Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_transform (sop/ext_color_input))|} in
  same_cook "set transform defaults" ~typed:transformed ~factory:Nodes.Set_transform.factory [] color_input;
  cache_identity ~changes:["m30", Param.Float_value 1e-13; "m31", Float_value 1e-13;
      "m32", Float_value 1e-13; "m33", Float_value (1. +. 1e-13)]
    "set transform all fields" transformed;
  same_cook "set transform matrix fields" ~typed:(Lisp_sop.node ~with_:["color_input", (color_input)] (Printf.sprintf {|(sop/set_transform
   (sop/ext_color_input)
   :m00 2.0
   :m01 0.1
   :m02 0.2
   :m03 3.0
   :m10 0.3
   :m11 4.0
   :m12 0.4
   :m13 5.0
   :m20 0.5
   :m21 0.6
   :m22 6.0
   :m23 7.0
   :m30 1e-13
   :m31 1e-13
   :m32 1e-13
   :m33 %s)|} ((Lisp_sop.float (1. +. 1e-13))))) ~factory:Nodes.Set_transform.factory
    ["m00", Param.Float_value 2.; "m01", Float_value 0.1; "m02", Float_value 0.2;
      "m03", Float_value 3.; "m10", Float_value 0.3; "m11", Float_value 4.;
      "m12", Float_value 0.4; "m13", Float_value 5.; "m20", Float_value 0.5;
      "m21", Float_value 0.6; "m22", Float_value 6.; "m23", Float_value 7.;
      "m30", Float_value 1e-13; "m31", Float_value 1e-13; "m32", Float_value 1e-13;
      "m33", Float_value (1. +. 1e-13)] color_input;
  List.iter (fun make ->
    check (rejected make)
      "set transform refuses invalid matrices at construction")
    [ (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_transform (sop/ext_color_input) :m30 0.1)|});
      (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_transform (sop/ext_color_input) :m33 0.0)|}) ];
  same_cook "set orient defaults" ~typed:oriented ~factory:Nodes.Set_orient.factory [] color_input;
  cache_identity "set orient all fields" oriented;
  same_cook "set orient quaternion channels" ~typed:(Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_orient (sop/ext_color_input) :x 0.2 :y 0.3 :z 0.4 :w 0.5)|})
    ~factory:Nodes.Set_orient.factory ["x", Param.Float_value 0.2; "y", Float_value 0.3;
      "z", Float_value 0.4; "w", Float_value 0.5] color_input;
  same_cook "set orient zero normalization" ~typed:(Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_orient (sop/ext_color_input) :w 0.0)|})
    ~factory:Nodes.Set_orient.factory ["w", Param.Float_value 0.] color_input;
  List.iter (fun choice ->
    same_cook ("set float " ^ choice) ~typed:(sop ~inputs:[color_input] "set_float" [ks "owner" choice; ks "name" "custom"; kf "value" 0.75])
      ~factory:Nodes.Set_float.factory ["owner", Param.Choice_value choice;
        "name", Text_value "custom"; "value", Float_value 0.75] color_input;
    same_cook ("set int " ^ choice) ~typed:(sop ~inputs:[color_input] "set_int" [ks "owner" choice; ks "name" "custom"; ki "value" 37])
      ~factory:Nodes.Set_int.factory ["owner", Param.Choice_value choice;
        "name", Text_value "custom"; "value", Int_value 37] color_input;
    same_cook ("set vector " ^ choice) ~typed:(sop ~inputs:[color_input] "set_vector"
        [ks "owner" choice; ks "name" "custom"; kv "value" (Vec3.create 0.2 0.3 0.4)]) ~factory:Nodes.Set_vector.factory
      ["owner", Param.Choice_value choice; "name", Text_value "custom";
        "x", Float_value 0.2; "y", Float_value 0.3; "z", Float_value 0.4] color_input)
    ["Point"; "Vertex"; "Primitive"; "Detail"];
  List.iter (fun make ->
    check (rejected make)
      "attribute setters refuse invalid parameters at construction")
    [ (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_float (sop/ext_color_input) :name " ")|});
      (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_float (sop/ext_color_input) :name "P")|});
      (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_int (sop/ext_color_input) :name " ")|});
      (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_int (sop/ext_color_input) :name "P")|});
      (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_vector (sop/ext_color_input) :name "P")|}) ];
  let colored = Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/color_by_height (sop/ext_color_input))|} in
  let constant_color = Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_color (sop/ext_color_input))|} in
  same_cook "set color defaults" ~typed:constant_color ~factory:Nodes.Set_color.factory [] color_input;
  cache_identity "set color all fields" constant_color;
  let grouped_colors = Lisp_sop.node ~with_:["color_input", (color_input)] {|(-> (sop/group_range (sop/ext_color_input) :name "selection" :end_ 0)
     (sop/group_range :owner "Vertices" :name "selection" :end_ 0)
     (sop/group_range :owner "Primitives" :name "selection" :end_ 0))|} in
  let enumerated = Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/enumerate (sop/ext_color_input))|} in
  same_cook "enumerate defaults" ~typed:enumerated ~factory:Nodes.Enumerate.factory [] color_input;
  cache_identity "enumerate all fields" enumerated;
  let enumeration_input = Lisp_sop.node ~with_:["grouped_colors", (grouped_colors)] {|(-> (sop/set_int (sop/ext_grouped_colors) :name "piece" :value 1)
     (sop/set_int :owner "Vertex" :name "piece" :value 2)
     (sop/set_int :owner "Primitive" :name "piece" :value 3))|} in
  let enumeration_geometry = cook 1 enumeration_input in
  List.iter (fun choice ->
    List.iter (fun (mode, mode_choice) ->
      let typed = sop ~inputs:[enumeration_input] "enumerate"
          [ks "owner" choice; ks "name" "number"; ks "group" "selection"; ki "start" 5; ki "step" (-2);
           ks "storage" "Text"; ks "prefix" "part_"; ks "piece_attribute" "piece"; ks "mode" mode_choice] in
      let owner,group_owner = match choice with
        | "Point" -> Rdk.Attribute.Point,Rdk.Group.Point | "Vertex" -> Rdk.Attribute.Vertex,Rdk.Group.Vertex
        | _ -> Rdk.Attribute.Primitive,Rdk.Group.Primitive in
      let selection = Rdk.Geometry.find_group ~owner:group_owner "selection" enumeration_geometry |> Option.get in
      check (equal_geometry (cook 1 typed)
          (Rdk.Attribute_ops.enumerate ~selection ~start:5 ~step:(-2) ~storage:(Rdk.Attribute_ops.Text {prefix="part_"})
             ~piece_attribute:"piece" ~mode ~owner ~name:"number" enumeration_geometry |> Result.get_ok))
        "enumerate owner and mode match native";
      same_cook ("enumerate " ^ choice ^ " " ^ mode_choice) ~typed
        ~factory:Nodes.Enumerate.factory ["owner", Param.Choice_value choice;
          "name", Text_value "number"; "group", Text_value "selection";
          "start", Int_value 5; "step", Int_value (-2); "storage", Choice_value "Text";
          "prefix", Text_value "part_"; "piece_attribute", Text_value "piece";
          "mode", Choice_value mode_choice] enumeration_input)
      [Rdk.Attribute_ops.Enumerate_piece_elements, "Elements within pieces"; Enumerate_pieces, "Pieces"])
    ["Point"; "Vertex"; "Primitive"];
  same_cook "enumerate unset names" ~typed:(Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/enumerate (sop/ext_color_input) :group " " :piece_attribute " ")|})
    ~factory:Nodes.Enumerate.factory ["group", Param.Text_value " ";
      "piece_attribute", Text_value " "] color_input;
  List.iter (fun make ->
    check (rejected make)
      "enumerate refuses invalid owners and names at construction")
    [ (fun () -> sop ~inputs:[color_input] "enumerate" [ks "owner" "Detail"]);
      (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/enumerate (sop/ext_color_input) :name " ")|});
      (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/enumerate (sop/ext_color_input) :name "P")|}) ];
  List.iter (fun choice ->
    let group = if choice = "Detail" then "" else "selection" in
    same_cook ("set color " ^ choice) ~typed:(sop ~inputs:[grouped_colors] "set_color"
        [ks "owner" choice; ks "group" group; kv "color" (Vec3.create 0.2 0.3 0.4); kf "alpha" 0.5])
      ~factory:Nodes.Set_color.factory ["owner", Param.Choice_value choice;
        "group", Text_value group; "color_r", Float_value 0.2;
        "color_g", Float_value 0.3; "color_b", Float_value 0.4; "alpha", Float_value 0.5] grouped_colors)
    ["Point"; "Vertex"; "Primitive"; "Detail"];
  same_cook "set color float alias" ~typed:(sop ~inputs:[color_input] "set_color" [kv "color" Vec3.unit_y])
    ~factory:Nodes.Set_color.factory ["color_r", Param.Float_value 0.; "color_b", Float_value 0.] color_input;
  same_cook "set color unset group" ~typed:(Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_color (sop/ext_color_input) :group " ")|})
    ~factory:Nodes.Set_color.factory ["group", Param.Text_value " "] color_input;
  List.iter (fun make ->
    check (rejected make)
      "set color refuses invalid channels or detail selections at construction")
    [ (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_color (sop/ext_color_input) :alpha 1.1)|});
      (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/set_color (sop/ext_color_input) :owner "Detail" :group "selection")|}) ];
  same_cook "color by height Lisp defaults" ~typed:colored
    ~factory:Nodes.Color_by_height.factory [] color_input;
  cache_identity "color by height all fields" colored;
  same_cook "color by height rgba" ~typed:(Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/color_by_height
   (sop/ext_color_input)
   :low_red 10
   :low_green 20
   :low_blue 30
   :low_alpha 40
   :high_red 50
   :high_green 60
   :high_blue 70
   :high_alpha 80)|})
    ~factory:Nodes.Color_by_height.factory ["low_red", Param.Int_value 10;
      "low_green", Int_value 20; "low_blue", Int_value 30; "low_alpha", Int_value 40;
      "high_red", Int_value 50; "high_green", Int_value 60;
      "high_blue", Int_value 70; "high_alpha", Int_value 80] color_input;
  List.iter (fun make ->
    check (rejected make)
      "color by height refuses invalid channels at construction")
    [ (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/color_by_height (sop/ext_color_input) :low_red -1)|});
      (fun () -> Lisp_sop.node ~with_:["color_input", (color_input)] {|(sop/color_by_height (sop/ext_color_input) :high_alpha 256)|}) ];
  let uv_source = Sources.point_split () in
  let point_uv = Rdk.Packed.Float2.of_owned ~x:[|0.; 1.; 1.; 0.|]
      ~y:[|0.; 0.; 1.; 1.|] |> Result.get_ok in
  let point_uv = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"uv"
      (Rdk.Attribute.Float2 point_uv) |> Result.get_ok in
  let uv_input = Lisp_sop.snapshot ((Rdk.Geometry.with_attribute point_uv uv_source |> Result.get_ok))
      |> group_indices "Points" "selection" [|0; 2|]
      |> group_indices "Vertices" "selection" [|0; 2|]
      |> group_indices "Primitives" "selection" [|0|]
      |> edge_group "selection" in
  let polyframed = Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/polyframe (sop/ext_uv_input))|} in
  same_cook "polyframe defaults" ~typed:polyframed ~factory:Nodes.Polyframe.factory [] uv_input;
  cache_identity "polyframe all fields" polyframed;
  List.iter (fun choice ->
    same_cook ("polyframe style " ^ choice) ~typed:(sop ~inputs:[uv_input] "polyframe" [ks "style" choice])
      ~factory:Nodes.Polyframe.factory ["style", Param.Choice_value choice] uv_input)
    ["First edge"; "Two edges"; "Primitive centroid"; "Texture UV"; "Texture UV gradient"; "Attribute gradient"];
  List.iter (fun choice ->
    same_cook ("polyframe selection " ^ choice) ~typed:(sop ~inputs:[uv_input] "polyframe"
        [ks "group_owner" choice; ks "group" "selection"; ks "style" "Texture UV gradient"; ks "style_attribute" "uv";
         kb "orthogonal" true; kb "left_handed" true; ks "normal_attribute" "frame_n";
         ks "tangent_attribute" "frame_u"; ks "bitangent_attribute" "frame_v"])
      ~factory:Nodes.Polyframe.factory ["group_owner", Param.Choice_value choice;
        "group", Text_value "selection"; "style", Choice_value "Texture UV gradient";
        "style_attribute", Text_value "uv"; "orthogonal", Bool_value true; "left_handed", Bool_value true;
        "normal_attribute", Text_value "frame_n"; "tangent_attribute", Text_value "frame_u";
        "bitangent_attribute", Text_value "frame_v"] uv_input)
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  same_cook "polyframe disabled outputs and unset group" ~typed:(Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/polyframe
   (sop/ext_uv_input)
   :group " "
   :tangent_attribute ""
   :bitangent_attribute "")|}) ~factory:Nodes.Polyframe.factory
    ["group", Param.Text_value " "; "tangent_attribute", Text_value "";
      "bitangent_attribute", Text_value ""] uv_input;
  List.iter (fun make ->
    check (rejected make)
      "polyframe refuses invalid output names at construction")
    [ (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/polyframe (sop/ext_uv_input) :normal_attribute " ")|});
      (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/polyframe (sop/ext_uv_input) :tangent_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/polyframe (sop/ext_uv_input) :tangent_attribute "N")|});
      (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/polyframe (sop/ext_uv_input) :style "Attribute gradient" :style_attribute " ")|}) ];
  let uv_transformed = Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/uv_transform (sop/ext_uv_input))|} in
  let connected = Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/connectivity (sop/ext_uv_input))|} in
  same_cook "connectivity defaults" ~typed:connected ~factory:Nodes.Connectivity.factory [] uv_input;
  cache_identity ~companions:["point_group", ["owner", Param.Choice_value "Points"]]
    "connectivity all fields" connected;
  List.iter (fun (_, choice) ->
    let point_group = if choice = "Points" then "selection" else "" in
    same_cook ("connectivity text " ^ choice) ~typed:(sop ~inputs:[uv_input] "connectivity"
        [ks "owner" choice; ks "primitive_group" "seam_face"; ks "point_group" point_group; ks "name" "island";
         ks "output" "Text"; ks "text_prefix" "part_"]) ~factory:Nodes.Connectivity.factory
      ["owner", Param.Choice_value choice; "primitive_group", Text_value "seam_face";
        "point_group", Text_value point_group; "name", Text_value "island";
        "output", Choice_value "Text"; "text_prefix", Text_value "part_"] uv_input)
    [Rdk.Analysis.Connectivity_points, "Points"; Connectivity_primitives, "Primitives"];
  let seam_input = Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/group_edges (sop/ext_uv_input) :name "seams")|} in
  same_cook "connectivity edge seams" ~typed:(Lisp_sop.node ~with_:["seam_input", (seam_input)] {|(sop/connectivity (sop/ext_seam_input) :seam_group "seams")|})
    ~factory:Nodes.Connectivity.factory ["seam_group", Param.Text_value "seams"] seam_input;
  same_cook "connectivity uv seams" ~typed:(Lisp_sop.node ~with_:["seam_input", (seam_input)] {|(sop/connectivity (sop/ext_seam_input) :uv_attribute "uv")|})
    ~factory:Nodes.Connectivity.factory ["uv_attribute", Param.Text_value "uv"] seam_input;
  same_cook "connectivity unset names" ~typed:(Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/connectivity
   (sop/ext_uv_input)
   :primitive_group " "
   :point_group " "
   :seam_group " "
   :uv_attribute " "
   :name " ")|})
    ~factory:Nodes.Connectivity.factory ["primitive_group", Param.Text_value " ";
      "point_group", Text_value " "; "seam_group", Text_value " ";
      "uv_attribute", Text_value " "; "name", Text_value " "] uv_input;
  check (rejected (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/connectivity (sop/ext_uv_input) :owner "Points" :name "P")|})) "connectivity refuses canonical P at construction";
  List.iter (fun make ->
    check (rejected make)
      "connectivity refuses incompatible selection modes at construction")
    [ (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/connectivity (sop/ext_uv_input) :point_group "selection")|});
      (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/connectivity (sop/ext_uv_input) :owner "Points" :uv_attribute "uv")|});
      (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/connectivity (sop/ext_uv_input) :seam_group "seams" :uv_attribute "uv")|}) ];
  let uv_projected = Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/uv_project (sop/ext_uv_input))|} in
  same_cook "uv project Lisp defaults" ~typed:uv_projected
    ~factory:Nodes.Uv_project.factory [] uv_input;
  cache_identity "uv project all fields" uv_projected;
  List.iter (fun choice ->
    same_cook ("uv project " ^ choice) ~typed:(sop ~inputs:[uv_input] "uv_project"
        [ks "projection" choice; ks "name" "mapped"; ks "group" "seam_face"; kv "origin" (Vec3.create 0.1 0.2 0.3);
         kv "axis" Vec3.unit_z; kv "seam" Vec3.unit_y; kv "planar_u" (Vec3.create 2. 0. 0.);
         kv "planar_v" (Vec3.create 0. 2. 0.); kf "height" 2.; kf "u_min" 0.1; kf "u_max" 0.9; kf "v_min" 0.2;
         kf "v_max" 0.8; kb "fix_seams" false; kb "fix_poles" false])
      ~factory:Nodes.Uv_project.factory ["projection", Param.Choice_value choice;
        "name", Text_value "mapped"; "group", Text_value "seam_face";
        "origin_x", Float_value 0.1; "origin_y", Float_value 0.2; "origin_z", Float_value 0.3;
        "axis_y", Float_value 0.; "axis_z", Float_value 1.;
        "seam_x", Float_value 0.; "seam_y", Float_value 1.;
        "planar_u_x", Float_value 2.; "planar_v_y", Float_value 2.; "planar_v_z", Float_value 0.;
        "height", Float_value 2.; "u_min", Float_value 0.1; "u_max", Float_value 0.9;
        "v_min", Float_value 0.2; "v_max", Float_value 0.8;
        "fix_seams", Bool_value false; "fix_poles", Bool_value false] uv_input)
    ["Planar"; "Cylindrical"; "Spherical"];
  same_cook "uv project unset group" ~typed:(Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/uv_project (sop/ext_uv_input) :group " ")|})
    ~factory:Nodes.Uv_project.factory ["group", Param.Text_value " "] uv_input;
  List.iter (fun make ->
    check (rejected make)
      "uv project refuses invalid parameters at construction")
    [ (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] (Printf.sprintf {|(sop/uv_project (sop/ext_uv_input) :planar_u %s)|} ((Lisp_sop.vec3 Vec3.zero))));
      (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] (Printf.sprintf {|(sop/uv_project (sop/ext_uv_input) :planar_v %s)|} ((Lisp_sop.vec3 Vec3.unit_x))));
      (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/uv_project (sop/ext_uv_input) :projection "Cylindrical" :height 0.0)|});
      (fun () -> Lisp_sop.node ~with_:["uv_input", (uv_input)] (Printf.sprintf {|(sop/uv_project (sop/ext_uv_input) :projection "Spherical" :seam %s)|} ((Lisp_sop.vec3 Vec3.unit_y)))) ];
  same_cook "uv transform defaults" ~typed:uv_transformed
    ~factory:Nodes.Uv_transform.factory [] uv_input;
  cache_identity "uv transform all fields" uv_transformed;
  List.iter (fun choice ->
    same_cook ("uv transform " ^ choice) ~typed:(sop ~inputs:[uv_input] "uv_transform"
        [ks "owner" choice; ks "group" "selection"; kf "translate_u" 0.2; kf "translate_v" 0.3; kf "scale_u" 2.;
         kf "scale_v" 3.; kf "angle" 0.4; kf "pivot_u" 0.1; kf "pivot_v" 0.6])
      ~factory:Nodes.Uv_transform.factory ["owner", Param.Choice_value choice;
        "group", Text_value "selection"; "translate_u", Float_value 0.2;
        "translate_v", Float_value 0.3; "scale_u", Float_value 2.; "scale_v", Float_value 3.;
        "angle", Float_value 0.4; "pivot_u", Float_value 0.1; "pivot_v", Float_value 0.6] uv_input)
    ["Point"; "Vertex"];
  same_cook "uv transform unset group" ~typed:(Lisp_sop.node ~with_:["uv_input", (uv_input)] {|(sop/uv_transform (sop/ext_uv_input) :group " ")|})
    ~factory:Nodes.Uv_transform.factory ["group", Param.Text_value " "] uv_input;
  List.iter (fun make ->
    check (rejected make)
      "uv transform refuses invalid parameters at construction")
    [ (fun () -> sop ~inputs:[uv_input] "uv_transform" [ks "owner" "Primitive"]) ];
  let normal_input = Lisp_sop.node {|(-> (sop/box :normals "Auto" :consolidate_points true)
     (sop/group_range :name "selection" :end_ 3)
     (sop/group_range :owner "Vertices" :name "selection" :end_ 3)
     (sop/group_range :owner "Primitives" :name "selection" :end_ 0)
     (sop/group_edges :name "selection"))|} in
  let bounded = Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/bound (sop/ext_normal_input))|} in
  let distance_input = Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(-> (sop/group_range (sop/ext_normal_input) :name "start" :end_ 0)
     (sop/group_range :owner "Vertices" :name "start" :end_ 0)
     (sop/group_range :owner "Primitives" :name "start" :end_ 0)
     (sop/group_edges :name "start"))|} in
  let along = Lisp_sop.node ~with_:["distance_input", (distance_input)] {|(sop/distance_along_geometry (sop/ext_distance_input))|} in
  let distance_reference = (let migration_translation = Vec3.create 2. 3. 4. in
Lisp_sop.node ~with_:["normal_input", (normal_input)] (Printf.sprintf {|(sop/transform
   (sop/ext_normal_input)
   :mode "Matrix"
   :m03 %s
   :m13 %s
   :m23 %s)|} ((Lisp_sop.float migration_translation.Vec3.x)) ((Lisp_sop.float migration_translation.Vec3.y)) ((Lisp_sop.float migration_translation.Vec3.z)))) in
  let geometry_distance = Lisp_sop.node ~with_:["normal_input", (normal_input); "distance_reference", (distance_reference)] {|(sop/distance_from_geometry (sop/ext_normal_input) (sop/ext_distance_reference))|} in
  same_cook ~inputs:[distance_reference] "distance from geometry defaults" ~typed:geometry_distance
    ~factory:Nodes.Distance_from_geometry.factory [] normal_input;
  cache_identity "distance from geometry all fields" geometry_distance;
  List.iter (fun choice ->
    List.iter (fun (_, reference_choice) ->
      same_cook ~inputs:[distance_reference] ("distance from geometry " ^ choice ^ " " ^ reference_choice)
        ~typed:(sop ~inputs:[normal_input; distance_reference] "distance_from_geometry"
          [ks "affected_owner" choice; ks "affected_group" "selection"; ks "reference_owner" choice;
           ks "reference_group" "selection"; ks "reference_kind" reference_choice; ks "falloff" "Quadratic";
           ks "radius_mode" "Fixed"; kf "radius" 3.; ks "distance_attribute" "nearest"; ks "mask_attribute" "mask"])
        ~factory:Nodes.Distance_from_geometry.factory ["affected_owner", Param.Choice_value choice;
          "affected_group", Text_value "selection"; "reference_owner", Choice_value choice;
          "reference_group", Text_value "selection"; "reference_kind", Choice_value reference_choice;
          "falloff", Choice_value "Quadratic"; "radius_mode", Choice_value "Fixed"; "radius", Float_value 3.;
          "distance_attribute", Text_value "nearest"; "mask_attribute", Text_value "mask"] normal_input)
      [Rdk.Transform_ops.Distance_reference_points, "Points"; Distance_reference_primitives, "Primitives"])
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  same_cook ~inputs:[distance_reference] "distance from geometry mask only and unset groups"
    ~typed:(Lisp_sop.node ~with_:["normal_input", (normal_input); "distance_reference", (distance_reference)] {|(sop/distance_from_geometry
   (sop/ext_normal_input)
   (sop/ext_distance_reference)
   :affected_group " "
   :reference_group " "
   :distance_attribute " "
   :mask_attribute "mask"
   :falloff "Cubic")|})
    ~factory:Nodes.Distance_from_geometry.factory ["affected_group", Param.Text_value " ";
      "reference_group", Text_value " "; "distance_attribute", Text_value " ";
      "mask_attribute", Text_value "mask"; "falloff", Choice_value "Cubic"] normal_input;
  List.iter (fun make ->
    check (rejected make)
      "distance from geometry refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input); "distance_reference", (distance_reference)] {|(sop/distance_from_geometry
   (sop/ext_normal_input)
   (sop/ext_distance_reference)
   :radius_mode "Fixed"
   :radius 0.0)|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input); "distance_reference", (distance_reference)] {|(sop/distance_from_geometry
   (sop/ext_normal_input)
   (sop/ext_distance_reference)
   :distance_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input); "distance_reference", (distance_reference)] {|(sop/distance_from_geometry
   (sop/ext_normal_input)
   (sop/ext_distance_reference)
   :distance_attribute "")|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input); "distance_reference", (distance_reference)] {|(sop/distance_from_geometry
   (sop/ext_normal_input)
   (sop/ext_distance_reference)
   :mask_attribute "distance")|}) ];
  let target_distance = Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/distance_from_target (sop/ext_normal_input))|} in
  same_cook "distance from target defaults" ~typed:target_distance ~factory:Nodes.Distance_from_target.factory [] normal_input;
  cache_identity ~companions:["metric", ["projection", Param.Choice_value "Planar"]]
    "distance from target all fields" target_distance;
  List.iter (fun owner_choice ->
    List.iter (fun (_, projection_choice) ->
      same_cook ("distance from target " ^ owner_choice ^ " " ^ projection_choice)
        ~typed:(sop ~inputs:[normal_input] "distance_from_target"
          [ks "affected_owner" owner_choice; ks "affected_group" "selection"; ks "projection" projection_choice;
           kv "origin" (Vec3.create 0.1 0.2 0.3); kv "direction" (Vec3.create 1. 2. 3.); ks "radius_mode" "Fixed";
           kf "radius" 3.; ks "falloff" "Cubic"; ks "distance_attribute" "analytic"; ks "mask_attribute" "mask"])
        ~factory:Nodes.Distance_from_target.factory ["affected_owner", Param.Choice_value owner_choice;
          "affected_group", Text_value "selection"; "projection", Choice_value projection_choice;
          "origin_x", Float_value 0.1; "origin_y", Float_value 0.2; "origin_z", Float_value 0.3;
          "direction_x", Float_value 1.; "direction_y", Float_value 2.; "direction_z", Float_value 3.;
          "radius_mode", Choice_value "Fixed"; "radius", Float_value 3.; "falloff", Choice_value "Cubic";
          "distance_attribute", Text_value "analytic"; "mask_attribute", Text_value "mask"] normal_input)
      [Rdk.Transform_ops.Distance_target_spherical, "Spherical"; Distance_target_cylindrical, "Cylindrical";
        Distance_target_planar, "Planar"])
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  same_cook "distance from target signed plane" ~typed:(Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/distance_from_target
   (sop/ext_normal_input)
   :projection "Planar"
   :metric "Signed"
   :falloff "Quadratic"
   :mask_attribute "mask")|})
    ~factory:Nodes.Distance_from_target.factory ["projection", Param.Choice_value "Planar";
      "metric", Choice_value "Signed"; "falloff", Choice_value "Quadratic"; "mask_attribute", Text_value "mask"] normal_input;
  same_cook "distance from target mask only and unset group" ~typed:(Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/distance_from_target
   (sop/ext_normal_input)
   :affected_group " "
   :distance_attribute " "
   :mask_attribute "mask")|})
    ~factory:Nodes.Distance_from_target.factory ["affected_group", Param.Text_value " ";
      "distance_attribute", Text_value " "; "mask_attribute", Text_value "mask"] normal_input;
  List.iter (fun make ->
    check (rejected make)
      "distance from target refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/distance_from_target (sop/ext_normal_input) :radius_mode "Fixed" :radius 0.0)|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/distance_from_target (sop/ext_normal_input) :distance_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/distance_from_target (sop/ext_normal_input) :distance_attribute "")|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/distance_from_target (sop/ext_normal_input) :mask_attribute "distance")|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] (Printf.sprintf {|(sop/distance_from_target (sop/ext_normal_input) :projection "Planar" :direction %s)|} ((Lisp_sop.vec3 Vec3.zero))));
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/distance_from_target (sop/ext_normal_input) :metric "Signed")|}) ];
  same_cook "distance along defaults" ~typed:along ~factory:Nodes.Distance_along_geometry.factory [] distance_input;
  cache_identity "distance along all fields" along;
  List.iter (fun owner_choice ->
    List.iter (fun falloff_choice ->
      List.iter (fun radius_choice ->
        same_cook ("distance along " ^ owner_choice ^ " " ^ falloff_choice ^ " " ^ radius_choice)
          ~typed:(sop ~inputs:[distance_input] "distance_along_geometry"
            [ks "start_owner" owner_choice; ks "affected_owner" owner_choice; ks "affected_group" "selection";
             ks "falloff" falloff_choice; ks "radius_mode" radius_choice; kf "radius" 3.;
             ks "distance_attribute" "along"; ks "mask_attribute" "mask"])
          ~factory:Nodes.Distance_along_geometry.factory ["start_owner", Param.Choice_value owner_choice;
            "affected_owner", Choice_value owner_choice; "affected_group", Text_value "selection";
            "falloff", Choice_value falloff_choice; "radius_mode", Choice_value radius_choice;
            "radius", Float_value 3.; "distance_attribute", Text_value "along"; "mask_attribute", Text_value "mask"] distance_input)
        ["Fixed"; "Maximum distance"])
      ["Linear"; "Quadratic"; "Cubic"])
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  same_cook "distance along mask only and unset groups" ~typed:(Lisp_sop.node ~with_:["distance_input", (distance_input)] {|(sop/distance_along_geometry
   (sop/ext_distance_input)
   :start_group " "
   :affected_group " "
   :distance_attribute " "
   :mask_attribute "mask")|})
    ~factory:Nodes.Distance_along_geometry.factory ["start_group", Param.Text_value " ";
      "affected_group", Text_value " "; "distance_attribute", Text_value " "; "mask_attribute", Text_value "mask"] distance_input;
  List.iter (fun make ->
    check (rejected make)
      "distance along refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["distance_input", (distance_input)] {|(sop/distance_along_geometry (sop/ext_distance_input) :radius_mode "Fixed" :radius 0.0)|});
      (fun () -> Lisp_sop.node ~with_:["distance_input", (distance_input)] {|(sop/distance_along_geometry (sop/ext_distance_input) :radius -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["distance_input", (distance_input)] {|(sop/distance_along_geometry (sop/ext_distance_input) :distance_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["distance_input", (distance_input)] {|(sop/distance_along_geometry (sop/ext_distance_input) :distance_attribute "")|});
      (fun () -> Lisp_sop.node ~with_:["distance_input", (distance_input)] {|(sop/distance_along_geometry (sop/ext_distance_input) :mask_attribute "distance")|}) ];
  let matched = Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/match_size (sop/ext_normal_input))|} in
  same_cook ~optional_inputs:[Some normal_input; None] "match size defaults" ~typed:matched
    ~factory:Nodes.Match_size.factory [] normal_input;
  cache_identity "match size all fields" matched;
  let fit_target = Lisp_sop.node {|(sop/box :size [3.0 4.0 5.0])|} in
  List.iter (fun (fit, choice) ->
    let typed = sop ~inputs:[normal_input; fit_target] "match_size" [ks "fit" choice] in
    check (equal_geometry (cook 1 typed)
        (Rdk.Match_size.run ~fit ~target:(cook 1 fit_target) (cook 1 normal_input) |> Rdk.Error.unguard |> get))
      ("match size " ^ choice ^ " matches native");
    same_cook ~optional_inputs:[Some normal_input; Some fit_target] ("match size " ^ choice)
      ~typed
      ~factory:Nodes.Match_size.factory ["fit", Param.Choice_value choice]
      normal_input)
    [Rdk.Match_size.Translate_only, "Translate only"; Stretch, "Stretch"; Contain, "Contain";
      Cover, "Cover"; Match_x, "Match X"; Match_y, "Match Y"; Match_z, "Match Z";
      Match_perimeter, "Match perimeter"; Match_area, "Match area"; Match_volume, "Match volume"];
  let match_target = (let migration_translation = Vec3.create 3. 4. 5. in
Lisp_sop.node ~with_:["normal_input", (normal_input)] (Printf.sprintf {|(sop/transform
   (sop/ext_normal_input)
   :mode "Matrix"
   :m03 %s
   :m13 %s
   :m23 %s)|} ((Lisp_sop.float migration_translation.Vec3.x)) ((Lisp_sop.float migration_translation.Vec3.y)) ((Lisp_sop.float migration_translation.Vec3.z)))) in
  List.iter (fun choice ->
    same_cook ~optional_inputs:[Some normal_input; Some match_target]
      ("match size selections " ^ choice) ~typed:(sop ~inputs:[normal_input; match_target] "match_size"
        [ks "group_owner" choice; ks "group" "selection"; ks "source_group_owner" choice; ks "source_group" "selection";
         ks "target_group_owner" choice; ks "target_group" "selection"; ks "fit" "Translate only";
         kb "translate_y" false; kb "translate_z" false; kb "scale_x" false; kb "scale_z" false;
         kv "justify" (Vec3.create (-1.) 0.5 1.); kv "target_justify" (Vec3.create 1. 0. (-1.));
         kv "offset" (Vec3.create 0.1 0.2 0.3); kf "scale" 2.]) ~factory:Nodes.Match_size.factory
      ["group_owner", Param.Choice_value choice; "group", Text_value "selection";
        "source_group_owner", Choice_value choice; "source_group", Text_value "selection";
        "target_group_owner", Choice_value choice; "target_group", Text_value "selection";
        "fit", Choice_value "Translate only"; "translate_y", Bool_value false; "translate_z", Bool_value false;
        "scale_x", Bool_value false; "scale_z", Bool_value false;
        "justify_x", Float_value (-1.); "justify_y", Float_value 0.5; "justify_z", Float_value 1.;
        "target_justify_x", Float_value 1.; "target_justify_z", Float_value (-1.);
        "offset_x", Float_value 0.1; "offset_y", Float_value 0.2; "offset_z", Float_value 0.3;
        "scale", Float_value 2.] normal_input)
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  let auto_match = Lisp_sop.node ~with_:["normal_input", (normal_input)] (Printf.sprintf {|(sop/match_size
   (sop/ext_normal_input)
   :target_justify_mode "Auto"
   :justify %s
   :target_center [3.0 4.0 5.0]
   :target_size [2.0 3.0 4.0])|} ((Lisp_sop.vec3 Vec3.unit_x))) in
  same_cook ~optional_inputs:[Some normal_input; None] "match size Auto" ~typed:auto_match
    ~factory:Nodes.Match_size.factory ["target_justify_mode", Param.Choice_value "Auto";
      "justify_x", Float_value 1.; "target_center_x", Float_value 3.; "target_center_y", Float_value 4.;
      "target_center_z", Float_value 5.; "target_size_x", Float_value 2.;
      "target_size_y", Float_value 3.; "target_size_z", Float_value 4.] normal_input;
  check (equal_geometry (cook 1 auto_match) (cook 1 (Lisp_sop.node ~with_:["normal_input", (normal_input)] (Printf.sprintf {|(sop/match_size
   (sop/ext_normal_input)
   :justify %s
   :target_justify %s
   :target_center [3.0 4.0 5.0]
   :target_size [2.0 3.0 4.0])|} ((Lisp_sop.vec3 Vec3.unit_x)) ((Lisp_sop.vec3 Vec3.unit_x)))))) "match size Auto inherits source justification";
  let connected_match = Lisp_sop.node ~with_:["normal_input", (normal_input); "match_target", (match_target)] {|(sop/match_size (sop/ext_normal_input) (sop/ext_match_target))|} in
  let match_document = Edit_graph.of_graph match_target
      |> Edit_graph.add_node ~factory:Nodes.Match_size.factory
           ~inputs:[|Some (Node.id normal_input); Some (Node.id match_target)|] connected_match |> get
      |> Edit_graph.disconnect ~consumer:(Node.id connected_match) ~input_index:1 |> get in
  let rewired_match = Edit_graph.compile_node match_document ~node_id:(Node.id connected_match) |> get in
  same_node "match size disconnect target" ~typed:matched ~catalog:rewired_match;
  check (Node.id rewired_match = Node.id connected_match) "match size retains identity after disconnection";
  List.iter (fun make ->
    check (rejected make)
      "match size refuses invalid values at construction")
    [ (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/match_size (sop/ext_normal_input) :justify [2.0 0.0 0.0])|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/match_size (sop/ext_normal_input) :scale -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/match_size (sop/ext_normal_input) :target_size [1.0 -1.0 1.0])|}) ];
  same_cook "bound defaults" ~typed:bounded ~factory:Nodes.Bound.factory [] normal_input;
  cache_identity "bound all fields" bounded;
  List.iter (fun owner_choice ->
    List.iter (fun shape_choice ->
      same_cook ("bound " ^ owner_choice ^ " " ^ shape_choice) ~typed:(sop ~inputs:[normal_input] "bound"
          [ks "group_owner" owner_choice; ks "group" "selection"; ks "shape" shape_choice; ki "divisions_x" 2;
           ki "divisions_y" 3; ki "divisions_z" 4; ki "segments" 12; ki "rings" 6; kf "minimum_radius" 0.2;
           kv "lower" (Vec3.create 0.1 0.2 0.3); kv "upper" (Vec3.create 0.3 0.2 0.1); ks "bounds_group" "bounds";
           ks "center_attribute" "center"; ks "radii_attribute" "radii"])
        ~factory:Nodes.Bound.factory ["group_owner", Param.Choice_value owner_choice;
          "group", Text_value "selection"; "shape", Choice_value shape_choice;
          "divisions_x", Int_value 2; "divisions_y", Int_value 3; "divisions_z", Int_value 4;
          "segments", Int_value 12; "rings", Int_value 6; "minimum_radius", Float_value 0.2;
          "lower_x", Float_value 0.1; "lower_y", Float_value 0.2; "lower_z", Float_value 0.3;
          "upper_x", Float_value 0.3; "upper_y", Float_value 0.2; "upper_z", Float_value 0.1;
          "bounds_group", Text_value "bounds"; "center_attribute", Text_value "center";
          "radii_attribute", Text_value "radii"] normal_input)
      ["Box"; "Sphere"])
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  same_cook "bound unset names" ~typed:(Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/bound
   (sop/ext_normal_input)
   :group " "
   :bounds_group " "
   :center_attribute " "
   :radii_attribute " ")|}) ~factory:Nodes.Bound.factory
    ["group", Param.Text_value " "; "bounds_group", Text_value " ";
      "center_attribute", Text_value " "; "radii_attribute", Text_value " "] normal_input;
  List.iter (fun make ->
    check (rejected make)
      "bound refuses invalid parameters at construction")
    [ (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/bound (sop/ext_normal_input) :divisions_x 0)|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/bound (sop/ext_normal_input) :segments 2)|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/bound (sop/ext_normal_input) :lower [-0.1 0.0 0.0])|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/bound (sop/ext_normal_input) :center_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/bound (sop/ext_normal_input) :center_attribute "same" :radii_attribute "same")|}) ];
  let filter_input = Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(-> (sop/set_float (sop/ext_normal_input) :name "weight" :value 0.7)
     (sop/set_float :name "alpha" :value 0.8))|} in
  let blurred = Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/attribute_blur (sop/ext_filter_input))|} in
  let promoted = Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/promote_attributes (sop/ext_filter_input))|} in
  same_cook "promote attributes Lisp defaults" ~typed:promoted
    ~factory:Nodes.Promote_attributes.factory [] filter_input;
  cache_identity "promote attributes all fields"
    (Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/promote_attributes (sop/ext_filter_input) :method_ "First" :pattern "weight")|});
  List.iter (fun (_method, choice) ->
    same_cook ("promote attributes " ^ choice) ~typed:(sop ~inputs:[filter_input] "promote_attributes"
        [ks "method_" choice; ks "pattern" "weight"]) ~factory:Nodes.Promote_attributes.factory
      ["method_", Param.Choice_value choice; "pattern", Text_value "weight"] filter_input)
    [Rdk.Attribute_ops.First, "First"; Last, "Last"; Average, "Average"; Minimum, "Minimum";
      Maximum, "Maximum"; Mode, "Mode"; Median, "Median"; Sum, "Sum";
      Sum_squares, "Sum of squares"; Root_mean_square, "Root mean square";
      Array_all, "Array of all"; Unique_values, "Unique values"];
  let promotion_input = Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/set_int (sop/ext_filter_input) :name "piece" :value 1)|} in
  same_cook "promote attributes piece and renamed index" ~typed:(Lisp_sop.node ~with_:["promotion_input", (promotion_input)] {|(sop/promote_attributes
   (sop/ext_promotion_input)
   :method_ "First"
   :delete_source true
   :piece_attribute "piece"
   :destination "Point"
   :pattern "weight"
   :into_pattern "reduced"
   :index_pattern "source")|})
    ~factory:Nodes.Promote_attributes.factory ["method_", Param.Choice_value "First";
      "delete_source", Bool_value true; "piece_attribute", Text_value "piece";
      "destination", Choice_value "Point"; "pattern", Text_value "weight";
      "into_pattern", Text_value "reduced"; "index_pattern", Text_value "source"] promotion_input;
  List.iter (fun choice ->
    let input = sop ~inputs:[filter_input] "set_float" [ks "owner" choice; ks "name" "payload"; kf "value" 0.8] in
    same_cook ("promote attributes source " ^ choice) ~typed:(sop ~inputs:[input] "promote_attributes"
        [ks "source" choice; ks "destination" "Point"; ks "pattern" "payload"])
      ~factory:Nodes.Promote_attributes.factory ["source", Param.Choice_value choice;
        "destination", Choice_value "Point"; "pattern", Text_value "payload"] input;
    same_cook ("promote attributes destination " ^ choice) ~typed:(sop ~inputs:[filter_input] "promote_attributes"
        [ks "destination" choice; ks "pattern" "weight"]) ~factory:Nodes.Promote_attributes.factory
      ["destination", Param.Choice_value choice; "pattern", Text_value "weight"] filter_input)
    ["Point"; "Vertex"; "Primitive"; "Detail"];
  List.iter (fun make ->
    check (rejected make)
      "promote attributes refuses invalid patterns and source-index modes at construction")
    [ (fun () -> Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/promote_attributes (sop/ext_filter_input) :pattern "broken[")|});
      (fun () -> Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/promote_attributes (sop/ext_filter_input) :index_pattern "source")|});
      (fun () -> Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/promote_attributes (sop/ext_filter_input) :pattern "weight" :into_pattern "out_*_*")|}) ];
  let smoothed = Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/smooth (sop/ext_filter_input))|} in
  same_cook "smooth Lisp defaults" ~typed:smoothed ~factory:Nodes.Smooth.factory [] filter_input;
  cache_identity "smooth all fields" smoothed;
  List.iter (fun (_boundary, choice) ->
    same_cook ("smooth boundary " ^ choice) ~typed:(sop ~inputs:[filter_input] "smooth"
        [ks "boundary" choice; ks "group" "selection"; ks "constrained_points" "selection"; ki "iterations" 3;
         ks "method_" "Edge length"; ks "mode" "Custom steps"; kf "step" 0.2; kf "odd_step" 0.3;
         kf "even_step" (-0.4); ks "weight_attribute" "weight"; ks "alpha_attribute" "alpha";
         ks "attributes" "P weight"; kb "recompute_normals" true; kf "original_blend" 0.2; kf "smoothed_blend" 0.8])
      ~factory:Nodes.Smooth.factory ["boundary", Param.Choice_value choice;
        "group", Text_value "selection"; "constrained_points", Text_value "selection";
        "iterations", Int_value 3; "method_", Choice_value "Edge length";
        "mode", Choice_value "Custom steps"; "step", Float_value 0.2;
        "odd_step", Float_value 0.3; "even_step", Float_value (-0.4);
        "weight_attribute", Text_value "weight"; "alpha_attribute", Text_value "alpha";
        "attributes", Text_value "P weight"; "recompute_normals", Bool_value true;
        "original_blend", Float_value 0.2; "smoothed_blend", Float_value 0.8] filter_input)
    [Rdk.Smooth.Smooth_free, "Free"; Smooth_unshared, "Pin unshared"; Smooth_group_boundary, "Pin group boundary"];
  same_cook "smooth unset names" ~typed:(Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/smooth
   (sop/ext_filter_input)
   :group " "
   :constrained_points " "
   :weight_attribute " "
   :alpha_attribute " ")|}) ~factory:Nodes.Smooth.factory
    ["group", Param.Text_value " "; "constrained_points", Text_value " ";
      "weight_attribute", Text_value " "; "alpha_attribute", Text_value " "] filter_input;
  List.iter (fun make ->
    check (rejected make)
      "smooth refuses invalid parameters at construction")
    [ (fun () -> Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/smooth (sop/ext_filter_input) :iterations 0)|});
      (fun () -> Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/smooth (sop/ext_filter_input) :attributes "broken[")|}) ];
  same_cook "attribute blur defaults" ~typed:blurred ~factory:Nodes.Attribute_blur.factory [] filter_input;
  cache_identity "attribute blur all fields" blurred;
  List.iter (fun mode_choice ->
    List.iter (fun (_method, method_choice) ->
      same_cook ("attribute blur " ^ mode_choice ^ " " ^ method_choice)
        ~typed:(sop ~inputs:[filter_input] "attribute_blur"
          [ks "mode" mode_choice; ks "method_" method_choice; ks "attributes" "P weight"; ks "group" "selection";
           ki "iterations" 3; kf "laplacian_step" 0.2; kf "odd_step" 0.3; kf "even_step" (-0.4);
           ks "weight_attribute" "weight"; ks "alpha_attribute" "alpha"; kb "pin_borders" true;
           kf "original_blend" 0.2; kf "blurred_blend" 0.8])
        ~factory:Nodes.Attribute_blur.factory ["mode", Param.Choice_value mode_choice;
          "method_", Choice_value method_choice; "attributes", Text_value "P weight";
          "group", Text_value "selection"; "iterations", Int_value 3;
          "laplacian_step", Float_value 0.2; "odd_step", Float_value 0.3;
          "even_step", Float_value (-0.4); "weight_attribute", Text_value "weight";
          "alpha_attribute", Text_value "alpha"; "pin_borders", Bool_value true;
          "original_blend", Float_value 0.2; "blurred_blend", Float_value 0.8] filter_input)
      [Rdk.Attribute_ops.Uniform, "Uniform"; Edge_length, "Edge length"])
    ["Laplacian"; "Custom steps"];
  same_cook "attribute blur unset names" ~typed:(Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/attribute_blur
   (sop/ext_filter_input)
   :group " "
   :weight_attribute " "
   :alpha_attribute " ")|})
    ~factory:Nodes.Attribute_blur.factory ["group", Param.Text_value " ";
      "weight_attribute", Text_value " "; "alpha_attribute", Text_value " "] filter_input;
  List.iter (fun make ->
    check (rejected make)
      "attribute blur refuses invalid parameters at construction")
    [ (fun () -> Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/attribute_blur (sop/ext_filter_input) :iterations -1)|});
      (fun () -> Lisp_sop.node ~with_:["filter_input", (filter_input)] {|(sop/attribute_blur (sop/ext_filter_input) :attributes "broken[")|}) ];
  let graph_colored = Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/graph_color (sop/ext_normal_input))|} in
  same_cook "graph color defaults" ~typed:graph_colored
    ~factory:Nodes.Graph_color.factory [] normal_input;
  cache_identity "graph color all fields" (Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/graph_color (sop/ext_normal_input) :sort_output true)|});
  List.iter (fun (_connectivity, choice) ->
    same_cook ("graph color connectivity " ^ choice)
      ~typed:(sop ~inputs:[normal_input] "graph_color" [ks "connectivity" choice]) ~factory:Nodes.Graph_color.factory
      ["connectivity", Param.Choice_value choice] normal_input)
    [Rdk.Graph_color.Graph_primitives_by_point, "Primitives by point";
     Graph_points_by_primitive, "Points by primitive"; Graph_primitives_by_edge, "Primitives by edge"];
  List.iter (fun choice ->
    same_cook ("graph color selection " ^ choice) ~typed:(sop ~inputs:[normal_input] "graph_color"
        [ks "group_owner" choice; ks "group" "selection"; ks "color_attribute" "schedule"; kb "sort_output" true;
         kb "output_worksets" true; ks "workset_begin_attribute" "begin"; ks "workset_length_attribute" "length"])
      ~factory:Nodes.Graph_color.factory ["group_owner", Param.Choice_value choice;
        "group", Text_value "selection"; "color_attribute", Text_value "schedule";
        "sort_output", Bool_value true; "output_worksets", Bool_value true;
        "workset_begin_attribute", Text_value "begin";
        "workset_length_attribute", Text_value "length"] normal_input)
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  List.iter (fun make ->
    check (rejected make)
      "graph color refuses invalid output names at construction")
    [ (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/graph_color (sop/ext_normal_input) :color_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/graph_color (sop/ext_normal_input) :output_worksets true)|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/graph_color
   (sop/ext_normal_input)
   :sort_output true
   :output_worksets true
   :workset_begin_attribute "same"
   :workset_length_attribute "same")|}) ];
  let normal = Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/normals (sop/ext_normal_input))|} in
  same_cook "normal Lisp defaults" ~typed:normal ~factory:Nodes.Normal.factory [] normal_input;
  cache_identity "normal all fields" normal;
  List.iter (fun choice ->
    same_cook ("normal output " ^ choice) ~typed:(sop ~inputs:[normal_input] "normals" [ks "owner" choice])
      ~factory:Nodes.Normal.factory ["owner", Param.Choice_value choice] normal_input)
    ["Point"; "Vertex"; "Primitive"; "Detail"];
  List.iter (fun choice ->
    same_cook ("normal selection " ^ choice) ~typed:(sop ~inputs:[normal_input] "normals"
        [ks "group_owner" choice; ks "group" "selection"; ks "weighting" "Each vertex"; kf "cusp_angle" 0.5;
         kb "keep_original_zero" true; kb "reverse" true; ks "attribute" "normal"]) ~factory:Nodes.Normal.factory
      ["group_owner", Param.Choice_value choice; "group", Text_value "selection";
       "weighting", Choice_value "Each vertex"; "cusp_angle", Float_value 0.5;
       "keep_original_zero", Bool_value true; "reverse", Bool_value true;
       "attribute", Text_value "normal"] normal_input)
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  same_cook "normal face-area weighting" ~typed:(Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/normals (sop/ext_normal_input) :weighting "Face area")|})
    ~factory:Nodes.Normal.factory ["weighting", Param.Choice_value "Face area"] normal_input;
  List.iter (fun make ->
    check (rejected make)
      "normal refuses invalid cusp angles at construction")
    [ (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/normals (sop/ext_normal_input) :cusp_angle -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/normals (sop/ext_normal_input) :cusp_angle 4.0)|}) ];
  let peak_input = Lisp_sop.node ~with_:["normal_input", (normal_input)] (Printf.sprintf {|(-> (sop/set_vector (sop/ext_normal_input) :name "N" :value %s)
     (sop/set_vector :name "flow" :value [2.0 0.0 0.0])
     (sop/set_float :name "mask" :value 0.5))|} ((Lisp_sop.vec3 Vec3.unit_y))) in
  let peaked = Lisp_sop.node ~with_:["peak_input", (peak_input)] {|(sop/peak (sop/ext_peak_input))|} in
  same_cook "peak Lisp defaults" ~typed:peaked ~factory:Nodes.Peak.factory [] peak_input;
  cache_identity "peak all fields" peaked;
  List.iter (fun choice ->
    same_cook ("peak selection " ^ choice) ~typed:(sop ~inputs:[peak_input] "peak"
        [ks "group_owner" choice; ks "group" "selection"; ks "direction_attribute" "flow";
         kb "normalize_direction" false; ks "mask_attribute" "mask"; kf "distance" 0.2; kb "recompute_normals" true]) ~factory:Nodes.Peak.factory
      ["group_owner", Param.Choice_value choice; "group", Text_value "selection";
       "direction_attribute", Text_value "flow"; "normalize_direction", Bool_value false;
       "mask_attribute", Text_value "mask"; "distance", Float_value 0.2;
       "recompute_normals", Bool_value true] peak_input)
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  same_cook "peak generated direction" ~typed:(Lisp_sop.node ~with_:["peak_input", (peak_input)] {|(sop/peak (sop/ext_peak_input) :direction_attribute "")|})
    ~factory:Nodes.Peak.factory ["direction_attribute", Param.Text_value ""] peak_input;
  let bent = Lisp_sop.node ~with_:["peak_input", (peak_input)] {|(sop/bend (sop/ext_peak_input))|} in
  same_cook "bend Lisp defaults" ~typed:bent ~factory:Nodes.Bend.factory [] peak_input;
  cache_identity "bend all fields" bent;
  List.iter (fun choice ->
    same_cook ("bend selection " ^ choice) ~typed:(sop ~inputs:[peak_input] "bend"
        [ks "group_owner" choice; ks "group" "selection"; ks "mask_attribute" "mask";
         kv "origin" (Vec3.create 0.1 0.2 0.3); kv "direction" (Vec3.create 1. 0. 1.); kv "up" Vec3.unit_y;
         kf "length" 2.; kf "bend_angle" 0.4; kf "twist_angle" 0.6; kb "limit" false; kb "both_directions" true;
         kb "continuous_twist" true; ks "capture_attribute" "capture"; kb "recompute_normals" true])
      ~factory:Nodes.Bend.factory ["group_owner", Param.Choice_value choice; "group", Text_value "selection";
        "mask_attribute", Text_value "mask"; "origin_x", Float_value 0.1; "origin_y", Float_value 0.2;
        "origin_z", Float_value 0.3; "direction_x", Float_value 1.; "direction_y", Float_value 0.;
        "direction_z", Float_value 1.; "up_y", Float_value 1.; "up_z", Float_value 0.;
        "length", Float_value 2.; "bend_angle", Float_value 0.4; "twist_angle", Float_value 0.6;
        "limit", Bool_value false; "both_directions", Bool_value true; "continuous_twist", Bool_value true;
        "capture_attribute", Text_value "capture"; "recompute_normals", Bool_value true] peak_input)
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  List.iter (fun make ->
    check (rejected make)
      "bend refuses invalid capture fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["peak_input", (peak_input)] {|(sop/bend (sop/ext_peak_input) :length 0.0)|});
      (fun () -> Lisp_sop.node ~with_:["peak_input", (peak_input)] (Printf.sprintf {|(sop/bend (sop/ext_peak_input) :direction %s)|} ((Lisp_sop.vec3 Vec3.zero))));
      (fun () -> Lisp_sop.node ~with_:["peak_input", (peak_input)] (Printf.sprintf {|(sop/bend (sop/ext_peak_input) :direction %s :up %s)|} ((Lisp_sop.vec3 Vec3.unit_y)) ((Lisp_sop.vec3 Vec3.unit_y))));
      (fun () -> Lisp_sop.node ~with_:["peak_input", (peak_input)] {|(sop/bend (sop/ext_peak_input) :capture_attribute "P")|}) ];
  check (Result.is_error (Node.apply_parameters bent ["length", Param.Float_value 0.]))
    "bend rejects zero-length edits";
  let clipped = Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/clip (sop/ext_normal_input))|} in
  same_cook "clip defaults" ~typed:clipped ~factory:Nodes.Clip.factory [] normal_input;
  cache_identity "clip all fields" (Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/clip (sop/ext_normal_input) :keep "All")|});
  List.iter (fun choice ->
    same_cook ("clip selection " ^ choice) ~typed:(sop ~inputs:[normal_input] "clip" [ks "group_owner" choice; ks "group" "selection"])
      ~factory:Nodes.Clip.factory ["group_owner", Param.Choice_value choice;
        "group", Text_value "selection"] normal_input)
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  same_cook "clip explicit fields" ~typed:(Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/clip
   (sop/ext_normal_input)
   :group_owner "Edge"
   :group "selection"
   :keep "All"
   :snapping_tolerance 0.001
   :fill true
   :split_connectivity true
   :distance 0.1
   :origin [0.0 0.1 0.0]
   :replace_existing_groups true
   :clipped_edge_group "edges"
   :cap_group "caps"
   :clipped_group "clipped"
   :above_group "above"
   :below_group "below")|}) ~factory:Nodes.Clip.factory
    ["group_owner", Param.Choice_value "Edge"; "group", Text_value "selection";
      "keep", Choice_value "All"; "snapping_tolerance", Float_value 0.001; "fill", Bool_value true;
      "split_connectivity", Bool_value true; "distance", Float_value 0.1; "origin_y", Float_value 0.1;
      "replace_existing_groups", Bool_value true; "clipped_edge_group", Text_value "edges";
      "cap_group", Text_value "caps"; "clipped_group", Text_value "clipped";
      "above_group", Text_value "above"; "below_group", Text_value "below"] normal_input;
  same_cook "clip unset attribute" ~typed:(Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/clip (sop/ext_normal_input) :clip_attribute "")|})
    ~factory:Nodes.Clip.factory ["clip_attribute", Param.Text_value ""] normal_input;
  List.iter (fun make ->
    check (rejected make)
      "clip refuses invalid plane fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] (Printf.sprintf {|(sop/clip (sop/ext_normal_input) :normal %s)|} ((Lisp_sop.vec3 Vec3.zero))));
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/clip (sop/ext_normal_input) :snapping_tolerance -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/clip (sop/ext_normal_input) :split_connectivity true)|});
      (fun () -> Lisp_sop.node ~with_:["normal_input", (normal_input)] {|(sop/clip (sop/ext_normal_input) :cap_group "same" :above_group "same")|}) ];
  let expand_input = Lisp_sop.node (Printf.sprintf {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 3 :rows 2 :size 2.0)
     (sop/group_range :name "group" :end_ 0)
     (sop/group_range :owner "Vertices" :name "group" :end_ 0)
     (sop/group_range :owner "Primitives" :name "group" :end_ 0)
     (sop/group_edges :name "group")
     (sop/group_edges :name "collision")
     (sop/set_int :owner "Primitive" :name "region")
     (sop/normals :weighting "Face area" :owner "Point")
     (sop/set_vector :owner "Primitive" :name "N" :value %s))|} ((Lisp_sop.vec3 Vec3.unit_y))) in
  let expanded = Lisp_sop.node ~with_:["expand_input", (expand_input)] {|(sop/group_expand (sop/ext_expand_input))|} in
  same_cook "group expand defaults" ~typed:expanded ~factory:Nodes.Group_expand.factory [] expand_input;
  cache_identity ~changes:["connectivity_attributes", Param.Text_value "primitive\tregion"]
    "group expand all fields" expanded;
  List.iter (fun choice ->
    same_cook ("group expand " ^ choice) ~typed:(sop ~inputs:[expand_input] "group_expand" [ks "owner" choice])
      ~factory:Nodes.Group_expand.factory ["owner", Param.Choice_value choice] expand_input)
    ["Vertices"; "Primitives"; "Edges"];
  same_cook "group expand constraints" ~typed:(sop ~inputs:[expand_input] "group_expand"
      [ks "owner" "Primitives"; ks "name" "expanded"; kb "flood" true; ki "steps" 2; ks "step_attribute" "growth";
       ks "primitive_connectivity" "Share edges"; kf "normal_spread" 0.1; kb "use_normal_attribute" true;
       ks "normal_owner" "Primitive"; ks "normal_name" "N"; ks "connectivity_attributes" "primitive\tregion";
       kf "connectivity_tolerance" 0.001; kb "use_collision" true; ks "collision_owner" "Primitives";
       ks "collision_group" "group"; kb "collision_contain" true; kb "collision_allow_boundary" true])
    ~factory:Nodes.Group_expand.factory ["owner", Param.Choice_value "Primitives";
      "name", Text_value "expanded"; "flood", Bool_value true; "steps", Int_value 2;
      "step_attribute", Text_value "growth"; "primitive_connectivity", Choice_value "Share edges";
      "normal_spread", Float_value 0.1; "use_normal_attribute", Bool_value true;
      "connectivity_attributes", Text_value "primitive\tregion"; "connectivity_tolerance", Float_value 0.001;
      "use_collision", Bool_value true; "collision_owner", Choice_value "Primitives";
      "collision_group", Text_value "group"; "collision_contain", Bool_value true;
      "collision_allow_boundary", Bool_value true] expand_input;
  List.iter (fun make ->
    check (rejected make)
      "group expand refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["expand_input", (expand_input)] {|(sop/group_expand (sop/ext_expand_input) :connectivity_tolerance -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["expand_input", (expand_input)] {|(sop/group_expand (sop/ext_expand_input) :use_collision true :collision_contain true)|});
      (fun () -> Lisp_sop.node ~with_:["expand_input", (expand_input)] {|(sop/group_expand (sop/ext_expand_input) :flood true :steps -1)|}) ];
  let curvature_input = Lisp_sop.snapshot ((Sources.curvature ())) in
  let curvature = Lisp_sop.node ~with_:["curvature_input", (curvature_input)] {|(sop/measure_curvature (sop/ext_curvature_input))|} in
  same_cook "measure curvature defaults" ~typed:curvature
    ~factory:Nodes.Measure_curvature.factory [] curvature_input;
  cache_identity "measure curvature" curvature;
  same_cook "measure curvature explicit fields" ~typed:(Lisp_sop.node ~with_:["curvature_input", (curvature_input)] {|(sop/measure_curvature
   (sop/ext_curvature_input)
   :point_group "upper"
   :boundary "One-sided"
   :smoothing_iterations 2
   :smoothing_strength 0.25
   :mean "mean"
   :gaussian "gaussian"
   :minimum "minimum"
   :maximum "maximum"
   :curvedness "curvedness"
   :shape_index "shape_index")|})
    ~factory:Nodes.Measure_curvature.factory ["point_group", Param.Text_value "upper";
      "boundary", Choice_value "One-sided"; "smoothing_iterations", Int_value 2;
      "smoothing_strength", Float_value 0.25; "mean", Text_value "mean";
      "gaussian", Text_value "gaussian"; "minimum", Text_value "minimum";
      "maximum", Text_value "maximum"; "curvedness", Text_value "curvedness";
      "shape_index", Text_value "shape_index"] curvature_input;
  List.iter (fun make ->
    check (rejected make)
      "measure curvature refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["curvature_input", (curvature_input)] {|(sop/measure_curvature (sop/ext_curvature_input) :mean "")|});
      (fun () -> Lisp_sop.node ~with_:["curvature_input", (curvature_input)] {|(sop/measure_curvature (sop/ext_curvature_input) :gaussian "curvature")|});
      (fun () -> Lisp_sop.node ~with_:["curvature_input", (curvature_input)] {|(sop/measure_curvature (sop/ext_curvature_input) :mean "P")|});
      (fun () -> Lisp_sop.node ~with_:["curvature_input", (curvature_input)] {|(sop/measure_curvature (sop/ext_curvature_input) :smoothing_iterations -1)|}) ];
  check (Result.is_error (Node.apply_parameters curvature ["gaussian", Param.Text_value "curvature"]))
    "measure curvature rejects duplicate output edits";
  let laplacian_input = Lisp_sop.node {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 3 :rows 3 :size 2.0)
     (sop/group_range :name "selection" :start 4 :end_ 5)
     (sop/set_float :name "height" :value 0.5))|} in
  let laplacian = Lisp_sop.node ~with_:["laplacian_input", (laplacian_input)] {|(sop/attribute_laplacian (sop/ext_laplacian_input))|} in
  same_cook "attribute Laplacian Lisp defaults" ~typed:laplacian
    ~factory:Nodes.Attribute_laplacian.factory [] laplacian_input;
  cache_identity "attribute Laplacian" laplacian;
  List.iter (fun (_weighting, choice) ->
    same_cook ("attribute Laplacian " ^ choice) ~typed:(sop ~inputs:[laplacian_input] "attribute_laplacian"
        [ks "point_group" "selection"; ks "weighting" choice; kb "normalize" false; ks "source" "height"; ks "output" "L"])
      ~factory:Nodes.Attribute_laplacian.factory ["point_group", Param.Text_value "selection";
        "weighting", Choice_value choice; "normalize", Bool_value false;
        "source", Text_value "height"; "output", Text_value "L"] laplacian_input)
    [Rdk.Laplacian.Laplacian_cotan, "Cotangent"; Laplacian_positive_cotan, "Positive cotangent";
     Laplacian_uniform, "Uniform"];
  check (Result.is_error (Node.apply_parameters laplacian ["output", Param.Text_value "P"]))
    "attribute Laplacian rejects canonical position edits";
  let seam_input = Lisp_sop.node ~with_:["in956", (Lisp_sop.snapshot ((Sources.point_split ())))] {|(sop/set_int (sop/ext_in956) :owner "Primitive" :name "piece")|} in
  let seamed = Lisp_sop.node ~with_:["seam_input", (seam_input)] {|(sop/uv_auto_seam (sop/ext_seam_input))|} in
  same_cook "UV auto seam defaults" ~typed:seamed ~factory:Nodes.Uv_auto_seam.factory [] seam_input;
  cache_identity "UV auto seam" seamed;
  same_cook "UV auto seam explicit fields" ~typed:(Lisp_sop.node ~with_:["seam_input", (seam_input)] {|(sop/uv_auto_seam
   (sop/ext_seam_input)
   :name "seams"
   :group "seam_face"
   :angle 0.5
   :include_boundaries false
   :include_non_manifold false
   :partition_attribute "piece"
   :existing_uv "uv"
   :uv_tolerance 0.001
   :island_attribute "island")|}) ~factory:Nodes.Uv_auto_seam.factory
    ["name", Param.Text_value "seams"; "group", Text_value "seam_face";
     "angle", Float_value 0.5; "include_boundaries", Bool_value false;
     "include_non_manifold", Bool_value false; "partition_attribute", Text_value "piece";
     "existing_uv", Text_value "uv"; "uv_tolerance", Float_value 0.001;
     "island_attribute", Text_value "island"] seam_input;
  List.iter (fun make ->
    check (rejected make)
      "UV auto seam refuses invalid numeric fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["seam_input", (seam_input)] {|(sop/uv_auto_seam (sop/ext_seam_input) :angle 4.0)|});
      (fun () -> Lisp_sop.node ~with_:["seam_input", (seam_input)] {|(sop/uv_auto_seam (sop/ext_seam_input) :uv_tolerance -1.0)|}) ];
  let split_input = Lisp_sop.snapshot ((Sources.point_split ()))
      |> group_indices "Vertices" "selected_vertices" [|0; 3|] in
  let split = Lisp_sop.node ~with_:["split_input", (split_input)] {|(sop/point_split (sop/ext_split_input))|} in
  same_cook "point split Lisp defaults" ~typed:split ~factory:Nodes.Point_split.factory [] split_input;
  cache_identity "point split" split;
  List.iter (fun (choice, group) ->
    same_cook ("point split " ^ choice) ~typed:(sop ~inputs:[split_input] "point_split"
        [ks "group_owner" choice; ks "group" group; ks "attributes" "uv"; kf "tolerance" 1e-6;
         kb "promote_attributes" true])
      ~factory:Nodes.Point_split.factory ["group_owner", Param.Choice_value choice;
        "group", Text_value group; "attributes", Text_value "uv"; "tolerance", Float_value 1e-6;
        "promote_attributes", Bool_value true] split_input)
    ["Point", "split_points"; "Vertex", "selected_vertices";
     "Primitive", "seam_face"];
  List.iter (fun make ->
    check (rejected make)
      "point split refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["split_input", (split_input)] {|(sop/point_split (sop/ext_split_input) :group_owner "Edge" :group "selection")|});
      (fun () -> Lisp_sop.node ~with_:["split_input", (split_input)] {|(sop/point_split (sop/ext_split_input) :tolerance -1.0)|}) ];
  let relax_input = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [3.0 0.0 0.0] [6.0 0.0 0.0]))
     (sop/group_range :name "selection" :start 1 :end_ 2)
     (sop/group_range :owner "Primitives" :name "selection" :end_ 0)
     (sop/group_range :name "pins" :end_ 0))|} in
  let relax_reference = Lisp_sop.node {|(sop/curve (list [0.0 0.0 0.0] [2.0 0.0 0.0] [3.0 0.0 0.0] [7.0 0.0 0.0]))|} in
  let relaxed = Lisp_sop.node ~with_:["relax_input", (relax_input); "relax_reference", (relax_reference)] {|(sop/edge_relax (sop/ext_relax_input) (sop/ext_relax_reference))|} in
  same_cook ~inputs:[relax_reference] "edge relax Lisp defaults" ~typed:relaxed
    ~factory:Nodes.Edge_relax.factory [] relax_input;
  List.iter (fun choice ->
    same_cook ~inputs:[relax_reference] ("edge relax " ^ choice)
      ~typed:(sop ~inputs:[relax_input; relax_reference] "edge_relax"
        [ks "group_owner" choice; ks "group" "selection"; ks "pin_group" "pins"; ki "iterations" 8;
         kf "step_size" 0.25; ks "target_mode" "Scale-independent distribution"; kb "only_shorten" true;
         kf "tolerance" 0.001])
      ~factory:Nodes.Edge_relax.factory ["group_owner", Param.Choice_value choice;
        "group", Text_value "selection"; "pin_group", Text_value "pins"; "iterations", Int_value 8;
        "step_size", Float_value 0.25; "target_mode", Choice_value "Scale-independent distribution";
        "only_shorten", Bool_value true; "tolerance", Float_value 0.001] relax_input)
    ["Point"; "Primitive"];
  cache_identity ~changes:["group_owner", Param.Choice_value "Primitive"] "edge relax all fields"
    (Lisp_sop.node ~with_:["relax_input", (relax_input); "relax_reference", (relax_reference)] {|(sop/edge_relax
   (sop/ext_relax_input)
   (sop/ext_relax_reference)
   :group_owner "Point"
   :group "selection")|});
  List.iter (fun make ->
    check (rejected make)
      "edge relax refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["relax_input", (relax_input); "relax_reference", (relax_reference)] {|(sop/edge_relax
   (sop/ext_relax_input)
   (sop/ext_relax_reference)
   :group_owner "Vertex"
   :group "selection")|});
      (fun () -> Lisp_sop.node ~with_:["relax_input", (relax_input); "relax_reference", (relax_reference)] {|(sop/edge_relax (sop/ext_relax_input) (sop/ext_relax_reference) :iterations 0)|});
      (fun () -> Lisp_sop.node ~with_:["relax_input", (relax_input); "relax_reference", (relax_reference)] {|(sop/edge_relax (sop/ext_relax_input) (sop/ext_relax_reference) :step_size 1.1)|});
      (fun () -> Lisp_sop.node ~with_:["relax_input", (relax_input); "relax_reference", (relax_reference)] {|(sop/edge_relax (sop/ext_relax_input) (sop/ext_relax_reference) :tolerance 0.0)|}) ];
  check (Result.is_error (Node.apply_parameters relaxed ["step_size", Param.Float_value 0.]))
    "edge relax rejects zero step-size edits";
  let hull_input = Lisp_sop.node ~with_:["in984", (Sources.hull ())] {|(sop/group_range (sop/ext_in984) :name "bottom" :end_ 3)|} in
  let hull = Lisp_sop.node ~with_:["hull_input", (hull_input)] {|(sop/convex_hull (sop/ext_hull_input))|} in
  same_cook "convex hull Lisp defaults" ~typed:hull ~factory:Nodes.Convex_hull.factory [] hull_input;
  cache_identity "convex hull" hull;
  same_cook "convex hull selection" ~typed:(Lisp_sop.node ~with_:["hull_input", (hull_input)] {|(sop/convex_hull
   (sop/ext_hull_input)
   :group "bottom"
   :preserve_point_payload false
   :source_point_attribute "source"
   :hull_group "plane")|}) ~factory:Nodes.Convex_hull.factory
    ["group", Param.Text_value "bottom"; "preserve_point_payload", Bool_value false;
     "source_point_attribute", Text_value "source"; "hull_group", Text_value "plane"] hull_input;
  check (Result.is_error (Node.apply_parameters hull ["source_point_attribute", Param.Text_value "P"]))
    "convex hull rejects reserved ancestry edits";
  let grouped_hull_input = Lisp_sop.node {|(-> (sop/box)
     (sop/group_range :name "selection" :end_ 3)
     (sop/group_range :owner "Vertices" :name "selection" :end_ 3)
     (sop/group_range :owner "Primitives" :name "selection" :end_ 0)
     (sop/group_edges :name "selection"))|} in
  List.iter (fun choice ->
    same_cook ("convex hull " ^ choice) ~typed:(sop ~inputs:[grouped_hull_input] "convex_hull"
        [ks "group_owner" choice; ks "group" "selection"]) ~factory:Nodes.Convex_hull.factory
      ["group_owner", Param.Choice_value choice; "group", Text_value "selection"] grouped_hull_input)
    ["Point"; "Vertex"; "Primitive"; "Edge"];
  let bridge_input = Lisp_sop.snapshot ((Sources.bridge ())) in
  let bridged = Lisp_sop.node ~with_:["bridge_input", (bridge_input)] {|(sop/poly_bridge (sop/ext_bridge_input))|} in
  same_cook "poly bridge Lisp defaults" ~typed:bridged ~factory:Nodes.Poly_bridge.factory [] bridge_input;
  cache_identity "poly bridge" bridged;
  same_cook "poly bridge explicit fields" ~typed:(Lisp_sop.node ~with_:["bridge_input", (bridge_input)] {|(sop/poly_bridge
   (sop/ext_bridge_input)
   :pairing "By centroid"
   :connect_closest_ends false
   :minimize "Three point distance"
   :reverse_source true
   :reverse_destination true
   :pairing_shift 1
   :divisions 2
   :keep_input false
   :output_group "surface"
   :collinearity_tolerance 0.1
   :recompute_normals true)|})
    ~factory:Nodes.Poly_bridge.factory ["pairing", Param.Choice_value "By centroid";
      "connect_closest_ends", Bool_value false; "minimize", Choice_value "Three point distance";
      "reverse_source", Bool_value true; "reverse_destination", Bool_value true;
      "pairing_shift", Int_value 1; "divisions", Int_value 2; "keep_input", Bool_value false;
      "output_group", Text_value "surface"; "collinearity_tolerance", Float_value 0.1;
      "recompute_normals", Bool_value true] bridge_input;
  List.iter (fun make ->
    check (rejected make)
      "poly bridge refuses invalid numeric fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["bridge_input", (bridge_input)] {|(sop/poly_bridge (sop/ext_bridge_input) :divisions 0)|});
      (fun () -> Lisp_sop.node ~with_:["bridge_input", (bridge_input)] {|(sop/poly_bridge (sop/ext_bridge_input) :collinearity_tolerance 1.1)|}) ];
  let circle_input = Lisp_sop.snapshot ((Sources.loops 2 16)) in
  let fitted = Lisp_sop.node ~with_:["circle_input", (circle_input)] {|(sop/circle_from_edges (sop/ext_circle_input))|} in
  same_cook "circle from edges defaults" ~typed:fitted
    ~factory:Nodes.Circle_from_edges.factory [] circle_input;
  cache_identity "circle from edges" fitted;
  same_cook "circle from edges explicit fields" ~typed:(Lisp_sop.node ~with_:["circle_input", (circle_input)] {|(sop/circle_from_edges
   (sop/ext_circle_input)
   :group "loops"
   :use_radius true
   :radius 1.5
   :scale [1.0 0.75 0.5]
   :output_group "fitted")|}) ~factory:Nodes.Circle_from_edges.factory
    ["group", Param.Text_value "loops"; "use_radius", Bool_value true;
     "radius", Float_value 1.5; "scale_y", Float_value 0.75; "scale_z", Float_value 0.5;
     "output_group", Text_value "fitted"] circle_input;
  check (Result.is_error (Node.apply_parameters fitted ["use_radius", Param.Bool_value true;
      "radius", Float_value 0.])) "circle from edges rejects enabled zero-radius edits";
  let resample_input = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [2.0 0.0 0.0]))
     (-> (sop/curve (list [10.0 0.0 0.0] [11.0 1.0 0.0] [12.0 0.0 0.0])) (sop/merge))
     (sop/group_range :owner "Primitives" :name "first_curve" :end_ 0)
     (sop/set_int :owner "Primitive" :name "counts" :value 4)
     (sop/set_float :owner "Primitive" :name "lengths" :value 0.4))|} in
  let resampled = Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/resample (sop/ext_resample_input))|} in
  same_cook "resample Lisp defaults" ~typed:resampled ~factory:Nodes.Resample.factory [] resample_input;
  List.iter (fun (name, typed, values) ->
    same_cook name ~typed ~factory:Nodes.Resample.factory values resample_input)
    [ "resample attribute sizing", Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/resample (sop/ext_resample_input) :use_segments false :segments_attribute "counts")|},
      ["use_segments", Param.Bool_value false; "segments_attribute", Text_value "counts"];
      "resample length sizing", Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/resample
   (sop/ext_resample_input)
   :use_segments false
   :use_maximum_segment_length true
   :maximum_segment_length 0.4)|},
      ["use_segments", Bool_value false; "use_maximum_segment_length", Bool_value true;
       "maximum_segment_length", Float_value 0.4];
      "resample explicit fields", Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/resample
   (sop/ext_resample_input)
   :group "first_curve"
   :segments 4
   :use_maximum_segment_length true
   :maximum_segment_length 0.4
   :segment_length_attribute "lengths"
   :segments_attribute "counts"
   :even_last_segment false
   :curve_u_attribute "u"
   :curve_number_attribute "curve"
   :distance_attribute "distance"
   :tangent_attribute "tangent")|},
      ["group", Text_value "first_curve"; "segments", Int_value 4;
       "use_maximum_segment_length", Bool_value true; "maximum_segment_length", Float_value 0.4;
       "segment_length_attribute", Text_value "lengths"; "segments_attribute", Text_value "counts";
       "even_last_segment", Bool_value false; "curve_u_attribute", Text_value "u";
       "curve_number_attribute", Text_value "curve"; "distance_attribute", Text_value "distance";
       "tangent_attribute", Text_value "tangent"] ];
  cache_identity "resample all fields" (Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/resample (sop/ext_resample_input) :segments_attribute "counts")|});
  List.iter (fun make ->
    check (rejected make)
      "resample refuses invalid numeric fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/resample (sop/ext_resample_input) :segments 0)|});
      (fun () -> Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/resample (sop/ext_resample_input) :use_segments false)|});
      (fun () -> Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/resample (sop/ext_resample_input) :curve_u_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/resample (sop/ext_resample_input) :curve_u_attribute "u" :distance_attribute "u")|});
      (fun () -> Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/resample
   (sop/ext_resample_input)
   :use_maximum_segment_length true
   :maximum_segment_length 0.0)|}) ];
  check (Result.is_error (Node.apply_parameters resampled ["use_maximum_segment_length", Bool_value true;
      "maximum_segment_length", Float_value 0.])) "resample rejects enabled zero-length edits";
  let carve_input = Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(-> (sop/set_float (sop/ext_resample_input) :owner "Primitive" :name "first_u" :value 0.2)
     (sop/set_float :owner "Primitive" :name "last_u" :value 0.8))|} in
  let carved = Lisp_sop.node ~with_:["carve_input", (carve_input)] {|(sop/carve (sop/ext_carve_input))|} in
  same_cook "carve defaults" ~typed:carved ~factory:Nodes.Carve.factory [] carve_input;
  cache_identity ~changes:["last", Float_value 0.75] "carve all fields" carved;
  List.iter (fun keep ->
    let choice = match keep with Rdk.Curve_ops.Inside -> "Inside"
      | Outside -> "Outside" | Inside_and_outside -> "Inside and outside" in
    same_cook ("carve " ^ choice) ~typed:(sop ~inputs:[carve_input] "carve" [kf "first" 0.2; kf "last" 0.8; ks "keep" choice])
      ~factory:Nodes.Carve.factory ["first", Float_value 0.2; "last", Float_value 0.8;
        "keep", Choice_value choice] carve_input)
    [Rdk.Curve_ops.Inside; Outside; Inside_and_outside];
  same_cook "carve explicit fields" ~typed:(Lisp_sop.node ~with_:["carve_input", (carve_input)] {|(sop/carve
   (sop/ext_carve_input)
   :group "first_curve"
   :relative_arc_length false
   :first 0.2
   :last 0.8
   :first_attribute "first_u"
   :last_attribute "last_u"
   :attribute_mode "Scale"
   :only_at_breakpoints true
   :cut_at_all_internal_breakpoints true
   :keep "Inside and outside"
   :divisions 2
   :keep_original true)|})
    ~factory:Nodes.Carve.factory ["group", Text_value "first_curve";
      "relative_arc_length", Bool_value false; "first", Float_value 0.2; "last", Float_value 0.8;
      "first_attribute", Text_value "first_u"; "last_attribute", Text_value "last_u";
      "attribute_mode", Choice_value "Scale"; "only_at_breakpoints", Bool_value true;
      "cut_at_all_internal_breakpoints", Bool_value true; "keep", Choice_value "Inside and outside";
      "divisions", Int_value 2; "keep_original", Bool_value true] carve_input;
  same_cook "carve equal extraction" ~typed:(Lisp_sop.node ~with_:["carve_input", (carve_input)] {|(sop/carve (sop/ext_carve_input) :first 0.5 :last 0.5 :extract_points true)|})
    ~factory:Nodes.Carve.factory ["first", Float_value 0.5; "last", Float_value 0.5;
      "extract_points", Bool_value true] carve_input;
  List.iter (fun make ->
    check (rejected make)
      "carve refuses invalid numeric fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["carve_input", (carve_input)] {|(sop/carve (sop/ext_carve_input) :first -0.1)|});
      (fun () -> Lisp_sop.node ~with_:["carve_input", (carve_input)] {|(sop/carve (sop/ext_carve_input) :first 0.8 :last 0.2)|});
      (fun () -> Lisp_sop.node ~with_:["carve_input", (carve_input)] {|(sop/carve (sop/ext_carve_input) :first 0.5 :last 0.5)|}) ];
  check (Result.is_error (Node.apply_parameters carved ["last", Float_value 0.]))
    "carve rejects collapsed non-extraction edits";
  let loft_input = Lisp_sop.node ~with_:["resample_input", (resample_input)] {|(sop/group_range (sop/ext_resample_input) :owner "Primitives" :name "both_curves" :end_ 1)|} in
  List.iter (fun (name, factory, default, explicit) ->
    same_cook ~optional_inputs:[Some loft_input; None] (name ^ " Lisp defaults")
      ~typed:(default None) ~factory [] loft_input;
    same_cook ~optional_inputs:[Some loft_input; Some loft_input] (name ^ " rest input")
      ~typed:(default (Some loft_input)) ~factory [] loft_input;
    cache_identity name (default None);
    same_cook ~optional_inputs:[Some loft_input; Some loft_input] (name ^ " explicit fields")
      ~typed:(explicit ()) ~factory ["group", Text_value "both_curves";
        "connect_closest_ends", Bool_value false; "minimize", Choice_value "Three point distance";
        "u_wrap", Bool_value true; "v_wrap", Bool_value true; "keep_primitives", Bool_value true;
        "output_group", Text_value "surface"; "collinearity_tolerance", Float_value 0.1;
        "recompute_normals", Bool_value false] loft_input)
    [ "poly loft", Nodes.Poly_loft.factory, (fun rest -> sop ~inputs:(loft_input :: Option.to_list rest) "poly_loft" []),
      (fun () -> Lisp_sop.node ~with_:["loft_input", (loft_input)] {|(sop/poly_loft
   (sop/ext_loft_input)
   (sop/ext_loft_input)
   :group "both_curves"
   :connect_closest_ends false
   :minimize "Three point distance"
   :u_wrap true
   :v_wrap true
   :keep_primitives true
   :output_group "surface"
   :collinearity_tolerance 0.1
   :recompute_normals false)|});
      "skin", Nodes.Skin.factory, (fun rest -> sop ~inputs:(loft_input :: Option.to_list rest) "skin" []),
      (fun () -> Lisp_sop.node ~with_:["loft_input", (loft_input)] {|(sop/skin
   (sop/ext_loft_input)
   (sop/ext_loft_input)
   :group "both_curves"
   :connect_closest_ends false
   :minimize "Three point distance"
   :u_wrap true
   :v_wrap true
   :keep_primitives true
   :output_group "surface"
   :collinearity_tolerance 0.1
   :recompute_normals false)|}) ];
  List.iter (fun make ->
    check (rejected make)
      "loft/skin refuse invalid collinearity tolerance at construction")
    [ (fun () -> Lisp_sop.node ~with_:["loft_input", (loft_input)] {|(sop/poly_loft (sop/ext_loft_input) :collinearity_tolerance 1.1)|}) ];
  let warped = Lisp_sop.node {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 6 :rows 6 :size 2.0)
     (sop/point_jitter :seed 7 :scale 0.3))|} in
  let remesh_input = Lisp_sop.node {|(-> (sop/grid :width_mode "Auto" :height_mode "Auto" :columns 2 :rows 2 :size 0.5)
     (sop/group_range :name "fixed" :end_ 0)
     (sop/group_edges :name "rim" :incidence "Boundary")
     (sop/set_float :name "mesh_target" :value 0.3))|} in
  let remeshed = Lisp_sop.node ~with_:["remesh_input", (remesh_input)] {|(sop/remesh (sop/ext_remesh_input))|} in
  same_cook "remesh Lisp defaults" ~typed:remeshed ~factory:Nodes.Remesh.factory [] remesh_input;
  cache_identity "remesh" remeshed;
  let explicit_remesh = Lisp_sop.node ~with_:["remesh_input", (remesh_input)] {|(sop/remesh
   (sop/ext_remesh_input)
   :iterations 1
   :smoothing 0.25
   :project false
   :use_input_points_only true
   :hard_point_group "fixed"
   :hard_edge_group "rim"
   :target_size_attribute "mesh_target"
   :preserve_uv_seams false
   :uv_attribute "other_uv"
   :output_hard_edges "hard"
   :output_mesh_size "lengths"
   :output_quality "quality"
   :recompute_point_normals false
   :target_length 0.3)|} in
  same_cook "remesh explicit fields" ~typed:explicit_remesh ~factory:Nodes.Remesh.factory
    [ "iterations", Param.Int_value 1; "smoothing", Float_value 0.25;
      "project", Bool_value false; "use_input_points_only", Bool_value true;
      "hard_point_group", Text_value "fixed"; "hard_edge_group", Text_value "rim";
      "target_size_attribute", Text_value "mesh_target";
      "preserve_uv_seams", Bool_value false; "uv_attribute", Text_value "other_uv";
      "output_hard_edges", Text_value "hard"; "output_mesh_size", Text_value "lengths";
      "output_quality", Text_value "quality"; "recompute_point_normals", Bool_value false;
      "target_length", Float_value 0.3 ] remesh_input;
  cache_identity "remesh explicit" explicit_remesh;
  List.iter (fun make ->
    check (rejected make)
      "remesh refuses invalid numeric fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["remesh_input", (remesh_input)] {|(sop/remesh (sop/ext_remesh_input) :target_length 0.0)|});
      (fun () -> Lisp_sop.node ~with_:["remesh_input", (remesh_input)] {|(sop/remesh (sop/ext_remesh_input) :iterations -1)|});
      (fun () -> Lisp_sop.node ~with_:["remesh_input", (remesh_input)] {|(sop/remesh (sop/ext_remesh_input) :smoothing 1.1)|}) ];
  check (Result.is_error (Node.apply_parameters remeshed ["target_length", Param.Float_value 0.]))
    "remesh rejects zero target-length edits";
  let equalize_input = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [4.0 0.0 0.0] [6.0 0.0 0.0]))
     (sop/group_edges :name "uneven"))|} in
  let equalized = Lisp_sop.node ~with_:["equalize_input", (equalize_input)] {|(sop/edge_equalize (sop/ext_equalize_input))|} in
  same_cook "edge_equalize Lisp defaults" ~typed:equalized
    ~factory:Nodes.Edge_equalize.factory [] equalize_input;
  cache_identity "edge_equalize" equalized;
  List.iter (fun (_method, choice) ->
    same_cook ("edge_equalize " ^ choice)
      ~typed:(sop ~inputs:[equalize_input] "edge_equalize"
        [ks "group" "uneven"; ks "method_" choice; ki "iterations" 80; kf "tolerance" 1e-7; ks "output_group" "equalized"])
      ~factory:Nodes.Edge_equalize.factory
      [ "group", Param.Text_value "uneven"; "method_", Choice_value choice;
        "iterations", Int_value 80; "tolerance", Float_value 1e-7;
        "output_group", Text_value "equalized" ] equalize_input)
    [ Rdk.Edge_ops.Equalize_average, "Average"; Rdk.Edge_ops.Equalize_longest, "Longest";
      Rdk.Edge_ops.Equalize_shortest, "Shortest" ];
  List.iter (fun tolerance ->
    check (rejected (fun () -> Lisp_sop.node ~with_:["equalize_input", (equalize_input)] (Printf.sprintf {|(sop/edge_equalize (sop/ext_equalize_input) :tolerance %s)|} ((Lisp_sop.float tolerance)))))
      "edge_equalize refuses invalid tolerance at construction") [0.; -1.];
  check (Result.is_error (Node.apply_parameters equalized
      ["tolerance", Param.Float_value 0.]))
    "edge_equalize returns an error for zero-tolerance edits";
  let delete_input = Lisp_sop.node ~with_:["warped", (warped)] {|(-> (sop/set_float (sop/ext_warped) :name "keep" :value 1.0)
     (sop/set_float :name "drop" :value 2.0))|} in
  let deleted = Lisp_sop.node ~with_:["delete_input", (delete_input)] {|(sop/delete_attributes (sop/ext_delete_input))|} in
  same_cook "delete_attributes Lisp defaults" ~typed:deleted
    ~factory:Nodes.Delete_attributes.factory ~optional_inputs:[Some delete_input; None] [] delete_input;
  cache_identity "delete_attributes" deleted;
  let reference = Lisp_sop.node ~with_:["in1038", (Lisp_sop.snapshot (Rdk.Line_geometry.points [|0., 0., 0.|]))] {|(sop/set_float (sop/ext_in1038) :name "keep" :value 1.0)|} in
  let delete_values = [ "delete_non_selected", Param.Bool_value true;
      "point_pattern", Text_value "keep"; "vertex_pattern", Text_value "N";
      "primitive_pattern", Text_value "material"; "detail_pattern", Text_value "metadata" ] in
  let referenced_delete = Lisp_sop.node ~with_:["delete_input", (delete_input); "reference", (reference)] {|(sop/delete_attributes
   (sop/ext_delete_input)
   (sop/ext_reference)
   :delete_non_selected true
   :point_pattern "keep"
   :vertex_pattern "N"
   :primitive_pattern "material"
   :detail_pattern "metadata")|} in
  same_cook "delete_attributes reference and patterns" ~typed:referenced_delete
    ~factory:Nodes.Delete_attributes.factory delete_values delete_input ~inputs:[reference];
  cache_identity "delete_attributes reference" referenced_delete;
  List.iter (fun make ->
    check (rejected make)
      "delete_attributes refuses invalid patterns at construction")
    [ (fun () -> Lisp_sop.node ~with_:["delete_input", (delete_input)] {|(sop/delete_attributes (sop/ext_delete_input) :point_pattern "broken[")|});
      (fun () -> Lisp_sop.node ~with_:["delete_input", (delete_input)] {|(sop/delete_attributes (sop/ext_delete_input) :vertex_pattern "broken[")|});
      (fun () -> Lisp_sop.node ~with_:["delete_input", (delete_input)] {|(sop/delete_attributes (sop/ext_delete_input) :primitive_pattern "broken[")|});
      (fun () -> Lisp_sop.node ~with_:["delete_input", (delete_input)] {|(sop/delete_attributes (sop/ext_delete_input) :detail_pattern "broken[")|}) ];
  check (Result.is_error (Node.apply_parameters deleted
      ["point_pattern", Param.Text_value "broken["]))
    "delete_attributes returns an error for invalid pattern edits";
  let transport_input = Lisp_sop.node {|(-> (sop/curve (list [0.0 0.0 0.0] [1.0 0.0 0.0] [3.0 0.0 0.0]))
     (-> (sop/curve (list [0.0 2.0 0.0] [2.0 2.0 0.0] [5.0 2.0 0.0])) (sop/merge))
     (sop/set_float :value 1.0)
     (sop/set_float :owner "Vertex" :value 2.0)
     (sop/group_range :owner "Primitives" :name "first" :end_ 0))|} in
  let curve_transport = Lisp_sop.node ~with_:["transport_input", (transport_input)] {|(sop/edge_transport_curves (sop/ext_transport_input))|} in
  same_cook "edge_transport_curves Lisp defaults" ~typed:curve_transport
    ~factory:Nodes.Edge_transport_curves.factory [] transport_input;
  cache_identity ~companions:transport_cache_companions "edge_transport_curves" curve_transport;
  let explicit_curve = Lisp_sop.node ~with_:["transport_input", (transport_input)] {|(sop/edge_transport_curves
   (sop/ext_transport_input)
   :primitive_group "first"
   :owner "Vertex"
   :direction "Backward"
   :operation "Total"
   :root_value "Hold"
   :integrate_constant true
   :scale_by_edge_length true
   :normalization "Per component")|} in
  same_cook "edge_transport_curves explicit fields" ~typed:explicit_curve
    ~factory:Nodes.Edge_transport_curves.factory
    [ "primitive_group", Param.Text_value "first"; "owner", Choice_value "Vertex";
      "direction", Choice_value "Backward"; "operation", Choice_value "Total";
      "root_value", Choice_value "Hold"; "integrate_constant", Bool_value true;
      "scale_by_edge_length", Bool_value true; "normalization", Choice_value "Per component" ] transport_input;
  cache_identity ~companions:transport_cache_companions "edge_transport_curves explicit" explicit_curve;
  let parent_geometry = cook 1 transport_input in
  let parent_attribute = Rdk.Attribute.create_owned ~owner:Rdk.Attribute.Point ~name:"parent"
      (Rdk.Attribute.Int (Array.init (Rdk.Geometry.point_count parent_geometry)
        (fun index -> max 0 (index - 1)))) |> Result.get_ok in
  let parent_input = Lisp_sop.node ~with_:["in1047", (Lisp_sop.snapshot (Rdk.Geometry.with_attribute parent_attribute parent_geometry |> Result.get_ok))] {|(sop/group_range (sop/ext_in1047) :name "all" :range_mode "From ends")|} in
  let parent_transport = Lisp_sop.node ~with_:["parent_input", (parent_input)] {|(sop/edge_transport_parent (sop/ext_parent_input))|} in
  same_cook "edge_transport_parent Lisp defaults" ~typed:parent_transport
    ~factory:Nodes.Edge_transport_parent.factory [] parent_input;
  cache_identity ~companions:transport_cache_companions "edge_transport_parent" parent_transport;
  let explicit_parent = Lisp_sop.node ~with_:["parent_input", (parent_input)] {|(sop/edge_transport_parent
   (sop/ext_parent_input)
   :point_group "all"
   :direction "Backward"
   :operation "Total"
   :root_value "Hold"
   :integrate_constant true
   :scale_by_edge_length true
   :split "Split"
   :merge "Maximum"
   :normalization "Global")|} in
  same_cook "edge_transport_parent explicit fields" ~typed:explicit_parent
    ~factory:Nodes.Edge_transport_parent.factory
    [ "point_group", Param.Text_value "all"; "direction", Choice_value "Backward";
      "operation", Choice_value "Total"; "root_value", Choice_value "Hold";
      "integrate_constant", Bool_value true; "scale_by_edge_length", Bool_value true;
      "split", Choice_value "Split"; "merge", Choice_value "Maximum";
      "normalization", Choice_value "Global" ] parent_input;
  cache_identity ~companions:transport_cache_companions "edge_transport_parent explicit" explicit_parent;
  List.iter (fun make ->
    check (rejected make)
      "edge transports refuse invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["transport_input", (transport_input)] {|(sop/edge_transport_curves (sop/ext_transport_input) :attribute "P")|});
      (fun () -> sop ~inputs:[transport_input] "edge_transport_curves" [ks "owner" "Primitive"]);
      (fun () -> Lisp_sop.node ~with_:["parent_input", (parent_input)] {|(sop/edge_transport_parent (sop/ext_parent_input) :attribute "")|});
      (fun () -> Lisp_sop.node ~with_:["parent_input", (parent_input)] {|(sop/edge_transport_parent (sop/ext_parent_input) :parent_attribute " ")|}) ];
  let normal_group = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_normal (sop/ext_warped))|} in
  same_cook "group_normal Lisp defaults" ~typed:normal_group
    ~factory:Nodes.Group_normal.factory [] warped;
  cache_identity "group_normal" normal_group;
  List.iter (fun (_owner, choice) ->
    same_cook ("group_normal " ^ choice)
      ~typed:(sop ~inputs:[warped] "group_normal" [ks "owner" choice; kb "use_existing_normal" false])
      ~factory:Nodes.Group_normal.factory
      ["owner", Param.Choice_value choice; "use_existing_normal", Bool_value false] warped)
    [ Rdk.Group_ops.Group_points, "Points";
      Rdk.Group_ops.Group_primitives, "Primitives"; Rdk.Group_ops.Group_edges, "Edges" ];
  let normal_input = Lisp_sop.node ~with_:["warped", (warped)] (Printf.sprintf {|(-> (sop/set_vector (sop/ext_warped) :name "N" :value %s)
     (sop/group_range :name "base" :range_mode "From ends"))|} ((Lisp_sop.vec3 Vec3.unit_y))) in
  let explicit_normal = Lisp_sop.node ~with_:["normal_input", (normal_input)] (Printf.sprintf {|(sop/group_normal
   (sop/ext_normal_input)
   :owner "Points"
   :name "facing"
   :normal_attribute "N"
   :base "base"
   :include_opposite true
   :merge "Union"
   :direction [1.0 1.0 0.0]
   :spread_angle %s)|} ((Lisp_sop.float (Float.pi /. 2.)))) in
  same_cook "group_normal explicit fields" ~typed:explicit_normal
    ~factory:Nodes.Group_normal.factory
    [ "owner", Param.Choice_value "Points"; "name", Text_value "facing";
      "normal_attribute", Text_value "N"; "base", Text_value "base";
      "include_opposite", Bool_value true; "merge", Choice_value "Union";
      "direction_x", Float_value 1.; "direction_y", Float_value 1.;
      "direction_z", Float_value 0.; "spread_angle", Float_value (Float.pi /. 2.) ] normal_input;
  cache_identity "group_normal explicit" explicit_normal;
  same_cook "group_normal blank optional names"
    ~typed:(Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_normal (sop/ext_warped))|})
    ~factory:Nodes.Group_normal.factory [] warped;
  List.iter (fun make ->
    check (rejected make)
      "group_normal refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["warped", (warped)] (Printf.sprintf {|(sop/group_normal (sop/ext_warped) :direction %s)|} ((Lisp_sop.vec3 Vec3.zero))));
      (fun () -> sop ~inputs:[warped] "group_normal" [ks "owner" "Vertices"]);
      (fun () -> Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_normal (sop/ext_warped) :spread_angle -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["warped", (warped)] (Printf.sprintf {|(sop/group_normal (sop/ext_warped) :spread_angle %s)|} ((Lisp_sop.float (Float.pi +. 0.1))))) ];
  check (Result.is_error (Node.apply_parameters normal_group
      ["direction_y", Param.Float_value 0.]))
    "group_normal rejects edits that make the direction zero";
  let ordered_default = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/ordered_group (sop/ext_warped))|} in
  same_cook "ordered_group Lisp defaults" ~typed:ordered_default
    ~factory:Nodes.Ordered_group.factory [] warped;
  cache_identity ~changes:["elements", Param.Text_value "1 0"]
    "ordered_group" ordered_default;
  List.iter (fun (_owner, choice) ->
    same_cook ("ordered_group " ^ choice)
      ~typed:(sop ~inputs:[warped] "ordered_group" [ks "owner" choice; ks "name" "authored"; ks "elements" "2 0 3"])
      ~factory:Nodes.Ordered_group.factory
      [ "owner", Param.Choice_value choice; "name", Text_value "authored";
        "elements", Text_value "2 0 3" ] warped)
    [ Rdk.Group.Point, "Points"; Rdk.Group.Vertex, "Vertices";
      Rdk.Group.Primitive, "Primitives" ];
  check (rejected (fun () -> sop ~inputs:[warped] "ordered_group" [ks "elements" "0 -1"]))
    "ordered_group refuses negative indices at construction";
  let piece_input = Lisp_sop.node ~with_:["warped", (warped)] {|(-> (sop/set_int (sop/ext_warped) :name "piece")
     (sop/set_int :owner "Primitive" :name "piece"))|} in
  let centroid = Lisp_sop.node ~with_:["piece_input", (piece_input)] {|(sop/extract_centroid (sop/ext_piece_input))|} in
  same_cook "extract_centroid Lisp defaults" ~typed:centroid
    ~factory:Nodes.Extract_centroid.factory [] piece_input;
  cache_identity "extract_centroid" centroid;
  List.iter (fun choice ->
    same_cook ("extract_centroid " ^ choice)
      ~typed:(sop ~inputs:[piece_input] "extract_centroid" [ks "run_over" choice])
      ~factory:Nodes.Extract_centroid.factory
      ["run_over", Param.Choice_value choice] piece_input)
    [ "Detail"; "Primitives"; "Point pieces"; "Primitive pieces" ];
  List.iter (fun (_method, choice) ->
    same_cook ("extract_centroid " ^ choice)
      ~typed:(sop ~inputs:[piece_input] "extract_centroid" [ks "method_" choice])
      ~factory:Nodes.Extract_centroid.factory
      ["method_", Param.Choice_value choice] piece_input)
    [ Rdk.Curve_topology.Centroid_point_mass, "Point mass";
      Rdk.Curve_topology.Centroid_bounding_box, "Bounding box";
      Rdk.Curve_topology.Centroid_convex_hull, "Convex hull" ];
  let piece_centroid = Lisp_sop.node ~with_:["piece_input", (piece_input)] {|(sop/extract_centroid
   (sop/ext_piece_input)
   :run_over "Primitive pieces"
   :source_primitive_attribute "source"
   :piece_output_attribute "island")|} in
  same_cook "extract_centroid output fields" ~typed:piece_centroid
    ~factory:Nodes.Extract_centroid.factory
    [ "run_over", Param.Choice_value "Primitive pieces";
      "piece_attribute", Text_value "piece";
      "source_primitive_attribute", Text_value "source";
      "piece_output_attribute", Text_value "island" ] piece_input;
  cache_identity "extract_centroid pieces" piece_centroid;
  List.iter (fun make ->
    check (rejected make)
      "extract_centroid refuses invalid attributes at construction")
    [ (fun () -> Lisp_sop.node ~with_:["piece_input", (piece_input)] {|(sop/extract_centroid (sop/ext_piece_input) :run_over "Point pieces" :piece_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["piece_input", (piece_input)] {|(sop/extract_centroid (sop/ext_piece_input) :source_primitive_attribute "P")|});
      (fun () -> Lisp_sop.node ~with_:["piece_input", (piece_input)] {|(sop/extract_centroid (sop/ext_piece_input) :piece_output_attribute "P")|}) ];
  let original_key = Node.parameter_key piece_centroid in
  check (Result.is_error (Node.apply_parameters piece_centroid
      ["piece_attribute", Param.Text_value "P"]))
    "extract_centroid returns an error for invalid record edits";
  check (Node.parameter_key piece_centroid = original_key)
    "extract_centroid preserves the existing node after invalid edits";
  List.iter (fun (name, typed, factory) ->
    same_cook name ~typed ~factory [] warped;
    check (Node.parameter_fields typed = []) (name ^ ": has no artificial fields");
    cache_identity name typed)
    [ "null", Lisp_sop.node ~with_:["warped", (warped)] {|(sop/null (sop/ext_warped))|}, Nodes.Null.factory;
      "compact_points", Lisp_sop.node ~with_:["warped", (warped)] {|(sop/compact_points (sop/ext_warped))|}, Nodes.Compact_points.factory ];
  let ranged = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_range (sop/ext_warped))|} in
  same_cook "group_range Lisp defaults" ~typed:ranged
    ~factory:Nodes.Group_range.factory [] warped;
  cache_identity "group_range" ranged;
  List.iter (fun choice ->
    same_cook ("group_range " ^ choice)
      ~typed:(sop ~inputs:[warped] "group_range"
        [ks "range_mode" choice; ki "start" 1; ki "end_" 4; ki "end_offset" 2; ki "length" 3; ki "partition" 1;
         ki "partitions" 3])
      ~factory:Nodes.Group_range.factory
      [ "range_mode", Param.Choice_value choice; "start", Int_value 1;
        "end_", Int_value 4; "end_offset", Int_value 2; "length", Int_value 3;
        "partition", Int_value 1; "partitions", Int_value 3 ] warped)
    [ "Start and end"; "From ends"; "Start and length"; "Partition" ];
  List.iter (fun choice ->
    same_cook ("group_range " ^ choice)
      ~typed:(sop ~inputs:[warped] "group_range" [ks "connectivity_mode" choice; kb "use_region" true; ki "region" 0])
      ~factory:Nodes.Group_range.factory
      [ "connectivity_mode", Param.Choice_value choice;
        "use_region", Bool_value true; "region", Int_value 0 ] warped)
    [ "None"; "Disconnected regions"; "Connected with seams" ];
  let range_input = Lisp_sop.node ~with_:["warped", (warped)] {|(-> (sop/group_range (sop/ext_warped) :name "cut" :end_ 1)
     (sop/group_range :name "base" :range_mode "From ends"))|} in
  let advanced_range = Lisp_sop.node ~with_:["range_input", (range_input)] {|(sop/group_range
   (sop/ext_range_input)
   :name "selected"
   :base "base"
   :invert true
   :merge "Union"
   :end_ 3
   :use_filter true
   :filter_select 2
   :filter_of 3
   :filter_offset 1
   :connectivity_mode "Connected with seams"
   :use_region true
   :connectivity_attributes "P"
   :connectivity_tolerance 0.25
   :use_collision true
   :collision_owner "Points"
   :collision_pattern "cut"
   :keep_boundary true
   :remove_other_regions true)|} in
  same_cook "group_range advanced fields" ~typed:advanced_range
    ~factory:Nodes.Group_range.factory
    [ "name", Param.Text_value "selected"; "base", Text_value "base";
      "invert", Bool_value true; "merge", Choice_value "Union";
      "start", Int_value 0; "end_", Int_value 3; "use_filter", Bool_value true;
      "filter_select", Int_value 2; "filter_of", Int_value 3; "filter_offset", Int_value 1;
      "connectivity_mode", Choice_value "Connected with seams";
      "use_region", Bool_value true; "region", Int_value 0;
      "connectivity_attributes", Text_value "P"; "connectivity_tolerance", Float_value 0.25;
      "use_collision", Bool_value true; "collision_owner", Choice_value "Points";
      "collision_pattern", Text_value "cut"; "keep_boundary", Bool_value true;
      "remove_other_regions", Bool_value true ] range_input;
  cache_identity "group_range advanced" advanced_range;
  List.iter (fun make ->
    check (rejected make)
      "group_range refuses invalid fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_range (sop/ext_warped) :length -1)|});
      (fun () -> Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_range (sop/ext_warped) :partition -1)|});
      (fun () -> Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_range (sop/ext_warped) :partitions 0)|});
      (fun () -> Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_range (sop/ext_warped) :filter_select -1)|});
      (fun () -> Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_range (sop/ext_warped) :filter_of 0)|});
      (fun () -> Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_range (sop/ext_warped) :region -1)|}) ];
  let indexed = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/enumerate (sop/ext_warped))|} in
  let sorted = Lisp_sop.node ~with_:["indexed", (indexed)] {|(sop/sort (sop/ext_indexed))|} in
  same_cook "sort Lisp defaults" ~typed:sorted ~factory:Nodes.Sort.factory [] indexed;
  cache_identity "sort" sorted;
  List.iter (fun choice ->
    same_cook ("sort " ^ choice) ~typed:(sop ~inputs:[indexed] "sort" [ks "key" choice])
      ~factory:Nodes.Sort.factory ["key", Param.Choice_value choice] indexed)
    [ "X"; "Y"; "Z"; "Distance to point"; "Along vector"; "Attribute component"; "Vertex order";
      "Primitive index"; "Spatial locality"; "Random"; "Index attribute"; "Reverse"; "Shift" ];
  same_cook "sort explicit vector fields"
    ~typed:(Lisp_sop.node ~with_:["indexed", (indexed)] {|(sop/sort
   (sop/ext_indexed)
   :key "Along vector"
   :x 2.0
   :y 3.0
   :z 4.0
   :owner "Primitives"
   :descending true
   :output_indices "rank")|})
    ~factory:Nodes.Sort.factory
    [ "key", Param.Choice_value "Along vector"; "x", Float_value 2.;
      "y", Float_value 3.; "z", Float_value 4.; "owner", Choice_value "Primitives";
      "descending", Bool_value true; "output_indices", Text_value "rank" ] indexed;
  same_cook "sort explicit random seed"
    ~typed:(Lisp_sop.node ~with_:["indexed", (indexed)] {|(sop/sort (sop/ext_indexed) :key "Random" :seed 73421)|})
    ~factory:Nodes.Sort.factory
    [ "key", Param.Choice_value "Random"; "seed", Int_value 73421 ] indexed;
  List.iter (fun make ->
    check (rejected make)
      "sort refuses invalid numeric fields at construction")
    [ (fun () -> Lisp_sop.node ~with_:["indexed", (indexed)] {|(sop/sort (sop/ext_indexed) :component -1)|}) ];
  let default_box = Lisp_sop.node {|(sop/box)|} in
  same_generator "box Lisp defaults" ~typed:default_box ~factory:Nodes.Box.factory [];
  let auto_box = Lisp_sop.node {|(sop/box :normals "Auto")|} in
  same_generator "box Auto normals" ~typed:auto_box ~factory:Nodes.Box.factory
    ["normals", Param.Choice_value "Auto"];
  let point_box = Lisp_sop.node {|(sop/box :normals "Auto" :connectivity "Lattice points")|} in
  same_generator "box Auto point normals" ~typed:point_box ~factory:Nodes.Box.factory
    [ "normals", Param.Choice_value "Auto";
      "connectivity", Choice_value "Lattice points" ];
  check (Node.parameter_key default_box <> Node.parameter_key auto_box)
    "box: Auto has its own cache identity";
  cache_identity "box" default_box;
  let box = Lisp_sop.node {|(sop/box :normals "Auto" :connectivity "Triangles" :size [2.0 2.0 2.0])|} in
  let non_planar = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_non_planar (sop/ext_warped) :tolerance 0.01 :name "warped")|} in
  same_cook "group_non_planar" ~typed:non_planar ~factory:Nodes.Group_non_planar.factory
    [ "tolerance", Param.Float_value 0.01; "name", Text_value "warped" ] warped;
  let backface = Lisp_sop.node ~with_:["box", (box)] {|(sop/group_backface (sop/ext_box) :viewpoint [0.0 0.0 5.0] :name "hidden" :merge "Union")|} in
  same_cook "group_backface" ~typed:backface ~factory:Nodes.Group_backface.factory
    [ "viewpoint_z", Param.Float_value 5.; "name", Text_value "hidden";
      "merge", Choice_value "Union" ] box;
  let unshared = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_unshared (sop/ext_warped) :owner "Points" :name "rim")|} in
  same_cook "group_unshared" ~typed:unshared ~factory:Nodes.Group_unshared.factory
    [ "owner", Param.Choice_value "Points"; "name", Text_value "rim" ] warped;
  let edges = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_edges
   (sop/ext_warped)
   :use_min_length true
   :name "rim"
   :incidence "Boundary"
   :min_length 0.1)|} in
  same_cook "group_edges" ~typed:edges ~factory:Nodes.Group_edges.factory
    [ "name", Param.Text_value "rim"; "incidence", Choice_value "Boundary";
      "use_min_length", Bool_value true; "min_length", Float_value 0.1 ] warped;
  let limited_edges = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_edges
   (sop/ext_warped)
   :use_min_length true
   :min_length 0.1
   :use_max_length true
   :max_length 10.0
   :use_min_angle true
   :use_max_angle true)|} in
  same_cook "group_edges enabled limits" ~typed:limited_edges ~factory:Nodes.Group_edges.factory
    [ "use_min_length", Param.Bool_value true; "min_length", Float_value 0.1;
      "use_max_length", Bool_value true; "max_length", Float_value 10.;
      "use_min_angle", Bool_value true; "use_max_angle", Bool_value true ] warped;
  let disabled_limits = Lisp_sop.node ~with_:["warped", (warped)] (Printf.sprintf {|(sop/group_edges
   (sop/ext_warped)
   :min_length 999.0
   :max_length 0.001
   :min_angle %s
   :max_angle 0.01)|} ((Lisp_sop.float Float.pi))) in
  same_cook "group_edges disabled limits" ~typed:disabled_limits ~factory:Nodes.Group_edges.factory
    [ "min_length", Param.Float_value 999.; "max_length", Float_value 0.001;
      "min_angle", Float_value Float.pi; "max_angle", Float_value 0.01 ] warped;
  check (equal_geometry (cook 1 disabled_limits) (cook 1 (Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_edges (sop/ext_warped))|})))
    "group_edges: inactive limits preserve default geometry";
  cache_identity "group_edges limits" limited_edges;
  List.iter (fun make ->
    check (rejected make)
      "group_edges refuses invalid limits at construction")
    [ (fun () -> Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_edges (sop/ext_warped) :min_length -1.0)|}) ];
  let random = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_random (sop/ext_warped) :seed 5 :probability 0.4)|} in
  same_cook "group_random" ~typed:random ~factory:Nodes.Group_random.factory
    [ "seed", Param.Int_value 5; "probability", Float_value 0.4; "name", Text_value "random" ]
    warped;
  let seeded = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_range (sop/ext_warped) :name "seed" :end_ 0)|} in
  let depth = Lisp_sop.node ~with_:["seeded", (seeded)] {|(sop/group_edge_depth (sop/ext_seeded) :depth 2 :name "near")|} in
  same_cook "group_edge_depth" ~typed:depth ~factory:Nodes.Group_edge_depth.factory
    [ "depth", Param.Int_value 2; "name", Text_value "near" ] seeded;
  let components = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_boundary_components (sop/ext_warped) :prefix "rim")|} in
  same_cook "group_boundary_components" ~typed:components
    ~factory:Nodes.Group_boundary_components.factory [ "prefix", Param.Text_value "rim" ] warped;
  let boundary = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_from_attribute_boundary (sop/ext_warped) :name "seams" :tolerance 1e-06)|} in
  same_cook "group_from_attribute_boundary" ~typed:boundary
    ~factory:Nodes.Group_from_attribute_boundary.factory
    [ "name", Param.Text_value "seams"; "tolerance", Float_value 1e-6 ] warped;
  let grouped = Lisp_sop.node ~with_:["box", (box)] {|(sop/group_range (sop/ext_box) :owner "Primitives" :name "half" :end_ 2)|} in
  let bounded = Lisp_sop.node ~with_:["box", (box)] {|(sop/group_bounds (sop/ext_box) :name "bounded" :size [1.5 2.0 2.0])|} in
  same_cook "group_bounds box" ~typed:bounded ~factory:Nodes.Group_bounds.factory
    [ "name", Param.Text_value "bounded";
      "size_x", Float_value 1.5; "size_y", Float_value 2.; "size_z", Float_value 2. ] box;
  same_cook "group_bounds sphere"
    ~typed:(Lisp_sop.node ~with_:["box", (box)] {|(sop/group_bounds (sop/ext_box) :shape "Sphere" :radius 1.5)|})
    ~factory:Nodes.Group_bounds.factory
    [ "shape", Param.Choice_value "Sphere"; "radius", Float_value 1.5 ] box;
  same_cook "group_bounds defaults" ~typed:(Lisp_sop.node ~with_:["box", (box)] {|(sop/group_bounds (sop/ext_box))|})
    ~factory:Nodes.Group_bounds.factory [] box;
  cache_identity "group_bounds" bounded;
  List.iter (fun build ->
    check (rejected build)
      "group_bounds refuses invalid bounds at construction")
    [ (fun () -> Lisp_sop.node ~with_:["box", (box)] {|(sop/group_bounds (sop/ext_box) :size [-1.0 1.0 1.0])|}) ];
  let promotions = Lisp_sop.node ~with_:["seeded", (seeded)] {|(sop/group_promotions (sop/ext_seeded))|} in
  let ranges = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_ranges (sop/ext_warped))|} in
  same_cook "group_promotions defaults" ~typed:promotions
    ~factory:Nodes.Group_promotions.factory [] seeded;
  same_cook "group_ranges defaults" ~typed:ranges
    ~factory:Nodes.Group_ranges.factory [] warped;
  List.iter (fun (name, node) ->
    cache_identity ~changes:["rules", Param.Text_value ""] name node)
    ["group_promotions", promotions; "group_ranges", ranges];
  List.iter (fun (name, node, factory, input) ->
    same_cook (name ^ " empty rules") ~typed:node ~factory
      ["rules", Param.Text_value ""] input;
    check (Node.operation node = name && Node.parameter_key node <> "")
      (name ^ ": empty rules retain node identity");
    check (equal_geometry (cook 1 node) (cook 1 input))
      (name ^ ": empty rules preserve input geometry"))
    [ "group_promotions", sop ~inputs:[seeded] "group_promotions" [ks "rules" ""],
        Nodes.Group_promotions.factory, seeded;
      "group_ranges", sop ~inputs:[warped] "group_ranges" [ks "rules" ""],
        Nodes.Group_ranges.factory, warped ];
  let inverted = sop ~inputs:[grouped] "group_invert" [ks "owner" "Primitives"; ks "pattern" "half"; ks "new_name" "other"] in
  same_cook "group_invert" ~typed:inverted ~factory:Nodes.Group_invert.factory
    [ "owner", Param.Choice_value "Primitives";
      "pattern", Text_value "half"; "new_name", Text_value "other" ] grouped;
  same_cook "group_invert Any defaults" ~typed:(Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/group_invert (sop/ext_grouped))|})
    ~factory:Nodes.Group_invert.factory [] grouped;
  same_cook "group_invert blank new name"
    ~typed:(Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/group_invert (sop/ext_grouped))|})
    ~factory:Nodes.Group_invert.factory [] grouped;
  let combined = Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/group_combine
   (sop/ext_grouped)
   :owner "Primitives"
   :name "complement"
   :base_pattern "half"
   :base_inverted true)|} in
  same_cook "group_combine" ~typed:combined ~factory:Nodes.Group_combine.factory
    [ "owner", Param.Choice_value "Primitives";
      "name", Text_value "complement"; "base_pattern", Text_value "half";
      "base_inverted", Bool_value true ] grouped;
  same_cook "group_combine defaults" ~typed:(Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/group_combine (sop/ext_grouped))|})
    ~factory:Nodes.Group_combine.factory [] grouped;
  let piece_source = Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/set_int (sop/ext_grouped) :owner "Primitive" :name "piece")|} in
  let separated = Lisp_sop.node ~with_:["piece_source", (piece_source)] {|(sop/separate_pieces (sop/ext_piece_source) :gap 0.2)|} in
  same_cook "separate_pieces" ~typed:separated ~factory:Nodes.Separate_pieces.factory
    [ "piece_attribute", Param.Text_value "piece"; "gap", Float_value 0.2 ] piece_source;
  List.iter (fun (name, node) -> cache_identity name node)
    [ "group_invert", inverted; "separate_pieces", separated ];
  cache_identity ~changes:["steps", Param.Text_value "union\thalf\tfalse"] "group_combine" combined;
  let named = Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/name_from_groups (sop/ext_grouped) :pattern "half")|} in
  same_cook "name_from_groups" ~typed:named ~factory:Nodes.Name_from_groups.factory
    [ "pattern", Param.Text_value "half"; "overlap", Choice_value "First group" ] grouped;
  let regrouped = Lisp_sop.node ~with_:["named", (named)] {|(sop/groups_from_name (sop/ext_named) :prefix "g_")|} in
  same_cook "groups_from_name" ~typed:regrouped ~factory:Nodes.Groups_from_name.factory
    [ "prefix", Param.Text_value "g_" ] named;
  let promoted = Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/group_promote_boundary
   (sop/ext_grouped)
   :tolerance 1e-06
   :destination "Points"
   :group "half"
   :name "rim")|} in
  same_cook "group_promote_boundary" ~typed:promoted ~factory:Nodes.Group_promote_boundary.factory
    [ "source", Param.Choice_value "Primitives"; "destination", Choice_value "Points";
      "group", Text_value "half"; "name", Text_value "rim";
      (* the typed default differs from the editor default (a listed drift) *)
      "tolerance", Float_value 1e-6 ] grouped;
  let deleted = sop ~inputs:[grouped] "group_delete" [kb "delete_unused" true; ks "rules" ""] in
  same_cook "group_delete" ~typed:deleted ~factory:Nodes.Group_delete.factory
    [ "delete_unused", Param.Bool_value true ] grouped;
  let renamed = sop ~inputs:[grouped] "group_rename" [ks "rules" ""] in
  same_cook "group_rename" ~typed:renamed ~factory:Nodes.Group_rename.factory [] grouped;
  let copied = Lisp_sop.node ~with_:["grouped", (grouped); "box", (box)] {|(sop/group_copy (sop/ext_grouped) (sop/ext_box) :copy_empty true)|} in
  same_cook "group_copy" ~typed:copied ~factory:Nodes.Group_copy.factory
    [ "copy_empty", Param.Bool_value true ] grouped ~inputs:[box];
  let transferred = Lisp_sop.node ~with_:["grouped", (grouped); "box", (box)] {|(sop/group_transfer (sop/ext_grouped) (sop/ext_box) :distance 0.5)|} in
  same_cook "group_transfer" ~typed:transferred ~factory:Nodes.Group_transfer.factory
    [ "distance", Param.Float_value 0.5 ] grouped ~inputs:[box];
  let copy_values = ["rules", Param.Text_value "primitive\thalf\tpicked_\t"] in
  let copied_rules = sop ~inputs:[grouped; box] "group_copy" [kb "use_rules" true; ks "rules" "primitive\thalf\tpicked_\t"] in
  same_cook "group_copy enabled rules" ~typed:copied_rules ~factory:Nodes.Group_copy.factory
    (("use_rules", Param.Bool_value true) :: copy_values) grouped ~inputs:[box];
  same_cook "group_copy disabled rules"
    ~typed:(sop ~inputs:[grouped; box] "group_copy" [ks "rules" "primitive\thalf\tpicked_\t"])
    ~factory:Nodes.Group_copy.factory copy_values grouped ~inputs:[box];
  let transfer_values = ["rules", Param.Text_value "primitive\thalf\tnear_"] in
  let transferred_rules = sop ~inputs:[grouped; box] "group_transfer" [kb "use_rules" true; ks "rules" "primitive\thalf\tnear_"] in
  same_cook "group_transfer enabled rules" ~typed:transferred_rules ~factory:Nodes.Group_transfer.factory
    (("use_rules", Param.Bool_value true) :: transfer_values) grouped ~inputs:[box];
  same_cook "group_transfer disabled rules"
    ~typed:(sop ~inputs:[grouped; box] "group_transfer" [ks "rules" "primitive\thalf\tnear_"])
    ~factory:Nodes.Group_transfer.factory transfer_values grouped ~inputs:[box];
  List.iter (fun (name, node) ->
    cache_identity ~changes:["rules", Param.Text_value ""] name node)
    ["group_copy rules", copied_rules; "group_transfer rules", transferred_rules];
  List.iter (fun distance ->
    check (rejected (fun () -> Lisp_sop.node ~with_:["grouped", (grouped); "box", (box)] (Printf.sprintf {|(sop/group_transfer (sop/ext_grouped) (sop/ext_box) :distance %s)|} ((Lisp_sop.float distance)))))
      "group_transfer refuses invalid distance at construction") [-1.];
  List.iter (fun tolerance ->
    List.iter (fun make ->
      check (rejected make)
        "boundary groups refuse invalid tolerance at construction")
      [ (fun () -> Lisp_sop.node ~with_:["grouped", (grouped)] (Printf.sprintf {|(sop/group_from_attribute_boundary (sop/ext_grouped) :tolerance %s)|} ((Lisp_sop.float tolerance))));
        (fun () -> Lisp_sop.node ~with_:["grouped", (grouped)] (Printf.sprintf {|(sop/group_promote_boundary (sop/ext_grouped) :tolerance %s)|} ((Lisp_sop.float tolerance)))) ])
    [-1.];
  let path_base = sop ~inputs:[warped] "ordered_group" [ks "owner" "Points"; ks "name" "path"; ks "elements" "0 3"] in
  let path = Lisp_sop.node ~with_:["path_base", (path_base)] {|(sop/group_find_path (sop/ext_path_base) :base_group "path" :name "walk")|} in
  same_cook "group_find_path" ~typed:path ~factory:Nodes.Group_find_path.factory
    [ "base_group", Param.Text_value "path"; "name", Text_value "walk" ] path_base;
  let edge_deleted = Lisp_sop.node ~with_:["edges", (edges)] {|(sop/delete_edge_group (sop/ext_edges) :name "rim")|} in
  same_cook "delete_edge_group" ~typed:edge_deleted ~factory:Nodes.Delete_edge_group.factory
    [ "name", Param.Text_value "rim" ] edges;
  let edge_renamed = Lisp_sop.node ~with_:["edges", (edges)] {|(sop/rename_edge_group (sop/ext_edges) :from "rim" :into "border")|} in
  same_cook "rename_edge_group" ~typed:edge_renamed ~factory:Nodes.Rename_edge_group.factory
    [ "from", Param.Text_value "rim"; "into", Text_value "border" ] edges;
  (* topology nodes *)
  let quads = Lisp_sop.node {|(sop/box :normals "Auto" :size [2.0 2.0 2.0])|} in
  let divided = Lisp_sop.node ~with_:["quads", (quads)] {|(sop/edge_divide (sop/ext_quads) :divisions 3)|} in
  same_cook "edge_divide" ~typed:divided ~factory:Nodes.Edge_divide.factory
    [ "divisions", Param.Int_value 3 ] quads;
  let collapsed = Lisp_sop.node ~with_:["edges", (edges)] {|(sop/edge_collapse (sop/ext_edges) :group "rim")|} in
  same_cook "edge_collapse" ~typed:collapsed ~factory:Nodes.Edge_collapse.factory
    [ "group", Param.Text_value "rim" ] edges;
  let dissolved = Lisp_sop.node ~with_:["edges", (edges)] {|(sop/dissolve
   (sop/ext_edges)
   :collinearity_tolerance 0.0
   :remove_inline_points false
   :group "rim")|} in
  same_cook "dissolve" ~typed:dissolved ~factory:Nodes.Dissolve.factory
    [ "group", Param.Text_value "rim";
      (* typed defaults that differ from the editor defaults (listed drifts) *)
      "remove_inline_points", Bool_value false; "collinearity_tolerance", Float_value 0. ] edges;
  let triangulated = Lisp_sop.node ~with_:["quads", (quads)] {|(sop/triangulate (sop/ext_quads))|} in
  same_cook "triangulate" ~typed:triangulated ~factory:Nodes.Triangulate.factory [] quads;
  let flipped = Lisp_sop.node ~with_:["box", (box)] {|(sop/edge_flip (sop/ext_box))|} in
  let reversed = Lisp_sop.node ~with_:["quads", (quads)] {|(sop/reverse (sop/ext_quads))|} in
  same_cook "reverse defaults" ~typed:reversed ~factory:Nodes.Reverse.factory [] quads;
  let shifted = Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/reverse (sop/ext_grouped) :group "half" :operation "Shift vertices" :shift -1)|} in
  same_cook "reverse shift" ~typed:shifted ~factory:Nodes.Reverse.factory
    [ "group", Param.Text_value "half";
      "operation", Choice_value "Shift vertices"; "shift", Int_value (-1) ] grouped;
  cache_identity "reverse" reversed;
  same_cook "edge_flip" ~typed:flipped ~factory:Nodes.Edge_flip.factory [] box;
  let cusped = Lisp_sop.node ~with_:["edges", (edges)] {|(sop/edge_cusp (sop/ext_edges) :group "rim")|} in
  same_cook "edge_cusp" ~typed:cusped ~factory:Nodes.Edge_cusp.factory
    [ "group", Param.Text_value "rim" ] edges;
  let straightened = Lisp_sop.node ~with_:["edges", (edges)] {|(sop/edge_straighten (sop/ext_edges) :group "rim" :output_group "straight")|} in
  same_cook "edge_straighten" ~typed:straightened ~factory:Nodes.Edge_straighten.factory
    [ "group", Param.Text_value "rim"; "output_group", Text_value "straight" ] edges;
  let extruded = Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/poly_extrude (sop/ext_grouped) :group "half" :distance 0.2)|} in
  same_cook "poly_extrude" ~typed:extruded ~factory:Nodes.Poly_extrude.factory
    [ "group", Param.Text_value "half"; "distance", Float_value 0.2 ] grouped;
  let filled = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/poly_fill (sop/ext_warped) :unique_points true)|} in
  same_cook "poly_fill" ~typed:filled ~factory:Nodes.Poly_fill.factory
    [ "unique_points", Param.Bool_value true ] warped;
  let lines = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/convert_line (sop/ext_warped) :connect_path true)|} in
  same_cook "convert_line" ~typed:lines ~factory:Nodes.Convert_line.factory
    [ "connect_path", Param.Bool_value true ] warped;
  let blasted = Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/blast (sop/ext_grouped) :group "half")|} in
  same_cook "blast" ~typed:blasted ~factory:Nodes.Blast.factory
    [ "group", Param.Text_value "half" ] grouped;
  let creased = Lisp_sop.node ~with_:["edges", (edges)] {|(sop/crease (sop/ext_edges) :group "rim" :weight 0.5)|} in
  same_cook "crease" ~typed:creased ~factory:Nodes.Crease.factory
    [ "group", Param.Text_value "rim"; "weight", Float_value 0.5 ] edges;
  let paths = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/poly_path (sop/ext_warped) :maximum_distance 0.25)|} in
  same_cook "poly_path" ~typed:paths ~factory:Nodes.Poly_path.factory
    [ "maximum_distance", Param.Float_value 0.25 ] warped;
  let swapped = sop ~inputs:[warped] "swap_attributes" [ks "rules" ""] in
  same_cook "swap_attributes" ~typed:swapped ~factory:Nodes.Swap_attributes.factory [] warped;
  List.iter (fun (name, node) -> cache_identity name node)
    [ "crease", creased; "poly_path", paths ];
  cache_identity ~changes:["rules", Param.Text_value "point\tweight\tweight_copy\tcopy"]
    "swap_attributes" swapped;
  let ends = Lisp_sop.node ~with_:["lines", (lines)] {|(sop/ends (sop/ext_lines) :mode "Unroll new point")|} in
  same_cook "ends optional mode default" ~typed:(Lisp_sop.node ~with_:["lines", (lines)] {|(sop/ends (sop/ext_lines))|})
    ~factory:Nodes.Ends.factory [] lines;
  same_cook "measure optional kind default" ~typed:(Lisp_sop.node ~with_:["quads", (quads)] {|(sop/measure (sop/ext_quads))|})
    ~factory:Nodes.Measure.factory [] quads;
  same_cook "ends" ~typed:ends ~factory:Nodes.Ends.factory
    [ "mode", Param.Choice_value "Unroll new point" ] lines;
  let measured = Lisp_sop.node ~with_:["quads", (quads)] {|(sop/measure (sop/ext_quads) :attribute "area")|} in
  let material = Lisp_sop.node ~with_:["quads", (quads)] {|(sop/material
   (sop/ext_quads)
   :material "blue"
   :color [0.2 0.4 0.8]
   :roughness 0.3
   :emission [0.1 0.0 0.0])|} in
  same_cook "material" ~typed:material ~factory:Nodes.Material.factory
    [ "material", Param.Text_value "blue";
      "color_r", Float_value 0.2; "color_g", Float_value 0.4; "color_b", Float_value 0.8;
      "roughness", Float_value 0.3; "emission_r", Float_value 0.1 ] quads;
  same_cook "material defaults" ~typed:(Lisp_sop.node ~with_:["quads", (quads)] {|(sop/material (sop/ext_quads))|})
    ~factory:Nodes.Material.factory [] quads;
  cache_identity "material" material;
  List.iter (fun build ->
    check (rejected build)
      "material refuses invalid channels at construction")
    [ (fun () -> Lisp_sop.node ~with_:["quads", (quads)] {|(sop/material (sop/ext_quads) :roughness -0.1)|});
      (fun () -> Lisp_sop.node ~with_:["quads", (quads)] {|(sop/material (sop/ext_quads) :emission [0.0 0.0 1.1])|}) ];
  same_cook "measure" ~typed:measured ~factory:Nodes.Measure.factory
    [ "attribute", Param.Text_value "area" ] quads;
  let uv_quads = Lisp_sop.node ~with_:["quads", (quads)] (Printf.sprintf {|(sop/uv_project (sop/ext_quads) :planar_v %s)|} ((Lisp_sop.vec3 Vec3.unit_y))) in
  let unitized = Lisp_sop.node ~with_:["uv_quads", (uv_quads)] {|(sop/uv_unitize (sop/ext_uv_quads) :uniform false)|} in
  same_cook "uv_unitize optional mode default" ~typed:(Lisp_sop.node ~with_:["uv_quads", (uv_quads)] {|(sop/uv_unitize (sop/ext_uv_quads))|})
    ~factory:Nodes.Uv_unitize.factory [] uv_quads;
  same_cook "uv_unitize" ~typed:unitized ~factory:Nodes.Uv_unitize.factory
    [ "uniform", Param.Bool_value false ] uv_quads;
  List.iter (fun (name, node) -> cache_identity name node)
    [ "ends", ends; "measure", measured; "uv_unitize", unitized ];
  (* Omitted typed values now use the Lisp defaults; legacy callers above
     spell out the old values and still cook the same geometry. *)
  same_cook "dissolve defaults" ~typed:(Lisp_sop.node ~with_:["edges", (edges)] {|(sop/dissolve (sop/ext_edges))|})
    ~factory:Nodes.Dissolve.factory [] edges;
  same_cook "attribute boundary defaults"
    ~typed:(Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_from_attribute_boundary (sop/ext_warped) :name "seams")|})
    ~factory:Nodes.Group_from_attribute_boundary.factory
    [ "name", Param.Text_value "seams" ] warped;
  same_cook "boundary promotion defaults"
    ~typed:(Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/group_promote_boundary (sop/ext_grouped) :destination "Points" :group "half")|})
    ~factory:Nodes.Group_promote_boundary.factory
    [ "source", Param.Choice_value "Primitives";
      "destination", Choice_value "Points"; "group", Text_value "half" ] grouped;
  same_cook "name from groups defaults"
    ~typed:(Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/name_from_groups (sop/ext_grouped))|})
    ~factory:Nodes.Name_from_groups.factory [] grouped;
  same_cook "group copy defaults" ~typed:(Lisp_sop.node ~with_:["grouped", (grouped); "box", (box)] {|(sop/group_copy (sop/ext_grouped) (sop/ext_box))|})
    ~factory:Nodes.Group_copy.factory [] grouped ~inputs:[box];
  same_cook "group transfer defaults" ~typed:(Lisp_sop.node ~with_:["grouped", (grouped); "box", (box)] {|(sop/group_transfer (sop/ext_grouped) (sop/ext_box))|})
    ~factory:Nodes.Group_transfer.factory [] grouped ~inputs:[box];
  List.iter (fun (name, node) -> cache_identity name node)
    [ "edge_divide", divided; "edge_collapse", collapsed; "dissolve", dissolved;
      "triangulate", triangulated; "edge_flip", flipped; "edge_cusp", cusped;
      "edge_straighten", straightened; "poly_extrude", extruded; "poly_fill", filled;
      "convert_line", lines; "blast", blasted ];
  (* attribute and shape nodes *)
  let flat = Lisp_sop.node {|(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 4 :rows 4 :size 2.0)|} in
  let flattened = Lisp_sop.node ~with_:["flat", (flat)] {|(sop/uv_flatten (sop/ext_flat))|} in
  same_cook "uv_flatten" ~typed:flattened ~factory:Nodes.Uv_flatten.factory [] flat;
  let relaxed = Lisp_sop.node ~with_:["flattened", (flattened)] {|(sop/uv_relax (sop/ext_flattened) :iterations 10)|} in
  same_cook "uv_relax" ~typed:relaxed ~factory:Nodes.Uv_relax.factory
    [ "iterations", Param.Int_value 10 ] flattened;
  let renamed_attributes = sop ~inputs:[warped] "rename_attributes" [ks "rules" ""] in
  same_cook "rename_attributes" ~typed:renamed_attributes ~factory:Nodes.Rename_attributes.factory
    [] warped;
  let line = Lisp_sop.node (Printf.sprintf {|(sop/line :points 5 :origin %s :direction %s :length 2.0)|} ((Lisp_sop.vec3 Vec3.zero)) ((Lisp_sop.vec3 Vec3.unit_y))) in
  same_generator "line" ~typed:line ~factory:Nodes.Line.factory
    [ "points", Param.Int_value 5; "length", Float_value 2. ];
  let mirrored = Lisp_sop.node ~with_:["box", (box)] (Printf.sprintf {|(sop/mirror (sop/ext_box) :keep_original false :origin %s :normal %s)|} ((Lisp_sop.vec3 Vec3.zero)) ((Lisp_sop.vec3 Vec3.unit_x))) in
  same_cook "mirror" ~typed:mirrored ~factory:Nodes.Mirror.factory
    [ "keep_original", Param.Bool_value false ] box;
  let matched = Lisp_sop.node ~with_:["box", (box)] (Printf.sprintf {|(sop/match_axis (sop/ext_box) :from %s :into %s)|} ((Lisp_sop.vec3 Vec3.unit_y)) ((Lisp_sop.vec3 Vec3.unit_x))) in
  same_cook "match_axis" ~typed:matched ~factory:Nodes.Match_axis.factory
    [ "into_x", Param.Float_value 1.; "into_y", Float_value 0. ] box;
  let displaced = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/noise_displace (sop/ext_warped) :seed 3 :amplitude 0.2 :frequency 2.0)|} in
  same_cook "noise_displace" ~typed:displaced ~factory:Nodes.Noise_displace.factory
    [ "seed", Param.Int_value 3; "amplitude", Float_value 0.2; "frequency", Float_value 2. ] warped;
  List.iter (fun (name, node) -> cache_identity name node)
    [ "uv_flatten", flattened; "uv_relax", relaxed;
      "line", line; "mirror", mirrored; "match_axis", matched; "noise_displace", displaced ];
  cache_identity ~changes:["rules", Param.Text_value "any\tweight\trenamed_weight\tskip"]
    "rename_attributes" renamed_attributes;
  List.iter (fun (name, node) -> cache_identity name node)
    [ "group_non_planar", non_planar; "group_backface", backface; "group_unshared", unshared;
      "group_edges", edges; "group_random", random; "group_edge_depth", depth;
      "group_boundary_components", components;
      "name_from_groups", named; "groups_from_name", regrouped;
      "group_find_path", path;
      "delete_edge_group", edge_deleted; "rename_edge_group", edge_renamed ];
  List.iter (fun (name, node, rules) ->
    cache_identity ~changes:["rules", Param.Text_value rules] name node)
    [ "group_delete", deleted, "any\t*";
      "group_rename", renamed, "any\thalf\trenamed_half\tskip";
      "group_copy", copied, "point\t*\tcopy_\t";
      "group_transfer", transferred, "point\t*\ttransfer_" ];
  cache_identity ~changes:["attributes", Param.Text_value "point\tP"]
    "group_from_attribute_boundary" boundary;
  cache_identity ~changes:["attributes", Param.Text_value "point\tP"]
    "group_promote_boundary" promoted;
  (* the typed constructor keeps its validation *)
  let primitive_group = Lisp_sop.node ~with_:["box", (box)] {|(sop/group_range (sop/ext_box) :owner "Primitives" :name "group" :end_ 0)|} in
  let edge_group = Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_edges (sop/ext_warped))|} in
  let ordered = sop ~inputs:[warped] "ordered_group" [ks "owner" "Points"; ks "name" "ordered"; ks "elements" "0 1 2"] in
  List.iter (fun (name, typed, factory, input) ->
    same_cook (name ^ " optional field defaults") ~typed ~factory [] input)
    [ "group_non_planar", Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_non_planar (sop/ext_warped))|}, Nodes.Group_non_planar.factory, warped;
      "group_backface", Lisp_sop.node ~with_:["box", (box)] {|(sop/group_backface (sop/ext_box))|}, Nodes.Group_backface.factory, box;
      "group_unshared", Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_unshared (sop/ext_warped))|}, Nodes.Group_unshared.factory, warped;
      (* Seed absence remains the separate Auto migration; pin the Lisp seed here. *)
      "group_random", Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_random (sop/ext_warped))|}, Nodes.Group_random.factory, warped;
      "group_edge_depth", Lisp_sop.node ~with_:["seeded", (seeded)] {|(sop/group_edge_depth (sop/ext_seeded))|}, Nodes.Group_edge_depth.factory, seeded;
      "group_from_attribute_boundary", Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_from_attribute_boundary (sop/ext_warped))|},
        Nodes.Group_from_attribute_boundary.factory, warped;
      "groups_from_name", Lisp_sop.node ~with_:["named", (named)] {|(sop/groups_from_name (sop/ext_named))|}, Nodes.Groups_from_name.factory, named;
      "name_from_groups", Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/name_from_groups (sop/ext_grouped))|}, Nodes.Name_from_groups.factory, grouped;
      "group_promote_boundary", Lisp_sop.node ~with_:["primitive_group", (primitive_group)] {|(sop/group_promote_boundary (sop/ext_primitive_group))|},
        Nodes.Group_promote_boundary.factory, primitive_group;
      "group_delete", Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/group_delete (sop/ext_grouped))|}, Nodes.Group_delete.factory, grouped;
      "group_rename", Lisp_sop.node ~with_:["grouped", (grouped)] {|(sop/group_rename (sop/ext_grouped))|}, Nodes.Group_rename.factory, grouped;
      "group_find_path", Lisp_sop.node ~with_:["ordered", (ordered)] {|(sop/group_find_path (sop/ext_ordered))|}, Nodes.Group_find_path.factory, ordered;
      "delete_edge_group", Lisp_sop.node ~with_:["edge_group", (edge_group)] {|(sop/delete_edge_group (sop/ext_edge_group))|}, Nodes.Delete_edge_group.factory, edge_group;
      "rename_edge_group", Lisp_sop.node ~with_:["edge_group", (edge_group)] {|(sop/rename_edge_group (sop/ext_edge_group))|}, Nodes.Rename_edge_group.factory, edge_group;
      "poly_extrude", Lisp_sop.node ~with_:["quads", (quads)] {|(sop/poly_extrude (sop/ext_quads))|}, Nodes.Poly_extrude.factory, quads;
      "blast", Lisp_sop.node ~with_:["primitive_group", (primitive_group)] {|(sop/blast (sop/ext_primitive_group))|}, Nodes.Blast.factory, primitive_group;
      "rename_attributes", Lisp_sop.node ~with_:["warped", (warped)] {|(sop/rename_attributes (sop/ext_warped))|}, Nodes.Rename_attributes.factory, warped;
      "swap_attributes", Lisp_sop.node ~with_:["warped", (warped)] {|(sop/swap_attributes (sop/ext_warped))|}, Nodes.Swap_attributes.factory, warped;
      "mirror", Lisp_sop.node ~with_:["box", (box)] {|(sop/mirror (sop/ext_box))|}, Nodes.Mirror.factory, box;
      "match_axis", Lisp_sop.node ~with_:["box", (box)] {|(sop/match_axis (sop/ext_box))|}, Nodes.Match_axis.factory, box;
      "noise_displace", Lisp_sop.node ~with_:["warped", (warped)] {|(sop/noise_displace (sop/ext_warped))|}, Nodes.Noise_displace.factory, warped;
      "separate_pieces", Lisp_sop.node ~with_:["piece_source", (piece_source)] {|(sop/separate_pieces (sop/ext_piece_source))|}, Nodes.Separate_pieces.factory, piece_source ];
  same_generator "line optional field defaults" ~typed:(Lisp_sop.node {|(sop/line)|}) ~factory:Nodes.Line.factory [];
  List.iter (fun (name, build) ->
    check (rejected build)
      (name ^ " still raises Invalid_argument"))
    [ "group_non_planar empty name", (fun () -> Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_non_planar (sop/ext_warped) :tolerance 0.01 :name " ")|});
      "group_non_planar negative tolerance", (fun () -> Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_non_planar (sop/ext_warped) :tolerance -1.0 :name "a")|});
      "group_unshared empty name", (fun () -> Lisp_sop.node ~with_:["box", (box)] {|(sop/group_unshared (sop/ext_box) :name "")|}) ];
  List.iter (fun build ->
    check (rejected build)
      "group_promotions refuses non-positive Lisp limits")
    [ (fun () -> Lisp_sop.node ~with_:["seeded", (seeded)] {|(sop/group_promotions (sop/ext_seeded) :max_outputs 0)|});
      (fun () -> Lisp_sop.node ~with_:["seeded", (seeded)] {|(sop/group_promotions (sop/ext_seeded) :max_payload_bytes 0)|}) ];
  let interpolation_count owner geometry = match owner with
    | Rdk.Attribute.Point -> Rdk.Geometry.point_count geometry
    | Rdk.Attribute.Vertex -> Rdk.Geometry.vertex_count geometry
    | Rdk.Attribute.Primitive -> Rdk.Geometry.primitive_count geometry
    | Rdk.Attribute.Detail -> 1 in
  let interpolation_owners = [Rdk.Attribute.Point,"point"; Vertex,"vertex";
    Primitive,"primitive"; Detail,"detail"] in
  let interpolation_add ~owner ~name storage geometry =
    Rdk.Attribute.create_owned ~owner ~name storage |> Result.get_ok
    |> fun attribute -> Rdk.Geometry.with_attribute attribute geometry |> Result.get_ok in
  let interpolation_source_geometry = List.fold_left (fun geometry (owner,_) ->
    let count = interpolation_count owner geometry in
    geometry |> interpolation_add ~owner ~name:"weight"
      (Rdk.Attribute.Float (Array.init count (fun i -> float (i+1)))))
    (Rdk.Plane_generators.grid ~columns:2 ~rows:2 ~size:2. () |> Result.get_ok)
    interpolation_owners in
  let interpolation_source = Lisp_sop.snapshot (interpolation_source_geometry) in
  let interpolation_rules = "point\tweight\tpoint_weight\nvertex\tweight\tvertex_weight\nprimitive\tweight\tprimitive_weight\ndetail\tweight\tdetail_weight" in
  let interpolation_target_geometry = List.fold_left (fun geometry (owner,_) ->
    let count = interpolation_count owner geometry in
    let offsets = Array.init (count+1) (fun i -> i*2) in
    let numbers = Rdk.Packed.Int_array.create_owned ~offsets
      ~values:(Array.init (count*2) (fun i -> i land 1)) |> Result.get_ok in
    let weights = Rdk.Packed.Float_array.create_owned ~offsets:(Array.copy offsets)
      ~values:(Array.init (count*2) (fun i -> if i land 1 = 0 then -.0.25 else 0.75)) |> Result.get_ok in
    let uvw = Rdk.Packed.Float3.of_owned ~x:(Array.make count 0.25)
      ~y:(Array.make count 0.5) ~z:(Array.make count 0.) |> Result.get_ok in
    geometry |> interpolation_add ~owner ~name:"sourceprim" (Rdk.Attribute.Int (Array.make count 0))
      |> interpolation_add ~owner ~name:"sourceuvw" (Rdk.Attribute.Float3 uvw)
      |> interpolation_add ~owner ~name:"sourcenums" (Rdk.Attribute.Int_array numbers)
      |> interpolation_add ~owner ~name:"sourceweights" (Rdk.Attribute.Float_array weights))
    (Rdk.Plane_generators.grid ~columns:2 ~rows:2 ~size:2. () |> Result.get_ok)
    interpolation_owners in
  let interpolation_target = Lisp_sop.snapshot (interpolation_target_geometry) in
  same_node "Attribute Interpolate Lisp defaults"
    ~typed:(Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] (Printf.sprintf {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :attributes %S)|} (interpolation_rules)))
    ~catalog:(from_factory Nodes.Attribute_interpolate.factory
      ["attributes",Text_value interpolation_rules] [interpolation_source;interpolation_target]);
  let interpolation_drivers = [
    "Primitive UVW",Rdk.Attribute_ops.Primitive_uvw {
      primitive_attribute="sourceprim";uvw_attribute="sourceuvw"};
    "Point weights",Rdk.Attribute_ops.Point_weights {
      numbers_attribute="sourcenums";weights_attribute="sourceweights"};
    "Vertex weights",Rdk.Attribute_ops.Vertex_weights {
      numbers_attribute="sourcenums";weights_attribute="sourceweights"};
    "Primitive weights",Rdk.Attribute_ops.Primitive_weights {
      numbers_attribute="sourcenums";weights_attribute="sourceweights"}] in
  let interpolation_driver_owners driver_name =
    let supported owner = match driver_name with
      | "Primitive UVW" | "Vertex weights" -> true
      | "Point weights" -> owner = Rdk.Attribute.Point || owner = Rdk.Attribute.Detail
      | _ -> owner = Rdk.Attribute.Primitive || owner = Rdk.Attribute.Detail in
    List.filter (fun (owner,_) -> supported owner) interpolation_owners in
  let interpolation_driver_rules driver_name =
    String.concat "\n" (List.map (fun (_,token) -> token ^ "\tweight\t" ^ token ^ "_weight")
      (interpolation_driver_owners driver_name)) in
  let interpolation_driver_attributes driver_name =
    List.map (fun (owner,token) -> {Rdk.Attribute_ops.interpolate_owner=owner;
        interpolate_source="weight";interpolate_target=token ^ "_weight"})
      (interpolation_driver_owners driver_name) in
  let interpolation_native result = match result with
    | Ok geometry -> geometry | Error error -> fail (Rdk.Error.to_string error) in
  List.iter (fun (target_owner,owner_name) ->
    List.iter (fun (driver_name,native_driver) ->
      let interpolation_rules = interpolation_driver_rules driver_name in
      List.iter (fun pre_scale -> List.iter (fun normalize_weights ->
        List.iter (fun threshold -> List.iter (fun blend ->
          List.iter (fun unmatched ->
            let unmatched_name = if unmatched = Rdk.Attribute_ops.Keep_target then "Keep target" else "Default value" in
            let typed = sop ~inputs:[interpolation_source; interpolation_target] "attribute_interpolate"
              [ks "target_owner" (String.capitalize_ascii owner_name); ks "driver" driver_name;
               ks "attributes" interpolation_rules; kf "pre_scale" pre_scale; kb "normalize_weights" normalize_weights;
               kf "threshold" threshold; kf "blend" blend; ks "unmatched" unmatched_name] in
            let values = ["target_owner",Param.Choice_value (String.capitalize_ascii owner_name);
              "driver",Choice_value driver_name;"attributes",Text_value interpolation_rules;
              "pre_scale",Float_value pre_scale;"normalize_weights",Bool_value normalize_weights;
              "threshold",Float_value threshold;"blend",Float_value blend;
              "unmatched",Choice_value (if unmatched = Rdk.Attribute_ops.Keep_target then "Keep target" else "Default value")] in
            let native = Rdk.Attribute_ops.interpolate ~target_owner ~driver:native_driver
              ~pre_scale ~normalize_weights ~threshold ~blend ~unmatched
              ~attributes:(interpolation_driver_attributes driver_name) ~source:interpolation_source_geometry
              ~target:interpolation_target_geometry () |> interpolation_native in
            check (equal_geometry native (cook 1 typed)) "Attribute Interpolate native numeric parity";
            same_node "Attribute Interpolate numeric modes" ~typed
              ~catalog:(from_factory Nodes.Attribute_interpolate.factory values
                [interpolation_source;interpolation_target]))
            [Rdk.Attribute_ops.Keep_target;Default_value]) [0.25;1.])
          [1e-6;0.5]) [false;true]) [-1.;0.;1.5]) interpolation_drivers)
    interpolation_owners;
  List.iter (fun (target_owner,owner_name) -> List.iter (fun computed_owner ->
    let typed = sop ~inputs:[interpolation_source; interpolation_target] "attribute_interpolate"
      [ks "target_owner" (String.capitalize_ascii owner_name); ks "attributes" interpolation_rules;
       kb "compute_weights" true; ks "computed_owner" (if computed_owner = Rdk.Attribute.Point then "Point" else "Vertex")] in
    let computed = {Rdk.Attribute_ops.computed_owner;
      computed_numbers_attribute="computednums";computed_weights_attribute="computedweights"} in
    let native = Rdk.Attribute_ops.interpolate ~target_owner ~attributes:(interpolation_driver_attributes "Primitive UVW")
      ~driver:(Rdk.Attribute_ops.Primitive_uvw {primitive_attribute="sourceprim";uvw_attribute="sourceuvw"})
      ~compute_weights:computed ~source:interpolation_source_geometry ~target:interpolation_target_geometry ()
      |> interpolation_native in
    check (equal_geometry native (cook 1 typed)) "Attribute Interpolate computed-array native parity";
    same_node "Attribute Interpolate computed arrays" ~typed
      ~catalog:(from_factory Nodes.Attribute_interpolate.factory [
        "target_owner",Choice_value (String.capitalize_ascii owner_name);
        "attributes",Text_value interpolation_rules;"compute_weights",Bool_value true;
        "computed_owner",Choice_value (if computed_owner=Rdk.Attribute.Point then "Point" else "Vertex")]
        [interpolation_source;interpolation_target])) [Rdk.Attribute.Point;Vertex]) interpolation_owners;
  List.iter (fun (target_owner,token) ->
    let owner = match target_owner with
      | Rdk.Attribute.Point -> Rdk.Group.Point
      | Vertex -> Rdk.Group.Vertex | Primitive -> Rdk.Group.Primitive
      | Detail -> assert false in
    let count = interpolation_count target_owner interpolation_target_geometry in
    let selected = Rdk.Group.init ~owner ~name:"selected" count (fun i -> i mod 2 = 0) in
    let target_geometry = Rdk.Geometry.with_group selected interpolation_target_geometry |> Result.get_ok in
    let target = Lisp_sop.snapshot (target_geometry) in
    List.iter (fun (driver_name,native_driver) ->
      let interpolation_rules = interpolation_driver_rules driver_name in
      List.iter (fun (group,group_pattern) ->
        let typed = sop ~inputs:[interpolation_source; target] "attribute_interpolate"
          [ks "target_owner" (String.capitalize_ascii token); ks "driver" driver_name; kf "threshold" 0.1;
           ks "attributes" interpolation_rules; ks "group" group; ks "group_pattern" group_pattern] in
        let native = Rdk.Attribute_ops.interpolate ~target_owner ~driver:native_driver
          ~selection:selected ~threshold:0.1 ~normalize_weights:true
          ~attributes:(interpolation_driver_attributes driver_name) ~source:interpolation_source_geometry
          ~target:target_geometry () |> interpolation_native in
        check (equal_geometry native (cook 1 typed)) "Attribute Interpolate selected native parity";
        same_node "Attribute Interpolate exact and patterned selections" ~typed
          ~catalog:(from_factory Nodes.Attribute_interpolate.factory [
            "target_owner",Choice_value (String.capitalize_ascii token);
            "driver",Choice_value driver_name;"threshold",Float_value 0.1;
            "attributes",Text_value interpolation_rules;
            "group",Text_value group;"group_pattern",Text_value group_pattern]
            [interpolation_source;target]))
        ["selected","";"","select*";"missing_ignored","select*"])
      interpolation_drivers) [Rdk.Attribute.Point,"point";Vertex,"vertex";Primitive,"primitive"];
  List.iter (fun keyword ->
    let pattern key = if keyword=key then Some "weight" else None in
    let point_pattern=pattern "point_pattern" and vertex_pattern=pattern "vertex_pattern"
    and primitive_pattern=pattern "primitive_pattern" and detail_pattern=pattern "detail_pattern" in
    let native = Rdk.Attribute_ops.interpolate ~target_owner:Rdk.Attribute.Point
      ~driver:(Rdk.Attribute_ops.Primitive_uvw {primitive_attribute="sourceprim";uvw_attribute="sourceuvw"})
      ?point_pattern ?vertex_pattern ?primitive_pattern ?detail_pattern
      ~attributes:[] ~source:interpolation_source_geometry ~target:interpolation_target_geometry ()
      |> function Ok geometry -> geometry | Error error -> fail (Rdk.Error.to_string error) in
    let typed = sop ~inputs:[interpolation_source; interpolation_target] "attribute_interpolate"
      (ks "attributes" "" :: List.filter_map (fun (key, value) -> Option.map (ks key) value)
        ["point_pattern", point_pattern; "vertex_pattern", vertex_pattern;
         "primitive_pattern", primitive_pattern; "detail_pattern", detail_pattern]) in
    check (equal_geometry native (cook 1 typed)) "Attribute Interpolate pattern expansion native parity";
    same_node "Attribute Interpolate owner pattern fields" ~typed
      ~catalog:(from_factory Nodes.Attribute_interpolate.factory ["attributes",Text_value "";
        keyword,Text_value "weight"] [interpolation_source;interpolation_target]))
    ["point_pattern";"vertex_pattern";"primitive_pattern";"detail_pattern"];
  List.iter (fun (driver_name,native_driver) ->
    let owner,group_owner,keyword = match driver_name with
      | "Primitive UVW" | "Point weights" -> Rdk.Attribute.Point,Rdk.Group.Point,"point_pattern"
      | "Vertex weights" -> Rdk.Attribute.Vertex,Rdk.Group.Vertex,"vertex_pattern"
      | _ -> Rdk.Attribute.Primitive,Rdk.Group.Primitive,"primitive_pattern" in
    let hot = Rdk.Group.init ~owner:group_owner ~name:"hot"
      (interpolation_count owner interpolation_source_geometry) (fun i -> i=0) in
    let source_geometry = Rdk.Geometry.with_group hot interpolation_source_geometry |> Result.get_ok in
    let source = Lisp_sop.snapshot (source_geometry) in
    let pattern key = if keyword=key then Some "weight hot" else None in
    let point_pattern=pattern "point_pattern" and vertex_pattern=pattern "vertex_pattern"
    and primitive_pattern=pattern "primitive_pattern" in
    let native = Rdk.Attribute_ops.interpolate ~target_owner:Rdk.Attribute.Point ~driver:native_driver
      ~threshold:0.1 ~normalize_weights:true ~match_groups:true
      ?point_pattern ?vertex_pattern ?primitive_pattern ~attributes:[]
      ~source:source_geometry ~target:interpolation_target_geometry ()
      |> function Ok geometry -> geometry | Error error -> fail (Rdk.Error.to_string error) in
    let typed = sop ~inputs:[source; interpolation_target] "attribute_interpolate"
      ([ks "driver" driver_name; kf "threshold" 0.1; kb "match_groups" true; ks "attributes" ""]
       @ List.filter_map (fun (key, value) -> Option.map (ks key) value)
         ["point_pattern", point_pattern; "vertex_pattern", vertex_pattern; "primitive_pattern", primitive_pattern]) in
    check (equal_geometry native (cook 1 typed)) "Attribute Interpolate group-match native parity";
    same_node "Attribute Interpolate group matching" ~typed
      ~catalog:(from_factory Nodes.Attribute_interpolate.factory ["driver",Choice_value driver_name;
        "threshold",Float_value 0.1;"match_groups",Bool_value true;"attributes",Text_value "";
        keyword,Text_value "weight hot"] [source;interpolation_target])) interpolation_drivers;
  let interpolation_escaped_geometry = interpolation_add ~owner:Rdk.Attribute.Point ~name:"weight\tvalue"
    (Rdk.Attribute.Float (Array.make (Rdk.Geometry.point_count interpolation_source_geometry) 7.))
    interpolation_source_geometry in
  let interpolation_escaped_source = Lisp_sop.snapshot (interpolation_escaped_geometry) in
  let interpolation_escaped_rules = "point\tweight\\tvalue\tout" in
  let interpolation_escaped = Lisp_sop.node ~with_:["in1249", (interpolation_escaped_source); "interpolation_target", (interpolation_target)] (Printf.sprintf {|(sop/attribute_interpolate
   (sop/ext_in1249)
   (sop/ext_interpolation_target)
   :attributes %S)|} (interpolation_escaped_rules)) in
  let interpolation_escaped_native = Rdk.Attribute_ops.interpolate ~target_owner:Rdk.Attribute.Point
    ~driver:(Rdk.Attribute_ops.Primitive_uvw {primitive_attribute="sourceprim";uvw_attribute="sourceuvw"})
    ~attributes:[{Rdk.Attribute_ops.interpolate_owner=Rdk.Attribute.Point;
      interpolate_source="weight\tvalue";interpolate_target="out"}]
    ~source:interpolation_escaped_geometry ~target:interpolation_target_geometry ()
    |> interpolation_native in
  check (equal_geometry interpolation_escaped_native (cook 1 interpolation_escaped))
    "Attribute Interpolate escaped rule table native parity";
  same_node "Attribute Interpolate escaped rule tables" ~typed:interpolation_escaped
    ~catalog:(from_factory Nodes.Attribute_interpolate.factory ["attributes",Text_value interpolation_escaped_rules]
      [interpolation_escaped_source;interpolation_target]);
  let interpolation_bad_inspector = Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] (Printf.sprintf {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :attributes %S)|} (interpolation_rules)) in
  List.iter (fun values -> check (Result.is_error (Node.apply_parameters interpolation_bad_inspector values))
    "Attribute Interpolate inspector returns validation errors") [
      ["attributes",Text_value "point\tweight"];
      ["pre_scale",Float_value nan];
      ["driver",Choice_value "Point weights"];
      ["compute_weights",Bool_value true;"computed_numbers_attribute",Text_value "P"]];
  cache_identity "Attribute Interpolate all fields"
    ~changes:["attributes",Text_value "point\tweight\tother"]
    ~companions:["driver",["threshold",Float_value 1e-6;"attributes",Text_value "point\tweight\tpoint_weight"]]
    (Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] (Printf.sprintf {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :attributes %S)|} (interpolation_rules)));
  List.iter (fun build -> check (rejected build)
    "Attribute Interpolate construction refusal") [
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :attributes "point	weight")|});
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :attributes "bad	weight	out")|});
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :attributes "point		out")|});
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :primitive_attribute " ")|});
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :driver "Point weights")|});
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :driver "Vertex weights"
   :threshold 0.1
   :compute_weights true)|});
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :target_owner "Detail"
   :group "g")|});
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :blend 1.1)|});
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :threshold -1.0)|});
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :compute_weights true
   :computed_numbers_attribute "computedweights")|});
    (fun () -> sop ~inputs:[interpolation_source; interpolation_target] "attribute_interpolate" [kb "compute_weights" true; ks "computed_owner" "Primitive"]);
    (fun () -> Lisp_sop.node ~with_:["interpolation_source", (interpolation_source); "interpolation_target", (interpolation_target)] {|(sop/attribute_interpolate
   (sop/ext_interpolation_source)
   (sop/ext_interpolation_target)
   :compute_weights true
   :computed_weights_attribute "P")|})];
  let transfer_source_geometry=interpolation_source_geometry in
  let transfer_target_geometry=Rdk.Plane_generators.grid ~columns:3 ~rows:2 ~size:3. () |> Result.get_ok in
  let transfer_source=Lisp_sop.snapshot (transfer_source_geometry)
  and transfer_target=Lisp_sop.snapshot (transfer_target_geometry) in
  let native_transfer ?names ?pattern ?source_group ?target_group ?source_vertices
      ~owner ~mode ?max_distance ~blend_width ~falloff ~unmatched ~source ~target () =
    (match owner with
    | Rdk.Attribute.Point -> Rdk.Attribute_ops.transfer_points ?names ?pattern ~mode
        ?max_distance ~blend_width ~falloff ~unmatched ?source_points:source_group
        ?target_points:target_group ~source ~target ()
    | Primitive -> Rdk.Attribute_ops.transfer_primitives ?names ?pattern ~mode
        ?max_distance ~blend_width ~falloff ~unmatched ?source_primitives:source_group
        ?target_primitives:target_group ~source ~target ()
    | Vertex -> Rdk.Attribute_ops.transfer_vertices ?names ?pattern ?max_distance
        ~blend_width ~falloff ~unmatched ?source_primitives:source_group ?source_vertices
        ?target_vertices:target_group ~source ~target ()
    | Detail -> Rdk.Attribute_ops.transfer_detail ?names ?pattern ~source ~target ())
    |> function Ok geometry -> geometry | Error error -> fail (Rdk.Error.to_string error) in
  same_node "Attribute Transfer Lisp defaults" ~typed:(Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer (sop/ext_transfer_source) (sop/ext_transfer_target))|})
    ~catalog:(from_factory Nodes.Attribute_transfer.factory [] [transfer_source;transfer_target]);
  let transfer_modes=["Nearest",Rdk.Attribute_ops.Nearest;
    "Inverse distance",Rdk.Attribute_ops.Inverse_distance {neighbors=3;power=1.5};
    "Links kernel",Rdk.Attribute_ops.Kernel {neighbors=3;radius=2.;kernel=Links};
    "RenderMan kernel",Rdk.Attribute_ops.Kernel {neighbors=3;radius=2.;kernel=RenderMan};
    "Hart kernel",Rdk.Attribute_ops.Kernel {neighbors=3;radius=2.;kernel=Hart}] in
  let transfer_falloffs=["Linear",Rdk.Attribute_ops.Linear;
    "Smoothstep",Rdk.Attribute_ops.Smoothstep;
    "Uniform",Rdk.Attribute_ops.Uniform 0.3] in
  List.iter (fun (owner,owner_name) ->
    List.iter (fun (mode_name,native_mode) ->
      if owner<>Rdk.Attribute.Vertex && owner<>Rdk.Attribute.Detail || mode_name="Nearest" then
      List.iter (fun (falloff_name,native_falloff) ->
        List.iter (fun max_distance -> List.iter (fun blend_width ->
          if owner<>Rdk.Attribute.Detail && (max_distance<>None || blend_width=0.)
              || owner=Rdk.Attribute.Detail && max_distance=None && blend_width=0. then
          List.iter (fun unmatched -> List.iter (fun use_names ->
            let maximum=Option.value ~default:1. max_distance in
            let typed=sop ~inputs:[transfer_source; transfer_target] "attribute_transfer"
              [ks "owner" (String.capitalize_ascii owner_name); ks "mode" mode_name; ki "neighbors" 3; kf "power" 1.5;
               kf "kernel_radius" 2.; ks "distance_mode" (if max_distance=None then "Auto" else "Explicit");
               kf "max_distance" maximum; kf "blend_width" blend_width; ks "falloff" falloff_name;
               kf "uniform_bias" 0.3;
               ks "unmatched" (if unmatched=Rdk.Attribute_ops.Keep_target then "Keep target" else "Default value");
               kb "use_names" use_names; ks "names" "weight"; ks "pattern" "w*"] in
            let native=native_transfer ~owner ~mode:native_mode ?max_distance ~blend_width
              ~falloff:native_falloff ~unmatched
              ?names:(if use_names then Some ["weight"] else None)
              ?pattern:(if use_names then None else Some "w*")
              ~source:transfer_source_geometry ~target:transfer_target_geometry () in
            check (equal_geometry native (cook 1 typed)) "Attribute Transfer native parity";
            same_node "Attribute Transfer owners and sampling controls" ~typed
              ~catalog:(from_factory Nodes.Attribute_transfer.factory [
                "owner",Choice_value (String.capitalize_ascii owner_name);
                "mode",Choice_value mode_name;"neighbors",Int_value 3;"power",Float_value 1.5;
                "kernel_radius",Float_value 2.;"distance_mode",Choice_value (if max_distance=None then "Auto" else "Explicit");
                "max_distance",Float_value maximum;"blend_width",Float_value blend_width;
                "falloff",Choice_value falloff_name;"uniform_bias",Float_value 0.3;
                "unmatched",Choice_value (if unmatched=Rdk.Attribute_ops.Keep_target then "Keep target" else "Default value");
                "use_names",Bool_value use_names;"names",Text_value "weight";"pattern",Text_value "w*"]
                [transfer_source;transfer_target])) [false;true]) [Rdk.Attribute_ops.Keep_target;Default_value])
          [0.;0.75]) [None;Some 0.;Some 0.5;Some 2.]) transfer_falloffs)
      transfer_modes) interpolation_owners;
  List.iter (fun (owner,token) ->
    let source_owner,target_owner=match owner with
      | Rdk.Attribute.Point -> Rdk.Group.Point,Rdk.Group.Point
      | Primitive -> Rdk.Group.Primitive,Rdk.Group.Primitive
      | Vertex -> Rdk.Group.Primitive,Rdk.Group.Vertex | Detail -> assert false in
    let group_count group_owner geometry=match group_owner with
      | Rdk.Group.Point -> Rdk.Geometry.point_count geometry
      | Vertex -> Rdk.Geometry.vertex_count geometry
      | Primitive -> Rdk.Geometry.primitive_count geometry in
    let source_group=Rdk.Group.init ~owner:source_owner ~name:"selected"
      (group_count source_owner transfer_source_geometry) (fun i -> i mod 2=0)
    and target_group=Rdk.Group.init ~owner:target_owner ~name:"selected"
      (group_count target_owner transfer_target_geometry) (fun i -> i mod 2=0) in
    let source_geometry=Rdk.Geometry.with_group source_group transfer_source_geometry |> Result.get_ok
    and target_geometry=Rdk.Geometry.with_group target_group transfer_target_geometry |> Result.get_ok in
    let source=Lisp_sop.snapshot (source_geometry) and target=Lisp_sop.snapshot (target_geometry) in
    List.iter (fun (group,pattern) ->
      let typed=sop ~inputs:[source; target] "attribute_transfer"
        [ks "owner" (String.capitalize_ascii token); ks "pattern" "weight"; ks "distance_mode" "Auto";
         ks "source_group" group; ks "source_group_pattern" pattern; ks "target_group" group;
         ks "target_group_pattern" pattern] in
      let native=native_transfer ~owner ~mode:Rdk.Attribute_ops.Nearest ~pattern:"weight"
        ~source_group ~target_group ~blend_width:0. ~falloff:Rdk.Attribute_ops.Linear
        ~unmatched:Rdk.Attribute_ops.Keep_target ~source:source_geometry ~target:target_geometry () in
      check (equal_geometry native (cook 1 typed)) "Attribute Transfer selected native parity";
      same_node "Attribute Transfer exact/pattern groups" ~typed
        ~catalog:(from_factory Nodes.Attribute_transfer.factory ["owner",Choice_value (String.capitalize_ascii token);
          "pattern",Text_value "weight";"distance_mode",Choice_value "Auto";
          "source_group",Text_value group;"source_group_pattern",Text_value pattern;
          "target_group",Text_value group;"target_group_pattern",Text_value pattern] [source;target]))
      ["selected","";"","select*";"ignored_missing","select*"])
    [Rdk.Attribute.Point,"point";Vertex,"vertex";Primitive,"primitive"];
  let transfer_corners=Rdk.Group.init ~owner:Rdk.Group.Vertex ~name:"corners"
    (Rdk.Geometry.vertex_count transfer_source_geometry) (fun i -> i mod 4=0) in
  let transfer_corner_geometry=Rdk.Geometry.with_group transfer_corners transfer_source_geometry |> Result.get_ok in
  let transfer_corner_source=Lisp_sop.snapshot (transfer_corner_geometry) in
  List.iter (fun (source_vertex_selection,selection_name) ->
    let typed=sop ~inputs:[transfer_corner_source; transfer_target] "attribute_transfer"
      [ks "owner" "Vertex"; ks "pattern" "weight"; ks "distance_mode" "Auto";
       ks "source_vertex_group_pattern" "corner*"; ks "source_vertex_selection" selection_name] in
    let native=Rdk.Attribute_ops.transfer_vertices ~pattern:"weight" ~source_vertices:transfer_corners
      ~source_vertex_selection ~falloff:Rdk.Attribute_ops.Linear
      ~source:transfer_corner_geometry ~target:transfer_target_geometry ()
      |> function Ok geometry -> geometry | Error error -> fail (Rdk.Error.to_string error) in
    check (equal_geometry native (cook 1 typed)) "Attribute Transfer all/any vertex selection native parity";
    same_node "Attribute Transfer source-corner selections" ~typed
      ~catalog:(from_factory Nodes.Attribute_transfer.factory ["owner",Choice_value "Vertex";
        "pattern",Text_value "weight";"distance_mode",Choice_value "Auto";
        "source_vertex_group_pattern",Text_value "corner*";
        "source_vertex_selection",Choice_value selection_name] [transfer_corner_source;transfer_target]))
    [Rdk.Attribute_ops.All_triangle_vertices,"All triangle vertices";
     Any_triangle_vertex,"Any triangle vertex"];
  let transfer_escaped_geometry=interpolation_add ~owner:Rdk.Attribute.Point ~name:"weight\tvalue"
    (Rdk.Attribute.Float (Array.make (Rdk.Geometry.point_count transfer_source_geometry) 7.)) transfer_source_geometry in
  let transfer_escaped_source=Lisp_sop.snapshot (transfer_escaped_geometry) in
  let transfer_escaped=Lisp_sop.node ~with_:["transfer_escaped_source", (transfer_escaped_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_escaped_source)
   (sop/ext_transfer_target)
   :use_names true
   :names "weight\\tvalue")|} in
  let transfer_escaped_native=native_transfer ~owner:Rdk.Attribute.Point ~mode:Rdk.Attribute_ops.Nearest
    ~names:["weight\tvalue"] ~max_distance:1. ~blend_width:0. ~falloff:Rdk.Attribute_ops.Linear
    ~unmatched:Rdk.Attribute_ops.Keep_target ~source:transfer_escaped_geometry ~target:transfer_target_geometry () in
  check (equal_geometry transfer_escaped_native (cook 1 transfer_escaped)) "Attribute Transfer escaped exact-name native parity";
  same_node "Attribute Transfer escaped exact names" ~typed:transfer_escaped
    ~catalog:(from_factory Nodes.Attribute_transfer.factory ["use_names",Bool_value true;
      "names",Text_value "weight\\tvalue"] [transfer_escaped_source;transfer_target]);
  let transfer_inspector=Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer (sop/ext_transfer_source) (sop/ext_transfer_target))|} in
  List.iter (fun values -> check (Result.is_error (Node.apply_parameters transfer_inspector values))
    "Attribute Transfer inspector validation error") [
      ["source_vertex_group",Text_value "corners"];
      ["pattern",Text_value "bad["];
      ["use_names",Bool_value true;"names",Text_value "weight\nweight"];
      ["distance_mode",Choice_value "Auto";"blend_width",Float_value 1.]];
  cache_identity "Attribute Transfer all fields"
    ~companions:["source_vertex_group",["owner",Choice_value "Vertex"];
      "source_vertex_group_pattern",["owner",Choice_value "Vertex"]]
    (Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :pattern "weight")|});
  List.iter (fun build -> check (rejected build) "Attribute Transfer construction refusal") [
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer (sop/ext_transfer_source) (sop/ext_transfer_target) :neighbors 0)|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :mode "Inverse distance"
   :power 0.0)|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :kernel_radius -1.0)|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] (Printf.sprintf {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :max_distance %s)|} ((Lisp_sop.float Float.max_float))));
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :distance_mode "Auto"
   :blend_width 0.5)|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :uniform_bias 1.1)|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :owner "Detail")|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :owner "Vertex"
   :mode "Links kernel")|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :source_vertex_group "g")|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :source_group_pattern "bad[")|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :pattern "bad[")|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :use_names true
   :names "weight
weight")|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :use_names true
   :names "P")|});
      (fun () -> Lisp_sop.node ~with_:["transfer_source", (transfer_source); "transfer_target", (transfer_target)] {|(sop/attribute_transfer
   (sop/ext_transfer_source)
   (sop/ext_transfer_target)
   :use_names true
   :names "weight	other")|})];
  let composite_geometry offset =
    let geometry = Rdk.Plane_generators.grid ~columns:2 ~rows:2 ~size:2. () |> Result.get_ok in
    List.fold_left (fun geometry (owner,_) ->
      let count = interpolation_count owner geometry in
      let values = Array.init count (fun i -> offset +. float_of_int (i+1)) in
      geometry
      |> interpolation_add ~owner ~name:"value" (Rdk.Attribute.Float values)
      |> interpolation_add ~owner ~name:"vector2" (Rdk.Attribute.Float2
          (Rdk.Packed.Float2.of_owned ~x:(Array.copy values) ~y:(Array.map (( *. ) 2.) values) |> Result.get_ok))
      |> interpolation_add ~owner ~name:"vector3" (Rdk.Attribute.Float3
          (Rdk.Packed.Float3.Private.of_owned_exn ~x:(Array.copy values)
            ~y:(Array.map (( *. ) 2.) values) ~z:(Array.map (( *. ) 3.) values)))
      |> interpolation_add ~owner ~name:"vector4" (Rdk.Attribute.Float4
          (Rdk.Packed.Float4.of_owned ~x:(Array.copy values) ~y:(Array.map (( *. ) 2.) values)
            ~z:(Array.map (( *. ) 3.) values) ~w:(Array.map (( *. ) 4.) values) |> Result.get_ok))
      |> interpolation_add ~owner ~name:"alpha" (Rdk.Attribute.Float
          (Array.init count (fun i -> if i mod 3=0 then 0. else 0.5)))
      |> interpolation_add ~owner ~name:"ignored" (Rdk.Attribute.Int (Array.make count 7)))
      geometry interpolation_owners in
  let composite_base_geometry=composite_geometry 0.
  and composite_layer_geometry=composite_geometry 5. in
  let composite_base=Lisp_sop.snapshot (composite_base_geometry)
  and composite_layer=Lisp_sop.snapshot (composite_layer_geometry) in
  let composite_operations=[Rdk.Attribute_composite.Composite_mean,"Mean";
    Composite_maximum,"Maximum";Composite_minimum,"Minimum";Composite_over,"Over";Composite_under,"Under"] in
  let composite_cases=[None,None,None,None,[],[];
    Some composite_layer,None,None,None,[],[0.75];
    None,Some composite_layer,None,Some composite_layer,[],[0.;1.5];
    Some composite_layer,Some composite_layer,Some composite_layer,Some composite_layer,
      [composite_layer;composite_layer;composite_layer],[0.75;0.;-.0.25;1.5;0.2;1.;-.0.1]] in
  List.iter (fun (operation,operation_name) -> List.iter (fun allow_position ->
    List.iter (fun alpha_attribute -> List.iter (fun selected_owner ->
      let pattern owner=if selected_owner=None || selected_owner=Some owner then "P value vector*" else "^*" in
      let point_attributes=pattern Rdk.Attribute.Point and vertex_attributes=pattern Rdk.Attribute.Vertex
      and primitive_attributes=pattern Rdk.Attribute.Primitive and detail_attributes=pattern Rdk.Attribute.Detail in
      List.iter (fun (layer1,layer2,layer3,layer4,layers,native_weights) ->
        let typed=composite composite_base layer1 layer2 layer3 layer4 layers
          ~args:[ks "operation" operation_name; kf "weight" 1.25; ks "point_attributes" point_attributes;
                 ks "vertex_attributes" vertex_attributes; ks "primitive_attributes" primitive_attributes;
                 ks "detail_attributes" detail_attributes; kb "allow_position" allow_position;
                 ks "alpha_attribute" alpha_attribute; kf "weight1" 0.75; kf "weight2" 0.; kf "weight3" (-0.25);
                 kf "weight4" 1.5; ks "weights" "0.2\n1\n-0.1"] in
        let inputs=List.map (fun weight -> Rdk.Attribute_composite.input ~weight composite_layer_geometry) native_weights in
        let native=Rdk.Attribute_composite.run ~operation ~weight:1.25 ~point_attributes ~vertex_attributes
          ~primitive_attributes ~detail_attributes ~allow_position
          ?alpha_attribute:(if String.trim alpha_attribute="" then None else Some alpha_attribute)
          ~inputs composite_base_geometry |> function Ok geometry -> geometry | Error error -> fail (Rdk.Error.to_string error) in
        check (equal_geometry native (cook 1 typed)) "Attribute Composite native operations/owners/weights parity";
        same_node "Attribute Composite fixed and unlimited layer controls" ~typed
          ~catalog:(from_factory ~optional_inputs:(Some composite_base :: layer1 :: layer2 :: layer3 :: layer4
              :: List.map Option.some layers) Nodes.Attribute_composite.factory [
            "operation",Choice_value operation_name;"weight",Float_value 1.25;
            "point_attributes",Text_value point_attributes;"vertex_attributes",Text_value vertex_attributes;
            "primitive_attributes",Text_value primitive_attributes;"detail_attributes",Text_value detail_attributes;
            "allow_position",Bool_value allow_position;"alpha_attribute",Text_value alpha_attribute;
            "weight1",Float_value 0.75;"weight2",Float_value 0.;"weight3",Float_value (-.0.25);
            "weight4",Float_value 1.5;"weights",Text_value "0.2\n1\n-0.1"] [])) composite_cases)
      [None;Some Rdk.Attribute.Point;Some Rdk.Attribute.Vertex;Some Rdk.Attribute.Primitive;Some Rdk.Attribute.Detail])
      ["";" ";"alpha"]) [false;true]) composite_operations;
  let composite_default=composite composite_base None None None None [] in
  same_node "Attribute Composite defaults" ~typed:composite_default
    ~catalog:(from_factory ~optional_inputs:[Some composite_base;None;None;None;None]
      Nodes.Attribute_composite.factory [] []);
  let composite_missing_weights=composite composite_base None None None None [composite_layer;composite_layer]
    ~args:[ks "weights" "0.5"] in
  let composite_missing_native=Rdk.Attribute_composite.run
    ~inputs:[Rdk.Attribute_composite.input ~weight:0.5 composite_layer_geometry;
      Rdk.Attribute_composite.input ~weight:1. composite_layer_geometry] composite_base_geometry |> Result.get_ok in
  check (equal_geometry composite_missing_native (cook 1 composite_missing_weights))
    "Attribute Composite missing additional weights use Lisp default";
  let composite_cache=Lisp_sop.node ~with_:["composite_base", (composite_base); "composite_layer", (composite_layer)] {|(sop/attribute_composite
   (sop/ext_composite_base)
   (sop/ext_composite_layer)
   (sop/ext_composite_layer)
   (sop/ext_composite_layer)
   (sop/ext_composite_layer)
   (sop/ext_composite_layer))|} in
  cache_identity "Attribute Composite all fields" ~changes:["weights",Text_value "0.5"] composite_cache;
  List.iter (fun build -> check (rejected build)
      "Attribute Composite construction validation") [
    (fun () -> composite composite_base None None None None [] ~args:[ks "weights" "nan"]);
    (fun () -> composite composite_base None None None None [] ~args:[ks "weights" "1\t2"]);
    (fun () -> composite composite_base None None None None [] ~args:[ks "weights" "invalid"]);
    (fun () -> composite composite_base None None None None [] ~args:[ks "point_attributes" "bad["])];
  List.iter (fun values -> check (Result.is_error (Node.apply_parameters composite_cache values))
      "Attribute Composite inspector validation") [
    ["weights",Text_value "infinity"];["weights",Text_value "1\t2"];
    ["vertex_attributes",Text_value "bad["]];
  let composite_third_geometry=composite_geometry 9. in
  let composite_third=Lisp_sop.snapshot (composite_third_geometry) in
  List.iter (fun (operation,operation_name) ->
    let typed=composite composite_base None (Some composite_layer) None (Some composite_third)
      [composite_third;composite_layer]
      ~args:[ks "operation" operation_name; kf "weight" 1.25; kf "weight2" 0.7; kf "weight4" 0.2;
             ks "weights" "0.3\n0.4"; ks "alpha_attribute" "alpha"; kb "allow_position" true] in
    let native=Rdk.Attribute_composite.run ~operation ~weight:1.25 ~alpha_attribute:"alpha" ~allow_position:true
      ~inputs:[Rdk.Attribute_composite.input ~weight:0.7 composite_layer_geometry;
        Rdk.Attribute_composite.input ~weight:0.2 composite_third_geometry;
        Rdk.Attribute_composite.input ~weight:0.3 composite_third_geometry;
        Rdk.Attribute_composite.input ~weight:0.4 composite_layer_geometry] composite_base_geometry |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "Attribute Composite distinct ordered layer native parity";
    same_node "Attribute Composite distinct fixed/rest input order" ~typed
      ~catalog:(from_factory ~optional_inputs:[Some composite_base;None;Some composite_layer;None;
        Some composite_third;Some composite_third;Some composite_layer] Nodes.Attribute_composite.factory [
        "operation",Choice_value (List.assoc operation composite_operations);"weight",Float_value 1.25;
        "weight2",Float_value 0.7;"weight4",Float_value 0.2;"weights",Text_value "0.3\n0.4";
        "alpha_attribute",Text_value "alpha";"allow_position",Bool_value true] [])) composite_operations;
  let composite_large offset=Rdk.Line_geometry.points
    (Array.init 10_000 (fun i -> offset +. float i,0.,0.))
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"value"
      (Rdk.Attribute.Float (Array.init 10_000 (fun i -> offset +. float (i mod 97)))) in
  let large_base_geometry=composite_large 0. and large_layer_geometry=composite_large 7. in
  let large_base=Lisp_sop.snapshot (large_base_geometry) and large_layer=Lisp_sop.snapshot (large_layer_geometry) in
  List.iter (fun (operation,name) ->
    let typed=composite large_base None None None None [large_layer]
      ~args:[ks "operation" name; ks "point_attributes" "P value"; kb "allow_position" true; kf "weight" 0.25;
             ks "weights" "0.75"] in
    let native=Rdk.Attribute_composite.run ~operation ~point_attributes:"P value" ~allow_position:true
      ~weight:0.25 ~inputs:[Rdk.Attribute_composite.input ~weight:0.75 large_layer_geometry]
      large_base_geometry |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "Attribute Composite parallel native parity";
    same_node "Attribute Composite parallel all-operation parity" ~typed
      ~catalog:(from_factory ~optional_inputs:[Some large_base;None;None;None;None;Some large_layer]
        Nodes.Attribute_composite.factory ["operation",Choice_value name;"point_attributes",Text_value "P value";
          "allow_position",Bool_value true;"weight",Float_value 0.25;"weights",Text_value "0.75"] [])) composite_operations;
  let blend_geometry offset shift =
    let count=9 in
    let values=Array.init count (fun i -> offset +. float (i+1)) in
    let ids=Array.init count (fun i -> (i+shift) mod count) in
    Rdk.Line_geometry.points (Array.init count (fun i -> offset +. float i,0.,0.))
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"value" (Rdk.Attribute.Float values)
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"vector2" (Rdk.Attribute.Float2
      (Rdk.Packed.Float2.of_owned ~x:(Array.copy values) ~y:(Array.map (( *. ) 2.) values) |> Result.get_ok))
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"vector3" (Rdk.Attribute.Float3
      (Rdk.Packed.Float3.Private.of_owned_exn ~x:(Array.copy values)
        ~y:(Array.map (( *. ) 2.) values) ~z:(Array.map (( *. ) 3.) values)))
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"vector4" (Rdk.Attribute.Float4
      (Rdk.Packed.Float4.of_owned ~x:(Array.copy values) ~y:(Array.map (( *. ) 2.) values)
        ~z:(Array.map (( *. ) 3.) values) ~w:(Array.map (( *. ) 4.) values) |> Result.get_ok))
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"mask" (Rdk.Attribute.Float
      (Array.init count (fun i -> float ((i+shift) mod 3) *. 0.5)))
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"other_mask" (Rdk.Attribute.Float
      (Array.init count (fun i -> float ((i+shift+1) mod 3) *. 0.5)))
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"id" (Rdk.Attribute.Int ids)
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"text_id" (Rdk.Attribute.Text (Array.map string_of_int ids))
    |> fun geometry -> Rdk.Geometry.with_group
      (Rdk.Group.init ~owner:Rdk.Group.Point ~name:"selected" count (fun i -> i mod 2=0)) geometry |> Result.get_ok in
  let blend_base_geometry=blend_geometry 0. 0
  and blend_first_geometry=blend_geometry 10. 1
  and blend_second_geometry=blend_geometry 20. 2 in
  let blend_base=Lisp_sop.snapshot (blend_base_geometry) and blend_first=Lisp_sop.snapshot (blend_first_geometry)
  and blend_second=Lisp_sop.snapshot (blend_second_geometry) in
  let blend_modes=[Rdk.Blend_shapes.Blend_normalized,"Normalized";Blend_differencing,"Differencing"]
  and blend_maskings=[Rdk.Blend_shapes.Blend_no_mask,"None";Blend_set_from_attribute,"Set from attribute";
    Blend_scale_from_attribute,"Scale from attribute"]
  and blend_sources=[Rdk.Blend_shapes.Blend_mask_first_input,"First input";Blend_mask_shape,"Shape"] in
  let blend_cases=[None,None,None,None,[],[];
    Some blend_first,None,None,None,[],[1,1.5,blend_first_geometry];
    None,Some blend_first,None,Some blend_second,[],[2,-.0.5,blend_first_geometry;4,0.,blend_second_geometry];
    Some blend_first,Some blend_second,Some blend_first,Some blend_second,[blend_second;blend_first;blend_second],
      [1,1.5,blend_first_geometry;2,-.0.5,blend_second_geometry;3,0.3,blend_first_geometry;4,0.,blend_second_geometry;
       5,0.4,blend_second_geometry;6,-.0.2,blend_first_geometry;7,0.6,blend_second_geometry]] in
  List.iter (fun (mode,mode_name) -> List.iter (fun (masking,masking_name) ->
    List.iter (fun (mask_source,source_name) -> List.iter (fun point_id_attribute ->
      List.iter (fun group -> List.iter (fun attributes -> List.iter (fun overrides ->
        let shape_masks=if overrides then "2\tother_mask\tshape\n5\tother_mask\tfirst\n6\t\tshape" else "" in
        let setting index=if overrides && index=2 then Some "other_mask",Rdk.Blend_shapes.Blend_mask_shape
          else if overrides && index=5 then Some "other_mask",Rdk.Blend_shapes.Blend_mask_first_input
          else if overrides && index=6 then None,Rdk.Blend_shapes.Blend_mask_shape else None,mask_source in
        List.iter (fun (shape1,shape2,shape3,shape4,shapes,native_shapes) ->
          let typed=blend_shapes blend_base shape1 shape2 shape3 shape4 shapes
            ~args:[ks "mode" mode_name; ks "masking" masking_name; ks "mask_source" source_name;
                   ks "point_id_attribute" point_id_attribute; ks "group" group; ks "attributes" attributes;
                   kf "weight1" 1.5; kf "weight2" (-0.5); kf "weight3" 0.3; kf "weight4" 0.;
                   ks "weights" "0.4\n-0.2\n0.6"; ks "shape_masks" shape_masks] in
          let native_shapes=List.map (fun (index,weight,geometry) ->
            let mask_attribute,mask_source=setting index in
            Rdk.Blend_shapes.shape ?mask_attribute ~mask_source ~weight geometry) native_shapes in
          let points=if group="" then None else Rdk.Geometry.find_group ~owner:Rdk.Group.Point group blend_base_geometry in
          let native=Rdk.Blend_shapes.run ~mode ~masking ?points ~attributes
            ?mask_attribute:(if masking=Rdk.Blend_shapes.Blend_no_mask then None else Some "mask")
            ?point_id_attribute:(if point_id_attribute="" then None else Some point_id_attribute)
            ~shapes:native_shapes blend_base_geometry |> function Ok geometry -> geometry | Error error -> fail (Rdk.Error.to_string error) in
          check (equal_geometry native (cook 1 typed)) "Blend Shapes native modes/masks/matching/weights parity";
          same_node "Blend Shapes flat fixed and unlimited shape controls" ~typed
            ~catalog:(from_factory ~optional_inputs:(Some blend_base::shape1::shape2::shape3::shape4
                :: List.map Option.some shapes) Nodes.Blend_shapes.factory [
              "mode",Choice_value mode_name;"masking",Choice_value masking_name;"mask_source",Choice_value source_name;
              "point_id_attribute",Text_value point_id_attribute;"group",Text_value group;"attributes",Text_value attributes;
              "weight1",Float_value 1.5;"weight2",Float_value (-.0.5);"weight3",Float_value 0.3;"weight4",Float_value 0.;
              "weights",Text_value "0.4\n-0.2\n0.6";"shape_masks",Text_value shape_masks] [])) blend_cases)
        [false;true]) ["^*";"value vector*"]) ["";"selected"])
      ["";"id";"text_id"]) blend_sources) blend_maskings) blend_modes;
  let blend_default=blend_shapes blend_base None None None None [] in
  same_node "Blend Shapes defaults/empty inputs" ~typed:blend_default
    ~catalog:(from_factory ~optional_inputs:[Some blend_base;None;None;None;None] Nodes.Blend_shapes.factory [] []);
  check (Node.id blend_default<>Node.id blend_base && Node.parameter_key blend_default<>""
    && equal_geometry (cook 1 blend_default) blend_base_geometry) "Blend Shapes empty list retains Lisp node identity";
  let blend_missing_weights=blend_shapes blend_base None None None None [blend_first;blend_second] ~args:[ks "weights" "0.5"] in
  let blend_missing_native=Rdk.Blend_shapes.run
    ~shapes:[Rdk.Blend_shapes.shape ~weight:0.5 blend_first_geometry;
      Rdk.Blend_shapes.shape ~weight:0. blend_second_geometry] blend_base_geometry |> Result.get_ok in
  check (equal_geometry blend_missing_native (cook 1 blend_missing_weights)) "Blend Shapes missing extra weights default to zero";
  let blend_cache=Lisp_sop.node ~with_:["blend_base", (blend_base); "blend_first", (blend_first); "blend_second", (blend_second)] {|(sop/blend_shapes
   (sop/ext_blend_base)
   (sop/ext_blend_first)
   (sop/ext_blend_second)
   (sop/ext_blend_first)
   (sop/ext_blend_second)
   (sop/ext_blend_first)
   :attributes "value vector*")|} in
  cache_identity "Blend Shapes every field"
    ~changes:["weights",Text_value "0.5";"shape_masks",Text_value "5\tother_mask\tshape"] blend_cache;
  List.iter (fun build -> check (rejected build)
    "Blend Shapes constructor validation") [
    (fun () -> blend_shapes blend_base None None None None [] ~args:[ks "weights" "nan"]);
    (fun () -> blend_shapes blend_base None None None None [] ~args:[ks "weights" "1\t2"]);
    (fun () -> blend_shapes blend_base None None None None [] ~args:[ks "shape_masks" "0\tmask\tshape"]);
    (fun () -> blend_shapes blend_base None None None None [] ~args:[ks "shape_masks" "1\tmask\tbad"]);
    (fun () -> blend_shapes blend_base None None None None [] ~args:[ks "shape_masks" "1\tmask\tshape\n1\tother_mask\tfirst"]);
    (fun () -> blend_shapes blend_base None None None None [] ~args:[ks "shape_masks" "1\tmask"]);
    (fun () -> blend_shapes blend_base None None None None [] ~args:[ks "attributes" "bad["])];
  List.iter (fun values -> check (Result.is_error (Node.apply_parameters blend_cache values))
    "Blend Shapes inspector validation") [["weights",Text_value "infinity"];
      ["shape_masks",Text_value "0\tmask\tshape"];["shape_masks",Text_value "1\tmask\tinvalid"];
      ["attributes",Text_value "bad["]];
  List.iter (fun mask_attribute -> List.iter (fun shape_masks ->
    let override=if shape_masks="" then None else Some "other_mask" in
    let typed=blend_shapes blend_base (Some blend_first) None None None []
      ~args:[ks "group" " "; ks "point_id_attribute" " "; ks "mask_attribute" mask_attribute;
             ks "masking" "Scale from attribute"; kf "weight1" 0.4; ks "shape_masks" shape_masks] in
    let native=Rdk.Blend_shapes.run ~masking:Rdk.Blend_shapes.Blend_scale_from_attribute
      ?mask_attribute:(if String.trim mask_attribute="" then None else Some mask_attribute)
      ~shapes:[Rdk.Blend_shapes.shape ~mask_source:Rdk.Blend_shapes.Blend_mask_first_input
        ?mask_attribute:override ~weight:0.4 blend_first_geometry] blend_base_geometry |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "Blend Shapes blank global names and per-shape override parity";
    same_node "Blend Shapes optional names and per-shape masks" ~typed
      ~catalog:(from_factory ~optional_inputs:[Some blend_base;Some blend_first;None;None;None] Nodes.Blend_shapes.factory [
        "group",Text_value " ";"point_id_attribute",Text_value " ";"mask_attribute",Text_value mask_attribute;
        "masking",Choice_value "Scale from attribute";"weight1",Float_value 0.4;"shape_masks",Text_value shape_masks] []))
      ["";"1\tother_mask\tfirst"]) ["";" ";"mask"];
  let blend_partial_geometry=Rdk.Line_geometry.points [|50.,0.,0.;60.,0.,0.|]
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"id" (Rdk.Attribute.Int [|0;7|])
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"text_id" (Rdk.Attribute.Text [|"0";"7"|]) in
  let blend_partial=Lisp_sop.snapshot (blend_partial_geometry) in
  List.iter (fun (mode,mode_name) -> List.iter (fun point_id_attribute ->
    let typed=blend_shapes blend_base None None None None [blend_partial]
      ~args:[ks "mode" mode_name; ks "point_id_attribute" point_id_attribute; ks "weights" "0.8"] in
    let native=Rdk.Blend_shapes.run ~mode ~point_id_attribute
      ~shapes:[Rdk.Blend_shapes.shape ~weight:0.8 blend_partial_geometry] blend_base_geometry |> Result.get_ok in
    check (equal_geometry native (cook 1 typed)) "Blend Shapes partial ID matching and missing target attribute parity";
    same_node "Blend Shapes differently sized partial-ID targets" ~typed
      ~catalog:(from_factory ~optional_inputs:[Some blend_base;None;None;None;None;Some blend_partial] Nodes.Blend_shapes.factory [
        "mode",Choice_value mode_name;"point_id_attribute",Text_value point_id_attribute;"weights",Text_value "0.8"] []))
      ["id";"text_id"]) blend_modes;
  let blend_large offset shift=let count=10_000 in
    composite_large offset
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"mask" (Rdk.Attribute.Float
      (Array.init count (fun i -> float ((i+shift) mod 3) *. 0.5)))
    |> interpolation_add ~owner:Rdk.Attribute.Point ~name:"id" (Rdk.Attribute.Int
      (Array.init count (fun i -> (i+shift) mod count))) in
  let blend_large_base_geometry=blend_large 0. 0 and blend_large_shape_geometry=blend_large 7. 1 in
  let blend_large_base=Lisp_sop.snapshot (blend_large_base_geometry) and blend_large_shape=Lisp_sop.snapshot (blend_large_shape_geometry) in
  List.iter (fun (mode,mode_name) -> List.iter (fun (masking,masking_name) ->
    List.iter (fun (mask_source,source_name) ->
      let typed=blend_shapes blend_large_base None None None None [blend_large_shape]
        ~args:[ks "mode" mode_name; ks "masking" masking_name; ks "mask_source" source_name;
               ks "point_id_attribute" "id"; ks "weights" "0.37"] in
      let native=Rdk.Blend_shapes.run ~mode ~masking ~mask_attribute:"mask" ~point_id_attribute:"id"
        ~shapes:[Rdk.Blend_shapes.shape ~weight:0.37 ~mask_source blend_large_shape_geometry]
        blend_large_base_geometry |> Result.get_ok in
      check (equal_geometry native (cook 1 typed)) "Blend Shapes native parallel masked/mapped parity";
      same_node "Blend Shapes large parallel modes and masks" ~typed
        ~catalog:(from_factory ~optional_inputs:[Some blend_large_base;None;None;None;None;Some blend_large_shape]
          Nodes.Blend_shapes.factory ["mode",Choice_value mode_name;"masking",Choice_value masking_name;
            "mask_source",Choice_value source_name;"point_id_attribute",Text_value "id";"weights",Text_value "0.37"] []))
      blend_sources) blend_maskings) blend_modes;
  let blend_escaped_geometry=interpolation_add ~owner:Rdk.Attribute.Point ~name:"mask\tvalue"
    (Rdk.Attribute.Float (Array.make 9 0.4)) blend_first_geometry in
  let blend_escaped=Lisp_sop.snapshot (blend_escaped_geometry) in
  let blend_escaped_typed=blend_shapes blend_base None None None None [blend_escaped]
    ~args:[ks "masking" "Scale from attribute"; ks "mask_attribute" ""; ks "weights" "0.75";
           ks "shape_masks" "5\tmask\\tvalue\tshape"] in
  let blend_escaped_native=Rdk.Blend_shapes.run ~masking:Rdk.Blend_shapes.Blend_scale_from_attribute
    ~shapes:[Rdk.Blend_shapes.shape ~weight:0.75 ~mask_attribute:"mask\tvalue"
      ~mask_source:Rdk.Blend_shapes.Blend_mask_shape blend_escaped_geometry] blend_base_geometry |> Result.get_ok in
  check (equal_geometry blend_escaped_native (cook 1 blend_escaped_typed)) "Blend Shapes escaped per-shape mask native parity";
  same_node "Blend Shapes escaped mask overrides" ~typed:blend_escaped_typed
    ~catalog:(from_factory ~optional_inputs:[Some blend_base;None;None;None;None;Some blend_escaped]
      Nodes.Blend_shapes.factory ["masking",Choice_value "Scale from attribute";"mask_attribute",Text_value "";
        "weights",Text_value "0.75";"shape_masks",Text_value "5\tmask\\tvalue\tshape"] []);
  same_cook "Group Random actual Lisp defaults" ~typed:(Lisp_sop.node ~with_:["warped", (warped)] {|(sop/group_random (sop/ext_warped))|})
    ~factory:Nodes.Group_random.factory [] warped;
  same_cook "Noise Displace actual Lisp defaults" ~typed:(Lisp_sop.node ~with_:["warped", (warped)] {|(sop/noise_displace (sop/ext_warped))|})
    ~factory:Nodes.Noise_displace.factory [] warped;
  List.iter (fun context_seed -> List.iter (fun seed ->
    same_cook "Group Random explicit context flag" ~typed:(Lisp_sop.node ~with_:["warped", (warped)] (Printf.sprintf {|(sop/group_random (sop/ext_warped) :context_seed %b :seed %d)|} (context_seed) (seed)))
      ~factory:Nodes.Group_random.factory ["context_seed",Bool_value context_seed;"seed",Int_value seed] warped;
    same_cook (Printf.sprintf "Noise Displace context_seed=%b seed=%d" context_seed seed) ~typed:(Lisp_sop.node ~with_:["warped", (warped)] (Printf.sprintf {|(sop/noise_displace (sop/ext_warped) :context_seed %b :seed %d)|} (context_seed) (seed)))
      ~factory:Nodes.Noise_displace.factory ["context_seed",Bool_value context_seed;"seed",Int_value seed] warped)
    [0;17]) [false;true];
  List.iter (fun count ->
    let inputs = List.take count merge_inputs in
    let a, b, rest = match inputs with a :: b :: rest -> a, b, rest | _ -> fail "switch inputs" in
    List.iteri (fun input branch ->
      let typed = sop ~inputs:(a :: b :: rest) "switch" [ki "input" input] in
      same_node (Printf.sprintf "switch %d of %d" input count) ~typed
        ~catalog:(from_factory ~optional_inputs:(List.map Option.some inputs) Nodes.Switch.factory
          ["input", Param.Int_value input] []);
      check (equal_geometry (cook 1 typed) (cook 1 branch)) "switch cooks its selected branch") inputs;
    check (rejected (fun () -> sop ~inputs:(a :: b :: rest) "switch" [ki "input" count]))
      "switch refuses a branch that is not connected")
    [2; 3; 5];
  (* sop/curve :closed joins the last point to the first *)
  let triangle = [|0.,0.,0.; 1.,0.,0.; 0.,1.,0.|] in
  let curve_text closed = Printf.sprintf "(sop/curve (list %s) :closed %b)"
    (String.concat " " (List.map (fun (x, y, z) -> Printf.sprintf "[%s %s %s]"
      (Lisp_sop.float x) (Lisp_sop.float y) (Lisp_sop.float z)) (Array.to_list triangle))) closed in
  let open_curve = cook 1 (Lisp_sop.node (curve_text false))
  and closed_curve = cook 1 (Lisp_sop.node (curve_text true)) in
  check (equal_geometry open_curve (Rdk.Line_geometry.polyline ~closed:false triangle |> get_rdk))
    "sop/curve :closed false is not the open polyline";
  check (equal_geometry closed_curve (Rdk.Line_geometry.polyline ~closed:true triangle |> get_rdk))
    "sop/curve :closed true is not the closed polyline";
  check (not (equal_geometry open_curve closed_curve)) "sop/curve ignored :closed";
  (* sop/merge :source_attribute / :source_base tag each primitive with its input, from the base *)
  let first_input = sop "box" [] and second_input = sop "box" [kv "size" (Vec3.create 2. 2. 2.)] in
  let tagged = cook 1 (sop ~inputs:[first_input; second_input] "merge"
    [ks "source_attribute" "src"; ki "source_base" 10]) in
  let first_count = Rdk.Geometry.primitive_count (cook 1 first_input)
  and second_count = Rdk.Geometry.primitive_count (cook 1 second_input) in
  (match Option.map Rdk.Attribute.storage
      (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive "src" tagged) with
   | Some (Rdk.Attribute.Int values) ->
       check (values = Array.init (first_count + second_count)
         (fun index -> if index < first_count then 10 else 11))
         "sop/merge :source_attribute did not tag each primitive with its input"
   | _ -> fail "sop/merge ignored :source_attribute");
  check (Rdk.Geometry.find_attribute ~owner:Rdk.Attribute.Primitive "src"
      (cook 1 (sop ~inputs:[first_input; second_input] "merge" [])) = None)
    "sop/merge tagged without :source_attribute";
  (* a prism from explicit points: a closed sop/curve swept along a two-point curve, with caps *)
  let star = Array.init 18 (fun i ->
    let a = Float.pi *. float i /. 9. and r = if i mod 2 = 0 then 1. else 0.45 in
    r *. cos a, 0., r *. sin a) in
  let prism = cook 1 (Lisp_sop.node (Printf.sprintf
    "(sop/sweep (sop/curve (list [0.0 0.0 0.0] [0.0 0.65 0.0])) (sop/curve (list %s) :closed true) :caps true)"
    (String.concat " " (List.map (fun (x, y, z) -> Printf.sprintf "[%s %s %s]"
      (Lisp_sop.float x) (Lisp_sop.float y) (Lisp_sop.float z)) (Array.to_list star))))) in
  let sizes = let view = Rdk.Topology.Private.view (Rdk.Geometry.topology prism) in
    List.init (Rdk.Geometry.primitive_count prism) (fun i ->
      view.primitive_offsets.(i + 1) - view.primitive_offsets.(i)) in
  check (Rdk.Geometry.point_count prism = 36
      && sizes = List.init 18 (fun _ -> 4) @ [18; 18])
    "a swept closed curve is not an 18-sided prism with two 18-point caps";
  (* :filled makes the same points one 18-sided polygon primitive, which poly_extrude solidifies *)
  let star_points = String.concat " " (List.map (fun (x, y, z) -> Printf.sprintf "[%s %s %s]"
      (Lisp_sop.float x) (Lisp_sop.float y) (Lisp_sop.float z)) (Array.to_list star)) in
  let filled = cook 1 (Lisp_sop.node (Printf.sprintf "(sop/curve (list %s) :filled true)" star_points)) in
  let view = Rdk.Topology.Private.view (Rdk.Geometry.topology filled) in
  check (Rdk.Geometry.point_count filled = 18 && view.primitive_offsets = [|0; 18|]
      && Rdk.Topology.primitive_kind (Rdk.Geometry.topology filled) 0 = Rdk.Topology.Polygon)
    "sop/curve :filled is not one 18-point polygon";
  let solid = cook 1 (Lisp_sop.node (Printf.sprintf
    "(sop/poly_extrude (sop/curve (list %s) :filled true) :distance 0.65)" star_points)) in
  let star_area = 18. *. 0.5 *. 0.45 *. sin (Float.pi /. 9.) in
  check (Rdk.Geometry.primitive_count solid = 20
      && abs_float (abs_float (Result.get_ok (Rdk.Analysis.signed_volume solid))
                    -. star_area *. 0.65) < 1e-9)
    "a filled curve extruded is not the 20-face star prism";
  check (!nonfinite_fields >= 130) "nonfinite declaration coverage fell below the removed cases";
  if !failures > 0 then failwith (Printf.sprintf "%d node checks failed" !failures);
  Printf.printf "SOP node declaration tests passed; %d floating field instances reject NaN and both infinities atomically\n"
    !nonfinite_fields
