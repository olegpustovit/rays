(* sop/attr_planes reads any float-family attribute as x/y/z/w planes with a cook-time width;
   sop/with_planes writes them back with the matching storage type. *)
open Rdk
open Rdk_test_support
module L = Flow_sop.Lower
module Edit = Sop.Edit_graph
let flow = function Ok x->x|Error d->failwith(Diagnostic.to_string d)
let raw = [|0.;-0.;1.5;Float.min_float /. 4.;-3.25e300;7.|]
let plane k n = Array.init n (fun i->raw.((i+k) mod Array.length raw))
let make () =
  let g = Plane_generators.grid ~connectivity:Grid_quads ~columns:3 ~rows:2 ~size:2. () |> get_ok in
  List.fold_left (fun g owner ->
    let n = match owner with Attribute.Point->Geometry.point_count g|Vertex->Geometry.vertex_count g
      |Primitive->Geometry.primitive_count g|Detail->1 in
    let add name storage g = Geometry.with_attribute (Attribute.create_owned ~name ~owner storage |> get_string_ok) g |> get_string_ok in
    g |> add "f" (Attribute.Float (plane 0 n))
      |> add "v2" (Attribute.Float2 (Packed.Float2.of_owned ~x:(plane 1 n) ~y:(plane 2 n) |> get_string_ok))
      |> add "v3" (Attribute.Float3 (Packed.Float3.View.of_owned_exn ~x:(plane 1 n) ~y:(plane 2 n) ~z:(plane 3 n)))
      |> add "v4" (Attribute.Float4 (Packed.Float4.of_owned ~x:(plane 1 n) ~y:(plane 2 n) ~z:(plane 3 n) ~w:(plane 4 n) |> get_string_ok))
      |> add "i" (Attribute.Int (Array.init n Fun.id))
      |> add "nf" (Attribute.Float (Array.init n (fun i->if i = 0 then Float.nan else 1.)))) g
    [Attribute.Point; Vertex; Primitive; Detail]
let src = lazy (make ())
let factory key geometry = Edit.factory ~key ~label:key ~category:["Test"] ~arity:0 (fun _->Ok (Lisp_sop.snapshot geometry))
let cook ?(reference=false) ?(domains=1) ?(grain=97) body =
  let text = Printf.sprintf "(workspace planes_test (graph g :context sop (let* [src (sop/src)] %s)))" body in
  let lowered = L.workspace ~reference ~factories:(factory "src" (Lazy.force src) :: Sop.Nodes.factories)
    ~inputs:["g",[]] (Flow.Syntax.parse text |> flow) |> flow in
  let graph = List.hd lowered.graphs in
  let node = Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok in
  let session = Sop.Session.create ~max_entries:32 ~max_payload_bytes:64_000_000 |> get_string_ok in
  Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
    Result.map (fun (o:Sop.Session.output)->Sop.Payload.geometry o.payload |> Result.get_ok)
      (Sop.Session.cook session ~context:(Sop.Context.create ~domains ~grain () |> get_string_ok) node))
let storage owner name g = Attribute.View.storage (Geometry.find_attribute ~owner name g |> Option.get)
let bits a = Array.map Int64.bits_of_float a
let planes_of = function
  | Attribute.Float a -> [bits a]
  | Float2 d -> let v = Packed.Float2.View.view d in [bits v.x; bits v.y]
  | Float3 d -> let v = Packed.Float3.View.view d in [bits v.x; bits v.y; bits v.z]
  | Float4 d -> let v = Packed.Float4.View.view d in [bits v.x; bits v.y; bits v.z; bits v.w]
  | _ -> []
let owners = ["point",Attribute.Point;"vertex",Attribute.Vertex;"primitive",Attribute.Primitive;"detail",Attribute.Detail]
let () =
  (* Round trip: every owner and width writes back identical storage under a new name. *)
  List.iter (fun (oname,owner) -> List.iter (fun name ->
    let body = Printf.sprintf "(sop/with_planes src \"copy\" (sop/attr_planes src %S :owner %S) :owner %S)" name oname oname in
    List.iter (fun reference -> List.iter (fun (domains,grain) ->
      match cook ~reference ~domains ~grain body with
      | Error d -> fail (Diagnostic.error_to_string d)
      | Ok out -> check (planes_of (storage owner "copy" out) = planes_of (storage owner name (Lazy.force src)) && planes_of (storage owner "copy" out) <> [])
          (Printf.sprintf "%s %s planes round trip changed storage" oname name))
      [1,97;4,1;4,4096]) [false;true]) ["f";"v2";"v3";"v4"]) owners;
  (* The cook-time width reaches the checker as a plain Int and a cooked branch can use it. *)
  (match cook "(sop/with_planes src \"w\" (sop/attr_planes src \"v3\") :owner \"point\")" with
   | Ok out -> check (planes_of (storage Attribute.Point "w" out) = planes_of (storage Attribute.Point "v3" (Lazy.force src))) "width 3 lost"
   | Error d -> fail (Diagnostic.error_to_string d));
  (* Computed planes, a literal width, and canonical P. *)
  (match cook {|(let* [p (sop/attr_planes src "P" :owner "point")
      doubled {:width p.width :x (exact (map (fn [(v : float)] (* v 2.)) p.x)) :y p.y :z p.z :w p.w}]
      (sop/with_planes src "P" doubled :owner "point"))|} with
   | Ok out -> let a = Geometry.positions out and b = Geometry.positions (Lazy.force src) in
       let a = Packed.Float3.View.view a and b = Packed.Float3.View.view b in
       check (Array.for_all2 (fun x y->x = 2. *. y) a.x b.x && a.y = b.y && a.z = b.z) "computed P planes"
   | Error d -> fail (Diagnostic.error_to_string d));
  (* Refusals keep typed codes. *)
  let zeros = "(array/float 7)" in
  List.iter (fun (what, body, code) -> match cook body with
    | Error (d:Diagnostic.t) -> check (d.code = code) (what ^ ": code " ^ d.code ^ ", expected " ^ code)
    | Ok _ -> fail (what ^ " was accepted"))
    ["int", {|(sop/with_planes src "x" (sop/attr_planes src "i" :owner "point") :owner "point")|}, "E_ATTR_TYPE";
     "missing", {|(sop/with_planes src "x" (sop/attr_planes src "nope" :owner "point") :owner "point")|}, "E_ATTR_TYPE";
     "width 0", Printf.sprintf {|(sop/with_planes src "x" {:width 0 :x %s :y %s :z %s :w %s} :owner "point")|} zeros zeros zeros zeros, "E_ATTR_WIDTH";
     "width 5", Printf.sprintf {|(sop/with_planes src "x" {:width 5 :x %s :y %s :z %s :w %s} :owner "point")|} zeros zeros zeros zeros, "E_ATTR_WIDTH";
     "count", {|(sop/with_planes src "x" {:width 1 :x (array/float 2) :y (array/float 2) :z (array/float 2) :w (array/float 2)} :owner "point")|}, "E_ATTR_COUNT";
     "nonfinite", {|(sop/with_planes src "x" (sop/attr_planes src "nf" :owner "point") :owner "point")|}, "E_NONFINITE"];
  print_endline "sop/attr_planes + sop/with_planes: all owners and widths round-trip storage, computed planes, P and typed refusals at reference/CPU/domains/grains"
