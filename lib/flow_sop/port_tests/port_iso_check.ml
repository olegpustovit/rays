(* PORT_ISO=1 main.exe nonexistent: compares the iso_surface draft with native on several fields and
   parameter sets. The port gate cannot feed a function slot (see port_iso_surface.ml), so this is the
   oracle comparison for that node: the same expression is lowered natively (Lisp_sop.node) and through
   the draft, both are cooked, and the geometries must agree within 4 ulps (close_geometry). *)
open Rdk
(* The OCaml oracle: iso_surface is registered as a Lisp node, so native is rebuilt from Sop.Nodes.Iso_surface
   over the embedded sources minus the iso_* definitions (Lisp_sop.node would now cook the Lisp node twice). *)
let native_node ctx body =
  let checked = function Ok v -> v | Error d -> failwith (Diagnostic.to_string d) in
  let drop form = let text = Flow.Lisp.flat form in
    List.exists (fun prefix -> String.starts_with ~prefix text) ["(defnode sop/iso_surface "; "(defn iso_"] in
  let sources = List.filter (fun f -> not (drop f)) (checked (Flow_sop.Catalog.sources ())) in
  let factories = Sop.Nodes.Iso_surface.factory :: ctx.Port_gate.factories in
  let catalog = checked (Flow_sop.Catalog.of_factories ~nodes:[] ~version:Flow_sop.Manifest.version factories) in
  let text = Printf.sprintf "(workspace port (graph g :context sop %s))" body in
  match Flow_sop.Catalog.check_workspace ~sources ~ops:Flow_sop.Operators.all catalog (checked (Flow.Syntax.parse text)) with
  | Some w, _ ->
      let l = checked (Flow_sop.Lower.of_checked ~factories w) in
      let g = List.hd l.graphs in
      Result.get_ok (Sop.Edit_graph.compile_node g.network.geometry ~node_id:(Option.get g.root))
  | None, ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
let () = if Sys.getenv_opt "PORT_ISO" <> None then at_exit (fun () ->
  let node = List.find (fun (n : Port_gate.t) -> n.key = "iso_surface") (Port_gate.registered ()) in
  let ctx = Port_gate.context ~dir:"lib/flow_sop/nodes" node [] in
  let lisp_node body =
    let text = Printf.sprintf "(workspace port (graph g :context sop %s))" body in
    match Flow.Syntax.parse text with
    | Error d -> Error (Diagnostic.to_string d)
    | Ok forms -> match Flow_sop.Catalog.check_workspace ~sources:ctx.Port_gate.sources ~ops:Flow_sop.Operators.all ctx.catalog forms with
      | Some w, ds when Port_gate.errors ds = [] ->
          (match Flow_sop.Lower.of_checked ~factories:ctx.factories w with
           | Error d -> Error (Diagnostic.to_string d)
           | Ok lowered -> let g = List.hd lowered.graphs in
               Sop.Edit_graph.compile_node g.network.geometry ~node_id:(Option.get g.root))
      | _, ds -> Error (String.concat "\n" (List.map Diagnostic.to_string ds)) in
  let fields = [
    "sphere", "(fn [p] (- (length p) 1.3))";
    "torus", "(fn [p] (- (+ (* (- (length [p.x p.y 0.0]) 1.2) (- (length [p.x p.y 0.0]) 1.2)) (* p.z p.z)) 0.25))";
    "waves", "(fn [p] (+ (* (sin (* 2.1 p.x)) (cos (* 1.7 p.y))) (* 0.5 (sin (* 3.0 p.z)))))";
    "plane", "(fn [p] (- (+ p.x (* 0.37 p.y)) 0.11))";
    "box", "(fn [p] (- (max (max (abs p.x) (abs p.y)) (abs p.z)) 1.0))" ] in
  let params = [
    "default", ":resolution [12 12 12]";
    "uneven", ":resolution [7 5 9] :min [-2.5 -1.5 -2.0] :max [2.0 2.5 1.75]";
    "iso", ":resolution [9 9 9] :iso 0.4";
    "flat", ":resolution [8 6 7] :smooth false";
    "thin", ":resolution [1 1 1] :min [-1.0 -1.0 -1.0] :max [1.5 1.0 1.0] :iso -0.2";
    "empty", ":resolution [4 4 4] :iso 50.0" ] in
  let session = Result.get_ok (Sop.Session.create ~max_entries:64 ~max_payload_bytes:512_000_000) in
  let cook n = match Sop.Session.cook session ~context:(Result.get_ok (Sop.Context.create ~domains:1 ~grain:97 ())) n with
    | Ok o -> Sop.Payload.geometry o.Sop.Session.payload |> Result.map_error Diagnostic.error_to_string
    | Error d -> Error d.Diagnostic.code in
  List.iter (fun (fname, field) -> List.iter (fun (pname, words) -> if Sys.getenv "PORT_ISO" = "1" || Sys.getenv "PORT_ISO" = fname ^ ":" ^ pname then
    let body = Printf.sprintf "(sop/iso_surface :field %s %s)" field words in
    let expected = cook (native_node ctx body) in
    let actual = match lisp_node body with Error m -> Error ("refused: " ^ m) | Ok n -> cook n in
    match expected, actual with
    | Ok e, Ok a ->
        Printf.printf "%-6s %-8s native %d pts / lisp %d pts: %s\n%!" fname pname (Geometry.point_count e) (Geometry.point_count a)
          (let pos g = Packed.Float3.Private.view (Geometry.positions g) in
           let nrm g = match Geometry.find_attribute ~owner:Attribute.Point "N" g with
             | Some a -> (match Attribute.Private.storage a with Attribute.Float3 v -> Packed.Float3.Private.view v | _ -> pos g)
             | None -> pos g in
           let worst (a : Packed.Float3.Private.view) (b : Packed.Float3.Private.view) =
             let w = ref 0. and at = ref 0 in
             Array.iteri (fun i v -> let d = Float.max (abs_float (v -. b.x.(i))) (Float.max (abs_float (a.y.(i) -. b.y.(i))) (abs_float (a.z.(i) -. b.z.(i)))) in
               if d > !w then (w := d; at := i)) a.x; !w, !at in
           let wp, ip = worst (pos e) (pos a) and wn, i_n = worst (nrm e) (nrm a) in
           (let bad (e : Packed.Float3.Private.view) (a : Packed.Float3.Private.view) tag = let far x y = let m = Float.max (abs_float x) (abs_float y) in abs_float (x -. y) > 4. *. (Float.succ m -. m) in (try Array.iteri (fun i _ -> List.iter (fun (x, y) -> if far x y then (Printf.printf "  %s[%d] native %h lisp %h\n" tag i x y; raise Exit)) [e.x.(i), a.x.(i); e.y.(i), a.y.(i); e.z.(i), a.z.(i)]) e.x with Exit -> ()) in bad (pos e) (pos a) "P"; bad (nrm e) (nrm a) "N");
           Printf.sprintf "%s (max |dP| %.3g at %d, max |dN| %.3g at %d)"
             (if Rdk_test_support.close_geometry ~ulps:4 e a then "OK" else "DIFFERS") wp ip wn i_n)
    | Error e, Error a -> Printf.printf "%-6s %-8s both error (%s / %s)\n%!" fname pname e a
    | Ok _, Error a -> Printf.printf "%-6s %-8s native ok, lisp error %s\n%!" fname pname a
    | Error e, Ok _ -> Printf.printf "%-6s %-8s native error %s, lisp ok\n%!" fname pname e) params) fields)

(* PORT_ISO=bench main.exe nonexistent: 100^3 cells sphere (1,030,301 samples), native vs draft, best of 2. *)
let () = if Sys.getenv_opt "PORT_ISO" = Some "bench" then at_exit (fun () ->
  let node = List.find (fun (n : Port_gate.t) -> n.key = "iso_surface") (Port_gate.registered ()) in
  let ctx = Port_gate.context ~dir:"lib/flow_sop/nodes" node [] in
  let body = "(sop/iso_surface :field (fn [p] (- (length p) 1.3)) :resolution [100 100 100])" in
  let text = Printf.sprintf "(workspace port (graph g :context sop %s))" body in
  let lisp = match Flow.Syntax.parse text with Ok forms ->
    (match Flow_sop.Catalog.check_workspace ~sources:ctx.Port_gate.sources ~ops:Flow_sop.Operators.all ctx.catalog forms with
     | Some w, _ -> (match Flow_sop.Lower.of_checked ~factories:ctx.factories w with
         | Ok l -> let g = List.hd l.graphs in Result.get_ok (Sop.Edit_graph.compile_node g.network.geometry ~node_id:(Option.get g.root))
         | Error d -> failwith (Diagnostic.to_string d))
     | _ -> failwith "check") | Error d -> failwith (Diagnostic.to_string d) in
  let native = native_node ctx body in
  List.iter (fun domains ->
    let time n = let best = ref infinity in
      for _ = 1 to 2 do
        let s = Result.get_ok (Sop.Session.create ~max_entries:4 ~max_payload_bytes:2_000_000_000) in
        let c = Result.get_ok (Sop.Context.create ~domains ~grain:4096 ()) in
        let t = Unix.gettimeofday () in
        (match Sop.Session.cook s ~context:c n with Ok _ -> () | Error d -> prerr_endline ("cook error " ^ d.Diagnostic.code));
        best := Float.min !best (Unix.gettimeofday () -. t);
        Sop.Session.close s done; !best in
    let tn = time native and tl = time lisp in
    Printf.printf "bench domains %d: native %.3fs lisp %.3fs ratio %.2fx\n%!" domains tn tl (tl /. tn)) [1; 4])
