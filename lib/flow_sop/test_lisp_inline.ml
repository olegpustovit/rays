(* Single-program design: a Lisp node call evaluates its body in the calling program, so chained
   nodes are one program for the region passes; the region count is printed for the step report. *)
open Rdk_test_support
module L = Flow_sop.Lower
module Edit = Sop.Edit_graph
module Session = Sop.Session

let flow_ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)

let chains =
  [
    ( "set_orient > color_by_height",
      "(sop/color_by_height (sop/set_orient (sop/grid :counts \"Point counts\" :connectivity \
       \"Points\" :columns 1025 :rows 2) :x 0.5))" );
    ( "color_by_height > color_by_height",
      "(sop/color_by_height (sop/color_by_height (sop/grid :counts \"Point counts\" :columns 65 \
       :rows 33)) :low_red 7)" );
  ]

let run body =
  let profiles = ref [] in
  let forms =
    Flow.Syntax.parse (Printf.sprintf "(workspace w (graph g :context sop %s))" body) |> flow_ok
  in
  let l =
    match L.workspace ~on_profile:(fun p -> profiles := p :: !profiles) forms with
    | Ok l -> l
    | Error d -> failwith (Diagnostic.to_string d)
  in
  let graph = List.hd l.graphs in
  let cook node =
    let session =
      Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
    in
    let context = Sop.Context.create ~domains:1 ~time:0. ~grain:257 () |> get_string_ok in
    let output = Session.cook session ~context node in
    Session.close session;
    match output with
    | Ok (o : Session.output) -> geometry_bytes (Result.get_ok (Sop.Payload.geometry o.payload))
    | Error d -> failwith (Diagnostic.error_to_string d)
  in
  let bytes =
    cook (get_string_ok (Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root)))
  in
  let runs =
    List.fold_left
      (fun n p -> n + List.length (Flow_ir.Profile.executions p))
      0 (l.profile :: !profiles)
  in
  (bytes, runs, cook (get_string_ok (Lisp_sop.root l graph)))

let () =
  List.iter
    (fun (name, body) ->
      let bytes, runs, factory = run body in
      Printf.printf "inline %s: region runs %d, bytes equal the factory node's\n" name runs;
      (* the chained bodies are one program with the grid's; the card's factory node (its own cook
         is one inlined call) gives the same bytes *)
      assert (bytes = factory))
    chains

(* Lisp cards lower inline unless they keep a node of their own (a structure that follows the
   frame): no plan node is a Lisp kind *)
let inlined (l : L.t) =
  not
    (Array.exists
       (fun (n : Flow.Eval.node) ->
         List.exists (fun f -> "sop/" ^ Edit.factory_key f = n.kind) l.factories)
       l.plan.nodes)

(* Step B: an inlined card keeps the node's boundaries. A body error is that card's cook error, a
   refused parameter (validation, normalizer) refuses the lowering, as a node build did; canonical
   parameters (hard ranges, encoded text, rest slots) give the same bytes. *)
let outcome ?(seed = 0L) ?(inline = ref true) text =
  let result =
    match L.workspace (Flow.Syntax.parse text |> flow_ok) with
    | Error d -> Error ("lower", d.code, d.message, d.cause)
    | Ok l -> (
        inline := inlined l;
        let graph = List.hd l.graphs in
        let live = Input.at_time 0.125 in
        let resolved =
          match
            Flow_sop.Value_lane.resolve ~live (Flow_sop.Value_lane.create ()) ~time:0.125
              graph.network
          with
          | Ok resolved -> resolved.geometry
          | Error d -> failwith (Diagnostic.to_string d)
        in
        let compiled =
          Edit.compile_node resolved ~node_id:(Option.get graph.root) |> get_string_ok
        in
        let session =
          Session.create ~max_entries:8 ~max_payload_bytes:(1 lsl 24) |> get_string_ok
        in
        let context =
          Sop.Context.create ~seed ~frame:3L ~time:0.125 ~input:live ~domains:1 () |> get_string_ok
        in
        let output = Session.cook session ~context compiled in
        Session.close session;
        match output with
        | Ok o -> Ok (geometry_bytes (Result.get_ok (Sop.Payload.geometry o.payload)))
        | Error d -> Error ("cook", d.code, d.message, d.cause))
  in
  result

let () =
  let normalized =
    {|(defnode sop/b_norm [(geo : geometry) (rules : text "point" :kind encoded :normalize (fn [(raw : text)] (case (text/lower (text/trim raw)) "points" "point" "point" "point" :else (error "E_RULES" (str "Unknown owner: " raw)))))] (sop/with_attr geo :tag (array/text (sop/count geo :points) rules) :type text))|}
  and rest =
    {|(defnode sop/b_rest :validate (> n 0) :code "E_N" :message "n must be positive" [(geos : geometry :rest true :required false) (n : int 1)] (if (> (count geos) 0) (last geos) (sop/line :kind "Points" :points n)))|}
  and valid =
    {|(defnode sop/b_valid :validate (> count 0) :code "E_POSITIVE" :message "count must be positive" [(count : int 1 :min -10 :max 10)] (sop/line :kind "Points" :points count))|}
  in
  List.iter
    (fun (expected, defnode, call) ->
      let text = Printf.sprintf "(workspace b %s (graph g :context sop %s))" defnode call in
      match (expected, outcome text) with
      | `Cook, Error ("cook", _, _, _) | `Lower, Error ("lower", _, _, _) | `Ok, Ok _ -> ()
      | _ -> failwith ("step B outcome changed: " ^ call))
    [
      ( `Cook,
        {|(defnode sop/b_fail [(geo : geometry)] (sop/with_attr geo :w (error "E_POINTS" "selected points" :cause "point cause")))|},
        "(sop/b_fail (sop/line :points 3))" );
      (`Lower, valid, "(sop/b_valid :count (- 0 3))");
      (`Ok, valid, "(sop/b_valid :count (+ 1 3))");
      ( `Ok,
        {|(defnode sop/b_range [(count : int 2 :min 1 :max 50 :hard-min 1 :hard-max 5)] (sop/line :kind "Points" :points count))|},
        "(sop/b_range :count (* 4 4))" );
      (`Ok, normalized, {|(sop/b_norm (sop/line :points 3) :rules (str " POINTS" ""))|});
      (`Lower, normalized, {|(sop/b_norm (sop/line :points 3) :rules (str "edges" ""))|});
      (`Ok, rest, "(sop/b_rest)");
      (`Ok, rest, "(sop/b_rest (sop/line :points 2) (sop/line :points 3))");
      (`Ok, rest, "(sop/b_rest :n (+ 2 2))");
      (`Lower, rest, "(sop/b_rest :n (- 2 2))");
    ];
  print_endline
    "inline cards: body errors cook, refusals lower, ranges/encoded text/rest slots as the node"

(* Step C: a body that reaches its cook context lowers as its own node, specialized per context
   (seeds from the card's label and the cook seed, frame, time, provenance base); the same card
   with the context branch untaken inlines. *)
let () =
  List.iter
    (fun (call, inlines) ->
      let text =
        Printf.sprintf
          "(workspace c (defnode sop/b_live [(geo : geometry) (on : bool false)] (if on \
           (sop/with_attr geo :P (map (fn [p] (+ p [1 0 0])) (sop/attr geo :P))) geo)) (graph g \
           :context sop %s))"
          call
      in
      let inline = ref true in
      if Result.is_error (outcome ~seed:2026L ~inline text) then
        failwith ("step C: " ^ call ^ " did not cook");
      if !inline <> inlines then failwith ("step C: " ^ call ^ " inlined wrongly"))
    [
      ("(sop/noise_displace (sop/line :points 9) :context_seed true :amplitude 0.5)", true);
      ("(sop/noise_displace (sop/line :points 9) :seed 4 :amplitude 0.5)", true);
      ("(sop/mountain (sop/line :points 9) :seed_mode \"Auto\")", true);
      ("(sop/mountain (sop/line :points 9))", true);
      ("(sop/attribute_fade (sop/line :points 9))", true);
      ("(sop/merge (sop/line :points 2) (sop/line :points 3))", true);
      (* live arguments are program inputs: per frame (a drive inside the body, a frame uniform)
         and cooked data; a structure that follows the frame keeps the card a node *)
      ("(sop/noise_displace (sop/line :points 9) :amplitude (* t 4.0))", true);
      ("(sop/attribute_fade (sop/line :points 9) :frame_offset (* t 8.0))", false);
      ("(sop/b_live (sop/line :points 9) :on (> t 0.1))", false);
    ];
  print_endline "inline cards: context-reaching bodies and live arguments inline"

(* Frame-driven arguments inline: a live vec3 group is its components, and a check of a live value
   (a validation, a test choosing an arm that fails) is made as the card cooks, with the
   diagnostic the written-out call refuses or fails with. Bytes equal the written-out call's. *)
let () =
  let text call =
    Printf.sprintf
      "(workspace f (graph g :context sop (sop/attribute_noise (sop/grid :rows 5 :columns 7) %s)))"
      call
  in
  List.iter
    (fun (live, written, inlines) ->
      let inline = ref false in
      let live_outcome = outcome ~inline (text live) in
      if !inline <> inlines then failwith ("frame arguments: " ^ live ^ " inlined wrongly");
      match (live_outcome, outcome (text written)) with
      | Ok a, Ok b when a = b -> ()
      | Error (_, _, a, _), Error (_, _, b, _) when a = b -> ()
      | a, b ->
          let show = function
            | Ok _ -> "ok"
            | Error (stage, code, message, _) -> stage ^ " " ^ code ^ " " ^ message
          in
          failwith
            ("frame arguments: " ^ live ^ " (" ^ show a ^ ") differs from " ^ written ^ " ("
           ^ show b ^ ")"))
    [
      (":offset [(* t 2.0) 0.5 (- 0.0 t)]", ":offset [0.25 0.5 -0.125]", true);
      ( ":frequency [1.0 (+ 1.0 t) 2.0] :name \"n\" :kind \"Vector\"",
        ":frequency [1.0 1.125 2.0] :name \"n\" :kind \"Vector\"",
        true );
      (":blend (* t 4.0)", ":blend 0.5", true);
      (* a validation and the body's own check, deferred to the cook *)
      (":blend (* t 16.0)", ":blend 2.0", true);
      ( ":max [(+ t 1.0) 1.0 1.0] :range \"Minimum / maximum\"",
        ":max [1.125 1.0 1.0] :range \"Minimum / maximum\"",
        true );
      (* a noise's lacunarity and gain are the region's uniforms: the card inlines *)
      ( ":lacunarity (+ t 1.0) :roughness (+ t 0.25) :octaves 3",
        ":lacunarity 1.125 :roughness 0.375 :octaves 3",
        true );
      (* and a live octave count is an Int uniform, the fold's bound *)
      (":octaves (int (+ 2.0 (* t 8.0)))", ":octaves 3", true);
      (* clamped to its hard minimum as the node's parameters clamp it (a literal 0 is refused by
         the checker) *)
      (":octaves (int (- 0.0 t))", ":octaves 1", true);
    ];
  (match outcome (text ":blend (* t 16.0)") with
  | Error ("cook", "E_LOWER", "sop/attribute_noise: blend must be in [0,1]", _) -> ()
  | _ -> failwith "frame arguments: a deferred validation does not fail the cook as refused");
  print_endline "inline cards: frame-driven vec3 groups and checks inline, failing as the call"

(* A driven transform inlines: its identity and singular tests are the write's data (a point keeps
   its P and N where they hold) and a singular matrix deletes N as the card cooks, unless nothing is
   selected. Bytes equal the written-out call's at t = 0.125, including an identity frame (translate
   t - 0.125) and singular ones (scale 0, with point or vertex N, an empty group). A group the cook
   decides, and recomputed normals, keep the card a node, as before; that node cooks without the N
   an owner lacks (it read every owner's N eagerly and failed). *)
let () =
  let text input call =
    Printf.sprintf "(workspace x (graph g :context sop (sop/transform %s %s)))" input call
  in
  let grid = "(sop/grid :rows 5 :columns 7)" in
  let vertex = "(sop/normals " ^ grid ^ " :owner \"Vertex\")"
  and range = "(sop/group_range " ^ grid ^ " :start 3 :end_ 12)"
  and empty = "(sop/group_range " ^ grid ^ " :range_mode \"Start and length\" :length 0)" in
  List.iter
    (fun (input, live, written, inlines) ->
      let inline = ref false in
      let live_outcome = outcome ~inline (text input live) in
      if !inline <> inlines then failwith ("driven transform: " ^ live ^ " inlined wrongly");
      match (live_outcome, outcome (text input written)) with
      | Ok a, Ok b when a = b -> ()
      | _ -> failwith ("driven transform: " ^ input ^ " " ^ live ^ " differs from " ^ written))
    [
      ( grid,
        ":shear_xy (* t 2.0) :translate [0.0 t 0.0]",
        ":shear_xy 0.25 :translate [0.0 0.125 0.0]",
        true );
      (grid, ":translate [(- t 0.125) 0.0 0.0]", "", true);
      (grid, ":scale [(- t 0.125) 1.0 1.0]", ":scale [0.0 1.0 1.0]", true);
      (vertex, ":shear_xz (* t 2.0)", ":shear_xz 0.25", true);
      (vertex, ":scale [(- t 0.125) 1.0 1.0]", ":scale [0.0 1.0 1.0]", true);
      (range, ":group \"range\" :shear_yz (* t 2.0)", ":group \"range\" :shear_yz 0.25", false);
      ( range,
        ":group \"range\" :scale [1.0 (- t 0.125) 1.0]",
        ":group \"range\" :scale [1.0 0.0 1.0]",
        false );
      ( empty,
        ":group \"range\" :scale [1.0 (- t 0.125) 1.0]",
        ":group \"range\" :scale [1.0 0.0 1.0]",
        false );
      ( grid,
        ":recompute_normals true :shear_xy (* t 2.0)",
        ":recompute_normals true :shear_xy 0.25",
        true );
    ];
  print_endline "inline cards: a driven transform's identity and singular frames as the call"

(* Recomputed normals are sop/normals' face-area bytes, gathered per point (or vertex), written
   where N was: quads, triangles, a sphere's pole fans, vertex N. *)
let () =
  List.iter
    (fun (input, owner) ->
      let moved = "(sop/transform " ^ input ^ " :shear_xy 0.25 :rotate [0.3 0.0 0.0])" in
      let recomputed =
        "(sop/transform " ^ input ^ " :shear_xy 0.25 :rotate [0.3 0.0 0.0] :recompute_normals true)"
      and scattered =
        Printf.sprintf
          "(sop/with_attr %s \"N\" (sop/attr (sop/normals %s :owner %S :weighting \"Face area\") \
           \"N\" :owner %S :type vec3) :owner %S :type vec3)"
          moved moved (String.capitalize_ascii owner) owner owner
      in
      let text body = Printf.sprintf "(workspace x (graph g :context sop %s))" body in
      match (outcome (text recomputed), outcome (text scattered)) with
      | Ok a, Ok b when a = b -> ()
      | _ -> failwith ("recomputed normals differ from sop/normals: " ^ input))
    [
      ("(sop/grid :rows 5 :columns 7)", "point");
      ("(sop/grid :rows 5 :columns 7 :connectivity \"Triangles\")", "point");
      ("(sop/normals (sop/uv_sphere) :owner \"Point\")", "point");
      ("(sop/normals (sop/grid :rows 5 :columns 7) :owner \"Vertex\")", "vertex");
    ];
  print_endline "inline cards: recomputed normals are sop/normals' face-area bytes, in place";
  (* dev's recompute, written out: sop/normals on the point N (which deletes every other owner's
     N), else on the vertex N; with point N last the in-place write is its attribute order too *)
  let dev =
    "(let* [m (sop/transform g :shear_xy 0.25) p (if (sop/has_attr m \"N\" :owner \"point\") \
     (sop/normals m :owner \"Point\" :weighting \"Face area\") m)] (if (sop/has_attr p \"N\" \
     :owner \"vertex\") (sop/normals p :owner \"Vertex\" :weighting \"Face area\") p))"
  and grid = "(sop/grid :rows 5 :columns 7)" in
  let point_last b =
    Printf.sprintf
      "b %s g (sop/with_attr b \"N\" (array/vec3 (sop/count b :points) [0.0 1.0 0.0]) :owner \
       \"point\" :type vec3)"
      b
  in
  List.iter
    (fun (name, bindings) ->
      let text body =
        Printf.sprintf "(workspace x (graph g :context sop (let* [%s] %s)))" bindings body
      in
      match
        ( outcome (text "(sop/transform g :shear_xy 0.25 :recompute_normals true)"),
          outcome (text dev) )
      with
      | Ok a, Ok b when a = b -> ()
      | Error a, Error b when a = b -> ()
      | _ -> failwith ("recomputed normals differ from dev's: " ^ name))
    [
      ("point and vertex N", point_last ("(sop/normals " ^ grid ^ " :owner \"Vertex\")"));
      ("point and primitive N", point_last ("(sop/normals " ^ grid ^ " :owner \"Primitive\")"));
      ("point and detail N", point_last ("(sop/normals " ^ grid ^ " :owner \"Detail\")"));
      ( "float point N",
        Printf.sprintf
          "b (sop/delete_attr %s \"N\" :owner \"point\") g (sop/with_attr b \"N\" (array/float \
           (sop/count b :points) 1.0) :owner \"point\" :type float)"
          grid );
    ];
  print_endline "inline cards: recomputed normals keep dev's owners and storage errors"

(* Step E2: a cooked-data argument (a count of the cooked input) inlines; the body reads it as a
   value resolved once per cook, so the card cooks as the same call with that value written out
   (nested refused it, E_DATA_SOURCE), in time linear in the points. *)
let () =
  let text amplitude points =
    Printf.sprintf
      "(workspace e (graph g :context sop (let* [g (sop/line :points %d)] (sop/noise_displace g \
       :amplitude %s))))"
      points amplitude
  in
  let timed points =
    let start = Unix.gettimeofday () in
    let result = outcome (text "(/ (float (sop/count g :points)) 4096.0)" points) in
    (result, Unix.gettimeofday () -. start)
  in
  List.iter
    (fun points ->
      let inline, _ = timed points in
      let written = outcome (text (Printf.sprintf "%.17g" (float points /. 4096.)) points) in
      if inline <> written || Result.is_error inline then
        failwith (Printf.sprintf "step E2: cooked argument differs at %d points" points))
    [ 9; 4096 ];
  let _, small = timed 20_000 and _, large = timed 80_000 in
  if large > (12. *. small) +. 0.05 then
    failwith (Printf.sprintf "step E2: 4x points took %.3fs vs %.3fs" large small);
  Printf.printf "inline cards: cooked-data arguments resolve once per cook (20k %.3fs, 80k %.3fs)\n"
    small large
