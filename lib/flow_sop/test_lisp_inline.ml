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
