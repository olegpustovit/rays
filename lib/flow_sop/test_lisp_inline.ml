(* Single-program design, step A: a Lisp node call evaluates its body in the calling program
   ([Flow.Eval.inline_nodes]), so chained nodes are one program for the region passes. The bytes equal
   the nested lowering's; the region count is printed for the step report. *)
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

let run inline body =
  Atomic.set Flow.Eval.inline_nodes inline;
  let profiles = ref [] in
  let before = Atomic.get L.Private.nested_lowerings in
  let forms =
    Flow.Syntax.parse (Printf.sprintf "(workspace w (graph g :context sop %s))" body) |> flow_ok
  in
  let l =
    match L.workspace ~on_profile:(fun p -> profiles := p :: !profiles) forms with
    | Ok l -> l
    | Error d -> failwith (Diagnostic.to_string d)
  in
  let graph = List.hd l.graphs in
  let compiled = Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) in
  let session =
    Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
  in
  let context = Sop.Context.create ~domains:1 ~time:0. ~grain:257 () |> get_string_ok in
  let output = Session.cook session ~context (get_string_ok compiled) in
  Session.close session;
  let bytes =
    match output with
    | Ok (o : Session.output) -> geometry_bytes (Result.get_ok (Sop.Payload.geometry o.payload))
    | Error d -> failwith (Diagnostic.error_to_string d)
  in
  let runs =
    List.fold_left
      (fun n p -> n + List.length (Flow_ir.Profile.executions p))
      0 (l.profile :: !profiles)
  in
  (bytes, runs, Atomic.get L.Private.nested_lowerings - before)

let () =
  List.iter
    (fun (name, body) ->
      let nested_bytes, nested_runs, nested = run false body in
      let inline_bytes, inline_runs, inlined = run true body in
      Printf.printf "inline %s: region runs %d -> %d, nested lowerings %d -> %d, bytes equal\n" name
        nested_runs inline_runs nested inlined;
      (* the chained bodies are one program with the grid's (its validations inline too): fewer
         region runs, no nested lowering *)
      assert (nested = 3 && inlined = 0 && inline_runs < nested_runs))
    chains;
  Atomic.set Flow.Eval.inline_nodes false

(* Step B: an inlined card keeps the node's boundaries. A body error is that card's cook error, a
   refused parameter (validation, normalizer) refuses the lowering, as a node build did; canonical
   parameters (hard ranges, encoded text, rest slots) give the same bytes. *)
let outcome ?(seed = 0L) inline text =
  Atomic.set Flow.Eval.inline_nodes inline;
  let result =
    match L.workspace (Flow.Syntax.parse text |> flow_ok) with
    | Error d -> Error ("lower", d.code, d.message, d.cause)
    | Ok l -> (
        let graph = List.hd l.graphs in
        let compiled =
          Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok
        in
        let session =
          Session.create ~max_entries:8 ~max_payload_bytes:(1 lsl 24) |> get_string_ok
        in
        let context =
          Sop.Context.create ~seed ~frame:3L ~time:0.125 ~domains:1 () |> get_string_ok
        in
        let output = Session.cook session ~context compiled in
        Session.close session;
        match output with
        | Ok o -> Ok (geometry_bytes (Result.get_ok (Sop.Payload.geometry o.payload)))
        | Error d -> Error ("cook", d.code, d.message, d.cause))
  in
  Atomic.set Flow.Eval.inline_nodes false;
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
      let nested = outcome false text and inline = outcome true text in
      (match (expected, nested) with
      | `Cook, Error ("cook", _, _, _) | `Lower, Error ("lower", _, _, _) | `Ok, Ok _ -> ()
      | _ -> failwith ("step B baseline changed: " ^ call));
      if nested <> inline then failwith ("step B: inline differs from nested for " ^ call))
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
      let text = Printf.sprintf "(workspace c (graph g :context sop %s))" call in
      let nested = outcome ~seed:2026L false text in
      let before = Atomic.get L.Private.nested_lowerings in
      let inline = outcome ~seed:2026L true text in
      let lowered = Atomic.get L.Private.nested_lowerings - before in
      if nested <> inline then failwith ("step C: inline differs from nested for " ^ call);
      if Result.is_error nested then failwith ("step C: " ^ call ^ " did not cook");
      if lowered = 0 <> inlines then failwith ("step C: " ^ call ^ " inlined wrongly"))
    [
      ("(sop/noise_displace (sop/line :points 9) :context_seed true :amplitude 0.5)", false);
      ("(sop/noise_displace (sop/line :points 9) :seed 4 :amplitude 0.5)", true);
      ("(sop/mountain (sop/line :points 9) :seed_mode \"Auto\")", false);
      ("(sop/mountain (sop/line :points 9))", true);
      ("(sop/attribute_fade (sop/line :points 9))", false);
      ("(sop/merge (sop/line :points 2) (sop/line :points 3))", false);
    ];
  print_endline "inline cards: context-reaching bodies lower per context, the rest inline"
