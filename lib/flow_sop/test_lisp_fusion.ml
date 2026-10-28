(* B.F: a chain of Lisp-bodied point deformers cooks as one kernel run, with the bytes of the
   chain run one deformer at a time. *)
open Rdk
open Rdk_test_support
module L = Flow_sop.Lower
module E = Flow.Eval
module Edit = Sop.Edit_graph
module Session = Sop.Session

let flow_ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let columns = 1025

let grid =
  Printf.sprintf "(sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns %d :rows 2)"
    columns

let chain_text stages =
  Printf.sprintf "(workspace fused (graph g :context sop (let* [g %s %s] %s)))" grid
    (String.concat " "
       (List.mapi
          (fun i (amplitude, frequency, seed) ->
            Printf.sprintf "d%d (sop/noise_displace %s :amplitude %g :frequency %g :seed %d)" (i + 1)
              (if i = 0 then "g" else Printf.sprintf "d%d" i)
              amplitude frequency seed)
          stages))
    (Printf.sprintf "d%d" (List.length stages))

let profiles : Flow_ir.Profile.t list ref = ref []

let lower ?previous text =
  let forms = Flow.Syntax.parse text |> flow_ok in
  let compiled_ids = Option.map (fun (l : L.t) -> l.compiled_ids) previous
  and sites = Option.map (fun (l : L.t) -> l.sites) previous in
  match
    L.workspace ~on_profile:(fun p -> profiles := p :: !profiles) ?compiled_ids ?sites forms
  with
  | Ok lowered -> lowered
  | Error d -> failwith (text ^ "\n" ^ Diagnostic.to_string d)

let compiled (l : L.t) =
  let graph = List.hd l.graphs in
  Edit.compile_node graph.network.geometry ~node_id:(Option.get graph.root) |> get_string_ok

let context ~domains = Sop.Context.create ~domains ~time:0. ~grain:257 () |> get_string_ok

let geometry = function
  | Ok (output : Session.output) -> Result.get_ok (Sop.Payload.geometry output.payload)
  | Error d -> failwith (Diagnostic.error_to_string d)

let kernel_runs (_ : L.t) =
  List.fold_left
    (fun count profile ->
      count
      + List.length
          (List.filter
             (fun (e : Flow_ir.execution) -> e.tier = Cpu_kernel)
             (Flow_ir.Profile.executions profile)))
    0 !profiles

let stages = [ (0.3, 0.7, 0); (-0.2, 1.3, 3); (0.15, 2.1, 5); (0.05, 0.4, 7) ]

let () =
  let session =
    Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
  in
  Fun.protect
    ~finally:(fun () -> Session.close session)
    (fun () ->
      let base_l = lower (Printf.sprintf "(workspace b (graph g :context sop %s))" grid) in
      let base = Session.cook session ~context:(context ~domains:1) (compiled base_l) |> geometry in
      let cook l domains =
        Session.cook session ~context:(context ~domains) (compiled l) |> geometry
      in
      let run stages =
        let reference = ref "" in
        List.iter
          (fun domains ->
            profiles := [];
            let l = lower (chain_text stages) in
            let fresh =
              Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
            in
            let output = Session.cook fresh ~context:(context ~domains) (compiled l) |> geometry in
            Session.close fresh;
            if domains = 1 then reference := geometry_bytes output
            else assert (geometry_bytes output = !reference);
            (* the grid is not a kernel run; the four deformers are at most one (inlined into the
               grid's program they fold at lowering) *)
            assert (kernel_runs l <= 1))
          [ 1; 4 ];
        lower (chain_text stages)
      in
      let l = run stages in
      (* editing one node's parameter recooks the whole fused chain *)
      let edited =
        List.mapi (fun i (a, f, s) -> if i = 1 then (a *. 2., f, s) else (a, f, s)) stages
      in
      let l' = lower ~previous:l (chain_text edited) in
      let first = geometry_bytes (cook l 1) in
      let second = geometry_bytes (cook l' 1) in
      assert (first <> second);
      ignore base);
  (* an intermediate with a second reader is not folded away: it still cooks for that reader *)
  profiles := [];
  let text = chain_text stages in
  let split = lower (String.sub text 0 (String.length text - 5) ^ "(sop/merge d4 d2))))") in
  let session =
    Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
  in
  ignore (Session.cook session ~context:(context ~domains:1) (compiled split) |> geometry);
  Session.close session;
  assert (kernel_runs split <= 2);
  print_endline
    "Lisp fusion: four noise_displace nodes cook as at most one kernel run, equal at one and four \
     domains"

(* E4: the chain over a cooked (native) input: each card's body writes P over the previous card,
   through its N deletions; the writes fold into one region, with the bytes of the unfolded
   (reference) lowering at one and four domains, and the intermediate cards keep their own. *)
let () =
  let base = Printf.sprintf "(sop/soft_transform %s)" grid in
  let text =
    Printf.sprintf "(workspace cooked (graph g :context sop (let* [g %s %s] d4)))" base
      (String.concat " "
         (List.mapi
            (fun i (amplitude, frequency, seed) ->
              Printf.sprintf "d%d (sop/noise_displace %s :amplitude %g :frequency %g :seed %d)"
                (i + 1)
                (if i = 0 then "g" else Printf.sprintf "d%d" i)
                amplitude frequency seed)
            stages))
  in
  let cook_root ?(reference = false) domains =
    profiles := [];
    let l =
      L.workspace ~reference
        ~on_profile:(fun p -> profiles := p :: !profiles)
        (Flow.Syntax.parse text |> flow_ok)
      |> flow_ok
    in
    let session =
      Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
    in
    let bytes = Session.cook session ~context:(context ~domains) (compiled l) |> geometry in
    Session.close session;
    (geometry_bytes bytes, kernel_runs l, l)
  in
  let expected, _, _ = cook_root ~reference:true 1 in
  List.iter
    (fun domains ->
      let bytes, runs, l = cook_root domains in
      assert (bytes = expected);
      assert (runs <= 1);
      (* the second card still cooks as two displacements *)
      if domains = 1 then begin
        let graph = List.hd l.graphs in
        let d2 =
          Array.to_list l.plan.nodes
          |> List.filter (fun (n : E.node) ->
              n.kind = E.card_kind
              && List.assoc_opt "kind" n.args = Some (E.Text "sop/noise_displace"))
          |> fun cards -> List.nth cards 1
        in
        let node =
          Edit.compile_node graph.network.geometry
            ~node_id:(Flow_sop.Network.Int_map.find d2.id l.compiled)
          |> get_string_ok
        in
        let session =
          Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
        in
        let two = Session.cook session ~context:(context ~domains) node |> geometry in
        Session.close session;
        let reference_text =
          Printf.sprintf "(workspace two (graph g :context sop (let* [g %s %s] d2)))" base
            (String.concat " "
               (List.filteri
                  (fun i _ -> i < 2)
                  (List.mapi
                     (fun i (amplitude, frequency, seed) ->
                       Printf.sprintf
                         "d%d (sop/noise_displace %s :amplitude %g :frequency %g :seed %d)" (i + 1)
                         (if i = 0 then "g" else Printf.sprintf "d%d" i)
                         amplitude frequency seed)
                     stages)))
        in
        let r = lower reference_text in
        let session =
          Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
        in
        let expected_two =
          Session.cook session ~context:(context ~domains) (compiled r) |> geometry
        in
        Session.close session;
        assert (geometry_bytes two = geometry_bytes expected_two)
      end)
    [ 1; 4 ];
  print_endline
    "Lisp fusion: four noise_displace cards over a cooked input fold into one region, as the \
     unfolded bytes; an intermediate card cooks as itself"

(* writes of different attributes fuse: one record program writes Cd, then P (over a transform
   card), with the unfolded bytes *)
let () =
  let text =
    Printf.sprintf
      "(workspace mixed (graph g :context sop (let* [g (sop/soft_transform %s) c \
       (sop/color_by_height g) d (sop/noise_displace c :amplitude 0.3 :frequency 0.7) tr \
       (sop/transform d :translate [0.5 0 0] :rotate [0 0.3 0])] tr)))"
      grid
  in
  let cook_root ?(reference = false) domains =
    profiles := [];
    let l =
      L.workspace ~reference
        ~on_profile:(fun p -> profiles := p :: !profiles)
        (Flow.Syntax.parse text |> flow_ok)
      |> flow_ok
    in
    let session =
      Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
    in
    let root = compiled l in
    let bytes = Session.cook session ~context:(context ~domains) root |> geometry in
    Session.close session;
    (* the writes the root cooks: Cd, P and P fold into one, before the two guarded N writes *)
    let writes =
      List.length
        (List.filter
           (fun (n : Sop.Node.info) -> n.operation = "flow.with_attr")
           (Sop.Node.inspect root))
    in
    (geometry_bytes bytes, writes)
  in
  let expected, unfolded = cook_root ~reference:true 1 in
  List.iter
    (fun domains ->
      let bytes, writes = cook_root domains in
      assert (bytes = expected);
      if writes <> 3 then
        failwith (Printf.sprintf "mixed chain: %d writes cooked, %d unfolded" writes unfolded))
    [ 1; 4 ];
  print_endline
    "Lisp fusion: color_by_height > noise_displace > transform write Cd and P in one region, as \
     the unfolded bytes"

(* a map that reads the chain inside its term (no let* binding) folds too: its read of the
   written Cd takes the values, its read of P the input *)
let () =
  let text =
    Printf.sprintf
      "(workspace lazy (graph g :context sop (let* [g (sop/soft_transform %s) c \
       (sop/color_by_height g)] (sop/with_attr c :P (exact (map (fn [(p : vec3) (k : vec4)] (+ p \
       [k.x k.y 0.0])) (sop/attr c :P) (sop/attr c \"Cd\" :type vec4)))))))"
      grid
  in
  let cook ~reference =
    let l = L.workspace ~reference (Flow.Syntax.parse text |> flow_ok) |> flow_ok in
    let session =
      Session.create ~max_entries:32 ~max_payload_bytes:(64 * 1024 * 1024) |> get_string_ok
    in
    let root = compiled l in
    let bytes = Session.cook session ~context:(context ~domains:4) root |> geometry in
    Session.close session;
    ( geometry_bytes bytes,
      List.length
        (List.filter
           (fun (n : Sop.Node.info) -> n.operation = "flow.with_attr")
           (Sop.Node.inspect root)) )
  in
  let expected, unfolded = cook ~reference:true and bytes, writes = cook ~reference:false in
  assert (bytes = expected);
  if writes <> 1 then failwith (Printf.sprintf "lazy reads: %d writes, %d unfolded" writes unfolded);
  print_endline "Lisp fusion: a map reading the chain in its term folds, as the unfolded bytes"
