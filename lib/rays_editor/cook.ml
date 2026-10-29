open Rays
open Sop
open Editor_document

type bounds = Rays_math.Vec3.t * Rays_math.Vec3.t

(* One geometry object's latest cook: its displayed SOP graph, the sketch's
   prepared value, and bounds in the object's own space. *)
type prepared = { geometry : Rdk.Geometry.t; surface : Rdk_rays.Surface.t }

type piece = {
  id : int;
  graph : Node.t;
  prepared : prepared;
  bounds : bounds option Lazy.t;
      (* read on the initial domain when asked (framing, the selected box): a device's P stays
         there until then *)
  settings : Settings.t;
  context : string; (* projection of the graph's declared context dependencies *)
  output : Session.output; (* what [prepared] was made from, before the tint *)
  lit : Pick.Set.t; (* the provenance tags [prepared] highlights, see {!Pick} *)
  surface : Rdk_rays.Geometry_view.surface option Lazy.t; (* built by the first click *)
}

(* What a footer shows of a geometry value: its object and compiled node. *)
type summary = (int * int) * Flow_graph.Probe.geometry

type cooked =
  | Displayed of piece list * summary list * ((int * int) * Rdk_rays.Geometry_view.t) list
  | Probed of piece list * summary list * ((int * int) * Rdk_rays.Geometry_view.t) list
    (* the footers' counts alone: the displayed objects are cache hits *)
  | Framed of bounds option

type t = {
  worker : cooked Async_cook.t;
  seed : int64;
  grain : int;
  domains : int;
  await : bool; (* block on each cook: fixed-step runs, see [create] *)
  schedule : Schedule.t;
  prepare : Settings.t -> Session.output -> (prepared, string) result;
  pieces : piece list;
  settings : Settings.t option; (* the latest desired submission settings *)
  error : string option;
  failure : (string * int) option ref;
      (* the code and compiled node id of the node the last failed cook names (read while [error] is set) *)
  seconds : float option;
  (* A framing job can supersede a display cook that must be resubmitted. *)
  framing : bool option;
  force : bool;
  (* Each object's last compiled network, reused node-by-node on the next
     edit, and this frame's displayed graphs. *)
  compiled : (Edit_graph.t * Edit_graph.compiled) Document.Int_map.t;
  value_lanes : Flow_sop.Value_lane.t Document.Int_map.t;
  state : Flow.Eval.state;
  applied : Flow_sop.Value_lane.resolved Document.Int_map.t;
  graphs : (int * Node.t) list;
  displayed : (int * int) list; (* the display node each graph compiles *)
  probe_job : bool; (* the job in flight only counts footers: a display cook may supersede it *)
  probing : (int * int) list; (* the (object, node) pairs the last submission asked to summarise *)
  summaries : summary list;
  sources : ((int * int) * Rdk_rays.Geometry_view.t) list;
      (* same bounded optional cook targets as summaries *)
}

type update = {
  cook : t;
  edit_error : string option;
  prepared_changed : bool;
  framed : bounds option option;
}

let geometry_bounds = Rdk_rays.Geometry_view.bounds

(* The eight corners of a box under a transform: the one walk for every box taken to another
   space (an instance's, the world's, the screen's). *)
let iter_corners matrix ((lo, hi) : bounds) f =
  for corner = 0 to 7 do
    f
      (Rays_math.Mat4.transform_point matrix
         (Rays_math.Vec3.create
            (if corner land 1 = 0 then lo.Rays_math.Vec3.x else hi.Rays_math.Vec3.x)
            (if corner land 2 = 0 then lo.y else hi.y)
            (if corner land 4 = 0 then lo.z else hi.z)))
  done

let union ((a, b) : bounds) ((c, d) : bounds) : bounds =
  ( Rays_math.Vec3.create (Float.min a.x c.x) (Float.min a.y c.y) (Float.min a.z c.z),
    Rays_math.Vec3.create (Float.max b.x d.x) (Float.max b.y d.y) (Float.max b.z d.z) )

(* The box around a box under each of [matrices]. *)
let transformed_all matrices bounds : bounds =
  let lower = ref (Rays_math.Vec3.create infinity infinity infinity)
  and upper = ref (Rays_math.Vec3.create neg_infinity neg_infinity neg_infinity) in
  Array.iter
    (fun matrix ->
      iter_corners matrix bounds (fun (p : Rays_math.Vec3.t) ->
          lower :=
            Rays_math.Vec3.create (Float.min !lower.x p.x) (Float.min !lower.y p.y)
              (Float.min !lower.z p.z);
          upper :=
            Rays_math.Vec3.create (Float.max !upper.x p.x) (Float.max !upper.y p.y)
              (Float.max !upper.z p.z)))
    matrices;
  (!lower, !upper)

let transformed matrix bounds = transformed_all [| matrix |] bounds

(* A packed output's bounds: the prototype's box at every instance. *)
let output_bounds (output : Session.output) =
  Result.map
    (fun geometry ->
      match (geometry_bounds geometry, output.instances) with
      | (None, _ | _, (None | Some [||])) as bounds -> fst bounds
      | Some box, Some transforms -> Some (transformed_all transforms box))
    (Payload.geometry output.payload)

(* [await]: a fixed-step run (export, a finite --max-frames run, tests) must show the
   geometry of exactly frame n, so [update] waits for the cook it submitted;
   interactive play never blocks (latest request wins). The default is false;
   [Frame.t] does not say the clock is fixed, so
   an export host passes [~await:true]. *)
let create ~prepare ~seed ~grain ?domains ?await ?clock ~max_entries ~max_payload_bytes () =
  let await = match await with Some flag -> flag | None -> false in
  if grain <= 0 then invalid_arg "Rays_editor: grain must be positive";
  let domains = Option.value ~default:(max 1 (Parallel.recommended_domains () - 1)) domains in
  if domains <= 0 then invalid_arg "Rays_editor: domains must be positive";
  Result.map
    (fun worker ->
      Option.iter (Async_cook.set_clock worker) clock;
      {
        worker;
        seed;
        grain;
        domains;
        await;
        prepare;
        schedule = Schedule.initial;
        pieces = [];
        settings = None;
        error = None;
        failure = ref None;
        seconds = None;
        framing = None;
        force = false;
        compiled = Document.Int_map.empty;
        graphs = [];
        value_lanes = Document.Int_map.empty;
        state = Flow.Eval.create_state ();
        applied = Document.Int_map.empty;
        displayed = [];
        probe_job = false;
        probing = [];
        summaries = [];
        sources = [];
      })
    (Async_cook.create ~max_entries ~max_payload_bytes)

(* the failing node of a cook error: the innermost entry of its trace *)
let failure_of = function
  | Async_cook.Cook_error { code; trace; _ } when trace <> [] ->
      Some (code, (List.nth trace (List.length trace - 1)).Diagnostic.node_id)
  | _ -> None

let failed_node value = if value.error = None then None else !(value.failure)
let status value = Async_cook.status value.worker

(* [Lower.is_volatile] of the current lowering: the nodes that recook per frame. *)
let set_volatile value predicate = Async_cook.set_volatile value.worker predicate
let stats value = Async_cook.stats value.worker
let pieces value = value.pieces

(* the counts of a compiled node of an object, once a cook has reported them *)
let geometry value ~object_id ~node_id = List.assoc_opt (object_id, node_id) value.summaries
let source value ~object_id ~node_id = List.assoc_opt (object_id, node_id) value.sources
let applied value id = Document.Int_map.find_opt id value.applied

let reset_state ?(host_state = true) value =
  Flow.Eval.reset_state ~host_state value.state;
  Document.Int_map.iter (fun _ lane -> Flow_sop.Value_lane.reset ~host_state lane) value.value_lanes

let context value timeline frame =
  Context.create ~seed:value.seed ~grain:value.grain ~domains:value.domains
    ~frame:(Timeline.frame timeline) ~time:(Timeline.time timeline)
    ~input:(Flow_native.Live_frame.of_frame frame)
    ()

let busy value = match status value with Async_cook.Idle -> false | Cooking _ -> true
let force value = { value with force = true }

(* [objects] are the visible geometry objects as (id, network graph,
   display node); [frame_request] is (object, node) to frame. *)
let update ?gpu ?input ?live ?(probes = []) ?(lit = Pick.Set.empty)
    ?(pump = fun () -> Unix.sleepf 0.0005) value ~settings ~objects ~edit_error ~effects
    ~timeline_changes ~timeline ~frame ~frame_request =
  Flow.Phase_timer.measure "cook" (fun () ->
      if
        List.exists
          (function Timeline.Reset_now | Stopped_now -> true | _ -> false)
          timeline_changes
      then reset_state ~host_state:false value;
      let input =
        match input with Some input -> input | None -> Flow_native.Live_frame.of_frame frame
      in
      let input =
        { input with t = Timeline.time timeline; frame = Int64.to_int (Timeline.frame timeline) }
      in
      let objects, value_lanes, applied, resolve_error =
        List.fold_left
          (fun (objects, lanes, applied, error) (id, (network : Flow_sop.Network.t), displayed) ->
            let lane =
              match Document.Int_map.find_opt id value.value_lanes with
              | Some lane -> lane
              | None -> Flow_sop.Value_lane.create ~state:value.state ()
            in
            let lanes = Document.Int_map.add id lane lanes in
            match
              Flow_sop.Value_lane.resolve ?gpu lane ~live:input ~time:(Timeline.time timeline)
                network
            with
            | Ok resolved ->
                ( (id, resolved.geometry, displayed) :: objects,
                  lanes,
                  Document.Int_map.add id resolved applied,
                  error )
            | Error diagnostic ->
                let previous = Document.Int_map.find_opt id value.applied in
                let objects =
                  match previous with
                  | Some previous -> (id, previous.geometry, displayed) :: objects
                  | None -> objects
                in
                let applied =
                  match previous with
                  | Some previous -> Document.Int_map.add id previous applied
                  | None -> applied
                in
                (objects, lanes, applied, Some (Diagnostic.to_string diagnostic)))
          ([], Document.Int_map.empty, Document.Int_map.empty, None)
          objects
      in
      let objects = List.rev objects in
      let edit_error = match edit_error with Some _ -> edit_error | None -> resolve_error in
      let compiled =
        List.fold_left
          (fun compiled (id, document, _) ->
            match Document.Int_map.find_opt id value.compiled with
            | Some (source, _) when source == document -> compiled
            | previous ->
                Document.Int_map.add id
                  ( document,
                    Edit_graph.compile_all
                      ?previous:(Option.map (fun (_, c) -> c) previous)
                      document )
                  compiled)
          value.compiled objects
      in
      let compiled =
        Document.Int_map.filter
          (fun id _ -> List.exists (fun (object_id, _, _) -> object_id = id) objects)
          compiled
      in
      (* An unchanged network and display node keep their graph physically, so
     an idle frame never looks like an edit and never resubmits a cook. *)
      let graphs, edit_error =
        List.fold_left
          (fun (graphs, error) (id, document, displayed) ->
            let unchanged =
              match Document.Int_map.find_opt id value.compiled with
              | Some (source, _) -> source == document
              | None -> false
            in
            match (List.assoc_opt id value.graphs, List.assoc_opt id value.displayed) with
            | Some graph, Some previous when unchanged && previous = displayed ->
                ((id, graph) :: graphs, error)
            | previous, _ -> (
                match
                  Edit_graph.compiled_node
                    (snd (Document.Int_map.find id compiled))
                    ~node_id:displayed
                with
                | Ok graph -> ((id, graph) :: graphs, error)
                | Error message ->
                    ( (match previous with Some graph -> (id, graph) :: graphs | None -> graphs),
                      Some message )))
          ([], edit_error) objects
      in
      let displayed = List.map (fun (id, _, displayed) -> (id, displayed)) objects in
      let graphs = List.rev graphs in
      let changed = not (List.equal (fun (a, g) (b, h) -> a = b && g == h) graphs value.graphs) in
      let completion = Async_cook.poll value.worker in
      let resume = value.framing = Some true in
      let summaries = ref value.summaries in
      let sources = ref value.sources in
      let pieces, error, seconds, prepared_changed, framed, framing, force_next =
        match completion with
        | None -> (value.pieces, value.error, value.seconds, false, None, value.framing, false)
        | Some { Async_cook.result = Ok (Displayed (pieces, found, data)); seconds; _ } ->
            (* a display cook that counted nothing keeps the counts it has *)
            if found <> [] then (
              summaries := found;
              sources := data);
            (pieces, None, Some seconds, true, None, None, false)
        | Some { result = Ok (Probed (pieces, found, data)); _ } ->
            summaries := found;
            sources := data;
            (pieces, value.error, value.seconds, true, None, None, false)
        | Some { result = Ok (Framed bounds); _ } ->
            (value.pieces, value.error, value.seconds, false, Some bounds, None, resume)
        | Some { result = Error _; _ } when value.framing <> None ->
            (value.pieces, value.error, value.seconds, false, Some None, None, resume)
        | Some { result = Error error; seconds; _ } ->
            value.failure := failure_of error;
            ( value.pieces,
              Some (Async_cook.error_to_string error),
              Some seconds,
              false,
              None,
              None,
              false )
      in
      (* Empty/removed objects have no current preview, including when a job
     from before the deletion completes. Invalid nonempty SOPs keep their
     last successful graph and preview through the compilation fallback. *)
      let pieces, prepared_changed =
        if
          (changed || Option.is_some completion)
          && List.exists (fun piece -> not (Document.Int_map.mem piece.id compiled)) pieces
        then (List.filter (fun piece -> Document.Int_map.mem piece.id compiled) pieces, true)
        else (pieces, prepared_changed)
      in
      let schedule, submit =
        Schedule.step ?live value.schedule ~graphs:(List.map snd graphs) ~effects
          ~context_changed:(Timeline.changed_context timeline_changes)
          ~force:
            (changed || value.force
            || (value.await && probes <> value.probing)
            || not (Option.fold ~none:false ~some:(( == ) settings) value.settings))
          ~busy:((busy value && not value.probe_job) || framing <> None)
          ~frame
      in
      (* Unchanged objects keep their prepared value: the worker only cooks
     (a session cache hit) and re-prepares what changed. *)
      let previous = pieces in
      (* the probed nodes are cooked after the displayed ones, so an upstream one is a cache hit (or a
     volatile slot); one that is not upstream of the display is cooked too, and either may fail
     without failing the display *)
      let node_of (object_id, node_id) =
        match
          Option.bind (Document.Int_map.find_opt object_id compiled) (fun (_, c) ->
              Result.to_option (Edit_graph.compiled_node c ~node_id))
        with
        | Some _ as node -> node
        | None ->
            Option.bind (List.assoc_opt object_id graphs) (fun graph -> Node.find graph ~node_id)
      in
      (* Footers are counted after the display cook, not with it: a changed graph is displayed
         first, and the probe job follows when the worker is idle.  A live recook carries the
         probes only when the visible set changed. *)
      let split = not value.await in
      let inline = submit && ((not split) || ((not changed) && probes <> value.probing)) in
      let probe_only =
        split && (not submit) && probes <> [] && probes <> value.probing && framing = None
        && ((not (busy value)) || value.probe_job)
      in
      let probed =
        if not (inline || probe_only) then []
        else
          List.filter_map
            (fun key -> Option.map (fun node -> (key, node)) (node_of key))
            (List.take 64 probes)
      in
      let summary (key, node) (output : Session.output) =
        let g = Result.get_ok (Payload.geometry output.payload) in
        let v = Rdk_rays.Geometry_view.summary g in
        ( key,
          {
            Flow_graph.Probe.seconds = Async_cook.node_seconds value.worker (Node.id node);
            points = v.points;
            prims = v.prims;
            extent =
              Option.map
                (fun (lo, hi) ->
                  (hi.Rays_math.Vec3.x -. lo.Rays_math.Vec3.x, hi.y -. lo.y, hi.z -. lo.z))
                (geometry_bounds g);
            groups = v.groups;
            attributes = v.attributes;
          } )
      in
      let prepare context outputs optional =
        let found =
          List.filter_map Fun.id
            (List.map2
               (fun target output ->
                 Option.bind output (fun (output : Session.output) ->
                     match output.payload with
                     | Payload.Geometry _ -> Some (summary target output)
                     | Image _ | Kernel _ -> None))
               probed optional)
        in
        let data =
          List.filter_map Fun.id
            (List.map2
               (fun (key, _) output ->
                 Option.bind output (fun (output : Session.output) ->
                     Option.map
                       (fun geometry -> (key, geometry))
                       (Result.to_option (Payload.geometry output.payload))))
               probed optional)
        in
        let rec loop reversed graphs outputs =
          match (graphs, outputs) with
          | [], [] ->
              let pieces = List.rev reversed in
              Ok
                (if probe_only then Probed (pieces, found, data) else Displayed (pieces, found, data))
          | (id, graph) :: graphs, (output : Session.output) :: outputs -> (
              let projection = Context.cache_projection (Node.graph_dependencies graph) context in
              let reused =
                List.find_opt
                  (fun piece ->
                    piece.id = id && piece.graph == graph && piece.settings == settings
                    && piece.context = projection && Pick.same_set piece.lit lit)
                  previous
              in
              match reused with
              | Some piece -> loop (piece :: reversed) graphs outputs
              | None ->
                  let ( let* ) = Result.bind in
                  let* geometry =
                    Payload.geometry output.payload |> Result.map_error Diagnostic.error_to_string
                  in
                  let bounds = lazy (Result.value ~default:None (output_bounds output)) in
                  let* tinted =
                    Pick.tint output lit |> Result.map_error Diagnostic.error_to_string
                  in
                  Result.bind (value.prepare settings tinted) (fun prepared ->
                      loop
                        ({
                           id;
                           graph;
                           prepared;
                           output;
                           lit;
                           surface = lazy (Pick.surface geometry);
                           bounds;
                           settings;
                           context = projection;
                         }
                        :: reversed)
                        graphs outputs))
          | _ -> Error "cook returned a different number of outputs"
        in
        loop [] graphs outputs
      in
      let error, framing =
        if submit || probe_only then
          match
            Result.bind (context value timeline frame) (fun context ->
                Async_cook.submit_some value.worker ~context ~nodes:(List.map snd graphs)
                  ~optional:(List.map snd probed) ~prepare:(prepare context))
          with
          | Ok _ -> (None, None)
          | Error message -> (Some message, None)
        else (error, framing)
      in
      (* the job a probe-only submission replaced shows no error of its own *)
      let error, framing = if probe_only then (value.error, value.framing) else (error, framing) in
      (* A fixed-step run waits for the cook it just submitted, serving the device meanwhile: a cook
     whose regions run on the GPU queues them for this (the initial) domain, so a blocking await
     would deadlock. [pump] serves and then sleeps: on a queued request (the editor's, a pipe) or
     at most 0.5 ms, which is what bounds noticing the cook's own completion. *)
      let pieces, error, seconds, prepared_changed =
        if submit && Option.is_none error && value.await then (
          let rec settle () =
            match Async_cook.poll value.worker with
            | Some completion -> completion
            | None ->
                pump ();
                settle ()
          in
          let awaited = settle () in
          match awaited.result with
          | Ok (Displayed (pieces, found, data)) ->
              summaries := found;
              sources := data;
              ( List.filter (fun piece -> Document.Int_map.mem piece.id compiled) pieces,
                None,
                Some awaited.seconds,
                true )
          | Ok (Framed _ | Probed _) -> (pieces, error, seconds, prepared_changed)
          | Error failure ->
              value.failure := failure_of failure;
              ( pieces,
                Some (Async_cook.error_to_string failure),
                Some awaited.seconds,
                prepared_changed ))
        else (pieces, error, seconds, prepared_changed)
      in
      (* a piece prepared under another highlight is prepared again from its kept
     output: no recook, no lowering, and nothing when no tag is involved *)
      let retinted = ref false in
      let pieces =
        List.map
          (fun piece ->
            if Pick.same_set piece.lit lit then piece
            else
              match Payload.geometry piece.output.payload with
              | Error _ -> piece
              | Ok geometry when Pick.tags geometry = None -> { piece with lit }
              | Ok _ -> (
                  match
                    Result.bind
                      (Pick.tint piece.output lit |> Result.map_error Diagnostic.error_to_string)
                      (value.prepare piece.settings)
                  with
                  | Ok prepared ->
                      retinted := true;
                      { piece with prepared; lit }
                  | Error _ -> piece))
          pieces
      in
      let prepared_changed = prepared_changed || !retinted in
      let framed, framing =
        match frame_request with
        | None -> (framed, framing)
        | Some (object_id, node_id) -> (
            match
              ( List.find_opt (fun piece -> piece.id = object_id) pieces,
                List.assoc_opt object_id graphs )
            with
            | Some piece, Some graph
              when Node.id graph = node_id
                   && piece.graph == graph
                   && Result.fold
                        ~ok:(fun context ->
                          piece.context
                          = Context.cache_projection (Node.graph_dependencies graph) context)
                        ~error:(fun _ -> false)
                        (context value timeline frame) ->
                (Some (Lazy.force piece.bounds), framing)
            | _ -> (
                match
                  Option.map
                    (fun (_, compiled) -> Edit_graph.compiled_node compiled ~node_id)
                    (Document.Int_map.find_opt object_id compiled)
                with
                | None | Some (Error _) -> (Some None, framing)
                | Some (Ok node) -> (
                    let was_busy = busy value && framing = None in
                    match
                      Result.bind (context value timeline frame) (fun context ->
                          Async_cook.submit value.worker ~context ~node ~prepare:(fun output ->
                              Result.map (fun bounds -> Framed bounds) (output_bounds output)
                              |> Result.map_error Diagnostic.error_to_string))
                    with
                    | Ok _ -> (framed, Some (was_busy || Option.value ~default:false framing))
                    | Error _ -> (Some None, framing))))
      in
      {
        cook =
          {
            value with
            schedule;
            pieces;
            settings = Some settings;
            error;
            seconds;
            framing;
            force = force_next;
            compiled;
            value_lanes;
            applied;
            graphs;
            displayed;
            probe_job = probe_only;
            probing =
              (if inline || probe_only then probes
               else if submit && (changed || value.probe_job) then []
               else value.probing);
            summaries = !summaries;
            sources = !sources;
          };
        edit_error;
        prepared_changed;
        framed;
      })

let close value = Async_cook.close value.worker

(* The nearest displayed primitive under a ray in the piece's own space, as (t, provenance tag)
   where the hit is [origin + t * direction]: the parameter, not the local distance, so hits of
   pieces and instances with any scale compare in the world's units.  A piece drawn as instances
   casts the ray into each instance's own frame (about 2 us each; per click, never per frame). *)
let pick_by pick_hit piece ~origin ~direction =
  let cast ~origin ~direction =
    Option.map
      (fun (distance, found) -> (distance /. Rays_math.Vec3.length direction, found))
      (match piece.output.payload with
      | Payload.Image _ | Payload.Kernel _ -> None
      | Geometry geometry -> pick_hit (Lazy.force piece.surface) geometry ~origin ~direction)
  in
  match piece.output.instances with
  | None | Some [||] -> cast ~origin ~direction
  | Some transforms ->
      Array.fold_left
        (fun best matrix ->
          match Rays_math.Mat4.inverse matrix with
          | None -> best
          | Some inverse -> (
              match
                ( cast
                    ~origin:(Rays_math.Mat4.transform_point inverse origin)
                    ~direction:(Rays_math.Mat4.transform_direction inverse direction),
                  best )
              with
              | Some (t, _), Some (nearer, _) when t >= nearer -> best
              | (Some _ as hit), _ -> hit
              | None, _ -> best))
        None transforms

let pick piece = pick_by Pick.cast piece

(* the [shop_materialpath] of the nearest primitive under the ray (Alt-click) *)
let pick_material piece = pick_by Pick.cast_material piece

(* as the app prepares a cooked object (instances, pieces and materials), and draws it *)
let prepare (_ : Settings.t) (output : Session.output) =
  let ( let* ) = Result.bind in
  let* geometry = Payload.geometry output.payload |> Result.map_error Diagnostic.error_to_string in
  Result.map
    (fun surface -> { geometry; surface })
    (Rdk_rays.Surface.of_geometry ?instances:output.instances geometry)

(* the editor's device positions binder ({!Flow_native.Workspace_gpu.display}) while it draws *)
let display = Domain.DLS.new_key (fun () -> None)

let with_display device f =
  let previous = Domain.DLS.get display in
  Domain.DLS.set display (Some device);
  Fun.protect ~finally:(fun () -> Domain.DLS.set display previous) f

let draw node ({ surface; _ } : prepared) =
  let explosion =
    Rdk_rays.Packed_pieces.explosion ~operation:(Node.operation node)
      ~fields:(Node.parameter_fields node)
  in
  Rdk_rays.Surface.scene3 ?explosion ?device:(Domain.DLS.get display) surface
