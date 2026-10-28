(* Live drives: the arguments of a lowered graph that depend on [t].  One cache retains the last
   plan and resolution; a static network reuses its result without recomputation. *)
type target = { port : Port.t; parameter : Port.parameter; program : Flow_ir.Executor.program }
type curve_target = { target : target; closed : bool; filled : bool }
type late_target = Late_text of target | Curve_points of curve_target

type plan = {
  network : Network.t;
  targets : target array;
  late : late_target array;
  states : Flow_ir.Executor.program option;
  time_dependent : bool;
}

type resolved = {
  geometry : Sop.Edit_graph.t;
  applied : Flow.Port_type.value Port.Map.t;
  applied_text : string Port.Map.t;
  applied_points : (float * float * float) array Port.Map.t;
  time_dependent : bool;
}

type cache = {
  plan : plan;
  live : Input.t;
  gpu : Flow_ir.Gpu.config;
  result : (resolved, Diagnostic.t) result;
}

type t = { mutable previous : cache option; state : Flow.Eval.state }

let create ?state () =
  { previous = None; state = Option.value ~default:(Flow.Eval.create_state ()) state }

let reset ?(host_state = true) t =
  t.previous <- None;
  Flow.Eval.reset_state ~host_state t.state

let prepare network =
  let compile =
    Flow_ir.Executor.compile ?profile:network.Network.profile ~approx:network.approx
      ~sink:Flow_ir.Sop_input
  in
  let states =
    if network.Network.states = [] then Ok None
    else Result.map Option.some (compile (Flow.Eval.List (Array.of_list network.states)))
  in
  Result.bind states (fun states ->
      if Port.Map.is_empty network.Network.drives then
        Ok
          {
            network;
            targets = [||];
            late = [||];
            states;
            time_dependent =
              network.states <> [] || not (Network.Int_map.is_empty network.frame_nodes);
          }
      else
        Result.bind (Network.validate network) (fun () ->
            let targets =
              Port.Map.fold
                (fun port value targets ->
                  Result.bind targets (fun targets ->
                      Result.bind (Network.parameter network port) (fun parameter ->
                          Result.map
                            (fun program -> { port; parameter; program } :: targets)
                            (compile value))))
                network.drives (Ok [])
            in
            Result.map
              (fun targets ->
                let ordered = List.rev targets in
                let classify target =
                  let curve =
                    match Sop.Edit_graph.find network.geometry ~node_id:target.port.node with
                    | Some node
                      when target.port.path = Curve.parameter
                           && Sop.Edit_graph.node_factory_key network.geometry
                                ~node_id:target.port.node
                              = Some (Sop.Edit_graph.factory_key Curve.factory) -> (
                        let fields = Sop.Node.parameter_fields node in
                        let setting name =
                          List.find_map
                            (fun (field : Param.field_view) ->
                              if field.name = name then
                                match field.current with
                                | Param.Bool_value value -> Some value
                                | _ -> None
                              else None)
                            fields
                        in
                        match (setting Curve.closed_parameter, setting Curve.filled_parameter) with
                        | Some closed, Some filled -> Some (closed, filled)
                        | _ -> None)
                    | _ -> None
                  in
                  match curve with
                  | Some (closed, filled) -> `Curve { target; closed; filled }
                  | None when target.parameter.ty = None -> `Text target
                  | None -> `Typed target
                in
                let classified = List.map classify ordered in
                {
                  network;
                  targets =
                    Array.of_list
                      (List.filter_map
                         (function `Typed target -> Some target | _ -> None)
                         classified);
                  late =
                    Array.of_list
                      (List.filter_map
                         (function
                           | `Text target -> Some (Late_text target)
                           | `Curve target -> Some (Curve_points target)
                           | `Typed _ -> None)
                         classified);
                  states;
                  time_dependent = true;
                })
              targets))

let live_error message = Error (Diagnostic.error ~code:"E_LIVE" message)

let live_port_value = function
  | Flow.Eval.Int n -> Ok (Flow.Port_type.Int_value n)
  | Flow.Eval.Float f -> Ok (Flow.Port_type.Float_value f)
  | Flow.Eval.Bool b -> Ok (Flow.Port_type.Bool_value b)
  | Flow.Eval.Vec3 (x, y, z) -> Ok (Flow.Port_type.Vec3_value (x, y, z))
  | Flow.Eval.Text text -> (
      match Port.color_of_text text with
      | Some (x, y, z) -> Ok (Flow.Port_type.Vec3_value (x, y, z))
      | None -> live_error ("Live value is not a colour: " ^ text))
  | _ -> live_error "Live value is not a number, bool, vec3 or colour"

(* Live text parameters. Curve points stay on their typed path below. *)
let live_text (parameter : Port.parameter) = function
  | Flow.Eval.Text text -> Ok text
  | _ -> live_error ("Live value does not fit " ^ parameter.path)

let live_points = function
  | Flow.Eval.List values ->
      let points =
        Array.map (function Flow.Eval.Vec3 (x, y, z) -> Some (x, y, z) | _ -> None) values
      in
      if Array.exists Option.is_none points then live_error "Live list must hold vec3"
      else Ok (Array.map Option.get points)
  | _ -> live_error "Live list must hold vec3"

let force ?(gpu = Flow_ir.Gpu.default) ~reference ~state ~live program =
  Result.bind (Flow_ir.Executor.force_display ~gpu ~reference ~state program ~live) (function
    | Flow_ir.Executor.Cpu value -> Ok value
    | Gpu _ -> Error (Diagnostic.error ~code:"E_APPROX_SINK" "Live SOP values require (exact x)."))

let normalize_target ~gpu ~reference ~state ~live target =
  Result.bind
    (Result.bind (force ~gpu ~reference ~state ~live target.program) live_port_value)
    (Port.normalize target.parameter)

let apply_geometry geometry target changes =
  Result.map fst
    (Result.map_error (Diagnostic.error ~code:"E_TYPE")
       (Sop.Edit_graph.apply_parameters geometry ~node_id:target.Port.node changes))

let compute ~gpu ~state previous plan ~live =
  let reference = plan.network.Network.reference in
  Result.bind
    (match plan.states with
    | None -> Ok Flow.Eval.No_geo
    | Some states -> force ~gpu ~reference ~state ~live states)
    (fun _ ->
      if
        Array.length plan.targets = 0
        && Array.length plan.late = 0
        && Network.Int_map.is_empty plan.network.frame_nodes
      then
        Ok
          {
            geometry = plan.network.geometry;
            applied = Port.Map.empty;
            applied_text = Port.Map.empty;
            applied_points = Port.Map.empty;
            time_dependent = plan.time_dependent;
          }
      else
        let applied = ref Port.Map.empty in
        let previous =
          Option.bind previous (fun cache ->
              Option.map
                (fun resolved -> (resolved, cache.plan.network))
                (Result.to_option cache.result))
        in
        let same_geometry =
          match previous with
          | None -> false
          | Some (_, old_network) -> plan.network.geometry == old_network.Network.geometry
        in
        let geometry =
          if same_geometry then (Option.get previous |> fst).geometry else plan.network.geometry
        in
        let restored =
          if not same_geometry then Ok geometry
          else
            let previous, old_network = Option.get previous in
            let restored =
              Port.Map.fold
                (fun port _ result ->
                  Result.bind result (fun geometry ->
                      if
                        Port.Map.mem port plan.network.drives
                        || Sop.Edit_graph.find geometry ~node_id:port.Port.node = None
                      then Ok geometry
                      else
                        Result.bind (Network.parameter old_network port) (fun parameter ->
                            Result.bind
                              (Port.literal_changes parameter (Port.literal parameter))
                              (apply_geometry geometry port))))
                previous.applied (Ok geometry)
            in
            Port.Map.fold
              (fun port _ result ->
                Result.bind result (fun geometry ->
                    if Port.Map.mem port plan.network.drives then Ok geometry
                    else
                      match Sop.Edit_graph.find plan.network.geometry ~node_id:port.Port.node with
                      | None -> Ok geometry
                      | Some node ->
                          Result.map_error
                            (Diagnostic.error ~code:"E_GEOMETRY")
                            (Sop.Edit_graph.replace_node node geometry)))
              previous.applied_points restored
        in
        let geometry =
          Array.fold_left
            (fun result target ->
              Result.bind result (fun geometry ->
                  Result.bind (normalize_target ~gpu ~reference ~state ~live target)
                    (fun (value, changes) ->
                      applied := Port.Map.add target.port value !applied;
                      if Sop.Edit_graph.find plan.network.geometry ~node_id:target.port.node = None
                      then Ok geometry
                      else if
                        same_geometry
                        &&
                        match previous with
                        | Some (previous, _) ->
                            Port.Map.find_opt target.port previous.applied = Some value
                        | None -> false
                      then Ok geometry
                      else apply_geometry geometry target.port changes)))
            restored plan.targets
        in
        let applied_text = ref Port.Map.empty and applied_points = ref Port.Map.empty in
        let geometry =
          Array.fold_left
            (fun result -> function
              | Late_text target ->
                  Result.bind result (fun geometry ->
                      Result.bind (force ~gpu ~reference ~state ~live target.program) (fun value ->
                          Result.bind (live_text target.parameter value) (fun text ->
                              applied_text := Port.Map.add target.port text !applied_text;
                              if
                                Sop.Edit_graph.find plan.network.geometry ~node_id:target.port.node
                                = None
                              then Ok geometry
                              else if
                                same_geometry
                                &&
                                match previous with
                                | Some (previous, _) ->
                                    Port.Map.find_opt target.port previous.applied_text = Some text
                                | None -> false
                              then Ok geometry
                              else
                                let literal =
                                  match target.parameter.fields with
                                  | [ { Param.kind = Param.Choice_view _; _ } ] ->
                                      Param.Choice_value text
                                  | _ -> Param.Text_value text
                                in
                                Result.bind
                                  (Port.literal_changes target.parameter (Port.Scalar literal))
                                  (apply_geometry geometry target.port))))
              | Curve_points { target; closed; filled } ->
                  Result.bind result (fun geometry ->
                      Result.bind (force ~gpu ~reference ~state ~live target.program) (fun value ->
                          Result.bind (live_points value) (fun points ->
                              applied_points := Port.Map.add target.port points !applied_points;
                              let prior =
                                match previous with
                                | Some (previous, _) ->
                                    Port.Map.find_opt target.port previous.applied_points
                                | None -> None
                              in
                              if
                                same_geometry
                                && Option.fold ~none:false
                                     ~some:(fun old ->
                                       Array.length old = Array.length points
                                       && Array.for_all2 ( = ) old points)
                                     prior
                              then Ok geometry
                              else
                                let port name = Port.{ node = target.port.node; path = name } in
                                let option name default =
                                  match Port.Map.find_opt (port name) !applied with
                                  | Some (Flow.Port_type.Bool_value value) -> value
                                  | _ -> default
                                in
                                match Sop.Edit_graph.find geometry ~node_id:target.port.node with
                                | None -> Ok geometry
                                | Some source ->
                                    let node =
                                      Curve.replace_points ~source
                                        ~closed:(option Curve.closed_parameter closed)
                                        ~filled:(option Curve.filled_parameter filled)
                                        points
                                    in
                                    Result.map_error
                                      (Diagnostic.error ~code:"E_GEOMETRY")
                                      (Sop.Edit_graph.replace_node node geometry)))))
            geometry plan.late
        in
        let geometry =
          Network.Int_map.fold
            (fun id rebuild result ->
              Result.bind result (fun geometry ->
                  match Sop.Edit_graph.find geometry ~node_id:id with
                  | None -> Ok geometry
                  | Some node ->
                      Result.bind (rebuild ~network:plan.network state live node) (fun node ->
                          Result.map_error
                            (Diagnostic.error ~code:"E_GEOMETRY")
                            (Sop.Edit_graph.replace_node node geometry))))
            plan.network.frame_nodes geometry
        in
        Result.map
          (fun geometry ->
            {
              geometry;
              applied = !applied;
              applied_text = !applied_text;
              applied_points = !applied_points;
              time_dependent = plan.time_dependent;
            })
          geometry)

let resolve ?gpu ?live state ~time network =
  let gpu = Option.value ~default:Flow_ir.Gpu.default gpu in
  let same_gpu (a : Flow_ir.Gpu.config) (b : Flow_ir.Gpu.config) =
    a.fast = b.fast && a.policy = b.policy && a.backend == b.backend
  in
  let live = Option.value ~default:(Input.at_time time) live in
  match state.previous with
  | Some previous
    when previous.plan.network == network && same_gpu previous.gpu gpu
         && ((not previous.plan.time_dependent) || Input.equal previous.live live) ->
      previous.result
  | previous ->
      let plan =
        match previous with
        | Some previous when previous.plan.network == network -> Ok previous.plan
        | _ -> prepare network
      in
      Result.bind plan (fun plan ->
          let result =
            Flow.Eval.transaction state.state (fun () ->
                compute ~gpu ~state:state.state previous plan ~live)
          in
          state.previous <- Some { plan; live; gpu; result };
          result)
