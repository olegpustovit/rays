open Rays
module E = Flow.Eval
module V = Flow.Value
module I = Flow_ir.Executor

type prepared = {
  plan : E.plan;
  effects : I.program;
  args : I.program option array;
  edges : bool array;
}

type t = {
  resources : Workspace_resources.t;
  images : Workspace_images.t;
  gpu : Workspace_gpu.t;
  mutable prepared : prepared option;
  mutable pending_saves : string list; (* newest first *)
  mutable quit_requested : bool;
  mutable fired : int;
  mutable deterministic : bool;
  mutable pending_events : Input.event list;
}

let create ?seed ?grain ?domains ?gpu () =
  let resources = Workspace_resources.create () in
  let gpu = match gpu with Some gpu -> gpu | None -> Workspace_gpu.create () in
  {
    resources;
    images = Workspace_images.create ?seed ?grain ?domains ~gpu resources;
    gpu;
    prepared = None;
    pending_saves = [];
    quit_requested = false;
    fired = 0;
    deterministic = false;
    pending_events = [];
  }

let take_events host =
  let events = List.rev host.pending_events in
  host.pending_events <- [];
  events

let effects (workspace : Editor_document.Workspace_doc.t) (plan : E.plan) =
  Option.bind (Editor_document.Workspace_doc.editor_graph workspace) (fun graph ->
      Option.bind
        (Array.find_opt (fun (i : E.instance) -> i.default && i.graph = graph.name) plan.instances)
        (fun instance ->
          match instance.result with
          | E.Struct ("ui/workspace", _, args) -> List.assoc_opt "effects" args
          | _ -> None))

let prepare plan effects =
  let ( let* ) = Result.bind in
  let* effects = I.compile effects in
  let args =
    Array.map
      (fun (node : E.node) ->
        if node.ty = Flow.Ty.Named "effect" || node.ty = Flow.Ty.Named "sample" then
          Result.map Option.some (I.compile (E.Record node.args))
        else Ok None)
      plan.E.nodes
  in
  match Array.find_opt Result.is_error args with
  | Some (Error error) -> Error error
  | _ ->
      Ok
        {
          plan;
          effects;
          args = Array.map Result.get_ok args;
          edges = Array.make (Array.length plan.nodes) false;
        }

let requests ?(reference = false) host ~state ~live workspace plan =
  match effects workspace plan with
  | None -> Ok []
  | Some value ->
      let ( let* ) = Result.bind in
      let* prepared =
        match host.prepared with
        | Some prepared when prepared.plan == plan -> Ok prepared
        | _ ->
            Result.map
              (fun prepared ->
                host.prepared <- Some prepared;
                prepared)
              (prepare plan value)
      in
      E.transaction state (fun () ->
          try
            let force program =
              match I.force ~state ~reference program ~live with
              | Ok value -> value
              | Error d -> raise (Diagnostic.Error d)
            in
            let rec collect = function
              | E.List values -> Array.to_list values |> List.concat_map collect
              | E.Deferred (Flow.Ty.Named "effect", id)
                when id >= 0 && id < Array.length plan.E.nodes ->
                  let args =
                    match force (Option.get prepared.args.(id)) with
                    | E.Record args -> args
                    | _ -> assert false
                  in
                  let when_ = V.truthy (List.assoc "when" args) in
                  [ (id, when_, plan.nodes.(id).kind, args) ]
              | _ -> V.fail "E_EFFECT" "Workspace effects are host effect nodes."
            in
            let collected = collect (force prepared.effects) in
            let rising =
              List.filter (fun (id, when_, _, _) -> when_ && not prepared.edges.(id)) collected
            in
            List.iter (fun (id, when_, _, _) -> prepared.edges.(id) <- when_) collected;
            Ok rising
          with Diagnostic.Error d -> Error d)

let sample host prepared ~state ~live = function
  | E.Deferred (Flow.Ty.Named "sample", id) when id >= 0 && id < Array.length prepared.plan.nodes
    -> (
      let ( let* ) = Result.bind in
      let* value = I.force ~state (Option.get prepared.args.(id)) ~live in
      let args = match value with E.Record args -> args | _ -> assert false in
      let text key default =
        match List.assoc_opt key args with
        | None -> default
        | Some (E.Text text) -> text
        | _ -> V.fail "E_TYPE" "A resource path or waveform is text."
      in
      let number key default = Option.fold ~none:default ~some:V.num (List.assoc_opt key args) in
      match prepared.plan.nodes.(id).kind with
      | "audio/load" ->
          let path = text "path" "" in
          Workspace_resources.sample host.resources ("load:" ^ path) (fun audio ->
              Audio.Sample.load audio path)
      | "audio/synth" ->
          let wave = text "waveform" "sine"
          and frequency = number "frequency" 440.
          and duration = number "duration" 0.1
          and volume = number "volume" 1. in
          let waveform =
            match wave with
            | "sine" -> Audio.Sample.Sine
            | "square" -> Square
            | "triangle" -> Triangle
            | "sawtooth" -> Saw
            | _ -> V.fail "E_AUDIO" "Unknown audio waveform."
          in
          let key = Printf.sprintf "synth:%s:%h:%h:%h" wave frequency duration volume in
          Workspace_resources.sample host.resources key (fun audio ->
              Audio.Sample.synth audio ~waveform ~frequency ~duration ~volume ())
      | _ -> Error (Diagnostic.error ~code:"E_AUDIO" "Expected an audio sample node."))
  | _ -> Error (Diagnostic.error ~code:"E_AUDIO" "Expected an audio sample value.")

let perform host ~state ~live requests =
  let ( let* ) = Result.bind in
  List.fold_left
    (fun result (_, _, kind, args) ->
      let* () = result in
      host.fired <- host.fired + 1;
      let arg name = List.assoc name args in
      let text value =
        match value with
        | E.Text text -> text
        | _ -> V.fail "E_TYPE" "Host paths and dialog kinds are text."
      in
      if host.deterministic && (kind = "host/quit" || kind = "host/dialog") then
        Error
          (Diagnostic.error ~code:"E_EFFECT_EXPORT"
             "Quit and file dialogs are unavailable in fixed-step runs.")
      else
        match kind with
        | "host/quit" ->
            host.quit_requested <- true;
            Sketch.quit ();
            Ok ()
        | "host/save_png" ->
            host.pending_saves <- text (arg "path") :: host.pending_saves;
            Ok ()
        | "host/play" when host.deterministic -> Ok ()
        | "host/play" ->
            let* sample = sample host (Option.get host.prepared) ~state ~live (arg "sample") in
            Result.map ignore
              (Result.map_error
                 (fun message -> Diagnostic.error ~code:"E_AUDIO" message)
                 (Audio.Sample.play (Option.get host.resources.audio)
                    ?volume:(Option.map V.num (List.assoc_opt "volume" args))
                    ?loops:(Option.map V.int_of (List.assoc_opt "loops" args))
                    sample))
        | "host/dialog" ->
            let kind =
              match text (arg "kind") with
              | "open_file" -> Sketch.Open_file
              | "open_files" -> Open_files
              | "save_file" -> Save_file
              | "open_folder" -> Open_folder
              | _ -> V.fail "E_EFFECT" "Unknown file dialog kind."
            in
            let filters =
              Option.map
                (function
                  | E.List entries ->
                      Array.to_list entries
                      |> List.map (function
                        | E.Record fields -> (
                            match
                              (List.assoc_opt "label" fields, List.assoc_opt "extensions" fields)
                            with
                            | Some (E.Text label), Some (E.List extensions) ->
                                (label, Array.to_list (Array.map text extensions))
                            | _ ->
                                V.fail "E_EFFECT" "A filter has text :label and list :extensions.")
                        | E.List [| E.Text label; E.List extensions |] ->
                            (label, Array.to_list (Array.map text extensions))
                        | _ -> V.fail "E_EFFECT" "A filter has :label and :extensions.")
                  | _ -> V.fail "E_EFFECT" "Dialog filters are a list.")
                (List.assoc_opt "filters" args)
            in
            Result.map
              (fun id -> host.pending_events <- Input.Dialog_opened id :: host.pending_events)
              (Result.map_error
                 (fun message -> Diagnostic.error ~code:"E_EFFECT" message)
                 (Sketch.show_file_dialog ?filters
                    ?default_location:(Option.map text (List.assoc_opt "default" args))
                    kind))
        | _ -> Error (Diagnostic.error ~code:"E_EFFECT" ("Unknown host effect " ^ kind)))
    (Ok ()) requests

let update host ~state ~live workspace plan =
  try Result.bind (requests host ~state ~live workspace plan) (perform host ~state ~live)
  with Diagnostic.Error d -> Error d

let save_pending host save =
  let paths = List.rev host.pending_saves in
  host.pending_saves <- [];
  List.fold_left
    (fun result path ->
      Result.bind result (fun () ->
          Result.map_error (fun message -> Diagnostic.error ~code:"E_EFFECT" message) (save path)))
    (Ok ()) paths

let close host =
  Workspace_resources.close host.resources;
  Workspace_images.close host.images;
  Workspace_gpu.close host.gpu

let export_check workspace plan =
  match effects workspace plan with
  | None -> Ok ()
  | Some effects ->
      let rec unsafe = function
        | E.List values -> Array.exists unsafe values
        | E.Deferred (Flow.Ty.Named "effect", id) when id >= 0 && id < Array.length plan.E.nodes ->
            let node = plan.nodes.(id) in
            (node.kind = "host/quit" || node.kind = "host/dialog")
            && List.assoc_opt "when" node.args = Some (E.Bool true)
        | _ -> false
      in
      if unsafe effects then
        Error
          (Diagnostic.error ~code:"E_EFFECT_EXPORT"
             "Quit and file dialogs are unavailable during export.")
      else Ok ()

let export_update host ~state ~live workspace plan =
  Result.bind (requests host ~state ~live workspace plan) (fun requests ->
      (* Export never initializes audio. Save paths are captured after presentation. *)
      if List.exists (fun (_, _, kind, _) -> kind = "host/quit" || kind = "host/dialog") requests
      then
        Error
          (Diagnostic.error ~code:"E_EFFECT_EXPORT"
             "Quit and file dialogs are unavailable during export.")
      else begin
        List.iter
          (fun (_, _, kind, args) ->
            if kind = "host/save_png" then
              match List.assoc "path" args with
              | E.Text path -> host.pending_saves <- path :: host.pending_saves
              | _ -> V.fail "E_TYPE" "A PNG path is text.")
          requests;
        Ok ()
      end)
