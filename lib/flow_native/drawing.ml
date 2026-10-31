open Rays
module E = Flow.Eval
module V = Flow.Value
module I = Flow_ir.Executor
module C = Flow_ir.Core_exec

(* Numbers the GPU mark sets of different drawings apart in the backend's sources. *)
let drawings = Atomic.make 0

type prepared = {
  id : int;
  states : I.program;
  core : (C.prepared * E.residual option * C.Buffers.t, Diagnostic.t) result Lazy.t;
  whole : bool;
  cache : Marks_scene.cache;
  display : Flow_ir.Packed.t -> live:Input.t -> (Flow_ir.Gpu.value option, Diagnostic.t) result;
  readback : Flow_ir.Packed.t -> live:Input.t -> (E.value option, Diagnostic.t) result;
  mutable kept : ((int * int) * Scene.t) option;
  mutable borrows : bool;
}
(* the last scene's mark sets bind GPU outputs a later draw replaces *)

let prepare ?(gpu = Flow_ir.Gpu.default) ?profile ?(approx = Flow.Workspace.Paths.empty)
    ?(states = []) (plan : E.plan) root =
  let ( let* ) = Result.bind in
  let* states = I.compile ?profile (E.List (Array.of_list states)) in
  let known = Array.make (Array.length plan.nodes) None in
  let rec static = function
    | E.Deferred (Flow.Ty.Named "drawing", id) -> node id
    | E.Deferred _ -> false
    | E.List values -> Array.for_all static values
    | E.Record fields | E.Struct (_, _, fields) ->
        List.for_all (fun (_, value) -> static value) fields
    | value -> not (E.is_live value)
  and node id =
    match known.(id) with
    | Some value -> value
    | None ->
        known.(id) <- Some false;
        let value = List.for_all (fun (_, value) -> static value) plan.nodes.(id).args in
        known.(id) <- Some value;
        value
  in
  Ok
    {
      id = Atomic.fetch_and_add drawings 1;
      states;
      core =
        lazy
          (match Flow_ir.Core_draw.lower plan root with
          | Ok (program, residual) ->
              Ok
                ( C.prepare ~remember_loops:true ~approx ~fast:gpu.fast program,
                  residual,
                  C.Buffers.create () )
          | Error name ->
              Error
                (Diagnostic.error ~code:"E_DRAW_KIND"
                   (Printf.sprintf "The drawing cannot be lowered to the core path (%s)." name)));
      whole = static root;
      kept = None;
      borrows = false;
      cache = Marks_scene.cache ();
      display =
        (let display = I.region_display ~gpu ?profile ~approx () in
         fun packed ~live -> display packed ~live);
      readback = I.region_readback ~gpu ();
    }

(* The draw program runs into a mark table which Marks_scene adapts. Retention is whole-program: a
   fully static draw graph is kept per size; the fold states still advance. *)
let render_core (program, root, buffers) ~state ?image ?gpu ?resident prepared ~live ~size =
  let show = Option.map snd resident and resident = Option.map fst resident in
  let ( let* ) = Result.bind in
  E.transaction state (fun () ->
      let* _ = I.force ~state prepared.states ~live in
      match prepared.kept with
      | Some (kept, scene) when prepared.whole && kept = size -> Ok scene
      | _ ->
          C.Buffers.clear buffers;
          let images = ref [] and count = ref 0 in
          let image =
            Option.map
              (fun resolve value ->
                let* loaded = resolve value in
                images := loaded :: !images;
                incr count;
                Ok !count)
              image
          in
          (* A region offered as a mark attribute may stay on the GPU; each runs at most once a
           frame, since a second dispatch would overwrite the first's output. *)
          let gpu = Option.map (fun gpu ~slot -> gpu ~slot:(slot lor (prepared.id lsl 20))) gpu in
          let devices = ref [] and ran = ref [] and ndevices = ref 0 in
          let device =
            Option.map
              (fun _ packed ->
                let view = Flow_ir.Packed.Host.view packed in
                if List.memq view.code !ran then Ok None
                else
                  Result.map
                    (Option.map (fun (v : Flow_ir.Gpu.value) ->
                         ran := view.code :: !ran;
                         Array.mapi
                           (fun output registers ->
                             let width = Array.length registers in
                             devices := { v with output; width } :: !devices;
                             incr ndevices;
                             (!ndevices - 1, v.count, width))
                           view.outputs))
                    (* over a resident fold's kept arrays, its host draws it (or the CPU does) *)
                    (match Option.bind show (fun show -> show packed ~live) with
                    | Some shown -> Ok shown
                    | None -> prepared.display packed ~live))
              gpu
          in
          (* in fast precision a region the drawing computes with may run on the GPU; a refusal or a
           device error leaves it to the CPU *)
          let force =
            Option.map
              (fun _ packed ->
                match prepared.readback packed ~live with
                | Ok (Some value) -> Ok value
                | Ok None | Error _ -> Flow_ir.Packed.force packed ~live)
              gpu
          in
          let* marks =
            match root with
            | None -> C.draw ?device ?force ?resident ?image program ~live ~size ~buffers
            | Some root ->
                (* Bind the residual's evaluator state plane before the core draws directly. *)
                let* () = E.Residual.bind_live ~state root ~live in
                C.draw ~root ?device ?force ?resident ?image program ~live ~size ~buffers
          in
          let table = Array.of_list (List.rev !images) in
          let table_devices = Array.of_list (List.rev !devices) in
          let* scene =
            Marks_scene.scene ~buffers ~cache:prepared.cache
              ~image:(fun id -> table.(id - 1))
              ?gpu
              ~device:(fun id ->
                let token, _, _ = C.Buffers.device buffers id in
                table_devices.(token))
              marks
          in
          if prepared.whole && !devices = [] then prepared.kept <- Some (size, scene);
          prepared.borrows <- !devices <> [];
          Ok scene)

let render_prepared ?state ?image ?gpu ?resident prepared ~live ~size =
  let state = Option.value ~default:(E.create_state ()) state in
  match Lazy.force prepared.core with
  | Ok core -> render_core core ~state ?image ?gpu ?resident prepared ~live ~size
  | Error d -> Error d

let borrows_gpu prepared = prepared.borrows
