open Rays
module E = Flow.Eval
module V = Flow.Value
module I = Flow_ir.Executor
module C = Flow_ir.Core_exec

(* Numbers the GPU mark sets of different drawings apart in the backend's sources. *)
let drawings = Atomic.make 0
type prepared = {id : int; states : I.program;
  core : (C.prepared * E.residual option * C.Buffers.t, Flow.Diagnostic.t) result Lazy.t; whole : bool;
  cache : Marks_scene.cache;
  display : Flow_ir.Packed.t -> live:Frame_input.t -> (Flow_ir.Gpu.value option, Flow.Diagnostic.t) result;
  readback : Flow_ir.Packed.t -> live:Frame_input.t -> (E.value option, Flow.Diagnostic.t) result;
  mutable kept : ((int*int)*Scene.t) option;
  mutable borrows : bool}  (* the last scene's mark sets bind GPU outputs a later draw replaces *)
let prepare ?profile ?(approx=Flow.Workspace.Paths.empty) ?(states = []) (plan : E.plan) root =
  let ( let* ) = Result.bind in
  let* states = I.compile ?profile (E.List (Array.of_list states)) in
  let known=Array.make(Array.length plan.nodes)None in
  let rec static = function
    |E.Deferred(Flow.Ty.Named "drawing",id)->node id
    |E.Deferred _->false
    |E.List values->Array.for_all static values
    |E.Record fields|E.Struct(_,_,fields)->List.for_all(fun(_,value)->static value)fields
    |value->not(E.is_live value)
  and node id=match known.(id)with
    |Some value->value
    |None->known.(id)<-Some false;
        let value=List.for_all(fun(_,value)->static value)plan.nodes.(id).args in
        known.(id)<-Some value;value in
  Ok {id = Atomic.fetch_and_add drawings 1; states;
    core=lazy(match Flow_ir.Core_draw.lower plan root with
      |Ok(program,residual)->Ok(C.prepare ~remember_loops:true program,residual,C.Buffers.create())
      |Error name->Error(Flow.Diagnostic.error ~code:"E_DRAW_KIND" (Printf.sprintf "The drawing cannot be lowered to the core path (%s)." name)));
    whole=static root; kept=None; borrows=false; cache=Marks_scene.cache ();
    display=(let display=I.region_display ?profile ~approx () in fun packed ~live->display packed ~live);
    readback=I.region_readback ()}

(* The draw program runs into a mark table which Marks_scene adapts. Retention is whole-program: a
   fully static draw graph is kept per size; the fold states still advance. *)
let render_core (program, root, buffers) ~state ?image ?gpu ?resident prepared ~live ~size =
  let ( let* ) = Result.bind in
  E.transaction state (fun () ->
    let* _ = I.force ~state prepared.states ~live in
    match prepared.kept with
    | Some (kept, scene) when prepared.whole && kept = size -> Ok scene
    | _ ->
        C.Buffers.clear buffers;
        let images = ref [] and count = ref 0 in
        let image = Option.map (fun resolve value ->
          let* loaded = resolve value in
          images := loaded :: !images; incr count; Ok !count) image in
        (* A region offered as a mark attribute may stay on the GPU; each runs at most once a
           frame, since a second dispatch would overwrite the first's output. *)
        let gpu = Option.map (fun gpu ~slot -> gpu ~slot:(slot lor (prepared.id lsl 20))) gpu in
        let devices = ref [] and ran = ref [] and ndevices = ref 0 in
        let device = Option.map (fun _ packed ->
          if List.memq packed !ran then Ok None else
          Result.map (Option.map (fun (v : Flow_ir.Gpu.value) ->
            ran := packed :: !ran; devices := v :: !devices; incr ndevices;
            !ndevices - 1, v.count, v.width)) (prepared.display packed ~live)) gpu in
        (* in fast precision a region the drawing computes with may run on the GPU; a refusal or a
           device error leaves it to the CPU *)
        let force = Option.map (fun _ packed -> match prepared.readback packed ~live with
          | Ok (Some value) -> Ok value
          | Ok None | Error _ -> Flow_ir.Packed.force packed ~live) gpu in
        let* marks = match root with
          | None -> C.draw ?device ?force ?resident ?image program ~live ~size ~buffers
          | Some root ->
              (* State reads need the residual's live frame, which the evaluator binds while it
                 offers the residual to its executor; the draw's own result rides out in a ref. *)
              let drawn = ref (Error (Flow.Diagnostic.error ~code:"E_CORE" "The draw program did not run.")) in
              let execute r live = drawn := C.draw ~root:r ?device ?force ?resident ?image program ~live ~size ~buffers;
                Some (Ok (E.Int 0)) in
              let* _ = E.Private.force_with_executor ~state ~execute (E.Residual root) ~live in
              !drawn in
        let table = Array.of_list (List.rev !images) in
        let table_devices = Array.of_list (List.rev !devices) in
        let* scene = Marks_scene.scene ~buffers ~cache:prepared.cache ~image:(fun id -> table.(id - 1)) ?gpu
          ~device:(fun id -> let token, _, _ = C.Buffers.device buffers id in table_devices.(token)) marks in
        if prepared.whole && !devices = [] then prepared.kept <- Some (size, scene);
        prepared.borrows <- !devices <> [];
        Ok scene)

let render_prepared ?state ?image ?gpu ?resident prepared ~live ~size =
  let state = Option.value ~default:(E.create_state ()) state in
  match Lazy.force prepared.core with
  | Ok core -> render_core core ~state ?image ?gpu ?resident prepared ~live ~size
  | Error d -> Error d

let borrows_gpu prepared = prepared.borrows
