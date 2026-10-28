type error =
  | Stage of string
  | Resource of Ogpu.Error.t
  | Begin of Renderer.error
  | Lower of Renderer.error
  | Step of Renderer.error

let pp_error formatter = function
  | Stage message -> Format.fprintf formatter "native Scene staging: %s" message
  | Resource error -> Format.fprintf formatter "native Scene resource: %a"
      Runtime_resources.pp_error error
  | Begin error ->
      Format.fprintf formatter "native Scene submission setup: %a"
        Renderer.pp_error error
  | Lower error ->
      Format.fprintf formatter "native Scene lowering: %a"
        Renderer.pp_error error
  | Step error ->
      Format.fprintf formatter "native Scene submission: %a"
        Renderer.pp_error error

let draw_of_scene3_entry (entry : Renderer.Scene_execution.scene3_entry) =
  Renderer.prepared_draw
    ~family:entry.family ~blend:entry.blend
    ?texture:entry.texture ?auxiliary:entry.auxiliary ?vertex_attributes:entry.vertex_attributes ~samples:entry.samples
    entry.draw

type retained_draws={staged:Scene.Layer.staged_native;
  layer:Renderer.Scene_execution.prepared_scene3;  (* a frame with several 3D layers caches each *)
  draws:Renderer.draw list}
let retained_draw_capacity=16
module Retained_key=struct
  type t=Scene.Layer.staged_native*Renderer.Scene_execution.prepared_scene3
  let equal(a,b)(c,d)=a==c&&b==d
  let hash((staged:Scene.Layer.staged_native),(layer:Renderer.Scene_execution.prepared_scene3))=
    Hashtbl.hash(staged.retained,Array.length layer.Renderer.Scene_execution.entries)
end
module Retained_table=Lru.Make(Retained_key)
type cache_context={retained_draws:retained_draws Retained_table.t}
let current_cache_context=Domain.DLS.new_key(fun()->None)
let create_cache_context()={retained_draws=Retained_table.create retained_draw_capacity}
let with_cache_context context f=
  let previous=Domain.DLS.get current_cache_context in
  Domain.DLS.set current_cache_context(Some context);
  Fun.protect~finally:(fun()->Domain.DLS.set current_cache_context previous)f
let draws_of_prepared (staged:Scene.Layer.staged_native) prepared=
  match staged.retained with
  |None->Array.to_list prepared.Renderer.Scene_execution.entries|>List.map draw_of_scene3_entry
  |Some _->(match Domain.DLS.get current_cache_context with
      |None->Array.to_list prepared.Renderer.Scene_execution.entries|>List.map draw_of_scene3_entry
      |Some context->
       let key=staged,prepared in
       match Retained_table.find context.retained_draws key with
       |cached->cached.draws
       |exception Not_found->
          let entries=prepared.Renderer.Scene_execution.entries in
          let rec build index reversed=
            if index=Array.length entries then List.rev reversed
            else build(index+1)(draw_of_scene3_entry
              (Array.unsafe_get entries index)::reversed)in
          let draws=build 0[]in
          Retained_table.add context.retained_draws key {staged;layer=prepared;draws};
          draws)

let rec validate_images = function
  | [] -> Ok ()
  | (_, Renderer.Image image) :: rest ->
      Result.bind (Runtime_resources.Image.gpu_snapshot image)
        (fun _ -> validate_images rest)
  | _ :: rest -> validate_images rest

let rec validate_layers = function
  | [] -> Ok ()
  | (Scene.Layer.Scene2_layer (_, resources)
    | Scene2_segment (_, resources) | Ui_layer (_, resources)) :: rest ->
      Result.bind (validate_images resources) (fun () -> validate_layers rest)
  | Scene3_layer _ :: rest -> validate_layers rest

let rec validate_mesh_images = function
  | [] -> Ok ()
  | image :: rest -> Result.bind (Runtime_resources.Image.gpu_snapshot image)
      (fun _ -> validate_mesh_images rest)

let render ~execution ~density ~width ~height scene =
  let active_submission=ref None in
  let prepared = try
      let outcome=Fun.protect
        ~finally:(fun () -> Scene.Layer.release scene)
        (fun () ->
        let staged=Scene.Layer.stage_native_render_checked ~density ~width ~height scene in
        match staged with
        | Error (Scene.Layer.Message message) -> Error (Stage message)
        | Error (Scene.Layer.Resource error) -> Error (Resource error)
        | Ok staged -> (
            let replayed=match Result.bind (validate_layers staged.layers)
              (fun () -> validate_mesh_images staged.mesh_images) with
            | Error error -> Error (Resource error)
            | Ok () -> Result.map_error (fun error -> Step error) (match staged.retained with
            |None->Ok None
            |Some(identity,version)->
                Renderer.Native.replay
                    ~clear:staged.clear ~identity ~version execution) in
            match replayed with
            |Error error->Error error
            |Ok(Some ())->Ok `Replayed
            |Ok None->
            match Renderer.Native.begin_submission execution with
            | Error error -> Error (Begin error)
            | Ok submission ->
                active_submission:=Some submission;
                    let rec lower reversed = function
                      | [] ->
                          Ok (`Prepared (submission, staged.retained, staged.clear,
                            List.rev reversed))
                      | Scene.Layer.Scene2_layer (ir, resources) :: rest -> (
                          match
                            Renderer.Native.lower_scene2 submission
                              ~density
                              ~resource:(fun id -> List.assoc_opt id resources)
                              ir
                          with
                          | Error error -> Error (Lower error)
                          | Ok batch -> lower (batch::reversed) rest)
                      | Scene.Layer.Scene2_segment (segment, resources) :: rest -> (
                          let cacheable=List.for_all(function
                            |_,Renderer.Text _->true
                            |_,Renderer.Image _
                            |_,Renderer.Canvas _->false)resources in
                          let version=Scene.Layer.native_segment_version
                            segment resources in
                          match Renderer.Native.lower_scene2_segment
                            submission
                            ~identity:(Scene_command.Display_list.id segment)
                            ~version
                            ~cacheable ~density
                            ~resource:(fun id -> List.assoc_opt id resources)
                            (Scene_command.Display_list.render_ir segment) with
                          | Error error -> Error (Lower error)
                          | Ok batch -> lower (batch :: reversed) rest)
                      | Scene.Layer.Ui_layer (ui, resources) :: rest -> (
                          match Renderer.Native.lower_ui submission
                            ~density ~resource:(fun id -> List.assoc_opt id resources)
                            ui with
                          | Error error -> Error (Lower error)
                          | Ok batch -> lower (batch :: reversed) rest)
                      | Scene.Layer.Scene3_layer prepared :: rest ->
                          let draws = draws_of_prepared staged prepared in
                          (match Renderer.Native.adopt_retained_view
                            submission ~density ~layer:prepared draws with
                          |Error error->Error(Lower error)
                          |Ok batch->lower(batch::reversed)rest)
                    in
                    lower [] staged.layers)) in
      (match outcome with
      |Error _->Option.iter Renderer.Native.cancel !active_submission
      |Ok _->());
      outcome
    with exn->
      Option.iter Renderer.Native.cancel !active_submission;
      raise exn
  in
  match prepared with
  | Error _ as error -> error
  | Ok `Replayed -> Ok ()
  | Ok (`Prepared (submission, retained, clear, batches)) -> (
      let stepped=match retained with
      |None->Renderer.Native.step ~clear submission batches
      |Some(identity,version)->Renderer.Native.step ~clear
          ~identity ~version submission batches in
      match stepped with
      | Ok () -> Ok ()
      | Error error -> Error (Step error))
