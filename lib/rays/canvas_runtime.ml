(* [save_screen_png] has no run argument, so bind its owner only for the
   dynamic extent of one Sketch loop. *)
type save = string -> (unit, string) result

let current : save option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)

let with_save save f =
  let previous = Domain.DLS.get current in
  Domain.DLS.set current (Some save);
  Fun.protect ~finally:(fun () -> Domain.DLS.set current previous) f

let save filename =
  match Domain.DLS.get current with
  | None -> Error "Canvas.save_screen_png: no active renderer"
  | Some callback ->
      Result.map_error
        (fun message -> "Canvas.save_screen_png: " ^ message)
        (Runtime_resources.write_atomic ~filename callback)
