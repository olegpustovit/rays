open Sdl3

let fail message = failwith ("SDL3 stress test: " ^ message)
let get = function Ok value -> value | Error error -> fail (Format.asprintf "%a" pp_error error)

(* RAYS_SDL3_STRESS_CYCLES=100000 under @qualification. *)
let iterations =
  Option.value ~default:5_000
    (Option.bind (Sys.getenv_opt "RAYS_SDL3_STRESS_CYCLES") int_of_string_opt)

let run () =
  get (Init.init [ Init.Video; Init.Events ]);
  for index = 1 to iterations do
    let window =
      get
        (Window.create ~title:"SDL3 recreation stress" ~width:16 ~height:16 ~flags:[ Window.Hidden ]
           ())
    in
    if get (Window.size window) <> (16, 16) then
      fail (Printf.sprintf "window facts changed during lifecycle cycle %d" index);
    get (Window.destroy window)
  done;
  (* a window or cursor left to the collector is released on the next SDL call *)
  for _ = 1 to iterations / 10 do
    ignore
      (get
         (Window.create ~title:"SDL3 finalizer stress" ~width:8 ~height:8 ~flags:[ Window.Hidden ]
            ()));
    match Cursor.create Cursor.Text with
    | Ok _ | Error { kind = Unsupported; _ } -> ()
    | Error error -> fail (Format.asprintf "%a" pp_error error)
  done;
  Gc.full_major ();
  ignore (get (Event.poll_coalesced ()));
  get (Init.quit_subsystems [ Init.Video; Init.Events ]);
  Printf.printf "SDL3 window lifecycle stress passed (%d cycles)\n%!" iterations
