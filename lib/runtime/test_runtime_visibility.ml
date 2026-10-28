let get = function Ok value -> value | Error error -> failwith (Ogpu.Error.to_string error)

let run () =
  match Runtime.create ~width:32 ~height:24 () with
  | Error _ -> print_endline "runtime visibility: skipped (no window/Metal device)"
  | Ok runtime ->
      Fun.protect
        ~finally:(fun () -> ignore (Runtime.destroy runtime))
        (fun () ->
          get (Runtime.set_text_input runtime (Some ((1, 2, 20, 12), 7)));
          get (Runtime.set_text_input runtime None);
          get (Runtime.set_text_input runtime None);
          if get (Runtime.visible runtime) then failwith "native window was not initially hidden";
          get (Runtime.show runtime);
          if not (get (Runtime.visible runtime)) then failwith "show did not set visible flag";
          print_endline "runtime visibility: hidden/show flags passed")
