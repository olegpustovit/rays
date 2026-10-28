module Native_thread = Thread
open Sdl3

let fail message = failwith ("SDL3 test: " ^ message)
let get = function Ok value -> value | Error error -> fail (Format.asprintf "%a" pp_error error)

let run () =
  let wrong_thread () =
    let outcome = ref (Ok ()) in
    let thread =
      Native_thread.create
        (fun () ->
          try
            if not (Sdl3.Thread.is_initial_domain ()) then
              fail "thread fixture did not use the initial OCaml domain";
            if Sdl3.Thread.is_sdl_main_thread () then
              fail "platform main-thread query accepted another thread";
            match Init.init [ Init.Events ] with
            | Error { kind = Wrong_domain; _ } -> ()
            | _ -> fail "SDL init accepted another thread"
          with exn -> outcome := Error exn)
        ()
    in
    Native_thread.join thread;
    match !outcome with Ok () -> () | Error exn -> raise exn
  in
  if not (Sdl3.Thread.is_sdl_main_thread ()) then
    fail "platform main thread refused before SDL init";
  (* SDL deliberately defines the main thread at initialization elsewhere. *)
  if Sys.os_type = "Unix" && Sys.file_exists "/System/Library/Frameworks/Metal.framework" then
    wrong_thread ();
  let triple (v : version) = (v.major, v.minor, v.patch) in
  let compiled = compiled_version and linked = linked_version () in
  (match
     Sdl3_lock.check_installed
       ~lock:(Sdl3_lock.read "../../packaging/sdl3.lock")
       ~key:"sdl3" ~compiled:(triple compiled) ~linked:(triple linked)
   with
  | Ok () -> ()
  | Error message -> fail message);
  get (check_version ~release:true ());
  (match validate_version ~release:true ~linked:{ compiled with major = compiled.major - 1 } () with
  | Error { kind = Incompatible_version; _ } -> ()
  | Ok () | Error _ -> fail "older linked version was not rejected");
  (match
     validate_version ~library:"SDL3_ttf"
       ~compiled:{ major = 3; minor = 3; patch = 1 }
       ~release:true
       ~linked:{ major = 3; minor = 3; patch = 1 }
       ()
   with
  | Error { kind = Incompatible_version; _ } -> ()
  | Ok () | Error _ -> fail "prerelease extension headers were not rejected");
  get (Init.init [ Init.Video; Init.Events ]);
  if not (Sdl3.Thread.is_sdl_main_thread ()) then fail "platform main thread refused after SDL init";
  wrong_thread ();
  (match Window.create ~title:"bad\x00title" ~width:8 ~height:8 () with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok window ->
      ignore (Window.destroy window);
      fail "NUL window title succeeded"
  | Error _ -> fail "NUL window title returned the wrong error");
  (match Window.create ~title:"empty" ~width:0 ~height:8 () with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok window ->
      ignore (Window.destroy window);
      fail "zero-width window succeeded"
  | Error _ -> fail "zero-width window returned the wrong error");
  let window =
    get
      (Window.create ~title:"SDL3 ownership test" ~width:96 ~height:64 ~flags:[ Window.Hidden ] ())
  in
  (match Metal_view.create window with
  | Error { kind = Sdl_error; message; _ } when message <> "" -> ()
  | Ok view ->
      get (Metal_view.destroy view);
      fail "dummy-video window unexpectedly created a Metal view"
  | Error error ->
      fail
        (Format.asprintf "dummy Metal-view constructor returned the wrong error: %a" pp_error error));
  if get (Window.title window) <> "SDL3 ownership test" then fail "window title snapshot changed";
  if get (Window.size window) <> (96, 64) then fail "logical window size changed";
  let pixel_width, pixel_height = get (Window.size_in_pixels window) in
  if pixel_width <> 96 || pixel_height <> 64 then
    fail "dummy-video window is not a 1x logical/drawable fixture";
  ignore (get (Window.position window));
  get (Window.center window);
  get (Window.set_size window ~width:112 ~height:72);
  get (Window.sync window);
  if get (Window.size window) <> (112, 72) || get (Window.size_in_pixels window) <> (112, 72) then
    fail "synchronized 1x window resize changed logical/drawable size";
  if get (Window.pixel_density window) <> 1. then fail "dummy window pixel density changed";
  (match Window.set_size window ~width:0 ~height:72 with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok () | Error _ -> fail "invalid window resize was not rejected");
  (* the dummy driver reports some toggles as unsupported, with a typed error *)
  let unsupported_ok = function
    | Ok () -> ()
    | Error { kind = Sdl_error; message; _ } when message <> "" -> ()
    | Error _ -> fail "window toggle returned an untyped error"
  in
  unsupported_ok (Window.set_resizable window false);
  get (Window.sync window);
  unsupported_ok (Window.set_resizable window true);
  get (Window.sync window);
  (match Window.set_relative_mouse window true with
  | Ok () -> get (Window.set_relative_mouse window false)
  | Error { kind = Sdl_error | Unsupported; message; _ } when message <> "" -> ()
  | Error _ -> fail "relative mouse capability returned an untyped error");
  (* the named visibility facts come from SDL's own flags *)
  if not (get (Window.state window)).hidden then
    fail "a window created hidden did not report hidden";
  get (Window.show window);
  get (Window.sync window);
  let shown = get (Window.state window) in
  if shown.hidden || shown.minimized || shown.occluded then
    fail "a shown window still reports hidden, minimized or occluded";
  get (Window.hide window);
  get (Window.sync window);
  if not (get (Window.state window)).hidden then fail "hidden transition did not update the state";
  (* text input: a region with a cursor, then a stop *)
  get (Text_input.start window);
  get (Text_input.set_area window { x = 3; y = 4; width = 40; height = 16 } ~cursor:7);
  (match Text_input.set_area window { x = 0; y = 0; width = 0; height = 4 } ~cursor:0 with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok () | Error _ -> fail "an empty text-input area was accepted");
  (match Text_input.set_area window { x = 0; y = 0; width = 4; height = 4 } ~cursor:(-1) with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok () | Error _ -> fail "a negative text-input cursor was accepted");
  get (Text_input.stop window);
  get (Clipboard.set_text "Rays ž clipboard");
  if get (Clipboard.get_text ()) <> "Rays ž clipboard" then
    fail "clipboard UTF-8 text did not round-trip";
  (match Clipboard.set_text "bad\x00text" with
  | Error { kind = Invalid_argument; _ } -> ()
  | Ok () | Error _ -> fail "NUL clipboard text was accepted");
  List.iter
    (fun shape ->
      match Cursor.create shape with
      | Error { kind = Unsupported; message; _ } when message <> "" -> ()
      | Error _ -> fail "cursor capability returned an untyped error"
      | Ok cursor -> (
          get (Cursor.set cursor);
          get (Cursor.destroy cursor);
          get (Cursor.destroy cursor);
          match Cursor.set cursor with
          | Error { kind = Destroyed; _ } -> ()
          | Ok () | Error _ -> fail "destroyed cursor was accepted"))
    Cursor.[ Default; Text; Ew_resize; Ns_resize ];
  get (Hint.control_click_is_right_click true);
  get (Hint.control_click_is_right_click false);
  ignore (get (Event.poll_coalesced ()));
  (* a file dialog refuses bad arguments before SDL is asked for anything *)
  let refused label = function
    | Error { kind = Invalid_argument; _ } -> ()
    | Ok _ | Error _ -> fail (label ^ " was not refused as an invalid argument")
  in
  refused "a filter name with a NUL"
    (Dialog.show window ~filters:[ { Dialog.name = "a\x00b"; pattern = "*" } ] Dialog.Open_file);
  refused "an empty filter pattern"
    (Dialog.show window ~filters:[ { Dialog.name = "all"; pattern = "" } ] Dialog.Open_file);
  refused "a default location with a NUL"
    (Dialog.show window ~default_location:"/tmp\x00" Dialog.Save_file);
  refused "33 filters"
    (Dialog.show window
       ~filters:(List.init 33 (fun _ -> { Dialog.name = "x"; pattern = "*" }))
       Dialog.Open_files);
  (match Domain.spawn (fun () -> Dialog.show window Dialog.Open_folder) |> Domain.join with
  | Error { kind = Wrong_domain; _ } -> ()
  | Ok _ | Error _ -> fail "a dialog was not refused on a worker domain");
  let expect_wrong_domain label = function
    | Error { kind = Wrong_domain; _ } -> ()
    | Ok _ | Error _ -> fail (label ^ " was not rejected on a worker domain")
  in
  expect_wrong_domain "init" (Domain.spawn (fun () -> Init.init [ Init.Events ]) |> Domain.join);
  expect_wrong_domain "pixel density"
    (Domain.spawn (fun () -> Window.pixel_density window) |> Domain.join);
  expect_wrong_domain "window state" (Domain.spawn (fun () -> Window.state window) |> Domain.join);
  expect_wrong_domain "clipboard" (Domain.spawn (fun () -> Clipboard.get_text ()) |> Domain.join);
  expect_wrong_domain "text-input start"
    (Domain.spawn (fun () -> Text_input.start window) |> Domain.join);
  expect_wrong_domain "window synchronization"
    (Domain.spawn (fun () -> Window.sync window) |> Domain.join);
  expect_wrong_domain "hint"
    (Domain.spawn (fun () -> Hint.control_click_is_right_click true) |> Domain.join);
  expect_wrong_domain "event poll" (Domain.spawn Event.poll_coalesced |> Domain.join);
  (match
     Domain.spawn (fun () -> Window.create ~title:"wrong domain" ~width:8 ~height:8 ())
     |> Domain.join
   with
  | Error { kind = Wrong_domain; _ } -> ()
  | Ok window ->
      ignore (Window.destroy window);
      fail "wrong-domain create succeeded"
  | Error _ -> fail "wrong-domain create returned the wrong error");
  get (Window.destroy window);
  get (Window.destroy window);
  (match Window.size window with
  | Error { kind = Destroyed; _ } -> ()
  | Ok _ | Error _ -> fail "stale window access was not rejected");
  (match Dialog.show window Dialog.Open_file with
  | Error { kind = Destroyed; _ } -> ()
  | Ok _ | Error _ -> fail "a dialog was opened over a destroyed window");
  get (Init.quit_subsystems [ Init.Video; Init.Events ]);
  Printf.printf "SDL3 %d.%d.%d ownership test passed\n%!" linked.major linked.minor linked.patch
