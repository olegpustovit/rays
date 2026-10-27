module System_thread = Thread

open Sdl3

let fail message = failwith ("SDL3 Metal test: " ^ message)
let get = function Ok value -> value | Error error -> fail (Format.asprintf "%a" pp_error error)
let rec drain_events () = match get (Event.poll_coalesced ()) with
  | [] -> () | _ :: _ -> drain_events ()

external post_scroll : int -> int -> unit = "caml_sdl3_test_post_scroll"

(* a trackpad gesture posted as AppKit delivers it reaches the poll with its phases and its points *)
let scroll_phases () =
  drain_events ();
  List.iter (fun (points, phase) -> post_scroll points phase) [ 0, 1; -7, 2; 0, 4 ];
  let deadline = Unix.gettimeofday () +. 5. in
  let rec collect seen =
    let seen = seen @ List.filter_map (function
      | Event.Scroll { y; phase; _ } -> Some (phase, y) | _ -> None) (get (Event.poll_coalesced ())) in
    if List.length seen >= 3 || Unix.gettimeofday () >= deadline then seen
    else (System_thread.delay 0.01; collect seen) in
  match collect [] with
  | [ Event.Scroll_began, _; Event.Scroll_changed, y; Event.Scroll_ended, _ ] when Float.abs y = 7. -> ()
  | seen -> fail (Printf.sprintf "the trackpad monitor delivered %d of 3 phased scrolls%s" (List.length seen)
      (String.concat "" (List.map (fun (_, y) -> Printf.sprintf " %.1f" y) seen)))

let run () =
  if Sys.os_type <> "Unix" || not (Sys.file_exists "/System/Library/Frameworks/Metal.framework")
  then Printf.printf "SDL3 Metal bridge skipped on this platform\n%!"
  else begin
    get (Init.init [Init.Video; Init.Events]);
    let window = get (Window.create ~title:"SDL3 Metal bridge test"
        ~width:64 ~height:48
        ~flags:[Window.Hidden; Window.High_pixel_density; Window.Metal] ()) in
    let logical_width, logical_height = get (Window.size window) in
    let pixel_width, pixel_height = get (Window.size_in_pixels window) in
    let density = get (Window.pixel_density window) in
    if abs_float ((float_of_int pixel_width /. float_of_int logical_width)
        -. density) > 0.01
        || abs_float ((float_of_int pixel_height /. float_of_int logical_height)
          -. density) > 0.01 then
      fail "logical/drawable sizes disagree with the reported pixel density";
    get (Window.set_size window ~width:80 ~height:60);
    get (Window.sync window);
    if get (Window.size window) <> (80, 60) then
      fail "native synchronized resize changed logical dimensions";
    let resized_pixel_width, resized_pixel_height =
      get (Window.size_in_pixels window)
    in
    if abs_float ((float_of_int resized_pixel_width /. 80.) -. density) > 0.01
        || abs_float ((float_of_int resized_pixel_height /. 60.) -. density)
          > 0.01 then
      fail "native resize double-scaled logical dimensions";
    if density <= 0. then fail "native window pixel density is invalid";
    get (Window.set_resizable window false);
    get (Window.set_resizable window true);
    get (Window.center window);
    (* the title bar's ground: the Cocoa calls run on a real NSWindow here *)
    get (Window.set_background window ~red:0.95 ~green:0.95 ~blue:0.93);
    scroll_phases ();
    List.iter (fun shape ->
      let cursor = get (Cursor.create shape) in
      get (Cursor.set cursor);
      get (Cursor.destroy cursor))
      Cursor.[Default; Text; Ew_resize; Ns_resize];
    let await_hidden ~label expected =
      let deadline = Unix.gettimeofday () +. 30. in
      let rec loop () =
        drain_events ();
        if (get (Window.state window)).hidden = expected then ()
        else if Unix.gettimeofday () >= deadline then
          fail (label ^ " did not reach the requested native window state")
        else begin
          System_thread.delay 0.01;
          loop ()
        end
      in
      loop ()
    in
    await_hidden ~label:"initially hidden" true;
    get (Window.show window);
    get (Window.sync window);
    await_hidden ~label:"show" false;
    get (Window.hide window);
    get (Window.sync window);
    await_hidden ~label:"hide" true;
    let view = get (Metal_view.create window) in
    let token = get (Metal_view.layer view) in
    if not (Native_layer_token.alive token) || get (Metal_view.layer view) != token then
      fail "presentation token was not live and stable";
    (match Domain.spawn (fun () -> Metal_view.layer view) |> Domain.join with
     | Error { kind = Wrong_domain; _ } -> ()
     | Ok _ | Error _ -> fail "wrong-domain Metal view access was not rejected");
    (match Window.destroy window with
     | Error { kind = Parent_has_dependents; _ } -> ()
     | Ok () | Error _ -> fail "parent teardown ignored a live Metal view");
    get (Metal_view.destroy view);
    get (Metal_view.destroy view);
    if Native_layer_token.alive token then
      fail "destroyed Metal view left a live presentation token";
    (match Metal_view.layer view with
     | Error { kind = Destroyed; _ } -> ()
     | Ok _ | Error _ -> fail "destroyed Metal view remained usable");
    get (Window.destroy window);
    get (Init.quit_subsystems [Init.Video; Init.Events]);
    Printf.printf "SDL3 CAMetalLayer ownership bridge passed\n%!"
  end
