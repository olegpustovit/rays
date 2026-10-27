(* An OCaml program has no Cocoa run loop, so nothing drains an autorelease
   pool the native stubs did not open themselves: an object autoreleased
   outside one stays retained for the life of the process. Before this test
   that kept every window (through the title bar restyling) and every command
   buffer, encoder, drawable and written buffer of every frame.

   The Objective-C runtime reports each such object when
   OBJC_DEBUG_MISSING_POOLS is set. The test runs window lifecycles and frames
   in a child with that variable and requires the initial thread to report
   none. System worker threads report their own and are not counted. *)

external thread_tag : unit -> int64 = "rays_test_thread_tag"

let get = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)

let create width height =
  Renderer.create {Renderer.default_configuration with logical_width=width;logical_height=height;
    drawable_width=width;drawable_height=height}
let adopt_draws draws=List.map(Renderer.prepared_draw ~family:Renderer.Scene2)draws

let draws extent =
  let vertices = Bytes.make 48 '\000' and indices = Bytes.make 12 '\000' in
  Bytes.set_int32_le indices 4 1l;
  Bytes.set_int32_le indices 8 2l;
  List.iteri (fun vertex (x, y) ->
      let offset = vertex * 16 in
      Bytes.set_int32_le vertices offset (Int32.bits_of_float x);
      Bytes.set_int32_le vertices (offset + 4) (Int32.bits_of_float y);
      Bytes.set_int32_le vertices (offset + 8) 0x4080BFFFl)
    [ (-1., 1.); (1., 1.); (-1., -1.) ];
  [ { Scene_execution.mesh =
        { key = "autorelease"; vertices; vertex_count = 3; indices; index_count = 3;
          primitive = Ogpu.Render_pass.Triangle_list };
      state =
        { viewport = (0, 0, extent, extent); scissor = (0, 0, extent, extent);
          cull = Ogpu.Render_pass.Cull_none; depth_compare = Ogpu.Render_pass.Always;
          depth_write = false; depth_load = Ogpu.Render_pass.Clear; depth_clear = 1.;
          transform_uniforms = None} } ]

let lifecycle () =
  let runtime = get (create 4 4) in
  let clear = (0., 0., 0., 0.) in
  get (Runtime.set_window_background (Renderer.Private.runtime runtime) (0.1, 0.2, 0.3));
  get (Runtime.set_cursor (Renderer.Private.runtime runtime) `Text);
  get (Runtime.set_text_input (Renderer.Private.runtime runtime) (Some ((0, 0, 4, 4), 0)));
  for _ = 1 to 8 do
    ignore (Sdl3.Event.poll_coalesced ());
    ignore (get (Renderer.draw ~clear runtime (adopt_draws(draws 4))))
  done;
  let facts = get (Runtime.presentation_facts (Renderer.Private.runtime runtime)) in
  ignore (get (Renderer.read_pixels runtime ~bytes_per_row:(facts.drawable_width * 4)));
  get (Runtime.resize (Renderer.Private.runtime runtime) ~width:8 ~height:8);
  ignore (get (Renderer.draw ~clear runtime (adopt_draws(draws 8))));
  ignore (get (Runtime.presentation_facts (Renderer.Private.runtime runtime)));
  get (Runtime.set_text_input (Renderer.Private.runtime runtime) None);
  get (Renderer.destroy runtime)

let offscreen () =
  let runtime =
    get (Renderer.create_offscreen {Renderer.default_configuration with logical_width=4;
      logical_height=4;drawable_width=8;drawable_height=8}) in
  for _ = 1 to 4 do
    ignore (get (Renderer.draw ~clear:(0., 0., 0., 0.) runtime (adopt_draws(draws 8))))
  done;
  ignore (get (Renderer.read_pixels runtime ~bytes_per_row:32));
  get (Renderer.destroy runtime)

let child () =
  Printf.eprintf "initial thread 0x%Lx\n%!" (thread_tag ());
  for _ = 1 to 2 do lifecycle (); offscreen () done

let contains text part =
  let n = String.length text and m = String.length part in
  let rec at i = i + m <= n && (String.sub text i m = part || at (i + 1)) in
  at 0

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--child" then child ()
  else begin
    let environment =
      Array.append [| "OBJC_DEBUG_MISSING_POOLS=YES" |] (Unix.environment ()) in
    let log = Filename.temp_file "rays-autorelease" ".log" in
    let output = Unix.openfile log [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
    let pid =
      Unix.create_process_env Sys.executable_name
        [| Sys.executable_name; "--child" |] environment Unix.stdin Unix.stdout output in
    Unix.close output;
    let _, status = Unix.waitpid [] pid in
    let lines = In_channel.with_open_text log In_channel.input_lines in
    Sys.remove log;
    if status <> Unix.WEXITED 0 then begin
      List.iter prerr_endline lines;
      failwith "autorelease child failed"
    end;
    let tag =
      match List.find_opt (fun line -> contains line "initial thread 0x") lines with
      | Some line -> String.sub line 15 (String.length line - 15)
      | None -> failwith "autorelease child did not report its thread" in
    let unpooled =
      List.filter (fun line -> contains line ("MISSING POOLS: (" ^ tag ^ ")")) lines in
    if unpooled <> [] then begin
      List.iter prerr_endline unpooled;
      failwith
        (Printf.sprintf
           "%d objects were autoreleased on the initial thread with no pool in place"
           (List.length unpooled))
    end;
    print_endline "runtime autorelease: the initial thread autoreleases nothing outside a pool"
  end
