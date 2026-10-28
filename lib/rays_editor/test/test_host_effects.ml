module Editor = Rays_editor.Editor

let load text =
  match Rays_editor.Workspace.load text with
  | Ok doc -> doc
  | Error diagnostics -> failwith (String.concat "\n" (List.map Diagnostic.to_string diagnostics))

let frame ?(keys = []) ?(events = []) count : Rays.Frame.t =
  {
    input =
      {
        (Input.at_time (float count /. 60.)) with
        dt = 1. /. 60.;
        frame = count;
        tick = count;
        size = (640, 360);
        pointer = (-100., -100.);
        keys;
        events;
      };
    pixel_scale = (1., 1.);
    fps = 60.;
    mouse_delta = (0., 0.);
  }

let workspace effects =
  load
    ("(workspace host (graph picture :context draw (draw/background \"#111827\")) "
   ^ "(graph actions :context host " ^ effects ^ ") (graph editor :context editor "
   ^ "(ui/workspace (ui/canvas (ref picture)) :effects (ref actions))))")

let with_editor workspace run =
  let editor =
    ref
      (Result.get_ok
         (Editor.create ~ui_text_size:11 ~workspace ~await:true ~domains:1
            ~prepare:Editor_prepare.empty
            ~draw:(fun _ _ -> Rays.Scene.D3.empty)
            ()))
  in
  Fun.protect ~finally:(fun () -> Editor.close !editor) (fun () -> run editor)

let () =
  let doc = workspace "(list (host/quit (key/down \"q\")))" in
  with_editor doc (fun editor ->
      editor := Editor.update !editor (frame 0);
      assert (Editor.Private.host_stats !editor = (false, 0, 0, 0));
      for count = 1 to 2 do
        editor :=
          Editor.update !editor
            (frame ~keys:[ Input.KeyChar 'q' ]
               ~events:[ Input.Key_pressed (Input.KeyChar 'q') ]
               count)
      done;
      assert (Editor.Private.host_stats !editor = (true, 1, 0, 0));
      editor := Editor.update !editor (frame 3);
      editor := Editor.update !editor (frame ~keys:[ Input.KeyChar 'q' ] 4);
      assert (Editor.Private.host_stats !editor = (true, 2, 0, 0)));
  List.iter
    (fun effects ->
      assert (
        match
          Rays_editor.Workspace.export ~directory:"/tmp/rays-effects-refused" ~frames:1
            (workspace effects)
        with
        | Error diagnostic -> diagnostic.Diagnostic.code = "E_EFFECT_EXPORT"
        | Ok () -> false))
    [ "(list (host/quit true))"; "(list (host/dialog \"open_file\" true))" ];
  let doc =
    workspace
      "(list (host/play (audio/synth :frequency 440.0 :duration 0.01) (key/down \"a\")) (host/play \
       (audio/synth :frequency 440.0 :duration 0.01) (key/down \"a\")))"
  in
  let closed =
    with_editor doc (fun editor ->
        editor := Editor.update !editor (frame ~keys:[ Input.KeyChar 'a' ] 1);
        assert (Editor.Private.host_stats !editor = (false, 2, 1, 0));
        editor := Editor.update !editor (frame ~keys:[ Input.KeyChar 'a' ] 2);
        assert (Editor.Private.host_stats !editor = (false, 2, 1, 0));
        !editor)
  in
  assert (Editor.Private.host_stats closed = (false, 2, 1, 1));
  print_endline
    "host: synthetic key edge fires once, export rejects quit, two nodes share one sample, close \
     releases once"

let () =
  if Array.length Sys.argv > 1 then begin
    let directory = Sys.argv.(1) in
    let shot = directory ^ "/screenshot.png" in
    let effects = Printf.sprintf "(host/save_png %S (= (frame/index) 1))" shot in
    assert (Rays_editor.Workspace.export ~directory ~frames:3 (workspace effects) = Ok ());
    let read path = In_channel.with_open_bin path In_channel.input_all in
    assert (read shot = read (directory ^ "/frame-000001.png"));
    print_endline "host: fixed-step screenshot matches the requested presented export frame"
  end
