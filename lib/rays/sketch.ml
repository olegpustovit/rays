type clock=Realtime|Fixed of float
type config={width:int;height:int;title:string;fps:int option;domains:int option;clock:clock;}
let default_config={width=800;height=600;title="Rays sketch";fps=Some 60;domains=None;clock=Realtime;}
let stopped=ref false let quit()=stopped:=true
let relative_current : (bool -> (unit, string) result) option ref = ref None
let cursor_current : ([`Default|`Horizontal_resize|`Vertical_resize|`Text] ->
  (unit,string) result) option ref = ref None
let set_relative_mouse enabled = match !relative_current with
  |None->Error"Sketch.set_relative_mouse: no sketch is running"
  |Some set->set enabled
let set_cursor shape = match !cursor_current with
  |None->Error"Sketch.set_cursor: no sketch is running"
  |Some set->set shape
let background_current : (float*float*float -> (unit,string) result) option ref = ref None
let set_window_background color = match !background_current with
  |None->Error"Sketch.set_window_background: no sketch is running"
  |Some set->let r,g,b,_=Color.to_tuple color in set(float r/.255.,float g/.255.,float b/.255.)
type dialog=Open_file|Open_files|Save_file|Open_folder
let dialog_current : (?filters:(string*string list)list-> ?default_location:string->dialog->
  (int,string)result)option ref=ref None
let show_file_dialog ?filters ?default_location kind=match !dialog_current with
  |None->Error"Sketch.show_file_dialog: no sketch is running"
  |Some show->show ?filters ?default_location kind
let frame config count time dt events={Frame.width=config.width;height=config.height;size=(config.width,config.height);pixel_scale=(1.,1.);time;dt;fps=(if dt > 0. then 1. /. dt else 0.);count;mouse=Input_state.mouse();mouse_delta=Input_state.mouse_delta();keys=Input_state.keys();mouse_buttons=Input_state.buttons();events}

(* ---- crash reports: every fatal error in a sketch leaves a folder under
   /tmp/rays-crash (or RAYS_CRASH_DIR) with the exception, backtrace,
   recent input, GC and environment facts, and whatever the sketch dumps. *)
let crash_root()=Option.value(Sys.getenv_opt"RAYS_CRASH_DIR")~default:"/tmp/rays-crash"
let key_name=function
  |Input.KeyChar c->Printf.sprintf"%C"c|ArrowUp->"Up"|ArrowDown->"Down"|ArrowLeft->"Left"
  |ArrowRight->"Right"|Space->"Space"|Enter->"Enter"|Escape->"Escape"|Backspace->"Backspace"
  |Tab->"Tab"|Shift->"Shift"|Ctrl->"Ctrl"|Alt->"Alt"|Meta->"Meta"|Home->"Home"|End->"End"
  |PageUp->"PageUp"|PageDown->"PageDown"|Insert->"Insert"|Delete->"Delete"
  |Unknown code->Printf.sprintf"key%d"code|key->Printf.sprintf"F%d"(match key with
    |F1->1|F2->2|F3->3|F4->4|F5->5|F6->6|F7->7|F8->8|F9->9|F10->10|F11->11|_->12)
let button_name=function Input.LeftButton->"left"|RightButton->"right"|MiddleButton->"middle"
  |MouseX1->"x1"|MouseX2->"x2"
let event_text=function
  |Event.KeyPressed k->"press "^key_name k|KeyReleased k->"release "^key_name k
  |MouseMoved(x,y)->Printf.sprintf"move %.0f,%.0f"x y
  |MousePressed(b,(x,y))->Printf.sprintf"down %s %.0f,%.0f"(button_name b)x y
  |MouseReleased(b,(x,y))->Printf.sprintf"up %s %.0f,%.0f"(button_name b)x y
  |PointerCancelled b->"cancel "^button_name b
  |MouseScrolled(x,y)->Printf.sprintf"scroll %.2f,%.2f"x y
  |TextInput t->Printf.sprintf"text %S"t|TextEditing{text;_}->Printf.sprintf"ime %S"text
  |FileDialog{id;result=Ok paths}->Printf.sprintf"dialog %d %d"id(List.length paths)
  |FileDialog{id;result=Error message}->Printf.sprintf"dialog %d failed %S"id message
  |FileDropped f->"drop "^f|WindowResized(w,h)->Printf.sprintf"resize %dx%d"w h
  |FileDragMoved(x,y)->Printf.sprintf"drag %.0f,%.0f"x y|FileDragEnded->"drag-end"
  |MousePinched scale->Printf.sprintf"pinch %.3f"scale
  |TrackpadScrolled{delta=(x,y);phase;_}->Printf.sprintf"trackpad %s %.1f,%.1f"
    (match phase with Touched->"touch"|Moved->"move"|Lifted->"lift"|Momentum->"momentum")x y
  |WindowFocusLost->"focus-lost"|WindowFocusGained->"focus-gained"|WindowClosed->"close"
let write_crash ~title ~dump ~recent exn backtrace=
  let tm=Unix.localtime(Unix.gettimeofday())in
  let name=Printf.sprintf"%s-%04d%02d%02d-%02d%02d%02d-%d"
    (String.map (function 'a'..'z' | 'A'..'Z' | '0'..'9' as c -> c | _ -> '_') title)
    (tm.tm_year+1900)(tm.tm_mon+1)tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec(Unix.getpid())in
  let directory=Filename.concat(crash_root())name in
  let rec mkdir path=if not(Sys.file_exists path)then(mkdir(Filename.dirname path);
    try Sys.mkdir path 0o755 with Sys_error _->())in
  (try
    mkdir directory;
    let buffer=Buffer.create 4096 in
    let line fmt=Printf.bprintf buffer(fmt^^"\n")in
    line"Rays crash report";line"sketch: %s"title;
    line"time: %04d-%02d-%02d %02d:%02d:%02d"(tm.tm_year+1900)(tm.tm_mon+1)tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec;
    line"command: %s"(String.concat" "(Array.to_list Sys.argv));
    line"ocaml: %s  pid: %d"Sys.ocaml_version(Unix.getpid());
    line"";line"exception: %s"(Printexc.to_string exn);line"";line"backtrace:";
    Buffer.add_string buffer(Printexc.raw_backtrace_to_string backtrace);line"";
    line"recent frames (oldest first; frames without input are counted, not listed):";
    let quiet=ref 0 in
    List.iter(fun(f:Frame.t)->
      if f.events=[]&&f.keys=[]&&f.mouse_buttons=[]then incr quiet
      else begin
        if !quiet>0 then(line"  … %d idle frames"!quiet;quiet:=0);
        line"  #%d t=%.3f %dx%d mouse=%.0f,%.0f keys=[%s] buttons=[%s] events=[%s]"f.count f.time
          f.width f.height(fst f.mouse)(snd f.mouse)
          (String.concat" "(List.map key_name f.keys))
          (String.concat" "(List.map button_name f.mouse_buttons))
          (String.concat"; "(List.map event_text f.events))end)recent;
    if !quiet>0 then line"  … %d idle frames"!quiet;
    let gc=Gc.quick_stat()in
    line"";line"gc: heap %d words, top %d words, minor %d, major %d, compactions %d"
      gc.heap_words gc.top_heap_words gc.minor_collections gc.major_collections gc.compactions;
    line"";line"environment:";
    Array.iter(fun entry->if String.starts_with~prefix:"RAYS_"entry
      ||String.starts_with~prefix:"SDL_"entry||String.starts_with~prefix:"OCAMLRUNPARAM"entry
      then line"  %s"entry)(Unix.environment());
    line"";line"native crashes (segfaults, GPU faults) are not caught here: macOS writes";
    line"~/Library/Logs/DiagnosticReports/%s-*.ips"(Filename.basename Sys.executable_name);
    Out_channel.with_open_text(Filename.concat directory"crash.txt")(fun channel->
      Out_channel.output_string channel(Buffer.contents buffer));
    (try dump directory with extra->
      Out_channel.with_open_text(Filename.concat directory"dump-failed.txt")(fun channel->
        Out_channel.output_string channel(Printexc.to_string extra)));
    Printf.eprintf"Rays crash report: %s\n%!"directory
  with error->Printf.eprintf"Rays crash report could not be written: %s\n%!"
    (Printexc.to_string error))
let run_state_internal ?(config=default_config)?max_frames ?(high_density=true)?(after_present=fun model _->model)?(crash_dump=fun _ _->())~init~update~view ?(on_stop=fun _->())()=
  if config.width<=0||config.height<=0 then invalid_arg"Sketch: dimensions must be positive";
  let max_frames=match max_frames,Sys.getenv_opt"RAYS_MAX_FRAMES"with
    |Some _,_|None,(None|Some"")->max_frames
    |None,Some text->(match int_of_string_opt text with Some _ as n->n|None->invalid_arg"Sketch: RAYS_MAX_FRAMES must be an integer")in
  Option.iter(fun frames->if frames<=0 then invalid_arg"Sketch: max_frames must be positive")max_frames;
  Option.iter(fun fps->if fps<=0 then invalid_arg"Sketch: fps must be positive")config.fps;
  Option.iter(fun domains->if domains<=0 then invalid_arg"Sketch: domains must be positive")config.domains;
  stopped:=false;Time.init();
  Time.set_frame_rate(Option.value config.fps~default:0);
  let vsync=match config.clock with Fixed _->false|Realtime->true in
  Time.set_vsync vsync;
  let first=frame config 0 0. 0.[]in
  (match config.clock with Fixed dt when not(Float.is_finite dt&&dt>0.)->
    invalid_arg"fixed dt must be finite and positive"|_->());
  let configuration={Rays_execution.logical_width=config.width;logical_height=config.height;drawable_width=config.width;drawable_height=config.height;title=config.title;vsync;high_density}in
  let get=function Ok x->x|Error e->failwith(Format.asprintf"%a"Rays_execution.pp_error e)in
  let coordinator=get(Rays_execution.create configuration)in
  get(Rays_execution.show coordinator);
  (match Rays_execution.presentation_facts coordinator with
   |Ok facts->Input_state.configure~logical_width:facts.logical_width
       ~logical_height:facts.logical_height
   |Error _->());
  let logical_width=ref config.width and logical_height=ref config.height in
  let capture ()=
    let facts=get(Rays_execution.presentation_facts coordinator)in
    Rays_execution.capture coordinator
    |>Result.map(fun bytes->facts.drawable_width,facts.drawable_height,bytes)
    |>Result.map_error(fun error->Format.asprintf"%a"Rays_execution.pp_error error)in
  let save filename=Result.bind(capture())(fun(width,height,bytes)->
        match Runtime_resources.Canvas.create~width~height with
        |Error error->Error(Format.asprintf"%a"Runtime_resources.pp_error error)
        |Ok canvas->Fun.protect~finally:(fun()->ignore(Runtime_resources.Canvas.destroy canvas))(fun()->
            for y=0 to height-1 do for x=0 to width-1 do let o=(y*width+x)*4 in
              let packed=Int32.logor(Int32.shift_left(Int32.of_int(Char.code(Bytes.get bytes o)))24)(Int32.logor(Int32.shift_left(Int32.of_int(Char.code(Bytes.get bytes(o+1))))16)(Int32.logor(Int32.shift_left(Int32.of_int(Char.code(Bytes.get bytes(o+2))))8)(Int32.of_int(Char.code(Bytes.get bytes(o+3))))))in
              Runtime_resources.Canvas.set_pixel canvas~x~y packed|>Result.get_ok done done;
            Runtime_resources.Canvas.save_png canvas filename
            |>Result.map_error(fun error->Format.asprintf"%a"Runtime_resources.pp_error error)))in
  Canvas_runtime.install~capture~save;
  relative_current:=Some(fun enabled->
    match Rays_execution.set_relative_mouse coordinator enabled with
    |Ok()->Input_state.set_relative enabled;Ok()
    |Error error->Error(Format.asprintf"%a"Rays_execution.pp_error error));
  background_current:=Some(fun color->
    Rays_execution.set_window_background coordinator color
    |>Result.map_error(fun error->Format.asprintf"%a"Rays_execution.pp_error error));
  cursor_current:=Some(fun shape->
    match Rays_execution.set_cursor coordinator shape with
    |Ok()->Ok()
    |Error error->Error(Format.asprintf"%a"Rays_execution.pp_error error));
  dialog_current:=Some(fun ?(filters=[]) ?default_location kind->
    let filters=List.map(fun(name,extensions)->
      {Rays_execution.name;pattern=(match extensions with []->"*"|_->String.concat";"extensions)})filters in
    let kind=match kind with
      |Open_file->Rays_execution.Open_file|Open_files->Open_files
      |Save_file->Save_file|Open_folder->Open_folder in
    match Rays_execution.show_dialog coordinator~filters?default_location kind with
    |Ok id->Ok id
    |Error error->Error(Format.asprintf"%a"Rays_execution.pp_error error));
  Scene.Private.install_renderer(fun scene->
    (* ponytail: scan scene metadata each frame; move the focused region into
       staged scene facts if large retained scenes make this measurable. *)
    let area=Scene.Private.text_regions scene
      |>List.find_opt(fun(_,_,_,_,focused,_)->focused)
      |>Option.map(fun(x,y,w,h,_,cursor)->(x,y,w,h),cursor)in
    get(Rays_execution.set_text_input coordinator area);
    let facts=get(Rays_execution.presentation_facts coordinator)in
    let density=
      let from_drawable=float facts.drawable_width/.float(max 1 facts.logical_width)in
      max 1(int_of_float(Float.round(max facts.pixel_density from_drawable)))in
    match Native_scene_lowering.render~execution:coordinator~density
      ~width:facts.logical_width~height:facts.logical_height scene with
    |Ok _->()
    |Error error->
        failwith(Format.asprintf"Sketch.render: %a"Native_scene_lowering.pp_error error));
  Printexc.record_backtrace true;
  let recent=Array.make 120 first and cursor=ref 0 in
  let crashed exn=
    let backtrace=Printexc.get_raw_backtrace()in
    let frames=List.init(Array.length recent)(fun index->
      recent.((!cursor+index)mod Array.length recent))
      |>List.filter(fun(f:Frame.t)->f.count>0)in
    exn,backtrace,frames in
  let model=ref(try init first with exn->
    let exn,backtrace,frames=crashed exn in
    write_crash~title:config.title~dump:(fun _->())~recent:frames exn backtrace;
    Printexc.raise_with_backtrace exn backtrace)in
  let cleanup()=Fun.protect~finally:(fun()->
      (* Never leave the pointer captured after the sketch stops. *)
      Option.iter(fun set->ignore(set false))!relative_current;
      relative_current:=None;cursor_current:=None;background_current:=None;dialog_current:=None;
      Canvas_runtime.clear();ignore(Rays_execution.destroy coordinator))(fun()->on_stop!model)in
  Fun.protect~finally:cleanup(fun()->
    let profile=Sys.getenv_opt"RAYS_PROFILE"<>None in
    let limit=max_frames in let count=ref 0 in
    let last_scene=ref None in
    let shown=ref None and quiet=ref 0 in
    let one_frame()=
      Time.update();
      let polled=match Input_state.poll()with
        |Ok polled->polled
        |Error message->failwith("Sketch: input: "^message)in
      let events=polled.events in
      (* SDL announces a size or density change; the window is not polled. *)
      if polled.window_changed then Rays_execution.window_changed coordinator;
      (* A covered window has no end event: while it is hidden, ask once a
         frame whether it is back. *)
      let visible=polled.visible||(match Rays_execution.visible coordinator with
        |Ok true->Input_state.set_visible true;true
        |Ok false|Error _->false)in
      if List.exists(function Event.WindowClosed->true|_->false)events then quit();
      incr count;let dt=match config.clock with Realtime->Time.delta()|Fixed value->value in
      let base=frame config !count(match config.clock with Realtime->Time.now()|Fixed _->float !count*.dt)dt events in
      let presentation=get(Rays_execution.presentation_facts coordinator)in
      if presentation.logical_width<> !logical_width
          ||presentation.logical_height<> !logical_height then
        Input_state.configure~logical_width:presentation.logical_width
          ~logical_height:presentation.logical_height;
      logical_width:=presentation.logical_width;logical_height:=presentation.logical_height;
      let scale_x=float presentation.drawable_width/.float presentation.logical_width
      and scale_y=float presentation.drawable_height/.float presentation.logical_height in
      let facts={base with width=presentation.logical_width;height=presentation.logical_height;
        size=(presentation.logical_width,presentation.logical_height);

        pixel_scale=(scale_x,scale_y)}in
      recent.(!cursor)<-facts;cursor:=(!cursor+1)mod Array.length recent;
      (try
        model:=update !model facts;
        let scene=view !model facts in
        last_scene:=Some scene;
        (* An interactive window that is minimized or covered is not drawn and
           the loop idles. A finite run (max_frames, RAYS_MAX_FRAMES, export)
           is a test or a capture: it always renders. A window whose picture did
           not change keeps the last one on screen, as a native view does: no
           draw, no present, and the loop waits for input instead of vsync. *)
        if limit<>None then Scene.render scene
        else if not visible then(shown:=None;Unix.sleepf 0.033)
        else if polled.window_changed||(match !shown with
            |Some last->not(Scene.Private.same last scene)|None->true)
        then(Scene.render scene;shown:=Some scene;quiet:=0)
        else incr quiet;
        model:=after_present !model facts;
        if profile&& !count mod 30=0 then(match Rays_execution.stats coordinator with
          |Ok s->Printf.eprintf"profile frame %d: gpu %.2f ms, draws %Ld, passes %Ld, uploaded %Ld B, sun passes %Ld, plan hits %Ld misses %Ld\n%!"
            !count(s.gpu_duration_seconds*.1000.)s.logical_draws s.logical_passes s.uploaded_bytes s.sun_shadow_passes
            s.retained_plan_hits s.retained_plan_misses
          |Error _->())
      with exn->
        let exn,backtrace,frames=crashed exn in
        write_crash~title:config.title~dump:(crash_dump !model)~recent:frames exn backtrace;
        Printexc.raise_with_backtrace exn backtrace);
    in
    (* A window edge is being dragged: the native pump is blocked and calls this.  It only draws
       the last picture at the new size (no poll, update, effects, autosave, reload or clock tick),
       and a failure is kept for the loop to raise.  Needs a Metal window and the SDL pump, so no
       window-free test drives it. *)
    let live_failure=ref None in
    Callback.register"rays_sdl3_live_redraw"(fun()->
      if limit=None&&Option.is_none !live_failure then
        try Rays_execution.window_changed coordinator;Option.iter Scene.render !last_scene
        with exn->
          let exn,backtrace,frames=crashed exn in
          write_crash~title:config.title~dump:(crash_dump !model)~recent:frames exn backtrace;
          live_failure:=Some exn);
    Fun.protect~finally:(fun()->Callback.register"rays_sdl3_live_redraw"(fun()->()))(fun()->
      while not !stopped&&Option.fold~none:true~some:(fun limit-> !count<limit)limit do
        Option.iter raise !live_failure;
        one_frame();
        (* an unchanged frame skipped vsync: wait a frame for input, and after half a second of
           the same picture up to 100 ms, so a still window costs a few ticks a second *)
        if !quiet>0 then Input_state.wait~timeout_ms:(if !quiet>30 then 100 else 16)
        else Time.limit_frame_rate()done;!model))
let run_state ?config ?max_frames ~init~update~view ?after_present ?crash_dump ?on_stop()=
  Parallel.run ?domains:(Option.bind config(fun value->value.domains))(fun()->run_state_internal?config?max_frames?after_present?crash_dump~init~update~view?on_stop())
let export_state ?(config=default_config)?(fps=60)?(prefix="frame")~directory~frames~init~update~view ?(after_present=fun model _->model) ?(on_stop=fun _->())()=
  if frames<=0 then invalid_arg"Sketch.export_state: frames must be positive";
  if fps<=0 then invalid_arg"Sketch.export_state: fps must be positive";
  if prefix=""||prefix="."||prefix=".."||Filename.basename prefix<>prefix then invalid_arg"Sketch.export_state: invalid prefix";
  Canvas_runtime.ensure_directory directory;let index=ref 0 in
  let after_export=after_present in
  let after_present model frame=let filename=Filename.concat directory(Printf.sprintf"%s-%06d.png"prefix !index)in match Canvas.save_screen_png filename with Ok()->incr index;after_export model frame|Error message->failwith("Frame export failed: "^message)in
  let config={config with clock=Fixed(1./.float fps);fps=None}in
  Parallel.run ?domains:config.domains(fun()->run_state_internal~config~max_frames:frames~high_density:false~after_present~init~update~view~on_stop())
let export ?config ?prefix ~directory ~frames view=ignore(export_state?config?prefix~directory~frames~init:(fun _->())~update:(fun()_->())~view:(fun()frame->view frame)())
