type clock=Realtime|Fixed of float
type error=Ogpu.Error.t
let error_to_string=Ogpu.Error.to_string
type config={width:int;height:int;title:string;fps:int option;domains:int option;clock:clock;}
let default_config={width=800;height=600;title="Rays sketch";fps=Some 60;domains=None;clock=Realtime;}
type dialog=Open_file|Open_files|Save_file|Open_folder
exception Startup_error of Ogpu.Error.t
let startup_error operation kind message=raise(Startup_error(Ogpu.Error.make operation kind message))
type run_context={
  stopped:bool ref;
  input:Input_state.t;
  time:Time.t;
  caches:Scene.Private.cache_context;
  world_caches:World.Private.cache_context;
  native_caches:Native_scene_lowering.cache_context;
  mutable relative:(bool->(unit,string)result)option;
  mutable cursor:([`Default|`Horizontal_resize|`Vertical_resize|`Text]->(unit,string)result)option;
  mutable background:(float*float*float->(unit,string)result)option;
  mutable dialog : (?filters:(string * string list) list -> ?default_location:string ->
    dialog -> (int,string) result) option;
}
let active_run=Domain.DLS.new_key(fun()->None)
let current_run()=Domain.DLS.get active_run
let quit()=Option.iter(fun state->state.stopped:=true)(current_run())
let set_relative_mouse enabled=match current_run()with
  |None->Error"Sketch.set_relative_mouse: no sketch is running"
  |Some state->(match state.relative with None->Error"Sketch.set_relative_mouse: no sketch is running"|Some set->set enabled)
let set_cursor shape=match current_run()with
  |None->Error"Sketch.set_cursor: no sketch is running"
  |Some state->(match state.cursor with None->Error"Sketch.set_cursor: no sketch is running"|Some set->set shape)
let set_window_background color=match current_run()with
  |None->Error"Sketch.set_window_background: no sketch is running"
  |Some state->(match state.background with
    |None->Error"Sketch.set_window_background: no sketch is running"
    |Some set->let r,g,b,_=Rays_math.Color.to_tuple color in set(float r/.255.,float g/.255.,float b/.255.))
let show_file_dialog ?filters ?default_location kind=match current_run()with
  |None->Error"Sketch.show_file_dialog: no sketch is running"
  |Some state->(match state.dialog with None->Error"Sketch.show_file_dialog: no sketch is running"
    |Some show->show ?filters ?default_location kind)
let frame config input_state count time dt events =
  let input = { (Input.at_time time) with dt; frame = count; tick = count;
    size = config.width, config.height; pointer = Input_state.mouse input_state;
    buttons = Input_state.buttons input_state; keys = Input_state.keys input_state; events } in
  { Frame.input; pixel_scale = 1., 1.; fps = (if dt > 0. then 1. /. dt else 0.);
    mouse_delta = Input_state.mouse_delta input_state }

(* ---- crash reports: every fatal error in a sketch leaves a folder under
   /tmp/rays-crash (or RAYS_CRASH_DIR) with the exception, backtrace,
   recent input, GC and environment facts, and whatever the sketch dumps. *)
let crash_root()=Option.value(Sys.getenv_opt"RAYS_CRASH_DIR")
  ~default:(Filename.concat (Filename.get_temp_dir_name ()) "rays-crash")
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
  |Input.Key_pressed k->"press "^key_name k|Key_released k->"release "^key_name k
  |Pointer_moved(x,y)->Printf.sprintf"move %.0f,%.0f"x y
  |Pointer_pressed(b,(x,y))->Printf.sprintf"down %s %.0f,%.0f"(button_name b)x y
  |Pointer_released(b,(x,y))->Printf.sprintf"up %s %.0f,%.0f"(button_name b)x y
  |Pointer_cancelled b->"cancel "^button_name b
  |Scrolled(x,y)->Printf.sprintf"scroll %.2f,%.2f"x y
  |Text_input t->Printf.sprintf"text %S"t|Text_editing{text;_}->Printf.sprintf"ime %S"text
  |File_dialog{id;result=Ok paths}->Printf.sprintf"dialog %d %d"id(List.length paths)
  |File_dialog{id;result=Error message}->Printf.sprintf"dialog %d failed %S"id message
  |File_dropped f->"drop "^f|Resized(w,h)->Printf.sprintf"resize %dx%d"w h
  |File_drag_moved(x,y)->Printf.sprintf"drag %.0f,%.0f"x y|File_drag_ended->"drag-end"
  |Pinched scale->Printf.sprintf"pinch %.3f"scale
  |Trackpad_scrolled{delta=(x,y);phase;_}->Printf.sprintf"trackpad %s %.1f,%.1f"
    (match phase with Touched->"touch"|Moved->"move"|Lifted->"lift"|Momentum->"momentum")x y
  |Dialog_opened id->Printf.sprintf"dialog-opened %d"id
  |Pixel_size_changed(w,h)->Printf.sprintf"pixel-size %dx%d"w h
  |Visibility_changed visible->Printf.sprintf"visible %b"visible
  |Focus_lost->"focus-lost"|Focus_gained->"focus-gained"|Closed->"close"
let write_crash ~title ~dump ~recent exn backtrace=
  let tm=Unix.localtime(Unix.gettimeofday())in
  let name=Printf.sprintf"%s-%04d%02d%02d-%02d%02d%02d-%d"
    (String.map (function 'a'..'z' | 'A'..'Z' | '0'..'9' as c -> c | _ -> '_') title)
    (tm.tm_year+1900)(tm.tm_mon+1)tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec(Unix.getpid())in
  let directory=Filename.concat(crash_root())name in
  let write name text = match Runtime_resources.write_text ~filename:(Filename.concat directory name) text with
    | Ok () -> () | Error message -> failwith message in
  (try
    (match Runtime_resources.ensure_directory directory with Ok () -> () | Error message -> failwith message);
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
      if f.input.events=[]&&f.input.keys=[]&&f.input.buttons=[]then incr quiet
      else begin
        if !quiet>0 then(line"  … %d idle frames"!quiet;quiet:=0);
        line"  #%d t=%.3f %dx%d mouse=%.0f,%.0f keys=[%s] buttons=[%s] events=[%s]"
          f.input.frame f.input.t (fst f.input.size) (snd f.input.size)
          (fst f.input.pointer) (snd f.input.pointer)
          (String.concat" "(List.map key_name f.input.keys))
          (String.concat" "(List.map button_name f.input.buttons))
          (String.concat"; "(List.map event_text f.input.events))end)recent;
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
    write "crash.txt" (Buffer.contents buffer);
    (try dump directory with extra->
      write "dump-failed.txt" (Printexc.to_string extra));
    Printf.eprintf"Rays crash report: %s\n%!"directory
  with error->Printf.eprintf"Rays crash report could not be written: %s\n%!"
    (Printexc.to_string error))
let run_state_internal ?(config=default_config)?max_frames ?(high_density=true)?(after_present=fun model _->model)?(crash_dump=fun _ _->())~init~update~view ?(on_stop=fun _->())()=
  if config.width<=0||config.height<=0 then startup_error"Sketch.run_state"Ogpu.Error.Invalid_argument"dimensions must be positive";
  let max_frames=match max_frames,Sys.getenv_opt"RAYS_MAX_FRAMES"with
    |Some _,_|None,(None|Some"")->max_frames
    |None,Some text->(match int_of_string_opt text with Some _ as n->n|None->startup_error"Sketch.run_state"Ogpu.Error.Invalid_argument"RAYS_MAX_FRAMES must be an integer")in
  Option.iter(fun frames->if frames<=0 then startup_error"Sketch.run_state"Ogpu.Error.Invalid_argument"max_frames must be positive")max_frames;
  Option.iter(fun fps->if fps<=0 then startup_error"Sketch.run_state"Ogpu.Error.Invalid_argument"fps must be positive")config.fps;
  Option.iter(fun domains->if domains<=0 then startup_error"Sketch.run_state"Ogpu.Error.Invalid_argument"domains must be positive")config.domains;
  let vsync=match config.clock with Fixed _->false|Realtime->true in
  (match config.clock with Fixed dt when not(Float.is_finite dt&&dt>0.)->
    startup_error"Sketch.run_state"Ogpu.Error.Invalid_argument"fixed dt must be finite and positive"|_->());
  let input=match Input_state.create()with
    |Ok input->input|Error message->startup_error"Sketch.run_state"Ogpu.Error.Invalid_state("input: "^message)in
  let time=Time.create~target_fps:(Option.value config.fps~default:0)~vsync_enabled:vsync in
  let caches=Scene.Private.create_cache_context()in
  let world_caches=World.Private.create_cache_context()in
  let native_caches=Native_scene_lowering.create_cache_context()in
  let state={stopped=ref false;input;time;caches;world_caches;native_caches;relative=None;cursor=None;background=None;dialog=None}in
  let with_run_caches f=Scene.Private.with_cache_context caches(fun()->
    World.Private.with_cache_context world_caches f)in
  let first=frame config input 0 0. 0.[]in
  let configuration={Renderer.logical_width=config.width;logical_height=config.height;drawable_width=config.width;drawable_height=config.height;title=config.title;vsync;high_density}in
  let get=function Ok x->x|Error e->raise(Startup_error e)in
  let coordinator=get(Renderer.create configuration)in
  let runtime=Renderer.Private.runtime coordinator in
  (match Runtime.show runtime with
   |Ok()->()
   |Error error->ignore(Renderer.destroy coordinator);raise(Startup_error error));
  let initial_facts=match Runtime.presentation_facts runtime with
    |Ok facts->facts
    |Error error->ignore(Renderer.destroy coordinator);raise(Startup_error error)in
  (match Input_state.configure input~logical_width:initial_facts.logical_width
       ~logical_height:initial_facts.logical_height with
   |Ok()->()|Error message->ignore(Renderer.destroy coordinator);
     startup_error"Sketch.run_state"Ogpu.Error.Invalid_state("input: "^message));
  let logical_width=ref config.width and logical_height=ref config.height in
  let capture ()=
    let facts=get(Runtime.presentation_facts runtime)in
    Renderer.capture coordinator
    |>Result.map(fun bytes->facts.drawable_width,facts.drawable_height,bytes)
    |>Result.map_error(fun error->Format.asprintf"%a"Renderer.pp_error error)in
  let save filename=Result.bind(capture())(fun(width,height,bytes)->
        match Runtime_resources.Canvas.of_rgba_bytes~width~height~rgba:bytes with
        |Error error->Error(Format.asprintf"%a"Runtime_resources.pp_error error)
        |Ok canvas->Fun.protect~finally:(fun()->ignore(Runtime_resources.Canvas.destroy canvas))(fun()->
            Runtime_resources.Canvas.save_png canvas filename
            |>Result.map_error(fun error->Format.asprintf"%a"Runtime_resources.pp_error error)))in
  state.relative<-Some(fun enabled->
    match Runtime.set_relative_mouse runtime enabled with
    |Ok()->Input_state.set_relative input enabled;Ok()
    |Error error->Error(Format.asprintf"%a"Renderer.pp_error error));
  state.background<-Some(fun color->
    Runtime.set_window_background runtime color
    |>Result.map_error(fun error->Format.asprintf"%a"Renderer.pp_error error));
  state.cursor<-Some(fun shape->
    match Runtime.set_cursor runtime shape with
    |Ok()->Ok()
    |Error error->Error(Format.asprintf"%a"Renderer.pp_error error));
  state.dialog<-Some(fun ?(filters=[]) ?default_location kind->
    let filters=List.map(fun(name,extensions)->
      {Runtime.name;pattern=(match extensions with []->"*"|_->String.concat";"extensions)})filters in
    let kind=match kind with
      |Open_file->Runtime.Open_file|Open_files->Open_files
      |Save_file->Save_file|Open_folder->Open_folder in
    match Runtime.show_dialog runtime~filters?default_location kind with
    |Ok id->Ok id
    |Error error->Error(Format.asprintf"%a"Renderer.pp_error error));
  let render_scene scene=
    (* ponytail: scan scene metadata each frame; move the focused region into
       staged scene facts if large retained scenes make this measurable. *)
    let area=Scene.Private.text_regions scene
      |>List.find_opt(fun(_,_,_,_,focused,_)->focused)
      |>Option.map(fun(x,y,w,h,_,cursor)->(x,y,w,h),cursor)in
    get(Runtime.set_text_input runtime area);
    let facts=get(Runtime.presentation_facts runtime)in
    let density=
      let from_drawable=float facts.drawable_width/.float(max 1 facts.logical_width)in
      max 1(int_of_float(Float.round(max facts.pixel_density from_drawable)))in
    match Native_scene_lowering.with_cache_context state.native_caches (fun () ->
      Native_scene_lowering.render~execution:coordinator~density
        ~width:facts.logical_width~height:facts.logical_height scene) with
    |Ok _->()
    |Error error->
        startup_error"Sketch.render"Ogpu.Error.Invalid_state
          (Format.asprintf"%a"Native_scene_lowering.pp_error error)in
  Printexc.record_backtrace true;
  let recent=Array.make 120 first and cursor=ref 0 in
  let crashed exn=
    let backtrace=Printexc.get_raw_backtrace()in
    let frames=List.init(Array.length recent)(fun index->
      recent.((!cursor+index)mod Array.length recent))
      |>List.filter(fun(f:Frame.t)->f.input.frame>0)in
    exn,backtrace,frames in
  let release_after_failed_init () =
    (try Scene.Private.clear_cache_context caches with error ->
      Printf.eprintf "Sketch initialization cache cleanup failed: %s\n%!"
        (Printexc.to_string error));
    (try ignore (Renderer.destroy coordinator) with error ->
      Printf.eprintf "Sketch initialization renderer cleanup failed: %s\n%!"
        (Printexc.to_string error)) in
  let model=ref(try with_run_caches(fun()->init first) with
    |Startup_error _ as error->release_after_failed_init();raise error
    |exn->
    let exn,backtrace,frames=crashed exn in
    write_crash~title:config.title~dump:(fun _->())~recent:frames exn backtrace;
    release_after_failed_init();
    Printexc.raise_with_backtrace exn backtrace)in
  let last_scene=ref None in
  let cleanup()=Fun.protect~finally:(fun()->
      (* Never leave the pointer captured after the sketch stops. *)
      Option.iter(fun set->ignore(set false))state.relative;
      state.relative<-None;state.cursor<-None;state.background<-None;state.dialog<-None;
      Option.iter Scene.Private.release !last_scene;last_scene:=None;
      Scene.Private.clear_cache_context state.caches;
      ignore(Renderer.destroy coordinator))(fun()->on_stop!model)in
  let previous=current_run()in
  Scene.Private.with_renderer render_scene(fun()->Canvas_runtime.with_save save (fun()->Fun.protect~finally:(fun()->Domain.DLS.set active_run previous;cleanup())(fun()->
    Domain.DLS.set active_run(Some state);
    let profile=Sys.getenv_opt"RAYS_PROFILE"<>None in
    let limit=max_frames in let count=ref 0 in
    let shown=ref None and quiet=ref 0 in
    let one_frame()=
      Time.update time;
      let polled=match Input_state.poll input with
        |Ok polled->polled
        |Error message->startup_error"Sketch.input"Ogpu.Error.Invalid_state message in
      let events=polled.events in
      (* SDL announces a size or density change; the window is not polled. *)
      if polled.window_changed then Runtime.window_changed runtime;
      (* A covered window has no end event: while it is hidden, ask once a
         frame whether it is back. *)
      let visible=polled.visible||(match Runtime.visible runtime with
        |Ok true->Input_state.set_visible input true;true
        |Ok false|Error _->false)in
      if List.exists(function Input.Closed->true|_->false)events then state.stopped:=true;
      incr count;let dt=match config.clock with Realtime->Time.delta time|Fixed value->value in
      let base=frame config input !count(match config.clock with Realtime->Time.now time|Fixed _->float !count*.dt)dt events in
      let presentation=get(Runtime.presentation_facts runtime)in
      if presentation.logical_width<> !logical_width
          ||presentation.logical_height<> !logical_height then
        (match Input_state.configure input~logical_width:presentation.logical_width
          ~logical_height:presentation.logical_height with
         |Ok()->()|Error message->startup_error"Sketch.input"Ogpu.Error.Invalid_state message);
      logical_width:=presentation.logical_width;logical_height:=presentation.logical_height;
      let scale_x=float presentation.drawable_width/.float presentation.logical_width
      and scale_y=float presentation.drawable_height/.float presentation.logical_height in
      let input = { base.input with size = presentation.logical_width, presentation.logical_height } in
      let facts = { base with input; pixel_scale = scale_x, scale_y } in
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
        if profile&& !count mod 30=0 then(match Renderer.stats coordinator with
          |Ok s->Printf.eprintf"profile frame %d: gpu %.2f ms, draws %Ld, passes %Ld, uploaded %Ld B, sun passes %Ld, plan hits %Ld misses %Ld\n%!"
            !count(s.gpu_duration_seconds*.1000.)s.logical_draws s.logical_passes s.uploaded_bytes s.sun_shadow_passes
            s.retained_plan_hits s.retained_plan_misses
          |Error _->())
      with
      |Startup_error _ as error->raise error
      |exn->
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
        try Runtime.window_changed runtime;Option.iter Scene.render !last_scene
        with exn->
          match exn with
          |Startup_error _->live_failure:=Some exn
          |_->let exn,backtrace,frames=crashed exn in
            write_crash~title:config.title~dump:(crash_dump !model)~recent:frames exn backtrace;
            live_failure:=Some exn);
    Fun.protect~finally:(fun()->Callback.register"rays_sdl3_live_redraw"(fun()->()))(fun()->
      while not !(state.stopped)&&Option.fold~none:true~some:(fun limit-> !count<limit)limit do
        Option.iter raise !live_failure;
        with_run_caches one_frame;
        (* an unchanged frame skipped vsync: wait a frame for input, and after half a second of
           the same picture up to 100 ms, so a still window costs a few ticks a second *)
        if !quiet>0 then Input_state.wait input~timeout_ms:(if !quiet>30 then 100 else 16)
        else Time.limit_frame_rate time done;!model))))
let run_state ?config ?max_frames ~init~update~view ?after_present ?crash_dump ?on_stop()=
  try Ok(Parallel.run ?domains:(Option.bind config(fun value->value.domains))(fun()->
    run_state_internal?config?max_frames?after_present?crash_dump~init~update~view?on_stop()))
  with Startup_error error->Error error
let export_state ?(config=default_config)?(fps=60)?(prefix="frame")~directory~frames~init~update~view ?(after_present=fun model _->model) ?(on_stop=fun _->())()=
  try
  if frames<=0 then startup_error"Sketch.export_state"Ogpu.Error.Invalid_argument"frames must be positive";
  if fps<=0 then startup_error"Sketch.export_state"Ogpu.Error.Invalid_argument"fps must be positive";
  if prefix=""||prefix="."||prefix=".."||Filename.basename prefix<>prefix then
    startup_error"Sketch.export_state"Ogpu.Error.Invalid_argument"invalid prefix";
  let index=ref 0 in
  let after_export=after_present in
  let after_present model frame=let filename=Filename.concat directory(Printf.sprintf"%s-%06d.png"prefix !index)in match Canvas.save_screen_png filename with
    |Ok()->incr index;after_export model frame
    |Error message->startup_error"Sketch.export_state"Ogpu.Error.Invalid_state("frame export failed: "^message)in
  let config={config with clock=Fixed(1./.float fps);fps=None}in
  Ok(Parallel.run ?domains:config.domains(fun()->run_state_internal~config~max_frames:frames~high_density:false~after_present~init~update~view~on_stop()))
  with Startup_error error->Error error
let export ?config ?prefix ~directory ~frames view=
  Result.map(fun _->())(export_state?config?prefix~directory~frames~init:(fun _->())~update:(fun()_->())~view:(fun()frame->view frame)())
