open Rays
module E=Flow.Eval
let ok=function Ok x->x|Error d->failwith(Diagnostic.to_string d)
let read path=In_channel.with_open_bin path In_channel.input_all
let names=["basic";"generative";"noise";"recursive_rectangles";"drawing";"audio";"file_dialog";"pxui"]
let dimensions=function "generative"->500,500|"noise"->800,450|"recursive_rectangles"->720,720
  |"drawing"->800,500|"file_dialog"->720,420|_->640,360
let frame ?(synthetic=true) name count =
  let width,height=dimensions name in
  let events=if not synthetic then []else match name,count with
    |"drawing",1->[Input.Pointer_pressed(Input.LeftButton,(100.,140.));Pointer_moved(120.,160.)]
    |"drawing",2->[Input.Pointer_moved(140.,180.)]
    |"drawing",3->[Input.Pointer_released(Input.LeftButton,(160.,190.))]
    |"recursive_rectangles",1->[Input.Pointer_pressed(Input.LeftButton,(100.,140.))]
    |"audio",1->[Input.Key_pressed(Input.KeyChar '3')]
    |"audio",3->[Input.Pointer_pressed(Input.LeftButton,(400.,180.))]
    |"file_dialog",1->[Input.File_dropped "sample.png";File_dialog{id=1;result=Ok["a.png";"b.png"]}]
    |"file_dialog",2->[Input.File_drag_moved(90.,140.);Pinched 1.25]
    |"file_dialog",3->[Input.File_drag_ended;File_dialog{id=2;result=Ok[]}]
    |_->[]in
  let mouse=if not synthetic then 0.,0. else if name="recursive_rectangles"then 100.,140. else -100.,-100. in
  { Frame.input = { (Input.at_time (float count /. 60.)) with dt = 1. /. 60.;
      frame = count; tick = count; size = width, height; pointer = mouse; events };
    pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. }
let load name=
  let source=read("../sketches/"^name^".rays")in
  let workspace=match Rays_editor.Workspace.load source with Ok w->w|Error ds->
    failwith(String.concat"\n"(List.map Diagnostic.to_string ds))in
  assert(Editor_document.Workspace_doc.to_text workspace=source);
  workspace
let save_roundtrip name workspace =
  let module Editor=Rays_editor.Editor in
  let source=read("../sketches/"^name^".rays")in
  let directory=Filename.temp_dir "rays-port-save-" ""in
  let rec remove directory=Array.iter(fun entry->let path=Filename.concat directory entry in
    if Sys.is_directory path then remove path else Sys.remove path)(Sys.readdir directory);
    Unix.rmdir directory in
  Fun.protect ~finally:(fun()->remove directory)(fun()->
    let file=Filename.concat directory "sketch.rays"in
    Out_channel.with_open_bin file(fun channel->output_string channel source);
    let editor=ref(Result.get_ok(Editor.create ~workspace ~await:true ~domains:1
      ~presets:(Filename.concat directory "presets")
      ~source:(Rays_editor.Source.at ~file ~digest:(Editor_document.Contexts.sha256 source))
      ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Scene3.empty)()))in
    Fun.protect ~finally:(fun()->Editor.close !editor)(fun()->
      editor:=Editor.update !editor(frame ~synthetic:false name 0);
      let inode=(Unix.stat file).st_ino in
      let input_frame = frame ~synthetic:false name 1 in
      let save_frame = { input_frame with input = { input_frame.input with keys = [Input.Meta];
        events = [Input.Key_pressed (Input.KeyChar 's')] } } in
      editor:=Editor.update !editor save_frame;
      editor:=Editor.update !editor(frame ~synthetic:false name 2);
      assert((Unix.stat file).st_ino<>inode);
      assert(read file=source);
      assert(Editor_document.Workspace_doc.to_text(Editor.workspace !editor)=source)));
  Printf.printf "port %s: Command-S atomically saves the unchanged source and comments\n%!"name
let modes=["lisp-a",1;"lisp-b",1;"cpu-1",1;"cpu-8",8]
let pictures ?(synthetic=true) ?(first=0) name workspace domains = Parallel.run ~domains(fun()->
  let evaluated=ok(E.static ~inputs:workspace.Editor_document.Workspace_doc.inputs workspace.checked)in
  let prepared=ok(Flow_native.Drawing.prepare ~states:evaluated.states evaluated.plan
    (List.assoc "picture" evaluated.results))in
  let state=E.create_state()in
  Array.init 4(fun count->let frame=frame ~synthetic name (first+count) in
    let live=Flow_native.Live_frame.of_frame frame in
    ok(Flow_native.Drawing.render_prepared ~state prepared ~live ~size:frame.input.size)))
let serialize scene=Scene.Private.commands scene |> Scene_command.Render_ir.create |> Result.get_ok
  |> Scene_command.Render_ir.serialize
let mode_parity ()=List.iter(fun name->
  let workspace=load name in
  let expected=Array.map serialize(pictures name workspace 1)in
  List.iter(fun(_,domains)->
    let actual=Array.map serialize(pictures name workspace domains)in
    assert(actual=expected))modes;
  Printf.printf "port %s: source roundtrip, four synthetic frames, four-mode command parity\n%!"name)names

(* Each *_oracle module is the example's OCaml main.ml at 3baf0095, up to its application
   entry point: the program the Lisp workspace was ported from, not code under test. *)
type oracle=Oracle : {name:string;init:Frame.t->'model;
  update:'model->Frame.t->'model;view:'model->Frame.t->Scene.t;stop:'model->unit}->oracle
let oracles=[
  Oracle{name="basic";init=Basic_oracle.init;update=Basic_oracle.update;view=Basic_oracle.view;stop=ignore};
  Oracle{name="generative";init=(fun _->0.);update=Generative_oracle.update;view=Generative_oracle.view;stop=ignore};
  Oracle{name="noise";init=Noise_oracle.init;update=Noise_oracle.update;view=Noise_oracle.view;stop=ignore};
  Oracle{name="recursive_rectangles";init=Recursive_rectangles_oracle.init;update=Recursive_rectangles_oracle.update;view=Recursive_rectangles_oracle.view;stop=ignore};
  Oracle{name="drawing";init=Drawing_oracle.init;update=Drawing_oracle.update;view=Drawing_oracle.view;stop=ignore};
  Oracle{name="audio";init=Audio_oracle.init;update=Audio_oracle.update;view=Audio_oracle.view;stop=Audio_oracle.stop};
  Oracle{name="file_dialog";init=File_dialog_oracle.init;update=File_dialog_oracle.update;view=File_dialog_oracle.view;stop=ignore};
  (* The inspector replaces the widget UI. Compare the drawing portion, and
     qualify the editor layout separately with ui_shot. *)
  Oracle{name="pxui";init=Pxui_oracle.init;update=Pxui_oracle.update;
    view=(fun model frame->match Pxui_oracle.view model frame with a::b::c::_->[a;b;c]|_->assert false);
    stop=Pxui_oracle.on_stop}]
(* Compare authored parameters with the original OCaml program without a GPU.
   Re-expand SDF instances through the legacy constructors; canonical triangle
   pairs ignore an equivalent rectangle's diagonal and packed command grouping.
   Native tests below still qualify the actual SDF rasterization. *)
let parameters ~size scene =
  let module R=Scene_command.Render_ir in
  let color bits=let byte shift=Int32.(to_int(logand(shift_right_logical bits shift)255l))in
    Rays_math.Color.rgba(byte 24)(byte 16)(byte 8)(byte 0)in
  let optional bits=if Int32.logand bits 255l=0l then None else Some(color bits)in
  let rec command = function
    |R.Clear bits->let width,height=size in
        commands(Scene.Private.commands[Scene.rect ~at:(0,0) ~w:width ~h:height ~fill:(color bits)()])
    |R.Shapes batch->
        let bytes=Scene_command.Ui_batch.instances batch in
        List.init(Scene_command.Ui_batch.count batch)(fun index->
          let word n=Bytes.get_int32_le bytes(index*64+n*4)in
          let f n=Int32.float_of_bits(word n)in
          let x0=f 0 and y0=f 1 and x1=f 2 and y1=f 3 in
          let fill=optional(word 8)and stroke=optional(word 9)in
          let node=if fill=None && stroke=None then Scene.empty else
            match word 10 with
            |0l->[Scene.rect ~at:(int_of_float x0,int_of_float y0)
              ~w:(int_of_float(x1-.x0)) ~h:(int_of_float(y1-.y0)) ?fill ?stroke()]
            |4l->[Scene.circle ~at:(int_of_float((x0+.x1)/.2.),int_of_float((y0+.y1)/.2.))
              ~radius:(int_of_float((x1-.x0)/.2.)) ?fill ?stroke()]
            |5l->[Scene.line ~from_:(int_of_float x0,int_of_float y0)
              ~to_:(int_of_float x1,int_of_float y1) ?color:fill ~width:(int_of_float(f 12))()]
            |_->assert false in
          commands(Scene.Private.commands node))|>List.concat
    |R.Geometry geometry when Int32.logand geometry.color 255l=0l->[]
    |R.Geometry geometry->
        let n=Array.length geometry.indices in
        List.init((n+5)/6)(fun pair->
          let first=pair*6 in
          let points=List.init(min 6(n-first))(fun i->
            let index=geometry.indices.(first+i)in geometry.vertices.(index*2),geometry.vertices.(index*2+1))
            |>List.sort_uniq compare in
          `Geometry(geometry.color,points))
    |R.Image image when image.destination.width=0. || image.destination.height=0.->[]
    |command->[`Command command]
  and commands input=Array.to_list input|>List.concat_map command in
  commands(Scene.Private.commands scene)
let pure ()=
  mode_parity();
  List.iter(fun name->save_roundtrip name(load name))names;
  List.iter(fun(Oracle port)->
    let workspace=load port.name in
    let scenes=pictures port.name workspace 1 in
    let model=ref(port.init(frame port.name 0))in
    Fun.protect ~finally:(fun()->port.stop !model)(fun()->
      Array.iteri(fun count scene->
        let frame=frame port.name count in
        model:=port.update !model frame;
        let expected=parameters ~size:frame.input.size(port.view !model frame)in
        let actual=parameters ~size:frame.input.size scene in
        if expected<>actual then begin
          let describe=function
            |`Geometry(color,points)->Printf.sprintf "geometry %lx %s" color
              (String.concat ","(List.map(fun(x,y)->Printf.sprintf "(%h,%h)" x y)points))
            |`Command(Scene_command.Render_ir.Image image)->Printf.sprintf "image %d at %h,%h size %h,%h"
              image.resource_id image.destination.x image.destination.y image.destination.width image.destination.height
            |`Command _->"state command"in
          let rec mismatch i expected actual=match expected,actual with
            |x::xs,y::ys when x=y->mismatch(i+1)xs ys
            |x::_,y::_->Printf.sprintf "at %d expected %s actual %s" i(describe x)(describe y)
            |_->"length"in
          failwith(Printf.sprintf "OCaml parameter parity: %s frame %d (%d/%d commands), %s"
            port.name count(List.length expected)(List.length actual)(mismatch 0 expected actual))
        end)scenes);
    Printf.printf "port %s: independent OCaml parameter parity\n%!"port.name)oracles
let recursive_extremes ()=
  let workspace=load "recursive_rectangles"in
  let evaluated=ok(E.static ~inputs:workspace.Editor_document.Workspace_doc.inputs workspace.checked)in
  let prepared=ok(Flow_native.Drawing.prepare ~states:evaluated.states evaluated.plan
    (List.assoc "picture" evaluated.results))in
  begin
    let state=E.create_state()and model=ref(Recursive_rectangles_oracle.init(frame "recursive_rectangles" 0))in
    List.iteri(fun count mouse->
      let frame = frame "recursive_rectangles" count in
      let frame = { frame with input = { frame.input with pointer = mouse; dt = 0.125 } } in
      model:=Recursive_rectangles_oracle.update !model frame;
      let scene=ok(Flow_native.Drawing.render_prepared ~state prepared
        ~live:(Flow_native.Live_frame.of_frame frame) ~size:frame.input.size)in
      assert(parameters ~size:frame.input.size scene=
        parameters ~size:frame.input.size(Recursive_rectangles_oracle.view !model frame)))
      [0.,0.;720.,720.;720.,360.] end;
  print_endline "recursive rectangles: minimum/maximum depth, damping and large exact palette hashes"
let moving_circles ()=match Rays_editor.Workspace.load {|(workspace moving
 (graph picture :context draw (let* [phase (state [p 0.0] (+ p (frame/dt)))
  positions (map (fn [i] [(+ (mod i 640) (* 5 (sin phase))) (mod (int/div i 640) 360) 0]) (array/range 10000))]
  (draw/merge (draw/background "#111827") (draw/circles positions :radius 2.0 :fill "#22d3ee"))))
 (graph window :context settings (settings/config :width 640 :height 360)))|}with
 |Ok workspace->workspace|Error ds->failwith(String.concat"\n"(List.map Diagnostic.to_string ds))
let native_modes directory name workspace =
  let width,height=dimensions name in
  let config={Sketch.default_config with width;height;clock=Sketch.Fixed(1./.60.);fps=None}in
  let all=List.map(fun(mode,domains)->mode,pictures ~synthetic:false ~first:1 name workspace domains)modes in
  (* A native export counts frames from one, like Workspace.export. *)
  List.iter(fun(mode,scenes)->
    let directory=directory^"/"^name^"/"^mode in
    if mode="lisp-a" || mode="lisp-b" then
      ignore(ok(Rays_editor.Workspace.export ~directory ~frames:4 workspace))else
    Parallel.run ~domains:(if String.ends_with ~suffix:"8" mode then 8 else 1)(fun()->
      Result.get_ok(Sketch.export_state ~config ~directory ~frames:4 ~init:(fun _->())
        ~update:(fun () _->()) ~view:(fun () frame->scenes.(frame.input.frame-1))())))all;
  for count=0 to 3 do
    let path mode=Printf.sprintf "%s/%s/%s/frame-%06d.png" directory name mode count in
    let expected=read(path"lisp-a")in
    List.iter(fun(mode,_)->if read(path mode)<>expected then
      failwith(Printf.sprintf "%s frame %d: %s differs from lisp-a" name count mode))modes
  done
let native directory=
 native_modes directory "moving_circles" (moving_circles());
 List.iter(fun(Oracle port)->
  let name=port.name in let width,height=dimensions name in
  let workspace=load name in
  native_modes directory name workspace;
  let model=ref(port.init(frame name 0))in
  let canvas=Canvas.create_exn ~width ~height in
  Fun.protect ~finally:(fun()->port.stop !model;ignore(Canvas.destroy canvas))(fun()->
    let scenes=pictures name workspace 1 in
    for count=0 to 3 do
      let frame=frame name count in
      model:=port.update !model frame;
      Result.get_ok (Canvas.render canvas (port.view !model frame));
      let expected=Result.get_ok (Canvas.pixels canvas) in
      Result.get_ok (Canvas.render canvas scenes.(count));
      let actual=Result.get_ok (Canvas.pixels canvas) in
      (* Noise uses only opaque, integer axis-aligned filled rectangles, whose
         pixel-center coverage is identical in both renderers. Other ports use
         circles, rotated rectangles or strokes and retain the rim gate. *)
      if name="noise" then assert(actual=expected)else begin
      (* Text and points retain their native rasterization. Circle SDF,
         transformed rectangles and strokes can differ at a one-pixel edge.
         Require every changed pixel to be adjacent to an edge in both images, and retain
         exact interiors. No whole-image average hides a misplaced primitive. *)
      let edge image i=let x=i mod width and y=i/width in
        List.exists(fun(dx,dy)->let x=x+dx and y=y+dy in x>=0 && y>=0 && x<width && y<height &&
          image.(i)<>image.(y*width+x))[-1,0;1,0;0,-1;0,1]in
      (* The SDF rim anti-aliases a true circle one pixel per axis, while the
         reference 32-gon truncates its vertices to integers before the
         drawing's scale: a changed pixel may sit within the scale, in pixels,
         of the reference rim (native: basic frame 0, 5% coverage at 307,139;
         generative scales by two; stroke caps meet at a joint in drawing).
         Within two pixels of an edge in both images; a moved primitive still
         changes pixels far from any edge. *)
      let rim=2 in
      let edge image i=let x=i mod width and y=i/width in
        (* A rim clipped by the canvas has its reference edge off-canvas. *)
        let found=ref(x<rim || y<rim || x>=width-rim || y>=height-rim) in
        for dy= -rim to rim do for dx= -rim to rim do
          let x=x+dx and y=y+dy in
          if x>=0 && y>=0 && x<width && y<height && edge image(y*width+x) then found:=true done done;
        !found in
      let bad=ref 0 and changed=ref 0 and first=ref(-1) and worst=ref 0 in
      Array.iteri(fun i (value:Rays_math.Color.t)->let (other:Rays_math.Color.t)=actual.(i) in if value<>other then begin incr changed;
        if not(edge expected i && edge actual i) then begin incr bad; if !first<0 then first:=i;
          worst:=List.fold_left max !worst [abs(value.r-other.r);abs(value.g-other.g);abs(value.b-other.b);abs(value.a-other.a)] end end)expected;
      Printf.printf "%s frame %d: %d changed pixels, %d off-edge (first at %d,%d, worst channel %d)\n%!" name count !changed !bad
        (!first mod width)(!first/width) !worst;
      (* Recorded stroke tolerance: the SDF stroke notches where two segments
         meet (native: drawing frame 2, two pixels at 120,159, channel 52),
         a partial coverage; a moved primitive differs by a whole colour. *)
      assert(!bad<=2 && !worst<=64)
      end
    done);
  Printf.printf "port %s: four PNG modes byte-identical, OCaml %s\n%!"name
    (if name="noise"then "exact pixels"else "interior parity with one-pixel edge tolerance"))oracles
let ()=if Array.length Sys.argv>1 then native Sys.argv.(1)else begin
  pure();
  recursive_extremes();
  let workspace=moving_circles()in
  let expected=Array.map serialize(pictures "moving_circles" workspace 1)in
  List.iter(fun(_,domains)->
    assert(Array.map serialize(pictures "moving_circles" workspace domains)=expected))modes;
  assert(expected.(0)<>expected.(3));
  print_endline "10000 moving circles: four synthetic frames, four-mode command parity"
end
