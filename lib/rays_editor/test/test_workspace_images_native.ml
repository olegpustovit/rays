module E=Flow.Eval
let ok=function Ok value->value|Error d->failwith(Diagnostic.to_string d)
let load text=match Rays_editor.Workspace.load text with Ok doc->doc|Error diagnostics->
  failwith(String.concat "; "(List.map Diagnostic.to_string diagnostics))
let ()=
  let module Editor=Rays_editor.Editor in
  let doc=load {|(workspace animated
    (graph picture :context draw
      (let* [bias (* t 0.2)]
        (draw/image (image/map (fn [uv] [bias uv.y 0 1]) :width 64 :height 64))))
    (graph editor :context editor (ui/workspace (ui/canvas (ref picture)))))|}in
  let owner=ref(Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains:1
    ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)()))in
  let canvas=Rays.Canvas.create_exn ~width:900 ~height:640 in
  Fun.protect ~finally:(fun()->Editor.close !owner;ignore(Rays.Canvas.destroy canvas))(fun()->
    let sample count=
      let frame : Rays.Frame.t = { input = { (Input.at_time (float count)) with
        dt = 1.; frame = count; tick = count; size = 900, 640; pointer = -100., -100. };
        pixel_scale = 1., 1.; fps = 1.; mouse_delta = 0., 0. } in
      owner:=Editor.update !owner frame;
      Result.get_ok (Rays.Canvas.render canvas (Editor.scene !owner frame));
      Result.get_ok (Rays.Canvas.pixel canvas ~x:10 ~y:40) in
    let first=sample 0 in
    assert(first<>sample 1));
  print_endline "Canvas playback refreshes image/map functions with captured time"
let ()=
  let module Editor=Rays_editor.Editor in
  let module P=Sop in
  let module I=Runtime_resources.Image in
  let handles = Ogpu.Impl.live_handles in
  let before=handles()in
  let doc=load {|(workspace captured
    (graph drawing :context draw [(image : image (image/noise :width 65 :height 17))] (draw/image image))
    (graph mesh :context sop [(offset : float 0.015625)]
      (let* [base (sop/box :consolidate_points true :center [(+ offset (* t 0.015625)) 0 0])
             geo (sop/with_attr base :Cd (exact (map (fn [p] [0.015625 0.03125 0.0625]) (sop/attr base :P))))
             p (reduce + [0 0 0] (sop/attr geo :P))
             c (reduce + [0 0 0] (sop/attr geo :Cd))
             img (image/map (fn [uv] [(+ uv.x p.x) (+ uv.y c.x) c.y 1]) :width 65 :height 17)
             parent (image/render (ref drawing :image img) :width 65 :height 17)]
        geo))
    (graph other :context sop (ref mesh :offset 0.03125))
    (graph backdrop :context draw (draw/background "#102030"))
    (graph fixed :context image (image/render (ref backdrop) :width 65 :height 17)))|}in
  let owner domains=Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains
    ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
  let display=owner 8 and cpu1=owner 1 and cpu8=owner 8 in
  Fun.protect ~finally:(fun()->List.iter Editor.close[display;cpu1;cpu8])(fun()->
    Editor.Private.gpu_qualification display;
    let node owner ~override kind graph=
      let plan=Editor.Private.image_plan owner in
      Array.find_opt(fun(n:E.node)->n.kind=kind && plan.instances.(n.inst).graph=graph
        && plan.instances.(n.inst).default<>override)plan.nodes |> Option.get in
    let value owner ~override kind graph=E.Deferred(Flow.Ty.image,(node owner ~override kind graph).id)in
    let live time=Input.at_time time in
    let exact owner time ~override kind graph=Editor.Private.image_payload ~live:(live time)owner
      (value owner ~override kind graph) |> ok in
    let rgba payload=match P.Image.rgba8 payload with Some bytes->bytes|None->
      let values=P.Image.storage payload in
      Bytes.init(Array.length values)(fun i->Char.chr(int_of_float(Float.round(values.(i)*.255.))))in
    let resolve time ~override kind graph=Editor.Private.with_images ~live:(live time)display
      (fun ~image ~texture:_->ok(image(value display ~override kind graph)))in
    let fixed=resolve 0. ~override:false "image/render" "fixed"in
    let generation image=I.generation(Rays.Image.resource image)in
    let fixed_generation=generation fixed in
    let images=Array.init 2(fun i->resolve 0. ~override:(i=1) "image/map" "mesh")in
    let saved=exact cpu1 0. ~override:true "image/map" "mesh"in
    let saved_bytes=Bytes.copy(rgba saved)in
    let parent=resolve 0. ~override:false "image/render" "mesh"in
    let parent_generation=generation parent in
    List.iter(fun time->
      Array.iteri(fun i image->
        let override=i=1 in
        assert(resolve time ~override "image/map" "mesh"==image);
        let resource=Rays.Image.resource image in
        assert(I.cpu_storage_bytes resource=0);
        let expected=rgba(exact cpu1 time ~override "image/map" "mesh")in
        assert(expected=rgba(exact cpu8 time ~override "image/map" "mesh"));
        let actual=Rays.Image.pixels image |> Result.get_ok in
        let maximum=ref 0 in
        Bytes.iteri(fun i c->maximum:=max !maximum(abs(Char.code c-Char.code(Bytes.get expected i))))actual;
        Printf.printf "workspace_geometry_capture,override=%b,time=%.2f,max=%d\n%!"override time !maximum;
        assert(!maximum<=1))images;
      assert(resolve time ~override:false "image/render" "mesh"==parent);
      let actual=Rays.Image.pixels parent |> Result.get_ok in
      let expected=rgba(exact cpu1 time ~override:false "image/render" "mesh")in
      assert(expected=rgba(exact cpu8 time ~override:false "image/render" "mesh"));
      Bytes.iteri(fun i c->assert(abs(Char.code c-Char.code(Bytes.get expected i))<=1))actual;
      assert(resolve time ~override:false "image/render" "fixed"==fixed && generation fixed=fixed_generation)
    )[0.;1.];
    assert(generation parent>parent_generation && rgba saved=saved_bytes);
    let stats,_,_,_,_=Editor.Private.image_gpu_stats display in
    Printf.printf "workspace_geometry_capture,status_reads=%d,readback_bytes=%d\n%!"stats.status_reads stats.readback_bytes;
    assert(stats.status_reads=4 && stats.readback_bytes=0));
  assert(handles()=before);
  print_endline "Native geometry captures: P/Cd, two instances, exact CPU domains, resident GPU, parent freshness, unrelated fixed render reuse and close pass"
let () =
  let exercise ~stateful =
    let bias=if stateful then "(state [previous 0.0] (+ previous 0.2))" else "(* t 0.1)"in
    let channel=if stateful then "0.5" else "bias"in
    let scale=if stateful then " :scale (+ 1 (ref bias))" else ""in
    let doc=load("(workspace mapped
      (graph bias :context value "^bias^")
      (graph img :context image (let* [bias (ref bias)]
        (image/map (fn [uv] [uv.x uv.y "^channel^" 1]) :width 65 :height 3)))
      (graph drawing :context draw (draw/image (ref img)"^scale^"))
      (graph rendered :context image (image/render (ref drawing) :width 65 :height 3)))")in
    let owner=Result.get_ok(Rays_editor.Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains:1
      ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
    Fun.protect ~finally:(fun()->Rays_editor.Editor.close owner)(fun()->
      let evaluated=ok(E.static doc.checked)in
      let image_value=List.assoc "rendered" evaluated.results in
      let snapshot state live=Rays_editor.Editor.Private.with_images ~plan:evaluated.plan ~state ~live owner
        (fun ~image ~texture:_->let image=ok(image image_value)in
          image,Result.get_ok(Rays.Image.pixels image))in
      let state=E.create_state()in
      let live0=Input.at_time 0. and live1={(Input.at_time 1.)with frame=1}in
      if stateful then ignore(ok(E.run ~state ~live:live0 ~time:0. doc.checked));
      let first,zero=snapshot state live0 in
      if stateful then ignore(ok(E.run ~state ~live:live1 ~time:1. doc.checked));
      let second,one=snapshot state live1 in
      assert(first==second && zero<>one);
      if stateful then begin
        let fresh=E.create_state()in
        ignore(ok(E.run ~state:fresh ~live:live1 ~time:1. doc.checked));
        let _,different=snapshot fresh live1 in
        assert(different<>one);
        let _,restored=snapshot state live1 in assert(restored=one)
      end;
      assert(Rays_editor.Editor.Private.image_stats owner=(2,0)));
    assert(Rays_editor.Editor.Private.image_stats owner=(2,2))in
  exercise ~stateful:false;exercise ~stateful:true;
  print_endline "Native nested image/map: frame and same-frame state captures refresh rendered pixels"
let ()=
  let doc=load "(workspace rendered (graph drawing :context draw \
    (draw/merge (draw/background \"#102030\") (draw/circle [4 4 0] 2 :fill \"#a0b0c0\"))) \
    (graph rendered :context image (image/render (ref drawing) :width 8 :height 8)) \
    (graph picture :context draw (draw/image (ref rendered))))"in
  let owner=Result.get_ok(Rays_editor.Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains:1
    ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
  Fun.protect ~finally:(fun()->Rays_editor.Editor.close owner)(fun()->
    let evaluated=ok(E.static doc.checked)in
    let image=ok(Rays_editor.Editor.Private.image owner(List.assoc "rendered" evaluated.results))in
    let expected=Result.get_ok(Rays.Image.pixels image)in
    assert(Result.get_ok (Rays.Image.get_size image)=(8,8));
    assert(ok(Rays_editor.Editor.Private.image owner(List.assoc "rendered" evaluated.results))==image);
    assert(Rays_editor.Editor.Private.image_stats owner=(1,0));
    List.iter(fun domains->Parallel.run ~domains(fun()->
      let fresh=Result.get_ok(Rays_editor.Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains
        ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
      Fun.protect ~finally:(fun()->Rays_editor.Editor.close fresh)(fun()->
        let actual=ok(Rays_editor.Editor.Private.image fresh(List.assoc "rendered" evaluated.results))in
        assert(Result.get_ok(Rays.Image.pixels actual)=expected))))[1;8]);
  assert(Rays_editor.Editor.Private.image_stats owner=(1,1));
  let directory ()=Filename.temp_dir "rays-rendered-image-" ""in
  let first=directory()and second=directory()in
  let remove path=Array.iter(fun name->Sys.remove(Filename.concat path name))(Sys.readdir path);Unix.rmdir path in
  Fun.protect ~finally:(fun()->remove first;remove second)(fun()->
    List.iter(fun directory->ok(Rays_editor.Workspace.export ~graph:"picture" ~fps:60
      ~directory ~frames:2 doc))[first;second];
    Array.iter(fun name->assert(In_channel.with_open_bin(Filename.concat first name)In_channel.input_all=
      In_channel.with_open_bin(Filename.concat second name)In_channel.input_all))(Sys.readdir first));
  print_endline "Native Canvas image snapshots: repeat cache, exact one/eight-domain bytes and close ownership passed"

let ()=
  let module Editor=Rays_editor.Editor in
  let module P=Sop in
  let module I=Runtime_resources.Image in
  let handles = Ogpu.Impl.live_handles in
  let before=handles()in
  let source=Filename.temp_file "rays-workspace-gpu-image-" ".rays"in
  let text width height body=Printf.sprintf
    "(workspace resident (graph image :context image (let* [bias (* t 0.5)] (image/map (fn [uv] %s) :width %d :height %d))))"
    body width height in
  let initial=text 65 17 "[uv.x uv.y bias 1]"in
  Out_channel.with_open_bin source(fun ch->output_string ch initial);
  let doc=load initial in
  let owner=ref(Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains:1
    ~source:(Rays_editor.Source.at ~file:source ~digest:(Editor_document.Lowering.sha256 initial))
    ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)()))in
  Fun.protect ~finally:(fun()->Editor.close !owner;Sys.remove source)(fun()->
    Editor.Private.gpu_qualification !owner;
    let value()=let plan=Editor.Private.image_plan !owner in
      let node=Array.find_opt(fun(n:E.node)->n.kind="image/map")plan.nodes |> Option.get in
      plan,node,E.Deferred(Flow.Ty.image,node.id)in
    let live time=Input.at_time time in
    let exact time=let _,_,value=value()in
      Editor.Private.image_payload ~live:(live time) !owner value |> ok in
    let resolve time=let _,_,value=value()in
      Editor.Private.with_images ~live:(live time) !owner(fun ~image ~texture->
        ok(image value),ok(texture value))in
    let rgba payload=P.Image.rgba8 payload |> Option.get in
    let saved=exact 0. in let saved_bytes=Bytes.copy(rgba saved)in
    assert(Editor.Private.image_stats !owner=(0,0));
    let image,texture=resolve 0. in
    let identity=Rays.Texture.Private.identity texture in
    let resource=Rays.Image.resource image in
    assert(I.cpu_storage_bytes resource=0 && I.readbacks resource=0);
    let generation=I.generation resource in
    assert(exact 0.==saved && I.generation resource=generation);
    print_endline "test,width,height,time,max_channel_difference,differing_channels,differing_pixels";
    let canvas=Rays.Canvas.create ~width:65 ~height:17 |> Result.get_ok in
    Fun.protect ~finally:(fun()->ignore(Rays.Canvas.destroy canvas))(fun()->
      let mesh=Rays.Mesh.plane ~width:(2.*.65./.17.) ~height:2.()in
      let camera=Rays.Camera.orthographic ~height:2. ~at:(Rays_math.Vec3.create 0. 0. 2.) ~target:Rays_math.Vec3.zero()in
      let scene source_image texture=Rays.Scene.[clear Rays_math.Color.black;
        image source_image ~at:(0,0)();Private.layer_break;
        view3d ~viewport:(33,0,32,17) ~camera(Rays.Scene.D3.create ~ambient:Rays_math.Color.white
          [Rays.Scene.D3.mesh ~material:(Rays.Material.matte Rays_math.Color.white) ~cull:Cull_none
            ~texture:(Rays.Scene.D3.textured ~filter:Nearest texture) mesh])]in
      let snapshot()=let image=Rays.Canvas.to_image canvas |> Result.get_ok in
        Fun.protect ~finally:(fun()->ignore(Rays.Image.destroy image))(fun()->Rays.Image.pixels image |> Result.get_ok)in
      let verify time=
        let actual_image,actual_texture=resolve time in
        assert(actual_image==image && Rays.Texture.Private.identity actual_texture=identity);
        assert(I.cpu_storage_bytes resource=0 && I.readbacks resource=0);
        let generation=I.generation resource in
        let payload=exact time in
        assert(I.generation resource=generation && I.readbacks resource=0);
        assert(rgba saved=saved_bytes);
        let _,node,_=value()in
        let width,height=Result.get_ok (Rays.Image.get_size image) in
        let fn=match List.assoc "function" node.args with E.Fn fn->fn|_->assert false in
        let kernel=Flow_sop.Image_kernel.prepare ~identity:0 ~width ~height ~fn ~sources:[] [] |> ok in
        List.iter(fun domains->
          let context=P.Context.create ~input:(live time) ~time ~domains() |> Result.get_ok in
          let session=P.Session.create ~max_entries:0 ~max_payload_bytes:0 |> Result.get_ok in
          Fun.protect ~finally:(fun()->P.Session.close session)(fun()->
            let cooked=P.Session.cook session ~context(Flow_sop.Image_kernel.node kernel) |> Result.get_ok in
            assert(rgba(P.Payload.image cooked.payload |> Result.get_ok)=rgba payload)))[1;8];
        let displayed=scene actual_image actual_texture in
        Result.get_ok (Rays.Canvas.render canvas displayed);Result.get_ok (Rays.Canvas.render canvas displayed);
        let uploaded=(Result.get_ok(Rays.Canvas.Private.native_stats canvas)).uploaded_bytes in
        Result.get_ok (Rays.Canvas.render canvas displayed);
        assert((Result.get_ok(Rays.Canvas.Private.native_stats canvas)).uploaded_bytes=uploaded);
        let actual=snapshot()in
        let reference=Rays.Image.upload_rgba ~width ~height ~rgba:(rgba payload)() |> Result.get_ok in
        Fun.protect ~finally:(fun()->ignore(Rays.Image.destroy reference))(fun()->
          let texture=Rays.Texture.init ~width ~height(fun ~x ~y->let o=(y*width+x)*4 in
            let bytes=rgba payload in Rays_math.Color.rgba(Char.code(Bytes.get bytes o))
              (Char.code(Bytes.get bytes(o+1)))(Char.code(Bytes.get bytes(o+2)))(Char.code(Bytes.get bytes(o+3))))in
          Result.get_ok (Rays.Canvas.render canvas (scene reference texture));
          let expected=snapshot()in
          let maximum=ref 0 and channels=ref 0 and pixels=ref 0 in
          Bytes.iteri(fun i c->let difference=abs(Char.code c-Char.code(Bytes.get expected i))in
            maximum:=max !maximum difference;if difference<>0 then incr channels)actual;
          for pixel=0 to Bytes.length actual/4-1 do
            if List.exists(fun channel->let i=pixel*4+channel in
              Bytes.get actual i<>Bytes.get expected i)[0;1;2;3]then incr pixels
          done;
          Printf.printf "workspace_gpu_image,%d,%d,%.2f,%d,%d,%d\n%!"
            width height time !maximum !channels !pixels;
          assert(!maximum<=1));
        assert(I.readbacks resource=0)in
      verify 0.;verify 1.;
      let frame time = { Rays.Frame.input = { (Input.at_time time) with size = 65, 17;
        frame = int_of_float time; tick = int_of_float time };
        pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. } in
      let reload time text=
        Out_channel.with_open_bin source(fun ch->output_string ch text);
        owner:=Editor.update !owner(frame time)in
      reload 10.(text 35 33 "[uv.y uv.x (* t 0.5) 1]");
      verify 0.5;
      assert(Result.get_ok (Rays.Image.get_size image)=(35,33));
      assert(Editor.Private.image_stats !owner=(1,0));
      reload 20.(text 65 3 "[uv.x uv.y 0.5 1]");
      let cpu_image,cpu_texture=resolve 0. in
      assert(cpu_image==image && Rays.Texture.Private.image cpu_texture=None);
      assert(Result.get_ok(I.gpu_snapshot resource)=None);
      reload 30.(text 65 17 "[uv.x uv.y (* t 0.5) 1]");
      verify 0.5;
      reload 40.(text 65 17 "[(if (> uv.x 0) 0 (* (* uv.x 1e20) (* uv.x 1e20))) uv.y 0.5 1]");
      verify 0.5;
      reload 45.(text 65 17 "[(if (< uv.x 0) 0 (* (* uv.x 1e20) (* uv.x 1e20))) uv.y 0.5 1]");
      let _,_,value=value()in
      List.iter(fun time->
        let failed=Editor.Private.with_images ~live:(live time) !owner(fun ~image ~texture:_->image value)in
        assert(match failed with Error d->d.Diagnostic.code="E_KERNEL"|Ok _->false))[0.;0.];
      assert(I.readbacks resource=0);
      reload 50.(text 65 17 "[uv.x uv.y 0.5 1]");
      verify 0.5;
      assert(rgba saved=saved_bytes)));
  assert(Editor.Private.image_stats !owner=(1,1));
  assert(handles()=before);
  print_endline "Workspace GPU images: exact-first independent CPU snapshots, live display, mesh/image parity, replan/resize, authority transitions and ownership pass"

let ()=
  let module Editor=Rays_editor.Editor in
  let module P=Sop in
  let module I=Runtime_resources.Image in
  let doc=load "(workspace nested_exact
    (graph img :context image (image/map (fn [uv] [uv.x uv.y 0.499999999 1]) :width 320 :height 240))
    (graph drawing :context draw (draw/image (ref img)))
    (graph rendered :context image (image/render (ref drawing) :width 320 :height 240))
    (graph picture :context draw (draw/image (ref rendered)))
    (graph settings :context settings (settings/config :width 320 :height 240)))"in
  let owner domains=Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains
    ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
  let display=owner 1 in
  let expected=Fun.protect ~finally:(fun()->Editor.close display)(fun()->
    Editor.Private.gpu_qualification display;
    let value owner kind=let plan=Editor.Private.image_plan owner in
      let node=Array.find_opt(fun(n:E.node)->n.kind=kind)plan.nodes |> Option.get in
      E.Deferred(Flow.Ty.image,node.id)in
    let image=Editor.Private.with_images display(fun ~image ~texture:_->ok(image(value display "image/map")))in
    let resource=Rays.Image.resource image in
    assert(I.cpu_storage_bytes resource=0);
    let generation=I.generation resource in
    let actual=Editor.Private.image_payload display(value display "image/render") |> ok in
    let rgba image=match P.Image.rgba8 image with Some bytes->bytes|None->
      let values=P.Image.storage image in
      Bytes.init(Array.length values)(fun i->Char.chr(int_of_float(Float.round(values.(i)*.255.))))in
    let bytes=rgba actual in
    for i=0 to 320*240-1 do assert(Bytes.get bytes(i*4+2)=Char.chr 127)done;
    List.iter(fun domains->let oracle=owner domains in
      Fun.protect ~finally:(fun()->Editor.close oracle)(fun()->
        let expected=Editor.Private.image_payload oracle(value oracle "image/render") |> ok in
        assert(rgba expected=bytes)))[1;8];
    assert(I.generation resource=generation && I.readbacks resource=0
      && I.cpu_storage_bytes resource=0);bytes)in
  let directory=Filename.temp_dir "rays-export-exact-image-" ""in
  Fun.protect ~finally:(fun()->Array.iter(fun name->Sys.remove(Filename.concat directory name))
    (Sys.readdir directory);Unix.rmdir directory)(fun()->
      ok(Rays_editor.Workspace.export ~graph:"picture" ~fps:60 ~directory ~frames:2 doc);
      let files=Sys.readdir directory in assert(Array.length files=2);
      Array.iter(fun name->let image=Rays.Image.load(Filename.concat directory name) |> Result.get_ok in
        Fun.protect ~finally:(fun()->ignore(Rays.Image.destroy image))(fun()->
          (* an export renders at the logical size whatever the display's density *)
          assert(Result.get_ok (Rays.Image.get_size image)=(320,240));
          assert(Result.get_ok(Rays.Image.pixels image)=expected)))files);
  print_endline "Nested image/render exactness: CPU rounding survives an already-resident GPU child without reading or replacing it"

let ()=
  let module Editor=Rays_editor.Editor in
  let doc=load "(workspace stamp (graph img :context image
    (image/noise :width 4 :height 4 :frequency (+ 0.1 (* t 0.01)) :seed 31)))"in
  let owner=Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains:1
    ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
  Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
    let node=(Editor.Private.image_plan owner).nodes.(0)in
    let value=E.Deferred(Flow.Ty.image,node.id)in
    let display time=Editor.Private.with_images ~live:(Input.at_time time) owner(fun ~image ~texture:_->
      let image=ok(image value)in Rays.Image.pixels image |> Result.get_ok)in
    let first=display 0. in
    ignore(ok(Editor.Private.image_payload ~live:(Input.at_time 10.) owner value));
    assert(display 0.=first));
  print_endline "Legacy image stamps: display A, exact B, display A restores A pixels"

let ()=
  let module Editor=Rays_editor.Editor in
  let module I=Runtime_resources.Image in
  let handles = Ogpu.Impl.live_handles in
  let baseline=handles()in
  let doc=load "(workspace dimensions
    (graph drawing :context draw (draw/background \"#102030\"))
    (graph child :context image (image/render (ref drawing)))
    (graph parent_drawing :context draw (draw/image (ref child)))
    (graph parent :context image (image/render (ref parent_drawing) :width 80 :height 40))
    (graph fixed :context image (image/render (ref drawing) :width 7 :height 5))
    (graph width_only :context image (image/render (ref drawing) :width 9))
    (graph height_only :context image (image/render (ref drawing) :height 11)))"in
  let owner=Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains:1
    ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
  Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
    let plan=Editor.Private.image_plan owner in
    let value name=(Array.find_opt(fun(instance:E.instance)->instance.graph=name && instance.default)
      plan.instances |> Option.get).result in
    let live width height={(Input.at_time 0.)with size=width,height}in
    let display size name=Editor.Private.with_images ~live:size owner
      (fun ~image ~texture:_->ok(image(value name)))in
    let a=live 65 17 and b=live 35 33 in
    let child=display a "child" and parent=display a "parent"
    and fixed=display a "fixed" and width=display a "width_only"
    and height=display a "height_only"in
    let generation image=I.generation(Rays.Image.resource image)in
    let parent_before=generation parent and fixed_before=generation fixed in
    assert(Editor.Private.image_render_stats owner=(5,0,0,0));
    assert(display b "child"==child && Result.get_ok (Rays.Image.get_size child)=(35,33));
    assert(display b "parent"==parent && generation parent>parent_before);
    assert(display b "fixed"==fixed && generation fixed=fixed_before);
    assert(display b "width_only"==width && Result.get_ok (Rays.Image.get_size width)=(9,33));
    assert(display b "height_only"==height && Result.get_ok (Rays.Image.get_size height)=(35,11));
    assert(Editor.Private.image_render_stats owner=(8,3,0,0));
    let parent_bytes=Result.get_ok(Rays.Image.pixels parent)in
    let offset=(20*80+50)*4 in
    assert(Bytes.sub parent_bytes offset 4=Bytes.make 4 '\000');
    let child_generation=generation child and fixed_generation=generation fixed in
    let exact size name=ok(Editor.Private.image_payload ~live:size owner(value name))in
    let child_a=exact a "child" and fixed_a=exact a "fixed"in
    assert((Sop.Image.width child_a,Sop.Image.height child_a)=(65,17));
    let child_b=exact b "child"in
    assert((Sop.Image.width child_b,Sop.Image.height child_b)=(35,33));
    assert(exact b "fixed"==fixed_a);
    assert(generation child=child_generation && generation fixed=fixed_generation);
    assert((Sop.Image.width child_a,Sop.Image.height child_a)=(65,17));
    assert(Editor.Private.image_render_stats owner=(11,6,3,3)));
  assert(Editor.Private.image_render_stats owner=(11,11,3,3));
  assert(handles()=baseline);
  print_endline "Resident Canvas dimensions: omitted axes, nested resize, fixed cache, independent CPU stamps, no implicit Canvas readback, zero handles"

let ()=
  let module Editor=Rays_editor.Editor in
  let module I=Runtime_resources.Image in
  let handles = Ogpu.Impl.live_handles in
  let baseline=handles()in
  let source=Filename.temp_file "rays-retained-canvas-" ".rays"in
  let text ?width_expression width height position=Printf.sprintf "(workspace retained
    (graph drawing :context draw (draw/merge (draw/background \"#102030\")
      (draw/translate [%s 0 0] (draw/rect [0 2 0] [16 5 0] :fill \"#a0b0c0\"))))
    (graph rendered :context image (image/render (ref drawing) :width %s :height %d)))"
    position (Option.value ~default:(string_of_int width)width_expression) height in
  let initial=text 65 17 "(* t 16)"in
  Out_channel.with_open_bin source(fun ch->output_string ch initial);
  let owner=ref(Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:(load initial) ~await:true ~domains:1
    ~source:(Rays_editor.Source.at ~file:source ~digest:(Editor_document.Lowering.sha256 initial))
    ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)()))in
  let destination=Rays.Canvas.create_exn ~width:65 ~height:17 in
  Fun.protect ~finally:(fun()->ignore(Rays.Canvas.destroy destination);Editor.close !owner;Sys.remove source)(fun()->
    let value()=(Array.find_opt(fun(instance:E.instance)->instance.graph="rendered")
      (Editor.Private.image_plan !owner).instances |> Option.get).result in
    let live=Input.at_time in
    let exact time=ok(Editor.Private.image_payload ~live:(live time) !owner(value()))in
    let rgba payload=match Sop.Image.rgba8 payload with Some bytes->bytes|None->
      let channels=Sop.Image.storage payload in
      Bytes.init(Array.length channels)(fun i->Char.chr(int_of_float(Float.round(channels.(i)*.255.))))in
    let saved=exact 0. in let saved_bytes=Bytes.copy(rgba saved)in
    assert(Editor.Private.image_stats !owner=(0,0));
    let resolve time=Editor.Private.with_images ~live:(live time) !owner(fun ~image ~texture->
      ok(image(value())),ok(texture(value())))in
    let image,texture=resolve 0. in
    let identity=Rays.Texture.Private.identity texture and resource=Rays.Image.resource image in
    let initial_generation=I.generation resource in
    assert(Editor.Private.image_render_stats !owner=(2,1,1,1));
    for frame=0 to 19 do
      let actual,view=resolve(float frame/.20.)in
      assert(actual==image && Rays.Texture.Private.identity view=identity)
    done;
    assert(Editor.Private.image_render_stats !owner=(2,1,1,1));
    assert(I.generation resource>initial_generation);
    assert(I.cpu_storage_bytes resource=0 && I.readbacks resource=0);
    let mesh=Rays.Mesh.plane ~width:(2.*.65./.17.) ~height:2.()in
    let camera=Rays.Camera.orthographic ~height:2. ~at:(Rays_math.Vec3.create 0. 0. 2.) ~target:Rays_math.Vec3.zero()in
    let scene source_image texture=Rays.Scene.[clear Rays_math.Color.black;image source_image ~at:(0,0)();Private.layer_break;
      view3d ~viewport:(33,0,32,17) ~camera(Rays.Scene.D3.create ~ambient:Rays_math.Color.white
        [Rays.Scene.D3.mesh ~material:(Rays.Material.matte Rays_math.Color.white) ~cull:Cull_none
          ~texture:(Rays.Scene.D3.textured ~filter:Nearest texture) mesh])]in
    let snapshot()=let image=Result.get_ok(Rays.Canvas.to_image destination)in
      Fun.protect ~finally:(fun()->ignore(Rays.Image.destroy image))(fun()->Result.get_ok(Rays.Image.pixels image))in
    let verify time=
      let actual,view=resolve time in
      assert(actual==image && Rays.Texture.Private.identity view=identity);
      let generation=I.generation resource in
      let payload=exact time in
      assert(I.generation resource=generation && I.readbacks resource=0);
      assert(rgba saved=saved_bytes);
      let displayed=scene actual view in
      Result.get_ok (Rays.Canvas.render destination displayed);Result.get_ok (Rays.Canvas.render destination displayed);
      let uploaded=(Result.get_ok(Rays.Canvas.Private.native_stats destination)).uploaded_bytes in
      Result.get_ok (Rays.Canvas.render destination displayed);
      assert((Result.get_ok(Rays.Canvas.Private.native_stats destination)).uploaded_bytes=uploaded);
      let actual=snapshot()in
      let width=Sop.Image.width payload and height=Sop.Image.height payload and bytes=rgba payload in
      let oracle=Result.get_ok(Rays.Image.upload_rgba ~width ~height ~rgba:bytes())in
      Fun.protect ~finally:(fun()->ignore(Rays.Image.destroy oracle))(fun()->
        let texture=Rays.Texture.init ~width ~height(fun ~x ~y->let o=(y*width+x)*4 in
          Rays_math.Color.rgba(Char.code(Bytes.get bytes o))(Char.code(Bytes.get bytes(o+1)))
            (Char.code(Bytes.get bytes(o+2)))(Char.code(Bytes.get bytes(o+3))))in
        Result.get_ok (Rays.Canvas.render destination (scene oracle texture));
        assert(snapshot()=actual));
      Printf.printf "workspace_retained_canvas,%d,%d,%.2f,0,0,0\n%!" width height time in
    verify 0.;verify 0.5;
    let reload time contents=
      Out_channel.with_open_bin source(fun ch->output_string ch contents);
      let frame : Rays.Frame.t = { input = { (Input.at_time time) with size = 65, 17;
        frame = int_of_float time; tick = int_of_float time };
        pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. } in
      owner:=Editor.update !owner frame in
    reload 10.(text 35 33 "(* t 16)");verify 0.5;
    assert(Result.get_ok (Rays.Image.get_size image)=(35,33));
    let failure_text=text 35 33 "(* t 1e19)"in
    ignore(load failure_text);
    reload 20. failure_text;
    let generation=I.generation resource and stats=Editor.Private.image_render_stats !owner in
    List.iter(fun _->assert(Editor.Private.with_images ~live:(live 2.) !owner
      (fun ~image ~texture:_->image(value())) |> Result.is_error))[0;1];
    assert(I.generation resource=generation && Editor.Private.image_render_stats !owner=stats);
    assert(match I.gpu_snapshot resource with Error{kind=Ogpu.Error.Destroyed;_}->true|_->false);
    verify 0.;
    reload 30.(text ~width_expression:"(+ 35 (int (* t 1e19)))" 35 33 "(* t 16)");
    ignore(resolve 0.);
    let generation=I.generation resource and stats=Editor.Private.image_render_stats !owner in
    List.iter(fun _->assert(Editor.Private.with_images ~live:(live 2.) !owner
      (fun ~image ~texture:_->image(value())) |> Result.is_error))[0;1];
    assert(I.generation resource=generation && Editor.Private.image_render_stats !owner=stats);
    assert(match I.gpu_snapshot resource with Error{kind=Ogpu.Error.Destroyed;_}->true|_->false);
    verify 0.;
    assert(I.cpu_storage_bytes resource=0 && I.readbacks resource=0);
    let _,_,captures,readbacks=Editor.Private.image_render_stats !owner in
    Editor.close !owner;
    let created,destroyed,captures_after,readbacks_after=Editor.Private.image_render_stats !owner in
    assert(created=destroyed && captures=captures_after && readbacks=readbacks_after);
    assert(rgba saved=saved_bytes));
  assert(handles()=baseline);
  print_endline "Resident Canvas live route: mixed mesh/image roundtrip parity, stable identity, exact isolation, replan/resize, failure/recovery, snapshots and teardown pass"

let ()=
  let module Editor=Rays_editor.Editor in
  let handles = Ogpu.Impl.live_handles in
  let baseline=handles()in
  let doc width=load(Printf.sprintf "(workspace failed_publication
    (graph drawing :context draw (draw/background \"#102030\"))
    (graph rendered :context image (image/render (ref drawing) :width %d :height 8)))" width)in
  let owner=Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:(doc 8) ~await:true ~domains:1
    ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
  Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
    let request width=let evaluated=ok(E.static(doc width).checked)in
      Editor.Private.with_images ~plan:evaluated.plan owner(fun ~image ~texture:_->
        image(List.assoc "rendered" evaluated.results))in
    let image=ok(request 8)in
    assert(Editor.Private.image_render_stats owner=(1,0,0,0));
    (* Force replacement publication to fail after a resized candidate renders. *)
    ignore(Rays.Image.destroy image);
    assert(match request 9 with Error d->d.Diagnostic.code="E_IMAGE"|Ok _->false);
    assert(Editor.Private.image_render_stats owner=(2,1,0,0));
    assert(match request 9 with Error d->d.Diagnostic.code="E_IMAGE"|Ok _->false);
    assert(Editor.Private.image_render_stats owner=(3,2,0,0)));
  assert(Editor.Private.image_render_stats owner=(3,3,0,0));
  assert(handles()=baseline);
  print_endline "Resident Canvas failed publication: resized candidates close once, prior allocation retained until owner close, no readback or handle leak"
