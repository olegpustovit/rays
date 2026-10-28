module E=Flow.Eval
module Editor=Rays_editor.Editor
let ok=function Ok value->value|Error d->failwith(Diagnostic.to_string d)
let load text=match Rays_editor.Workspace.load text with Ok doc->doc|Error ds->
  failwith(String.concat "; "(List.map Diagnostic.to_string ds))
let editor doc=Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains:1
  ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())
let workspace producer=load("(workspace images (graph img :context image "^producer^
  ") (graph picture :context draw (draw/image (ref img) :at [1 2 0] :scale 2.0 :angle 0.1)))")
let ()=
  let module P=Sop in
  let observed=Atomic.make 0 in
  let dependencies=List.fold_left P.Context.Dependencies.union P.Context.Dependencies.static
    (List.map P.Context.Dependencies.one[Seed;Grain;Domains])in
  let source()=P.Node.Private.make ~operation:"image_context" ~version:1 ~parameters:""
    ~cook_mode:Generator ~dependencies ~inputs:[||](fun ~node_id:_ context _->
      Atomic.set observed(P.Context.domains context);
      let point=Int64.to_float(P.Context.seed context)/.256.,float(P.Context.grain context)/.1024.,0. in
      P.Node.Private.cook(Lisp_sop.snapshot(Rdk.Line_geometry.points[|point|]))context[||])in
  let factory=P.Edit_graph.factory ~key:"image_context" ~label:"Image context" ~category:["Test"]
    ~arity:0(function []->source()|_->assert false)in
  let factories=factory::Sop.Nodes.factories in
  let doc=match Rays_editor.Workspace.load ~factories {|(workspace context
    (graph mesh :context sop (let* [geo (sop/image_context)
      p (array/sum (sop/attr geo :P))
      img (image/map (fn [uv] [(+ uv.x p.x) p.y 0.5 1]) :width 65 :height 17)] geo)))|}with
    |Ok doc->doc|Error ds->failwith(String.concat "; "(List.map Diagnostic.to_string ds))in
  let run seed domains=
    let grain=257 in
    let owner=Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:doc ~factories ~seed ~grain ~await:true ~domains
      ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
    Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
      let plan=Editor.Private.image_plan owner in
      let node=Array.find_opt(fun(n:E.node)->n.kind="image/map")plan.nodes |> Option.get in
      Atomic.set observed 0;
      let payload=Editor.Private.image_payload owner(E.Deferred(Flow.Ty.image,node.id)) |> ok in
      assert(Atomic.get observed=domains);
      let fn=match List.assoc "function" node.args with E.Fn fn->fn|_->assert false in
      let sources=Flow_sop.Attribute_kernel.sources(E.Fn fn)in
      let kernel=Flow_sop.Image_kernel.prepare ~identity:0 ~width:65 ~height:17 ~fn ~sources [source()] |> ok in
      let session=P.Session.create ~max_entries:0 ~max_payload_bytes:0 |> Result.get_ok in
      Fun.protect ~finally:(fun()->P.Session.close session)(fun()->
        let context=P.Context.create ~seed ~grain ~domains() |> Result.get_ok in
        let expected=P.Session.cook session ~context(Flow_sop.Image_kernel.node kernel)
          |> Result.get_ok |> fun output->P.Payload.image output.payload |> Result.get_ok in
        let bytes=P.Image.rgba8 payload |> Option.get |> Bytes.copy in
        assert(P.Image.rgba8 expected=Some bytes && Char.code(Bytes.get bytes 1)=64);
        bytes))in
  assert(run 17L 1=run 17L 8 && run 49L 1=run 49L 8 && run 17L 1<>run 49L 1);
  print_endline "Image captures: effective owner seed/grain/domains match independent cooks and exact domain bytes"
let ()=
  let doc=load {|(workspace captures
    (graph mesh :context sop [(offset : float 0.015625)]
      (let* [geo (sop/box :consolidate_points true :center [(+ offset (* t 0.015625)) 0 0])
             p (reduce + [0 0 0] (sop/attr geo :P))
             img (image/map (fn [uv] [(+ uv.x p.x) uv.y 0.5 1]) :width 7 :height 3)] geo))
    (graph other :context sop (ref mesh :offset 0.03125)))|}in
  let run domains=
    let owner=Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains
      ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
    Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
      let plan=Editor.Private.image_plan owner in
      let instance=Array.find_index(fun(i:E.instance)->i.graph="mesh" && not i.default)plan.instances |> Option.get in
      let producer=Array.find_opt(fun(n:E.node)->n.kind="image/map" && n.inst=instance)plan.nodes |> Option.get in
      let image=E.Deferred(Flow.Ty.image,producer.id)in
      let fn=List.assoc "function" producer.args in
      assert(not(E.frame_dependent fn));
      let at time=Editor.Private.image_payload ~live:(Input.at_time time)owner image |> ok in
      let first=at 0. in
      let bytes=Sop.Image.rgba8 first |> Option.get |> Bytes.copy in
      assert(Char.code(Bytes.get bytes 0)=82);
      assert(at 0.==first);
      let later=at 1. in
      let changed=Sop.Image.rgba8 later |> Option.get |> Bytes.copy in
      assert(changed<>bytes && Sop.Image.rgba8 first=Some bytes);
      assert(at 1.==later && Editor.Private.image_stats owner=(0,0));
      bytes,changed)in
  assert(run 1=run 8);
  print_endline "image captures: actual owner, current live nondefault SOP source, independent CPU stamps and one/eight-domain full bytes pass"
let ()=
  let nested=load {|(workspace nested
    (graph mesh :context sop
      (let* [base (sop/box :consolidate_points true :center [0.015625 0 0])
             noise (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 3 :height 2)
             textured (sop/with_attr base :uv (exact (map (fn [p] [0.25 0.5 0]) (sop/attr base :P))))
             geo (sop/attr_from_image textured noise :attribute "sample")
             p (reduce + [0 0 0] (sop/attr geo :P))
             img (image/map (fn [uv] [(+ uv.x p.x) uv.y 0.5 1]) :width 7 :height 3)]
        geo)))|}in
  let owner=editor nested in
  Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
    let plan=Editor.Private.image_plan owner in
    let node=Array.find_opt(fun(n:E.node)->n.kind="image/map" && n.site=["mesh";"img"])plan.nodes |> Option.get in
    let image=ok(Editor.Private.image_payload owner(E.Deferred(Flow.Ty.image,node.id)))in
    assert(Sop.Image.width image=7 && Sop.Image.height image=3));
  let doc=load {|(workspace stateful
    (graph mesh :context sop
      (let* [bias (state [n 0.015625] (+ n 0.015625))
             geo (sop/box :consolidate_points true :center [bias 0 0])
             p (reduce + [0 0 0] (sop/attr geo :P))
             img (image/map (fn [uv] [(+ uv.x p.x) uv.y 0.5 1]) :width 7 :height 3)]
        geo)))|}in
  let state=E.create_state()in
  let stamp=E.state_stamp state in
  let snapshot ~first domains=
    let owner=Result.get_ok(Editor.create ~ui_text_size:11 ~workspace:doc ~await:true ~domains
      ~prepare:(fun _ _->Ok()) ~draw:(fun _ ()->Rays.Scene.D3.empty)())in
    Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
      let node=Array.find_opt(fun(n:E.node)->n.kind="image/map")(Editor.Private.image_plan owner).nodes |> Option.get in
      let image=E.Deferred(Flow.Ty.image,node.id)in
      let at frame=Editor.Private.image_payload ~state ~live:{(Input.at_time(float frame))with frame}owner image |> ok in
      if first then ignore(at 0);
      let result=at 1 in
      assert(E.state_stamp state=stamp);
      Sop.Image.rgba8 result |> Option.get |> Bytes.copy)in
  let sequential=snapshot ~first:true 1 in
  assert(sequential=snapshot ~first:false 1 && sequential=snapshot ~first:true 8);
  print_endline "image captures: direct nested resource resolution and stateful source snapshot parity without mutating caller state pass"
let () =
  List.iter (fun producer ->
    let doc=workspace producer in
    let owner=editor doc in
    Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
      let evaluated=ok(E.static doc.checked)in
      let image_value=List.assoc "img" evaluated.results in
      let image_at time=Editor.Private.with_images ~plan:evaluated.plan
        ~live:(Input.at_time time) owner (fun ~image ~texture:_ -> ok(image image_value))in
      let first=image_at 0. in
      assert(Result.get_ok (Rays.Image.get_size first)=(65,3));
      let pixels=Rays.Image.pixels first |> Result.get_ok in
      assert(Bytes.get pixels 0=Char.chr 2 && Bytes.get pixels 1=Char.chr 42
        && Bytes.get pixels 2=Char.chr 128 && Bytes.get pixels 3=Char.chr 255);
      assert(image_at 0.==first);
      assert(image_at 1.==first);
      assert(Rays.Image.pixels first |> Result.get_ok <> pixels);
      let prepared=ok(Flow_native.Drawing.prepare evaluated.plan(List.assoc "picture" evaluated.results))in
      let scene=ok(Flow_native.Drawing.render_prepared ~image:(fun v->Editor.Private.with_images
        ~plan:evaluated.plan ~live:(Input.at_time 1.) owner (fun ~image ~texture:_->image v))
        prepared ~live:(Input.at_time 1.) ~size:(65,3))in
      assert(Array.exists(function Scene_command.Render_ir.Image _->true|_->false)(Rays.Scene.Private.commands scene));
      assert(Editor.Private.image_stats owner=(1,0)));
    assert(Editor.Private.image_stats owner=(1,1)))
    ["(image/map (fn [uv] [(+ uv.x (* t 0.25)) uv.y 0.5 1]) :width 65 :height 3)";
     "(let* [bias (* t 0.25)] (image/map (fn [uv] [(+ uv.x bias) uv.y 0.5 1]) :width 65 :height 3))"];
  print_endline "image/map: initial-domain drawing, live callable captures, stable image identity and close pass"
let () =
  let doc=workspace "(image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 65 :height 3)"in
  let owner=editor doc in
  let resolve doc=let evaluated=ok(E.static doc.Editor_document.Workspace_doc.checked)in
    Editor.Private.with_images ~plan:evaluated.plan ~live:(Input.at_time 0.) owner
      (fun ~image ~texture:_->ok(image(List.assoc "img" evaluated.results)))in
  Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
    let image=resolve doc in
    let resized=workspace "(image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 17 :height 5)"in
    assert(resolve resized==image && Result.get_ok (Rays.Image.get_size image)=(17,5));
    let previous=Rays.Image.pixels image |> Result.get_ok in
    let edited=workspace "(image/map (fn [uv] [uv.y uv.x 0.5 1]) :width 17 :height 5)"in
    assert(resolve edited==image);
    assert(Rays.Image.pixels image |> Result.get_ok <> previous);
    assert(Editor.Private.image_stats owner=(1,0)));
  assert(Editor.Private.image_stats owner=(1,1));
  print_endline "image/map: replan, body edit and resize replace pixels under one owned image identity"
let ()=
  let doc=workspace "(image/noise :width 2 :height 2 :frequency 0.3 :seed 31)"in
  let evaluated=ok(E.static doc.checked)in
  let value=List.assoc "img" evaluated.results in
  let owner=editor doc in
  Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
    let image=ok(Editor.Private.image owner value)in
    assert(Result.get_ok (Rays.Image.get_size image)=(2,2));
    assert(ok(Editor.Private.image owner value)==image);
    let oracle=Result.get_ok(Rays.Image.pixels image)in
    let ui=Pxui.Ui.create()in
    Fun.protect ~finally:(fun()->Pxui.Ui.destroy ui)(fun()->
      let frame : Rays.Frame.t = { input = { (Input.at_time 0.) with size = 32, 32 };
        pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. } in
      Pxui.Ui.frame ui frame(fun ui->
        let box=Pxui.Ui.box ui ~w:(Px 16.) ~h:(Px 16.) "image"in
        Pxui.Ui.draw ui box(fun paint(x,y,w,h)->Pxui.Ui.Paint.image paint ~x ~y ~w ~h image));
      assert(Pxui.Ui.scene ui<>[]));
    assert(Result.get_ok(Rays.Image.pixels image)=oracle);
    let prepared=ok(Flow_native.Drawing.prepare evaluated.plan(List.assoc "picture" evaluated.results))in
    let commands=ref None in
    List.iter(fun domains->Parallel.run ~domains(fun()->
      let scene=ok(Flow_native.Drawing.render_prepared ~image:(Editor.Private.image owner)
        prepared ~live:(Input.at_time 0.) ~size:(32,32))in
      let actual=Rays.Scene.Private.commands scene in
      assert(Array.exists(function Scene_command.Render_ir.Image _->true|_->false)actual);
      (match !commands with None->commands:=Some actual|Some expected->assert(actual=expected));
      assert(Result.get_ok(Rays.Image.pixels image)=oracle);
      ())) [1;1;1;8];
    assert(Editor.Private.image_stats owner=(1,0)));
  assert(Editor.Private.image_stats owner=(1,1));
  let doc=load "(workspace images (graph a :context image (image/load \"../../sdl3_image/fixtures/sample.png\")) \
    (graph b :context image (image/load \"../../sdl3_image/fixtures/sample.png\")) \
    (graph picture :context draw (draw/merge (draw/image (ref a)) (draw/image (ref b)))))"in
  let owner=editor doc in
  Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
    let evaluated=ok(E.static doc.checked)in
    let a=ok(Editor.Private.image owner(List.assoc "a" evaluated.results))in
    let b=ok(Editor.Private.image owner(List.assoc "b" evaluated.results))in
    assert(a==b && Editor.Private.image_stats owner=(1,0)));
  assert(Editor.Private.image_stats owner=(1,1));
  let doc=workspace "(image/load \"../../sdl3_image/fixtures/sample.png\")"in
  let owner=editor doc in
  let evaluated=ok(E.static doc.checked)in
  Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
    let first=ok(Editor.Private.image owner(List.assoc "img" evaluated.results))in
    assert(ok(Editor.Private.image owner(List.assoc "img" evaluated.results))==first);
    assert(Editor.Private.image_stats owner=(1,0)));
  assert(Editor.Private.image_stats owner=(1,1));
  let doc=workspace "(image/load \"missing-rgbaa-image-file.png\")"in
  let owner=editor doc in
  Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
    let evaluated=ok(E.static doc.checked)in
    assert(match Editor.Private.image owner(List.assoc "img" evaluated.results)with
      |Error d->d.Diagnostic.code="E_IMAGE"|Ok _->false);
    assert(Editor.Private.image_stats owner=(0,0)));
  print_endline "images: two-pixel CPU noise, six drawing modes, one load/upload, exact ownership and missing-file diagnostic passed"

let ()=
  let doc=workspace "(image/load \"unused.png\")"in
  let lowered=ok(Flow_sop.Lower.of_checked ~factories:Sop.Nodes.factories doc.checked)in
  let graph=List.hd lowered.graphs in
  let lane=Flow_sop.Value_lane.create()in
  assert(match Flow_sop.Value_lane.resolve lane ~time:0. graph.network with
    |Error d->d.Diagnostic.code="E_IMAGE"|Ok _->false);
  let payload=Result.get_ok(Sop.Image.create ~width:1 ~height:1 ~rgba:[|0.2;0.3;0.4;1.|])in
  let calls=ref 0 in
  Flow_sop.Lower.with_images(fun ?gpu:_ ?context plan ~state:_ ~live:_ _->
    assert(plan==lowered.plan);
    let context=Option.get context in
    assert(context.network==graph.network && context.compiled==lowered.compiled);
    incr calls;Ok payload)(fun()->
    Flow_sop.Value_lane.reset lane;
    let resolved=ok(Flow_sop.Value_lane.resolve lane ~time:1. graph.network)in
    let node=Result.get_ok(Sop.Edit_graph.compile_node resolved.geometry ~node_id:(Option.get graph.root))in
    let session=Result.get_ok(Sop.Session.create ~max_entries:16 ~max_payload_bytes:65536)in
    Fun.protect ~finally:(fun()->Sop.Session.close session)(fun()->
      List.iter(fun domains->let context=Result.get_ok(Sop.Context.create ~domains())in
        let output=Result.get_ok(Sop.Session.cook session ~context node)in
        assert(Result.get_ok(Sop.Payload.image output.payload)==payload)) [1;8]));
  assert(!calls=1);
  print_endline "images: resources snapshot before worker submission, exact one/eight-domain payload identity passed"

let ()=
  let texture=Rays.Scene.D3.textured(Rays.Texture.init ~width:2 ~height:2(fun ~x:_ ~y:_->Rays_math.Color.white))in
  let scene=Rays.Scene.D3.create ~samples:4 ~ambient:Rays_math.Color.red
    [Rays.Scene.D3.translate (Rays_math.Vec3.create 2. 3. 4.)
      [Rays.Scene.D3.with_blend Add [Rays.Scene.D3.with_depth(Rays.Scene.D3.depth_state ~write:false())
        [Rays.Scene.D3.plane ~cull:Cull_front ~width:2. ~height:3.()]]]]in
  let textured=Rays.Scene.D3.Private.with_texture texture scene in
  List.iter2(fun original textured->
    assert(textured.Rays.Scene.D3.Private.texture=Some texture);
    assert({textured with texture=None}=original))
    (Rays.Scene.D3.Private.drawings scene)(Rays.Scene.D3.Private.drawings textured);
  assert(Rays.Scene.D3.Private.samples textured=4 && Rays.Scene.D3.Private.ambient textured=Rays_math.Color.red);
  print_endline "Scene image textures preserve affine, cull, blend, depth and scene settings"

let () =
  List.iter (fun producer ->
  let document = load ("(workspace image_oracle (graph image :context image " ^ producer ^ ")" ^ {|
    (graph grid :context sop
      (sop/attr_from_image (sop/grid :rows 2 :columns 2 :uv_attribute "uv") (ref image)))
    (graph picture :context draw (draw/image (ref image)))
    (graph scene :context scene (scene/geometry (ref grid) :texture (ref image))))|}) in
  Workspace_parity.check ~commands:true ~factories:Sop.Nodes.factories
    ~name:"image_oracle" document;
  if String.starts_with ~prefix:"(image/noise" producer then begin
    let owner = editor document in
    Fun.protect ~finally:(fun () -> Editor.close owner) (fun () ->
      let evaluated = ok (E.static document.checked) in
      let pixels time = Editor.Private.with_images ~plan:evaluated.plan
        ~live:(Input.at_time time) owner (fun ~image ~texture:_ ->
          Rays.Image.pixels (ok (image (List.assoc "image" evaluated.results))) |> Result.get_ok) in
      assert (pixels 0. <> pixels 10.));
    let created, destroyed = Editor.Private.image_stats owner in
    assert (created > 0 && created = destroyed)
  end)
    ["(image/noise :width 2 :height 2 :frequency (+ 0.3 (* 0.01 t)) :seed 31)";
     "(image/map (fn [uv] [uv.x uv.y (* t 0.05) 1]) :width 65 :height 3)";
     "(exact (image/map (fn [uv] [uv.x uv.y (* t 0.05) 1]) :width 65 :height 3))";
     "(exact (image/noise :width 2 :height 2 :frequency (+ 0.3 (* 0.01 t)) :seed 31))";
     "(image/load \"../../sdl3_image/fixtures/sample.png\")"]

let ()=
  let maps=List.init 64(fun i->Printf.sprintf
    "(graph m%d :context image (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 1 :height 1))" i)in
  let doc=load("(workspace capacity "^String.concat " " maps^
    " (graph drawing :context draw (draw/image (ref m63)))"^
    " (graph rendered :context image (image/render (ref drawing) :width 1 :height 1)))")in
  let owner=editor doc in
  Fun.protect ~finally:(fun()->Editor.close owner)(fun()->
    let plan=Editor.Private.image_plan owner in
    let nodes=Array.to_list plan.nodes |> List.filter(fun(n:E.node)->n.kind="image/map")in
    let value(n:E.node)=E.Deferred(Flow.Ty.image,n.id)in
    let exact node=Editor.Private.image_payload owner(value node)in
    let retained=ok(exact(List.hd nodes))in
    List.iter(fun node->ignore(ok(exact node)))(List.drop 1 (List.take 63 nodes));
    assert(Editor.Private.image_stats owner=(0,0));
    let render=Array.find_opt(fun(n:E.node)->n.kind="image/render")plan.nodes |> Option.get in
    let refused()=assert(match exact render with Error d->d.Diagnostic.code="E_IMAGE"|Ok _->false)in
    refused();
    ignore(ok(exact(List.nth nodes 63)));
    refused();
    assert(ok(exact(List.hd nodes))==retained);
    assert(Editor.Private.image_stats owner=(0,0)));
  print_endline "Image capacity: exact-only entries reserve recursive parents, failed resolution consumes no slot or runtime image"
