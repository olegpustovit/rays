module B=Ogpu.Backend
module P=Renderer.Native
let get=function Ok value->value|Error error->failwith(Format.asprintf"%a"Renderer.pp_error error)
let native_get=function Ok value->value|Error error->failwith(Ogpu.Error.to_string error)
let words a=let b=Bytes.create(4*Array.length a)in
  Array.iteri(fun i v->Bytes.set_int32_le b(i*4)v)a;b
let floats a=words(Array.map Int32.bits_of_float a)
let ()=
  let live=Ogpu.Impl.live_handles in
  let baseline=live()in
  List.iter(fun density->
    let execution=get(Renderer.create_offscreen{Renderer.
      logical_width=32;logical_height=24;drawable_width=32*density;drawable_height=24*density;
      title="GPU drawing parity";vsync=false;high_density=true})in
    let gpu=get(Renderer.acquire_gpu())in
    let sink=get(Renderer.create_gpu_marks gpu)in
    let positions=native_get(B.create_buffer(Renderer.gpu_device gpu)
      {label=Some"GPU parity points";size=24L;usage=[Vertex;Storage;Copy_dst]})in
    Fun.protect~finally:(fun()->Renderer.close_gpu_marks sink;
      ignore(B.destroy_buffer positions);Renderer.release_gpu gpu;
      ignore(get(Renderer.destroy execution)))(fun()->
      let points=[|8.;8.;0.;20.;14.;0.|]in
      native_get(B.write_buffer positions~offset:0L(floats points));
      let generation=ref 1 in
      (* A device source is current only while its producer's output is. *)
      let source stamp()=if !generation=stamp then Some positions else None in
      let marks ?(fill=0x12345678l) stamp=Renderer.gpu_marks sink~kind:4~count:2
        [|Renderer.Device(source stamp,3);Host(floats[|2.5|],0);Host(words[|fill|],0);
          Host(words[|0xff00ff80l|],0);Host(floats[|3.|],0)|]in
      let expected fill=
        let b=Scene_command.Ui_batch.Builder.create()in
        for index=0 to 1 do Scene_command.Ui_batch.Builder.circle b
          ~x:points.(index*3)~y:points.(index*3+1)~radius:2.5
          ~fill~stroke:0xff00ff80l~stroke_width:3.()done;
        Scene_command.Ui_batch.Builder.publish b in
      let expected_shapes=expected 0x12345678l in
      let first=get(marks !generation)in
      (* Nothing changed: the same token, not a new registration. *)
      assert(get(marks !generation)=first);
      let ir blend shapes=Result.get_ok(Scene_command.Render_ir.create
        Scene_command.Render_ir.[|Set_blend blend;
          Push_clip{x=2.;y=3.;width=25.;height=17.};
          Push_transform{xx=0.875;xy=(-0.2);yx=0.15;yy=1.125;tx=0.375;ty=0.125};
          Shapes shapes;Pop_transform;Pop_clip|])in
      let clear=0.125,0.25,0.375,1. in
      let capture ?identity ?version ir=
        let submission=get(P.begin_submission execution)in
        Fun.protect~finally:(fun()->P.cancel submission)(fun()->
          let batch=get(P.lower_scene2 submission~density
            ~resource:(fun _->None)ir)in
          get(P.step~clear ?identity ?version submission[batch]);
          let pixels=get(Renderer.capture execution)in
          assert(Bytes.length pixels=32*24*density*density*4);pixels)in
      let shapes=Scene_command.Ui_batch.of_marks first in
      List.iter(fun blend->
        let reference=capture(ir blend expected_shapes)in
        if blend=Scene_command.Render_ir.Replace then
          assert(Bytes.sub reference 0 4<>
            Bytes.sub reference((10*density*32*density+6*density)*4)4);
        for _=1 to 3 do assert(capture(ir blend shapes)=reference)done)
        Scene_command.Render_ir.[Replace;Alpha;Add;Multiply;Screen;Subtract];
      let stamp=Scene_command.Marks.gpu_stamp in
      let retained token=capture~identity:"gpu-marks"~version:(stamp token)
        (ir Replace(Scene_command.Ui_batch.of_marks token))in
      ignore(retained first);
      assert(get(P.replay~clear~identity:"gpu-marks"~version:(stamp first) execution)=Some());
      let refuse_replay token=match P.replay~clear
        ~identity:"gpu-marks"~version:(stamp token) execution with
        |Error error when error.Ogpu.Error.kind=Ogpu.Error.Stale_handle&&
            error.operation="Scene_execution.replay"&&
            error.message="GPU vertices are stale or closed"->()
        |_->failwith"retained GPU mark replay accepted a stale source"in
      let refuse_lower token=
        let submission=get(P.begin_submission execution)in
        Fun.protect~finally:(fun()->P.cancel submission)(fun()->
          match P.lower_scene2 submission~density
            ~resource:(fun _->None)(ir Replace(Scene_command.Ui_batch.of_marks token))with
          |Error error when error.Ogpu.Error.kind=Ogpu.Error.Invalid_state&&
            error.operation="Renderer.lower_scene2"&&
            error.message="GPU shapes are stale or closed"->()
          |_->failwith"GPU mark lowering accepted a stale token")in
      (* The producer's output is overwritten: its source returns None, so the token is stale and a
         new registration refuses until the source is current again. *)
      incr generation;
      refuse_replay first;refuse_lower first;
      (match Renderer.gpu_marks sink~kind:4~count:2
         [|Renderer.Device(source 1,3);Host(floats[|2.5|],0);Host(words[|0x12345678l|],0);
           Host(words[|0xff00ff80l|],0);Host(floats[|3.|],0)|]with
       |Error{Ogpu.Error.kind=Ogpu.Error.Stale_handle;_}->()
       |_->failwith"a stale device source was registered");
      let second=get(marks !generation)in
      assert(stamp first<stamp second);
      let reference=capture(ir Replace expected_shapes)in
      assert(retained second=reference);
      (* A host attribute that changes supersedes the previous token. *)
      let third=get(marks~fill:0x87654321l !generation)in
      assert(stamp second<stamp third);
      refuse_replay second;refuse_lower second;
      assert(retained third=capture(ir Replace(expected 0x87654321l)));
      assert(retained third<>reference);
      (* A mark set that is not a valid description is refused before it registers. *)
      (match Renderer.gpu_marks sink~kind:4~count:0[|Renderer.Host(floats[|0.|],0);Host(floats[|0.|],0);Host(words[|0l|],0);
         Host(words[|0l|],0);Host(floats[|0.|],0)|]with
       |Error{Ogpu.Error.kind=Ogpu.Error.Invalid_argument;_}->()
       |_->failwith"an empty mark set was registered");
      Renderer.release_gpu gpu;
      refuse_replay third;refuse_lower third;
      assert(Scene_execution.Private.gpu_vertex_count_for_test
        (Renderer.Native.gpu_vertices_owner execution)=0)))
    [1;2];
  assert(live()=baseline);
  print_endline"GPU marks: exact native parity under transform, clip, six blends, Retina and stale output/update/lease guards"
