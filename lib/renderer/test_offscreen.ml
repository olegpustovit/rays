let require condition message=if not condition then failwith message
let get=function Ok value->value|Error error->
  failwith(Format.asprintf"%a"Renderer.pp_error error)
let get_resource=function Ok value->value|Error error->
  failwith(Format.asprintf"%a"Runtime_resources.pp_error error)
let get_ir=function Ok value->value|Error _->failwith"render IR fixture"
let expect_error message=function Error _->()|Ok _->failwith message
let live_handles=Ogpu.Impl.live_handles
let rss_kib()=
  let argv=[|"/bin/ps";"-o";"rss=";"-p";string_of_int(Unix.getpid())|]in
  let input=Unix.open_process_args_in argv.(0)argv in
  Fun.protect~finally:(fun()->ignore(Unix.close_process_in input))
    (fun()->int_of_string(String.trim(input_line input)))
let pixel bytes offset expected message=
  require(Bytes.sub bytes offset 4=expected)message
let putf bytes offset value=Bytes.set_int64_le bytes offset(Int64.bits_of_float value)
let scene3_draw key red=
  let vertices=Bytes.make(3*68)'\000'and indices=Bytes.create 12 in
  List.iteri(fun index(x,y)->let offset=index*68 in
    putf vertices offset x;putf vertices(offset+8)y;
    putf vertices(offset+16)0.;putf vertices(offset+24)1.;
    putf vertices(offset+28)red;putf vertices(offset+36)0.;
    putf vertices(offset+44)0.;putf vertices(offset+52)0.;
    putf vertices(offset+60)0.)[-1.,1.;1.,1.;-1.,-1.];
  List.iteri(fun index value->Bytes.set_int32_le indices(index*4)value)[0l;1l;2l];
  let mesh:Scene_execution.mesh={key;vertices;vertex_count=3;indices;index_count=3; primitive=Ogpu.Render_pass.Triangle_list}
  and state:Scene_execution.state={viewport=(0,0,5,4);scissor=(0,0,5,4);
    cull=Ogpu.Render_pass.Cull_none;depth_compare=Ogpu.Render_pass.Always;
    depth_write=false;depth_load=Ogpu.Render_pass.Clear;depth_clear=1.;
    transform_uniforms=None}in
  Renderer.prepared_draw~family:Scene3
    {Scene_execution.mesh;state}

let test_retained execution red=
  let render_retained version draw=
    let submission=get(Renderer.Private.begin_submission execution)in
    let batch=get(Renderer.Private.adopt_draws submission[draw])in
    ignore(get(Renderer.Private.step~identity:"offscreen-retained"
      ~version submission[batch]))in
  render_retained 1L red;
  let uploaded=(get(Renderer.stats execution)).uploaded_bytes in
  render_retained 1L(scene3_draw"ignored-same-version"0.);
  require((get(Renderer.stats execution)).uploaded_bytes=uploaded)
    "retained replay revalidated or uploaded a same-version payload";
  render_retained 2L(scene3_draw"retained-black"0.);
  require((get(Renderer.stats execution)).uploaded_bytes>uploaded)
    "retained version invalidation did not prepare the replacement payload";
  let segment_ir=get_ir(Scene_command.Render_ir.create[|
    Scene_command.Render_ir.Geometry{vertices=[|0.;0.;1.;0.;0.;1.|];
      indices=[|0;1;2|];color=Int32.minus_one}|])in
  let lower_segment version=
    let submission=get(Renderer.Private.begin_submission execution)in
    ignore(get(Renderer.Private.lower_scene2_segment submission
      ~identity:77L~version~cacheable:true~density:1~resource:(fun _->None)
      segment_ir));
    Renderer.Private.cancel submission in
  lower_segment 1L;
  lower_segment 1L;
  let entries,hits,misses=Renderer.Private.retained_scene2_segment_stats execution in
  require(entries=1&&hits=1L&&misses=1L)
    "same retained Scene2 segment did not bypass lowering";
  lower_segment 2L;
  let entries,hits,misses=Renderer.Private.retained_scene2_segment_stats execution in
  require(entries=1&&hits=1L&&misses=2L)
    "retained Scene2 generation did not replace only its prior entry";
  let invalid=get(Renderer.Private.begin_submission execution)in
  expect_error"invalid retained Scene2 identity was accepted"
    (Renderer.Private.lower_scene2_segment invalid
      ~identity:0L~version:1L~cacheable:true~density:1
      ~resource:(fun _->None)segment_ir);
  let incomplete=get(Renderer.Private.begin_submission execution)in
  let incomplete_batch=get(Renderer.Private.adopt_draws incomplete[red])in
  expect_error"retained identity without version was accepted"
    (Renderer.Private.step~identity:"incomplete"incomplete[incomplete_batch])

let run () =
  let baseline=live_handles()in
  let configuration={Renderer.
    logical_width=3;logical_height=2;drawable_width=3;drawable_height=2;
    title="offscreen-test";vsync=false;high_density=true}in
  match Renderer.create_offscreen configuration with
  |Error _->print_endline"Rays offscreen: skipped (no native Metal device)"
  |Ok execution->
      try
        expect_error "offscreen execution accepted a native IME area"
          (Runtime.set_text_input (Renderer.Private.runtime execution)
            (Some ((1, 1, 2, 1), 1)));
        expect_error "offscreen execution accepted a native cursor"
          (Runtime.set_cursor (Renderer.Private.runtime execution) `Horizontal_resize);
        let clear_to execution colour=
          let submission=get(Renderer.Private.begin_submission execution)in
          get(Renderer.Private.step~clear:colour submission[])in
        clear_to execution(1.,0.,0.,1.);
        let first=get(Renderer.capture execution)in
        require(Bytes.length first=3*2*4)"offscreen initial extent";
        pixel first 0(Bytes.of_string"\255\000\000\255")
          "offscreen initial clear pixel";
        require(get(Renderer.capture execution)=first)
          "offscreen capture changed pixels";
        let resized=
          let bigger=get(Renderer.create_offscreen
            {configuration with logical_width=5;logical_height=4;
              drawable_width=5;drawable_height=4})in
          clear_to bigger(0.,0.,1.,1.);
          let facts=get(Runtime.presentation_facts (Renderer.Private.runtime bigger))in
          require(facts.logical_width=5&&facts.logical_height=4&&
            facts.drawable_width=5&&facts.drawable_height=4&&facts.pixel_density=1.)
            "offscreen resized facts";
          let pixels=get(Renderer.capture bigger)in
          ignore(get(Renderer.destroy bigger));pixels in
        require(Bytes.length resized=5*4*4)"offscreen resized extent";
        pixel resized((5*4-1)*4)(Bytes.of_string"\000\000\255\255")
          "offscreen resized clear pixel";
        let red=scene3_draw"retained-red"1. in
        test_retained execution red;
        let image=get_resource(Runtime_resources.Image.create~width:1
          ~height:1~rgba:(Bytes.of_string"\xff\x00\x00\xff"))in
        let replacement=ref 0 in
        let replace image=
          incr replacement;
          let red=if !replacement land 1=0 then '\xff' else '\x00' in
          Runtime_resources.Image.replace image~width:1~height:1
            ~rgba:(Bytes.of_string(String.make 1 red^"\x00\x00\xff"))in
        Fun.protect
          ~finally:(fun()->ignore(Runtime_resources.Image.destroy image))
          (fun()->
            let rect:Scene_command.Render_ir.rect=
              {x=0.;y=0.;width=1.;height=1.}in
            let image_ir=get_ir(Scene_command.Render_ir.create[|
              Scene_command.Render_ir.Image
                {resource_id=1;source=rect;destination=rect}|])
            and missing_ir=get_ir(Scene_command.Render_ir.create[|
              Scene_command.Render_ir.Image
                {resource_id=2;source=rect;destination=rect}|])in
            let resolve=function 1->Some(Renderer.Image image)|_->None in
            ignore(get(Renderer.lower_scene2 execution~density:1
              ~resource:resolve image_ir));
            ignore(get_resource(replace image));
            let first=get(Renderer.Private.begin_submission execution)in
            ignore(get(Renderer.Private.lower_scene2 first~density:1
              ~resource:resolve image_ir));
            ignore(get_resource(replace image));
            let failed=get(Renderer.Private.begin_submission execution)in
            ignore(get(Renderer.Private.lower_scene2 failed~density:1
              ~resource:resolve image_ir));
            ignore(get_resource(replace image));
            expect_error"later lowering failure was accepted"
              (Renderer.Private.lower_scene2 failed~density:1
                ~resource:resolve missing_ir);
            let overlap=get(Renderer.Private.begin_submission execution)in
            ignore(get(Renderer.Private.lower_scene2 overlap~density:1
              ~resource:resolve image_ir));
            ignore(get_resource(replace image));
            Renderer.Private.cancel first;
            ignore(get_resource(replace image));
            Renderer.Private.cancel first;
            Renderer.Private.cancel overlap;
            let step_failed=get(Renderer.Private.begin_submission execution)in
            let step_draws=get(Renderer.Private.lower_scene2 step_failed
              ~density:1~resource:resolve image_ir)in
            ignore(get_resource(replace image));
            let step_overlap=get(Renderer.Private.begin_submission execution)in
            ignore(get(Renderer.Private.lower_scene2 step_overlap
              ~density:1~resource:resolve image_ir));
            ignore(get_resource(replace image));
            expect_error"non-finite clear unexpectedly submitted"
              (Renderer.Private.step~clear:(Float.nan,0.,0.,1.)
                step_failed[step_draws]);
            ignore(get_resource(replace image));
            expect_error"consumed failed submission was reusable"
              (Renderer.Private.step step_failed[]);
            Renderer.Private.cancel step_failed;
            Renderer.Private.cancel step_overlap;
            let cross_first=get(Renderer.Private.begin_submission execution)in
            let cross_batch=get(Renderer.Private.lower_scene2 cross_first
              ~density:1~resource:resolve image_ir)in
            let cross_second=get(Renderer.Private.begin_submission execution)in
            expect_error"cross-submission draw batch was accepted"
              (Renderer.Private.step cross_second[cross_batch]);
            Renderer.Private.cancel cross_first;
            let two_image_ir=get_ir(Scene_command.Render_ir.create[|
              Scene_command.Render_ir.Image
                {resource_id=1;source=rect;destination=rect};
              Scene_command.Render_ir.Image
                {resource_id=2;source=rect;destination=rect}|])in
            let second_lookups=ref 0 in
            let raising_resolver=function
              |1->Some(Renderer.Image image)
              |2->incr second_lookups;
                  if !second_lookups>1 then raise Exit
                  else Some(Renderer.Image image)
              |_->None in
            let exceptional=get(Renderer.Private.begin_submission execution)in
            expect_error"raising resolver escaped the typed boundary"
              (Renderer.Private.lower_scene2 exceptional
                ~density:1~resource:raising_resolver two_image_ir);
            let exception_probe=get(Renderer.Private.begin_submission execution)in
            ignore(get(Renderer.Private.lower_scene2 exception_probe
              ~density:1~resource:resolve image_ir));
            ignore(get_resource(replace image));
            Renderer.Private.cancel exception_probe;
            let succeeded=get(Renderer.Private.begin_submission execution)in
            let succeeded_batch=get(Renderer.Private.lower_scene2 succeeded
              ~density:1~resource:resolve image_ir)in
            let expected=get_resource(Runtime_resources.Image.pixels image)in
            ignore(get(Renderer.Private.step succeeded[succeeded_batch]));
            let submitted=get(Renderer.capture execution)in
            pixel submitted 0 expected"successful transaction changed snapshot pixels";
            (* Replacing pixels behind a stable identity must reach the GPU
               texture instead of the static-image reuse path. *)
            ignore(get_resource(replace image));
            let replaced=get(Renderer.Private.begin_submission execution)in
            let replaced_batch=get(Renderer.Private.lower_scene2 replaced
              ~density:1~resource:resolve image_ir)in
            let expected=get_resource(Runtime_resources.Image.pixels image)in
            ignore(get(Renderer.Private.step replaced[replaced_batch]));
            pixel(get(Renderer.capture execution))0 expected
              "replaced image pixels did not reach the presented frame";
            ignore(get_resource(replace image));
            let labels=Array.init 45(fun index->
              get_resource(Runtime_resources.Image.create~width:1~height:1
                ~rgba:(Bytes.of_string
                  (String.init 4(function 0->Char.chr index|3->'\xff'|_->'\000')))))in
            Fun.protect
              ~finally:(fun()->Array.iter(fun label->
                ignore(Runtime_resources.Image.destroy label))labels)
              (fun()->
                let commands=Array.init 45(fun index->
                  Scene_command.Render_ir.Image
                    {resource_id=100+index;source=rect;destination=rect})in
                let labels_ir=get_ir(Scene_command.Render_ir.create commands)
                and resolve_label id=
                  if id>=100&&id<145 then
                    Some(Renderer.Image labels.(id-100))
                  else None in
                let open_submission()=
                  let submission=get(Renderer.Private.begin_submission execution)in
                  ignore(get(Renderer.Private.lower_scene2 submission
                    ~density:1~resource:resolve_label labels_ir));
                  submission in
                let no_lease=open_submission()in
                Array.iteri(fun index label->
                  let rgba value=Bytes.of_string
                    (String.init 4(function 0->Char.chr value|3->'\xff'|_->'\000'))in
                  ignore(get_resource(Runtime_resources.Image.replace label
                    ~width:1~height:1~rgba:(rgba((index+1)land 255))));
                  ignore(get_resource(Runtime_resources.Image.replace label
                    ~width:1~height:1~rgba:(rgba((index+2)land 255)))))labels;
                Renderer.Private.cancel no_lease;
                let warm=open_submission()in
                Renderer.Private.cancel warm;
                let before=Gc.quick_stat()and allocated_before=Gc.allocated_bytes()in
                for _frame=1 to 600 do
                  let submission=open_submission()in
                  Renderer.Private.cancel submission
                done;
                let after=Gc.quick_stat()and allocated_after=Gc.allocated_bytes()in
                let allocated_per_frame=(allocated_after-.allocated_before)/.600.
                and promoted_per_frame=(after.promoted_words-.before.promoted_words)*.
                  float(Sys.word_size/8)/.600. in
                require(allocated_per_frame<131072.)
                  "45-image cached lowering allocation ceiling";
                require(promoted_per_frame<16384.)
                  "45-image cached lowering promotion ceiling";
                require(Renderer.Private.snapshot_count_for_test execution<=256)
                  "image snapshot cache exceeded entry capacity";
                let last=labels.(44)in
                let blue=Bytes.of_string"\000\000\xff\xff"in
                ignore(get_resource(Runtime_resources.Image.replace last
                  ~width:1~height:1~rgba:blue));
                let mutation=get(Renderer.Private.begin_submission execution)in
                let batch=get(Renderer.Private.lower_scene2 mutation
                  ~density:1~resource:resolve_label labels_ir)in
                ignore(get(Renderer.Private.step mutation[batch]));
                pixel(get(Renderer.capture execution))0 blue
                  "image snapshot cache did not invalidate mutated pixels");
            let large_a=Bytes.make(640*480*4)'\000'
            and large_b=Bytes.make(640*480*4)'\000'in
            Bytes.set large_a 3 '\xff';Bytes.set large_b 2 '\xff';
            Bytes.set large_b 3 '\xff';
            let large=get_resource(Runtime_resources.Image.create~width:640
              ~height:480~rgba:large_a)in
            Fun.protect
              ~finally:(fun()->ignore(Runtime_resources.Image.destroy large))
              (fun()->
                let resolve_large=function
                  |1->Some(Renderer.Image large)|_->None in
                let cache_before=Renderer.Private.snapshot_count_for_test execution in
                Gc.full_major();
                let rss_before=rss_kib()and allocated_before=Gc.allocated_bytes()in
                let rss_middle=ref rss_before in
                for frame=1 to 600 do
                  ignore(get_resource(Runtime_resources.Image.replace large
                    ~width:640~height:480~rgba:(if frame land 1=0 then large_a else large_b)));
                  let submission=get(Renderer.Private.begin_submission execution)in
                  ignore(get(Renderer.Private.lower_scene2 submission
                    ~density:1~resource:resolve_large image_ir));
                  Renderer.Private.cancel submission;
                  (* Two immediate mutations require both resource buffers to be
                     unleased, rather than merely finding one spare buffer. *)
                  ignore(get_resource(Runtime_resources.Image.replace large
                    ~width:640~height:480~rgba:large_b));
                  ignore(get_resource(Runtime_resources.Image.replace large
                    ~width:640~height:480~rgba:large_a));
                  if frame=300 then(Gc.full_major();rss_middle:=rss_kib())
                done;
                Gc.full_major();
                let rss_after=rss_kib()and allocated_after=Gc.allocated_bytes()in
                require((allocated_after-.allocated_before)/.600.<131072.)
                  "changing large-image lowering allocation ceiling";
                require(!rss_middle<=rss_before+32768&&rss_after<= !rss_middle+16384)
                  "changing large-image lowering RSS did not plateau";
                Printf.printf
                  "large transient image: %.0f bytes/frame, RSS %d/%d/%d KiB\n%!"
                  ((allocated_after-.allocated_before)/.600.)rss_before
                  !rss_middle rss_after;
                require(Renderer.Private.snapshot_count_for_test execution=cache_before)
                  "large transient image entered the immutable snapshot cache";
                ignore(get_resource(Runtime_resources.Image.replace large
                  ~width:640~height:480~rgba:large_b));
                let submission=get(Renderer.Private.begin_submission execution)in
                let batch=get(Renderer.Private.lower_scene2 submission
                  ~density:1~resource:resolve_large image_ir)in
                ignore(get(Renderer.Private.step submission[batch]));
                pixel(get(Renderer.capture execution))0
                  (Bytes.of_string"\000\000\xff\xff")
                  "large transient image changed submitted snapshot pixels";
                (* A completed step must have released both snapshot leases
                   before returning, so two immediate mutations stay legal. *)
                ignore(get_resource(Runtime_resources.Image.replace large
                  ~width:640~height:480~rgba:large_a));
                ignore(get_resource(Runtime_resources.Image.replace large
                  ~width:640~height:480~rgba:large_b)));
            let eviction_images=Array.init 257(fun index->
              get_resource(Runtime_resources.Image.create~width:1~height:1
                ~rgba:(Bytes.of_string
                  (String.init 4(function 0->Char.chr(index land 255)|3->'\xff'|_->'\000')))))in
            Fun.protect
              ~finally:(fun()->Array.iter(fun cached->
                ignore(Runtime_resources.Image.destroy cached))eviction_images)
              (fun()->Array.iter(fun cached->
                ignore(get(Renderer.lower_scene2 execution~density:1
                  ~resource:(function 1->Some(Renderer.Image cached)|_->None)
                  image_ir)))eviction_images;
                require(Renderer.Private.snapshot_count_for_test execution=256)
                  "image snapshot cache did not evict to its exact entry capacity");
            let destroy_first=get(Renderer.Private.begin_submission execution)in
            ignore(get(Renderer.Private.lower_scene2 destroy_first
              ~density:1~resource:resolve image_ir));
            ignore(get_resource(replace image));
            let destroy_second=get(Renderer.Private.begin_submission execution)in
            ignore(get(Renderer.Private.lower_scene2 destroy_second
              ~density:1~resource:resolve image_ir));
            ignore(get_resource(replace image));
            ignore(get(Renderer.destroy execution));
            ignore(get_resource(replace image));
            expect_error"destroyed coordinator began a submission"
              (Renderer.Private.begin_submission execution);
            Renderer.Private.cancel destroy_first;
            Renderer.Private.cancel destroy_second);
        ignore(get(Renderer.destroy execution));
        require(live_handles()=baseline)"offscreen resize Metal live-handle delta";
        print_endline"Rays offscreen: clear/readback/resize, no present, zero delta"
      with exn->ignore(Renderer.destroy execution);raise exn
