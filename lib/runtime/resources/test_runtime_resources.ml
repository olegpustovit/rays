open Runtime_resources
open Ogpu.Error
let get=function Ok x->x|Error e->failwith(Format.asprintf"%a"pp_error e)
(* A canvas is written a pixel at a time: [0xRRGGBBAA] at every point. *)
let fill canvas color=
  let width,height=get(Canvas.size canvas)in
  for y=0 to height-1 do for x=0 to width-1 do get(Canvas.set_pixel canvas~x~y color)done done
let solid count bytes=Bytes.init(count*4)(fun i->bytes.[i mod 4])
let test_atomic_file_writes () =
  let root = Filename.temp_dir "rays-resources-io" "" in
  let directory = Filename.concat (Filename.concat root "nested") "deeper" in
  let target = Filename.concat directory "state.txt" in
  let blocker = Filename.concat root "not-a-directory" in
  Fun.protect ~finally:(fun () ->
    Sys.remove target;
    Unix.rmdir directory;
    Unix.rmdir (Filename.dirname directory);
    Sys.remove blocker;
    Unix.rmdir root) (fun () ->
    (match ensure_directory directory with Ok () -> () | Error message -> failwith message);
    (match write_text ~filename:target "old" with Ok () -> () | Error message -> failwith message);
    (match write_atomic ~filename:target (fun _ -> Error "writer failed") with
     | Error "writer failed" -> () | _ -> failwith "atomic writer swallowed its error");
    if In_channel.with_open_bin target In_channel.input_all <> "old"
       || Array.to_list (Sys.readdir directory) <> ["state.txt"] then
      failwith "failed atomic write replaced or leaked a temporary file";
    (match write_text ~filename:target "new\ntext" with
     | Ok () -> () | Error message -> failwith message);
    if In_channel.with_open_bin target In_channel.input_all <> "new\ntext" then
      failwith "atomic text replacement";
    Out_channel.with_open_text blocker (fun channel -> output_string channel "file");
    (match ensure_directory blocker with Error _ -> () | Ok () -> failwith "file accepted as directory"))

let run () =
  test_atomic_file_writes ();
  let original=Bytes.of_string"\x11\x22\x33\xff\x44\x55\x66\xff\x77\x88\x99\xff\xaa\xbb\xcc\xff"in
  let kept=Bytes.copy original in
  let image=get(Image.create~width:2~height:2~rgba:original)in Bytes.fill original 0 16 '\000';
  if get(Image.pixels image)<>kept then failwith"image kept the caller's storage";
  (match Image.create~width:2~height:2~rgba:(Bytes.create 15)with
   |Error{kind=Invalid_argument;_}->()|_->failwith"image accepted a short pixel buffer");
  let identity=Image.identity image and generation=Image.generation image in
  get(Image.replace image~width:1~height:1~rgba:(Bytes.of_string"\xff\x00\x00\xff"));
  if Image.identity image<>identity||Image.generation image<>generation+1
     ||get(Image.size image)<>(1,1)||get(Image.pixels image)<>Bytes.of_string"\xff\x00\x00\xff"
  then failwith"image identity/generation";
  let canvas=get(Canvas.create~width:4~height:4)in
  let canvas_generation=Canvas.generation canvas in
  get(Canvas.set_pixel canvas~x:1~y:2 0x010203ffl);get(Canvas.set_pixel canvas~x:3~y:3 0x7f7f7f7fl);
  if Canvas.generation canvas<>canvas_generation+2 then failwith"canvas mutation generation";
  List.iter(fun(x,y)->match Canvas.set_pixel canvas~x~y 0l with
    |Error{kind=Invalid_argument;_}when Canvas.generation canvas=canvas_generation+2->()
    |_->failwith"canvas accepted a pixel outside its extent")[-1,0;0,-1;4,0;0,4];
  let expected=Bytes.make 64 '\000' in
  Bytes.blit_string"\x01\x02\x03\xff"0 expected((2*4+1)*4)4;
  Bytes.blit_string"\x7f\x7f\x7f\x7f"0 expected((3*4+3)*4)4;
  let sw,sh,sg,snapshot=get(Canvas.snapshot canvas)in
  if(sw,sh,sg,snapshot)<>(4,4,Canvas.generation canvas,expected)then
    failwith"canvas atomic snapshot";
  let bulk_bytes=Bytes.of_string"\x01\x02\x03\xff\x10\x20\x30\x40"in
  let bulk=get(Canvas.of_rgba_bytes~width:2~height:1~rgba:bulk_bytes)in
  let _,_,_,bulk_snapshot=get(Canvas.snapshot bulk)in
  if bulk_snapshot<>bulk_bytes then failwith"bulk RGBA canvas changed pixel bytes";
  (match Canvas.of_rgba_bytes~width:2~height:1~rgba:(Bytes.create 7)with
   |Error{kind=Invalid_argument;_}->()|_->failwith"bulk RGBA accepted a short buffer");
  get(Canvas.destroy bulk);
  let stable=get(Image.create~width:4~height:4~rgba:(Bytes.make 64 '\000'))in
  let stable_identity=Image.identity stable in
  let moved=get(Image.create~width:4~height:4~rgba:(Bytes.make 64 '\x42'))in
  let moved_generation=Image.generation stable in
  get(Image.replace_owned stable moved);
  if not(Image.destroyed moved)||Image.identity stable<>stable_identity
     ||Image.generation stable<>moved_generation+1
     ||get(Image.pixels stable)<>Bytes.make 64 '\x42'
  then failwith"owned image replacement";
  let rejected=get(Image.create~width:1~height:1~rgba:(Bytes.make 4 '\x31'))in
  let rejected_pixels=get(Image.pixels rejected)and stable_pixels=get(Image.pixels stable)
  and stable_generation=Image.generation stable in
  (match Image.replace_owned stable stable with
   |Error{kind=Invalid_argument;_}->()
   |_->failwith"self owned replacement accepted");
  if Image.destroyed rejected||get(Image.pixels rejected)<>rejected_pixels
     ||Image.generation stable<>stable_generation||get(Image.pixels stable)<>stable_pixels
  then failwith"owned replacement rejection mutated images";
  get(Image.destroy rejected);
  let allocation=ref 0. in
  for _=1 to 32 do
    let source=get(Image.create~width:640~height:480~rgba:(Bytes.make(640*480*4)'\x5a'))in
    let before=Gc.allocated_bytes()in
    get(Image.replace_owned stable source);
    allocation:=!allocation+.(Gc.allocated_bytes()-.before)
  done;
  if !allocation/.32.>2048. then
    failwith(Printf.sprintf"owned replacement allocated %.0f bytes/copy"(!allocation/.32.));
  get(Image.destroy stable);
  (* Leased bytes stay as they were lent: a replacement goes to a second
     buffer, at most two are lent at once, and a leased source cannot be moved. *)
  let leased_image=get(Image.create~width:8~height:8~rgba:(solid 64"\x10\x20\x30\xff"))in
  let _,_,_,leased_pixels,lease1=get(Image.Private.borrow_snapshot leased_image)in
  let leased_copy=Bytes.copy leased_pixels in
  let lease_target=get(Image.create~width:1~height:1~rgba:(Bytes.make 4 '\x5c'))in
  let lease_target_before=get(Image.pixels lease_target)in
  (match Image.replace_owned lease_target leased_image with
   |Error{kind=Invalid_argument;_}->()
   |_->failwith"leased source storage was moved");
  if Image.destroyed leased_image||get(Image.pixels lease_target)<>lease_target_before then
    failwith"leased owned replacement was not atomic";
  get(Image.destroy lease_target);
  get(Image.replace leased_image~width:8~height:8~rgba:(solid 64"\xaa\xbb\xcc\xff"));
  if leased_pixels<>leased_copy then failwith"image mutation changed leased snapshot";
  let _,_,_,second_pixels,lease2=get(Image.Private.borrow_snapshot leased_image)in
  if second_pixels<>solid 64"\xaa\xbb\xcc\xff"then failwith"second lease is not the replacement";
  (match Image.replace leased_image~width:8~height:8~rgba:(solid 64"\x11\x22\x33\xff")with
   |Error{kind=Invalid_argument;_}->()
   |_->failwith"bounded image snapshot buffers were exceeded");
  if get(Image.pixels leased_image)<>solid 64"\xaa\xbb\xcc\xff"then
    failwith"a refused replacement changed the image";
  Image.Private.release_snapshot lease1;
  get(Image.replace leased_image~width:8~height:8~rgba:(solid 64"\x11\x22\x33\xff"));
  if second_pixels<>solid 64"\xaa\xbb\xcc\xff"then failwith"image mutation changed the second lease";
  Image.Private.release_snapshot lease2;
  Image.Private.release_snapshot lease2;
  get(Image.destroy leased_image);
  let destroy_leased=get(Image.create~width:1~height:1~rgba:(Bytes.of_string"\x12\x34\x56\x78"))in
  let _,_,_,destroy_bytes,destroy_lease=get(Image.Private.borrow_snapshot destroy_leased)in
  get(Image.destroy destroy_leased);
  if destroy_bytes<>Bytes.of_string"\x12\x34\x56\x78"then
    failwith"destroy invalidated leased image bytes";
  Image.Private.release_snapshot destroy_lease;
  Image.Private.release_snapshot destroy_lease;
  let direct=get(Canvas.create~width:640~height:480)in
  if Canvas.Private.gpu_snapshot direct<>None then failwith"fresh Canvas reports a GPU frame";
  (* A canvas published every frame: a capture lends the canvas storage, the
     next write moves the canvas to its spare, and destroying the previous
     capture returns that storage. 600 frames allocate less than one frame. *)
  let published=ref(get(Canvas.capture direct))in
  let publish frame=
    get(Canvas.set_pixel direct~x:(frame mod 640)~y:(frame mod 480)
      (Int32.of_int(((frame land 255)lsl 24)lor 0x111fff)));
    let next=get(Canvas.capture direct)in
    get(Image.destroy !published);published:=next in
  publish 0;publish 1;
  let before=Gc.allocated_bytes()in
  for frame=2 to 601 do publish frame done;
  let allocated=Gc.allocated_bytes()-.before in
  if allocated>=float(640*480*4)then
    failwith(Printf.sprintf"alternating capture/destroy allocated %.0f bytes"allocated);
  let last=get(Image.pixels !published)in
  if Bytes.sub last((601 mod 480*640+601)*4)4<>Bytes.of_string"\x59\x11\x1f\xff"then
    failwith"the published capture is not the canvas after its last write";
  get(Image.destroy !published);get(Canvas.destroy direct);
  (* A capture is a picture of its moment: later canvas writes and a
     replacement of another capture leave it alone. *)
  let bank_canvas=get(Canvas.create~width:4~height:4)in
  fill bank_canvas 0x2468acffl;
  let bank_image=get(Canvas.capture bank_canvas)in
  Gc.compact();let bank_live_before=(Gc.quick_stat()).live_words in
  for round=1 to 100_000 do
    get(Canvas.set_pixel bank_canvas~x:(round land 3)~y:0 0x2468acffl);
    get(Image.destroy(get(Canvas.capture bank_canvas)))
  done;
  Gc.compact();let bank_live_after=(Gc.quick_stat()).live_words in
  if bank_live_after-bank_live_before>4096 then failwith"canvas/image two-bank loop did not plateau";
  let first_pixels=get(Image.pixels bank_image)in
  if first_pixels<>solid 16"\x24\x68\xac\xff"then failwith"capture is not the canvas pixels";
  let second=get(Canvas.capture bank_canvas)in
  fill bank_canvas 0x010203ffl;
  if get(Image.pixels bank_image)<>first_pixels||get(Image.pixels second)<>first_pixels then
    failwith"a canvas write changed its captures";
  get(Image.replace bank_image~width:4~height:4~rgba:(Bytes.make 64 '\x3c'));
  let _,_,_,canvas_pixels=get(Canvas.snapshot bank_canvas)in
  if canvas_pixels<>solid 16"\x01\x02\x03\xff"then failwith"external image replacement changed canvas";
  get(Image.destroy bank_image);get(Image.destroy second);get(Canvas.destroy bank_canvas);
  let captures_before,_=Canvas.Private.pixel_stats canvas in
  List.iter(fun _frame->let capture=get(Canvas.capture canvas)in
    if get(Image.size capture)<>(4,4)||get(Image.pixels capture)<>expected then failwith"capture";
    get(Image.destroy capture))[1;2;60;600];
  if Canvas.Private.pixel_stats canvas<>(captures_before+4,0)then
    failwith"CPU Canvas successful capture/readback counters";
  let png=Filename.temp_file"rays-next-"".png"in get(Canvas.save_png canvas png);let input=open_in_bin png in let signature=really_input_string input 8 in close_in input;Sys.remove png;if signature<>"\x89PNG\r\n\x1a\n"then failwith"PNG";
  get(Canvas.destroy canvas);get(Canvas.destroy canvas);
  (match Canvas.set_pixel canvas~x:0~y:0 0l,Canvas.capture canvas with
   |Error{kind=Destroyed;_},Error{kind=Destroyed;_}->()
   |_->failwith"a destroyed canvas was written or captured");
  get(Image.destroy image);
  (match Image.pixels image with Error{kind=Destroyed;_}->()|_->failwith"a destroyed image was read");
  for _=1 to 100_000 do let x=get(Image.create~width:1~height:1~rgba:(Bytes.make 4 '\000'))in get(Image.destroy x)done;
  (match Domain.spawn(fun()->Image.create~width:1~height:1~rgba:(Bytes.make 4 '\000'))|>Domain.join with
   |Error(error:Ogpu.Error.t) when error.operation="Image.create"
       && error.kind=Ogpu.Error.Wrong_domain
       && error.message="resource operation requires the initial SDL domain"->()
   |_->failwith"resource domain error lost its shared operation, kind, or message");
  print_endline"runtime_resources: image/canvas leases, storage reuse, frame600 PNG 100k passed"
