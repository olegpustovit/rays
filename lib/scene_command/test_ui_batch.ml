open Scene_command

let () =
  let builder = Ui_batch.Builder.create ~capacity:1 () in
  Ui_batch.Builder.rect builder ~x:1.25 ~y:(-2.5) ~width:3.75 ~height:4.5 ~color:0x12345678l ();
  let clip = { Ui_batch.x = 0.; y = 0.; width = 10.; height = 10. } in
  Ui_batch.Builder.set_clip builder (Some clip);
  Ui_batch.Builder.wire builder (100., 100.) (120., 100.) (140., 100.) (160., 100.) ~width:1.
    ~color:0x01020304l;
  Ui_batch.Builder.wire builder (1., 2.) (3., 2.) (5., 2.) (7., 2.) ~width:1. ~color:0x01020304l;
  List.iter
    (fun texture ->
      Ui_batch.Builder.textured builder ~texture ~x:0. ~y:0. ~width:2. ~height:2. ~u0:3. ~v0:4.
        ~u1:5. ~v1:6. ~color:0xffffffffl)
    [ 2; 3 ];
  let packed = Ui_batch.Builder.publish builder in
  assert (Ui_batch.count packed = 7);
  List.iteri
    (fun word expected -> assert (Ui_batch.float packed ~instance:0 ~word = expected))
    [ 1.25; -2.5; 5.; 2. ];
  assert (Bytes.get_int32_le (Ui_batch.instances packed) 32 = 0x12345678l);
  for i = 0 to 3 do
    let instance = i + 1 in
    List.iteri
      (fun word expected -> assert (Ui_batch.float packed ~instance ~word = expected))
      [ 1.; 2.; 3.; 2.; 5.; 2.; 7.; 2. ];
    assert (Ui_batch.float packed ~instance ~word:14 = float i /. 4.);
    assert (Ui_batch.float packed ~instance ~word:15 = float (i + 1) /. 4.)
  done;
  let batches = Ui_batch.batches packed in
  assert (Array.length batches = 3);
  Array.iteri
    (fun i (batch : Ui_batch.batch) ->
      let first, count, texture = [| (0, 1, 0); (1, 5, 2); (6, 1, 3) |].(i) in
      assert (batch.first = first && batch.count = count && batch.texture = texture);
      assert (batch.clip = if i = 0 then None else Some clip))
    batches;
  assert (Ui_batch.textures packed = [ 2; 3 ]);
  let bytes = Bytes.copy (Ui_batch.instances packed) in
  Ui_batch.Builder.reset builder;
  Ui_batch.Builder.rect builder ~x:50. ~y:60. ~width:1. ~height:1. ();
  let reset = Ui_batch.Builder.publish builder in
  assert (Ui_batch.count reset = 1 && Array.length (Ui_batch.batches reset) = 1);
  assert ((Ui_batch.batches reset).(0).clip = None);
  assert (Ui_batch.instances packed = bytes && Array.length (Ui_batch.batches packed) = 3);
  print_endline "UI batch: logical coordinates, clipped chords, texture batches, immutable reset"
