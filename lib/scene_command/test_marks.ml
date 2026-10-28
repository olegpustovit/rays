open Scene_command

let ok t = assert (Marks.validate t = Ok ())
let bad t = assert (Result.is_error (Marks.validate t))
let buf ?(id = 1) ?(count = 2) width = Marks.Buffer { id; count; width }
let c v = Marks.Constant v

let () =
  let rect ?(count = 2) ?(position = buf 2) ?(size = buf 3) ?(sw = c 1.) () =
    [|
      Marks.Mark_set
        {
          count;
          kind =
            Rect { position; size; fill = c 0xffffffffl; stroke = buf ~id:2 3; stroke_width = sw };
        };
    |]
  in
  (* One valid table of every entry kind. *)
  ok
    [|
      Mark_set
        {
          count = 2;
          kind =
            Rect
              {
                position = buf 2;
                size = c (3., 4.);
                fill = buf ~id:2 3;
                stroke = c 0l;
                stroke_width = c 1.;
              };
        };
      Mark_set
        {
          count = 0;
          kind =
            Circle
              {
                position = c (1., 2.);
                radius = c 1.;
                fill = c 0l;
                stroke = c 0l;
                stroke_width = c 0.;
              };
        };
      Mark_set
        {
          count = 2;
          kind =
            Line
              {
                position = buf 3;
                end_ = buf ~id:2 2;
                stroke = c 0xffffffffl;
                stroke_width = buf ~id:3 1;
              };
        };
      Mark_set { count = 2; kind = Point { position = buf 2; fill = 0xffffffffl } };
      Geometry { vertices = [||]; indices = [||]; color = 0l };
      Geometry { vertices = [| 0.; 0.; 1.; 0.; 1.; 1. |]; indices = [| 0; 1; 2 |]; color = 1l };
      Image { resource = 1; x = 0; y = 0; scale = 1.; angle = 0. };
      Text { x = 0; y = 0; size = 0; color = 0l; text = "" };
      Push_transform { xx = 1.; xy = 0.; yx = 0.; yy = 1.; tx = 0.; ty = 0. };
      Push_clip { x = 0.; y = 0.; width = 1.; height = 1. };
      Pop_clip;
      Pop_transform;
    |];
  ok (rect ());
  (* A device colour is a region's float r, g, b: three floats per instance, like a colour buffer. *)
  let device ?(count = 2) width = Marks.Device { id = 4; count; width } in
  let coloured fill stroke =
    [|
      Marks.Mark_set
        {
          count = 2;
          kind =
            Circle { position = device 3; radius = device 1; fill; stroke; stroke_width = c 1. };
        };
    |]
  in
  ok (coloured (device 3) (device 3));
  ok
    [|
      Marks.Mark_set
        {
          count = 2;
          kind =
            Line { position = buf 2; end_ = buf ~id:2 2; stroke = device 3; stroke_width = c 1. };
        };
    |];
  bad (coloured (device 4) (c 0l));
  bad (coloured (c 0l) (device 1));
  bad (coloured (device ~count:3 3) (c 0l));
  (* Refusals. *)
  bad (rect ~count:(-1) ());
  bad (rect ~count:1_000_001 ~position:(c (0., 0.)) ~size:(c (1., 1.)) ());
  bad (rect ~position:(buf ~count:3 2) ());
  bad (rect ~position:(buf ~id:0 2) ());
  bad (rect ~position:(buf 1) ());
  bad (rect ~size:(c (-1., 1.)) ());
  bad (rect ~sw:(c nan) ());
  bad (rect ~sw:(c (-1.)) ());
  bad (rect ~sw:(c 3.5e38) ());
  bad [| Marks.Mark_set { count = 1; kind = Point { position = c (infinity, 0.); fill = 0l } } |];
  bad [| Geometry { vertices = [| 0.; 0.; 1. |]; indices = [||]; color = 0l } |];
  bad [| Geometry { vertices = [| 0.; 0. |]; indices = [| 0; 0 |]; color = 0l } |];
  bad [| Geometry { vertices = [| 0.; nan |]; indices = [||]; color = 0l } |];
  bad [| Geometry { vertices = [| 0.; 0. |]; indices = [| 0; 0; 1 |]; color = 0l } |];
  bad [| Image { resource = 0; x = 0; y = 0; scale = 1.; angle = 0. } |];
  bad [| Image { resource = 1; x = 0; y = 0; scale = 0.; angle = 0. } |];
  bad [| Image { resource = 1; x = 0; y = 0; scale = 1.; angle = nan } |];
  bad [| Push_transform { xx = nan; xy = 0.; yx = 0.; yy = 1.; tx = 0.; ty = 0. } |];
  bad [| Push_clip { x = 0.; y = 0.; width = -1.; height = 1. } |];
  bad [| Pop_transform |];
  bad [| Pop_clip |];
  bad [| Push_clip { x = 0.; y = 0.; width = 1.; height = 1. } |];
  (* Families are independent: a clip pop cannot close a transform push. *)
  bad [| Push_transform { xx = 1.; xy = 0.; yx = 0.; yy = 1.; tx = 0.; ty = 0. }; Pop_clip |];
  bad
    (Array.make (Marks.max_entries + 1)
       (Marks.Text { x = 0; y = 0; size = 1; color = 0l; text = "" }));
  (* Exact bytes of a tiny table, and determinism. *)
  let tiny =
    [|
      Marks.Mark_set { count = 1; kind = Point { position = c (1., 2.); fill = 0x11223344l } };
      Text { x = 1; y = 2; size = 3; color = 5l; text = "hi" };
    |]
  in
  let b = Buffer.create 64 in
  let i64 n = Buffer.add_int64_le b (Int64.of_int n) in
  let f x = Buffer.add_int64_le b (Int64.bits_of_float x) in
  Buffer.add_int32_le b 0x524d4b31l;
  i64 2;
  Buffer.add_uint8 b 0;
  Buffer.add_uint8 b 3;
  i64 1;
  Buffer.add_uint8 b 0;
  f 1.;
  f 2.;
  Buffer.add_uint8 b 0;
  Buffer.add_int32_le b 0x11223344l;
  Buffer.add_uint8 b 3;
  i64 1;
  i64 2;
  i64 3;
  Buffer.add_int32_le b 5l;
  i64 2;
  Buffer.add_string b "hi";
  assert (Marks.serialize tiny = Buffer.to_bytes b);
  assert (Marks.serialize tiny = Marks.serialize tiny);
  assert (Bytes.sub_string (Marks.serialize tiny) 0 4 = "1KMR")
