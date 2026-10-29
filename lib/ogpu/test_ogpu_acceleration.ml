let fail message = raise (Failure message)
let ok = function Ok value -> value | Error e -> fail (Ogpu.Error.to_string e)

let expect kind = function
  | Error (e : Ogpu.Error.t) when e.kind = kind -> ()
  | _ -> fail "unexpected acceleration result"

let run () =
  let d1 = Ogpu.Handle.create_device () and d2 = Ogpu.Handle.create_device () in
  let buffer = Ogpu.Handle.create ~device:d1 in
  let range = { Ogpu.Acceleration.buffer; buffer_size = 4096L; offset = 0L; length = 1024L } in
  let curves per_segment =
    Ogpu.Acceleration.Curves
      {
        control_points = [ range ];
        control_stride = 12;
        control_point_count = 4;
        radii = [ range ];
        radius_stride = 4;
        indices = range;
        segment_count = 1;
        control_points_per_segment = per_segment;
      }
  in
  (* One BLAS holds one geometry kind: Metal rejects a mix at sizing. *)
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Acceleration.create d1 ~ray_tracing:true
       (Blas
          {
            geometries =
              [|
                Triangles { vertices = range; vertex_stride = 12; vertex_count = 3 };
                Bounding_boxes { boxes = [ range ]; stride = 24; count = 1 };
                curves 4;
              |];
            motion_keyframes = None;
          }));
  ignore
    (ok
       (Ogpu.Acceleration.create d1 ~ray_tracing:true
          (Blas
             {
               geometries =
                 [|
                   Bounding_boxes { boxes = [ range ]; stride = 24; count = 1 };
                   Bounding_boxes { boxes = [ range ]; stride = 24; count = 2 };
                 |];
               motion_keyframes = None;
             })));
  ignore
    (ok
       (Ogpu.Acceleration.create d1 ~ray_tracing:true
          (Blas { geometries = [| curves 4 |]; motion_keyframes = None })));
  let descriptor =
    Ogpu.Acceleration.Blas
      {
        geometries =
          [|
            Triangles { vertices = range; vertex_stride = 12; vertex_count = 3 };
            Triangles { vertices = range; vertex_stride = 12; vertex_count = 6 };
          |];
        motion_keyframes = None;
      }
  in
  expect Ogpu.Error.Unsupported (Ogpu.Acceleration.create d1 ~ray_tracing:false descriptor);
  (* Refit: only a built refittable primitive structure, from a descriptor of its shape. *)
  let refittable = ok (Ogpu.Acceleration.create d1 ~ray_tracing:true ~refit:true descriptor) in
  expect Ogpu.Error.Invalid_state (Ogpu.Acceleration.refit d1 refittable descriptor);
  ok (Ogpu.Acceleration.build d1 refittable);
  ok (Ogpu.Acceleration.refit d1 refittable descriptor);
  ok (Ogpu.Acceleration.refit d1 refittable descriptor);
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Acceleration.refit d1 refittable
       (Blas
          {
            geometries = [| Triangles { vertices = range; vertex_stride = 12; vertex_count = 3 } |];
            motion_keyframes = None;
          }));
  expect Ogpu.Error.Cross_device (Ogpu.Acceleration.refit d2 refittable descriptor);
  let value = ok (Ogpu.Acceleration.create d1 ~ray_tracing:true descriptor) in
  ok (Ogpu.Acceleration.build d1 value);
  expect Ogpu.Error.Invalid_state (Ogpu.Acceleration.refit d1 value descriptor);
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Acceleration.create d1 ~ray_tracing:true ~refit:true
       (Sized { size = 512L; template = Ogpu.Acceleration.handle value }));
  expect Ogpu.Error.Cross_device (Ogpu.Acceleration.compacted_size d2 value);
  let sized =
    ok
      (Ogpu.Acceleration.create d1 ~ray_tracing:true
         (Sized { size = 512L; template = Ogpu.Acceleration.handle value }))
  in
  expect Ogpu.Error.Invalid_state (Ogpu.Acceleration.build d1 sized);
  ok (Ogpu.Acceleration.compacted_size d1 value);
  ok (Ogpu.Acceleration.compact_into d1 ~source:value ~destination:sized);
  expect Ogpu.Error.Invalid_state
    (Ogpu.Acceleration.compact_into d1 ~source:value ~destination:sized);
  expect Ogpu.Error.Invalid_state (Ogpu.Acceleration.compacted_size d1 sized);
  (* Motion: every keyframed range must match the keyframe count. *)
  let motion =
    Ogpu.Acceleration.Blas
      {
        geometries =
          [|
            Motion_triangles { keyframes = [ range; range ]; vertex_stride = 12; vertex_count = 3 };
          |];
        motion_keyframes = Some 2;
      }
  in
  ignore (ok (Ogpu.Acceleration.create d1 ~ray_tracing:true motion));
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Acceleration.create d1 ~ray_tracing:true
       (Blas
          {
            geometries =
              [| Motion_triangles { keyframes = [ range ]; vertex_stride = 12; vertex_count = 3 } |];
            motion_keyframes = Some 2;
          }));
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Acceleration.create d1 ~ray_tracing:true
       (Blas
          {
            geometries = [| Triangles { vertices = range; vertex_stride = 12; vertex_count = 3 } |];
            motion_keyframes = Some 2;
          }));
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Acceleration.create d1 ~ray_tracing:true
       (Blas { geometries = [| curves 5 |]; motion_keyframes = None }));
  let invalid_ranges =
    [
      { range with offset = -1L };
      { range with offset = 2L };
      { range with offset = 4000L; length = 128L };
    ]
  in
  List.iter
    (fun vertices ->
      expect Ogpu.Error.Invalid_argument
        (Ogpu.Acceleration.create d1 ~ray_tracing:true
           (Blas
              {
                geometries = [| Triangles { vertices; vertex_stride = 12; vertex_count = 3 } |];
                motion_keyframes = None;
              })))
    invalid_ranges;
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Acceleration.create d1 ~ray_tracing:true
       (Tlas
          {
            instances = range;
            instance_stride = 67;
            instance_count = 1;
            instance_kind = User_id_instances;
            structures = [ Ogpu.Acceleration.handle value ];
          }));
  ignore
    (ok
       (Ogpu.Acceleration.create d1 ~ray_tracing:true
          (Tlas
             {
               instances = range;
               instance_stride = 68;
               instance_count = 1;
               instance_kind = User_id_instances;
               structures = [ Ogpu.Acceleration.handle value ];
             })));
  (* Each instance kind has its own minimum record size: 44-byte motion records are valid, user-id records need 68. *)
  ignore
    (ok
       (Ogpu.Acceleration.create d1 ~ray_tracing:true
          (Tlas
             {
               instances = range;
               instance_stride = 44;
               instance_count = 1;
               instance_kind = Motion_instances;
               structures = [ Ogpu.Acceleration.handle value ];
             })));
  expect Ogpu.Error.Invalid_argument
    (Ogpu.Acceleration.create d1 ~ray_tracing:true
       (Tlas
          {
            instances = range;
            instance_stride = 64;
            instance_count = 1;
            instance_kind = User_id_instances;
            structures = [ Ogpu.Acceleration.handle value ];
          }));
  (* Packing follows the driver layout exactly. *)
  let layout = [| 68; 0; 48; 52; 60; 64; -1; -1; -1; -1; -1; -1 |] in
  let packed =
    Ogpu.Acceleration.pack_instances layout
      [| ([| 1.; 0.; 0.; 5.; 0.; 1.; 0.; 6.; 0.; 0.; 1.; 7. |], 0xff, 2, 9) |]
  in
  if
    Bytes.length packed <> 68
    || Bytes.get_int32_le packed 52 <> 0xffl
    || Bytes.get_int32_le packed 56 <> 0l
    || Bytes.get_int32_le packed 60 <> 2l
    || Bytes.get_int32_le packed 64 <> 9l
    || Int32.float_of_bits (Bytes.get_int32_le packed 36) <> 5.
  then fail "instance record packing drift";
  let motion_layout = [| 44; -1; 8; 12; 20; 24; 0; 4; 28; 32; 36; 40 |] in
  let packed =
    Ogpu.Acceleration.pack_motion_instances motion_layout
      [| ((0xff, 1, 4), (2, 3), (0, 1), (0.25, 0.75)) |]
  in
  if
    Bytes.length packed <> 44
    || Bytes.get_int32_le packed 0 <> 2l
    || Bytes.get_int32_le packed 4 <> 3l
    || Bytes.get_int32_le packed 12 <> 0xffl
    || Bytes.get_int32_le packed 24 <> 4l
    || Bytes.get_int32_le packed 32 <> 1l
    || Int32.float_of_bits (Bytes.get_int32_le packed 40) <> 0.75
  then fail "motion instance packing drift";
  print_endline "OGPU acceleration lifecycle validation passed"
