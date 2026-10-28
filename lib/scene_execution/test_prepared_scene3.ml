let state : Scene_execution.state =
  {
    viewport = (0, 0, 16, 16);
    scissor = (0, 0, 16, 16);
    cull = Ogpu.Render_pass.Cull_none;
    depth_compare = Ogpu.Render_pass.Less_equal;
    depth_write = true;
    depth_load = Ogpu.Render_pass.Clear;
    depth_clear = 1.;
    transform_uniforms = Some (Bytes.make 208 '\000');
  }

let mesh : Scene_execution.mesh =
  {
    key = Host "scene3:test";
    vertices = Bytes.make (3 * 24) '\000';
    vertex_count = 3;
    indices = Bytes.make 12 '\000';
    index_count = 3;
    primitive = Ogpu.Render_pass.Triangle_list;
  }

let entry : Scene_execution.scene3_entry =
  {
    family = Scene3;
    blend = Ogpu.Pipeline.Replace;
    texture = None;
    auxiliary = None;
    vertex_attributes = Some ("attributes", Bytes.make 36 '\000');
    samples = 1;
    draw = { mesh; state };
  }

let expect_invalid label = function
  | Error { Ogpu.Error.kind = Invalid_argument; _ } -> ()
  | Error error -> failwith (label ^ ": " ^ Ogpu.Error.to_string error)
  | Ok _ -> failwith (label ^ " was accepted")

let run () =
  let source = [| entry |] in
  let prepared =
    Result.get_ok (Scene_execution.prepare_scene3 ~clear:(0., 0., 0., 1.) ~clear_depth:1. source)
  in
  if Array.length prepared.entries <> 1 || prepared.entries == source then
    failwith "prepared Scene3 entry array ownership";
  let textured = { entry with family = Scene3_textured } in
  if
    Result.is_ok
      (Scene_execution.prepare_scene3 ~clear:(0., 0., 0., 1.) ~clear_depth:1. [| textured |])
  then failwith "textured Scene3 accepted without texture";
  let invalid =
    { entry with draw = { entry.draw with state = { state with viewport = (0, 0, 0, 16) } } }
  in
  if
    Result.is_ok
      (Scene_execution.prepare_scene3 ~clear:(0., 0., 0., 1.) ~clear_depth:1. [| invalid |])
  then failwith "Scene3 accepted empty viewport";
  if
    Result.is_ok
      (Scene_execution.prepare_scene3 ~clear:(0., 0., 0., 1.) ~clear_depth:nan [| entry |])
  then failwith "Scene3 accepted non-finite clear";
  let packed = { entry with vertex_attributes = Some ("other", Bytes.make 36 '\000') } in
  let prepare ?(clear = (0., 0., 0., 1.)) ?(clear_depth = 1.) entries =
    Scene_execution.prepare_scene3 ~clear ~clear_depth entries
  in
  let valid candidate = Result.is_ok (prepare [| candidate |]) in
  assert (valid packed);
  List.iter
    (fun (label, candidate) -> expect_invalid label (prepare [| candidate |]))
    [
      ("Scene3 without attributes", { packed with vertex_attributes = None });
      ( "short packed attributes",
        { packed with vertex_attributes = Some ("attributes", Bytes.make 35 '\000') } );
      ("empty attribute key", { packed with vertex_attributes = Some ("", Bytes.make 36 '\000') });
      ( "legacy 68-byte vertices",
        {
          packed with
          draw = { packed.draw with mesh = { mesh with vertices = Bytes.make 204 '\000' } };
        } );
    ];
  let overflow =
    {
      entry with
      family = Scene3_points;
      draw =
        {
          entry.draw with
          mesh =
            {
              mesh with
              primitive = Point_list;
              index_count = 1 lsl (Sys.int_size - 2);
              indices = Bytes.empty;
            };
        };
    }
  in
  expect_invalid "Scene3 index byte length overflow" (prepare [| overflow |]);
  (* Copy the entry array, while retaining its borrowed resources. *)
  let source = [| entry; packed |] in
  let prepared = Result.get_ok (prepare source) in
  assert (prepared.entries.(0) == entry && prepared.entries.(1) == packed);
  source.(0) <- packed;
  assert (prepared.entries.(0) == entry);
  prepared.entries.(1) <- entry;
  assert (source.(1) == packed);
  assert (prepared.entries.(0).draw.mesh.vertices == mesh.vertices);
  assert (prepared.entries.(0).draw.mesh.indices == mesh.indices);
  assert (prepared.entries.(0).draw.state.transform_uniforms == state.transform_uniforms);
  let texture : Scene_execution.sampled_texture =
    {
      key = "prepared-texture";
      levels = [| { width = 1; height = 1; bytes = Bytes.make 4 '\255' } |];
      gpu = None;
      sampler =
        {
          label = None;
          min_filter = Nearest;
          mag_filter = Nearest;
          mip_filter = No_mip;
          address_u = Clamp_to_edge;
          address_v = Clamp_to_edge;
          lod_min = 0.;
          lod_max = 0.;
          max_anisotropy = 1;
        };
    }
  in
  let auxiliary : Scene_execution.auxiliary_resource =
    {
      key = "prepared-world";
      buffer = Bytes.make 84 '\000';
      texture;
      environment = Some texture;
      sun_shadow = None;
    }
  in
  let points =
    {
      entry with
      family = Scene3_points;
      draw = { entry.draw with mesh = { mesh with primitive = Point_list } };
    }
  in
  let textured = { entry with family = Scene3_textured; texture = Some texture } in
  let families =
    [ entry; points; textured; { textured with family = Scene3_world; auxiliary = Some auxiliary } ]
  in
  List.iter
    (fun candidate ->
      List.iter (fun samples -> assert (valid { candidate with samples })) [ 1; 4; 9; 16 ])
    families;
  let with_mesh mesh = { entry with draw = { entry.draw with mesh } }
  and with_state state = { entry with draw = { entry.draw with state } } in
  List.iter
    (fun (label, candidate) -> expect_invalid label (prepare [| candidate |]))
    [
      ("Scene2 family", { entry with family = Scene2 });
      ("UI family", { entry with family = Ui });
      ("points topology", { entry with family = Scene3_points });
      ("point topology on triangles", with_mesh { mesh with primitive = Point_list });
      ("untextured family with texture", { entry with texture = Some texture });
      ("plain family with auxiliary", { entry with auxiliary = Some auxiliary });
      ("textured family without texture", { entry with family = Scene3_textured });
      ("textured family with auxiliary", { textured with auxiliary = Some auxiliary });
      ("World without texture", { entry with family = Scene3_world; auxiliary = Some auxiliary });
      ( "World without environment",
        {
          textured with
          family = Scene3_world;
          auxiliary = Some { auxiliary with environment = None };
        } );
      ("empty mesh key", with_mesh { mesh with key = Host "" });
      ("negative vertex count", with_mesh { mesh with vertex_count = -1 });
      ("negative index count", with_mesh { mesh with index_count = -1 });
      ("short indices", with_mesh { mesh with indices = Bytes.make 11 '\000' });
      ("long indices", with_mesh { mesh with indices = Bytes.make 13 '\000' });
      ( "triangle cardinality",
        with_mesh { mesh with index_count = 2; indices = Bytes.make 8 '\000' } );
      ("line cardinality", with_mesh { mesh with primitive = Line_list });
      ("empty viewport", with_state { state with viewport = (0, 0, 0, 16) });
      ("negative viewport", with_state { state with viewport = (0, 0, 16, -1) });
      ("empty scissor", with_state { state with scissor = (0, 0, 16, 0) });
      ("negative scissor", with_state { state with scissor = (0, 0, -1, 16) });
    ];
  List.iter
    (fun samples -> expect_invalid "sample count" (prepare [| { entry with samples } |]))
    [ 0; 2; 17 ];
  List.iter
    (fun value ->
      expect_invalid "clear red" (prepare ~clear:(value, 0., 0., 1.) [| entry |]);
      expect_invalid "clear green" (prepare ~clear:(0., value, 0., 1.) [| entry |]);
      expect_invalid "clear blue" (prepare ~clear:(0., 0., value, 1.) [| entry |]);
      expect_invalid "clear alpha" (prepare ~clear:(0., 0., 0., value) [| entry |]);
      expect_invalid "draw depth clear"
        (prepare [| with_state { state with depth_clear = value } |]))
    [ nan; infinity; neg_infinity ];
  List.iter
    (fun clear_depth -> expect_invalid "depth clear" (prepare ~clear_depth [| entry |]))
    [ nan; infinity; neg_infinity; -0.01; 1.01 ];
  print_endline "prepared Scene3: exact native validation"

let bench () =
  let source = Array.make 4096 entry in
  let measure () =
    Gc.full_major ();
    let allocated = Gc.allocated_bytes () and started = Unix.gettimeofday () in
    for _ = 1 to 2000 do
      let prepared =
        Result.get_ok
          (Scene_execution.prepare_scene3 ~clear:(0., 0., 0., 1.) ~clear_depth:1. source)
      in
      assert (Array.length prepared.entries = 4096)
    done;
    (Unix.gettimeofday () -. started, Gc.allocated_bytes () -. allocated)
  in
  ignore (measure ());
  let samples = Array.init 3 (fun _ -> measure ()) in
  let median project =
    let values = Array.map project samples in
    Array.sort Float.compare values;
    values.(1)
  in
  Printf.printf "prepared_scene3: 4096 entries x 2000, %.6f s, %.0f bytes\n" (median fst)
    (median snd)
