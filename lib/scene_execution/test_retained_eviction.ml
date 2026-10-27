let get = function
  | Ok value -> value
  | Error error -> failwith (Ogpu.Error.to_string error)

let require condition message = if not condition then failwith message

let configuration : Ogpu.Surface.configuration =
  {logical_width=64; logical_height=64; physical_width=64; physical_height=64;
   format=Rgba8_unorm; present_mode=Immediate; max_acquired=2;layer=None}

let state : Scene_execution.state =
  {viewport=(0,0,64,64); scissor=(0,0,64,64); cull=Cull_none;
   depth_compare=Always; depth_write=false; depth_load=Load; depth_clear=1.;
   transform_uniforms=None}

let mesh index : Scene_execution.mesh =
  let indices=Bytes.make 12 '\000' in
  Bytes.set_int32_le indices 4 1l; Bytes.set_int32_le indices 8 2l;
  {key=Printf.sprintf "eviction-%d" index; vertices=Bytes.make 48 '\000';
   vertex_count=3; indices; index_count=3; primitive=Ogpu.Render_pass.Triangle_list}

let texture index : Scene_execution.sampled_texture =
  {key=Printf.sprintf "eviction-texture-%d" index;
   levels=[|{width=1; height=1; bytes=Bytes.of_string "\xff\xff\xff\xff"}|];
   sampler={label=None; min_filter=Nearest; mag_filter=Nearest; mip_filter=No_mip;
     address_u=Clamp_to_edge; address_v=Clamp_to_edge; lod_min=0.; lod_max=0.;
     max_anisotropy=1};gpu=None}

let run_case name count entry =
  let driver,live_handles=Ogpu.Impl.create_driver () in
  let before=live_handles () in
  match Scene_execution_fixtures.create_offscreen driver configuration with
  | Error { Ogpu.Error.kind = No_adapter; _ } ->
      Printf.printf "retained eviction %s: skipped (no device)\n%!" name;
      false
  | Error error -> failwith (Ogpu.Error.to_string error)
  | Ok renderer ->
  Fun.protect ~finally:(fun () -> get (Scene_execution.destroy renderer)) (fun () ->
  let draws=List.init count entry in
  let clear = 0., 0., 0., 1. in
  let expected = Bytes.make (64*64*4) '\xff' in
  let pixels () = require (Bytes.equal expected
    (get (Scene_execution.read_pixels renderer ~bytes_per_row:256)))
    (name ^ " eviction full-image pixel drift") in
  (* First render succeeds even when resources are evicted during preparation:
     their destruction waits for completion. The next retained replay must not
     submit those now-dead resources. *)
  for frame=1 to 60 do
    require (get (Scene_execution.submit ~clear
      ~prepared:(name,1L) renderer draws)) "eviction preparation skipped";
    if List.mem frame [1;2;60] then pixels ();
    (match get (Scene_execution.replay_prepared_sampled_resources ~clear
        ~identity:name ~version:1L renderer) with
     | None -> ()
     | Some _ -> failwith "evicted resources remained replayable");
    require (Scene_execution.Private.cache_count_for_report renderer<=256) "mesh cache overflow";
    if frame mod 10=0 then Gc.full_major ()
  done;
  (* Automatic admission must obey the same resource lifetime rule. *)
  for _=1 to 4 do
    require (get (Scene_execution.submit ~clear renderer draws))
      "automatic eviction submission skipped";
    pixels ()
  done;
  ());
  let after=live_handles () in
  require (after=before) (Printf.sprintf "%s leaked handles %d -> %d" name before after);
  true

let run () =
  let plain = run_case "plain-300" 300 (fun index ->
    {Scene_execution.family=Scene2; blend=Ogpu.Pipeline.Replace; texture=None;
     auxiliary=None;vertex_attributes=None; samples=1;
     draw={mesh=mesh index; state={state with scissor=(index mod 2,0,63,64)}}}) in
  let textured = run_case "textured-300" 300 (fun index ->
    {Scene_execution.family=Scene2_textured; blend=Ogpu.Pipeline.Replace;
     texture=Some (texture index); auxiliary=None;vertex_attributes=None; samples=1;
     draw={mesh=mesh index; state}}) in
  if plain && textured then
    print_endline "retained eviction: full-image prepared/automatic draws, stale replay refusal, zero delta"
