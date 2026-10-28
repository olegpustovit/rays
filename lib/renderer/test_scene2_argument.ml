open Ogpu.Types

let get = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)

let live_handles = Ogpu.Impl.live_handles

let create_window width height =
  Renderer.create
    {
      Renderer.default_configuration with
      logical_width = width;
      logical_height = height;
      drawable_width = width;
      drawable_height = height;
    }

let run () =
  let before = live_handles () in
  match create_window 4 4 with
  | Error _ -> print_endline "runtime-next scene2 retained argument: skipped (no Metal device)"
  | Ok runtime ->
      let vertices = Bytes.make (3 * 68) '\000' in
      let indices = Bytes.make 12 '\000' in
      let put index x y u v =
        let offset = index * 68 in
        Bytes.set_int64_le vertices offset (Int64.bits_of_float x);
        Bytes.set_int64_le vertices (offset + 8) (Int64.bits_of_float y);
        Bytes.set_int32_le vertices (offset + 48) Int32.minus_one;
        Bytes.set_int64_le vertices (offset + 52) (Int64.bits_of_float u);
        Bytes.set_int64_le vertices (offset + 60) (Int64.bits_of_float v)
      in
      put 0 (-1.) (-1.) 0. 0.;
      put 1 3. (-1.) 1. 0.;
      put 2 (-1.) 3. 0. 1.;
      Bytes.set_int32_le indices 4 1l;
      Bytes.set_int32_le indices 8 2l;
      let mesh : Renderer.Scene_execution.mesh =
        {
          key = Host "scene2-retained-argument";
          vertices;
          vertex_count = 3;
          indices;
          index_count = 3;
          primitive = Ogpu.Render_pass.Triangle_list;
        }
      in
      let state : Renderer.Scene_execution.state =
        {
          viewport = (0, 0, 4, 4);
          scissor = (0, 0, 4, 4);
          cull = Ogpu.Render_pass.Cull_none;
          depth_compare = Ogpu.Render_pass.Always;
          depth_write = false;
          depth_load = Ogpu.Render_pass.Load;
          depth_clear = 1.;
          transform_uniforms = None;
        }
      in
      let sampler : sampler_descriptor =
        {
          label = Some "scene2-retained-nearest";
          min_filter = Nearest;
          mag_filter = Nearest;
          mip_filter = No_mip;
          address_u = Clamp_to_edge;
          address_v = Clamp_to_edge;
          lod_min = 0.;
          lod_max = 0.;
          max_anisotropy = 1;
        }
      in
      let texture : Renderer.Scene_execution.sampled_texture =
        {
          key = "scene2-retained-pixel";
          levels = [| { width = 1; height = 1; bytes = Bytes.of_string "\x11\x22\x33\xff" } |];
          sampler;
          gpu = None;
        }
      in
      let draw =
        {
          Renderer.Scene_execution.family = Scene2_textured;
          blend = Ogpu.Pipeline.Replace;
          texture = Some texture;
          auxiliary = None;
          vertex_attributes = None;
          samples = 1;
          draw = { mesh; state };
        }
      in
      let first_uploaded = ref None in
      for frame = 1 to 20 do
        if not (get (Renderer.draw runtime [ draw ])) then
          failwith "scene2 retained argument frame was not presented";
        let pixels =
          get
            (Renderer.read_pixels runtime
               ~bytes_per_row:
                 ((get (Runtime.presentation_facts (Renderer.Native.runtime runtime)))
                    .drawable_width * 4))
        in
        if Bytes.sub pixels 0 4 <> Bytes.of_string "\x11\x22\x33\xff" then
          failwith "scene2 retained argument exact pixel mismatch";
        if frame = 1 then first_uploaded := Some (get (Renderer.stats runtime)).uploaded_bytes
      done;
      let uploaded = (get (Renderer.stats runtime)).uploaded_bytes in
      if uploaded <= 0L then failwith "scene2 retained argument fixture was not uploaded";
      if Some uploaded <> !first_uploaded then
        failwith "scene2 retained argument stable draw was re-expanded or re-uploaded";
      let counters (stats : Renderer.stats) =
        (stats.retained_plan_misses, stats.retained_plan_hits)
      in
      let describe (stats : Renderer.stats) =
        Printf.sprintf "misses %Ld hits %Ld" stats.retained_plan_misses stats.retained_plan_hits
      in
      let retained = get (Renderer.stats runtime) in
      (* Automatic replay: frame 1 records the candidate, frame 2 admits and
         builds the plan, frames 3..20 replay it. *)
      if counters retained <> (2L, 18L) then
        failwith ("scene2 retained argument counters are not exact: " ^ describe retained);
      let settled_release = ref None in
      let peak_live = ref 0 in
      for frame = 1 to 1000 do
        let rgba = if frame land 1 = 0 then "\x11\x22\x33\xff" else "\x99\x55\x22\xff" in
        (* Managed-image keys carry their generation: same-key image bytes are
           never re-hashed, and a new generation recycles the same-shape texture. *)
        let changing : Renderer.Scene_execution.sampled_texture =
          {
            key = Printf.sprintf "image:managed-churn:1:%d" frame;
            levels = [| { width = 1; height = 1; bytes = Bytes.of_string rgba } |];
            sampler;
            gpu = None;
          }
        in
        let changing_draw = { draw with texture = Some changing } in
        if not (get (Renderer.draw runtime [ changing_draw ])) then
          failwith "managed-image churn frame was not presented";
        let pixels =
          get
            (Renderer.read_pixels runtime
               ~bytes_per_row:
                 ((get (Runtime.presentation_facts (Renderer.Native.runtime runtime)))
                    .drawable_width * 4))
        in
        if Bytes.sub pixels 0 4 <> Bytes.of_string rgba then
          failwith
            (Printf.sprintf
               "managed-image churn exact pixel mismatch at frame %d: got %S expected %S" frame
               (Bytes.sub_string pixels 0 4) rgba);
        (* Sample after the automatic plan is admitted (frame 2): the live
           handle count must then stay flat across all generations. *)
        if frame mod 100 = 0 then begin
          let current = live_handles () in
          if frame = 100 then settled_release := Some current;
          peak_live := max !peak_live current
        end
      done;
      let churn = get (Renderer.stats runtime) in
      (* The churn frames admit one new automatic plan (two misses, one build)
         that replaces the earlier one, then replay it for 998 frames while the
         same-shape texture is recycled with one upload per generation. *)
      if counters churn <> (4L, 1016L) then
        failwith ("managed-image churn rebuilt its retained render plan: " ^ describe churn);
      if Int64.sub churn.uploaded_bytes uploaded <> 256000L then
        failwith
          (Printf.sprintf "managed-image churn upload cardinality is not exact: %Ld"
             (Int64.sub churn.uploaded_bytes uploaded));
      if !peak_live <> Option.get !settled_release then
        failwith "managed-image churn grew native live-handle state";
      let plain_mesh = { mesh with Renderer.Scene_execution.key = Host "scene2-retained-plain" } in
      let plain_draw =
        {
          draw with
          family = Scene2;
          texture = None;
          draw = { Renderer.Scene_execution.mesh = plain_mesh; state };
        }
      in
      let textured_draw = { draw with blend = Ogpu.Pipeline.Alpha } in
      for _frame = 1 to 600 do
        if not (get (Renderer.draw runtime [ plain_draw; textured_draw ])) then
          failwith "mixed canonical Scene2 frame was not presented"
      done;
      let pixels =
        get
          (Renderer.read_pixels runtime
             ~bytes_per_row:
               ((get (Runtime.presentation_facts (Renderer.Native.runtime runtime))).drawable_width
              * 4))
      in
      if Bytes.sub pixels 0 4 <> Bytes.of_string "\x11\x22\x33\xff" then
        failwith "mixed canonical Scene2 exact pixel mismatch";
      let mixed = get (Renderer.stats runtime) in
      if counters mixed <> (6L, 1614L) then
        failwith ("mixed plain/textured Scene2 did not retain one stable plan: " ^ describe mixed);
      for frame = 0 to 159 do
        let identity = frame mod 80 in
        let mesh =
          {
            mesh with
            Renderer.Scene_execution.key = Host (Printf.sprintf "scene2-plan-churn-%02d" identity);
          }
        in
        if
          not
            (get
               (Renderer.draw runtime
                  [ Renderer.prepared_draw ~family:Scene2 { Renderer.Scene_execution.mesh; state } ]))
        then failwith "80-identity retained-plan frame was not presented"
      done;
      let cycled = get (Renderer.stats runtime) in
      (* Every frame differs from its predecessor, so no automatic plan is
         admitted: 160 misses and no replay. *)
      if counters cycled <> (Int64.add (fst (counters mixed)) 160L, snd (counters mixed)) then
        failwith ("80 mesh identities thrashed the aligned retained-plan cache: " ^ describe cycled);
      let affine tx =
        let bytes = Bytes.make 48 '\000' in
        Bytes.set_int64_le bytes 0 (Int64.bits_of_float 1.);
        Bytes.set_int64_le bytes 16 (Int64.bits_of_float tx);
        Bytes.set_int64_le bytes 32 (Int64.bits_of_float 1.);
        bytes
      in
      let animated tx =
        let state = { state with Renderer.Scene_execution.transform_uniforms = Some (affine tx) } in
        { draw with draw = { Renderer.Scene_execution.mesh; state } }
      in
      for frame = 0 to 19 do
        ignore (get (Renderer.draw runtime [ animated (if frame land 1 = 0 then 0. else 2.) ]))
      done;
      Gc.full_major ();
      let allocation_before = Gc.allocated_bytes ()
      and promotion_before = (Gc.quick_stat ()).promoted_words in
      let measured_frames = 400 in
      for frame = 0 to measured_frames - 1 do
        if not (get (Renderer.draw runtime [ animated (if frame land 1 = 0 then 0. else 2.) ])) then
          failwith "animated affine frame was not presented"
      done;
      let allocation_after = Gc.allocated_bytes ()
      and promotion_after = (Gc.quick_stat ()).promoted_words in
      let allocated_per_frame = (allocation_after -. allocation_before) /. float measured_frames
      and promoted_per_frame =
        (promotion_after -. promotion_before) *. float (Sys.word_size / 8) /. float measured_frames
      in
      if allocated_per_frame > 100_000. then
        failwith
          (Printf.sprintf "animated affine retained path allocated %.0f bytes/frame"
             allocated_per_frame);
      if promoted_per_frame > 1_024. then
        failwith
          (Printf.sprintf "animated affine retained path promoted %.0f bytes/frame"
             promoted_per_frame);
      ignore (get (Renderer.draw runtime [ animated 2. ]));
      let shifted =
        get
          (Renderer.read_pixels runtime
             ~bytes_per_row:
               ((get (Runtime.presentation_facts (Renderer.Native.runtime runtime))).drawable_width
              * 4))
      in
      if Bytes.sub shifted 0 4 <> Bytes.of_string "\000\000\000\000" then
        failwith "animated affine translated pixel mismatch";
      ignore (get (Renderer.draw runtime [ animated 0. ]));
      let restored =
        get
          (Renderer.read_pixels runtime
             ~bytes_per_row:
               ((get (Runtime.presentation_facts (Renderer.Native.runtime runtime))).drawable_width
              * 4))
      in
      if Bytes.sub restored 0 4 <> Bytes.of_string "\x11\x22\x33\xff" then
        failwith "animated affine restored pixel mismatch";
      get (Renderer.destroy runtime);
      let after = live_handles () in
      if after <> before then failwith "scene2 retained argument live-handle delta";
      Printf.printf
        "runtime-next scene2 retained argument: indexed expansion + 1000 managed-image generations \
         + 600 mixed plain/textured + 400 affine frames, %.0f allocated and %.0f promoted \
         bytes/frame, exact pixels, bounded release state, zero delta\n"
        allocated_per_frame promoted_per_frame
