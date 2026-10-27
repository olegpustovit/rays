open Metal
open Test_metal_support

let formats =
  Texture.
    [ R8_unorm; Rgba8_unorm; Bgra8_unorm; Bgra8_unorm_srgb; Rgba16_float; Rgba32_float
    ; Depth32_float ]

let read ?(mip_level = 0) texture ~region ~bytes_per_row ~bytes_per_image =
  Texture.read_bytes texture ~region ~mip_level ~slice:0 ~bytes_per_row ~bytes_per_image

let full_region ?(depth = 1) width height =
  { Texture.x = 0; y = 0; z = 0; width; height; depth }

let test_format_matrix device _queue =
  if List.length formats <> 7 || List.length (List.sort_uniq compare formats) <> 7 then
    fail "Metal/OGPU format inventory drift";
  List.iter
    (fun format ->
      let descriptor = Texture.descriptor_2d ~format ~width:4 ~height:4 () in
      match Texture.create ~device descriptor with
      | Ok texture ->
          if (Texture.descriptor texture).format <> format then
            fail "Metal changed a retained pixel format";
          get (Texture.destroy texture)
      | Error { kind = Native_error | Unsupported; _ } -> ()
      | Error error -> fail "%s" (Format.asprintf "%a" pp_error error))
    formats

let read_shader_source =
  {|#include <metal_stdlib>
using namespace metal;
kernel void texel_read(texture2d<float, access::read> source [[texture(0)]],
                       device uint *result [[buffer(0)]]) {
  result[0] = uint(round(source.read(uint2(0u, 0u)).r * 255.0f));
}
kernel void read_swizzle(texture2d<float, access::sample> source [[texture(0)]],
                         device uint4 *result [[buffer(0)]]) {
  constexpr sampler nearest_sampler(coord::pixel, filter::nearest);
  result[0] = uint4(round(source.sample(nearest_sampler, float2(0.5f)) * 255.0f));
}|}

let test_swizzle_and_compression device queue =
  let base_swizzle : Texture.swizzle =
    { red = Texture.Red; green = Texture.One; blue = Texture.One; alpha = Texture.Green }
  in
  let swizzled ?(storage = Buffer.Shared) ?(usage = [ Texture.Shader_read ]) ~width ~height () =
    { (Texture.descriptor_2d ~storage ~usage ~format:Texture.Rgba8_unorm ~width ~height ()) with
      swizzle = base_swizzle
    }
  in
  let texture = get (Texture.create ~device (swizzled ~width:1 ~height:1 ())) in
  if (Texture.descriptor texture).swizzle <> base_swizzle then
    fail "base texture swizzle did not round-trip";
  upload ~device ~queue texture ~region:(full_region 1 1) ~bytes_per_row:4
    (Bytes.of_string "\010\020\030\040");
  let plain =
    get
      (Texture.create ~device
         (Texture.descriptor_2d ~storage:Buffer.Shared ~format:Texture.Rgba8_unorm ~width:1
            ~height:1 ()))
  in
  upload ~device ~queue plain ~region:(full_region 1 1) ~bytes_per_row:4
    (Bytes.of_string "\010\020\030\040");
  let output = get (Buffer.create ~device ~length:16L ~storage:Buffer.Shared ()) in
  let library = get (Library.compile_source ~device read_shader_source) in
  let function_ = get (Function.find ~library "read_swizzle") in
  let pipeline = get (Compute_pipeline.create function_) in
  let sample name source expected =
    let commands = get (Command_buffer.create queue ()) in
    let encoder = get (Compute_encoder.create commands) in
    get (Compute_encoder.set_pipeline encoder pipeline);
    get (Compute_encoder.set_texture encoder ~index:0 source);
    get (Compute_encoder.set_buffer encoder ~index:0 ~offset:0L output);
    get (Compute_encoder.dispatch_threads encoder ~threads:(1, 1, 1) ~threadgroup:(1, 1, 1));
    get (Compute_encoder.end_encoding encoder);
    complete commands;
    let bytes = get (Buffer.read_bytes output ~offset:0L ~length:16) in
    let actual = Array.init 4 (fun index -> Bytes.get_int32_le bytes (index * 4) |> Int32.to_int) in
    if actual <> expected then
      fail "%s texture swizzle: expected [%s], got [%s]" name
        (expected |> Array.to_list |> List.map string_of_int |> String.concat "; ")
        (actual |> Array.to_list |> List.map string_of_int |> String.concat "; ")
  in
  sample "identity" plain [| 10; 20; 30; 40 |];
  sample "base" texture [| 10; 255; 255; 20 |];
  get (Compute_pipeline.destroy pipeline);
  get (Function.destroy function_);
  get (Library.destroy library);
  get (Buffer.destroy output);
  get (Texture.destroy plain);
  get (Texture.destroy texture);
  without_handles "invalid writable swizzles" (fun () ->
    List.iter
      (fun usage ->
        reject Invalid_argument
          (Texture.create ~device (swizzled ~storage:Buffer.Private ~usage ~width:4 ~height:4 ())))
      [ [ Texture.Shader_write ]; [ Texture.Shader_atomic ] ]);
  let lossy_descriptor =
    { (Texture.descriptor_2d ~storage:Buffer.Private ~usage:[ Texture.Render_target ]
         ~format:Texture.Rgba8_unorm ~width:64 ~height:64 ())
      with
      compression = Texture.Lossy
    }
  in
  without_handles "invalid lossy texture descriptors" (fun () ->
    [ { lossy_descriptor with storage = Buffer.Shared }
    ; { lossy_descriptor with allow_gpu_optimized_contents = false }
    ; { lossy_descriptor with usage = [ Texture.Pixel_format_view ] }
    ; { lossy_descriptor with usage = [ Texture.Shader_write ] }
    ; { lossy_descriptor with usage = [ Texture.Shader_atomic ] }
    ; { lossy_descriptor with kind = Texture.Texture_1d; height = 1 }
    ]
    |> List.iter (fun invalid -> reject Invalid_argument (Texture.create ~device invalid)));
  without_handles "lossy compression is unsupported for retained formats" (fun () ->
    reject Invalid_argument (Texture.create ~device lossy_descriptor))

let test_transfer_and_descriptors device queue =
  let texture_descriptor =
    { (Texture.descriptor_2d ~storage:Buffer.Shared
         ~usage:[ Texture.Shader_read; Texture.Pixel_format_view ]
         ~label:"Metal conformance texture" ~format:Texture.Rgba8_unorm ~width:4 ~height:4 ())
      with
      mip_levels = 3
    }
  in
  finalizer_releases "texture" ~count:1 (fun () ->
    ignore (get (Texture.create ~device texture_descriptor)));
  let texture = get (Texture.create ~device texture_descriptor) in
  if (Texture.descriptor texture).mip_levels <> 3 then fail "2D texture mip cardinality is wrong";
  if (Texture.descriptor texture).hazard_tracking <> Texture.Tracked then
    fail "direct texture resource properties are wrong";
  if (Texture.descriptor texture).label <> Some "Metal conformance texture" then
    fail "texture label did not round-trip";
  if not (Device.same (Texture.device texture) device) || Texture.destroyed texture then
    fail "texture identity is wrong";
  reject Parent_has_dependents (Device.destroy device);
  (* Rows of sixteen bytes carried at a pitch of twenty: the pad never reaches
     the texture and reads back as zero. *)
  let texture_bytes = Bytes.make 80 '\xee' in
  for row = 0 to 3 do
    for column_byte = 0 to 15 do
      Bytes.set_uint8 texture_bytes ((row * 20) + column_byte) ((row * 16) + column_byte)
    done
  done;
  let full = full_region 4 4 in
  upload ~device ~queue texture ~region:full ~bytes_per_row:20 texture_bytes;
  let texture_copy = get (read texture ~region:full ~bytes_per_row:20 ~bytes_per_image:80) in
  for row = 0 to 3 do
    for column_byte = 0 to 15 do
      let offset = (row * 20) + column_byte in
      if Bytes.get_uint8 texture_copy offset <> Bytes.get_uint8 texture_bytes offset then
        fail "texture byte transfer changed active pixel data"
    done;
    for padding = 16 to 19 do
      if Bytes.get_uint8 texture_copy ((row * 20) + padding) <> 0 then
        fail "texture read exposed uninitialized row padding"
    done
  done;
  let into = Bytes.make 64 '\xaa' in
  get
    (Texture.read_bytes_into texture ~region:full ~mip_level:0 ~slice:0 ~bytes_per_row:16
       ~bytes_per_image:64 ~destination:into);
  for row = 0 to 3 do
    if Bytes.sub into (row * 16) 16 <> Bytes.sub texture_bytes (row * 20) 16 then
      fail "texture read into caller storage changed pixel data"
  done;
  reject Invalid_argument
    (Texture.read_bytes_into texture ~region:full ~mip_level:0 ~slice:0 ~bytes_per_row:16
       ~bytes_per_image:64 ~destination:(Bytes.create 63));
  reject Invalid_argument (read texture ~region:full ~bytes_per_row:15 ~bytes_per_image:60);
  reject Invalid_argument (read texture ~region:full ~bytes_per_row:20 ~bytes_per_image:79);
  reject Invalid_argument (read texture ~region:full ~bytes_per_row:max_int ~bytes_per_image:max_int);
  reject Invalid_argument
    (read texture ~region:{ full with x = 3; width = 2 } ~bytes_per_row:8 ~bytes_per_image:32);
  reject Invalid_argument (read ~mip_level:3 texture ~region:full ~bytes_per_row:16 ~bytes_per_image:64);
  reject Invalid_argument
    (Texture.read_bytes texture ~region:full ~mip_level:0 ~slice:1 ~bytes_per_row:16
       ~bytes_per_image:64);
  let mip_region = full_region 2 2 in
  let mip_bytes = Bytes.init 16 (fun index -> Char.chr (index + 20)) in
  upload ~device ~queue ~level:1 texture ~region:mip_region ~bytes_per_row:8 mip_bytes;
  if get (read ~mip_level:1 texture ~region:mip_region ~bytes_per_row:8 ~bytes_per_image:16)
     <> mip_bytes
  then fail "texture mip transfer did not round-trip";
  if Bytes.sub (get (read texture ~region:full ~bytes_per_row:16 ~bytes_per_image:64)) 0 16
     <> Bytes.sub texture_bytes 0 16
  then fail "a mip upload overwrote the base level";
  let private_texture =
    get
      (Texture.create ~device
         (Texture.descriptor_2d ~storage:Buffer.Private ~usage:[ Texture.Render_target ]
            ~format:Texture.Bgra8_unorm ~width:4 ~height:4 ()))
  in
  reject Unsupported (read private_texture ~region:full ~bytes_per_row:16 ~bytes_per_image:64);
  get (Texture.destroy private_texture);
  without_handles "invalid texture descriptors" (fun () ->
    [ { texture_descriptor with width = 0 }
    ; { texture_descriptor with mip_levels = 9 }
    ; { texture_descriptor with kind = Texture.Texture_1d; height = 2; mip_levels = 1 }
    ; { texture_descriptor with sample_count = 2; mip_levels = 1 }
    ; { texture_descriptor with usage = [ Texture.Shader_read; Texture.Shader_read ] }
    ; { texture_descriptor with label = Some "invalid\000label" }
    ; { texture_descriptor with kind = Texture.Texture_buffer; height = 1; mip_levels = 1 }
    ; { texture_descriptor with kind = Texture.Texture_2d_array; mip_levels = 1 }
    ; { texture_descriptor with kind = Texture.Texture_cube; height = 2; mip_levels = 1 }
    ; { texture_descriptor with kind = Texture.Texture_3d; array_length = 2; mip_levels = 1 }
    ]
    |> List.iter (fun invalid -> reject Invalid_argument (Texture.create ~device invalid)));
  without_handles "texture label Metal refuses" (fun () ->
    reject Native_error (Texture.create ~device { texture_descriptor with label = Some "\255" }));
  if get (Device.supports_texture_sample_count device 2) then begin
    let multisample_texture =
      get
        (Texture.create ~device
           { texture_descriptor with
             kind = Texture.Texture_2d_multisample
           ; mip_levels = 1
           ; sample_count = 2
           ; storage = Buffer.Private
           ; usage = [ Texture.Render_target ]
           ; label = None
           })
    in
    reject Unsupported (read multisample_texture ~region:full ~bytes_per_row:16 ~bytes_per_image:64);
    get (Texture.destroy multisample_texture)
  end;
  let cube =
    get
      (Texture.create ~device
         { (Texture.descriptor_2d ~storage:Buffer.Shared ~format:Texture.Rgba8_unorm ~width:4
              ~height:4 ())
           with
           kind = Texture.Texture_cube
         })
  in
  (* A cube has six slices and no seventh. *)
  ignore
    (get
       (Texture.read_bytes cube ~region:full ~mip_level:0 ~slice:5 ~bytes_per_row:16
          ~bytes_per_image:64));
  reject Invalid_argument
    (Texture.read_bytes cube ~region:full ~mip_level:0 ~slice:6 ~bytes_per_row:16
       ~bytes_per_image:64);
  get (Texture.destroy cube);
  get (Texture.destroy texture);
  get (Texture.destroy texture);
  if not (Texture.destroyed texture) then fail "destroyed texture remained live";
  reject Destroyed (read texture ~region:full ~bytes_per_row:16 ~bytes_per_image:64)

(* The blit from a buffer: what it refuses, what it holds while encoded, and
   that a kernel then reads the copied texel. *)
let test_blit device queue =
  let texture =
    get
      (Texture.create ~device
         (Texture.descriptor_2d ~storage:Buffer.Private
            ~usage:[ Texture.Shader_read; Texture.Shader_write ]
            ~format:Texture.R8_unorm ~width:4 ~height:4 ()))
  in
  let volume =
    get
      (Texture.create ~device
         { (Texture.descriptor_2d ~storage:Buffer.Private ~format:Texture.R8_unorm ~width:1
              ~height:1 ())
           with
           kind = Texture.Texture_3d
         ; depth = 2
         })
  in
  let staging_bytes = Bytes.make 16 '\000' in
  Bytes.set staging_bytes 0 (Char.chr 37);
  let staging = get (Buffer.create ~device ~length:16L ~storage:Buffer.Shared ()) in
  get (Buffer.write_bytes staging ~dst_offset:0L staging_bytes);
  let region = full_region 4 4 in
  let commands = get (Command_buffer.create queue ()) in
  let blit = get (Blit_encoder.create commands) in
  reject Invalid_state (Blit_encoder.create commands);
  reject Invalid_state (Command_buffer.commit commands);
  let copy ?(source_offset = 0L) ?(bytes_per_row = 4) ?(bytes_per_image = 16)
      ?(destination = texture) ?(slice = 0) ?(level = 0) region =
    Blit_encoder.copy_buffer_to_texture blit ~source:staging ~source_offset
      ~source_bytes_per_row:bytes_per_row ~source_bytes_per_image:bytes_per_image ~destination
      ~destination_slice:slice ~destination_level:level ~destination_region:region
  in
  without_handles "refused blits" (fun () ->
    reject Invalid_argument (copy ~source_offset:(-1L) region);
    reject Invalid_argument
      (copy ~bytes_per_row:1 ~bytes_per_image:max_int ~destination:volume (full_region ~depth:2 1 1));
    reject Invalid_argument (copy ~bytes_per_row:3 region);
    reject Invalid_argument (copy ~bytes_per_image:15 region);
    reject Invalid_argument (copy ~source_offset:1L region);
    reject Invalid_argument (copy ~level:1 region);
    reject Invalid_argument (copy ~slice:1 region);
    reject Invalid_argument (copy { region with x = 1 });
    reject Invalid_argument (copy { region with width = 0 }));
  get (copy region);
  reject Parent_has_dependents (Buffer.destroy staging);
  reject Parent_has_dependents (Texture.destroy texture);
  get (Blit_encoder.end_encoding blit);
  reject Destroyed (copy region);
  reject Destroyed (Blit_encoder.end_encoding blit);
  complete commands;
  get (Buffer.destroy staging);
  let library = get (Library.compile_source ~device read_shader_source) in
  let read_function = get (Function.find ~library "texel_read") in
  let read_pipeline = get (Compute_pipeline.create read_function) in
  let output = get (Buffer.create ~device ~length:4L ~storage:Buffer.Shared ()) in
  let commands = get (Command_buffer.create queue ()) in
  let encoder = get (Compute_encoder.create commands) in
  get (Compute_encoder.set_pipeline encoder read_pipeline);
  reject Invalid_argument (Compute_encoder.set_texture encoder ~index:(-1) texture);
  reject Invalid_argument (Compute_encoder.set_texture encoder ~index:31 texture);
  get (Compute_encoder.set_texture encoder ~index:0 texture);
  get (Compute_encoder.set_buffer encoder ~index:0 ~offset:0L output);
  get (Compute_encoder.dispatch_threads encoder ~threads:(1, 1, 1) ~threadgroup:(1, 1, 1));
  get (Compute_encoder.end_encoding encoder);
  complete commands;
  let value = Bytes.get_int32_le (get (Buffer.read_bytes output ~offset:0L ~length:4)) 0 in
  if value <> 37l then fail "blitted texel read back as %ld, expected 37" value;
  get (Buffer.destroy output);
  get (Compute_pipeline.destroy read_pipeline);
  get (Function.destroy read_function);
  get (Library.destroy library);
  get (Texture.destroy volume);
  get (Texture.destroy texture)

let run () =
  match Device.system_default () with
  | Error _ -> print_endline "Metal texture safe: skipped (no device)"
  | Ok device ->
      let queue = get (Command_queue.create device) in
      test_format_matrix device queue;
      test_swizzle_and_compression device queue;
      test_transfer_and_descriptors device queue;
      test_blit device queue;
      get (Command_queue.destroy queue);
      get (Device.destroy device);
      ignore (settle_finalizers ~expected_live:0);
      print_endline
        "Metal texture safe: retained format matrix, swizzle, transfer and blit checks passed"
