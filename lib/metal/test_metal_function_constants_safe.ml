open Metal
open Test_metal_support

let shader_source =
  {|
#include <metal_stdlib>
using namespace metal;

kernel void increment(device uint *values [[buffer(0)]],
                      uint index [[thread_position_in_grid]]) {
  values[index] += 1;
}

constant uint increment_amount [[function_constant(0)]];
constant bool apply_twice [[function_constant(1)]];

kernel void specialized_increment(device uint *values [[buffer(0)]],
                                  uint index [[thread_position_in_grid]]) {
  values[index] += apply_twice ? increment_amount * 2u : increment_amount;
}

[[visible]] uint linked_identity(uint value) {
  return value;
}

constant char scalar_i8 [[function_constant(2)]];
constant uchar scalar_u8 [[function_constant(3)]];
constant short scalar_i16 [[function_constant(4)]];
constant ushort scalar_u16 [[function_constant(5)]];
constant int scalar_i32 [[function_constant(6)]];
constant uint scalar_u32 [[function_constant(7)]];
constant long scalar_i64 [[function_constant(8)]];
constant ulong scalar_u64 [[function_constant(9)]];
constant half scalar_f16 [[function_constant(10)]];
constant float scalar_f32 [[function_constant(11)]];
constant bool scalar_bool [[function_constant(12)]];

kernel void scalar_constant_sum(device uint *result [[buffer(0)]]) {
  result[0] = uint(scalar_i8) + uint(scalar_u8) + uint(scalar_i16)
      + uint(scalar_u16) + uint(scalar_i32) + scalar_u32
      + uint(scalar_i64) + uint(scalar_u64) + uint(scalar_f16)
      + uint(scalar_f32) + uint(scalar_bool);
}

constant int first [[function_constant(13)]];
constant int second [[function_constant(14)]];

kernel void constant_copy(device int *out [[buffer(0)]]) {
  out[0] = first * 100 + second;
}
|}

let input_values () = int32_bytes [| 1l; 41l; 99l; -2l |]
let expected_values = [| 2l; 42l; 100l; Int32.minus_one |]

let run_once device queue function_ ~initial =
  let pipeline = get (Compute_pipeline.create function_) in
  let output = get (Buffer.create ~device ~length:4L ~storage:Buffer.Shared ()) in
  get (Buffer.write_bytes output ~dst_offset:0L (int32_bytes [| initial |]));
  let commands = get (Command_buffer.create queue ()) in
  let encoder = get (Compute_encoder.create commands) in
  get (Compute_encoder.set_pipeline encoder pipeline);
  get (Compute_encoder.set_buffer encoder ~index:0 ~offset:0L output);
  get (Compute_encoder.dispatch_threads encoder ~threads:(1, 1, 1) ~threadgroup:(1, 1, 1));
  get (Compute_encoder.end_encoding encoder);
  complete commands;
  let result = Bytes.get_int32_le (get (Buffer.read_bytes output ~offset:0L ~length:4)) 0 in
  get (Buffer.destroy output);
  get (Compute_pipeline.destroy pipeline);
  result

let buffer_binding (binding : Binding.t) =
  match binding.kind with
  | Binding.Buffer_binding layout -> Some (binding.name, binding.index, binding.access, layout.data_type)
  | _ -> None

let run () =
  match Device.system_default () with
  | Error _ -> print_endline "metal function constants safe: skipped"
  | Ok device ->
      let info = get (Device.info device) in
      let invalid_shader =
        expect Native_error
          (Library.compile_source ~device ~label:"invalid shader diagnostic" "not a Metal program")
      in
      if invalid_shader.message = "" || not (contains invalid_shader.message "invalid shader diagnostic")
      then fail "shader compilation lost its full labeled diagnostic";
      let library =
        get (Library.compile_source ~device ~label:"Metal conformance library" shader_source)
      in
      let function_ = get (Function.find ~library "increment") in
      without_handles "invalid function lookups" (fun () ->
        reject Invalid_argument (Function.find ~library "");
        reject Invalid_argument (Function.find ~library "bad\000name"));
      without_handles "invalid function constants" (fun () ->
        reject Invalid_argument
          (Function.specialize ~library "specialized_increment"
             ~constants:
               [ "increment_amount", Function.Uint32_constant 1L
               ; "increment_amount", Function.Uint32_constant 2L
               ]);
        reject Invalid_argument
          (Function.specialize ~library "specialized_increment"
             ~constants:[ "increment_amount", Function.Uint32_constant 0x1_0000_0000L ]);
        reject Invalid_argument
          (Function.specialize ~library "specialized_increment"
             ~constants:[ "", Function.Uint32_constant 1L ]);
        reject Invalid_argument (Function.specialize ~library "" ~constants:[]);
        List.iter
          (fun value ->
            reject Invalid_argument
              (Function.specialize ~library "scalar_constant_sum"
                 ~constants:[ "scalar_i8", value ]))
          [ Function.Int8_constant 128; Function.Uint8_constant (-1); Function.Int16_constant 32_768
          ; Function.Uint16_constant 65_536
          ]);
      (* A constant of the wrong type is Metal's to refuse. *)
      reject Native_error
        (Function.specialize ~library "specialized_increment"
           ~constants:[ "increment_amount", Function.Bool_constant true ]);
      let specialized_function =
        get
          (Function.specialize ~library "specialized_increment"
             ~constants:
               [ "increment_amount", Function.Uint32_constant 7L
               ; "apply_twice", Function.Bool_constant true
               ])
      in
      let scalar_constant_function =
        get
          (Function.specialize ~library "scalar_constant_sum"
             ~constants:
               [ "scalar_i8", Function.Int8_constant (-2)
               ; "scalar_u8", Function.Uint8_constant 3
               ; "scalar_i16", Function.Int16_constant (-4)
               ; "scalar_u16", Function.Uint16_constant 5
               ; "scalar_i32", Function.Int32_constant (-6l)
               ; "scalar_u32", Function.Uint32_constant 7L
               ; "scalar_i64", Function.Int64_constant (-8L)
               ; "scalar_u64", Function.Uint64_bits_constant 9L
               ; "scalar_f16", Function.Float16_constant 10.
               ; "scalar_f32", Function.Float32_constant 11.
               ; "scalar_bool", Function.Bool_constant true
               ])
      in
      let linked_function =
        if info.function_pointers then Some (get (Function.find ~library "linked_identity")) else None
      in
      reject Wrong_domain
        (Domain.spawn (fun () -> Function.find ~library "increment") |> Domain.join);
      let stale_function = get (Function.find ~library "increment") in
      get (Function.destroy stale_function);
      reject Destroyed (Compute_pipeline.create stale_function);
      reject Destroyed (Function.argument_encoder stale_function ~buffer_index:0L);
      without_handles "invalid compute descriptors" (fun () ->
        reject Invalid_argument
          (Compute_pipeline.create ~label:"invalid\000label" specialized_function);
        (* A kernel cannot be linked, and a linked name appears once. *)
        reject Invalid_argument
          (Compute_pipeline.create ~linked_functions:[ function_ ] specialized_function);
        Option.iter
          (fun linked ->
            reject Invalid_argument
              (Compute_pipeline.create ~linked_functions:[ linked; linked ] specialized_function);
            reject Invalid_argument (Compute_pipeline.create linked))
          linked_function);
      reject Parent_has_dependents (Library.destroy library);
      let pipeline = get (Compute_pipeline.create function_) in
      let specialized_pipeline =
        get
          (Compute_pipeline.create ~label:"specialized reflected pipeline"
             ~linked_functions:(Option.to_list linked_function) ~reflection:true
             specialized_function)
      in
      let scalar_constant_pipeline = get (Compute_pipeline.create scalar_constant_function) in
      if Compute_pipeline.bindings pipeline <> None then
        fail "reflection was kept without being asked for";
      let reflected_bindings =
        match Compute_pipeline.bindings specialized_pipeline with
        | Some bindings -> bindings
        | None -> fail "requested compute pipeline reflection is missing"
      in
      (match List.filter_map buffer_binding reflected_bindings with
       | [ ("values", 0L, Binding.Read_write, Shader_type.Scalar Shader_type.Uint) ] -> ()
       | _ -> fail "compute reflection does not describe the one read-write uint buffer");
      if List.length reflected_bindings <> 1 then fail "compute reflection invented a binding";
      let buffer = get (Buffer.create ~device ~length:16L ~storage:Buffer.Shared ()) in
      get (Buffer.write_bytes buffer ~dst_offset:0L (input_values ()));
      let specialized_buffer = get (Buffer.create ~device ~length:4L ~storage:Buffer.Shared ()) in
      get (Buffer.write_bytes specialized_buffer ~dst_offset:0L (int32_bytes [| 10l |]));
      let scalar_constant_buffer =
        get (Buffer.create ~device ~length:4L ~storage:Buffer.Shared ())
      in
      let queue = get (Command_queue.create device) in
      let commands = get (Command_buffer.create queue ()) in
      if not (Device.same (Command_buffer.device commands) device) then
        fail "command buffer device drift";
      (match get (Command_buffer.status commands) with
       | Command_buffer.Not_enqueued -> ()
       | _ -> fail "a new command buffer is already enqueued");
      reject Invalid_state (Command_buffer.wait_until_completed commands);
      let encoder = get (Compute_encoder.create commands) in
      reject Invalid_state (Compute_encoder.create commands);
      get (Compute_encoder.set_pipeline encoder pipeline);
      if get (Buffer.read_bytes buffer ~offset:0L ~length:16) <> input_values () then
        fail "compute input changed before command encoding";
      get (Compute_encoder.set_buffer encoder ~index:0 ~offset:0L buffer);
      reject Parent_has_dependents (Buffer.destroy buffer);
      reject Invalid_argument
        (Compute_encoder.dispatch_threads encoder ~threads:(4, 1, 1) ~threadgroup:(max_int, 2, 1));
      get (Compute_encoder.dispatch_threads encoder ~threads:(4, 1, 1) ~threadgroup:(4, 1, 1));
      get (Compute_encoder.set_pipeline encoder specialized_pipeline);
      get (Compute_encoder.set_buffer encoder ~index:0 ~offset:0L specialized_buffer);
      get (Compute_encoder.dispatch_threads encoder ~threads:(1, 1, 1) ~threadgroup:(1, 1, 1));
      get (Compute_encoder.set_pipeline encoder scalar_constant_pipeline);
      get (Compute_encoder.set_buffer encoder ~index:0 ~offset:0L scalar_constant_buffer);
      get (Compute_encoder.dispatch_threads encoder ~threads:(1, 1, 1) ~threadgroup:(1, 1, 1));
      reject Invalid_state (Command_buffer.commit commands);
      get (Compute_encoder.end_encoding encoder);
      get (Command_buffer.commit commands);
      reject Invalid_state (Command_buffer.commit commands);
      reject Invalid_state (Compute_encoder.create commands);
      get (Command_buffer.wait_until_completed commands);
      (match get (Command_buffer.status commands) with
       | Command_buffer.Completed -> ()
       | _ -> fail "command buffer did not complete");
      let output = get (Buffer.read_bytes buffer ~offset:0L ~length:16) in
      Array.iteri
        (fun index expected ->
          let actual = Bytes.get_int32_le output (index * 4) in
          if actual <> expected then
            fail "compute output %d: expected %ld, got %ld" index expected actual)
        expected_values;
      if Bytes.get_int32_le (get (Buffer.read_bytes specialized_buffer ~offset:0L ~length:4)) 0
         <> 24l
      then fail "specialized function constants did not execute on the GPU";
      if Bytes.get_int32_le (get (Buffer.read_bytes scalar_constant_buffer ~offset:0L ~length:4)) 0
         <> 26l
      then fail "typed scalar function constants did not round-trip to the GPU";
      get (Buffer.destroy scalar_constant_buffer);
      get (Buffer.destroy specialized_buffer);
      get (Buffer.destroy buffer);
      get (Command_buffer.destroy commands);
      if not (Command_buffer.destroyed commands) then fail "destroyed command buffer remained live";
      (* The values a constant carries are the ones passed, each call its own:
         7 and 11 give 711, and a second specialization does not see them. *)
      let copy first second =
        let specialized =
          get
            (Function.specialize ~library "constant_copy"
               ~constants:
                 [ "first", Function.Int32_constant first; "second", Function.Int32_constant second ])
        in
        let result = run_once device queue specialized ~initial:(-1l) in
        get (Function.destroy specialized);
        result
      in
      if copy 7l 11l <> 711l then fail "function constants did not copy the original typed values";
      if copy 0l 0l <> 0l then fail "a later specialization kept stale function constant values";
      if copy (-3l) 5l <> -295l then fail "signed function constants lost their sign";
      get (Command_queue.destroy queue);
      get (Compute_pipeline.destroy scalar_constant_pipeline);
      get (Compute_pipeline.destroy specialized_pipeline);
      get (Compute_pipeline.destroy pipeline);
      Option.iter (fun value -> get (Function.destroy value)) linked_function;
      get (Function.destroy scalar_constant_function);
      get (Function.destroy specialized_function);
      get (Function.destroy function_);
      get (Library.destroy library);
      reject Destroyed (Function.find ~library "increment");
      reject Destroyed
        (Function.specialize ~library "specialized_increment"
           ~constants:[ "increment_amount", Function.Uint32_constant 1L ]);
      get (Device.destroy device);
      ignore (settle_finalizers ~expected_live:0);
      print_endline "metal function constants safe: ok"
