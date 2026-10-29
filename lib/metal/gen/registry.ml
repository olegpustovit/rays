type scalar = Bool | Int | Nsuint | Nsuint_int | Nsint | Float | Double

(* A value crossing a generated call. [Obj kind] is a borrowed handle of the
   bridge's [Handle_kind::kind] (retained when returned); [Enum_of c] is a
   C enum or options type carried as int64; [Str] is NSString (invalid UTF-8
   is an [Error]); [Rec r] passes the [Record] entry [r] by value, and
   [Tuple r] uses the same native struct with an OCaml tuple ABI.
   [Nsuint_int] keeps bounded native indices as OCaml machine integers. *)
type ty =
  | Scalar of scalar
  | Enum_of of string
  | Str
  | Obj of string
  | Opt_obj of string
  | Rec of string
  | Tuple of string

type access = Get | Set | Get_set
type enum_case = string * string * int64 * (int * int * int)
type sdk_type = Protocol of string | Class of string

let feature_name : Ogpu_core.Caps.feature -> string =
  let open Ogpu_core.Caps in
  function
  | Buffer -> "buffer"
  | Texture -> "texture"
  | Sampler -> "sampler"
  | Compute_pipeline -> "compute_pipeline"
  | Render_pipeline -> "render_pipeline"
  | Queue -> "queue"
  | Surface -> "surface"
  | Ray_tracing -> "ray_tracing"
  | Ray_tracing_curves -> "ray_tracing_curves"
  | Ray_tracing_motion -> "ray_tracing_motion"
  | Ray_tracing_refit -> "ray_tracing_refit"
  | Unknown name -> name

let feature_map : (Ogpu_core.Caps.feature * sdk_type list) list =
  let open Ogpu_core.Caps in
  [
    (Buffer, [ Protocol "MTLBuffer" ]);
    (Texture, [ Protocol "MTLTexture" ]);
    (Sampler, [ Protocol "MTLSamplerState" ]);
    (Compute_pipeline, [ Protocol "MTLComputePipelineState" ]);
    (Render_pipeline, [ Protocol "MTLRenderPipelineState" ]);
    (Queue, [ Protocol "MTLCommandQueue" ]);
    (Surface, [ Class "CAMetalLayer" ]);
    (Ray_tracing, [ Protocol "MTLAccelerationStructure" ]);
    (Ray_tracing_curves, [ Class "MTLAccelerationStructureCurveGeometryDescriptor" ]);
    (Ray_tracing_motion, [ Class "MTLMotionKeyframeData" ]);
    (Ray_tracing_refit, [ Protocol "MTLAccelerationStructureCommandEncoder" ]);
  ]

type entry =
  (* Custom marshalling/lifecycle code still implements these primitives.
     Keep its ABI here as well, so the audit covers every raw binding. *)
  | Native of { ocaml : string; signature : string; primitives : string list; reason : string }
  | Class_method of {
      objc : string;
      sel : string;
      args : ty list;
      ret : ty option;
      ocaml : string;
      since : (int * int) option;
      feature : Ogpu_core.Caps.feature;
    }
  | Enum of {
      sdk : string;
      ocaml : string;
      cases : enum_case list;
      feature : Ogpu_core.Caps.feature;
    }
  | Record of {
      sdk : string;
      ocaml : string;
      fields : (string * scalar) list;
      feature : Ogpu_core.Caps.feature;
    }
  (* [objc] receives [sel] (colons included) with [args]; [error] appends a
     trailing NSError** and turns failure into [Error]. [recv] is the
     receiver's Handle_kind, [objc] its Objective-C type. [pool = false]
     preserves pool-free encoding calls returning unit without NSString or
     NSError temporaries; exception conversion still owns a pool. *)
  | Method of {
      recv : string;
      objc : string;
      sel : string;
      args : ty list;
      ret : ty option;
      error : bool;
      pool : bool;
      ocaml : string;
      since : (int * int) option;
      feature : Ogpu_core.Caps.feature;
    }
  (* An Objective-C property: [Get] generates [ocaml], [Set] [set_ocaml]. *)
  | Property of {
      recv : string;
      objc : string;
      name : string;
      ty : ty;
      access : access;
      ocaml : string;
      since : (int * int) option;
      feature : Ogpu_core.Caps.feature;
    }

module C = Ogpu_core.Caps

let entries : entry list =
  [
    Enum
      {
        sdk = "MTLDataType";
        ocaml = "Mtl_data_type";
        cases =
          [
            ("mtl_data_type_array", "MTLDataTypeArray", 0x0000000000000002L, (10, 11, 0));
            ("mtl_data_type_b_float", "MTLDataTypeBFloat", 0x0000000000000079L, (14, 0, 0));
            ("mtl_data_type_b_float2", "MTLDataTypeBFloat2", 0x000000000000007aL, (14, 0, 0));
            ("mtl_data_type_b_float3", "MTLDataTypeBFloat3", 0x000000000000007bL, (14, 0, 0));
            ("mtl_data_type_b_float4", "MTLDataTypeBFloat4", 0x000000000000007cL, (14, 0, 0));
            ("mtl_data_type_bool", "MTLDataTypeBool", 0x0000000000000035L, (10, 11, 0));
            ("mtl_data_type_bool2", "MTLDataTypeBool2", 0x0000000000000036L, (10, 11, 0));
            ("mtl_data_type_bool3", "MTLDataTypeBool3", 0x0000000000000037L, (10, 11, 0));
            ("mtl_data_type_bool4", "MTLDataTypeBool4", 0x0000000000000038L, (10, 11, 0));
            ("mtl_data_type_char", "MTLDataTypeChar", 0x000000000000002dL, (10, 11, 0));
            ("mtl_data_type_char2", "MTLDataTypeChar2", 0x000000000000002eL, (10, 11, 0));
            ("mtl_data_type_char3", "MTLDataTypeChar3", 0x000000000000002fL, (10, 11, 0));
            ("mtl_data_type_char4", "MTLDataTypeChar4", 0x0000000000000030L, (10, 11, 0));
            ( "mtl_data_type_compute_pipeline",
              "MTLDataTypeComputePipeline",
              0x000000000000004fL,
              (11, 0, 0) );
            ( "mtl_data_type_depth_stencil_state",
              "MTLDataTypeDepthStencilState",
              0x000000000000008bL,
              (26, 0, 0) );
            ("mtl_data_type_float", "MTLDataTypeFloat", 0x0000000000000003L, (10, 11, 0));
            ("mtl_data_type_float2", "MTLDataTypeFloat2", 0x0000000000000004L, (10, 11, 0));
            ("mtl_data_type_float2x2", "MTLDataTypeFloat2x2", 0x0000000000000007L, (10, 11, 0));
            ("mtl_data_type_float2x3", "MTLDataTypeFloat2x3", 0x0000000000000008L, (10, 11, 0));
            ("mtl_data_type_float2x4", "MTLDataTypeFloat2x4", 0x0000000000000009L, (10, 11, 0));
            ("mtl_data_type_float3", "MTLDataTypeFloat3", 0x0000000000000005L, (10, 11, 0));
            ("mtl_data_type_float3x2", "MTLDataTypeFloat3x2", 0x000000000000000aL, (10, 11, 0));
            ("mtl_data_type_float3x3", "MTLDataTypeFloat3x3", 0x000000000000000bL, (10, 11, 0));
            ("mtl_data_type_float3x4", "MTLDataTypeFloat3x4", 0x000000000000000cL, (10, 11, 0));
            ("mtl_data_type_float4", "MTLDataTypeFloat4", 0x0000000000000006L, (10, 11, 0));
            ("mtl_data_type_float4x2", "MTLDataTypeFloat4x2", 0x000000000000000dL, (10, 11, 0));
            ("mtl_data_type_float4x3", "MTLDataTypeFloat4x3", 0x000000000000000eL, (10, 11, 0));
            ("mtl_data_type_float4x4", "MTLDataTypeFloat4x4", 0x000000000000000fL, (10, 11, 0));
            ("mtl_data_type_half", "MTLDataTypeHalf", 0x0000000000000010L, (10, 11, 0));
            ("mtl_data_type_half2", "MTLDataTypeHalf2", 0x0000000000000011L, (10, 11, 0));
            ("mtl_data_type_half2x2", "MTLDataTypeHalf2x2", 0x0000000000000014L, (10, 11, 0));
            ("mtl_data_type_half2x3", "MTLDataTypeHalf2x3", 0x0000000000000015L, (10, 11, 0));
            ("mtl_data_type_half2x4", "MTLDataTypeHalf2x4", 0x0000000000000016L, (10, 11, 0));
            ("mtl_data_type_half3", "MTLDataTypeHalf3", 0x0000000000000012L, (10, 11, 0));
            ("mtl_data_type_half3x2", "MTLDataTypeHalf3x2", 0x0000000000000017L, (10, 11, 0));
            ("mtl_data_type_half3x3", "MTLDataTypeHalf3x3", 0x0000000000000018L, (10, 11, 0));
            ("mtl_data_type_half3x4", "MTLDataTypeHalf3x4", 0x0000000000000019L, (10, 11, 0));
            ("mtl_data_type_half4", "MTLDataTypeHalf4", 0x0000000000000013L, (10, 11, 0));
            ("mtl_data_type_half4x2", "MTLDataTypeHalf4x2", 0x000000000000001aL, (10, 11, 0));
            ("mtl_data_type_half4x3", "MTLDataTypeHalf4x3", 0x000000000000001bL, (10, 11, 0));
            ("mtl_data_type_half4x4", "MTLDataTypeHalf4x4", 0x000000000000001cL, (10, 11, 0));
            ( "mtl_data_type_indirect_command_buffer",
              "MTLDataTypeIndirectCommandBuffer",
              0x0000000000000050L,
              (10, 14, 0) );
            ( "mtl_data_type_instance_acceleration_structure",
              "MTLDataTypeInstanceAccelerationStructure",
              0x0000000000000076L,
              (11, 0, 0) );
            ("mtl_data_type_int", "MTLDataTypeInt", 0x000000000000001dL, (10, 11, 0));
            ("mtl_data_type_int2", "MTLDataTypeInt2", 0x000000000000001eL, (10, 11, 0));
            ("mtl_data_type_int3", "MTLDataTypeInt3", 0x000000000000001fL, (10, 11, 0));
            ("mtl_data_type_int4", "MTLDataTypeInt4", 0x0000000000000020L, (10, 11, 0));
            ( "mtl_data_type_intersection_function_table",
              "MTLDataTypeIntersectionFunctionTable",
              0x0000000000000074L,
              (11, 0, 0) );
            ("mtl_data_type_long", "MTLDataTypeLong", 0x0000000000000051L, (12, 0, 0));
            ("mtl_data_type_long2", "MTLDataTypeLong2", 0x0000000000000052L, (12, 0, 0));
            ("mtl_data_type_long3", "MTLDataTypeLong3", 0x0000000000000053L, (12, 0, 0));
            ("mtl_data_type_long4", "MTLDataTypeLong4", 0x0000000000000054L, (12, 0, 0));
            ("mtl_data_type_none", "MTLDataTypeNone", 0x0000000000000000L, (10, 11, 0));
            ("mtl_data_type_pointer", "MTLDataTypePointer", 0x000000000000003cL, (10, 13, 0));
            ( "mtl_data_type_primitive_acceleration_structure",
              "MTLDataTypePrimitiveAccelerationStructure",
              0x0000000000000075L,
              (11, 0, 0) );
            ("mtl_data_type_r16_snorm", "MTLDataTypeR16Snorm", 0x0000000000000041L, (11, 0, 0));
            ("mtl_data_type_r16_unorm", "MTLDataTypeR16Unorm", 0x0000000000000040L, (11, 0, 0));
            ("mtl_data_type_r8_snorm", "MTLDataTypeR8Snorm", 0x000000000000003fL, (11, 0, 0));
            ("mtl_data_type_r8_unorm", "MTLDataTypeR8Unorm", 0x000000000000003eL, (11, 0, 0));
            ( "mtl_data_type_rg11_b10_float",
              "MTLDataTypeRG11B10Float",
              0x000000000000004cL,
              (11, 0, 0) );
            ("mtl_data_type_rg16_snorm", "MTLDataTypeRG16Snorm", 0x0000000000000045L, (11, 0, 0));
            ("mtl_data_type_rg16_unorm", "MTLDataTypeRG16Unorm", 0x0000000000000044L, (11, 0, 0));
            ("mtl_data_type_rg8_snorm", "MTLDataTypeRG8Snorm", 0x0000000000000043L, (11, 0, 0));
            ("mtl_data_type_rg8_unorm", "MTLDataTypeRG8Unorm", 0x0000000000000042L, (11, 0, 0));
            ( "mtl_data_type_rgb10_a2_unorm",
              "MTLDataTypeRGB10A2Unorm",
              0x000000000000004bL,
              (11, 0, 0) );
            ( "mtl_data_type_rgb9_e5_float",
              "MTLDataTypeRGB9E5Float",
              0x000000000000004dL,
              (11, 0, 0) );
            ("mtl_data_type_rgba16_snorm", "MTLDataTypeRGBA16Snorm", 0x000000000000004aL, (11, 0, 0));
            ("mtl_data_type_rgba16_unorm", "MTLDataTypeRGBA16Unorm", 0x0000000000000049L, (11, 0, 0));
            ("mtl_data_type_rgba8_snorm", "MTLDataTypeRGBA8Snorm", 0x0000000000000048L, (11, 0, 0));
            ("mtl_data_type_rgba8_unorm", "MTLDataTypeRGBA8Unorm", 0x0000000000000046L, (11, 0, 0));
            ( "mtl_data_type_rgba8_unorm_s_rgb",
              "MTLDataTypeRGBA8Unorm_sRGB",
              0x0000000000000047L,
              (11, 0, 0) );
            ( "mtl_data_type_render_pipeline",
              "MTLDataTypeRenderPipeline",
              0x000000000000004eL,
              (10, 14, 0) );
            ("mtl_data_type_sampler", "MTLDataTypeSampler", 0x000000000000003bL, (10, 13, 0));
            ("mtl_data_type_short", "MTLDataTypeShort", 0x0000000000000025L, (10, 11, 0));
            ("mtl_data_type_short2", "MTLDataTypeShort2", 0x0000000000000026L, (10, 11, 0));
            ("mtl_data_type_short3", "MTLDataTypeShort3", 0x0000000000000027L, (10, 11, 0));
            ("mtl_data_type_short4", "MTLDataTypeShort4", 0x0000000000000028L, (10, 11, 0));
            ("mtl_data_type_struct", "MTLDataTypeStruct", 0x0000000000000001L, (10, 11, 0));
            ("mtl_data_type_tensor", "MTLDataTypeTensor", 0x000000000000008cL, (26, 0, 0));
            ("mtl_data_type_texture", "MTLDataTypeTexture", 0x000000000000003aL, (10, 13, 0));
            ("mtl_data_type_u_char", "MTLDataTypeUChar", 0x0000000000000031L, (10, 11, 0));
            ("mtl_data_type_u_char2", "MTLDataTypeUChar2", 0x0000000000000032L, (10, 11, 0));
            ("mtl_data_type_u_char3", "MTLDataTypeUChar3", 0x0000000000000033L, (10, 11, 0));
            ("mtl_data_type_u_char4", "MTLDataTypeUChar4", 0x0000000000000034L, (10, 11, 0));
            ("mtl_data_type_u_int", "MTLDataTypeUInt", 0x0000000000000021L, (10, 11, 0));
            ("mtl_data_type_u_int2", "MTLDataTypeUInt2", 0x0000000000000022L, (10, 11, 0));
            ("mtl_data_type_u_int3", "MTLDataTypeUInt3", 0x0000000000000023L, (10, 11, 0));
            ("mtl_data_type_u_int4", "MTLDataTypeUInt4", 0x0000000000000024L, (10, 11, 0));
            ("mtl_data_type_u_long", "MTLDataTypeULong", 0x0000000000000055L, (12, 0, 0));
            ("mtl_data_type_u_long2", "MTLDataTypeULong2", 0x0000000000000056L, (12, 0, 0));
            ("mtl_data_type_u_long3", "MTLDataTypeULong3", 0x0000000000000057L, (12, 0, 0));
            ("mtl_data_type_u_long4", "MTLDataTypeULong4", 0x0000000000000058L, (12, 0, 0));
            ("mtl_data_type_u_short", "MTLDataTypeUShort", 0x0000000000000029L, (10, 11, 0));
            ("mtl_data_type_u_short2", "MTLDataTypeUShort2", 0x000000000000002aL, (10, 11, 0));
            ("mtl_data_type_u_short3", "MTLDataTypeUShort3", 0x000000000000002bL, (10, 11, 0));
            ("mtl_data_type_u_short4", "MTLDataTypeUShort4", 0x000000000000002cL, (10, 11, 0));
            ( "mtl_data_type_visible_function_table",
              "MTLDataTypeVisibleFunctionTable",
              0x0000000000000073L,
              (11, 0, 0) );
          ];
        feature = Ogpu_core.Caps.Compute_pipeline;
      };
    Enum
      {
        sdk = "MTLFeatureSet";
        ocaml = "Mtl_feature_set";
        cases =
          [
            ( "mtl_feature_set_osx_gpu_family1_v1",
              "MTLFeatureSet_OSX_GPUFamily1_v1",
              0x0000000000002710L,
              (10, 11, 0) );
            ( "mtl_feature_set_osx_gpu_family1_v2",
              "MTLFeatureSet_OSX_GPUFamily1_v2",
              0x0000000000002711L,
              (10, 12, 0) );
            ( "mtl_feature_set_osx_read_write_texture_tier2",
              "MTLFeatureSet_OSX_ReadWriteTextureTier2",
              0x0000000000002712L,
              (10, 12, 0) );
            ( "mtl_feature_set_mac_os_gpu_family1_v1",
              "MTLFeatureSet_macOS_GPUFamily1_v1",
              0x0000000000002710L,
              (10, 11, 0) );
            ( "mtl_feature_set_mac_os_gpu_family1_v2",
              "MTLFeatureSet_macOS_GPUFamily1_v2",
              0x0000000000002711L,
              (10, 12, 0) );
            ( "mtl_feature_set_mac_os_gpu_family1_v3",
              "MTLFeatureSet_macOS_GPUFamily1_v3",
              0x0000000000002713L,
              (10, 13, 0) );
            ( "mtl_feature_set_mac_os_gpu_family1_v4",
              "MTLFeatureSet_macOS_GPUFamily1_v4",
              0x0000000000002714L,
              (10, 14, 0) );
            ( "mtl_feature_set_mac_os_gpu_family2_v1",
              "MTLFeatureSet_macOS_GPUFamily2_v1",
              0x0000000000002715L,
              (10, 14, 0) );
            ( "mtl_feature_set_mac_os_read_write_texture_tier2",
              "MTLFeatureSet_macOS_ReadWriteTextureTier2",
              0x0000000000002712L,
              (10, 12, 0) );
          ];
        feature = Ogpu_core.Caps.Buffer;
      };
    Record
      {
        sdk = "MTLViewport";
        ocaml = "Mtl_viewport";
        fields =
          [
            ("originX", Double);
            ("originY", Double);
            ("width", Double);
            ("height", Double);
            ("znear", Double);
            ("zfar", Double);
          ];
        feature = Ogpu_core.Caps.Render_pipeline;
      };
    Record
      {
        sdk = "MTLScissorRect";
        ocaml = "Mtl_scissor_rect";
        fields = [ ("x", Nsuint); ("y", Nsuint); ("width", Nsuint); ("height", Nsuint) ];
        feature = Ogpu_core.Caps.Render_pipeline;
      };
    Method
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        sel = "supportsFamily:";
        args = [ Enum_of "MTLGPUFamily" ];
        ret = Some (Scalar Bool);
        error = false;
        ocaml = "device_supports_family";
        since = None;
        feature = Ogpu_core.Caps.Buffer;
        pool = true;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "maxThreadgroupMemoryLength";
        ty = Scalar Nsuint;
        access = Get;
        ocaml = "device_max_threadgroup_memory_length";
        since = Some (10, 13);
        feature = Ogpu_core.Caps.Compute_pipeline;
      };
    Property
      {
        recv = "Render_pipeline";
        objc = "id<MTLRenderPipelineState>";
        name = "supportIndirectCommandBuffers";
        ty = Scalar Bool;
        access = Get;
        ocaml = "render_pipeline_state_support_indirect_command_buffers";
        since = Some (10, 14);
        feature = Ogpu_core.Caps.Render_pipeline;
      }
    (* MTLDevice *);
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "name";
        ty = Str;
        access = Get;
        ocaml = "device_name";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "registryID";
        ty = Scalar Nsuint;
        access = Get;
        ocaml = "device_registry_id";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "lowPower";
        ty = Scalar Bool;
        access = Get;
        ocaml = "device_is_low_power";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "removable";
        ty = Scalar Bool;
        access = Get;
        ocaml = "device_is_removable";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "headless";
        ty = Scalar Bool;
        access = Get;
        ocaml = "device_is_headless";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "hasUnifiedMemory";
        ty = Scalar Bool;
        access = Get;
        ocaml = "device_has_unified_memory";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "recommendedMaxWorkingSetSize";
        ty = Scalar Nsuint;
        access = Get;
        ocaml = "device_recommended_max_working_set_size";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "currentAllocatedSize";
        ty = Scalar Nsuint;
        access = Get;
        ocaml = "device_current_allocated_size";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "maxBufferLength";
        ty = Scalar Nsuint;
        access = Get;
        ocaml = "device_max_buffer_length";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "supportsRaytracing";
        ty = Scalar Bool;
        access = Get;
        ocaml = "device_supports_raytracing";
        since = None;
        feature = C.Ray_tracing;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "supportsRaytracingFromRender";
        ty = Scalar Bool;
        access = Get;
        ocaml = "device_supports_raytracing_from_render";
        since = None;
        feature = C.Ray_tracing;
      };
    Property
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        name = "supportsDynamicLibraries";
        ty = Scalar Bool;
        access = Get;
        ocaml = "device_supports_dynamic_libraries";
        since = None;
        feature = C.Buffer;
      };
    Method
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        sel = "supportsTextureSampleCount:";
        args = [ Scalar Nsuint ];
        ret = Some (Scalar Bool);
        error = false;
        ocaml = "device_supports_texture_sample_count";
        since = None;
        feature = C.Texture;
        pool = true;
      };
    Method
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        sel = "newCommandQueue";
        args = [];
        ret = Some (Obj "Command_queue");
        error = false;
        ocaml = "command_queue_create";
        since = None;
        feature = C.Queue;
        pool = true;
      }
    (* MTLCommandQueue, MTLCommandBuffer and encoder lifetimes *);
    Method
      {
        recv = "Command_queue";
        objc = "id<MTLCommandQueue>";
        sel = "commandBuffer";
        args = [];
        ret = Some (Obj "Command_buffer");
        error = false;
        ocaml = "command_buffer_create";
        since = None;
        feature = C.Queue;
        pool = true;
      };
    Property
      {
        recv = "Command_buffer";
        objc = "id<MTLCommandBuffer>";
        name = "label";
        ty = Str;
        access = Set;
        ocaml = "command_buffer_label";
        since = None;
        feature = C.Queue;
      };
    Property
      {
        recv = "Command_buffer";
        objc = "id<MTLCommandBuffer>";
        name = "status";
        ty = Enum_of "MTLCommandBufferStatus";
        access = Get;
        ocaml = "command_buffer_status";
        since = None;
        feature = C.Queue;
      };
    Method
      {
        recv = "Command_buffer";
        objc = "id<MTLCommandBuffer>";
        sel = "commit";
        args = [];
        ret = None;
        error = false;
        ocaml = "command_buffer_commit";
        since = None;
        feature = C.Queue;
        pool = true;
      };
    Method
      {
        recv = "Command_buffer";
        objc = "id<MTLCommandBuffer>";
        sel = "computeCommandEncoder";
        args = [];
        ret = Some (Obj "Compute_encoder");
        error = false;
        ocaml = "command_buffer_compute_encoder";
        since = None;
        feature = C.Compute_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Command_buffer";
        objc = "id<MTLCommandBuffer>";
        sel = "blitCommandEncoder";
        args = [];
        ret = Some (Obj "Blit_encoder");
        error = false;
        ocaml = "command_buffer_blit_encoder";
        since = None;
        feature = C.Buffer;
        pool = true;
      };
    Method
      {
        recv = "Command_buffer";
        objc = "id<MTLCommandBuffer>";
        sel = "accelerationStructureCommandEncoder";
        args = [];
        ret = Some (Obj "Acceleration_encoder");
        error = false;
        ocaml = "command_buffer_acceleration_encoder";
        since = None;
        feature = C.Ray_tracing;
        pool = true;
      };
    Method
      {
        recv = "Command_buffer";
        objc = "id<MTLCommandBuffer>";
        sel = "renderCommandEncoderWithDescriptor:";
        args = [ Obj "Render_pass_descriptor" ];
        ret = Some (Obj "Render_encoder");
        error = false;
        ocaml = "command_buffer_render_encoder_from_pass";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Compute_encoder";
        objc = "id<MTLComputeCommandEncoder>";
        sel = "endEncoding";
        args = [];
        ret = None;
        error = false;
        ocaml = "compute_encoder_end";
        since = None;
        feature = C.Compute_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "endEncoding";
        args = [];
        ret = None;
        error = false;
        ocaml = "render_encoder_end";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Blit_encoder";
        objc = "id<MTLBlitCommandEncoder>";
        sel = "endEncoding";
        args = [];
        ret = None;
        error = false;
        ocaml = "blit_encoder_end";
        since = None;
        feature = C.Buffer;
        pool = true;
      };
    Method
      {
        recv = "Acceleration_encoder";
        objc = "id<MTLAccelerationStructureCommandEncoder>";
        sel = "endEncoding";
        args = [];
        ret = None;
        error = false;
        ocaml = "acceleration_encoder_end";
        since = None;
        feature = C.Ray_tracing;
        pool = true;
      }
    (* MTLRenderCommandEncoder state and draws *);
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setRenderPipelineState:";
        args = [ Obj "Render_pipeline" ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_pipeline";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setVertexBuffer:offset:atIndex:";
        args = [ Obj "Buffer"; Scalar Nsuint; Scalar Nsuint ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_vertex_buffer";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setFragmentBuffer:offset:atIndex:";
        args = [ Obj "Buffer"; Scalar Nsuint; Scalar Nsuint ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_fragment_buffer";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setVertexTexture:atIndex:";
        args = [ Obj "Texture"; Scalar Nsuint ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_vertex_texture";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setFragmentTexture:atIndex:";
        args = [ Obj "Texture"; Scalar Nsuint ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_fragment_texture";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setVertexSamplerState:atIndex:";
        args = [ Obj "Sampler"; Scalar Nsuint ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_vertex_sampler";
        since = None;
        feature = C.Sampler;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setFragmentSamplerState:atIndex:";
        args = [ Obj "Sampler"; Scalar Nsuint ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_fragment_sampler";
        since = None;
        feature = C.Sampler;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setDepthStencilState:";
        args = [ Opt_obj "Depth_stencil" ];
        ret = None;
        error = false;
        ocaml = "render_depth_stencil";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setViewport:";
        args = [ Rec "Mtl_viewport" ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_viewport";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setScissorRect:";
        args = [ Rec "Mtl_scissor_rect" ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_scissor";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setCullMode:";
        args = [ Enum_of "MTLCullMode" ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_cull_mode";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "setFrontFacingWinding:";
        args = [ Enum_of "MTLWinding" ];
        ret = None;
        error = false;
        ocaml = "render_encoder_set_winding";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "drawPrimitives:vertexStart:vertexCount:instanceCount:";
        args = [ Enum_of "MTLPrimitiveType"; Scalar Nsuint; Scalar Nsuint; Scalar Nsuint ];
        ret = None;
        error = false;
        ocaml = "render_encoder_draw_primitives";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel = "drawIndexedPrimitives:indexCount:indexType:indexBuffer:indexBufferOffset:";
        args =
          [
            Enum_of "MTLPrimitiveType";
            Scalar Nsuint;
            Enum_of "MTLIndexType";
            Obj "Buffer";
            Scalar Nsuint;
          ];
        ret = None;
        error = false;
        ocaml = "render_draw_indexed_basic";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel =
          "drawIndexedPrimitives:indexCount:indexType:indexBuffer:indexBufferOffset:instanceCount:";
        args =
          [
            Enum_of "MTLPrimitiveType";
            Scalar Nsuint;
            Enum_of "MTLIndexType";
            Obj "Buffer";
            Scalar Nsuint;
            Scalar Nsuint;
          ];
        ret = None;
        error = false;
        ocaml = "render_draw_indexed_instances";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Render_encoder";
        objc = "id<MTLRenderCommandEncoder>";
        sel =
          "drawIndexedPrimitives:indexType:indexBuffer:indexBufferOffset:indirectBuffer:indirectBufferOffset:";
        args =
          [
            Enum_of "MTLPrimitiveType";
            Enum_of "MTLIndexType";
            Obj "Buffer";
            Scalar Nsuint;
            Obj "Buffer";
            Scalar Nsuint;
          ];
        ret = None;
        error = false;
        ocaml = "render_draw_indexed_indirect";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      }
    (* MTLResidencySet *);
    Native
      {
        ocaml = "is_main_thread";
        signature = "unit -> bool";
        primitives = [ "caml_rays_metal_is_main_thread" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "destroy";
        signature = "handle -> bool";
        primitives = [ "caml_rays_metal_destroy" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "drain_releases";
        signature = "unit -> int";
        primitives = [ "caml_rays_metal_drain_releases" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "pending_releases";
        signature = "unit -> int";
        primitives = [ "caml_rays_metal_pending_releases" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "live_handles";
        signature = "unit -> int";
        primitives = [ "caml_rays_metal_live_handles" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "total_created";
        signature = "unit -> int64";
        primitives = [ "caml_rays_metal_total_created" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "total_released";
        signature = "unit -> int64";
        primitives = [ "caml_rays_metal_total_released" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "external_deallocations";
        signature = "unit -> int64";
        primitives = [ "caml_rays_metal_external_deallocations" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "external_deallocation_mismatches";
        signature = "unit -> int64";
        primitives = [ "caml_rays_metal_external_deallocation_mismatches" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "resident_bytes";
        signature = "unit -> int64";
        primitives = [ "caml_rays_metal_resident_bytes" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "default_device";
        signature = "unit -> (handle, string) result";
        primitives = [ "caml_rays_metal_default_device" ];
        reason = "platform/handle lifecycle; not an Objective-C instance call";
      };
    Native
      {
        ocaml = "device_supports_sampler_reduction";
        signature = "handle -> bool";
        primitives = [ "caml_rays_metal_device_supports_sampler_reduction" ];
        reason = "SDK availability and hardware capability policy";
      };
    Method
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        sel = "newBufferWithLength:options:";
        args = [ Scalar Nsuint; Enum_of "MTLResourceOptions" ];
        ret = Some (Obj "Buffer");
        error = false;
        ocaml = "buffer_create";
        since = None;
        feature = C.Buffer;
        pool = true;
      };
    Property
      {
        recv = "Buffer";
        objc = "id<MTLBuffer>";
        name = "length";
        ty = Scalar Nsuint;
        access = Get;
        ocaml = "buffer_info_length";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Buffer";
        objc = "id<MTLBuffer>";
        name = "storageMode";
        ty = Enum_of "MTLStorageMode";
        access = Get;
        ocaml = "buffer_info_storageMode";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Buffer";
        objc = "id<MTLBuffer>";
        name = "cpuCacheMode";
        ty = Enum_of "MTLCPUCacheMode";
        access = Get;
        ocaml = "buffer_info_cpuCacheMode";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Buffer";
        objc = "id<MTLBuffer>";
        name = "hazardTrackingMode";
        ty = Enum_of "MTLHazardTrackingMode";
        access = Get;
        ocaml = "buffer_info_hazardTrackingMode";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Buffer";
        objc = "id<MTLBuffer>";
        name = "heapOffset";
        ty = Scalar Nsuint;
        access = Get;
        ocaml = "buffer_info_heapOffset";
        since = None;
        feature = C.Buffer;
      };
    Property
      {
        recv = "Buffer";
        objc = "id<MTLBuffer>";
        name = "label";
        ty = Str;
        access = Set;
        ocaml = "buffer_label";
        since = None;
        feature = C.Buffer;
      };
    Native
      {
        ocaml = "buffer_write";
        signature = "handle -> int64 -> bytes -> int -> int -> (unit, string) result";
        primitives = [ "caml_rays_metal_buffer_write" ];
        reason = "byte buffers/native memory and checked ranges";
      };
    Native
      {
        ocaml = "buffer_create_mapped";
        signature = "handle -> int64 -> int64 -> (handle * floats, string) result";
        primitives = [ "caml_rays_metal_buffer_create_mapped" ];
        reason =
          "ownership transfer: page-aligned memory a GC-managed float32 Bigarray owns, wrapped \
           with newBufferWithBytesNoCopy (a pointer argument the registry cannot express)";
      };
    Native
      {
        ocaml = "buffer_read";
        signature = "handle -> int64 -> int -> (bytes, string) result";
        primitives = [ "caml_rays_metal_buffer_read" ];
        reason = "byte buffers/native memory and checked ranges";
      };
    Native
      {
        ocaml = "texture_create";
        signature = "handle -> texture_descriptor -> string option -> (handle, string) result";
        primitives = [ "caml_rays_metal_texture_create" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "texture_info";
        signature = "handle -> int array";
        primitives = [ "caml_rays_metal_texture_info" ];
        reason = "array/aggregate marshalling and native validation";
      };
    Property
      {
        recv = "Texture";
        objc = "id<MTLTexture>";
        name = "shareable";
        ty = Scalar Bool;
        access = Get;
        ocaml = "texture_is_shareable";
        since = None;
        feature = C.Texture;
      };
    Native
      {
        ocaml = "texture_read";
        signature =
          "handle ->\n\
          \  ((int * int * int * int * int * int) * int * int * int * int * int) ->\n\
          \    (bytes, string) result";
        primitives = [ "caml_rays_metal_texture_read" ];
        reason = "byte buffers/native memory and checked ranges";
      };
    Native
      {
        ocaml = "texture_read_into";
        signature =
          "handle ->\n\
          \  ((int * int * int * int * int * int) * int * int * int * int * int) ->\n\
          \    bytes -> (unit, string) result";
        primitives = [ "caml_rays_metal_texture_read_into" ];
        reason = "byte buffers/native memory and checked ranges";
      };
    Native
      {
        ocaml = "sampler_create";
        signature =
          "handle ->\n\
          \  (int * int * int * int * int * int * int * int * int * bool * float * float\n\
          \    * bool * float * int * bool) -> string option -> (handle, string) result";
        primitives = [ "caml_rays_metal_sampler_create" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "depth_stencil_create";
        signature = "handle -> depth_stencil_descriptor -> (handle, string) result";
        primitives = [ "caml_rays_metal_depth_stencil_create" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "indirect_command_buffer_create";
        signature =
          "handle ->\n\
          \  indirect_command_buffer_descriptor ->\n\
          \    int64 -> int64 -> (handle, string) result";
        primitives = [ "caml_rays_metal_indirect_command_buffer_create" ];
        reason = "descriptor construction and checked object graphs";
      };
    Method
      {
        recv = "Indirect_command_buffer";
        objc = "id<MTLIndirectCommandBuffer>";
        sel = "indirectRenderCommandAtIndex:";
        args = [ Scalar Nsuint ];
        ret = Some (Obj "Indirect_render_command");
        error = false;
        ocaml = "indirect_render_command";
        since = None;
        feature = C.Render_pipeline;
        pool = true;
      };
    Method
      {
        recv = "Indirect_render_command";
        objc = "id<MTLIndirectRenderCommand>";
        sel = "setRenderPipelineState:";
        args = [ Obj "Render_pipeline" ];
        ret = None;
        error = false;
        ocaml = "indirect_render_command_set_pipeline";
        since = None;
        feature = C.Render_pipeline;
        pool = false;
      };
    Method
      {
        recv = "Indirect_render_command";
        objc = "id<MTLIndirectRenderCommand>";
        sel = "setVertexBuffer:offset:atIndex:";
        args = [ Obj "Buffer"; Scalar Nsuint; Scalar Nsuint_int ];
        ret = None;
        error = false;
        ocaml = "indirect_render_command_set_vertex_buffer";
        since = None;
        feature = C.Render_pipeline;
        pool = false;
      };
    Method
      {
        recv = "Indirect_render_command";
        objc = "id<MTLIndirectRenderCommand>";
        sel = "setFragmentBuffer:offset:atIndex:";
        args = [ Obj "Buffer"; Scalar Nsuint; Scalar Nsuint_int ];
        ret = None;
        error = false;
        ocaml = "indirect_render_command_set_fragment_buffer";
        since = None;
        feature = C.Render_pipeline;
        pool = false;
      };
    Method
      {
        recv = "Indirect_render_command";
        objc = "id<MTLIndirectRenderCommand>";
        sel = "drawPrimitives:vertexStart:vertexCount:instanceCount:baseInstance:";
        args =
          [ Enum_of "MTLPrimitiveType"; Scalar Nsuint; Scalar Nsuint; Scalar Nsuint; Scalar Nsuint ];
        ret = None;
        error = false;
        ocaml = "indirect_render_command_draw_primitives";
        since = None;
        feature = C.Render_pipeline;
        pool = false;
      };
    Native
      {
        ocaml = "library_compile";
        signature = "handle -> string -> string option -> (handle, string) result";
        primitives = [ "caml_rays_metal_library_compile" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "library_function_names";
        signature = "handle -> string array";
        primitives = [ "caml_rays_metal_library_function_names" ];
        reason = "array/aggregate marshalling and native validation";
      };
    Native
      {
        ocaml = "function_find";
        signature = "handle -> string -> (handle, string) result";
        primitives = [ "caml_rays_metal_function_find" ];
        reason = "checked function name/device identity after SDK lookup";
      };
    Property
      {
        recv = "Function";
        objc = "id<MTLFunction>";
        name = "functionType";
        ty = Enum_of "MTLFunctionType";
        access = Get;
        ocaml = "function_kind";
        since = None;
        feature = C.Compute_pipeline;
      };
    Native
      {
        ocaml = "function_specialize";
        signature =
          "handle ->\n\
          \  string ->\n\
          \    function_constant_value array -> string option -> (handle, string) result";
        primitives = [ "caml_rays_metal_function_specialize" ];
        reason = "array/aggregate marshalling and native validation";
      };
    Native
      {
        ocaml = "compiler_create";
        signature = "handle -> handle option -> string option -> (handle, string) result";
        primitives = [ "caml_rays_metal_compiler_create" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "compiler_create_render_pipeline";
        signature =
          "handle ->\n\
          \  metal4_render_descriptor ->\n\
          \    ((handle * render_pipeline_reflection), string) result";
        primitives = [ "caml_rays_metal_compiler_create_render_pipeline" ];
        reason = "descriptor construction and checked object graphs";
      };
    Method
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        sel = "newComputePipelineStateWithFunction:error:";
        args = [ Obj "Function" ];
        ret = Some (Obj "Compute_pipeline");
        error = true;
        ocaml = "compute_pipeline_create";
        since = None;
        feature = C.Compute_pipeline;
        pool = true;
      };
    Native
      {
        ocaml = "compute_pipeline_create_descriptor";
        signature =
          "handle ->\n\
          \  handle ->\n\
          \    compute_pipeline_descriptor ->\n\
          \      ((handle * pipeline_binding_info array), string) result";
        primitives = [ "caml_rays_metal_compute_pipeline_create_descriptor" ];
        reason = "array/aggregate marshalling and native validation";
      };
    Property
      {
        recv = "Compute_pipeline";
        objc = "id<MTLComputePipelineState>";
        name = "maxTotalThreadsPerThreadgroup";
        ty = Scalar Nsuint;
        access = Get;
        ocaml = "compute_pipeline_max_total_threads";
        since = None;
        feature = C.Compute_pipeline;
      };
    Native
      {
        ocaml = "accel_descriptor_primitive";
        signature = "accel_primitive_raw -> (handle, string) result";
        primitives = [ "caml_rays_metal_accel_descriptor_primitive" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "accel_descriptor_instances";
        signature = "accel_instances_raw -> (handle, string) result";
        primitives = [ "caml_rays_metal_accel_descriptor_instances" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "accel_descriptor_sizes";
        signature = "handle -> handle -> ((int64 * int64 * int64), string) result";
        primitives = [ "caml_rays_metal_accel_descriptor_sizes" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "accel_encoder_build_descriptor";
        signature = "handle -> handle -> handle -> handle -> int64 -> (unit, string) result";
        primitives = [ "caml_rays_metal_accel_encoder_build_descriptor" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "accel_encoder_refit_descriptor";
        signature = "handle -> handle -> handle -> handle -> int64 -> (unit, string) result";
        primitives = [ "caml_rays_metal_accel_encoder_refit_descriptor" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "accel_instance_layout";
        signature = "int -> int array";
        primitives = [ "caml_rays_metal_accel_instance_layout" ];
        reason = "array/aggregate marshalling and native validation";
      };
    Method
      {
        recv = "Device";
        objc = "id<MTLDevice>";
        sel = "newAccelerationStructureWithSize:";
        args = [ Scalar Nsuint ];
        ret = Some (Obj "Acceleration_structure");
        error = false;
        ocaml = "acceleration_structure_create";
        since = None;
        feature = C.Ray_tracing;
        pool = true;
      };
    Method
      {
        recv = "Acceleration_encoder";
        objc = "id<MTLAccelerationStructureCommandEncoder>";
        sel = "copyAndCompactAccelerationStructure:toAccelerationStructure:";
        args = [ Obj "Acceleration_structure"; Obj "Acceleration_structure" ];
        ret = None;
        error = false;
        ocaml = "acceleration_encoder_copy_and_compact";
        since = None;
        feature = C.Ray_tracing;
        pool = true;
      };
    Native
      {
        ocaml = "command_buffer_render_encoder_attachments";
        signature =
          "handle ->\n\
          \  handle ->\n\
          \    handle option ->\n\
          \      handle option ->\n\
          \        (float * float * float * float) -> (handle, string) result";
        primitives =
          [
            "caml_rays_metal_command_buffer_render_encoder_attachments_bytecode";
            "caml_rays_metal_command_buffer_render_encoder_attachments";
          ];
        reason = "temporary render-pass descriptor graph and attachment ownership";
      };
    Native
      {
        ocaml = "layer_adopt_borrowed";
        signature = "handle -> Native_layer_token.t -> int64 -> int64 -> (handle, string) result";
        primitives = [ "caml_rays_metal_layer_adopt_borrowed" ];
        reason = "borrowed native layer ownership/token validation";
      };
    Native
      {
        ocaml = "layer_configure";
        signature =
          "handle ->\n\
          \  int ->\n\
          \    int -> int -> (bool * int * bool * bool * bool) -> (unit, string) result";
        primitives = [ "caml_rays_metal_layer_configure" ];
        reason = "checked presentation flags/format and native CGSize marshalling";
      };
    Method
      {
        recv = "Metal_layer";
        objc = "CAMetalLayer *";
        sel = "nextDrawable";
        args = [];
        ret = Some (Opt_obj "Metal_drawable");
        error = false;
        ocaml = "layer_next_drawable";
        since = None;
        feature = C.Surface;
        pool = true;
      };
    Native
      {
        ocaml = "drawable_texture";
        signature = "handle -> ((handle * int * int * int), string) result";
        primitives = [ "caml_rays_metal_drawable_texture" ];
        reason = "owned texture plus native dimension/format snapshot";
      };
    Method
      {
        recv = "Command_buffer";
        objc = "id<MTLCommandBuffer>";
        sel = "presentDrawable:";
        args = [ Obj "Metal_drawable" ];
        ret = None;
        error = false;
        ocaml = "command_buffer_present";
        since = None;
        feature = C.Surface;
        pool = true;
      };
    Method
      {
        recv = "Command_buffer";
        objc = "id<MTLCommandBuffer>";
        sel = "presentDrawable:atTime:";
        args = [ Obj "Metal_drawable"; Scalar Double ];
        ret = None;
        error = false;
        ocaml = "command_buffer_present_at_time";
        since = None;
        feature = C.Surface;
        pool = true;
      };
    Method
      {
        recv = "Command_buffer";
        objc = "id<MTLCommandBuffer>";
        sel = "presentDrawable:afterMinimumDuration:";
        args = [ Obj "Metal_drawable"; Scalar Double ];
        ret = None;
        error = false;
        ocaml = "command_buffer_present_after_duration";
        since = None;
        feature = C.Surface;
        pool = true;
      };
    Native
      {
        ocaml = "presentation_command_snapshot";
        signature = "handle -> (presentation_command_snapshot, string) result";
        primitives = [ "caml_rays_metal_presentation_command_snapshot" ];
        reason = "completion state, diagnostic string and native timing snapshot";
      };
    Native
      {
        ocaml = "command_buffer_cancel_handler";
        signature = "nativeint -> unit";
        primitives = [ "caml_rays_metal_command_buffer_cancel_handler" ];
        reason = "callback root ownership and cancellation";
      };
    Class_method
      {
        objc = "MTLRenderPassDescriptor";
        sel = "renderPassDescriptor";
        args = [];
        ret = Some (Obj "Render_pass_descriptor");
        ocaml = "render_pass_descriptor_create";
        since = None;
        feature = C.Render_pipeline;
      };
    Native
      {
        ocaml = "render_pass_descriptor_set_sizes";
        signature = "handle -> int -> int -> int -> int -> (unit, string) result";
        primitives = [ "caml_rays_metal_render_pass_descriptor_set_sizes" ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "render_pass_resolve_texture";
        signature = "handle -> handle option -> bool -> (handle option, string) result";
        primitives = [ "caml_rays_metal_render_pass_resolve_texture" ];
        reason = "nullable resolve-texture access through a descriptor child";
      };
    Native
      {
        ocaml = "render_pass_color_store_action";
        signature = "handle -> int -> (unit, string) result";
        primitives = [ "caml_rays_metal_render_pass_color_store_action" ];
        reason = "store action on descriptor colorAttachments[0]; future property-path lowering";
      };
    Native
      {
        ocaml = "render_pass_color_load_action";
        signature = "handle -> int -> (unit, string) result";
        primitives = [ "caml_rays_metal_render_pass_color_load_action" ];
        reason = "load action on descriptor colorAttachments[0]; future property-path lowering";
      };
    Native
      {
        ocaml = "render_pass_descriptor_set_attachments";
        signature =
          "handle ->\n\
          \  handle ->\n\
          \    handle option ->\n\
          \      handle option ->\n\
          \        handle option ->\n\
          \          (float * float * float * float) -> (unit, string) result";
        primitives =
          [
            "caml_rays_metal_render_pass_descriptor_set_attachments_bytecode";
            "caml_rays_metal_render_pass_descriptor_set_attachments";
          ];
        reason = "descriptor construction and checked object graphs";
      };
    Native
      {
        ocaml = "render_encoder_use_resources";
        signature = "handle -> handle array -> int -> int -> (unit, string) result";
        primitives = [ "caml_rays_metal_render_encoder_use_resources" ];
        reason = "array/aggregate marshalling and native validation";
      };
    Native
      {
        ocaml = "render_encoder_execute_icb_range";
        signature = "handle -> handle -> int -> int -> (unit, string) result";
        primitives = [ "caml_rays_metal_render_encoder_execute_icb_range" ];
        reason = "NSRange assembled from scalar bounds; future record adapter";
      };
    Native
      {
        ocaml = "render_encoder_execute_indexed_draws";
        signature =
          "handle ->\n\
          \  handle array ->\n\
          \    handle array array ->\n\
          \      int array array ->\n\
          \        int64 array array ->\n\
          \          int array array ->\n\
          \            int array ->\n\
          \              int64 array ->\n\
          \                int array ->\n\
          \                  handle array -> int64 array -> (unit, string) result";
        primitives =
          [
            "caml_rays_metal_render_encoder_execute_indexed_draws_bytecode";
            "caml_rays_metal_render_encoder_execute_indexed_draws";
          ];
        reason = "array/aggregate marshalling and native validation";
      };
    Native
      {
        ocaml = "render_pass_depth_stencil_actions";
        signature = "handle -> int -> int -> float -> int -> int -> int -> (unit, string) result";
        primitives =
          [
            "caml_rays_metal_render_pass_depth_stencil_actions_bytecode";
            "caml_rays_metal_render_pass_depth_stencil_actions";
          ];
        reason = "conditional depth/stencil child updates and native clear-value conversion";
      };
    Method
      {
        recv = "Compute_encoder";
        objc = "id<MTLComputeCommandEncoder>";
        sel = "setComputePipelineState:";
        args = [ Obj "Compute_pipeline" ];
        ret = None;
        error = false;
        ocaml = "compute_encoder_set_pipeline";
        since = None;
        feature = C.Compute_pipeline;
        pool = false;
      };
    Method
      {
        recv = "Compute_encoder";
        objc = "id<MTLComputeCommandEncoder>";
        sel = "setBuffer:offset:atIndex:";
        args = [ Obj "Buffer"; Scalar Nsuint; Scalar Nsuint_int ];
        ret = None;
        error = false;
        ocaml = "compute_encoder_set_buffer";
        since = None;
        feature = C.Compute_pipeline;
        pool = false;
      };
    Method
      {
        recv = "Compute_encoder";
        objc = "id<MTLComputeCommandEncoder>";
        sel = "setTexture:atIndex:";
        args = [ Obj "Texture"; Scalar Nsuint_int ];
        ret = None;
        error = false;
        ocaml = "compute_encoder_set_texture";
        since = None;
        feature = C.Compute_pipeline;
        pool = false;
      };
    Method
      {
        recv = "Compute_encoder";
        objc = "id<MTLComputeCommandEncoder>";
        sel = "dispatchThreads:threadsPerThreadgroup:";
        args = [ Tuple "Mtl_size_int"; Tuple "Mtl_size_int" ];
        ret = None;
        error = false;
        ocaml = "compute_encoder_dispatch";
        since = None;
        feature = C.Compute_pipeline;
        pool = false;
      };
    Method
      {
        recv = "Compute_encoder";
        objc = "id<MTLComputeCommandEncoder>";
        sel = "setAccelerationStructure:atBufferIndex:";
        args = [ Opt_obj "Acceleration_structure"; Scalar Nsuint ];
        ret = None;
        error = false;
        ocaml = "compute35_acceleration";
        since = None;
        feature = C.Ray_tracing;
        pool = false;
      };
    Native
      {
        ocaml = "compute35_bytes_plain";
        signature = "handle -> bytes -> int64 -> (unit, string) result";
        primitives = [ "caml_rays_metal_compute35_bytes_plain" ];
        reason = "byte buffers/native memory and checked ranges";
      };
    Native
      {
        ocaml = "compute35_accelerations";
        signature = "handle -> handle array -> (unit, string) result";
        primitives = [ "caml_rays_metal_compute35_accelerations" ];
        reason = "array/aggregate marshalling and native validation";
      };
    Native
      {
        ocaml = "blit_encoder_copy_buffer_to_texture";
        signature =
          "handle ->\n\
          \  handle ->\n\
          \    handle ->\n\
          \      (int64 * int * int * (int * int * int) * int * int * (int * int * int))\n\
          \        -> (unit, string) result";
        primitives = [ "caml_rays_metal_blit_encoder_copy_buffer_to_texture" ];
        reason = "checked packed copy specification and native MTLSize/MTLOrigin marshalling";
      };
    Native
      {
        ocaml = "command_buffer_wait";
        signature = "handle -> unit";
        primitives = [ "caml_rays_metal_command_buffer_wait" ];
        reason = "blocking wait releases the OCaml runtime lock";
      };
    Native
      {
        ocaml = "command_buffer_error";
        signature = "handle -> string option";
        primitives = [ "caml_rays_metal_command_buffer_error" ];
        reason = "nullable NSError-to-diagnostic-string conversion";
      };
    Method
      {
        recv = "Function";
        objc = "id<MTLFunction>";
        sel = "newArgumentEncoderWithBufferIndex:";
        args = [ Scalar Nsuint ];
        ret = Some (Obj "Shader_argument_encoder");
        error = false;
        ocaml = "shader_function_argument_encoder";
        since = None;
        feature = C.Compute_pipeline;
        pool = true;
      };
    Native
      {
        ocaml = "argument_encoder_snapshot";
        signature = "handle -> ((string option * int64 * int64 * int64), string) result";
        primitives = [ "caml_rays_metal_argument_encoder_snapshot" ];
        reason = "nullable label plus length/alignment/device identity snapshot";
      };
    Native
      {
        ocaml = "argument_encoder_set_buffer";
        signature = "handle -> handle -> int64 -> int64 -> int64 -> (unit, string) result";
        primitives = [ "caml_rays_metal_argument_encoder_set_buffer" ];
        reason = "buffer-range/device checks and selector dispatch";
      };
    Native
      {
        ocaml = "argument_encoder_single";
        signature = "handle -> int -> handle -> int64 -> int64 -> (unit, string) result";
        primitives = [ "caml_rays_metal_argument_encoder_single" ];
        reason = "heterogeneous handle-kind selector dispatch and device/index checks";
      };
    Native
      {
        ocaml = "acceleration_encoder_write_type";
        signature = "handle -> handle -> handle -> int64 -> int -> (unit, string) result";
        primitives = [ "caml_rays_metal_acceleration_encoder_write_type" ];
        reason = "compacted-size type selector dispatch and destination range validation";
      };
    Native
      {
        ocaml = "indirect_render_draw_indexed";
        signature =
          "handle ->\n\
          \  int ->\n\
          \    int64 ->\n\
          \      int ->\n\
          \        handle ->\n\
          \          int64 -> int64 -> int64 -> int64 -> int64 -> (unit, string) result";
        primitives =
          [
            "caml_rays_metal_indirect_render_draw_indexed_bytecode";
            "caml_rays_metal_indirect_render_draw_indexed";
          ];
        reason = "index layout, device identity and byte-range/overflow validation";
      };
    Property
      {
        recv = "Metal_drawable";
        objc = "id<MTLDrawable>";
        name = "drawableID";
        ty = Scalar Nsuint;
        access = Get;
        ocaml = "drawable10_snapshot_drawableID";
        since = None;
        feature = C.Surface;
      };
    Property
      {
        recv = "Metal_drawable";
        objc = "id<MTLDrawable>";
        name = "presentedTime";
        ty = Scalar Double;
        access = Get;
        ocaml = "drawable10_snapshot_presentedTime";
        since = None;
        feature = C.Surface;
      };
    Record
      {
        sdk = "MTLSize";
        ocaml = "Mtl_size_int";
        fields = [ ("width", Nsuint_int); ("height", Nsuint_int); ("depth", Nsuint_int) ];
        feature = C.Compute_pipeline;
      };
  ]
