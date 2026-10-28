(** WebGPU kernel legality as diagnostics with spans.

    Limits are the WebGPU specification's defaults, which every adapter guarantees
    (https://www.w3.org/TR/webgpu/ section 3.6.2 "Limits", table of supported limits):
    [maxStorageBuffersPerShaderStage] is 8 and [maxComputeWorkgroupsPerDimension] is 65,535. *)

val max_elements : int
(** 64 * 65,535 = 4,194,240: dispatch is one-dimensional with [@workgroup_size(64)]. *)

val region : Flow_ir.Packed.t -> count:int option -> Diagnostic.t list
(** Why the region cannot run as a web kernel, each with the region's span: [E_GPU_FORM] (the printer's
    own refusal), [E_WEB_BUFFERS] (more than {!max_storage_buffers} storage buffers in one kernel),
    [E_WEB_ELEMENTS] (more than {!max_elements} elements in one dispatch; checked when [count] is known).
    Empty when the region is legal. *)
