(** The web export: a checked workspace's draw graph, lowered to a core program and printed as the
    files of a standalone browser application. *)

type lowered = {
  program : Flow_ir.Core.program;
  span : Diagnostic.span;  (** the draw graph *)
  root : Flow.Eval.residual option;  (** the residual whose state the program reads *)
  plan : Flow.Eval.plan;  (** image producers referenced by the draw program *)
}

val lower : Editor_document.Workspace_doc.t -> (lowered, Diagnostic.t) result
(** Run the static stage with graph inputs at their defaults and lower the first draw graph. A draw
    graph that reaches a [sop] or [scene] graph is refused with the reference's span
    ([E_WEB_GRAPH]). *)

type files = (string * string) list
(** Relative path and contents, in a fixed order. *)

val files : Editor_document.Workspace_doc.t -> lowered -> (files, Diagnostic.t) result
(** The export of a lowered draw graph:
    - [program.js]: {!Flow_web.Js.frame} (packed-array constants are views into [rt.data]) followed
      by [export const regions], one object per core region: [source] ({!Flow_web.Js.region}),
      [count], [kept], [inputs] (pairs [offset, length] of doubles in [data.bin]), [uniforms],
      [frame] (pairs of instruction slot and frame-fact name: [t], [frame/dt], [frame/index],
      [frame/width], [frame/height]), [length] (instructions), [width] (components per element) and
      [kernel] (its id in [kernels.json], or [null]).
    - [program.map]: {!Flow_web.Js.frame_map}.
    - [kernels.json] and [kernels.wgsl]: the WGSL kernels of the regions legal on the web target (a
      static count of at least 1,024 and {!Flow_web.Target.region} empty), in the table [gpu.js]
      loads. A kernel that the frame can use without reading its value is dispatched by the runtime
      and its region is not invoked on the CPU: a region whose every [Invoke] result is defined once
      and used only as the positions, radii, sizes, colours, [from] or [to] of rect, circle and line
      sets, whose sets' region attributes all qualify, has [kernel] and [gpu: {output}] in the
      region table, [rt.gpuArray(k)] in the frame, and an entry of
      [export const gpu = {id: {count, inputs, uniforms, frame}}], the kernel's inputs and captures.
      Attributes of one set with equal count and rate are merged by {!Flow_ir.Core_fuse.merge} into
      one kernel with an output each (id of the first member). Any other region is invoked on the
      CPU, since a frame that reads a value would need an in-frame readback, which is illegal on the
      web.
    - [program.js] exports [images], keyed by plan id, with [kind] and [animated]. [image/map] adds
      [kernel], [args] and [frame]: its packed pixel body emits WGSL vec4 output with GPU-derived
      pixel-center UVs and a resident straight-alpha RGBA8 sink (clamp, ties to even). [image/noise]
      adds [args] and preserves native double noise, defaults, frequency alias and seeds before
      uploading RGBA8. [image/render] adds [args] and [drawing], both core-compiled modules: the
      drawing resolves nested images and uses the same GPU/CPU region placement and presenter with
      an owned RGBA8 offscreen target at DPR 1. Backgrounds use the target size; frame facts retain
      the main frame size. All modules share folds by native source identity. Modules are
      content-addressed so watch reload cannot reuse stale captures or constant views. [image/load]
      adds [offset], [length], [width] and [height]: its static file asset is bundled using the
      native SDL3_image decoder, preserving formats, EXIF orientation and straight alpha without
      relying on browser codecs. Static images run once per load, shared/animated images at most
      once per frame. Capacity 64; textures are reused, resized or released on reload/stop, with
      device bounds checked before allocation.
    - [data.bin]: little-endian doubles followed by decoded RGBA8 assets; every view starts at a
      multiple of 8.
    - [index.html] and [runtime/*.js]. Refused: a file path not known at export time,
      missing/invalid file images, an unsupported image pixel body or more than 64 image producers
      ([E_WEB_IMAGE] or the pixel compiler's actual diagnostic), a region that is not a collecting
      single-output one ([E_WEB_REGION]), or one whose inputs change from frame to frame
      ([E_WEB_CAPTURE]), each with a span. *)

val load : string -> string * (Editor_document.Workspace_doc.t, Diagnostic.t list) result
(** The text of a [.rays] file and its checked document (imports read beside it). *)
