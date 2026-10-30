(** The kernel table lib/web_export/runtime/gpu.js loads (format: its header comment), as JSON text.
*)

val json_string : string -> string
(** A JSON (and JavaScript) string literal. Valid UTF-8 passes through; invalid bytes use reversible
    isolated low-surrogate escapes. *)

val entry : ?cpu:string -> id:string -> Wgsl.wgsl -> string
(** One kernel object: the printer's source, entry, interface, uniform layout, noise seeds with
    their 512-entry permutations ([Rays_math.Noise.permutation], as the Metal host uploads), and
    widths. [id] is the name the program dispatches by and must be unique in a table. [cpu], when
    given, is the region's {!Js.region} source (golden fixtures carry one for the browser check). *)

val table : compaction:string -> dispatch:string -> string list -> string
(** [{"kernels":[...], "compaction": ..., "dispatch": ...}] over {!entry} objects, one per line, and
    the resident fold's compaction and dispatch-arguments WGSL (printed from Flow_gpu's
    shaders/compact.rays and dispatch.rays at build time, lib/flow_shader/print_shader.exe web);
    newline-terminated. *)
