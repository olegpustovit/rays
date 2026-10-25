(** The adapter from the mark table ({!Scene_command.Marks}, [specification/marks.md]) to
    [Rays.Scene]. It builds the same [Shape_batch] records, geometry and transforms
    [Drawing] builds today, so the renderer sees the same commands. *)

(** One attribute of a [Marks]-family draw: [Bytes] float32 elements (or [0xRRGGBBAA] words for
    a colour) of [stride] elements per instance, 0 for one constant, or a GPU region's output. *)
type source = Bytes of bytes | Device of Flow_ir.Gpu.value
type attribute = { source : source; stride : int }

(** Registers one mark set drawn by the [Marks] family: the [slot] numbers the frame's GPU sets
    from 0, [kind] is the [Shape_batch] kind word (rect 0, circle 4, line 5) and the attributes
    are position, extent, fill, stroke and width. *)
type gpu = slot:int -> kind:int -> count:int -> attribute array ->
  (Scene_command.Shape_batch.gpu_token, Flow.Diagnostic.t) result

type cache
val cache : unit -> cache
(** The nodes built for the mark sets of the last frame, by position in the table: at most 1024
    nodes and 4096 converted attribute arrays are kept, the least recently used going first. *)

val cache_size : cache -> int
(** The nodes held. *)

val scene : buffers:Flow_ir.Core_exec.Buffers.t -> ?image:(int -> Rays.Image.t) -> ?gpu:gpu -> ?cache:cache ->
  ?device:(int -> Flow_ir.Gpu.value) -> Scene_command.Marks.t ->
  (Rays.Scene.t, Flow.Diagnostic.t) result
(** [buffers] is the table the marks' buffer ids refer to; [image] maps an [Image] entry's
    resource id to the host image ([E_IMAGE] when absent). A buffer value or constant a
    builder rejects is [E_DRAW_RANGE]; a nonfinite colour channel [E_NONFINITE].

    A mark set with a [Device] attribute (its buffer id resolved by [device]) goes to [gpu] and
    becomes one [Marks]-family draw; its CPU attributes are uploaded in the same layout (a
    colour buffer as packed words; a [Device] colour is the region's float r, g, b, bound as it
    is). Every other mark set stays a [Shape_batch], so a CPU-only frame is unchanged. Without
    [gpu] a [Device] attribute is [E_GPU].

    With [cache], a rect, circle, line or point set whose entry and attribute arrays (physically)
    equal the previous frame's at the same position reuses its node: a static batch is not built
    again. In a device set the bytes of each host array are kept the same way (at most five per
    set), so a static attribute beside a GPU one is converted once and not uploaded again. *)
