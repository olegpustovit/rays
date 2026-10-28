(** The scene, world and settings contexts of a workspace.

    Their Lisp spellings are generated from the schemas that exist:
    [scene/geometry], [scene/light] and [scene/camera] from the object
    schemas, [world/gradient], [world/sky], [world/sun],
    [world/shape], [world/scatter], [world/room] from the World schemas and
    [settings/config] from the workspace settings record.  A keyword is a
    schema field; three consecutive [_x _y _z] or [_r _g _b] floats are one
    vec3 or colour ([:translate [0 1 0]], [:color "#3b7d4e"]), and [:name] is
    the node's label.  A geometry object takes its geometry as the first
    argument, a World layer the layer below it.  [scene/merge] joins scenes.
    Scene, world and settings arguments are evaluated at [t = 0]. *)

type window = Kinds.window = { title : string; width : int; height : int; fps : int; seed : int; fast : bool }
(** What [(settings/config :title :width :height :fps :seed)] asks of the host
    when it starts. These fields apply on restart; editing saved settings does
    not reconfigure the running window, frame scheduler or cook seed. *)

val window : Workspace_doc.t -> (window, Diagnostic.t) result
(** The window of a checked workspace: its settings graph evaluated, defaults
    without one. *)

val plan_window : Workspace_doc.t -> Flow.Eval.plan -> (window, Diagnostic.t) result
(** {!window} read from a plan the caller already evaluated: no second evaluation of the workspace. *)

val fast : Document.t -> bool
(** [(settings/config :precision "fast")], the default: what only reaches the screen may run on the
    GPU in float32 with 64-bit integer registers, and on the web its regions' integers are numbers.
    [:precision "exact"] keeps every value bit-identical on the CPU. *)

val document_window : Document.t -> (window, Diagnostic.t) result
(** {!window} of the workspace a document was made from, read from the document's own
    lowering ({!of_workspace}) with no second static evaluation. *)

val has_settings : Workspace_doc.t -> bool

val of_workspace : ?factories:Sop.Edit_graph.factory list -> ?gpu:Flow_ir.Gpu.config -> ?previous:Document.t ->
  Workspace_doc.t -> (Document.t, Diagnostic.t) result
(** The document of a workspace: the objects of its scene graph as nodes of the
    scene network (each geometry object owning the lowered network of its sop
    graph; without a scene graph, one geometry object per sop graph), the World
    of its world graph with its layers as a network, and its settings graph as
    the document settings.  [previous] keeps object ids (matched by where the text
    is, then by operation and label), tile layouts, the objects the workspace does not declare (the host's
    camera, lights and World), and the lowering's compiled ids.  The workspace owns
    geometry always, every object when it has a scene graph (an empty one means
    none: the host seeds nothing) and the World when it has a world graph
    ([world/none] means none). Light intensity/color retain their residuals for
    timeline composition. Other time-dependent fields in scene, World, settings
    and editor structs are refused with [E_CONTEXT_TIME], naming the graph and
    field. Geometry references with live SOP parameters are supported. *)

val resolve_scene : ?previous:Kind_graph.t -> ?state:Flow.Eval.state -> ?live:Input.t -> Document.t -> time:float ->
  Kind_graph.t * Diagnostic.t list
(** Resolve recorded live light intensity/color fields for composition, without
    changing source, networks, identities or history. A failed light retains
    its previous value (or the authored zero-time value without [previous]);
    successful siblings still advance. Diagnostics name its fields and view. *)

val instance_root : Flow.Eval.value -> Document.view_root option
(** How a viewport's scene instance renders: the settings of its [scene/root] (with the
    defaults for what the root leaves out) and the view of the camera object its [:camera]
    slot names; [None] for a part, which renders as the document does. The document stores
    one per viewport over another scene instance as [Document.view_roots]. *)

val graph_of : Workspace_doc.t -> Flow.Workspace.context -> Flow.Workspace.graph option
(** The first graph of a context. *)

val sha256 : string -> string
(** Lower-case hex SHA-256: the digest of a sketch source. *)
