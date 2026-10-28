(** The scene, world and settings kinds of a workspace: one Lisp kind per object factory
    ([scene/geometry], [scene/light], ...), per World layer ([world/gradient], ...) and the
    workspace settings record ([settings/config]).  Their spellings are generated from the
    schemas: a new field in a schema is a new keyword. *)

val scene_kinds : (string * Sop.Edit_graph.factory) list
(** The qualified scene kinds with their factories ([scene/world] and [scene/root] included). *)

val world_kinds : (string * Sop.Edit_graph.factory) list
(** The qualified World kinds ([world/sky], ...) with their factories. *)

type window = { title : string; width : int; height : int; fps : int; seed : int; fast : bool }
(** What [(settings/config :title :width :height :fps :seed)] asks of the host when it starts.
    These fields apply on restart; editing saved settings does not reconfigure the running window,
    frame scheduler or cook seed. *)

val window_schema : window Param.schema

val descriptors : Flow_sop.Catalog.descriptor list
(** Every generated kind; the manifest generator writes them. *)

val catalog : version:int -> Sop.Edit_graph.factory list -> (Flow.Check.catalog, Diagnostic.t) result
(** {!Flow_sop.Catalog.of_factories} with {!descriptors}: what a workspace text is checked against.
    Not cached: a {!Document.t} keeps its own. *)

val ports : string -> Flow_sop.Port.parameter list
(** The parameters of a qualified kind; none for an unknown one. *)
