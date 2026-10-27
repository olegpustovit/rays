(** Sketch presets: the editable document as one s-expression file.

    A preset is a workspace ({!Workspace_doc.to_text}: the [(workspace ...)]
    form, then optional [(layout ...)] and [(settings ...)] forms) followed by
    a [(view {...})] form holding the environment's camera and render
    settings.  Comments in the source survive save and load.  There is no
    other format and no older version: a file that is not this fails with a
    message.  Every document is a workspace, so every document saves. *)

type loaded = {
  doc : Document.t;
  view : Flow.Syntax.t;  (** environment camera/render settings *)
}

val sanitize : string -> string
(** Keep [A-Za-z0-9_-]; every other character becomes [_]. *)

val default_name : unit -> string
(** Local time as [YYYY-MM-DD_HH-MM-SS]. *)

val path : directory:string -> name:string -> string
(** [<directory>/<name>.rays]. *)

val save :
  directory:string -> name:string -> doc:Document.t -> view:Flow.Syntax.t ->
  (string, string) result
(** Write atomically ({!Editor_core.Store.write_text}); returns the path.
    [Error] when the view has nonfinite numbers or the file cannot be written.
    The document's current settings are saved. *)

val text : Document.t -> string
(** The canonical workspace text of [doc] (its settings beside it), as a preset
    or a [.rays] source is written. *)

val list : directory:string -> (string * float) list
(** Preset names with modification times, newest first; empty when the
    directory is missing. *)

val delete : directory:string -> name:string -> (unit, string) result

val load :
  ?gpu:Flow_ir.Gpu.config -> path:string -> factories:Sop.Edit_graph.factory list ->
  settings:Settings.t -> unit -> (loaded, string) result
(** Parse, check and lower the file; [settings] is the sketch's settings
    value the saved fields apply to.  An unreadable, unchecked or unlowerable
    file is an [Error] carrying the diagnostic text; nothing else is touched. *)

val load_with_ops :
  ?gpu:Flow_ir.Gpu.config -> ops:Flow.Op.t list -> path:string -> factories:Sop.Edit_graph.factory list ->
  settings:Settings.t -> unit -> (loaded, string) result
(** {!load} with the host's immutable operator extensions. *)
