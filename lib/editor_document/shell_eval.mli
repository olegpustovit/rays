(** The panel tree a workspace's [ui/workspace] editor graph evaluates to: its panels, where each
    was written, the viewports and canvases it names, the start state and the layout switch. *)

type editor = { tree : Editor_core.Panels.t; origins : (Editor_core.Panels.path * Document.origin) list;
                named : (Editor_core.Panels.path * string) list; start : Document.start;
                repeated : string list; follows : (Editor_core.Panels.path * Editor_core.Panels.path) list;
                viewports : (string * Flow.Eval.value) list;
                canvases : (string * Flow.Eval.value) list;
                preview_sources : (string * Document.preview_source) list;
                switch : Document.switch option }

val editor : Workspace_doc.t -> Flow.Eval.plan -> (editor option, Diagnostic.t) result
(** The editor graph's tree, or none when the workspace has no editor graph. *)
