val scene2_textured_argument : string

val ui : string
(** Instanced PXUI pipeline for [Scene_command.Ui_batch] instances. *)

val marks : string
(** Instanced Marks pipeline: six per-attribute vertex buffers indexed by [vid / 4] and the Ui
    affine. *)
