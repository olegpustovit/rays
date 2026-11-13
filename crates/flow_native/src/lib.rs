//! native Flow drawing, marks, workspace execution and live-frame glue.
//! Uses the UI-free document model and renderer, never PXUI or the editor.
#![allow(clippy::too_many_arguments, clippy::type_complexity, clippy::new_without_default)]
pub mod live_frame;
pub mod probe_run;
pub mod marks_scene;
pub mod drawing;
pub mod workspace_resources;
pub mod workspace_gpu;
pub mod workspace_images;
pub mod workspace_host;
#[cfg(test)]
mod test_workspace_gpu_service;
#[cfg(test)]
mod test_drawing;
#[cfg(test)]
mod test_probe_run;
#[cfg(test)]
mod test_workspace_images;

#[cfg(test)]
mod test_source_atlas;
