//! Rays Editor: the native workspace editor (port of lib/rays_editor). One module per OCaml file.
pub mod bars;
pub mod carry;
pub mod cook;
pub mod ec_frame;
pub mod echo;
pub mod editor_actions;
pub mod editor_add;
pub mod editor_carry;
pub mod editor_commands;
pub mod editor_frame;
pub mod editor_host;
pub mod editor_inspect;
pub mod editor_inspector_pane;
pub mod editor_list;
pub mod editor_loop;
pub mod editor_menu;
pub mod editor_model;
pub mod editor_panels;
pub mod editor_panes;
pub mod editor_reduce;
pub mod editor_scope;
pub mod editor_setup;
pub mod editor_shell;
pub mod editor_status;
pub mod editor_text;
pub mod editor_viewport_pane;
pub mod lisp_text;
pub mod loop_model;
pub mod loop_source;
pub mod loop_view;
pub mod navigator;
pub mod panel_map;
pub mod pick;
pub mod preset_viewport;
pub mod rays_editor;
pub mod editor_renderer;
pub mod schedule;
pub mod selection;
pub mod source_file;
pub mod spreadsheet;
pub mod text_pane;
pub mod timeline;
pub mod viewport;
pub mod world_map;

// tests: B
#[cfg(test)]
mod test_navigator;
#[cfg(test)]
mod test_text_pane;

// tests: A
#[cfg(test)]
mod test_timeline;
#[cfg(test)]
mod test_schedule;
#[cfg(test)]
mod test_selection;
#[cfg(test)]
mod test_bars;
#[cfg(test)]
mod test_editor_commands;
#[cfg(test)]
mod test_preset_viewport;
#[cfg(test)]
mod test_source_file;

// history: D (editor_core History specialised to the editor Document)
pub mod editor_history;

// tests: C
#[cfg(test)]
mod test_editor_inspect;

// tests: D
#[cfg(test)]
mod test_editor_history;
#[cfg(test)]
mod test_editor_model;

// tests: E
#[cfg(test)]
mod test_editor_reduce;

// api: F (the public surface of rays_editor.rs, as `Rays_editor.Editor` etc.)
pub use rays_editor::{window, workspace_catalog, workspace_window, Editor, Measure, Renderer, Settings, Source, Timeline, Workspace, Workspace_doc};
#[cfg(feature = "test-hooks")]
pub use rays_editor::Reduce;

// tests: F
#[cfg(test)]
mod test_loop_pure;

