//! Port of lib/editor_document: the private editor document boundary (scene/network/settings
//! model, edits, lowering, write-back). One module per OCaml file.
#![allow(clippy::all)]
pub mod settings;
pub mod kind_graph;
pub mod objects;
pub mod layers;
pub mod kinds;
pub mod panel_kinds;
pub mod layout_by_path;
pub mod layout_edit;
pub mod literal_edit;
pub mod workspace_doc;
pub mod document;
pub mod shell_eval;
pub mod lowering;
pub mod scene_sync;
pub mod document_edit;
pub mod preset;

#[cfg(test)]
mod test_workspace_doc;
#[cfg(test)]
mod test_lowering;
#[cfg(test)]
mod test_scene_sync;
