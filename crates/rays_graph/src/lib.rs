#![allow(clippy::all)]
//! the SOP-network presentation. `scope_pane` is the graph pane (`Scope`),
//! `node_menu` the add menu. The pane emits typed `Change` requests and never edits.
pub mod scope_wires;
pub mod scope_state;
pub mod node_menu;
pub mod scope_paint;
pub mod scope_commands;
pub mod scope_frame;
pub mod scope_pane;
