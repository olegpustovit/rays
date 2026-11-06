// Port of lib/pxui_graph/scope_pane.ml: the facade `Scope` over the state, the commands and the frame.
pub use crate::scope_commands::{bindings, run_command};
pub use crate::scope_frame::{num_field, selected_wire, update, Inspect};
pub use crate::scope_state::*;
