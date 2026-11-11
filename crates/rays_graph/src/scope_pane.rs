// the facade `Scope` over the state, the commands and the frame.
pub use crate::scope_commands::{bindings, run_command};
pub use crate::scope_frame::{num_field, selected_wire, update, update_cached, inspect};
pub use crate::scope_state::*;
