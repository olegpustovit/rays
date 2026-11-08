//! Port of lib/editor_core: pure editor state shared by the sketch host and presentation adapters.
//! `editor_core` holds Keymap, Guide_context, Command and Router, re-exported at the crate
//! root as the OCaml module paths `Editor_core.History` and so on.
#![allow(non_camel_case_types)]

pub mod editor_core;
pub mod frame;
pub mod panels;
pub mod store;

pub use editor_core::{command, guide_context, keymap, router};

#[cfg(test)]
mod test_history;
#[cfg(test)]
mod test_panels;
#[cfg(test)]
mod test_store;
