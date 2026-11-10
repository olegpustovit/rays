//! Pure editor state shared by sketch hosts and presentation adapters, re-exported at the crate root.
#![allow(non_camel_case_types)]

pub mod editor_core;
pub use input::frame;
pub mod panels;
pub mod store;

pub use editor_core::{command, guide_context, keymap, router};

#[cfg(test)]
mod test_history;
#[cfg(test)]
mod test_panels;
#[cfg(test)]
mod test_store;
