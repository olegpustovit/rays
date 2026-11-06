//! Port of lib/pxui_shell: editor chrome over PXUI.
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

pub mod chrome;
pub mod inspector;
pub mod kit;
pub mod layout;
pub mod prompt;
pub mod status_bar;
pub mod timeline_bar;
pub mod tree;
pub mod which_key;

#[cfg(test)]
mod test_layout;
#[cfg(test)]
mod test_logic;
