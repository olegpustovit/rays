#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals, clippy::all)]
//! PXUI owns immediate-mode engine, kit and text widgets over Rays; ui re-exports the shared palette, typography and widget API.

pub mod theme;
pub mod ui_core;
pub mod ui_kit;
pub mod ui_text;
pub mod ui;

pub use theme::Theme;
