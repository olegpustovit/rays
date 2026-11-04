#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals, clippy::all)]
//! PXUI: the one immediate-mode UI engine (`ui::Ui`), over `rays` and `rays_math`.
//! `theme` is the design kit's palette and typography; `ui_core`, `ui_kit` and `ui_text` are the
//! engine, the kit widgets and the text widgets; `ui` re-exports all three as OCaml's `Ui` does.

pub mod theme;
pub mod ui_core;
pub mod ui_kit;
pub mod ui_text;
pub mod ui;

pub use theme::Theme;
