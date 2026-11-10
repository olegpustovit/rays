#![allow(clippy::all)]
//! PXUI owns immediate-mode engine, kit and text widgets over Rays; ui re-exports the shared palette, typography and widget API.

pub mod draw;
pub mod geom;
pub mod theme;
pub mod ui_core;
pub mod ui_kit;
pub mod ui_text;
pub mod ui;

pub use theme::Theme;
