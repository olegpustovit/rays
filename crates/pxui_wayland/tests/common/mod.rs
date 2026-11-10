//! The shared libtest-protocol runner for `harness = false` window targets.
#![allow(dead_code)]
pub fn release_keepalive() {}
#[path = "../../../../tools/t/window_harness.rs"]
pub mod window;
pub use window::run;
