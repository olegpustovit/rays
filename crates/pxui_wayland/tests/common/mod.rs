//! The shared libtest-protocol runner for `harness = false` window targets.
#![allow(dead_code)]
pub fn release_keepalive() {}
#[path = "../../../renderer/tests/common/window.rs"]
pub mod window;
pub use window::run;
