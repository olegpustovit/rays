//! libtest never gives a test the process main thread, which SDL requires.
#![allow(dead_code)]
pub fn release_keepalive() {}
#[path = "../../../renderer/tests/common/window.rs"]
pub mod window;
pub use window::run;
