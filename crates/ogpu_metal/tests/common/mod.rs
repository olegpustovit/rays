//! libtest never gives a test the process main thread, which SDL requires.
#![allow(dead_code)]
pub fn release_keepalive() {}
#[path = "../../../../tools/t/window_harness.rs"]
pub mod window;
