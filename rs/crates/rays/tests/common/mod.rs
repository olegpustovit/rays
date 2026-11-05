//! Use the renderer's native backend and lease policy; retain this runner's editor-suite sharding.
#![allow(dead_code)]
#[path = "../../../renderer/tests/common/mod.rs"]
mod native;
pub use native::{floats, gpu, release_keepalive, words};
pub mod window;
