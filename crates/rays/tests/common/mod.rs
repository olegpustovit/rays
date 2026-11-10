//! Use the renderer's native backend and lease policy; retain this runner's editor-suite sharding.
#![allow(dead_code)]
#[path = "../../../renderer/tests/common/mod.rs"]
mod native;
#[allow(unused_imports)] // Shared test facade: each consuming target uses a different subset.
pub use native::{floats, gpu, release_keepalive, words};
#[path = "../../../renderer/tests/common/window.rs"]
pub mod window;
