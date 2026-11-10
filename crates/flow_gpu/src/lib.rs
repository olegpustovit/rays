//! runs flow_shader's kernels on OGPU. A pipeline cache of 64 and owned
//! native runners. Command submission and mutable OGPU handles remain owner-thread-only (`Rc`).
//! Settled outputs capture an owning, read-only native allocation handle: their first materialise
//! may run on any thread without asking the owner to pump or copying bytes eagerly at settle.
#![allow(clippy::too_many_arguments, clippy::type_complexity, clippy::new_without_default)]
pub mod small_lru;
pub mod kernels;
pub mod pipelines;
pub mod run;
pub mod host;
pub mod image_sink;
