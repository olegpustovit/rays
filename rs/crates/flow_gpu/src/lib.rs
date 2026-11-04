//! Port of lib/flow_gpu: runs flow_shader's kernels on OGPU. A pipeline cache of 64 and owned
//! native runners. Everything here is single-threaded by design (OGPU handles are `Rc`): the
//! closures a `Gpu::Backend` hands out find their host through a thread-local registry, so a call
//! from another thread is the OCaml "called from another domain" error.
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals, clippy::too_many_arguments, clippy::type_complexity, clippy::new_without_default)]
pub mod small_lru;
pub mod kernels;
pub mod pipelines;
pub mod run;
pub mod host;
pub mod image_sink;
