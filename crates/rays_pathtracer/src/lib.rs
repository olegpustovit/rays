//! the hardware path tracer. The Lisp kernels (pathtrace.rays,
//! pathtrace_helpers.rays, resolve_preview.rays) stay Lisp and are read in place from lib/.

// ponytail: the Lisp op bodies return flow_check's large `Diagnostic`; box it there, not here.
#![allow(clippy::too_many_arguments, clippy::type_complexity, clippy::result_large_err)]

pub mod frame_data;
pub mod lisp_kernel;
pub mod lisp_kernel_cache;
pub mod mesh_build;
pub mod mesh_data;
pub mod rays_pathtracer;
