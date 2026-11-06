//! Port of lib/rays_pathtracer: the hardware path tracer. The Lisp kernels (pathtrace.rays,
//! pathtrace_helpers.rays, resolve_preview.rays) stay Lisp and are read in place from lib/.
#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals, clippy::all)]

pub mod frame_data;
pub mod lisp_kernel;
pub mod lisp_kernel_cache;
pub mod mesh_build;
pub mod mesh_data;
pub mod rays_pathtracer;
