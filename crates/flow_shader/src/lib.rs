//! Port of lib/flow_shader: the shader printers (`emit` MSL, `spirv`, `raster` stages) over flow,
//! flow_ir and ogpu_core, and the `print_shader` binary. `trace.rays` and `noise.rays` are copies of
//! the OCaml library's device-side helpers (compiled once per scene variant / octave count).
pub mod emit;
pub mod lisp_stage;
pub mod md5;
pub mod noise;
pub mod print;
pub mod raster;
pub mod spirv;
pub mod trace;
