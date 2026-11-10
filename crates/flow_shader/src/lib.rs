//! MSL, SPIR-V and raster-stage printers consume typed Flow IR; embedded trace/noise helpers compile per scene variant or octave count.
pub mod emit;
pub mod lisp_stage;
pub mod noise;
pub mod print;
pub mod raster;
pub mod spirv;
pub mod trace;
