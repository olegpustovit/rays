//! Port of lib/flow_ir (typed dataflow IR over flow evaluations). One module per OCaml file.
//! OCaml `Core` is `core_ir` here: a module named `core` would shadow Rust's `core` crate.
pub mod core_ir;
pub mod core_marks;
pub mod fallback;
pub mod core_region;
pub mod core_fuse;
pub mod packed;
pub mod kernel_layout;
pub mod core_lower;
pub mod core_draw;
pub mod core_exec;
pub mod flow_ir;
#[cfg(test)]
mod region_snapshot;

// Ported OCaml test files (lib/flow_ir/test_*.ml).
#[cfg(test)] mod test_arena;
#[cfg(test)] mod test_array_composition;
#[cfg(test)] mod test_array_max_length;
#[cfg(test)] mod test_compact_scan;
#[cfg(test)] mod test_gather;
#[cfg(test)] mod test_scatter;
#[cfg(test)] mod test_inner_sum;
#[cfg(test)] mod test_reductions;
#[cfg(test)] mod test_grade_index;
#[cfg(test)] mod test_static_map;
#[cfg(test)] mod test_uniforms;
#[cfg(test)] mod test_core_records;
#[cfg(test)] mod test_core_control;
#[cfg(test)] mod test_core_errors;
#[cfg(test)] mod test_core_lists;
#[cfg(test)] mod test_core_operators;
#[cfg(test)] mod test_core_state;
#[cfg(test)] mod test_core_regions;
#[cfg(test)] mod test_error_rows;
#[cfg(test)] mod test_with_error;
#[cfg(test)] mod test_core_marks;
#[cfg(test)] mod test_region_snapshot;
#[doc(hidden)]
pub mod test_support;
