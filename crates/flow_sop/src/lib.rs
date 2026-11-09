//! Port of lib/flow_sop (typed SOP/value overlay over flow + flow_ir + sop). One module per OCaml file.
//! Lisp SOP bodies stay in crates/flow_sop/nodes/*.rays (the contract), read in place.
pub use flow_sop_catalog::{catalog, encoded_parameters, network, port};
pub mod manifest;
pub mod curve;
pub mod lisp_node;
pub mod trace_scene;
pub mod graph_kernel;
pub mod attribute_metadata;
pub mod value_lane;
pub mod operators;
pub mod group_kernel;
pub mod context_kernel;
pub mod image_kernel;
pub mod function_kernel;
pub mod topology_kernel;
pub mod topology_kernel_impl;
pub mod attribute_kernel;
pub mod lower;
pub mod lisp_sop;

#[cfg(test)]
#[path = "test_manifest_golden.rs"]
mod test_manifest_golden;
#[cfg(test)]
#[path = "test_encoded_parameters.rs"]
mod test_encoded_parameters;
#[cfg(test)]
#[path = "test_catalog_ops.rs"]
mod test_catalog_ops;
#[cfg(test)]
#[path = "test_alloc_budget.rs"]
mod test_alloc_budget;
