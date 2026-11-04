//! Port of lib/flow_sop (typed SOP/value overlay over flow + flow_ir + sop). One module per OCaml file.
//! Lisp SOP bodies stay in lib/flow_sop/nodes/*.rays (the contract), read in place.
pub mod port;
pub mod curve;
pub mod network;
pub mod catalog;
pub mod encoded_parameters;
pub mod manifest;
pub mod flow_manifest;
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
#[cfg(any(test, feature = "test-support"))] pub mod lisp_sop;
