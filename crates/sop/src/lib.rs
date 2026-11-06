//! Port of lib/sop: immutable SOP graphs over rdk. See PORTING.md for the node pattern.
pub mod context;
pub mod decl;
pub mod image;
pub mod kernel;
pub mod payload;
pub mod node;
pub mod support;
pub mod groups;
pub mod custom;
pub mod zone;
pub mod edit_graph;
pub mod session;
pub mod async_cook;
pub mod nodes;
pub mod shapes;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod test_payload;
#[cfg(test)]
mod test_edit_graph;
#[cfg(test)]
mod test_diagnostics;
#[cfg(test)]
mod test_facts;
#[cfg(test)]
mod test_manifest;
#[cfg(test)]
mod test_nodes;
#[cfg(test)]
mod test_parallel_branches;
// Deferred: node-definition files, filled by a later wave with the pattern in PORTING.md.
pub mod sop_topology;
pub mod attributes;
pub mod image_nodes;
