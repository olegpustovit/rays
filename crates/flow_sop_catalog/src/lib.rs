//! Catalog, manifest, port and network types extracted from flow_sop so editor_document
//! does not depend on the full flow_sop compile graph (lower, kernels, rdk).
pub mod port;
pub mod network;
pub mod catalog;
pub mod encoded_parameters;
pub mod manifest;
