//! Catalog manifest generation; `generate` wires in the SOP workspace operators.
pub use flow_sop_catalog::manifest::*;
use crate::catalog::Descriptor;
use flow_check::defnode::Defnode;
use rays_diagnostic::Diagnostic;
use sop::edit_graph::Factory;

pub fn generate(extra: &[Descriptor], nodes: Option<&[Defnode]>, factories: &[Factory]) -> Result<(String, String), Diagnostic> {
    generate_with(crate::operators::all(), extra, nodes, factories)
}
