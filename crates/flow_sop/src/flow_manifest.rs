//! Port of lib/flow_sop/flow_manifest.ml: the generator behind `flow_manifest.sexp` (the OCaml
//! module is a tiny executable printing `Manifest.generate ~extra:Editor_document.Kinds.descriptors
//! Sop.Nodes.factories`). `extra` is the editor document's generated scene/world/settings kinds.
use rays_diagnostic::Diagnostic;

use crate::catalog::Descriptor;
use crate::manifest;

/// The text the OCaml executable prints: the manifest of the native factories and the embedded
/// Lisp node library, then `extra`.
pub fn text(extra: &[Descriptor]) -> Result<String, Diagnostic> {
    manifest::generate(extra, None, &sop::nodes::factories()).map(|(text, _)| text)
}
