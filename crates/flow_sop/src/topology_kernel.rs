//! Port of lib/flow_sop/topology_kernel.ml(i): the public face of `topology_kernel_impl`.
use crate::topology_kernel_impl as imp;
use flow_ir::flow_ir::Gpu::Integers;
use flow_syntax::value::Value;
use rays_diagnostic::Diagnostic;
use rdk::geometry::T as Geometry;
use std::sync::Arc;

pub fn mesh_points(points: &Value) -> Result<(i64, imp::Positions), Diagnostic> {
    imp::mesh_points(points)
}

pub fn mesh(args: &[(String, Value)]) -> Result<Geometry, Diagnostic> {
    imp::mesh(args)
}

/// Unread resident vertex points, primitive offsets and kinds, with their logical lengths. Counts
/// and holder lookup do not materialise topology.
pub fn resident(geometry: &Geometry) -> Option<(Arc<Integers>, Arc<Integers>, Arc<Integers>)> {
    imp::resident(geometry)
}
