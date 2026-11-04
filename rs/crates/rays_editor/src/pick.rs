//! Viewport provenance. Every collecting merge of a workspace tags its primitives with
//! `flow_sop::lower::source_attribute`; this module reads the tag back from the displayed geometry,
//! casts a CPU ray to find the primitive under the pointer, and tints the primitives of the probed
//! iteration.
//!
//! ponytail: a CPU ray over `Rdk.Surface_index` (a BVH of the displayed geometry, built at the first
//! click and kept with the piece), not an ID buffer. The upgrade path when meshes pass about 1M
//! triangles is an OGPU ID-buffer feature (skill add-ogpu-feature).
use std::collections::BTreeSet;

use rays_math::vec3;
use rdk_rays::geometry_view as gv;
use sop::session::Output;

pub type Set = BTreeSet<i64>;

pub fn tags(geometry: &gv::T) -> Option<Vec<i64>> {
    gv::tags(flow_sop::lower::source_attribute(), geometry)
}

pub fn same_set(a: &Set, b: &Set) -> bool {
    a == b
}

pub fn surface(geometry: &gv::T) -> Option<gv::Surface> {
    gv::surface(geometry)
}

/// The nearest hit as (distance, tag); tags of a geometry without provenance are absent, so it
/// never picks.
pub fn cast(surface: Option<&gv::Surface>, geometry: &gv::T, origin: vec3::T, direction: vec3::T) -> Option<(f64, i64)> {
    gv::cast(flow_sop::lower::source_attribute(), surface, geometry, origin, direction)
}

/// The nearest hit as (distance, `shop_materialpath` of that primitive): what Alt-click reads. It
/// needs no provenance tags.
pub fn cast_material(surface: Option<&gv::Surface>, geometry: &gv::T, origin: vec3::T, direction: vec3::T) -> Option<(f64, Option<String>)> {
    gv::cast_material(surface, geometry, origin, direction)
}

pub const DIM: f64 = 0.3;
pub const MIX: f64 = 0.6;

/// The primitives whose tag is in `lit` take the selection tint over their own colour, the rest are
/// dimmed: a vertex colour per corner, so a prepare that reads `Cd` (as `to_mesh` does) draws it
/// without knowing about picking.
pub fn tint(output: &Output, lit: &Set) -> Result<Output, rays_diagnostic::Diagnostic> {
    let geometry = sop::payload::geometry(&output.payload)?;
    if lit.is_empty() {
        return Ok(output.clone());
    }
    let (r, g, b, _) = rays_math::color::to_floats(pxui::theme::default().accent);
    let tinted = gv::tint(flow_sop::lower::source_attribute(), (r, g, b), &|tag| lit.contains(&tag), &geometry);
    // OCaml `tinted == geometry`: an untagged geometry comes back as itself
    if rdk::geometry::data_id(&tinted) == rdk::geometry::data_id(&geometry) {
        return Ok(output.clone());
    }
    Ok(Output { payload: sop::payload::T::Geometry(tinted), ..output.clone() })
}
