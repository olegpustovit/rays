//! Port of lib/sop/nodes.ml: the native SOPs (exact predicates, spatial indexes, solvers, wiring,
//! carriers); every other kind is a Lisp `defnode`.
//!
//! The registry is the OCaml `factories` list in the OCaml order. Ported nodes are listed; the
//! others are kept as comments at their place so a later wave only uncomments one line per node
//! it ports (and `test_manifest` then checks it against flow_manifest.sexp).
use std::sync::OnceLock;

use crate::edit_graph::Factory;
use crate::{groups, shapes};

fn build_all() -> Vec<Factory> {
    vec![
        shapes::switch::factory(),
        // attributes::image_texels::factory(),
        // shapes::clip::factory(),
        // sop_topology::edge_collapse::factory(),
        // sop_topology::dissolve::factory(),
        // sop_topology::poly_bevel::factory(),
        // sop_topology::triangulate::factory(),
        // shapes::pack_instances::factory(),
        // sop_topology::subdivide_cracks::factory(),
        // sop_topology::clean::factory(),
        // sop_topology::facet::factory(),
        // sop_topology::remesh::factory(),
        // sop_topology::poly_extrude::factory(),
        // sop_topology::poly_fill::factory(),
        // sop_topology::join_curves::factory(),
        // sop_topology::boolean_fracture::factory(),
        // sop_topology::boolean::factory(),
        // sop_topology::boolean_seam::factory(),
        // sop_topology::boolean_detect::factory(),
        // sop_topology::intersection_analysis::factory(),
        // sop_topology::poly_reduce::factory(),
        // sop_topology::convex_hull::factory(),
        // sop_topology::fuse::factory(),
        // shapes::ray::factory(),
        // attributes::distance_from_geometry::factory(),
        // attributes::uv_flatten::factory(),
        // attributes::uv_relax::factory(),
        groups::group_transfer::factory(),
        // sop_topology::poly_cut::factory(),
        // attributes::attribute_mirror::factory(),
        // sop_topology::triangulate_2d::factory(),
        // shapes::soft_transform::factory(),
        // attributes::attribute_transfer::factory(),
        // attributes::attribute_transfer_surface::factory(),
        // attributes::attribute_transfer_all::factory(),
        shapes::null::factory(),
        // shapes::exploded_view::factory(),
    ]
}

/// The registry, built once; clones share factories.
pub fn factories() -> Vec<Factory> {
    static ALL: OnceLock<Vec<Factory>> = OnceLock::new();
    ALL.get_or_init(build_all).clone()
}
