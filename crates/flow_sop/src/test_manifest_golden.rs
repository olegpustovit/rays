//! Manifest golden tests (need flow_sop::operators).
use crate::manifest::generate;
use crate::{catalog, operators};
use flow_check::workspace;
use flow_sop_catalog::manifest::{descriptor_entry, group, lisp_node, md5_hex, quote, VERSION};
use flow_syntax::syntax;
use sop::edit_graph;

const GOLDEN: &str = include_str!("../flow_manifest.sexp");
    fn sop_golden() -> &'static str {
        let cut = ["scene/", "world/", "settings/"]
            .iter()
            .filter_map(|prefix| GOLDEN.find(&format!("\n    (kind \"{prefix}")))
            .min()
            .expect("the golden holds editor kinds");
        &GOLDEN[..cut]
    }

    fn kinds() -> Vec<String> {
        sop_golden().split("\n    (kind ").skip(1).map(|body| format!("(kind {body}")).collect()
    }

    #[test]
    fn manifest_native_kinds_order_and_digest_match_the_checked_in_file() {
        let factories = sop::nodes::factories();
        let nodes = catalog::node_definitions().unwrap();
        let expected = kinds();
        assert_eq!(expected.len(), factories.len() + nodes.len());
        assert_eq!(NODES, nodes.iter().map(|node| node.kind.qualified.as_str()).collect::<Vec<_>>());
        for (factory, expected) in factories.iter().zip(&expected) {
            assert_eq!(&descriptor_entry(Some(edit_graph::factory_facts(factory).clone()), &catalog::descriptor(factory)), expected);
        }
        for (node, expected) in nodes.iter().zip(&expected[factories.len()..]) {
            assert!(expected.starts_with(&format!("(kind {}\n", quote(&node.kind.qualified))));
        }
        let payload = format!("{}\n{}", group("version", &[VERSION.to_string()]), group("kinds", &expected));
        assert_eq!(sop_golden(), format!("(flow_manifest\n  (version {VERSION})\n  (digest \"{}\")\n  (kinds\n    {}", md5_hex(payload.as_bytes()), expected.join("\n    ")));
        // Exercise the public generator without rechecking the entire node library.
        let (native, digest) = generate(&[], Some(&[]), &factories).unwrap();
        let native_kinds = &expected[..factories.len()];
        let payload = format!("{}\n{}", group("version", &[VERSION.to_string()]), group("kinds", native_kinds));
        assert_eq!(digest, md5_hex(payload.as_bytes()));
        assert_eq!(native, format!("(flow_manifest\n  (version {VERSION})\n  (digest \"{digest}\")\n  (kinds\n    {}))\n", native_kinds.join("\n    ")));
    }

    fn checked_node_matches(name: &'static str) {
        std::thread::Builder::new().stack_size(256 << 20).spawn(move || {
            let node = catalog::node_definitions().unwrap().iter().find(|node| node.kind.qualified == name).expect("shipped node");
            let factories = sop::nodes::factories();
            let catalog = catalog::of_factories(VERSION, &[], None, &factories).unwrap();
            // Check the real definition and its transitive helpers, not the whole unrelated library.
            let sources = catalog::private::reachable_sources(catalog::sources().unwrap(), std::slice::from_ref(&node.form));
            let forms = syntax::parse("(workspace sop_nodes)").unwrap();
            let (checked, diagnostics) = catalog::check_workspace(operators::all(), true, Some(&sources), &catalog, &forms);
            let checked = checked.unwrap_or_else(|| panic!("{name}: {}", diagnostics.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")));
            let definition = checked.nodes.iter().find(|definition| definition.kind.qualified == name).expect("checked node");
            let mut shown = node.clone();
            shown.fields = definition.fields.clone();
            shown.kind = definition.kind.clone();
            let actual = lisp_node(&workspace::kind_calls(&definition.body.body), &shown);
            let prefix = format!("(kind {}\n", quote(name));
            let expected = kinds().into_iter().find(|kind| kind.starts_with(&prefix)).expect("golden entry");
            assert_eq!(actual, expected, "{name}");
        }).unwrap().join().unwrap();
    }

    macro_rules! node_goldens {
        ($($test:ident $name:literal),* $(,)?) => {
            const NODES: &[&str] = &[$($name),*];
            $(#[test] fn $test() { checked_node_matches($name) })*
        };
    }
    node_goldens!(
        node_attr_from_image "sop/attr_from_image",
        node_attribute_blur "sop/attribute_blur",
        node_attribute_composite "sop/attribute_composite",
        node_attribute_copy "sop/attribute_copy",
        node_attribute_fade "sop/attribute_fade",
        node_attribute_interpolate "sop/attribute_interpolate",
        node_attribute_laplacian "sop/attribute_laplacian",
        node_attribute_noise "sop/attribute_noise",
        node_attribute_noise_quaternion "sop/attribute_noise_quaternion",
        node_attribute_randomize "sop/attribute_randomize",
        node_attribute_remap "sop/attribute_remap",
        node_bend "sop/bend",
        node_blast "sop/blast",
        node_blast_by_attribute "sop/blast_by_attribute",
        node_blend_shapes "sop/blend_shapes",
        node_bound "sop/bound",
        node_box "sop/box",
        node_carve "sop/carve",
        node_circle "sop/circle",
        node_circle_from_edges "sop/circle_from_edges",
        node_color_by_height "sop/color_by_height",
        node_compact_points "sop/compact_points",
        node_connectivity "sop/connectivity",
        node_convert_line "sop/convert_line",
        node_copy_to_points "sop/copy_to_points",
        node_crease "sop/crease",
        node_delete_attributes "sop/delete_attributes",
        node_delete_edge_group "sop/delete_edge_group",
        node_distance_along_geometry "sop/distance_along_geometry",
        node_distance_from_target "sop/distance_from_target",
        node_duplicate "sop/duplicate",
        node_edge_cusp "sop/edge_cusp",
        node_edge_divide "sop/edge_divide",
        node_edge_equalize "sop/edge_equalize",
        node_edge_flip "sop/edge_flip",
        node_edge_relax "sop/edge_relax",
        node_edge_straighten "sop/edge_straighten",
        node_edge_transport "sop/edge_transport",
        node_edge_transport_curves "sop/edge_transport_curves",
        node_edge_transport_parent "sop/edge_transport_parent",
        node_ends "sop/ends",
        node_enumerate "sop/enumerate",
        node_extract_centroid "sop/extract_centroid",
        node_extract_point_from_curve "sop/extract_point_from_curve",
        node_graph_color "sop/graph_color",
        node_grid "sop/grid",
        node_group_backface "sop/group_backface",
        node_group_boundary_components "sop/group_boundary_components",
        node_group_bounds "sop/group_bounds",
        node_group_combine "sop/group_combine",
        node_group_copy "sop/group_copy",
        node_group_delete "sop/group_delete",
        node_group_edge_depth "sop/group_edge_depth",
        node_group_edges "sop/group_edges",
        node_group_expand "sop/group_expand",
        node_group_find_path "sop/group_find_path",
        node_group_from_attribute_boundary "sop/group_from_attribute_boundary",
        node_group_invert "sop/group_invert",
        node_group_non_planar "sop/group_non_planar",
        node_group_normal "sop/group_normal",
        node_group_promote_boundary "sop/group_promote_boundary",
        node_group_promotions "sop/group_promotions",
        node_group_random "sop/group_random",
        node_group_range "sop/group_range",
        node_group_ranges "sop/group_ranges",
        node_group_rename "sop/group_rename",
        node_group_unshared "sop/group_unshared",
        node_groups_from_name "sop/groups_from_name",
        node_iso_surface "sop/iso_surface",
        node_line "sop/line",
        node_match_axis "sop/match_axis",
        node_match_size "sop/match_size",
        node_material "sop/material",
        node_measure "sop/measure",
        node_measure_curvature "sop/measure_curvature",
        node_merge "sop/merge",
        node_mirror "sop/mirror",
        node_mountain "sop/mountain",
        node_name_from_groups "sop/name_from_groups",
        node_noise_displace "sop/noise_displace",
        node_normals "sop/normals",
        node_ordered_group "sop/ordered_group",
        node_peak "sop/peak",
        node_platonic "sop/platonic",
        node_point_generate "sop/point_generate",
        node_point_jitter "sop/point_jitter",
        node_point_replicate "sop/point_replicate",
        node_point_split "sop/point_split",
        node_point_velocity "sop/point_velocity",
        node_points "sop/points",
        node_poly_bridge "sop/poly_bridge",
        node_poly_loft "sop/poly_loft",
        node_poly_path "sop/poly_path",
        node_polyframe "sop/polyframe",
        node_polywire "sop/polywire",
        node_promote_attributes "sop/promote_attributes",
        node_rename_attributes "sop/rename_attributes",
        node_rename_edge_group "sop/rename_edge_group",
        node_resample "sop/resample",
        node_rest_position "sop/rest_position",
        node_reverse "sop/reverse",
        node_revolve "sop/revolve",
        node_rewire_vertices "sop/rewire_vertices",
        node_scatter "sop/scatter",
        node_separate_pieces "sop/separate_pieces",
        node_set_color "sop/set_color",
        node_set_float "sop/set_float",
        node_set_int "sop/set_int",
        node_set_orient "sop/set_orient",
        node_set_transform "sop/set_transform",
        node_set_vector "sop/set_vector",
        node_skin "sop/skin",
        node_smooth "sop/smooth",
        node_snap_to_grid "sop/snap_to_grid",
        node_sort "sop/sort",
        node_spiral "sop/spiral",
        node_subdivide "sop/subdivide",
        node_swap_attributes "sop/swap_attributes",
        node_sweep "sop/sweep",
        node_sweep_circle "sop/sweep_circle",
        node_torus "sop/torus",
        node_transform "sop/transform",
        node_tube "sop/tube",
        node_uv_auto_seam "sop/uv_auto_seam",
        node_uv_project "sop/uv_project",
        node_uv_sphere "sop/uv_sphere",
        node_uv_transform "sop/uv_transform",
        node_uv_unitize "sop/uv_unitize",
    );


    /// The checked-in manifest, byte for byte up to the generated scene/world/settings kinds
    /// (`Editor_document.Kinds.descriptors`, outside the digest and not ported to this crate).
    #[test]
    #[ignore = "slow: exhaustive full-library regeneration; default node goldens check every entry independently"]
    fn manifest_reproduces_the_checked_in_file() {
        // the checker recurses on Lisp depth: the app runs it on a 256 MB stack
        std::thread::Builder::new()
            .stack_size(256 << 20)
            .spawn(reproduces)
            .expect("spawns")
            .join()
            .unwrap_or_else(|panic| std::panic::resume_unwind(panic));
    }

    fn reproduces() {
        let (text, digest) = generate(&[], None, &sop::nodes::factories()).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        let cut = ["scene/", "world/", "settings/"]
            .iter()
            .filter_map(|prefix| GOLDEN.find(&format!("\n    (kind \"{prefix}")))
            .min()
            .expect("the golden holds editor kinds");
        let expected = format!("{}))\n", &GOLDEN[..cut]);
        // ponytail: compare the sop and Lisp kinds only; add the editor descriptors when editor_document exists.
        assert_eq!(text.len(), expected.len(), "generated length");
        if text != expected {
            let at = text.bytes().zip(expected.bytes()).position(|(a, b)| a != b).unwrap_or(0);
            panic!("first difference at byte {at}: {:?} vs {:?}", &text[at.saturating_sub(60)..(at + 60).min(text.len())], &expected[at.saturating_sub(60)..(at + 60).min(expected.len())]);
        }
        assert!(GOLDEN.contains(&format!("(digest \"{digest}\")")));
    }
