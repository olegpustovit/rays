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
        }).unwrap().join().unwrap_or_else(|_| panic!("node golden {name} failed"));
    }

    // One process per quarter of the library shares the cached catalog and node definitions
    // (~0.2 s per process vs ~0.06 s per node once warm); each failure still names its node.
    #[test] fn node_goldens_1() { NODES[..32].iter().for_each(|name| checked_node_matches(name)) }
    #[test] fn node_goldens_2() { NODES[32..64].iter().for_each(|name| checked_node_matches(name)) }
    #[test] fn node_goldens_3() { NODES[64..96].iter().for_each(|name| checked_node_matches(name)) }
    #[test] fn node_goldens_4() { NODES[96..].iter().for_each(|name| checked_node_matches(name)) }

    const NODES: &[&str] = &[
        "sop/attr_from_image",
        "sop/attribute_blur",
        "sop/attribute_composite",
        "sop/attribute_copy",
        "sop/attribute_fade",
        "sop/attribute_interpolate",
        "sop/attribute_laplacian",
        "sop/attribute_noise",
        "sop/attribute_noise_quaternion",
        "sop/attribute_randomize",
        "sop/attribute_remap",
        "sop/bend",
        "sop/blast",
        "sop/blast_by_attribute",
        "sop/blend_shapes",
        "sop/bound",
        "sop/box",
        "sop/carve",
        "sop/circle",
        "sop/circle_from_edges",
        "sop/color_by_height",
        "sop/compact_points",
        "sop/connectivity",
        "sop/convert_line",
        "sop/copy_to_points",
        "sop/crease",
        "sop/delete_attributes",
        "sop/delete_edge_group",
        "sop/distance_along_geometry",
        "sop/distance_from_target",
        "sop/duplicate",
        "sop/edge_cusp",
        "sop/edge_divide",
        "sop/edge_equalize",
        "sop/edge_flip",
        "sop/edge_relax",
        "sop/edge_straighten",
        "sop/edge_transport",
        "sop/edge_transport_curves",
        "sop/edge_transport_parent",
        "sop/ends",
        "sop/enumerate",
        "sop/extract_centroid",
        "sop/extract_point_from_curve",
        "sop/graph_color",
        "sop/grid",
        "sop/group_backface",
        "sop/group_boundary_components",
        "sop/group_bounds",
        "sop/group_combine",
        "sop/group_copy",
        "sop/group_delete",
        "sop/group_edge_depth",
        "sop/group_edges",
        "sop/group_expand",
        "sop/group_find_path",
        "sop/group_from_attribute_boundary",
        "sop/group_invert",
        "sop/group_non_planar",
        "sop/group_normal",
        "sop/group_promote_boundary",
        "sop/group_promotions",
        "sop/group_random",
        "sop/group_range",
        "sop/group_ranges",
        "sop/group_rename",
        "sop/group_unshared",
        "sop/groups_from_name",
        "sop/iso_surface",
        "sop/line",
        "sop/match_axis",
        "sop/match_size",
        "sop/material",
        "sop/measure",
        "sop/measure_curvature",
        "sop/merge",
        "sop/mirror",
        "sop/mountain",
        "sop/name_from_groups",
        "sop/noise_displace",
        "sop/normals",
        "sop/ordered_group",
        "sop/peak",
        "sop/platonic",
        "sop/point_generate",
        "sop/point_jitter",
        "sop/point_replicate",
        "sop/point_split",
        "sop/point_velocity",
        "sop/points",
        "sop/poly_bridge",
        "sop/poly_loft",
        "sop/poly_path",
        "sop/polyframe",
        "sop/polywire",
        "sop/promote_attributes",
        "sop/rename_attributes",
        "sop/rename_edge_group",
        "sop/resample",
        "sop/rest_position",
        "sop/reverse",
        "sop/revolve",
        "sop/rewire_vertices",
        "sop/scatter",
        "sop/separate_pieces",
        "sop/set_color",
        "sop/set_float",
        "sop/set_int",
        "sop/set_orient",
        "sop/set_transform",
        "sop/set_vector",
        "sop/skin",
        "sop/smooth",
        "sop/snap_to_grid",
        "sop/sort",
        "sop/spiral",
        "sop/subdivide",
        "sop/swap_attributes",
        "sop/sweep",
        "sop/sweep_circle",
        "sop/torus",
        "sop/transform",
        "sop/tube",
        "sop/uv_auto_seam",
        "sop/uv_project",
        "sop/uv_sphere",
        "sop/uv_transform",
        "sop/uv_unitize",
    ];


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
