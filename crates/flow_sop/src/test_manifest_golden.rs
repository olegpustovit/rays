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

    fn node_goldens(part: usize) {
        let nodes = catalog::node_definitions().unwrap();
        let size = nodes.len().div_ceil(4);
        for node in nodes.iter().skip(part * size).take(size) { checked_node_matches(&node.kind.qualified); }
    }
    #[test] fn node_goldens_1() { node_goldens(0) }
    #[test] fn node_goldens_2() { node_goldens(1) }
    #[test] fn node_goldens_3() { node_goldens(2) }
    #[test] fn node_goldens_4() { node_goldens(3) }
