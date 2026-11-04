//! Port of the slice of lib/sop/test_diagnostics.ml that does not need Lisp: the diagnostic code of a
//! missing group, decided by `support`. The OCaml file is one table of Lisp texts over every node
//! (`(sop/attribute_fade ...)`, ...) cooked at 1 and 4 domains; those rows return when the
//! node-definition waves and the Lisp layer exist.
use crate::support::{self, ElementGroup};
use crate::test_support::fixture;

#[test]
fn missing_groups_have_the_missing_group_code() {
    let g = fixture();
    for selection in [
        ElementGroup::Point("missing".into()),
        ElementGroup::Vertex("missing".into()),
        ElementGroup::Primitive("missing".into()),
        ElementGroup::Edge("missing".into()),
    ] {
        match support::resolve_element_group("op", &Some(selection), &g) {
            Err(d) => assert_eq!(d.code, "missing_group"),
            Ok(_) => panic!("a missing group resolved"),
        }
    }
    assert!(support::resolve_element_group("op", &None, &g).unwrap().is_none());
    let d = support::resolve_optional_primitive_group("poly_cut", &Some("missing".into()), &g).err().unwrap();
    assert_eq!(d.message, "poly_cut could not find primitive group \"missing\"");
}

#[test]
fn rdk_failures_become_traced_diagnostics() {
    let d = support::rdk_error(Vec::new(), "blast", "boom");
    assert_eq!((d.code.as_str(), d.cause.as_deref()), ("blast_failed", Some("boom")));
    assert_eq!(d.message, "blast could not produce valid geometry");
}
