// Port of lib/flow_graph/test_string_bytes.ml.
use crate::flow_edit::{apply_checked, ArgKey, Op};
use crate::projection as p;
use crate::test_support::*;
use flow_eval::Value;
use flow_syntax::lisp;
use flow_syntax::syntax as s;
use flow_syntax::ty::Ty;
use std::sync::Arc;

#[test]
fn string_bytes() {
    let source = parse("(workspace bytes (graph g :context host (let* [text (array/text 1 \"default\")] text)))");
    let ws = check(&source);
    let card = p::find(&p::of_graph(&catalog(), &ws, "g").unwrap(), &path(&["g", "text"])).unwrap();
    assert_eq!(card.ty, Ty::Array(Box::new(Ty::Text)));
    let bytes: Vec<u8> = (0..=255u8).collect();
    let (edited, workspace) = apply_checked(
        &catalog(),
        &source,
        &Op::SetArg { node: card.path.clone(), key: ArgKey::Pos(1), sub: vec![], value: s::make(s::Draft::Str(bytes.clone())) },
    )
    .unwrap();
    let expected = Value::Text_array(Arc::new(vec![bytes.clone()]));
    assert_eq!(result(&workspace, 0.0), expected);
    let saved = lisp::print(&edited).unwrap().0;
    let restored = check(&parse(&saved));
    assert_eq!(result(&restored, 0.0), expected);
    assert_eq!(p::find(&p::of_graph(&catalog(), &restored, "g").unwrap(), &card.path).unwrap().ty, Ty::Array(Box::new(Ty::Text)));
}
