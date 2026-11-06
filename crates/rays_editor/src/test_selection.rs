//! The selection, the gesture echo and the pick sets (small checks of the pure view-state modules).
use crate::echo;
use crate::pick;
use crate::selection as sel;
use flow_graph::flow_edit::{ArgKey, Op};
use flow_syntax::syntax as s;

#[test]
fn selection_is_sorted_unique_with_the_first_primary() {
    assert_eq!(sel::selected(&sel::empty()), None);
    let one = sel::select(4);
    assert_eq!((sel::selected(&one), sel::selected_nodes(&one).to_vec()), (Some(4), vec![4]));
    let many = sel::select_nodes(&[7, 2, 7, 5]);
    assert_eq!((sel::selected(&many), sel::selected_nodes(&many).to_vec()), (Some(7), vec![2, 5, 7]));
    assert_eq!(sel::select_nodes(&[]), sel::clear());
}

fn path(parts: &[&str]) -> Vec<String> {
    parts.iter().map(|p| p.to_string()).collect()
}

fn value(text: &str) -> s::Syntax {
    s::parse(text).unwrap().remove(0)
}

#[test]
fn echo_words_of_an_edit() {
    let set = |key: ArgKey, sub: Vec<i64>, text: &str| Op::SetArg { node: path(&["g", "n"]), key, sub, value: value(text) };
    assert_eq!(echo::words(&set(ArgKey::Kw("size".into()), vec![], "2.5")), Some(":size 2.5 on g/n".to_string()));
    assert_eq!(echo::words(&set(ArgKey::Pos(1), vec![0, 2], "7")), Some("input 1 part 0.2 7 on g/n".to_string()));
    assert_eq!(echo::words(&set(ArgKey::Whole, vec![], "7")), None);
    let connect = Op::Connect { node: path(&["g", "@result"]), key: ArgKey::Whole, src: "box".into(), iter: false };
    assert_eq!(echo::words(&connect), Some("box as the result of g/@result".to_string()));
    let rename = Op::Rename { node: path(&["g", "a"]), to_: "b".into() };
    let bypass = Op::ToggleBypass { node: path(&["g", "b"]) };
    let hoist = Op::Hoist { node: path(&["g", "c"]) };
    assert_eq!(echo::words(&rename), Some("g/a renamed b".to_string()));
    assert_eq!(echo::batch(&[rename.clone()]), Some("g/a renamed b".to_string()));
    assert_eq!(echo::batch(&[rename, bypass, hoist.clone()]), Some("g/a renamed b and 1 more".to_string()));
    assert_eq!(echo::batch(&[hoist]), None);
}

#[test]
fn pick_sets_compare_by_value() {
    let a: pick::Set = [1, 2].into_iter().collect();
    let b: pick::Set = [2, 1].into_iter().collect();
    assert!(pick::same_set(&a, &b) && !pick::same_set(&a, &pick::Set::new()));
}
