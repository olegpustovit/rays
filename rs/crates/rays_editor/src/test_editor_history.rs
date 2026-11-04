//! The History half of lib/editor_core/test_history.ml, over the specialised history: the present is
//! a real `Document` whose `active_camera` carries the test's integer (and the snapshot's
//! `saved_camera` is the entry's), so every assertion of the OCaml test keeps its meaning.
use std::cell::RefCell;
use std::sync::Arc;

use editor_document::document::{self, Snapshot, T as Document};
use editor_document::lowering::{self, Options};
use editor_document::workspace_doc::{self, TextOptions};
use rays_diagnostic::Diagnostic;

use crate::editor_history::{self as hist, Merge};

fn base() -> Document {
    let catalog = editor_document::kinds::catalog(flow_sop::manifest::VERSION, &sop::nodes::factories()).expect("catalog");
    let ws = workspace_doc::of_text(&TextOptions::default(), &catalog, "(workspace w (graph g :context sop (sop/box)))").expect("workspace");
    lowering::of_workspace(&Options { factories: Some(sop::nodes::factories()), ..Default::default() }, &ws).expect("document")
}

fn doc(base: &Document, n: i64) -> Arc<Document> {
    let mut d = base.clone();
    d.active_camera = Some(n);
    Arc::new(d)
}

fn n_of(entry: &Snapshot) -> i64 {
    entry.saved_camera.unwrap_or(0)
}

struct Fixture {
    base: Document,
}

impl Fixture {
    fn new_h(&self, capacity: Option<i64>, present: i64) -> hist::t {
        hist::create(capacity, None, None, Arc::new(document::snapshot), doc(&self.base, present)).unwrap()
    }
    fn record(&self, merge: Merge, label: &str, value: i64, h: hist::t) -> hist::t {
        hist::record(merge, label, doc(&self.base, value), h)
    }
    fn step(&self, value: i64, h: hist::t) -> hist::t {
        self.record(Merge::Step, "Edit", value, h)
    }
    fn restore(&self, entry: &Snapshot) -> Result<Document, Diagnostic> {
        let mut d = self.base.clone();
        d.active_camera = entry.saved_camera;
        Ok(d)
    }
    fn undo(&self, h: &hist::t) -> Option<hist::t> {
        hist::undo(&|e| self.restore(e), h).map(|r| r.unwrap())
    }
    fn redo(&self, h: &hist::t) -> Option<hist::t> {
        hist::redo(&|e| self.restore(e), h).map(|r| r.unwrap())
    }
}

fn present(h: &hist::t) -> i64 {
    hist::present(h).active_camera.unwrap_or(0)
}

fn depth(h: &hist::t) -> i64 {
    hist::depth(h)
}

#[test]
fn history_merge_seal_undo_redo_capacity_labels() {
    let f = Fixture { base: base() };
    let h = f.new_h(Some(3), 0);
    let h = f.step(1, h);
    let h = f.step(2, h);
    let h = f.record(Merge::Repair, "Edit", 3, h);
    assert!(present(&h) == 3 && depth(&h) == 2);
    let h = f.undo(&h).unwrap();
    assert!(present(&h) == 1 && hist::can_redo(&h));
    let h = f.redo(&h).unwrap();
    assert!(present(&h) == 3 && !hist::can_redo(&h));
    let h = f.step(4, h);
    let h = f.step(5, h);
    let h = f.step(6, h);
    assert!(depth(&h) == 3);
    let mut bottom = h.clone();
    while let Some(next) = f.undo(&bottom) {
        bottom = next;
    }
    assert!(present(&bottom) == 3 && !hist::can_undo(&bottom));
    let h = f.record(Merge::Gesture("move/7".into()), "Edit", 7, h);
    let h = f.record(Merge::Gesture("move/7".into()), "Edit", 8, h);
    assert!(present(&h) == 8 && depth(&h) == 3);
    let h = hist::seal(false, h);
    let h = f.record(Merge::Gesture("move/7".into()), "Edit", 9, h);
    assert!(present(&f.undo(&h).unwrap()) == 8);
    let burst = |at: f64, value: i64, h: hist::t| f.record(Merge::Burst { key: "view".into(), at, window: 0.25 }, "Edit", value, h);
    let h = burst(1.2, 2, burst(1.0, 1, f.new_h(None, 0)));
    assert!(depth(&h) == 1 && present(&f.undo(&h).unwrap()) == 0);
    let h = burst(1.5, 3, h);
    assert!(depth(&h) == 2 && present(&f.undo(&h).unwrap()) == 2);
    let h = f.undo(&h).unwrap();
    // a repair is no edit: it leaves what redo restores (E12)
    let h = f.record(Merge::Repair, "Edit", 10, h);
    assert!(present(&h) == 10 && hist::can_redo(&h) && present(&f.redo(&h).unwrap()) == 3);
    // Labels name what undo reverts and what redo reapplies.
    let h = f.record(Merge::Step, "Move", 2, f.record(Merge::Step, "Connect", 1, f.new_h(None, 0)));
    assert!(hist::label(&h) == "Move");
    let h = f.undo(&h).unwrap();
    assert!(hist::label(&h) == "Connect" && hist::redo_label(&h) == Some("Move"));
    // The cap counts exactly the entries to undo and to redo, at every step of any sequence.
    let bytes_fn: Arc<dyn Fn(&Snapshot) -> i64> = Arc::new(|e: &Snapshot| 10 + n_of(e));
    let keep: Arc<dyn Fn(&Document) -> Snapshot> = Arc::new(document::snapshot);
    let check = |h: &hist::t| {
        let (entries, _) = hist::retained(h);
        assert!(entries.iter().map(|e| 10 + n_of(e)).sum::<i64>() == hist::byte_depth(h));
        assert!(hist::byte_depth(h) >= 0);
    };
    let h = hist::create(None, Some(200), Some(bytes_fn.clone()), keep.clone(), doc(&f.base, 0)).unwrap();
    assert!(hist::byte_depth(&h) == 0);
    let h = f.step(1, h);
    check(&h);
    assert!(hist::byte_depth(&h) == 10);
    let h = f.step(2, h);
    check(&h);
    let h = f.record(Merge::Gesture("a".into()), "Edit", 3, h);
    check(&h);
    let at_three = hist::byte_depth(&h);
    let h = f.record(Merge::Gesture("a".into()), "Edit", 4, h);
    check(&h);
    assert!(hist::byte_depth(&h) == at_three);
    let h = f.undo(&h).unwrap();
    check(&h);
    assert!(present(&h) == 2);
    let h = f.undo(&h).unwrap();
    check(&h);
    let h = f.redo(&h).unwrap();
    check(&h);
    // a commit abandons what redo held, releasing it once
    let h = f.step(50, h);
    check(&h);
    assert!(!hist::can_redo(&h));
    // the byte cap goes oldest first and keeps the newest
    let mut h = h;
    for n in [60, 70, 80, 90, 100, 110] {
        h = f.step(n, h);
        check(&h);
    }
    assert!(hist::byte_depth(&h) <= 200 && hist::can_undo(&h));
    let huge = hist::create(None, Some(1), Some(bytes_fn.clone()), keep.clone(), doc(&f.base, 1000)).unwrap();
    let huge = f.step(1001, huge);
    assert!(depth(&huge) == 1 && hist::byte_depth(&huge) == 10 + 1000);
    let huge = hist::release(huge);
    assert!(depth(&huge) == 0 && hist::byte_depth(&huge) == 0 && present(&huge) == 1001);
    check(&h);
    // the retreat's step: only the oldest go
    let shed_from = depth(&h);
    let h = f.step(7, h);
    let h = f.step(8, h);
    let h = f.step(9, h);
    let shed_from = shed_from.max(depth(&h));
    let h = hist::shed(2, h);
    check(&h);
    assert!(depth(&h) == 2 && shed_from > 2);
    let h = hist::restart(doc(&f.base, 5), h);
    check(&h);
    assert!(present(&h) == 5 && depth(&h) == 0 && !hist::can_undo(&h) && hist::byte_depth(&h) == 0);
}

/// An entry is only what `keep` took; the last present stays live, older ones come from `restore`.
#[test]
fn history_entries_are_snapshots_the_last_present_stays_live() {
    let f = Fixture { base: base() };
    let restored: RefCell<Vec<i64>> = RefCell::new(Vec::new());
    let restore = |entry: &Snapshot| -> Result<Document, Diagnostic> {
        restored.borrow_mut().insert(0, n_of(entry));
        if n_of(entry) == -1 {
            Err(rays_diagnostic::error("E_TEST".to_string(), "no".to_string(), None, None, None, Vec::new(), Vec::new()))
        } else {
            f.restore(entry)
        }
    };
    let h = f.new_h(None, 1);
    let h = f.step(2, h);
    let h = f.step(3, h);
    let back = |h: &hist::t| hist::undo(&restore, h).unwrap().unwrap();
    let forth = |h: &hist::t| hist::redo(&restore, h).unwrap().unwrap();
    let h = back(&h);
    assert!(present(&h) == 2 && restored.borrow().is_empty());
    let h = forth(&h);
    assert!(present(&h) == 3 && restored.borrow().is_empty());
    let h = back(&back(&h));
    assert!(present(&h) == 1 && *restored.borrow() == vec![1]);

    let bad_keep: Arc<dyn Fn(&Document) -> Snapshot> = Arc::new(|d: &Document| Snapshot { saved_camera: Some(-1), ..document::snapshot(d) });
    let bad = hist::create(None, None, None, bad_keep, doc(&f.base, 1)).unwrap();
    let bad = f.step(2, bad);
    let bad = f.step(3, bad);
    let ok_restore = |_: &Snapshot| -> Result<Document, Diagnostic> { Ok((*doc(&f.base, 9)).clone()) };
    let bad = hist::undo(&ok_restore, &bad).unwrap().unwrap();
    assert!(present(&bad) == 2);
    let bad = f.step(4, bad);
    match hist::undo(&restore, &bad) {
        Some(Ok(_)) => {}
        _ => panic!("expected the last present to revive without restore"),
    }
}

#[test]
fn no_op_edit_is_detected_by_value() {
    use crate::editor_model::same_document;
    let b = base();
    assert!(same_document(&b, &b.clone()));
    let mut moved = b.clone();
    moved.active_camera = Some(3);
    assert!(!same_document(&b, &moved));
}
