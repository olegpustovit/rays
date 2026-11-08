//! test_workspace_source.ml: the source file of a .rays sketch: found, saved over only while it is
//! what the document came from, polled twice a second, reloaded as one history entry, refused text
//! leaving the last good document; autosave, start keywords, imports, libraries, vocabulary.
use super::t4::*;
use editor_document::workspace_doc as Doc;
use flow_graph::flow_edit as fe;
use flow_syntax::syntax as S;
use input::{Event, Key};
use rays_editor::{editor, Source};
use std::cell::Cell;
use std::fs;
use std::sync::Arc;

fn sha(t: &str) -> String {
    editor_document::lowering::sha256(t)
}
fn has(t: &str, p: &str) -> bool {
    t.contains(p)
}
fn read(p: &str) -> String {
    fs::read_to_string(p).unwrap()
}
fn catalog() -> flow_check::catalog::Catalog {
    (*rays_editor::workspace_catalog(None).unwrap()).clone()
}
fn doc_of(text: &str, imports: Vec<(String, String)>) -> Result<Doc::T, Vec<rays_diagnostic::Diagnostic>> {
    Doc::of_text(&Doc::TextOptions { imports, ..Default::default() }, &catalog(), text)
}

fn sketch(radius: &str) -> String {
    format!(";; the header\n(workspace w\n  ;; one sphere\n  (graph g :context sop []\n    (sop/uv_sphere :radius {radius} :segments 8 :rings 4))) ; the tail\n")
}
const TYPO: &str = "(workspace w (graph g :context sop [] (sop/uv_spher :radius 0.5)))";

thread_local! { static CLOCK: Cell<f64> = const { Cell::new(1_700_000_000.) }; }

/// a write that always moves the mtime, whatever the file system's resolution
fn write(file: &str, text: &str) {
    fs::write(file, text).unwrap();
    let t = CLOCK.with(|c| {
        c.set(c.get() + 10.);
        c.get()
    });
    let f = fs::OpenOptions::new().write(true).open(file).unwrap();
    f.set_modified(std::time::UNIX_EPOCH + std::time::Duration::from_secs_f64(t)).unwrap();
}

fn printed(doc: &Doc::T) -> String {
    let t = Doc::to_text(doc).unwrap();
    if t.ends_with('\n') { t } else { t + "\n" }
}

fn gpu() -> bool {
    crate::common::gpu()
}

pub fn files() {
    let dir = temp_dir("src-files");
    let file = format!("{dir}/sketch.rays");
    let (text0, text1) = (sketch("0.5"), sketch("0.7"));
    write(&file, &text0);
    // Save: digest match writes the canonical text, comments intact, and re-reading it is the text
    let source = Source::at(&file, &sha(&text0));
    let doc = doc_of(&text0, vec![]).unwrap();
    let saved = printed(&doc);
    let source = Source::save(&source, &saved).unwrap_or_else(|_| panic!("save refused"));
    assert!(read(&file) == saved, "the file is the written text");
    assert!(has(&read(&file), "; the header") && has(&read(&file), "; one sphere") && has(&read(&file), "; the tail"), "comments survive: {}", read(&file));
    assert!(printed(&doc_of(&read(&file), vec![]).unwrap()) == saved, "the re-read prints the same");
    let names: Vec<_> = fs::read_dir(&dir).unwrap().map(|e| e.unwrap().file_name().to_string_lossy().into_owned()).collect();
    assert!(names == vec!["sketch.rays".to_string()], "no temporary file is left: {names:?}");
    // Save does not reload: the write moved the mtime, the digest is the remembered one
    write(&file, &read(&file));
    assert!(Source::poll(100., &source).1.is_none(), "own write reloaded");
    // digest mismatch: another writer changed the file, so Save refuses
    write(&file, &text1);
    assert!(Source::save(&source, "x").err() == Some(Source::Save_error::Changed), "saved over another writer's text");
    assert!(read(&file) == text1, "the refused save left the file alone");
    // the poll: one content read per half second
    let source = Source::at(&file, &sha(&text0));
    let (source, changed) = Source::poll(0., &source);
    assert!(changed == Some(text1.clone()), "a file that already differed at startup did not reload on the first poll");
    assert!(Source::poll(1., &source).1.is_none(), "the startup difference was reported twice");
    let (_, same) = Source::poll(0., &Source::at(&file, &sha(&text1)));
    assert!(same.is_none(), "a file that matches the built text reloaded");
    let source = Source::at(&file, &sha(&text1));
    let (source, _) = Source::poll(0., &source);
    write(&file, &text0);
    let (source, early) = Source::poll(0.3, &source);
    assert!(early.is_none(), "polled again within half a second");
    let (source, late) = Source::poll(0.6, &source);
    assert!(late == Some(text0.clone()), "the change was not seen after half a second");
    let (source, again) = Source::poll(1.2, &source);
    assert!(again.is_none(), "the same change reported twice");
    let stamp = fs::metadata(&file).unwrap().modified().unwrap();
    fs::write(&file, &text1).unwrap();
    fs::OpenOptions::new().write(true).open(&file).unwrap().set_modified(stamp).unwrap();
    let (source, changed) = Source::poll(1.8, &source);
    assert!(changed == Some(text1.clone()), "a preserved-mtime content edit was missed");
    let replacement = format!("{dir}/replacement.rays");
    write(&replacement, &text0);
    fs::OpenOptions::new().write(true).open(&replacement).unwrap().set_modified(stamp).unwrap();
    fs::rename(&replacement, &file).unwrap(); // (the inode check is POSIX-only; the content differs)
    let (_, changed) = Source::poll(2.4, &source);
    assert!(changed == Some(text0.clone()), "a same-mtime replacement was missed");
    let _ = fs::remove_dir_all(&dir);
}

pub fn find() {
    let dir = temp_dir("src-find");
    let start = std::env::current_dir().unwrap();
    fs::write(format!("{dir}/Cargo.toml"), "[workspace]\n").unwrap();
    fs::create_dir(format!("{dir}/sk")).unwrap();
    fs::write(format!("{dir}/sk/Cargo.toml"), "[package]\nname = \"nested\"\n").unwrap();
    fs::create_dir(format!("{dir}/target")).unwrap();
    fs::write(format!("{dir}/target/Cargo.toml"), "[workspace]\n").unwrap();
    write(&format!("{dir}/sk/x.rays"), &sketch("0.5"));
    let expect = |from: &str| {
        std::env::set_current_dir(from).unwrap();
        let s = Source::find("sk/x.rays", "").unwrap_or_else(|| panic!("no source from {from}"));
        assert!(fs::canonicalize(Source::file(&s)).unwrap() == fs::canonicalize(format!("{dir}/sk/x.rays")).unwrap(), "the source file is joined to the project root");
    };
    expect(&format!("{dir}/sk"));
    expect(&format!("{dir}/target"));
    std::env::set_current_dir(&start).unwrap();
    assert!(Source::find("sk/nowhere.rays", "").is_none(), "a missing file is no source");
    let _ = fs::remove_dir_all(&dir);
}

// ---- through the editor ----

fn editor(presets: Option<&str>, imports: Vec<(String, String)>, source: Source::SourceFile, text: &str) -> editor::Editor {
    let o = editor::Create { ui_text_size: Some(11), platform: Some(input::Platform::Apple), await_: Some(true), domains: Some(1), presets: presets.map(|p| p.to_string()), ..Default::default() };
    editor::create(o, &doc_of(text, imports).unwrap(), Some(source), editor::prepare_fn(), editor::draw_fn()).unwrap_or_else(|m| panic!("{m}"))
}
fn source_text(e: &editor::Editor) -> String {
    flow_syntax::lisp::print(&editor::workspace(e).source).unwrap().0
}
fn dump_line(e: &editor::Editor, key: &str) -> String {
    let dir = temp_dir("dump");
    editor::crash_dump(e, &dir).unwrap();
    let text = read(&format!("{dir}/editor.txt"));
    let _ = fs::remove_dir_all(&dir);
    text.lines().find_map(|l| l.strip_prefix(&format!("{key}: ")).map(|s| s.to_string())).unwrap_or_else(|| panic!("no {key} in the dump"))
}
fn cook_line(e: &editor::Editor) -> String {
    dump_line(e, "cook")
}

/// frame of test_editor_input.frame at a fixed mouse
struct Run {
    d: Driver,
    mouse: (f64, f64),
}
impl Run {
    fn new(e: editor::Editor, mouse: (f64, f64)) -> Run {
        Run { d: Driver::new(e), mouse }
    }
    fn e(&self) -> &editor::Editor {
        self.d.e()
    }
    fn step(&mut self, events: Vec<Event>) {
        self.d.step_at(self.mouse, events)
    }
    fn keys(&mut self, keys: &[Key], events: Vec<Event>) {
        let m = self.mouse;
        self.d.bk(&[], keys, m, events)
    }
    // ponytail: one step per 30 frames (the clock jumps with the frame counter) instead of 60 idle frames a second: debug frames cost ~70 ms
    fn advance(&mut self, frames: i64) {
        let mut left = frames;
        while left > 0 {
            let n = left.min(30);
            self.d.count += n - 1;
            self.step(vec![]);
            left -= n;
        }
    }
    fn run_for(&mut self, seconds: f64) {
        self.advance((seconds * 60.) as i64)
    }
    fn replace(&mut self, e: editor::Editor) {
        self.d.ed = Some(e);
    }
    fn edit(&mut self, op: fe::Op) {
        let e = self.d.ed.take().unwrap();
        self.d.ed = Some(editor::edit(e, &op).unwrap_or_else(|m| panic!("{m}")));
    }
}
fn set_arg(path: &[&str], kw: &str, value: S::Draft) -> fe::Op {
    fe::Op::SetArg { node: path.iter().map(|s| s.to_string()).collect(), key: fe::ArgKey::Kw(kw.into()), sub: vec![], value: S::make(value) }
}
fn meta(c: char) -> Event {
    Event::Key_pressed(Key::KeyChar(c))
}

pub fn editor_flow() {
    if !gpu() {
        return;
    }
    let dir = temp_dir("src-editor");
    let file = format!("{dir}/sketch.rays");
    let presets = format!("{dir}/presets");
    let (text0, text1) = (sketch("0.5"), sketch("0.7"));
    write(&file, &text0);
    let mut r = Run::new(editor(Some(&presets), vec![], Source::at(&file, &sha(&text0)), &text0), (450., 300.));
    r.step(vec![]);
    r.step(vec![]);
    r.step(vec![]);
    let history = editor::undo_label(r.e());
    // Command-S over the matching source
    r.keys(&[Key::Meta], vec![meta('s')]);
    r.step(vec![]);
    assert!(has(&cook_line(r.e()), "Saved sketch.rays"), "the status says it saved: {}", cook_line(r.e()));
    let written = read(&file);
    assert!(written == printed(&editor::workspace(r.e())), "Save wrote the document's text");
    assert!(has(&written, "; the header") && has(&written, "; the tail"), "the saved file keeps its comments");
    assert!(!std::path::Path::new(&presets).exists(), "no preset was written");
    r.run_for(1.5);
    assert!(editor::undo_label(r.e()) == history, "Save reloaded its own write");
    // an edit from another editor reloads as one history entry; probes stay by path
    let zone = vec!["g".to_string(), "zone".to_string()];
    let e = r.d.ed.take().unwrap();
    r.replace(editor::set_probe(e, &zone, 3));
    write(&file, &text1);
    r.run_for(1.);
    r.step(vec![]);
    assert!(has(&source_text(r.e()), "0.7"), "the file's text is the document: {}", source_text(r.e()));
    assert!(editor::undo_label(r.e()) == Some("Reload sketch.rays".to_string()), "one history entry named Reload sketch.rays");
    assert!(editor::probe(r.e(), &zone) == Some(3), "the probe survived by path");
    assert!(has(&cook_line(r.e()), "Reloaded sketch.rays") || has(&cook_line(r.e()), "checked"), "{}", cook_line(r.e()));
    // text that does not check keeps the last good document and says why
    write(&file, TYPO);
    r.run_for(1.);
    r.step(vec![]);
    assert!(has(&source_text(r.e()), "0.7"), "the refused text replaced the document");
    assert!(editor::undo_label(r.e()) == Some("Reload sketch.rays".to_string()), "the refused text made a history entry");
    assert!(has(&cook_line(r.e()), "not reloaded") && has(&cook_line(r.e()), "line 1"), "the status names the failure: {}", cook_line(r.e()));
    write(&file, &text1);
    r.run_for(1.);
    r.step(vec![]);
    assert!(has(&cook_line(r.e()), "Reloaded"), "the recovered file reloads: {}", cook_line(r.e()));
    // a (layout ...) form edited into the file is the new layout; a file without one keeps it
    write(&file, &format!("{text1}(layout (node [\"g\" \"@result\"] :at [10 20]) (frame [\"g\"] \"Sphere\" :at [0 0] :size [200 100]))"));
    r.run_for(1.);
    r.step(vec![]);
    let key = vec!["g".to_string(), "@result".to_string()];
    let layout = |r: &Run| editor::workspace(r.e()).layout;
    assert!(layout(&r).at.contains_key(&key) && !layout(&r).frames.is_empty(), "the layout form of the file was ignored");
    write(&file, &text1);
    r.run_for(1.);
    r.step(vec![]);
    assert!(layout(&r).at.contains_key(&key), "a reload without a layout form dropped the layout");
    let before = editor::workspace(r.e());
    let label = editor::undo_label(r.e());
    fs::remove_file(&file).unwrap();
    r.run_for(1.);
    assert!(Arc::ptr_eq(&editor::workspace(r.e()).checked, &before.checked) && editor::undo_label(r.e()) == label, "unreadable source changed the document/history");
    assert!(has(&cook_line(r.e()), "Source unreadable"), "source read failure was silent: {}", cook_line(r.e()));
    r.keys(&[Key::Meta], vec![meta('s')]);
    assert!(has(&cook_line(r.e()), "Not saved:"), "unreadable source was reported as an external edit");
    assert!(!std::path::Path::new(&file).exists(), "Save recreated an unreadable source");
    write(&file, &text1);
    r.run_for(1.);
    assert!(has(&cook_line(r.e()), "Source readable again"), "source recovery was not reported: {}", cook_line(r.e()));
    assert!(Arc::ptr_eq(&editor::workspace(r.e()).checked, &before.checked) && editor::undo_label(r.e()) == label, "same-content source recovery made a document change");
    r.d.close();
    // a file edited since the build reloads on the first poll, as one history entry
    write(&file, &text1);
    r = Run::new(editor(Some(&presets), vec![], Source::at(&file, &sha(&text0)), &text0), (450., 300.));
    r.step(vec![]);
    r.step(vec![]);
    r.run_for(1.);
    r.step(vec![]);
    assert!(has(&source_text(r.e()), "0.7"), "the edited file was not loaded at startup: {}", source_text(r.e()));
    assert!(editor::undo_label(r.e()) == Some("Reload sketch.rays".to_string()), "the startup reload is one history entry");
    r.d.close();
    // a file that differs and does not check keeps the built text; Save then falls back to a preset
    write(&file, TYPO);
    r = Run::new(editor(Some(&presets), vec![], Source::at(&file, &sha(&text0)), &text0), (450., 300.));
    r.step(vec![]);
    r.step(vec![]);
    r.run_for(1.);
    r.step(vec![]);
    assert!(has(&cook_line(r.e()), "not reloaded"), "the refused startup file says so: {}", cook_line(r.e()));
    r.keys(&[Key::Meta], vec![meta('s')]);
    r.step(vec![]);
    assert!(has(&cook_line(r.e()), "saved as preset"), "the status says why: {}", cook_line(r.e()));
    assert!(read(&file) == TYPO, "the changed source file was not overwritten");
    assert!(editor_document::preset::list(&presets).len() == 1, "one preset was written");
    r.d.close();
    let _ = fs::remove_dir_all(&dir);
}

/// X5/E8: an external change never replaces unsaved work.
pub fn external_over_dirty() {
    if !gpu() {
        return;
    }
    let dir = temp_dir("src-dirty");
    let file = format!("{dir}/sketch.rays");
    let (text0, text1) = (sketch("0.5"), sketch("0.7"));
    write(&file, &text0);
    let mut r = Run::new(editor(Some(&format!("{dir}/presets")), vec![], Source::at(&file, &sha(&text0)), &text0), (450., 300.));
    r.step(vec![]);
    r.step(vec![]);
    r.edit(set_arg(&["g", "@result"], "segments", S::Draft::Num("12".into())));
    let label = editor::undo_label(r.e());
    write(&file, &text1);
    r.run_for(1.5);
    assert!(has(&source_text(r.e()), ":segments 12") && has(&source_text(r.e()), "0.5") && editor::undo_label(r.e()) == label, "an external change replaced a document with unsaved edits: {}", source_text(r.e()));
    assert!(has(&cook_line(r.e()), "changed on disk"), "the kept edits were not announced: {}", cook_line(r.e()));
    r.keys(&[Key::Meta], vec![meta('z')]);
    r.run_for(1.);
    assert!(has(&source_text(r.e()), "0.7") && editor::undo_label(r.e()) == Some("Reload sketch.rays".to_string()), "the file's text did not load once the edits were undone: {}", source_text(r.e()));
    r.d.close();
    let _ = fs::remove_dir_all(&dir);
}

pub fn autosave() {
    if !gpu() {
        return;
    }
    let dir = temp_dir("src-autosave");
    let file = format!("{dir}/sketch.rays");
    let presets = format!("{dir}/presets");
    let (text0, _) = (sketch("0.5"), 0);
    write(&file, &text0);
    let create = || editor(Some(&presets), vec![], Source::at(&file, &sha(&text0)), &text0);
    let mut r = Run::new(create(), (200., 300.));
    r.step(vec![]);
    r.step(vec![]);
    let real = fs::canonicalize(&file).unwrap().to_string_lossy().into_owned();
    let real = real.strip_prefix("\\\\?\\").unwrap_or(&real).to_string();
    let state = editor_document::preset::path(&format!("{presets}/state"), &sha(&format!("file:{real}")));
    assert!(!std::path::Path::new(&state).exists(), "opening a sketch wrote over its recovery state");
    r.edit(set_arg(&["g", "@result"], "radius", S::Draft::Num("1.25".into())));
    let e = r.d.ed.take().unwrap();
    r.replace(editor::set_renderer(e, rays_editor::Renderer::Mode::Wireframe));
    r.step(vec![Event::Pointer_moved((200., 300.)), Event::Scrolled((0., -2.))]);
    let eye = |e: &editor::Editor| rays::camera::position(&rays::easy_camera::camera(&editor::camera(e)).unwrap());
    let saved_eye = eye(r.e());
    r.d.close();
    assert!(has(&read(&state), ":radius 1.25") && has(&read(&state), "; the header") && has(&read(&state), "(view"), "close did not flush the edited document and viewport");
    assert!(read(&file) == text0, "autosave rewrote the source file");
    assert!(fs::read_dir(std::path::Path::new(&state).parent().unwrap()).unwrap().count() == 1, "autosave left multiple snapshots or temporary files");
    let snapshot = read(&state);
    let stamp = fs::metadata(&state).unwrap().modified().unwrap();
    r = Run::new(create(), (200., 300.));
    r.advance(70);
    assert!(read(&state) == snapshot && fs::metadata(&state).unwrap().modified().unwrap() == stamp, "an unchanged restarted sketch overwrote the last edited state");
    // E7: orbiting in the pristine document does not write over the earlier session's unsaved work
    r.step(vec![Event::Pointer_moved((200., 300.)), Event::Scrolled((0., -2.))]);
    r.advance(70);
    assert!(read(&state) == snapshot, "orbiting after a restart overwrote the last edited state");
    r.step(vec![meta_k('/'), meta_k('b')]);
    r.step(vec![Event::Key_pressed(Key::Enter)]);
    assert!(has(&source_text(r.e()), ":radius 1.25") && editor::undo_label(r.e()) == Some("Restore last edited state".to_string()), "/ b did not restore the last edited state as one undo entry: {:?}; {}; {}", editor::undo_label(r.e()), cook_line(r.e()), source_text(r.e()));
    assert!(rays_math::vec3::nearly_equal(saved_eye, eye(r.e()), 1e-9), "recovery did not restore the viewport");
    assert!(editor::renderer(r.e()) == rays_editor::Renderer::Mode::Wireframe, "recovery did not restore the shared renderer choice");
    r.keys(&[Key::Meta], vec![meta('z')]);
    assert!(has(&source_text(r.e()), ":radius 0.5"), "undo did not revert recovery");
    r.d.close();
    // A corrupt recovery file is reported without replacing the good document or the file.
    write(&state, TYPO);
    r = Run::new(create(), (200., 300.));
    r.step(vec![]);
    r.step(vec![meta_k('/'), meta_k('b')]);
    r.step(vec![Event::Key_pressed(Key::Enter)]);
    assert!(has(&source_text(r.e()), ":radius 0.5") && has(&cook_line(r.e()), "rejected"), "a broken autosave replaced the live document or hid its error");
    r.step(vec![meta_k('/'), meta_k('b')]);
    r.step(vec![Event::Key_pressed(Key::Delete)]);
    r.step(vec![Event::Key_pressed(Key::Delete)]);
    assert!(!std::path::Path::new(&state).exists(), "Delete twice did not remove the recovery file");
    r.d.close();
    assert!(!std::path::Path::new(&state).exists(), "closing an unchanged sketch recreated deleted recovery");
    // Failed writes keep the edit and retry after the destination becomes usable.
    let state_dir = std::path::Path::new(&state).parent().unwrap().to_string_lossy().into_owned();
    fs::remove_dir(&state_dir).unwrap();
    write(&state_dir, "blocked");
    r = Run::new(create(), (200., 300.));
    r.step(vec![]);
    r.edit(set_arg(&["g", "@result"], "radius", S::Draft::Num("2".into())));
    r.advance(35);
    assert!(has(&dump_line(r.e(), "autosave"), "Autosave failed") && has(&source_text(r.e()), ":radius 2"), "a failed periodic save lost the edit or hid its error");
    fs::remove_file(&state_dir).unwrap();
    r.advance(35);
    assert!(has(&read(&state), ":radius 2") && dump_line(r.e(), "autosave") == "-", "autosave did not retry and clear its error while the editor remained open");
    r.d.close();
    let _ = fs::remove_dir_all(&dir);
}
fn meta_k(c: char) -> Event {
    Event::Key_pressed(Key::KeyChar(c))
}

/// The start keywords (flow.md 11.11) through the two routes that replace the document from a file.
pub fn start_keywords() {
    if !gpu() {
        return;
    }
    let dir = temp_dir("src-keywords");
    let file = format!("{dir}/sketch.rays");
    let presets = format!("{dir}/presets");
    let text = |radius: &str, view: &str, tab: &str| {
        format!("(workspace w\n  (graph g :context sop []\n    (sop/uv_sphere :radius {radius} :segments 8 :rings 4))\n  (graph scene :context scene (scene/merge (scene/geometry (ref g))))\n  (graph editor :context editor\n    (let* [net (ui/graph \"g\" :view \"{view}\" :focus true)\n           code (ui/lisp :tab \"{tab}\")]\n      (ui/workspace (ui/split \"horizontal\" net code)))))\n")
    };
    let first = text("0.5", "list", "graph");
    write(&file, &first);
    let mut r = Run::new(editor(Some(&presets), vec![], Source::at(&file, &sha(&first)), &first), (100., 300.));
    let line = |r: &Run, k: &str| dump_line(r.e(), k);
    fn show_graph(r: &mut Run) {
        r.step(vec![meta_k('/'), meta_k('l')]);
        r.step(vec![meta_k('g')]);
        r.step(vec![]);
    }
    r.step(vec![]);
    r.step(vec![]);
    assert!(line(&r, "projection") == "list" && has(&line(&r, "panels"), "lisp code: graph tab"), "the sketch opens as its keywords say: {} / {}", line(&r, "projection"), line(&r, "panels"));
    // a reload that leaves the keywords alone leaves the view the user chose
    show_graph(&mut r);
    assert!(line(&r, "projection") == "graph", "/ l g did not show the graph");
    write(&file, &text("0.7", "list", "graph"));
    r.run_for(1.);
    r.step(vec![]);
    assert!(editor::undo_label(r.e()) == Some("Reload sketch.rays".to_string()) && has(&source_text(r.e()), "0.7"), "the file did not reload");
    assert!(line(&r, "projection") == "graph", "a reload that left :view alone reset the view the user chose");
    // a reload that changes them is followed
    write(&file, &text("0.7", "text", "document"));
    r.run_for(1.);
    r.step(vec![]);
    assert!(line(&r, "projection") == "text" && has(&line(&r, "panels"), "lisp code: document tab"), "a reload did not follow the changed keywords: {} / {}", line(&r, "projection"), line(&r, "panels"));
    // a preset: saved with :view "text", loaded over a document that says "list"
    r.step(vec![meta_k('/'), meta_k('s')]);
    r.step(vec![Event::Key_pressed(Key::Enter)]);
    r.step(vec![]);
    let name = match editor_document::preset::list(&presets).as_slice() {
        [(name, _)] => name.clone(),
        _ => panic!("/ s did not save one preset"),
    };
    r.edit(set_arg(&["editor", "net"], "view", S::Draft::Str("list".into())));
    r.step(vec![]);
    r.step(vec![]);
    assert!(line(&r, "projection") == "list", "an edit of :view was not followed");
    let load = |r: &mut Run| {
        r.step(vec![meta_k('/'), meta_k('b')]);
        r.step(vec![Event::Text_input(name.clone()), Event::Key_pressed(Key::Enter)]);
        r.step(vec![]);
        r.step(vec![]);
    };
    load(&mut r);
    assert!(editor::undo_label(r.e()) == Some("Load preset".to_string()) && line(&r, "projection") == "text", "a preset load did not follow its :view: {}", line(&r, "projection"));
    // the same preset again changes no keyword: the user's view stays
    show_graph(&mut r);
    load(&mut r);
    assert!(editor::undo_label(r.e()) == Some("Load preset".to_string()) && line(&r, "projection") == "graph", "a preset load that left :view alone reset the view the user chose");
    r.d.close();
    let _ = fs::remove_dir_all(&dir);
}

pub fn imports() {
    if !gpu() {
        return;
    }
    let dir = temp_dir("src-imports");
    let (file, lib) = (format!("{dir}/sketch.rays"), format!("{dir}/lib.rays"));
    let library = |radius: &str| format!("(workspace library (graph shared :context sop (sop/uv_sphere :radius {radius} :segments 8 :rings 4)))\n");
    let text = "(workspace main (graph local :context sop (ref shared)))\n(import \"lib.rays\")\n";
    write(&file, text);
    write(&lib, &library("0.5"));
    let mut r = Run::new(editor(None, vec![("lib.rays".into(), library("0.5"))], Source::at(&file, &sha(text)), text), (450., 300.));
    let poll = |r: &mut Run| {
        r.advance(65);
    };
    poll(&mut r);
    assert!(editor::undo_label(r.e()).is_none(), "unchanged imported text reloaded at startup");
    write(&lib, &library("0.7"));
    poll(&mut r);
    assert!(has(&source_text(r.e()), "0.7") && editor::undo_label(r.e()) == Some("Reload sketch.rays".to_string()), "changed import did not reload");
    let document = editor::workspace(r.e());
    poll(&mut r);
    assert!(Arc::ptr_eq(&editor::workspace(r.e()).checked, &document.checked), "import reloaded repeatedly (normalized digest)");
    let unchanged = read(&lib);
    r.keys(&[Key::Meta], vec![meta('s')]);
    assert!(read(&lib) == unchanged && !has(&read(&file), "(graph shared"), "save wrote imported forms or file");
    r.edit(fe::Op::SetNote { node: vec!["local".into(), "@result".into()], text: "local edit".into() });
    write(&lib, &library("0.9"));
    poll(&mut r);
    assert!(has(&source_text(r.e()), "0.7") && has(&cook_line(r.e()), "changed on disk"), "import replaced unsaved work");
    r.keys(&[Key::Meta], vec![meta('z')]);
    poll(&mut r);
    assert!(has(&source_text(r.e()), "0.9"), "held imported change did not load after undo");
    fs::remove_file(&lib).unwrap();
    poll(&mut r);
    assert!(has(&cook_line(r.e()), "E_IMPORT") || has(&cook_line(r.e()), "not reloaded"), "missing import was silent");
    write(&lib, &library("0.9"));
    poll(&mut r);
    assert!(!Arc::ptr_eq(&editor::workspace(r.e()).checked, &document.checked), "recreated import did not recover");
    r.d.close();
    let _ = fs::remove_dir_all(&dir);
}

pub fn library() {
    if !gpu() {
        return;
    }
    let dir = temp_dir("src-library");
    let (file, lib) = (format!("{dir}/sketch.rays"), format!("{dir}/lib.rays"));
    let library = |n: &str| format!("; editable library\n(defn twice :context value [(x : float)] (* x {n}))\n");
    let text = "(workspace main (graph local :context value (twice 3)))\n(import \"lib.rays\")\n";
    write(&file, text);
    write(&lib, &library("2"));
    let e = editor(None, vec![("lib.rays".into(), library("2"))], Source::at(&file, &sha(text)), text);
    let mut e = editor::update(rays_editor::reduce::open_import(e, "lib.rays"), &frame(&Opts::default(), (450., 300.), vec![], 0));
    assert!(editor::workspace(&e).fragment && editor::workspace(&e).imports.is_empty(), "open library did not switch to editable definitions");
    e = editor::edit(e, &fe::Op::SetArg { node: vec!["def:twice".into(), "@result".into()], key: fe::ArgKey::Pos(1), sub: vec![], value: S::make(S::Draft::Num("4".into())) }).unwrap();
    e = editor::update(e, &frame(&Opts { keys: vec![Key::Meta], ..Default::default() }, (450., 300.), vec![meta('s')], 1));
    assert!(has(&read(&lib), "(* x 4)") && has(&read(&lib), "editable library") && !has(&read(&lib), "workspace"), "library save added a wrapper or lost edit/comment");
    assert!(read(&file) == text, "open library saved over importer");
    write(&lib, &library("5"));
    for count in 2..=70 {
        e = editor::update(e, &frame(&Opts::default(), (450., 300.), vec![], count));
    }
    assert!(has(&Doc::to_text(&editor::workspace(&e)).unwrap(), "(* x 5)"), "bare definition library did not reload");
    editor::close(&e);
    let _ = fs::remove_dir_all(&dir);
}

pub fn embedded_nodes() {
    let text = ";; embedded node call\n(workspace w (graph g :context sop\n  (let* [source (sop/line)\n         shifted (sop/noise_displace source :amplitude 0.5)] shifted)))\n";
    let contains_node = |d: &Doc::T| d.checked.nodes.iter().any(|n| n.kind.qualified == "sop/noise_displace");
    let doc = doc_of(text, vec![]).unwrap();
    assert!(contains_node(&doc), "embedded definition absent at load");
    let doc = Doc::edit(&catalog(), &doc, &set_arg(&["g", "shifted"], "amplitude", S::Draft::Num("0.75".into()))).unwrap();
    assert!(doc.literal.is_some(), "embedded scalar scrub missed literal edit path");
    assert!(contains_node(&doc), "scalar scrub lost embedded definition");
    assert!(!has(&printed(&doc), "(defnode"), "scalar scrub copied library forms into saved source");
    let doc = Doc::edit(&catalog(), &doc, &fe::Op::SetNote { node: vec!["g".into(), "shifted".into()], text: "edited node".into() }).unwrap();
    assert!(doc.literal.is_none() && contains_node(&doc), "structural gesture lost embedded definition");
    let saved = printed(&doc);
    assert!(has(&saved, ":amplitude 0.75") && has(&saved, "; edited node"), "edits absent from saved source");
    assert!(!has(&saved, "(defnode") && !has(&saved, "sop/context_seed"), "saving authored calls copied embedded library into document");
    let dir = temp_dir("src-embedded");
    let file = format!("{dir}/sketch.rays");
    write(&file, text);
    Source::save(&Source::at(&file, &sha(text)), &saved).unwrap_or_else(|_| panic!("save"));
    let reopened = doc_of(&read(&file), vec![]).unwrap();
    assert!(contains_node(&reopened) && printed(&reopened) == saved, "saved embedded call did not reopen with identical authored source");
    let _ = fs::remove_dir_all(&dir);
}

pub fn library_navigation() {
    use rays_editor::navigator as N;
    let parse = |t: &str| flow_syntax::syntax::parse(t).unwrap();
    let sources = parse("(defn embedded_helper :context value [(x : float)] (+ x 1))\n    (defmacro embedded_macro [x] `(+ ~x 1))");
    let authored = parse("(workspace w\n    (defn local_helper :context value [(x : float)] (* x 2))\n    (defmacro local_macro [x] `(* ~x 2))\n    (graph g :context value (embedded_helper (embedded_macro (local_helper 1)))))");
    let cat = editor_document::kinds::catalog(flow_sop::manifest::VERSION, &sop::nodes::factories()).unwrap();
    let workspace = match flow_sop::catalog::check_workspace(flow_sop::operators::all(), false, Some(&sources), &cat, &authored) {
        (Some(w), d) if d.is_empty() => w,
        (_, d) => panic!("{d:?}"),
    };
    assert!(workspace.defs.len() == 2 && workspace.macros.len() == 2, "library helper fixture lost checked definitions");
    let params = N::Params { workspace: Arc::new(workspace), active: None, scope: None, records: None, probes: std::rc::Rc::new(|_| 0), selected: vec![], chips: vec![], objects: vec![], root_detail: String::new(), layouts: None, notes: vec![] };
    let descriptions = |state: &N::State| N::rows(false, state, &params).iter().map(N::describe).collect::<Vec<_>>().join("\n");
    let rows = descriptions(&N::initial());
    assert!(has(&rows, "local_helper") && has(&rows, "local_macro"), "authored definitions missing from navigation");
    assert!(!has(&rows, "embedded_helper") && !has(&rows, "embedded_macro"), "embedded helpers exposed as editable navigation rows");
    assert!(!has(&descriptions(&N::with_query("embedded", &N::initial())), "embedded_helper"), "search exposed an embedded helper as an editable definition");
}

pub fn vocabulary_refresh() {
    if !gpu() {
        return;
    }
    let text = |field: Option<&str>| match field {
        None => "(workspace w (graph g :context sop (sop/line)))\n".to_string(),
        Some(f) => format!("(workspace w\n      (defnode sop/vocab_card :label \"Vocabulary card\" :category [\"Test\"]\n        [(geo : geometry) ({f} : float 0.5)] geo)\n      (graph g :context sop (sop/vocab_card (sop/line) :{f} 0.5)))\n"),
    };
    let dir = temp_dir("src-vocab");
    let file = format!("{dir}/sketch.rays");
    write(&file, &text(None));
    let mut r = Run::new(editor(None, vec![], Source::at(&file, &sha(&text(None))), &text(None)), (450., 300.));
    let poll = |r: &mut Run| {
        r.advance(65);
    };
    let labels = |r: &Run, suffix: &str| -> Vec<String> { editor::private::lisp_completions(r.e(), &format!("(workspace w (graph g :context sop ({suffix}")).into_iter().map(|c| c.label).collect() };
    let head = |r: &Run| labels(r, "sop/vocab_").iter().any(|l| l == "sop/vocab_card");
    assert!(!head(&r), "new vocabulary kind existed before declaration");
    write(&file, &text(Some("distance")));
    poll(&mut r);
    assert!(head(&r) && labels(&r, "sop/vocab_card geo :dis").iter().any(|l| l == ":distance"), "reload did not add local kind and parameter completions");
    let step = |r: &mut Run, a: rays_editor::editor_commands::action| {
        r.d.count += 1;
        let f = frame(&Opts::default(), (450., 300.), vec![], r.d.count);
        let e = r.d.ed.take().unwrap();
        r.d.ed = Some(rays_editor::reduce::step(e, None, None, &[a], &f).unwrap());
    };
    step(&mut r, rays_editor::editor_commands::action::Undo);
    assert!(!head(&r), "undo retained removed declaration completion");
    step(&mut r, rays_editor::editor_commands::action::Redo);
    assert!(head(&r), "redo did not restore declaration completion");
    write(&file, &text(Some("amount")));
    poll(&mut r);
    assert!(labels(&r, "sop/vocab_card geo :am").iter().any(|l| l == ":amount") && !labels(&r, "sop/vocab_card geo :dis").iter().any(|l| l == ":distance"), "schema edit kept old keyword vocabulary");
    r.edit(set_arg(&["g", "@result"], "amount", S::Draft::Num("0.75".into())));
    assert!(head(&r) && labels(&r, "sop/vocab_card geo :am").iter().any(|l| l == ":amount"), "scalar scrub lost declaration vocabulary");
    step(&mut r, rays_editor::editor_commands::action::Undo);
    write(&file, &text(None));
    poll(&mut r);
    assert!(!head(&r), "reload retained removed local kind completion");
    r.d.close();
    let _ = fs::remove_dir_all(&dir);
}
