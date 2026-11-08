//! test_host_effects.ml, test_threading.ml, test_workspace_start.ml (errors only: start/reload run
//! a real window through Workspace::main, which has no headless route; see SKIPPED).
use super::t4::*;
use flow_syntax::{lisp, syntax};
use input::{Event, Key};
use rays_editor::{editor, workspace};

fn effects_workspace(effects: &str) -> editor_document::workspace_doc::T {
    let text = format!("(workspace host (graph picture :context draw (draw/background \"#111827\")) (graph actions :context host {effects}) (graph editor :context editor (ui/workspace (ui/canvas (ref picture)) :effects (ref actions))))");
    workspace::load(None, vec![], None, &text).unwrap_or_else(|d| panic!("{d:?}"))
}

fn eframe(keys: Vec<Key>, events: Vec<Event>, count: i64) -> rays::frame::Frame {
    let mut f = frame(&Opts { keys, ..Default::default() }, (-100., -100.), events, count);
    f.input.size = (640, 360);
    f
}

fn with_editor<R>(ws: &editor_document::workspace_doc::T, run: impl FnOnce(&mut editor::Editor) -> R) -> (R, editor::Editor) {
    let o = editor::Create { ui_text_size: Some(11), platform: Some(input::Platform::Apple), await_: Some(true), domains: Some(1), ..Default::default() };
    let mut ed = editor::create(o, ws, None, editor::prepare_fn(), editor::draw_fn()).unwrap();
    let r = run(&mut ed);
    editor::close(&ed);
    (r, ed)
}

pub fn host_effects_edge() {
    if !crate::common::gpu() {
        return;
    }
    let doc = effects_workspace("(list (host/quit (key/down \"q\")))");
    with_editor(&doc, |editor| {
        let up = |ed: &mut editor::Editor, f: rays::frame::Frame| *ed = editor::update(ed.clone(), &f);
        up(editor, eframe(vec![], vec![], 0));
        assert_eq!(editor::private::host_stats(editor), (false, 0, 0, 0));
        for count in 1..=2 {
            up(editor, eframe(vec![Key::KeyChar('q')], vec![Event::Key_pressed(Key::KeyChar('q'))], count));
        }
        assert_eq!(editor::private::host_stats(editor), (true, 1, 0, 0));
        up(editor, eframe(vec![], vec![], 3));
        up(editor, eframe(vec![Key::KeyChar('q')], vec![], 4));
        assert_eq!(editor::private::host_stats(editor), (true, 2, 0, 0));
    });
    host_effects_samples();
}

pub fn host_effects_export() {
    if !crate::common::gpu() {
        return;
    }
    for effects in ["(list (host/quit true))", "(list (host/dialog \"open_file\" true))"] {
        let dir = temp_dir("effects-refused");
        match workspace::export(None, None, &dir, 1, &effects_workspace(effects)) {
            Err(d) => assert_eq!(d.code, "E_EFFECT_EXPORT"),
            Ok(()) => panic!("export accepted {effects}"),
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
}

fn host_effects_samples() {
    let doc = effects_workspace("(list (host/play (audio/synth :frequency 440.0 :duration 0.01) (key/down \"a\")) (host/play (audio/synth :frequency 440.0 :duration 0.01) (key/down \"a\")))");
    let (_, closed) = with_editor(&doc, |editor| {
        *editor = editor::update(editor.clone(), &eframe(vec![Key::KeyChar('a')], vec![], 1));
        assert_eq!(editor::private::host_stats(editor), (false, 2, 1, 0));
        *editor = editor::update(editor.clone(), &eframe(vec![Key::KeyChar('a')], vec![], 2));
        assert_eq!(editor::private::host_stats(editor), (false, 2, 1, 0));
    });
    assert_eq!(editor::private::host_stats(&closed), (false, 2, 1, 1));
}

fn files(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
    let mut entries: Vec<_> = std::fs::read_dir(dir).unwrap().map(|e| e.unwrap().path()).collect();
    entries.sort();
    for p in entries {
        if p.is_dir() {
            files(&p, out);
        } else if matches!(p.extension().and_then(|e| e.to_str()), Some("rays" | "lisp")) {
            out.push(p);
        }
    }
}

/// Printing a workspace threads chains with [->]; reading the print gives back the same forms.
pub fn threading() {
    let root = format!("{}/../../", env!("CARGO_MANIFEST_DIR"));
    let mut paths = Vec::new();
    files(std::path::Path::new(&format!("{root}sketches")), &mut paths);
    files(std::path::Path::new(&format!("{root}specification/workspace/cases")), &mut paths);
    let mut threaded = 0;
    for path in &paths {
        let text = std::fs::read(path).map(|b| String::from_utf8_lossy(&b).into_owned()).unwrap();
        let forms = syntax::parse(&text).unwrap_or_else(|d| panic!("{}: {}", path.display(), rays_diagnostic::to_string(&d)));
        let printed = lisp::print(&forms).unwrap().0;
        let again = syntax::parse(&printed).unwrap_or_else(|d| panic!("{} reprint: {}", path.display(), rays_diagnostic::to_string(&d)));
        let flat = |f: &[syntax::Syntax]| f.iter().map(lisp::flat).collect::<Vec<_>>();
        assert!(flat(&forms) == flat(&again), "{}: the print reads differently", path.display());
        assert!(lisp::print(&again).unwrap().0 == printed, "{}: the print is not stable", path.display());
        threaded += printed.matches("(->").count();
    }
    assert!(threaded > 0, "no checked-in file threads a chain (shattered_studio shows it)");
}

pub fn workspace_start_errors() {
    let path = format!("{}/rays-missing-workspace.rays", temp_dir("start-errors"));
    let refuses = |code: &str, r: Result<(), Vec<rays_diagnostic::Diagnostic>>| match r {
        Err(ds) => assert!(ds.iter().any(|d| d.code == code), "workspace start did not return {code}"),
        Ok(()) => panic!("workspace start accepted invalid input"),
    };
    refuses("E_IO", workspace::main(None, None, &path));
    refuses("E_UNCLOSED", workspace::main(Some("(workspace".into()), None, &path));
}
