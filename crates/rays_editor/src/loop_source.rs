//! The source file of the loop: polled, imported, saved over, and the autosave of the recovery
//! file. //!
//! Source operations mutate the stored model and the frame's change metadata.

use std::path::Path;
use std::sync::Arc;
use std::rc::Rc;

use editor_core::store;
use editor_document::lowering;
use editor_document::preset;
use editor_document::workspace_doc;
use input::Event;
use rays::sketch;

use crate::editor_commands::Action;
use crate::editor_host;
use crate::editor_model::{self as m, NoticeKind, Update};
use crate::editor_setup;
use crate::editor_text;
use crate::loop_model::LoopModel;
use crate::source_file;
use crate::viewport as V;

fn notice(value: &mut LoopModel, kind: NoticeKind, text: String) {
    Rc::make_mut(&mut value.core).notice = Some((kind, text));
}

fn basename(file: &str) -> String {
    Path::new(file).file_name().map(|f| f.to_string_lossy().into_owned()).unwrap_or_default()
}

/// The source file, at most every half second: a changed text reloads the document. Unsaved work is
/// never replaced: while the document differs from the file's last text or a text pane holds a
/// draft, the file's new text waits (`held`) and the strip says how to choose. It loads once the
/// edits are undone, or at once by "Reload sketch from its file" (the palette). Nothing is polled
/// while a payload is carried: the document shown is a preview then.
pub fn open_import(value: &mut LoopModel, update_: &mut Update) {
    let (path, source) = match (&value.core.open_import, &value.source) {
        (Some(path), Some(source)) => (path.clone(), source.clone()),
        _ => return,
    };
    Rc::make_mut(&mut value.core).open_import = None;
    if editor_text::unsaved(None, &value.core) {
        notice(value, NoticeKind::Refusal, format!("Save or undo your changes before opening {path}."));
        return;
    }
    let file = if Path::new(&path).is_relative() {
        Path::new(source_file::file(&source)).parent().map(|d| d.join(&path).to_string_lossy().into_owned()).unwrap_or_else(|| path.clone())
    } else {
        path.clone()
    };
    match store::read_text(&file) {
        Err(message) => {
            notice(value, NoticeKind::Refusal, message);
        }
        Ok(original) => match editor_host::reload(Some(&[]), Rc::make_mut(&mut value.core), &basename(&file), &original) {
            Err(ds) => {
                editor_host::reload_failed(Rc::make_mut(&mut value.core), &path, &original, &ds);
            }
            Ok(()) => {
                Rc::make_mut(&mut value.core).file = basename(&file);
                value.source = Some(source_file::at(&file, &lowering::sha256(&original)));
                value.held = None;
                value.refused = None;
                update_.scene_changed = true;
            }
        },
    }
}

/// Command-O: the system dialog over the sketch's folder; its answer, a frame later, is opened the
/// way an import is (unsaved work refuses it).
pub fn open_dialog(value: &mut LoopModel, update_: &mut Update, events: &[Event]) {
    if update_.actions.contains(&Action::OpenSource) {
        match &value.source {
            None => notice(value, NoticeKind::Refusal, "This editor was not opened from a .rays file.".to_string()),
            Some(source) => {
                let dir = Path::new(source_file::file(source)).parent().map(|d| d.to_string_lossy().into_owned()).unwrap_or_default();
                match sketch::show_file_dialog(Some(&[("Rays sketch".to_string(), vec!["rays".to_string()])]), Some(&dir), sketch::Dialog::OpenFile) {
                    Ok(id) => value.open_dialog = Some(id),
                    Err(message) => notice(value, NoticeKind::Refusal, format!("Open: {message}")),
                }
            }
        }
    }
    for event in events {
        if let Event::FileDialog { id, result } = event {
            if value.open_dialog == Some(*id) {
                value.open_dialog = None;
                match result {
                    Ok(paths) => {
                        if let Some(path) = paths.first() {
                            Rc::make_mut(&mut value.core).open_import = Some(path.clone());
                        }
                    }
                    Err(message) => notice(value, NoticeKind::Refusal, format!("Open: {message}")),
                }
            }
        }
    }
}

pub fn reload_source(value: &mut LoopModel, update_: &mut Update, now: f64) {
    let previous = match &value.source {
        Some(previous) if !editor_host::carrying(&value.core) => previous.clone(),
        _ => return,
    };
    let (file, read) = source_file::poll(now, &previous);
    let name = basename(source_file::file(&file));
    if file.error != previous.error {
        Rc::make_mut(&mut value.core).notice = Some(match &file.error {
            Some(message) => (NoticeKind::Refusal, format!("Source unreadable: {message}")),
            None => (NoticeKind::Info, format!("Source readable again: {name}")),
        });
    }
    let confirmed = update_.actions.contains(&Action::ReloadSourceConfirmed);
    let forced = confirmed || update_.actions.contains(&Action::ReloadSource);
    if forced && !confirmed && editor_text::unsaved(value.refused.as_deref(), &value.core) {
        let drafts = std::iter::once(&value.core.text).chain(value.core.locals.iter().map(|(_, local)| &local.code))
            .filter(|text| text.draft.as_deref().is_some_and(|draft| Some(draft) != value.refused.as_deref())
                || !text.graphs.is_empty() || text.binding_draft.is_some())
            .count();
        let message = match drafts {
            0 => "Unsaved edits will be lost.".to_owned(),
            1 => "Unapplied edits in 1 pane will be lost.".to_owned(),
            n => format!("Unapplied edits in {n} panes will be lost."),
        };
        if let Some(text) = read { value.held = Some(text); }
        value.source = Some(file);
        Rc::make_mut(&mut value.core).prompt = Some(m::Prompt::Confirming {
            title: "Reload from file?",
            message,
            accept: "Reload",
            cancel: "Keep editing",
            action: Action::ReloadSourceConfirmed,
            accepting: false,
        });
        return;
    }
    let read_changed = read.is_some();
    let text = if confirmed {
        match store::read_text(source_file::file(&file)) {
            Ok(text) => Some(text),
            Err(error) => {
                notice(value, NoticeKind::Refusal, format!("{name} not reloaded: {error}"));
                return;
            }
        }
    } else {
        read.or_else(|| value.held.clone()).or_else(|| forced.then(|| store::read_text(source_file::file(&file)).ok()).flatten())
    };
    value.source = Some(file.clone());
    let text = match text {
        None => return,
        Some(text) => text,
    };
    if !forced && (matches!(value.core.prompt, Some(m::Prompt::Confirming { .. })) || editor_text::unsaved(value.refused.as_deref(), &value.core)) {
        value.held = Some(text);
        if read_changed {
            Rc::make_mut(&mut value.core).notice = Some((
                NoticeKind::Info,
                format!("{name} changed on disk \u{b7} your unsaved edits are kept \u{b7} undo them or run \"Reload sketch from its file\" to take the file; Command-S saves yours as a preset"),
            ));
        }
        return;
    }
    let imports = source_file::read_imports(source_file::file(&file), &text);
    match imports.and_then(|imports| editor_host::reload(Some(&imports), Rc::make_mut(&mut value.core), &name, &text)) {
        Ok(()) => {
            value.source = Some(source_file::with_imports(&source_file::accepted(&file, &text), &workspace_doc::import_texts(&value.core.doc.workspace.ws)));
            value.held = None;
            value.refused = None;
            update_.scene_changed = true;
        }
        Err(diagnostics) => {
            value.held = None;
            value.refused = Some(text.clone());
            editor_host::reload_failed(Rc::make_mut(&mut value.core), &name, &text, &diagnostics);
        }
    }
}

/// Command-S: over the source file while it is what the document came from, else a preset.
pub fn save_source(core: &mut m::EditorModel, source: Option<source_file::SourceFile>, view: &flow_syntax::syntax::Syntax) -> Option<source_file::SourceFile> {
    let notice = |core: &mut m::EditorModel, kind, text| { core.notice = Some((kind, text)); };
    let preset = |core: &mut m::EditorModel, why: &str| {
        let name = preset::default_name((core.clock)());
        match preset::save(&core.presets, &name, &core.doc, view) {
            Ok(path) => notice(core, NoticeKind::Info, format!("{why}; saved as preset {}", basename(&path))),
            Err(message) => notice(core, NoticeKind::Refusal, format!("Not saved: {message}")),
        }
    };
    let text = match preset::text(&core.doc) {
        Ok(text) => text,
        Err(message) => {
            notice(core, NoticeKind::Refusal, format!("Not saved: {message}"));
            return source;
        }
    };
    match source {
        None => { preset(core, "no source file"); None },
        Some(file) => match source_file::save(&file, &text) {
            Ok(file) => {
                notice(core, NoticeKind::Info, format!("Saved {}", basename(source_file::file(&file))));
                core.filed = editor_document::document::text(&core.doc);
                Some(file)
            }
            Err(source_file::SaveError::Changed) => { preset(core, "source changed since build"); Some(file) },
            Err(source_file::SaveError::Failed(message)) => { notice(core, NoticeKind::Refusal, format!("Not saved: {message}")); Some(file) },
        },
    }
}

/// A single writer for the recovery file. New submissions replace the pending save, and the
/// writer takes the newest text after finishing any write already in progress.
#[derive(Default)]
pub struct Saver {
    shared: Arc<(std::sync::Mutex<SaveSlot>, std::sync::Condvar)>,
    thread: std::cell::RefCell<Option<std::thread::JoinHandle<()>>>,
}

#[derive(Default)]
struct SaveSlot {
    next: Option<Arc<(String, String)>>,
    text: String,
    capacity: usize,
    busy: bool,
    closed: bool,
    done: Option<Result<(), String>>,
}

impl Saver {
    fn lock(&self) -> std::sync::MutexGuard<'_, SaveSlot> {
        self.shared.0.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    fn warm(&self, capacity: usize) {
        {
            let mut slot = self.lock();
            slot.capacity = slot.capacity.max(capacity);
            let additional = capacity.saturating_sub(slot.text.len());
            slot.text.reserve(additional);
        }
        let mut thread = self.thread.borrow_mut();
        if thread.is_none() {
            let text = String::with_capacity(capacity);
            let shared = self.shared.clone();
            *thread = Some(std::thread::Builder::new().name("rays-autosave".into())
                .spawn(move || write_saves(&shared, text)).expect("autosave thread"));
        }
    }

    fn submit(&self, target: &Arc<(String, String)>, text: &str) {
        let mut slot = self.lock();
        slot.text.clear();
        slot.text.push_str(text);
        slot.next = Some(target.clone());
        self.shared.1.notify_one();
    }

    /// Waits for every submitted save; the outcome of the last one, if not read yet.
    pub fn flush(&self) -> Option<Result<(), String>> {
        let mut slot = self.lock();
        while slot.next.is_some() || slot.busy {
            slot = self.shared.1.wait(slot).unwrap_or_else(std::sync::PoisonError::into_inner);
        }
        slot.done.take()
    }

    fn take_done(&self) -> Option<Result<(), String>> {
        self.lock().done.take()
    }
}

impl Drop for Saver {
    fn drop(&mut self) {
        self.lock().closed = true;
        self.shared.1.notify_all();
        if let Some(thread) = self.thread.get_mut().take() {
            let _ = thread.join();
        }
    }
}

fn write_saves(shared: &(std::sync::Mutex<SaveSlot>, std::sync::Condvar), mut text: String) {
    let (mutex, wake) = shared;
    let mut slot = mutex.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
    loop {
        if let Some(target) = slot.next.take() {
            text.clear();
            text.reserve(slot.capacity);
            std::mem::swap(&mut text, &mut slot.text);
            slot.busy = true;
            drop(slot);
            let outcome = std::panic::catch_unwind(|| preset::save_text(&target.0, &target.1, &text).map(|_| ()))
                .unwrap_or_else(|_| Err("the writer panicked".to_owned()));
            slot = mutex.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
            slot.busy = false;
            slot.done = Some(outcome);
            wake.notify_all();
        } else if slot.closed {
            return;
        } else {
            slot = wake.wait(slot).unwrap_or_else(std::sync::PoisonError::into_inner);
        }
    }
}

/// A finished write: a failure is shown and the document saved again at the next poll.
fn read_outcome(value: &mut LoopModel, outcome: Option<Result<(), String>>) {
    match outcome {
        Some(Ok(())) => value.state_error = None,
        Some(Err(message)) => {
            value.state_error = Some(format!("Autosave failed: {message}"));
            value.saved_doc = std::sync::Weak::new();
            value.saved_drafts = None;
            value.recovery_print = None;
        }
        None => {}
    }
}

/// Cached workspace body for recovery prints. When source/extra/settings Arc identity still holds,
/// only `(layout ...)` is rebuilt — no `SyntaxBuilder::import` of the authored source.
#[derive(Clone)]
pub struct RecoveryPrint {
    body: Arc<str>,
    source: Arc<Vec<flow_syntax::syntax::Syntax>>,
    extra: Arc<Vec<flow_syntax::syntax::Syntax>>,
    imports: Arc<Vec<(String, Vec<flow_syntax::syntax::Syntax>)>>,
    ws_settings: editor_document::settings::T,
    doc_settings: editor_document::settings::T,
    fragment: bool,
    /// The viewport syntax identity and its printed form stay paired across saves.
    view: (u64, u32),
    view_line: Arc<str>,
    target: Arc<(String, String)>,
    written: String,
}

impl RecoveryPrint {
    fn matches(&self, doc: &editor_document::document::T) -> bool {
        let ws = &doc.workspace.ws;
        Arc::ptr_eq(&self.source, &ws.source)
            && Arc::ptr_eq(&self.extra, &ws.extra)
            && Arc::ptr_eq(&self.imports, &ws.imports)
            && editor_document::settings::same_values(&self.ws_settings, &ws.settings)
            && editor_document::settings::same_values(&self.doc_settings, &doc.settings)
            && self.fragment == ws.fragment
    }
}

fn prepare_recovery(value: &mut LoopModel, doc: &editor_document::document::T, view: &flow_syntax::syntax::Syntax) -> Result<(), String> {
    if let Some(cache) = value.recovery_print.as_mut().filter(|cache| cache.matches(doc)) {
        if cache.view != view.identity() {
            cache.view_line = preset::print_view(view)?.into();
            cache.view = view.identity();
        }
        return Ok(());
    }
    let body = preset::recovery_body(doc)?;
    let view_line = preset::print_view(view)?;
    let ws = &doc.workspace.ws;
    // ponytail: leave room for one small panel entry. Larger layout changes grow the buffers.
    let mut layout = String::new();
    editor_document::layout_by_path::write_into(&ws.layout, &mut layout);
    let capacity = body.len() + layout.len() + view_line.len() + 128;
    value.recovery_buf.reserve(capacity.saturating_sub(value.recovery_buf.len()));
    value.saver.warm(capacity);
    value.recovery_print = Some(RecoveryPrint {
        body: body.into(),
        source: ws.source.clone(),
        extra: ws.extra.clone(),
        imports: ws.imports.clone(),
        ws_settings: ws.settings.clone(),
        doc_settings: doc.settings.clone(),
        fragment: ws.fragment,
        view: view.identity(),
        view_line: view_line.into(),
        target: Arc::new((editor_setup::state_directory(&value.core), value.core.state_name.clone())),
        written: String::with_capacity(capacity),
    });
    Ok(())
}

/// One recovery file per sketch. Polling at 2 Hz keeps checking out of idle frames; compose runs on
/// the UI thread (layout-only splices the prior body), the write runs on the `Saver` thread; a live
/// drag is saved after its release (the caller skips held frames); close and cook.await_ flush.
pub fn autosave(value: &mut LoopModel, force: bool, now: f64) {
    let outcome = value.saver.take_done();
    read_outcome(value, outcome);
    let saved_same = value.saved_doc.upgrade().is_some_and(|d| Arc::ptr_eq(&d, &value.core.doc));
    let polled_recently = !force && now >= value.state_checked && now - value.state_checked < 0.5;
    // Layout-only edits can use the warmed print buffers on the frame that commits them.
    if polled_recently && (saved_same || !value.recovery_print.as_ref().is_some_and(|cache| cache.matches(&value.core.doc))) {
        return;
    }
    let key = V::section_key(&value.camera, &value.extra);
    // B42: unapplied drafts are work too; they are saved as the applies Command-S would make. A change of
    // any draft saves again.
    let drafts = editor_text::draft_signature(&value.core);
    if polled_recently && (drafts.is_some() || key != value.saved_view_key) {
        return;
    }
    value.state_checked = now;
    // a draft still being typed is not written: it must stand through one more poll (applying it
    // checks the document)
    let settled = drafts == value.seen_drafts;
    value.seen_drafts = drafts;
    if !force && drafts.is_some() && drafts != value.saved_drafts && !settled {
        return;
    }
    // a preview document is never saved
    if (saved_same && key == value.saved_view_key && drafts == value.saved_drafts) || editor_host::carrying(&value.core) {
        // Warm the print base while the document is idle so a later layout-only edit splices.
        if !editor_host::carrying(&value.core)
            && !value.recovery_print.as_ref().is_some_and(|cache| cache.matches(&value.core.doc) && cache.view == value.saved_view.identity())
        {
            let doc = value.core.doc.clone();
            let view = value.saved_view.clone();
            let _ = prepare_recovery(value, &doc, &view);
        }
        if force || value.core.cook.await_ {
            let outcome = value.saver.flush();
            read_outcome(value, outcome);
        }
        return;
    }
    let view = if key == value.saved_view_key { value.saved_view.clone() } else { V::section(&value.camera, &value.extra) };
    // the recovery file of an earlier session holds its unsaved work: orbiting in a document that is
    // still the one opened never writes over it
    if !value.state_owned && drafts.is_none() && editor_text::same_text(&value.core.doc, &value.opened) && Path::new(&editor_setup::state_file(&value.core)).exists() {
        value.saved_doc = Arc::downgrade(&value.core.doc);
        value.saved_view = view;
        value.saved_view_key = key;
        return;
    }
    let doc = if drafts.is_some() {
        let mut applied = (*value.core).clone();
        editor_text::apply_drafts(None, &mut applied);
        applied.doc
    } else {
        value.core.doc.clone()
    };
    if let Err(message) = prepare_recovery(value, &doc, &view) {
        read_outcome(value, Some(Err(message)));
        return;
    }
    let cache = value.recovery_print.as_mut().expect("prepared recovery");
    preset::splice_recovery_into(&cache.body, &doc.workspace.ws.layout, &cache.view_line, &mut value.recovery_buf);
    if cache.written != value.recovery_buf {
        value.saver.submit(&cache.target, &value.recovery_buf);
        std::mem::swap(&mut cache.written, &mut value.recovery_buf);
    }
    value.saved_doc = Arc::downgrade(&value.core.doc);
    value.saved_view = view;
    value.saved_view_key = key;
    value.state_owned = true;
    value.saved_drafts = drafts;
    if force || value.core.cook.await_ {
        let outcome = value.saver.flush();
        read_outcome(value, outcome);
    }
}

#[cfg(test)]
mod saver {
    use super::*;

    #[test]
    fn newest_save_wins_and_drop_waits_for_it() {
        let directory = std::env::temp_dir().join(format!("rays-saver-{}", std::process::id()));
        let target = Arc::new((directory.to_string_lossy().into_owned(), "newest".to_owned()));
        let saver = Saver::default();
        saver.warm(64);
        for text in ["first", "second", "newest"] {
            saver.submit(&target, text);
        }
        assert_eq!(saver.flush(), Some(Ok(())));
        let file = preset::path(&target.0, &target.1);
        assert_eq!(std::fs::read_to_string(&file).unwrap(), "newest");
        saver.submit(&target, "last");
        drop(saver);
        assert_eq!(std::fs::read_to_string(&file).unwrap(), "last");
        assert_eq!(std::fs::read_dir(&directory).unwrap().count(), 1);
        std::fs::remove_dir_all(directory).unwrap();
    }
}
