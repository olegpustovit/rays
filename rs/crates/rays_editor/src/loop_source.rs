//! The source file of the loop: polled, imported, saved over, and the autosave of the recovery
//! file. Port of lib/rays_editor/loop_source.ml.
//!
//! Conventions: the model `value` and the frame's `update` travel together and are returned
//! together, as OCaml's `(value, update)` pairs.
#![allow(non_camel_case_types)]
use std::path::Path;
use std::sync::Arc;

use editor_core::store;
use editor_document::lowering;
use editor_document::preset;
use editor_document::workspace_doc;
use input::Event;
use rays::sketch;

use crate::editor_commands::action;
use crate::editor_host;
use crate::editor_model::{self as m, notice_kind, update};
use crate::editor_setup;
use crate::editor_text;
use crate::loop_model::t;
use crate::source_file;
use crate::viewport as V;

fn notice(mut update_: update, kind: notice_kind, text: String) -> update {
    update_.core.notice = Some((kind, text));
    update_
}

fn basename(file: &str) -> String {
    Path::new(file).file_name().map(|f| f.to_string_lossy().into_owned()).unwrap_or_default()
}

/// The source file, at most every half second: a changed text reloads the document. Unsaved work is
/// never replaced: while the document differs from the file's last text or a text pane holds a
/// draft, the file's new text waits (`held`) and the strip says how to choose. It loads once the
/// edits are undone, or at once by "Reload sketch from its file" (the palette). Nothing is polled
/// while a payload is carried: the document shown is a preview then.
pub fn open_import(value: t, update_: update) -> (t, update) {
    let (path, source) = match (&update_.core.open_import, &value.source) {
        (Some(path), Some(source)) => (path.clone(), source.clone()),
        _ => return (value, update_),
    };
    let mut update_ = update_;
    update_.core.open_import = None;
    if editor_text::unsaved(None, &update_.core) {
        let update_ = notice(update_, notice_kind::Refusal, format!("Save or undo your changes before opening {path}."));
        return (value, update_);
    }
    let file = if Path::new(&path).is_relative() {
        Path::new(source_file::file(&source)).parent().map(|d| d.join(&path).to_string_lossy().into_owned()).unwrap_or_else(|| path.clone())
    } else {
        path.clone()
    };
    match store::read_text(&file) {
        Err(message) => {
            let update_ = notice(update_, notice_kind::Refusal, message);
            (value, update_)
        }
        Ok(original) => match editor_host::reload(Some(&[]), &update_.core, &basename(&file), &original) {
            Err(ds) => {
                update_.core = editor_host::reload_failed(&update_.core, &path, &original, &ds);
                (value, update_)
            }
            Ok(mut core) => {
                core.file = basename(&file);
                let mut value = value;
                value.source = Some(source_file::at(&file, &lowering::sha256(&original)));
                value.held = None;
                value.refused = None;
                update_.core = core;
                update_.scene_changed = true;
                (value, update_)
            }
        },
    }
}

/// Command-O: the system dialog over the sketch's folder; its answer, a frame later, is opened the
/// way an import is (unsaved work refuses it).
pub fn open_dialog(value: t, update_: update, events: &[Event]) -> (t, update) {
    let mut value = value;
    let mut update_ = update_;
    if !update_.actions.contains(&action::Open_source) {
    } else {
        match &value.source {
            None => update_ = notice(update_, notice_kind::Refusal, "This editor was not opened from a .rays file.".to_string()),
            Some(source) => {
                let dir = Path::new(source_file::file(source)).parent().map(|d| d.to_string_lossy().into_owned()).unwrap_or_default();
                match sketch::show_file_dialog(Some(&[("Rays sketch".to_string(), vec!["rays".to_string()])]), Some(&dir), sketch::dialog::Open_file) {
                    Ok(id) => value.open_dialog = Some(id),
                    Err(message) => update_ = notice(update_, notice_kind::Refusal, format!("Open: {message}")),
                }
            }
        }
    }
    for event in events {
        if let Event::File_dialog { id, result } = event {
            if value.open_dialog == Some(*id) {
                value.open_dialog = None;
                match result {
                    Ok(paths) => {
                        if let Some(path) = paths.first() {
                            update_.core.open_import = Some(path.clone());
                        }
                    }
                    Err(message) => update_ = notice(update_, notice_kind::Refusal, format!("Open: {message}")),
                }
            }
        }
    }
    (value, update_)
}

pub fn reload_source(value: t, update_: update, now: f64) -> (t, update) {
    let previous = match &value.source {
        Some(previous) if !editor_host::carrying(&update_.core) => previous.clone(),
        _ => return (value, update_),
    };
    let mut value = value;
    let mut update_ = update_;
    let (file, read) = source_file::poll(now, &previous);
    let name = basename(source_file::file(&file));
    if file.error != previous.error {
        update_.core.notice = Some(match &file.error {
            Some(message) => (notice_kind::Refusal, format!("Source unreadable: {message}")),
            None => (notice_kind::Info, format!("Source readable again: {name}")),
        });
    }
    let forced = update_.actions.contains(&action::Reload_source);
    let text = match (&read, &value.held) {
        (Some(_), _) => read.clone(),
        (None, None) if forced => store::read_text(source_file::file(&file)).ok(),
        (None, held) => held.clone(),
    };
    value.source = Some(file.clone());
    let text = match text {
        None => return (value, update_),
        Some(text) => text,
    };
    if !forced && editor_text::unsaved(value.refused.as_deref(), &update_.core) {
        value.held = Some(text);
        if read.is_some() {
            update_.core.notice = Some((
                notice_kind::Info,
                format!("{name} changed on disk \u{b7} your unsaved edits are kept \u{b7} undo them or run \"Reload sketch from its file\" to take the file; Command-S saves yours as a preset"),
            ));
        }
        return (value, update_);
    }
    let imports = source_file::read_imports(source_file::file(&file), &text);
    match imports.map_err(|d| d).and_then(|imports| editor_host::reload(Some(&imports), &update_.core, &name, &text)) {
        Ok(core) => {
            value.source = Some(source_file::with_imports(&source_file::accepted(&file, &text), &workspace_doc::import_texts(&core.doc.workspace.ws)));
            value.held = None;
            value.refused = None;
            update_.core = core;
            update_.scene_changed = true;
            (value, update_)
        }
        Err(diagnostics) => {
            value.held = None;
            value.refused = Some(text.clone());
            update_.core = editor_host::reload_failed(&update_.core, &name, &text, &diagnostics);
            (value, update_)
        }
    }
}

/// Command-S: over the source file while it is what the document came from, else a preset.
pub fn save_source(core: m::t, source: Option<source_file::t>, view: &flow_syntax::syntax::Syntax) -> (m::t, Option<source_file::t>) {
    let notice_ = |mut core: m::t, kind: notice_kind, text: String| -> m::t {
        core.notice = Some((kind, text));
        core
    };
    let preset_ = |core: m::t, why: &str| -> m::t {
        let name = preset::default_name((core.clock)());
        match preset::save(&core.presets, &name, &core.doc, view) {
            Ok(path) => notice_(core, notice_kind::Info, format!("{why}; saved as preset {}", basename(&path))),
            Err(message) => notice_(core, notice_kind::Refusal, format!("Not saved: {message}")),
        }
    };
    let text = preset::text(&core.doc);
    match source {
        None => (preset_(core, "no source file"), None),
        Some(file) => match source_file::save(&file, &text) {
            Ok(file) => {
                let message = format!("Saved {}", basename(source_file::file(&file)));
                (editor_text::filed(notice_(core, notice_kind::Info, message)), Some(file))
            }
            Err(source_file::Save_error::Changed) => (preset_(core, "source changed since build"), Some(file)),
            Err(source_file::Save_error::Failed(message)) => (notice_(core, notice_kind::Refusal, format!("Not saved: {message}")), Some(file)),
        },
    }
}

/// One recovery file per sketch. Polling at 2 Hz coalesces live drags and keeps printing and
/// filesystem work out of idle frames; close flushes.
pub fn autosave(value: t, force: bool, now: f64) -> t {
    if !force && now >= value.state_checked && now - value.state_checked < 0.5 {
        return value;
    }
    let view = V::section(&value.camera, &value.extra);
    let mut value = value;
    value.state_checked = now;
    let saved_same = value.saved_doc.upgrade().is_some_and(|d| Arc::ptr_eq(&d, &value.core.doc));
    // a preview document is never saved
    if (saved_same && view == value.saved_view) || editor_host::carrying(&value.core) {
        return value;
    }
    // the recovery file of an earlier session holds its unsaved work: orbiting in a document that is
    // still the one opened never writes over it
    if !value.state_owned && editor_text::same_text(&value.core.doc, &value.opened) && Path::new(&editor_setup::state_file(&value.core)).exists() {
        value.saved_doc = Arc::downgrade(&value.core.doc);
        value.saved_view = view;
        return value;
    }
    match preset::save(&editor_setup::state_directory(&value.core), &value.core.state_name, &value.core.doc, &view) {
        Ok(_) => {
            value.saved_doc = Arc::downgrade(&value.core.doc);
            value.saved_view = view;
            value.state_error = None;
            value.state_owned = true;
            value
        }
        Err(message) => {
            value.state_error = Some(format!("Autosave failed: {message}"));
            value
        }
    }
}
