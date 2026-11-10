//! Helpers shared by the t6 files: ports of  /  bits and the frame builders.
#![allow(dead_code)]
use editor_document::workspace_doc as wd;
use input::{Event, Platform};
use rays::frame;
use rays_editor::{editor, workspace};

pub fn diag_text(ds: &[rays_diagnostic::Diagnostic]) -> String {
    ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")
}

/// `Rays_editor.Workspace.load text |> get_ok`.
pub fn load(text: &str) -> wd::T {
    workspace::load(None, Vec::new(), None, text).unwrap_or_else(|ds| panic!("{}", diag_text(&ds)))
}

/// A 1000x700 frame at time zero with supplied events.
pub fn frame(events: Vec<Event>) -> frame::Frame {
    frame::Frame { input: input::Frame { dt: 1. / 60., size: (1000, 700), events, ..input::at_time(0.) }, pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) }
}

pub fn options() -> editor::Create {
    editor::Create { platform: Some(Platform::Apple), ui_text_size: Some(11), await_: Some(true), domains: Some(1), ..Default::default() }
}

/// The repository root (`sketches/`, `specification/`).
pub fn repo_root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

pub fn read_repo(path: &str) -> String {
    std::fs::read_to_string(repo_root().join(path)).unwrap_or_else(|e| panic!("{path}: {e}"))
}
