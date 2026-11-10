//! The .rays file a running sketch was built from: found once, polled for edits, saved over.
//! Immutable; the caller keeps the returned value. `digest` is the SHA-256 of the text the running
//! document last came from (built from, reloaded or saved), so an edit from any other writer differs
//! from it.

use std::path::{Path, PathBuf};

use editor_core::store;
use editor_document::{lowering, workspace_doc};

#[derive(Clone, Debug, PartialEq)]
pub struct SourceFile {
    pub file: String,
    pub digest: String,
    pub observed: Option<String>,
    pub polled: f64,
    pub error: Option<String>,
    pub imports: Vec<(String, SourceFile)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SaveError {
    Changed,
    Failed(String),
}

fn sha(text: &str) -> String {
    lowering::sha256(text)
}

fn read(file: &str) -> Result<String, String> {
    store::read_text(file)
}

const INTERVAL: f64 = 0.5;

fn join(dir: &str, name: &str) -> String {
    Path::new(dir).join(name).to_string_lossy().into_owned()
}

fn dirname(file: &str) -> String {
    match Path::new(file).parent() {
        Some(p) if !p.as_os_str().is_empty() => p.to_string_lossy().into_owned(),
        _ => ".".to_string(),
    }
}

/// the file as it is now, whose text has `digest`
pub fn synced(file: &str, digest: &str) -> SourceFile {
    SourceFile { file: file.to_string(), digest: digest.to_string(), observed: Some(digest.to_string()), polled: f64::NEG_INFINITY, error: None, imports: Vec::new() }
}

/// The first poll reads the file, so a file that already differs from the text the sketch was built
/// from (edited since the build) reloads at once.
pub fn at(file: &str, digest: &str) -> SourceFile {
    SourceFile { observed: None, ..synced(file, digest) }
}

/// the Cargo workspace at or above `dir`, excluding build output
pub fn root(dir: &str) -> Option<String> {
    let mut dir = PathBuf::from(dir);
    loop {
        // Windows' getcwd returns backslashes: split on both separators
        let in_target = dir.to_string_lossy().replace('\\', "/").split('/').any(|c| c == "target");
        if !in_target && std::fs::read_to_string(dir.join("Cargo.toml")).is_ok_and(|text| text.lines().any(|line| line.trim() == "[workspace]")) {
            return Some(dir.to_string_lossy().into_owned());
        }
        match dir.parent() {
            Some(parent) if parent != dir && !parent.as_os_str().is_empty() => dir = parent.to_path_buf(),
            _ => return None,
        }
    }
}

/// ponytail: a sketch launched from outside its project has no source and only presets
pub fn find(path: &str, digest: &str) -> Option<SourceFile> {
    let cwd = std::env::current_dir().ok()?;
    let absolute = |p: PathBuf| if p.is_relative() { cwd.join(p) } else { p };
    let exe = std::env::current_exe().ok().map(|p| absolute(p).parent().map(|d| d.to_path_buf()).unwrap_or_default());
    let starts = [exe, Some(cwd.clone())];
    starts.into_iter().flatten().find_map(|start| {
        root(&start.to_string_lossy()).and_then(|dir| {
            let file = join(&dir, path);
            if Path::new(&file).exists() { Some(at(&file, digest)) } else { None }
        })
    })
}

pub fn file(t: &SourceFile) -> &str {
    &t.file
}

pub fn read_imports(file: &str, text: &str) -> Result<Vec<(String, String)>, Vec<rays_diagnostic::Diagnostic>> {
    let paths = workspace_doc::import_paths(text)?;
    let mut imports = Vec::new();
    for path in paths {
        match read(&join(&dirname(file), &path)) {
            Ok(text) => imports.push((path, text)),
            Err(message) => {
                return Err(vec![rays_diagnostic::error("E_IMPORT".to_string(), format!("{path}: {message}"), None, None, None, Vec::new(), Vec::new())]);
            }
        }
    }
    Ok(imports)
}

pub fn with_imports(t: &SourceFile, imports: &[(String, String)]) -> SourceFile {
    let imports = imports.iter().map(|(path, text)| (path.clone(), at(&join(&dirname(&t.file), path), &sha(text)))).collect();
    SourceFile { imports, ..t.clone() }
}

/// One content read per interval, independent of mtime/inode. `observed` suppresses repeated attempts
/// to reload the same refused text.
pub fn poll(now: f64, value: &SourceFile) -> (SourceFile, Option<String>) {
    if now - value.polled < INTERVAL {
        return (value.clone(), None);
    }
    let mut t = value.clone();
    t.polled = now;
    match read(&t.file) {
        Err(message) => {
            t.error = Some(message);
            (t, None)
        }
        Ok(text) => {
            let digest = sha(&text);
            let changed = t.observed.as_deref() != Some(digest.as_str()) && (t.observed.is_some() || digest != t.digest);
            let mut imports = Vec::new();
            let mut imported_change = false;
            for (path, source) in &t.imports {
                let old_error = source.error.clone();
                let (source, fresh) = poll(now, source);
                imported_change = imported_change || fresh.is_some() || source.error != old_error;
                imports.push((path.clone(), source));
            }
            t.observed = Some(digest);
            t.error = None;
            t.imports = imports;
            (t, if changed || imported_change { Some(text) } else { None })
        }
    }
}

pub fn accepted(t: &SourceFile, text: &str) -> SourceFile {
    SourceFile { digest: sha(text), ..t.clone() }
}

/// Only over the text the document came from; the file keeps its permissions (a temporary file is
/// created 0600). The digest check and rename cannot exclude another editor writing between them;
/// that requires a cooperative writer protocol, not an atomic rename alone.
pub fn save(value: &SourceFile, text: &str) -> Result<SourceFile, SaveError> {
    let text = if text.ends_with('\n') { text.to_string() } else { format!("{text}\n") };
    match read(&value.file) {
        Ok(disk) if sha(&disk) == value.digest => {
            let perm = std::fs::metadata(&value.file).ok().map(|m| m.permissions());
            store::write_text(&value.file, &text).map_err(SaveError::Failed)?;
            if let Some(perm) = perm {
                let _ = std::fs::set_permissions(&value.file, perm);
            }
            Ok(SourceFile { polled: value.polled, imports: value.imports.clone(), ..synced(&value.file, &sha(&text)) })
        }
        Ok(_) => Err(SaveError::Changed),
        Err(message) => Err(SaveError::Failed(message)),
    }
}
