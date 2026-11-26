//! A child Rays process owns its native renderer. The editor's main thread only polls messages.
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::rc::Rc;
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};

use editor_document::{kind_graph, workspace_doc};

use crate::{editor_inspect::{View, view_wants}, loop_model::LoopModel, source_file};

#[derive(Clone)]
pub(crate) struct Job(Rc<Receiver<(bool, String)>>);

impl Job {
    pub fn poll(&self) -> Option<(bool, String)> {
        let mut latest = None;
        loop {
            match self.0.try_recv() {
                Ok(message @ (true, _)) => return Some(message),
                Ok(message) => latest = Some(message),
                Err(TryRecvError::Empty) => return latest,
                Err(TryRecvError::Disconnected) => return Some((true, "Camera render stopped.".into())),
            }
        }
    }
}

const OUTPUT_KEY: &str = "render_output";

fn home() -> PathBuf {
    std::env::var_os(if cfg!(windows) { "USERPROFILE" } else { "HOME" }).map(PathBuf::from).unwrap_or_else(|| PathBuf::from("."))
}

fn pictures() -> PathBuf {
    let chosen = if cfg!(windows) {
        Command::new("powershell.exe").args(["-NoProfile", "-NonInteractive", "-Command", "[Environment]::GetFolderPath('MyPictures')"]).output().ok()
    } else if cfg!(target_os = "linux") {
        Command::new("xdg-user-dir").arg("PICTURES").output().ok()
    } else {
        None
    };
    chosen.and_then(|output| {
        let path = PathBuf::from(String::from_utf8(output.stdout).ok()?.trim());
        (output.status.success() && path.is_absolute() && path != home()).then_some(path)
    }).unwrap_or_else(|| home().join("Pictures"))
}

pub(crate) fn output_preference(preferences: &str) -> String {
    if let Ok(values) = crate::editor_setup::read_preferences(preferences) {
        for (key, node) in values {
            if key == OUTPUT_KEY {
                if let flow_syntax::syntax::Node::Str(bytes) = node.node {
                    let value = String::from_utf8_lossy(node.bytes(bytes));
                    if !value.trim().is_empty() {
                        return value.into_owned();
                    }
                }
            }
        }
    }
    pictures().join("Rays").to_string_lossy().into_owned()
}

pub(crate) fn reveal(path: &str) -> Result<(), String> {
    let path = Path::new(path);
    let folder = if path.is_dir() { path } else { path.parent().unwrap_or(Path::new(".")) };
    let mut command = if cfg!(target_os = "macos") {
        let mut command = Command::new("open");
        command.arg("-R").arg(path);
        command
    } else if cfg!(windows) {
        let mut command = Command::new("explorer");
        if path.is_dir() {
            command.arg(path);
        } else {
            command.arg(format!("/select,{}", path.display()));
        }
        command
    } else {
        let mut command = Command::new("xdg-open");
        command.arg(folder);
        command
    };
    let mut child = command.spawn().map_err(|e| format!("Could not reveal {}: {e}", path.display()))?;
    std::thread::spawn(move || { let _ = child.wait(); });
    Ok(())
}

fn optional_positive(text: &str, label: &str) -> Result<Option<i64>, String> {
    let text = text.trim();
    if text.is_empty() { return Ok(None); }
    text.parse::<i64>().ok().filter(|value| *value > 0).map(Some)
        .ok_or_else(|| format!("{label} must be a positive integer."))
}

pub(crate) fn has_camera(doc: &editor_document::document::T) -> bool {
    doc.active_camera.is_some() && doc.workspace.ws.checked.graphs.iter().any(|g| g.context == flow_syntax::context::scene())
}

pub(crate) fn start(editor: &mut LoopModel, all: bool, control: &crate::viewport::Control) -> Result<(), String> {
    if editor.render_output.is_some() {
        return Err("A camera render is already running.".into());
    }
    let output = control.output.trim();
    if output.is_empty() { return Err("Choose an output folder.".into()); }
    let output = if let Some(suffix) = output.strip_prefix("~/") {
        home().join(suffix)
    } else {
        PathBuf::from(output)
    };
    let output = std::path::absolute(output).map_err(|e| e.to_string())?;
    let width = optional_positive(&control.render_width, "Render width")?;
    let spp = optional_positive(&control.render_spp, "Render spp")?;
    let doc = &editor.core.doc;
    if !has_camera(doc) { return Err("Choose an authored scene camera first.".into()); }
    let cameras: Vec<String> = if all {
        doc.scene.iter().filter(|node| node.kind == "camera" && view_wants(&editor.core, &View::Primary, node.id))
            .map(|node| node.label.clone()).collect()
    } else {
        crate::editor_host::selected_camera(&editor.core).and_then(|id| kind_graph::find(&doc.scene, id))
            .map(|node| vec![node.label.clone()]).unwrap_or_default()
    };
    if cameras.is_empty() {
        return Err("Choose a scene camera first.".into());
    }
    let now = (editor.core.clock)();
    let text = workspace_doc::to_text(&doc.workspace.ws).map_err(|e| e.message)?;
    let origin = editor.source.as_ref().map(source_file::file).unwrap_or("untitled.rays").to_owned();
    let sketch = Path::new(&origin).file_stem().and_then(|s| s.to_str())
        .filter(|s| *s != "untitled").map_or_else(|| workspace_doc::name(&doc.workspace.ws), str::to_owned);
    let frame = editor.core.timeline.frame;
    let exe = std::env::current_exe().map_err(|e| e.to_string())?;
    let (send, receive) = mpsc::channel();
    std::thread::Builder::new().name("rays-render-output".into()).spawn(move || {
        let result = render(&exe, &origin, &text, frame, &output, &sketch, &cameras, now, width, spp, &send);
        let status = match result {
            Ok(path) => format!("Saved {}", path.display()),
            Err(error) => format!("Camera render failed: {error}"),
        };
        let _ = send.send((true, status));
    }).map_err(|e| e.to_string())?;
    editor.render_output = Some(Job(Rc::new(receive)));
    Ok(())
}

fn reserve(folder: &Path, sketch: &str, camera: &str, mut now: f64) -> std::io::Result<PathBuf> {
    let sketch = editor_document::preset::sanitize(sketch);
    let camera = editor_document::preset::sanitize(camera);
    loop {
        let timestamp = editor_document::preset::default_name(now).replace('-', "").replace('_', "-");
        let path = folder.join(format!("{sketch}-{camera}-{timestamp}.png"));
        match std::fs::OpenOptions::new().write(true).create_new(true).open(&path) {
            Ok(_) => return Ok(path),
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => now += 1.,
            Err(error) => return Err(error),
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn render(exe: &Path, source: &str, text: &str, frame: i64, folder: &Path, sketch: &str, cameras: &[String], now: f64, width: Option<i64>, spp: Option<i64>, progress: &Sender<(bool, String)>) -> Result<PathBuf, String> {
    std::fs::create_dir_all(folder).map_err(|e| format!("{}: {e}", folder.display()))?;
    let mut saved = folder.to_owned();
    for (index, camera) in cameras.iter().enumerate() {
        let prefix = format!("Rendering {camera} ({}/{})", index + 1, cameras.len());
        if progress.send((false, prefix.clone())).is_err() {
            return Ok(saved);
        }
        let path = reserve(folder, sketch, camera, now).map_err(|e| e.to_string())?;
        let result = render_one(exe, source, text, frame, camera, &path, width, spp, &prefix, progress);
        if result.is_err() {
            let _ = std::fs::remove_file(&path);
        }
        result?;
        if cameras.len() == 1 {
            saved = path;
        }
    }
    Ok(saved)
}

#[allow(clippy::too_many_arguments)]
fn render_one(exe: &Path, source: &str, text: &str, frame: i64, camera: &str, path: &Path, width: Option<i64>, spp: Option<i64>, prefix: &str, progress: &Sender<(bool, String)>) -> Result<(), String> {
    let mut command = Command::new(exe);
    command.arg("render").arg(source).args(["--source", "-", "--camera", camera, "--frame", &frame.to_string(), "-o"]).arg(path);
    if let Some(width) = width { command.args(["--width", &width.to_string()]); }
    if let Some(spp) = spp { command.args(["--samples", &spp.to_string()]); }
    let mut child = command.env("RAYS_RENDER_PROGRESS", "1")
        .stdin(Stdio::piped()).stdout(Stdio::null()).stderr(Stdio::piped())
        .spawn().map_err(|e| e.to_string())?;
    let write = child.stdin.take().unwrap().write_all(text.as_bytes());
    let mut error = String::new();
    let read = (|| {
        write?;
        for line in BufReader::new(child.stderr.take().unwrap()).lines() {
            let line = line?;
            if let Some(percent) = line.strip_prefix("RAYS_PROGRESS ") {
                if progress.send((false, format!("{prefix} · {percent}%"))).is_err() {
                    let _ = child.kill();
                    break;
                }
            } else if !line.trim().is_empty() && error.len() < 4096 {
                error.push_str(&line);
                error.push('\n');
            }
        }
        Ok::<_, std::io::Error>(())
    })();
    if read.is_err() {
        let _ = child.kill();
    }
    let exit = child.wait().map_err(|e| e.to_string())?;
    read.map_err(|e| e.to_string())?;
    if !exit.success() {
        return Err(if error.is_empty() { format!("{camera}: {exit}") } else { error });
    }
    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn camera_render_output_preferences_and_unique_names() {
        let folder = std::env::temp_dir().join(format!("rays-render-output-test-{}", std::process::id()));
        std::fs::create_dir_all(&folder).unwrap();
        let preferences = folder.join("preferences.rays");
        let preferences = preferences.to_str().unwrap();
        crate::editor_setup::save_guide(preferences, true).unwrap();
        crate::editor_setup::save_preference(preferences, OUTPUT_KEY,
            flow_syntax::arena::Draft::Str(folder.as_os_str().to_str().unwrap().as_bytes().to_vec())).unwrap();
        assert_eq!(output_preference(preferences), folder.to_str().unwrap());
        assert!(crate::editor_setup::read_preferences(preferences).unwrap().iter().any(|(k, _)| k == "guide"));
        let first = reserve(&folder, "hero", "tele/usage", 0.).unwrap();
        std::fs::write(&first, b"older frame").unwrap();
        let second = reserve(&folder, "hero", "tele/usage", 0.).unwrap();
        assert!(first.ends_with("hero-tele_usage-19700101-000000.png"));
        assert!(second.ends_with("hero-tele_usage-19700101-000001.png"));
        assert_eq!(std::fs::read(first).unwrap(), b"older frame");
        std::fs::remove_dir_all(folder).unwrap();
    }
}
