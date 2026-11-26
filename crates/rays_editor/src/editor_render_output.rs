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

pub(crate) fn start(editor: &mut LoopModel, all: bool, output: &str) -> Result<(), String> {
    if editor.render_output.is_some() {
        return Err("A camera render is already running.".into());
    }
    let output = PathBuf::from(output.trim());
    if output.as_os_str().is_empty() {
        return Err("Choose an output path.".into());
    }
    let doc = &editor.core.doc;
    let jobs = if all {
        let mut paths = std::collections::BTreeSet::new();
        doc.scene.iter().filter(|node| node.kind == "camera" && view_wants(&editor.core, &View::Primary, node.id)).map(|node| {
            let stem = editor_document::preset::sanitize(&node.label);
            let path = output.join(format!("{stem}.png"));
            if !paths.insert(path.to_string_lossy().to_ascii_lowercase()) {
                return Err(format!("Camera names have the same output filename: {}", path.display()));
            }
            Ok((node.label.clone(), path))
        }).collect::<Result<Vec<_>, String>>()?
    } else {
        doc.active_camera.and_then(|id| kind_graph::find(&doc.scene, id))
            .map(|node| vec![(node.label.clone(), output)]).unwrap_or_default()
    };
    if jobs.is_empty() {
        return Err("Choose a scene camera first.".into());
    }
    let text = workspace_doc::to_text(&doc.workspace.ws).map_err(|e| e.message)?;
    let origin = editor.source.as_ref().map(source_file::file).unwrap_or("untitled.rays").to_owned();
    let frame = editor.core.timeline.frame;
    let exe = std::env::current_exe().map_err(|e| e.to_string())?;
    let (send, receive) = mpsc::channel();
    std::thread::Builder::new().name("rays-render-output".into()).spawn(move || {
        let result = render(&exe, &origin, &text, frame, &jobs, &send);
        let status = match result {
            Ok(()) if jobs.len() == 1 => format!("Saved {}", jobs[0].1.display()),
            Ok(()) => format!("Saved {} camera renders", jobs.len()),
            Err(error) => format!("Camera render failed: {error}"),
        };
        let _ = send.send((true, status));
    }).map_err(|e| e.to_string())?;
    editor.render_output = Some(Job(Rc::new(receive)));
    Ok(())
}

fn render(exe: &Path, source: &str, text: &str, frame: i64, jobs: &[(String, PathBuf)], progress: &Sender<(bool, String)>) -> Result<(), String> {
    for (index, (camera, path)) in jobs.iter().enumerate() {
        let prefix = format!("Rendering {camera} ({}/{})", index + 1, jobs.len());
        if progress.send((false, prefix.clone())).is_err() {
            return Ok(());
        }
        let mut child = Command::new(exe)
            .arg("render").arg(source).args(["--source", "-", "--camera", camera, "--frame", &frame.to_string(), "-o"])
            .arg(path).env("RAYS_RENDER_PROGRESS", "1")
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
    }
    Ok(())
}
