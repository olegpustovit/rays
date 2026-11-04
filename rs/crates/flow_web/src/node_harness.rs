//! Test support: run a generated JavaScript module under node (the OCaml tests shell out to node
//! the same way). The tests skip loudly when node is absent.
use std::path::PathBuf;
use std::process::Command;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicUsize, Ordering};

pub fn node_available() -> bool {
    static AVAILABLE: OnceLock<bool> = OnceLock::new();
    *AVAILABLE.get_or_init(|| Command::new("node").arg("--version").output().map(|o| o.status.success()).unwrap_or(false))
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../").canonicalize().expect("repository root")
}

/// `file:///C:/.../lib/web_export/runtime/<name>` for an ES module import.
pub fn runtime_url(name: &str) -> String {
    let path = repo_root().join("lib/web_export/runtime").join(name);
    let text = path.to_string_lossy().replace('\\', "/");
    let text = text.trim_start_matches("//?/").to_owned();
    if text.starts_with('/') { format!("file://{text}") } else { format!("file:///{text}") }
}

/// The repository path of a file under tools/web/test or sketches, as an absolute path.
pub fn repo_path(relative: &str) -> PathBuf { repo_root().join(relative) }

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn scratch(name: &str) -> PathBuf {
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    std::env::temp_dir().join(format!("flow_web-{}-{n}-{name}", std::process::id()))
}

/// Run a module with node; returns (success, stdout, stderr).
pub fn run_module(source: &str) -> (bool, String, String) {
    let file = scratch("script.mjs");
    std::fs::write(&file, source).expect("write script");
    let output = Command::new("node").arg(&file).output().expect("spawn node");
    let _ = std::fs::remove_file(&file);
    (output.status.success(), String::from_utf8_lossy(&output.stdout).into_owned(), String::from_utf8_lossy(&output.stderr).into_owned())
}

/// Run a module and require success; returns stdout.
pub fn run_ok(source: &str) -> String {
    let (ok, out, err) = run_module(source);
    assert!(ok, "node failed:\n{err}\n{out}");
    out
}
