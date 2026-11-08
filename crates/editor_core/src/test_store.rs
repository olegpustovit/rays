//! Port of lib/editor_core/test_store.ml.
use crate::store::{read_text, settings, user_folder, write_text};
use input::Platform;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

/// A fresh directory under the platform temporary folder (OCaml's Filename.temp_dir).
fn temp_dir(prefix: &str) -> PathBuf {
    let n = COUNTER.fetch_add(1, Ordering::SeqCst);
    let dir = std::env::temp_dir().join(format!("{}-{}-{}", prefix, std::process::id(), n));
    std::fs::create_dir_all(&dir).expect("temp dir");
    dir
}

/// Removes what it holds, in the order given, when the test ends (OCaml's Fun.protect ~finally).
struct Cleanup(Vec<PathBuf>);

impl Drop for Cleanup {
    fn drop(&mut self) {
        for path in self.0.iter() {
            if std::fs::symlink_metadata(path).map(|m| m.is_dir()).unwrap_or(false) {
                let _ = std::fs::remove_dir(path);
            } else {
                let _ = std::fs::remove_file(path);
            }
        }
    }
}

#[cfg(unix)]
fn symlink_file(target: &std::path::Path, link: &std::path::Path) -> std::io::Result<()> {
    std::os::unix::fs::symlink(target, link)
}

#[cfg(windows)]
fn symlink_file(target: &std::path::Path, link: &std::path::Path) -> std::io::Result<()> {
    std::os::windows::fs::symlink_file(target, link)
}

// a symlinked file stays a link after a save, and the file it points at gets the text
#[test]
fn symlink_save_keeps_link() {
    let root = temp_dir("rays-store-link");
    let target = root.join("real.rays");
    let link = root.join("link.rays");
    let _cleanup = Cleanup(vec![link.clone(), target.clone(), root.clone()]);
    write_text(target.to_str().unwrap(), "old").unwrap();
    symlink_file(&target, &link).expect("symlink (Windows needs developer mode or admin)");
    write_text(link.to_str().unwrap(), "new").unwrap();
    assert!(std::fs::symlink_metadata(&link).unwrap().file_type().is_symlink());
    assert!(read_text(target.to_str().unwrap()).unwrap() == "new");
}

#[test]
fn settings_round_trip() {
    let root = temp_dir("rays-store");
    let directory = root.join("nested");
    let path = directory.join("settings.rays");
    let _cleanup = Cleanup(vec![path.clone(), directory.clone(), root.clone()]);
    let filename = path.to_str().unwrap();
    let values = vec![
        ("animate".to_string(), settings::Value::Bool(true)),
        ("density".to_string(), settings::Value::Float(0.625)),
        ("count".to_string(), settings::Value::Int(42)),
        ("mode".to_string(), settings::Value::Choice("orbit".into())),
        ("label".to_string(), settings::Value::Text("hello\nworld".into())),
        ("range".to_string(), settings::Value::Pair(0.2, 0.8)),
    ];
    settings::save("test", filename, &values).unwrap();
    assert!(settings::load("test", filename).unwrap() == values);
    assert!(settings::load("other", filename).is_err());
    let text = read_text(filename).unwrap();
    assert!(text.starts_with("(settings :sketch \"test\""));
    assert!(
        settings::save("test", filename, &[("bad".to_string(), settings::Value::Float(f64::INFINITY))])
            .is_err()
    );
    assert!(settings::load("test", filename).unwrap() == values);
    println!("editor store: atomic s-expression round-trip");
}

// The per-user folder is a function of the platform and the facts it is given.
#[test]
fn user_folder_follows_platform() {
    let pref = Some("C:/AppData/rays/editor/");
    // Filename.concat "/h" ".rays": OCaml joins with the platform's directory separator
    let expected = if cfg!(windows) { "/h\\.rays" } else { "/h/.rays" };
    assert!(user_folder(Platform::Apple, pref, "/h") == expected);
    assert!(user_folder(Platform::Other, pref, "/h") == "C:/AppData/rays/editor/");
    assert!(user_folder(Platform::Other, None, "/h") == expected);
    println!("editor store: user folder follows the platform's preference folder ok");
}
