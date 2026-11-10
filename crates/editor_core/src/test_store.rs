use crate::store::{read_text, settings, user_folder, write_text};
use input::Platform;
use flow_syntax::syntax::{self, Syntax};

fn value(text: &str) -> Syntax { syntax::parse(text).unwrap().remove(0) }

fn same_settings(a: &settings::Settings, b: &settings::Settings) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|((ka, va), (kb, vb))| ka == kb && syntax::equal(va, vb))
}
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

/// A fresh directory under the platform temporary folder.
fn temp_dir(prefix: &str) -> PathBuf {
    let n = COUNTER.fetch_add(1, Ordering::SeqCst);
    let dir = std::env::temp_dir().join(format!("{}-{}-{}", prefix, std::process::id(), n));
    std::fs::create_dir_all(&dir).expect("temp dir");
    dir
}

/// Remove retained entries in order when this test ends.
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
    let _cleanup = Cleanup(vec![link.clone(), target.clone(), root]);
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
    let _cleanup = Cleanup(vec![path.clone(), directory, root]);
    let filename = path.to_str().unwrap();
    let values = vec![
        ("animate".to_string(), value("true")),
        ("density".to_string(), value("0.625")),
        ("count".to_string(), value("42")),
        ("mode".to_string(), value("(choice \"orbit\")")),
        ("label".to_string(), value("\"hello\\nworld\"")),
        ("range".to_string(), value("[0.2 0.8]")),
    ];
    settings::save("test", filename, &values).unwrap();
    assert!(same_settings(&settings::load("test", filename).unwrap(), &values));
    assert!(settings::load("other", filename).is_err());
    let text = read_text(filename).unwrap();
    assert!(text.starts_with("(settings :sketch \"test\""));
    assert!(
        settings::save("test", filename, &[("bad".to_string(), crate::store::form::number(f64::INFINITY))])
            .is_err()
    );
    assert!(same_settings(&settings::load("test", filename).unwrap(), &values));
    println!("editor store: atomic s-expression round-trip");
}

#[test]
fn settings_int_above_2_62_decodes_as_int() {
    let root = temp_dir("rays-store-int63");
    let path = root.join("settings.rays");
    let _cleanup = Cleanup(vec![path.clone(), root]);
    let filename = path.to_str().unwrap();
    for value in [1i64 << 62, i64::MIN, i64::MAX] {
        write_text(filename, &format!("(settings :sketch \"test\" :lim {value})")).unwrap();
        let loaded = settings::load("test", filename).unwrap();
        assert_eq!(settings::int(&loaded, "lim"), Some(value));
        assert_eq!(settings::float(&loaded, "lim"), None);
        settings::save("test", filename, &loaded).unwrap();
        assert_eq!(settings::int(&settings::load("test", filename).unwrap(), "lim"), Some(value));
    }
}

// The per-user folder is a function of the platform and the facts it is given.
#[test]
fn user_folder_follows_platform() {
    let pref = Some("C:/AppData/rays/editor/");
    let expected = if cfg!(windows) { "/h\\.rays" } else { "/h/.rays" };
    assert!(user_folder(Platform::Apple, pref, "/h") == expected);
    assert!(user_folder(Platform::Other, pref, "/h") == "C:/AppData/rays/editor/");
    assert!(user_folder(Platform::Other, None, "/h") == expected);
    println!("editor store: user folder follows the platform's preference folder ok");
}

#[test]
fn settings_keep_first_typed_entry_and_refuse_invalid_overwrite() {
    let root = temp_dir("rays-store-preferences");
    let path = root.join("preferences.rays");
    let _cleanup = Cleanup(vec![path.clone(), root]);
    let filename = path.to_str().unwrap();
    let original = "(settings :sketch \"rays-editor\" :guide false :guide true :memory_budget -9223372036854775808 :unknown (choice \"é\"))";
    write_text(filename, original).unwrap();
    let values = settings::load("rays-editor", filename).unwrap();
    assert_eq!(settings::bool(&values, "guide"), Some(false));
    assert_eq!(settings::int(&values, "memory_budget"), Some(i64::MIN));
    assert_eq!(settings::int(&values, "guide"), None);
    assert_eq!(settings::bool(&vec![("guide".into(), value("1")), ("guide".into(), value("true"))], "guide"), None);
    assert!(settings::save("rays-editor", filename, &[("bad".into(), value("[1 2 3]"))]).is_err());
    assert_eq!(read_text(filename).unwrap(), original);
    settings::save("rays-editor", filename, &values).unwrap();
    assert!(same_settings(&settings::load("rays-editor", filename).unwrap(), &values));
    for text in ["(settings :guide)", "(settings guide false)", "(settings :guide (run))", "(settings :guide [1 NaN])"] {
        write_text(filename, text).unwrap();
        assert!(settings::load("rays-editor", filename).is_err(), "accepted {text}");
    }
}
