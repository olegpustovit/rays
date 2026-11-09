//! Port of lib/editor_core/store.ml: atomic file writes and the s-expression state stored beside
//! documents (a sketch's or the editor's saved settings; viewport camera forms live in rays_editor).

use input::Platform;
use std::sync::atomic::{AtomicI64, Ordering};

pub mod form;

static TEMPORARY: AtomicI64 = AtomicI64::new(0);

fn ensure_directory(path: &str) -> Result<(), String> {
    if path.is_empty() || path == "." {
        return Ok(());
    }
    match std::fs::metadata(path) {
        Ok(meta) if meta.is_dir() => Ok(()),
        Ok(_) => Err(format!("{path}: not a directory")),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            let parent = match std::path::Path::new(path).parent() {
                Some(p) if p.as_os_str().is_empty() => ".".to_string(),
                Some(p) => p.to_string_lossy().into_owned(),
                None => return Err(format!("{path}: no parent directory")),
            };
            if parent == path {
                return Err(format!("{path}: no parent directory"));
            }
            ensure_directory(&parent)?;
            match std::fs::create_dir(path) {
                Ok(()) => Ok(()),
                Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => ensure_directory(path),
                Err(e) => Err(format!("{path}: {e}")),
            }
        }
        Err(e) => Err(format!("{path}: {e}")),
    }
}

fn write_atomic(filename: &str, write: &dyn Fn(&str) -> Result<(), String>) -> Result<(), String> {
    let filename = match std::fs::canonicalize(filename) {
        Ok(p) => {
            let p = p.to_string_lossy().into_owned();
            p.strip_prefix(r"\\?\").map(str::to_owned).unwrap_or(p)
        }
        Err(_) => filename.to_owned(),
    };
    let directory = match std::path::Path::new(&filename).parent() {
        Some(p) if p.as_os_str().is_empty() => ".".to_string(),
        Some(p) => p.to_string_lossy().into_owned(),
        None => ".".to_string(),
    };
    ensure_directory(&directory)?;
    let unique = TEMPORARY.fetch_add(1, Ordering::Relaxed);
    let temporary = std::path::Path::new(&directory)
        .join(format!(".rays-{:x}{:x}.tmp", std::process::id(), unique))
        .to_string_lossy()
        .into_owned();
    std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&temporary)
        .map_err(|e| e.to_string())?;
    let finish = || -> Result<(), String> {
        write(&temporary)?;
        let file = std::fs::OpenOptions::new().write(true).open(&temporary).map_err(|e| e.to_string())?;
        file.sync_all().map_err(|e| e.to_string())?;
        drop(file);
        std::fs::rename(&temporary, &filename).map_err(|e| e.to_string())
    };
    let result = finish();
    let _ = std::fs::remove_file(&temporary);
    result
}

/// Write atomically; creates the parent directory when needed.
pub fn write_text(filename: &str, text: &str) -> Result<(), String> {
    write_atomic(filename, &|temporary| std::fs::write(temporary, text).map_err(|e| e.to_string()))
}

pub fn read_text(filename: &str) -> Result<String, String> {
    let bytes = std::fs::read(filename).map_err(|e| e.to_string())?;
    String::from_utf8(bytes).map_err(|e| e.to_string())
}

/// `home/.rays` on Apple, and the platform's preference folder `pref` (SDL's) elsewhere, when
/// SDL names one.
pub fn user_folder(platform: Platform, pref: Option<&str>, home: &str) -> String {
    match (platform, pref) {
        (Platform::Other, Some(folder)) => folder.to_string(),
        (Platform::Apple, _) | (Platform::Other, None) => filename_concat(home, ".rays"),
    }
}

/// OCaml's `Filename.concat`: a separator only when the directory does not end in one.
pub fn filename_concat(dir: &str, file: &str) -> String {
    let (sep, ends) = if cfg!(windows) {
        ("\\", dir.is_empty() || dir.ends_with('/') || dir.ends_with('\\'))
    } else {
        ("/", dir.is_empty() || dir.ends_with('/'))
    };
    if ends { format!("{}{}", dir, file) } else { format!("{}{}{}", dir, sep, file) }
}

/// A sketch's or the editor's saved settings: one `(settings :sketch "name" :key value ...)` form.
pub mod settings {
    use super::{read_text, write_text};
    use super::form::{container, kw, num_of, print, vec_values};
    use flow_syntax::syntax::{make as make_node, Draft, Node, Syntax};

    // Keep the syntax for arbitrary sketch settings and unknown preference keys without a second
    // value model. The editor preferences use typed accessors below.
    pub type Settings = Vec<(String, Syntax)>;

    fn valid(form: &Syntax) -> bool {
        match form.node {
            Node::Sym(name) => matches!(form.name(name), "true" | "false"),
            Node::Num(name) => form.name(name).parse::<i64>().is_ok() || num_of(form).is_some(),
            Node::Str(text) => std::str::from_utf8(form.bytes(text)).is_ok(),
            Node::List(_) => matches!(form.children(), [head, value]
                if matches!(form.arena().row(*head).node, Node::Sym(name) if form.name(name) == "choice")
                && matches!(form.arena().row(*value).node, Node::Str(text) if std::str::from_utf8(form.bytes(text)).is_ok())),
            Node::Vec(_) => vec_values(form).is_some_and(|values| values.len() == 2),
            _ => false,
        }
    }

    /// One `(settings :sketch "name" :key value ...)` form, written atomically.
    pub fn save(sketch: &str, filename: &str, values: &[(String, Syntax)]) -> Result<(), String> {
        if !values.iter().all(|(_, value)| valid(value)) {
            return Err("invalid settings value".into());
        }
        let mut items = vec![
            make_node(Draft::Sym("settings".into())),
            kw("sketch"),
            make_node(Draft::Str(sketch.as_bytes().to_vec())),
        ];
        for (name, value) in values {
            items.push(kw(name));
            items.push(value.clone());
        }
        let form = container(Draft::List, items);
        write_text(filename, &print(&form)?)
    }

    pub fn load(sketch: &str, filename: &str) -> Result<Settings, String> {
        let text = read_text(filename)?;
        let forms = flow_syntax::syntax::parse(&text).map_err(|d| rays_diagnostic::to_string(&d))?;
        let form = match forms.as_slice() {
            [form] if matches!(form.node, Node::List(_)) => form,
            _ => return Err("a settings file is one (settings ...) form".into()),
        };
        let items = match form.children().split_first() {
            Some((head, rest)) if matches!(form.arena().row(*head).node, Node::Sym(s) if form.name(s) == "settings") => rest,
            _ => return Err("a settings file is one (settings ...) form".into()),
        };
        let mut acc: Settings = Vec::new();
        let mut rest = items;
        loop {
            match rest {
                [] => return Ok(acc),
                [key, v, tail @ ..] if matches!(form.arena().row(*key).node, Node::Kw(_)) => {
                    let Node::Kw(k) = form.arena().row(*key).node else { unreachable!() };
                    let k = form.name(k);
                    if k == "sketch" {
                        if !matches!(form.arena().row(*v).node, Node::Str(name) if form.bytes(name) == sketch.as_bytes()) {
                            return Err("settings belong to another sketch".into());
                        }
                    } else {
                        let value = form.child(*v);
                        if !valid(&value) { return Err("invalid settings value".into()); }
                        acc.push((k.to_owned(), value));
                    }
                    rest = tail;
                }
                _ => return Err("invalid settings entry".into()),
            }
        }
    }

    /// The first entry named `name`, when it has the right kind.
    fn first_named<'a>(settings: &'a Settings, name: &str) -> Option<&'a Syntax> {
        settings.iter().find(|(key, _)| key == name).map(|(_, value)| value)
    }

    pub fn bool(settings: &Settings, name: &str) -> Option<bool> {
        let form = first_named(settings, name)?;
        match form.node {
            Node::Sym(value) if form.name(value) == "true" => Some(true),
            Node::Sym(value) if form.name(value) == "false" => Some(false),
            _ => None,
        }
    }

    pub fn float(settings: &Settings, name: &str) -> Option<f64> {
        let form = first_named(settings, name)?;
        match form.node {
            Node::Num(value) if form.name(value).parse::<i64>().is_err() => num_of(form),
            _ => None,
        }
    }

    pub fn int(settings: &Settings, name: &str) -> Option<i64> {
        let form = first_named(settings, name)?;
        match form.node {
            Node::Num(value) => form.name(value).parse().ok(),
            _ => None,
        }
    }
}
