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
    use super::form::{container, kw, num_of, print, vec_of_numbers, vec_values};
    use flow_syntax::syntax::{make as make_node, Draft, Node, Syntax};

    #[derive(Clone, Debug, PartialEq)]
    pub enum Value {
        Bool(bool),
        Float(f64),
        Int(i64),
        Text(String),
        Choice(String),
        Pair(f64, f64),
    }

    pub type Settings = Vec<(String, Value)>;

    fn encode(value: &Value) -> Syntax {
        match value {
            Value::Bool(v) => make_node(Draft::Sym(v.to_string())),
            Value::Float(v) => super::form::number(*v),
            Value::Int(v) => make_node(Draft::Num(v.to_string())),
            Value::Text(v) => make_node(Draft::Str(v.clone().into_bytes())),
            Value::Choice(v) => container(Draft::List, vec![
                make_node(Draft::Sym("choice".into())),
                make_node(Draft::Str(v.clone().into_bytes())),
            ]),
            Value::Pair(x, y) => vec_of_numbers(&[*x, *y]),
        }
    }

    fn decode(form: &Syntax) -> Result<Value, String> {
        match &form.node {
            Node::Sym(s) if form.name(*s) == "true" => Ok(Value::Bool(true)),
            Node::Sym(s) if form.name(*s) == "false" => Ok(Value::Bool(false)),
            Node::Num(n) => match form.name(*n).parse::<i64>() {
                Ok(v) => Ok(Value::Int(v)),
                Err(_) => match num_of(form) {
                    Some(v) => Ok(Value::Float(v)),
                    None => Err("invalid settings number".into()),
                },
            },
            Node::Str(v) => match String::from_utf8(form.bytes(*v).to_vec()) {
                Ok(text) => Ok(Value::Text(text)),
                Err(_) => Err("invalid settings text".into()),
            },
            Node::List(items) => match form.range(*items) {
                [head, value] if matches!(form.arena().row(*head).node, Node::Sym(s) if form.name(s) == "choice")
                    && matches!(form.arena().row(*value).node, Node::Str(_)) =>
                {
                    let Node::Str(v) = form.arena().row(*value).node else { unreachable!() };
                    match String::from_utf8(form.bytes(v).to_vec()) {
                        Ok(text) => Ok(Value::Choice(text)),
                        Err(_) => Err("invalid settings text".into()),
                    }
                }
                _ => Err("invalid settings value".into()),
            },
            Node::Vec(_) => match vec_values(form) {
                Some(values) if values.len() == 2 => Ok(Value::Pair(values[0], values[1])),
                _ => Err("invalid settings pair".into()),
            },
            _ => Err("invalid settings value".into()),
        }
    }

    /// One `(settings :sketch "name" :key value ...)` form, written atomically.
    pub fn save(sketch: &str, filename: &str, values: &[(String, Value)]) -> Result<(), String> {
        let finite = values.iter().all(|(_, value)| match value {
            Value::Float(x) => x.is_finite(),
            Value::Pair(x, y) => x.is_finite() && y.is_finite(),
            _ => true,
        });
        if !finite {
            return Err("non-finite settings value".into());
        }
        let mut items = vec![
            make_node(Draft::Sym("settings".into())),
            kw("sketch"),
            make_node(Draft::Str(sketch.as_bytes().to_vec())),
        ];
        for (name, value) in values {
            items.push(kw(name));
            items.push(encode(value));
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
                        let value = decode(&form.child(*v))?;
                        acc.push((k.to_owned(), value));
                    }
                    rest = tail;
                }
                _ => return Err("invalid settings entry".into()),
            }
        }
    }

    /// The first entry named `name` (OCaml's `List.assoc_opt`), when it has the right kind.
    fn first_named(settings: &Settings, name: &str) -> Option<Value> {
        settings.iter().find(|(key, _)| key == name).map(|(_, value)| value.clone())
    }

    pub fn bool(settings: &Settings, name: &str) -> Option<bool> {
        match first_named(settings, name)? {
            Value::Bool(v) => Some(v),
            _ => None,
        }
    }

    pub fn float(settings: &Settings, name: &str) -> Option<f64> {
        match first_named(settings, name)? {
            Value::Float(v) => Some(v),
            _ => None,
        }
    }

    pub fn int(settings: &Settings, name: &str) -> Option<i64> {
        match first_named(settings, name)? {
            Value::Int(v) => Some(v),
            _ => None,
        }
    }
}
